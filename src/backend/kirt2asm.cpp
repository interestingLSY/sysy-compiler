#include "backend/kirt2asm.h"

#include <cassert>
#include <map>
#include <optional>
#include <unordered_map>
#include <vector>

#include "utils/utils.h"

namespace ASM {

using std::list;
using std::pair;
using std::string;
using std::shared_ptr;
using std::vector;

extern list<string> cur_func_asm;	// Will be defined later in this file
#define PUSH_ASM(...) cur_func_asm.push_back(format(__VA_ARGS__))

// A variable & array manager
// This utility manages all function params / local vars / local arrays / temp vars,
// providing functions for loading and storing values
//
// We have the following aggrements:
// - Each array is coupled with a variable named `<arr_name>#addr`, which stores
//   the address of the array
// - One should not modify the register returned by `load_var_to_reg` whose
//   source is local var / global var / local array address / func param. Temp
//   vars is OK as long as you release it immediately after using it
class VarManager {
private:
	// NOTE All `offsets` are offset relative to the OLD stack pointer (i.e. the
	// top of the stack frame)
	struct VarMeta {
		int offset;
	};
	struct ArrayMeta {
		int space_offset;	// Offset to the space allocated for the array
		int size;	// Size of the array, in words
	};

	Counter temp_var_counter;

	// The current number of elements (ints) on the stack
	int cur_num_stack_elems;

	// Data about all local variables (including function parameters which will
	// be copied to the stack, param/local array addresses, and temp vars)
	std::unordered_map<string, VarMeta> local_var_meta_db;

	// Data about all local arrays (only local arrays, not function parameters)
	std::unordered_map<string, ArrayMeta> local_arr_meta_db;

	// Data about all parameterized arrays (we just record their names here)
	std::unordered_map<string, bool> param_arr_meta_db;

	bool have_allocated_idle_reg;

	int _register_var(const string &ident) {
		int cur_offset = cur_num_stack_elems+1;
		cur_num_stack_elems += 1;
		local_var_meta_db[ident] = {cur_offset};
		return cur_offset;
	}

	int _register_local_arr(const string &ident, int size) {
		int cur_offset = cur_num_stack_elems+size;
		cur_num_stack_elems += size;
		local_arr_meta_db[ident] = {cur_offset, size};
		return cur_offset;
	}

	void _register_param_arr(const string &ident) {
		param_arr_meta_db[ident] = true;
	}

public:
	int get_cur_num_stack_elems() {
		return cur_num_stack_elems;
	}

	// Called when entering a function
	// `num_saved_regs` slots will be reserved for saving registers
	void on_enter_func(int num_saved_regs) {
		temp_var_counter.reset();
		cur_num_stack_elems = num_saved_regs;
		local_var_meta_db.clear();
		local_arr_meta_db.clear();
		param_arr_meta_db.clear();
		have_allocated_idle_reg = false;
	}

	// Register function parameters
	void register_param(const KIRT::FuncFParam &param, int kth) {
		int offset;
		if (param.type.is_int()) {
			// An integer parameter
			offset = _register_var(param.ident);
		} else if (param.type.is_arr()) {
			// An array parameter
			offset = _register_var(param.ident + "#addr");
			_register_param_arr(param.ident);
		} else {
			assert(0);
		}
		// Copy the argument to my stack frame
		if (kth <= 7) {
			// We can safely assume offset*4 <= 2047 here since
			// - This func arg  is the first 8 func args
			// - We always first register the first 8 func args
			my_assert(34, offset*4 <= 2047);
			PUSH_ASM("  sw a%d, %d(sp)", kth, -offset*4);
		} else {
			PUSH_ASM("  lw t1, %d(sp)", 4*(kth-8));
			PUSH_ASM("  sw t1, %d(sp)", -offset*4);
		}
	}

	// Register a local variable
	void register_local_var(const KIRT::FuncLocalVar &local_var) {
		_register_var(local_var.ident);
	}

	// Register a local array
	// TODO Maybe we should register all vars first to avoid using indirect
	// address to load/store variables
	void register_local_arr(const KIRT::FuncLocalVar &local_arr) {
		string arr_addr_var_ident = local_arr.ident + "#addr";
		_register_var(arr_addr_var_ident);
		int offset = _register_local_arr(local_arr.ident, local_arr.type.numel());
		// Write down the address of the array
		string arr_addr_reg = alloc_reg_for_var(arr_addr_var_ident);
		PUSH_ASM("  li %s, %d", arr_addr_reg.c_str(), -offset*4);
		PUSH_ASM("  add %s, sp, %s", arr_addr_reg.c_str(), arr_addr_reg.c_str());
		store_var_from_reg(arr_addr_var_ident, arr_addr_reg);
	}

	// Register a temp variable
	// Return the virtual variable name
	string register_temp_var() {
		string ident = format("temp%d", temp_var_counter.next());
		_register_var(ident);
		return ident;
	}

	// Release a variable if it is a temp var (i.e. it won't be used in the future)
	void release_var_if_temp(const string &ident) {
		// Do nothing now.
		// TODO Release the stack space of the temp var, to better leverage
		// spatial locality and temporal locality
	}

	// Give me an idle register
	// WARN: alloc_idle_reg can not be called twice without calling free_idle_reg
	string alloc_idle_reg() {
		my_assert(36, !have_allocated_idle_reg);
		have_allocated_idle_reg = true;
		return "t3";
	}

	// Free an idle register
	void free_idle_reg(const string &ident) {
		my_assert(35, have_allocated_idle_reg);
		have_allocated_idle_reg = false;
	}

	// Allocate a register for a variable (without loading the value)
	// if hold_reg is not empty, then that register won't be used
	string alloc_reg_for_var(
		const string &ident,
		const std::optional<string> &hold_reg1 = std::nullopt,
		const std::optional<string> &hold_reg2 = std::nullopt
	) {
		if (ident == "0") {
			return "x0";
		}
		int hold_mask = 0;
		if (hold_reg1.has_value()) hold_mask |= hold_reg1.value() == "t0" ? 1 : hold_reg1.value() == "t1" ? 2 : 0;
		if (hold_reg2.has_value()) hold_mask |= hold_reg2.value() == "t0" ? 1 : hold_reg2.value() == "t1" ? 2 : 0;
		const char* final_reg = (hold_mask&1) == 0 ? "t0" : (hold_mask&2) == 0 ? "t1" : "t2";
		return final_reg;
	}

	// Load the value of one variable to a register
	// if hold_reg is not empty, then that register won't be used
	string load_var_to_reg(
		const string &ident,
		const std::optional<string> &hold_reg1 = std::nullopt,
		const std::optional<string> &hold_reg2 = std::nullopt
	) {
		if (ident == "0") {
			return "x0";
		}
		string final_reg_string = alloc_reg_for_var(ident, hold_reg1, hold_reg2);
		const char* final_reg = final_reg_string.c_str();
		if (KIRT::global_decl_map.count(ident)) {
			// A global variable
			PUSH_ASM("  la %s, %s", final_reg, ident.substr(1).c_str());
			PUSH_ASM("  lw %s, 0(%s)", final_reg, final_reg);
		} else if (local_var_meta_db.count(ident)) {
			// A local variable
			VarMeta& meta = local_var_meta_db[ident];
			if (-2048 <= -meta.offset*4 && -meta.offset*4 < 2048) {
				PUSH_ASM("  lw %s, %d(sp)", final_reg, -meta.offset*4);
			} else {
				PUSH_ASM("  li %s, %d", final_reg, -meta.offset*4);
				PUSH_ASM("  add %s, sp, %s", final_reg, final_reg);
				PUSH_ASM("  lw %s, 0(%s)", final_reg, final_reg);
			}
		} else {
			// Variable not found
			my_assert(32, false);
		}
		return final_reg;
	}

	// Store the value of one variable from a register
	// `reg` may be the register returned by `load_var_to_reg`
	void store_var_from_reg(const string &ident, const string &reg) {
		my_assert(34, ident != "0");
		const char* temp_reg = reg == "t0" ? "t1" : "t0";
		if (KIRT::global_decl_map.count(ident)) {
			// A global variable
			PUSH_ASM("  la %s, %s", temp_reg, ident.substr(1).c_str());
			PUSH_ASM("  sw %s, 0(%s)", reg.c_str(), temp_reg);
		} else if (local_var_meta_db.count(ident)) {
			// A local variable
			VarMeta& meta = local_var_meta_db[ident];
			if (-2048 <= -meta.offset*4 && -meta.offset*4 < 2048) {
				PUSH_ASM("  sw %s, %d(sp)", reg.c_str(), -meta.offset*4);
			} else {
				PUSH_ASM("  li %s, %d", temp_reg, -meta.offset*4);
				PUSH_ASM("  add %s, sp, %s", temp_reg, temp_reg);
				PUSH_ASM("  sw %s, 0(%s)", reg.c_str(), temp_reg);
			}
		} else {
			// Variable not found
			my_assert(32, false);
		}
	}

	// Load the address of one array to a register
	// if hold_reg is not empty, then that register won't be used
	string load_arr_addr_to_reg(const string &arr_ident, const std::optional<string> &hold_reg) {
		const char* final_reg = hold_reg.has_value() && hold_reg.value() == "t0" ? "t1" : "t0";
		if (KIRT::global_decl_map.count(arr_ident)) {
			// This is a global array
			PUSH_ASM("  la %s, %s", final_reg, arr_ident.substr(1).c_str());
			return final_reg;
		} else if (local_arr_meta_db.count(arr_ident) | param_arr_meta_db.count(arr_ident)) {
			// A local array / param array
			return load_var_to_reg(arr_ident + "#addr", hold_reg);
		} else {
			printf("arr_ident: %s\n", arr_ident.c_str());
			// Array not found
			my_assert(33, false);
		}
	}
} var_manager;

static string kirt_exp_t2asm(KIRT::exp_t type) {
	switch (type) {
		case KIRT::exp_t::NUMBER:
			my_assert(13, 0);
		case KIRT::exp_t::LVAL:
			my_assert(14, 0);
		case KIRT::exp_t::FUNC_CALL:
			my_assert(15, 0);
		case KIRT::exp_t::ARR_ADDR:
			my_assert(16, 0);
		case KIRT::exp_t::ADDR_ADD:
			return "add";
		case KIRT::exp_t::ADD:
			return "add";
		case KIRT::exp_t::SUB:
			return "sub";
		case KIRT::exp_t::MUL:
			return "mul";
		case KIRT::exp_t::DIV:
			return "div";
		case KIRT::exp_t::REM:
			return "rem";
		case KIRT::exp_t::LT:
			return "slt";
		case KIRT::exp_t::GT:
			return "sgt";
		case KIRT::exp_t::BITWISE_AND:
			return "and";
		case KIRT::exp_t::BITWISE_OR:
			return "or";
		case KIRT::exp_t::BITWISE_XOR:
			return "xor";
		case KIRT::exp_t::SHL:
			return "sll";
		case KIRT::exp_t::SHR:
			return "srl";
		case KIRT::exp_t::SAR:
			return "sra";
		default:
			my_assert(16, 0);
	}
}

// const vector<string> callee_saved_regs({
// 	"ra", "s0", "s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9", "s10", "s11"
// });

// Currently we never modify s? registers, so we do not need to save & restore them
const vector<string> callee_saved_regs({
	"ra"
});

// Some global status
string cur_func_name;	// Current function name
list<string> cur_func_epilogue;	// Current function epilogue (e.g. restore stack pointer)
list<string> cur_func_asm;	// Current function assembly

inline string rename_block(const string &block_name) {
	return cur_func_name + "_" + block_name;
}

list<string> kirt2asm(const KIRT::Function &func);
void kirt2asm(const KIRT::Block &block);
void kirt2asm(const shared_ptr<KIRT::Inst> &inst);
void kirt2asm(const shared_ptr<KIRT::TermInst> &inst);
string kirt2asm(const KIRT::Exp &inst);

list<string> kirt2asm(const KIRT::Program &prog) {
	list<string> res;

	res.push_back("  .data");
	for (const std::shared_ptr<KIRT::GlobalDecl> &global_decl : prog.global_decls) {
		res.push_back("  .globl " + global_decl->ident.substr(1));
		res.push_back(global_decl->ident.substr(1) + ":");
		if (global_decl->type.is_int()) {
			res.push_back("  .word " + std::to_string(global_decl->init_val));
		} else if (global_decl->type.is_arr()) {
			// TODO Optimize by using `.zeros`
			int pos = 0;
			const int numel = global_decl->arr_init_vals.size();
			while (pos < numel) {
				if (global_decl->arr_init_vals[pos] != 0) {
					res.push_back("  .word " + std::to_string(global_decl->arr_init_vals[pos]));
					pos++;
				} else {
					int next_non0 = pos;
					while (next_non0 < numel && global_decl->arr_init_vals[next_non0] == 0) {
						next_non0 += 1;
					}
					res.push_back("  .zero " + std::to_string(4*(next_non0-pos)));
					pos = next_non0;
				}
			}
		} else {
			my_assert(11, 0);
		}
	}
	res.push_back("");

	res.push_back("  .text");
	for (const std::shared_ptr<KIRT::Function> &func : prog.funcs) {
		list<string> func_asm = kirt2asm(*func);
		res << func_asm;
	}

	return res;
}

list<string> kirt2asm(const KIRT::Function &func) {
	cur_func_name = func.name;
	cur_func_asm.clear();

	PUSH_ASM("  .globl %s", func.name.c_str());
	PUSH_ASM("%s:", func.name.c_str());

	// Save registers
	for (int i = 0; i < callee_saved_regs.size(); i++) {
		PUSH_ASM("  sw %s, %d(sp)", callee_saved_regs[i].c_str(), -4*(i+1));
	}

	var_manager.on_enter_func(callee_saved_regs.size());

	// Arguments registration
	for (int i = 0; i < func.fparams.size(); i++) {
		const KIRT::FuncFParam &param = func.fparams[i];
		var_manager.register_param(param, i);
	}

	// Local variable registration
	for (const KIRT::FuncLocalVar &local_var : func.local_vars) {
		if (local_var.type.is_int()) {
			var_manager.register_local_var(local_var);
		} else if (local_var.type.is_arr()) {
			var_manager.register_local_arr(local_var);
		} else {
			my_assert(17, 0);
		}
	}

	// Construct the epilogue
	{
		cur_func_epilogue.clear();
		for (int i = (int)callee_saved_regs.size()-1; i >= 0; i--) {
			cur_func_epilogue.push_back(format("  lw %s, %d(sp)", callee_saved_regs[i].c_str(), -4*(i+1)));
		}
	}

	for (const std::shared_ptr<KIRT::Block> &block : func.blocks.blocks) {
		kirt2asm(*block);
	}

	return cur_func_asm;
}

void kirt2asm(const KIRT::Block &block) {
	PUSH_ASM("%s:", rename_block(block.name).c_str());

	for (const shared_ptr<KIRT::Inst> &inst : block.insts) {
		kirt2asm(inst);
	}

	kirt2asm(block.term_inst);
}

void kirt2asm(const shared_ptr<KIRT::Inst> &inst) {
	if (const KIRT::AssignInst *assign_inst = dynamic_cast<const KIRT::AssignInst *>(inst.get())) {
		string exp_var_ident = kirt2asm(assign_inst->exp);
		if (assign_inst->lval.is_int()) {
			string exp_var_ident_reg = var_manager.load_var_to_reg(exp_var_ident);
			var_manager.store_var_from_reg(assign_inst->lval.ident, exp_var_ident_reg);
		} else if (assign_inst->lval.is_arr()) {
			assert(assign_inst->lval.type.dims() == 1);
			string index_var_ident = kirt2asm(*assign_inst->lval.indices[0]);
			string index_reg = var_manager.load_var_to_reg(index_var_ident);

			string arr_addr_reg = var_manager.load_arr_addr_to_reg(assign_inst->lval.ident, index_reg);

			string item_addr_reg = var_manager.alloc_idle_reg();
			
			PUSH_ASM("  add %s, %s, %s", item_addr_reg.c_str(), arr_addr_reg.c_str(), index_reg.c_str());

			string exp_var_ident_reg = var_manager.load_var_to_reg(exp_var_ident);
			PUSH_ASM("  sw %s, 0(%s)", exp_var_ident_reg.c_str(), item_addr_reg.c_str());

			var_manager.release_var_if_temp(index_var_ident);
			var_manager.free_idle_reg(item_addr_reg);
		} else {
			my_assert(18, 0);
		}
		var_manager.release_var_if_temp(exp_var_ident);
	} else if (const KIRT::ExpInst *exp_inst = dynamic_cast<const KIRT::ExpInst *>(inst.get())) {
		string exp_var_ident = kirt2asm(exp_inst->exp);
		var_manager.release_var_if_temp(exp_var_ident);
	} else {
		my_assert(18, 0);
	}
}

void kirt2asm(const shared_ptr<KIRT::TermInst> &inst) {
	if (const KIRT::ReturnInst *ret_inst = dynamic_cast<const KIRT::ReturnInst *>(inst.get())) {
		if (ret_inst->ret_exp) {
			string exp_var_ident = kirt2asm(*ret_inst->ret_exp);
			string exp_var_ident_reg = var_manager.load_var_to_reg(exp_var_ident, std::nullopt);
			PUSH_ASM("  mv a0, %s", exp_var_ident_reg.c_str());
			var_manager.release_var_if_temp(exp_var_ident);
		}
		cur_func_asm <= cur_func_epilogue;
		PUSH_ASM("  ret");
	} else if (const KIRT::JumpInst *jump_inst = dynamic_cast<const KIRT::JumpInst *>(inst.get())) {
		PUSH_ASM(
			"  j %s",
			rename_block(jump_inst->target_block->name).c_str()
		);
	} else if (const KIRT::BranchInst *branch_inst = dynamic_cast<const KIRT::BranchInst *>(inst.get())) {
		string cond_var_ident = kirt2asm(branch_inst->cond);
		string cond_var_ident_reg = var_manager.load_var_to_reg(cond_var_ident, std::nullopt);

		// TODO Now we first store the value of the instruction into a reg, then use
		// bnez to jump. This can be optimized
		// TODO Now we always jump to the false block, via `j %s`. In the future we may
		// get rid of this jump via BlockReorderPass
		PUSH_ASM(
			"  bnez %s, %s",
			cond_var_ident_reg.c_str(),
			rename_block(branch_inst->true_block->name).c_str()
		);
		PUSH_ASM(
			"  j %s",
			rename_block(branch_inst->false_block->name).c_str()
		);
	} else {
		my_assert(19, 0);
	}
}

// Return: result var ident
string kirt2asm(const KIRT::Exp &exp) {
	if (exp.type == KIRT::exp_t::LVAL) {
		if (exp.lval.is_int()) {
			string ident = exp.lval.ident;
			return ident;
		} else if (exp.lval.is_arr()) {
			assert(exp.lval.type.dims() == 1);
			string index_var_ident = kirt2asm(*exp.lval.indices[0]);
			string index_reg = var_manager.load_var_to_reg(index_var_ident, std::nullopt);

			string arr_addr_reg = var_manager.load_arr_addr_to_reg(exp.lval.ident, index_reg);

			string res_virt_ident = var_manager.register_temp_var();
			string res_reg = var_manager.alloc_reg_for_var(res_virt_ident, index_reg, arr_addr_reg);
			PUSH_ASM("  add %s, %s, %s", res_reg.c_str(), index_reg.c_str(), arr_addr_reg.c_str());
			PUSH_ASM("  lw %s, 0(%s)", res_reg.c_str(), res_reg.c_str());
			var_manager.store_var_from_reg(res_virt_ident, res_reg);

			var_manager.release_var_if_temp(index_var_ident);
			return res_virt_ident;
		} else {
			my_assert(20, 0);
		}
	} else if (exp.type == KIRT::exp_t::FUNC_CALL) {
		int num_args = exp.args.size();
		vector<string> arg_virt_idents;

		// Calculate args
		for (int i = 0; i < exp.args.size(); ++i) {
			string arg_virt_ident = kirt2asm(*exp.args[i]);
			arg_virt_idents.push_back(arg_virt_ident);
		}

		// Advance SP
		// Caution: make sure code after reading `stack_frame_len_var_ident` use
		// no more stack frame space
		string stack_frame_len_var_ident = var_manager.register_temp_var();
		string stack_frame_len_reg = var_manager.alloc_reg_for_var(stack_frame_len_var_ident);
		int num_stack_elems = var_manager.get_cur_num_stack_elems() + 1;	// +1 since we want to store the frame len to the stack
		int stack_frame_len = num_stack_elems*4; 
		PUSH_ASM("  li %s, %d", stack_frame_len_reg.c_str(), stack_frame_len);
		var_manager.store_var_from_reg(stack_frame_len_var_ident, stack_frame_len_reg);

		// Place args
		string arg_addr_temp_reg = var_manager.alloc_idle_reg();
		for (int i = 0; i < exp.args.size(); ++i) {
			string arg_virt_ident = arg_virt_idents[i];
			string arg_reg = var_manager.load_var_to_reg(arg_virt_ident);
			if (i <= 7) {
				// Save the argument to the register
				PUSH_ASM(
					"  mv a%d, %s",
					i,
					arg_reg.c_str()
				);
			} else {
				// Save the argument to the stack
				PUSH_ASM(
					"  li %s, %d",
					arg_addr_temp_reg.c_str(),
					-stack_frame_len-(num_args-i)*4
				);
				PUSH_ASM(
					"  add %s, sp, %s",
					arg_addr_temp_reg.c_str(),
					arg_addr_temp_reg.c_str()
				);
				PUSH_ASM(
					"  sw %s, 0(%s)",
					arg_reg.c_str(),
					arg_addr_temp_reg.c_str()
				);
			}
			var_manager.release_var_if_temp(arg_virt_ident);
		}
		var_manager.free_idle_reg(arg_addr_temp_reg);

		stack_frame_len_reg = var_manager.load_var_to_reg(stack_frame_len_var_ident);
		PUSH_ASM("  sub sp, sp, %s", stack_frame_len_reg.c_str());
		PUSH_ASM("  sw %s, 0(sp)", stack_frame_len_reg.c_str());
		var_manager.release_var_if_temp(stack_frame_len_var_ident);

		// Adjust SP
		if (num_args > 8) {
			PUSH_ASM(
				"  addi sp, sp, -%d",
				(num_args-8)*4
			);
		}

		// Call
		PUSH_ASM(
			"  call %s",
			exp.func_name.c_str()
		);

		// Restore SP
		if (num_args > 8) {
			PUSH_ASM(
				"  addi sp, sp, %d",
				(num_args-8)*4
			);
		}

		// Step-back SP
		stack_frame_len_reg = var_manager.alloc_idle_reg();
		PUSH_ASM("  lw %s, 0(sp)", stack_frame_len_reg.c_str());
		PUSH_ASM("  add sp, sp, %s", stack_frame_len_reg.c_str());
		var_manager.free_idle_reg(stack_frame_len_reg);

		// Save the return value
		string res_virt_ident = var_manager.register_temp_var();
		var_manager.store_var_from_reg(res_virt_ident, "a0");
		return res_virt_ident;
	} else if (exp.type == KIRT::exp_t::ARR_ADDR) {
		string res_virt_ident = var_manager.register_temp_var();
		string addr_reg = var_manager.load_arr_addr_to_reg(exp.arr_name, std::nullopt);
		var_manager.store_var_from_reg(res_virt_ident, addr_reg);
		return res_virt_ident;
	} else {
		string res_virt_ident = var_manager.register_temp_var();
		if (exp.type == KIRT::exp_t::NUMBER) {
			string res_reg = var_manager.alloc_reg_for_var(res_virt_ident, std::nullopt);
			PUSH_ASM("  li %s, %d", res_reg.c_str(), exp.number);
			var_manager.store_var_from_reg(res_virt_ident, res_reg);
			return res_virt_ident;
			// TODO Optimize for zero
			// if (exp.number != 0) {
			// 	var_manager.store_local_var_from_int(res_virt_ident, exp.number);
			// 	return res_virt_ident;
			// } else {
			// 	var_manager.release_var_if_temp(res_virt_ident);
			// 	return "0";
			// }
		} else if (exp.type == KIRT::exp_t::EQ0 || exp.type == KIRT::exp_t::NEQ0) {
			string lhs_virt_ident = kirt2asm(*exp.lhs);
			string lhs_reg = var_manager.load_var_to_reg(lhs_virt_ident, std::nullopt);
			string res_reg = var_manager.alloc_reg_for_var(res_virt_ident, lhs_reg);

			PUSH_ASM(
				"  %s %s, %s",
				exp.type == KIRT::exp_t::EQ0 ? "seqz" : "snez",
				res_reg.c_str(),
				lhs_reg.c_str()
			);
			var_manager.store_var_from_reg(res_virt_ident, res_reg);
			var_manager.release_var_if_temp(lhs_virt_ident);
			return res_virt_ident;
		} else {
			auto lhs_virt_ident = kirt2asm(*exp.lhs);
			auto rhs_virt_ident = kirt2asm(*exp.rhs);
			string lhs_reg = var_manager.load_var_to_reg(lhs_virt_ident, std::nullopt);
			string rhs_reg = var_manager.load_var_to_reg(rhs_virt_ident, lhs_reg);
			string res_reg = var_manager.alloc_reg_for_var(res_virt_ident, lhs_reg, rhs_reg);

			PUSH_ASM(
				"  %s %s, %s, %s",
				kirt_exp_t2asm(exp.type).c_str(),
				res_reg.c_str(),
				lhs_reg.c_str(),
				rhs_reg.c_str()
			);
			var_manager.store_var_from_reg(res_virt_ident, res_reg);
			var_manager.release_var_if_temp(lhs_virt_ident);
			var_manager.release_var_if_temp(rhs_virt_ident);
			return res_virt_ident;
		}
	}
}

}