#include "backend/kirt2asm.h"

#include <cassert>
#include <map>
#include <vector>

#include "utils/utils.h"

namespace ASM {

using std::list;
using std::pair;
using std::string;
using std::shared_ptr;
using std::vector;

enum class var_loc_t {
	REG,
	STACK,
	GLOBAL
};

// VarLocation - Register allocation status
// If type == REG, then id is the register id
// If type == STACK, then id is the stack offset (in bytes)
// If type == GLOBAL, then ident is the global identifier (without the leading "@")
struct VarLocation {
	var_loc_t loc;
	int id;
	string ident;

	string get_target_regid(bool is_rhs = false) {
		// If type == REG, return the register id (e.g. "x12")
		// If type == STACK, return t0
		if (loc == var_loc_t::REG) {
			return format("x%d", id);
		} else if (loc == var_loc_t::GLOBAL) {
			return is_rhs ? "t1" : "t0";
		} else if (loc == var_loc_t::STACK) {
			return is_rhs ? "t1" : "t0";
		} else {
			assert(0);
		}
	}

	list<string> get_load_inst(bool is_rhs = false) {
		// If type == REG, return an empty list
		// If type == STACK, return a list of instructions to load the value from the stack
		if (loc == var_loc_t::REG) {
			return {};
		} else if (loc == var_loc_t::GLOBAL) {
			return {
				format(
					"  la t2, %s",
					ident.c_str()
				),
				format(
					"  lw %s, 0(t2)",
					is_rhs ? "t1" : "t0"
				)
			};
		} else if (loc == var_loc_t::STACK) {
			return {
				format("  li t2, %d", id),
				format("  add t2, sp, t2"),
				format(
					"  lw %s, 0(t2)",
					is_rhs ? "t1" : "t0"
				)
			};
		} else {
			assert(0);
		}
	}

	list<string> get_store_inst() {
		// If type == REG, return an empty list
		// If type == STACK, return a list of instructions to store the value to the stack
		if (loc == var_loc_t::REG) {
			return {};
		} else if (loc == var_loc_t::GLOBAL) {
			return {
				format(
					"  la t2, %s",
					ident.c_str()
				),
				format(
					"  sw t0, 0(t2)"
				)
			};
		} else if (loc == var_loc_t::STACK) {
			return {
				format("  li t2, %d", id),
				format("  add t2, sp, t2"),
				format(
					"  sw t0, 0(t2)",
					id
				)
			};
		} else {
			assert(0);
		}
	}
};

// VarLocManager - An on-the-fly variable location manager
class VarLocManager {
public:
	// std::vector<int> free_regids;
	int stack_var_cnt;
	std::map<string, VarLocation> ident2stat;

	// Called when entering a function
	void on_enter_function() {
		stack_var_cnt = 1;	// Start from "1" since "0" is reserved for the stack frame size
		ident2stat.clear();
	}

	int get_max_num_local_vars() {
		return stack_var_cnt;
	}

	// Called when defining a function argument
	list<string> on_new_func_arg(string ident, int kth, int total_local_var_and_saved_regs) {
		// kth starts from 0
		// `total_local_var_and_saved_regs` is (address of the 8th argument) - (final sp), in bytes
		// Currently we save all function arguments to the stack
		ident2stat[ident] = {var_loc_t::STACK, stack_var_cnt*4};
		stack_var_cnt += 1;
		// Save the variable to the stack
		if (kth <= 7) {
			return {
				format("  li t0, %d", (stack_var_cnt-1)*4),
				format("  add t0, sp, t0"),
				format("  sw a%d, 0(t0)", kth)
			};
		} else {
			return {
				format("  li t0, %d", 4*(kth-8) + (total_local_var_and_saved_regs)*4),
				format("  add t0, sp, t0"),
				format("  lw t1, 0(t0)"),
				format("  li t0, %d", (stack_var_cnt-1)*4),
				format("  add t0, sp, t0"),
				format("  sw t1, 0(t0)")
			};
		}
	}

	// Called when loading an identifer
	VarLocation on_load(string ident) {
		if (ident == "0") {
			return {var_loc_t::REG, 0, ""};
		}
		if (KIRT::global_decl_map.count(ident)) {
			return {var_loc_t::GLOBAL, 0, ident.substr(1)};
		}
		my_assert (9, ident2stat.count(ident));
		VarLocation stat = ident2stat[ident];
		my_assert (10, stat.loc == var_loc_t::STACK);
		return stat;
	}

	// Called when storing an identifier
	VarLocation on_store(string ident) {
		if (ident == "0") {
			return {var_loc_t::REG, 0, ""};
		}
		if (KIRT::global_decl_map.count(ident)) {
			return {var_loc_t::GLOBAL, 0, ident.substr(1)};
		}
		if (!ident2stat.count(ident)) {
			int stack_offset = stack_var_cnt*4;
			stack_var_cnt += 1;
			ident2stat[ident] = {var_loc_t::STACK, stack_offset, ""};
		}
		VarLocation stat = ident2stat[ident];
		my_assert (12, stat.loc == var_loc_t::STACK);
		return stat;
	}
} var_loc_manager;

static string kirt_exp_t2asm(KIRT::exp_t type) {
	switch (type) {
		case KIRT::exp_t::NUMBER:
			my_assert(13, 0);
		case KIRT::exp_t::LVAL:
			my_assert(14, 0);
		case KIRT::exp_t::FUNC_CALL:
			my_assert(15, 0);
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
Counter virt_ident_counter;	// A counter for generating virtual identifer
string cur_func_name;	// Current function name
list<string> cur_func_epilogue;	// Current function epilogue (e.g. restore stack pointer)

inline string rename_block(const string &block_name) {
	return cur_func_name + "_" + block_name;
}

list<string> kirt2asm(const KIRT::Function &func);
list<string> kirt2asm(const KIRT::Block &block);
list<string> kirt2asm(const shared_ptr<KIRT::Inst> &inst);
list<string> kirt2asm(const shared_ptr<KIRT::TermInst> &inst);
pair<list<string>, string> kirt2asm(const KIRT::Exp &inst);

list<string> kirt2asm(const KIRT::Program &prog) {
	list<string> res;

	res.push_back("  .data");
	for (const std::shared_ptr<KIRT::GlobalDecl> &global_decl : prog.global_decls) {
		res.push_back("  .globl " + global_decl->ident.substr(1));
		res.push_back(global_decl->ident.substr(1) + ":");
		if (global_decl->type.is_int()) {
			res.push_back("  .word " + std::to_string(global_decl->init_val));
		} else if (global_decl->type.is_arr()) {
			for (int i = 0; i < global_decl->arr_init_vals.size(); i++) {
				res.push_back("  .word " + std::to_string(global_decl->arr_init_vals[i]));
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
	var_loc_manager.on_enter_function();
	cur_func_name = func.name;

	// Construct the epilogue
	{
		cur_func_epilogue.clear();
		cur_func_epilogue.push_back("  lw t0, 0(sp)");
		cur_func_epilogue.push_back("  add sp, sp, t0");
		// TODO Sometimes we do not utilize all registers, hence do not need to
		// store & restore all callee-saved registers
		for (auto it = callee_saved_regs.rbegin(); it != callee_saved_regs.rend(); ++it) {
			cur_func_epilogue.push_back(format(
				"  lw %s, 0(sp)",
				it->c_str()
			));
			// TODO Optimize this addition
			cur_func_epilogue.push_back("  addi sp, sp, 4");
		}
	}

	list<string> res;

	int stack_frame_length;	// in 4 bytes, include local vars, but does not include saved regs
	// stack_frame_length = var_loc_manager.get_max_num_local_vars() + 1;	// +1 for the "stack frame length" (stored at 0(sp))
	stack_frame_length = 1000 + 1 + std::max(0, (int)func.fparams.size()-8);	// +1 for the "stack frame length" (stored at 0(sp))
	if (stack_frame_length % 4 != 0) {
		stack_frame_length += 4 - (stack_frame_length % 4);
	}

	// Arguments registration
	int total_local_var_and_saved_regs = callee_saved_regs.size() + stack_frame_length;
	for (int i = 0; i < func.fparams.size(); i++) {
		const KIRT::FuncFParam &param = func.fparams[i];
		list<string> save_arg_insts = var_loc_manager.on_new_func_arg(param.ident, i, total_local_var_and_saved_regs);
		res << save_arg_insts;
	}

	for (const std::shared_ptr<KIRT::Block> &block : func.blocks.blocks) {
		list<string> block_asm = kirt2asm(*block);
		res << block_asm;
	}

	my_assert (17, var_loc_manager.get_max_num_local_vars() <= 1000);

	// Construct the prologue
	{
		list<string> prolouge;
		// Function declaration
		prolouge.push_back("  .globl " + func.name);
		prolouge.push_back(func.name + ":");

		// Save registers
		// TODO Optmize `main()` does not need to save & store registers
		for (const string &reg : callee_saved_regs) {
			// TODO Optimize this subtraction
			prolouge.push_back("  addi sp, sp, -4");
			prolouge.push_back(format(
				"  sw %s, 0(sp)",
				reg.c_str()
			));
		}

		// Stack frame allocation
		// TODO Optimize when space_to_alloc == 0
		// TODO Optimize when space_to_alloc <= 2047
		prolouge.push_back(format(
			"  li t0, %d",
			stack_frame_length*4
		));
		prolouge.push_back("  sub sp, sp, t0");
		prolouge.push_back("  sw t0, 0(sp)");

		prolouge >> res;
	}

	return res;
}

list<string> kirt2asm(const KIRT::Block &block) {
	list<string> res;
	res.push_back(rename_block(block.name) + ":");

	for (const shared_ptr<KIRT::Inst> &inst : block.insts) {
		list<string> inst_asm = kirt2asm(inst);
		res << inst_asm;
	}

	list<string> term_inst_asm = kirt2asm(block.term_inst);
	res << term_inst_asm;

	return res;
}

list<string> kirt2asm(const shared_ptr<KIRT::Inst> &inst) {
	list<string> res;

	if (const KIRT::AssignInst *assign_inst = dynamic_cast<const KIRT::AssignInst *>(inst.get())) {
		auto [exp_inst_list, exp_virt_ident] = kirt2asm(assign_inst->exp);
		res << exp_inst_list;

		VarLocation exp_alloc_stat = var_loc_manager.on_load(exp_virt_ident);
		res <= exp_alloc_stat.get_load_inst();

		if (assign_inst->lval.is_int()) {
			VarLocation target_alloc_stat = var_loc_manager.on_store(assign_inst->lval.ident);
			res.push_back(format(
				"  mv %s, %s",
				target_alloc_stat.get_target_regid().c_str(),
				exp_alloc_stat.get_target_regid().c_str()
			));
			res <= target_alloc_stat.get_store_inst();
		} else if (assign_inst->lval.is_arr()) {
			assert(0);
		} else {
			my_assert(18, 0);
		}
	} else if (const KIRT::ExpInst *exp_inst = dynamic_cast<const KIRT::ExpInst *>(inst.get())) {
		auto [exp_inst_list, exp_virt_ident] = kirt2asm(exp_inst->exp);
		res << exp_inst_list;
	} else {
		my_assert(18, 0);
	}
	return res;
}

list<string> kirt2asm(const shared_ptr<KIRT::TermInst> &inst) {
	list<string> res;

	if (const KIRT::ReturnInst *ret_inst = dynamic_cast<const KIRT::ReturnInst *>(inst.get())) {
		if (ret_inst->ret_exp) {
			auto [exp_inst_list, exp_virt_ident] = kirt2asm(*ret_inst->ret_exp);
			res << exp_inst_list;
			
			VarLocation exp_alloc_stat = var_loc_manager.on_load(exp_virt_ident);
			res <= exp_alloc_stat.get_load_inst();
			res.push_back(format(
				"  mv a0, %s",
				exp_alloc_stat.get_target_regid().c_str()
			));
		}
		res <= cur_func_epilogue;
		res.push_back("  ret");
	} else if (const KIRT::JumpInst *jump_inst = dynamic_cast<const KIRT::JumpInst *>(inst.get())) {
		res.push_back(format(
			"  j %s",
			rename_block(jump_inst->target_block->name).c_str()
		));
	} else if (const KIRT::BranchInst *branch_inst = dynamic_cast<const KIRT::BranchInst *>(inst.get())) {
		auto [cond_inst_list, cond_virt_ident] = kirt2asm(branch_inst->cond);
		res << cond_inst_list;

		// TODO Now we first store the value of the instruction into a reg, then use
		// bnez to jump. This can be optimized
		// TODO Now we always jump to the false block, via `j %s`. In the future we may
		// get rid of this jump via BlockReorderPass
		// TODO Currently short-circuit is not supported
		VarLocation cond_alloc_stat = var_loc_manager.on_load(cond_virt_ident);
		res <= cond_alloc_stat.get_load_inst();
		res.push_back(format(
			"  bnez %s, %s",
			cond_alloc_stat.get_target_regid().c_str(),
			rename_block(branch_inst->true_block->name).c_str()
		));
		res.push_back(format(
			"  j %s",
			rename_block(branch_inst->false_block->name).c_str()
		));
	} else {
		my_assert(19, 0);
	}

	return res;
}

// Return: (instruction list, result virtual identifier)
pair<list<string>, string> kirt2asm(const KIRT::Exp &exp) {
	list<string> res;
	if (exp.type == KIRT::exp_t::LVAL) {
		assert(0);
		string ident = exp.lval.ident;
		return {res, ident};
	} else if (exp.type == KIRT::exp_t::FUNC_CALL) {
		string res_virt_ident = format("v%d", virt_ident_counter.next());
		VarLocation res_reg_alloc_stat = var_loc_manager.on_store(res_virt_ident);

		int num_args = exp.args.size();
		vector<string> arg_virt_idents;

		// Calculate args
		for (int i = 0; i < exp.args.size(); ++i) {
			auto [arg_inst_list, arg_virt_ident] = kirt2asm(*exp.args[i]);
			res << arg_inst_list;
			arg_virt_idents.push_back(arg_virt_ident);
		}
		// Place args
		for (int i = 0; i < exp.args.size(); ++i) {
			string arg_virt_ident = arg_virt_idents[i];
			if (i <= 7) {
				// Save the argument to the register
				VarLocation arg_alloc_stat = var_loc_manager.on_load(arg_virt_ident);
				res <= arg_alloc_stat.get_load_inst();
				res.push_back(format(
					"  mv a%d, %s",
					i,
					arg_alloc_stat.get_target_regid().c_str()
				));
			} else {
				// Save the argument to the stack
				VarLocation arg_alloc_stat = var_loc_manager.on_load(arg_virt_ident);
				res <= arg_alloc_stat.get_load_inst();
				res.push_back(format(
					"  sw %s, %d(sp)",
					arg_alloc_stat.get_target_regid().c_str(),
					-(num_args-i)*4
				));
			}
		}
		// Adjust SP
		if (num_args > 8) {
			res.push_back(format(
				"  addi sp, sp, -%d",
				(num_args-8)*4
			));
		}
		// Call
		res.push_back(format(
			"  call %s",
			exp.func_name.c_str()
		));
		// Restore SP
		if (num_args > 8) {
			res.push_back(format(
				"  addi sp, sp, %d",
				(num_args-8)*4
			));
		}
		// Save the return value
		res.push_back(format(
			"  mv %s, a0",
			res_reg_alloc_stat.get_target_regid().c_str()
		));
		res <= res_reg_alloc_stat.get_store_inst();
		return {res, res_virt_ident};
	} else {
		string res_virt_ident = format("v%d", virt_ident_counter.next());
		VarLocation res_reg_alloc_stat = var_loc_manager.on_store(res_virt_ident);
		if (exp.type == KIRT::exp_t::NUMBER) {
			if (exp.number != 0) {
				res.push_back(format(
					"  li %s, %d",
					res_reg_alloc_stat.get_target_regid().c_str(),
					exp.number
				));
				res <= res_reg_alloc_stat.get_store_inst();
				return {res, res_virt_ident};
			} else {
				return {res, "0"};
			}
		} else if (exp.type == KIRT::exp_t::EQ0 || exp.type == KIRT::exp_t::NEQ0) {
			auto [lhs_inst_list, lhs_virt_ident] = kirt2asm(*exp.lhs);
			res << lhs_inst_list;

			VarLocation lhs_alloc_stat = var_loc_manager.on_load(lhs_virt_ident);
			res <= lhs_alloc_stat.get_load_inst();
			res.push_back(format(
				"  %s %s, %s",
				exp.type == KIRT::exp_t::EQ0 ? "seqz" : "snez",
				res_reg_alloc_stat.get_target_regid().c_str(),
				lhs_alloc_stat.get_target_regid().c_str()
			));
			res <= res_reg_alloc_stat.get_store_inst();
			return {res, res_virt_ident};
		} else {
			auto [lhs_inst_list, lhs_virt_ident] = kirt2asm(*exp.lhs);
			auto [rhs_inst_list, rhs_virt_ident] = kirt2asm(*exp.rhs);
			res << lhs_inst_list;
			res << rhs_inst_list;

			VarLocation lhs_alloc_stat = var_loc_manager.on_load(lhs_virt_ident);
			VarLocation rhs_alloc_stat = var_loc_manager.on_load(rhs_virt_ident);
			res <= lhs_alloc_stat.get_load_inst();
			res <= rhs_alloc_stat.get_load_inst(true);
			res.push_back(format(
				"  %s %s, %s, %s",
				kirt_exp_t2asm(exp.type).c_str(),
				res_reg_alloc_stat.get_target_regid().c_str(),
				lhs_alloc_stat.get_target_regid().c_str(),
				rhs_alloc_stat.get_target_regid(true).c_str()
			));
			res <= res_reg_alloc_stat.get_store_inst();
			return {res, res_virt_ident};
		}
	}
}

}