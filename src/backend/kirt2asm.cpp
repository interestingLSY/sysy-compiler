#include "backend/kirt2asm.h"

#include <cassert>
#include <climits>
#include <map>
#include <optional>
#include <set>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "utils/utils.h"

namespace ASM {

using std::list;
using std::pair;
using std::string;
using std::shared_ptr;
using std::optional, std::nullopt;
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
	enum class var_t {
		LOCAL_VAR,		// Include local variables, function parameters, temp vars, and addr of local arrs
		GLOBAL_VAR,		// A global variable
		GLOBAL_ARR_ADDR	// The address of a global variable
	};

	// NOTE All `offsets` are offset relative to the OLD stack pointer (i.e. the
	// top of the stack frame)
	struct VarMeta {
		var_t type;
		// The offset relative to the OLD stack pointer. Only valid for local vars
		int offset;
		// Name of the global var / arr, only valid for global vars / global arr addrs
		string global_ident;
		// Whether it has a corresponding register
		optional<string> corresp_reg;
		// Whether the corresponding register is dirty (which means that a
		// write-back is necessary when evicting the register)
		bool is_dirty;
		// The last time this variable is used (for LRU replacement policy)
		int last_use_timestamp;
	};

	struct ArrayMeta {
		int space_offset;	// Offset to the space allocated for the array
		int size;	// Size of the array, in words
	};

	Counter temp_var_counter;

	// The current number of elements (ints) on the stack
	int cur_num_stack_elems;
	std::set<int> free_stack_offsets;	// Free stack offsets (maybe after freeing a temp var)

	// Data about allvariables (including function parameters which will
	// be copied to the stack, param/local array addresses, global vars, and temp vars)
	std::unordered_map<string, VarMeta> var_meta_db;

	// Data about all local arrays (only local arrays, not function parameters)
	std::unordered_map<string, ArrayMeta> local_arr_meta_db;

	void _register_var(const string &ident, var_t type) {
		if (type == var_t::LOCAL_VAR) {
			int cur_offset;
			if (!free_stack_offsets.empty()) {
				auto iter = free_stack_offsets.begin();
				cur_offset = *iter;
				free_stack_offsets.erase(iter);
			} else {
				cur_offset = cur_num_stack_elems+1;
				cur_num_stack_elems += 1;
			}
			var_meta_db[ident] = {var_t::LOCAL_VAR, cur_offset, "", nullopt, false, 0};
		} else if (type == var_t::GLOBAL_VAR || type == var_t::GLOBAL_ARR_ADDR) {
			var_meta_db[ident] = {type, -1, ident.substr(1), nullopt, false, 0};
		} else {
			assert(0);
		}
	}

	int _register_local_arr(const string &ident, int size) {
		int cur_offset = cur_num_stack_elems+size;
		cur_num_stack_elems += size;
		local_arr_meta_db[ident] = {cur_offset, size};
		return cur_offset;
	}

	bool is_temp_var(const string &ident) {
		return ident.substr(0, 5) == "~temp";
	}

	// Below are fields for register allocation
	// Currenty we map local vars & global vars to callee-saved registers, and
	// map temp vars to caller-saved registers
	const vector<string> all_callee_saved_regs = {
		"s0", "s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9", "s10", "s11"
	};
	const vector<string> all_caller_saved_regs = {
		"ra", "a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7", "t0", "t1", "t2", "t3", "t4", "t5", "t6"
	};
	inline bool is_callee_saved_reg(const string &s) { return s[0] == 'r' || s[0] == 's'; }

	Counter cur_timestamp_cnter;	// For LRU

	std::unordered_map<string, string> reg2var;
	std::unordered_set<string> used_callee_saved_regs;

	// Simply load a variable to a register
	void load_var_to_reg_simple(const string &ident, const string &target_reg_string) {
		const char* target_reg = target_reg_string.c_str();
		if (ident == "0") {
			PUSH_ASM("  li %s, 0", target_reg);
			return;
		}
		VarMeta &meta = var_meta_db[ident];
		if (meta.type == var_t::GLOBAL_VAR) {
			// A global variable
			PUSH_ASM("  la %s, %s", target_reg, meta.global_ident.c_str());
			PUSH_ASM("  lw %s, 0(%s)", target_reg, target_reg);
		} else if (meta.type == var_t::GLOBAL_ARR_ADDR) {
			// The address of a global array
			PUSH_ASM("  la %s, %s", target_reg, meta.global_ident.c_str());
		} else if (meta.type == var_t::LOCAL_VAR) {
			// A local variable
			VarMeta& meta = var_meta_db[ident];
			if (-2048 <= -meta.offset*4 && -meta.offset*4 < 2048) {
				PUSH_ASM("  lw %s, %d(sp)", target_reg, -meta.offset*4);
			} else {
				PUSH_ASM("  li %s, %d", target_reg, -meta.offset*4);
				PUSH_ASM("  add %s, sp, %s", target_reg, target_reg);
				PUSH_ASM("  lw %s, 0(%s)", target_reg, target_reg);
			}
		} else {
			// Variable not found
			my_assert(32, false);
		}
	}

	// Simply store a variable from a register
	// If a temp register is needed but not provided, return false
	bool store_var_from_reg_simple_helper(const string &ident, const string &reg, const optional<string> &temp_reg_string = nullopt) {
		const char* temp_reg = temp_reg_string.has_value() ? temp_reg_string.value().c_str() : nullptr;
		VarMeta& meta = var_meta_db[ident];
		if (meta.type == var_t::GLOBAL_VAR) {
			// A global variable
			if (temp_reg == nullptr) return false;
			PUSH_ASM("  la %s, %s", temp_reg, ident.substr(1).c_str());
			PUSH_ASM("  sw %s, 0(%s)", reg.c_str(), temp_reg);
			return true;
		} else if (meta.type == var_t::GLOBAL_ARR_ADDR) {
			// A global array address. This should never happen
			assert(0);
		} else if (var_meta_db.count(ident)) {
			// A local variable
			if (-2048 <= -meta.offset*4 && -meta.offset*4 < 2048) {
				PUSH_ASM("  sw %s, %d(sp)", reg.c_str(), -meta.offset*4);
			} else {
				if (temp_reg == nullptr) return false;
				PUSH_ASM("  li %s, %d", temp_reg, -meta.offset*4);
				PUSH_ASM("  add %s, sp, %s", temp_reg, temp_reg);
				PUSH_ASM("  sw %s, 0(%s)", reg.c_str(), temp_reg);
			}
			return true;
		} else {
			// Variable not found
			my_assert(32, false);
		}
	}
	void store_var_from_reg_simple(const string &ident, const string &reg) {
		my_assert(34, ident != "0");
		bool is_success = store_var_from_reg_simple_helper(ident, reg);
		if (!is_success) {
			// Must provide a temp register
			string temp_reg = get_one_free_reg(true);
			is_success = store_var_from_reg_simple_helper(ident, reg, temp_reg);
			assert(is_success);
		}
	}

	// Evict a register and clear relevant data
	void evict_reg(const string &reg) {
		assert(reg2var.count(reg));
		string &ident = reg2var[reg];
		VarMeta &meta = var_meta_db[ident];
		if (meta.is_dirty) {
			// Write back
			store_var_from_reg_simple(ident, reg);
		}
		assert(meta.corresp_reg.value() == reg);
		meta.corresp_reg = nullopt;
		meta.is_dirty = false;
		reg2var.erase(reg);
	}

	// Get one free register. Evict one if all registers are occupied
	string get_one_free_reg(
		bool is_caller_saved,
		const std::optional<string> &hold_reg1 = std::nullopt,
		const std::optional<string> &hold_reg2 = std::nullopt
	) {
		const vector<string> &candidates = is_caller_saved ? all_caller_saved_regs : all_callee_saved_regs;
		optional<string> result_reg = nullopt;
		for (const string &reg : candidates) {
			// NOTE Under some scenarios, we may want to hold some registers even
			// they are free. Examples including returning from function calls
			if (hold_reg1.has_value() && hold_reg1.value() == reg) continue;
			if (hold_reg2.has_value() && hold_reg2.value() == reg) continue;
			if (!reg2var.count(reg)) {
				result_reg = reg;
				break;
			}
		}
		if (!result_reg.has_value()) {
			// Must evict one register
			int least_lru = INT_MAX;
			for (const string &reg : candidates) {
				if (hold_reg1.has_value() && hold_reg1.value() == reg) continue;
				if (hold_reg2.has_value() && hold_reg2.value() == reg) continue;
				string &ident = reg2var[reg];
				VarMeta &meta = var_meta_db[ident];
				if (meta.last_use_timestamp < least_lru) {
					result_reg = reg;
					least_lru = meta.last_use_timestamp;
				}
			}
			// Evict `result_reg`
			evict_reg(result_reg.value());
		}
		if (!is_caller_saved) {
			// If the register is callee-saved, we should mark it as used
			used_callee_saved_regs.insert(result_reg.value());
		}
		return result_reg.value();
	}

public:
	int get_cur_num_stack_elems() {
		return cur_num_stack_elems;
	}

	vector<string> get_used_callee_saved_regs() {
		vector<string> res;
		for (const string &reg : used_callee_saved_regs) {
			res.push_back(reg);
		}
		return res;
	}

	void manually_add_used_callee_saved_regs(const string &reg) {
		// For adding "ra" into the list
		used_callee_saved_regs.insert(reg);
	}

	void manually_evict_reg(const string &reg) {
		// For pushing function params
		if (reg2var.count(reg)) {
			evict_reg(reg);
		}
	}

	// Called when entering a function
	void on_enter_func() {
		temp_var_counter.reset();
		cur_num_stack_elems = all_callee_saved_regs.size()+1;	// Allocate space for callee-saved regs. +1 for ra
		free_stack_offsets.clear();
		var_meta_db.clear();
		local_arr_meta_db.clear();

		cur_timestamp_cnter.reset();
		used_callee_saved_regs.clear();
		reg2var.clear();
	}

	// Called when exiting from a blocktemp 
	// Ignore `ignore_reg` if provided (useful for generating branching instructions)
	void on_exit_block(const optional<string> &ignore_reg = nullopt) {
		// Evict all vars
		for (const string &reg : all_caller_saved_regs) {
			if (reg2var.count(reg) && (!ignore_reg.has_value() || reg != ignore_reg.value())) {
				evict_reg(reg);
			}
		}
		for (const string &reg : all_callee_saved_regs) {
			if (reg2var.count(reg) && (!ignore_reg.has_value() || reg != ignore_reg.value())){
				evict_reg(reg);
			}
		}
	}

	// Register function parameters
	void register_param(const KIRT::FuncFParam &param, int kth) {
		string ident;
		if (param.type.is_int()) {
			// An integer parameter
			ident = param.ident;
			_register_var(param.ident, var_t::LOCAL_VAR);
		} else if (param.type.is_arr()) {
			// An array parameter
			ident = param.ident + "#addr";
			_register_var(ident, var_t::LOCAL_VAR);
		} else {
			assert(0);
		}
		// Copy the argument to my stack frame
		int offset = var_meta_db[ident].offset;
		if (kth <= 7) {
			// We can safely assume offset*4 <= 2047 here since
			// - This func arg  is the first 8 func args
			// - We always first register the first 8 func args
			my_assert(34, offset*4 <= 2047);
			PUSH_ASM("  sw a%d, %d(sp)", kth, -offset*4);
			// TODO Indeed now this param is displayed by register aX. Write
			// that info into the library
		} else {
			manually_evict_reg("t1");
			PUSH_ASM("  lw t1, %d(sp)", 4*(kth-8));
			PUSH_ASM("  sw t1, %d(sp)", -offset*4);
		}
	}

	// Register a local variable
	void register_local_var(const KIRT::FuncLocalVar &local_var) {
		_register_var(local_var.ident, var_t::LOCAL_VAR);
	}

	// Register a global variable / array
	void register_global_var_or_arr(const KIRT::GlobalDecl &global_decl) {
		if (global_decl.type.is_int()) {
			_register_var(global_decl.ident, var_t::GLOBAL_VAR);
		} else if (global_decl.type.is_arr()) {
			_register_var(global_decl.ident+"#addr", var_t::GLOBAL_ARR_ADDR);
		} else {
			assert(0);
		}
	}

	// Register a temp variable
	// Return the virtual variable name
	string register_temp_var() {
		string ident = format("~temp%d", temp_var_counter.next());
		_register_var(ident, var_t::LOCAL_VAR);
		return ident;
	}

	// Invalidate all caller-saved registers and all global vars
	void on_function_call() {
		for (const string &reg : all_caller_saved_regs) {
			if (reg2var.count(reg)) {
				evict_reg(reg);
			}
		}
		for (const string &reg : all_callee_saved_regs) {
			if (reg2var.count(reg)) {
				VarMeta &meta = var_meta_db[reg2var[reg]];
				if (meta.type == var_t::GLOBAL_VAR) {
					evict_reg(reg);
				}
			}
		}
	}

	// Release a variable if it is a temp var (i.e. it won't be used in the future)
	void release_var_if_temp(const string &ident) {
		if (ident.substr(0, 5) == "~temp") {
			// Is a temp var
			VarMeta &meta = var_meta_db[ident];
			if (meta.corresp_reg.has_value()) {
				string reg = meta.corresp_reg.value();
				reg2var.erase(reg);
			}
			free_stack_offsets.insert(meta.offset);
			var_meta_db.erase(ident);
		}
	}

	// Allocate a register for a variable and build their connection
	string stage_var(
		const string &ident,
		const std::optional<string> &hold_reg1 = std::nullopt,
		const std::optional<string> &hold_reg2 = std::nullopt
	) {
		if (ident == "0") {
			return "x0";
		}
		assert (var_meta_db.count(ident));
		VarMeta &meta = var_meta_db[ident];
		if (!meta.corresp_reg.has_value()) {
			// Need to allocate a register
			string target = get_one_free_reg(is_temp_var(ident), hold_reg1, hold_reg2);
			meta.corresp_reg = target;
			meta.is_dirty = false;
			reg2var[target] = ident;
		}
		meta.last_use_timestamp = cur_timestamp_cnter.next();
		return meta.corresp_reg.value();
	}

	// Stage + load the value
	// If the variable has already been staged, skip it
	string stage_and_load_var(
		const string &ident,
		const std::optional<string> &hold_reg1 = std::nullopt,
		const std::optional<string> &hold_reg2 = std::nullopt
	) {
		if (ident == "0") {
			return "x0";
		}
		assert (var_meta_db.count(ident));
		VarMeta &meta = var_meta_db[ident];
		if (!meta.corresp_reg.has_value()) {
			string target = stage_var(ident, hold_reg1, hold_reg2);
			load_var_to_reg_simple(ident, target);
		}
		return meta.corresp_reg.value();
	}

	void on_store(const string &ident) {
		assert(var_meta_db.count(ident));
		VarMeta &meta = var_meta_db[ident];
		meta.is_dirty = true;
		meta.last_use_timestamp = cur_timestamp_cnter.next();
		assert(meta.corresp_reg.has_value());
	}

	// Register a local array
	// TODO Maybe we should register all vars first to avoid using indirect
	// address to load/store variables
	void register_local_arr(const KIRT::FuncLocalVar &local_arr) {
		string arr_addr_var_ident = local_arr.ident + "#addr";
		_register_var(arr_addr_var_ident, var_t::LOCAL_VAR);
		int offset = _register_local_arr(local_arr.ident, local_arr.type.numel());
		// Write down the address of the array
		string arr_addr_reg = stage_var(arr_addr_var_ident);
		PUSH_ASM("  li %s, %d", arr_addr_reg.c_str(), -offset*4);
		PUSH_ASM("  add %s, sp, %s", arr_addr_reg.c_str(), arr_addr_reg.c_str());
		on_store(arr_addr_var_ident);
	}

	// A small helper function for loading the address of an array to a register
	string load_arr_addr_to_reg(
		const string &arr_ident,
		const std::optional<string> &hold_reg1 = std::nullopt,
		const std::optional<string> &hold_reg2 = std::nullopt
	) {
		string arr_addr_var_ident = arr_ident + "#addr";
		string arr_addr_reg = stage_and_load_var(arr_addr_var_ident, hold_reg1, hold_reg2);
		return arr_addr_reg;
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

// Some global status
string cur_func_name;	// Current function name
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
	PUSH_ASM("<SAVE_REGISTERS>");

	var_manager.on_enter_func();
	// ra is different from other caller-saved registers since we must restore it
	var_manager.manually_add_used_callee_saved_regs("ra");

	// Arguments registration
	for (int i = 0; i < func.fparams.size(); i++) {
		const KIRT::FuncFParam &param = func.fparams[i];
		var_manager.register_param(param, i);
	}

	// Global variable registration
	for (const auto &[ident, global_decl] : KIRT::global_decl_map) {
		var_manager.register_global_var_or_arr(*global_decl);
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

	for (const std::shared_ptr<KIRT::Block> &block : func.blocks.blocks) {
		kirt2asm(*block);
	}

	// Construct & replace prologue and epilogue
	{
		vector<string> callee_saved_regs = var_manager.get_used_callee_saved_regs();
		list<string> prologue, epilogue;
		for (int i = 0; i < (int)callee_saved_regs.size(); i++) {
			prologue.push_back(format("  sw %s, %d(sp)", callee_saved_regs[i].c_str(), -4*(i+1)));
			epilogue.push_back(format("  lw %s, %d(sp)", callee_saved_regs[i].c_str(), -4*(i+1)));
		}
		auto iter = cur_func_asm.begin();
		// Replace all "<SAVE_REGISTERS>" with prologue
		// and "<EPILOGUE>" with epilogue
		while (iter != cur_func_asm.end()) {
			if (*iter == "<SAVE_REGISTERS>") {
				iter = cur_func_asm.erase(iter);
				cur_func_asm.insert(iter, prologue.begin(), prologue.end());
			} else if (*iter == "<EPILOGUE>") {
				iter = cur_func_asm.erase(iter);
				cur_func_asm.insert(iter, epilogue.begin(), epilogue.end());
			} else {
				iter = std::next(iter);
			}
		}
	}

	return cur_func_asm;
}

void kirt2asm(const KIRT::Block &block) {
	if (!cur_func_asm.empty() && cur_func_asm.back() == "  j " + rename_block(block.name)) {
		// Peephole optimization: remove the last jump instruction
		cur_func_asm.pop_back();
	}
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
			string exp_reg = var_manager.stage_and_load_var(exp_var_ident);
			string lval_reg = var_manager.stage_var(assign_inst->lval.ident, exp_reg);
			if (lval_reg != exp_reg) {
				PUSH_ASM("  mv %s, %s", lval_reg.c_str(), exp_reg.c_str());
			}
			var_manager.on_store(assign_inst->lval.ident);
		} else if (assign_inst->lval.is_arr()) {
			assert(assign_inst->lval.type.dims() == 1);
			string index_var_ident = kirt2asm(*assign_inst->lval.indices[0]);
			string index_reg = var_manager.stage_and_load_var(index_var_ident);

			string arr_addr_reg = var_manager.load_arr_addr_to_reg(assign_inst->lval.ident, index_reg);

			string item_addr_temp_var = var_manager.register_temp_var();
			string item_addr_reg = var_manager.stage_var(item_addr_temp_var, index_reg, arr_addr_reg);
			
			PUSH_ASM("  add %s, %s, %s", item_addr_reg.c_str(), arr_addr_reg.c_str(), index_reg.c_str());
			var_manager.on_store(item_addr_temp_var);

			string exp_reg = var_manager.stage_and_load_var(exp_var_ident, item_addr_reg);
			PUSH_ASM("  sw %s, 0(%s)", exp_reg.c_str(), item_addr_reg.c_str());

			var_manager.release_var_if_temp(index_var_ident);
			var_manager.release_var_if_temp(item_addr_temp_var);
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
			string exp_reg = var_manager.stage_and_load_var(exp_var_ident, std::nullopt);
			if (exp_reg != "a0")
				PUSH_ASM("  mv a0, %s", exp_reg.c_str());
			var_manager.release_var_if_temp(exp_var_ident);
		}
		var_manager.on_exit_block();
		PUSH_ASM("<EPILOGUE>");	// Will be filled later
		PUSH_ASM("  ret");
	} else if (const KIRT::JumpInst *jump_inst = dynamic_cast<const KIRT::JumpInst *>(inst.get())) {
		var_manager.on_exit_block();
		PUSH_ASM(
			"  j %s",
			rename_block(jump_inst->target_block->name).c_str()
		);
	} else if (const KIRT::BranchInst *branch_inst = dynamic_cast<const KIRT::BranchInst *>(inst.get())) {
		string cond_var_ident = kirt2asm(branch_inst->cond);
		string cond_reg = var_manager.stage_and_load_var(cond_var_ident, std::nullopt);

		// TODO Now we first store the value of the instruction into a reg, then use
		// bnez to jump. This can be optimized
		// TODO Now we always jump to the false block, via `j %s`. In the future we may
		// get rid of this jump via BlockReorderPass
		var_manager.on_exit_block(cond_reg);
		PUSH_ASM(
			"  bnez %s, %s",
			cond_reg.c_str(),
			rename_block(branch_inst->true_block->name).c_str()
		);
		var_manager.release_var_if_temp(cond_var_ident);
		PUSH_ASM(
			"  j %s",
			rename_block(branch_inst->false_block->name).c_str()
		);
	} else {
		my_assert(19, 0);
	}
}

// Return: the (possibly temp) variable name
string kirt2asm(const KIRT::Exp &exp) {
	if (exp.type == KIRT::exp_t::LVAL) {
		if (exp.lval.is_int()) {
			string ident = exp.lval.ident;
			return ident;
		} else if (exp.lval.is_arr()) {
			assert(exp.lval.type.dims() == 1);
			string index_var_ident = kirt2asm(*exp.lval.indices[0]);
			string index_reg = var_manager.stage_and_load_var(index_var_ident);

			string arr_addr_reg = var_manager.load_arr_addr_to_reg(exp.lval.ident, index_reg);

			string res_virt_ident = var_manager.register_temp_var();
			string res_reg = var_manager.stage_var(res_virt_ident, index_reg, arr_addr_reg);
			PUSH_ASM("  add %s, %s, %s", res_reg.c_str(), index_reg.c_str(), arr_addr_reg.c_str());
			PUSH_ASM("  lw %s, 0(%s)", res_reg.c_str(), res_reg.c_str());
			var_manager.on_store(res_virt_ident);

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
		int num_stack_elems = var_manager.get_cur_num_stack_elems();
		int stack_frame_len = num_stack_elems*4; 

		// Place args
		// After evicting these two regs, `arg_virt_ident` must be placed here
		var_manager.manually_evict_reg("s0");
		var_manager.manually_evict_reg("ra");
		// Evict this for temp offset calculation
		var_manager.manually_evict_reg("t0");
		string arg_addr_temp_reg = "t0";
		for (int i = (int)exp.args.size()-1; i >= 0; i--) {
			// We perform this backwards to avoid overwriting the aX registers
			string arg_virt_ident = arg_virt_idents[i];
			string arg_reg = var_manager.stage_and_load_var(arg_virt_ident);
			if (i <= 7) {
				// Save the argument to the register
				string target = format("a%d", i);
				if (arg_reg != target) {
					var_manager.manually_evict_reg(target);
					PUSH_ASM(
						"  mv %s, %s",
						target.c_str(),
						arg_reg.c_str()
					);
				}
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
			var_manager.manually_evict_reg(arg_reg);
		}

		// Save vars (evict all caller-saved regs and global vars)
		var_manager.on_function_call();

		if (stack_frame_len > 2048) {
			// Here using t0 is fine since all caller-saved regs have been invalidated
			PUSH_ASM("  li t0, %d", stack_frame_len);
			PUSH_ASM("  sub sp, sp, t0");
		} else {
			PUSH_ASM("  addi sp, sp, -%d", stack_frame_len);
		}

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
		if (stack_frame_len > 2047) {
			PUSH_ASM("  li t0, %d", stack_frame_len);
			PUSH_ASM("  add sp, sp, t0");
		} else {
			PUSH_ASM("  addi sp, sp, %d", stack_frame_len);
		}

		// Save the return value
		string res_virt_ident = var_manager.register_temp_var();
		string res_reg = var_manager.stage_var(res_virt_ident);
		if (res_reg != "a0")
			PUSH_ASM("  mv %s, a0", res_reg.c_str());
		var_manager.on_store(res_virt_ident);
		return res_virt_ident;
	} else if (exp.type == KIRT::exp_t::ARR_ADDR) {
		return exp.arr_name+"#addr";
	} else if (exp.type == KIRT::exp_t::NUMBER) {
		if (exp.number == 0) {
			return "0";
		}
		string res_virt_ident = var_manager.register_temp_var();
		string res_reg = var_manager.stage_var(res_virt_ident);
		PUSH_ASM("  li %s, %d", res_reg.c_str(), exp.number);
		var_manager.on_store(res_virt_ident);
		return res_virt_ident;
	} else if (exp.type == KIRT::exp_t::EQ0 || exp.type == KIRT::exp_t::NEQ0) {
		string lhs_virt_ident = kirt2asm(*exp.lhs);
		string lhs_reg = var_manager.stage_and_load_var(lhs_virt_ident);
		string res_virt_ident = var_manager.register_temp_var();
		string res_reg = var_manager.stage_var(res_virt_ident, lhs_reg);

		PUSH_ASM(
			"  %s %s, %s",
			exp.type == KIRT::exp_t::EQ0 ? "seqz" : "snez",
			res_reg.c_str(),
			lhs_reg.c_str()
		);
		var_manager.on_store(res_virt_ident);
		var_manager.release_var_if_temp(lhs_virt_ident);
		return res_virt_ident;
	} else {
		auto lhs_virt_ident = kirt2asm(*exp.lhs);
		auto rhs_virt_ident = kirt2asm(*exp.rhs);
		string lhs_reg = var_manager.stage_and_load_var(lhs_virt_ident);
		string rhs_reg = var_manager.stage_and_load_var(rhs_virt_ident, lhs_reg);
		string res_virt_ident = var_manager.register_temp_var();
		string res_reg = var_manager.stage_var(res_virt_ident, lhs_reg, rhs_reg);

		PUSH_ASM(
			"  %s %s, %s, %s",
			kirt_exp_t2asm(exp.type).c_str(),
			res_reg.c_str(),
			lhs_reg.c_str(),
			rhs_reg.c_str()
		);
		var_manager.on_store(res_virt_ident);
		var_manager.release_var_if_temp(lhs_virt_ident);
		var_manager.release_var_if_temp(rhs_virt_ident);
		return res_virt_ident;
	}
}

}