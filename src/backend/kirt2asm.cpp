#include "backend/kirt2asm.h"

#include <algorithm>
#include <cassert>
#include <climits>
#include <map>
#include <optional>
#include <set>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "utils/utils.h"
#include "optim/optim_dbs.h"

namespace ASM {

using std::list;
using std::pair;
using std::string;
using std::shared_ptr;
using std::optional, std::nullopt;
using std::vector;

// Can we use an immediate value in a binary operation?
inline bool is_binary_op_immediateable(KIRT::exp_t exp_type) {
	switch (exp_type) {
		case KIRT::exp_t::ADDR_ADD:
		case KIRT::exp_t::ADD:
		case KIRT::exp_t::BITWISE_AND:
		case KIRT::exp_t::BITWISE_OR:
		case KIRT::exp_t::BITWISE_XOR:
		case KIRT::exp_t::SHL:
		case KIRT::exp_t::SHR:
		case KIRT::exp_t::SAR:
		case KIRT::exp_t::LT:
			return true;

		case KIRT::exp_t::MUL:
		case KIRT::exp_t::DIV:
		case KIRT::exp_t::REM:
		case KIRT::exp_t::GT:
		case KIRT::exp_t::LEQ:
		case KIRT::exp_t::GEQ:
		case KIRT::exp_t::EQ:
		case KIRT::exp_t::NEQ:
		case KIRT::exp_t::SUB:	// Should be handled separately by using `addi -X`
			return false;
			
		default:
			assert(0);
	}
}

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
		// When pin_mode is activated, it means whether 1) The var is a global
		// var 2) The var has been swapped.
		int last_use_timestamp;
	};

	struct ArrayMeta {
		int space_offset;	// Offset to the space allocated for the array
		int addr_var_offset;
		int size;	// Size of the array, in words
	};

	struct ParamMeta {
		int var_offset;
		int kth;
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

	int num_pinnable_vars;
	bool is_pin_mode;
	std::unordered_map<string, ParamMeta> param_db;

	void _register_var(const string &ident, var_t type) {
		num_pinnable_vars += 1;
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

	int _register_local_arr(const string &ident, int size, int addr_var_offset) {
		int cur_offset = cur_num_stack_elems+size;
		cur_num_stack_elems += size;
		local_arr_meta_db[ident] = {cur_offset, size, addr_var_offset};
		return cur_offset;
	}

	bool is_temp_var(const string &ident) {
		return ident.substr(0, 5) == "~temp";
	}

	// Below are fields for register allocation
	// Currenty we map local vars & global vars to callee-saved registers, and
	// map temp vars to caller-saved registers
	// 
	// Here we mark t3 ~ t6 as callee-saved regs to enable better pinning. I'm
	// sure that lib funcs does not modify them, so it's safe to make such an assumption
	// 
	// If the function is not a leaf function, use callee saved regs for local vars
	// and caller saved regs for temp vars
	// 
	// If the function is a leaf function, use all regs for all vars
	const vector<string> all_callee_saved_regs = {
		"s0", "s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9", "s10", "s11", "t3", "t4", "t5", "t6"
	};
	const vector<string> all_caller_saved_regs = {
		"t0", "t1", "t2", "ra", "a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7"
	};
	const vector<string> leaf_func_local_var_regs = {
		// Put caller_saved regs first to avoid the expensive load-store for
		// leaf functions
		"a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7", "ra",
		"s0", "s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9", "t3", "t4", "t5", "t6"
	};
	const vector<string> leaf_func_temp_var_regs = {
		"t0", "t1", "t2", "s10", "s11"
	};

	bool is_leaf_func;

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
		if (!var_meta_db.count(ident)) {
			fprintf(stderr, "Variable `%s` not found\n", ident.c_str());
			my_assert(38, var_meta_db.count(ident));
		}
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
	void store_var_from_reg_simple(
		const string &ident,
		const string &reg,
		const std::optional<string> &hold_reg1 = std::nullopt,
		const std::optional<string> &hold_reg2 = std::nullopt
	) {
		my_assert(34, ident != "0");
		bool is_success = store_var_from_reg_simple_helper(ident, reg);
		if (!is_success) {
			// Must provide a temp register
			string temp_reg = get_one_free_reg(
				is_leaf_func ? leaf_func_temp_var_regs : all_caller_saved_regs,
				hold_reg1,
				hold_reg2
			);
			is_success = store_var_from_reg_simple_helper(ident, reg, temp_reg);
			assert(is_success);
		}
	}

	// Evict a register and clear relevant data
	void evict_reg(
		const string &reg,
		const std::optional<string> &hold_reg1 = std::nullopt,
		const std::optional<string> &hold_reg2 = std::nullopt
	) {
		if (is_pin_mode) {
			// We should never evict local-var backended regs in pin mode
			for (auto &[ident, meta] : var_meta_db) {
				if (meta.corresp_reg.has_value() && meta.corresp_reg.value() == reg) {
					if (meta.type == var_t::LOCAL_VAR && !is_temp_var(ident)) {
						fprintf(stderr, "Variable `%s` is pinned to register `%s`\n", ident.c_str(), reg.c_str());
						my_assert(37, false);
					}
				}
			}
		}
		assert(reg2var.count(reg));
		string &ident = reg2var[reg];
		VarMeta &meta = var_meta_db[ident];
		if (meta.is_dirty) {
			// Write back
			store_var_from_reg_simple(ident, reg, hold_reg1, hold_reg2);
		}
		assert(meta.corresp_reg.value() == reg);
		meta.corresp_reg = nullopt;
		meta.is_dirty = false;
		reg2var.erase(reg);
	}

	void add_used_reg(const string &reg) {
		if (std::count(all_callee_saved_regs.begin(), all_callee_saved_regs.end(), reg) || reg == "ra") {
			// If the register is callee-saved, we should mark it as used
			used_callee_saved_regs.insert(reg);
		}
	}

	// Get one free register. Evict one if all registers are occupied
	string get_one_free_reg(
		const vector<string> &candidates,
		const std::optional<string> &hold_reg1 = std::nullopt,
		const std::optional<string> &hold_reg2 = std::nullopt,
		std::optional<string> preference = std::nullopt
	) {
		optional<string> result_reg = nullopt;
		if ((preference && hold_reg1 && preference.value() == hold_reg1.value()) || 
			(preference && hold_reg2 && preference.value() == hold_reg2.value()))
			// `preference` is held by `hold_reg1` or `hold_reg2`. Ignore it
			preference = nullopt;

		if (preference && !reg2var.count(preference.value())) {
			// The preferred register is free
			result_reg = preference;
		} else {
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
				if (preference && std::count(candidates.begin(), candidates.end(), preference.value())) {
					result_reg = preference;
				} else {
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
				}
				// Evict `result_reg`
				evict_reg(result_reg.value());
			}
		}
		add_used_reg(result_reg.value());
		return result_reg.value();
	}

public:
	int get_cur_num_stack_elems() {
		return cur_num_stack_elems;
	}

	bool get_is_pin_mode() {
		return is_pin_mode;
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
		add_used_reg(reg);
	}

	void manually_evict_reg(const string &reg) {
		// For pushing function params
		if (reg2var.count(reg)) {
			evict_reg(reg);
		}
	}

	void clear_reg_a0_before_return() {
		if (!reg2var.count("a0")) return;
		string ident = reg2var["a0"];
		VarMeta &meta = var_meta_db[ident];
		if (meta.type == var_t::GLOBAL_VAR) {
			store_var_from_reg_simple(ident, "a0");
			meta.corresp_reg = nullopt;
			meta.is_dirty = false;
			meta.last_use_timestamp = 0;
		}
		// We do not need to write back local vars since we are going to return
		reg2var.erase("a0");
	}

	// Called when entering a function
	void on_enter_func(bool is_leaf_func) {
		this->is_leaf_func = is_leaf_func;

		temp_var_counter.reset();
		cur_num_stack_elems = all_callee_saved_regs.size()+2;	// Allocate space for callee-saved regs. +1 for ra
		free_stack_offsets.clear();
		var_meta_db.clear();
		local_arr_meta_db.clear();

		num_pinnable_vars = 0;
		is_pin_mode = false;
		param_db.clear();

		cur_timestamp_cnter.reset();
		used_callee_saved_regs.clear();
		reg2var.clear();

		if (!this->is_leaf_func) {
			// ra is different from other caller-saved registers since we must restore it
			manually_add_used_callee_saved_regs("ra");
		}
	}

	// Called when exiting from a blocktemp 
	// Specify `branch_reg` if you don't want a variable to be touched (and not to
	// be stored back for temp vars) when exiting the block
	void on_exit_block(const optional<string> &branch_reg1 = nullopt, const optional<string> &branch_reg2 = nullopt) {
		auto is_branch_reg = [&](const string &reg) -> bool {
			return (branch_reg1.has_value() && reg == branch_reg1.value()) ||
				(branch_reg2.has_value() && reg == branch_reg2.value());
		};
		if (is_pin_mode) {
			// Pin mode. Evict temp vars and mark global vars
			for (auto &[ident, meta] : var_meta_db) {
				if (meta.corresp_reg.has_value()) {
					if (meta.type == var_t::LOCAL_VAR && is_temp_var(ident)) {
						if (!is_branch_reg(meta.corresp_reg.value()))
							evict_reg(meta.corresp_reg.value());
					} else if (meta.type == var_t::GLOBAL_VAR) {
						// Pin mode. Just write back without evicting
						if (meta.is_dirty) {
							store_var_from_reg_simple("@" + meta.global_ident, meta.corresp_reg.value());
							meta.is_dirty = false;
						}
						// Mark it as obsolete
						meta.last_use_timestamp = 1;
					}
				}
			}
		} else {
			// Evict all vars
			vector<string> all_regs = all_callee_saved_regs;
			all_regs.insert(all_regs.end(), all_caller_saved_regs.begin(), all_caller_saved_regs.end());
			for (const string &reg : all_regs) {
				if (reg2var.count(reg)) {
					VarMeta &meta = var_meta_db[reg2var[reg]];
					if (meta.type == var_t::LOCAL_VAR && is_temp_var(reg2var[reg]) &&
						is_branch_reg(reg))
						continue;
					evict_reg(reg, branch_reg1, branch_reg2);
				}
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
		param_db[ident] = {offset, kth};
	}

	// Register a local variable
	void register_local_var(const KIRT::FuncLocalVar &local_var) {
		_register_var(local_var.ident, var_t::LOCAL_VAR);
	}

	// Register a local array
	// TODO Maybe we should register all vars first to avoid using indirect
	// address to load/store variables
	void register_local_arr(const KIRT::FuncLocalVar &local_arr) {
		string arr_addr_var_ident = local_arr.ident + "#addr";
		_register_var(arr_addr_var_ident, var_t::LOCAL_VAR);
		int addr_var_offset = var_meta_db[arr_addr_var_ident].offset;
		_register_local_arr(local_arr.ident, local_arr.type.numel(), addr_var_offset);
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

	void on_all_register_done(const string &func_name) {
		const vector<string> &pin_slot_reg = is_leaf_func ? leaf_func_local_var_regs : all_callee_saved_regs;
		is_pin_mode = num_pinnable_vars <= (int)pin_slot_reg.size();
		fprintf(stderr, "`%s`: Number of local variables: %d. %s pin mode. %s\n",
			func_name.c_str(), num_pinnable_vars, is_pin_mode ? "Using" : "Not using",
			is_leaf_func ? "(leaf)" : "");
		if (is_pin_mode) {
			// Allocate & load regs for all pinnable vars
			std::vector<string> remaining_regs = pin_slot_reg;

			// Load func params first, or aX will be overwritten
			for (auto &[ident, meta] : var_meta_db) {
				if (is_temp_var(ident)) continue;
				if (meta.type == var_t::LOCAL_VAR && param_db.count(ident)) {
					// A parameter. If its kth >= 8 we need to load it from the stack
					ParamMeta &param_meta = param_db[ident];
					string reg;
					if (param_meta.kth <= 7 && is_leaf_func)
						reg = format("a%d", param_meta.kth);
					else {
						for (auto iter = remaining_regs.begin(); iter != remaining_regs.end(); ) {
							if ((*iter)[0] != 'a') {
								reg = *iter;
								break;
							} else {
								++iter;
							}
						}
					}
					remaining_regs.erase(std::find(remaining_regs.begin(), remaining_regs.end(), reg));

					meta.corresp_reg = reg;
					meta.is_dirty = false;
					meta.last_use_timestamp = 0;
					reg2var[reg] = ident;

					if (param_meta.kth >= 8) {
						PUSH_ASM("  lw %s, %d(sp)", reg.c_str(), 4*(param_meta.kth-8));
					} else {
						string src_reg = format("a%d", param_meta.kth);
						if (src_reg != reg) {
							PUSH_ASM("  mv %s, %s", reg.c_str(), src_reg.c_str());
						}	
					}

					add_used_reg(reg);
				}
			}

			for (auto &[ident, meta] : var_meta_db) {
				if (is_temp_var(ident)) continue;
				if (meta.type == var_t::LOCAL_VAR && param_db.count(ident))
					continue;
				string reg = *remaining_regs.begin();
				remaining_regs.erase(std::find(remaining_regs.begin(), remaining_regs.end(), reg));

				meta.corresp_reg = reg;
				meta.is_dirty = false;
				meta.last_use_timestamp = 0;
				reg2var[reg] = ident;

				if (meta.type == var_t::LOCAL_VAR && local_arr_meta_db.count(ident.substr(0, ident.size()-5))) {
					const ArrayMeta &arr_meta = local_arr_meta_db[ident.substr(0, ident.size()-5)];
					PUSH_ASM("  li %s, %d", reg.c_str(), -arr_meta.space_offset*4);
					PUSH_ASM("  add %s, sp, %s", reg.c_str(), reg.c_str());
				} else if (meta.type == var_t::LOCAL_VAR && !param_db.count(ident)) {
					// A local variable. No need to load
				} else {
					load_var_to_reg_simple(ident, reg);
				}
				add_used_reg(reg);
			}
		} else {
			for (const auto &[ident, meta] : local_arr_meta_db) {
				string arr_addr_reg = stage_var(ident + "#addr");
				PUSH_ASM("  li %s, %d", arr_addr_reg.c_str(), -meta.space_offset*4);
				PUSH_ASM("  add %s, sp, %s", arr_addr_reg.c_str(), arr_addr_reg.c_str());
				on_store(ident + "#addr");
			}
			for (auto &[ident, meta] : param_db) {
				int kth = meta.kth;
				int offset = meta.var_offset;
				if (kth <= 7) {
					// We can safely assume offset*4 <= 2048 here since
					// - This func arg  is the first 8 func args
					// - We always first register the first 8 func args
					my_assert(34, offset*4 <= 2048);
					PUSH_ASM("  sw a%d, %d(sp)", kth, -offset*4);
					// TODO Indeed now this param is displayed by register aX. Write
					// that info into the library
				} else {
					manually_evict_reg("t1");
					PUSH_ASM("  lw t1, %d(sp)", 4*(kth-8));
					PUSH_ASM("  sw t1, %d(sp)", -offset*4);
				}
			}
		}
	}

	// Register a temp variable
	// Return the virtual variable name
	string get_new_temp_var() {
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
					if (is_pin_mode) {
						// Pin mode. Just write back without evicting
						if (meta.is_dirty) {
							store_var_from_reg_simple("@" + meta.global_ident, reg);
							meta.is_dirty = false;
						}
						// Mark it as obsolete
						meta.last_use_timestamp = 1;
					} else {
						// Non pin mode. Evict the register
						evict_reg(reg);
					}
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
	// Will put the variable into `preference` if provided and the var is not staged
	string stage_var(
		const string &ident,
		const std::optional<string> &hold_reg1 = std::nullopt,
		const std::optional<string> &hold_reg2 = std::nullopt,
		const std::optional<string> &preference = std::nullopt
	) {
		if (ident == "0") {
			return "x0";
		}
		assert (var_meta_db.count(ident));
		VarMeta &meta = var_meta_db[ident];
		if (is_pin_mode && !is_temp_var(ident)) {
			my_assert(38, meta.corresp_reg.has_value());
		} else {
			if (!meta.corresp_reg.has_value()) {
				// Need to allocate a register
				string target = get_one_free_reg(
					is_temp_var(ident) ?
					(is_leaf_func ? leaf_func_temp_var_regs : all_caller_saved_regs) :
					(is_leaf_func ? leaf_func_local_var_regs : all_callee_saved_regs),
					hold_reg1, hold_reg2,
					preference
				);
				meta.corresp_reg = target;
				meta.is_dirty = false;
				reg2var[target] = ident;
			}
			meta.last_use_timestamp = cur_timestamp_cnter.next();
		}
		string res = meta.corresp_reg.value();
		add_used_reg(res);
		return res;
	}

	// Stage + load the value
	// If the variable has already been staged, skip it
	string stage_and_load_var(
		const string &ident,
		const std::optional<string> &hold_reg1 = std::nullopt,
		const std::optional<string> &hold_reg2 = std::nullopt,
		const std::optional<string> &preference = std::nullopt
	) {
		if (ident == "0") {
			return "x0";
		}
		assert (var_meta_db.count(ident));
		VarMeta &meta = var_meta_db[ident];
		if (is_pin_mode && !is_temp_var(ident)) {
			my_assert(39, meta.corresp_reg.has_value());
			if (meta.last_use_timestamp) {
				// Must be a global var
				my_assert(38, meta.type == var_t::GLOBAL_VAR);
				load_var_to_reg_simple(ident, meta.corresp_reg.value());
				meta.last_use_timestamp = 0;
			}
			return stage_var(ident, hold_reg1, hold_reg2, preference);
		} else {
			if (!meta.corresp_reg.has_value()) {
				string target = stage_var(ident, hold_reg1, hold_reg2, preference);
				load_var_to_reg_simple(ident, target);
			}
			return meta.corresp_reg.value();
		}
	}

	void on_store(const string &ident) {
		assert(var_meta_db.count(ident));
		VarMeta &meta = var_meta_db[ident];
		meta.is_dirty = true;
		if (is_pin_mode && !is_temp_var(ident)) {
			if (meta.type == var_t::GLOBAL_VAR)
				meta.last_use_timestamp = 0;
		} else {
			meta.last_use_timestamp = cur_timestamp_cnter.next();
		}
		assert(meta.corresp_reg.has_value());
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
void kirt2asm(const KIRT::Block &cur_block, const shared_ptr<KIRT::TermInst> &inst);
string kirt2asm(const KIRT::Exp &inst, const optional<string> &res_reg_suggestion = nullopt);

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

// A tiny utility for collecting all used global variables in a function
std::unordered_set<string> cur_func_used_global_idents;
void traverse_used_global_idents(const KIRT::Exp &exp);
void traverse_used_global_idents(const KIRT::LVal &lval) {
	if (KIRT::global_decl_map.count(lval.ident)) {
		cur_func_used_global_idents.insert(lval.ident);
	}
	for (const std::shared_ptr<KIRT::Exp> &exp : lval.indices) {
		traverse_used_global_idents(*exp);
	}
}
void traverse_used_global_idents(const KIRT::Exp &exp) {
	if (exp.type == KIRT::exp_t::LVAL) {
		traverse_used_global_idents(exp.lval);
	}
	if (exp.type == KIRT::exp_t::ARR_ADDR) {
		if (KIRT::global_decl_map.count(exp.arr_name)) {
			cur_func_used_global_idents.insert(exp.arr_name);
		}
	}
	for (const std::shared_ptr<KIRT::Exp> &arg : exp.args) {
		traverse_used_global_idents(*arg);
	}
	if (exp.lhs) {
		traverse_used_global_idents(*exp.lhs);
	}
	if (exp.rhs) {
		traverse_used_global_idents(*exp.rhs);
	}
}
void traverse_used_global_idents(const KIRT::Function &func) {
	for (const auto &block : func.blocks.blocks) {
		for(const auto &inst : block->insts) {
			if (const KIRT::AssignInst *assign_inst = dynamic_cast<const KIRT::AssignInst *>(inst.get())) {
				traverse_used_global_idents(assign_inst->exp);
				traverse_used_global_idents(assign_inst->lval);
			} else if (const KIRT::ExpInst *exp_inst = dynamic_cast<const KIRT::ExpInst *>(inst.get())) {
				traverse_used_global_idents(exp_inst->exp);
			} else {
				my_assert(12, 0);
			}
		}
		if (const KIRT::BranchInst *branch_inst = dynamic_cast<const KIRT::BranchInst *>(block->term_inst.get())) {
			traverse_used_global_idents(branch_inst->cond);
		} else if (const KIRT::JumpInst *jump_inst = dynamic_cast<const KIRT::JumpInst *>(block->term_inst.get())) {
			// Do nothing
		} else if (const KIRT::ReturnInst *return_inst = dynamic_cast<const KIRT::ReturnInst *>(block->term_inst.get())) {
			if (return_inst->ret_exp) {
				traverse_used_global_idents(*return_inst->ret_exp);
			}
		} else {
			my_assert(13, 0);
		}
	}
}

list<string> kirt2asm(const KIRT::Function &func) {
	cur_func_name = func.name;
	cur_func_asm.clear();

	PUSH_ASM("");
	PUSH_ASM("  .globl %s", func.name.c_str());
	PUSH_ASM("%s:", func.name.c_str());

	// Save registers
	PUSH_ASM("<SAVE_REGISTERS>");

	bool is_leaf_func = !KIRT::func2callees.count(func.name) || KIRT::func2callees.at(func.name).empty();
	var_manager.on_enter_func(is_leaf_func);

	// Arguments registration
	for (int i = 0; i < func.fparams.size(); i++) {
		const KIRT::FuncFParam &param = func.fparams[i];
		var_manager.register_param(param, i);
	}

	// Global variable registration
	cur_func_used_global_idents.clear();
	traverse_used_global_idents(func);
	for (const string &global_ident : cur_func_used_global_idents) {
		var_manager.register_global_var_or_arr(*KIRT::global_decl_map[global_ident]);
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

	var_manager.on_all_register_done(func.name);

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

	kirt2asm(block, block.term_inst);
}

void kirt2asm(const shared_ptr<KIRT::Inst> &inst) {
	if (const KIRT::AssignInst *assign_inst = dynamic_cast<const KIRT::AssignInst *>(inst.get())) {
		if (assign_inst->lval.is_int()) {
			string exp_var_ident = kirt2asm(assign_inst->exp, assign_inst->lval.ident);
			string exp_reg = var_manager.stage_and_load_var(exp_var_ident);
			string lval_reg = var_manager.stage_var(assign_inst->lval.ident, exp_reg);
			if (lval_reg != exp_reg) {
				PUSH_ASM("  mv %s, %s", lval_reg.c_str(), exp_reg.c_str());
			}
			var_manager.on_store(assign_inst->lval.ident);
			var_manager.release_var_if_temp(exp_var_ident);
		} else if (assign_inst->lval.is_arr()) {
			string exp_var_ident = kirt2asm(assign_inst->exp);
			assert(assign_inst->lval.type.dims() == 1);
			string index_var_ident = kirt2asm(*assign_inst->lval.indices[0]);
			string index_reg = var_manager.stage_and_load_var(index_var_ident);

			string arr_addr_reg = var_manager.load_arr_addr_to_reg(assign_inst->lval.ident, index_reg);

			string item_addr_temp_var = var_manager.get_new_temp_var();
			string item_addr_reg = var_manager.stage_var(item_addr_temp_var, index_reg, arr_addr_reg);
			
			PUSH_ASM("  add %s, %s, %s", item_addr_reg.c_str(), arr_addr_reg.c_str(), index_reg.c_str());
			var_manager.release_var_if_temp(index_var_ident);
			var_manager.on_store(item_addr_temp_var);

			string exp_reg = var_manager.stage_and_load_var(exp_var_ident, item_addr_reg);
			PUSH_ASM("  sw %s, 0(%s)", exp_reg.c_str(), item_addr_reg.c_str());

			var_manager.release_var_if_temp(item_addr_temp_var);
			var_manager.release_var_if_temp(exp_var_ident);
		} else {
			my_assert(18, 0);
		}
	} else if (const KIRT::ExpInst *exp_inst = dynamic_cast<const KIRT::ExpInst *>(inst.get())) {
		string exp_var_ident = kirt2asm(exp_inst->exp);
		var_manager.release_var_if_temp(exp_var_ident);
	} else {
		my_assert(18, 0);
	}
}

void kirt2asm(const KIRT::Block &cur_block, const shared_ptr<KIRT::TermInst> &inst) {
	if (const KIRT::ReturnInst *ret_inst = dynamic_cast<const KIRT::ReturnInst *>(inst.get())) {
		if (ret_inst->ret_exp) {
			string exp_var_ident = kirt2asm(*ret_inst->ret_exp);
			string exp_reg = var_manager.stage_and_load_var(exp_var_ident);
			if (exp_reg != "a0") {
				var_manager.clear_reg_a0_before_return();
				PUSH_ASM("  mv a0, %s", exp_reg.c_str());
			}
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
		KIRT::exp_t cond_type = branch_inst->cond.type;
		
		string branch_inst_name;	// The name of the branch instruction
		string branch_inst_arg;

		if (cond_type == KIRT::exp_t::EQ0 || cond_type == KIRT::exp_t::NEQ0) {
			// Use beq0 / bne0
			string cond_var_ident = kirt2asm(*branch_inst->cond.lhs);
			string cond_reg = var_manager.stage_and_load_var(cond_var_ident);
			var_manager.on_exit_block(cond_reg);
			branch_inst_name = (cond_type == KIRT::exp_t::EQ0 ? "beqz" : "bnez"),
			branch_inst_arg = cond_reg.c_str();
			PUSH_ASM("<BRANCH_INST>");
			var_manager.release_var_if_temp(cond_var_ident);
		} else if (cond_type == KIRT::exp_t::EQ || cond_type == KIRT::exp_t::NEQ ||
			cond_type == KIRT::exp_t::LT || cond_type == KIRT::exp_t::GT ||
			cond_type == KIRT::exp_t::LEQ || cond_type == KIRT::exp_t::GEQ ) {
			// Use beq / bne / blt / bge
			string lhs_var_ident = kirt2asm(*branch_inst->cond.lhs);
			string rhs_var_ident = kirt2asm(*branch_inst->cond.rhs);
			string lhs_reg = var_manager.stage_and_load_var(lhs_var_ident);
			string rhs_reg = var_manager.stage_and_load_var(rhs_var_ident, lhs_reg);

			if (!var_manager.get_is_pin_mode()) {
				// We use this workaround to avoid `on_exit_block` from ignoring
				// local vars on block exit when is_pin_mode is false
				string lhs_temp_var_ident = var_manager.get_new_temp_var();
				string rhs_temp_var_ident = var_manager.get_new_temp_var();
				string lhs_temp_reg = var_manager.stage_var(lhs_temp_var_ident, lhs_reg, rhs_reg);
				PUSH_ASM("  mv %s, %s", lhs_temp_reg.c_str(), lhs_reg.c_str());
				string rhs_temp_reg = var_manager.stage_var(rhs_temp_var_ident, lhs_temp_reg, rhs_reg);
				PUSH_ASM("  mv %s, %s", rhs_temp_reg.c_str(), rhs_reg.c_str());

				var_manager.release_var_if_temp(lhs_var_ident);
				var_manager.release_var_if_temp(rhs_var_ident);

				lhs_var_ident = lhs_temp_var_ident;
				rhs_var_ident = rhs_temp_var_ident;
				lhs_reg = lhs_temp_reg;
				rhs_reg = rhs_temp_reg;
			}

			if (cond_type == KIRT::exp_t::GT) {
				// Change to LT
				std::swap(lhs_var_ident, rhs_var_ident);
				std::swap(lhs_reg, rhs_reg);
				cond_type = KIRT::exp_t::LT;
			} else if (cond_type == KIRT::exp_t::LEQ) {
				// Change to GEQ
				std::swap(lhs_var_ident, rhs_var_ident);
				std::swap(lhs_reg, rhs_reg);
				cond_type = KIRT::exp_t::GEQ;
			}

			switch (cond_type) {
				case KIRT::exp_t::EQ: branch_inst_name = "beq"; break;
				case KIRT::exp_t::NEQ: branch_inst_name = "bne"; break;
				case KIRT::exp_t::LT: branch_inst_name = "blt"; break;
				case KIRT::exp_t::GEQ: branch_inst_name = "bge"; break;
				default: my_assert(19, 0);
			}

			var_manager.on_exit_block(lhs_reg, rhs_reg);
			branch_inst_arg = format(
				"%s, %s",
				lhs_reg.c_str(),
				rhs_reg.c_str()
			);
			PUSH_ASM("<BRANCH_INST>");
			var_manager.release_var_if_temp(lhs_var_ident);
			var_manager.release_var_if_temp(rhs_var_ident);
		} else {
			string cond_var_ident = kirt2asm(branch_inst->cond);
			string cond_reg = var_manager.stage_and_load_var(cond_var_ident);
			var_manager.on_exit_block(cond_reg);
			branch_inst_name = "bnez";
			branch_inst_arg = cond_reg.c_str();
			PUSH_ASM("<BRANCH_INST>");
			var_manager.release_var_if_temp(cond_var_ident);
		}
		
		assert(cur_func_asm.back() == "<BRANCH_INST>");
		cur_func_asm.pop_back();

		// WARN This is not correct
		// Here, in order to save a `j`, we use `beqz` to jump to the false block
		// However, if the false block is too far (address delta < -2048 or > 2047,
		// the assembler will boom). Here we use a simple heuristic to determine
		// whether to use `beqz` or `bnez`, based on the block id delta
		string true_block_name = rename_block(branch_inst->true_block->name);
		string false_block_name = rename_block(branch_inst->false_block->name);
		string br_target, jmp_target;
		bool force_jmp_to_true = abs(branch_inst->true_block->id - cur_block.id) > 40;
		bool force_jmp_to_false = abs(branch_inst->false_block->id - cur_block.id) > 40;
		if (force_jmp_to_false || (!force_jmp_to_true && branch_inst->false_block->id == cur_block.id + 1)) {
			// Branch to true
			br_target = true_block_name;
			jmp_target = false_block_name;
		} else {
			// Branch to false
			br_target = false_block_name;
			jmp_target = true_block_name;
			if (branch_inst_name == "beqz") {
				branch_inst_name = "bnez";
			} else if (branch_inst_name == "bnez") {
				branch_inst_name = "beqz";
			} else if (branch_inst_name == "beq") {
				branch_inst_name = "bne";
			} else if (branch_inst_name == "bne") {
				branch_inst_name = "beq";
			} else if (branch_inst_name == "blt") {
				branch_inst_name = "bge";
			} else if (branch_inst_name == "bge") {
				branch_inst_name = "blt";
			} else {
				my_assert(19, 0);
			}
		}

		PUSH_ASM("  %s %s, %s", branch_inst_name.c_str(), branch_inst_arg.c_str(), br_target.c_str());
		PUSH_ASM("  j %s", jmp_target.c_str());
	} else {
		my_assert(19, 0);
	}
}

// Return: the (possibly temp) variable name
// The result will be put into res_ident_pref (pref = preference) if provided
// (this is not guaranteed. The returned ident may not be the same as res_ident_pref)
string kirt2asm(const KIRT::Exp &exp, const optional<string> &res_ident_pref) {
	if (exp.type == KIRT::exp_t::LVAL) {
		if (exp.lval.is_int()) {
			string ident = exp.lval.ident;
			return ident;
		} else if (exp.lval.is_arr()) {
			assert(exp.lval.type.dims() == 1);
			string index_var_ident = kirt2asm(*exp.lval.indices[0]);
			string index_reg = var_manager.stage_and_load_var(index_var_ident);

			string arr_addr_reg = var_manager.load_arr_addr_to_reg(exp.lval.ident, index_reg);

			string res_virt_ident = res_ident_pref.value_or(var_manager.get_new_temp_var());
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
		// After evicting this reg, `arg_virt_ident` won't touch aX registers,
		// keeping them safe

		string arg_addr_temp_reg = "ra";
		if (num_args > 8) {
			// Evict this for temp offset calculation if necessary
			var_manager.manually_evict_reg("t2");
			var_manager.manually_evict_reg("ra");
		} else {
			var_manager.manually_evict_reg("ra");
		}
		for (int i = (int)exp.args.size()-1; i >= 0; i--) {
			// We perform this backwards to avoid overwriting the aX registers
			string arg_virt_ident = arg_virt_idents[i];
			if (num_args > 8) {
				var_manager.manually_evict_reg("t1");
				var_manager.manually_evict_reg(arg_addr_temp_reg);
			}
			if (i <= 7) {
				// Save the argument to the register
				string target = format("a%d", i);
				string arg_reg = var_manager.stage_and_load_var(arg_virt_ident, nullopt, nullopt, target);
				if (arg_reg != target) {
					var_manager.manually_evict_reg(target);
					PUSH_ASM(
						"  mv %s, %s",
						target.c_str(),
						arg_reg.c_str()
					);
				}
			} else {
				string arg_reg = var_manager.stage_and_load_var(arg_virt_ident);
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
			// var_manager.manually_evict_reg(arg_reg);
		}

		// Save vars (evict all caller-saved regs and global vars)
		var_manager.on_function_call();

		int sp_adjust = stack_frame_len + (num_args > 8 ? (num_args-8)*4 : 0);
		if (sp_adjust > 2048) {
			// Here using t0 is fine since all caller-saved regs have been invalidated
			PUSH_ASM("  li t0, %d", sp_adjust);
			PUSH_ASM("  sub sp, sp, t0");
		} else {
			PUSH_ASM("  addi sp, sp, -%d", sp_adjust);
		}

		// Call
		PUSH_ASM(
			"  call %s",
			exp.func_name.c_str()
		);

		// Step-back SP
		if (sp_adjust > 2048) {
			// Here we load -sp_adjust and use `sub` to get better performance
			// when sp_adjust == 2048
			PUSH_ASM("  li t0, %d", -sp_adjust);
			PUSH_ASM("  sub sp, sp, t0");
		} else {
			PUSH_ASM("  addi sp, sp, %d", sp_adjust);
		}

		// Save the return value
		string res_virt_ident = res_ident_pref.value_or(var_manager.get_new_temp_var());
		string res_reg = var_manager.stage_var(res_virt_ident, nullopt, nullopt, "a0");
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
		string res_virt_ident = res_ident_pref.value_or(var_manager.get_new_temp_var());
		string res_reg = var_manager.stage_var(res_virt_ident);
		PUSH_ASM("  li %s, %d", res_reg.c_str(), exp.number);
		var_manager.on_store(res_virt_ident);
		return res_virt_ident;
	} else if (exp.type == KIRT::exp_t::EQ0 || exp.type == KIRT::exp_t::NEQ0) {
		string lhs_virt_ident = kirt2asm(*exp.lhs);
		string lhs_reg = var_manager.stage_and_load_var(lhs_virt_ident);
		string res_virt_ident = res_ident_pref.value_or(var_manager.get_new_temp_var());
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
		if (exp.type == KIRT::exp_t::REM &&
			exp.rhs->type == KIRT::exp_t::NUMBER &&
			exp.rhs->number == 998244353) {
			// Optimize %998244353 by hand
			// (Steal from Clang's codegen)
			// 
			// Clang's code for x (in a0) % 998244353
			// 
			// lui	a1, 70493
			// addi	a1, a1, -2031
			// mulh	a1, a0, a1
			// srli	a2, a1, 31
			// srai	a1, a1, 26
			// add	a1, a1, a2
			// lui	a2, 243712
			// addi	a2, a2, 1
			// mul	a1, a1, a2
			// sub	a0, a0, a1
			// ret

			string lhs_virt_ident = kirt2asm(*exp.lhs);
			string lhs_reg = var_manager.stage_and_load_var(lhs_virt_ident);
			string res_ident = res_ident_pref.value_or(var_manager.get_new_temp_var());	// "a1"
			string res_reg = var_manager.stage_var(res_ident, lhs_reg);

			string temp_virt_ident = var_manager.get_new_temp_var();	// "a2"
			string temp_reg = var_manager.stage_var(temp_virt_ident, lhs_reg, res_reg);

			PUSH_ASM("  lui %s, 70493", res_reg.c_str());
			PUSH_ASM("  addi %s, %s, -2031", res_reg.c_str(), res_reg.c_str());
			PUSH_ASM("  mulh %s, %s, %s", res_reg.c_str(), lhs_reg.c_str(), res_reg.c_str());
			PUSH_ASM("  srli %s, %s, 31", temp_reg.c_str(), res_reg.c_str());
			PUSH_ASM("  srai %s, %s, 26", res_reg.c_str(), res_reg.c_str());
			PUSH_ASM("  add %s, %s, %s", res_reg.c_str(), res_reg.c_str(), temp_reg.c_str());

			PUSH_ASM("  lui %s, 243712", temp_reg.c_str());
			PUSH_ASM("  addi %s, %s, 1", temp_reg.c_str(), temp_reg.c_str());

			PUSH_ASM("  mul %s, %s, %s", res_reg.c_str(), res_reg.c_str(), temp_reg.c_str());
			PUSH_ASM("  sub %s, %s, %s", res_reg.c_str(), lhs_reg.c_str(), res_reg.c_str());

			var_manager.on_store(res_ident);

			var_manager.release_var_if_temp(lhs_virt_ident);
			var_manager.release_var_if_temp(temp_virt_ident);
			return res_ident;
		}
		if (exp.type == KIRT::exp_t::DIV &&
			exp.rhs->type == KIRT::exp_t::NUMBER &&
			exp.rhs->number == 30) {
			// Optimize /30 by hand
			// (Steal from Clang's codegen)
			// 
			// Clang's code for x (in a0) % 998244353
			// lui	a1, 559241
			// addi	a1, a1, -1911
			// mulh	a1, a0, a1
			// add	a0, a1, a0
			// srli	a1, a0, 31
			// srai	a0, a0, 4
			// add	a0, a0, a1
			// ret

			string lhs_virt_ident = kirt2asm(*exp.lhs);
			string lhs_reg = var_manager.stage_and_load_var(lhs_virt_ident);
			string res_ident = res_ident_pref.value_or(var_manager.get_new_temp_var());	// "a1"
			string res_reg = var_manager.stage_var(res_ident, lhs_reg);

			string temp_virt_ident;
			string temp_reg;
			if (is_start_with(lhs_virt_ident, "~temp")) {
				temp_virt_ident = lhs_virt_ident;
				temp_reg = lhs_reg;
			} else {
				temp_virt_ident = var_manager.get_new_temp_var();
				temp_reg = var_manager.stage_var(temp_virt_ident, lhs_reg, res_reg);
			}

			PUSH_ASM("  lui %s, 559241", res_reg.c_str());
			PUSH_ASM("  addi %s, %s, -1911", res_reg.c_str(), res_reg.c_str());
			PUSH_ASM("  mulh %s, %s, %s", res_reg.c_str(), lhs_reg.c_str(), res_reg.c_str());
			PUSH_ASM("  add %s, %s, %s", temp_reg.c_str(), res_reg.c_str(), lhs_reg.c_str());
			PUSH_ASM("  srli %s, %s, 31", res_reg.c_str(), temp_reg.c_str());
			PUSH_ASM("  srai %s, %s, 4", temp_reg.c_str(), temp_reg.c_str());
			PUSH_ASM("  add %s, %s, %s", res_reg.c_str(), temp_reg.c_str(), res_reg.c_str());

			var_manager.on_store(res_ident);

			var_manager.release_var_if_temp(lhs_virt_ident);
			if (temp_virt_ident != lhs_virt_ident) {
				var_manager.release_var_if_temp(temp_virt_ident);
			}
			return res_ident;
		}
		if (exp.rhs->type == KIRT::exp_t::NUMBER) {
			// Try to use instruction XXXi instead of la + XXX
			// For example, use addi instead of add
			int x = exp.rhs->number;
			KIRT::exp_t imm_type = exp.type;
			if (exp.type == KIRT::exp_t::SUB) {
				imm_type = KIRT::exp_t::ADD;
				x = -x;
			}
			if (is_binary_op_immediateable(imm_type) && -2048 <= x && x <= 2047) {
				// We can use imm
				auto lhs_virt_ident = kirt2asm(*exp.lhs);
				auto lhs_reg = var_manager.stage_and_load_var(lhs_virt_ident);
				string res_virt_ident = res_ident_pref.value_or(var_manager.get_new_temp_var());
				string res_reg = var_manager.stage_var(res_virt_ident, lhs_reg);

				PUSH_ASM(
					"  %si %s, %s, %d",
					kirt_exp_t2asm(imm_type).c_str(),
					res_reg.c_str(),
					lhs_reg.c_str(),
					x
				);
				var_manager.on_store(res_virt_ident);
				var_manager.release_var_if_temp(lhs_virt_ident);
				return res_virt_ident;
			}
		}
		auto lhs_virt_ident = kirt2asm(*exp.lhs);
		auto rhs_virt_ident = kirt2asm(*exp.rhs);
		string lhs_reg = var_manager.stage_and_load_var(lhs_virt_ident);
		string rhs_reg = var_manager.stage_and_load_var(rhs_virt_ident, lhs_reg);
		string res_virt_ident = res_ident_pref.value_or(var_manager.get_new_temp_var());
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