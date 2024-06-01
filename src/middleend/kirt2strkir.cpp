#include "kirt2strkir.h"

#include <algorithm>
#include <cassert>
#include <cstdio>
#include <functional>
#include <iostream>
#include <memory>
#include <unordered_set>
#include <vector>

#include "utils/utils.h"

namespace KIRT {

Counter temp_reg_cnter;

static string kirt_exp_t2str(exp_t type) {
	switch (type) {
		case exp_t::ADDR_ADD:
			assert(0);	// Should be processed separately since we need to divide the offset by 4
		case exp_t::ADD:
			return "add";
		case exp_t::SUB:
			return "sub";
		case exp_t::MUL:
			return "mul";
		case exp_t::DIV:
			return "div";
		case exp_t::REM:
			return "mod";
		case exp_t::LT:
			return "lt";
		case exp_t::GT:
			return "gt";
		case exp_t::LEQ:
			return "le";
		case exp_t::GEQ:
			return "ge";
		case exp_t::EQ:
		case exp_t::EQ0:
			return "eq";
		case exp_t::NEQ:
		case exp_t::NEQ0:
			return "ne";
		case exp_t::BITWISE_AND:
			return "and";
		case exp_t::BITWISE_OR:
			return "or";
		case exp_t::BITWISE_XOR:
			return "xor";
		case exp_t::SHL:
			return "shl";
		case exp_t::SHR:
			return "shr";
		case exp_t::SAR:
			return "sar";
		default:
			assert(0);
	}
}

list<string> kirt2str(const Function &func, const list<string> &load_global_arr_addr_prologue);
list<string> kirt2str(const BlockList &assembling_blocks);
list<string> kirt2str(const Block &block);
list<string> kirt2str(const std::shared_ptr<TermInst> &term_inst);
list<string> kirt2str(const ReturnInst &return_inst);
list<string> kirt2str(const JumpInst &jump_inst);
list<string> kirt2str(const BranchInst &branch_inst);
list<string> kirt2str(const std::shared_ptr<Inst> &inst);
list<string> kirt2str(const AssignInst &assign_inst);
list<string> kirt2str(const ExpInst &exp_inst);
pair<list<string>, string> kirt2str(const Exp &exp);

// All Arrays Are Pointers Rule:
// 
// For convenience, all arrays in the generated Koopa IR are pointers. When loading
// or storing an element, we use `getptr` followed by `load` or `store`. When
// calculating the address of an element, we use `getptr` as well.
// 
// To achieve this:
// - When allocating an array, we first allocate its underlying array, then use
//   `getelemptr` to get the pointer to the first element. This applies for both
//   global and local arrays.
// - When passing arguments to a function, we pass the pointer (*i32).

list<string> kirt2str(const Program &program) {
	list<string> res;
	// Library function declarations
	res <= list<string>({
		"decl @getint(): i32",
		"decl @getch(): i32",
		"decl @getarray(*i32): i32",
		"decl @putint(i32)",
		"decl @putch(i32)",
		"decl @putarray(i32, *i32)",
		"decl @starttime()",
		"decl @stoptime()",
		""
	});

	// Global variable declarations
	list<string> load_global_arr_addr_prologue;
	for (const shared_ptr<GlobalDecl> &global_decl : program.global_decls) {
		if (global_decl->type.is_int()) {
			string command = format(
				"global %s = alloc i32, %d",
				global_decl->ident.c_str(),
				global_decl->init_val
			);
			res.push_back(command);
		} else if (global_decl->type.is_arr()) {
			assert(global_decl->type.dims() == 1);
			string init_list_str;
			int num_nonzero_elems = std::count_if(
				global_decl->arr_init_vals.begin(),
				global_decl->arr_init_vals.end(),
				[](int x) { return x != 0; 
			});
			if (num_nonzero_elems > 0) {
				init_list_str = "{";
				for (int i = 0; i < global_decl->arr_init_vals.size(); i++) {
					init_list_str += std::to_string(global_decl->arr_init_vals[i]);
					if (i + 1 < global_decl->arr_init_vals.size())
						init_list_str += ", ";
				}
				init_list_str += "}";
			} else {
				init_list_str = "zeroinit";
			}
			string command = format(
				"global %s_underlying_arr = alloc [i32, %d], %s",
				global_decl->ident.c_str(),
				global_decl->type.numel(),
				init_list_str.c_str()
			);
			res.push_back(command);
			command = format(
				"  %s = getelemptr %s_underlying_arr, 0",
				global_decl->ident.c_str(),
				global_decl->ident.c_str()
			);
			load_global_arr_addr_prologue.push_back(command);
		} else {
			assert(0);
		}
	}
	res.push_back("");

	// Function definitions
	for (const shared_ptr<Function> &func : program.funcs) {
		list<string> func_str = kirt2str(*func, load_global_arr_addr_prologue);
		res << func_str;
	}

	return res;
}

list<string> kirt2str(const Function &func, const list<string> &load_global_arr_addr_prologue) {
	temp_reg_cnter.reset();	// Clear the register counter

	list<string> res;

	// Forge the function definition line and variable-loading lines
	list<string> store_var_lines;
	string func_def_line = "fun @" + func.name + "(";
	for (const FuncFParam &fparam : func.fparams) {
		if (func_def_line.back() != '(')
			func_def_line += ", ";
		if (fparam.type.is_int()) {
			func_def_line += fparam.ident + "_param___" + ": i32";
			store_var_lines.push_back(format(
				"  %s = alloc i32",
				fparam.ident.c_str()
			));
			store_var_lines.push_back(format(
				"  store %s_param___, %s",
				fparam.ident.c_str(),
				fparam.ident.c_str()
			));
		} else if (fparam.type.is_arr()) {
			func_def_line += fparam.ident + ": *i32";
		} else {
			assert(0);
		}
	}
	if (func_def_line.back() == ' ' && func_def_line[func_def_line.size() - 2] == ',') {
		func_def_line.pop_back();
		func_def_line.pop_back();
	}
	func_def_line += ")";
	if (func.ret_type != type_t::VOID) {
		func_def_line += ": i32";	// Return type can only be int
	}
	func_def_line += " {";
	res.push_back(func_def_line);

	// Forge the entry block
	res.push_back("%entry:");

	// Load the global array addresses
	res <= load_global_arr_addr_prologue;

	// Allocate space for all local variables (besides global vars and function params)
	for (const FuncLocalVar &local_var : func.local_vars) {
		if (local_var.type.is_int()) {
			res.push_back("  " + local_var.ident + " = alloc i32");
		} else if (local_var.type.is_arr()) {
			assert(local_var.type.dims() == 1);
			res.push_back(format(
				"  %s_underlying_arr = alloc [i32, %d]",
				local_var.ident.c_str(),
				local_var.type.numel()
			));
			res.push_back(format(
				"  %s = getelemptr %s_underlying_arr, 0",
				local_var.ident.c_str(),
				local_var.ident.c_str()
			));
		} else {
			assert(0);
		}
	}

	// Insert all "store variable" lines
	res << store_var_lines;
	res.push_back("  jump %real_entry");

	list<string> blocks_str = kirt2str(func.blocks);
	res << blocks_str;

	res.push_back("}");
	return res;
}

list<string> kirt2str(const BlockList &blocks) {
	list<string> res;
	for (const std::shared_ptr<Block> &block : blocks.blocks) {
		list<string> block_str = kirt2str(*block);
		res << block_str;
	}
	return res;
}

list<string> kirt2str(const Block &block) {
	list<string> res;
	res.push_back("%" + block.name + ":");
	for (const shared_ptr<Inst> &inst : block.insts) {
		list<string> inst_str = kirt2str(inst);
		res << inst_str;
	}
	list<string> term_inst_str = kirt2str(block.term_inst);
	res << term_inst_str;
	return res;
}

list<string> kirt2str(const std::shared_ptr<TermInst> &term_inst) {
	assert (term_inst);
	// printf("%d\n", !!term_inst);
	TermInst* term_inst_ptr = term_inst.get();
	if (const ReturnInst *return_inst = dynamic_cast<ReturnInst*>(term_inst_ptr)) {
		return kirt2str(*return_inst);
	} else if (const JumpInst *jump_inst = dynamic_cast<JumpInst*>(term_inst_ptr)) {
		return kirt2str(*jump_inst);
	} else if (const BranchInst *branch_inst = dynamic_cast<BranchInst*>(term_inst_ptr)) {
		return kirt2str(*branch_inst);
	} else {
		assert(0);
	}
}

list<string> kirt2str(const ReturnInst &return_inst) {
	list<string> res;
	if (return_inst.ret_exp) {
		auto [exp_inst_list, exp_coid] = kirt2str(*return_inst.ret_exp);
		res << exp_inst_list;
		res.push_back("  ret " + exp_coid);
	} else {
		res.push_back("  ret");
	}
	return res;
}

list<string> kirt2str(const JumpInst &jump_inst) {
	list<string> res;
	assert(jump_inst.target_block);
	res.push_back("  jump %" + jump_inst.target_block->name);
	return res;
}

list<string> kirt2str(const BranchInst &branch_inst) {
	list<string> res;
	assert (branch_inst.true_block);
	assert (branch_inst.false_block);
	auto [cond_inst_list, cond_coid] = kirt2str(branch_inst.cond);
	res << cond_inst_list;
	res.push_back(format(
		"  br %s, %%%s, %%%s",
		cond_coid.c_str(),
		branch_inst.true_block->name.c_str(),
		branch_inst.false_block->name.c_str()
	));
	return res;
}

list<string> kirt2str(const std::shared_ptr<Inst> &inst) {
	if (const AssignInst *assign_inst = dynamic_cast<AssignInst*>(inst.get())) {
		return kirt2str(*assign_inst);
	} else if (const ExpInst *exp_inst = dynamic_cast<ExpInst*>(inst.get())) {
		return kirt2str(*exp_inst);
	} else {
		my_assert(26, false);
	}
}

list<string> kirt2str(const AssignInst &assign_inst) {
	list<string> res;
	auto [exp_inst_list, exp_coid] = kirt2str(assign_inst.exp);
	res << exp_inst_list;
	if (assign_inst.lval.is_int()) {
		res.push_back(format(
			"  store %s, %s",
			exp_coid.c_str(),
			assign_inst.lval.ident.c_str())
		);
	} else if (assign_inst.lval.is_arr()) {
		// The arrary must be collapsed to a single dimension
		my_assert(27, assign_inst.lval.indices.size() == 1);
		auto [lval_exp_inst_list, lval_exp_coid] = kirt2str(*assign_inst.lval.indices[0]);
		res << lval_exp_inst_list;
		// Divide the offset by 4 (sizeof(int))
		string lval_exp_div4_coid = "%" + std::to_string(temp_reg_cnter.next());
		res.push_back(format(
			"  %s = sar %s, 2",
			lval_exp_div4_coid.c_str(),
			lval_exp_coid.c_str()
		));
		int ptr_id = temp_reg_cnter.next();
		res.push_back(format(
			"  %%ptr_%d = getptr %s, %s",
			ptr_id,
			assign_inst.lval.ident.c_str(),
			lval_exp_div4_coid.c_str()
		));
		res.push_back(format(
			"  store %s, %%ptr_%d",
			exp_coid.c_str(),
			ptr_id
		));
	} else {
		my_assert(28, false);
	}
	return res;
}

list<string> kirt2str(const ExpInst &exp_inst) {
	list<string> res;
	auto [exp_inst_list, exp_coid] = kirt2str(exp_inst.exp);
	res << exp_inst_list;
	return res;
}

pair<list<string>, string> kirt2str(const Exp &exp) {
	if (exp.type == exp_t::NUMBER) {
		return { {}, std::to_string(exp.number) };
	} else if (exp.type == exp_t::LVAL) {
		string res_coid = "%" + std::to_string(temp_reg_cnter.next());
		list<string> load_insts;
		if (exp.lval.is_int()) {
			load_insts.push_back(format(
				"  %s = load %s",
				res_coid.c_str(),
				exp.lval.ident.c_str()
			));
		} else if (exp.lval.is_arr()) {
			// The arrary must be collapsed to a single dimension
			my_assert(29, exp.lval.indices.size() == 1);
			auto [lval_exp_inst_list, lval_exp_coid] = kirt2str(*exp.lval.indices[0]);
			load_insts << lval_exp_inst_list;
			string lval_exp_div4_coid = "%" + std::to_string(temp_reg_cnter.next());
			load_insts.push_back(format(
				"  %s = sar %s, 2",
				lval_exp_div4_coid.c_str(),
				lval_exp_coid.c_str()
			));
			int ptr_id = temp_reg_cnter.next();
			load_insts.push_back(format(
				"  %%ptr_%d = getptr %s, %s",
				ptr_id,
				exp.lval.ident.c_str(),
				lval_exp_div4_coid.c_str()
			));
			load_insts.push_back(format(
				"  %s = load %%ptr_%d",
				res_coid.c_str(),
				ptr_id
			));
		} else {
			my_assert(30, false);
		}
		return { load_insts, res_coid };
	} else if (exp.type == exp_t::FUNC_CALL) {
		assert(KIRT::func_map.count(exp.func_name));
		shared_ptr<Function> func = KIRT::func_map[exp.func_name];
		list<string> res;

		vector<string> args_coid;
		for (const shared_ptr<Exp> &arg : exp.args) {
			auto [arg_inst_list, arg_coid] = kirt2str(*arg);
			res << arg_inst_list;
			args_coid.push_back(arg_coid);
		}

		string call_inst = "  ";
		string res_coid;
		if (func->ret_type == type_t::VOID) {
		} else if (func->ret_type == type_t::INT) {
			res_coid = "%" + std::to_string(temp_reg_cnter.next());
			call_inst += res_coid + " = ";
		} else {
			assert(0);
		}

		call_inst += format("call @%s(", exp.func_name.c_str());
		for (size_t i = 0; i < args_coid.size(); i++) {
			call_inst += args_coid[i];
			if (i + 1 < args_coid.size())
				call_inst += ", ";
		}
		call_inst += ")";
		
		res.push_back(call_inst);
		return { res, res_coid };
	} else if (exp.type == exp_t::ARR_ADDR) {
		return {{}, exp.arr_name};
	} else if (exp.type == exp_t::EQ0 || exp.type == exp_t::NEQ0) {
		assert (exp.lhs);
		auto [lhs_inst_list, lhs_coid] = kirt2str(*exp.lhs);
		string res_coid = "%" + std::to_string(temp_reg_cnter.next());
		list<string> res;
		res << lhs_inst_list;
		res.push_back(
			"  " + 
			res_coid +
			" = " + 
			kirt_exp_t2str(exp.type) +
			" " +
			lhs_coid +
			", 0"
		);
		return {res, res_coid};
	} else if (exp.type == exp_t::ADDR_ADD) {
		assert (exp.lhs);
		assert (exp.rhs);
		auto [lhs_inst_list, lhs_coid] = kirt2str(*exp.lhs);
		auto [rhs_inst_list, rhs_coid] = kirt2str(*exp.rhs);
		list<string> res;
		res << lhs_inst_list;
		res << rhs_inst_list;
		string res_coid = "%" + std::to_string(temp_reg_cnter.next());
		string rhs_div4_coid = "%" + std::to_string(temp_reg_cnter.next());
		res.push_back(format(
			"  %s = sar %s, 2",
			rhs_div4_coid.c_str(),
			rhs_coid.c_str()
		));
		res.push_back(format(
			"  %s = getptr %s, %s",
			res_coid.c_str(),
			lhs_coid.c_str(),
			rhs_div4_coid.c_str()
		));
		return {res, res_coid};
	} else {
		assert (exp.lhs);
		assert (exp.rhs);
		auto [lhs_inst_list, lhs_coid] = kirt2str(*exp.lhs);
		auto [rhs_inst_list, rhs_coid] = kirt2str(*exp.rhs);
		string res_coid = "%" + std::to_string(temp_reg_cnter.next());
		list<string> res;
		res << lhs_inst_list;
		res << rhs_inst_list;
		// res.push_back XXX
		res.push_back(
			"  " + 
			res_coid +
			" = " + 
			kirt_exp_t2str(exp.type) +
			" " +
			lhs_coid +
			", " +
			rhs_coid
		);
		return {res, res_coid};
	}
}

}
