#include "kirt2strkir.h"

#include <algorithm>
#include <cassert>
#include <functional>
#include <memory>
#include <vector>

#include "utils/utils.h"

namespace KIRT {

Counter temp_reg_cnter;

static string kirt_type_t2str(type_t type) {
	switch (type) {
		case type_t::INT:
			return "i32";
		default:
			assert(0);
	}
}

static string kirt_exp_t2str(exp_t type) {
	switch (type) {
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

std::vector<string> get_all_varid(const Function &func) {
	std::vector<string> res;
	std::function<void(const Exp &exp)> collect_varids_in_exp = [&](const Exp &exp) {
		if (exp.type == exp_t::NUMBER) {
		} else if (exp.type == exp_t::LVAL) {
			res.push_back(exp.ident);
		} else if (exp.type == exp_t::EQ0 || exp.type == exp_t::NEQ0) {
			collect_varids_in_exp(*exp.lhs);
		} else if (exp.type == exp_t::FUNC_CALL) {
			for (const shared_ptr<Exp> &arg : exp.args) {
				collect_varids_in_exp(*arg);
			}
		} else {
			collect_varids_in_exp(*exp.lhs);
			collect_varids_in_exp(*exp.rhs);
		}
	};
	for (const FuncFParam &func_fparam : func.fparams) {
		res.push_back(func_fparam.ident);
	}
	for (const std::shared_ptr<Block> &block : func.blocks.blocks) {
		for (const shared_ptr<Inst> &inst : block->insts) {
			if (const AssignInst *assign_inst = dynamic_cast<AssignInst*>(inst.get())) {
				res.push_back(assign_inst->ident);
				collect_varids_in_exp(assign_inst->exp);
			} else if (const ExpInst *exp_inst = dynamic_cast<ExpInst*>(inst.get())) {
				collect_varids_in_exp(exp_inst->exp);
			} else {
				assert(0);
			}
		}
		if (const ReturnInst *return_inst = dynamic_cast<ReturnInst*>(block->term_inst.get())) {
			if (return_inst->ret_exp)
				collect_varids_in_exp(*return_inst->ret_exp);
		}
	}
	std::sort(res.begin(), res.end());
	res.resize(std::unique(res.begin(), res.end()) - res.begin());
	return res;
}


list<string> kirt2str(const Function &func);
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
	for (const shared_ptr<GlobalDecl> &global_decl : program.global_decls) {
		string command = format(
			"global %s = alloc %s, zeroinit",
			global_decl->ident.c_str(),
			"i32"	// TODO support array type
		);
		res.push_back(command);
	}
	res.push_back("");

	// Function definitions
	for (const shared_ptr<Function> &func : program.funcs) {
		list<string> func_str = kirt2str(*func);
		res << func_str;
	}

	return res;
}

list<string> kirt2str(const Function &func) {
	vector<string> varids = get_all_varid(func);
	temp_reg_cnter.reset();	// Clear the register counter

	list<string> res;

	// Forge the function definition line
	string func_def_line = "fun @" + func.name + "(";
	for (const FuncFParam &fparam : func.fparams) {
		if (func_def_line.back() != '(')
			func_def_line += ", ";
		func_def_line += fparam.ident+"_param___" + ": " + kirt_type_t2str(fparam.type);
	}
	if (func_def_line.back() == ' ' && func_def_line[func_def_line.size() - 2] == ',') {
		func_def_line.pop_back();
		func_def_line.pop_back();
	}
	func_def_line += ")";
	if (func.ret_type != type_t::VOID) {
		func_def_line += ": " + kirt_type_t2str(func.ret_type);
	}
	func_def_line += " {";
	res.push_back(func_def_line);

	// Forge the entry block
	res.push_back("%entry:");
	for (string &varid : varids) {
		if (!global_decl_map.count(varid))
			res.push_back("  " + varid + " = alloc i32");
	}
	for (const FuncFParam &fparam : func.fparams) {
		res.push_back("  store " + fparam.ident+"_param___, " + fparam.ident);
	}
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
		assert(0);
	}
}

list<string> kirt2str(const AssignInst &AssignInst) {
	list<string> res;
	auto [exp_inst_list, exp_coid] = kirt2str(AssignInst.exp);
	res << exp_inst_list;
	// AssignInst.ident must be a variable name
	res.push_back(format("  store %s, %s", exp_coid.c_str(), AssignInst.ident.c_str()));
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
		string load_inst = format("  %s = load %s", res_coid.c_str(), exp.ident.c_str());
		return { { load_inst }, res_coid };
	} else if (exp.type == exp_t::FUNC_CALL) {
		assert(KIRT::func_map.count(exp.ident));
		shared_ptr<Function> func = KIRT::func_map[exp.ident];
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

		call_inst += format("call @%s(", exp.ident.c_str());
		for (size_t i = 0; i < args_coid.size(); i++) {
			call_inst += args_coid[i];
			if (i + 1 < args_coid.size())
				call_inst += ", ";
		}
		call_inst += ")";
		
		res.push_back(call_inst);
		return { res, res_coid };
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
