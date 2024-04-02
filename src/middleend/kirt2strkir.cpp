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
		} else {
			collect_varids_in_exp(*exp.lhs);
			collect_varids_in_exp(*exp.rhs);
		}
	};
	for (const std::shared_ptr<Block> &block : func.blocks.blocks) {
		for (const shared_ptr<Inst> &inst : block->insts) {
			if (const AssignInst *assign_inst = dynamic_cast<AssignInst*>(inst.get())) {
				res.push_back(assign_inst->ident);
			}
		}
		if (const ReturnInst *return_inst = dynamic_cast<ReturnInst*>(block->term_inst.get())) {
			collect_varids_in_exp(return_inst->ret_exp);
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
list<string> kirt2str(const std::shared_ptr<Inst> &inst);
list<string> kirt2str(const AssignInst &assign_inst);
pair<list<string>, string> kirt2str(const Exp &exp);

list<string> kirt2str(const Program &program) {
	list<string> res;
	for (const shared_ptr<Inst> &inst : program.global_defs) {
		list<string> inst_str = kirt2str(inst);
		res << inst_str;
	}
	for (const Function &func : program.funcs) {
		list<string> func_str = kirt2str(func);
		res << func_str;
	}
	return res;
}

list<string> kirt2str(const Function &func) {
	vector<string> varids = get_all_varid(func);
	temp_reg_cnter.reset();	// Clear the register counter

	list<string> res;
	res.push_back("fun @" + func.name + "(): " + kirt_type_t2str(func.ret_type) + " {");
	res.push_back("\%entry:");
	for (string &varid : varids) {
		res.push_back("  " + varid + " = alloc i32");
	}
	res.push_back("  jump \%real_entry");

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
	res.push_back("\%" + block.name + ":");
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
	} else {
		assert(0);
	}
}

list<string> kirt2str(const ReturnInst &return_inst) {
	list<string> res;
	auto [exp_inst_list, exp_coid] = kirt2str(return_inst.ret_exp);
	res << exp_inst_list;
	res.push_back("  ret " + exp_coid);
	return res;
}

list<string> kirt2str(const JumpInst &jump_inst) {
	list<string> res;
	assert(jump_inst.target_block);
	res.push_back("  jump \%" + jump_inst.target_block->name);
	return res;
}

list<string> kirt2str(const std::shared_ptr<Inst> &inst) {
	if (const AssignInst *assign_inst = dynamic_cast<AssignInst*>(inst.get())) {
		return kirt2str(*assign_inst);
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

pair<list<string>, string> kirt2str(const Exp &exp) {
	if (exp.type == exp_t::NUMBER) {
		return { {}, std::to_string(exp.number) };
	} else if (exp.type == exp_t::LVAL) {
		string res_coid = "%" + std::to_string(temp_reg_cnter.next());
		string load_inst = format("  %s = load %s", res_coid.c_str(), exp.ident.c_str());
		return { { load_inst }, res_coid };
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
