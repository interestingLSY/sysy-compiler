#include "kirt2strkir.h"

#include <cassert>
#include <memory>

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
list<string> kirt2str(const Program &program) {
	list<string> res;
	for (const shared_ptr<Inst> &inst : program.global_defs) {
		list<string> inst_str = kirt2str(inst);
		res.splice(res.end(), inst_str);
	}
	for (const Function &func : program.funcs) {
		list<string> func_str = kirt2str(func);
		res.splice(res.end(), func_str);
	}
	return res;
}

list<string> kirt2str(const Function &func) {
	temp_reg_cnter.reset();	// Clear the register counter
	list<string> res;
	res.push_back("fun @" + func.name + "(): " + kirt_type_t2str(func.ret_type) + " {");
	res.push_back("\%entry:");
	list<string> blocks_str = kirt2str(func.blocks);
	res.splice(res.end(), blocks_str);
	res.push_back("}");
	return res;
}

list<string> kirt2str(const BlockList &assembling_blocks) {
	list<string> res;
	for (const Block &block : assembling_blocks.blocks) {
		list<string> block_str = kirt2str(block);
		res.splice(res.end(), block_str);
	}
	return res;
}

list<string> kirt2str(const Block &block) {
	list<string> res;
	for (const shared_ptr<Inst> &inst : block.insts) {
		list<string> inst_str = kirt2str(inst);
		res.splice(res.end(), inst_str);
	}
	return res;
}

list<string> kirt2str(const std::shared_ptr<Inst> &inst) {
	if (const ReturnInst *return_inst = dynamic_cast<ReturnInst*>(inst.get())) {
		return kirt2str(*return_inst);
	} else {
		assert(0);
	}

}

list<string> kirt2str(const ReturnInst &return_inst) {
	list<string> res;
	auto [exp_inst_list, exp_varid] = kirt2str(return_inst.ret_exp);
	res.splice(res.end(), exp_inst_list);
	res.push_back("  ret " + exp_varid);
	return res;
}

pair<list<string>, string> kirt2str(const Exp &exp) {
	if (exp.type == exp_t::NUMBER) {
		return { {}, std::to_string(exp.number) };
	} else if (exp.type == exp_t::EQ0 || exp.type == exp_t::NEQ0) {
		assert (exp.lhs);
		auto [lhs_inst_list, lhs_varid] = kirt2str(*exp.lhs);
		string res_varid = "%" + std::to_string(temp_reg_cnter.next());
		list<string> res;
		res.splice(res.end(), lhs_inst_list);
		res.push_back(
			"  " + 
			res_varid +
			" = " + 
			kirt_exp_t2str(exp.type) +
			" " +
			lhs_varid +
			", 0"
		);
		return {res, res_varid};
	} else {
		assert (exp.lhs);
		assert (exp.rhs);
		auto [lhs_inst_list, lhs_varid] = kirt2str(*exp.lhs);
		auto [rhs_inst_list, rhs_varid] = kirt2str(*exp.rhs);
		string res_varid = "%" + std::to_string(temp_reg_cnter.next());
		list<string> res;
		res.splice(res.end(), lhs_inst_list);
		res.splice(res.end(), rhs_inst_list);
		// res.push_back XXX
		res.push_back(
			"  " + 
			res_varid +
			" = " + 
			kirt_exp_t2str(exp.type) +
			" " +
			lhs_varid +
			", " +
			rhs_varid
		);
		return {res, res_varid};
	}
}

}
