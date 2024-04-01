#include "backend/kirt2asm.h"

#include <cassert>

#include "utils/utils.h"

namespace ASM {

// Currently we use a simple register allocation strategy: use registers one by one
// This may lead to overflow of registers
Counter reg_alloc_counter;

static string kirt_exp_t2asm(KIRT::exp_t type) {
	switch (type) {
		case KIRT::exp_t::NUMBER:
			assert(0);
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
			assert(0);
	}
}

list<string> kirt2asm(const KIRT::Program &prog) {
	list<string> res;
	res.push_back(".text");
	res.push_back(".globl main");
	reg_alloc_counter.reset(5);	// x0-x4 are reserved

	for (const KIRT::Function &func : prog.funcs) {
		list<string> func_asm = kirt2asm(func);
		res.splice(res.end(), func_asm);
	}

	return res;
}

list<string> kirt2asm(const KIRT::Function &func) {
	list<string> res;
	res.push_back(func.name + ":");

	for (const KIRT::Block &block : func.blocks.blocks) {
		list<string> block_asm = kirt2asm(block);
		res.splice(res.end(), block_asm);
	}

	return res;
}

list<string> kirt2asm(const KIRT::Block &block) {
	list<string> res;

	for (const shared_ptr<KIRT::Inst> &inst : block.insts) {
		list<string> inst_asm = kirt2asm(inst);
		res.splice(res.end(), inst_asm);
	}

	return res;
}

list<string> kirt2asm(const shared_ptr<KIRT::Inst> &inst) {
	list<string> res;

	if (const KIRT::ReturnInst *ret_inst = dynamic_cast<const KIRT::ReturnInst *>(inst.get())) {
		auto [exp_inst_list, exp_regid] = kirt2asm(ret_inst->ret_exp);
		res.splice(res.end(), exp_inst_list);
		if (exp_regid != 10) {
			res.push_back(format(
				"  mv a0, x%d",
				exp_regid
			));
		}
		res.push_back("  ret");
	}

	return res;
}

pair<list<string>, int> kirt2asm(const KIRT::Exp &inst) {
	list<string> res;
	int res_regid = reg_alloc_counter.next();
	if (inst.type == KIRT::exp_t::NUMBER) {
		// TODO Optimize if number == 0
		res.push_back(format(
			"  li x%d, %d",
			res_regid,
			inst.number
		));
		return {res, res_regid};
	} else if (inst.type == KIRT::exp_t::EQ0 || inst.type == KIRT::exp_t::NEQ0) {
		auto [lhs_inst_list, lhs_regid] = kirt2asm(*inst.lhs);
		res.splice(res.end(), lhs_inst_list);
		res.push_back(format(
			"  %s x%d, x%d",
			inst.type == KIRT::exp_t::EQ0 ? "seqz" : "snez",
			res_regid,
			lhs_regid
		));
		return {res, res_regid};
	} else {
		auto [lhs_inst_list, lhs_regid] = kirt2asm(*inst.lhs);
		auto [rhs_inst_list, rhs_regid] = kirt2asm(*inst.rhs);
		res.splice(res.end(), lhs_inst_list);
		res.splice(res.end(), rhs_inst_list);
		res.push_back(format(
			"  %s x%d, x%d, x%d",
			kirt_exp_t2asm(inst.type).c_str(),
			res_regid,
			lhs_regid,
			rhs_regid
		));
		return {res, res_regid};
	}
}

}