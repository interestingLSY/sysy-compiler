// Replace Unasm: Replace un-assembly instructions (i.e. LEQ/GEQ/EQ/NEQ for riscv)
// with other instructions

#include "pass_repl_unasm.h"

namespace KIRT {

static void pass_repl_unasm(Exp &exp) {
	if (exp.lhs)
		pass_repl_unasm(*exp.lhs);
	if (exp.rhs)
		pass_repl_unasm(*exp.rhs);

	auto is_zero = [](const Exp &exp) {
		return exp.type == exp_t::NUMBER && exp.number == 0;
	};
	// NOTE Not all times we need to replace the instructions. For example,
	// althrough RISC-V does not have a EQ instruction, it has BEQ.
	// So for code like `if (a == b)`, we can use BEQ directly
	if (exp.type == exp_t::EQ) {
		if (is_zero(*exp.rhs)) {
			exp.type = exp_t::EQ0;
			exp.rhs.reset();
		} else if (is_zero(*exp.lhs)) {
			exp.type = exp_t::EQ0;
			exp.lhs = exp.rhs;
			exp.rhs.reset();
		} else {
			// a = (b == c) <=> a = (b - c == 0)
			auto sub_exp = std::make_shared<Exp>();
			sub_exp->type = exp_t::SUB;
			sub_exp->lhs = exp.lhs;
			sub_exp->rhs = exp.rhs;
			exp.type = exp_t::EQ0;
			exp.lhs = sub_exp;
			exp.rhs.reset();
		}
	} else if (exp.type == exp_t::NEQ) {
		if (is_zero(*exp.rhs)) {
			exp.type = exp_t::NEQ0;
			exp.rhs.reset();
		} else if (is_zero(*exp.lhs)) {
			exp.type = exp_t::NEQ0;
			exp.lhs = exp.rhs;
			exp.rhs.reset();
		} else {
			// a = (b != c) <=> a = (b - c != 0)
			auto sub_exp = std::make_shared<Exp>();
			sub_exp->type = exp_t::SUB;
			sub_exp->lhs = exp.lhs;
			sub_exp->rhs = exp.rhs;
			exp.type = exp_t::NEQ0;
			exp.lhs = sub_exp;
			exp.rhs.reset();
		}
	} else if (exp.type == exp_t::LEQ) {
		// a = (b <= c) <=> a = (b > c) == 0
		auto gt_exp = std::make_shared<Exp>();
		gt_exp->type = exp_t::GT;
		gt_exp->lhs = exp.lhs;
		gt_exp->rhs = exp.rhs;
		exp.type = exp_t::EQ0;
		exp.lhs = gt_exp;
		exp.rhs.reset();
	} else if (exp.type == exp_t::GEQ) {
		// a = (b >= c) <=> a = (b < c) == 0
		auto lt_exp = std::make_shared<Exp>();
		lt_exp->type = exp_t::LT;
		lt_exp->lhs = exp.lhs;
		lt_exp->rhs = exp.rhs;
		exp.type = exp_t::EQ0;
		exp.lhs = lt_exp;
		exp.rhs.reset();
	}
}

static void pass_repl_unasm(shared_ptr<Inst> &inst) {
	if (auto ret_inst = dynamic_cast<ReturnInst *>(inst.get())) {
		pass_repl_unasm(ret_inst->ret_exp);
	}
}

static void pass_repl_unasm(Block &block) {
	for (auto &inst : block.insts)
		pass_repl_unasm(inst);
}

static void pass_repl_unasm(Function &func) {
	for (auto &block : func.blocks.blocks)
		pass_repl_unasm(block);
}

void pass_repl_unasm(Program &prog) {
	for (auto &func : prog.funcs)
		pass_repl_unasm(func);
}

}