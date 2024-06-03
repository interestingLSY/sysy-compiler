// Pass Optimize Exp Ops: Replace heavy ops (e.g. mul/div/rem) with lighter ops
// (e.g. shl/shr) if possible

#include "pass_optim_exp_op.h"

#include <cassert>
#include <unordered_map>

#include "utils/utils.h"
#include "middleend/kirt_utils.h"

namespace KIRT {

// Statistics
static int num_propagated = 0;
static int num_replaced = 0;

static void pass_optim_exp_op(Exp &exp) {
	if (exp.lhs)
		pass_optim_exp_op(*exp.lhs);
	if (exp.rhs)
		pass_optim_exp_op(*exp.rhs);
	if (!exp.args.empty())
		for (auto &arg : exp.args)
			pass_optim_exp_op(*arg);
	if (!exp.lval.indices.empty())
		for (auto &index : exp.lval.indices)
			pass_optim_exp_op(*index);
	
	exp_t cur_exp_type = exp.type;
	exp_category_t cur_exp_category = get_exp_category(cur_exp_type);
	if (cur_exp_category == exp_category_t::NUMBER)
		return;
	else if (cur_exp_category == exp_category_t::SPECIAL)
		return;
	else if (cur_exp_category == exp_category_t::UNARY) {
		if (exp.lhs->type == exp_t::NUMBER) {
			int final_val = calc_exp_val(cur_exp_type, exp.lhs->number);
			exp.type = exp_t::NUMBER;
			exp.number = final_val;
			exp.lhs.reset();
			num_propagated += 1;
		}
		return;
	} else if (cur_exp_category == exp_category_t::BINARY) {
		if (exp.lhs->type == exp_t::NUMBER && exp.rhs->type == exp_t::NUMBER) {
			int final_val = calc_exp_val(cur_exp_type, exp.lhs->number, exp.rhs->number);
			exp.type = exp_t::NUMBER;
			exp.number = final_val;
			exp.lhs.reset();
			exp.rhs.reset();
			num_propagated += 1;
		} else {
			if (is_binary_op_commutative(cur_exp_type) && exp.lhs->type == exp_t::NUMBER) {
				std::swap(exp.lhs, exp.rhs);
			}
			// Try to optimize this op
			if (cur_exp_type == exp_t::MUL) {
				if (exp.rhs->type == exp_t::NUMBER) {
					int x = exp.rhs->number;
					int log_x = log2_floor(x);
					if (x > 0 && (1<<log_x) == x) {
						exp.type = exp_t::SHL;
						exp.rhs->number = log_x;
						num_replaced += 1;
					}
				}
			} else if (cur_exp_type == exp_t::DIV) {
				if (exp.rhs->type == exp_t::NUMBER) {
					int x = exp.rhs->number;
					int log_x = log2_floor(x);
					if (x > 0 && (1<<log_x) == x) {
						exp.type = exp_t::SAR;
						exp.rhs->number = log_x;
						num_replaced += 1;
					}
				}
			} else if (cur_exp_type == exp_t::REM) {
				if (exp.rhs->type == exp_t::NUMBER) {
					int x = exp.rhs->number;
					int log_x = log2_floor(x);
					if (x > 0 && (1<<log_x) == x) {
						exp.type = exp_t::BITWISE_AND;
						exp.rhs->number = x-1;
						num_replaced += 1;
					}
				}
			}
		}
		return;
	} else {
		assert(0);
	}
}

static void pass_optim_exp_op(LVal &lval) {
	for (shared_ptr<Exp> &exp : lval.indices)
		pass_optim_exp_op(*exp);
}

static void pass_optim_exp_op(shared_ptr<Inst> &inst) {
	if (auto assign_inst = dynamic_cast<AssignInst *>(inst.get())) {
		pass_optim_exp_op(assign_inst->exp);
		pass_optim_exp_op(assign_inst->lval);
	} else if (auto exp_inst = dynamic_cast<ExpInst *>(inst.get())) {
		pass_optim_exp_op(exp_inst->exp);
	} else {
		assert(0);
	}
}

static void pass_optim_exp_op(shared_ptr<TermInst> &term_inst) {
	if (auto ret_inst = dynamic_cast<ReturnInst *>(term_inst.get())) {
		if (ret_inst->ret_exp)
			pass_optim_exp_op(*ret_inst->ret_exp);
	} else if (auto br_inst = dynamic_cast<BranchInst *>(term_inst.get())) {
		pass_optim_exp_op(br_inst->cond);
	} else if (auto jump_inst = dynamic_cast<JumpInst *>(term_inst.get())) {
	} else {
		assert(0);
	}
}

static void pass_optim_exp_op(Block &block) {
	for (auto &inst : block.insts)
		pass_optim_exp_op(inst);
	pass_optim_exp_op(block.term_inst);
}

static void pass_optim_exp_op(Function &func) {
	for (auto &block : func.blocks.blocks)
		pass_optim_exp_op(*block);
}

void pass_optim_exp_op(Program &prog) {
	num_propagated = 0;
	num_replaced = 0;
	fprintf(stderr, "======== Optimization: Optimize Exp Ops ========\n");
	for (auto &func : prog.funcs)
		pass_optim_exp_op(*func);
	fprintf(stderr, "Propagated %d expressions\n", num_propagated);
	fprintf(stderr, "Replaced %d expressions\n", num_replaced);
}

}
