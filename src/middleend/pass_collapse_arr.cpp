// Pass Collapse Array: Collapse multi-dimensional array to one-dimensional array,
// and convert pointer arithmetics to addition.
// 
// After this conversion, an LVAL will have three types:
// 
// - INT (an integer variable)
// - ARR (an element from an one-dimensional array)
// - ARR_ADDR (the address of an one-dimensional array)
//
// For example, the following code
//
// int a[2][3][4];
// f(a[1]);
// g(a[1][2][3]);
//
// Will be converted to
// 
// int a[24];
// f(a + 12);
// g(a[23]);
// 

#include "pass_collapse_arr.h"

#include <cassert>

namespace KIRT {

static void pass_collapse_arr(Exp &exp);

static LVal collapse_lval(const LVal &lval) {
	assert(lval.is_arr());
	// Collapse this array
	shared_ptr<Exp> final_exp = nullptr;
	for (int dim = 0; dim < lval.indices.size(); ++dim) {
		shared_ptr<Exp> index_exp = lval.indices[dim];
		int cur_stride = lval.type.stride(dim);
		if (cur_stride != 1) {
			shared_ptr<Exp> new_index_exp = std::make_shared<Exp>();
			new_index_exp->type = exp_t::MUL;
			new_index_exp->lhs = std::make_shared<Exp>(cur_stride);
			new_index_exp->rhs = index_exp;
			index_exp = new_index_exp;
		}
		if (final_exp == nullptr) {
			final_exp = index_exp;
		} else {
			shared_ptr<Exp> new_exp = std::make_shared<Exp>();
			new_exp->type = exp_t::ADD;
			new_exp->lhs = final_exp;
			new_exp->rhs = index_exp;
			final_exp = new_exp;
		}
	}
	if (!final_exp) {
		// No indices found. This is a pointer to the first element
		// Forge a 0 index
		final_exp = std::make_shared<Exp>(0);
	}
	pass_collapse_arr(*final_exp);
	LVal new_lval = LVal::make_arr(lval.ident, {lval.type.numel()}, {final_exp});
	return new_lval;
}

static void pass_collapse_arr(Exp &exp) {
	if (exp.type == exp_t::LVAL) {
		if (exp.lval.is_arr()) {
			if (exp.lval.indices.size() == exp.lval.type.dims()) {
				// The number of indices matches the number of dimensions
				exp.lval = collapse_lval(exp.lval);
			} else {
				// Otherwise
				LVal new_lval = collapse_lval(exp.lval);
				exp.type = exp_t::ADDR_ADD;
				exp.lhs = std::make_shared<Exp>();
				exp.lhs->type = exp_t::ARR_ADDR;
				exp.lhs->arr_name = new_lval.ident;
				exp.rhs = new_lval.indices[0];	// TODO mul 4
			}
		}
	}
	if (exp.lhs)
		pass_collapse_arr(*exp.lhs);
	if (exp.rhs)
		pass_collapse_arr(*exp.rhs);
	if (!exp.args.empty())
		for (auto &arg : exp.args)
			pass_collapse_arr(*arg);
}

static void pass_collapse_arr(shared_ptr<Inst> &inst) {
	if (auto assign_inst = dynamic_cast<AssignInst *>(inst.get())) {
		pass_collapse_arr(assign_inst->exp);
		if (assign_inst->lval.is_arr()) {
			assign_inst->lval = collapse_lval(assign_inst->lval);
		}
	} else if (auto exp_inst = dynamic_cast<ExpInst *>(inst.get())) {
		pass_collapse_arr(exp_inst->exp);
	} else {
		assert(0);
	}
}

static void pass_collapse_arr(shared_ptr<TermInst> &term_inst) {
	if (auto ret_inst = dynamic_cast<ReturnInst *>(term_inst.get())) {
		if (ret_inst->ret_exp)
			pass_collapse_arr(*ret_inst->ret_exp);
	} else if (auto br_inst = dynamic_cast<BranchInst *>(term_inst.get())) {
		pass_collapse_arr(br_inst->cond);
	} else if (auto jump_inst = dynamic_cast<JumpInst *>(term_inst.get())) {
	} else {
		assert(0);
	}
}

static void pass_collapse_arr(Block &block) {
	for (auto &inst : block.insts)
		pass_collapse_arr(inst);
	pass_collapse_arr(block.term_inst);
}

static void pass_collapse_arr(Function &func) {
	for (auto &fparam : func.fparams)
		if (fparam.type.is_arr() && fparam.type.dims() > 1)
			fparam.type.shape = {fparam.type.numel()};
	for (auto &local_var : func.local_vars)
		if (local_var.type.is_arr() && local_var.type.dims() > 1)
			local_var.type.shape = {local_var.type.numel()};
	for (auto &block : func.blocks.blocks)
		pass_collapse_arr(*block);
}

void pass_collapse_arr(Program &prog) {
	for (auto &func : prog.funcs)
		pass_collapse_arr(*func);
	for (auto &global_decl : prog.global_decls)
		if (global_decl->type.is_arr() && global_decl->type.dims() > 1)
			global_decl->type.shape = {global_decl->type.numel()};
}

}
