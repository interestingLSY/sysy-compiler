#include "kirt.h"

namespace KIRT {
	std::map<string, shared_ptr<Function>> func_map;
	std::map<string, std::shared_ptr<GlobalDecl>> global_decl_map;

	LVal LVal::clone() const {
		// auto new_lval = std::make_shared<LVal>();
		// new_lval->ident = ident;
		// new_lval->type = type;
		// for (auto &idx : indices) {
		// 	new_lval->indices.push_back(idx->clone());
		// }
		// return new_lval;
		auto new_lval = *this;
		for (auto &idx : new_lval.indices) {
			idx = std::make_shared<Exp>(idx->clone());
		}
		return new_lval;
	}

	Exp Exp::clone() const {
		auto new_exp = *this;
		for (auto &arg : new_exp.args) {
			arg = std::make_shared<Exp>(arg->clone());
		}
		if (lhs)
			new_exp.lhs = std::make_shared<Exp>(lhs->clone());
		if (rhs)
			new_exp.rhs = std::make_shared<Exp>(rhs->clone());
		return new_exp;
	}

	shared_ptr<Inst> AssignInst::clone() const {
		auto new_inst = std::make_shared<AssignInst>();
		new_inst->lval = lval.clone();
		new_inst->exp = exp.clone();
		return new_inst;
	}	

	shared_ptr<Inst> ExpInst::clone() const {
		auto new_inst = std::make_shared<ExpInst>();
		new_inst->exp = exp.clone();
		return new_inst;
	}

	shared_ptr<TermInst> BranchInst::clone() const {
		auto new_inst = std::make_shared<BranchInst>();
		new_inst->cond = cond.clone();
		new_inst->true_block = true_block;	// Don't clone blocks
		new_inst->false_block = false_block;
		return new_inst;
	}

	shared_ptr<TermInst> JumpInst::clone() const {
		auto new_inst = std::make_shared<JumpInst>();
		new_inst->target_block = target_block;
		return new_inst;
	}

	shared_ptr<TermInst> ReturnInst::clone() const {
		auto new_inst = std::make_shared<ReturnInst>();
		new_inst->ret_exp = std::make_shared<Exp>(ret_exp->clone());
		return new_inst;
	}

	Block Block::clone() const {
		auto new_block = *this;
		new_block.insts.clear();
		for (auto &inst : insts) {
			new_block.insts.push_back(inst->clone());
		}
		new_block.term_inst = term_inst->clone();
		return new_block;
	}

	BlockList BlockList::clone() const {
		auto new_block_list = *this;
		new_block_list.blocks.clear();
		for (auto &block : blocks) {
			new_block_list.blocks.push_back(
				std::make_shared<Block>(
					block->clone()
				)
			);
		}
		return new_block_list;
	}
}
