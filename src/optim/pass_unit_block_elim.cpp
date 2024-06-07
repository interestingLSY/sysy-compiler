// Pass Unit Block Elimination: Eliminate unit blocks (blocks containing no
// instructions and only a "jump" terminal instruction) in the function

#include "pass_unit_block_elim.h"

#include <cassert>
#include <unordered_map>

#include "utils/utils.h"

namespace KIRT {

static void pass_unit_block_elim(Function &func) {
	int num_blocks = func.blocks.blocks.size();

	vector<shared_ptr<Block>> id2block(num_blocks);
	for (auto &block : func.blocks.blocks) {
		id2block[block->id] = block;
	}

	// Step 1. Mark all unit blocks
	vector<bool> is_unit_block(num_blocks, false);
	for (auto &block : func.blocks.blocks) {
		if (block->insts.empty()) {
			TermInst* term_inst_ptr = block->term_inst.get();
			if (const JumpInst *jump_inst = dynamic_cast<JumpInst*>(term_inst_ptr)) {
				is_unit_block[block->id] = true;
			}
		}
	}
	is_unit_block[func.blocks.blocks.front()->id] = false; // The first block cannot be a unit block
	is_unit_block[func.blocks.blocks.back()->id] = false; // The last block cannot be a unit block

	// Step 2. Eliminate unit blocks
	auto redirect_block_ptr = [&](shared_ptr<Block> &ptr) {
		while (is_unit_block[ptr->id]) {
			TermInst* term_inst_ptr = ptr->term_inst.get();
			if (const JumpInst *jump_inst = dynamic_cast<JumpInst*>(term_inst_ptr)) {
				ptr = jump_inst->target_block;
			} else {
				assert(false);
			}
		}
	};
	for (auto &block : func.blocks.blocks) {
		if (is_unit_block[block->id]) continue;
		TermInst* term_inst_ptr = block->term_inst.get();
		if (JumpInst *jump_inst = dynamic_cast<JumpInst*>(term_inst_ptr)) {
			redirect_block_ptr(jump_inst->target_block);
		} else if (BranchInst *branch_inst = dynamic_cast<BranchInst*>(term_inst_ptr)) {
			redirect_block_ptr(branch_inst->true_block);
			redirect_block_ptr(branch_inst->false_block);
		} else if (const ReturnInst *return_inst = dynamic_cast<ReturnInst*>(term_inst_ptr)) {
			// Do nothing
		} else {
			assert(false);
		}
	}

	// Step 3. Erase all unit blocks
	list<shared_ptr<Block>> new_blocks;
	for (int i = 0; i < num_blocks; i++) {
		if (!is_unit_block[i]) {
			new_blocks.push_back(id2block[i]);
		}
	}
	func.blocks.blocks = new_blocks;

	fprintf(stderr, "Function `%10s`: %d -> %lu (%.1f%%)\n",
			func.name.c_str(), num_blocks, func.blocks.blocks.size(),
			100.0 * func.blocks.blocks.size() / num_blocks);
}

void pass_unit_block_elim(Program &prog) {
	fprintf(stderr, "======== Optimization: Unit Block Elimination ========\n");
	for (auto &func : prog.funcs)
		pass_unit_block_elim(*func);
}

}
