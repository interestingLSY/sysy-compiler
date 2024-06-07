// Pass Block Fusion: Merge blocks to reduce the number of jumps

#include "pass_block_fusion.h"

#include <cassert>
#include <unordered_map>

#include "utils/utils.h"

namespace KIRT {

static void pass_block_fusion(Function &func, bool avoid_while) {
	int num_blocks = func.blocks.blocks.size();

	vector<shared_ptr<Block>> id2block(num_blocks);
	for (auto &block : func.blocks.blocks) {
		id2block[block->id] = block;
	}

	// Step 1. Construct the graph
	vector<vector<int>> outgoing_edges(num_blocks);
	vector<vector<int>> incoming_edges(num_blocks);
	auto add_edge = [&outgoing_edges, &incoming_edges](int src, int dst) {
		outgoing_edges[src].push_back(dst);
		incoming_edges[dst].push_back(src);
	};
	for (auto &block : func.blocks.blocks) {
		TermInst* term_inst_ptr = block->term_inst.get();
		if (const ReturnInst *return_inst = dynamic_cast<ReturnInst*>(term_inst_ptr)) {
			// Do nothing
		} else if (const JumpInst *jump_inst = dynamic_cast<JumpInst*>(term_inst_ptr)) {
			add_edge(block->id, jump_inst->target_block->id);
		} else if (const BranchInst *branch_inst = dynamic_cast<BranchInst*>(term_inst_ptr)) {
			// Here two edges will be added even if `true` and `false` point
			// to the same block. This prevent the block from being merged.
			// However this is necessary since `cond` in `BranchInst` may
			// contain side effects.
			add_edge(block->id, branch_inst->true_block->id);
			add_edge(block->id, branch_inst->false_block->id);
		} else {
			assert(0);
		}
	}

	// Step 2. Generate "merge_prev" (merge with the previous block?) and
	// "merge_next" (merge with the next block?) label
	vector<bool> merge_prev(num_blocks, false);
	vector<bool> merge_next(num_blocks, false);
	for (int i = 0; i < num_blocks; i++) {
		if (outgoing_edges[i].size() == 1) {
			int v = outgoing_edges[i][0];
			if (incoming_edges[v].size() == 1) {
				if (avoid_while && (is_start_with(id2block[i]->name, "while") || is_start_with(id2block[v]->name, "while")))
					continue;
				merge_next[i] = true;
				merge_prev[v] = true;
			}
		}
	}

	// Step 3. Now all markers on the graph forms a set of disjoint chains
	// For each chain, we start from the "head" block, traverse along the chain,
	// gather instructions from all the blocks, and merge them into a larger
	// block
	vector<bool> is_deleted(num_blocks, false);
	for (int i = 0; i < num_blocks; i++) {
		if (!merge_prev[i] && merge_next[i]) {
			// This block is a header block
			// Gather the chain
			vector<int> chain;
			int cur = i;
			while (merge_next[cur]) {
				chain.push_back(cur);
				cur = outgoing_edges[cur][0];
			}
			chain.push_back(cur);

			// Gather all non-term instructions
			auto &head_block = id2block[chain[0]];
			for (int j = 1; j < chain.size(); j++) {
				auto &cur_block = id2block[chain[j]];
				head_block->insts << cur_block->insts;
			}

			// Set the term inst as the term inst of the last block
			head_block->term_inst = id2block[chain.back()]->term_inst;

			// Erase all blocks except the head block
			for (int j = 1; j < chain.size(); j++) {
				is_deleted[chain[j]] = true;
			}
		}
	}

	// Step 4. Erase all deleted blocks
	list<shared_ptr<Block>> new_blocks;
	for (int i = 0; i < num_blocks; i++) {
		if (!is_deleted[i]) {
			new_blocks.push_back(id2block[i]);
		}
	}
	func.blocks.blocks = new_blocks;

	fprintf(stderr, "Function `%10s`: %d -> %lu (%.1f%%)\n",
			func.name.c_str(), num_blocks, func.blocks.blocks.size(),
			100.0 * func.blocks.blocks.size() / num_blocks);
}

void pass_block_fusion(Program &prog, bool avoid_while) {
	fprintf(stderr, "======== Optimization: Block Fusion%s ========\n", avoid_while ? " (Avoid While)" : "");
	for (auto &func : prog.funcs)
		pass_block_fusion(*func, avoid_while);
}

}
