// Pass Fill Block Name: generate a random block name for every unnamed block

#include "pass_fill_block_name.h"

#include <cassert>

#include "utils/utils.h"

namespace KIRT {

Counter block_name_counter;

static void pass_fill_block_name(Block &block) {
	if (block.name.empty())
		block.name = "block_" + std::to_string(block_name_counter.next());	
}

static void pass_fill_block_name(Function &func) {
	for (auto &block : func.blocks.blocks)
		pass_fill_block_name(*block);
}

void pass_fill_block_name(Program &prog) {
	for (auto &func : prog.funcs)
		pass_fill_block_name(*func);
}

}
