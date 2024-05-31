// Pass Fill Block Id And Name: Generate a block id for every block and
// a name for every unnamedblock

#include "pass_fill_block_id_name.h"

#include <cassert>

#include "utils/utils.h"

namespace KIRT {

static Counter block_id_counter;

static void pass_fill_block_id_name(Function &func) {
	block_id_counter.reset();
	for (auto &block : func.blocks.blocks) {
		block->id = block_id_counter.next();
		if (block->name.empty())
			block->name = "b" + std::to_string(block->id);
	}
}

void pass_fill_block_id_name(Program &prog) {
	for (auto &func : prog.funcs)
		pass_fill_block_id_name(*func);
}

}
