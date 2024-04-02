// Remove Unreachable - Remove unreachable instructions
// Now it only removes instructions after `ret` in the same block

#include "pass_remove_unreachable.h"

namespace KIRT {

static void pass_remove_unreachable(Block &block) {
	bool ret_found = false;
	for (auto iter = block.insts.begin(); iter != block.insts.end(); ) {
		auto &inst = *iter;
		if (ret_found) {
			iter = block.insts.erase(iter);
			continue;
		}
		if (auto ret_inst = dynamic_cast<ReturnInst *>(inst.get())) {
			ret_found = true;
		}
		iter = std::next(iter);
	}
}

static void pass_remove_unreachable(Function &func) {
	for (auto &block : func.blocks.blocks)
		pass_remove_unreachable(block);
}

void pass_remove_unreachable(Program &prog) {
	for (auto &func : prog.funcs)
		pass_remove_unreachable(func);
}

}
