#include "backend/kirt2asm.h"

namespace ASM {

list<string> kirt2asm(const KIRT::Program &prog) {
	list<string> res;
	res.push_back(".text");
	res.push_back(".globl main");

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
		res.push_back("  li a0, ");
		res.push_back("  ret");
	}

	return res;
}

}