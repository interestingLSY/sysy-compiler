#include "kirt2strkir.h"

#include <cassert>
#include <memory>

namespace KIRT {

string kirt_type_t2str(type_t type_t) {
	switch (type_t) {
		case type_t::INT:
			return "i32";
		default:
			assert(0);
	}
}

list<string> kirt2str(const Program &program) {
	list<string> res;
	for (const shared_ptr<Inst> &inst : program.global_defs) {
		list<string> inst_str = kirt2str(inst);
		res.splice(res.end(), inst_str);
	}
	for (const Function &func : program.funcs) {
		list<string> func_str = kirt2str(func);
		res.splice(res.end(), func_str);
	}
	return res;
}

list<string> kirt2str(const Function &func) {
	list<string> res;
	res.push_back("fun @" + func.name + "(): " + kirt_type_t2str(func.ret_type) + " {");
	res.push_back("\%entry:");
	list<string> blocks_str = kirt2str(func.blocks);
	res.splice(res.end(), blocks_str);
	res.push_back("}");
	return res;
}

list<string> kirt2str(const BlockList &assembling_blocks) {
	list<string> res;
	for (const Block &block : assembling_blocks.blocks) {
		list<string> block_str = kirt2str(block);
		res.splice(res.end(), block_str);
	}
	return res;
}

list<string> kirt2str(const Block &block) {
	list<string> res;
	for (const shared_ptr<Inst> &inst : block.insts) {
		list<string> inst_str = kirt2str(inst);
		res.splice(res.end(), inst_str);
	}
	return res;
}

list<string> kirt2str(const std::shared_ptr<Inst> &inst) {
	if (const ReturnInst *return_inst = dynamic_cast<ReturnInst*>(inst.get())) {
		return kirt2str(*return_inst);
	} else {
		assert(0);
	}

}

list<string> kirt2str(const ReturnInst &return_inst) {
	list<string> res;
	res.push_back("  ret " + std::to_string(return_inst.ret_val));
	return res;
}

}
