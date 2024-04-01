#pragma once

#include "middleend/kirt.h"

#include <list>
#include <memory>
#include <string>

namespace KIRT {

using std::list;
using std::pair;
using std::string;

list<string> kirt2str(const Program &program);

list<string> kirt2str(const Function &func);

list<string> kirt2str(const BlockList &assembling_blocks);

list<string> kirt2str(const Block &block);

list<string> kirt2str(const std::shared_ptr<Inst> &inst);

list<string> kirt2str(const ReturnInst &return_inst);

pair<list<string>, string> kirt2str(const Exp &exp);

}
