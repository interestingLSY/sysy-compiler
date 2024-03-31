#pragma once

#include "middleend/kirt.h"

#include <list>
#include <string>
#include <memory>

namespace ASM {

using std::list;
using std::string;
using std::shared_ptr;

list<string> kirt2asm(const KIRT::Program &prog);

list<string> kirt2asm(const KIRT::Function &func);

list<string> kirt2asm(const KIRT::Block &block);

list<string> kirt2asm(const shared_ptr<KIRT::Inst> &inst);

}