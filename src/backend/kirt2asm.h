#pragma once

#include "middleend/kirt.h"

#include <list>
#include <string>
#include <memory>

namespace ASM {

using std::list;
using std::pair;
using std::string;
using std::shared_ptr;

list<string> kirt2asm(const KIRT::Program &prog);

}