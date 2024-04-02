#pragma once

#include "middleend/kirt.h"

#include <list>
#include <string>
#include <memory>

namespace ASM {

std::list<std::string> kirt2asm(const KIRT::Program &prog);

}