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

}
