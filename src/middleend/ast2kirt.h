#pragma once

#include "frontend/ast.h"
#include "middleend/kirt.h"

namespace KIRT {

Program ast2kirt(const AST::CompUnit &comp_unit);

}
