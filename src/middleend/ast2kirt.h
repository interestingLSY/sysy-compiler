#pragma once

#include "frontend/ast.h"
#include "middleend/kirt.h"

namespace KIRT {

Program ast2kirt(AST::CompUnit &comp_unit);

}
