#pragma once

#include "frontend/ast.h"
#include "middleend/kirt.h"

namespace KIRT {

type_t ast_type_t2kirt_type_t(type_t ast_type_t);

Program ast2kirt(const AST::CompUnit &comp_unit);

Program ast2kirt(const AST::TopLevel &top_level);

Function ast2kirt(const AST::FuncDef &func_def);

BlockList ast2kirt(const AST::Block &block);

BlockList ast2kirt(const AST::BlockBody &block_body);

BlockList ast2kirt(const AST::BlockItem &block_item);

BlockList ast2kirt(const AST::Stmt &stmt);

shared_ptr<Exp> ast2kirt(const AST::Exp &exp);

}
