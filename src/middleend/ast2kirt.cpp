#include "ast2kirt.h"

#include <cassert>

#include "utils/utils.h"

namespace KIRT {

type_t ast_type_t2kirt_type_t(AST::type_t ast_type_t) {
	switch (ast_type_t) {
		case AST::type_t::INT:
			return type_t::INT;
		default:
			assert(0);
	}
}

Program ast2kirt(const AST::CompUnit &comp_unit) {
	return ast2kirt(*comp_unit.top_level);
}

Program ast2kirt(const AST::TopLevel &top_level) {
	Program program;
	if (is_instance_of(top_level.def.get(), AST::FuncDef*)) {
		Function func = ast2kirt(*dynamic_cast<AST::FuncDef *>(top_level.def.get()));
		program.funcs.emplace_back(func);
	} else {
		// NOTE def may be a declaration of const/var, will be implemented in the future
		assert(0);
	}

	if (top_level.recur) {
		Program sub_program = ast2kirt(*top_level.recur);
		program.funcs.splice(program.funcs.end(), sub_program.funcs);
		program.global_defs.splice(program.global_defs.end(), sub_program.global_defs);
	}
	return program;
}

Function ast2kirt(const AST::FuncDef &func_def) {
	Function func;
	func.ret_type = ast_type_t2kirt_type_t(func_def.ret_type->type);
	func.name = func_def.ident;
	func.blocks = ast2kirt(*func_def.block);
	return func;
}

BlockList ast2kirt(const AST::Block &block) {
	return ast2kirt(*block.body);
}

BlockList ast2kirt(const AST::BlockBody &block_body) {
	assert(!block_body.recur);
	assert(is_instance_of(block_body.item.get(), AST::ReturnStmt*));
	AST::ReturnStmt *return_stmt = dynamic_cast<AST::ReturnStmt *>(block_body.item.get());
	BlockList assembling_blocks;
	Block block;
	shared_ptr<ReturnInst> return_inst = std::make_unique<ReturnInst>();
	return_inst->ret_val = return_stmt->number;
	block.insts.emplace_back(return_inst);
	assembling_blocks.blocks.push_back(block);
	return assembling_blocks;
}

}
