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

exp_t ast_binary_exp_t2kirt_exp_r(AST::exp_t ast_type_t) {
	assert (AST::is_exp_t_binary(ast_type_t));
	switch (ast_type_t) {
		case AST::exp_t::ADD:
			return exp_t::ADD;
		case AST::exp_t::SUB:
			return exp_t::SUB;
		case AST::exp_t::MUL:
			return exp_t::MUL;
		case AST::exp_t::DIV:
			return exp_t::DIV;
		case AST::exp_t::REM:
			return exp_t::REM;
		case AST::exp_t::LT:
			return exp_t::LT;
		case AST::exp_t::GT:
			return exp_t::GT;
		case AST::exp_t::LEQ:
			return exp_t::LEQ;
		case AST::exp_t::GEQ:
			return exp_t::GEQ;
		case AST::exp_t::EQ:
			return exp_t::EQ;
		case AST::exp_t::NEQ:
			return exp_t::NEQ;
		default:
			// Some exp_t, like LOGICAL_AND/OR, does not have a corresponding exp_t in KIRT
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
	return_inst->ret_exp = *ast2kirt(*return_stmt->ret_exp);

	block.insts.emplace_back(return_inst);
	assembling_blocks.blocks.push_back(block);
	return assembling_blocks;
}

shared_ptr<Exp> ast2kirt(const AST::Exp &exp) {
	shared_ptr<Exp> kirt_exp = std::make_shared<Exp>();
	switch (exp.type) {
		case AST::exp_t::NUMBER:
			kirt_exp->type = exp_t::NUMBER;
			kirt_exp->number = exp.number;
			break;
		case AST::exp_t::NEGATIVE:
			kirt_exp->type = exp_t::SUB;
			kirt_exp->lhs = std::make_shared<Exp>(0);
			kirt_exp->rhs = ast2kirt(*exp.rhs);
			break;
		case AST::exp_t::LOGICAL_NOT:
			kirt_exp->type = exp_t::EQ;
			kirt_exp->lhs = ast2kirt(*exp.rhs);
			kirt_exp->rhs = std::make_shared<Exp>(0);
			break;
		case AST::exp_t::LOGICAL_AND: {
			// a && b = (a!=0) & (b!=0)
			shared_ptr<Exp> a = ast2kirt(*exp.lhs);
			shared_ptr<Exp> b = ast2kirt(*exp.rhs);
			shared_ptr<Exp> a_neq_0 = std::make_shared<Exp>();
			a_neq_0->type = exp_t::NEQ;
			a_neq_0->lhs = a;
			a_neq_0->rhs = std::make_shared<Exp>(0);
			shared_ptr<Exp> b_neq_0 = std::make_shared<Exp>();
			b_neq_0->type = exp_t::NEQ;
			b_neq_0->lhs = b;
			b_neq_0->rhs = std::make_shared<Exp>(0);
			kirt_exp->type = exp_t::BITWISE_AND;
			kirt_exp->lhs = a_neq_0;
			kirt_exp->rhs = b_neq_0;
			break;
		}
		case AST::exp_t::LOGICAL_OR: {
			// a || b = (a!=0) | (b!=0)
			shared_ptr<Exp> a = ast2kirt(*exp.lhs);
			shared_ptr<Exp> b = ast2kirt(*exp.rhs);
			shared_ptr<Exp> a_neq_0 = std::make_shared<Exp>();
			a_neq_0->type = exp_t::NEQ;
			a_neq_0->lhs = a;
			a_neq_0->rhs = std::make_shared<Exp>(0);
			shared_ptr<Exp> b_neq_0 = std::make_shared<Exp>();
			b_neq_0->type = exp_t::NEQ;
			b_neq_0->lhs = b;
			b_neq_0->rhs = std::make_shared<Exp>(0);
			kirt_exp->type = exp_t::BITWISE_OR;
			kirt_exp->lhs = a_neq_0;
			kirt_exp->rhs = b_neq_0;
			break;
		}
		default:
			// These exp_t can be easily converted to exp_t in KIRT by a simple one-to-one mapping
			kirt_exp->type = ast_binary_exp_t2kirt_exp_r(exp.type);
			kirt_exp->lhs = ast2kirt(*exp.lhs);
			kirt_exp->rhs = ast2kirt(*exp.rhs);
			break;
	}
	return kirt_exp;
}

}
