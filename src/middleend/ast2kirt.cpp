#include "ast2kirt.h"

#include <algorithm>
#include <cassert>
#include <map>
#include <vector>

#include "utils/utils.h"

namespace KIRT {

// The register scope renamer
// With the introduction of scoping, variable from different scopes may have the same name
// To avoid confusion, we rename the variables to a unique name:
//	@original_name_lvlevel
// Each time we enter a new scope, we increment the level. When we leave the
// scope, we decrement the level.
class RegScopeRenamer {
private:
	int cur_lvl = 0;	// Current level
	std::map<std::string, std::vector<int>> orig_name2defined_at_lvls;

public:
	// Called when entering a new scope
	void on_enter_scope() {
		cur_lvl += 1;
	}

	// Called when leaving a scope
	void on_exit_scope() {
		for (auto &[orig_name, defined_at_lvls] : orig_name2defined_at_lvls) {
			while (!defined_at_lvls.empty() && defined_at_lvls.back() == cur_lvl) {
				defined_at_lvls.pop_back();
			}
		}
		cur_lvl -= 1;
	}

	// Called when defining a new variable
	void on_define_var(string orig_name) {
		// printf("on_define_var: %s at %d\n", orig_name.c_str(), cur_lvl);
		if (!orig_name2defined_at_lvls[orig_name].empty() &&
			orig_name2defined_at_lvls[orig_name].back() == cur_lvl) {
			printf("Variable %s redefined at level %d\n", orig_name.c_str(), cur_lvl);
			assert(0);
		}
		orig_name2defined_at_lvls[orig_name].push_back(cur_lvl);
	}

	// Rename a variable
	string operator()(string orig_name) {
		// printf("Attempting to rename %s\n", orig_name.c_str());
		assert (orig_name2defined_at_lvls.count(orig_name) != 0);
		int target_lvl = orig_name2defined_at_lvls[orig_name].back();
		// printf("rename: %s to %d\n", orig_name.c_str(), target_lvl);
		return "@" + orig_name + "_lv" + std::to_string(target_lvl);
	}
} reg_scope_renamer;


Program ast2kirt(const AST::TopLevel &top_level);
Function ast2kirt(const AST::FuncDef &func_def);
BlockList ast2kirt(const AST::Block &block);
BlockList ast2kirt(const AST::BlockItem &block_item);
BlockList ast2kirt(const AST::Stmt &stmt);
Block ast2kirt(const AST::VarDef &var_def);
shared_ptr<Exp> ast2kirt(const AST::Exp &exp);

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
	func.ret_type = ast_type_t2kirt_type_t(func_def.ret_type);
	func.name = func_def.ident;
	func.blocks = ast2kirt(*func_def.block);
	return func;
}

BlockList ast2kirt(const AST::Block &block) {
	reg_scope_renamer.on_enter_scope();
	auto result = ast2kirt(*block.item);
	reg_scope_renamer.on_exit_scope();
	return result;
}

BlockList ast2kirt(const AST::BlockItem &block_item) {
	AST::Base* cur_item = block_item.item.get();	// Can be a statement / declaration / Block

	if (is_instance_of(cur_item, AST::Block*)) {
		BlockList cur_blocks = ast2kirt(*dynamic_cast<AST::Block *>(cur_item));
		BlockList following_blocks = block_item.recur ? ast2kirt(*block_item.recur) : BlockList();
		// Recall that the first block of following_blocks is the entrypoint, while
		// the last block of cur_blocks is the exitpoint
		// So we merge the two blocks
		cur_blocks.blocks.back().insts.splice(cur_blocks.blocks.back().insts.end(), following_blocks.blocks.front().insts);
		following_blocks.blocks.pop_front();
		cur_blocks.blocks.splice(cur_blocks.blocks.end(), following_blocks.blocks);
		return cur_blocks;

	} else if (is_instance_of(cur_item, AST::VarDef*)) {
		// Variable definition
		AST::VarDef *var_def = dynamic_cast<AST::VarDef *>(block_item.item.get());
		Block var_def_block = ast2kirt(*var_def);
		BlockList following_blocks = block_item.recur ? ast2kirt(*block_item.recur) : BlockList();
		// Merge the current var_def_block, with the following_blocks
		following_blocks.blocks.front().insts.splice(
			following_blocks.blocks.front().insts.begin(),
			var_def_block.insts
		);
		return following_blocks;
	} else if (is_instance_of(cur_item, AST::Stmt*)) {
		// A statement
		if (is_instance_of(block_item.item.get(), AST::NopStmt*)) {
			// Do nothing
			BlockList following_blocks = block_item.recur ? ast2kirt(*block_item.recur) : BlockList();
			return following_blocks;
		}
		shared_ptr<Inst> new_inst;
		if (is_instance_of(cur_item, AST::ReturnStmt*)) {
			AST::ReturnStmt *return_stmt = dynamic_cast<AST::ReturnStmt *>(block_item.item.get());
			shared_ptr<ReturnInst> return_inst = std::make_unique<ReturnInst>();
			return_inst->ret_exp = *ast2kirt(*return_stmt->ret_exp);
			new_inst = std::move(return_inst);
		} else if (is_instance_of(block_item.item.get(), AST::AssignStmt*)) {
			AST::AssignStmt *assign_stmt = dynamic_cast<AST::AssignStmt *>(block_item.item.get());
			shared_ptr<AssignInst> assign_inst = std::make_unique<AssignInst>();
			assign_inst->ident = reg_scope_renamer(assign_stmt->lval->ident);
			assign_inst->exp = *ast2kirt(*assign_stmt->exp);
			new_inst = std::move(assign_inst);
		} else {
			assert(0);
		}
		BlockList following_blocks = block_item.recur ? ast2kirt(*block_item.recur) : BlockList();
		following_blocks.blocks.front().insts.emplace_front(std::move(new_inst));
		return following_blocks;
	} else {
		assert(0);
	}

	assert(0);
}

Block ast2kirt(const AST::VarDef &var_def) {
	reg_scope_renamer.on_define_var(var_def.ident);
	shared_ptr<AssignInst> assign_inst;
	if (var_def.init_val) {
		assign_inst = std::make_unique<AssignInst>();
		assign_inst->ident = reg_scope_renamer(var_def.ident);
		assign_inst->exp = *ast2kirt(*var_def.init_val);
	}
	Block res = var_def.recur ? ast2kirt(*var_def.recur) : Block();
	if (assign_inst) {
		res.insts.emplace_front(std::move(assign_inst));
	}
	return res;
}

shared_ptr<Exp> ast2kirt(const AST::Exp &exp) {
	shared_ptr<Exp> kirt_exp = std::make_shared<Exp>();
	switch (exp.type) {
		case AST::exp_t::NUMBER:
			kirt_exp->type = exp_t::NUMBER;
			kirt_exp->number = exp.number;
			break;
		case AST::exp_t::LVAL:
			kirt_exp->type = exp_t::LVAL;
			kirt_exp->ident = reg_scope_renamer(exp.lval.get()->ident);
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
