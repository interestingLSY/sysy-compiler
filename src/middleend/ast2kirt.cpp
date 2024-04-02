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

// Generate a block with an empty JumpInst terminal instruction
// This block is called as "unit block" since it behaves like a unit in the blocklist
std::shared_ptr<Block> get_unit_block(std::shared_ptr<Block> target = nullptr) {
	std::shared_ptr<Block> res = std::make_shared<Block>();
	res->term_inst = std::make_shared<JumpInst>();
	if (target) {
		dynamic_cast<JumpInst*>(res->term_inst.get())->target_block = target;
	}
	return res;
}

// Generate a block list with only one block, which has an empty JumpInst terminal instruction
BlockList get_unit_blocklist(std::shared_ptr<Block> target = nullptr) {
	BlockList res;
	res.blocks.push_back(get_unit_block(target));
	return res;
}

// Fill in the empty terminal instruction target in the victim block list
void fill_in_empty_terminst_target(BlockList& victim, std::shared_ptr<Block> &target) {
	for (const std::shared_ptr<Block> &block : victim.blocks) {
		assert (block->term_inst);
		TermInst* term_inst = block->term_inst.get();
		if (JumpInst *jump_inst = dynamic_cast<JumpInst*>(term_inst)) {
			if (!jump_inst->target_block) {
				jump_inst->target_block = target;
			}
		} else if (BranchInst *branch_inst = dynamic_cast<BranchInst*>(term_inst)) {
			if (!branch_inst->true_block) {
				branch_inst->true_block = target;
			}
			if (!branch_inst->false_block) {
				branch_inst->false_block = target;
			}
		} else if (ReturnInst *return_inst = dynamic_cast<ReturnInst*>(term_inst)) {
			// Do nothing
		} else {
			assert(0);
		}
	}
}

// Convert a AST::type_t to KIRT::type_t
type_t ast_type_t2kirt_type_t(AST::type_t ast_type_t) {
	switch (ast_type_t) {
		case AST::type_t::INT:
			return type_t::INT;
		default:
			assert(0);
	}
}

// Convert a binary AST::exp_t to KIRT::exp_t
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

// A counter for block naming
Counter block_id_counter;

// Function declarations
Program ast2kirt(const AST::TopLevel &top_level);
Function ast2kirt(const AST::FuncDef &func_def);
BlockList ast2kirt(const AST::Block &block);
BlockList ast2kirt(const AST::BlockItem &block_item);
BlockList ast2kirt(const AST::Stmt &stmt);
Block ast2kirt(const AST::VarDef &var_def);
shared_ptr<Exp> ast2kirt(const AST::Exp &exp);

Program ast2kirt(const AST::CompUnit &comp_unit) {
	return ast2kirt(*comp_unit.top_level);
}

Program ast2kirt(const AST::TopLevel &top_level) {
	block_id_counter.reset();
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
		program.funcs << sub_program.funcs;
		program.global_defs << sub_program.global_defs;
	}
	return program;
}

Function ast2kirt(const AST::FuncDef &func_def) {
	Function func;
	func.ret_type = ast_type_t2kirt_type_t(func_def.ret_type);
	func.name = func_def.ident;
	func.blocks = ast2kirt(*func_def.block);
	func.blocks.blocks.front()->name = "real_entry";	// The `entry` block performs some initialization, and "real_entry" is the real entry

	// Construct the epilogue block
	shared_ptr<ReturnInst> epilogue_block_ret_inst = std::make_shared<ReturnInst>();
	epilogue_block_ret_inst->ret_exp.type = exp_t::NUMBER;
	epilogue_block_ret_inst->ret_exp.number = 0;
	shared_ptr<Block> epilogue_block = get_unit_block();
	epilogue_block->name = "epilogue";
	epilogue_block->term_inst = epilogue_block_ret_inst;

	fill_in_empty_terminst_target(func.blocks, epilogue_block);
	func.blocks.blocks.push_back(epilogue_block);
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
		BlockList following_blocks = block_item.recur ? ast2kirt(*block_item.recur) : get_unit_blocklist();
		BlockList res = get_unit_blocklist(cur_blocks.blocks.front());	// Here we create a unit blocklist for convenience
		fill_in_empty_terminst_target(cur_blocks, following_blocks.blocks.front());
		
		string block_id = std::to_string(block_id_counter.next());
		cur_blocks.blocks.front()->name = "b_" + block_id;			// "block #id"
		following_blocks.blocks.front()->name = "ab_" + block_id;	// "after block #id"

		res.blocks << cur_blocks.blocks;
		res.blocks << following_blocks.blocks;
		return res;

	} else if (is_instance_of(cur_item, AST::VarDef*)) {
		// Variable definition
		AST::VarDef *var_def = dynamic_cast<AST::VarDef *>(cur_item);
		Block var_def_block = ast2kirt(*var_def);
		BlockList following_blocks = block_item.recur ? ast2kirt(*block_item.recur) : get_unit_blocklist();
		var_def_block.insts >> following_blocks.blocks.front()->insts;
		return following_blocks;

	} else if (is_instance_of(cur_item, AST::Stmt*)) {
		// A statement
		if (is_instance_of(cur_item, AST::NopStmt*)) {
			BlockList following_blocks = block_item.recur ? ast2kirt(*block_item.recur) : get_unit_blocklist();
			return following_blocks;
		}

		if (is_instance_of(cur_item, AST::ReturnStmt*)) {
			AST::ReturnStmt *return_stmt = dynamic_cast<AST::ReturnStmt *>(cur_item);
			shared_ptr<ReturnInst> return_inst = std::make_unique<ReturnInst>();
			return_inst->ret_exp = *ast2kirt(*return_stmt->ret_exp);
			BlockList res = get_unit_blocklist();
			res.blocks.front()->term_inst = std::move(return_inst);
			return res;
		}

		if (is_instance_of(cur_item, AST::AssignStmt*)) {
			AST::AssignStmt *assign_stmt = dynamic_cast<AST::AssignStmt *>(cur_item);
			shared_ptr<AssignInst> assign_inst = std::make_unique<AssignInst>();
			assign_inst->ident = reg_scope_renamer(assign_stmt->lval->ident);
			assign_inst->exp = *ast2kirt(*assign_stmt->exp);

			BlockList following_blocks = block_item.recur ? ast2kirt(*block_item.recur) : get_unit_blocklist();
			following_blocks.blocks.front()->insts.emplace_front(std::move(assign_inst));
			return following_blocks;
		}

		auto ast2kirt_stmt_or_block = [&](const std::unique_ptr<AST::Base> &stmt_or_block) -> BlockList {
			// A helper function for converting a statement or block to a block list
			// Useful in if() and while()
			reg_scope_renamer.on_enter_scope();
			BlockList block_list = ast2kirt(*dynamic_cast<AST::BlockItem *>(stmt_or_block.get()));
			reg_scope_renamer.on_exit_scope();
			return block_list;
		};
		if (is_instance_of(cur_item, AST::IfStmt*)) {
			AST::IfStmt *if_stmt = dynamic_cast<AST::IfStmt *>(cur_item);
			shared_ptr<BranchInst> branch_inst = std::make_unique<BranchInst>();

			// KIRT generation
			branch_inst->cond = *ast2kirt(*if_stmt->cond);
			BlockList true_blocks = ast2kirt_stmt_or_block(if_stmt->then);
			BlockList false_blocks = if_stmt->otherwise ? ast2kirt_stmt_or_block(if_stmt->otherwise) : get_unit_blocklist();
			BlockList following_blocks = block_item.recur ? ast2kirt(*block_item.recur) : get_unit_blocklist();

			// Naming
			string block_id = std::to_string(block_id_counter.next());
			true_blocks.blocks.front()->name = "true_" + block_id;
			false_blocks.blocks.front()->name = "false_" + block_id;
			following_blocks.blocks.front()->name = "aif_" + block_id;

			fill_in_empty_terminst_target(true_blocks, following_blocks.blocks.front());
			fill_in_empty_terminst_target(false_blocks, following_blocks.blocks.front());
			branch_inst->true_block = true_blocks.blocks.front();
			branch_inst->false_block = false_blocks.blocks.front();
			
			BlockList res = get_unit_blocklist();
			res.blocks.front()->term_inst = std::move(branch_inst);
			res.blocks << true_blocks.blocks;
			res.blocks << false_blocks.blocks;
			res.blocks << following_blocks.blocks;
			return res;
		}
		assert(0);
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
