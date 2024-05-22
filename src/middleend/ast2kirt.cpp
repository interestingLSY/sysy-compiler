#include "ast2kirt.h"

#include <algorithm>
#include <cassert>
#include <map>
#include <optional>
#include <vector>

#include "utils/utils.h"

namespace KIRT {

using std::pair;

// The register scope renamer
// With the introduction of scoping, variable from different scopes may have the same name
// To avoid confusion, we rename the variables to a unique name:
//	@<original_name>_lv<level>
// Each time we enter a new scope, we increment the level. When we leave the
// scope, we decrement the level.
// TODO This renamer is inefficient. Redesign the algorithm
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
		if (!orig_name2defined_at_lvls[orig_name].empty() &&
			orig_name2defined_at_lvls[orig_name].back() == cur_lvl) {
			printf("Variable %s redefined at level %d\n", orig_name.c_str(), cur_lvl);
			assert(0);
		}
		orig_name2defined_at_lvls[orig_name].push_back(cur_lvl);
	}

	// Rename a variable
	string operator()(string orig_name) {
		assert (orig_name2defined_at_lvls.count(orig_name) != 0);
		int target_lvl = orig_name2defined_at_lvls[orig_name].back();
		return "@" + orig_name + "_lv" + std::to_string(target_lvl);
	}
} reg_scope_renamer;

// Generate a block with an empty JumpInst terminal instruction
// This block is called as "unit block" since it behaves like a unit in the blocklist
shared_ptr<Block> get_unit_block(std::shared_ptr<Block> target = nullptr) {
	shared_ptr<Block> res = std::make_shared<Block>();
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

shared_ptr<AssignInst> get_assign_inst(const string &ident, const Exp &exp) {
	shared_ptr<AssignInst> res = std::make_shared<AssignInst>();
	res->ident = ident;
	res->exp = exp;
	return res;
}

shared_ptr<JumpInst> get_jump_inst(const std::shared_ptr<Block> target_block, jump_inst_t type = jump_inst_t::NORMAL) {
	shared_ptr<JumpInst> res = std::make_shared<JumpInst>();
	res->target_block = target_block;
	res->type = type;
	return res;
}

shared_ptr<BranchInst> get_branch_inst(const Exp &cond, const std::shared_ptr<Block> true_block, const std::shared_ptr<Block> false_block) {
	shared_ptr<BranchInst> res = std::make_shared<BranchInst>();
	res->cond = cond;
	res->true_block = true_block;
	res->false_block = false_block;
	return res;
}

// Fill in the empty terminal instruction target in the victim block list
// If the terminal instruction is a JumpInst, its type must match the given type
void fill_in_empty_terminst_target(BlockList& victim, std::shared_ptr<Block> &target, jump_inst_t jump_inst_type = jump_inst_t::NORMAL) {
	for (const std::shared_ptr<Block> &block : victim.blocks) {
		assert (block->term_inst);
		TermInst* term_inst = block->term_inst.get();
		if (JumpInst *jump_inst = dynamic_cast<JumpInst*>(term_inst)) {
			if (jump_inst->type == jump_inst_type && !jump_inst->target_block) {
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
		case AST::type_t::VOID:
			return type_t::VOID;
		default:
			assert(0);
	}
}

// Convert a binary AST::exp_t to KIRT::exp_t
exp_t ast_binary_exp_t2kirt_exp_t(AST::exp_t ast_type_t) {
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
Function ast2kirt(AST::FuncDef &func_def, BlockList global_decl_blocks);
BlockList ast2kirt(AST::Block &block);
BlockList ast2kirt(AST::BlockItem &block_item);
BlockList ast2kirt(AST::VarDef &var_def);
pair<shared_ptr<Exp>, BlockList> ast2kirt(AST::Exp &exp);
std::pair<shared_ptr<GlobalDecl>, BlockList> ast2kirt_global_decl(AST::VarDef &var_def);

Program ast2kirt(AST::CompUnit &comp_unit) {
	block_id_counter.reset();
	// Register library functions
	auto register_library_function = [&](string name, vector<type_t> args_type, type_t ret_type) {
		shared_ptr<Function> func_ptr = std::make_shared<Function>();
		func_ptr->ret_type = ret_type;
		func_ptr->name = name;
		for (type_t arg_type : args_type) {
			func_ptr->fparams.push_back(FuncFParam {arg_type, "arg"});
		}
		KIRT::func_map[name] = func_ptr;
	};
	register_library_function("getint", {}, type_t::INT);
	register_library_function("getch", {}, type_t::INT);
	// register_library_function("getarray", {??}, type_t::INT);
	register_library_function("putint", {type_t::INT}, type_t::VOID);
	register_library_function("putch", {type_t::INT}, type_t::VOID);
	// register_library_function("putarray", {??}, type_t::VOID);
	register_library_function("starttime", {}, type_t::VOID);
	register_library_function("stoptime", {}, type_t::VOID);

	vector<AST::VarDef*> global_decls;
	vector<AST::FuncDef*> funcs;
	std::unique_ptr<AST::TopLevel>* cur_item = &comp_unit.top_level;
	while (cur_item->get()) {
		if (is_instance_of((*cur_item)->def.get(), AST::VarDef*)) {
			AST::VarDef* cur_var_def = dynamic_cast<AST::VarDef *>((*cur_item)->def.get());
			while (cur_var_def) {
				global_decls.push_back(cur_var_def);
				cur_var_def = cur_var_def->recur.get();
			}
		} else if (is_instance_of((*cur_item)->def.get(), AST::FuncDef*)) {
			funcs.push_back(dynamic_cast<AST::FuncDef *>((*cur_item)->def.get()));
		} else {
			assert(0);
		}
		cur_item = &(*cur_item)->recur;
	}

	Program program;

	// Get blocklists for initializing global variables
	// There blocks will be inserted before `main()`
	BlockList global_decl_blocks = get_unit_blocklist();
	for (AST::VarDef *var_def : global_decls) {
		auto [global_decl, blocklist] = ast2kirt_global_decl(*var_def);
		program.global_decls.push_back(global_decl);
		fill_in_empty_terminst_target(global_decl_blocks, blocklist.blocks.front());
		global_decl_blocks.blocks << blocklist.blocks;
		KIRT::global_decl_map[global_decl->ident] = global_decl;
	}

	for (AST::FuncDef *func_def : funcs) {
		Function func = ast2kirt(*func_def, global_decl_blocks);
		shared_ptr<Function> func_ptr = std::make_shared<Function>(func);
		program.funcs.emplace_back(func_ptr);
		KIRT::func_map[func.name] = func_ptr;
	}
	
	return program;
}

// Convert a global variable declaration to KIRT
// Return (global_decl, blocklist)
// The blocklist contains zero or one assignment instruction, depending on
// whether the variable is initialized
std::pair<shared_ptr<GlobalDecl>, BlockList> ast2kirt_global_decl(AST::VarDef &var_def) {
	reg_scope_renamer.on_define_var(var_def.ident);
	shared_ptr<GlobalDecl> global_decl = std::make_shared<GlobalDecl>();
	global_decl->type = KIRT::type_t::INT;	// TODO Support array
	global_decl->ident = reg_scope_renamer(var_def.ident);
	if (var_def.init_val) {
		auto [init_val_exp, init_val_blocks] = ast2kirt(*var_def.init_val);
		init_val_blocks.blocks.front()->name = "init_" + global_decl->ident.substr(1);

		shared_ptr<AssignInst> assign_inst = get_assign_inst(global_decl->ident, *init_val_exp);
		shared_ptr<Block> assign_block = get_unit_block();
		assign_block->name = "init_assign_" + global_decl->ident.substr(1);
		assign_block->insts.push_back(assign_inst);

		fill_in_empty_terminst_target(init_val_blocks, assign_block);
		init_val_blocks.blocks.push_back(assign_block);
		return {global_decl, init_val_blocks};
	} else {
		BlockList placeholder_blocklist = get_unit_blocklist();
		placeholder_blocklist.blocks.front()->name = "init_" + global_decl->ident.substr(1);
		return {global_decl, placeholder_blocklist};
	}
}

Function ast2kirt(AST::FuncDef &func_def, BlockList global_decl_blocks) {
	reg_scope_renamer.on_enter_scope();

	Function func;
	func.ret_type = ast_type_t2kirt_type_t(func_def.ret_type);
	func.name = func_def.ident;

	std::unique_ptr<AST::FuncFParam>* cur_fparam = &func_def.fparam;
	while (cur_fparam->get()) {
		AST::FuncFParam *fparam = cur_fparam->get();
		reg_scope_renamer.on_define_var(fparam->ident);
		func.fparams.push_back(FuncFParam {
			ast_type_t2kirt_type_t(fparam->type),
			reg_scope_renamer(fparam->ident)
		});
		cur_fparam = &(cur_fparam->get()->recur);
	}

	func.blocks = ast2kirt(*func_def.block);
	if (func.name != "main") {
		func.blocks.blocks.front()->name = "real_entry";	// The `entry` block performs some initialization, and "real_entry" is the real entry	
	} else {
		// Perform global variable initialization
		func.blocks.blocks.front()->name = "real_entry2";
		global_decl_blocks.blocks.front()->name = "real_entry";
		fill_in_empty_terminst_target(global_decl_blocks, func.blocks.blocks.front());
		global_decl_blocks.blocks >> func.blocks.blocks;
	}
	
	// Construct the epilogue block
	shared_ptr<ReturnInst> epilogue_block_ret_inst = std::make_shared<ReturnInst>();
	if (func.ret_type == type_t::VOID) {
		epilogue_block_ret_inst->ret_exp = nullptr;
	} else if (func.ret_type == type_t::INT) {
		std::shared_ptr<Exp> ret_exp = std::make_shared<Exp>();
		ret_exp->type = exp_t::NUMBER;
		ret_exp->number = 0;
		epilogue_block_ret_inst->ret_exp = ret_exp;
	} else {
		assert(0);
	}
	shared_ptr<Block> epilogue_block = get_unit_block();
	epilogue_block->name = "epilogue";
	epilogue_block->term_inst = epilogue_block_ret_inst;

	fill_in_empty_terminst_target(func.blocks, epilogue_block);
	func.blocks.blocks.push_back(epilogue_block);

	reg_scope_renamer.on_exit_scope();

	return func;
}

BlockList ast2kirt(AST::Block &block) {
	reg_scope_renamer.on_enter_scope();
	auto result = ast2kirt(*block.item);
	reg_scope_renamer.on_exit_scope();
	return result;
}

BlockList ast2kirt(AST::BlockItem &block_item) {
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
		BlockList var_def_blocks = ast2kirt(*var_def);
		BlockList following_blocks = block_item.recur ? ast2kirt(*block_item.recur) : get_unit_blocklist();
		following_blocks.blocks.front()->name = "avar_" + std::to_string(block_id_counter.next());
		fill_in_empty_terminst_target(var_def_blocks, following_blocks.blocks.front());
		var_def_blocks.blocks << following_blocks.blocks;
		return var_def_blocks;

	} else if (is_instance_of(cur_item, AST::Stmt*)) {
		// A statement
		if (is_instance_of(cur_item, AST::NopStmt*)) {
			BlockList following_blocks = block_item.recur ? ast2kirt(*block_item.recur) : get_unit_blocklist();
			return following_blocks;
		}

		if (is_instance_of(cur_item, AST::ReturnStmt*)) {
			AST::ReturnStmt *return_stmt = dynamic_cast<AST::ReturnStmt *>(cur_item);
			if (return_stmt->ret_exp) {
				shared_ptr<ReturnInst> return_inst = std::make_shared<ReturnInst>();
				auto [ret_exp, ret_exp_blocks] = ast2kirt(*return_stmt->ret_exp);
				return_inst->ret_exp = ret_exp;
				ret_exp_blocks.blocks.back()->term_inst = std::move(return_inst);
				return ret_exp_blocks;
			} else {
				BlockList res = get_unit_blocklist();
				shared_ptr<ReturnInst> return_inst = std::make_shared<ReturnInst>();
				res.blocks.front()->term_inst = std::move(return_inst);
				return res;
			}
		}

		if (is_instance_of(cur_item, AST::BreakStmt*)) {
			shared_ptr<JumpInst> jump_inst = std::make_shared<JumpInst>();
			jump_inst->type = jump_inst_t::BREAK;
			BlockList res = get_unit_blocklist();
			res.blocks.front()->term_inst = std::move(jump_inst);
			return res;
		}

		if (is_instance_of(cur_item, AST::ContinueStmt*)) {
			shared_ptr<JumpInst> jump_inst = std::make_shared<JumpInst>();
			jump_inst->type = jump_inst_t::CONTINUE;
			BlockList res = get_unit_blocklist();
			res.blocks.front()->term_inst = std::move(jump_inst);
			return res;
		}

		if (is_instance_of(cur_item, AST::AssignStmt*)) {
			AST::AssignStmt *assign_stmt = dynamic_cast<AST::AssignStmt *>(cur_item);
			auto [exp, exp_blocks] = ast2kirt(*assign_stmt->exp);

			shared_ptr<AssignInst> assign_inst = get_assign_inst(reg_scope_renamer(assign_stmt->lval->ident), *exp);
			shared_ptr<Block> assign_inst_block = get_unit_block();
			assign_inst_block->insts.push_back(assign_inst);
			assign_inst_block->name = "assign_" + std::to_string(block_id_counter.next());

			BlockList following_blocks = block_item.recur ? ast2kirt(*block_item.recur) : get_unit_blocklist();
			following_blocks.blocks.front()->name = "aassign_" + std::to_string(block_id_counter.next());

			exp_blocks.blocks.back()->term_inst = get_jump_inst(assign_inst_block);
			assign_inst_block->term_inst = get_jump_inst(following_blocks.blocks.front());

			exp_blocks.blocks.push_back(assign_inst_block);
			exp_blocks.blocks << following_blocks.blocks;
			return exp_blocks;
		}

		if (is_instance_of(cur_item, AST::ExpStmt*)) {
			int exp_id = block_id_counter.next();

			AST::ExpStmt *exp_stmt = dynamic_cast<AST::ExpStmt *>(cur_item);
			auto [exp, exp_blocks] = ast2kirt(*exp_stmt->exp);

			shared_ptr<ExpInst> exp_inst = std::make_shared<ExpInst>();
			exp_inst->exp = *exp;
			shared_ptr<Block> exp_inst_block = get_unit_block();
			exp_inst_block->insts.push_back(exp_inst);
			exp_inst_block->name = "exp_" + std::to_string(exp_id);

			BlockList following_blocks = block_item.recur ? ast2kirt(*block_item.recur) : get_unit_blocklist();
			following_blocks.blocks.front()->name = "aexp_" + std::to_string(exp_id);

			exp_blocks.blocks.back()->term_inst = get_jump_inst(exp_inst_block);
			exp_inst_block->term_inst = get_jump_inst(following_blocks.blocks.front());

			exp_blocks.blocks.push_back(exp_inst_block);
			exp_blocks.blocks << following_blocks.blocks;
			return exp_blocks;
		}

		auto ast2kirt_stmt_or_block = [&](std::unique_ptr<AST::Base> &stmt_or_block) -> BlockList {
			// A helper function for converting a statement or block to a block list
			// Useful in if() and while()
			reg_scope_renamer.on_enter_scope();
			BlockList block_list = ast2kirt(*dynamic_cast<AST::BlockItem *>(stmt_or_block.get()));
			reg_scope_renamer.on_exit_scope();
			return block_list;
		};
		if (is_instance_of(cur_item, AST::IfStmt*)) {
			AST::IfStmt *if_stmt = dynamic_cast<AST::IfStmt *>(cur_item);

			// KIRT generation
			auto [cond_expr, cond_expr_blocks] = ast2kirt(*if_stmt->cond);
			BlockList true_blocks = ast2kirt_stmt_or_block(if_stmt->then);
			BlockList false_blocks = if_stmt->otherwise ? ast2kirt_stmt_or_block(if_stmt->otherwise) : get_unit_blocklist();
			BlockList following_blocks = block_item.recur ? ast2kirt(*block_item.recur) : get_unit_blocklist();

			// Naming
			string block_id = std::to_string(block_id_counter.next());
			true_blocks.blocks.front()->name = "true_" + block_id;
			false_blocks.blocks.front()->name = "false_" + block_id;
			following_blocks.blocks.front()->name = "aif_" + block_id;

			// Redirection
			fill_in_empty_terminst_target(true_blocks, following_blocks.blocks.front());
			fill_in_empty_terminst_target(false_blocks, following_blocks.blocks.front());

			// Branching
			shared_ptr<BranchInst> branch_inst = std::make_shared<BranchInst>();
			branch_inst->cond = *cond_expr;
			branch_inst->true_block = true_blocks.blocks.front();
			branch_inst->false_block = false_blocks.blocks.front();
			cond_expr_blocks.blocks.back()->term_inst = std::move(branch_inst);
			
			// List merging
			cond_expr_blocks.blocks << true_blocks.blocks;
			cond_expr_blocks.blocks << false_blocks.blocks;
			cond_expr_blocks.blocks << following_blocks.blocks;
			return cond_expr_blocks;
		}

		if (is_instance_of(cur_item, AST::WhileStmt*)) {
			AST::WhileStmt *while_stmt = dynamic_cast<AST::WhileStmt *>(cur_item);

			// KIRT generation
			auto [cond_expr, cond_expr_blocks] = ast2kirt(*while_stmt->cond);
			BlockList body_blocks = ast2kirt_stmt_or_block(while_stmt->body);
			BlockList following_blocks = block_item.recur ? ast2kirt(*block_item.recur) : get_unit_blocklist();

			// Naming
			string block_id = std::to_string(block_id_counter.next());
			body_blocks.blocks.front()->name = "body_" + block_id;
			following_blocks.blocks.front()->name = "awhile_" + block_id;
			cond_expr_blocks.blocks.front()->name = "while_" + block_id;

			// Redirection
			BlockList res = get_unit_blocklist(cond_expr_blocks.blocks.front());
			fill_in_empty_terminst_target(body_blocks, cond_expr_blocks.blocks.front());
			fill_in_empty_terminst_target(body_blocks, following_blocks.blocks.front(), jump_inst_t::BREAK);
			fill_in_empty_terminst_target(body_blocks, cond_expr_blocks.blocks.front(), jump_inst_t::CONTINUE);

			// Branching
			shared_ptr<BranchInst> branch_inst = std::make_shared<BranchInst>();
			branch_inst->cond = *cond_expr;
			branch_inst->true_block = body_blocks.blocks.front();
			branch_inst->false_block = following_blocks.blocks.front();
			cond_expr_blocks.blocks.back()->term_inst = std::move(branch_inst);
			
			// Merging
			res.blocks << cond_expr_blocks.blocks;
			res.blocks << body_blocks.blocks;
			res.blocks << following_blocks.blocks;
			return res;
		}

		assert(0);
	} else {
		assert(0);
	}
	assert(0);
}

BlockList ast2kirt(AST::VarDef &var_def) {
	reg_scope_renamer.on_define_var(var_def.ident);
	BlockList assign_insts = get_unit_blocklist();
	if (var_def.init_val) {
		// Forge a virtual assignment statement
		std::unique_ptr<AST::LVal> assign_stmt_lval = std::make_unique<AST::LVal>();
		assign_stmt_lval->ident = var_def.ident;
		std::unique_ptr<AST::AssignStmt> assign_stmt = std::make_unique<AST::AssignStmt>();
		assign_stmt->lval = std::move(assign_stmt_lval);
		assign_stmt->exp = std::move(var_def.init_val);
		std::unique_ptr<AST::BlockItem> assign_stmt_block_item = std::make_unique<AST::BlockItem>();
		assign_stmt_block_item->item = std::move(assign_stmt);
		assign_insts = ast2kirt(*assign_stmt_block_item);
	}

	BlockList following_blocks = var_def.recur ? ast2kirt(*var_def.recur) : get_unit_blocklist();
	following_blocks.blocks.front()->name = "avar_" + std::to_string(block_id_counter.next());

	fill_in_empty_terminst_target(assign_insts, following_blocks.blocks.front());
	assign_insts.blocks << following_blocks.blocks;
	return assign_insts;
}

pair<shared_ptr<Exp>, BlockList> ast2kirt(AST::Exp &exp) {
	switch (exp.type) {
		case AST::exp_t::NUMBER: {
			shared_ptr<Exp> kirt_exp = std::make_shared<Exp>();
			kirt_exp->type = exp_t::NUMBER;
			kirt_exp->number = exp.number;
			return {kirt_exp, get_unit_blocklist()};
		} case AST::exp_t::LVAL: {
			shared_ptr<Exp> kirt_exp = std::make_shared<Exp>();
			kirt_exp->type = exp_t::LVAL;
			kirt_exp->ident = reg_scope_renamer(exp.lval.get()->ident);
			return {kirt_exp, get_unit_blocklist()};
		} case AST::exp_t::FUNC_CALL: {
			int func_call_id = block_id_counter.next();
			shared_ptr<Exp> kirt_exp = std::make_shared<Exp>();
			kirt_exp->type = exp_t::FUNC_CALL;
			kirt_exp->ident = exp.func_name;

			BlockList res = get_unit_blocklist();
			std::unique_ptr<AST::FuncRParam>* cur_rparam = &exp.func_rparam;
			int cur_rparam_id = 0;
			while (cur_rparam->get()) {
				auto [rparam_exp, rparam_exp_blocks] = ast2kirt(*(*cur_rparam)->exp);
				fill_in_empty_terminst_target(res, rparam_exp_blocks.blocks.front());
				rparam_exp_blocks.blocks.front()->name = format(
					"func_%s_rparam_%d_%d",
					exp.func_name.c_str(),
					cur_rparam_id,
					func_call_id
				);
				
				kirt_exp->args.push_back(rparam_exp);
				res.blocks << rparam_exp_blocks.blocks;

				cur_rparam = &(cur_rparam->get()->recur);
				cur_rparam_id += 1;
			}
			return {kirt_exp, res};
		} case AST::exp_t::NEGATIVE: {
			shared_ptr<Exp> kirt_exp = std::make_shared<Exp>();
			kirt_exp->type = exp_t::SUB;
			kirt_exp->lhs = std::make_shared<Exp>(0);
			auto [rhs_exp, rhs_exp_blocks] = ast2kirt(*exp.rhs);
			kirt_exp->rhs = rhs_exp;
			return {kirt_exp, rhs_exp_blocks};
		}
		case AST::exp_t::LOGICAL_NOT: {
			shared_ptr<Exp> kirt_exp = std::make_shared<Exp>();
			kirt_exp->type = exp_t::EQ;
			auto [rhs_exp, rhs_exp_blocks] = ast2kirt(*exp.rhs);
			kirt_exp->lhs = rhs_exp;
			kirt_exp->rhs = std::make_shared<Exp>(0);
			return {kirt_exp, rhs_exp_blocks};
		}
		case AST::exp_t::LOGICAL_AND: {
			// a && b = if (a) { $ = b != 0; } else { $ = 0; }
			string land_id = std::to_string(block_id_counter.next());
			string res_varid = "%land_res_" + land_id;
			shared_ptr<Block> final_block = get_unit_block();
			final_block->name = "land_fin_" + land_id;

			auto [a_exp, a_exp_blocks] = ast2kirt(*exp.lhs);
			auto [b_exp, b_exp_blocks] = ast2kirt(*exp.rhs);

			// Construct block "$ = (b != 0)"
			shared_ptr<Exp> b_neq_0 = std::make_shared<Exp>();
			b_neq_0->type = exp_t::NEQ0;
			b_neq_0->lhs = b_exp;
			b_neq_0->rhs = std::make_shared<Exp>(0);
			shared_ptr<AssignInst> assign_inst_ans_b = get_assign_inst(res_varid, *b_neq_0);
			b_exp_blocks.blocks.back()->insts.push_back(assign_inst_ans_b);
			b_exp_blocks.blocks.back()->term_inst = get_jump_inst(final_block);
			b_exp_blocks.blocks.front()->name = "land_t_" + land_id;

			// Construct block "$ = 0"
			shared_ptr<Block> else_block = get_unit_block();
			shared_ptr<AssignInst> assign_inst_ans_0 = get_assign_inst(res_varid, Exp(0));
			else_block->insts.push_back(assign_inst_ans_0);
			else_block->term_inst = get_jump_inst(final_block);
			else_block->name = "land_f_" + land_id;

			a_exp_blocks.blocks.back()->term_inst = get_branch_inst(*a_exp, b_exp_blocks.blocks.front(), else_block);

			BlockList res = a_exp_blocks;
			res.blocks << b_exp_blocks.blocks;
			res.blocks.push_back(else_block);
			res.blocks.push_back(final_block);

			shared_ptr<Exp> final_exp = std::make_shared<Exp>();
			final_exp->type = exp_t::LVAL;
			final_exp->ident = res_varid;
			return {final_exp, res};
		}
		case AST::exp_t::LOGICAL_OR: {
			// a || b = if (a) { $ = 1; } else { $ = b != 0; }
			string lor_id = std::to_string(block_id_counter.next());
			string res_varid = "%lor_res_" + lor_id;
			shared_ptr<Block> final_block = get_unit_block();
			final_block->name = "lor_fin_" + lor_id;

			auto [a_exp, a_exp_blocks] = ast2kirt(*exp.lhs);
			auto [b_exp, b_exp_blocks] = ast2kirt(*exp.rhs);

			shared_ptr<AssignInst> assign_inst_ans_1 = get_assign_inst(res_varid, Exp(1));
			shared_ptr<Block> if_block = get_unit_block();
			if_block->insts.push_back(assign_inst_ans_1);
			if_block->term_inst = get_jump_inst(final_block);
			if_block->name = "lor_t_" + lor_id;

			shared_ptr<Exp> b_neq_0 = std::make_shared<Exp>();
			b_neq_0->type = exp_t::NEQ0;
			b_neq_0->lhs = b_exp;
			b_neq_0->rhs = std::make_shared<Exp>(0);
			shared_ptr<AssignInst> assign_inst_ans_b = get_assign_inst(res_varid, *b_neq_0);
			b_exp_blocks.blocks.back()->insts.push_back(assign_inst_ans_b);
			b_exp_blocks.blocks.back()->term_inst = get_jump_inst(final_block);
			b_exp_blocks.blocks.front()->name = "lor_f_" + lor_id;

			a_exp_blocks.blocks.back()->term_inst = get_branch_inst(*a_exp, if_block, b_exp_blocks.blocks.front());

			BlockList res = a_exp_blocks;
			res.blocks.push_back(if_block);
			res.blocks << b_exp_blocks.blocks;
			res.blocks.push_back(final_block);

			shared_ptr<Exp> final_exp = std::make_shared<Exp>();
			final_exp->type = exp_t::LVAL;
			final_exp->ident = res_varid;
			return {final_exp, res};
		}
		default: {
			// These exp_t can be easily converted to exp_t in KIRT by a simple one-to-one mapping
			shared_ptr<Exp> kirt_exp = std::make_shared<Exp>();
			kirt_exp->type = ast_binary_exp_t2kirt_exp_t(exp.type);
			auto [lhs_exp, lhs_exp_blocks] = ast2kirt(*exp.lhs);
			auto [rhs_exp, rhs_exp_blocks] = ast2kirt(*exp.rhs);
			kirt_exp->lhs = lhs_exp;
			kirt_exp->rhs = rhs_exp;
			rhs_exp_blocks.blocks.front()->name = "exp_" + std::to_string(block_id_counter.next());	// Just give it a random name
			lhs_exp_blocks.blocks.back()->term_inst = get_jump_inst(rhs_exp_blocks.blocks.front());
			lhs_exp_blocks.blocks << rhs_exp_blocks.blocks;
			return {kirt_exp, lhs_exp_blocks};
		}
	}
	assert(0); // The control should never reach here
}

}
