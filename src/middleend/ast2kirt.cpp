#include "ast2kirt.h"

#include <algorithm>
#include <cassert>
#include <functional>
#include <map>
#include <optional>
#include <unordered_map>
#include <vector>

#include "utils/utils.h"

namespace KIRT {

using std::pair;

// The variable manager
//
// Provides two important functionalities:
// - Variable renaming
// - Constant management
//
// With the introduction of scoping, variable from different scopes may have the same name
// To avoid confusion, we rename the variables to a unique name:
//	@<original_name>_lv<level>
// Each time we enter a new scope, we increment the level. When we leave the
// scope, we decrement the level.
//
// For constant management, we promise that, the identifier of a constant shoud
// never appear in the KIRT. When we encounter a constant definition, we store
// the constant value in the manager. When we encounter a variable, we check if
// it is a constant. If it is, we replace the variable with the constant value.
enum class var_t {
	CONST,
	LOCAL_VAR,
	GLOBAL_VAR,
	PARAM,
};

struct VarDefinitionMeta {
	int level;
	int idx;	// We can not use leval to rename vars, since it is not unique
	Type type;
	var_t var_type;
	int const_val;
};

class VarManager {
private:
	Counter idx_counter;
	int cur_lvl = 0;	// Current level
	std::unordered_map<std::string, std::vector<VarDefinitionMeta>> orig_name2def_metas;

	// A list of the current function's local variables
	// Cleared upon entering a new function, and collected upon exiting a function
	vector<FuncLocalVar> cur_func_local_vars;

	inline string rename(string ident, const VarDefinitionMeta &meta) {
		if (meta.var_type == var_t::GLOBAL_VAR) {
			return "@" + ident + "_globl";
		}
		return "@" + ident + "_idx" + std::to_string(meta.idx);
	}

public:
	// Called when entering a new function
	void on_enter_func() {
		idx_counter.reset();
		cur_func_local_vars.clear();
	}

	vector<FuncLocalVar>& get_cur_func_new_local_vars() {
		return cur_func_local_vars;
	}

	// Called when exiting a function
	void on_exit_func() {
	}

	// Called when entering a new scope
	void on_enter_scope() {
		cur_lvl += 1;
	}

	// Called when leaving a scope
	void on_exit_scope() {
		for (auto &[orig_name, def_metas] : orig_name2def_metas) {
			while (!def_metas.empty() && def_metas.back().level == cur_lvl) {
				def_metas.pop_back();
			}
		}
		cur_lvl -= 1;
	}

	// Called when defining a new variable or constant
	void on_define_var(string orig_name, const Type &type, var_t var_type, int const_value = 0) {
		if (!orig_name2def_metas[orig_name].empty() &&
			orig_name2def_metas[orig_name].back().level == cur_lvl) {
			printf("Variable %s redefined at level %d\n", orig_name.c_str(), cur_lvl);
			assert(0);
		}
		orig_name2def_metas[orig_name].push_back({cur_lvl, idx_counter.next(), type, var_type, const_value});
		if (var_type == var_t::LOCAL_VAR) {
			cur_func_local_vars.push_back({type, this->operator()(orig_name)});
		}
	}

	// Directly add a local variable to the current function, useful when processing
	// short-circuit evaluation (where we need to add a temporary variable)
	void add_local_var_directly(string name, Type type) {
		cur_func_local_vars.push_back({type, name});
	}

	// Rename a variable
	string operator()(string orig_name) {
		assert (orig_name2def_metas.count(orig_name) != 0);
		VarDefinitionMeta &meta = orig_name2def_metas[orig_name].back();
		return rename(orig_name, meta);
	}

	// Return the definition meta of a variable
	VarDefinitionMeta& get_def_meta(string orig_name) {
		return orig_name2def_metas[orig_name].back();
	}
} var_manager;

// A helper function that converts an AST init val list (can be incomplete) to a full list
vector<std::unique_ptr<AST::Exp>> ast_init_val_list2full(
	const KIRT::Type &arr_type,
	std::unique_ptr<AST::InitValList> &init_val_list
) {
	using std::unique_ptr;
	vector<std::unique_ptr<AST::Exp>> res;

	int ndims = arr_type.dims();
	int numel = arr_type.numel();
	for (int i = 0; i < numel; ++i)
		res.push_back(nullptr);
	
	// Use the given init_val_list to fill in a subarray in `res`, which starts
	// from `start_index` and has dimensions from the `start_dim`
	std::function<void(AST::InitValList*, int, int)> fill_in_result = [&](
		AST::InitValList *init_val_list,
		int start_dim,
		int start_index
	) {
		int next_elem_offset = 0;	// The offset of the next element to be filled, relative to the start_index
		for (std::unique_ptr<AST::Base> &item : init_val_list->items) {
			if (is_instance_of(item.get(), AST::InitValList*)) {
				// Find the smallest dim that "aligns" with `next_elem_offset`
				int aligned_dim = -1;
				for (int dim = ndims-1; dim >= 0; --dim) {
					if (next_elem_offset % arr_type.stride(dim) == 0) {
						aligned_dim = dim;
					}
				}
				my_assert(33, aligned_dim != -1 && aligned_dim != ndims-1);
				fill_in_result(
					dynamic_cast<AST::InitValList*>(item.get()),
					aligned_dim+1,
					start_index + next_elem_offset
				);
				next_elem_offset += arr_type.stride(aligned_dim);
			} else if (is_instance_of(item.get(), AST::Exp*)) {
				res[start_index + next_elem_offset] = std::unique_ptr<AST::Exp>(
					dynamic_cast<AST::Exp *>(item.release())
				);
				next_elem_offset += 1;
			} else {
				assert(0);
			}
		}
	};
	fill_in_result(init_val_list.get(), 0, 0);

	for (std::unique_ptr<AST::Exp> &ptr : res)
		if (!ptr) {
			ptr = std::make_unique<AST::Exp>();
			ptr->type = AST::exp_t::NUMBER;
			ptr->number = 0;
		}
	
	return res;
}	

// Calculate the value of a constant expression
int calc_const_value(const std::unique_ptr<AST::Exp> &exp) {
	switch (exp->type) {
		case AST::exp_t::NUMBER:
			return exp->number;
		case AST::exp_t::LVAL:
			my_assert(24, var_manager.get_def_meta(exp->lval->ident).var_type == var_t::CONST);
			return var_manager.get_def_meta(exp->lval->ident).const_val;
		case AST::exp_t::FUNC_CALL:
			printf("FUNC_CALL encountered when calculating constant value\n");
			my_assert(25, false);
		case AST::exp_t::POSITIVE:
			return calc_const_value(exp->rhs);
		case AST::exp_t::NEGATIVE:
			return -calc_const_value(exp->rhs);
		case AST::exp_t::LOGICAL_NOT:
			return !calc_const_value(exp->rhs);
		case AST::exp_t::ADD:
			return calc_const_value(exp->lhs) + calc_const_value(exp->rhs);
		case AST::exp_t::SUB:
			return calc_const_value(exp->lhs) - calc_const_value(exp->rhs);
		case AST::exp_t::MUL:
			return calc_const_value(exp->lhs) * calc_const_value(exp->rhs);
		case AST::exp_t::DIV:
			return calc_const_value(exp->lhs) / calc_const_value(exp->rhs);
		case AST::exp_t::REM:
			return calc_const_value(exp->lhs) % calc_const_value(exp->rhs);
		case AST::exp_t::LT:
			return calc_const_value(exp->lhs) < calc_const_value(exp->rhs);
		case AST::exp_t::GT:
			return calc_const_value(exp->lhs) > calc_const_value(exp->rhs);
		case AST::exp_t::LEQ:
			return calc_const_value(exp->lhs) <= calc_const_value(exp->rhs);
		case AST::exp_t::GEQ:
			return calc_const_value(exp->lhs) >= calc_const_value(exp->rhs);
		case AST::exp_t::EQ:
			return calc_const_value(exp->lhs) == calc_const_value(exp->rhs);
		case AST::exp_t::NEQ:
			return calc_const_value(exp->lhs) != calc_const_value(exp->rhs);
		case AST::exp_t::LOGICAL_AND:
			return calc_const_value(exp->lhs) && calc_const_value(exp->rhs);
		case AST::exp_t::LOGICAL_OR:
			return calc_const_value(exp->lhs) || calc_const_value(exp->rhs);
		default:
			assert(0);
	}
}

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

shared_ptr<AssignInst> get_assign_inst(const LVal &lval, const Exp &exp) {
	shared_ptr<AssignInst> res = std::make_shared<AssignInst>();
	res->lval = lval;
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
Function ast2kirt(AST::FuncDef &func_def);
BlockList ast2kirt(AST::Block &block);
BlockList ast2kirt(AST::BlockItem &block_item);
BlockList ast2kirt(AST::VarDef &var_def);
pair<shared_ptr<Exp>, BlockList> ast2kirt(AST::Exp &exp);
shared_ptr<GlobalDecl> ast2kirt_global_decl(AST::VarDef &var_def);

Program ast2kirt(AST::CompUnit &comp_unit) {
	block_id_counter.reset();
	// Register library functions
	auto register_library_function = [&](string name, vector<Type> args_type, type_t ret_type) {
		shared_ptr<Function> func_ptr = std::make_shared<Function>();
		func_ptr->ret_type = ret_type;
		func_ptr->name = name;
		for (const Type &arg_type : args_type) {
			func_ptr->fparams.push_back(FuncFParam {arg_type, "arg"});
		}
		KIRT::func_map[name] = func_ptr;
	};
	Type type_int = {type_t::INT, {}};
	Type type_1d_arr = {type_t::ARR, {0}};
	register_library_function("getint", {}, type_t::INT);
	register_library_function("getch", {}, type_t::INT);
	register_library_function("getarray", {type_1d_arr}, type_t::INT);
	register_library_function("putint", {type_int}, type_t::VOID);
	register_library_function("putch", {type_int}, type_t::VOID);
	register_library_function("putarray", {type_int, type_1d_arr}, type_t::VOID);
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
	for (AST::VarDef *var_def : global_decls) {
		auto global_decl = ast2kirt_global_decl(*var_def);
		if (global_decl) {
			program.global_decls.push_back(global_decl);
			KIRT::global_decl_map[global_decl->ident] = global_decl;
		} else {
			// A constant, do nothing
		}
	}

	for (AST::FuncDef *func_def : funcs) {
		Function func = ast2kirt(*func_def);
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
shared_ptr<GlobalDecl> ast2kirt_global_decl(AST::VarDef &var_def) {
	if (var_def.lval->type == AST::lval_t::VAR) {
		if (!var_def.is_const) {
			// A mutable global variable
			var_manager.on_define_var(var_def.lval->ident, {type_t::INT, {}}, var_t::GLOBAL_VAR);
			shared_ptr<GlobalDecl> global_decl = std::make_shared<GlobalDecl>();
			global_decl->type.type = KIRT::type_t::INT;
			global_decl->ident = var_manager(var_def.lval->ident);
			if (var_def.init_val)
				global_decl->init_val = calc_const_value(var_def.init_val);	// Must be a constant expression
			else
				global_decl->init_val = 0;
			return global_decl;
		} else {
			// A global constant
			int const_val = calc_const_value(var_def.init_val);
			var_manager.on_define_var(var_def.lval->ident, {type_t::INT, {}}, var_t::CONST, const_val);
			return nullptr;
		}
	} else if (var_def.lval->type == AST::lval_t::ARR) {
		vector<int> shape;
		for (const std::unique_ptr<AST::Exp> &exp : var_def.lval->indices) {	// Here indices are the shape of the array
			shape.push_back(calc_const_value(exp));
		}
		// Here we regard a const array as a mutable array
		Type arr_type = {type_t::ARR, shape};
		var_manager.on_define_var(var_def.lval->ident, arr_type, var_t::GLOBAL_VAR);

		vector<int> init_vals;
		if (var_def.init_val_list) {
			vector<std::unique_ptr<AST::Exp>> init_exps;
			init_exps = ast_init_val_list2full(arr_type, var_def.init_val_list);
			for (std::unique_ptr<AST::Exp> &exp : init_exps) {
				init_vals.push_back(calc_const_value(exp));
			}
		} else {
			init_vals = vector<int>(arr_type.numel(), 0);
		}

		shared_ptr<GlobalDecl> global_decl = std::make_shared<GlobalDecl>();
		global_decl->ident = var_manager(var_def.lval->ident);
		global_decl->type = arr_type;
		global_decl->arr_init_vals = init_vals;
		return global_decl;
	} else {
		assert(0);
	}
}

Function ast2kirt(AST::FuncDef &func_def) {
	var_manager.on_enter_func();
	var_manager.on_enter_scope();

	Function func;
	func.ret_type = ast_type_t2kirt_type_t(func_def.ret_type);
	func.name = func_def.ident;

	std::unique_ptr<AST::FuncFParam>* cur_fparam = &func_def.fparam;
	while (cur_fparam->get()) {
		AST::FuncFParam *fparam = cur_fparam->get();

		Type type;
		vector<int> shape;
		if (fparam->lval->type == AST::lval_t::VAR) {
			type = {type_t::INT, {}};
		} else if (fparam->lval->type == AST::lval_t::ARR) {
			for (const std::unique_ptr<AST::Exp> &exp : fparam->lval->indices) {
				shape.push_back(calc_const_value(exp));
			}
			type = {type_t::ARR, shape};
		} else {
			assert(0);
		}

		var_manager.on_define_var(fparam->lval->ident, type, var_t::PARAM);
		func.fparams.push_back(FuncFParam {
			type,
			var_manager(fparam->lval->ident)
		});
		cur_fparam = &(cur_fparam->get()->recur);
	}

	func.blocks = ast2kirt(*func_def.block);
	func.blocks.blocks.front()->name = "real_entry";
	
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

	func.local_vars = var_manager.get_cur_func_new_local_vars();

	var_manager.on_exit_scope();
	var_manager.on_exit_func();

	return func;
}

BlockList ast2kirt(AST::Block &block) {
	var_manager.on_enter_scope();
	auto result = ast2kirt(*block.item);
	var_manager.on_exit_scope();
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

			string ident = var_manager(assign_stmt->lval->ident);
			LVal lval;
			if (assign_stmt->lval->type == AST::lval_t::VAR) {
				lval = LVal::make_int(ident);
			} else {
				vector<shared_ptr<Exp>> indices;
				for (const std::unique_ptr<AST::Exp> &exp_ptr : assign_stmt->lval->indices) {
					auto [index_exp, index_exp_blocks] = ast2kirt(*exp_ptr);
					exp_blocks.blocks.back()->term_inst = get_jump_inst(index_exp_blocks.blocks.front());
					index_exp_blocks.blocks.front()->name = "index_" + std::to_string(block_id_counter.next());
					indices.push_back(index_exp);
					exp_blocks.blocks << index_exp_blocks.blocks;
				}
				vector<int> &shape = var_manager.get_def_meta(assign_stmt->lval->ident).type.shape;
				assert(shape.size() == indices.size());
				lval = LVal::make_arr(ident, shape, indices);
			}

			shared_ptr<AssignInst> assign_inst = get_assign_inst(lval, *exp);
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
			var_manager.on_enter_scope();
			BlockList block_list = ast2kirt(*dynamic_cast<AST::BlockItem *>(stmt_or_block.get()));
			var_manager.on_exit_scope();
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
	if (var_def.lval->type == AST::lval_t::VAR) {
		if (!var_def.is_const) {
			var_manager.on_define_var(var_def.lval->ident, {type_t::INT, {}}, var_t::LOCAL_VAR);
			BlockList assign_insts = get_unit_blocklist();
			if (var_def.init_val) {
				// Forge a virtual assignment statement
				std::unique_ptr<AST::LVal> assign_stmt_lval = std::make_unique<AST::LVal>();
				assign_stmt_lval->type = AST::lval_t::VAR;
				assign_stmt_lval->ident = var_def.lval->ident;
				std::unique_ptr<AST::AssignStmt> assign_stmt = std::make_unique<AST::AssignStmt>();
				assign_stmt->lval = std::move(assign_stmt_lval);
				assign_stmt->exp = std::move(var_def.init_val);
				std::unique_ptr<AST::BlockItem> assign_stmt_block_item = std::make_unique<AST::BlockItem>();
				assign_stmt_block_item->item = std::move(assign_stmt);
				assign_insts = ast2kirt(*assign_stmt_block_item);
			}

			BlockList following_blocks = var_def.recur ? ast2kirt(*var_def.recur) : get_unit_blocklist();
			following_blocks.blocks.front()->name = "avar_" + std::to_string(block_id_counter.next());

			// Recall that all blocks except the last one in a blocklist returned
			// by a non-recursive `assign` statement should have a valid terminst
			assign_insts.blocks.back()->term_inst = get_jump_inst(following_blocks.blocks.front());
			assign_insts.blocks << following_blocks.blocks;
			return assign_insts;
		} else {
			int const_val = calc_const_value(var_def.init_val);
			var_manager.on_define_var(var_def.lval->ident, {type_t::INT, {}}, var_t::CONST, const_val);

			BlockList following_blocks = var_def.recur ? ast2kirt(*var_def.recur) : get_unit_blocklist();
			return following_blocks;
		}
	} else if (var_def.lval->type == AST::lval_t::ARR) {
		vector<int> shape;
		for (const std::unique_ptr<AST::Exp> &exp : var_def.lval->indices) {	// Here indices are the shape of the array
			shape.push_back(calc_const_value(exp));
		}
		// Here we regard a const array as a mutable array
		Type arr_type = {type_t::ARR, shape};
		var_manager.on_define_var(var_def.lval->ident, arr_type, var_t::LOCAL_VAR);

		BlockList assign_insts = get_unit_blocklist();
		if (var_def.init_val_list) {
			vector<std::unique_ptr<AST::Exp>> init_vals = ast_init_val_list2full(arr_type, var_def.init_val_list);
			int numel = arr_type.numel();
			int ndims = arr_type.dims();
			for (int i = 0; i < numel; ++i) {
				// Calculate the index
				int temp_i = i;
				vector<int> indices;
				for (int dim = ndims-1; dim >= 0; --dim) {
					indices.push_back(temp_i % shape[dim]);
					temp_i /= shape[dim];
				}
				std::reverse(indices.begin(), indices.end());
				
				// Forge the virtual assign statement
				vector<std::unique_ptr<AST::Exp>> indices_exp;
				for (int index : indices) {
					std::unique_ptr<AST::Exp> index_exp = std::make_unique<AST::Exp>();
					index_exp->type = AST::exp_t::NUMBER;
					index_exp->number = index;
					indices_exp.push_back(std::move(index_exp));
				}

				std::unique_ptr<AST::LVal> assign_stmt_lval = std::make_unique<AST::LVal>();
				assign_stmt_lval->type = AST::lval_t::ARR;
				assign_stmt_lval->ident = var_def.lval->ident;
				assign_stmt_lval->indices = std::move(indices_exp);

				std::unique_ptr<AST::Exp> assign_stmt_exp = std::make_unique<AST::Exp>();
				if (var_def.is_const) {
					assign_stmt_exp->type = AST::exp_t::NUMBER;
					assign_stmt_exp->number = calc_const_value(init_vals[i]);
				} else {
					assign_stmt_exp = std::move(init_vals[i]);
				}
				
				std::unique_ptr<AST::AssignStmt> assign_stmt = std::make_unique<AST::AssignStmt>();
				assign_stmt->lval = std::move(assign_stmt_lval);
				assign_stmt->exp = std::move(assign_stmt_exp);

				std::unique_ptr<AST::BlockItem> assign_stmt_block_item = std::make_unique<AST::BlockItem>();
				assign_stmt_block_item->item = std::move(assign_stmt);
				BlockList cur_assign_insts = ast2kirt(*assign_stmt_block_item);

				assign_insts.blocks.back()->term_inst = get_jump_inst(cur_assign_insts.blocks.front());
				assign_insts.blocks << cur_assign_insts.blocks;
			}
		}

		BlockList following_blocks = var_def.recur ? ast2kirt(*var_def.recur) : get_unit_blocklist();
		assign_insts.blocks.back()->term_inst = get_jump_inst(following_blocks.blocks.front());
		assign_insts.blocks << following_blocks.blocks;

		return assign_insts;
	} else {
		assert(0);
	}
}

pair<shared_ptr<Exp>, BlockList> ast2kirt(AST::Exp &exp) {
	switch (exp.type) {
		case AST::exp_t::NUMBER: {
			shared_ptr<Exp> kirt_exp = std::make_shared<Exp>();
			kirt_exp->type = exp_t::NUMBER;
			kirt_exp->number = exp.number;
			return {kirt_exp, get_unit_blocklist()};
		} case AST::exp_t::LVAL: {
			VarDefinitionMeta def_meta = var_manager.get_def_meta(exp.lval->ident);
			if (def_meta.var_type == var_t::CONST) {
				// A constant
				shared_ptr<Exp> kirt_exp = std::make_shared<Exp>();
				kirt_exp->type = exp_t::NUMBER;
				kirt_exp->number = var_manager.get_def_meta(exp.lval->ident).const_val;
				return {kirt_exp, get_unit_blocklist()};
			} else {
				if (def_meta.type.is_arr()) {
					// An array
					BlockList res_blocklist = get_unit_blocklist();
					vector<shared_ptr<Exp>> indices;
					for (const std::unique_ptr<AST::Exp> &exp_ptr : exp.lval->indices) {
						auto [index_exp, index_exp_blocks] = ast2kirt(*exp_ptr);
						res_blocklist.blocks.back()->term_inst = get_jump_inst(index_exp_blocks.blocks.front());
						index_exp_blocks.blocks.front()->name = "index_" + std::to_string(block_id_counter.next());
						indices.push_back(index_exp);
						res_blocklist.blocks << index_exp_blocks.blocks;
					}
					shared_ptr<Exp> kirt_exp = std::make_shared<Exp>();
					kirt_exp->type = exp_t::LVAL;
					kirt_exp->lval = LVal::make_arr(var_manager(exp.lval->ident), def_meta.type.shape, indices);
					return {kirt_exp, res_blocklist};
				} else if (def_meta.type.is_int()) {
					// A mutable variable
					shared_ptr<Exp> kirt_exp = std::make_shared<Exp>();
					kirt_exp->type = exp_t::LVAL;
					kirt_exp->lval = LVal::make_int(var_manager(exp.lval->ident));
					return {kirt_exp, get_unit_blocklist()};
				} else {
					my_assert(30, false);
				}
			}
		} case AST::exp_t::FUNC_CALL: {
			int func_call_id = block_id_counter.next();
			shared_ptr<Exp> kirt_exp = std::make_shared<Exp>();
			kirt_exp->type = exp_t::FUNC_CALL;
			kirt_exp->func_name = exp.func_name;

			BlockList res = get_unit_blocklist();
			std::unique_ptr<AST::FuncRParam>* cur_rparam = &exp.func_rparam;
			int cur_rparam_id = 0;
			while (cur_rparam->get()) {
				auto [rparam_exp, rparam_exp_blocks] = ast2kirt(*(*cur_rparam)->exp);
				res.blocks.back()->term_inst = get_jump_inst(rparam_exp_blocks.blocks.front());
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
			var_manager.add_local_var_directly(res_varid, {type_t::INT, {}});

			auto [a_exp, a_exp_blocks] = ast2kirt(*exp.lhs);
			auto [b_exp, b_exp_blocks] = ast2kirt(*exp.rhs);

			// Construct block "$ = (b != 0)"
			shared_ptr<Exp> b_neq_0 = std::make_shared<Exp>();
			b_neq_0->type = exp_t::NEQ0;
			b_neq_0->lhs = b_exp;
			b_neq_0->rhs = std::make_shared<Exp>(0);
			shared_ptr<AssignInst> assign_inst_ans_b = get_assign_inst(LVal::make_int(res_varid), *b_neq_0);
			b_exp_blocks.blocks.back()->insts.push_back(assign_inst_ans_b);
			b_exp_blocks.blocks.back()->term_inst = get_jump_inst(final_block);
			b_exp_blocks.blocks.front()->name = "land_t_" + land_id;

			// Construct block "$ = 0"
			shared_ptr<Block> else_block = get_unit_block();
			shared_ptr<AssignInst> assign_inst_ans_0 = get_assign_inst(LVal::make_int(res_varid), Exp(0));
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
			final_exp->lval = LVal::make_int(res_varid);
			return {final_exp, res};
		}
		case AST::exp_t::LOGICAL_OR: {
			// a || b = if (a) { $ = 1; } else { $ = b != 0; }
			string lor_id = std::to_string(block_id_counter.next());
			string res_varid = "%lor_res_" + lor_id;
			var_manager.add_local_var_directly(res_varid, {type_t::INT, {}});
			shared_ptr<Block> final_block = get_unit_block();
			final_block->name = "lor_fin_" + lor_id;

			auto [a_exp, a_exp_blocks] = ast2kirt(*exp.lhs);
			auto [b_exp, b_exp_blocks] = ast2kirt(*exp.rhs);

			shared_ptr<AssignInst> assign_inst_ans_1 = get_assign_inst(LVal::make_int(res_varid), Exp(1));
			shared_ptr<Block> if_block = get_unit_block();
			if_block->insts.push_back(assign_inst_ans_1);
			if_block->term_inst = get_jump_inst(final_block);
			if_block->name = "lor_t_" + lor_id;

			shared_ptr<Exp> b_neq_0 = std::make_shared<Exp>();
			b_neq_0->type = exp_t::NEQ0;
			b_neq_0->lhs = b_exp;
			b_neq_0->rhs = std::make_shared<Exp>(0);
			shared_ptr<AssignInst> assign_inst_ans_b = get_assign_inst(LVal::make_int(res_varid), *b_neq_0);
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
			final_exp->lval = LVal::make_int(res_varid);
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
			lhs_exp_blocks.blocks.back()->term_inst = get_jump_inst(rhs_exp_blocks.blocks.front());
			lhs_exp_blocks.blocks << rhs_exp_blocks.blocks;
			return {kirt_exp, lhs_exp_blocks};
		}
	}
	assert(0); // The control should never reach here
}

}
