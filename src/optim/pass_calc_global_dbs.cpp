// Pass Calculate Global DBs: Calculate global dbs (e.g. `func2modified_global_arrs`)

#include "pass_calc_global_dbs.h"

#include <cassert>
#include <functional>
#include <unordered_map>

#include "optim_dbs.h"
#include "utils/utils.h"
#include "middleend/kirt_utils.h"

namespace KIRT {

// Put all direct infomation
static void pass_put_direct_data(Function &func) {
	std::function<void(Exp &)> traverse_exp = [&](Exp &exp) {
		exp_category_t cat = get_exp_category(exp.type);
		switch (cat) {
			case exp_category_t::NUMBER:
				return;
			case exp_category_t::SPECIAL: {
				switch (exp.type) {
					case exp_t::LVAL:
						if (exp.lval.is_arr()) {
							assert(exp.lval.indices.size() == 1);
							traverse_exp(*exp.lval.indices[0]);
						}
						return;
					case exp_t::FUNC_CALL:
						func2callees[func.name].insert(exp.func_name);
						for (auto &arg : exp.args)
							traverse_exp(*arg);
						return;
					case exp_t::ARR_ADDR:
						func2as_arred_params[func.name].insert(exp.arr_name);
						return;
					default:
						assert(0);
				}
				return;
			}
			case exp_category_t::UNARY:
				traverse_exp(*exp.lhs);
				return;
			case exp_category_t::BINARY:
				traverse_exp(*exp.lhs);
				traverse_exp(*exp.rhs);
				return;
		}	
	};

	for (auto &block : func.blocks.blocks) {
		for (auto &inst : block->insts)  {
			if (auto assign_inst = dynamic_cast<AssignInst *>(inst.get())) {
				traverse_exp(assign_inst->exp);
				if (assign_inst->lval.is_arr()) {
					assert (assign_inst->lval.indices.size() == 1);
					traverse_exp(*assign_inst->lval.indices[0]);
				}
				if (KIRT::global_decl_map.count(assign_inst->lval.ident))
					func2modified_global_arrs[func.name].insert(assign_inst->lval.ident);
			} else if (auto exp_inst = dynamic_cast<ExpInst *>(inst.get())) {
				traverse_exp(exp_inst->exp);
			} else {
				assert(0);
			}
		}
		TermInst *term_inst = block->term_inst.get();
		if (auto ret_inst = dynamic_cast<ReturnInst *>(term_inst)) {
			if (ret_inst->ret_exp)
				traverse_exp(*ret_inst->ret_exp);
		} else if (auto br_inst = dynamic_cast<BranchInst *>(term_inst)) {
			traverse_exp(br_inst->cond);
		} else if (auto jump_inst = dynamic_cast<JumpInst *>(term_inst)) {
		} else {
			assert(0);
		}
	}
}

static void calc_func_callee_transitive_closure() {
	std::unordered_map<std::string, std::unordered_set<std::string>> func2callees_copy = func2callees;
	std::function<void(const std::string &)> dfs = [&](const std::string &func_name) {
		for (auto &callee : func2callees_copy[func_name]) {
			if (func2callees[func_name].count(callee))
				continue;
			func2callees[func_name].insert(callee);
			dfs(callee);
		}
	};
	for (auto &func : func2callees) {
		dfs(func.first);
	}
}

static void calc_other_dbs_transitive_closure() {
	for (auto &func : func2callees) {
		for (auto &callee : func.second) {
			for (auto &global_arr : func2modified_global_arrs[callee]) {
				func2modified_global_arrs[func.first].insert(global_arr);
			}
		}
	}
}

void pass_calc_global_dbs(Program &prog) {
	for (auto &func : prog.funcs)
		pass_put_direct_data(*func);
	
	calc_func_callee_transitive_closure();

	calc_other_dbs_transitive_closure();
}

}
