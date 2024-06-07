// Pass Scalar Promotion: Promote const scalar variables in a loop body to
// loop-invariant variables

#include "pass_scalar_promotion.h"

#include <algorithm>
#include <cassert>
#include <functional>
#include <queue>
#include <unordered_map>
#include <unordered_set>

#include "utils/utils.h"
#include "middleend/kirt_utils.h"
#include "optim_dbs.h"

namespace KIRT {

using std::pair;

inline int get_binary_exp_cost(exp_t type) {
	switch (type) {
		case exp_t::ADDR_ADD:
		case exp_t::ADD:
		case exp_t::SUB:
		case exp_t::LT:
		case exp_t::GT:
		case exp_t::LEQ:
		case exp_t::GEQ:
		case exp_t::EQ:
		case exp_t::NEQ:
		case exp_t::BITWISE_AND:
		case exp_t::BITWISE_OR:
		case exp_t::BITWISE_XOR:
		case exp_t::SHL:
		case exp_t::SHR:
		case exp_t::SAR:
			return 1;
		
		case exp_t::MUL:
		case exp_t::DIV:
		case exp_t::REM:
			return 5;

		default:
			assert(0);
	}
}

static constexpr int COST_THRES = 1;	// Only pre-calculate if the cost reduction >= this threshold
static constexpr int ARRAY_LOAD_COST = 3;
static constexpr int FUNC_CALL_COST = 100;

bool is_library_func(const string &func_name) {
	return func_name == "getint" || func_name == "getch" || func_name == "getarray" || \
			func_name == "putint" || func_name == "putch" || func_name == "putarray"|| \
			func_name == "starttime" || func_name == "stoptime";
}

static void pass_scalar_promotion(Function &func, Block &body_block) {
	int num_blocks = func.blocks.blocks.size();
	vector<shared_ptr<Block>> id2block(num_blocks);
	for (auto &block : func.blocks.blocks) {
		id2block[block->id] = block;
	}
	int while_id, lvl;
	assert(sscanf(body_block.name.c_str(), "while_body_%d_lvl_%d", &while_id, &lvl) == 2);

	// Step 0. Construct the graph
	vector<vector<int>> incoming_edges(num_blocks);
	auto add_edge = [&incoming_edges](int src, int dst) {
		incoming_edges[dst].push_back(src);
	};
	for (auto &block : func.blocks.blocks) {
		TermInst* term_inst_ptr = block->term_inst.get();
		if (const ReturnInst *return_inst = dynamic_cast<ReturnInst*>(term_inst_ptr)) {
			// Do nothing
		} else if (const JumpInst *jump_inst = dynamic_cast<JumpInst*>(term_inst_ptr)) {
			add_edge(block->id, jump_inst->target_block->id);
		} else if (const BranchInst *branch_inst = dynamic_cast<BranchInst*>(term_inst_ptr)) {
			add_edge(block->id, branch_inst->true_block->id);
			add_edge(block->id, branch_inst->false_block->id);
		} else {
			assert(0);
		}
	}


	// Step 1. Identify all blocks in the current loop
	vector<bool> in_loop(num_blocks, false);	// Whether there exists a path that goes back to the loop and pass the block

	// printf("Loop %s\n", body_block.name.c_str());
	{
		std::queue<int> q;
		q.push(body_block.id);
		in_loop[body_block.id] = true;
		while (!q.empty()) {
			int cur = q.front();
			q.pop();
			// printf("%s\n", id2block[cur]->name.c_str());
			for (int next : incoming_edges[cur]) {
				if (cur == body_block.id && next < cur)
					continue;
				if (!in_loop[next]) {
					in_loop[next] = true;
					q.push(next);
				}
			}
		}
	}


	// Step 2. Collect all modified vars in the current loop
	// "Modified var" refers to a variable that is assigned (appeared on the
	// left side of an assignment instruction) in the loop
	// Can be a local variable / a global variable / a parameter / an array
	std::unordered_set<string> modified_vars;
	for (int i = 0; i < num_blocks; i++) {
		if (!in_loop[i])
			continue;
		auto &block = id2block[i];
		for (auto &inst : block->insts) {
			if (auto *assign_inst = dynamic_cast<AssignInst*>(inst.get())) {
				modified_vars.insert(assign_inst->lval.ident);
			}
		}
	}
	// TODO Add support for array elements

	// Step 2.5: Collect all called functions in the current loop
	std::unordered_set<string> called_functions;
	std::function<void(Exp&)> traverse_exp_collect_functions = [&](Exp &exp) {
		exp_category_t exp_category = get_exp_category(exp.type);
		switch (exp_category) {
			case exp_category_t::NUMBER:
				break;
			case exp_category_t::SPECIAL:
				switch (exp.type) {
					case exp_t::LVAL:
						for (auto &index : exp.lval.indices) {
							traverse_exp_collect_functions(*index);
						}
					case exp_t::FUNC_CALL:
						called_functions.insert(exp.func_name);
						for (auto &arg : exp.args) {
							traverse_exp_collect_functions(*arg);
						}
						break;
					case exp_t::ARR_ADDR:
						break;
					default:
						assert(0);
				}
				break;
			case exp_category_t::UNARY:
				traverse_exp_collect_functions(*exp.lhs);
				break;
			case exp_category_t::BINARY:
				traverse_exp_collect_functions(*exp.lhs);
				traverse_exp_collect_functions(*exp.rhs);
				break;
			default:
				assert(0);
		}
	};
	for (int i = 0; i < num_blocks; i++) {
		if (!in_loop[i])
			continue;
		auto &block = id2block[i];
		for (auto &inst : block->insts) {
			if (AssignInst *assign_inst = dynamic_cast<AssignInst*>(inst.get())) {
				traverse_exp_collect_functions(assign_inst->exp);
				if (assign_inst->lval.is_arr()) {
					traverse_exp_collect_functions(*assign_inst->lval.indices[0]);
				}
			} else if (ExpInst *exp_inst = dynamic_cast<ExpInst*>(inst.get())) {
				traverse_exp_collect_functions(exp_inst->exp);
			} else {
				assert(0);
			}
		}
		TermInst* term_inst_ptr = block->term_inst.get();
		if (BranchInst *branch_inst = dynamic_cast<BranchInst*>(term_inst_ptr)) {
			traverse_exp_collect_functions(branch_inst->cond);
		} else if (JumpInst *jump_inst = dynamic_cast<JumpInst*>(term_inst_ptr)) {
			// Do nothing
		} else if (ReturnInst *return_inst = dynamic_cast<ReturnInst*>(term_inst_ptr)) {
			// Does not promote since the return value is only calculated once
			// Wait, this block should not be in the loop!
			// assert(0);
		} else {
			assert(0);
		}
	}

	// Step 3. Traverse along each `Exp`. Try our best to promote it
	// Return (whether pre-calculable, cost-reduction)
	vector<pair<shared_ptr<Exp>*, int>> precalcable_exps;	// (exp, cost)
	auto add_precalc_exp = [&](std::shared_ptr<Exp> *exp, int cost) {
		precalcable_exps.push_back({exp, cost});
	};

	std::unordered_set<string> callee_touched_global_vars;
	for (const string &callee : called_functions) {
		for (const string &global_arr : KIRT::func2modified_global_arrs[callee]) {
			callee_touched_global_vars.insert(global_arr);
		}
	}

	std::function<pair<bool, int>(Exp&)> traverse_exp_and_promote = [&](Exp &exp) -> pair<bool, int> {
		exp_category_t exp_category = get_exp_category(exp.type);
		switch (exp_category) {
			case exp_category_t::NUMBER: {
				return {true, 0};
			}
			case exp_category_t::SPECIAL: {
				switch (exp.type) {
					case exp_t::LVAL: {
						// If the LVal is a global var, and it is modified by
						// one of the callees in the loop, we cannot pre-calculate
						// it
						if (callee_touched_global_vars.count(exp.lval.ident))
							return {false, 0};
						if (exp.lval.is_int()) {
							if (modified_vars.count(exp.lval.ident))
								return {false, 0};
							else
								return {true, 0};
						} else if (exp.lval.is_arr()) {
							assert(exp.lval.indices.size() == 1);	// Only support 1D array
							auto [pre_calculable, cost_reduction] = traverse_exp_and_promote(*exp.lval.indices[0]);
							if (pre_calculable) {
								// If an array is not modified in the loop, and it has
								// never been passed as an argument to a function, we can
								// pre-calculate it
								// TODO Use a finer-grained analysis on modification
								// via argument passing
								if (!modified_vars.count(exp.lval.ident) && !func2as_arred_params[func.name].count(exp.lval.ident))
									return {true, cost_reduction + ARRAY_LOAD_COST};
								else {
									add_precalc_exp(&exp.lval.indices[0], cost_reduction);
									return {false, 0};
								}
							} else {
								return {false, 0};
							}
						}else {
							assert(0);
						}
					}
					case exp_t::FUNC_CALL: {
						bool has_non_pre_calcable_arg = false;
						int cost_reduction_sum = 0;
						for (auto &arg : exp.args) {
							auto [pre_calculable, cost_reduction] = traverse_exp_and_promote(*arg);
							cost_reduction_sum += cost_reduction;
							if (!pre_calculable) {
								has_non_pre_calcable_arg = true;
							} else if (has_non_pre_calcable_arg) {
								// For convenience we only add precalc exps after
								// the first non-precalcable arg
								// TODO Optimize this by considering all
								// pre-calcable args
								add_precalc_exp(&arg, cost_reduction);
							}
						}
						bool indirect_call_lib_func = false;
						if (!is_library_func(exp.func_name)) {
							for (const string &callee : KIRT::func2callees[exp.func_name]) {
								if (is_library_func(callee)) {
									indirect_call_lib_func = true;
									break;
								}
							}
						}
						if (!has_non_pre_calcable_arg && KIRT::func2modified_global_arrs[exp.func_name].empty() && !is_library_func(exp.func_name) && !indirect_call_lib_func) {
							// If the function and all functions it calls does not
							// touch global vars, and it does not have any array
							// arguments, we can pre-calculate it
							bool have_arr_arg = false;
							shared_ptr<Function> callee = KIRT::func_map[exp.func_name];
							for (const FuncFParam &param : callee->fparams) {
								if (param.type.is_arr()) {
									have_arr_arg = true;
									break;
								}
							}
							if (!have_arr_arg) {
								return {true, cost_reduction_sum + FUNC_CALL_COST};
							}
						}
						// TODO Push all pre-calculable args
						return {false, 0};
					}
					case exp_t::ARR_ADDR:
						return {true, 0};
					default:
						assert(0);
				}
			}
			case exp_category_t::UNARY: {
				auto [pre_calculable, cost_reduction] = traverse_exp_and_promote(*exp.lhs);
				if (pre_calculable) {
					return {true, cost_reduction+1};
				} else {
					return {false, 0};
				}
			}
			case exp_category_t::BINARY: {
				auto [lhs_pre_calcable, lhs_cost_reduction] = traverse_exp_and_promote(*exp.lhs);
				auto [rhs_pre_calcable, rhs_cost_reduction] = traverse_exp_and_promote(*exp.rhs);
				if (lhs_pre_calcable && rhs_pre_calcable) {
					int cur_op_cost = get_binary_exp_cost(exp.type);
					return {true, lhs_cost_reduction + rhs_cost_reduction + cur_op_cost};
				} else {
					if (lhs_pre_calcable) {
						add_precalc_exp(&exp.lhs, lhs_cost_reduction);
					}
					if (rhs_pre_calcable) {
						add_precalc_exp(&exp.rhs, rhs_cost_reduction);
					}
					return {false, 0};
				}
			}
			default:
				assert(0);
		}
	};

	for (int i = 0; i < num_blocks; i++) {
		if (!in_loop[i])
			continue;
		auto &block = id2block[i];
		for (auto &inst : block->insts) {
			if (AssignInst *assign_inst = dynamic_cast<AssignInst*>(inst.get())) {
				traverse_exp_and_promote(assign_inst->exp);
				if (assign_inst->lval.is_arr()) {
					traverse_exp_and_promote(*assign_inst->lval.indices[0]);
				}
			} else if (ExpInst *exp_inst = dynamic_cast<ExpInst*>(inst.get())) {
				traverse_exp_and_promote(exp_inst->exp);
			} else {
				assert(0);
			}
		}
		TermInst* term_inst_ptr = block->term_inst.get();
		if (BranchInst *branch_inst = dynamic_cast<BranchInst*>(term_inst_ptr)) {
			traverse_exp_and_promote(branch_inst->cond);
		} else if (JumpInst *jump_inst = dynamic_cast<JumpInst*>(term_inst_ptr)) {
			// Do nothing
		} else if (ReturnInst *return_inst = dynamic_cast<ReturnInst*>(term_inst_ptr)) {
			// Does not promote since the return value is only calculated once
			// Wait, this block should not be in the loop!
			// assert(0);
		} else {
			assert(0);
		}
	}

	// Step 4. Merge all same pre-calcable exps
	vector<pair<shared_ptr<Exp>*, int>> merged_precalcable_exps;
	for (auto [exp_ptr, cost] : precalcable_exps) {
		bool merged = false;
		for (auto &[merged_exp_ptr, bef_cost] : merged_precalcable_exps) {
			if (**exp_ptr == **merged_exp_ptr) {
				merged = true;
				bef_cost += cost;
				break;
			}
		}
		if (!merged) {
			merged_precalcable_exps.push_back({exp_ptr, cost});
		}
	}

	// Step 5. Perform promotion
	Counter precalc_var_cnter;
	shared_ptr<Block> assign_inst_blocks = std::make_shared<Block>();
	int num_new_vars = 0, num_merged_exps = 0, sum_cost_reduction = 0;
	for (auto [exp_ptr, cost] : merged_precalcable_exps) {
		if (cost >= COST_THRES) {
			num_new_vars += 1;
			sum_cost_reduction += cost;

			shared_ptr<Exp> old_exp_ptr = nullptr;
			old_exp_ptr = *exp_ptr;
			string var_ident = format(
				"%while_%d_lvl_%d_precalc_%d",
				while_id, lvl, precalc_var_cnter.next()
			);
			func.local_vars.push_back({{type_t::INT, {}}, var_ident});

			// Replace all occurrences of `old_exp_ptr` with `new_lval_exp_ptr`
			// (refer to the var directly)
			shared_ptr<Exp> new_lval_exp_ptr = std::make_shared<Exp>();
			new_lval_exp_ptr->type = exp_t::LVAL;
			new_lval_exp_ptr->lval = LVal::make_int(var_ident);
			for (auto [v_exp_ptr, _] : precalcable_exps)
				if (*v_exp_ptr == *exp_ptr) {
					num_merged_exps += 1;
					*v_exp_ptr = new_lval_exp_ptr;
				}
			
			// Add a virtual assignment instruction
			auto assign_inst = std::make_shared<AssignInst>();
			assign_inst->lval = LVal::make_int(var_ident);
			assign_inst->exp = *old_exp_ptr;
			assign_inst_blocks->insts.push_back(assign_inst);
		}
	}

	assign_inst_blocks->id = num_blocks;
	assign_inst_blocks->name = format("while_%d_lvl_%d_precalc", while_id, lvl);
	assign_inst_blocks->term_inst = std::make_shared<JumpInst>();

	// Step 6. Insert the new block before while_cond1 block
	string while_cond1_name = format("while_cond1_%d_lvl_%d", while_id, lvl);
	auto while_cond1_iter = func.blocks.blocks.end();
	shared_ptr<Block> while_cond1_block;
	for (auto iter = func.blocks.blocks.begin(); iter != func.blocks.blocks.end(); ) {
		if ((*iter)->name == while_cond1_name) {
			while_cond1_block = *iter;
			break;
		} else {
			iter = std::next(iter);
		}
	}
	my_assert(40, while_cond1_block);

	((JumpInst*)assign_inst_blocks->term_inst.get())->target_block = id2block[while_cond1_block->id];
	func.blocks.blocks.insert(while_cond1_iter, assign_inst_blocks);

	// Step 7. Reconnect all edges
	int cond1_block_id = while_cond1_block->id;
	for (int cur_id : incoming_edges[cond1_block_id]) {
		assert (!in_loop[cur_id]);
		auto &cur_block = id2block[cur_id];
		TermInst* term_inst_ptr = cur_block->term_inst.get();
		if (BranchInst *branch_inst = dynamic_cast<BranchInst*>(term_inst_ptr)) {
			if (branch_inst->true_block->id == cond1_block_id) {
				branch_inst->true_block = assign_inst_blocks;
			} else if (branch_inst->false_block->id == cond1_block_id) {
				branch_inst->false_block = assign_inst_blocks;
			} else {
				assert(0);
			}
		} else if (JumpInst *jump_inst = dynamic_cast<JumpInst*>(term_inst_ptr)) {
			assert (jump_inst->target_block->id == cond1_block_id);
			jump_inst->target_block = assign_inst_blocks;
		} else {
			assert(0);
		}
	}

	fprintf(stderr, "Block `%s`: %2d new vars, %2d merged exps, %2d cost reduction\n",
		body_block.name.c_str(), num_new_vars, num_merged_exps, sum_cost_reduction);
}

static void pass_scalar_promotion(Function &func) {
	// Step 1. Find out all while_body blocks
	// Since each while_body block has at least 2 inputs, it 
	// should never be (appendly) fused with other blocks
	vector<pair<int, shared_ptr<Block>>> while_body_blocks;	// (level, block)
	for (auto &block : func.blocks.blocks) {
		if (is_start_with(block->name, "while_body")) {
			int while_id, lvl;
			assert(sscanf(block->name.c_str(), "while_body_%d_lvl_%d", &while_id, &lvl) == 2);
			while_body_blocks.push_back({lvl, block});
		}
	}

	// Step 2. Sort all whiles by their levels (in descending order)
	std::sort(while_body_blocks.begin(), while_body_blocks.end(), [](const pair<int, shared_ptr<Block>> &a, const pair<int, shared_ptr<Block>> &b) {
		return a.first > b.first;
	});

	// Step 3. Process each `while` separately
	fprintf(stderr, "Function `%10s`: %2d whiles\n",
		func.name.c_str(), (int)while_body_blocks.size());

	for (auto &while_body : while_body_blocks) {
		auto &body_block = while_body.second;
		pass_scalar_promotion(func, *body_block);
	}
}

void pass_scalar_promotion(Program &prog) {
	fprintf(stderr, "======== Optimization: Scalar Promotion ========\n");
	for (auto &func : prog.funcs)
		pass_scalar_promotion(*func);
}

}
