// Pass Idempotent Function Promotion: Just calculate once the value of an
// idempotent function

#include "pass_idempotent_func_promotion.h"

#include <cassert>
#include <functional>
#include <unordered_map>

#include "utils/utils.h"
#include "middleend/kirt_utils.h"
#include "optim/optim_dbs.h"

namespace KIRT {

// Statistics
int num_new_vars;
int num_eliminated_calls;

static void pass_idempotent_func_promotion(Function &func, Block &block) {
	struct PromotionCandi {	// Candi = Candidate
		shared_ptr<Inst> caller_inst;	// The instruction that calls the function
		int last_modfied_lval_version;	// The "version index" of the most recent modified LVal
		string func_ident;
		vector<shared_ptr<Exp>> args;		// Arguments
		vector<Exp*> exp_ptr;	// Pointer to the expressions to be promoted
	};
	vector<PromotionCandi> promotion_candis;

	Counter version_counter;	// Increased upon assignments
	std::unordered_map<string, int> var_version;	// The "version index" of every variable

	struct TraverseMeta {
		int most_recent_modified_var;	// The "version index" of the most recent modified LVal
		bool have_promoted;	// Whether we have promoted a idempotent function in this subtree
							// TODO Allow multiple promotions in a subtree
		bool is_volatile;	// If this flag is set, we assume this expression changes
							// volatily and never perform promotion
	};
	std::function<TraverseMeta(shared_ptr<Inst>&, Exp&)> traverse = [&](shared_ptr<Inst> &caller_inst, Exp &exp) -> TraverseMeta {
		exp_category_t exp_category = get_exp_category(exp.type);
		switch (exp_category) {
			case exp_category_t::NUMBER: {
				return {0, false, false};
			}
			case exp_category_t::SPECIAL: {
				switch (exp.type) {
					case exp_t::LVAL: {
						if (exp.lval.is_int()) {
							// A global var / local var / parameter
							string var_ident = exp.lval.ident;
							if (!var_version.count(var_ident))
								var_version[var_ident] = version_counter.cur();
							return {var_version[var_ident], false, false};
						} else if (exp.lval.is_arr()) {
							// A global arr / local arr
							assert (exp.lval.indices.size() == 1);
							traverse(caller_inst, *exp.lval.indices[0]);
							// Never perform promotion on the array
							return {0, false, true};
						} else {
							assert(0);
						}
					}
					case exp_t::FUNC_CALL: {
						int last_modified_lval = 0;
						bool have_promoted = false;
						bool have_volatile = false;
						for (auto &arg : exp.args) {
							auto [last_modified, promoted, volatile_] = traverse(caller_inst, *arg);
							last_modified_lval = std::max(last_modified_lval, last_modified);
							have_promoted |= promoted;
							have_volatile |= volatile_;
						}
						if (!have_promoted && !have_volatile) {
							if (is_function_idempotent(exp.func_name)) {
								// This function is idempotent
								// We can promote it
								bool found_home = false;
								for (PromotionCandi &candi : promotion_candis) {
									if (candi.func_ident == exp.func_name) {
										bool args_match = true;
										for (size_t i = 0; i < exp.args.size(); i++) {
											if (*exp.args[i] != *candi.args[i]) {
												args_match = false;
												break;
											}
										}
										if (args_match) {
											// Found a candidate
											if (candi.last_modfied_lval_version < last_modified_lval) {
												printf("%d %d\n", candi.last_modfied_lval_version, last_modified_lval);
												// Version changed. Need to create a new candi
												break;
											} else {
												// Found a home
												candi.exp_ptr.push_back(&exp);
												found_home = true;
												break;
											}
										}
									}
								}
								if (!found_home) {
									// Add a new entry
									promotion_candis.push_back({
										caller_inst,
										last_modified_lval,
										exp.func_name,
										exp.args,
										{&exp}
									});
								}
								return {0, true, false};
							} else {
								return {0, false, true};
							}
						} else if (!have_volatile) {
							assert(have_promoted);
							// Don't promote since we only consider the "lowest"
							// level of functions
							return {0, true, false};
						} else {
							// One of the arguments is volatile
							return {0, false, true};
						}
					}
					case exp_t::ARR_ADDR: {
						// An array address. Never changes
						return {0, false};
					}
					default:
						assert(0);
				}
			}
			case exp_category_t::UNARY: {
				return traverse(caller_inst, *exp.lhs);
			}
			case exp_category_t::BINARY: {
				TraverseMeta lhs_meta = traverse(caller_inst, *exp.lhs);
				TraverseMeta rhs_meta = traverse(caller_inst, *exp.rhs);
				return {
					std::max(lhs_meta.most_recent_modified_var, rhs_meta.most_recent_modified_var),
					lhs_meta.have_promoted || rhs_meta.have_promoted,
					lhs_meta.is_volatile || rhs_meta.is_volatile
				};
			}
			default:
				assert(0);
		}
	};

	for (auto &inst : block.insts) {
		if (AssignInst *assign_inst = dynamic_cast<AssignInst*>(inst.get())) {
			traverse(inst, assign_inst->exp);
			if (assign_inst->lval.is_arr()) {
				traverse(inst, *assign_inst->lval.indices[0]);
			}
			// Version update!
			var_version[assign_inst->lval.ident] = version_counter.next();
		} else if (ExpInst *exp_inst = dynamic_cast<ExpInst*>(inst.get())) {
			traverse(inst, exp_inst->exp);
		} else {
			assert(0);
		}
	}
	// TODO Consider TermInst

	if (promotion_candis.empty())
		return;

	for (PromotionCandi &candi : promotion_candis) {
		int num_calls = candi.exp_ptr.size();
		if (num_calls == 1)
			continue;

		num_new_vars += 1;
		num_eliminated_calls += num_calls;

		string var_ident = format(
			"%%%s_promoted_%s_%d",
			block.name.c_str(),
			"_promoted_",
			candi.func_ident.c_str(),
			num_new_vars
		);
		func.local_vars.push_back({{type_t::INT, {}}, var_ident});
		Exp old_exp = candi.exp_ptr[0]->clone();

		// Replace all the calls
		for (Exp *exp_ptr : candi.exp_ptr) {
			exp_ptr->type = exp_t::LVAL;
			exp_ptr->lval = LVal::make_int(var_ident);
		}

		// Add a new assignment
		auto assign_inst = std::make_shared<AssignInst>();
		assign_inst->lval = LVal::make_int(var_ident);
		assign_inst->exp = old_exp;

		// Push the instruction
		bool is_inserted = false;
		for (auto it = block.insts.begin(); it != block.insts.end(); it++) {
			if (it->get() == candi.caller_inst.get()) {
				block.insts.insert(it, assign_inst);
				is_inserted = true;
				break;
			}
		}
		assert(is_inserted);
	}

	return;
}

static void pass_idempotent_func_promotion(Function &func) {
	num_new_vars = 0;
	num_eliminated_calls = 0;
	for (auto &block : func.blocks.blocks)
		pass_idempotent_func_promotion(func, *block);
	fprintf(stderr, "Function `%10s` promoted %d idempotent functions calls to %d new vars\n",
		func.name.c_str(), num_eliminated_calls, num_new_vars);
}

void pass_idempotent_func_promotion(Program &prog) {
	fprintf(stderr, "======== Optimization: Promote Idempotent Functions ========\n");
	for (auto &func : prog.funcs)
		pass_idempotent_func_promotion(*func);
}

}
