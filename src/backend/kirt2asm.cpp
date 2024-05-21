#include "backend/kirt2asm.h"

#include <cassert>
#include <map>
#include <vector>

#include "utils/utils.h"

namespace ASM {

using std::list;
using std::pair;
using std::string;
using std::shared_ptr;
using std::vector;

enum class reg_alloc_stat_t {
	REG,
	STACK
};

// RegAllocStat - Register allocation result
// If type == REG, then id is the register id
// If type == STACK, then id is the stack offset (in bytes)
struct RegAllocStat {
	reg_alloc_stat_t type;
	int id;

	string get_target_regid(bool is_rhs = false) {
		// If type == REG, return the register id (e.g. "x12")
		// If type == STACK, return t0
		if (type == reg_alloc_stat_t::REG) {
			return format("x%d", id);
		} else {
			return is_rhs ? "t1" : "t0";
		}
	}

	list<string> get_load_inst(bool is_rhs = false) {
		// If type == REG, return an empty list
		// If type == STACK, return a list of instructions to load the value from the stack
		if (type == reg_alloc_stat_t::REG) {
			return {};
		} else {
			return {
				format(
					"  lw %s, %d(sp)",
					is_rhs ? "t1" : "t0",
					id
				)
			};
		}
	}

	list<string> get_store_inst() {
		// If type == REG, return an empty list
		// If type == STACK, return a list of instructions to store the value to the stack
		if (type == reg_alloc_stat_t::REG) {
			return {};
		} else {
			return {
				format(
					"  sw t0, %d(sp)",
					id
				)
			};
		}
	}
};

// RegAllocator - An on-the-fly register allocator
class RegAllocator {
public:
	// std::vector<int> free_regids;
	int stack_var_cnt;
	std::map<string, RegAllocStat> ident2stat;

	// Called when entering a function
	void on_enter_function() {
		stack_var_cnt = 1;	// Start from "1" since "0" is reserved for the stack frame size
		ident2stat.clear();
	}

	int get_max_num_local_vars() {
		return stack_var_cnt;
	}

	// Called when defining a function argument
	list<string> on_new_func_arg(string ident, int kth, int total_local_var_and_saved_regs) {
		// kth starts from 0
		// `total_local_var_and_saved_regs` is (address of the 8th argument) - (final sp), in bytes
		// Currently we save all function arguments to the stack
		ident2stat[ident] = {reg_alloc_stat_t::STACK, stack_var_cnt*4};
		stack_var_cnt += 1;
		// Save the variable to the stack
		if (kth <= 7) {
			return {
				format(
					"  sw a%d, %d(sp)",
					kth,
					(stack_var_cnt-1)*4
				)
			};
		} else {
			return {
				format(
					"  lw t0, %d(sp)",
					4*(kth-8) + (total_local_var_and_saved_regs)*4
				),
				format(
					"  sw t0, %d(sp)",
					(stack_var_cnt-1)*4
				)
			};
		}
	}

	// Called when loading an identifer
	RegAllocStat on_load(string ident) {
		if (ident == "0") {
			return {reg_alloc_stat_t::REG, 0};
		}
		if (!ident2stat.count(ident)) {
			ident2stat[ident] = {reg_alloc_stat_t::STACK, stack_var_cnt*4};
			stack_var_cnt += 1;
		}
		RegAllocStat stat = ident2stat[ident];
		assert (stat.type == reg_alloc_stat_t::STACK);
		return stat;
	}

	// Called when storing an identifier
	RegAllocStat on_store(string ident) {
		if (ident == "0") {
			return {reg_alloc_stat_t::REG, 0};
		}
		if (!ident2stat.count(ident)) {
			ident2stat[ident] = {reg_alloc_stat_t::STACK, stack_var_cnt*4};
			stack_var_cnt += 1;
		}
		RegAllocStat stat = ident2stat[ident];
		assert (stat.type == reg_alloc_stat_t::STACK);
		return stat;
	}
} reg_allocator;

static string kirt_exp_t2asm(KIRT::exp_t type) {
	switch (type) {
		case KIRT::exp_t::NUMBER:
			assert(0);
		case KIRT::exp_t::LVAL:
			assert(0);
		case KIRT::exp_t::FUNC_CALL:
			assert(0);
		case KIRT::exp_t::ADD:
			return "add";
		case KIRT::exp_t::SUB:
			return "sub";
		case KIRT::exp_t::MUL:
			return "mul";
		case KIRT::exp_t::DIV:
			return "div";
		case KIRT::exp_t::REM:
			return "rem";
		case KIRT::exp_t::LT:
			return "slt";
		case KIRT::exp_t::GT:
			return "sgt";
		case KIRT::exp_t::BITWISE_AND:
			return "and";
		case KIRT::exp_t::BITWISE_OR:
			return "or";
		case KIRT::exp_t::BITWISE_XOR:
			return "xor";
		case KIRT::exp_t::SHL:
			return "sll";
		case KIRT::exp_t::SHR:
			return "srl";
		case KIRT::exp_t::SAR:
			return "sra";
		default:
			assert(0);
	}
}

const vector<string> callee_saved_regs({
	"ra", "s0", "s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9", "s10", "s11"
});

// Some global status
Counter virt_ident_counter;	// A counter for generating virtual identifer
string cur_func_name;	// Current function name
list<string> cur_func_epilogue;	// Current function epilogue (e.g. restore stack pointer)

inline string rename_block(const string &block_name) {
	return cur_func_name + "_" + block_name;
}

list<string> kirt2asm(const KIRT::Function &func);
list<string> kirt2asm(const KIRT::Block &block);
list<string> kirt2asm(const shared_ptr<KIRT::Inst> &inst);
list<string> kirt2asm(const shared_ptr<KIRT::TermInst> &inst);
pair<list<string>, string> kirt2asm(const KIRT::Exp &inst);

list<string> kirt2asm(const KIRT::Program &prog) {
	list<string> res;

	for (const std::shared_ptr<KIRT::Function> &func : prog.funcs) {
		list<string> func_asm = kirt2asm(*func);
		res << func_asm;
	}

	return res;
}

list<string> kirt2asm(const KIRT::Function &func) {
	reg_allocator.on_enter_function();
	cur_func_name = func.name;

	// Construct the epilogue
	{
		cur_func_epilogue.clear();
		cur_func_epilogue.push_back("  lw t0, 0(sp)");
		cur_func_epilogue.push_back("  add sp, sp, t0");
		// TODO Sometimes we do not utilize all registers, hence do not need to
		// store & restore all callee-saved registers
		for (auto it = callee_saved_regs.rbegin(); it != callee_saved_regs.rend(); ++it) {
			cur_func_epilogue.push_back(format(
				"  lw %s, 0(sp)",
				it->c_str()
			));
			// TODO Optimize this addition
			cur_func_epilogue.push_back("  addi sp, sp, 4");
		}
	}

	list<string> res;

	int stack_frame_length;	// in 4 bytes, include local vars, but does not include saved regs
	// stack_frame_length = reg_allocator.get_max_num_local_vars() + 1;	// +1 for the "stack frame length" (stored at 0(sp))
	stack_frame_length = 400 + 1 + std::max(0, (int)func.fparams.size()-8);	// +1 for the "stack frame length" (stored at 0(sp))
	if (stack_frame_length % 4 != 0) {
		stack_frame_length += 4 - (stack_frame_length % 4);
	}

	// Arguments registration
	int total_local_var_and_saved_regs = callee_saved_regs.size() + stack_frame_length;
	for (int i = 0; i < func.fparams.size(); i++) {
		const KIRT::FuncFParam &param = func.fparams[i];
		list<string> save_arg_insts = reg_allocator.on_new_func_arg(param.ident, i, total_local_var_and_saved_regs);
		res << save_arg_insts;
	}

	for (const std::shared_ptr<KIRT::Block> &block : func.blocks.blocks) {
		list<string> block_asm = kirt2asm(*block);
		res << block_asm;
	}

	assert (reg_allocator.get_max_num_local_vars() <= 400);

	// Construct the prologue
	{
		list<string> prolouge;
		// Function declaration
		prolouge.push_back("  .text");
		prolouge.push_back("  .globl " + func.name);
		prolouge.push_back(func.name + ":");

		// Save registers
		// TODO Optmize `main()` does not need to save & store registers
		for (const string &reg : callee_saved_regs) {
			// TODO Optimize this subtraction
			prolouge.push_back("  addi sp, sp, -4");
			prolouge.push_back(format(
				"  sw %s, 0(sp)",
				reg.c_str()
			));
		}

		// Stack frame allocation
		// TODO Optimize when space_to_alloc == 0
		// TODO Optimize when space_to_alloc <= 2047
		prolouge.push_back(format(
			"  li t0, %d",
			stack_frame_length*4
		));
		prolouge.push_back("  sub sp, sp, t0");
		prolouge.push_back("  sw t0, 0(sp)");

		prolouge >> res;
	}

	return res;
}

list<string> kirt2asm(const KIRT::Block &block) {
	list<string> res;
	res.push_back(rename_block(block.name) + ":");

	for (const shared_ptr<KIRT::Inst> &inst : block.insts) {
		list<string> inst_asm = kirt2asm(inst);
		res << inst_asm;
	}

	list<string> term_inst_asm = kirt2asm(block.term_inst);
	res << term_inst_asm;

	return res;
}

list<string> kirt2asm(const shared_ptr<KIRT::Inst> &inst) {
	list<string> res;

	if (const KIRT::AssignInst *assign_inst = dynamic_cast<const KIRT::AssignInst *>(inst.get())) {
		auto [exp_inst_list, exp_virt_ident] = kirt2asm(assign_inst->exp);
		res << exp_inst_list;

		RegAllocStat exp_alloc_stat = reg_allocator.on_load(exp_virt_ident);
		res <= exp_alloc_stat.get_load_inst();
		RegAllocStat target_alloc_stat = reg_allocator.on_store(assign_inst->ident);
		res.push_back(format(
			"  mv %s, %s",
			target_alloc_stat.get_target_regid().c_str(),
			exp_alloc_stat.get_target_regid().c_str()
		));
		res <= target_alloc_stat.get_store_inst();
	} else if (const KIRT::ExpInst *exp_inst = dynamic_cast<const KIRT::ExpInst *>(inst.get())) {
		auto [exp_inst_list, exp_virt_ident] = kirt2asm(exp_inst->exp);
		res << exp_inst_list;
	} else {
		assert(0);
	}
	return res;
}

list<string> kirt2asm(const shared_ptr<KIRT::TermInst> &inst) {
	list<string> res;

	if (const KIRT::ReturnInst *ret_inst = dynamic_cast<const KIRT::ReturnInst *>(inst.get())) {
		if (ret_inst->ret_exp) {
			auto [exp_inst_list, exp_virt_ident] = kirt2asm(*ret_inst->ret_exp);
			res << exp_inst_list;
			
			RegAllocStat exp_alloc_stat = reg_allocator.on_load(exp_virt_ident);
			res <= exp_alloc_stat.get_load_inst();
			res.push_back(format(
				"  mv a0, %s",
				exp_alloc_stat.get_target_regid().c_str()
			));
		}
		res <= cur_func_epilogue;
		res.push_back("  ret");
	} else if (const KIRT::JumpInst *jump_inst = dynamic_cast<const KIRT::JumpInst *>(inst.get())) {
		res.push_back(format(
			"  j %s",
			rename_block(jump_inst->target_block->name).c_str()
		));
	} else if (const KIRT::BranchInst *branch_inst = dynamic_cast<const KIRT::BranchInst *>(inst.get())) {
		auto [cond_inst_list, cond_virt_ident] = kirt2asm(branch_inst->cond);
		res << cond_inst_list;

		// TODO Now we first store the value of the instruction into a reg, then use
		// bnez to jump. This can be optimized
		// TODO Now we always jump to the false block, via `j %s`. In the future we may
		// get rid of this jump via BlockReorderPass
		// TODO Currently short-circuit is not supported
		RegAllocStat cond_alloc_stat = reg_allocator.on_load(cond_virt_ident);
		res <= cond_alloc_stat.get_load_inst();
		res.push_back(format(
			"  bnez %s, %s",
			cond_alloc_stat.get_target_regid().c_str(),
			rename_block(branch_inst->true_block->name).c_str()
		));
		res.push_back(format(
			"  j %s",
			rename_block(branch_inst->false_block->name).c_str()
		));
	} else {
		assert(0);
	}

	return res;
}

// Return: (instruction list, result virtual identifier)
pair<list<string>, string> kirt2asm(const KIRT::Exp &exp) {
	list<string> res;
	if (exp.type == KIRT::exp_t::LVAL) {
		string ident = exp.ident;
		return {res, ident};
	} else if (exp.type == KIRT::exp_t::FUNC_CALL) {
		string res_virt_ident = format("v%d", virt_ident_counter.next());
		RegAllocStat res_reg_alloc_stat = reg_allocator.on_store(res_virt_ident);

		int num_args = exp.args.size();
		vector<string> arg_virt_idents;

		// Calculate args
		for (int i = 0; i < exp.args.size(); ++i) {
			auto [arg_inst_list, arg_virt_ident] = kirt2asm(*exp.args[i]);
			res << arg_inst_list;
			arg_virt_idents.push_back(arg_virt_ident);
		}
		// Place args
		for (int i = 0; i < exp.args.size(); ++i) {
			string arg_virt_ident = arg_virt_idents[i];
			if (i <= 7) {
				// Save the argument to the register
				RegAllocStat arg_alloc_stat = reg_allocator.on_load(arg_virt_ident);
				res <= arg_alloc_stat.get_load_inst();
				res.push_back(format(
					"  mv a%d, %s",
					i,
					arg_alloc_stat.get_target_regid().c_str()
				));
			} else {
				// Save the argument to the stack
				RegAllocStat arg_alloc_stat = reg_allocator.on_load(arg_virt_ident);
				res <= arg_alloc_stat.get_load_inst();
				res.push_back(format(
					"  sw %s, %d(sp)",
					arg_alloc_stat.get_target_regid().c_str(),
					-(num_args-i)*4
				));
			}
		}
		// Adjust SP
		if (num_args > 8) {
			res.push_back(format(
				"  addi sp, sp, -%d",
				(num_args-8)*4
			));
		}
		// Call
		res.push_back(format(
			"  call %s",
			exp.ident.c_str()
		));
		// Restore SP
		if (num_args > 8) {
			res.push_back(format(
				"  addi sp, sp, %d",
				(num_args-8)*4
			));
		}
		// Save the return value
		res.push_back(format(
			"  mv %s, a0",
			res_reg_alloc_stat.get_target_regid().c_str()
		));
		res <= res_reg_alloc_stat.get_store_inst();
		return {res, res_virt_ident};
	} else {
		string res_virt_ident = format("v%d", virt_ident_counter.next());
		RegAllocStat res_reg_alloc_stat = reg_allocator.on_store(res_virt_ident);
		if (exp.type == KIRT::exp_t::NUMBER) {
			if (exp.number != 0) {
				res.push_back(format(
					"  li %s, %d",
					res_reg_alloc_stat.get_target_regid().c_str(),
					exp.number
				));
				res <= res_reg_alloc_stat.get_store_inst();
				return {res, res_virt_ident};
			} else {
				return {res, "0"};
			}
		} else if (exp.type == KIRT::exp_t::EQ0 || exp.type == KIRT::exp_t::NEQ0) {
			auto [lhs_inst_list, lhs_virt_ident] = kirt2asm(*exp.lhs);
			res << lhs_inst_list;

			RegAllocStat lhs_alloc_stat = reg_allocator.on_load(lhs_virt_ident);
			res <= lhs_alloc_stat.get_load_inst();
			res.push_back(format(
				"  %s %s, %s",
				exp.type == KIRT::exp_t::EQ0 ? "seqz" : "snez",
				res_reg_alloc_stat.get_target_regid().c_str(),
				lhs_alloc_stat.get_target_regid().c_str()
			));
			res <= res_reg_alloc_stat.get_store_inst();
			return {res, res_virt_ident};
		} else {
			auto [lhs_inst_list, lhs_virt_ident] = kirt2asm(*exp.lhs);
			auto [rhs_inst_list, rhs_virt_ident] = kirt2asm(*exp.rhs);
			res << lhs_inst_list;
			res << rhs_inst_list;

			RegAllocStat lhs_alloc_stat = reg_allocator.on_load(lhs_virt_ident);
			RegAllocStat rhs_alloc_stat = reg_allocator.on_load(rhs_virt_ident);
			res <= lhs_alloc_stat.get_load_inst();
			res <= rhs_alloc_stat.get_load_inst(true);
			res.push_back(format(
				"  %s %s, %s, %s",
				kirt_exp_t2asm(exp.type).c_str(),
				res_reg_alloc_stat.get_target_regid().c_str(),
				lhs_alloc_stat.get_target_regid().c_str(),
				rhs_alloc_stat.get_target_regid(true).c_str()
			));
			res <= res_reg_alloc_stat.get_store_inst();
			return {res, res_virt_ident};
		}
	}
}

}