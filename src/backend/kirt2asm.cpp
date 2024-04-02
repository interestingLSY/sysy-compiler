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

	int get_max_stack_size() {
		return stack_var_cnt*4;
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
	res.push_back(".text");
	res.push_back(".globl main");

	for (const KIRT::Function &func : prog.funcs) {
		list<string> func_asm = kirt2asm(func);
		res << func_asm;
	}

	return res;
}

list<string> kirt2asm(const KIRT::Function &func) {
	reg_allocator.on_enter_function();
	cur_func_name = func.name;

	cur_func_epilogue.clear();
	cur_func_epilogue.push_back("  lw t0, 0(sp)");
	cur_func_epilogue.push_back("  add sp, sp, t0");

	list<string> res;
	for (const std::shared_ptr<KIRT::Block> &block : func.blocks.blocks) {
		list<string> block_asm = kirt2asm(*block);
		res << block_asm;
	}

	list<string> prolouge;
	prolouge.push_back(func.name + ":");

	int space_to_alloc = reg_allocator.get_max_stack_size() + 4;	// +4 for the stack frame length
	if (space_to_alloc % 16 != 0) {
		space_to_alloc += 16 - (space_to_alloc % 16);
	}
	// TODO Optimize when space_to_alloc == 0
	// TODO Optimize when space_to_alloc <= 2047
	prolouge.push_back(format(
		"  li t0, %d",
		space_to_alloc
	));
	prolouge.push_back("  sub sp, sp, t0");
	prolouge.push_back("  sw t0, 0(sp)");

	prolouge >> res;

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
	} else {
		assert(0);
	}
	return res;
}

list<string> kirt2asm(const shared_ptr<KIRT::TermInst> &inst) {
	list<string> res;

	if (const KIRT::ReturnInst *ret_inst = dynamic_cast<const KIRT::ReturnInst *>(inst.get())) {
		auto [exp_inst_list, exp_virt_ident] = kirt2asm(ret_inst->ret_exp);
		res << exp_inst_list;
		
		RegAllocStat exp_alloc_stat = reg_allocator.on_load(exp_virt_ident);
		res <= exp_alloc_stat.get_load_inst();
		res.push_back(format(
			"  mv a0, %s",
			exp_alloc_stat.get_target_regid().c_str()
		));
		res <= cur_func_epilogue;
		res.push_back("  ret");
	} else if (const KIRT::JumpInst *jump_inst = dynamic_cast<const KIRT::JumpInst *>(inst.get())) {
		res.push_back(format(
			"  j %s",
			rename_block(jump_inst->target_block->name).c_str()
		));
	} else {
		assert(0);
	}

	return res;
}

pair<list<string>, string> kirt2asm(const KIRT::Exp &exp) {
	list<string> res;
	if (exp.type == KIRT::exp_t::LVAL) {
		string ident = exp.ident;
		return {res, ident};
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