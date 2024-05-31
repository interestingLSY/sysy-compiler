// kirt.h - Definition of KIR nodes
#pragma once

#include <cassert>
#include <list>
#include <memory>
#include <numeric>
#include <string>
#include <vector>
#include <map>

#include "utils/utils.h"

namespace KIRT {

using std::list;
using std::string;
using std::shared_ptr;
using std::vector;

class Exp;

enum class type_t {
	INT,
	ARR,
	VOID
};

struct Type {
	type_t type;
	vector<int> shape;

	inline bool is_int() const {
		return type == type_t::INT;
	}
	inline bool is_arr() const {
		return type == type_t::ARR;
	}
	inline int dims() const {
		assert(type == type_t::ARR);
		return shape.size();
	}
	inline int numel() const {
		assert(type == type_t::ARR);
		return std::accumulate(shape.begin(), shape.end(), 1, std::multiplies<int>());
	}
	inline int stride(int i) const {
		assert(type == type_t::ARR);
		assert(i < shape.size());
		return std::accumulate(shape.begin() + i + 1, shape.end(), 1, std::multiplies<int>());
	}
};

struct LVal {
	string ident;
	Type type;
	vector<shared_ptr<Exp>> indices;
	// When indices.size() == shape.size(), the result should be a int
	// When indices.size() < shape.size(), the result should be another arr

	inline bool is_int() const {
		return type.is_int();
	}
	inline bool is_arr() const {
		return type.is_arr();
	}

	static LVal make_int(const string &ident) {
		return LVal{ident, Type{type_t::INT, {}}, {}};
	}
	static LVal make_arr(const string &ident, const vector<int> &shape, const vector<shared_ptr<Exp>> &indices) {
		return LVal{ident, Type{type_t::ARR, shape}, indices};
	}
};

enum class exp_t {
	NUMBER,

	LVAL,

	FUNC_CALL,

	ARR_ADDR,
	ADDR_ADD,

	// POSITIVE won't appear in the final AST
	// NEGATIVE will be converted to SUB
	// LOGICAL_NOT will be converted to EQ 0

	ADD,
	SUB,
	MUL,
	DIV,
	REM,

	LT,
	GT,
	LEQ,
	GEQ,
	EQ,
	NEQ,

	BITWISE_AND,
	BITWISE_OR,
	BITWISE_XOR,

	SHL,
	SHR,
	SAR,

	EQ0,
	NEQ0
};

class Exp;
class Inst;
class TermInst;
class Block;
class BlockList;
class FuncFParam;
class Function;
class Program;

// Exp - An expression
class Exp {
public:
	exp_t type;
	int number;		// For NUMBER
	LVal lval;	// For LVAL
	string func_name;	// For FUNC_CALL
	string arr_name;	// For ARR_ADDR
	vector<shared_ptr<Exp>> args;	// For FUNC_CALL
	shared_ptr<Exp> lhs;	// For binary ops
	shared_ptr<Exp> rhs;	// For unary & binary ops

	Exp() = default;
	Exp(int x) : type(exp_t::NUMBER), number(x) {}
};


// Inst - An instruction
class Inst {
public:
	virtual ~Inst() = default;	// Make Inst polymorphic
};

class AssignInst : public Inst {
public:
	LVal lval;
	Exp exp;
};

class ExpInst : public Inst {
public:
	Exp exp;
};


// TermInst - A terminal instruction
// "Terminal" means that the instruction triggers a control flow change
// It must appear at the end of a block
// If TermInst.std::shared_ptr<Block> XXX is nullptr, it means that the control
// flow will "break" from the current BlockList, and enter the entry block
// of the next BlockList
class TermInst {
public:
	virtual ~TermInst() = default;	// Make TermInst polymorphic
};

class BranchInst : public TermInst {
public:
	Exp cond;
	std::shared_ptr<Block> true_block;
	std::shared_ptr<Block> false_block;
};

enum class jump_inst_t {
	NORMAL,
	BREAK,
	CONTINUE
};
class JumpInst : public TermInst {
public:
	jump_inst_t type = jump_inst_t::NORMAL;	// The type of the jump instruction. It affects jump relocation
	std::shared_ptr<Block> target_block;
};

class ReturnInst : public TermInst {
public:
	std::shared_ptr<Exp> ret_exp;	// is nullptr if the function returns void
};

// Block - A basic block
// Each basic block contains zero or more instructions, and one terminal instruction
class Block {
public:
	int id;			// Id. Unique within each function
	string name;	// An optional name. Will be generated automatically if not provided
	list<std::shared_ptr<Inst>> insts;
	std::shared_ptr<TermInst> term_inst;
};


// BlockList - A list of basic blocks, corresponds to "Block" in AST
// The first block will be considered as the entry block
//
// # Blocklist Construction
//
// We define a block as "complete", iff:
//  [1] It has a terminal instruction
//  [2] The target block of the terminal instruction is not nullptr
//  [3] The block contains all the instructions it should have
// 
// Most structures in AST, e.g. block and block_item, are converted to BlockList.
// Compared to full blocks, this blocklist may contain incomplete blocks. In
// particular:
//  [1] All blocks satisfy [1]
//  [2] The target block of a terminal instruction may be nullptr, meaning that
//      the control flow will leave the current blocklist, heading to the entry
//      block of the next blocklist.
// 		Specifically, for a BlockList returned by `ast2kirt(AST::Exp &exp)`, or
//		returned by `ast2kirt(some assignment statement without "recur")`
// 		all blocks except the last one contains a complete terminal instruction,
// 		while the last block has a JumpInst with target = nullptr
//  [3] The first block, and only the first block, may not satisfy [3]
// Besides, the returned block list must contain at least one block.
// 
// # Block Naming
// 
// Each block has a distinctive name. This name may be used for:
// - debugging
// - labels in Koopa IR
// - labels in assembly code
// 
// During the ast2kirt process, the name of a block is given at the same time
// at the block "becomes complete".
class BlockList {
public:
	// The first block will be considered as the entry block
	list<std::shared_ptr<Block>> blocks;
};


// FuncFParam - A function formal parameter
class FuncFParam {
public:
	Type type;
	string ident;
};


// FuncLocalVar - A function local variable
class FuncLocalVar {
public:
	Type type;
	string ident;
};


// Function - A function definition
class Function {
public:
	type_t ret_type;	// Can only be INT or VOID
	string name;
	vector<FuncFParam> fparams;
	vector<FuncLocalVar> local_vars;
	BlockList blocks;
};


// GlobalDecl - A global variable declaration
class GlobalDecl {
public:
	Type type;
	string ident;
	int init_val;	// For type == INT
	vector<int> arr_init_vals;	// For type == ARR
};


// Program - The entire program
class Program {
public:
	list<std::shared_ptr<Function>> funcs;
	list<std::shared_ptr<GlobalDecl>> global_decls;
};

extern std::map<string, std::shared_ptr<Function>> func_map;
extern std::map<string, std::shared_ptr<GlobalDecl>> global_decl_map;

}
