// kirt.h - Definition of KIR nodes
#pragma once

#include <list>
#include <memory>
#include <string>
#include <vector>

namespace KIRT {

using std::list;
using std::string;
using std::shared_ptr;
using std::vector;

enum class type_t {
	INT
};

enum class exp_t {
	NUMBER,

	LVAL,

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

// Exp - An expression
class Exp {
public:
	exp_t type;
	int number;		// For NUMBER
	string ident;	// For LVAL
	shared_ptr<Exp> lhs;
	shared_ptr<Exp> rhs;

	Exp() = default;
	Exp(int x) : type(exp_t::NUMBER), number(x) {}
};

// Inst - An instruction
class Inst {
public:
	virtual ~Inst() = default;	// Make Inst polymorphic
};

class ReturnInst : public Inst {
public:
	Exp ret_exp;
};

class AssignInst : public Inst {
public:
	string ident;
	Exp exp;
};

// Block - A basic block
class Block {
public:
	list<std::shared_ptr<Inst>> insts;
};

// BlockList - A list of basic blocks, corresponds to "Block" in AST
// The first block will be considered as the entry block, and
// the last block will be considered as the exit block (except return)
class BlockList {
public:
	// The first block will be considered as the entry block
	list<Block> blocks;

	BlockList() {
		// Make sure there is at least one block
		blocks.push_back(Block());
	}
};

// Function - A function definition
class Function {
public:
	type_t ret_type;
	string name;
	BlockList blocks;
};

// Program - The entire program
class Program {
public:
	list<std::shared_ptr<Inst>> global_defs;
	list<Function> funcs;
};

}