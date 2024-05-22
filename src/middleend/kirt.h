// kirt.h - Definition of KIR nodes
#pragma once

#include <list>
#include <memory>
#include <string>
#include <vector>
#include <map>

namespace KIRT {

using std::list;
using std::string;
using std::shared_ptr;
using std::vector;

enum class type_t {
	INT,
	VOID
};

enum class exp_t {
	NUMBER,

	LVAL,

	FUNC_CALL,

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
	string ident;	// For LVAL or FUNC_CALL
	shared_ptr<Exp> lhs;
	shared_ptr<Exp> rhs;
	vector<shared_ptr<Exp>> args;	// For FUNC_CALL

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
	string ident;
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
	string name;
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
//  [4] Has a meaningful, non-empty name
// 
// Most structures in AST, e.g. block and block_item, are converted to BlockList.
// Compared to full blocks, this blocklist may contain incomplete blocks. In
// particular:
//  [1] All blocks satisfy [1]
//  [2] The target block of a terminal instruction may be nullptr, meaning that
//      the control flow will leave the current blocklist, heading to the entry
//      block of the next blocklist.
// 		Specifically, for a BlockList returned by `ast2kirt(AST::Exp &exp)`, 
// 		all blocks except the last one contains a complete terminal instruction,
// 		while the last block has a JumpInst with target = nullptr
//  [3] The first block, and only the first block, may not satisfy [3]
//  [4] The first block, and only the first block, may not satisfy [4]
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
	type_t type;
	string ident;
};


// Function - A function definition
class Function {
public:
	type_t ret_type;
	string name;
	vector<FuncFParam> fparams;
	BlockList blocks;
};


// GlobalDecl - A global variable declaration
class GlobalDecl {
public:
	type_t type;
	string ident;
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
