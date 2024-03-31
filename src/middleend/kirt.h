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

// Inst - An instruction
class Inst {
public:
	virtual ~Inst() = default;	// Make Inst polymorphic
};

class ReturnInst : public Inst {
public:
	int ret_val;
};

// Block - A basic block
class Block {
public:
	list<std::shared_ptr<Inst>> insts;
};

class BlockList {
public:
	list<Block> blocks;
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