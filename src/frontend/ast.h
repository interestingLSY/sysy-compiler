// ast.h - Definition of AST nodes
#pragma once

#include <string>
#include <memory>

namespace AST {

enum class type_t {
	INT
};

enum class exp_t {
	NUMBER,

	POSITIVE,	// This only appears in UnaryOp
	NEGATIVE,
	LOGICAL_NOT,

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

	LOGICAL_AND,
	LOGICAL_OR
};

inline bool is_exp_t_unary(exp_t type) {
	return type == exp_t::POSITIVE || type == exp_t::NEGATIVE || type == exp_t::LOGICAL_NOT;
}
inline bool is_exp_t_binary(exp_t type) {
	return type != exp_t::NUMBER && !is_exp_t_unary(type);
}

class Base {
public:
	virtual ~Base() = default;
	virtual void print(int depth = 0) const = 0;
};

class CompUnit;
class TopLevel;

class VarDef;

class FuncDef;
class FuncType;

class Block;
class BlockBody;
class BlockItem;

class Stmt;
class ReturnStmt;
class AssignStmt;

class Exp;
class LVal;

// CompUnit - The entire program
class CompUnit : public Base {
public:
	std::unique_ptr<TopLevel> top_level;

	void print(int depth) const;
};

// TopLevel - A top-level declaration, can be recursive
// We do not directive make CompUnit recursive since it is a "special" node in Bison
class TopLevel : public Base {
public:
	std::unique_ptr<Base> def;	// Can be VarDef or FuncDef
	std::unique_ptr<TopLevel> recur;

	void print(int depth) const;
};


// VarDef - A variable/constant definition, can be recursive
// My compiler does not distinguish between variable and constant
class VarDef : public Base {
public:
	std::string ident;
	std::unique_ptr<Exp> init_val;
	std::unique_ptr<VarDef> recur;

	void print(int depth) const;
};

// FuncDef - A function definition
class FuncDef : public Base {
public:
	type_t ret_type;
	std::string ident;
	std::unique_ptr<Block> block;

	void print(int depth) const;
};

// Block - A block of statements, looks like `{ XXX }`
class Block : public Base {
public:
	std::unique_ptr<BlockBody> body;

	void print(int depth) const;
};

// BlockBody - The body of a block, contains block items, can be recursive
class BlockBody : public Base {
public:
	std::unique_ptr<Base> item;	// Can be a Stmt or a VarDef
	std::unique_ptr<BlockBody> recur;
	
	void print(int depth) const;
};

class Stmt : public Base {
};

class NopStmt : public Stmt {
public:
	void print(int depth) const;
};

class ReturnStmt : public Stmt {
public:
	std::unique_ptr<Exp> ret_exp;

	void print(int depth) const;
};

class AssignStmt : public Stmt {
public:
	std::unique_ptr<LVal> lval;
	std::unique_ptr<Exp> exp;

	void print(int depth) const;	
};

class LVal : public Base {
public:
	std::string ident;

	void print(int depth) const;
};

class Exp : public Base {
public:
	exp_t type;

	int number;	// Only valid when type == NUMBER
	std::unique_ptr<Exp> lhs;	// Valid for binary ops
	std::unique_ptr<Exp> rhs;	// Valid for binary ops and unary ops

	void print(int depth) const;
};


}
