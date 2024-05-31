// ast.h - Definition of AST nodes
#pragma once

#include <string>
#include <memory>
#include <vector>

namespace AST {

enum class type_t {
	INT,
	VOID
};

enum class exp_t {
	NUMBER,

	LVAL,

	FUNC_CALL,

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

enum class lval_t {
	VAR,
	ARR
};

inline bool is_exp_t_unary(exp_t type) {
	return type == exp_t::POSITIVE || type == exp_t::NEGATIVE || type == exp_t::LOGICAL_NOT;
}
inline bool is_exp_t_binary(exp_t type) {
	return type == exp_t::ADD || type == exp_t::SUB || type == exp_t::MUL || type == exp_t::DIV || type == exp_t::REM ||
		type == exp_t::LT || type == exp_t::GT || type == exp_t::LEQ || type == exp_t::GEQ || type == exp_t::EQ || type == exp_t::NEQ ||
		type == exp_t::LOGICAL_AND || type == exp_t::LOGICAL_OR;
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
class FuncFParam;

class Block;
class BlockBody;
class BlockItem;

class Stmt;
class ReturnStmt;
class AssignStmt;

class Exp;
class InitValList;
class LVal;
class FuncRParam;

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
class VarDef : public Base {
public:
	std::unique_ptr<LVal> lval;

	bool is_const;	// True if it is a constant (defined with `const`)
	std::unique_ptr<Exp> init_val;	// Valid when the variable is not an array
	std::unique_ptr<InitValList> init_val_list;	// Valid when the variable is an array

	std::unique_ptr<VarDef> recur;

	void print(int depth) const;
};

// FuncDef - A function definition
class FuncDef : public Base {
public:
	type_t ret_type;
	std::string ident;
	std::unique_ptr<FuncFParam> fparam;
	std::unique_ptr<Block> block;

	void print(int depth) const;
};

// FuncFParam: A parameter in a function definition, can be recursive
class FuncFParam : public Base {
public:
	std::unique_ptr<LVal> lval;	// Here "indices" represents the array shape
	std::unique_ptr<FuncFParam> recur;

	void print(int depth) const;
};

// Block - A block of statements, looks like `{ XXX }`
class Block : public Base {
public:
	std::unique_ptr<BlockItem> item;

	void print(int depth) const;
};

// BlockItem - One item in the block, can be recursive
class BlockItem : public Base {
public:
	std::unique_ptr<Base> item;	// Can be a Stmt, Vardef, or Block
	std::unique_ptr<BlockItem> recur;
	
	void print(int depth) const;
};

// Abstract class for all statements
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

class IfStmt : public Stmt {
public:
	std::unique_ptr<Exp> cond;
	std::unique_ptr<Base> then;			// Can be a Stmt or Block
	std::unique_ptr<Base> otherwise;	// Can be a Stmt, Block, or empty

	void print(int depth) const;
};

class WhileStmt : public Stmt {
public:
	std::unique_ptr<Exp> cond;
	std::unique_ptr<Base> body;	// Can be a Stmt or Block

	void print(int depth) const;
};

class BreakStmt : public Stmt {
public:
	void print(int depth) const;
};

class ContinueStmt : public Stmt {
public:
	void print(int depth) const;
};

class ExpStmt : public Stmt {
public:
	std::unique_ptr<Exp> exp;

	void print(int depth) const;
};

class LVal : public Base {
public:
	lval_t type;
	std::string ident;
	std::vector<std::unique_ptr<Exp>> indices;	// Only valid when type == ARR_REF

	void print(int depth) const;
};

class Exp : public Base {
public:
	exp_t type;

	int number;	// Only valid when type == NUMBER
	std::string func_name;	// Only valid when type == FUNC_CALL
	std::unique_ptr<FuncRParam> func_rparam;	// Only valid when type == FUNC_CALL
	std::unique_ptr<LVal> lval;	// Only valid when type == LVAL
	std::unique_ptr<Exp> lhs;	// Valid for binary ops
	std::unique_ptr<Exp> rhs;	// Valid for binary ops and unary ops

	void print(int depth) const;
};

// Array initialization list
class InitValList : public Base {
public:
	std::vector<std::unique_ptr<Base>> items;	// Can be InitValList or Exp

	void print(int depth) const;
};

class FuncRParam : public Base {
public:
	std::unique_ptr<Exp> exp;
	std::unique_ptr<FuncRParam> recur;

	void print(int depth) const;
};


}
