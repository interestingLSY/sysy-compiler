#pragma once

#include <string>
#include <memory>

namespace AST {

enum class Type {
	INT
};

class Base {
public:
	virtual ~Base() = default;
	virtual void print(int depth = 0) const = 0;
};

class CompUnit;
class TopLevel;
class TopLevelDef;
class FuncDef;
class FuncType;
class Block;
class BlockBody;
class BlockItem;
class Stmt;
class ReturnStmt;

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
	std::unique_ptr<TopLevelDef> def;
	std::unique_ptr<TopLevel> recur;

	void print(int depth) const;
};

// An abstract class for top level definition, can be Decl / FuncDef
class TopLevelDef : public Base {
};

// FuncDef - A function definition
class FuncDef : public TopLevelDef {
public:
	std::unique_ptr<FuncType> ret_type;
	std::string ident;
	std::unique_ptr<Block> block;

	void print(int depth) const;
};

// FuncType - The return type of a function
class FuncType : public Base {
public:
	Type type;

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
	std::unique_ptr<BlockItem> item;
	std::unique_ptr<BlockBody> recur;
	
	void print(int depth) const;
};

// BlockItem - An item in a block (abstract), can be a variable declaration (Decl) of a statement (Stmt)
class BlockItem : public Base {
};

class Stmt : public BlockItem {
};

class ReturnStmt : public Stmt {
public:
	int number;

	void print(int depth) const;
};

}
