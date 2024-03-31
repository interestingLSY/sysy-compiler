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

class CompUnit : public Base {
public:
	std::unique_ptr<Base> func_def;

	void print(int depth) const;
};

class FuncDef : public Base {
public:
	std::unique_ptr<Base> ret_type;
	std::string ident;
	std::unique_ptr<Base> block;

	void print(int depth) const;
};

class FuncType : public Base {
public:
	Type type;

	void print(int depth) const;
};

class Block : public Base {
public:
	std::unique_ptr<Base> stmt;

	void print(int depth) const;
};

class Stmt : public Base {
public:
	int number;

	void print(int depth) const;
};

}
