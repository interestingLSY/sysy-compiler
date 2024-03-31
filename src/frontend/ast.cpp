#include "ast.h"

#include <cassert>
#include <iostream>

using std::cout, std::endl;

namespace AST {

std::string indent(int depth) {
    return std::string(depth * 4, ' ');
}

std::string type2str(Type type) {
    switch (type) {
        case Type::INT:
            return "int";
        default:
            assert(0);
            return "unknown";
    }
}

void CompUnit::print(int depth) const {
    cout << indent(depth) << "CompUnit {" << endl;
    func_def->print(depth + 1);
    cout << indent(depth) << "}" << endl;
}

void FuncDef::print(int depth) const {
    cout << indent(depth) << "FuncDef {" << endl;
    ret_type->print(depth + 1);
    cout << indent(depth + 1) << "ident: " << ident << endl;
    block->print(depth + 1);
    cout << indent(depth) << "}" << endl;
}

void FuncType::print(int depth) const {
    cout << indent(depth) << "FuncType {" << endl;
    cout << indent(depth + 1) << "type: " << type2str(type) << endl;
    cout << indent(depth) << "}" << endl;
}

void Block::print(int depth) const {
    cout << indent(depth) << "Block {" << endl;
    stmt->print(depth + 1);
    cout << indent(depth) << "}" << endl;
}

void Stmt::print(int depth) const {
    cout << indent(depth) << "Stmt {" << endl;
    cout << indent(depth + 1) << "number: " << number << endl;
    cout << indent(depth) << "}" << endl;
}


}