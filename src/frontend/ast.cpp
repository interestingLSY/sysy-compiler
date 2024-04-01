#include "ast.h"

#include <cassert>
#include <iostream>

using std::cout, std::endl;

namespace AST {

static std::string indent(int depth) {
    return std::string(depth * 4, ' ');
}

static std::string type2str(type_t type) {
    switch (type) {
        case type_t::INT:
            return "int";
        default:
            assert(0);
            return "unknown";
    }
}

static std::string exp_t2str(exp_t type) {
    switch (type) {
        case exp_t::NUMBER:
            return "number";
        case exp_t::POSITIVE:
            return "positive";
        case exp_t::NEGATIVE:
            return "negative";
        case exp_t::LOGICAL_NOT:
            return "logical_not";
        case exp_t::ADD:
            return "add";
        case exp_t::SUB:
            return "sub";
        case exp_t::MUL:
            return "mul";
        case exp_t::DIV:
            return "div";
        case exp_t::REM:
            return "rem";
        case exp_t::LT:
            return "lt";
        case exp_t::GT:
            return "gt";
        case exp_t::LEQ:
            return "leq";
        case exp_t::GEQ:
            return "geq";
        case exp_t::EQ:
            return "eq";
        case exp_t::NEQ:
            return "neq";
        case exp_t::LOGICAL_AND:
            return "logical_and";
        case exp_t::LOGICAL_OR:
            return "logical_or";
        default:
            assert(0);
            return "unknown";
    }
}

void CompUnit::print(int depth) const {
    cout << indent(depth) << "CompUnit {" << endl;
    top_level->print(depth + 1);
    cout << indent(depth) << "}" << endl;
}

void TopLevel::print(int depth) const {
    cout << indent(depth) << "TopLevel {" << endl;
    def->print(depth + 1);
    if (recur) {
        recur->print(depth);
    }
    cout << indent(depth) << "}" << endl;
}

void VarDef::print(int depth) const {
    cout << indent(depth) << "VarDef {" << endl;
    cout << indent(depth + 1) << "ident: " << ident << endl;
    if (init_val) {
        init_val->print(depth + 1);
    }
    if (recur) {
        recur->print(depth);
    }
    cout << indent(depth) << "}" << endl;
}

void FuncDef::print(int depth) const {
    cout << indent(depth) << "FuncDef {" << endl;
    cout << indent(depth + 1) << "type: " << type2str(ret_type) << endl;
    cout << indent(depth + 1) << "ident: " << ident << endl;
    block->print(depth + 1);
    cout << indent(depth) << "}" << endl;
}

void Block::print(int depth) const {
    cout << indent(depth) << "Block {" << endl;
    body->print(depth + 1);
    cout << indent(depth) << "}" << endl;
}

void BlockBody::print(int depth) const {
    cout << indent(depth) << "BlockBody {" << endl;
    item->print(depth + 1);
    if (recur) {
        recur->print(depth);
    }
    cout << indent(depth) << "}" << endl;
}

void NopStmt::print(int depth) const {
    cout << indent(depth) << "NopStmt" << endl;
}

void ReturnStmt::print(int depth) const {
    cout << indent(depth) << "ReturnStmt {" << endl;
    ret_exp->print(depth + 1);
    cout << indent(depth) << "}" << endl;
}

void AssignStmt::print(int depth) const {
    cout << indent(depth) << "AssignStmt {" << endl;
    lval->print(depth + 1);
    exp->print(depth + 1);
    cout << indent(depth) << "}" << endl;
}

void LVal::print(int depth) const {
    cout << indent(depth) << "LVal {" << endl;
    cout << indent(depth + 1) << "ident: " << ident << endl;
    cout << indent(depth) << "}" << endl;
}

void Exp::print(int depth) const {
    cout << indent(depth) << "Exp {" << endl;
    cout << indent(depth+1) << "type: " << exp_t2str(type) << endl;
    if (type == exp_t::NUMBER) {
        cout << indent(depth+1) << "number: " << number << endl;
    } else if (is_exp_t_unary(type)) {
        rhs->print(depth + 1);
    } else if (is_exp_t_binary(type)) {
        lhs->print(depth + 1);
        rhs->print(depth + 1);
    }
    cout << indent(depth) << "}" << endl;
}


}