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
        case type_t::VOID:
            return "void";
        default:
            assert(0);
            return "unknown";
    }
}

static std::string exp_t2str(exp_t type) {
    switch (type) {
        case exp_t::NUMBER:
            return "number";
        case exp_t::LVAL:
            return "lval";
        case exp_t::FUNC_CALL:
            return "func_call";
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
    lval->print(depth + 1);
    if (is_const) {
        cout << indent(depth + 1) << "constant" << endl;
    }
    if (init_val) {
        init_val->print(depth + 1);
    }
    if (init_val_list) {
        init_val_list->print(depth + 1);
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
    if (fparam) {
        fparam->print(depth + 1);
    }
    block->print(depth + 1);
    cout << indent(depth) << "}" << endl;
}

void FuncFParam::print(int depth) const {
    cout << indent(depth) << "FuncFParam {" << endl;
    lval->print(depth + 1);
    if (recur) {
        recur->print(depth);
    }
    cout << indent(depth) << "}" << endl;
}

void Block::print(int depth) const {
    cout << indent(depth) << "Block {" << endl;
    item->print(depth + 1);
    cout << indent(depth) << "}" << endl;
}

void BlockItem::print(int depth) const {
    cout << indent(depth) << "BlockItem {" << endl;
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
    if (ret_exp)
        ret_exp->print(depth + 1);
    cout << indent(depth) << "}" << endl;
}

void AssignStmt::print(int depth) const {
    cout << indent(depth) << "AssignStmt {" << endl;
    lval->print(depth + 1);
    exp->print(depth + 1);
    cout << indent(depth) << "}" << endl;
}

void IfStmt::print(int depth) const {
    cout << indent(depth) << "IfStmt {" << endl;
    cond->print(depth + 1);
    then->print(depth + 1);
    if (otherwise) {
        otherwise->print(depth + 1);
    }
    cout << indent(depth) << "}" << endl;
}

void WhileStmt::print(int depth) const {
    cout << indent(depth) << "WhileStmt {" << endl;
    cond->print(depth + 1);
    body->print(depth + 1);
    cout << indent(depth) << "}" << endl;
}

void BreakStmt::print(int depth) const {
    cout << indent(depth) << "BreakStmt" << endl;
}

void ContinueStmt::print(int depth) const {
    cout << indent(depth) << "ContinueStmt" << endl;
}

void ExpStmt::print(int depth) const {
    cout << indent(depth) << "ExpStmt {" << endl;
    exp->print(depth + 1);
    cout << indent(depth) << "}" << endl;
}

void LVal::print(int depth) const {
    cout << indent(depth) << "LVal {" << endl;
    cout << indent(depth + 1) << "ident: " << ident << endl;
    if (type == lval_t::ARR) {
        cout << indent(depth + 1) << "array" << endl;
    }
    if (type == AST::lval_t::ARR) {
        for (auto &exp : indices) {
            exp->print(depth + 1);
        }
    }
    cout << indent(depth) << "}" << endl;
}

void Exp::print(int depth) const {
    cout << indent(depth) << "Exp {" << endl;
    cout << indent(depth+1) << "type: " << exp_t2str(type) << endl;
    if (type == exp_t::NUMBER) {
        cout << indent(depth+1) << "number: " << number << endl;
    } else if (type == exp_t::LVAL) {
        lval->print(depth + 1);
    } else if (type == exp_t::FUNC_CALL) {
        cout << indent(depth+1) << "func_name: " << func_name << endl;
        if (func_rparam) {
            func_rparam->print(depth + 1);
        }
    } else if (is_exp_t_unary(type)) {
        rhs->print(depth + 1);
    } else if (is_exp_t_binary(type)) {
        lhs->print(depth + 1);
        rhs->print(depth + 1);
    } else {
        assert(0);
    }
    cout << indent(depth) << "}" << endl;
}

void InitValList::print(int depth) const {
    cout << indent(depth) << "InitValList {" << endl;
    for (auto &item : items) {
        item->print(depth + 1);
    }
    cout << indent(depth) << "}" << endl;
}

void FuncRParam::print(int depth) const {
    cout << indent(depth) << "FuncRParam {" << endl;
    exp->print(depth + 1);
    if (recur) {
        recur->print(depth);
    }
    cout << indent(depth) << "}" << endl;
}


}