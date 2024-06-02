#pragma once

#include "middleend/kirt.h"

namespace KIRT {

enum class exp_category_t {
	NUMBER,
	SPECIAL,	// LVAL, FUNC_CALL, ARR_ADDR
	BINARY,
	UNARY
};

inline exp_category_t get_exp_category(exp_t type) {
	switch (type) {
		case exp_t::NUMBER:
			return exp_category_t::NUMBER;

		case exp_t::LVAL:
		case exp_t::FUNC_CALL:
		case exp_t::ARR_ADDR:
			return exp_category_t::SPECIAL;

		case exp_t::ADDR_ADD:
		case exp_t::ADD:
		case exp_t::SUB:
		case exp_t::MUL:
		case exp_t::DIV:
		case exp_t::REM:
		case exp_t::LT:
		case exp_t::GT:
		case exp_t::LEQ:
		case exp_t::GEQ:
		case exp_t::EQ:
		case exp_t::NEQ:
		case exp_t::BITWISE_AND:
		case exp_t::BITWISE_OR:
		case exp_t::BITWISE_XOR:
		case exp_t::SHL:
		case exp_t::SHR:
		case exp_t::SAR:
			return exp_category_t::BINARY;
		
		case exp_t::EQ0:
		case exp_t::NEQ0:
			return exp_category_t::UNARY;
		
		default:
			assert(0);
	}
}

inline int calc_exp_val(exp_t type, int lhs, int rhs = 0) {
	switch (type) {
		case exp_t::ADDR_ADD:
		case exp_t::ADD:
			return lhs + rhs;
		case exp_t::SUB:
			return lhs - rhs;
		case exp_t::MUL:
			return lhs * rhs;
		case exp_t::DIV:
			return lhs / rhs;
		case exp_t::REM:
			return lhs % rhs;
		case exp_t::LT:
			return lhs < rhs;
		case exp_t::GT:
			return lhs > rhs;
		case exp_t::LEQ:
			return lhs <= rhs;
		case exp_t::GEQ:
			return lhs >= rhs;
		case exp_t::EQ:
			return lhs == rhs;
		case exp_t::NEQ:
			return lhs != rhs;
		case exp_t::BITWISE_AND:
			return lhs & rhs;
		case exp_t::BITWISE_OR:
			return lhs | rhs;
		case exp_t::BITWISE_XOR:
			return lhs ^ rhs;
		case exp_t::SHL:
			return lhs << rhs;
		case exp_t::SHR:
			return (int)((unsigned)lhs >> rhs);
		case exp_t::SAR:
			return lhs >> rhs;
		case exp_t::EQ0:
			return lhs == 0;
		case exp_t::NEQ0:
			return lhs != 0;
		default:
			assert(0);
	}
}

inline bool operator==(const Exp &e1, const Exp &e2) {
	if (e1.type != e2.type)
		return false;
	exp_category_t cat = get_exp_category(e1.type);
	switch (cat) {
		case exp_category_t::NUMBER:
			return e1.number == e2.number;
		case exp_category_t::SPECIAL: {
			switch (e1.type) {
				case exp_t::LVAL:
					if (e1.lval.is_int() != e2.lval.is_int())
						return false;
					if (e1.lval.ident != e2.lval.ident)
						return false;
					if (e1.lval.is_int()) return true;
					else {
						assert (e1.lval.type.dims() == 1);
						return e1.lval.indices[0] == e2.lval.indices[0];
					}
				case exp_t::FUNC_CALL:
					return false;
				case exp_t::ARR_ADDR:
					return e1.arr_name == e2.arr_name;
				default:
					assert(0);
			}
		}
		case exp_category_t::BINARY:
			return *e1.lhs == *e2.lhs && *e1.rhs == *e2.rhs;
		case exp_category_t::UNARY:
			return *e1.lhs == *e2.lhs;
		default:
			assert(0);
	}
}

}
