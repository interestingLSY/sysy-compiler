#pragma once

#include "middleend/kirt.h"

namespace KIRT {

enum class exp_category_t {
	NUMBER,
	SPECIAL,	// LVAL, FUNC_CALL, ARR_ADDR
	BINARY,
	UNARY
};

exp_category_t get_exp_category(exp_t type) {
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

int calc_exp_val(exp_t type, int lhs, int rhs = 0) {
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

}
