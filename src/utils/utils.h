#pragma once

// Test whether a pointer can be dynamically casted to a target type
template<
	typename POINTER_T,
	typename TARGET_T
>
bool is_instance_of_(const POINTER_T &ptr) {
	return dynamic_cast<TARGET_T>(ptr) != nullptr;
}
#define is_instance_of(ptr, TARGET_T) (is_instance_of_<decltype(ptr), TARGET_T>(ptr))
