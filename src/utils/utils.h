#pragma once

#include <cstring>

// Test whether a pointer can be dynamically casted to a target type
template<
	typename POINTER_T,
	typename TARGET_T
>
bool is_instance_of_(const POINTER_T &ptr) {
	return dynamic_cast<TARGET_T>(ptr) != nullptr;
}
#define is_instance_of(ptr, TARGET_T) (is_instance_of_<decltype(ptr), TARGET_T>(ptr))

// An auto incrementing counter
class Counter {
public:
	int count;
	
	Counter() : count(0) {}
	void reset(int val = 0) { count = val; }
	int next() { return count++; }
};

// A function that wraps `sprintf` to return a `std::string`
template<typename... Args>
std::string format(const char *fmt, Args... args) {
	size_t nbytes = snprintf(NULL, 0, fmt, args...) + 1; /* +1 for the '\0' */
	char *str = (char*)malloc(nbytes);
	snprintf(str, nbytes, fmt, args...);
	std::string res(str);
	free(str);
	return res;
}
