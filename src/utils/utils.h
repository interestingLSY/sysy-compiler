#pragma once

#include <cstring>
#include <list>

// Test whether a pointer can be dynamically casted to a target type
template<
	typename POINTER_T,
	typename TARGET_T
>
static inline bool is_instance_of_(const POINTER_T &ptr) {
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
static inline std::string format(const char *fmt, Args... args) {
	size_t nbytes = snprintf(NULL, 0, fmt, args...) + 1; /* +1 for the '\0' */
	char *str = (char*)malloc(nbytes);
	snprintf(str, nbytes, fmt, args...);
	std::string res(str);
	free(str);
	return res;
}

// Some helper functions for list manupulation
template<typename T>
static inline std::list<T>& operator<<(std::list<T> &lhs, std::list<T> &rhs) {
	lhs.splice(lhs.end(), rhs);
	return lhs;
}

template<typename T>
static inline std::list<T>& operator>>(std::list<T> &lhs, std::list<T> &rhs) {
	rhs.splice(rhs.begin(), lhs);
	return lhs;
}

template<typename T>
static inline std::list<T>& operator<=(std::list<T> &lhs, const std::list<T> &rhs) {
	lhs.insert(lhs.end(), rhs.begin(), rhs.end());
	return lhs;
}

template<typename T>
static inline std::list<T>& operator>=(std::list<T> &lhs, const std::list<T> &rhs) {
	lhs.insert(lhs.begin(), rhs.begin(), rhs.end());
	return lhs;
}

// The online judge returns the exit code when the program aborts
// We use an `assert()` with custom return code to handle this
#define my_assert(code, cond) \
	if (!(cond)) { \
		fprintf(stderr, "Assertion failed: %s\n", #cond); \
		exit(code); \
	}

inline int log2_floor(int x) {
	if (x <= 0) return -1;
	if (x == 1) return 0;
	int res = 0;
	while (x != 1) {
		res += 1;
		x >>= 1;
	}
	return res;
}
