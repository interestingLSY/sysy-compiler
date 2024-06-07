#pragma once

#include "middleend/kirt.h"

#include <string>
#include <unordered_map>
#include <unordered_set>

using std::unordered_map;
using std::unordered_set;

namespace KIRT {

// A mapping between function name and the set of functions it calls (directly or
// indirectly)
extern unordered_map<string, unordered_set<string>> func2callees;

// A mapping between function name and the set of global variables / arrays modified by
// the function (including the global var/array modified by the functions it calls)
extern unordered_map<string, unordered_set<string>> func2modified_global_vars;

// A mapping between function name and the set of array passed as parameters
// to callees (direct call only)
extern unordered_map<string, unordered_set<string>> func2as_arred_params;

bool is_library_func(const string &func_name);

bool is_function_idempotent(const string &ident);

}
