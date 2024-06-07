#include "optim_dbs.h"

namespace KIRT {

unordered_map<string, unordered_set<string>> func2callees;

unordered_map<string, unordered_set<string>> func2modified_global_vars;

unordered_map<string, unordered_set<string>> func2as_arred_params;

bool is_library_func(const string &func_name) {
	return func_name == "getint" || func_name == "getch" || func_name == "getarray" || \
			func_name == "putint" || func_name == "putch" || func_name == "putarray"|| \
			func_name == "starttime" || func_name == "stoptime";
}

// Return whether the given function is idempotent.
// To be idempotent, the function must:
// - Do not touch any global variables / arrays, either directly or indirectly
// - Do not have an array as a parameter
// - Do not call any library functions, either directly or indirectly
bool is_function_idempotent(const string &ident) {
	if (!func2modified_global_vars[ident].empty())
		return false;
	if (is_library_func(ident))
		return false;

	bool called_lib_func = false;
	for (const auto &callee : func2callees[ident]) {
		if (is_library_func(callee)) {
			called_lib_func = true;
			break;
		}
	}
	if (called_lib_func)
		return false;
		
	const shared_ptr<Function> &func = KIRT::func_map[ident];
	for (const auto &param : func->fparams) {
		if (param.type.is_arr())
			return false;
	}
	return true;
}

}
