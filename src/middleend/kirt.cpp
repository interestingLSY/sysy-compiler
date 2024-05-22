#include "kirt.h"

namespace KIRT {
	std::map<string, shared_ptr<Function>> func_map;
	std::map<string, std::shared_ptr<GlobalDecl>> global_decl_map;
}
