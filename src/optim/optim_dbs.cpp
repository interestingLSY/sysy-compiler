#include "optim_dbs.h"

namespace KIRT {

unordered_map<string, unordered_set<string>> func2callees;

unordered_map<string, unordered_set<string>> func2modified_global_arrs;

unordered_map<string, unordered_set<string>> func2as_arred_params;

}
