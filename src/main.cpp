#include <cassert>
#include <cstdio>
#include <iostream>
#include <memory>
#include <string>

#include "frontend/ast.h"
#include "middleend/ast2kirt.h"
#include "middleend/kirt2strkir.h"

extern FILE *yyin;
extern int yyparse(std::unique_ptr<AST::Base> &ast);

// Mode selection
bool print_ast = false; 
bool print_ir = false;
bool print_asm = false;

int main(int argc, const char *argv[]) {
  assert(argc == 5);
  std::string mode = std::string(argv[1]);
  auto input = argv[2];
  auto output = argv[4];

  if (mode == "-ast") {
    print_ast = true;
  } else if (mode == "-koopa") {
    print_ir = true;
  } else if (mode == "-riscv") {
    print_asm = true;
  } else if (mode == "-debug") {
    print_ast = true;
    print_ir = true;
    print_asm = true;
  } else {
    std::cerr << "Invalid mode: " << mode << std::endl;
    assert(false);
  }

  // Read the input file
  yyin = fopen(input, "r");
  assert(yyin);

  // Parse the AST
  std::unique_ptr<AST::Base> ast;
  auto ret = yyparse(ast);
  assert(!ret);

  if (print_ast) {
    ast->print();
  }
  
  KIRT::Program kirt = KIRT::ast2kirt(*static_cast<AST::CompUnit *>(ast.get()));
  std::list<std::string> strkir = KIRT::kirt2str(kirt);

  // Print KIR
  if (print_ir) {
    for (const std::string &line : strkir) {
      std::cout << line << std::endl;
    }
  }

  return 0;
}


