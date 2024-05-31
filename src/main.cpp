#include <cassert>
#include <cstdio>
#include <iostream>
#include <memory>
#include <string>
#include <fstream>

#include "frontend/ast.h"
#include "middleend/ast2kirt.h"
#include "middleend/kirt2strkir.h"
#include "middleend/pass_repl_unasm.h"
#include "middleend/pass_collapse_arr.h"
#include "middleend/pass_fill_block_name.h"
#include "backend/kirt2asm.h"

extern FILE *yyin;
extern int yyparse(std::unique_ptr<AST::Base> &ast);

bool print_to_file = false;
std::ostream *output_stream = &std::cout;

// Mode selection
bool print_ast = false; 
bool print_ir = false;
bool print_asm = false;

int main(int argc, const char *argv[]) {
  if (argc != 5 && argc != 3) {
    std::cerr << "Usage: " << argv[0] << " <mode> <input> [-o <output>]" << std::endl;
    return 1;
  }
  std::string mode = std::string(argv[1]);
  auto input = argv[2];
  if (argc == 5) {
    assert(std::string(argv[3]) == "-o");
    print_to_file = true;
    auto output_file_path = argv[4];
    output_stream = new std::ofstream;
    static_cast<std::ofstream *>(output_stream)->open(output_file_path);
  }

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
  fprintf(stderr, "Parsing AST...\n");
  std::unique_ptr<AST::Base> ast;
  auto ret = yyparse(ast);
  assert(!ret);

  if (print_ast) {
    // AST does not support print to file
    ast->print();
  }
  
  fprintf(stderr, "Generating KIRT...\n");
  KIRT::Program kirt = KIRT::ast2kirt(*static_cast<AST::CompUnit *>(ast.get()));

  fprintf(stderr, "Running pass: replace unasmable instructions...\n");
  KIRT::pass_repl_unasm(kirt);

  fprintf(stderr, "Running pass: collapse arrays...\n");
  KIRT::pass_collapse_arr(kirt);

  fprintf(stderr, "Running pass: fill block names...\n");
  KIRT::pass_fill_block_name(kirt);

  // Print KIR
  fprintf(stderr, "Converting KIRT to string...\n");
  std::list<std::string> strkir = KIRT::kirt2str(kirt);
  if (print_ir) {
    for (const std::string &line : strkir) {
      *output_stream << line << std::endl;
    }
  }

  if (print_asm) {
    std::list<std::string> strasm = ASM::kirt2asm(kirt);
    for (const std::string &line : strasm) {
      *output_stream << line << std::endl;
    }
  }

  return 0;
}


