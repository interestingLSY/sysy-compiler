%code requires {
  #include <memory>
  #include <string>
  #include "frontend/ast.h"
}
%define lr.type ielr

%{

#include <iostream>
#include <memory>
#include <string>
#include <vector>

#include "frontend/ast.h"
#include "utils/utils.h"

// 声明 lexer 函数和错误处理函数
int yylex();
void yyerror(std::unique_ptr<AST::Base> &ast, const char *s);

using std::string;
using std::to_string;
using std::unique_ptr;

// A helper function for casting AST::Base* to a specific type, and then
// wrapping it in a unique_ptr
template<typename TARGET>
std::unique_ptr<TARGET> cast_uptr(AST::Base *base) {
  TARGET* target = dynamic_cast<TARGET*>(base);
  if (target == nullptr) {
    throw std::runtime_error("cast_unique failed");
  }
  return std::unique_ptr<TARGET>(
    dynamic_cast<TARGET*>(base)
  );
}

// A helper class for representing a list of indices
class Indices {
public:
  std::unique_ptr<AST::Exp> exp;
  std::unique_ptr<Indices> recur;

  std::vector<std::unique_ptr<AST::Exp>> to_vector() {
    std::vector<std::unique_ptr<AST::Exp>> ret;
    ret.push_back(std::move(exp));
    std::unique_ptr<Indices> *cur = &recur;
    while (cur->get()) {
      ret.push_back(std::move((*cur)->exp));
      cur = &((*cur)->recur);
    }
    return ret;
  }
};

// A helper class for parsing the array initialization list
class InitValListItem : public AST::Base {
public:
  // If exp_item is non-null, it means this item is an expression
  // If list_item is non-null, it means this item is another InitValListItem
  // If both are null, it means "the current list is empty", which should only
  // appear as the first element of some list
  std::unique_ptr<AST::Exp> exp_item;
  std::unique_ptr<InitValListItem> list_item;

  std::unique_ptr<InitValListItem> recur;  // Points to the next item, can be nullptr

  std::unique_ptr<AST::InitValList> to_init_val_list() {
    std::unique_ptr<AST::InitValList> ret = std::make_unique<AST::InitValList>();
    if (!exp_item && !list_item) {
      // I am representing an empty list
      return ret;
    }
    InitValListItem* cur = this;
    while (cur) {
      if (cur->exp_item) {
        ret->items.push_back(std::move(cur->exp_item));
      } else {
        ret->items.push_back(cur->list_item->to_init_val_list());
      }
      cur = cur->recur.get();
    }
    return ret;
  }

  void print(int depth) const {}
};

%}

// 定义 parser 函数和错误处理函数的附加参数
// 我们需要返回一个字符串作为 AST, 所以我们把附加参数定义成字符串的智能指针
// 解析完成后, 我们要手动修改这个参数, 把它设置成解析得到的字符串
%parse-param { std::unique_ptr<AST::Base> &ast }

// yylval 的定义, 我们把它定义成了一个联合体 (union)
// 因为 token 的值有的是字符串指针, 有的是整数
// 之前我们在 lexer 中用到的 str_val 和 int_val 就是在这里被定义的
// 至于为什么要用字符串指针而不直接用 string 或者 unique_ptr<string>?
// 请自行 STFW 在 union 里写一个带析构函数的类会出现什么情况
%union {
  std::string *str_val;
  int int_val;
  AST::Base *ast_val;
  AST::type_t ast_type_val;
  class Indices *indices_val;
  class InitValListItem *init_val_list_val;
}

// lexer 返回的所有 token 种类的声明
%token CONST INT VOID
%token RETURN IF ELSE WHILE BREAK CONTINUE
%token LOGICAL_OR LOGICAL_AND EQ NEQ LEQ GEQ
%token <str_val> IDENT
%token <int_val> INT_CONST

// 非终结符的类型定义
%type <int_val> Number
%type <ast_type_val> Type
%type <indices_val> Indices
%type <init_val_list_val> InitValListItem
%type <ast_val> TopLevel TopLevelDef_
%type <ast_val> VarDecl_ VarDef SingleVarDef_
%type <ast_val> FuncDef SingleFuncFParam_ FuncFParam FuncRParam
%type <ast_val> Block BlockBody BlockItem_
%type <ast_val> LVal Exp LOrExp LAndExp EqExp RelExp AddExp MulExp UnaryExp UnaryOp PrimaryExp
%type <ast_val> Stmt ReturnStmt AssignStmt NopStmt ExpStmt
%type <ast_val> Stmt_ForceIfWithElse_ IfStmtForceWithElse_ IfStmtMaybeWithoutElse_ StmtOrBlock_ StmtOrBlock_ForceIfWithElse_
%type <ast_val> WhileStmt WhileStmtForceWithElse_ BreakStmt ContinueStmt

%%

CompUnit
  : TopLevel {
    auto comp_unit = std::make_unique<AST::CompUnit>();
    comp_unit->top_level = cast_uptr<AST::TopLevel>($1);
    ast = std::move(comp_unit);
  }


TopLevel
  : TopLevelDef_ {
    auto ast = new AST::TopLevel();
    ast->def = cast_uptr<AST::Base>($1);
    $$ = ast;
  }
  | TopLevelDef_ TopLevel {
    auto ast = new AST::TopLevel();
    ast->def = cast_uptr<AST::Base>($1);
    ast->recur = cast_uptr<AST::TopLevel>($2);
    $$ = ast;
  }


TopLevelDef_
  : FuncDef {
    $$ = $1;
  }
  | VarDecl_ {
    $$ = $1;
  }


VarDecl_
  : CONST Type VarDef ';' {
    // Here we must use `Type` instead of `INT` to avoid shift/reduce conflict,
    // although here `Type` can only be `INT`
    $$ = $3;
    AST::VarDef* cur_vardef = dynamic_cast<AST::VarDef*>($$);
    while (cur_vardef) {
      cur_vardef->is_const = true;
      cur_vardef = cur_vardef->recur.get();
    }
  }
  | Type VarDef ';' {
    $$ = $2;
    AST::VarDef* cur_vardef = dynamic_cast<AST::VarDef*>($$);
    while (cur_vardef) {
      cur_vardef->is_const = false;
      cur_vardef = cur_vardef->recur.get();
    }
  }


Type
  : INT {
    $$ = AST::type_t::INT;
  }
  | VOID {
    $$ = AST::type_t::VOID;
  }


InitValListItem
  : Exp {
    auto ast = new InitValListItem();
    ast->exp_item = cast_uptr<AST::Exp>($1);
    $$ = ast;
  }
  | Exp ',' InitValListItem {
    auto ast = new InitValListItem();
    ast->exp_item = cast_uptr<AST::Exp>($1);
    ast->recur = cast_uptr<InitValListItem>($3);
    $$ = ast;
  }
  | '{' '}' {
    auto lower_item = std::make_unique<InitValListItem>();
    lower_item->exp_item = nullptr;
    lower_item->list_item = nullptr;
    auto ast = new InitValListItem();
    ast->list_item = std::move(lower_item);
    $$ = ast;
  }
  | '{' '}' ',' InitValListItem {
    auto lower_item = std::make_unique<InitValListItem>();
    lower_item->exp_item = nullptr;
    lower_item->list_item = nullptr;
    auto ast = new InitValListItem();
    ast->list_item = std::move(lower_item);
    ast->recur = cast_uptr<InitValListItem>($4);
    $$ = ast;
  }
  | '{' InitValListItem '}' {
    auto ast = new InitValListItem();
    ast->list_item = cast_uptr<InitValListItem>($2);
    $$ = ast;
  }
  | '{' InitValListItem '}' ',' InitValListItem {
    auto ast = new InitValListItem();
    ast->list_item = cast_uptr<InitValListItem>($2);
    ast->recur = cast_uptr<InitValListItem>($5);
    $$ = ast;
  }

  
SingleVarDef_
  : LVal {
    auto ast = new AST::VarDef();
    ast->lval = cast_uptr<AST::LVal>($1);
    $$ = ast;
  }
  | LVal '=' Exp {
    auto ast = new AST::VarDef();
    ast->lval = cast_uptr<AST::LVal>($1);
    ast->init_val = cast_uptr<AST::Exp>($3);
    $$ = ast;
  }
  | LVal '=' '{' InitValListItem '}' {
    auto ast = new AST::VarDef();
    ast->lval = cast_uptr<AST::LVal>($1);
    ast->init_val_list = cast_uptr<InitValListItem>($4)->to_init_val_list();
    $$ = ast;
  }
  | LVal '=' '{' '}' {
    auto ast = new AST::VarDef();
    ast->lval = cast_uptr<AST::LVal>($1);
    ast->init_val_list = std::make_unique<AST::InitValList>();
    $$ = ast;
  }


VarDef
  : SingleVarDef_ {
    $$ = $1;
  }
  | SingleVarDef_ ',' VarDef {
    auto ast = (AST::VarDef*)($1);
    ast->recur = cast_uptr<AST::VarDef>($3);
    $$ = $1;
  }


FuncDef
  : Type IDENT '(' ')' Block {
    auto ast = new AST::FuncDef();
    ast->ret_type = $1;
    ast->ident = *unique_ptr<string>($2);
    ast->block = cast_uptr<AST::Block>($5);
    $$ = ast;
  }
  | Type IDENT '(' FuncFParam ')' Block {
    auto ast = new AST::FuncDef();
    ast->ret_type = $1;
    ast->ident = *unique_ptr<string>($2);
    ast->fparam = cast_uptr<AST::FuncFParam>($4);
    ast->block = cast_uptr<AST::Block>($6);
    $$ = ast;
  }


SingleFuncFParam_
  : INT IDENT {
    auto ast = new AST::FuncFParam();
    ast->lval = std::make_unique<AST::LVal>();
    ast->lval->type = AST::lval_t::VAR;
    ast->lval->ident = *unique_ptr<string>($2);
    $$ = ast;
  }
  | INT IDENT '[' ']' {
    auto ast = new AST::FuncFParam();
    ast->lval = std::make_unique<AST::LVal>();
    ast->lval->type = AST::lval_t::ARR;
    ast->lval->ident = *unique_ptr<string>($2);

    std::unique_ptr<AST::Exp> zero_exp = std::make_unique<AST::Exp>();
    zero_exp->type = AST::exp_t::NUMBER;
    zero_exp->number = 0;
    ast->lval->indices.push_back(std::move(zero_exp));
    $$ = ast;
  }
  | INT IDENT '[' ']' Indices {
    auto ast = new AST::FuncFParam();
    ast->lval = std::make_unique<AST::LVal>();
    ast->lval->type = AST::lval_t::ARR;
    ast->lval->ident = *unique_ptr<string>($2);
    ast->lval->indices = std::unique_ptr<Indices>($5)->to_vector();

    std::unique_ptr<AST::Exp> zero_exp = std::make_unique<AST::Exp>();
    zero_exp->type = AST::exp_t::NUMBER;
    zero_exp->number = 0;
    ast->lval->indices.insert(ast->lval->indices.begin(), std::move(zero_exp));
    $$ = ast;
  }


FuncFParam
  : SingleFuncFParam_ {
    $$ = $1;
  }
  | SingleFuncFParam_ ',' FuncFParam {
    auto ast = (AST::FuncFParam*)($1);
    ast->recur = cast_uptr<AST::FuncFParam>($3);
    $$ = $1;
  }


Block
  : '{' '}' {
    // An empty block
    auto item = std::make_unique<AST::BlockItem>();
    item->item = std::make_unique<AST::NopStmt>();
    auto ast = new AST::Block();
    ast->item = std::move(item);
    $$ = ast;
  }
  | '{' BlockBody '}' {
    auto ast = new AST::Block();
    ast->item = cast_uptr<AST::BlockItem>($2);
    $$ = ast;
  }
  

BlockBody
  : BlockItem_ {
    auto ast = new AST::BlockItem();
    ast->item = cast_uptr<AST::Base>($1);
    $$ = ast;
  }
  | BlockItem_ BlockBody {
    auto ast = new AST::BlockItem();
    ast->item = cast_uptr<AST::Base>($1);
    ast->recur = cast_uptr<AST::BlockItem>($2);
    $$ = ast;
  }


BlockItem_
  : Stmt {
    $$ = $1;
  }
  | VarDecl_ {
    $$ = $1;
  }
  | Block {
    // NOTE Here we modify the BNF to BlockItem ::= Decl | Stmt | Block
    // and Stmt ::= ... | "while" "(" Exp ")" BlockItem | ...
    // DO NOT FORGET TO MODIFY THE BNF WHEN INPLEMENTING IF/WHILE!!!

    // NOTE Xuanlin Jiang warns me that modifying the BNF is a super-dangerous
    // operation, but I do not believe him. Let's wait and see.
    $$ = $1;
  }


Stmt
  : ReturnStmt {
    $$ = $1;
  }
  | AssignStmt {
    $$ = $1;
  }
  | NopStmt {
    $$ = $1;
  }
  | ExpStmt {
    $$ = $1;
  }
  | WhileStmt {
    $$ = $1;
  }
  | BreakStmt {
    $$ = $1;
  }
  | ContinueStmt {
    $$ = $1;
  }
  | IfStmtMaybeWithoutElse_ {
    $$ = $1;
  }


Stmt_ForceIfWithElse_
  : ReturnStmt {
    $$ = $1;
  }
  | AssignStmt {
    $$ = $1;
  }
  | NopStmt {
    $$ = $1;
  }
  | ExpStmt {
    $$ = $1;
  }
  | WhileStmtForceWithElse_ {
    $$ = $1;
  }
  | BreakStmt {
    $$ = $1;
  }
  | ContinueStmt {
    $$ = $1;
  }
  | IfStmtForceWithElse_ {
    $$ = $1;
  }


ReturnStmt
  : RETURN Exp ';' {
    auto ast = new AST::ReturnStmt();
    ast->ret_exp = cast_uptr<AST::Exp>($2);
    $$ = ast;
  }
  | RETURN ';' {
    auto ast = new AST::ReturnStmt();
    ast->ret_exp = nullptr;
    $$ = ast;
  }
  

AssignStmt
  : LVal '=' Exp ';' {
    auto ast = new AST::AssignStmt();
    ast->lval = cast_uptr<AST::LVal>($1);
    ast->exp = cast_uptr<AST::Exp>($3);
    $$ = ast;
  }


IfStmtMaybeWithoutElse_
  : IF '(' Exp ')' StmtOrBlock_ForceIfWithElse_ ELSE StmtOrBlock_ {
    auto ast = new AST::IfStmt();
    ast->cond = cast_uptr<AST::Exp>($3);
    ast->then = cast_uptr<AST::Base>($5);
    ast->otherwise = cast_uptr<AST::Base>($7);
    $$ = ast;
  }
  | IF '(' Exp ')' StmtOrBlock_ {
    auto ast = new AST::IfStmt();
    ast->cond = cast_uptr<AST::Exp>($3);
    ast->then = cast_uptr<AST::Base>($5);
    $$ = ast;
  }


IfStmtForceWithElse_
  : IF '(' Exp ')' StmtOrBlock_ForceIfWithElse_ ELSE StmtOrBlock_ForceIfWithElse_ {
    auto ast = new AST::IfStmt();
    ast->cond = cast_uptr<AST::Exp>($3);
    ast->then = cast_uptr<AST::Base>($5);
    ast->otherwise = cast_uptr<AST::Base>($7);
    $$ = ast;
  }
  

StmtOrBlock_
  : Stmt {
    auto ast = new AST::BlockItem();
    ast->item = cast_uptr<AST::Base>($1);
    $$ = ast;
  }
  | Block {
    auto ast = new AST::BlockItem();
    ast->item = cast_uptr<AST::Base>($1);
    $$ = ast;
  }


StmtOrBlock_ForceIfWithElse_
  : Stmt_ForceIfWithElse_ {
    auto ast = new AST::BlockItem();
    ast->item = cast_uptr<AST::Base>($1);
    $$ = ast;
  }
  | Block {
    auto ast = new AST::BlockItem();
    ast->item = cast_uptr<AST::Base>($1);
    $$ = ast;
  }


WhileStmt
  : WHILE '(' Exp ')' StmtOrBlock_ {
    auto ast = new AST::WhileStmt();
    ast->cond = cast_uptr<AST::Exp>($3);
    ast->body = cast_uptr<AST::Base>($5);
    $$ = ast;
  }


WhileStmtForceWithElse_
  : WHILE '(' Exp ')' StmtOrBlock_ForceIfWithElse_ {
    auto ast = new AST::WhileStmt();
    ast->cond = cast_uptr<AST::Exp>($3);
    ast->body = cast_uptr<AST::Base>($5);
    $$ = ast;
  }


BreakStmt
  : BREAK ';' {
    $$ = new AST::BreakStmt();
  }


ContinueStmt
  : CONTINUE ';' {
    $$ = new AST::ContinueStmt();
  }


NopStmt
  : ';' {
    $$ = new AST::NopStmt();
  }


ExpStmt
  : Exp ';' {
    // EXP; <=> int XXXXX = EXP;
    auto ast = new AST::ExpStmt();
    ast->exp = cast_uptr<AST::Exp>($1);
    $$ = ast;
  }


LVal
  : IDENT {
    auto ast = new AST::LVal();
    ast->type = AST::lval_t::VAR;
    ast->ident = *unique_ptr<string>($1);
    $$ = ast;
  }
  | IDENT Indices {
    auto ast = new AST::LVal();
    ast->type = AST::lval_t::ARR;
    ast->ident = *unique_ptr<string>($1);
    ast->indices = std::unique_ptr<Indices>($2)->to_vector();
    $$ = ast;
  }


Indices
  : '[' Exp ']' {
    auto ast = new Indices();
    ast->exp = cast_uptr<AST::Exp>($2);
    $$ = ast;
  }
  | '[' Exp ']' Indices {
    auto ast = new Indices();
    ast->exp = cast_uptr<AST::Exp>($2);
    ast->recur = std::unique_ptr<Indices>($4);
    $$ = ast;
  }


Exp
  : LOrExp {
    $$ = $1;
  }


LOrExp
  : LAndExp {
    $$ = $1;
  }
  | LOrExp LOGICAL_OR LAndExp {
    auto ast = new AST::Exp();
    ast->type = AST::exp_t::LOGICAL_OR;
    ast->lhs = cast_uptr<AST::Exp>($1);
    ast->rhs = cast_uptr<AST::Exp>($3);
    $$ = ast;
  }


LAndExp
  : EqExp {
    $$ = $1;
  }
  | LAndExp LOGICAL_AND EqExp {
    auto ast = new AST::Exp();
    ast->type = AST::exp_t::LOGICAL_AND;
    ast->lhs = cast_uptr<AST::Exp>($1);
    ast->rhs = cast_uptr<AST::Exp>($3);
    $$ = ast;
  }


EqExp
  : RelExp {
    $$ = $1;
  }
  | EqExp EQ RelExp {
    auto ast = new AST::Exp();
    ast->type = AST::exp_t::EQ;
    ast->lhs = cast_uptr<AST::Exp>($1);
    ast->rhs = cast_uptr<AST::Exp>($3);
    $$ = ast;
  }
  | EqExp NEQ RelExp {
    auto ast = new AST::Exp();
    ast->type = AST::exp_t::NEQ;
    ast->lhs = cast_uptr<AST::Exp>($1);
    ast->rhs = cast_uptr<AST::Exp>($3);
    $$ = ast;
  }


RelExp
  : AddExp {
    $$ = $1;
  }
  | RelExp '<' AddExp {
    auto ast = new AST::Exp();
    ast->type = AST::exp_t::LT;
    ast->lhs = cast_uptr<AST::Exp>($1);
    ast->rhs = cast_uptr<AST::Exp>($3);
    $$ = ast;
  }
  | RelExp '>' AddExp {
    auto ast = new AST::Exp();
    ast->type = AST::exp_t::GT;
    ast->lhs = cast_uptr<AST::Exp>($1);
    ast->rhs = cast_uptr<AST::Exp>($3);
    $$ = ast;
  }
  | RelExp LEQ AddExp {
    auto ast = new AST::Exp();
    ast->type = AST::exp_t::LEQ;
    ast->lhs = cast_uptr<AST::Exp>($1);
    ast->rhs = cast_uptr<AST::Exp>($3);
    $$ = ast;
  }
  | RelExp GEQ AddExp {
    auto ast = new AST::Exp();
    ast->type = AST::exp_t::GEQ;
    ast->lhs = cast_uptr<AST::Exp>($1);
    ast->rhs = cast_uptr<AST::Exp>($3);
    $$ = ast;
  }


AddExp
  : MulExp {
    $$ = $1;
  }
  | AddExp '+' MulExp {
    auto ast = new AST::Exp();
    ast->type = AST::exp_t::ADD;
    ast->lhs = cast_uptr<AST::Exp>($1);
    ast->rhs = cast_uptr<AST::Exp>($3);
    $$ = ast;
  }
  | AddExp '-' MulExp {
    auto ast = new AST::Exp();
    ast->type = AST::exp_t::SUB;
    ast->lhs = cast_uptr<AST::Exp>($1);
    ast->rhs = cast_uptr<AST::Exp>($3);
    $$ = ast;
  }


MulExp
  : UnaryExp {
    $$ = $1;
  }
  | MulExp '*' UnaryExp {
    auto ast = new AST::Exp();
    ast->type = AST::exp_t::MUL;
    ast->lhs = cast_uptr<AST::Exp>($1);
    ast->rhs = cast_uptr<AST::Exp>($3);
    $$ = ast;
  }
  | MulExp '/' UnaryExp {
    auto ast = new AST::Exp();
    ast->type = AST::exp_t::DIV;
    ast->lhs = cast_uptr<AST::Exp>($1);
    ast->rhs = cast_uptr<AST::Exp>($3);
    $$ = ast;
  }
  | MulExp '%' UnaryExp {
    auto ast = new AST::Exp();
    ast->type = AST::exp_t::REM;
    ast->lhs = cast_uptr<AST::Exp>($1);
    ast->rhs = cast_uptr<AST::Exp>($3);
    $$ = ast;
  }


UnaryExp
  : UnaryOp UnaryExp {
    if (((AST::Exp*)($1))->type == AST::exp_t::POSITIVE) {
      // Pass through if it's a positive sign
      $$ = $2;
    } else {
      auto ast = new AST::Exp();
      ast->type = cast_uptr<AST::Exp>($1) -> type;
      ast->rhs = cast_uptr<AST::Exp>($2);
      $$ = ast;
    }
  }
  | PrimaryExp {
    $$ = $1;
  }
  | IDENT '(' ')' {
    auto ast = new AST::Exp();
    ast->type = AST::exp_t::FUNC_CALL;
    ast->func_name = *unique_ptr<string>($1);
    $$ = ast;
  }
  | IDENT '(' FuncRParam ')' {
    auto ast = new AST::Exp();
    ast->type = AST::exp_t::FUNC_CALL;
    ast->func_name = *unique_ptr<string>($1);
    ast->func_rparam = cast_uptr<AST::FuncRParam>($3);
    $$ = ast;
  }


FuncRParam
  : Exp {
    auto ast = new AST::FuncRParam();
    ast->exp = cast_uptr<AST::Exp>($1);
    $$ = ast;
  }
  | Exp ',' FuncRParam {
    auto ast = new AST::FuncRParam();
    ast->exp = cast_uptr<AST::Exp>($1);
    ast->recur = cast_uptr<AST::FuncRParam>($3);
    $$ = ast;
  }


UnaryOp
  : '+' {
    auto ast = new AST::Exp();
    ast->type = AST::exp_t::POSITIVE;
    $$ = ast;
  }
  | '-' {
    auto ast = new AST::Exp();
    ast->type = AST::exp_t::NEGATIVE;
    $$ = ast;
  }
  | '!' {
    auto ast = new AST::Exp();
    ast->type = AST::exp_t::LOGICAL_NOT;
    $$ = ast;
  }


PrimaryExp
  : '(' Exp ')' {
    $$ = $2;
  }
  | Number {
    auto ast = new AST::Exp();
    ast->type = AST::exp_t::NUMBER;
    ast->number = $1;
    $$ = ast;
  }
  | LVal {
    auto ast = new AST::Exp();
    ast->type = AST::exp_t::LVAL;
    ast->lval = cast_uptr<AST::LVal>($1);
    $$ = ast;
  }


Number
  : INT_CONST {
    $$ = $1;
  }
  

%%

// 定义错误处理函数, 其中第二个参数是错误信息
// parser 如果发生错误 (例如输入的程序出现了语法错误), 就会调用这个函数
void yyerror(std::unique_ptr<AST::Base> &ast, const char *s) {
  std::cerr << "error: " << s << std::endl;
}
