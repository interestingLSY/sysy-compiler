# find Flex/Bison
find_package(FLEX REQUIRED)
find_package(BISON REQUIRED)

# generate lexer/parser
set(L_SOURCES sysy.l)
set(Y_SOURCES sysy.y)
string(REGEX REPLACE "(.*)\\.l" "${CMAKE_CURRENT_BINARY_DIR}/\\1.lex.cpp" L_OUTPUTS "${L_SOURCES}")
string(REGEX REPLACE "(.*)\\.y" "${CMAKE_CURRENT_BINARY_DIR}/\\1.tab.cpp" Y_OUTPUTS "${Y_SOURCES}")
flex_target(Lexer ${L_SOURCES} ${L_OUTPUTS})
bison_target(Parser ${Y_SOURCES} ${Y_OUTPUTS})
add_flex_bison_dependency(Lexer Parser)

add_library(frontend ast.cpp
	${FLEX_Lexer_OUTPUTS} ${BISON_Parser_OUTPUT_SOURCE})
