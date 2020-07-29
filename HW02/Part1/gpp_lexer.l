%%
"(+ " {printf("OP_OP\nOP_PLUS\n");}
"(- " {printf("OP_OP\nOP_MINUS\n");}
"(/ " {printf("OP_OP\nOP_DIV\n");}
"(* " {printf("OP_OP\nOP_MULT\n");}
"(" {printf("OP_OP\n");}
")" {printf("OP_CP\n");}
"(** " {printf("OP_OP\nOP_DBLMULT\n");}
"(“" {printf("OP_OC\n");}
"," {printf("OP_OP\nOP_COMMA\n");}
"(and " {printf("OP_OP\nKW_AN\nD\n");}
"(or " {printf("OP_OP\nKW_OR\n");}
"(not " {printf("OP_OP\nNOT\n");}
"(equal " {printf("OP_OP\nKW_EQUAL\n");}
"(less " {printf("OP_OP\nKW_LESS\n");}
"(nil " {printf("OP_OP\nKW_NIL\n");}
"(list " {printf("OP_OP\nKW_LIST\n");}
"(append " {printf("OP_OP\nKW_APPEND\n");}
"(concat " {printf("OP_OP\nKW_CONCAT\n");}
"(set " {printf("OP_OP\nKW_SET\n");}
"(deffun " {printf("OP_OP\nKW_DEFFUN\n");}
"(for " {printf("OP_OP\nKW_FOR\n");}
"(if " {printf("OP_OP\nKW_IF\n");}
"(exit " {printf("OP_OP\nKW_EXIT\n");}
"(load " {printf("OP_OP\n\nKW_LOAD\n");}
"(disp " {printf("OP_OP\nKW_DISP\n");}
"(true " {printf("OP_OP\nKW_TRUE\n");}
"(false " {printf("OP_OP\nKW_FALSE\n");}
";;".* {printf("COMMENT");}
[0-9]* {printf("VALUE\n");}
[a-zA-Z_][a-zA-Z0-9_]* {printf("IDENTIFIER\n");}
%%

main(void){
yylex();
printf("\n");
}