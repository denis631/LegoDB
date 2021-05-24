%{
open Ast
%}

%token <int> INT
%token <string> ID
%token <string> STRING
%token SEMICOLON
%token COMMA
%token SELECT
%token WHERE
%token AND
%token FROM
%token LPAR
%token RPAR
%token EQ
%token EOF

%start <Ast.sql_expr> query

%%

query:
    | e = select; { e }
    ;

/* TODO: extend the supported select grammar https://github.com/mysql/mysql-workbench/blob/8.0/library/parsers/grammars/MySQLParser.g4#L1063 */
select:
    | SELECT; attr_lst = attribute_list; FROM; tbl_lst = table_list; SEMICOLON;
      { Select ({ attr_lst; tbl_lst; pred_lst=[]; }) }
    | SELECT; attr_lst = attribute_list; FROM; tbl_lst = table_list; WHERE; pred_lst = predicate_list; SEMICOLON;
      { Select ({ attr_lst; tbl_lst; pred_lst; }) }
    ;

attribute_list:
    | x = attribute; COMMA; xs = attribute_list  { x::xs }
    | x = attribute;                             { [x]   }
    ;

table_list:
    | x = ID; COMMA; xs = table_list             { (TblName x)::xs }
    | x = ID;                                    { [(TblName x)]   }
    ;

predicate_list:
    | x = predicate; AND; xs = predicate_list    { x::xs }
    | x = predicate;                             { [x]   }
    ;

predicate:
    |  attr  = attribute; EQ; const = constant;  { EqConst (attr, const)  }
    |  attr1 = attribute; EQ; attr2 = attribute; { EqAttr  (attr1, attr2) }
    ;

attribute:
    | attr = ID                                  { AttrName attr }

constant:
    | s = STRING                                 { Str s }
    | i = INT                                    { Int i }
    ;
