%{
open Ast
%}

%token <int> INT
%token <string> ID
%token <string> STRING
%token SEMICOLON
%token STAR
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
  | SELECT; attr_lst = select_item_list; tbl_lst = from_clause; pred_lst=option(where_clause); SEMICOLON;
    { Select ( attr_lst, tbl_lst, pred_lst) }
;

select_item_list:
  | x = select_item; COMMA; xs = select_item_list  { x::xs  }
  | x = select_item;                               { [x]    }
;

select_item:
  | x = attribute; { x    }
  | STAR;          { Star }
;

from_clause:
  | FROM; tbl_lst = table_list                 { tbl_lst }
;

table_list:
  | x = ID; COMMA; xs = table_list             { (TblName x)::xs }
  | x = ID;                                    { [(TblName x)]   }
;

where_clause:
  | WHERE; pred_lst = predicate_list           { pred_lst }
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
