%{
open Ast
open Utils
%}

%token <int> INT
%token <string> ID
%token <string> STRING
%token SEMICOLON_SYMBOL
%token STAR_SYMBOL
%token COMMA_SYMBOL
%token SELECT_SYMBOL
%token WHERE_SYMBOL
%token AND_SYMBOL
%token FROM_SYMBOL
%token ORDER_SYMBOL
%token BY_SYMBOL
%token ASC_SYMBOL
%token DESC_SYMBOL
%token LPAR_SYMBOL
%token RPAR_SYMBOL
%token CREATE_SYMBOL
%token DROP_SYMBOL
%token COPY_SYMBOL
%token TABLE_SYMBOL
%token PRIMARY_SYMBOL
%token KEY_SYMBOL
%token INT_SYMBOL
%token NUMERIC_SYMBOL
%token CHAR_SYMBOL
%token VARCHAR_SYMBOL
%token TIMESTAMP_SYMBOL
%token EQ_SYMBOL
%token EOF

%start <Ast.sql_expr> query

%%

/* TODO: extend the supported select grammar https://github.com/mysql/mysql-workbench/blob/8.0/library/parsers/grammars/MySQLParser.g4#L1063 */
query:
  | e = dml; { e }
  | e = ddl; { e }
;

// --- DDL ---------------------------------------------------------------------------------------
ddl:
  | e = create_tbl; { e }
  | e = drop_tbl;   { e }
;

create_tbl:
  | CREATE_SYMBOL; TABLE_SYMBOL; name = tbl_name; LPAR_SYMBOL; elt_list = option(tbl_elt_list); RPAR_SYMBOL; SEMICOLON_SYMBOL;
    { CreateTbl (name, Option.value elt_list ~default:[]) }
;

drop_tbl:
  | DROP_SYMBOL; TABLE_SYMBOL; tbls = separated_list(COMMA_SYMBOL, tbl_name); SEMICOLON_SYMBOL;
    { DropTbl (tbls) }
;

tbl_elt_list:
  | elts = separated_list(COMMA_SYMBOL, tbl_elt)                                { elts }
;

tbl_elt:
  | col = col_def                                                               { col }
  | constraint_def = tbl_constraint_def                                         { constraint_def }
;

col_def:
  | name = col_name; field = fieldDefinition;                                   { ColDef (name, field) }
;

tbl_constraint_def:
  | PRIMARY_SYMBOL; KEY_SYMBOL; cols = key_list_with_expr                       { ConstraintDef(PrimaryKey, cols) }
;

key_list_with_expr:
(* TODO: col_name is not really correct, as col_name will have dotted support later on *)
  | LPAR_SYMBOL; cols = separated_list(COMMA_SYMBOL, col_name) RPAR_SYMBOL      { cols }

fieldDefinition:
  | t = data_type                                                               { t }
    /* dataType col_attr* */
;

data_type:
  | INT_SYMBOL                                                                  { Value_type.Integer }
  | NUMERIC_SYMBOL; l = float_options                                           { Value_type.Numeric (fst l, snd l) }
  | CHAR_SYMBOL; l = fieldLength                                                { Value_type.Char l }
  | VARCHAR_SYMBOL; l = fieldLength                                             { Value_type.VarChar l }
  | TIMESTAMP_SYMBOL;                                                           { Value_type.Timestamp }
;

float_options:
    /* fieldLength */
  | x = precision                                                               { x }
;

precision:
  | LPAR_SYMBOL; a = INT; COMMA_SYMBOL; b = INT; RPAR_SYMBOL                    { (a, b) }

fieldLength:
  | LPAR_SYMBOL; a = INT; RPAR_SYMBOL                                           { a }

/* col_attr: */
/*     NOT_SYMBOL? nullLiteral */
/*     | {serverVersion >= 80014}? NOT_SYMBOL SECONDARY_SYMBOL */
/*     | value = DEFAULT_SYMBOL ( */
/*         signedLiteral */
/*         | NOW_SYMBOL timeFunctionParameters? */
/*         | {serverVersion >= 80013}? exprWithParentheses */
/*     ) */
/*     | value = ON_SYMBOL UPDATE_SYMBOL NOW_SYMBOL timeFunctionParameters? */
/*     | value = AUTO_INCREMENT_SYMBOL */
/*     | value = SERIAL_SYMBOL DEFAULT_SYMBOL VALUE_SYMBOL */
/*     | PRIMARY_SYMBOL? value = KEY_SYMBOL */
/*     | value = UNIQUE_SYMBOL KEY_SYMBOL? */
/*     | value = COMMENT_SYMBOL textLiteral */
/*     | collate */
/*     | value = COLUMN_FORMAT_SYMBOL columnFormat */
/*     | value = STORAGE_SYMBOL storageMedia */
/*     | {serverVersion >= 80000}? value = SRID_SYMBOL real_ulonglong_number */
/*     | {serverVersion >= 80017}? constraintName? checkConstraint */
/*     | {serverVersion >= 80017}? constraintEnforcement */
/* ; */

// --- DML ---------------------------------------------------------------------------------------
dml:
  | e = select;                                                                 { e }
  | c = copy;                                                                   { c }
;

select:
  | SELECT_SYMBOL;
    attr_list = select_item_list;
    tbl_list = from_clause;
    pred_list = option(where_clause);
    order_clause = option(order_clause);
    SEMICOLON_SYMBOL;
      { Select ( attr_list, tbl_list, pred_list, order_clause) }
;

select_item_list:
  | items = separated_list(COMMA_SYMBOL, select_item)                           { items }
;

select_item:
  | x = attribute;                                                              { x }
  | STAR_SYMBOL;                                                                { Star }
;

from_clause:
  | FROM_SYMBOL; tbl_list = table_list                                          { tbl_list }
;

table_list:
  | ids = separated_list(COMMA_SYMBOL, ID)                                      { ids }
;

where_clause:
  | WHERE_SYMBOL; pred_list = predicate_list                                    { pred_list }
;

predicate_list:
  | preds = separated_list(AND_SYMBOL, predicate)                               { preds }
;

predicate:
  |  attr  = attribute; EQ_SYMBOL; const = constant;  { EqConst (attr, const)  }
  |  attr1 = attribute; EQ_SYMBOL; attr2 = attribute; { EqAttr  (attr1, attr2) }
;

order_clause:
  | ORDER_SYMBOL; BY_SYMBOL; order_list = order_list
    { OrderClause order_list }
;

direction:
  | ASC_SYMBOL  { Order.Ascending  }
  | DESC_SYMBOL { Order.Descending }
;

order_list:
  | exprs = separated_list(COMMA_SYMBOL, order_expr)
    { exprs }
;

order_expr:
  | attr = ID; dir = option(direction)
    { OrderExpr (attr, Option.value dir ~default:Order.Ascending) }
;

/* AWS COPY-like command */
copy:
  | COPY_SYMBOL; name = tbl_name; FROM_SYMBOL; path = STRING; SEMICOLON_SYMBOL;
    { Copy (name, path) }

// --- Common basic rules ------------------------------------------------------------------------
attribute:
  | attr = ID                                  { AttrName attr }

constant:
  | s = STRING                                 { Str s }
  | i = INT                                    { Int i }
;

tbl_name:
  | id = qualifiedIdentifier                   { id }
;

col_name:
  | id = identifier                            { id }

identifier:
  | id = ID                                     { id }
;

/* TODO: add support for dotted identifiers */
/* dotIdentifier: */
/*   | DOT_SYMBOL identifier */
/* ; */

qualifiedIdentifier:
  | id = identifier                            { id }
  /* | dotIdentifier */
;
