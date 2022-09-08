%{
open Ast
open Expr
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
%token LIMIT_SYMBOL
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
  | CREATE_SYMBOL; TABLE_SYMBOL; name = tbl_ref; LPAR_SYMBOL; elt_list = option(tbl_elt_list); RPAR_SYMBOL; SEMICOLON_SYMBOL;
    { CreateTbl (name, Option.value elt_list ~default:[]) }
;

drop_tbl:
  | DROP_SYMBOL; TABLE_SYMBOL; tbls = separated_list(COMMA_SYMBOL, tbl_ref); SEMICOLON_SYMBOL;
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
    from_clause = from_clause;
    where_clause = option(where_clause);
    order_clause = option(order_clause);
    limit_clause = option(limit_clause);
    SEMICOLON_SYMBOL;
      { Select ( attr_list, from_clause, where_clause, order_clause, limit_clause) }

/* add limit */
;

select_item_list:
  | items = separated_list(COMMA_SYMBOL, select_item)                           { items }
;

select_item:
  | attr = qualifiedIdentifier;
    { Attr attr }
  | STAR_SYMBOL;
    { Star }
;

from_clause:
  | FROM_SYMBOL; tbl_list = separated_list(COMMA_SYMBOL, tbl_factor)
    { FromClause tbl_list }
;

where_clause:
  | WHERE_SYMBOL; expr = separated_list(AND_SYMBOL, expr)
    { WhereClause (Match.Expr.And expr) }
;

expr:
  | pred = predicate
    { pred }

  /* | NOT_SYMBOL; expr = expr */
  /*   { Match.Expr.Not expr } */
;

predicate:
  | lhs = predicate_elt; EQ_SYMBOL; rhs = predicate_elt
    { Match.Expr.Eq (lhs, rhs) }
;

predicate_elt:
  | attr = attribute
    { Match.Expr.Leaf (Match.Expr.TableAttrName attr) }
  | s = STRING
    { Match.Expr.Leaf (Match.Expr.Const (Value.StringLiteral s)) }
  | i = INT
    { Match.Expr.Leaf (Match.Expr.Const (Value.Integer (Int64.of_int i))) }
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
  | attr = qualifiedIdentifier; dir = option(direction)
    { OrderExpr (attr, Option.value dir ~default:Order.Ascending) }
;

limit_clause:
  | LIMIT_SYMBOL; limit = INT { LimitClause limit }

/* AWS COPY-like command */
copy:
  /* TODO: add opportunity to add a separator */
  | COPY_SYMBOL; name = tbl_ref; FROM_SYMBOL; path = STRING; SEMICOLON_SYMBOL;
    { Copy (name, path) }

// --- Common basic rules ------------------------------------------------------------------------
attribute:
  | attr = ID                                  { attr }

tbl_factor:
  | tbl = single_tbl
    { tbl }
  /* derived_tbl (subquery) */
;

single_tbl:
  | tbl = tbl_ref /* tbl_alias */
    { tbl }
;

tbl_ref:
  | id = qualifiedIdentifier
    { id }
;

col_name:
  | id = identifier                            { id }

identifier:
  | id = ID                                    { id }
;

/* TODO: add support for dotted identifiers */
/* dotIdentifier: */
/*   | DOT_SYMBOL identifier */
/* ; */

qualifiedIdentifier:
  | id = identifier                            { id }
  /* | dotIdentifier */
;
