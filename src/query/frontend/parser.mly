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
%token LPAR_SYMBOL
%token RPAR_SYMBOL
%token CREATE_SYMBOL
%token TABLE_SYMBOL
%token INT_SYMBOL
%token NUMERIC_SYMBOL
%token CHAR_SYMBOL
%token VARCHAR_SYMBOL
%token TIMESTAMP_SYMBOL
%token EQ_SYMBOL
%token EOF

%start <Ast.sql_expr> query

%%

query:
  | e = dml; { DML (e) }
  | e = ddl; { DDL (e) }
;

// --- DDL ---------------------------------------------------------------------------------------
ddl:
  | e = create_tbl; { e }
;

create_tbl:
  | CREATE_SYMBOL; TABLE_SYMBOL; name = tbl_name; LPAR_SYMBOL; elt_lst = tbl_elt_lst; RPAR_SYMBOL; SEMICOLON_SYMBOL;
    { CreateTbl (name, elt_lst) }
;

tbl_elt_lst:
  | x = tbl_elt; COMMA_SYMBOL; xs = tbl_elt_lst       { x::xs }
  | x = tbl_elt                                { [x] }
;

tbl_elt:
  | col = col_def                              { col }
  /* | tbl_constraint_def */
;

col_def:
  | name = col_name; field = fieldDefinition;  { ColDef (name, field) }
;

/* tbl_constraint_def: */
/*     type = (KEY_SYMBOL | INDEX_SYMBOL) indexNameAndType? keyListVariants indexOption* */
/*     | type = FULLTEXT_SYMBOL keyOrIndex? indexName? keyListVariants fulltextIndexOption* */
/*     | type = SPATIAL_SYMBOL keyOrIndex? indexName? keyListVariants spatialIndexOption* */
/*     | constraintName? ( */
/*         (type = PRIMARY_SYMBOL KEY_SYMBOL | type = UNIQUE_SYMBOL keyOrIndex?) indexNameAndType? keyListVariants indexOption* */
/*         | type = FOREIGN_SYMBOL KEY_SYMBOL indexName? keyList references */
/*         | checkConstraint ({serverVersion >= 80017}? constraintEnforcement)? */
/*     ) */
/* ; */

/* constraintName: */
/*     CONSTRAINT_SYMBOL identifier? */
/* ; */

fieldDefinition:
  | t = data_type                              { t }
    /* dataType col_attr* */
;

data_type:
  | INT_SYMBOL                                        { Value_type.Integer }
  | NUMERIC_SYMBOL; l = floatOptions                  { Value_type.Numeric (fst l, snd l) }
  | CHAR_SYMBOL; l = fieldLength                      { Value_type.Char l }
  | VARCHAR_SYMBOL; l = fieldLength                   { Value_type.VarChar l }
  | TIMESTAMP_SYMBOL;                                 { Value_type.Timestamp }
;

floatOptions:
    /* fieldLength */
  | x = precision                              { x }
;

precision:
  | LPAR_SYMBOL; a = INT; COMMA_SYMBOL; b = INT; RPAR_SYMBOL        { (a, b) }

fieldLength:
  | LPAR_SYMBOL; a = INT; RPAR_SYMBOL                        { a }

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
  | e = select; { e }
;

/* TODO: extend the supported select grammar https://github.com/mysql/mysql-workbench/blob/8.0/library/parsers/grammars/MySQLParser.g4#L1063 */
select:
  | SELECT_SYMBOL; attr_lst = select_item_list; tbl_lst = from_clause; pred_lst=option(where_clause); SEMICOLON_SYMBOL;
    { Select ( attr_lst, tbl_lst, pred_lst) }
;

select_item_list:
  | x = select_item; COMMA_SYMBOL; xs = select_item_list  { x::xs  }
  | x = select_item;                               { [x]    }
;

select_item:
  | x = attribute; { x    }
  | STAR_SYMBOL;          { Star }
;

from_clause:
  | FROM_SYMBOL; tbl_lst = table_list                 { tbl_lst }
;

table_list:
  | x = ID; COMMA_SYMBOL; xs = table_list             { x::xs }
  | x = ID;                                           { [x]   }
;

where_clause:
  | WHERE_SYMBOL; pred_lst = predicate_list           { pred_lst }
;

predicate_list:
  | x = predicate; AND_SYMBOL; xs = predicate_list    { x::xs }
  | x = predicate;                             { [x]   }
;

predicate:
  |  attr  = attribute; EQ_SYMBOL; const = constant;  { EqConst (attr, const)  }
  |  attr1 = attribute; EQ_SYMBOL; attr2 = attribute; { EqAttr  (attr1, attr2) }
;

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
