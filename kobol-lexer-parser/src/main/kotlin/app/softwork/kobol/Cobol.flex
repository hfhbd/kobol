package app.softwork.kobol;

import com.intellij.lexer.FlexLexer;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.TokenType;
import static app.softwork.kobol.CobolTypes.*;

%%

%class CobolLexer
%implements FlexLexer
%unicode
%function advance
%type IElementType
%eof{  return;
%eof}

NUMBER=\d+(\.\d+)?
LINENUMBER=\d{6}
WHITE_SPACE=\s+
END_OF_LINE_COMMENT=\*.*
STRING=('([^'\\]|\\.)*'|\"([^\"\\]|\\.)*\")
VARNAME=[a-zA-Z]([\w|-]+[\w|_])*

%state IDENTIFICATION
%state ENVIRONMENT
%state DATA
%state PROCEDURE


%state PROGRAMID
%state ANY
%state MOVE

%state WORKINGSTORAGE
%state WORKINGSTORAGE_SA
%state WORKINGSTORAGE_SA_NUMBER
%state WORKINGSTORAGE_SA_NUMBER_LINE
%state WORKINGSTORAGE_SA_NAME

%state CONFIGURATION
%state SPECIAL_NAMES
%state SPECIAL_NAMES_START
%state FILE_CONTROL
%state FILE_CONTROL_START

%%

"DIVISION"                      { return DIVISION; }
"SECTION"                       { return SECTION; }

<YYINITIAL>
{
    "IDENTIFICATION"                { yybegin(IDENTIFICATION); return CobolTypes.IDENTIFICATION; }
    "ENVIRONMENT"                   { yybegin(ENVIRONMENT); return CobolTypes.ENVIRONMENT; }
    "DATA"                          { yybegin(DATA); return CobolTypes.DATA; }
    "PROCEDURE"                     { yybegin(PROCEDURE); return CobolTypes.PROCEDURE; }
}

<ENVIRONMENT> {
    "CONFIGURATION"                 { yybegin(CONFIGURATION); return CobolTypes.CONFIGURATION; }
    "INPUT-OUTPUT"                  { return INPUT_OUTPUT_LITERAL; }
    "SELECT"                        { return FILE_CONFIG_SELECT_LITERAL; }
    "ASSIGN"                        { return FILE_CONFIG_ASSIGN_LITERAL; }
    "FILE"                          { return FILE_LITERAL; }
    "STATUS"                        { return FILE_CONFIG_STATUS_STATUS_LITERAL; }
    "FILE-CONTROL"                  { yybegin(FILE_CONTROL_START); return CobolTypes.FILE_CONTROL_LITERAL; }


    "DATA"                          { yybegin(DATA); return CobolTypes.DATA; }
    "PROCEDURE"                     { yybegin(PROCEDURE); return CobolTypes.PROCEDURE; }
}

<FILE_CONTROL_START> {
"."                             { yybegin(FILE_CONTROL); return DOT; }
}

<FILE_CONTROL> {
    "SELECT"                        { return FILE_CONFIG_SELECT_LITERAL; }
    "ASSIGN"                        { return FILE_CONFIG_ASSIGN_LITERAL; }
    "FILE"                          { return FILE_LITERAL; }
    "STATUS"                        { return FILE_CONFIG_STATUS_STATUS_LITERAL; }
    "TO"                            { return CobolTypes.TO; }

    {VARNAME}                       { return VARNAME; }
    "."                             { yybegin(ENVIRONMENT); return DOT; }
}

<CONFIGURATION> {
    "SPECIAL-NAMES"                 { yybegin(SPECIAL_NAMES_START); return SPECIAL_NAMES_LITERAL; }
}

<SPECIAL_NAMES_START> {
"."                             { yybegin(SPECIAL_NAMES); return DOT; }
}

<SPECIAL_NAMES> {
    "IS"                            { return IS; }

    {VARNAME}                       { return VARNAME; }
    "."                             { yybegin(ENVIRONMENT); return DOT; }
}

<DATA> {
  "PROCEDURE"                     { yybegin(PROCEDURE); return CobolTypes.PROCEDURE; }

  "WORKING-STORAGE"               { yybegin(WORKINGSTORAGE); return WORKING_STORAGE; }
}

<WORKINGSTORAGE> {
  "SECTION"                       { return SECTION; }
  "01"                            { return CobolTypes.RECORD; }
  "77"                            { yybegin(WORKINGSTORAGE_SA_NAME); return SA_LITERAL; }
  "PROCEDURE"                     { yybegin(PROCEDURE);return CobolTypes.PROCEDURE; }
}

<WORKINGSTORAGE_SA_NAME> {
    {VARNAME} { yybegin(WORKINGSTORAGE_SA); return VARNAME; }
}

<WORKINGSTORAGE_SA> {
"PIC" { return PIC_LITERAL;}
      "X" { return PIC_XA;}
      "A" { return PIC_XA;}
      "9" { return PIC_9;}
      "S" { return PIC_S;}
      "(" { return LP; }
      ")" { return RP; }
      {NUMBER} { return NUMBER;}
      "VALUE" { yybegin(WORKINGSTORAGE_SA_NUMBER); return VALUE;}
      "." { yybegin(WORKINGSTORAGE); return DOT;}
}
<WORKINGSTORAGE_SA_NUMBER> {
      {NUMBER} { yybegin(WORKINGSTORAGE_SA_NUMBER_LINE); return NUMBER; }
      {STRING} { yybegin(WORKINGSTORAGE_SA_NUMBER_LINE); return STRING; }
}
<WORKINGSTORAGE_SA_NUMBER_LINE> {
      {LINENUMBER} { yybegin(WORKINGSTORAGE); return TokenType.WHITE_SPACE; }
      "." { yybegin(WORKINGSTORAGE); return DOT; }
}

<PROCEDURE> {
    "DISPLAY"                       { return CobolTypes.DISPLAY_LITERAL; }
    "MOVE"                          { yybegin(MOVE); return CobolTypes.MOVE; }
    "TO"                            { return CobolTypes.TO; }

    "PERFORM"                       { return PERFORM; }
    "IS"                            { return IS; }

    {VARNAME}                       { return VARNAME; }
}

<IDENTIFICATION> {
    "AUTHOR"                        { yybegin(ANY); return AUTHOR; }
    "INSTALLATION"                  { yybegin(ANY); return INSTALLATION; }
    "DATE-WRITTEN"                  { yybegin(ANY); return DATE; }
    "PROGRAM-ID"                    { yybegin(PROGRAMID); return PROGRAM_ID; }

    "ENVIRONMENT"                   { yybegin(ENVIRONMENT); return CobolTypes.ENVIRONMENT; }
    "DATA"                          { yybegin(DATA); return CobolTypes.DATA; }
    "PROCEDURE"                     { yybegin(PROCEDURE); return CobolTypes.PROCEDURE; }
}

<PROGRAMID> {
    "."                             { return DOT; }
    {VARNAME}                       { yybegin(IDENTIFICATION); return VARNAME; }
}

<ANY> {
    {WHITE_SPACE}                   { return TokenType.WHITE_SPACE; }
    {LINENUMBER}                    { yybegin(IDENTIFICATION); return TokenType.WHITE_SPACE; }
    [^]                             { return CobolTypes.ANY; }
}

<MOVE> {
      {NUMBER}                        { return NUMBER; }
      {LINENUMBER}                    { return NUMBER; }
      {VARNAME}                       { return VARNAME; }
      "TO"                            { yybegin(PROCEDURE); return CobolTypes.TO; }
}

    {LINENUMBER}                    { return TokenType.WHITE_SPACE; }
    {WHITE_SPACE}                   { return TokenType.WHITE_SPACE; }
    "/"                             { return TokenType.WHITE_SPACE; }
    {END_OF_LINE_COMMENT}           { return COMMENT; }
    "."                             { return DOT; }
    {STRING}                        { return STRING; }
    [^]                             { return TokenType.BAD_CHARACTER; }
