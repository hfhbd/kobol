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

%state PROGRAMID
%state ANY
%state MOVE

%state WORKINGSTORAGE
%state WORKINGSTORAGE_SA
%state WORKINGSTORAGE_SA_NUMBER
%state WORKINGSTORAGE_SA_NUMBER_LINE
%state WORKINGSTORAGE_SA_NAME

%%

<YYINITIAL>
{
    "IDENTIFICATION"                { return CobolTypes.IDENTIFICATION; }
    "ENVIRONMENT"                   { return CobolTypes.ENVIRONMENT; }
    "DATA"                          { return CobolTypes.DATA; }
    "PROCEDURE"                     { return CobolTypes.PROCEDURE; }
    "AUTHOR"                        { yybegin(ANY); return AUTHOR; }
    "INSTALLATION"                  { yybegin(ANY); return INSTALLATION; }
    "DATE-WRITTEN"                  { yybegin(ANY); return DATE; }
    "PROGRAM-ID"                    { yybegin(PROGRAMID); return PROGRAM_ID; }
    "DISPLAY"                       { return CobolTypes.DISPLAY; }
    "MOVE"                          { yybegin(MOVE); return CobolTypes.MOVE; }
    "TO"                            { return CobolTypes.TO; }
    "DIVISION"                      { return DIVISION; }
    "WORKING-STORAGE"               { yybegin(WORKINGSTORAGE); return WORKING_STORAGE; }
    "SECTION"                       { return SECTION; }
    "PERFORM"                       { return PERFORM; }
    "IS"                            { return IS; }
    "CONFIGURATION"                 { return CONFIGURATION; }
    "SPECIAL-NAMES"                 { return SPECIAL_NAMES_LITERAL; }
    "INPUT-OUTPUT"                  { return INPUT_OUTPUT_LITERAL; }
    "SELECT"                        { return FILE_CONFIG_SELECT_LITERAL; }
    "ASSIGN"                        { return FILE_CONFIG_ASSIGN_LITERAL; }
    "FILE"                          { return FILE_CONFIG_STATUS_FILE_LITERAL; }
    "STATUS"                        { return FILE_CONFIG_STATUS_STATUS_LITERAL; }
    {VARNAME}                       { return VARNAME; }
}

<PROGRAMID> {
    "."                             { return DOT; }
    {VARNAME}                       { yybegin(YYINITIAL); return VARNAME; }
}

<MOVE> {
      {NUMBER}                        { return NUMBER; }
      {LINENUMBER}                    { return NUMBER; }
      {VARNAME}                       { return VARNAME; }
      "TO"                            { yybegin(YYINITIAL); return CobolTypes.TO; }
}

<ANY> {
    {WHITE_SPACE}                   { return TokenType.WHITE_SPACE; }
    {LINENUMBER}                    { yybegin(YYINITIAL); return TokenType.WHITE_SPACE; }
    [^]                             { return CobolTypes.ANY; }
}

<WORKINGSTORAGE> {
  "SECTION"                       { return SECTION; }
  "01"                            { return CobolTypes.RECORD; }
  "77"                            { yybegin(WORKINGSTORAGE_SA_NAME); return SA_LITERAL; }
  "PROCEDURE"                     { yybegin(YYINITIAL);return CobolTypes.PROCEDURE; }
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


    {LINENUMBER}                    { return TokenType.WHITE_SPACE; }
    {WHITE_SPACE}                   { return TokenType.WHITE_SPACE; }
    "/"                             { return TokenType.WHITE_SPACE; }
    {END_OF_LINE_COMMENT}           { return COMMENT; }
    "."                             { return DOT; }
    {STRING}                        { return STRING; }
    [^]                             { return TokenType.BAD_CHARACTER; }
