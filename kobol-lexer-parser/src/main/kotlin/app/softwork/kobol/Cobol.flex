// Copyright 2000-2022 JetBrains s.r.o. and other contributors. Use of this source code is governed by the Apache 2.0 license that can be found in the LICENSE file.
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
    "WORKING-STORAGE"               { yybeginn(WORKINGSTORAGE); return CobolTypes.WORKINGSTORAGE; }
    "SECTION"                       { return SECTION; }
    {VARNAME}                       { return VARNAME; }
}

<PROGRAMID> {
    "."                             { return DOT; }
    {WHITE_SPACE}                   { return TokenType.WHITE_SPACE; }
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

  "01" { return CobolTypes.RECORD; }

}

    {LINENUMBER}                    { return TokenType.WHITE_SPACE; }
    {WHITE_SPACE}                   { return TokenType.WHITE_SPACE; }
    "/"                             { return TokenType.WHITE_SPACE; }
    {END_OF_LINE_COMMENT}           { return COMMENT; }
    "."                             { return DOT; }
    {STRING}                        { return STRING; }
    [^]                             { return TokenType.BAD_CHARACTER; }
