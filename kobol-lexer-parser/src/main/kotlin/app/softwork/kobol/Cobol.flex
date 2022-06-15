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

LINENUMBER=\d{6}
WHITE_SPACE=\s+
END_OF_LINE_COMMENT=\*.*
STRING=('([^'\\]|\\.)*'|\"([^\"\\]|\\.)*\")
VARNAME=[a-zA-Z]([\w|-]+[\w|_])*
ASSIGN="="

%state PROGRAMID
%state ANY
%state DISPLAY

%%

<YYINITIAL>
{
    {WHITE_SPACE}                   { return TokenType.WHITE_SPACE; }
    {END_OF_LINE_COMMENT}           { return COMMENT; }
    "."                             { return DOT; }
    "PROGRAM-ID"                    { yybegin(PROGRAMID); return PROGRAM_ID; }
    "DISPLAY"                       { yybegin(DISPLAY); return CobolTypes.DISPLAY; }
    "AUTHOR"                        { yybegin(ANY); return AUTHOR; }
    "INSTALLATION"                  { yybegin(ANY); return INSTALLATION; }
    "DATE-WRITTEN"                  { yybegin(ANY); return DATE; }
    "IDENTIFICATION"                { return IDENTIFICATION; }
    "DIVISION"                      { return DIVISION; }
    {VARNAME}                       { return VARNAME; }
    {STRING}                        { return STRING; }
    {ASSIGN}                        { return ASSIGN; }
    {LINENUMBER}                    { return TokenType.WHITE_SPACE; }
    "/"                             { return TokenType.WHITE_SPACE; }
    [^]                             { return TokenType.BAD_CHARACTER; }
}

<PROGRAMID> {
    "."                             { return DOT; }
    {WHITE_SPACE}                   { return TokenType.WHITE_SPACE; }
    {VARNAME}                       { yybegin(YYINITIAL); return VARNAME; }
    {LINENUMBER}                    { return TokenType.WHITE_SPACE; }
    [^]                             { return TokenType.BAD_CHARACTER; }
}

<ANY> {
    {WHITE_SPACE}                   { return TokenType.WHITE_SPACE; }
    {LINENUMBER}                    { yybegin(YYINITIAL); return TokenType.WHITE_SPACE; }
    [^]                             { return CobolTypes.ANY; }
}

<DISPLAY> {
    {END_OF_LINE_COMMENT}           { return COMMENT; }
    "."                             { yybegin(YYINITIAL); return DOT; }
    {STRING}                        { return STRING; }
    {WHITE_SPACE}                   { return TokenType.WHITE_SPACE; }
    {VARNAME}                       { yybegin(YYINITIAL); return VARNAME; }
    {LINENUMBER}                    { yybegin(YYINITIAL); return TokenType.WHITE_SPACE; }
    [^]                             { return TokenType.BAD_CHARACTER; }
}
