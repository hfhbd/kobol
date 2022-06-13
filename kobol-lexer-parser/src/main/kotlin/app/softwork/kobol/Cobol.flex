// Copyright 2000-2022 JetBrains s.r.o. and other contributors. Use of this source code is governed by the Apache 2.0 license that can be found in the LICENSE file.
package app.softwork.kobol;

import com.intellij.lexer.FlexLexer;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.TokenType;

%%

%class CobolLexer
%implements FlexLexer
%unicode
%function advance
%type IElementType
%eof{  return;
%eof}

LINENUMBER=\d{6}(\*)?
WHITE_SPACE=\s+
END_OF_LINE_COMMENT=("*")[^\r\n]*
STRING=(\"([^\"])*\")
VARNAME=[a-zA-Z]([\w|-]+[\w|_])*
ASSIGN="="

%state PROGRAMID
%state ID
%state ID_DOT
%state DISPLAY

%%

<YYINITIAL>
{
    {WHITE_SPACE}                   { return TokenType.WHITE_SPACE; }
    {END_OF_LINE_COMMENT}           { return CobolTypes.COMMENT; }
    "."                             { return CobolTypes.DOT; }
    "PROGRAM-ID"                    { yybegin(PROGRAMID); return CobolTypes.PROGRAM_ID; }
    "DISPLAY"                       { yybegin(DISPLAY); return CobolTypes.DISPLAY; }
    "AUTHOR"                        { yybegin(ID); return CobolTypes.AUTHOR; }
    "INSTALLATION"                  { yybegin(ID); return CobolTypes.INSTALLATION; }
    "DATE-WRITTEN"                  { yybegin(ID); return CobolTypes.DATE; }
    {VARNAME}                       { return CobolTypes.VARNAME; }
    {STRING}                        { return CobolTypes.STRING; }
    {ASSIGN}                        { return CobolTypes.ASSIGN; }
    {LINENUMBER}                    { return TokenType.WHITE_SPACE; }
    [^]                             { return TokenType.BAD_CHARACTER; }
}

<PROGRAMID> {
    "."                             { return CobolTypes.DOT; }
    {WHITE_SPACE}                   { return TokenType.WHITE_SPACE; }
    {VARNAME}                       { yybegin(YYINITIAL); return CobolTypes.VARNAME; }
    {LINENUMBER}                    { return TokenType.WHITE_SPACE; }
    [^]                             { return TokenType.BAD_CHARACTER; }
}

<ID> {
    "."                             { yybegin(ID_DOT); return CobolTypes.DOT; }
    {WHITE_SPACE}                   { return TokenType.WHITE_SPACE; }
    {LINENUMBER}                    { return TokenType.WHITE_SPACE; }
    [^]                             { yybegin(ID_DOT); return CobolTypes.ANY; }
}

<ID_DOT> {
    "."                             { yybegin(YYINITIAL); return CobolTypes.DOT; }
    {WHITE_SPACE}                   { return TokenType.WHITE_SPACE; }
    {LINENUMBER}                    { return TokenType.WHITE_SPACE; }
    [^]                             { return CobolTypes.ANY; }
}

<DISPLAY> {
    "."                             { return CobolTypes.DOT; }
    {STRING}                        { return CobolTypes.STRING; }
    {WHITE_SPACE}                   { return TokenType.WHITE_SPACE; }
    {VARNAME}                       { yybegin(YYINITIAL); return CobolTypes.VARNAME; }
    {LINENUMBER}                    { return TokenType.WHITE_SPACE; }
    [^]                             { return TokenType.BAD_CHARACTER; }
}
