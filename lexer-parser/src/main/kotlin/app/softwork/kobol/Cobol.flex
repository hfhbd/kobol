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

WHITE_SPACE=\s+
END_OF_LINE_COMMENT=("*")[^\r\n]*
STRING=(\"([^\"])*\")
VARNAME=[a-zA-Z]([\w|-]+[\w|_])*

%state PROGRAMID
%state DISPLAY

%%

<YYINITIAL>
{
    {WHITE_SPACE}                   { return TokenType.WHITE_SPACE; }
    {END_OF_LINE_COMMENT}           { return CobolTypes.COMMENT; }
    "."                             { return CobolTypes.DOT; }
    "PROGRAM-ID"                    { yybegin(PROGRAMID); return CobolTypes.PROGRAM_ID; }
    "DISPLAY"                       { yybegin(DISPLAY); return CobolTypes.DISPLAY; }
    {VARNAME}                       { return CobolTypes.VARNAME; }
    {STRING}                        { return CobolTypes.STRING; }
    [^]                             { return TokenType.BAD_CHARACTER; }
}

<PROGRAMID> {
    "."                             { return CobolTypes.DOT; }
    {WHITE_SPACE}                   { return TokenType.WHITE_SPACE; }
    {VARNAME}                       { yybegin(YYINITIAL); return CobolTypes.VARNAME; }
    [^]                             { return TokenType.BAD_CHARACTER; }
}

<DISPLAY> {
    "."                             { return CobolTypes.DOT; }
    {STRING}                        { return CobolTypes.STRING; }
    {WHITE_SPACE}                   { return TokenType.WHITE_SPACE; }
    {VARNAME}                       { yybegin(YYINITIAL); return CobolTypes.VARNAME; }
    [^]                             { return TokenType.BAD_CHARACTER; }
}
