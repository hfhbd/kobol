package app.softwork.kobol;

import com.intellij.lexer.FlexLexer;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.TokenType;
import static app.softwork.kobol.CobolTypes.*;

%%
%class CobolLexer
%implements FlexLexer
%unicode
%ignorecase
%column
%function advance
%type IElementType
%eof{
    yycolumn = 0;
    returnState = -1;
%eof}

%init{
    yycolumn = 0;
%init}

NUMBER=([+\-])?(\d+(\.\d+)?)|(\.\d+)
WHITE_SPACE=\s+
END_OF_LINE_COMMENT=.*
STRING=X?('([^'\\]|\\.)*'|\"([^\"\\]|\\.)*\")
VARNAME=[a-zA-Z]([\w\-_])*

%state IDENTIFICATION
%state ENVIRONMENT
%state DATA
%state PROCEDURE

%state COMMENT

// ID
%state ANY

// ENV
%state WORKINGSTORAGE
%state SQL_STATE
%state PIC_STATE

%state FILE_CONTROL
%state FILE_CONTROL_START

// DATA
%state FILE
%state FD

%{
  private boolean isID = false;

  private IElementType number() {
      if (yycolumn >= 7) {
        return NUMBER;
      }
      return TokenType.WHITE_SPACE;
  }

  public int returnState = -42;
%}

%%

"DIVISION"                         { return DIVISION; }
"SECTION"                          { return SECTION; }

<YYINITIAL>
{
    "IDENTIFICATION"                { yybegin(IDENTIFICATION); return CobolTypes.IDENTIFICATION; }
    "ID"                            { yybegin(IDENTIFICATION); return CobolTypes.IDENTIFICATION; }
    "ENVIRONMENT"                   { yybegin(ENVIRONMENT); return CobolTypes.ENVIRONMENT; }
    "DATA"                          { yybegin(DATA); return CobolTypes.DATA; }
    "PROCEDURE"                     { yybegin(PROCEDURE); return CobolTypes.PROCEDURE; }
}

<ENVIRONMENT> {
    "CONFIGURATION"                 { return CobolTypes.CONFIGURATION; }
    "SPECIAL-NAMES"                 { return CobolTypes.SPECIAL_NAMES; }
    "IS"                            { return IS; }
    "INPUT-OUTPUT"                  { return INPUT_OUTPUT; }
    "SELECT"                        { return SELECT; }
    "ASSIGN"                        { return ASSIGN; }
    "FILE"                          { return CobolTypes.FILE; }
    "STATUS"                        { return STATUS; }
    "FILE-CONTROL"                  { yybegin(FILE_CONTROL_START); return CobolTypes.FILE_CONTROL; }


    "DATA"                          { yybegin(DATA); return CobolTypes.DATA; }
    "PROCEDURE"                     { yybegin(PROCEDURE); return CobolTypes.PROCEDURE; }
    {VARNAME}                       { return VARNAME; }
}

<FILE_CONTROL_START> {
    "SELECT"                        { yybegin(FILE_CONTROL); return SELECT; }

    "DATA"                          { yybegin(DATA); return CobolTypes.DATA; }
    "PROCEDURE"                     { yybegin(PROCEDURE); return CobolTypes.PROCEDURE; }
}

<FILE_CONTROL> {
    "ASSIGN"                        { return ASSIGN; }
    "FILE"                          { return CobolTypes.FILE; }
    "STATUS"                        { return STATUS; }
    "TO"                            { return TO; }
    "IS"                            { return IS; }
    "ORGANIZATION"                  { return CobolTypes.ORGANIZATION; }
    "LINE"                          { return CobolTypes.LINE; }
    "SEQUENTIAL"                    { return CobolTypes.SEQUENTIAL; }
    "."                             { yybegin(FILE_CONTROL_START); return DOT; }
    {VARNAME}                       { return VARNAME; }
}

<DATA> {
   "PROCEDURE"                     { yybegin(PROCEDURE); return CobolTypes.PROCEDURE; }
   "FILE"                          { yybegin(FILE); return CobolTypes.FILE; }
   "WORKING-STORAGE"               { yybegin(WORKINGSTORAGE); return WORKING_STORAGE; }
   "LINKAGE"                       { yybegin(WORKINGSTORAGE); return LINKAGE; }
}

<FILE> {
   "FD"                            { yybegin(FD); return CobolTypes.FD; }
   {NUMBER}                        {
          IElementType number = number();
          if (number == NUMBER) {
              returnState = FILE;
              yybegin(PIC_STATE);
          }
          return number();
      }
   "WORKING-STORAGE"               { yybegin(WORKINGSTORAGE); return WORKING_STORAGE; }
   "PROCEDURE"                     { yybegin(PROCEDURE); return CobolTypes.PROCEDURE; }
}

<FD> {
   "RECORDING"                     { return RECORDING; }
   "LABEL"                         { return CobolTypes.LABEL; }
   "RECORD"                        { return CobolTypes.RECORD; }
   "RECORDS"                       { return CobolTypes.RECORDS; }
   "DATA"                          { return CobolTypes.DATA; }
   "BLOCK"                         { return CobolTypes.BLOCK; }
   "TO"                            { return TO; }
   "IS"                            { return IS; }
   "ARE"                           { return ARE; }
   "CONTAINS"                      { return CONTAINS; }
   "MODE"                          { return MODE; }
   "VARYING"                       { return VARYING; }
   "DEPENDING"                     { return DEPENDING; }
   "ON"                            { return ON; }
   "RECORD"                        { return CobolTypes.RECORD; }
   "STANDARD"                      { return STANDARD; }
   "."                             { yybegin(FILE); return DOT; }
   {VARNAME}                       { return VARNAME; }
}

<WORKINGSTORAGE> {
   {NUMBER}                        {
          IElementType number = number();
          if (number == NUMBER) {
              returnState = WORKINGSTORAGE;
              yybegin(PIC_STATE);
          }
          return number;
      }
   "EXEC"                          { return EXEC; }
   "SQL"                           { returnState = WORKINGSTORAGE; yybegin(SQL_STATE); return SQL; }
   "LINKAGE"                       { return LINKAGE; }
   "PROCEDURE"                     { yybegin(PROCEDURE); return CobolTypes.PROCEDURE; }
}

<SQL_STATE> {
    "END-EXEC"                      {
          yybegin(returnState);
          return END_EXEC;
      }
    {NUMBER}                        {
          if (yycolumn > 5) {
              return CobolTypes.ANY;
          }
          return TokenType.WHITE_SPACE; }
    {WHITE_SPACE}                   { return CobolTypes.SP; }
    [^]                             { return CobolTypes.ANY; }
}

<PIC_STATE> {
      "PIC" { return PIC; }
      "X" { return X; }
      "A" { return A; }
      "S9" { return S9; }
      "V9" { return V9; }
      "+" { return PLUS; }
      "Z" { return Z; }
      "(" { return LP; }
      ")" { return RP; }
      "VALUE" {return VALUE; }
      "OCCURS" {return OCCURS; }
      "DEPENDING" { return DEPENDING; }
      "REDEFINES" { return REDEFINES; }
      "ON" { return ON; }
      "TO" { return TO; }
      "BINARY" { return BINARY; }
      "USAGE" { return USAGE; }
      "IS" { return IS; }
      "POINTER" { return POINTER; }
      "COMP" { return COMP; }
      "COMP-3" { return COMP_3; }
      "COMP-5" { return COMP_5; }
      "TIMES" { return TIMES; }
      "DEPENDING" { return DEPENDING; }
      "SPACE" { return SPACE; }
      "SPACES" { return SPACE; }
      "NULL" { return NULL; }
      "LOW-VALUE" { return LOW_VALUE; }
      "HIGH-VALUE" { return HIGH_VALUE; }
      "SIGN" { return SIGN; }
      "LEADING" { return LEADING; }
      "SEPARATE" { return SEPARATE; }
      "ZEROES" { return ZERO; }
      "ZEROS" { return ZERO; }
      "ZERO" { return ZERO; }
      "." {
          yybegin(returnState);
          return DOT;
      }
      "SYNC" { return SYNC; }
          {VARNAME}                       { return VARNAME; }
}

<PROCEDURE> {
    "ACCEPT"                        { return ACCEPT; }
    "USING"                         { return USING; }
    "DISPLAY"                       { return DISPLAY; }
    "MOVE"                          { return MOVE; }
    "PERFORM"                       { return PERFORM; }
    "END-PERFORM"                   { return END_PERFORM; }
    "WITH"                          { return WITH; }
    "TEST"                          { return TEST; }
    "AFTER"                         { return AFTER; }
    "BEFORE"                        { return BEFORE; }
    "UNTIL"                         { return UNTIL; }
    "IF"                            { return IF; }
    "THEN"                          { return THEN; }
    "ELSE"                          { return ELSE; }
    "END-IF"                        { return END_IF; }
    "="                             { return EQUAL; }
    ">"                             { return BIGGER; }
    "GREATER"                       { return BIGGER; }
    "<"                             { return SMALLER; }
    "LESS"                          { return SMALLER; }
    "THAN"                          { return THAN; }
    "+"                             { return PLUS; }
    "-"                             { return MINUS; }
    "EQUAL"                         { return EQUAL; }
    "NOT"                           { return NOT; }
    "GOBACK"                        { return GOBACK; }
    "STOP"                          { return STOP; }
    "RUN"                           { return RUN; }
    "INITIALIZE"                    { return INITIALIZE; }
    "EXEC"                          { return EXEC; }
    "SQL"                           { returnState = PROCEDURE; yybegin(SQL_STATE); return SQL; }
    "OPEN"                          { return OPEN; }
    "CLOSE"                         { return CLOSE; }
    "INPUT"                         { return INPUT; }
    "OUTPUT"                        { return OUTPUT; }
    "CALL"                          { return CALL; }
    "CONTINUE"                      { return CONTINUE; }
    "READ"                          { return READ; }
    "END-READ"                      { return END_READ; }
    "WRITE"                         { return WRITE; }
    "END-WRITE"                     { return END_WRITE; }
    "AT"                            { return AT; }
    "END"                           { return END; }
    "ADD"                           { return ADD; }
    "WHEN"                          { return WHEN; }
    "ALSO"                          { return ALSO; }
    "EVALUATE"                      { return EVALUATE; }
    "END-EVALUATE"                  { return END_EVALUATE; }
    "("                             { return LP; }
    ")"                             { return RP; }
    ":"                             { return COLON; }
    "OF"                            { return OF; }
    "IN"                            { return IN; }
    "OTHER"                         { return OTHER; }
    "OR"                            { return OR; }
    "AND"                           { return AND; }
    "NEXT"                          { return NEXT; }
    "SENTENCE"                      { return SENTENCE; }
    "VARYING"                       { return CobolTypes.VARYING; }
    "FROM"                          { return CobolTypes.FROM; }
    "UNSTRING"                      { return UNSTRING; }
    "DELIMITED"                     { return DELIMITED; }
    "BY"                            { return BY; }
    "TO"                            { return TO; }
    "UNTIL"                         { return UNTIL; }
    "INTO"                          { return INTO; }
    "COMPUTE"                       { return COMPUTE; }
    "SUBTRACT"                      { return SUBTRACT; }
    "ALL" { return ALL; }
    "SPACE" { return SPACE; }
    "NULL" { return NULL; }
    "LOW-VALUE" { return LOW_VALUE; }
    "HIGH-VALUE" { return HIGH_VALUE; }
    "ZEROES" { return ZERO; }
    "ZEROS" { return ZERO; }
    "ZERO" { return ZERO; }
    {VARNAME}                       { return VARNAME; }
}

<IDENTIFICATION> {
    "AUTHOR"                        { yybegin(ANY); isID = true; return AUTHOR; }
    "INSTALLATION"                  { yybegin(ANY); isID = true; return INSTALLATION; }
    "DATE-WRITTEN"                  { yybegin(ANY); isID = true; return DATE_WRITTEN; }
    "PROGRAM-ID"                    { return PROGRAM_ID; }

    "ENVIRONMENT"                   { yybegin(ENVIRONMENT); return CobolTypes.ENVIRONMENT; }
    "DATA"                          { yybegin(DATA); return CobolTypes.DATA; }
    "PROCEDURE"                     { yybegin(PROCEDURE); return CobolTypes.PROCEDURE; }

    {VARNAME}                       { return VARNAME; }
}

<ANY> {
    {NUMBER}                        {
          if (yycolumn >= 7) {
              return CobolTypes.ANY;
          }
          yybegin(IDENTIFICATION);
          return TokenType.WHITE_SPACE;
    }
    "\n" {
          if (isID) {
              yybegin(IDENTIFICATION);
              isID = false;
          }
          return TokenType.WHITE_SPACE;
      }
    {WHITE_SPACE}                   { return TokenType.WHITE_SPACE; }  
    [^]                             {
          if (yycolumn == 0) {
              yybegin(IDENTIFICATION);
              return TokenType.WHITE_SPACE;
          }
          return CobolTypes.ANY; }
}

<COMMENT> {
{END_OF_LINE_COMMENT} {
          yybegin(returnState);
          return CobolTypes.COMMENT;
      }
}

    {NUMBER}                        { return number(); }
    {WHITE_SPACE}                   { return TokenType.WHITE_SPACE; }
    "*"           {
          if (yycolumn == 0 || yycolumn == 6) {
              returnState = yystate();
              yybegin(COMMENT);
          } else {
          return CobolTypes.STAR;
          }
      }
      "/"           {
          if (yycolumn == 0 || yycolumn == 6) {
              returnState = yystate();
              yybegin(COMMENT);
          } else {
          return CobolTypes.SLASH;
          }
      }
    "."                             { return DOT; }
    {STRING}                        { return STRING; }
    [^]                             { return TokenType.BAD_CHARACTER; }
