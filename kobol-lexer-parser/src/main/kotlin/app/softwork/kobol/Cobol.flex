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
%eof}

%init{
    yycolumn = 0;
%init}

NUMBER=([+\-])?(\d+(\.\d+)?)|(\.\d+)
WHITE_SPACE=\s+
END_OF_LINE_COMMENT=\*.*
STRING=X?('([^'\\]|\\.)*'|\"([^\"\\]|\\.)*\")
VARNAME=[a-zA-Z]([\w\-_])*

%state IDENTIFICATION
%state ENVIRONMENT
%state DATA
%state PROCEDURE

// ID
%state PROGRAMID
%state ANY

// ENV
%state WORKINGSTORAGE
%state WORKINGSTORAGE_SA_NUMBER
%state WORKINGSTORAGE_SA_NUMBER_LINE
%state WORKINGSTORAGE_SA_PIC
%state WORKINGSTORAGE_SA_PIC_LENGTH
%state WORKINGSTORAGE_SA_OCCURS
%state WORKINGSTORAGE_SA_TO
%state WORKINGSTORAGE_SQL

%state CONFIGURATION
%state SPECIAL_NAMES
%state SPECIAL_NAMES_START
%state FILE_CONTROL
%state FILE_CONTROL_START

// DATA
%state FILE
%state FD
%state FD_SA_NAME
%state FD_SA
%state FD_SA_PIC
%state FD_SA_PIC_LENGTH
%state FD_SA_TO
%state FD_SA_NUMBER
%state FD_SA_NUMBER_LINE
%state FD_SA_OCCURS

// PROCEDURE
%state PROCEDURE_SQL

%{
  public int yycolumn;

  private IElementType number() {
      if (yycolumn > 5) {
        return NUMBER;
      }
      return TokenType.WHITE_SPACE;
  }
%}

%%

"DIVISION"                         { return DIVISION; }
"SECTION"                          { return SECTION; }

<YYINITIAL>
{
    "IDENTIFICATION"                { yybegin(IDENTIFICATION); return CobolTypes.IDENTIFICATION; }
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
              yybegin(FD_SA_PIC);
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
   "DATA"                          { return CobolTypes.DATA; }
   "BLOCK"                         { return CobolTypes.BLOCK; }
   "RECORDS"                       { return RECORDS; }
   "TO"                            { return TO; }
   "RECORD"                        { return CobolTypes.RECORD; }
   "STANDARD"                      { return STANDARD; }
   "."                             { yybegin(FILE); return DOT; }
   {VARNAME}                       { return VARNAME; }
}

<WORKINGSTORAGE> {
   {NUMBER}                        {
          IElementType number = number();
          if (number == NUMBER) {
              yybegin(WORKINGSTORAGE_SA_PIC);
          }
          return number;
      }
   "EXEC"                          { return EXEC; }
   "SQL"                           { yybegin(WORKINGSTORAGE_SQL); return SQL; }
   "LINKAGE"                       { return LINKAGE; }
   "PROCEDURE"                     { yybegin(PROCEDURE); return CobolTypes.PROCEDURE; }
}

<WORKINGSTORAGE_SQL, PROCEDURE_SQL> {
    "END-EXEC"                      {
          if(yystate() == WORKINGSTORAGE_SQL) {
            yybegin(WORKINGSTORAGE);
          } else if(yystate() == PROCEDURE_SQL) {
            yybegin(PROCEDURE);
          }
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

<WORKINGSTORAGE_SA_PIC, FD_SA_PIC> {
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
      "POINTER" { return POINTER; }
      "COMP" { return COMP; }
      "COMP-3" { return COMP_3; }
      "COMP-5" { return COMP_5; }
      "TIMES" { return TIMES; }
      "DEPENDING" { return DEPENDING; }
      "SPACE" { return SPACE; }
      "NULL" { return NULL; }
      "LOW-VALUE" { return LOW_VALUE; }
      "HIGH-VALUE" { return HIGH_VALUE; }
      "ZEROES" { return ZERO; }
      "ZERO" { return ZERO; }
      "." {
          if (yystate() == FD_SA_PIC) {
              yybegin(FILE);
          } else if(yystate() == WORKINGSTORAGE_SA_PIC) {
              yybegin(WORKINGSTORAGE);
          }
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
    "UNTIL"                         { return UNTIL; }
    "IF"                            { return IF; }
    "THEN"                          { return THEN; }
    "ELSE"                          { return ELSE; }
    "END-IF"                        { return END_IF; }
    "="                             { return EQUAL; }
    ">"                             { return BIGGER; }
    "<"                             { return SMALLER; }
    "+"                             { return PLUS; }
    "-"                             { return MINUS; }
    "EQUAL"                         { return EQUAL; }
    "NOT"                           { return NOT; }
    "GOBACK"                        { return GOBACK; }
    "INITIALIZE"                    { return INITIALIZE; }
    "EXEC"                          { return EXEC; }
    "SQL"                           { yybegin(PROCEDURE_SQL); return SQL; }
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
    "ZERO" { return ZERO; }
    {VARNAME}                       { return VARNAME; }
}

<IDENTIFICATION> {
    "AUTHOR"                        { yybegin(ANY); return AUTHOR; }
    "INSTALLATION"                  { yybegin(ANY); return INSTALLATION; }
    "DATE-WRITTEN"                  { yybegin(ANY); return DATE_WRITTEN; }
    "PROGRAM-ID"                    { return PROGRAM_ID; }

    "ENVIRONMENT"                   { yybegin(ENVIRONMENT); return CobolTypes.ENVIRONMENT; }
    "DATA"                          { yybegin(DATA); return CobolTypes.DATA; }
    "PROCEDURE"                     { yybegin(PROCEDURE); return CobolTypes.PROCEDURE; }

    {VARNAME}                       { return VARNAME; }
}

<ANY> {
    {WHITE_SPACE}                   { return TokenType.WHITE_SPACE; }
    {NUMBER}                        {
          if (yycolumn > 5) {
              return CobolTypes.ANY;
          }
          yybegin(IDENTIFICATION);
          return TokenType.WHITE_SPACE;
    }
    [^]                             {
          if (yycolumn == 0) {
              yybegin(IDENTIFICATION);
              return TokenType.WHITE_SPACE;
          }
          return CobolTypes.ANY; }
}

    {NUMBER}                        { return number(); }
    {WHITE_SPACE}                   { return TokenType.WHITE_SPACE; }
    "/"                             { return TokenType.WHITE_SPACE; }
    {END_OF_LINE_COMMENT}           { return COMMENT; }
    "."                             { return DOT; }
    {STRING}                        { return STRING; }
    [^]                             { return TokenType.BAD_CHARACTER; }
