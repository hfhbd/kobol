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

NUMBER=\+?(\d+(\.\d+)?)|(\.\d+)
LINENUMBER=\d{6}
WHITE_SPACE=\s+
END_OF_LINE_COMMENT=\*.*
STRING=('([^'\\]|\\.)*'|\"([^\"\\]|\\.)*\")
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
%state WORKINGSTORAGE_SA
%state WORKINGSTORAGE_SA_NUMBER
%state WORKINGSTORAGE_SA_NUMBER_LINE
%state WORKINGSTORAGE_SA_NAME
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
%state FILE_CONTROL_NEXTNUMBER
%state FILE_CONTROL_LABEL

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
%state MOVE
%state ADD
%state PROCEDURE_SQL
%state WHEN
%state LP
%state FROM

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
    "CONFIGURATION"                 { yybegin(CONFIGURATION); return CobolTypes.CONFIGURATION; }
    "INPUT-OUTPUT"                  { return INPUT_OUTPUT; }
    "SELECT"                        { return SELECT; }
    "ASSIGN"                        { return ASSIGN; }
    "FILE"                          { return CobolTypes.FILE; }
    "STATUS"                        { return STATUS; }
    "FILE-CONTROL"                  { yybegin(FILE_CONTROL_START); return CobolTypes.FILE_CONTROL; }


    "DATA"                          { yybegin(DATA); return CobolTypes.DATA; }
    "PROCEDURE"                     { yybegin(PROCEDURE); return CobolTypes.PROCEDURE; }
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

    {VARNAME}                       { return VARNAME; }
    "."                             { yybegin(FILE_CONTROL_START); return DOT; }
}

<CONFIGURATION> {
    "SPECIAL-NAMES"                 { yybegin(SPECIAL_NAMES_START); return CobolTypes.SPECIAL_NAMES; }
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
   "FILE"                          { yybegin(FILE); return CobolTypes.FILE; }
   "WORKING-STORAGE"               { yybegin(WORKINGSTORAGE); return WORKING_STORAGE; }
}

<FILE> {
   "FD"                            { yybegin(FD); return CobolTypes.FD; }
   {LINENUMBER}                    { return TokenType.WHITE_SPACE; }
   {NUMBER}                        { yybegin(FD_SA_PIC); return NUMBER; }
   "WORKING-STORAGE"               { yybegin(WORKINGSTORAGE); return WORKING_STORAGE; }
}

<FD> {
   "RECORDING"                     { return RECORDING; }
   "LABEL"                         { yybegin(FILE_CONTROL_LABEL); return CobolTypes.LABEL; }
   "RECORD"                        { yybegin(FILE_CONTROL_NEXTNUMBER); return CobolTypes.RECORD; }
   "DATA"                          { yybegin(FILE_CONTROL_LABEL); return CobolTypes.DATA; }
   "BLOCK"                         { yybegin(FILE_CONTROL_NEXTNUMBER); return CobolTypes.BLOCK; }
   "."                             { yybegin(FILE); return DOT; }
   {VARNAME}                       { return VARNAME; }
}

<FILE_CONTROL_NEXTNUMBER> {
   {NUMBER}                        { yybegin(FD);return NUMBER; }
}
<FILE_CONTROL_LABEL> {
    "RECORD"                       { return CobolTypes.RECORD; }
    "STANDARD"                     { yybegin(FD); return STANDARD; }
    {VARNAME}                      { yybegin(FD); return VARNAME; }
}

<WORKINGSTORAGE> {
   {LINENUMBER}                    { return TokenType.WHITE_SPACE; }
   {NUMBER}                        { yybegin(WORKINGSTORAGE_SA_PIC); return NUMBER; }
   "EXEC"                          { return EXEC; }
   "SQL"                           { yybegin(WORKINGSTORAGE_SQL); return SQL; }
   "LINKAGE"                       { return LINKAGE; }
   "PROCEDURE"                     { yybegin(PROCEDURE); return CobolTypes.PROCEDURE; }
}

<WORKINGSTORAGE_SQL, PROCEDURE_SQL> {
    {LINENUMBER}                    { return TokenType.WHITE_SPACE; }
    "END-EXEC"                      {
          if(yystate() == WORKINGSTORAGE_SQL) {
            yybegin(WORKINGSTORAGE);
          } else if(yystate() == PROCEDURE_SQL) {
            yybegin(PROCEDURE);
          }
          return END_EXEC;
      }
    {WHITE_SPACE}                   { return TokenType.WHITE_SPACE; }
    [^]                             { return CobolTypes.ANY; }
}

<WORKINGSTORAGE_SA_PIC, FD_SA_PIC> {
      "PIC" { return PIC; }
      "pic" { return PIC; }
      "X" { return X; }
      "x" { return X; }
      "A" { return A;}
      "a" { return A;}
      "9" { return PIC_9;}
      "S9" { return S9;}
      "s9" { return S9;}
      "V9" { return V9; }
      "v9" { return V9; }
      "+" { return PLUS; }
      "Z" { return Z; }
      "z" { return Z; }
      "(" {
                    if (yystate() == FD_SA_PIC) {
                                  yybegin(FD_SA_PIC_LENGTH);
                              } else if(yystate() == WORKINGSTORAGE_SA_PIC) {
                                  yybegin(WORKINGSTORAGE_SA_PIC_LENGTH);
                              }
          return CobolTypes.LP; }
      {NUMBER} { return NUMBER;}
      "VALUE" {
          if (yystate() == FD_SA_PIC) {
              yybegin(FD_SA_NUMBER);
          } else if(yystate() == WORKINGSTORAGE_SA_PIC) {
              yybegin(WORKINGSTORAGE_SA_NUMBER);
          }
          return VALUE;
      }
      "OCCURS" {
          if (yystate() == FD_SA_PIC) {
                        yybegin(FD_SA_OCCURS);
                    } else if(yystate() == WORKINGSTORAGE_SA_PIC) {
                        yybegin(WORKINGSTORAGE_SA_OCCURS);
                    }
          return OCCURS; }
      "DEPENDING" { return DEPENDING; }
      "REDEFINES" { return REDEFINES; }
      "ON" { return ON; }
      "BINARY" { return BINARY; }
      "USAGE" { return USAGE; }
      "POINTER" { return POINTER; }
      "COMP" { return COMP; }
      "COMP-3" { return COMP_3; }
      "." {
          if (yystate() == FD_SA_PIC) {
              yybegin(FILE);
          } else if(yystate() == WORKINGSTORAGE_SA_PIC) {
              yybegin(WORKINGSTORAGE);
          }
          return DOT;
      }
          {VARNAME}                       { return VARNAME; }
}

<WORKINGSTORAGE_SA_TO, FD_SA_TO> {
{NUMBER} { return NUMBER;}
      "." {
          if (yystate() == FD_SA_TO) {
                        yybegin(FILE);
                    } else if(yystate() == WORKINGSTORAGE_SA_TO) {
                        yybegin(WORKINGSTORAGE);
                    }
          return DOT; }
                "DEPENDING" {
                    if (yystate() == FD_SA_TO) {
                                  yybegin(FD_SA_PIC);
                              } else if(yystate() == WORKINGSTORAGE_SA_TO) {
                                  yybegin(WORKINGSTORAGE_SA_PIC);
                              }
                    return DEPENDING; }
}

<WORKINGSTORAGE_SA_OCCURS, FD_SA_OCCURS> {
{NUMBER} { return NUMBER;}
      "." {
          if (yystate() == FD_SA_OCCURS) {
                        yybegin(FILE);
                    } else if(yystate() == WORKINGSTORAGE_SA_OCCURS) {
                        yybegin(WORKINGSTORAGE);
                    }
          return DOT; }
                "TO" {
                    if (yystate() == FD_SA_OCCURS) {
                                  yybegin(FD_SA_TO);
                              } else if(yystate() == WORKINGSTORAGE_SA_OCCURS) {
                                  yybegin(WORKINGSTORAGE_SA_TO);
                              }
                    return TO; }
}

<WORKINGSTORAGE_SA_PIC_LENGTH, FD_SA_PIC_LENGTH> {
{NUMBER} { return NUMBER;}
      ")" {
          if (yystate() == FD_SA_PIC_LENGTH) {
                        yybegin(FD_SA_PIC);
                    } else if(yystate() == WORKINGSTORAGE_SA_PIC_LENGTH) {
                        yybegin(WORKINGSTORAGE_SA_PIC);
                    }
          return RP; }
}

<WORKINGSTORAGE_SA_NUMBER, FD_SA_NUMBER> {
      {NUMBER} {
          if (yystate() == FD_SA_NUMBER) {
              yybegin(FD_SA_NUMBER_LINE);
          } else if(yystate() == WORKINGSTORAGE_SA_NUMBER) {
              yybegin(WORKINGSTORAGE_SA_NUMBER_LINE);
          }
          return NUMBER;
      }
      "SPACE" {
                        if (yystate() == FD_SA_NUMBER) {
                            yybegin(FD_SA_NUMBER_LINE);
                        } else if(yystate() == WORKINGSTORAGE_SA_NUMBER) {
                            yybegin(WORKINGSTORAGE_SA_NUMBER_LINE);
                        }
                        return SPACE;
                    }
 "NULL" {
                        if (yystate() == FD_SA_NUMBER) {
                            yybegin(FD_SA_NUMBER_LINE);
                        } else if(yystate() == WORKINGSTORAGE_SA_NUMBER) {
                            yybegin(WORKINGSTORAGE_SA_NUMBER_LINE);
                        }
                        return NULL;
                    }
                    "LOW-VALUE" {
                                            if (yystate() == FD_SA_NUMBER) {
                                                yybegin(FD_SA_NUMBER_LINE);
                                            } else if(yystate() == WORKINGSTORAGE_SA_NUMBER) {
                                                yybegin(WORKINGSTORAGE_SA_NUMBER_LINE);
                                            }
                                            return LOW_VALUE;
                                        }
                    "HIGH-VALUE" {
                                            if (yystate() == FD_SA_NUMBER) {
                                                yybegin(FD_SA_NUMBER_LINE);
                                            } else if(yystate() == WORKINGSTORAGE_SA_NUMBER) {
                                                yybegin(WORKINGSTORAGE_SA_NUMBER_LINE);
                                            }
                                            return HIGH_VALUE;
                                        }
                    "ZEROES" {
                                            if (yystate() == FD_SA_NUMBER) {
                                                yybegin(FD_SA_NUMBER_LINE);
                                            } else if(yystate() == WORKINGSTORAGE_SA_NUMBER) {
                                                yybegin(WORKINGSTORAGE_SA_NUMBER_LINE);
                                            }
                                            return ZERO;
                                        }
                    "ZERO" {
                                            if (yystate() == FD_SA_NUMBER) {
                                                yybegin(FD_SA_NUMBER_LINE);
                                            } else if(yystate() == WORKINGSTORAGE_SA_NUMBER) {
                                                yybegin(WORKINGSTORAGE_SA_NUMBER_LINE);
                                            }
                                            return ZERO;
                                        }
      {STRING} {
          if (yystate() == FD_SA_NUMBER) {
              yybegin(FD_SA_NUMBER_LINE);
          } else if(yystate() == WORKINGSTORAGE_SA_NUMBER) {
              yybegin(WORKINGSTORAGE_SA_NUMBER_LINE);
          }
          return STRING;
      }
}

<WORKINGSTORAGE_SA_NUMBER_LINE, FD_SA_NUMBER_LINE> {
      {LINENUMBER} {
          if (yystate() == FD_SA_NUMBER_LINE) {
              yybegin(FD);
          } else if(yystate() == WORKINGSTORAGE_SA_NUMBER_LINE) {
              yybegin(WORKINGSTORAGE);
          }
          return TokenType.WHITE_SPACE;
      }
      "." {
          if (yystate() == FD_SA_NUMBER_LINE) {
              yybegin(FD);
          } else if(yystate() == WORKINGSTORAGE_SA_NUMBER_LINE) {
              yybegin(WORKINGSTORAGE);
          }
          return DOT;
      }
      "OCCURS" {
                         if (yystate() == FD_SA_NUMBER_LINE) {
                             yybegin(FD_SA_OCCURS);
                         } else if(yystate() == WORKINGSTORAGE_SA_NUMBER_LINE) {
                             yybegin(WORKINGSTORAGE_SA_OCCURS);
                         }
                         return OCCURS;
                     }
}

<PROCEDURE> {
    "USING"                         { return USING; }
    "DISPLAY"                       { return CobolTypes.DISPLAY; }
    "MOVE"                          { yybegin(MOVE); return CobolTypes.MOVE; }
    "PERFORM"                       { return PERFORM; }
    "END-PERFORM"                   { return END_PERFORM; }
    "UNTIL"                         { return UNTIL; }
    "IF"                            { return IF; }
    "ELSE"                          { return ELSE; }
    "END-IF"                        { return END_IF; }
    "="                             { return EQUAL; }
    ">"                             { yybegin(WHEN); return BIGGER; }
    "<"                             { yybegin(WHEN); return SMALLER; }
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
    "READ"                          { return CobolTypes.READ; }
    "END-READ"                      { return END_READ; }
    "AT"                            { return AT; }
    "END"                           { return END; }
    "ADD"                           { yybegin(ADD); return CobolTypes.ADD; }
    "WHEN"                          { yybegin(WHEN); return CobolTypes.WHEN; }
    "EVALUATE"                      { return EVALUATE; }
    "END-EVALUATE"                  { return END_EVALUATE; }
    "("                             { yybegin(LP); return CobolTypes.LP; }
    "OF"                            { return OF; }
    "OTHER"                         { return OTHER; }
    "OR"                            { return OR; }
    "AND"                           { return AND; }
    "NEXT"                          { return NEXT; }
    "SENTENCE"                      { return SENTENCE; }
    "VARYING"                       { return VARYING; }
    "FROM"                          { yybegin(FROM); return CobolTypes.FROM; }
    {VARNAME}                       { return VARNAME; }
}

<FROM> {
    {NUMBER} { return NUMBER; }
    "TO" { return TO; }
    "BY" { return BY; }
    "UNTIL" { yybegin(PROCEDURE); return UNTIL; }
}

<LP> {
    ")" { yybegin(PROCEDURE); return RP; }
    {NUMBER} { return NUMBER; }
    ":" { return COLON; }
    "OF"                            { return OF; }
    {VARNAME}                       { return VARNAME; }
}

<IDENTIFICATION> {
    "AUTHOR"                        { yybegin(ANY); return AUTHOR; }
    "INSTALLATION"                  { yybegin(ANY); return INSTALLATION; }
    "DATE-WRITTEN"                  { yybegin(ANY); return DATE_WRITTEN; }
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

<MOVE, ADD> {
      {NUMBER}                        { return NUMBER; }
      "OF"                            { return OF; }
      "TO"                            { yybegin(PROCEDURE); return CobolTypes.TO; }
      {VARNAME}                       { return VARNAME; }
}

<WHEN> {
      {NUMBER}                        { yybegin(PROCEDURE); return NUMBER; }
      {STRING}                        { yybegin(PROCEDURE); return STRING; }
      "OF"                            { return OF; }
      {VARNAME}                       { yybegin(PROCEDURE); return VARNAME; }
}

    {LINENUMBER}                    { return TokenType.WHITE_SPACE; }
    {WHITE_SPACE}                   { return TokenType.WHITE_SPACE; }
    "/"                             { return TokenType.WHITE_SPACE; }
    {END_OF_LINE_COMMENT}           { return COMMENT; }
    "."                             { return DOT; }
    {STRING}                        { return STRING; }
    [^]                             { return TokenType.BAD_CHARACTER; }
