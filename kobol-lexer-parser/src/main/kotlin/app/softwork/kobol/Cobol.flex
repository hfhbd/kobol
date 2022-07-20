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

%state SQLS

// PROCEDURE
%state MOVE

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
    "SELECT"                        { yybegin(FILE_CONTROL); return FILE_CONFIG_SELECT_LITERAL; }

    "DATA"                          { yybegin(DATA); return CobolTypes.DATA; }
    "PROCEDURE"                     { yybegin(PROCEDURE); return CobolTypes.PROCEDURE; }
}

<FILE_CONTROL> {
    "ASSIGN"                        { return FILE_CONFIG_ASSIGN_LITERAL; }
    "FILE"                          { return FILE_LITERAL; }
    "STATUS"                        { return FILE_CONFIG_STATUS_STATUS_LITERAL; }
    "TO"                            { return CobolTypes.TO; }

    {VARNAME}                       { return VARNAME; }
    "."                             { yybegin(FILE_CONTROL_START); return DOT; }
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
   "FILE"                          { yybegin(FILE); return FILE_LITERAL; }
   "WORKING-STORAGE"               { yybegin(WORKINGSTORAGE); return WORKING_STORAGE; }
}

<FILE> {
   "FD"                            { yybegin(FD); return CobolTypes.FD; }
   {LINENUMBER}                    { return TokenType.WHITE_SPACE; }
   {NUMBER}                        { yybegin(FD_SA_PIC); return NUMBER; }
   "WORKING-STORAGE"               { yybegin(WORKINGSTORAGE); return WORKING_STORAGE; }
}

<FD> {
   "RECORDING"                     { return RECORDING_LITERAL; }
   "LABEL"                         { yybegin(FILE_CONTROL_LABEL); return CobolTypes.LABEL_LITERAL; }
   "RECORD"                        { yybegin(FILE_CONTROL_NEXTNUMBER); return CobolTypes.RECORD_LITERAL; }
   "DATA"                          { yybegin(FILE_CONTROL_LABEL); return CobolTypes.DATA; }
   "BLOCK"                         { yybegin(FILE_CONTROL_NEXTNUMBER); return CobolTypes.BLOCK_LITERAL; }
   "."                             { yybegin(FILE); return DOT; }
   {VARNAME}                       { return VARNAME; }
}

<FILE_CONTROL_NEXTNUMBER> {
   {NUMBER}                        { yybegin(FD);return NUMBER; }
}
<FILE_CONTROL_LABEL> {
    "RECORD"                       { return CobolTypes.RECORD_LITERAL; }
    "STANDARD"                     { yybegin(FD); return STANDARD; }
    {VARNAME}                      { yybegin(FD); return VARNAME; }
}

<WORKINGSTORAGE> {
   {LINENUMBER}                    { return TokenType.WHITE_SPACE; }
   {NUMBER}                        { yybegin(WORKINGSTORAGE_SA_PIC); return NUMBER; }
   "EXEC"                          { return EXEC; }
   "SQL"                           { yybegin(SQLS); return SQL; }
   "PROCEDURE"                     { yybegin(PROCEDURE); return CobolTypes.PROCEDURE; }
}

<SQLS> {
    {LINENUMBER}                    { return TokenType.WHITE_SPACE; }
    "END-EXEC"                      { yybegin(WORKINGSTORAGE); return END_EXEC; }
    {WHITE_SPACE}                   { return TokenType.WHITE_SPACE; }
    [^]                             { return CobolTypes.ANY; }
}

<WORKINGSTORAGE_SA_PIC, FD_SA_PIC> {
      "PIC" { return PIC_LITERAL;}
      "X" { return PIC_XA;}
      "A" { return PIC_XA;}
      "9" { return PIC_9;}
      "S9" { return PIC_S_9;}
      "V9" { return PIC_V_9; }
      "(" {
                    if (yystate() == FD_SA_PIC) {
                                  yybegin(FD_SA_PIC_LENGTH);
                              } else if(yystate() == WORKINGSTORAGE_SA_PIC) {
                                  yybegin(WORKINGSTORAGE_SA_PIC_LENGTH);
                              }
          return LP; }
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
      "ON" { return ON; }
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
                    "LOW-VALUE" {
                                            if (yystate() == FD_SA_NUMBER) {
                                                yybegin(FD_SA_NUMBER_LINE);
                                            } else if(yystate() == WORKINGSTORAGE_SA_NUMBER) {
                                                yybegin(WORKINGSTORAGE_SA_NUMBER_LINE);
                                            }
                                            return LOW_VALUE;
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
    "DISPLAY"                       { return CobolTypes.DISPLAY; }
    "MOVE"                          { yybegin(MOVE); return CobolTypes.MOVE; }
    "PERFORM"                       { return PERFORM; }

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
      "TO"                            { yybegin(PROCEDURE); return CobolTypes.TO; }
      {VARNAME}                       { return VARNAME; }
}

    {LINENUMBER}                    { return TokenType.WHITE_SPACE; }
    {WHITE_SPACE}                   { return TokenType.WHITE_SPACE; }
    "/"                             { return TokenType.WHITE_SPACE; }
    {END_OF_LINE_COMMENT}           { return COMMENT; }
    "."                             { return DOT; }
    {STRING}                        { return STRING; }
    [^]                             { return TokenType.BAD_CHARACTER; }
