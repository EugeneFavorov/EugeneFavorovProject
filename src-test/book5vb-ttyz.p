/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2003 ТОО "Банковские информационные системы"
     Filename: book5vb.p
      Comment: Книга регистрации лицевых счетов
               (переделан из book5v-tty.p)
   Parameters:
         Uses: book5vb-tty.i
      Used by:
      Created: 01.03.2012 kraa
     Modified: 
*/

{globals.i}

DEFINE INPUT PARAMETER iParmStr AS CHARACTER NO-UNDO.
DEFINE BUFFER prevpos FOR acct-pos.

DEFINE VARIABLE i             AS INT64 NO-UNDO.
DEFINE VARIABLE mNewPage      AS LOGICAL NO-UNDO.  /* признак перехода на новую страницу */
DEFINE VARIABLE mNewPage1     AS LOGICAL NO-UNDO. /* признак перехода на вторую страницу */

DEFINE VARIABLE mDataSogl AS DATE NO-UNDO.                              

DEFINE VARIABLE in-name       AS CHARACTER FORMAT "x(35)" EXTENT 3  NO-UNDO.
DEFINE VARIABLE name          AS CHARACTER EXTENT 10                NO-UNDO.
DEFINE VARIABLE acct-surr     AS CHARACTER                          NO-UNDO.
DEFINE VARIABLE col1          AS CHARACTER EXTENT 10 FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE col2          AS CHARACTER EXTENT 10 FORMAT "x(40)" NO-UNDO.
DEFINE VARIABLE col3          AS CHARACTER FORMAT "x(18)"           NO-UNDO.
DEFINE VARIABLE col4          AS CHARACTER EXTENT 10 FORMAT "x(20)" NO-UNDO.
DEFINE VARIABLE col5          AS CHARACTER EXTENT 10 FORMAT "x(12)" NO-UNDO.
DEFINE VARIABLE col6          AS CHARACTER                          NO-UNDO.
DEFINE VARIABLE mCustCat2     AS CHARACTER                          NO-UNDO.
DEFINE VARIABLE mSewMode      AS CHARACTER                          NO-UNDO.
DEFINE VARIABLE mAcctCat      AS CHARACTER                          NO-UNDO.

{book1par.def &inpar="iParmStr"}

&IF DEFINED(v302) EQ 0 &THEN
   IF v302 THEN DO:
      RUN book55vb.p (iParmStr).
      RETURN.
   END.
&ENDIF

{acc-file.i &file=acct}

{chkacces.i}
{wordwrap.def}
{getdates.i}
{op-flt.i new}

/* запрос типа сшива */
RUN messmenu.p(
       10,
       "[Типы счетов]",
       "",
       "Все," +
       "Юридические," +
       "Физические," +
       "Внутрибанковские"
).
CASE INT64(pick-value):
   WHEN 1 THEN mSewMode = "".
   WHEN 2 THEN mSewMode = "Ю".
   WHEN 3 THEN mSewMode = "Ч".
   WHEN 4 THEN mSewMode = "В".
   OTHERWISE RETURN.
END CASE.

mAcctCat = GetParamByNameAsChar(iParmStr, "КатСч", "*").

{setdest.i &cols=150 &custom="printer.page-line -"} 

{book5vb-ttyz.i {&*}}

{preview.i}
