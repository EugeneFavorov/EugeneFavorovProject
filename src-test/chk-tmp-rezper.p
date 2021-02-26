/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-1999 ТОО "Банковские информационные системы"
     Filename: chk-tmp.p
      Comment: Предшаблонная проверка наличия счета с определенной
               ролью для кредитного договора в loan-acct
   Parameters:
      Created: 12/10/2001 by ARSEN
*/

DEFINE INPUT  PARAMETER rec_opt AS RECID   NO-UNDO.
DEFINE OUTPUT PARAMETER result  AS INT64 NO-UNDO.

DEFINE VARIABLE mcontract AS CHARACTER NO-UNDO.
DEFINE VARIABLE mcontcode AS CHARACTER NO-UNDO.
DEFINE VARIABLE rol-acct  AS CHARACTER NO-UNDO.
DEFINE VARIABLE db-result AS LOG   NO-UNDO.
DEFINE VARIABLE cr-result AS LOG   NO-UNDO.
DEFINE VARIABLE phand AS HANDLE.

{globals.i}
{loan_sn.i}

/* Определение роли счета */
FUNCTION def-type-acct RETURN CHAR(input acct as char):

  define variable ws  as character no-undo.
  define variable wws as character no-undo.
  define variable i   as INT64   no-undo.

  wws = acct.
  IF CAN-DO("РольСРез*(*КредРез*", wws) THEN
    DO:
      wws = SUBSTRING(wws, INDEX(wws, "КредРез")).
      i = 1.
      DO WHILE i <= LENGTH(wws)
           AND SUBSTRING(wws, i, 1) <> ")"
           AND SUBSTRING(wws, i, 1) <> " "
           AND SUBSTRING(wws, i, 1) <> '"'
           AND SUBSTRING(wws, i, 1) <> "'"
           :
        i = i + 1.
      END.
      RETURN SUBSTRING(wws, 1, i - 1).
    END.
  ELSE RETURN "".

END FUNCTION.

/* Поиск счета в таблице loan-acct */
FUNCTION find-sm-acct RETURN LOG():
    DEF VAR aii AS INT NO-UNDO.
    aii = 0.
    FOR EACH loan-acct WHERE loan-acct.contract  EQ mcontract AND
                             loan-acct.cont-code EQ mcontcode AND
                             loan-acct.acct-type EQ rol-acct  AND
                             loan-acct.since     LE gend-date NO-LOCK:
      IF CAN-FIND(FIRST acct WHERE 
                        acct.acct       EQ loan-acct.acct
                    AND acct.currency   EQ loan-acct.currency
                    AND acct.close-date EQ ?) THEN aii = aii + 1.
    END.
    RETURN (aii > 1).
END FUNCTION.

RUN LOAN_VALID_HANDLE (input-output phand).
IF NOT VALID-HANDLE(phand) THEN
 DO:
   MESSAGE "Не установлены назначение и номер договора!" 
     VIEW-AS ALERT-BOX.
   result = 0.
 END.

mcontract = ENTRY(1, phand:PRIVATE-DATA).
mcontcode = ENTRY(2, phand:PRIVATE-DATA).

db-result = FALSE.
cr-result = FALSE.
                          
FIND op-template WHERE RECID(op-template) = rec_opt NO-LOCK.

rol-acct = def-type-acct (op-template.acct-db).
/* message "db=" rol-acct view-as alert-box.*/
IF rol-acct <> "" THEN db-result = find-sm-acct ().
                  ELSE db-result = FALSE.

rol-acct = def-type-acct (op-template.acct-cr).
/*message "cr=" rol-acct view-as alert-box.*/
IF rol-acct <> "" THEN cr-result = find-sm-acct ().
                  ELSE cr-result = FALSE.

IF db-result OR cr-result THEN result = 0.
                          ELSE result = -1.

