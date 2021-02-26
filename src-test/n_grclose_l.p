/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2009 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: grclose_l.p
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
      Created: 30.12.2009 12:41 Daru    
     Modified: 30.12.2009 12:41 Daru    
*/

DEFINE INPUT  PARAMETER iContract  AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER iDate      AS DATE        NO-UNDO.
DEFINE INPUT  PARAMETER iGrupParam AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER iFilter    AS CHARACTER   NO-UNDO. 
DEFINE INPUT  PARAMETER iDisplay   AS LOGICAL     NO-UNDO.

{globals.i}             /* �������� ��६���� ��ᨨ. */
{tmprecid.def }
{intrface.get cdrep}
{intrface.get tmess}    /* �����㬥��� ��ࠡ�⪨ ᮮ�饭��. */
{intrface.get loan}     /* �����㬥��� ��� ࠡ��� � ⠡��窮� loan. */

DEFINE BUFFER bloan FOR loan. /* ���������� ����. */
DEFINE NEW SHARED STREAM err.
DEFINE NEW SHARED FRAME l.



def var dateclose as date no-undo.
dateclose = today.
  
pause 0.

Do on error undo, leave on endkey undo, leave with frame ftune:
  Update
    dateclose label "��� �������" help "������ ���� �������"   

  with centered row 10 overlay side-labels 1 col
  title "[  ]".
End.
Hide frame ftune no-pause.

if LASTKEY EQ KEYCODE("ESC") THEN
	return.


FUNCTION ParamsClose RETURNS  LOGICAL
   (iContract AS CHAR,
    iContCode AS CHAR,
    iDate     AS DATE,
    iParams   AS CHAR):

   DEFINE VARIABLE vI      AS INT64     NO-UNDO.
   DEFINE VARIABLE vReturn AS LOGICAL     NO-UNDO.

   block-param:
   DO vI = 1 TO NUM-ENTRIES(iParams):
      IF LN_GetParams(iContract,
                     iContCode,
                     REPLACE(ENTRY(vI,iParams),"+",","),
                     iDate) GT 0
      THEN DO:
         vReturn = TRUE.
         LEAVE block-param.
      END.
   END.
   RETURN vReturn.
END FUNCTION. 
 
IF    iFilter EQ "" 
   OR iFilter EQ ? 
   OR iFilter EQ "*" 
THEN ASSIGN
   iFilter  = "*"
   iDisplay = YES
.

IF NOT iDisplay 
THEN
   SUBSCRIBE "AfterNavigate" ANYWHERE RUN-PROCEDURE "SelectRecords". 
RUN browseld.p ((IF iContract EQ "�।��"
                 THEN "loan_allocat"
                 ELSE "loan_attract"),
                "UserConf"                             +  CHR(1) + "end-date2"   + chr(1) + "RidRest",
                IF iFilter EQ "" THEN "*" ELSE iFilter +  CHR(1) + STRING(iDate) + chr(1) + "yes",
                "end-date2",
                4).
     
UNSUBSCRIBE "AfterNavigate".
{setdest.i &stream="stream err" &filename='_spool1.tmp'} 
OS-DELETE '_spool3.tmp'.
RUN SetSysConf IN h_base("AUTOTEST:autotest","yes"). 
RUN SetSysConf IN h_base("Append_spool3","yes").
RUN SetSysConf IN h_base("NoProtocol", "YES").


def temp-table mytemp
field id like tmprecid.id.

for each tmprecid no-lock:
create mytemp.
assign 
mytemp.id = tmprecid.id.
end.
/*
message string(dateclose) view-as alert-box.
  */
/* �஢�ਬ ������ ������� � �����⠥� */

FOR EACH mytemp EXCLUSIVE-LOCK,
   FIRST loan WHERE RECID(loan)   EQ mytemp.id
NO-LOCK:

   /* ������ ������� �� ���⢥ত����� �������� ����. */
   IF loan.since LE loan.end-date THEN 
      RUN l-calc2.p (loan.contract,       /* �����祭�� �������. */
                     loan.cont-code,      /* ����� �������. */
                     dateclose + 1,   /* ����砭�� ������� + ���� ��� �믮������ ��⮬. */
                     TRUE,
                     FALSE).
  
/*   IF loan.since LE dateclose THEN
   DO:
      RUN PutStrem (
         "� ������� � ���ண�⮬ '" + loan.contract + "," + loan.cont-code +
         "�������� �訡�� �� ������. ������� �� �㤥� ������.\n").
      DELETE mytemp.
   END.
*/
END.


FOR EACH mytemp EXCLUSIVE-LOCK,
   FIRST loan WHERE RECID(loan)   EQ mytemp.id
                AND loan.cont-type NE "��祭��"
NO-LOCK:
   IF ParamsClose(loan.contract,
                  loan.cont-code,
                  dateclose,
                  iGrupParam)
   THEN
      RUN PutStrem (
         "� ������� � ���ண�⮬ '" + loan.contract + "," + loan.cont-code +
         "������⥫쭠� �㬬� ��ࠬ��஢. ������� �� �㤥� ������.\n").
   ELSE
      RUN CloseLoan IN h_loan (loan.contract,
                               loan.cont-code,
                               dateclose,
                               1).

   DELETE mytemp.

END.

FOR EACH mytemp EXCLUSIVE-LOCK,
   FIRST loan WHERE RECID(loan)   EQ mytemp.id
                AND loan.cont-type EQ "��祭��"
NO-LOCK:
   IF     loan.cont-type EQ "��祭��"
      AND NOT CAN-FIND (FIRST bloan WHERE bloan.contract  EQ loan.contract
                                      AND loan.close-date EQ ?
                                      AND bloan.cont-code BEGINS loan.cont-code + " "
                                      AND NUM-ENTRIES(bloan.cont-code," ") EQ 2
                ) 
   THEN DO:
      RUN PutStrem (
         "� ������� � ���ண�⮬ '" + loan.contract + "," + loan.cont-code +
         "���� �� ������� �࠭�. ������� �� �㤥� ������.\n").
   END.
   ELSE
      IF ParamsClose(loan.contract,
                     loan.cont-code,
                     dateclose,
                     iGrupParam)
      THEN
         RUN PutStrem (
            "� ������� � ���ண�⮬ '" + loan.contract + "," + loan.cont-code +
            "������⥫쭠� �㬬� ��ࠬ��஢. ������� �� �㤥� ������.\n").
      ELSE
         RUN CloseLoan IN h_loan (loan.contract,
                                  loan.cont-code,
                                  dateclose,
                                  1).
END.

RUN PutStrem ("\n").
RUN DeleteOldDataProtocol IN h_base("AUTOTEST:autotest"). 
RUN DeleteOldDataProtocol IN h_base("Append_spool3").
RUN DeleteOldDataProtocol IN h_base("NoProtocol").
{preview.i &stream="stream err" &filename='_spool1.tmp'}

PROCEDURE SelectRecords.

   DEFINE INPUT  PARAMETER iH    AS HANDLE  NO-UNDO. /* ��� ��㧥� */
   DEFINE OUTPUT PARAMETER oCont AS LOGICAL NO-UNDO. /* �����뢠��/�� �����뢠�� ��㧥� */

   DEFINE VARIABLE vHQ     AS HANDLE  NO-UNDO.
   DEFINE VARIABLE vHB     AS HANDLE  NO-UNDO.
   DEFINE VARIABLE vCnt    AS INT64 NO-UNDO.
   DEFINE VARIABLE vbuf-id AS INT64 INIT ? NO-UNDO.

   RUN Open-Query IN iH. /* ������ ����� � ��㧥� */

   vHQ = DYNAMIC-FUNCTION("GetHandleQuery" IN iH).

   IF NOT vHQ:IS-OPEN THEN RETURN.

   DO vCnt = 1 TO vHQ:NUM-BUFFERS:
      IF vHQ:GET-BUFFER-HANDLE(vCnt):TABLE EQ "loan" THEN
      DO:
         vbuf-id = vCnt.
         LEAVE.
      END.
   END.
   {empty tmprecid}
   IF vbuf-id <> ? THEN
   DO:
      vHB = vHQ:GET-BUFFER-HANDLE(vbuf-id).
     
      vHQ:GET-FIRST.
     
      DO WHILE vHB:AVAIL:
     
         RUN CalcVar IN iH.
     
         IF DYNAMIC-FUNCTION("ValidRecord" IN iH) THEN
         DO:
            CREATE tmprecid.
            tmprecid.id = vHB:RECID.
            MESSAGE vHB:RECID
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
         END.

         vHQ:GET-NEXT.
      END. 
   END.

END PROCEDURE.

define var d      as char    format "x(60)".
form
    a like d at 1 skip
    b like d at 1 skip
    c like d at 1
with frame l
    title "[ �����⨥ ������� " +
        string(loan.cont-code) + "]"
    col 2 row 15 color messages
        top-only overlay no-labels 1 col width 65.

PROCEDURE PutStrem:
   DEFINE INPUT PARAMETER iMess AS CHARACTER   NO-UNDO.
   PUT STREAM err UNFORMATTED iMess.
END PROCEDURE.

PROCEDURE DispMess:
   DEFINE INPUT PARAMETER iMess AS CHARACTER   NO-UNDO.
   
   OUTPUT TO TERMINAL.

   CLEAR FRAME l NO-PAUSE.

   DISP iMess @ a
   WITH FRAME l.

   OUTPUT CLOSE.

   PUT STREAM err UNFORMATTED  " " SKIP iMess SKIP.

END PROCEDURE.


PROCEDURE DispMess2:
   DEFINE INPUT PARAMETER iMess1 AS CHARACTER   NO-UNDO.
   DEFINE INPUT PARAMETER iMess2 AS CHARACTER   NO-UNDO.
   
   OUTPUT TO TERMINAL.
   CLEAR FRAME l NO-PAUSE.

   DISP iMess1  @ a
        iMess2  @ b
   WITH FRAME l.

   OUTPUT CLOSE.
   iMess1 = iMess1 + iMess2.
   PUT STREAM err UNFORMATTED " " SKIP  iMess1 SKIP.

END PROCEDURE.

