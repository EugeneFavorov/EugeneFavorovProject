/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2015 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: loanLD.p
      Comment: ���������� ⠡��� recid �⮡࠭���  � 䨫��� ����ᥩ.
   Parameters: "-1" - �� ������� ����ன�� 䨫���.
   Parameters:
         Uses: all_flt.p
      Used by: 
      Created: 06/03/00 Om 
     Modified: 24/05/2002 Om ��ࠡ�⪠: ������祭�� � �������᪮�� 䨫����.
                             ������  : ��� �訡�� �ᥣ�� "-1". :-(
     Modified: 08/08/2003 Om ��ࠡ�⪠: ���� ����ன�� 䨫��� � ��⮬ sub-code.
  Last change:
*/

form "~n@(#) loanLD.p 1.0 Om 06/03/00"
with frame sccs-id stream-io width 250.

{globals.i}         /* �������� ��६���� ��ᨨ. */
{flt-file.i new}    /* ��।������ �������� �������᪮�� 䨫���. */
{all_note.def}      /* ������ � recid, ��࠭��� �� 䨫���� ����ᥩ Shared */
{flt_var.def}       /* ��६����, ��騥 ��� ��� ⨯�� 䨫��஢ Shared */

{tmpobj.def}      /* ������ ��� ࠡ��� � ��㧥஬. */
{tmprecid.def}    /* ��� ��।�� �⮡࠭�� ����ᥩ. */

{intrface.get "rights"} /* ����㧪� �����㬥���� ��।������ �����. */
{wclass.i}

def var list-class as char no-undo. /* ᯨ᮪ ��� �������ᮢ ⥪�饣� ����� */
def var num-class  as INT64  no-undo. /* N ����� */

DEF VAR sub_user    AS CHAR NO-UNDO. /* ���짮��⥫� � ��� ���稭����. */
DEF VAR class_avail AS CHAR NO-UNDO. /* ���祭� ����㯭�� ����ᮢ. */
DEF VAR vContract   AS CHAR NO-UNDO. /* ��� QRY     */
DEF VAR incontr     AS CHAR INIT "�।��" NO-UNDO. /* �����祭�� �������. */
DEF VAR vClass      AS CHAR   NO-UNDO.
DEF VAR vCounter    AS INT64  NO-UNDO.


DEFINE VARIABLE iPotok  AS INT64 NO-UNDO.
DEFINE VARIABLE iFilial AS CHARACTER NO-UNDO.
DEFINE VARIABLE iDelim  AS INT64 NO-UNDO.
DEFINE VARIABLE mInt AS INTEGER NO-UNDO.

mInt = 0.

/*
#  1 - ��砫�� ��⮪
#  2 - ������  ��⮪
#  3 -           ��⮪
#  4 - ������⢮ ��⮪��
#  5 - 䨫���
#  6 - ᯨ᮪ �࠭���権 �१ ;
#  7 - �� ��直� ��砩
*/

IF NUM-ENTRIES(session:parameter) GE 4 THEN
ASSIGN
   iPotok = INT64(ENTRY(3,session:parameter))
   iFilial = ENTRY(5,session:parameter)
   iDelim  = INT64(ENTRY(4,session:parameter)).
ELSE 
ASSIGN
   iPotok = INT64(OS-GETENV("POTOK"))
   iFilial = OS-GETENV("FILIAL")
   iDelim  = INT64(OS-GETENV("DELIM")).

FOR EACH loan WHERE
      (loan.close-date EQ ? OR
       loan.close-date GE TODAY)
   AND loan.contract   EQ "������"
   AND loan.filial-id  = iFilial
   AND INT64(RECID(loan)) MODULO iDelim EQ iPotok
   NO-LOCK:

   CREATE all_recids.
   CREATE TmpObj.
   CREATE tmprecid.
   ASSIGN
      i = i + 1
      mInt = mInt + 1
      all_recids.count = i
      all_recids.rid   = RECID(loan)
      TmpObj.rid       = RECID(loan)
      tmprecid.id      = RECID(loan)
   .
END.

/* ���㧪� �����㬥�⮢. */
{intrface.del "rights"}

RETURN.

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
   {empty all_recids}
   IF vbuf-id <> ? THEN
   DO:
      vHB = vHQ:GET-BUFFER-HANDLE(vbuf-id).
      RUN GetFirstRecord IN iH (vHQ).
      DO WHILE NOT vHQ:QUERY-OFF-END:
         CREATE all_recids.
         CREATE TmpObj.
         CREATE tmprecid.
         ASSIGN
            i = i + 1
            all_recids.count = i
            all_recids.rid   = vHB:RECID
            TmpObj.rid       = vHB:RECID
            tmprecid.id      = vHB:RECID
         .
         RUN GetNextRecord IN iH (vHQ).
      END. 
   END.

END PROCEDURE.
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='17/08/2015 07:51:17.810+04:00' */
/* $LINTUSER='glaa' */
/* $LINTMODE='1' */
/* $LINTFILE='loanld.p' */
/*prosign0RTDPKaDd0atZCfeYIApmg*/