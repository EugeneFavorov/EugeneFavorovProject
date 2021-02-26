DEFINE INPUT PARAMETER in-op-date   LIKE op.op-date NO-UNDO.
DEFINE INPUT PARAMETER in-rec-kind  AS   RECID      NO-UNDO.

{globals.i}
{pick-val.i}
{wordwrap.def}
{intrface.get xclass}
{intrface.get cust}
{tt-ps.def}
{getdates.i}
def stream out_.
{setdest2.i &stream="stream out_" &filename="spool.tmp" &cols=160}


DEF VAR vReturnCode  AS INT64   NO-UNDO. /* ��� ������       */
DEF VAR bStr      AS CHAR  NO-UNDO. /* ��ப� ��� �஢�ન � ������� ��. */
DEF VAR VStr      AS CHAR  NO-UNDO. 
def var vOk  as logic.
put stream out_ unformatted " ��⮪�� ���������  �������⥫��� ४����⮢. ���᮪ �������� ���㬥�⮢."  skip.
put stream out_ unformatted " ���㬥�� �����:  ����ঠ���: "  skip.
for each op-entry where op-entry.acct-cr begins "30111810"
                    and op-entry.op-date ge beg-date
                    and op-entry.op-date le end-date
                    and op-entry.filial-id eq shfilial
                    .
   find first op where op.op = op-entry.op no-lock no-error.
   RUN ChkDetails (op.details,OUTPUT vReturnCode).
   if vReturnCode <> 0 THEN do:
      bStr = "00034" + ",," + string(op-entry.amt-rub) + ",0,," + string(op-entry.amt-rub) + ",1,1,�,".
      vOk =  UpdateSigns("op",STRING(op.op),"��������117",bStr,no).
      vStr = fill(" ",14 - length(trim(op.doc-num))).  
      PUT STREAM  out_  UNFORMATTED vStr .
      PUT STREAM  out_  trim(op.doc-num) " "  op.details  skip.

   end.

end.
PUT STREAM out_ unformatted "���  : " TODAY SKIP.
PUT STREAM out_ unformatted "�६� : " STRING(TIME, "HH:MM:SS AM") SKIP(1).
{preview2.i &stream="stream out_" &filename=spool.tmp }




PROCEDURE ChkDetails:
   DEFINE INPUT  PARAMETER iStr        AS CHAR    NO-UNDO. /* ����ঠ��� ���㬥��. */
   DEFINE OUTPUT PARAMETER oRetCode    AS INT64 NO-UNDO.
   DEF VAR iDelim1      AS CHAR  NO-UNDO. 
   DEF VAR iDelim2      AS CHAR  NO-UNDO. 

   ASSIGN
      iDelim1  = "~{"
      iDelim2 = "~}"
   .

   DEF VAR vStr      AS CHAR  NO-UNDO CASE-SENSITIVE. 
   DEF VAR vResult   AS CHAR  NO-UNDO. 
   DEF VAR vErrMsg   AS CHAR  NO-UNDO. 
   DEF VAR vPSCode   AS CHAR  NO-UNDO. 
   DEF VAR vPS       AS CHAR  NO-UNDO. 
   DEF VAR i         AS int  NO-UNDO. 
   DEFINE VARIABLE vDate318p AS DATE    NO-UNDO. /* ��� ����㯫���� ���㬥�� */
   oRetCode = 0.
   IF AVAILABLE(op) THEN
      IF op.ins-date NE ? THEN
         vDate318p = op.ins-date.
      ELSE
         vDate318p = op.op-date.
   ELSE
      vDate318p = TODAY.

   /* ����� ������ ���� ��। ⥪�⮢�� ����� ���㬥��.
   ** ����� ������ ���� 㪠��� � 䨣���� ᪮����. */
   IF    INDEX (iStr, iDelim1) EQ 0
      OR INDEX (iStr, iDelim2) EQ 0
   THEN DO:
      oRetCode = 1.
      RETURN.
   END.

   /* ����稫� ��ப� ��� �������. */
   ASSIGN
      vStr = SUBSTRING (iStr, INDEX (iStr, iDelim1) + 1)
      vStr = SUBSTRING (vStr, 1, INDEX (vStr, iDelim2) - 1)
   .
   IF INDEX(vStr,"PS") > 0 THEN ASSIGN
      vPSCode = SUBSTR(vStr,INDEX(vStr,"PS") + 2)
      vPS     = SUBSTR(vStr,INDEX(vStr,"PS"))
      vStr    = SUBSTR(vStr,1,INDEX(vStr,"PS") - 1)
   .

   /* �஡���� �� ������ ����. */
   IF INDEX (vStr, " ") NE 0
   THEN DO:
      oRetCode = 2.
      RETURN.
   END.

   /* �஢�ઠ ���४⭮�� �����. */
   IF vPS NE "" THEN DO:
         oRetCode = 3.
         RETURN.
   END.

   IF substr(vStr,1,2) <> "VO"
   THEN DO:
      oRetCode = 31.
      RETURN.
   END.
   do i = 1 to 5.
      IF INDEX ("1234567890",substr(vStr, i + 2 ,1)) = 0
      THEN DO:
         oRetCode = 32.
         RETURN.
      END.
   end.
   vStr = SUBSTRING(vStr, 3, 5).
   IF GetCode("��������117", vStr) = ? THEN
   DO: 
      oRetCode = 4.
      RETURN.
   END.

   IF CAPS(GetCodeMisc("��������117", vStr,1)) EQ "��" THEN
   DO: 
      oRetCode = 41.
      RETURN.
   END.
   RETURN.
END PROCEDURE.
