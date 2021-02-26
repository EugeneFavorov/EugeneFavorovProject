/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2006 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: cust-chk.p
      Comment: ��⮤ ������樨 ��.
   Parameters:
         Uses:
      Used by:
      Created: 
     Modified: 15/06/2006 ilvi (0062863) �������� ��易⥫��� ४����⮢ � ����ᨬ��� �� ஫�.
*/

DEF INPUT PARAM in-rec AS RECID NO-UNDO.

DEF VAR unkg      AS CHAR NO-UNDO.
DEF VAR regn      AS CHAR NO-UNDO.
DEF VAR mMsgErr   AS CHAR NO-UNDO. /* ����饭�� �� �訡��. */

{globals.i}             /* �������� ��६���� ��ᨨ. */
{intrface.get xclass}   /* ������⥪� �����㬥�⮢ ����奬�. */
{intrface.get tmess}    /* �����㬥��� ��ࠡ�⪨ ᮮ�饭��. */
{intrface.get cust}     /* ������⥪� ��� ࠡ��� � �����⠬�. */

MAIN_BLOCK:
DO
ON ERROR  UNDO MAIN_BLOCK, LEAVE MAIN_BLOCK
ON ENDKEY UNDO MAIN_BLOCK, LEAVE MAIN_BLOCK:

   FIND FIRST cust-corp WHERE
       RECID (cust-corp) EQ in-rec NO-LOCK NO-ERROR.
   IF NOT AVAIL cust-corp
   THEN DO:
      mMsgErr = "Error".
      LEAVE MAIN_BLOCK.
   END.
   
   regn = GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id),"RegNum",?).

   RUN chk-inn.p(string(cust-corp.cust-id), regn) NO-ERROR. /* �饬, ��� ��  ��� ����� � ⠪��� �� ४����⠬� */
   
   IF ERROR-STATUS:ERROR THEN DO: /* ������� ᮢ������� �� ४����⠬ ������ */
      RUN Fill-SysMes IN h_tmess ("", "", "-1",RETURN-VALUE).
      mMsgErr = "Error".
      LEAVE MAIN_BLOCK.
   END.

   {lg7001cl.i
      &in-class  = 'cust-corp'
      &surrogate = STRING(cust-corp.cust-id)
      &cl_name1  = "cust-corp.name-corp"
      &cl_name2  = "cust-corp.name-short"
   }  


   /* �஢�ઠ ���������� ��易⥫��� ४����⮢
   ** � ����ᨬ��� �� ஫� ��ꥪ�. */
   RUN ChkFieldManByRole IN h_cust ("cust-corp",(BUFFER cust-corp:HANDLE)) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN DO:
      mMsgErr = "Error".
      LEAVE MAIN_BLOCK.
   END.
   /* �᫨ ������ ᮧ���� ����, ���� 㤠�塞 */
   IF IsSubjClient("�",cust-corp.cust-id) THEN
      unkg = UNKg("cust-corp",STRING(cust-corp.cust-id)).
   ELSE
      UpdateSigns("cust-corp",STRING(cust-corp.cust-id),'����',?,?).

END.
RUN RunClassMethod IN h_xclass ('cust-corp',
                                "ExtraChkUpd",
                                "",
                                "",
                                ?,
                                STRING(cust-corp.cust-id)).
{intrface.del}          /* ���㧪� �����㬥����. */ 
IF mMsgErr NE ""
   THEN RETURN ERROR mMsgErr.
RETURN.

