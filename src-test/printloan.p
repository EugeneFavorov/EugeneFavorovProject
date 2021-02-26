/*
               KSV Editor
    Copyright: (C) 2000-2006 Serguey Klimoff (bulklodd)
     Filename: PRINTLOAN.P
      Comment: ��।������ ������� �� indicate � �����
   Parameters:
         Uses:
      Used by:
      Created: 18.06.2009 13:27 M.SEMENO
     Modified: 18.06.2009 13:27 M.SEMENO <comment>
     
     sku: ��� ��� �롮� ���. ��� ᤥ���� ࠢ��� ��� ������ ������
          ��� ���४⭮� ���� ������ � ࠧ�묨 ��⠬� ������
     
*/

{globals.i}
{intrface.get tmess}
{intrface.get refer}
{norm.i NEW}
{prn-doc.def &with_proc=YES}

DEFINE VARIABLE in-branch-id    LIKE DataBlock.branch-id    NO-UNDO.
DEFINE VARIABLE in-dataclass-id LIKE DataClass.DataClass-Id NO-UNDO.
DEFINE VARIABLE in-beg-date     LIKE DataBlock.Beg-Date     NO-UNDO.
DEFINE VARIABLE in-end-date     LIKE DataBlock.End-Date     NO-UNDO.

DEFINE VARIABLE InputFName      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mTemplateName   AS CHARACTER NO-UNDO. /* ��� 蠡���� ����⮩ ����, 
                                                         ���祭�� ���ண� ���� ��ࠡ��뢠���� */
DEFINE BUFFER dl1 FOR DataLine.
DEFINE BUFFER dl2 FOR DataLine.
DEFINE BUFFER dl3 FOR DataLine.
DEFINE BUFFER dl4 FOR DataLine.

DEFINE SHARED VARIABLE rid_loan  AS RECID.

/* DEFINE TEMP-TABLE TTNames NO-UNDO 
   FIELD tnumb  AS INT64
   FIELD tname  AS CHARACTER
   FIELD tvalue AS CHARACTER
&IF INT64(ENTRY(1,PROVERSION,".")) GE 10 &THEN
   FIELD tlong  AS BLOB
&ENDIF
   FIELD tCirc  AS CHARACTER
   FIELD tId    AS INT64
   FIELD mnum   AS INT64
INDEX tnumb AS UNIQUE PRIMARY tId tname tnumb
INDEX tname tId tname
. */

FIND FIRST loan WHERE
     RECID(loan) EQ rid_loan
    NO-LOCK NO-ERROR.
    
ASSIGN 
   in-beg-date = (if avail loan then loan.open-date else gbeg-date)
   in-end-date = (if avail loan then loan.open-date else gend-date)
   .
{norm-beg.i &title="'��������� ������' " &nofil=yes &is-branch = yes &nodate}

/*IF shFilial EQ "0500" THEN
DO:
*/
  /*�롮� �����ᠭ⮢*/
  {sign_select.i}
/*END.*/

RUN crtagval.p (gbeg-date, gend-date, ?,      NO, INPUT-OUTPUT TABLE ttnames).
/*���冷� ��ப �� ������, ᭠砫� ��ࠡ��뢠�� ��訬� ��楤�ࠬ� ��⮬ �⠭����묨*/ 
RUN crtagval.p (gbeg-date, gend-date, "����", NO, INPUT-OUTPUT TABLE ttnames).
RUN crtagval.p (gbeg-date, gend-date, "��",   NO, INPUT-OUTPUT TABLE ttnames).

{norm-end.i &nofil=yes}
FIND FIRST loan WHERE
     RECID(loan) EQ rid_loan
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE loan THEN
DO:
   RUN Fill-SysMes IN h_tmess ("", "", "0", "���������� ���� ������� ��� ����.").
   RETURN.
END.
mTemplateName = GetRefVal("prnloan",
                          in-end-date,
                          loan.Class-Code + "," + loan.cont-type + "," + loan.currency).
/*message mTemplateName VIEW-AS ALERT-BOX.*/

RUN printvd.p(mTemplateName, INPUT TABLE ttnames).

{intrface.del}
