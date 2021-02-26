/*                      
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2014 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: incjourtemp.p
      Comment: ����, ᮧ����� ������஬ ���⮢
      Comment: ��ୠ� ��� �㬮�
   Parameters: Input: RID -  RecID(op)
         Uses: Globals.I ChkAcces.I SetDest.I PreView.I
      Used by:
      Created: 21/02/11 10:31:07
     Modified:
*/
Form "~n@(#) incjour-318p.p 1.0 RGen 21/02/11 RGen 21/02/11 [ AutoReport By R-Gen ]"
     with frame sccs-id stream-io width 250.

{globals.i}
{sh-defs.i}
{chkacces.i}
/*-------------------- �室�� ��ࠬ���� --------------------*/
Define Input Param RID as RecID no-undo.

/*-------------------- ������� ��६����� --------------------*/

Define Buffer buf_0_op               For op.

/*--------------- ���� ��� ����� ��: ---------------*/

/*--------------- ��६���� ��� ᯥ樠���� �����: ---------------*/
Define Variable c-baul           As Character            No-Undo.
Define Variable c-empty          As Character            No-Undo.
Define Variable c-number         As INT64              No-Undo.
Define Variable c-totec          As Character            No-Undo.
Define Variable c-totsc          As Character            No-Undo.
Define Variable c-totst          As Character Extent   2 No-Undo.
Define Variable dnow             As Date                 No-Undo.
Define Variable dnow1            As Date                 No-Undo.
Define Variable incass           As Character            No-Undo.
Define Variable NameBank         As Character            No-Undo.
Define Variable NMarsh           As Character            No-Undo.
Define Variable sum-nac          As Decimal              No-Undo.
Define Variable sum-tot          As Decimal              No-Undo.
Define Variable SumBeg           As Character            No-Undo.
Define Variable tnow             As Character            No-Undo.
Define Variable prim             As Character            No-Undo.

/*--------------- ��।������ �� ��� 横��� ---------------*/
/* ��ଠ ��� 横�� "NameBankCycle" */
/*Form
         NameBank format "x(71)" at 8 skip
with frame frm_-2 down no-labels no-underline no-box width 102.

Def Var FH_incjour-318p-2 as INT64 init 1 no-undo. /* frm_2: ���. ��ப �� ���室� �� ����� ��࠭��� */

/* ��ଠ ��� 横�� "baul" */
Form
         "�" at 8 c-number format ">>>>>9" at 9 " �" at 15 c-baul format "x(12)" at 19 "  �" at 31 sum-nac format "zzz,zzz,zzz,zz9.99" at 37 "     �" at 45 prim format "x(10)" "  �" at 55 skip
Header
                                        "������" at 39 skip(1)
                        "��� �ਭ���� �㬮� � ��஦��� �㬮�" at 23 skip(1)
                               /* "���� ___ ���⮢ ____" at 31 skip*/
         "����������������������������������������������������������������������Ŀ" at 8 skip
         "�N �/�  �     �����     �   �㬬� �� �������� �   �  �ਬ�砭��      �" at 8 skip
         "�       � �ਭ���� �㬮� �     �㬪�� (��ࠬ�)     �                  �" at 8 skip
         "����������������������������������������������������������������������Ĵ" at 8 skip
with frame frm_-1 down no-labels no-underline no-box width 102.

Form
         "������������������������������������������������������������������������" at 8 skip
with frame frm_1 down no-labels no-underline no-box width 102.
Def Var FH_incjour-318p-1 as INT64 init 10 no-undo.*/ /* frm_1: ���. ��ப �� ���室� �� ����� ��࠭��� */


/* ��砫�� ����⢨� */
{wordwrap.def}
{intrface.get xclass}
{inc-nac.def new}
{intrface.get tmess}    /* �����㬥��� ��ࠡ�⪨ ᮮ�饭��. */
{def-wf.i new}
{defframe.i new}
{tmprecid.def}
{ttretval.def}

DEFINE VARIABLE in-date AS CHARACTER no-UNDO.
DEFINE VARIABLE c-marsh AS CHARACTER NO-UNDO.

def var ost as char.
DEFINE VARIABLE mTimeRep AS INT64 NO-UNDO.
DEFINE VARIABLE mHr      AS INT64 NO-UNDO.
DEFINE VARIABLE mMin     AS INT64 NO-UNDO.
DEFINE VARIABLE mSec     AS INT64 NO-UNDO.

DEFINE VARIABLE i          AS INT64 NO-UNDO.
DEFINE VARIABLE mcNameBank AS CHARACTER EXTENT 5 NO-UNDO.

DEF TEMP-TABLE ttTemp NO-UNDO
	FIELD vNumM  AS CHAR
	FIELD vNumS  AS CHAR
	FIELD vSum   AS DEC
	FIELD vDateM AS DATE.
	
DEF VAR vTmpNum1 AS CHAR.
DEF VAR vTmpNum2 AS CHAR.
DEF VAR vTmpNumM AS CHAR.
DEF VAR vOpDate1 AS DATE. 
DEF VAR vTSumN   AS CHAR.
DEF VAR vTSumI   AS CHAR.

&GLOB Months "ﭢ���,䥢ࠫ�,����,��५�,���,���,���,������,ᥭ����,~
������,�����,�������"
&GLOB Days "��ࢮ�,��஥,����,�⢥�⮥,��⮥,��⮥,ᥤ쬮�,���쬮�,����⮥,����⮥,��������⮥,�������⮥,�ਭ���⮥,���ୠ��⮥,~
��⭠��⮥,��⭠��⮥,ᥬ����⮥,��ᥬ����⮥,����⭠��⮥,�����⮥,������� ��ࢮ�,������� ��஥,������� ����,~
������� �⢥�⮥,������� ��⮥,������� ��⮥,������� ᥤ쬮�,������� ���쬮�,������� ����⮥,�ਤ�⮥,�ਤ��� ��ࢮ�"

/*FIND FIRST tmprecid NO-LOCK NO-ERROR.
FIND FIRST op WHERE RECID(op) = tmprecid.id NO-LOCK NO-ERROR.
	IF AVAIL(op) THEN vOpDate1 = op.op-date.*/
	
{getdate.i}

RUN g-prompt.p('CHAR',
               '� �������',
               'x(6)',
               '?',
               '������� � ��������',
               30,
               '',
               '',
               ?,?,OUTPUT vTmpNumM).
			   
FOR EACH op-entry
 WHERE op-entry.filial-id EQ shFilial 
   AND op-entry.acct-db begins '20202810505001000000'
   AND op-entry.op-date GE end-date - 7
   AND op-entry.op-date LE end-date + 14 /*�ࠧ�����*/
    NO-LOCK,
FIRST op OF op-entry WHERE op.op-kind BEGINS '0303i' 
					 AND   op.doc-date EQ end-date 
					 AND   op.op-status NE '�' NO-LOCK:
	vTmpNum1 = GetXattrValue('op',string(op.op),'������������').
	vTmpNum2 = GetXattrValue('op',string(op.op),'�����_�㬪�').
	IF vTmpNum1 EQ vTmpNumM THEN DO:
		FIND FIRST ttTemp 
			WHERE ttTemp.vNumM  EQ vTmpNum1
			AND   ttTemp.vDateM EQ op.doc-date 
			AND   ttTemp.vNumS  EQ vTmpNum2 NO-ERROR.
		IF NOT AVAIL(ttTemp) THEN DO:
			vTSumN = GetXattrValue('op',string(op.op),'�㬬��������').
			vTSumI = GetXattrValue('op',string(op.op),'�㬬������').
			CREATE ttTemp.
			ASSIGN 
				ttTemp.vNumM  = vTmpNum1
				ttTemp.VNumS  = vTmpNum2
				ttTemp.vSum   = op-entry.amt-rub + (IF vTSumN NE '' THEN DEC(vTSumN) ELSE 0) - (IF vTSumN NE '' THEN DEC(vTSumI) ELSE 0)
				ttTemp.vDateM = op.doc-date
			.
		END.
		ELSE ttTemp.vSum = ttTemp.vSum + op-entry.amt-rub.
	END.
END.

mcNameBank[1] = '��᪨� 䨫��� ��� "���� ����"'.
/*FIND FIRST branch WHERE 
   branch.Branch-Type EQ "00" 
   NO-LOCK NO-ERROR.
mcNameBank[1] = (IF AVAIL branch THEN (branch.name + "~n") ELSE "") + fGetSetting("����","","").
FIND FIRST branch WHERE
   branch.Branch-Id EQ dept.Branch
   NO-LOCK NO-ERROR.
mcNameBank[1] = mcNameBank[1] + (IF AVAIL branch THEN ("~n" + branch.name) ELSE "").*/

{wordwrap.i
 &s = mcNameBank
 &n = 5
 &l = 71
 &centered = YES
 }

/* ��⠭���� �ਧ��� ⮣�, �� �� �� ���� ���ᮭ, � �������� */
/*RUN SetSysConf IN h_base("DRIncass","YES").

RUN browseld.p ("person",
               "sc-1" + chr(1) + "sv-1" + chr(1) + "crClass-Code" + chr(1) + "formmeth" + chr(1) + "RetRcp" + chr(1) + "RetFld" + chr(1) + "RetType",
               "incass" + chr(1) + "��" + chr(1) + "*" + chr(1) + "U1" + chr(1) + STRING(TEMP-TABLE ttRetVal:HANDLE) + chr(1) + "person-id" + chr(1) + "Multi",
                ?,
               4).
IF KEYFUNCTION(LASTKEY) NE 'END-ERROR' THEN DO:
	FOR EACH ttRetVal:
		FIND FIRST person 
			WHERE STRING(person.person-id) EQ ttRetVal.PickValue
		NO-LOCK NO-ERROR.
		IF AVAIL(person) THEN
			incass = incass + ', ' + person.name-last + ' ' + person.first-names.
	END.
END.

/* ���६ �ਧ��� ⮣�, �� �� �� ���� ���ᮭ, � �������� */
RUN SetSysConf IN h_base ("DRIncass", "?").*/

dnow = TODAY.
in-date = STRING(DAY(dnow),"99") + " " + ENTRY(MONTH(dnow),{&Months}) + " " + STRING(YEAR(dnow)) + " �.".
mTimeRep = TIME.
ASSIGN
   mSec     = mTimeRep MOD 60
   mTimeRep = (mTimeRep - mSec) / 60
   mMin     = mTimeRep MOD 60
   mHr      = (mTimeRep - mMin) / 60 + 3
   tnow     = STRING(mHr, "99") + " ��. " + STRING(mMin, "99") + " ���."
.   

/* �࠭����� ��� �।���饭�� �㡫�஢���� ���ଠ樨 � ��砥    ����୮�� �ନ஢���� ��᫥ �⬥�� �� �����-�� �����-� */
DO ON ERROR UNDO, RETRY:  /* END. � ������� ����⢨�� */

 /* 25.06.03 old �㦭� ⮫쪮 �㬬� �ய���� - ���-�� ��஦��� �㬮�
 c-totec = string(accum count tt-nac.opr) + " ( " + c-totec + " )".
 */

/*-------------------- ��ନ஢���� ���� --------------------*/
{strtout3.i &cols=102 &option=Paged}

put unformatted "                                                                                 �������������������Ŀ" skip.
put unformatted "                                                                                 �     ��� ���     �" skip.
put unformatted "                                                                                 � ���㬥�� �� ���� �" skip.
put unformatted "                                                                                 �������������������Ĵ" skip.
put unformatted "                                                                                 �      0402301      �" skip.
put unformatted "                                                                                 ���������������������" skip.

/* ��砫� 横�� "NameBankCycle" */
put skip.
put unformatted "              " mcNameBAnk[1] skip.
/*do:
  i = 1.
  DO WHILE i < 5 AND mcNameBAnk[i] NE "" :
     NameBank = mcNameBAnk[i].
     i = i + 1.

  Disp
    NameBank
  with frame frm_-2.
  if Line-Count + FH_incjour-318p-2 >= Page-Size and Page-Size <> 0 then do:
    Page.
  end.
  else
    Down with frame frm_-2.
  end.
end.*/
/* ����� 横�� "NameBankCycle" */

PUT UNFORMATTED
   FILL('�', 101) SKIP
   "������ �ଥ���� (᮪�饭��� �ଥ����) ������������"
   " �।�⭮� �࣠����樨 ��� ������ (᮪�饭���) " SKIP
   "������������ 䨫����, ��� ������������"
   " � (���) ����� ��� (�� ����稨) ���� ��� ����������騥 " SKIP
   "            �ਧ���� ��� (��"
   " ������⢨� ������������ � �����)  � 㪠������ �� ��� " SKIP
   "                       �ਭ���������� �।�⭮�"
   " �࣠����樨 (䨫����)" SKIP.

put skip(1).

put unformatted "                                               ������" skip.
put unformatted "                               ��� �ਭ���� �㬮� � ��஦��� �㬮�" skip.
put unformatted "             ����������������������������������������������������������������������Ŀ" skip
                "             �N �/�  �     �����     �   �㬬� �� �������� �   �  �ਬ�砭��      �" skip
                "             �       � �ਭ���� �㬮� �     �㬪�� (��ࠬ�)     �                  �" skip.
/* ��砫� 横�� "baul" */
do:
  c-number = 0.
  for each ttTemp where ttTemp.vNumM eq vTmpNumM
  break by ttTemp.vNumS: 
  
  c-baul = ttTemp.vNumS.
  sum-nac = ttTemp.vSum.
  c-number = c-number + 1 .
  prim = STRING(ttTemp.vDate,"99.99.9999").  
  
  accumulate ttTemp.vSum (total) ttTemp.vNumS (count).
  
  if last(ttTemp.vNumS) 
  then 
   do:
    sum-tot = accum total ttTemp.vSum.
  
    run amtgend.p (accum count ttTemp.vNumS, "�", output c-totsc, output ost).
    run amtstr2.p (sum-tot, "", output c-totst[1], output ost).
    c-totst[1] = string(sum-tot,"zzz,zzz,zz9.99") + " (" + c-totst[1] + " " + ost + "���.)" .
  
    SumBeg = substr(c-totst[1],1,R-Index(substr(c-totst[1],1,51),' ') - 1).
    c-totst[1] = substr(c-totst[1],length(SumBeg) + 1).
    {wordwrap.i &s=c-totst &n=2 &l=83}
   end.
put unformatted "             ����������������������������������������������������������������������Ĵ" skip.
put unformatted "             � " c-number Format ">>>9" "  � " c-baul Format "x(14)" " �   " sum-nac Format "zzz,zzz,zzz,zz9.99"
                "     �    " prim Format "x(10)" "    �" skip.
  end.
end.
/* ����� 横�� "baul" */

put unformatted "             ����������������������������������������������������������������������Ĵ" skip.
put unformatted "             ������  � "c-number Format ">>>9" "           �   " sum-tot Format "zzz,zzz,zzz,zz9.99"
                "     �                  �" skip.
put unformatted "             ������������������������������������������������������������������������" skip.
put unformatted FILL('-', 100) skip.
put unformatted "  �������஢���� �㬪� � �����묨 ���죠�� � ������⢥ " c-totsc Format "x(27)"
                "" skip.
put unformatted "                                                      (�ய����)" skip.
put unformatted "  ��� �� �������� �㬬� ������� �����: " SumBeg Format "x(54)"
                "" skip.
put unformatted "  " c-totst[1] Format "x(83)"
                "" skip.
put unformatted "  " c-totst[2] Format "x(83)"
                "" skip.
put unformatted "  ________________________________________________________________________________________________" skip.
put unformatted "                                                 (��ࠬ� � �ய����)" skip.
put unformatted "  �� �������� � �㬪�� � �����묨 ���죠��;" skip.
put unformatted "  ������ ���죨, �����⠭�� �� ���०������ �㬮� ___________________________________________" skip.
put unformatted "                                                            (��ࠬ� � �ய����)" skip.
put unformatted " " skip.
put unformatted "  ________________________________________________________________________________________________" skip.
put unformatted " " skip.
put unformatted "  �� ��������(������) � " vTmpNumM Format "x(10)"
                " �ਭ��� �� ��������᪨� ࠡ�⭨���:" skip.
put unformatted "  " /*incass*/ Format "x(70)"
                "" skip.
put unformatted "  ������������������������������������������������������������������������������������������������" skip.
put unformatted "  �஬� ⮣�, �� ��������᪨� ࠡ�⭨��� �ਭ�� ��஦��� �㬮� " /*c-totec Format "x(30)"*/
                "" skip.
put unformatted "  �� ����ࠬ� " /*c-empty Format "x(83)"*/
                "" skip.
put unformatted " " skip.
put unformatted FILL('-', 100) skip.
put unformatted "  �������஢���� �㬪� � �����묨 ���죠�� � ������⢥ __________________________________ ���." skip.
put unformatted "                                                                    (�ய����)" skip.
put unformatted "  �� �������� �㬬� ������� ����� ____________________________________________________________" skip.
put unformatted "                                                              (��ࠬ� � �ய����)" skip.
put unformatted "  ____________________________________________________ �� �������� � �㬪�� � �����묨 ���죠��;" skip(1).
put unformatted "  ������ ���죨, �����⠭�� �� ���०������ �㬮� ___________________________________________" skip.
put unformatted "                                                                  (��ࠬ� � �ய����)" skip.
put unformatted "  ________________________________________________________________________________________________" skip(1).
put unformatted "  �ਭ��� �� �।�⠢�⥫� �࣠����樨 ___________________________________________________________" skip.
put unformatted "                                        (������������ ��-樨, �.�.�. �।�⠢�⥫�)" skip.
put unformatted "  ________________________________________________________________________________________________" skip(1).
put unformatted "  ������⢮ � ����� ᤠ���� ��������᪨�� ࠡ�⭨���� �㬮� � �����묨 ���죠�� ᮮ⢥�����" skip.
put unformatted "  �������� � ����ࠬ �� ������ � ��� ����窠� � ��������� � �㬪�� � �����묨 ���죠��." skip.
/*put unformatted "  ������⢮ � ����� ᤠ���� �࣠����樥� �㬮� � �����묨 ���죠�� ᮮ⢥������ �������� �" skip.
put unformatted "  � ����ࠬ �� ������ � ��������� � ���⠭��� � �㬪�� � �����묨 ���죠�� (���㦭�� ���ભ���)" skip.*/
put skip(1).
put unformatted "      ���訩 �����                                         �����୮�� �.�." skip.                               
put unformatted "  ______________________________ ______________________  __________________________" skip.
put unformatted "     (������������ ��������)       (��筠� �������)        (䠬���� � ���樠��)" skip.
put skip(1).
put unformatted "  ����� �㬪� � �।�⭮� �࣠����樨:" skip.
put skip(1).
put unformatted "      ��������" skip.
put unformatted "  ______________________________ ______________________  __________________________" skip.
put unformatted "    (������������ ��������)       (��筠� �������)        (䠬���� � ���樠��)" skip.
put skip(1).
put unformatted "  �ਭ﫨 �㬪� � �।�⭮� �࣠����樨:" skip.
put skip(1).
put unformatted "      ���訩 �����                                         �����୮�� �.�." skip.
put unformatted "  ______________________________ ______________________  __________________________" skip.
put unformatted "     (������������ ��������)       (��筠� �������)        (䠬���� � ���樠��)" skip.
put skip(1).
put unformatted "  ��� " in-date 
                "  " tnow Format "x(30)"
                "" skip.
put skip(1).
put unformatted "  ���ࠧ������� ������樨:" skip(1).
put unformatted "      ��������" skip.
put unformatted "  ______________________________ ______________________  __________________________" skip.
put unformatted "     (������������ ��������)       (��筠� �������)        (䠬���� � ���樠��)" skip.
put unformatted " " skip.
put unformatted "  ��� " in-date 
                "" skip.

/* ������ ����⢨� */
END.


{endout3.i &nofooter=yes}
/* $LINTUSER='STRE' */
/* $LINTENV ='dpl' */
/* $LINTVSS ='$/ws3-dpl/bq' */
/* $LINTDATE='22/10/2014 12:08:18.686+04:00' */
/* $LINTFILE='incjour-318p.p' */
/*prosignS4sU6FF3uBUXUq78uIXfJA*/