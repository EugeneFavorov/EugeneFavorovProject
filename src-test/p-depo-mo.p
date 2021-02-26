/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-1998 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: p-depo.p
      Comment: ���� �� �ᯮ������ ��壠���᪮� ����樨
      Comment:
   Parameters: Input: RID -  RecID(op)
         Uses: Globals.I ChkAcces.I SetDest.I PreView.I
      Used by:
      Created: 25/12/98 19:36:48 Lera
     Modified: 19.07.2006 13:48 OZMI     (0065266)
*/
Form "~n@(#) p-depo-k.p 1.0 Lera 25/12/98 Lera 25/12/98 "
     with frame sccs-id stream-io width 250.

{globals.i}
{chkacces.i}

&GLOBAL-DEFINE FILE_sword_p YES
{pp-uni.var}
&UNDEFINE FILE_sword_p
{pp-uni.prg}
{prn-doc.def &with_proc=YES}

/*DEFINE INPUT PARAMETER iParms AS CHARACTER NO-UNDO.*/

/*-------------------- �室�� ��ࠬ���� --------------------*/
Define Input Param RID as RecID no-undo.

/*-------------------- ������� ��६����� --------------------*/
Define Buffer buf_0_op For op.

/*--------------- ���� ��� ����� ��: ---------------*/

/*--------------- ��६���� ��� ᯥ樠���� �����: ---------------*/
Define Variable AcctCr           As Character            No-Undo.
Define Variable AcctDb           As Character            No-Undo.
/*Define Variable BankName         As Character            No-Undo.*/
Define Variable Dep              As Character            No-Undo.
Define Variable DepCr            As Character            No-Undo.
Define Variable reg-num          As Character            No-Undo.
Define Variable sum-nom          As Decimal              No-Undo.

DEFINE VARIABLE vDetails    AS CHARACTER EXTENT 10 NO-UNDO.
DEFINE VARIABLE vDep        AS CHARACTER EXTENT  2 NO-UNDO.
DEFINE VARIABLE vDepCr      AS CHARACTER EXTENT  2 NO-UNDO.
DEFINE VARIABLE vDetItem    AS INT64     NO-UNDO.
DEFINE VARIABLE vDetails2   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE qtybkv_1    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE qtybkv_2    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE qtybkv1     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE qtybkv2     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE qtybkv3     AS CHARACTER   NO-UNDO.

Def Var FH_p-depo-1 as INT64 init 5 no-undo. /* frm_1: ���. ��ப �� ���室� �� ����� ��࠭��� */


/* ��砫�� ����⢨� */
/*{wordwrap.def}*/
{get-fmt.i &obj=D-Acct-Fmt}

/*def var AcctName as char extent 2 no-undo.*/

/*-----------------------------------------
   �஢�ઠ ������ ����� ������� ⠡����, �� ������ 㪠�뢠�� Input Param RID
-------------------------------------------*/
Find op Where RecID(op) = RID no-lock no-error.
If Not Avail(op) then do:
  message "��� ����� <op>".
  Return.
end.

/*------------------------------------------------
   ���⠢��� buffers �� �����, �������� � ᮮ⢥��⢨� � ������묨 � ���� �ࠢ�����
------------------------------------------------*/
/* �.�. �� ������ �ࠢ��� ��� �롮ન ����ᥩ �� ������� ⠡����, ���� ���⠢�� ��� buffer �� input RecID  */
find buf_0_op where RecID(buf_0_op) = RecID(op) no-lock.

/* ���᫥��� ���祭�� ᯥ樠�쭮�� ���� BankName */
/*BankName = dept.name-bank.*/

/* ���᫥��� ���祭�� ᯥ樠�쭮�� ���� Dep */
find first op-entry of op no-lock.

find first acct where acct.acct EQ op-entry.acct-db no-lock.
vDep = acct.details.
{wordwrap.i &s=vDep &l=83 &n=2}
Dep = vDep[1] + vDep[2].

find first acct where acct.acct EQ op-entry.acct-cr no-lock.
vDepCr = acct.details.
{wordwrap.i &s=vDepCr &l=83 &n=2}
DepCr = vDepCr[1] + vDepCr[2].

    assign
      AcctDb = {out-fmt.i op-entry.acct-db Fmt}
      AcctCr = {out-fmt.i op-entry.acct-cr Fmt}
    .

/* ���᫥��� ���祭�� ᯥ樠�쭮�� ���� reg-num */
find first sec-code where sec-code.sec-code = op-entry.currency no-lock no-error.
if avail sec-code then reg-num = sec-code.reg-num.
                  else reg-num = "".

/*-------------------- ��ନ஢���� ���� --------------------*/
/*{strtout3.i &cols=106 &option=Paged}*/
find first _user where _user._userid = op-entry.user-id no-lock.
find first code where code.class = "�����" and code.code = op-entry.op-cod no-lock no-error.

put skip(7).
RUN Insert_TTName("DateDoc", string(op.doc-date, "99.99.9999")).
/*put unformatted "������������ ����� (����) �                                 " string(op.doc-date, "99.99.9999") " �." skip.*/
/*put skip(3).*/
RUN Insert_TTName("AcctDb", AcctDb).
/*put unformatted "����� - ��楢�� ���: " AcctDb skip.*/
RUN Insert_TTName("Dep", Dep).
put unformatted "��������: " dep skip.
/*put skip(2).*/
RUN Insert_TTName("AcctCr", AcctCr).
/*put unformatted "�।�� - ��楢�� ���: " AcctCr skip.*/
RUN Insert_TTName("DepCr", DepCr).
/*put unformatted "��������: " depCr skip.*/
/*put skip(2).*/
RUN amtgend.p (op-entry.qty, '�', OUTPUT qtybkv_1, OUTPUT qtybkv_2).

qtybkv_1 = SUBSTR(qtybkv_1, 1, (LENGTH(qtybkv_1) - 1)).
qtybkv1 = "(".
qtybkv2 = SUBSTR(qtybkv_1, 1, 1).
qtybkv3 = SUBSTR(qtybkv_1, 2, LENGTH(qtybkv_1)).

IF can-do('1', STRING(SUBSTR(STRING(op-entry.qty), LENGTH(STRING(op-entry.qty)), 1))) EQ TRUE THEN qtybkv3 = qtybkv3 + ") ��㪠.".
IF can-do('2,3,4', STRING(SUBSTR(STRING(op-entry.qty), LENGTH(STRING(op-entry.qty)), 1))) EQ TRUE THEN qtybkv3 = qtybkv3 + ") ��㪨.".
IF can-do('5,6,7,8,9,0', STRING(SUBSTR(STRING(op-entry.qty), LENGTH(STRING(op-entry.qty)), 1))) EQ TRUE THEN qtybkv3 = qtybkv3 + ") ���.".

/*RUN amtstr2.p (op-entry.qty, "", OUTPUT qtybkv_1, OUTPUT qtybkv_2).*/
RUN Insert_TTName("Qty", op-entry.qty).

RUN Insert_TTName("QtyBkv1", qtybkv1).
RUN Insert_TTName("QtyBkv2", qtybkv2).
RUN Insert_TTName("QtyBkv3", qtybkv3).

/*put unformatted "������⢮ ��: " op-entry.qty " (" qtybkv_1 ") ��.".*/
/*put skip(2).*/
RUN Insert_TTName("Details", buf_0_op.details).
/*put unformatted "�᭮����� ᮢ��蠥��� ����樨:" vDetails2 SKIP.*/
IF vDetails2 EQ "" THEN
DO:
    vDetails[1] = buf_0_op.details.
    {wordwrap.i &s=vDetails &l=83 &n=10}

    IF buf_0_op.details NE "" THEN
    DO vDetItem = 1 TO 10:
/*        PUT UNFORMATTED "" vDetails[vDetItem] SKIP.*/
        IF vDetails[vDetItem] EQ "" THEN
            LEAVE.
    END.
END.
/*put skip(2).*/
RUN Insert_TTName("DocNum", op.doc-num).
/*put unformatted "����� ����祭��, ���樨஢��襣� ������: " op.doc-num skip.*/
/*put unformatted "��� ����祭��, ���樨஢��襣� ������: " string(op.doc-date, "99.99.9999") " �." skip.*/
/*put skip(1).*/
/*put unformatted "�ਫ������: ����祭�� ���� �� __________ �����." skip.*/
/*put skip(2).*/
/*put unformatted "������: " skip.*/
/*put skip(2).*/
IF AVAIL _user 
   THEN RUN Insert_TTName("UserName", _user._user-name).
   ELSE RUN Insert_TTName("UserName", "").
/*put unformatted "���⠢�⥫�                                                  " (if avail _user then _user._user-name else "") skip.*/
/*put unformatted "              ________________________   ________________   ___________________" skip.*/
/*put unformatted "              (������������ ��������)   (��筠� �������)   (䠬����, ���樠��)" skip.*/
/*put unformatted "����" skip.*/
/*put unformatted "              ________________________   ________________   ___________________" skip.*/
/*put unformatted "              (������������ ��������)   (��筠� �������)   (䠬����, ���樠��)" skip.*/
/*put unformatted "����稫" skip.*/
/*put unformatted "              ________________________   ________________   ___________________" skip.*/
/*put unformatted "              (������������ ��������)   (��筠� �������)   (䠬����, ���樠��)" skip.*/

/*{endout3.i &nofooter=yes}*/

RUN printvd.p("p-depo-mo", INPUT TABLE ttnames).
