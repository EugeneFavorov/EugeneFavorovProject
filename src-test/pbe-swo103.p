/* 
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2017 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: e-swo103.p
      Comment: ������� SWIFT
   Parameters: rec-op        - ��뫪� �� op
               in-op-kind    - �࠭�����
               in-series-num - ᥠ�� 
         Uses:
      Used by:
      Created: 01.01.2001 mkv
     Modified: 
*/

&GLOBAL-DEFINE format-message swift

def input param rec-op as recid no-undo.
def input param in-op-kind like op-kind.op-kind.
def input param in-series-num as char no-undo.

DEFINE VARIABLE mBuf AS CHARACTER NO-UNDO. 

/* ������� ��६����� */
/* ������ ���� ����
{e-tel100.def} */
{pbe-tel100.def}
DEF VAR h_swi AS HANDLE NO-UNDO.
h_swi = h_swi_n.
/* ����� ������ ���� ���� */
def var out-ref as char no-undo.
/* ������� �६���. ⠡�. ᮤ��. ��뫪� ��� ���㬥�� */
{defexp.tab} 

/* �� ४������ ��襣� ����� */
{bank-id.i}

/* ����� ��室�� ⥪�� �� ��� */
{wordwrap.def}

/* �।�. ���᪨ � ��⠭���� */
{e-tel100.i &type-message=100}
/* ���樫����� */
assign
 end-file   = "-~}" 
 end-msg    = "-~}" 
 begin-msg  = "~{2:"
 begin-file = "~{2:"
 tmpcode   = ""
 tmptype   = "103"
 tmpformat = "swift".

/* �ନ஢���� 蠯�� */
{e-swift.hdr &frm-mess=tmptype}

/* �ନ஢���� ⥫� ᮮ�饭�� */

/* ���७� ����樨 */
find first acct where acct.acct eq op-entry.acct-cr no-lock no-error.
run getref.p (op.op, tmptype, acct.acct).


assign
  buf-work = if return-value ne ? then  return-value else ""
  out-ref = (if f-trans and CAN-DO("RUR5,RUR6",VerForm) then "+"
             else if f-trans then "'" else "") 
            + if return-value ne ? then  return-value else ""
  buf-work = ":20:" + out-ref
  buf-copy-doc = buf-copy-doc + "~n" + buf-work.

/* ��� �����, �����, �㬬� */
{e-tel100.13}
{e-tel100.23b}
{e-tel100.23e}
{e-tel100.26}

mTag32 = GetXattrValueEx("op", STRING(op.op), "sw-benef-amt", "" ).
/* ������ ���� ����
IF mMultiPulti AND */
IF
/* ����� ������ ���� ���� */
   mTag32 NE "" THEN DO:
   assign
    buf-work = ":32A:" +
      (if op.op-value-date = ? then (substr(string(op.op-date,"999999"),5,2) + substr(string(op.op-date,"999999"),3,2) + substr(string(op.op-date,"999999"),1,2) )
       else ({date-swf.i &date="op.op-value-date"}))
      + mTag32
    buf-copy-doc = buf-copy-doc + "~n" + buf-work.
END.
ELSE DO:
   assign
    buf-work = ":32A:" +
      (if op.op-value-date = ? then (substr(string(op.op-date,"999999"),5,2) + substr(string(op.op-date,"999999"),3,2) + substr(string(op.op-date,"999999"),1,2) )
       else ({date-swf.i &date="op.op-value-date"}))
      + curr-iso + buf-s
    buf-copy-doc = buf-copy-doc + "~n" + buf-work.
END.
IF NOT CAN-DO("RUR5,RUR6",VerForm) THEN
DO:
   {e-tel100.33}
END.
{e-tel100.36}

/* ������-��ࠢ�⥫� */
if not avail op-template then
find first op-template where op-template.op-kind eq in-op-kind no-lock no-error.
run e-sw50-103 in h_swi (input       recid(op),
                         input       recid(op-entry),
                         input       foreign,
                         input       f-trans,
                         VerForm +
                         (if avail op-template and avail mail-user
                          then (',' + mail-user.mail-format + "," +
                                in-op-kind + "," + STRING(op-templ.op-templ))
                          else ""),
                         tmptype,
                         input-output buf-copy-doc,
                         input-output op-err-50 ).

/* ����-��ࠢ�⥫� */
{e-tel100.52 send 100}

/* ����ᯮ�����-��ࠢ�⥫� */
{e-tel100.53}
/* ����ᯮ�����-�����⥫� */

/* ? */

if GetXAttrValue("op", string(op.op), "Cover-Transfer") eq "��" then
do:
   {e-tel100.54}
end.
{e-tel100.55}
/* ����-���।��� */
{e-tel100.56}

/* ����-�����⥫� */
run e-sw57 in h_swi (recid(op),
                     foreign,
                     f-trans,
                     VerForm +
                        (if avail op-template and avail mail-user
                            then (',' + mail-user.mail-format + "," +
                                  in-op-kind + "," + STRING(op-templ.op-templ))
                         else ""),
                     tmptype,
                     input-output buf-copy-doc,
                     input-output op-err-50 ).
/* ������-�����⥫� */
run e-sw59-103 in h_swi (input          recid(op),
                         input       foreign,
                         input       f-trans,
                         VerForm +
                         (if avail op-template and avail mail-user
                          then (',' + mail-user.mail-format + "," +
                                in-op-kind + "," + STRING(op-templ.op-templ))
                          else ""),
                         tmptype,
                         input-output buf-copy-doc,
                         input-output op-err-50 ).

/* �����祭�� ���⥦� */
/* ��⠢�� ���� ���� */
detail[1] = TRIM(REPLACE(GetXAttrValue("op",STRING(op.op),"sw-details"),"~n"," ")).
/* ����� ��⠢�� ���� ���� */
IF mFormat-ISO EQ "��" THEN 
   detail[1] = SUBSTRING(detail[1],1,199).

detail[1] = word-wrap(detail[1],"35,35,35,35,26,33,33,33,33",yes).
if entry(1,detail[1],chr(1)) ne "" then do:
 buf-copy-doc = buf-copy-doc + "~n" + ":70:" + entry(1,detail[1],chr(1)).

   DO i = 2 TO 4:
      ASSIGN
         mBuf = entry(i,detail[1],chr(1))
      NO-ERROR.

      IF {assigned mBuf} THEN
         ASSIGN
            mBuf = IF mBuf BEGINS ":" 
                      THEN SUBSTRING(mBuf,2)
                      ELSE mBuf 
            buf-copy-doc = buf-copy-doc + "~n" + mBuf
      NO-ERROR.
   END.
end.

/* ��⠫� ��室�� */
buf-work = "".
{exp-read.sgn "swift-det-pay" buf-work}
assign
 buf-work  = entry(1, buf-work,"/")
 buf-work = if buf-work eq "" then "OUR" else buf-work
 buf-work = ":71A:" + buf-work
 buf-copy-doc = buf-copy-doc + "~n" + buf-work + "~n".

IF NOT CAN-DO("RUR5,RUR6",VerForm) THEN
DO:
   {e-tel100.71f}
   {e-tel100.71g}
END.
/*���ଠ�� ��ࠢ�⥫� �����⥫�*/
/* ��������� ���⥦���� ����祭�� */

{e-swift.72}
{e-tel100.77b}
/* ��ப� ����砭�� */
{e-tel100.cl &one=yes}
{e-swo.put}
return out-ref. 
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='08/10/2015 16:10:17.776+04:00' */
/* $LINTUSER='mike' */
/* $LINTMODE='1' */
/* $LINTFILE='e-swo103.p' */
/*prosignNMoGvK3IZFIu0Jl4PyAKWA*/