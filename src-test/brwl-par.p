/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2017 �� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: BRWL-PAR.P
      Comment: l-par(*).p C���ﭨ� �������
   Parameters: ���
         Uses:
      Used by:
      Created: 20.03.2007 09:34 Daru    
     Modified: 20.03.2007 09:34 Daru     <comment>
*/

{globals.i}             /* �������� ��६���� ��ᨨ. */
{flt-file.i}            /* ��।������ �������� �������᪮�� 䨫���. */
{intrface.get xclass}   /* ������⥪� �����㬥�⮢ ����奬�. */
{intrface.get lv}
{intrface.get loan}     /* �����㬥��� ��� ࠡ��� � ⠡��窮� loan. */
{intrface.get chwch}
{intrface.get db2l}     /* �����㬥�� ��� �������᪮� ࠡ��� � ��. */
{clcprmdog.i}

DEF VAR incontr   LIKE loan.contract      NO-UNDO. /* �����祭�� ������� */
DEF VAR incontc   LIKE loan.cont-code     NO-UNDO. /* ��� ������� */
DEF VAR name1     LIKE loan-par.NAME      NO-UNDO.
DEF VAR name2     LIKE loan-par.NAME      NO-UNDO.
DEF VAR name3     LIKE loan-par.NAME      NO-UNDO.
DEF VAR ssum1     LIKE loan-var.balance   NO-UNDO.
DEF VAR ssum2     LIKE loan-var.balance   NO-UNDO.
DEF VAR ssum3     LIKE loan-var.balance   NO-UNDO.
DEF VAR ssum      LIKE loan-var.balance   NO-UNDO.
DEF VAR ssumv     LIKE loan-var.balance   NO-UNDO.

DEF VAR priz            AS LOGICAL     NO-UNDO.
DEF VAR par-str         AS CHARACTER   NO-UNDO.
DEF VAR in-par          AS INT64     NO-UNDO. 
DEF VAR zagolovok       AS CHARACTER   NO-UNDO.
DEF VAR mFHelpLabel     AS LOGICAL     NO-UNDO. /* ��� ��।������ ���᪠��� */
DEF VAR mHelpLabel      AS CHARACTER   NO-UNDO. /* ���᪠��� */
DEF VAR i1              AS INT64     NO-UNDO.
DEF VAR ind             AS INT64     NO-UNDO.
DEF VAR unind           AS INT64     NO-UNDO.
DEF VAR cur-date        AS DATE        NO-UNDO.
DEF VAR id_             AS INT64     NO-UNDO.
DEF VAR mGroup          AS CHARACTER   NO-UNDO.
DEF VAR mSumSpis        AS DECIMAL     NO-UNDO.
DEF VAR mNameParam      AS CHARACTER   NO-UNDO.
DEF VAR mDogVidUch      AS CHARACTER   NO-UNDO. /* ���祭�� ����. ४����� ��������� */
DEF VAR mClassCode      AS CHARACTER   NO-UNDO. 
DEF VAR num-line        AS INT64     NO-UNDO. /* ����� �⮪� �࠭� */
DEF VAR dat-t1          AS DATE        NO-UNDO .
DEF VAR vSpisSokr       AS CHARACTER   NO-UNDO.
DEF VAR temp-summ       AS DEC         NO-UNDO.
DEF VAR n-form          AS INT64     NO-UNDO INIT 1.
DEF VAR vNachSumm       AS DECIMAL     NO-UNDO INIT ? .
DEF VAR fl-ras          AS LOGICAL             INIT YES.
DEF VAR dr              AS DATE FORMAT "99.99.99" LABEL "��� ����" NO-UNDO.

DEF VAR vRecid          AS CHARACTER   NO-UNDO.
DEF VAR vIn-Par         AS CHARACTER   NO-UNDO.
DEF VAR vIn-Brw         AS CHARACTER   NO-UNDO.
DEF VAR mProcStr        AS CHARACTER   NO-UNDO.

DEF BUFFER xlint     FOR loan-int.
DEF BUFFER xloan-var FOR loan-var.
DEF BUFFER xloan-par FOR loan-par.
DEF BUFFER yloan     FOR loan.

&GLOBAL-DEFINE advansed /* ��� �ᯮ�짮����� ���� sum-int */
&GLOBAL-DEFINE brwl-par

{lshpr.pro}     /* ������祭�� �����㬥�⮢ ��� ���� ��ࠬ��஢ ������� */
{par_mass.i}    /* ���ᨢ ��� ���� ��ࠬ��஢ ������� */
 /* �६����� ⠡��� ��� ���⮢ ��ࠬ��஢ ������� */
{loan_par.def
    &new = new
}
{svarloan.def}  /* ����७�� ��६���� ����� "�।��� � ��������". */
{brwl-par.frm} 
{brwl-par.qry}
{navigate.def}

ASSIGN 
   vRecid  = GetFltVal ("iRecid")
   vIn-Par = GetFltVal ("in-par")
   vIn-Brw = GetFltVal ("in-brw")
.   

/* kam */
def var newrights as char no-undo.

 FIND FIRST loan WHERE string(recid(loan)) = vRecid NO-LOCK NO-ERROR.
 if loan.user-id = 'servsouz' then do:
   newrights = GETXATTRVALUEEX('_user',USERID('bisquit'),'menu_access_f2',''). 
   IF USERID('bisquit') BEGINS 'i0400' THEN newrights = 'servsouz'.
   IF INDEX(newrights,"servsouz") = 0 THEN do:
	message "������� �த��\n ����� � ���ଠ樨 �� ������ ��࠭�祭�" view-as alert-box title "��������".
	return.
   end.	
 end.
/* end of kam */




/* ����ࠥ� ��ࠬ���� �⭮��騥�� � �������㠫�� �������, � ⠪��
** �易��� ��� ��ࠡ�⪨ ������� �����ᨩ */
NACH_COMM:
FOR EACH code WHERE code.class =  "��犮�" NO-LOCK:
   mProcStr = mProcStr + ":" + ENTRY(1,code.val).
   FOR EACH chowhe WHERE chowhe.id-d =  INT64(ENTRY(1,code.val))
                      OR chowhe.id-k =  INT64(ENTRY(1,code.val))
   NO-LOCK:
      IF   CAN-DO("5,?",STRING(chowhe.id-d))
        OR CAN-DO("5,?",STRING(chowhe.id-k)) THEN
         NEXT NACH_COMM.

      IF chowhe.id-d =  INT64(ENTRY(1,code.val)) THEN
         mProcStr = mProcStr + "," + STRING(chowhe.id-k).
      ELSE
         mProcStr = mProcStr + "," + STRING(chowhe.id-d).
   END.
END.
mProcStr = SUBSTRING(mProcStr,2,LENGTH(mProcStr)).

IF vIn-Brw <> "*"
   THEN ASSIGN
      n-frm  = INT64(vIn-Brw)
      n-form = n-frm
   .
{navigate.cqr
   &file       = work-var
   
   &workfile   = "/*"
   &filt       = YES
   &nodel      = "/*"
   &help-label = mHelpLabel
   
   &maxfrm = 4
   &bf1    = "work-var.name work-var.amt-id work-var.currency"
   &cf1    = "work-var.name work-var.amt-id work-var.currency work-var.balance work-var.bal-rub"
   &bf2    = "work-var.num1 mNameParam work-var.amt-id work-var.currency"
   &cf2    = "work-var.num1 mNameParam work-var.amt-id work-var.currency work-var.balance work-var.bal-rub"
   &bf3    = "mNameParam work-var.amt-id work-var.currency"
   &cf3    = "mNameParam work-var.amt-id work-var.currency work-var.balance work-var.bal-rub"
   &bf4    = "work-var.num1 work-var.currency work-var.balance"
   &cf4    = "work-var.num1 work-var.currency work-var.balance"
   
   &startup  = "l-par(l).str "
   &postfind = "l-par().fnd "
   &repaint  = "brwl-par.rpt "
   &look     = "floanpar.nav " 
   &oth4     = "l-par(l).ras "
   &oth5     = "l-par(l).par "
   &oth6     = "brwl-par.f6 "
   &oth7     = "l-par(l).sps "
   &oth8     = "l-par(l).edt "
   &print    = "l-par(n).prt "
   &befjoin  = "l-par(n).bfj "  
}
   
IF GetFltVal ("in-brw") <> "4"
   THEN parent-cont = "".

{intrface.del}          /* ���㧪� �����㬥����. */

PROCEDURE Select-Browse:
   DEF VAR vState AS CHAR   NO-UNDO. /* ��� ���ﭨ� �������. */
   IF NOT AVAIL loan THEN
      FIND FIRST loan WHERE loan.contract  =  incontr 
                        AND loan.cont-code =  incontc NO-LOCK NO-ERROR.

   ASSIGN
      dr       =  IF AVAIL loan  THEN loan.since   ELSE ?
      vState   =  IF dr =  ?     THEN "?"          ELSE STRING (dr)
   .
   CASE n-frm:
      WHEN 1   THEN 
         ASSIGN
            help-label           =  IF NOT CAN-DO("��蓼���,�ॡ����", loan.alt-contract)
                                       THEN "F1�F2 ���ᠭ���F3 ��ଠ�F5 ������F6 �����.�ᥳF7 �����F9 �����᪠"
                                       ELSE "F1�F3 ��ଠ �F5������F6 �����.�ᥳF7 �����"
            h-frm[n-frm]:TITLE   =  "[ ��������� �������� N "                    +
                                    GetContCode(loan.contract,loan.cont-code)    + 
                                    (IF loan.currency <> ""
                                       THEN "/" + STRING(loan.currency)
                                       ELSE "")                                  +
                                    " �� " + STRING(vState) +  " ]".
      WHEN 2   THEN  h-frm[n-frm]:TITLE = "[ ����� ��������� (������) N "  +
                        GetContCode(incontr,incontc)                 +
                        " �� " + string(vState) + " ]".
      WHEN 3   THEN  h-frm[n-frm]:TITLE = "[ ����� ��������� N "  + 
                        GetContCode(incontr,incontc)                 +  
                        " �� " + string(vState) + " ]".
      WHEN 4   THEN  
      DO:
         ASSIGN
            help-label = "F1"
            h-frm[n-frm]:TITLE = "[ " + STRING(zagolovok, "x(20)") + " ]".
      END.
   END CASE. 
   RETURN.
END PROCEDURE.
/* $LINTFILE='brwl-par.p' */
/* $LINTMODE='11,-9,6,3' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='soav' */
/* $LINTDATE='07/03/2017 11:27:36.001+03:00' */
/*prosignbHBoG0co1hfj2brQOJQ6+A*/