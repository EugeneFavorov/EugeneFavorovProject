/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2005 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: SCH-TRAN.P
      Comment: ���室��� ��� ����᪠ 㭨���ᠫ쭮� �࠭���樨 �� schedule
   Parameters:
         Uses:
      Used by:
      Created: 21.12.2005 ILVI (44294)  
     Modified: 19.07.2010 16:15 ksv      (0129302) ��������� ᡮઠ ����,
                                         ���������� ��᫥ �믮������ g-trans.p
*/

DEFINE NEW GLOBAL SHARED VARIABLE h_cache AS HANDLE     NO-UNDO.
RUN pp-cache.p PERSIST SET h_cache.

{globals.i}                 /* �������� ��६���� ��ᨨ. */
{intrface.get tmess}    /* �����㬥��� ��ࠡ�⪨ ᮮ�饭��. */
{intrface.get crd}
{intrface.get pbase} 
{intrface.get date}
{intrface.get trans}
{intrface.get acct}
{intrface.get tmess}
{intrface.get instrum}
{intrface.get trans}
{intrface.get tparam}
{intrface.get rights}
{intrface.get data}
{intrface.get db2l}
{intrface.get count}
{intrface.get strng}
{intrface.get brnch}
{intrface.get refer}
{intrface.get cust}
{intrface.get osyst}
{intrface.get print}
{intrface.get prnvd}
{intrface.get kau}
{intrface.get parsr}
{intrface.get prsfn}
{intrface.get oldpr}
{intrface.get widg}
{intrface.get xclass}
{intrface.get rights}
{intrface.get osyst}
{intrface.get loan}
{intrface.get cdrep}
{intrface.get lv}
{intrface.get chwch}
{svarloan.def new global}   /* ��६���� ����� �।��� � ��������. */
{topkind.def}
{filial.pro}



DEFINE INPUT  PARAMETER iOpKind AS CHARACTER  NO-UNDO.

DEF VAR mOK      AS LOG   NO-UNDO.
DEF VAR mFil     AS CHAR  NO-UNDO.
DEF VAR mMessage AS CHAR  NO-UNDO.
DEF VAR mLastOpDate AS DATE NO-UNDO.




IF num-entries(iOpKind) GE 2 THEN
DO:
   IF num-entries(iOpKind) GE 3 THEN
      mLastOpDate = DATE(FGetLastClsDate(?,'*')) + 1.
   ELSE
      mLastOpDate = gend-date.
   IF mLastOpDate LT DATE("17.03.2017") THEN mLastOpDate = DATE("17.03.2017").
   /* run dbgprint.p("sch-tran-fil.p",mLastOpDate). */
   ASSIGN
      mFil    = entry(2,iOpKind)
      iOpKind = entry(1,iOpKind).

   IF mFil = "0300" THEN SETUSERID("serv0300", "1234567", "BISQUIT").
   IF mFil = "0000" THEN SETUSERID("serv0000", "123", "BISQUIT").
   IF mFil = "0500" THEN SETUSERID("serv0500", "Zx1234567", "BISQUIT").

   RUN DelConnectLink.
   RUN SetConnectLink (mFil).
   RUN SetEnvironment (mFil). /* ���⥪�� ��࠭���� 䨫���� */
END.

{bqgc.def "NEW"}

/* ***************************  Main Block  ********************************* */
MAIN-BLOCK:
DO 
   ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
 
   /* ���������� ���-�� � ⥪. �������᪨� ��ꥪ�� */
   RUN bqgc.p( {&GC_SAVEOBJECTS} ).
   
   FIND FIRST op-kind WHERE
        op-kind.op-kind EQ iOpKind NO-LOCK NO-ERROR.
   IF NOT AVAIL op-kind THEN
      UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.


FOR EACH op-date WHERE op-date.op-date >= mLastOpDate
                   AND op-date.op-date <= gend-date NO-LOCK:
   {empty tOpKindParams} /* ������ ⠡���� ��ࠬ��஢ */
   RUN ex-trans.p(iOpKind,op-date.op-date,TABLE tOpKindParams, OUTPUT mOk, OUTPUT mMessage).

   IF NOT mOk THEN
      RUN Fill-SysMes IN h_tmess ("", "", "-1",mMessage).   
END.
  
   
END. /* MAIN-BLOCK: */

/* ����塞 �� ���. ��ꥪ��, �஬� ����⢮����� �� �맮�� ��楤��� */
 RUN bqgc.p( {&GC_DELOBJECTS} ).
/* ************************************************************************** */

IF shFilial NE "0000" THEN
DO:
   SETUSERID("serv0000", "123", "BISQUIT").
   RUN DelConnectLink.
   RUN SetConnectLink ("0000").
   RUN SetEnvironment ("0000"). /* ���⥪�� ��࠭���� 䨫���� */
END.