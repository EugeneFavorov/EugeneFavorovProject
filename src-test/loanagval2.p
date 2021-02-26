/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2009 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: loanagval.p
      Comment: �����⢫�� ������ ࠧ��� 蠡���� ����, 
               ����᪠�� ��楤���, 㪠������ � 蠡���� � ��ࠬ��஬,
               ����祭��� ���祭�� �����뢠�� �� �६����� ⠡����.
               ����⠥� ⮫쪮 �� �ଠ�� ���_��楤���(��ࠬ����).
               �� �ॡ�� ���ᠭ�� ��६�����.
   Parameters:
         Uses:
      Used by: precrdprint.p
      Created: 14.10.2009 09:09 Jadv    
     Modified:
*/

{globals.i}
{intrface.get xclass}
{prn-doc.def &with_proc=YES}
{norm.i}

DEF INPUT PARAM iProc  AS CHAR NO-UNDO.   /* ��� ��楤���, ������� ���⮢ */
DEF INPUT-OUTPUT PARAM TABLE FOR ttnames. /* ������ ��� ����� १���⮢ */

DEF NEW SHARED VAR rid_loan  AS RECID.
DEF SHARED VAR rid-p     AS RECID NO-UNDO.

rid_loan = rid-p.

DEF VAR mSupportedProc   AS CHAR  NO-UNDO. /* ���᮪ ��ࠡ��뢠���� ��楤�� */
DEF VAR mTag             AS CHAR  NO-UNDO. /* �㭪�� */
DEF VAR mBraceBeg        AS INT64 NO-UNDO. /* ������ ��砫� ᪮��� */
DEF VAR mBraceEnd        AS INT64 NO-UNDO. /* ������ ����砭�� ᪮��� */
DEF VAR sTermRecidZam    AS INT64 NO-UNDO. /* ��� �࠭���� RECID(term-obl) �� ������ �।��� ������*/
DEF VAR mProcName        AS CHAR  NO-UNDO. /* ������������ ��楤��� */
DEF VAR mProcParm        AS CHAR  NO-UNDO. /* ��ࠬ���� ��楤��� */
DEF VAR mRetValue        AS CHAR  NO-UNDO. /* �����頥��� ���祭�� */

DEF  NEW GLOBAL SHARED VAR sTermRecid AS RECID NO-UNDO. /* ��� ���४⭮�� ���᪠ ����ᯥ祭�� � lgarterm.p */
DEF  NEW GLOBAL SHARED VAR sStr       AS CHAR  NO-UNDO. /* ��� ���४⭮� ࠡ��� loanagval �� ������ ������ */

DEFINE TEMP-TABLE tt NO-UNDO
   FIELD NomDog   AS CHAR
   FIELD CodeVal  AS CHAR
   FIELD NomPP    AS INT
   FIELD ChVal    AS CHAR
   FIELD term-obl-id AS RECID.
   
DEFINE QUERY q_term FOR tt.
DEFINE BROWSE b_term QUERY q_term NO-LOCK 
DISPLAY
   tt.NomPP   COLUMN-LABEL "#"               FORMAT 99
   tt.NomDog  COLUMN-LABEL "����� ��������"  FORMAT "x(20)" 
   tt.CodeVal COLUMN-LABEL "���"             FORMAT "x(45)"
/*   tt.ChVal   COLUMN-LABEL "�����������"     FORMAT "x(45)" */
   WITH 5 DOWN WIDTH 73 TITLE "".

DEFINE FRAME f_term 
   b_term SKIP 
   WITH 1 COLUMN SIDE-LABELS ROW 5 CENTERED OVERLAY NO-BOX.

   /* ���᮪ �����ন������ ��楤�� */
mSupportedProc = "bankinfo,userinfo,dog,dog2,lgarterm,lcond,loanform,param,dps_prn".
RetString = YES.

sTermRecid = ?. /* sku */

/*RUN dbgprint.p (PROGRAM-NAME(1) + " line = {&line-number}",*/
/*   "iProc = " + iProc).                                    */

   /* �஢��塞 ���� �� ⠪�� ��楤�� � ������� ���⮢ */
FIND FIRST user-proc WHERE
           user-proc.procedure EQ ENTRY(1,iProc,"|")
NO-LOCK NO-ERROR.
IF AVAIL user-proc THEN
DO:
   
/*   RUN dbgprint.p (PROGRAM-NAME(1) + " line = {&line-number}",*/
/*      "AVAIL(user-proc) = " + STRING(AVAIL(user-proc))).      */

   /* ���� �� 蠡����� ��楤��� */
   FOR EACH reports WHERE 
            reports.name EQ ENTRY(1,iProc,"|")
   NO-LOCK:
         /* ��ࠡ��뢠�� ⮫쪮 蠡���� ᮤ�ঠ騥 <#���#> */
      
      IF NUM-ENTRIES(reports.txt, "#") GE 2 THEN
      DO:
            /* �뤥�塞 ��楤��� � ��।��塞 ����樨 ᪮��� */
         ASSIGN
            mTag      = ENTRY(2, reports.txt, "#")
            mBraceBeg = INDEX(mTag, "(") 
            mBraceEnd = INDEX(mTag, ")")
         . 
         
/*         RUN dbgprint.p (PROGRAM-NAME(1) + " line = {&line-number}",*/
/*            "mTag = " + mTag).                                      */
         
         /* �᫨ ᪮��� ��������, � ��ࠡ��뢠�� ����� */
         IF (mBraceBeg + mBraceEnd) GT 0 THEN
         DO:
            /* ����稬 ��� ��楤��� � ��ࠬ��� */
            ASSIGN 
               mProcName = SUBSTR(mTag,             1, mBraceBeg - 1)
               mProcParm = SUBSTR(mTag, mBraceBeg + 1, mBraceEnd - mBraceBeg - 1)
            .
            
            /* �᫨ ��楤�� ���� � ᯨ᪥ �����ন������ */
            IF CAN-DO(mSupportedProc, mProcName) THEN
            DO:
                  /* �� ����᪠�� �� �� ��ࠡ���, �஢�ઠ �� ����⢮����� ��楤��� �� �㦭�,
                  ** �.�. � ᯨ᪥ �����ন������ 㪠�뢠���� ॠ��� ��楤���  */
               RUN VALUE(mProcName + ".p") (OUTPUT mRetValue,
                                            gbeg-date, 
                                            gend-date,
                                            mProcParm).
                  /* ����稬 १���� ��ࠡ�⪨ */
               ASSIGN
                  mRetValue = IF {assigned RETURN-VALUE} THEN RETURN-VALUE ELSE printtext
                  printtext = "" /* ���� ���㫨�� */
               .
               
/*               RUN dbgprint.p (PROGRAM-NAME(1) + " line = {&line-number}",  */
/*                  "mProcName = " + mProcName + " mRetValue = " + mRetValue).*/
               
                  /* ��襬 �� �६����� ⠡���� */
               RUN Insert_TTName(mTag,
                                 IF mRetValue BEGINS "[table" THEN mRetValue
                                                               ELSE REPLACE(mRetValue, "~n", " ")).

            END.
         END.
      END.
   END.  /* FOR EACH reports */
END.  /* AVAIL user-proc */

{intrface.del}
