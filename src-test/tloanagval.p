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

DEF SHARED VAR rid_loan  AS RECID.
DEF SHARED VAR rid-p     AS RECID NO-UNDO.

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

message "5" view-as alert-box.

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
mSupportedProc = "bankinfo,userinfo,dog,lgarterm,lcond,loanform,param,dps_prn".
RetString = YES.

sTermRecid = ?. /* sku */

   /* �஢��塞 ���� �� ⠪�� ��楤�� � ������� ���⮢ */
FIND FIRST user-proc WHERE
           user-proc.procedure EQ ENTRY(1,iProc,"|")
NO-LOCK NO-ERROR.
IF AVAIL user-proc THEN
DO:
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
            /* �᫨ ᪮��� ��������, � ��ࠡ��뢠�� ����� */
         IF (mBraceBeg + mBraceEnd) GT 0 THEN
         DO:
               /* ����稬 ��� ��楤��� � ��ࠬ��� */
            ASSIGN 
               mProcName = SUBSTR(mTag,             1, mBraceBeg - 1)
               mProcParm = SUBSTR(mTag, mBraceBeg + 1, mBraceEnd - mBraceBeg - 1)
            .
               /* �᫨ ��楤�� ���� � ᯨ᪥ �����ন������ */

message "6" CAN-DO(mSupportedProc, mProcName)  mProcName view-as alert-box.
            IF CAN-DO(mSupportedProc, mProcName) THEN
            DO:
message "7" view-as alert-box.
               IF mProcName EQ "lgarterm" THEN
               DO:
                  /* sku �롮� ������� ���ᯥ祭�� */
message "8" view-as alert-box.
                  FIND FIRST loan WHERE
                       RECID(loan) EQ rid-p
                  NO-LOCK.
                  IF sTermRecid = ? THEN
                  DO:
                     FOR EACH term-obl WHERE term-obl.contract EQ loan.contract
                                        AND term-obl.cont-code EQ loan.cont-code
                                        AND term-obl.idnt EQ 5
                                        NO-LOCK:
                        CREATE tt.
                        ASSIGN
                          tt.NomDog  = GetXattrValueEx("term-obl", 
                                                  STRING(term-obl.contract + "," + 
                                                         term-obl.cont-code + ",5," + 
                                                         STRING(term-obl.end-date,"99/99/99") + "," + 
                                                         STRING(term-obl.nn)
                                                         ), 
                                                  "��������", "*"
                                                  )
                          tt.NomPP   = term-obl.nn
                          tt.CodeVal = GetCodeName("�����", 
                                       GetXattrValueEx("term-obl", 
                                                                  STRING(term-obl.contract + "," + 
                                                                   term-obl.cont-code + ",5," + 
                                                                    STRING(term-obl.end-date,"99/99/99") + "," + 
                                                                 STRING(term-obl.nn)
                                                                 ), 
                                                        "�����", "*"
                                                        )
                                                )
                          tt.term-obl-id = recid(term-obl)
                         .
                         ACCUMULATE term-obl.nn (count).
                     END. 
                     /* �᫨ ���� ������� ���ᯥ祭��*/
                     IF (ACCUM count term-obl.nn) < 2 THEN
                     DO:
                        FIND FIRST tt NO-LOCK NO-ERROR.
                        sTermRecid = IF AVAIL tt THEN tt.term-obl-id ELSE ?.
                     END.
                     /*�롮� ������� ���ᯥ祭�� ⮫쪮 ��� 㪠������ ���⮢*/
                     IF sTermRecid = ? AND (ACCUM count term-obl.nn) > 1 THEN 
                     DO:
                        b_term:NUM-LOCKED-COLUMNS = 2.
                         b_term:TITLE = " [ " + DelFilFromLoan(loan.cont-code) + ", ����� �������� ����������� ] ".
                        OPEN QUERY q_term FOR EACH tt NO-LOCK BY tt.NomPP.
                        PAUSE 0.
                        VIEW b_term.
                        ENABLE ALL WITH FRAME f_term.
                        WAIT-FOR ENTER,ESC OF FRAME f_term.
                        HIDE FRAME f_term.
                        IF KEYFUNCTION(LASTKEY) NE "END-ERROR" THEN 
                        DO:
                           sTermRecid = tt.term-obl-id.
                           /* �᫨ 2 ������� ���ᯥ祭�� � ���⠥� ������ �� ������ ������, � ��।���塞 sTermRecidZam */
                           IF (ACCUM count term-obl.nn) EQ 2 
                             AND CAN-DO("zay_zam_ts|,zay_zamts2|,spis_zalog|",sStr)
                             AND sTermRecid NE ? THEN
                           DO:
                              FIND FIRST tt WHERE tt.term-obl-id NE sTermRecid 
                              NO-LOCK NO-ERROR.
                             sTermRecidZam = IF AVAIL tt THEN tt.term-obl-id ELSE ?.
                           END.
                        END.
                        ELSE 
                        DO:
                           sTermRecid = ?.
                        END.
                        /* ��।������ RECID(term-obl) �ᯮ������ ��� ���᪠ ��ࠬ��஢ �� ������ ������ */
                        IF sTermRecidZam = ? 
                           AND CAN-DO("zay_zam_ts|,zay_zamts2|,spis_zalog|",sStr) THEN
                        DO:
                           MESSAGE "�������� ������� ��� �������������" 
                           VIEW-AS ALERT-BOX TITLE "����".
                           b_term:NUM-LOCKED-COLUMNS = 2.
                           b_term:TITLE = " [ " + DelFilFromLoan(loan.cont-code) + ", ����� �������� ��� ������������� ] ".
                      
                           OPEN QUERY q_term FOR EACH tt NO-LOCK BY tt.NomPP.
                
                           PAUSE 0.
                           VIEW b_term.
             
                           ENABLE ALL WITH FRAME f_term.
                           WAIT-FOR ENTER,ESC OF FRAME f_term.
                           HIDE FRAME f_term.
                           IF KEYFUNCTION(LASTKEY) NE "END-ERROR" THEN 
                           DO:
                              sTermRecidZam = tt.term-obl-id.
                           END.
                        END.
                
                     END.
                 END.
                 /* �ᯮ������ ��� ���᪠ ��ࠬ��஢ �� ������ ������ */
                 IF (NUM-ENTRIES(mProcParm, "|") EQ 4 
                    AND ENTRY(4, mProcParm, "|") EQ "������") THEN 
                    mProcParm = mProcParm + ",###" + STRING(sTermRecidZam).
                 ELSE 
                    mProcParm = mProcParm + ",###" + STRING(sTermRecid).  /*RECID(term-obl)*/
                 END.
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
                 /* ��襬 �� �६����� ⠡���� */
                 RUN Insert_TTName(mTag,
                                 IF mRetValue BEGINS "[table" THEN mRetValue
                                                               ELSE REPLACE(mRetValue, "~n", " ")).

             END.
message "22" view-as alert-box.
          END.
message "23" view-as alert-box.
      END.
message "24" view-as alert-box.
   END.  /* FOR EACH reports */
END.  /* AVAIL user-proc */
{intrface.del}
