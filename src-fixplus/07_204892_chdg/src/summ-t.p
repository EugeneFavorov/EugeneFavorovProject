/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2014 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: summ-t.p
      Comment: ����� ���⪠ ������������

   Parameters: ���
         Uses:
      Used by:
      Created: ??/??/???? ???
      Last change:  SXS  25 Mar 2002    4:59 pm
     Modified:  11/12/02 Lera ��易⥫��⢠ ������ �����뢠���� �ᥣ�� �⭮�⥫쭮 ���� ����,
                              ���� �᫨ ������� ������. �� �����⨨ ������� ���⠢�����
                              ��� ������� ��易⥫��⢠ � ⮣�� ��� ������� �� ���뢠����
                              ���� �᫨ ���� ��ந��� �� ���� ����� ������� �� �� ������.
     Modified: 15/03/2004 ����  - �� ���� �㬬� �� �����᭥��� ��
                                   ������ ������� ���뢠�� ⮫쪮 ��
                                   ��襤襥 ����襭�� ��易⥫��⢠, �.� ��
                                   �� ��襫 ��⮬���᪨� �뭮� �� �����᭥���
     Modified: 19/03/2004 ����  - �� ���� �맮�� ���� ࠧ�ࢠ �१ olap,
                                   �᫨ ������� ������ (�㪠�� � ���� ������ ���
                                   ��  F8 � ᯨ᪥) � ��� ������� �����
                                   ���� ���� , � ����뢠�� ��ࠬ����
                                   ��� ࠧ��᪨ � ��⠥� �� ��易⥫��⢠
                                   ������� ��⮩ ������� ������� ����
                                   ������ (��� ���� ����).
     Modified: 14/03/2004 ����  - (19283) �� ���� �㯠� ����窠 ��� ���⭮��
     Modified: 26/04/2004 ����  - (22988) ����� ���� �㯠� ����窠 ��� ���⭮��,
                                   ��� �� ����窨 19.03.04 ���������
     Modified: 23.04.2008 jadv - (75633) ���ꥬ �� ᯥ梥�ᨨ ��� 57948
     Modified: 23.04.2008 ches - (0161696) ! ᪮�४�஢��� ࠡ�� � ������묨 ��易⥫��⢠�� (0161969 -date).
                                            ��ࠫ� ���� � GetSysConf("���뢠�쏮��襭�易⥫���")
                     �롮ઠ
                      (xerm-obl.sop-date  = ?
                       OR xerm-obl.sop-date >= dat-otch
                       OR (mOlapClose AND xerm-obl.sop-date = loan.close-date))
                       ⥯��� ����砥�:  ���������� ��� �� ���� ���� � ������� � ���� ������� �������.


*/

{globals.i}
{intrface.get loan}
{intrface.get xclass}

DEF OUTPUT PARAM summ-t  LIKE term-obl.amt-rub   NO-UNDO.
DEF INPUT  PARAM incontr LIKE loan.contract  NO-UNDO.
DEF INPUT  PARAM in-code LIKE loan.cont-code NO-UNDO.
DEF INPUT  PARAM l         AS RECID          NO-UNDO.    /* ��易⥫��⢮ */
DEF INPUT  PARAM dat-otch  AS DATE           NO-UNDO.    /* ��� ���� */

DEF VAR i1          AS INT64              INIT 0 NO-UNDO.
DEF VAR j1          AS INT64              INIT 0 NO-UNDO.
DEF VAR mParamOst LIKE loan-var.balance INIT 0 NO-UNDO.
DEF VAR mTermOst  LIKE loan-var.balance INIT 0 NO-UNDO.
DEF VAR mDiffOst  LIKE loan-var.balance INIT 0 NO-UNDO.

DEF VAR dat-r      AS DATE NO-UNDO .
DEF VAR mDb        AS DEC  NO-UNDO.
DEF VAR mCr        AS DEC  NO-UNDO.
DEF VAR mOst0      AS DEC  NO-UNDO.
DEF VAR mOst7      AS DEC  NO-UNDO.
DEF VAR mOst13     AS DEC  NO-UNDO.
DEF VAR mOst47     AS DEC  NO-UNDO.
DEF VAR mOlapClose AS LOG  NO-UNDO.
DEF VAR mSummDate  AS DATE NO-UNDO.
DEF VAR modeOfCalc AS LOG NO-UNDO.
DEF VAR mModeStr   AS CHAR NO-UNDO.
DEF VAR mIdnt LIKE term-obl.idnt NO-UNDO.

DEF BUFFER xerm-obl    FOR term-obl.
DEF BUFFER tt-term-obl FOR term-obl.


FIND FIRST term-obl WHERE RECID(term-obl) EQ l NO-LOCK NO-ERROR.

mIdnt = term-obl.idnt.

FIND FIRST loan WHERE loan.contract    = incontr AND
                      loan.cont-code   = in-code NO-LOCK NO-ERROR.

/*{{{ ����� ���� - �/��� ����窨 �����⥫��� ���⥦�� �� �᭮����� �����  */
mModeStr = GetSysConf ("������������⥦�").
IF mModeStr EQ ? THEN
   mModeStr = getXAttrValueEx("loan",
                              loan.contract + "," + loan.cont-code, 
                              "������������⥦�", ?).
IF mModeStr EQ ? THEN
   mModeStr = GetXattrInit (loan.class-code, "������������⥦�").

ASSIGN
   modeOfCalc = CAN-DO("��,yes,true,ok", mModeStr).
   mOlapClose =      GetSysConf("������멄������") = "��" 
                AND  loan.close-date <> ?
.

IF term-obl.sop-date <> ?        AND
   term-obl.sop-date <  dat-otch AND
   NOT mOlapClose                THEN
   summ-t = 0.
ELSE
DO:

   RUN RE_PARAM_EX IN h_Loan (0,
                              dat-otch,
                              loan.since,
                              incontr,
                              in-code,
                              OUTPUT mOst0,
                              OUTPUT mDb,
                              OUTPUT mCr ).


   RUN RE_PARAM_EX IN h_Loan (7,
                              dat-otch,
                              loan.since,
                              incontr,
                              in-code,
                              OUTPUT mOst7,
                              OUTPUT mDb,
                              OUTPUT mCr).

   IF GetSysConf("����⇠���������⨏��ப��") = "��" THEN
      FOR EACH loan-int OF loan WHERE
               loan-int.id-d = 7
           AND loan-int.id-k = 0
           AND loan-int.mdate = dat-otch
      NO-LOCK:
         mOst7 = mOst7 - loan-int.amt-rub.
      END.

   RUN RE_PARAM_EX IN h_Loan (13,
                              dat-otch,
                              loan.since,
                              incontr,
                              in-code,
                              OUTPUT mOst13,
                              OUTPUT mDb,
                              OUTPUT mCr).

   RUN RE_PARAM_EX IN h_Loan (47,
                              dat-otch,
                              loan.since,
                              incontr,
                              in-code,
                              OUTPUT mOst47,
                              OUTPUT mDb,
                              OUTPUT mCr).
   mParamOst = mOst0 + mOst7 + mOst13 + mOst47.

   IF mParamOst > 0 THEN
   DO:
      FOR EACH xerm-obl WHERE
               xerm-obl.contract  = loan.contract
           AND xerm-obl.cont-code = loan.cont-code
           AND xerm-obl.idnt      = mIdnt
      NO-LOCK :
         IF xerm-obl.sop-date  = ? OR
            xerm-obl.sop-date >= dat-otch OR
            (mOlapClose AND xerm-obl.sop-date = loan.close-date)
         THEN
            ACCUM xerm-obl.amt-rub (TOTAL).
      END.

      mTermOst =  ACCUM TOTAL xerm-obl.amt-rub .

      RELEASE tt-term-obl.

      /* ��� ��� �뫠 ��⪠ - �� �� ��१���  */

         mDiffOst = mTermOst - mParamOst.
         
          /* ०�� � ����窮� ���⥦� */
         IF modeOfCalc THEN DO:
             FOR EACH tt-term-obl WHERE
                      tt-term-obl.contract  = loan.contract
                 AND  tt-term-obl.cont-code = loan.cont-code
                 AND  tt-term-obl.idnt      = mIdnt
                 AND (tt-term-obl.sop-date  = ?
                   OR tt-term-obl.sop-date >= dat-otch
                   OR (mOlapClose AND tt-term-obl.sop-date = loan.close-date))
        
                NO-LOCK
                BREAK BY tt-term-obl.end-date WHILE mDiffOst > 0:
        
                IF RECID(tt-term-obl) = RECID(term-obl) THEN LEAVE.
        
                mDiffOst = mDiffOst - tt-term-obl.amt-rub.
             END.
            
         END.
         /* ०�� ��� ����窨 ���⥦� */
         ELSE DO:
             /* ���⠥� ������� ���⥦� �� �㬬� ������ �� ���� ���� 
             ** ��� �� �������� ���� ���⥦� (�᫨ ��� ࠭��) */
             FOR EACH tt-term-obl WHERE
                      tt-term-obl.contract  = loan.contract
                 AND  tt-term-obl.cont-code = loan.cont-code
                 AND  tt-term-obl.idnt      = mIdnt
                 AND  tt-term-obl.end-date <= dat-otch
                 AND (tt-term-obl.sop-date  = ?
                   OR tt-term-obl.sop-date >= dat-otch
                   OR (mOlapClose AND tt-term-obl.sop-date = loan.close-date))
        
                NO-LOCK
                BREAK BY tt-term-obl.end-date WHILE mDiffOst > 0:
        
                IF RECID(tt-term-obl) = RECID(term-obl) THEN LEAVE.
        
                mDiffOst = mDiffOst - tt-term-obl.amt-rub.
             END.
             
            /* �᫨ ���� ��������� ���⥦� ����� ���� ���� � �� ��⠫��� 
            ** ��-� �� ������ */
            IF mDiffOst > 0 AND
               term-obl.end-date > dat-otch 
            THEN DO:
                /* ���⠥� � ���� ��䨪� ������� ���⥦� �� �������� ���� */
                FOR EACH  tt-term-obl WHERE
                          tt-term-obl.contract  = loan.contract
                     AND  tt-term-obl.cont-code = loan.cont-code
                     AND  tt-term-obl.idnt      = mIdnt
                     AND  tt-term-obl.end-date > dat-otch
                     AND (tt-term-obl.sop-date  = ?
                       OR tt-term-obl.sop-date >= dat-otch
                       OR (mOlapClose AND tt-term-obl.sop-date = loan.close-date))
            
                    NO-LOCK
                    BY tt-term-obl.end-date DESC:
            
                    IF RECID(tt-term-obl) = RECID(term-obl) THEN LEAVE.

                    mDiffOst = mDiffOst - tt-term-obl.amt-rub.
                    
                END.
            END.                        
         END.

         IF mDiffOst <= 0 THEN
                summ-t = term-obl.amt-rub.
             ELSE
                summ-t = IF mDiffOst - term-obl.amt-rub >= 0 THEN
                            0
                         ELSE
                            term-obl.amt-rub - mDiffOst.
         
   END. /*if e1 > 0 then do:*/
   ELSE
      summ-t = 0.
END.

{intrface.del}
/* $LINTUSER='BIS' */
/* $LINTENV ='common' */
/* $LINTVSS ='$/ws3-dpl/common/bq/' */
/* $LINTDATE='05/12/2014 16:53:32.569+04:00' */
/* $LINTFILE='summ-t.p' */
/*prosignxPVN7qhxWXXUeWsPKKET0w*/