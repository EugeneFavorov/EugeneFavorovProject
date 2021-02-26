/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2017 �� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: BAGCHNG.P
      Comment: �ਢ離� ������஢ � ����.
   Parameters: tmprecid,०��
         Uses:
      Used by:
      Created: 30.06.2007 12:13 Om      
     Modified: 24.03.2009 20:15 KSV      (0108286) ��ࠢ���� �訡�� � �롮஬
                                         �� F1 � QBIS
     Modified: 30.03.2009 17:34 feok     <comment>
     Modified: 23.01.2011 15:44 Om        (0139888) ��⨬����� ����.  
*/

{globals.i}             /* �������� ��६���� ��ᨨ. */
{intrface.get tmess}    /* �����㬥��� ��ࠡ�⪨ ᮮ�饭��. */
{intrface.get bag}      /* ������⥪� ��� ࠡ�� � ���. */
{intrface.get i254}
{intrface.get xclass}   /* ������⥪� �����㬥�⮢ ����奬�. */
                        /* �����쭠� ⠡��� �⬥⮪. */
{tmprecid.def &NGSH =  "LOCAL"}

DEF INPUT   PARAM TABLE FOR tmprecid BIND.   /* ����砥� ⠡���� �� 㪠��⥫�. */
DEF INPUT   PARAM iMode AS CHAR NO-UNDO.     /* ����� ࠡ��� (POSAdd, POSRemove). */

DEF VAR mPos         AS CHAR     NO-UNDO.
DEF VAR mBegDate     AS DATE     NO-UNDO.
DEF VAR mResult      AS LOG      NO-UNDO. /* �㧥���� ᮧ����� POSa. */
DEF VAR mFl          AS LOG      NO-UNDO.

DEFINE BUFFER bBag   FOR loan.
DEFINE BUFFER tloan  FOR loan.

pick-value = ?.
CNHBG:
DO
ON ERROR    UNDO CNHBG, LEAVE CNHBG
ON ENDKEY   UNDO CNHBG, LEAVE CNHBG:
   {getdate.i
      &DispAfterDate =  "mPos FORMAT 'X(30)' LABEL '���' HELP '�롥�� ���'"
      &UpdAfterDate  =  "mPos WHEN  iMode EQ 'POSAdd'"
      &AddLookUp     =  "IF {&LAST_KEY} EQ KEY-CODE ('F1') AND {&FRAME_FIELD} EQ 'mPos'
                         THEN DO TRANSACTION:
                           RUN browseld.p ('UniformBag', 'open-date2', end-date, '', 4).
                           IF LASTKEY EQ 10
                              THEN DISPLAY pick-value @ mPos.
                         END.
                         ELSE "
      &AddPostUpd    =  "IF mPos NE '' THEN DO:
                           IF NOT CAN-FIND (FIRST loan WHERE
                                                   loan.contract  EQ '���'
                                             AND   loan.cont-code EQ mPos)
                           THEN DO:
                              RUN Fill-SysMes IN h_tmess (
                                  '', '', '1',
                                  '������ ������ ��� ����䥫�.'
                              ).
                              UNDO, RETRY.
                           END.
                           IF CAN-FIND (FIRST loan WHERE
                                              loan.parent-contract  EQ '���'
                                          AND loan.parent-cont-code EQ mPos)
                           THEN DO:
                              RUN Fill-SysMes IN h_tmess (
                                  '', '', '1',
                                  '����� �ਢ�뢠�� � ���-�� ���孥�� �஢��.'
                              ).
                              UNDO, RETRY.
                           END.
                         END.
                         ELSE DO:
                           IF iMode EQ 'POSAdd' THEN
                           DO:
                              RUN Fill-SysMes IN h_tmess (
                                 '', '', '1',
                                 '����室��� 㪧��� ���.'
                              ).                           
                              UNDO, RETRY.
                           END.
                           ELSE DO:
                              RUN Fill-SysMes IN h_tmess (
                                  '', '', '4',
                                  '�뢥�� ���� �� ����䥫��?'
                              ).
                              IF pick-value NE 'YES'
                                 THEN UNDO CNHBG, LEAVE CNHBG.
                           END.
                         END.
                        "
      &DateLabel     =  "��� �ਢ離�"
      &DateHelp      =  "������ ���� �ਢ離� � ���� (F1 - ���������)"
      &return        =  "UNDO CNHBG, LEAVE CNHBG"
   }

   mBegDate = end-date.
   IF mPos <> "" THEN
   FIND FIRST bBag WHERE bBag.contract  = "���"
                     AND bBag.cont-code = mPos
   NO-LOCK NO-ERROR.

   IF    AVAIL bBag 
     AND bBag.close-date   <= end-date THEN
   DO:
      RUN Fill-SysMes IN h_tmess ("","","0","��࠭ ���, ������� �� ���� " + QUOTER (bBag.close-date) + ".").
      UNDO CNHBG, LEAVE CNHBG.
   END.
   
                        /* ���樠������ ����� ��⮪���஢����. */
   RUN Init-SysMes IN h_tmess (
      "op-kind,��������",
      (IF   iMode =  "POSAdd"
       THEN  SUBSTITUTE("�ਢ離� ������஢ � ��� &1 �� ���� &2.",mPos,mBegDate)
       ELSE "�뢮� ������஢ �� ���."
      ), "").

   N_LOAN:
   FOR EACH tmprecid  NO-LOCK,
      FIRST loan WHERE RECID(loan) =  tmprecid.id
   NO-LOCK:
      IF loan.open-date >  mBegDate THEN
      DO:
         RUN Fill-SysMes IN h_tmess (
            "", "", "-1",
            "������ �ਢ離� ������� " + QUOTER (loan.cont-code) + " � ���: "         +
            "��� �ਢ離� � ����䥫� �� ����� ��室��� �� ࠬ�� ����⢨� ������� "   +
            QUOTER (STRING (loan.open-date, "99/99/9999")) + "." 
         ).
         NEXT N_LOAN.
     END.
     IF CAN-FIND(FIRST term-obl WHERE
                       term-obl.contract     =  loan.contract
                   AND term-obl.cont-code    =  loan.cont-code
                   AND term-obl.idnt         =  128
                   AND term-obl.end-date     =  mBegDate
                   AND term-obl.lnk-contract =  "���" 
                 NO-LOCK) 
     THEN DO:
        RUN Fill-SysMes IN h_tmess ('', '', '1','����� �뢥�� ������� �� ���� � ���� ����祭��. ������ ������ ������!').
        NEXT N_LOAN.
     END.

                        /* �᫨ �� "����᪫" ��⠭����� � "��" - �ᥣ�� �᪫���� ������� �� ����.
                        ** � ������� � ���祭 � ���. */
      IF       GetXAttrValue("loan",loan.contract + "," + loan.cont-code, "����᪫")   =  "��"                         
         AND   LnInBagOnDate(loan.contract, loan.cont-code, mBegDate)                  <> ?     THEN
      DO:
                        /* �᪫�砥� ������� �� ���. */
         RUN SetLinkPos(loan.contract, loan.cont-code, "", mBegDate, ?, YES, OUTPUT mFl).         
         RUN Fill-SysMes IN h_tmess (
            "", "", "-1",
            "�������� ������� " + QUOTER (loan.cont-code) + " �� ���. " +            
            " �� ~"����᪫~"(�᪫���� �� ���)~ ����� ���祭�� ~"��~"."
         ).
         NEXT N_LOAN.
         END.
      IF LENGTH (mPos) >  0   THEN
         DO:
/*                
           /* �஢�ઠ �� ����稥 ����権 �� �ॣ㫨஢���� १�ࢠ �� �������� */
         IF VerifyPOSDate(loan.contract, loan.cont-code, mBegDate)   <> NO THEN
            DO:
            RUN Fill-SysMes IN h_tmess (
               "","","-1",
               "������ �ਢ離� ������� " + QUOTER (loan.cont-code) + " � ���: "   +
               "�� ��諠 �஢�ઠ �� ����稥 ����権 �� �ॣ㫨஢���� १�ࢠ."
            ).
            NEXT N_LOAN.
            END.
*/
                           /* �஢�ઠ ���������� ����祭�� � ��� �࠭� */
         IF       NUM-ENTRIES(loan.cont-code, " ")                            =  2 
            AND   CanTranshToPOS ((BUFFER loan:HANDLE),  mPos, mBegDate, YES) <> YES   THEN
               DO:
            RUN Fill-SysMes IN h_tmess (
               "", "", "-1",
               "������ �ਢ離� ������� " + QUOTER (loan.cont-code) + " � ���: " +
               "�� ��諠 �஢�ઠ ���������� ����祭�� � ��� �࠭�."               
            ). 
            NEXT N_LOAN.
               END.
                           /* �஢�ઠ ����祭�� ���� � ��� �� �ਧ����� ����த����. */
            RUN ChkLoanUniformity IN h_bag (
               (BUFFER loan:HANDLE),
               (BUFFER bBag:HANDLE),
               mBegDate,
            YES,
               OUTPUT mResult
            ).
            IF NOT mResult THEN
            DO:
            RUN Fill-SysMes IN h_tmess (
               "", "", "-1",
               "������ �ਢ離� ������� " + QUOTER (loan.cont-code) + " � ���: "    +
               "�� ��諠 �஢�ઠ ����祭�� ���� � ��� �� �ਧ����� ����த����."
            ). 
            NEXT N_LOAN.
            END.
                           /* �஢�ઠ ᮮ⢥��⢨� �� �ப�� */
            RUN ChkDelayConformity IN h_bag (
               (BUFFER loan:HANDLE),
               (BUFFER bBag:HANDLE),
               mBegDate,
            YES,
               OUTPUT mResult
            ).
            IF NOT mResult THEN 
            DO:
            RUN Fill-SysMes IN h_tmess (
               "", "", "-1",
               "������ �ਢ離� ������� " + QUOTER (loan.cont-code) + " � ���: "   +
               "�� ��諠 �஢�ઠ ᮮ⢥��⢨� �� �ப��."
            ). 
            NEXT N_LOAN.
            END.
         END.
                           /* ��ନࢮ���� �裡. */ 
         RUN SetLinkPos IN h_bag (loan.contract, 
                                  loan.cont-code, 
                                  mPos, 
                                  mBegDate, 
                                  ?,
                                  YES, 
                                  OUTPUT mResult).
         IF NOT mResult THEN
         DO:
         RUN Fill-SysMes IN h_tmess (
            "", "", "-1",
            "������ �ਢ離� ������� " + QUOTER (loan.cont-code) + " � ���:  "  +
            "�� 㤠���� ��ନ஢��� ���."            
         ).
         NEXT N_LOAN.
         END.
         
      IF loan.cont-type =  "��祭��" THEN 
         FOR EACH tloan WHERE tloan.contract  =  loan.contract 
                          AND tloan.cont-code =  loan.cont-code
                          AND tloan.cont-code BEGINS loan.cont-code + " "
                          AND NUM-ENTRIES(tloan.cont-code," ") =  2
                          AND tloan.open-date >= mBegDate
                          AND tloan.close-date =  ?
         NO-LOCK:
            RUN UpdateUniformBag IN h_bag (BUFFER tloan).
         END.
             
      RUN Fill-SysMes IN h_tmess (
         "", "", "1",         
         IF LENGTH (mPos) > 0
            THEN ("������� " + QUOTER (loan.cont-code) + 
                  " �ᯥ譮 �ਢ易� � ���� " + QUOTER (mPos) + ".")
            ELSE ("������� " + QUOTER (loan.cont-code) 
                             + " �ᯥ譮 �뢥��� �� ���.")
      ).
      IF LENGTH (mPos) <= 0
      THEN DO:
         IF    pick-value <> "3"
           AND pick-value <> "4" 
         THEN
            RUN messmenu.p (10,
               "[�᪫���� �� ��� ��ᮢᥬ]",
               " ��⠭����� �ਧ��� ����ﭭ��� �᪫�祭�� " +
               "�� ��� ��� ������� " + loan.cont-code + "?",
               "1. ��,"                +
               "2. ���,"               +
               "3. �� ��� ���,"       +
               "4. ��� ��� ���"     
               ).
         IF NOT KEYFUNCTION(LASTKEY) =  "end-error"
         THEN DO:
            IF    pick-value =  "1"
               OR pick-value =  "3" 
            THEN DO:
               IF NOT UpdateSignsEx (loan.class-code,
                                     loan.contract + "," + loan.cont-code,
                                     "����᪫",
                                     "��") 
               THEN
                  RUN Fill-sysmes IN h_tmess ("", "", "1",
                        "�� ���� ��⠭����� ����᪫ = �� ��� ������� " + loan.cont-code).
            END.
         END.
      END.
   END.
END.
                        /* �����襭�� ����� ��⮪���஢����. */
RUN End-SysMes IN h_tmess.
{intrface.del}          /* ���㧪� �����㬥����. */ 

RETURN.
/* $LINTFILE='bagchng.p' */
/* $LINTMODE='1,4,6,3' */
/* $LINTENV ='2st' */
/* $LINTVSS ='*' */
/* $LINTUSER='pase' */
/* $LINTDATE='17/03/2017 16:04:13.005+04:00' */
/*prosignxyy7Tes3AtYY1rwqgapdmQ*/