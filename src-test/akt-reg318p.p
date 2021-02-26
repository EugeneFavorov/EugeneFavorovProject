/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2014 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: book-reg318p.p
      Comment: ����� �࠭���� 業���⥩ 318-�
   Parameters: ���� ���㬥�⮢, �����
         Uses:
      Used by:
      Created: 03.08.2008 15:49 elus
     Modified: 03.08.2008 15:49 elus
*/
{globals.i}
{wordwrap.def}
{intrface.get tparam}
{intrface.get sessions}
{intrface.get vok} /* �����㬥�� ࠡ��� � ��쥪⠬� ��� */
{intrface.get instrum}  /* �����㬥��� ��� ���ᮢ � ����� PP-INSTR.P */
{intrface.get kau}
{tmprecid.def}


{sh-defs.i}
{ksh-defs.i}

DEFINE INPUT  PARAMETER iParam AS CHARACTER NO-UNDO. 

DEFINE VARIABLE iDocType AS CHARACTER NO-UNDO.
DEFINE VARIABLE iOtstup  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNn      AS INT64     NO-UNDO INIT 0.
DEFINE VARIABLE mNacCur  AS CHARACTER NO-UNDO.

iDocType = IF ENTRY(1,iParam,";") EQ "" THEN "*" ELSE ENTRY(1,iParam,";").

IF NUM-ENTRIES(iParam,";") > 1 THEN
   iOtstup = FILL(" ",INT(ENTRY(2,iParam,";"))) NO-ERROR.
IF NUM-ENTRIES(iParam,";") > 2 THEN
   mNacCur = STRING(ENTRY(3,iParam,";"),"x(3)").
ELSE
   mNacCur = "RUB".


&GLOB Months "ﭢ���,䥢ࠫ�,����,��५�,���,���,���,������,ᥭ����,~
������,�����,�������"
&GLOB Days "��ࢮ�,��஥,����,�⢥�⮥,��⮥,��⮥,ᥤ쬮�,���쬮�,����⮥,����⮥,��������⮥,�������⮥,�ਭ���⮥,���ୠ��⮥,~
��⭠��⮥,��⭠��⮥,ᥬ����⮥,��ᥬ����⮥,����⭠��⮥,�����⮥,������� ��ࢮ�,������� ��஥,������� ����,~
������� �⢥�⮥,������� ��⮥,������� ��⮥,������� ᥤ쬮�,������� ���쬮�,������� ����⮥,�ਤ�⮥,�ਤ��� ��ࢮ�"
&GLOB Years "��ࢮ��,��ண�,���쥣�,�⢥�⮣�,��⮣�,��⮣�,ᥤ쬮��,���쬮��,����⮣�,����⮣�,�������⮣�,�������⮣�,�ਭ���⮣�,~
���ୠ��⮣�,��⭠��⮣�,��⭠��⮣�,ᥬ����⮣�,��ᥬ����⮣�,����⭠�⮣�,�����⮣�,������� ��ࢮ��,������� ��ࢮ��,~
������� ��ண�,������� ���쥣�,������� �⢥�⮣�,������� ��⮣�,��⮣�,������� ᥤ쬮��,������� ���쬮��,������� ����⮣�,�ਤ�⮣�,�ਤ��� ��ࢮ��,~
�ਤ��� ��ࢮ��,�ਤ��� ��ண�,�ਤ��� ���쥣�,�ਤ��� �⢥�⮣�,�ਤ��� ��⮣�,�ਤ��� ��⮣�,�ਤ��� ᥤ쬮��,�ਤ��� ���쬮��,�ਤ��� ����⮣�"


DEFINE TEMP-TABLE ttBookReg NO-UNDO /* ����� ��� ��� �� ������ � 業���� */  
   FIELD Acct       LIKE Acct.acct                                      
   FIELD Bal-acct   LIKE acct.bal-acct                                  
   FIELD Currency   LIKE Acct.currency                                  
   FIELD Details    LIKE Acct.Details                                   
   FIELD RepGroup   AS CHARACTER /* ��㯯� �����ᮢ�� ��⮢ */
   FIELD Balance    AS DECIMAL /* ���⮪ � ���. */                     
   FIELD SummDb     AS DECIMAL /* ��室 � ���. */                      
   FIELD SummCr     AS DECIMAL /* ��室 � ���. */                      
   FIELD BalanceRub AS DECIMAL /* ���⮪ � �� */                    
   FIELD SummDbRub  AS DECIMAL /* ��室 � �� */                     
   FIELD SummCrRub  AS DECIMAL /* ��室 � �� */                     
   FIELD DocDate    AS CHARACTER
   FIELD OpDate     AS DATE
   FIELD Consolid   AS CHARACTER
   FIELD form-type-code  AS CHARACTER

   INDEX idx Acct Currency
   INDEX idxdate DocDate
   INDEX idxCons form-type-code
.                     

DEFINE VARIABLE mColAmt     AS INT64     NO-UNDO. /* ��ਭ� ���� �㬬� */
DEFINE VARIABLE mCols       AS INT64     NO-UNDO. /* ��ਭ� ���� */
DEFINE VARIABLE mLines      AS CHARACTER NO-UNDO EXTENT 14. /* �����窠 */
DEFINE VARIABLE mLines2     AS CHARACTER NO-UNDO EXTENT 10.  /* �����窠 ���������*/
DEFINE VARIABLE mCurKau     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mKau-id     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mGrupType   AS LOGICAL   NO-UNDO. /* ��㯯�஢�� �� �����䨪���� */
DEFINE VARIABLE vOpTime     AS INT64     NO-UNDO.
DEFINE VARIABLE vOpDate     AS DATE      NO-UNDO.
DEFINE VARIABLE vDate       AS CHARACTER NO-UNDO.
DEFINE VARIABLE vConsolid AS CHARACTER NO-UNDO.
DEFINE VARIABLE vform-type-code AS CHARACTER NO-UNDO.
DEFINE VARIABLE lFIOInRep   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lPostInRep  AS CHARACTER NO-UNDO.
DEFINE VARIABLE lCodUser    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCurrName   AS CHARACTER NO-UNDO.
DEFINE VARIABLE SummaStr    AS CHARACTER EXTENT 2 NO-UNDO.
DEFINE VARIABLE mStr        AS CHARACTER  NO-UNDO.

DEFINE VARIABLE lFIOInRep01   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lFIOInRep02   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lFIOInRep03   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lFIOInRep04   AS CHARACTER NO-UNDO.

DEFINE VARIABLE lPostInRep01   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lPostInRep02   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lPostInRep03   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lPostInRep04   AS CHARACTER NO-UNDO.


DEFINE BUFFER cAcct FOR Acct. /* ���� ��� acct */
DEFINE BUFFER bufttBookReg  FOR ttBookReg.
DEFINE BUFFER buf2ttBookReg FOR ttBookReg.

DEFINE VARIABLE mUser01    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mUser02    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mUser03    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mUser04    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDpr       AS CHARACTER NO-UNDO.
DEFINE VARIABLE bDpr       AS CHARACTER NO-UNDO.
DEFINE VARIABLE mStr00     AS CHARACTER EXTENT 6 NO-UNDO.
DEFINE VARIABLE mBranchId  AS CHARACTER NO-UNDO.


DEFINE VARIABLE mOpDate     AS CHARACTER NO-UNDO.
DEFINE VARIABLE bOpDate     AS CHARACTER NO-UNDO.

DEFINE NEW SHARED VARIABLE list-id AS CHARACTER NO-UNDO.

DEF BUFFER bsessions  for sessions.
DEF BUFFER bkau-entry for kau-entry.


{agr-beg.def 
   &NameTitle = "����� ��������� ���������"
   &TypeDoc   = "iDocType"
   &NameRep   = "'������࠭'"} /* ����� ⨯ ���㬥�� �� ����� */
   
mDateLst = GetDprDays(mUsDprIDLst).
{agr-beg.i} 

mDpr = tGetParam("dpr-id", "vok", "").
find first sessions where sessions.dpr-id    = int(mDpr) no-lock no-error.
if avail sessions then do:
    mUser01   = sessions.user-id.
    mBranchID = sessions.branch-id.
end.
else do:
   message " �� ������� ᬥ�� ���짮��⥫� ." view-as alert-box. 
end.

for each kau-entry where kau-entry.kau = mDpr 
                     and kau-entry.op-date > sessions.op-date
                     no-lock
                     by kau-entry.op-date .
    find first op where op.op = kau-entry.op no-lock.
    if avail op then do:
       if op.op-kind = "������" then do:
          find first bkau-entry where bkau-entry.op = kau-entry.op
                                  and bkau-entry.kau <> mDpr 
                                  no-lock no-error.
          if avail bkau-entry then do:
             find first bsessions where bsessions.dpr-id = int(bkau-entry.kau)
                                  no-lock no-error.
             if avail bsessions then do:
                 mUser03 = bsessions.user-id.
             end.
             leave.
          end.
       end.
    end.
end.
find first branch where branch.branch-id = mBranchID.
if avail branch then do:
   mLines2[1] = padc(branch.name,98).
end.


mOpDate = STRING(DAY(sessions.op-date),"99") + " " + ENTRY(MONTH(sessions.op-date),{&Months}) + " " + STRING(YEAR(sessions.op-date)) + " ����".
if not avail bsessions then do: 
   message " �� ������� ᬥ�� , � ������ ��।��� 業���� " view-as alert-box.
   return.
end.
bOpDate = STRING(DAY(bsessions.op-date),"99") + " " + ENTRY(MONTH(bsessions.op-date),{&Months}) + " " + STRING(YEAR(bsessions.op-date)) + " ����".


FORM
   mUser01
      FORMAT "x(3000)" VIEW-AS FILL-IN SIZE 30 by 1 
      LABEL  "����"  
      HELP   "C���㤭��, ᤠ�訩 業���� �� ����"
   mUser02
      FORMAT "x(3000)" VIEW-AS FILL-IN SIZE 30 by 1 
      LABEL  "� ������⢨�"  
      HELP   "����㤭��, ������⢮��訩 �� ᤠ� 業���⥩ �� ����"
   mUser03
      FORMAT "x(3000)" VIEW-AS FILL-IN SIZE 30 by 1 
      LABEL  "�ਭ�" 
      HELP   "����㤭��, �ਭ�訩 業���� �� ����"
   mUser04
      FORMAT "x(3000)" VIEW-AS FILL-IN SIZE 30 by 1 
      LABEL  "� ������⢨�"  
      HELP   "���㤭��, ���������騩 �� �ਭ�⨨ 業���⥩ �� ����"
WITH FRAME frParam 1 COL OVERLAY CENTERED ROW 10 TITLE "[ ������� ������]".

ON F1 OF mUser01 DO:
   list-id   = FRAME-VALUE.
   DO TRANSACTION:
      RUN op-user7.p(4,mBranchID).
   END.
   IF LASTKEY EQ 10 THEN 
      DISPLAY list-id @ mUser01 WITH FRAME frParam.
END.
ON F1 OF mUser02 DO:
   list-id   = FRAME-VALUE.
   DO TRANSACTION:
      RUN op-user7.p(4,mBranchID).
   END.
   IF LASTKEY EQ 10 THEN 
      DISPLAY list-id @ mUser02 WITH FRAME frParam.
END.
ON F1 OF mUser03 DO:
   list-id   = FRAME-VALUE.
   DO TRANSACTION:
      RUN op-user7.p(4,mBranchID).
   END.
   IF LASTKEY EQ 10 THEN 
      DISPLAY list-id @ mUser03 WITH FRAME frParam.
END.
ON F1 OF mUser04 DO:
   list-id   = FRAME-VALUE.
   DO TRANSACTION:
      RUN op-user7.p(4,mBranchID).
   END.
   IF LASTKEY EQ 10 THEN 
      DISPLAY list-id @ mUser04 WITH FRAME frParam.
END.



PAUSE 0.
UPDATE
   mUser01
   mUser02
   mUser03
   mUser04
WITH FRAME frParam.

HIDE FRAME frParam NO-PAUSE.
IF NOT (   KEYFUNC(LASTKEY) EQ "GO" 
        OR KEYFUNC(LASTKEY) EQ "RETURN") 
THEN LEAVE.



ASSIGN
   mColAmt   = IF mNameCurr THEN 3 ELSE {&format-cur-name}
   mGrupType = CAN-FIND(FIRST code WHERE
                              code.class  EQ "��㯊������"
                          AND code.parent EQ "��㯊������")
   .



ASSIGN
   mLines2[ 2]   = "�����������������������������������������������������������������������������������������������"
   mLines2[ 3] = "      (������ �ଥ���� (᮪�饭��� �ଥ����) ������������ �।�⭮� �࣠����樨   "
   mLines2[ 4] = "      ��� ������ (᮪�饭���) ������������ 䨫����, ��� ������������ � (���) �����  "                                        
   mLines2[ 5] = "      ��� (�� ����稨) ���� ��� ����������騥 �ਧ���� ��� (�� ������⢨�      "
   mLines2[ 6] = "      ������������ � �����) � 㪠������ �� ��� �ਭ���������� �।�⭮� �࣠����樨 "                                        
   mLines2[ 7] = "                                    (䨫����)                                        "
   mLines2[ 8] = " "
   mLines2[ 9] = "                                     ��� ���-��।�� "
   mLines2[10] = "     ���ᮢ�� ���㬥�⮢, ������� ����� � �㡫�� � �����࠭��� ����� � ��㣨� 業���⥩."
   .
ASSIGN
   mLines[1]   = "������������������������������������������������������������������������������������������������Ŀ"
   mLines[2]   = "��८�।������ ����"
   mLines[3]   = "�     �         業���⥩          � ������  ��� ������⢮   �                                 �"
   mLines[4]   = "������������������������������������������������������������������������������������������������Ĵ"  
   mLines[5]   = "�     �                            �       �                   �                                 �"
   mLines[6]   = "�     �                            �       �                   �                                 �"
   mLines[7]   = "��������������������������������������������������������������������������������������������������"  
   .           




&GLOB Cols mCols

DO i = 1 TO NUM-ENTRIES(mUsDprIDLst):
   mCurKau = ENTRY(i,mUsDprIDLst).
   FOR EACH kau-entry WHERE kau-entry.kau-id    BEGINS "��������"   
                        AND kau-entry.kau       EQ mCurKau
                        AND kau-entry.op-date   EQ mCuDate          
                        AND kau-entry.op-status GE CHR(251)         
   NO-LOCK,

   FIRST op WHERE op.op EQ kau-entry.op
   NO-LOCK:

      RUN GetDateTimeOpTr(op.op-transaction, op.op, OUTPUT vOpTime,OUTPUT vOpDate).

      CASE mDateOtc:
         WHEN "" THEN vDate = "*".
         WHEN "*" THEN
         DO:
            vDate = STRING(vOpDate, "99/99/9999").
            IF NOT CAN-DO(mDateLst, vDate) THEN NEXT.
         END.
         OTHERWISE
         DO:
            vDate = STRING(vOpDate, "99/99/9999").
            IF mDateOtc NE vDate THEN NEXT.
         END.
      END CASE.


      RUN AddRecInTemp-Table(kau-entry.acct,kau-entry.Currency, vDate).
   END.
END.

IF mCuUserID = ? THEN /* ����� �������� �� ���ࠧ�������, � */
DO:  /* �� �६����� ⠡���� ������� ��⠫�� ��� ���ࠧ������� */
   each-acct:
   FOR EACH cAcct WHERE 
            cAcct.branch-id EQ mCuBrchID 
      NO-LOCK:
      CASE mSessFlt:
         WHEN "������" THEN 
         DO:
            IF GetXattrValueEx("acct",cacct.acct + "," + cacct.currency,"�������","���") NE "���" THEN
               NEXT each-acct.
         END.
         WHEN "���୨�" THEN
         DO:
            IF GetXattrValue("acct",cacct.acct + "," + cacct.currency,"�������") NE "��" THEN
               NEXT each-acct.
         END.
      END CASE.
      RUN get-kau-id(cAcct.acct,cAcct.Currency,OUTPUT mKau-id).
      IF mKau-id BEGINS "��������" THEN
      CASE mDateOtc:
         WHEN "" THEN RUN AddRecInTemp-Table(cAcct.acct,cAcct.Currency,STRING(mCuDate)).
         WHEN "*" THEN
         DO i = 1 TO NUM-ENTRIES(mDateLst):
            RUN AddRecInTemp-Table(cAcct.acct,cAcct.Currency,ENTRY(i,mDateLst)).
         END.
         OTHERWISE
         DO:
            RUN AddRecInTemp-Table(cAcct.acct,cAcct.Currency,mDateOtc).
         END.
      END CASE.      
   END. /* FOR EACH �Acct */
END. /* IF mCuUserID = ? THEN */
ELSE
   IF mDocType NE "*" THEN
      RUN EditRecInTemp-Table.

IF NOT mShowZero THEN 
   FOR EACH ttBookReg WHERE 
            ttBookReg.Balance EQ 0:
       DELETE ttBookReg.
   END.               

DEFINE VARIABLE mBookBeg AS DATE NO-UNDO. /* ��� ��砫� ����� (���. ४�����) */
DEFINE VARIABLE mPageNum AS INT64 NO-UNDO. /* ����� ��࠭��� */

IF mCuUserID = ? THEN /* ����� �������� �� ���ࠧ������� */
   mBookBeg = GetDateBeginBook(mCuDate,mCuBrchID,"br-book-cash").
ELSE                  /* ����� �������� �� ������ */
   mBookBeg = GetDateBeginBook(mCuDate,mCuUserID,"us-book-cash").

mPageNum = OpDaysVOK(mBookBeg,mCuDate,mCuUserID,mCuBrchID).


FOR EACH bufttBookReg
BREAK BY bufttBookReg.OpDate:
   IF FIRST-OF(bufttBookReg.OpDate) THEN do:
      FOR EACH ttBookReg WHERE ttBookReg.DocDate EQ bufttBookReg.DocDate NO-LOCK BREAK  
         BY ttBookReg.form-type-code :
         IF FIRST-OF(ttBookReg.form-type-code)  THEN do:
          if ttBookReg.Consolid = "��ꥤ�����" then do:
            vform-type-code = ttBookReg.form-type-code.   
            create  buf2ttBookReg.
            ASSIGN 
               buf2ttBookReg.Acct       = substr(ttBookReg.Acct,1,8) + "%"      
               buf2ttBookReg.Currency   = ttBookReg.Currency  
               buf2ttBookReg.Details    = ttBookReg.Details   
               buf2ttBookReg.RepGroup   = ttBookReg.RepGroup  
               buf2ttBookReg.Balance    = ttBookReg.Balance    /* ���⮪ � ���. */
               buf2ttBookReg.SummDb     = ttBookReg.SummDb     /* ��室 � ���.  */
               buf2ttBookReg.SummCr     = ttBookReg.SummCr     /* ��室 � ���.  */
               buf2ttBookReg.BalanceRub = ttBookReg.BalanceRub /* ���⮪ � ��  */
               buf2ttBookReg.SummDbRub  = ttBookReg.SummDbRub  /* ��室 � ��   */
               buf2ttBookReg.SummCrRub  = ttBookReg.SummCrRub  /* ��室 � ��   */
               buf2ttBookReg.Bal-acct   = ttBookReg.Bal-acct  
               buf2ttBookReg.DocDate    = ttBookReg.DocDate   
               buf2ttBookReg.OpDate     = ttBookReg.OpDate    
            .
            buf2ttBookReg.Consolid       = "��⮢�". 
            buf2ttBookReg.form-type-code = ttBookReg.form-type-code.
            delete ttBookReg.
          end.
         end.
         else do:
          if ttBookReg.Consolid = "��ꥤ�����" then do:
            find first buf2ttBookReg where buf2ttBookReg.form-type-code = ttBookReg.form-type-code
                                       and buf2ttBookReg.Consolid = "��⮢�" no-error.
            if avail buf2ttBookReg then do:
               ASSIGN 
                  buf2ttBookReg.Balance    = buf2ttBookReg.Balance    + ttBookReg.Balance    /* ���⮪ � ���. */
                  buf2ttBookReg.SummDb     = buf2ttBookReg.SummDb     + ttBookReg.SummDb     /* ��室 � ���.  */
                  buf2ttBookReg.SummCr     = buf2ttBookReg.SummCr     + ttBookReg.SummCr     /* ��室 � ���.  */
                  buf2ttBookReg.BalanceRub = buf2ttBookReg.BalanceRub + ttBookReg.BalanceRub /* ���⮪ � ��  */
                  buf2ttBookReg.SummDbRub  = buf2ttBookReg.SummDbRub  + ttBookReg.SummDbRub  /* ��室 � ��   */
                  buf2ttBookReg.SummCrRub  = buf2ttBookReg.SummCrRub  + ttBookReg.SummCrRub  /* ��室 � ��   */
               .
               delete ttBookReg.
            end.
          end.
         end.
      end.
   end.
end.


{agr-end.i 
   &OnePageRep  = "YES"
   &OnePageName = "PrintOnePageBookReg"
   &Cols = 130
}

{intrface.del}

/* ========================== Procedure block ============================ */
PROCEDURE PrintOnePageBookReg.

   DEFINE VARIABLE vBegDate   AS CHARACTER NO-UNDO. /* ���� */
   DEFINE VARIABLE vEndDate   AS CHARACTER NO-UNDO. /* ����祭� */
   DEFINE VARIABLE vDate      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vCurrName  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vDeteils   AS CHARACTER NO-UNDO EXTENT 6. /* ��� ࠧ������ �� ��ப� */
   DEFINE VARIABLE vSummStr   AS CHARACTER NO-UNDO EXTENT 6. /* ��� ࠧ������ �� ��ப� */
   DEFINE VARIABLE vTmpDate   AS DATE      NO-UNDO.
   DEFINE VARIABLE vChDate    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vGroupName AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vBalName   AS CHARACTER NO-UNDO EXTENT 6.
   DEFINE VARIABLE vAlreadyPrint AS CHARACTER   NO-UNDO.

   DEFINE VARIABLE vMonthsStr  AS CHARACTER NO-UNDO. /* 12 ����楢 */
   DEFINE VARIABLE vMonthName  AS CHARACTER NO-UNDO. /* ����� */
   DEFINE VARIABLE vMonthName2 AS CHARACTER NO-UNDO. /* ����� */
   DEFINE VARIABLE vRepDate    AS DATE NO-UNDO.

   DEFINE BUFFER bttBookReg FOR ttBookReg.
   DEFINE BUFFER b2ttBookReg FOR ttBookReg.
   DEFINE BUFFER currency   FOR currency.

   vMonthsStr = '������,���ࠫ�,����,��५�,���,���,' + 
                '���,������,�������,������,�����,�������'.
   vMonthName2 = '������,���ࠫ�,����,��५�,���,���,' + 
                '���,������,�������,������,�����,�������'.
 
   FOR EACH bttBookReg
   BREAK BY bttBookReg.OpDate:

   IF FIRST-OF(bttBookReg.OpDate) THEN
   DO:
      vRepDate = IF {assigned mDateOtc} THEN bttBookReg.OpDate
                                        ELSE mCuDate.

      vMonthName  = ENTRY(MONTH(vRepDate),vMonthsStr).
      mChrDate = '"' + SUBSTRING(STRING(vRepDate,"99/99/9999"),1,2) + '" ' + vMonthName + " " + SUBSTRING(STRING(vRepDate,"99/99/9999"),7,4) + " ����".
      vTmpDate    = DATE(IF mCuUserID = ? THEN GetXattrValue("branch",STRING(mCuBrchID),"beg-book-val") ELSE GetUserXattrValue(mCuUserID,"beg-book-val")).
      vChDate     = '���� ' + '"' + SUBSTRING(STRING(vTmpDate,"99/99/9999"),1,2) + '" ' + ENTRY(MONTH(vTmpDate),vMonthName2) + " " + SUBSTRING(STRING(vTmpDate,"99/99/9999"),7,4) + " ����".

/*ayv ���⮪ ������ ��।������� �� ᫥���騩 ࠡ�稩 ����*/
      DEF VAR vR AS DATE NO-UNDO.
      DEF BUFFER b-ses FOR sessions.

      FOR EACH b-ses WHERE b-ses.branch-id EQ mCuBrchID
                     AND   b-ses.op-date GT mCuDate
            NO-LOCK BY b-ses.op-date:
            LEAVE.
      END.
      
      IF AVAIL(b-ses) THEN 
         vR = b-ses.op-date.
      ELSE DO:
         MESSAGE '�������� ᬥ�� �� �����! ���⮪ �� ' + STRING(mCuDate,'99.99.9999') VIEW-AS ALERT-BOX.
         vR = mCuDate.
      END.
/**/

      mLines[2] = "�  �  �        ������������        �  ���  �      �㬬�        �       �㬬� �ய����            �".
/*       + PADC("���⮪ �� " + REPLACE({strdate.i vR},'�.','����') ,mColAmt + 24) + IF mNameCurr THEN "  �" ELSE " �".
*/
      ASSIGN
      vChDate     = '���� "___" __________ 20___ ����' WHEN vChDate EQ ?.
      ASSIGN
         vBegDate = vChDate
         vEndDate = '����祭� "___" __________ 20___ ����'
         vDate    = '����� ' + vMonthName + ' ��� ' + STRING(YEAR(vRepDate),"9999")
         .      
      
/* ========================== ������ ���� ============================= */
   IF mBookTit THEN
   DO:   /* ������ ���� ���⠥� */
      {head-318p.i
         &CodForm = "'0402118'"
      }
      {orgname318p.i
         &CurBranchName = mBranchName
      }
      PUT UNFORMATTED
         PADC("�����",                                    {&cols}) SKIP
         PADC("�࠭���� 業���⥩",                      {&cols}) SKIP(1).
/*          FILL(' ',{&cols} - LENGTH(vBegDate)) vBegDate             SKIP
         FILL(' ',{&cols} - LENGTH(vEndDate)) vEndDate             SKIP.
        PADC("����� � �����饩 �����",                 {&cols}) SKIP
         PADC("�ந�������� �� ������� �� �ᯮ�짮�����", {&cols}) SKIP(1).
         PAGE.*/
   END.
/* ========================== ����� ⠡��窨 ============================= */
   DO i = 1 TO 10:
         PUT UNFORMATTED iOtstup mLines2[i] SKIP.
   END.
   DO i = 1 TO 4:
      IF mLines[i] NE "" THEN
         PUT UNFORMATTED iOtstup mLines[i] SKIP.
   END.
/* ++++++++++++++++++++  ��७����������� �㡫�� +++++++++++++++++++++++++++*/
   FOR EACH ttBookReg NO-LOCK.  
      if ttBookReg.Currency = "" then ttBookReg.Currency = "643".
   end.

/* ==================== �������� ⠡��窨 � �㬬��� ====================== */
   FOR EACH ttBookReg WHERE ttBookReg.DocDate EQ bttBookReg.DocDate NO-LOCK BREAK  
      BY ttBookReg.RepGroup 
      BY ttBookReg.Bal-acct
      BY ttBookReg.Currency:
      mNn = mNn + 1.
      IF mGrupType THEN
         vGroupName = GetCodeName("��㯊������",ttBookReg.RepGroup).
      ELSE
      DO:
         CASE ttBookReg.RepGroup:
            WHEN "1" THEN vGroupName = "�������� ����筮���".
            WHEN "2" THEN vGroupName = "��㣨� 業����    ".
         END CASE.
      END.

      IF FIRST-OF(ttBookReg.RepGroup) THEN 
         vAlreadyPrint = "".   
      

      IF NOT CAN-DO(vAlreadyPrint,ttBookReg.RepGroup) THEN
      DO: /* ���⠥� ࠧ����⥫� */
         IF mGrupType THEN
         DO:   
            /**/
         END.
         ELSE
            IF mPrnAcct THEN
               PUT UNFORMATTED iOtstup 
                  mLines[5] SKIP
                  iOtstup "�       � " STRING(vGroupName,"x(20)") 
                  substr(mLines[6], 30)
                  iOtstup mLines[5] SKIP.
            ELSE
               PUT UNFORMATTED iOtstup
                  mLines[5] SKIP
                  iOtstup "�       �" STRING(vGroupName,"x(20)") " � " 
                  ttBookReg.bal-acct "                �" 
                  substr(mLines[6], 30) SKIP
                  iOtstup mLines[5] SKIP.
      
         {additem.i vAlreadyPrint ttBookReg.RepGroup}
      END.

      vDeteils = ttBookReg.Details.
      {wordwrap.i 
        &s = vDeteils
        &n = 6
        &l = 22
      }

      FIND FIRST currency WHERE currency.currency EQ ttBookReg.Currency
         NO-LOCK NO-ERROR.
      IF AVAIL currency THEN
         IF mNameCurr THEN
            vCurrName = IF currency.currency = "" 
                        THEN mNacCur 
                        ELSE STRING(currency.i-currency,"x(3)").
         ELSE
            vCurrName = STRING(currency.name-currenc,"x({&format-cur-name})").
      ELSE
         IF mNameCurr THEN
            vCurrName = " - ".
         ELSE
            vCurrName = STRING("����� �� �������","x({&format-cur-name})").
      IF AVAIL currency THEN do:

         mCurrName = IF currency.currency = "" 
                        THEN "810" 
                        ELSE STRING(currency.currency,"x(3)").
      end.
      else do:
         mCurrName = " - ".
      end.
      RUN x-amtstr.p(ttBookReg.Balance,currency.currency, TRUE, TRUE, OUTPUT SummaStr[1], OUTPUT SummaStr[2]).
      vSummStr = SummaStr[1] + " " + SummaStr[2].
      {wordwrap.i 
        &s = vSummStr
        &n = 6
        &l = 33
      }
      if vDeteils[1] = "" then vDeteils[1] = vCurrName.
      IF mPrnAcct THEN
         PUT UNFORMATTED iOtstup 
            "�" mNn FORMAT ">>>>9"
             "� " vDeteils[1] FORMAT "x(26)"  " �  " 
            /* vCurrName  FORMAT "x(19)"  " �  "  */ mCurrName format "x(3)" "  � "
            ttBookReg.Balance FORMAT "->,>>>,>>>,>>9.99"  
            " �" vSummStr[1]  FORMAT "x(33)" "� "
            SKIP.
      ELSE
         PUT UNFORMATTED iOtstup 
            "�" mNn FORMAT ">>>>9"
             "� " vDeteils[1] FORMAT "x(26)"  " �  "
            /* vCurrName  FORMAT "x(19)"  " �  " */ mCurrName format "x(3)" "  � "
            ttBookReg.Balance FORMAT "->,>>>,>>>,>>9.99"  
            " �" vSummStr[1]  FORMAT "x(33)"  "� "
            SKIP.

      DO i = 2 TO 6:
         mStr =  mLines[6].
         IF vDeteils[i] NE "" THEN do:
            mStr = substr(mStr,1,8) + string(vDeteils[i],"x(22)") + substr(mStr,31).
         END.
         IF vSummStr[i] NE "" THEN do:
            mStr = substr(mStr,1,64)  + string(vSummStr[i],"x(33)") + substr(mStr,98).
         END.
         IF vDeteils[i] EQ "" and vSummStr[i] EQ "" THEN do:
            LEAVE.
         end.
         PUT UNFORMATTED iOtstup mStr
             SKIP.
      END. /* DO i = 2  */

/*
�  �   �    ������������     �  ���  �      �㬬�        �       �㬬� �ய����            �
*/
   END.
   PUT UNFORMATTED iOtstup mLines[7] SKIP.
/* ================ ������ � �����⥫쭠� ������� ====================== */

   PUT UNFORMATTED SKIP(1)
       iOtstup "���ᮢ� ���㬥��� �� " mOpDate format "x(30)"  SKIP(1).
   PUT UNFORMATTED SKIP(1)
       iOtstup "������⢮ ���ᮢ�� ���㬥�⮢  __________________________ ��."  SKIP(1).

   mStr00  =  trim(GetRefVal("��⏏",vRepDate,sessions.Branch-Id + ",1")).
   {wordwrap.i 
     &s = mStr00
     &n = 6
     &l = 90
   }
   IF mStr00[1] NE ? THEN do:
       PUT UNFORMATTED   iOtstup mStr00[1] SKIP(0).
   end.
   DO i = 2 TO 6:
      IF mStr00[i] NE ? THEN
         PUT UNFORMATTED iOtstup mStr00[i]  SKIP(0).
      ELSE
        LEAVE.
   END.

   mStr00  =  trim(GetRefVal("��⏏",vRepDate,sessions.Branch-Id + ",2")).
   {wordwrap.i 
     &s = mStr00
     &n = 6
     &l = 90
   }
   IF mStr00[1] NE ? THEN do:
       PUT UNFORMATTED   iOtstup mStr00[1] SKIP(0).
   end.
   DO i = 2 TO 6:
      IF mStr00[i] NE ? THEN
         PUT UNFORMATTED iOtstup mStr00[i] SKIP(0).
      ELSE
        LEAVE.
   END.
   mStr00  =  trim(GetRefVal("��⏏",vRepDate,sessions.Branch-Id + ",3")).
   {wordwrap.i 
     &s = mStr00
     &n = 6
     &l = 90
   }
   IF mStr00[1] NE ? THEN do:
       PUT UNFORMATTED   iOtstup mStr00[1] SKIP(0).
   end.
   DO i = 2 TO 6:
      IF mStr00[i] NE ? THEN
         PUT UNFORMATTED iOtstup mStr00[i] SKIP(0).
      ELSE
        LEAVE.
   END.
   mStr00  =  trim(GetRefVal("��⏏",vRepDate,sessions.Branch-Id + ",4")).
   {wordwrap.i 
     &s = mStr00
     &n = 6
     &l = 90
   }
   IF mStr00[1] NE ? THEN do:
       PUT UNFORMATTED   iOtstup mStr00[1] SKIP(0).
   end.
   DO i = 2 TO 6:
      IF mStr00[i] NE ? THEN
         PUT UNFORMATTED iOtstup mStr00[i]  SKIP(0).
      ELSE
        LEAVE.
   END.


   lFIOInRep  = GetUserName(mCuUserID). /* ���⠥� ��� �� ������ */
   lPostInRep = GetCode("��␠����",GetUserXAttrValue(mCuUserID,"VOK")).
   lFIOInRep01  = if GetUserName(mUser01) = "���짮��⥫� �� ������" then " " else GetUserName(mUser01). /* ���⠥� ��� �� ������ */
   lFIOInRep02  = if GetUserName(mUser02) = "���짮��⥫� �� ������" then " " else GetUserName(mUser02). /* ���⠥� ��� �� ������ */
   lFIOInRep03  = if GetUserName(mUser03) = "���짮��⥫� �� ������" then " " else GetUserName(mUser03). /* ���⠥� ��� �� ������ */
   lFIOInRep04  = if GetUserName(mUser04) = "���짮��⥫� �� ������" then " " else GetUserName(mUser04). /* ���⠥� ��� �� ������ */
   lPostInRep01 = GetUserXAttrValue(mUser01,"���������").
   lPostInRep02 = GetUserXAttrValue(mUser02,"���������").
   lPostInRep03 = GetUserXAttrValue(mUser03,"���������").
   lPostInRep04 = GetUserXAttrValue(mUser04,"���������").
   

   PUT UNFORMATTED  skip(1)
        iOtstup mOpDate   format "x(30)"  " ����:  _________    "  lFIOInRep01  format "x(24)"  
        lPostInRep01   format "x(25)"  SKIP(0).

   PUT UNFORMATTED 
        iOtstup "                                      (�������)        (���)                  (���������)                        " SKIP(1). 
   PUT UNFORMATTED 
        iOtstup "� ������⢨�:           ______________________    " lFIOInRep02  format "x(24)"  lPostInRep02  format "x(25)" SKIP(0).
   PUT UNFORMATTED 
        iOtstup "                              (�������)                (���)                  (���������)                        " SKIP(1).
   PUT UNFORMATTED 
        iOtstup bOpDate   format "x(28)"  " �ਭ�:  _________    " lFIOInRep03  format "x(24)" lPostInRep03  format "x(25)"  SKIP(0).
   PUT UNFORMATTED 
        iOtstup "                                      (�������)        (���)                  (���������)                        " SKIP(1).
   PUT UNFORMATTED 
        iOtstup "� ������⢨�:           ______________________    " lFIOInRep04  format "x(24)" lPostInRep04  format "x(25)"  SKIP(0).
   PUT UNFORMATTED 
        iOtstup "                              (�������)                (���)                  (���������)                        " SKIP(1).
   PUT UNFORMATTED 
        iOtstup "                                                                                                                    " SKIP(0).




   {bookend-318p.i} /* �����⥫쭠� ������� */

   END.
   END.
END PROCEDURE. /* PrintOnePageBookReg */

/*------------------------------------------------------------------------------
   �����祭��: �������� �� �६����� ⠡���� ����� � ���� 
------------------------------------------------------------------------------*/
PROCEDURE AddRecInTemp-Table:

   DEFINE INPUT  PARAMETER iAcct     AS CHARACTER NO-UNDO. /* ����� ��� */
   DEFINE INPUT  PARAMETER iCurrency AS CHARACTER NO-UNDO.  /* ����� */
   DEFINE INPUT  PARAMETER iOpDate   AS CHARACTER NO-UNDO.

   DEFINE BUFFER bAcct FOR Acct.
   DEFINE BUFFER bbAcct FOR Acct.
   DEFINE BUFFER bCurrency FOR Currency.

   DEFINE VARIABLE vNumEntry  AS INT64. /* ���-�� ᬥ� � */
   DEFINE VARIABLE vDprId     AS CHARACTER NO-UNDO. /* ����� */
   DEFINE VARIABLE vOutQtyDb  AS INT64   NO-UNDO INIT 0 . /* ���-�� ���㬥�⮢ �� ������ */   
   DEFINE VARIABLE vOutQtyCr  AS INT64   NO-UNDO INIT 0 . /* ���-�� ���㬥�⮢ �� �।��� */
   DEFINE VARIABLE vQtyDocs   AS DECIMAL   NO-UNDO. /* ������⢮ ���㬥�⮢ */

/* ��६���� ��� ����� �ᥣ� */
   DEFINE VARIABLE vBalance    AS DECIMAL NO-UNDO. /* ���⮪ � ����� */
   DEFINE VARIABLE vSummCr     AS DECIMAL NO-UNDO. /* ���室 � ����� */
   DEFINE VARIABLE vSummDb     AS DECIMAL NO-UNDO. /* ��室 � ����� */
   
   DEFINE VARIABLE vBalanceRub AS DECIMAL NO-UNDO. /* ���⮪ � �㡫�� */
   DEFINE VARIABLE vSummCrRub  AS DECIMAL NO-UNDO. /* ���室 � �㡫�� */
   DEFINE VARIABLE vSummDbRub  AS DECIMAL NO-UNDO. /* ��室 � �㡫�� */

   DEFINE VARIABLE vBalAcct    AS CHARACTER NO-UNDO.

   DEFINE VARIABLE vDetails    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vGroup      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vDate       AS DATE      NO-UNDO.
   DEFINE VARIABLE vDateStr    AS CHARACTER NO-UNDO.

   DEFINE BUFFER code      FOR code.

   vDate = IF {assigned mDateOtc} THEN DATE(iOpDate)
                                  ELSE mCuDate.
   vDateStr = IF {assigned mDateOtc} THEN iOpDate
                                     ELSE "*".

   FIND FIRST ttBookReg WHERE ttBookReg.acct     EQ iAcct 
                          AND ttBookReg.Currency EQ iCurrency
                          AND ttBookReg.DocDate  EQ vDateStr
   NO-LOCK NO-ERROR.
/* �᫨ �� ����� ������ �� �६����� ⠡��� */
   IF NOT AVAILABLE ttBookReg THEN
   DO:
/* ����⠥� ������ � ���⪨ �� ���� */
      IF mCuUserID = ? THEN /* ����� �������� �� ���ࠧ������� */
      DO:
         RUN acct-pos IN h_base (iAcct,
                                 iCurrency,
                                 vDate,
                                 vDate,
                                 CHR(251)).   
         IF iCurrency = "" THEN ASSIGN
            vBalance = sh-bal 
            vSummDb  = sh-db
            vSummCr  = sh-cr
         .                   
         ELSE ASSIGN
            vBalance = sh-val
            vSummDb  = sh-vdb
            vSummCr  = sh-vcr
         .

         vBalanceRub = sh-bal.
         vSummDbRub  = sh-db.
         vSummCrRub  = sh-cr.
      END. /* ����� �������� �� ���ࠧ������� */
      ELSE /* ����� �������� �� ������ */   
      DO:
         IF mUsDprIDLst GT "" THEN
         DO:
            ASSIGN
               vBalance    = 0
               vSummDb     = 0
               vSummCr     = 0
               vBalanceRub = 0
               vSummDbRub  = 0
               vSummCrRub  = 0
            .              

            REPEAT vNumEntry = 1 TO NUM-ENTRIES(mUsDprIDLst):
               vDprId = ENTRY(vNumEntry,mUsDprIDLst).
               RUN kau-pos.p (iAcct,
                              iCurrency,
                              vDate,
                              vDate,
                              CHR(251), /* "�",  */
                              vDprId).

               IF iCurrency = "" THEN ASSIGN
                  vBalance = vBalance + ksh-bal 
                  vSummDb  = vSummDb  + ksh-db
                  vSummCr  = vSummCr  + ksh-cr
               .                   
               ELSE ASSIGN
                  vBalance = vBalance + ksh-val
                  vSummDb  = vSummDb  + ksh-vdb
                  vSummCr  = vSummCr  + ksh-vcr
               .

               vBalanceRub = vBalanceRub + ksh-bal.
               vSummDbRub  = vSummDbRub  + ksh-db.
               vSummCrRub  = vSummCrRub  + ksh-cr.

            END. /* REPEAT vNumEntry = 1 TO */
         END. /* IF mDprIDLstNF GT "" THEN */

      END. /* ����� �������� �� ������ */                                                           
/* ����⠥� ���-�� ���㬥�⮢ (uvok.fun)*/
      IF mUsDprIDLst GT "" THEN
      DO:
         vQtyDocs = 0.
         REPEAT vNumEntry = 1 TO NUM-ENTRIES(mUsDprIDLst):
            vDprId = ENTRY(vNumEntry,mUsDprIDLst).
            RUN KauQtyOp(iAcct,
                         iCurrency,
                         vDprId,
                         vDate,
                         OUTPUT vOutQtyDb,
                         OUTPUT vOutQtyCr).
            vQtyDocs = vQtyDocs + vOutQtyDb + vOutQtyCr. /* ���-�� */
         END. /* REPEAT vNumEntry = 1 TO */                             
      END.  /* IF mUsDprIDLst GT "" THEN */                                                            

      IF vBalance NE 0 OR vQtyDocs NE 0 THEN /* �᫨ ����� �� ����� */
      DO:
         CREATE ttBookReg.
         ASSIGN
            vBalAcct  = SUBSTRING(iAcct,1,5)
            mAgreeYes = TRUE   /* �ਧ��� �����⮣� ���� */
            .
         IF mGrupType THEN
         DO:
            FIND FIRST code WHERE 
                       code.class EQ '��㯊������'
                   AND code.code  EQ vBalAcct
               NO-LOCK NO-ERROR.
            IF AVAIL code THEN
               ASSIGN
                  vGroup   = code.parent
                  .
         END.

         ASSIGN
            vGroup   = "1" WHEN NOT mGrupType
            vDetails = GetXAttrValueEx("Acct",
                                      iAcct + "," + iCurrency,
                                      "form-type-code",
                                      "-")
            .
         IF vDetails EQ "-" THEN /* �� �� ����� */
         DO:
            IF iCurrency GT "999" THEN
               FIND FIRST bCurrency WHERE bCurrency.currency EQ iCurrency 
                  NO-LOCK NO-ERROR.
            IF AVAILABLE bCurrency THEN 
               ASSIGN
                  vGroup   = "2" WHEN NOT mGrupType
                  vDetails = bCurrency.name-currenc.
            ELSE
               vDetails = " ".
         END. /* �� �� ����� */
         ELSE  do:
            /* �᫨ ���� �뢥�� ������������ ���� */
            vConsolid =  GetCode("��������������",vDetails).
            ttBookReg.Consolid   = vConsolid. 
            ttBookReg.form-type-code = vDetails.
            if  vDetails = "��0050000001" or
              /* vDetails = "��0050000001" or */
               vDetails = "��0050000001"
               then do:
               find first bbAcct where bbAcct.acct = iAcct no-lock no-error.
               if avail bbAcct then do:
                  vDetails = bbacct.Details.
                  vGroup   = IF NOT mGrupType THEN "2" ELSE vGroup.
               end.
            end.
            else do:
               ASSIGN
                  vDetails  = GetSecCodeNameEx(vDetails,GetCodeName("��������������",vDetails))
                  vDetails  = IF vDetails EQ ? THEN " " ELSE vDetails              
                  vGroup    = IF NOT mGrupType THEN "2" ELSE vGroup
                  .
            end.

         end.
         ASSIGN 
            ttBookReg.Acct       = iAcct
            ttBookReg.Currency   = iCurrency
            ttBookReg.Details    = vDetails
            ttBookReg.RepGroup   = vGroup
            ttBookReg.Balance    = vBalance    /* ���⮪ � ���. */
            ttBookReg.SummDb     = vSummDb     /* ��室 � ���.  */
            ttBookReg.SummCr     = vSummCr     /* ��室 � ���.  */
            ttBookReg.BalanceRub = vBalanceRub /* ���⮪ � ��  */
            ttBookReg.SummDbRub  = vSummDbRub  /* ��室 � ��   */
            ttBookReg.SummCrRub  = vSummCrRub  /* ��室 � ��   */
            ttBookReg.Bal-acct   = INT(vBalAcct)
            ttBookReg.DocDate    = vDateStr
            ttBookReg.OpDate     = DATE(vDateStr)
         NO-ERROR.
      END. /* */
   END. /* ELSE AVAILABLE ttAgrCur */
END PROCEDURE. /* AddCurrencyRec */

/*------------------------------------------------------------------------------
   �����祭��: �ࠢ�� ����� �� �६����� ⠡����
------------------------------------------------------------------------------*/

PROCEDURE EditRecInTemp-Table:

   DEFINE VARIABLE vRate    AS DECIMAL INIT 0 NO-UNDO.
   DEFINE VARIABLE vSumm    AS DECIMAL INIT 0 NO-UNDO.
   DEFINE VARIABLE vSummRub AS DECIMAL INIT 0 NO-UNDO.

   DEFINE BUFFER bacct     FOR acct.
   DEFINE BUFFER bcurrency FOR currency.
   DEFINE BUFFER kau-entry FOR kau-entry.
   DEFINE BUFFER op        FOR op.

   FOR EACH kau-entry WHERE kau-entry.op-date   EQ mCuDate
                        AND kau-entry.kau-id    BEGINS "��������"
                        AND kau-entry.op-status GE CHR(251)
                        AND CAN-DO(mUsDprIDLst,kau-entry.kau)
       NO-LOCK,
   FIRST op OF kau-entry WHERE
                NOT CAN-DO(iDocType,op.doc-type) NO-LOCK.

      IF kau-entry.Currency EQ "" THEN
         ASSIGN
            vSumm    = kau-entry.amt-rub
            vSummRub = vSumm
          .
      ELSE
      DO:
         vRate = FindRateSimple("����",
                                kau-entry.currency,
                                mCuDate).         
         ASSIGN
            vSumm    = kau-entry.amt-cur
            vSummRub = kau-entry.amt-cur * vRate
         .
      END.

      FIND FIRST ttBookReg 
         WHERE ttBookReg.acct     EQ kau-entry.acct
           AND ttBookReg.Currency EQ kau-entry.currency
      NO-LOCK NO-ERROR.

      IF AVAIL ttBookReg THEN
         IF kau-entry.debit THEN
            ASSIGN
               ttBookReg.Balance    = ttBookReg.Balance    - vSumm    /* ���⮪ � ���. */  
               ttBookReg.SummDb     = ttBookReg.SummDb     - vSumm    /* ��室 � ���. */    
               ttBookReg.BalanceRub = ttBookReg.BalanceRub - vSummRub /* ���⮪ � �� */    
               ttBookReg.SummDbRub  = ttBookReg.SummDbRub  - vSummRub /* ��室 � �� */     
            .                                                        
         ELSE                                                        
            ASSIGN                                                   
               ttBookReg.Balance    = ttBookReg.Balance    + vSumm    /* ���⮪ � ���. */  
               ttBookReg.SummCr     = ttBookReg.SummCr     - vSumm    /* ��室 � ���. */    
               ttBookReg.BalanceRub = ttBookReg.BalanceRub + vSummRub /* ���⮪ � �� */    
               ttBookReg.SummCrRub  = ttBookReg.SummCrRub  - vSummRub /* ��室 � �� */     
            .
   END. /* FOR EACH kau-entry */
END PROCEDURE. /* EditRecInTemp-Table */

PROCEDURE PrintFioAndPostV1.
   DEFINE INPUT PARAMETER iFio    AS CHARACTER NO-UNDO. /* ��� ���஥ ���⠥�               */
   DEFINE INPUT PARAMETER iPost   AS CHARACTER NO-UNDO. /* ��������� ������ ���⠥�         */
   DEFINE INPUT PARAMETER iOtstup AS INT64     NO-UNDO. /* ������ �� ���, �᫨ �� �㦥� � 0 */
   
   IF iFio  EQ "" THEN iFio  = FILL('_', 15).
   IF iPost EQ "" THEN iPost = FILL('_', 15).

   RUN PrintFioAndPost31(iFio,
                        iPost,
                        iOtstup,
                        "������������ ��������,��筠� �������,䠬���� � ���樠��"
                        ).

END PROCEDURE.


PROCEDURE PrintFioAndPost31.

   DEFINE INPUT PARAMETER iFio    AS CHARACTER NO-UNDO. /* ��� ���஥ ���⠥�               */
   DEFINE INPUT PARAMETER iPost   AS CHARACTER NO-UNDO. /* ��������� ������ ���⠥�         */
   DEFINE INPUT PARAMETER iOtstup AS INT64     NO-UNDO. /* ������ �� ���, �᫨ �� �㦥� � 0 */
   DEFINE INPUT PARAMETER iText   AS CHARACTER NO-UNDO.

   DEFINE VARIABLE vPost          AS CHARACTER NO-UNDO EXTENT 7.
   DEFINE VARIABLE vSignLength    AS INT64     NO-UNDO.
   DEFINE VARIABLE i              AS INT64     NO-UNDO.

   vSignLength    = MAX(25,LENGTH(ENTRY(1,iText)),LENGTH(ENTRY(2,iText)),LENGTH(ENTRY(3,iText))).
   mMaxLengthPost = 40.
   vPost[1] = iPost.
   {wordwrap.i
       &s = vPost
       &n = 7 
       &l = mMaxLengthPost
   }
   PUT UNFORMATTED SKIP.
   DO i = 1 TO 7:
      IF vPost[i] GT "" OR i EQ 1 THEN DO:
         PUT UNFORMATTED SPACE(iOtstup).
         IF i EQ 1 THEN 
            PUT UNFORMATTED PADR(ENTRY(1,iText),vSignLength).
         ELSE
            PUT UNFORMATTED SPACE(vSignLength).
         PUT UNFORMATTED SPACE(3) vPost[i] SKIP.
      END.
   END.

   PUT UNFORMATTED
      SKIP(1)
      SPACE(iOtstup) PADR(ENTRY(2,iText),vSignLength) SPACE(3) FILL('_', 15)
      SKIP
      SPACE(iOtstup) PADR(ENTRY(3,iText),vSignLength) SPACE(3) iFio
      SKIP(1).
END PROCEDURE.
