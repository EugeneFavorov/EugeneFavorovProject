/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2003 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: LGARTERM.P
      Comment: �����㬥��� ��� ���� ����⮢ ���ᯥ祭��
   Parameters: 
         Uses:
      Used by: 
      Created: 22/08/2003 ���� (0007460)
     Modified: 17/04/2008 jadv  (0078661)
     Modified:

���㬬�    - �㬬�
�������   - �����
����⠇��� - ��� �����祭��
����⠎��� - ��� ����砭��
����⠂�  - ��� �����

*/

/*
	aa4 - ������� �ଠ� ��� "�������"
	aa4 - �������� �� "�����1" - 27.11.2013 
*/




{globals.i}             /* �������� ��६���� ��ᨨ. */
{svarloan.def}          /* Shared ��६���� ����� "�।��� � ��������". */
{client.i}          /* ��ନ஢���� ����� ������ */
{norm.i}
{wordwrap.def}
{intrface.get xclass}
{intrface.get cust}
{intrface.get i254}
{sh-defs.i} /* ��� ���-�� acct-pos */
{loan.pro} /* ��� ��楤��� ����祭�� ���祭�� ⠡���� term-obl */ 

DEF OUTPUT PARAM iResult AS DECIMAL NO-UNDO.
DEF INPUT  PARAM iDate1  AS DATE    NO-UNDO.
DEF INPUT  PARAM iDate   AS DATE    NO-UNDO.
DEF INPUT  PARAM iStrPar AS CHAR    NO-UNDO.

DEF VAR mParam    AS CHAR  NO-UNDO.
DEF VAR mPutStr   AS CHAR  NO-UNDO.
DEF VAR mSummaStr AS CHAR  NO-UNDO.
DEF VAR mCurrStr  AS CHAR  NO-UNDO.
DEF VAR mDate     AS DATE  NO-UNDO.
DEF VAR mCounter  AS INT64   NO-UNDO.
DEF VAR mLength   AS INT64   NO-UNDO.
DEF VAR mInd      AS INT64   NO-UNDO.
DEF VAR mInd2     AS CHAR  NO-UNDO.
DEF VAR mTmpStr   AS CHAR  NO-UNDO.
DEF VAR mSrcChar  AS CHAR  NO-UNDO.
DEF VAR mRecId    AS RECID NO-UNDO. /* RECID term */
DEF VAR vTmpRec   AS RECID NO-UNDO.
DEF VAR vCountInt AS INT64   NO-UNDO. /* ���쪮 ��� ���稪 */
DEF VAR mSort     AS CHAR  NO-UNDO.
DEF VAR mSurr     AS CHAR  NO-UNDO.
DEF VAR mRecidStr AS CHAR  NO-UNDO.
DEF VAR vFormat   AS CHAR  NO-UNDO.
DEF VAR vCurrStr  AS CHAR  NO-UNDO.
DEF VAR vCurrSrt  AS CHAR  NO-UNDO.
DEF VAR digit     AS INT64 EXTENT 12 NO-UNDO.
DEF VAR vCounStr  AS INT64 NO-UNDO.
DEF VAR vStinstr  AS CHAR NO-UNDO.
DEF VAR mTemp-Str AS CHAR NO-UNDO. /*ᯨ᮪ ᯮᮡ�� �業��*/
DEF VAR mExtStr   AS CHAR EXTENT 52 NO-UNDO.
DEF VAR mParamId  AS INT64 INIT 0 NO-UNDO.
DEF VAR vCountTo  AS INT64 NO-UNDO INIT 0.
DEF VAR logTrim   AS LOG  NO-UNDO INIT NO.
DEF VAR chTrim    AS CHAR NO-UNDO.
DEF VAR iIndex    AS INT64  NO-UNDO.
DEF VAR vName     AS CHAR NO-UNDO EXTENT 2. /* �ᯮ������ ��� �ନ஢���� ������������ ��� */
DEF VAR vDRCode   AS CHAR NO-UNDO. /* ��� �� */
DEF VAR vPhone    AS CHAR NO-UNDO.

DEFINE TEMP-TABLE t-sort  /* ��� ("���稪|*") */
  FIELD order  AS CHAR
  FIELD rec-id AS RECID
   INDEX order IS UNIQUE PRIMARY order
.

DEF NEW GLOBAL SHARED VAR sTermRecid AS RECID NO-UNDO.
DEF NEW GLOBAL SHARED VAR sStr       AS CHAR  NO-UNDO. /*��� ���४⭮� ࠡ��� lgarterm �� ������ ������*/

DEF BUFFER loan      FOR loan.
DEF BUFFER term-obl  FOR term-obl.
DEF BUFFER loan-cond FOR loan-cond.
DEF BUFFER currency  FOR currency.

FUNCTION TrimFormula RETURNS CHAR(INPUT FormatTrim AS CHAR, INPUT cValue AS CHAR) FORWARD.

/* ��� ���� ������������ �����*/
&GLOB mMonths "ﭢ���,䥢ࠫ�,����,��५�,���,���,~
���,������,ᥭ����,������,�����,�������"

/* ��� �㭪樨 Trim - �१���� �� ������ �஡���� */
FUNCTION TrimFormula RETURNS CHAR(INPUT FormatTrim AS CHAR, INPUT cValue AS CHAR) :
   CASE FormatTrim:
      WHEN "trim"  THEN cValue = TRIM(cValue).
      WHEN "ltrim" THEN cValue = LEFT-TRIM(cValue).
      WHEN "rtrim" THEN cValue = RIGHT-TRIM(cValue).
   END CASE.
   RETURN cValue.
END.

printres = NO. /*�� �뢮���� ���祭�� xrezult */

   /*MESSAGE  'lgarterm.p ��ࠬ���'  iStrPar SKIP 'sTermRecid' sTermRecid
      VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

IF NUM-ENTRIES(iStrPar) GE 1 THEN DO:
   mInd = INDEX(iStrPar,"###") .
   IF mInd NE 0 THEN DO:   
      /* ��᫥���� � ᯨ᪥ ��ࠬ��஢ ��।�� recid(term-obl) : */
      mParamId = INT64( SUBSTRING( ENTRY (NUM-ENTRIES(iStrPar), iStrPar), 4) ) .
      IF mParamId GT 0 THEN
         mRecId = mParamId .  /* recid(term-obl) */
      /* 㤠����� recid(term-obl) �� ᯨ᪠ ��ࠬ��஢ : */
      iStrPar = SUBSTRING(iStrPar, 1, mInd - 2) .
   END.
END.

IF sTermRecid EQ ? THEN
   RETURN ''.
   
/* �饬 ��魮�� */
FIND FIRST term-obl WHERE
     RECID(term-obl)     = mRecId
NO-LOCK NO-ERROR.

/*IF AVAIL term-obl  THEN
   MESSAGE iStrPar sTermRecid
      VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

FIND FIRST loan WHERE
      loan.contract   = term-obl.contract AND
      loan.cont-code  = term-obl.cont-code
     /* RECID(loan) = rid-p */
NO-LOCK NO-ERROR.

FIND FIRST loan-cond WHERE
     RECID(loan-cond) = rid-t
NO-LOCK NO-ERROR.

DO TRANSACTION ON ENDKEY UNDO, LEAVE
               ON ERROR  UNDO, LEAVE:

   mParam = ENTRY(1,iStrPar).

   IF mParam MATCHES "*trim*" THEN 
   DO:
      logTrim = YES.
      iIndex  = INDEX(iStrPar,",").
      chTrim  = SUBSTRING(iStrPar,1,iIndex - 1).
      mParam  = SUBSTRING(iStrPar,iIndex + 1,LENGTH(iStrPar) - iIndex).
   END.

   /*
   �㬬� ���ᯥ祭��, �뢮��� �㬬� � �ଠ� "->>>,>>>,>>>,>>9.99" ���
   � �ଠ� 㪠����� 2 ��ࠬ��஬ �-樨 + � ᪮���� �뢮����� ��ப��
   */

   IF mParam MATCHES "���㬬�*" THEN
   DO:
      RUN "x-amtstr.p" (term-obl.amt,
                        term-obl.currency,
                        YES,
                        YES,
                        OUTPUT mSummaStr,
                        OUTPUT mCurrStr).
      mPutStr = STRING(term-obl.amt-rub,"->>>,>>>,>>>,>>9.99") + " (" + TRIM(mSummaStr) + " " + TRIM(mCurrStr) + ")".

      /*mPutStr = (IF NUM-ENTRIES(iStrPar,"|") = 1 THEN
                    STRING(term-obl.amt-rub,"->>>,>>>,>>>,>>9.99")
                 ELSE STRING(term-obl.amt-rub,ENTRY(2,iStrPar,"|"))
                ) + " (" + TRIM(mSummaStr) + " " + TRIM(mCurrStr) + ")".*/

      PUT STREAM Fil UNFORMATTED (IF logTrim 
                                     THEN TrimFormula(chTrim,mPutStr)
                                     ELSE mPutStr).

   END.

/* 
�業�� �࠭ᯮ�⭮�� �।�⢠ ��� �।��� ������
�뢮��� 業� ������ � �ଠ� "->>>,>>>,>>>,>>9.99" ���
� �ଠ� 㪠����� 2 ��ࠬ��஬ �-樨 + � ᪮���� �뢮����� ��ப��
*/
   IF mParam MATCHES "���㬬��*" THEN
   DO:
      RUN "x-amtstr.p" (term-obl.amt,
                        term-obl.currency,
                        NO,
                        YES,
                        OUTPUT mSummaStr,
                        OUTPUT mCurrStr).

    FIND FIRST currency WHERE currency.currency EQ term-obl.currency NO-LOCK NO-ERROR.

    vStinstr = STRING(IF term-obl.amt GE 0 THEN term-obl.amt ELSE (- term-obl.amt), "999999999999.99").
    DO vCounStr = 1 TO 12:
       digit[vCounStr] = INT64(SUBSTR(vStinstr, 13 - vCounStr, 1)).
    END.

      vCurrStr = (IF digit[1] = 1 AND digit[2] <> 1                    THEN IF AVAIL(currency) THEN currency.curr-form1 ELSE "???"
                  ELSE IF digit[2] = 1 OR digit[1] > 4 OR digit[1] = 0 THEN IF AVAIL(currency) THEN currency.curr-form5 ELSE "???"
                  ELSE                                                      IF AVAIL(currency) THEN currency.curr-form2 ELSE "???" ).
                  
      mPutStr = (IF NUM-ENTRIES(iStrPar,'|') = 1 THEN
                    STRING(term-obl.amt-rub,"->>>,>>>,>>>,>>9.99")
                 ELSE
                    STRING(term-obl.amt-rub,ENTRY(2,iStrPar,"|"))
					) + " (" + TRIM(mSummaStr) + " " + TRIM(vCurrStr) + " " + TRIM(mCurrStr) + ")".
                
      PUT STREAM Fil UNFORMATTED (IF logTrim 
                                     THEN TrimFormula(chTrim,mPutStr)
                                     ELSE mPutStr).

   END.
   /*
   ����� ���ᯥ祭��
   */
   IF mParam = "�������" THEN
   DO:
      IF term-obl.currency = "" THEN
         mPutStr = " � {&in-LP-C6} ".
      ELSE
      DO:
         FIND FIRST currency WHERE
                    currency.currency = term-obl.currency
         NO-LOCK NO-ERROR.
         mPutStr = "� �����: " + currency.name-currenc.
      END.
      PUT STREAM Fil UNFORMATTED (IF logTrim 
                                     THEN TrimFormula(chTrim,STRING(mPutStr))
                                     ELSE STRING(mPutStr)).
   END.

   /*
   ��� �����祭��/����砭��/�����
    �ਬ��� �ᯮ�짮�����
      [lgarterm(����⠇���)]            - ��� �����祭�� �ଠ� 99/99/9999
                                          �� 㬮�砭��
      [lgarterm(����⠇���,��)]         - ���  �����祭��
                                          (�᫮ + ����� �ய���� + ���)
      [lgarterm(����⠇���,99.99.9999)] - ��� �����祭�� � �ଠ� 2 ��ࠬ���
   */

   IF mParam = "����⠇���" OR
      mParam = "����⠎���" OR
      mParam = "����⠂�"  THEN
   DO:
      ASSIGN
         mDate = IF mParam = "����⠇���" THEN
                    term-obl.fop-date
                 ELSE
                 IF mParam = "����⠎���" THEN
                    term-obl.end-date
                 ELSE
                    term-obl.sop-date

         mPutStr = IF NUM-ENTRIES(iStrPar) = 1 THEN
                      STRING(mDate,"99/99/9999")
                   ELSE
                   IF ENTRY(2,iStrPar) = "��" THEN
                      STRING(DAY(mDate)) + " "
                       + ENTRY(MONTH(mDate),{&mMonths})
                       + STRING(YEAR(mDate), " 9999 �.")
                   ELSE
                      STRING(mDate,ENTRY(2,iStrPar))
         .

      PUT STREAM Fil UNFORMATTED (IF logTrim 
                                     THEN TrimFormula(chTrim,mPutStr)
                                     ELSE mPutStr).
   END.
   /* ���⪮� ������������ ������ */
   IF mParam = "��������" THEN
   DO:
      mPutStr = "".
      RUN RE_CLIENT(term-obl.symbol,
                    term-obl.fop,
                    INPUT-OUTPUT mPutStr).
      PUT STREAM Fil UNFORMATTED (IF logTrim 
                                     THEN TrimFormula(chTrim,mPutStr)
                                     ELSE mPutStr).
   END.
   
   /* ������ ������������ ������ */
   IF mParam = "��������" THEN
   DO:
      mPutStr = "".
      RUN RE_CLIENT_FULL(term-obl.symbol,
                    term-obl.fop,
                    INPUT-OUTPUT mPutStr).
      PUT STREAM Fil UNFORMATTED (IF logTrim 
                                     THEN TrimFormula(chTrim,mPutStr)
                                     ELSE mPutStr).
   END.
   /* ������ ������������ ����� �� ������ */
   IF mParam = "����������" THEN
   DO:
      mPutStr = "".
          IF term-obl.symbol EQ "�" THEN DO:
             FIND FIRST cust-corp WHERE cust-corp.cust-id EQ term-obl.fop NO-LOCK NO-ERROR.
             IF AVAIL(cust-corp) THEN DO:
                IF {assigned cust-corp.cust-stat} THEN DO:
					FIND code WHERE code.val EQ cust-corp.cust-stat
					AND code.class EQ "����।�".
					mPutStr = code.name.
				END.
                  /* mPutStr = GetCodeVal("����।�", cust-corp.cust-stat). */
                IF mPutStr = ? THEN mPutStr = "".
             END.
          END.
      PUT STREAM Fil UNFORMATTED (IF logTrim 
                                     THEN TrimFormula(chTrim,mPutStr)
                                     ELSE mPutStr).
   END.

/* IF mParam EQ "�������|���" THEN    - ⨯ ���㬥�� �����⥫� */
/* IF mParam EQ "�������|�����" THEN  - ����� ���㬥�� �����⥫� */
/* IF mParam EQ "�������|�뤠�" THEN  - ��� � ����� �뤠� ���㬥�� �����⥫� */
/* IF mParam EQ "�������" THEN        - ���� �����⥫� */
/* IF mParam matches "������|<��� ��>" THEN  - ���.४����� �����⥫� */
/* IF mParam EQ "�������" THEN               - ��� �����⥫� */
/* IF mParam matches "����|<��� ��>" THEN    - ���. ४����� ����騪�*/
/* IF mParam EQ "�����" THEN                 - ��� ����騪�*/

   /* ��ᯮ��� ����� �����⥫� (䨧��᪮�� ���) : */
   /* ⨯ ���㬥�� �����⥫� : */
   IF mParam EQ "�������|���" THEN
      IF term-obl.symbol EQ "�" THEN
      DO :
         FIND FIRST person WHERE person.person-id EQ term-obl.fop
            NO-LOCK NO-ERROR .
         IF AVAILABLE person THEN DO :
            mPutStr = person.document-id .
            PUT STREAM Fil UNFORMATTED (IF logTrim 
                                           THEN TrimFormula(chTrim,mPutStr)
                                           ELSE mPutStr).
         END .
      END .
   /* ����� ���㬥�� �����⥫� : */
   IF mParam EQ "�������|�����" THEN
      IF term-obl.symbol EQ "�" THEN
      DO :
         FIND FIRST person WHERE person.person-id EQ term-obl.fop
            NO-LOCK NO-ERROR .
         IF AVAILABLE person THEN DO :
            mPutStr = person.document .
            PUT STREAM Fil UNFORMATTED (IF logTrim 
                                           THEN TrimFormula(chTrim,mPutStr)
                                           ELSE mPutStr).
         END .
      END .
   /* ��� � ����� �뤠� ���㬥�� �����⥫� : */
   IF mParam EQ "�������|�뤠�" THEN
      IF term-obl.symbol EQ "�" THEN
      DO :
         FIND FIRST person WHERE person.person-id EQ term-obl.fop
            NO-LOCK NO-ERROR .
         IF AVAILABLE person THEN DO :
                 mPutStr = REPLACE(fGetDocIssue(person.person-id),",",", ��� ���ࠧ������� ").
				 SUBSTRING(mPutStr,LENGTH(mPutStr, "CHARACTER") - 10 ,1,"CHARACTER") = ", ".
				 mPutStr = REPLACE(mPutStr,"/",".").
            PUT STREAM Fil UNFORMATTED (IF logTrim 
                                           THEN TrimFormula(chTrim,mPutStr)
                                           ELSE mPutStr).
         END .
      END .

   /* ���� �����⥫� : */
   IF mParam MATCHES "�������*" THEN DO :                   /* aa4 - ������� �ଠ� ���� */
      IF term-obl.symbol EQ "�" THEN
      DO :
         FIND FIRST person WHERE person.person-id EQ term-obl.fop
            NO-LOCK NO-ERROR .
         IF AVAILABLE person THEN DO :
            mPutStr = "".
			IF NUM-ENTRIES(mParam,"|")	= 1 THEN DO :
				RUN RetAdr.p(person.person-id,  "�", "����ய", ?, OUTPUT mPutStr).
			END.
			IF NUM-ENTRIES(mParam,"|")	> 1 THEN DO:
				RUN RetAdr.p(person.person-id,  "�", "����ய", ?, OUTPUT mPutStr).
				IF NOT RetString THEN
					PUT STREAM fil UNFORMATTED TRIM(mPutStr,",").
				ELSE
					RETURN TRIM(mPutStr,",").
			END.
			mTmpStr = mPutStr.
			mPutStr = "".
			DO mCounter = 2 TO 8 :
				IF TRIM(ENTRY(mCounter,mTmpStr)) NE "" THEN
					mPutStr = mPutStr + ENTRY(mCounter, mTmpStr) + ",".
			END.
            mPutStr = SUBSTR(mPutStr,1,LENGTH(mPutStr) - 1) . /* delete last comma */
            PUT STREAM Fil UNFORMATTED (IF logTrim 
                                           THEN TrimFormula(chTrim,mPutStr)
                                           ELSE mPutStr).
         END.
      END.
      IF term-obl.symbol EQ "�" THEN
      DO :
         FIND FIRST cust-corp WHERE cust-corp.cust-id EQ term-obl.fop
            NO-LOCK NO-ERROR.
		 IF AVAILABLE cust-corp THEN DO:
			IF NUM-ENTRIES(mParam,"|")	= 1 THEN DO :
				RUN RetAdr.p(cust-corp.cust-id,  "�", "�����", ?, OUTPUT mPutStr).
				PUT STREAM Fil UNFORMATTED (IF logTrim 
                                           THEN TrimFormula(chTrim,mPutStr)
                                           ELSE mPutStr).
			END.
			IF NUM-ENTRIES(mParam,"|")	> 1 THEN DO:
				RUN RetAdr.p(cust-corp.cust-id,  "�", "�����", ?, OUTPUT mPutStr).
				IF NOT RetString THEN
					PUT STREAM fil UNFORMATTED TRIM(mPutStr,",").
				ELSE
					RETURN TRIM(mPutStr,",").
			END.
		 END.
		 
      END.
      IF term-obl.symbol EQ "�" THEN
      DO :
         FIND FIRST banks WHERE banks.bank-id EQ term-obl.fop
            NO-LOCK NO-ERROR .
         IF AVAILABLE banks THEN DO :
            mPutStr = banks.law-address .
            PUT STREAM Fil UNFORMATTED (IF logTrim 
                                           THEN TrimFormula(chTrim,mPutStr)
                                           ELSE mPutStr).
         END .
      END .
   END.

  
   /* ���.४����� �����⥫� : */
   IF mParam MATCHES ("������|*") THEN
   DO:
      mSrcChar = ENTRY(2,mParam,"|") . /* ��� �� */

      IF term-obl.symbol EQ "�" THEN
         mPutStr = GetXAttrValueEx("banks",STRING(term-obl.fop),mSrcChar,"").
      ELSE IF term-obl.symbol EQ "�" THEN
         mPutStr = GetXAttrValueEx("cust-corp",STRING(term-obl.fop),mSrcChar,"").
      ELSE IF term-obl.symbol EQ "�" THEN DO:
		IF mParam MATCHES ("������|phone-home")
		OR mParam MATCHES ("������|cell-phone") THEN DO:
			FIND FIRST person WHERE person.person-id EQ term-obl.fop NO-LOCK NO-ERROR.
			IF AVAILABLE person THEN DO:
				IF person.phone[1] NE "," THEN mPutStr = mPutStr + person.phone[1].
				IF person.phone[2] NE "," THEN mPutStr = mPutStr + person.phone[2].
			END.
		END.
		ELSE IF mParam MATCHES ("������|birthday") THEN DO:
			FIND FIRST person WHERE person.person-id EQ term-obl.fop NO-LOCK NO-ERROR.
			IF AVAILABLE person THEN
				IF person.birthday NE ? THEN mPutStr = STRING(person.birthday).
        END.
		mPutStr = mPutStr + GetXAttrValueEx("person",STRING(term-obl.fop),mSrcChar,"").	 
	  END.
      PUT STREAM Fil UNFORMATTED (IF logTrim 
                                     THEN TrimFormula(chTrim,mPutStr)
                                     ELSE mPutStr).
   END.

   /* ���.४����� ����騪� : */
    IF mParam MATCHES ("����|*") THEN
   DO:
      mSrcChar = ENTRY(2,mParam,"|"). 

      IF loan.cust-cat EQ "�" THEN
         mPutStr = GetXAttrValueEx("cust-corp",STRING(loan.cust-id),mSrcChar,"").
      ELSE IF loan.cust-cat EQ "�" THEN
         mPutStr = GetXAttrValueEx("person",STRING(loan.cust-id),mSrcChar,"").
      ELSE IF loan.cust-cat EQ "�" THEN
         mPutStr = GetXAttrValueEx("banks",STRING(loan.cust-id),mSrcChar,"").

      PUT STREAM Fil UNFORMATTED (IF logTrim 
                                     THEN TrimFormula(chTrim,mPutStr)
                                     ELSE mPutStr).
   END.

   /* ��� �����⥫� : */
   IF mParam MATCHES ("�������") THEN
   DO:
      IF term-obl.symbol EQ "�" THEN
      DO :
         FIND FIRST person WHERE person.person-id EQ term-obl.fop
            NO-LOCK NO-ERROR .
         IF AVAILABLE person THEN DO :
            mPutStr = person.inn .
            PUT STREAM Fil UNFORMATTED (IF logTrim 
                                           THEN TrimFormula(chTrim,mPutStr)
                                           ELSE mPutStr).
         END .
      END .
      ELSE IF term-obl.symbol EQ "�" THEN
      DO :
         FIND FIRST cust-corp WHERE cust-corp.cust-id EQ term-obl.fop
            NO-LOCK NO-ERROR .
         IF AVAILABLE cust-corp THEN DO :
            mPutStr = cust-corp.inn .
            PUT STREAM Fil UNFORMATTED (IF logTrim 
                                           THEN TrimFormula(chTrim,mPutStr)
                                           ELSE mPutStr).
         END .
      END .
      ELSE IF term-obl.symbol EQ "�" THEN
      DO :
         FIND FIRST banks WHERE banks.bank-id EQ term-obl.fop
            NO-LOCK NO-ERROR .
         IF AVAILABLE banks THEN DO :
            mPutStr = banks.inn .
            PUT STREAM Fil UNFORMATTED (IF logTrim 
                                           THEN TrimFormula(chTrim,mPutStr)
                                           ELSE mPutStr).
         END .
      END .
   END.

   /* ��� ����騪� : */
   IF mParam MATCHES ("�����") THEN
   DO:
      IF loan.cust-cat EQ "�" THEN
      DO :
         FIND FIRST person WHERE person.person-id EQ loan.cust-id
            NO-LOCK NO-ERROR .
         IF AVAILABLE person THEN DO :
            mPutStr = person.inn .
            PUT STREAM Fil UNFORMATTED (IF logTrim 
                                           THEN TrimFormula(chTrim,mPutStr)
                                           ELSE mPutStr).
         END .
      END .
      ELSE IF loan.cust-cat EQ "�" THEN
      DO :
         FIND FIRST cust-corp WHERE cust-corp.cust-id EQ loan.cust-id
            NO-LOCK NO-ERROR .
         IF AVAILABLE cust-corp THEN DO :
            mPutStr = cust-corp.inn .
            PUT STREAM Fil UNFORMATTED (IF logTrim 
                                           THEN TrimFormula(chTrim,mPutStr)
                                           ELSE mPutStr).
         END .
      END .
      ELSE IF loan.cust-cat EQ "�" THEN
      DO :
         FIND FIRST banks WHERE banks.bank-id EQ loan.cust-id
            NO-LOCK NO-ERROR .
         IF AVAILABLE banks THEN DO :
            mPutStr = banks.inn .
            PUT STREAM Fil UNFORMATTED (IF logTrim 
                                           THEN TrimFormula(chTrim,mPutStr)
                                           ELSE mPutStr).
         END .
      END .
   END.
   
   /*
   �����頥� �������⥫�� ४����� �� ��������
   */
   IF mParam MATCHES ("���|*") THEN
   DO:
      
      mPutStr = GetXAttrValueEx ("term-obl",
                                 Loan.Contract + "," + Loan.Cont-Code + ",5,"
                                  + STRING(term-obl.end-date) + ","
                                  + STRING(term-obl.nn),
                                 ENTRY(2, mParam, '|'),
                                 "").
      
      /*MESSAGE '���' Loan.Contract + "," + Loan.Cont-Code SKIP 'mPutStr' mPutStr
         VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
      IF NUM-ENTRIES(mParam,"|") EQ 4 AND
         ENTRY(3,mParam,"|") EQ "99.99.9999" THEN
         mPutStr = REPLACE(mPutStr,"/",".").

      IF mPutStr <> "" THEN
      DO:
         /* �᫨ �� ��।���� �� �����䨪���, � �� ���� �⮣�
            �����䨪��� �����頥� ��� ������������ */
         IF GetCodeName(ENTRY(2, mParam, '|'),mPutStr) <> ? THEN
            mPutStr = GetCodeName(ENTRY(2, mParam, '|'),mPutStr).
      END.

      mLength = IF NUM-ENTRIES(mParam,"|") = 3 THEN
                   INT64(ENTRY(2,mParam,"|"))
                ELSE
                   80.
      IF LENGTH(mPutStr) > mLength THEN
      DO:
         mExtStr[1] = mPutStr.
         {wordwrap.i
            &s = mExtStr
            &n = 52
            &l = mLength
         }
         mPutStr = mExtStr[1].

         PUT STREAM Fil UNFORMATTED (IF logTrim 
                                        THEN TrimFormula(chTrim,mPutStr)
                                        ELSE mPutStr).

         DO mCounter = 2 TO 52:

            IF mExtStr[mCounter] = "" THEN
                LEAVE.

            PUT STREAM Fil UNFORMATTED
               SKIP
               mExtStr[mCounter].
         END.
      END.
      ELSE
         PUT STREAM Fil UNFORMATTED (IF logTrim 
                                        THEN TrimFormula(chTrim,mPutStr)
                                        ELSE mPutStr).

   END.
   
   IF mParam MATCHES ("��⎡|*") THEN
   DO:
      mTmpStr = ENTRY(2,mParam,"|").
      
      ASSIGN
         mSrcChar  = GetXAttrValueEx("term-obl",
                                     loan.contract + "," + loan.cont-code + ",5,"
                                     + STRING(term-obl.end-date) + ","
                                     + STRING(term-obl.nn),
                                     "��������",
                                     "")
         mInd = INT64(GetXAttrValueEx ("term-obl",
                                     loan.contract + "," + loan.cont-code + ",5,"
                                     + STRING(term-obl.end-date) + ","
                                     + STRING(term-obl.nn),
                                     "�������",
                                     ?))
         .
      
      ASSIGN 
         mInd2 = IF mInd EQ ? OR mInd = 0 
                    THEN ""
                    ELSE string(mInd).
      
      FIND FIRST loan-acct WHERE
             loan-acct.contract  = term-obl.Contract
         AND loan-acct.cont-code = term-obl.Cont-Code
         AND loan-acct.acct-type = mSrcChar + string(mInd2)
        NO-LOCK NO-ERROR.

      IF NOT AVAIL loan-acct AND mTmpStr NE "����" THEN LEAVE.

      FIND FIRST acct WHERE
                 acct.acct     = loan-acct.acct
             AND acct.currency = loan-acct.currency
        NO-LOCK NO-ERROR.
      FIND FIRST code WHERE code.parent EQ "��������" AND
                            code.code EQ mSrcChar NO-LOCK NO-ERROR.
                           
      CASE mTmpStr:
          WHEN "����" THEN
              mPutStr = code.name.
          WHEN "���" THEN 
              mPutStr = delFilFromAcct(acct.acct).
          WHEN "�������" THEN
          DO:
             {getcust.i
                &name    = vName
                &Offinn  = {comment}
             }
             vName[1] = TRIM (vName[1] + " " + vName[2]).
             mPutStr = vName[1].
          END.
          WHEN "���" THEN
          DO:
              RUN acct-pos IN h_base (acct.acct,acct.currency,idate,idate,gop-status ).
              mPutStr = STRING(IF acct.currency EQ "" THEN sh-bal ELSE sh-val).
          END.
      END CASE.
      IF NUM-ENTRIES(mParam,'|') > 2 AND ENTRY(3,mParam,'|') NE "" THEN
      DO:           
         vFormat = ENTRY(3,mParam,'|').
         vFormat = REPLACE(vFormat,"_",",").
         IF mTmpStr EQ "���" THEN
            PUT STREAM fil UNFORMATTED (IF logTrim 
                                           THEN TrimFormula(chTrim,string(dec(mPutStr),vFormat))
                                           ELSE string(dec(mTmpStr),vFormat)).
         ELSE
         DO:
            vFormat = "x(" + vFormat + ")".
            PUT STREAM fil UNFORMATTED (IF logTrim 
                                           THEN TrimFormula(chTrim,string(mPutStr,vFormat))
                                           ELSE string(mTmpStr,vFormat)).
         END.
         mPutStr = "".
      END.
      PUT STREAM fil UNFORMATTED (IF logTrim 
                                     THEN TrimFormula(chTrim,STRING(mPutStr))
                                     ELSE STRING(mTmpStr)).
   END.

   IF mParam MATCHES ("���稪|*") THEN
   DO:
      mPutStr = ENTRY(2,mParam,"|").

      vTmpRec = RECID(term-obl).
      FOR EACH term-obl OF loan WHERE term-obl.idnt EQ 5:
         vCountTo = vCountTo + 1.
         mSurr = term-obl.contract + "," + term-obl.cont-code  + "," +
                    STRING (term-obl.idnt)     + "," +
                    STRING (term-obl.end-date) + "," +
                    STRING (term-obl.nn).

         CREATE t-sort.
         ASSIGN
            t-sort.order = STRING(vCountTo, "999")
            t-sort.rec-id = RECID(term-obl)
         .
      END.

      FOR EACH t-sort :
          vCountInt = vCountInt + 1.
          IF rec-id EQ vTmpRec THEN LEAVE.
      END.

      vCountInt = vCountInt - INT64(mPutStr) + 1.
      IF vCountInt >= 1 THEN
      PUT STREAM fil UNFORMATTED STRING(vCountInt).
   END.
   
   IF mParam MATCHES ("���") THEN
   DO:

      vTmpRec = RECID(term-obl).
      FOR EACH term-obl OF loan WHERE term-obl.idnt EQ 5:
         vCountTo = vCountTo + 1.
         mSurr = term-obl.contract + "," + term-obl.cont-code  + "," +
                    STRING (term-obl.idnt)     + "," +
                    STRING (term-obl.end-date) + "," +
                    STRING (term-obl.nn).

         CREATE t-sort.
         ASSIGN
            t-sort.order = STRING(vCountTo, "999")
            t-sort.rec-id = RECID(term-obl)
         .
      END.

      FOR EACH t-sort :
          vCountInt = vCountInt + 1.
          IF rec-id EQ vTmpRec THEN LEAVE.
      END.
	  
      mPutStr = STRING(vCountInt).
   END.   

   IF mParam MATCHES ("�������|*") THEN
   DO:
      mSrcChar = ENTRY(2,mParam,'|').

      CASE mSrcChar:
          WHEN "���業��" THEN DO:
             mTemp-Str = "".
             FOR EACH code WHERE code.class  = "�業������" AND
                                 code.parent = "�業������"
             NO-LOCK:
                IF mTemp-Str = ""
                THEN  mTemp-Str = code.name.
                ELSE  mTemp-Str = mTemp-Str + ',' + code.name.
             END.
             mTmpStr  = ENTRY(term-obl.fop-offbal + 1, mTemp-Str).

          END.
          WHEN "��⊠�" THEN DO:
             mTmpStr = Get_QualityGar ("comm-rate",
                                       loan.Contract + "," + 
                                       loan.Cont-Code + ",5," + 
                                       STRING(term-obl.end-date) + "," + 
                                       STRING(term-obl.nn), 
                                       iDate).
             IF    mTmpStr EQ ? 
                OR mTmpStr EQ "?"
             THEN 
                mTmpStr = "".
          END.
          WHEN "������⢮" THEN DO:
             mTmpStr = string(term-obl.sop-offbal).
          END.
      END CASE.

      IF NUM-ENTRIES(mParam,'|') > 2 THEN
      DO:
         vFormat = ENTRY(3,mParam,'|').
         IF mSrcChar EQ "������⢮" THEN 
         DO:
            vFormat = REPLACE(vFormat,"_",",").
            PUT STREAM fil UNFORMATTED (IF logTrim 
                                           THEN TrimFormula(chTrim,STRING(INT64(mTmpStr),vFormat))
                                           ELSE STRING(INT64(mTmpStr),vFormat)).
         END.
         ELSE
         DO:
            vFormat = "x(" + vFormat + ")".
            PUT STREAM fil UNFORMATTED (IF logTrim 
                                           THEN TrimFormula(chTrim,STRING(mTmpStr,vFormat))
                                           ELSE STRING(mTmpStr,vFormat)).
         END.
         mTmpStr = "".
      END.
      PUT STREAM fil UNFORMATTED (IF logTrim 
                                     THEN TrimFormula(chTrim,STRING(mTmpStr))
                                     ELSE STRING(mTmpStr)).
   END.
      /* ���ଠ�� �� ��������: (��ன ��ࠬ��� ��⠢��� - ࠧ����⥫� ;)
                                 1 ��ࠬ��� - ��� �� �� �������
                                 2 ��ࠬ��� - ��� ⥣� � ��
                                 (����易⥫��)
                                 3 ��ࠬ��� - ��� �����䨪���, ��� �࠭���� ���祭�� �� 㬮�砭�� 
                                 4 ��ࠬ��� - ��� ��ࠬ��� ���孥�� �஢��, �᫨ � �����䨪��� �࣠�������� ������
      */
   IF mParam MATCHES ("���ଠ���������|*;*") THEN
   DO:
      vDRCode = ENTRY(2, mParam, "|").
      CASE NUM-ENTRIES(vDRCode, ";"):
         WHEN 2 THEN
            RUN infoloan.p ((BUFFER term-obl:HANDLE),
                            ENTRY (1, vDRCode, ";"),
                            ENTRY (2, vDRCode, ";"),
                            "",
                            "",
                            OUTPUT mTmpStr
                            ).
         WHEN 3 THEN
            RUN infoloan.p ((BUFFER term-obl:HANDLE),
                            ENTRY (1, vDRCode, ";"),
                            ENTRY (2, vDRCode, ";"),
                            ENTRY (3, vDRCode, ";"),
                            "",
                            OUTPUT mTmpStr
                            ).
         WHEN 4 THEN
            RUN infoloan.p ((BUFFER term-obl:HANDLE),
                            ENTRY (1, vDRCode, ";"),
                            ENTRY (2, vDRCode, ";"),
                            ENTRY (3, vDRCode, ";"),
                            ENTRY (4, vDRCode, ";"),
                            OUTPUT mTmpStr
                            ).
      END CASE.
      mPutStr = mTmpStr.
      PUT STREAM fil UNFORMATTED mTmpStr.
      mTmpStr = "".
   END.
   /* ��� ������� ���ᯥ祭�� */
   IF mParam EQ "��������" THEN
   DO:
       mTmpStr = GetXAttrValueEx("term-obl",
                                  loan.contract + "," + loan.cont-code + ",5,"
                                  + STRING(term-obl.end-date) + ","
                                  + STRING(term-obl.nn),
                                  "��������",
                                  "").
       mTmpStr = GetCodeName ("��������", mTmpStr).
       mPutStr = mTmpStr.
       PUT STREAM fil UNFORMATTED mTmpStr.
       mTmpStr = "".
   END.
   /* ��� �।��� ���ᯥ祭�� */
   IF mParam EQ "�����" THEN
   DO:
       mTmpStr = GetXAttrValueEx("term-obl",
                                  loan.contract + "," + loan.cont-code + ",5,"
                                  + STRING(term-obl.end-date) + ","
                                  + STRING(term-obl.nn),
                                  "�����",
                                  "").
       mTmpStr = GetCodeName ("�����", mTmpStr).
       mPutStr = mTmpStr.
       PUT STREAM fil UNFORMATTED mTmpStr.
       mTmpStr = "".
   END.   
   IF mParam EQ "�����1" THEN
   DO:
	  mPutStr = "".
      FOR EACH term-obl WHERE term-obl.contract   EQ loan.contract
      AND term-obl.cont-code  EQ loan.cont-code
      AND term-obl.class-code EQ "term-obl-gar" 
      NO-LOCK:   
		   mTmpStr = GetXAttrValueEx("term-obl",
									  loan.contract + "," + loan.cont-code + ",5,"
									  + STRING(term-obl.end-date) + ","
									  + STRING(term-obl.nn),
									  "�����",
									  "").
		   mTmpStr = GetCodeName ("�����", mTmpStr).
		   
		   
		  RUN "x-amtstr.p" (term-obl.amt,
							term-obl.currency,
							NO,
							YES,
							OUTPUT mSummaStr,
							OUTPUT mCurrStr).
		  FIND FIRST currency WHERE currency.currency EQ term-obl.currency NO-LOCK NO-ERROR.
		  vStinstr = STRING(IF term-obl.amt GE 0 THEN term-obl.amt ELSE (- term-obl.amt), "999999999999.99").
		  DO vCounStr = 1 TO 12:
			 digit[vCounStr] = INT64(SUBSTR(vStinstr, 13 - vCounStr, 1)).
		  END.
		  vCurrStr = (IF digit[1] = 1 AND digit[2] <> 1                    THEN IF AVAIL(currency) THEN currency.curr-form1 ELSE "???"
					  ELSE IF digit[2] = 1 OR digit[1] > 4 OR digit[1] = 0 THEN IF AVAIL(currency) THEN currency.curr-form5 ELSE "???"
					  ELSE                                                      IF AVAIL(currency) THEN currency.curr-form2 ELSE "???" ).              
		  vCurrSrt = (IF NUM-ENTRIES(iStrPar,'|') = 1 THEN
						STRING(term-obl.amt-rub,"->>>,>>>,>>>,>>9.99")
					 ELSE
						STRING(term-obl.amt-rub,ENTRY(2,iStrPar,"|"))
						) + " (" + TRIM(mSummaStr) + " " + TRIM(vCurrStr) + " " + TRIM(mCurrStr) + ")".
                
		   
		   
		   IF mPutStr EQ "" THEN
			  mPutStr = mTmpStr + " �� �㬬� " + LEFT-TRIM(vCurrSrt).
		   ELSE
			  mPutStr = mPutStr + ". " + mTmpStr + " �� �㬬� " + LEFT-TRIM(vCurrSrt).
		   mTmpStr = "".
	  END.
	  PUT STREAM fil UNFORMATTED mTmpStr.
	END.
END.
{intrface.del}
RETURN mPutStr.