/*                          
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2000 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: LOANFORM.P
      Comment: ����� ��� ���� ���㬥�⮢ �� ����� "�।��� � �������".
   Parameters:
      Created: Om 15/02/00
     Modified: Om 31/10/00 ��᪫��� :) ᫮�� "��業�".
     Modified: Om 03/09/2001 "������" -0 ⥯��� ࠡ�⠥� �� ⮫쪮 � �ਪ���,
                             �� � � 䨧�����.
    Modified: 15/04/2003 ���� - �ନ�஢���� by Becom
                                  comm-rate.acct = "0"
                                  comm-rate.kau  = ���ண�� �������
    Modified: 26/12/2005 (0053859) �訡�� � LOANFORM.P �� ���� ���� 
                                   [⥫�䮭]
    Modified: 19/07/2007 muta 0075116  ������ १����� �뢮����� �� ⮫쪮 � ��⮪. 
                                       ��������� ��ࠡ�⪠ ���
                                        - ����������                                                
                                       - ������.����.��������
                                       - ��℮�����
                                       - ���������
                                       - ���த��������
                                       - �����.��ᐥ�.������
                                       - ���.��ᐥ�.���������
                                       - ��.������
    Modified: 19/07/2007 muta 0075117
    Modified: 17/04/2008 jadv 0078661
*/

FORM "~n@(#) loanform.p 1.0 Om 15/02/00"
   WITH FRAME sccs-id STREAM-IO WIDTH-CHARS 250.

{norm.i}
{globals.i}
{svarloan.def}          /* Shared ��६���� ����� "�।��� � ��������". */
{amtsclon.i}
{intrface.get loan}
{intrface.get comm}
{intrface.get cust}     /* ������⥪� ��� ࠡ��� � �����⠬�. */
{intrface.get xclass}
{intrface.get strng}
{client.i}

printres = NO.

&GLOB Months "ﭢ���,䥢ࠫ�,����,��५�,���,���,���,������,ᥭ����,~
������,�����,�������"

DEFINE OUTPUT PARAMETER Xresult AS DECIMAL   NO-UNDO.
DEFINE INPUT  PARAMETER Xdate1  AS DATE      NO-UNDO.
DEFINE INPUT  PARAMETER Xdate   AS DATE      NO-UNDO.
DEFINE INPUT  PARAMETER strpar  AS CHARACTER NO-UNDO.

DEF VAR command   AS CHAR NO-UNDO. /* ����室���� �㭪�� */
DEF VAR max_len   AS INT64  NO-UNDO. /* ���ᨬ��쭠� ����� ���� */
DEF VAR offset    AS INT64  NO-UNDO. /* ���饭�� �⭮�⥫쭮 ������ ��� */
DEF VAR mSignsVal AS CHAR NO-UNDO. /* ���祭�� ४����� �� cust-corp */
DEF VAR mCommRate AS DEC  NO-UNDO. /* % �⠢�� */
DEF VAR logTrim   AS LOG  NO-UNDO. /* �ਧ��� - ���� ������ trim ��� ���. ��� ��� �㭪権. */
DEF VAR chTrim    AS CHAR NO-UNDO. /* �ᯮ������ � trim (trim,ltrim,rtrim) */
DEF VAR iIndex    AS INT64  NO-UNDO. /* �ᯮ������ � trim */
DEF VAR mUnit     AS CHAR NO-UNDO.
DEF VAR mRetValue AS CHAR NO-UNDO.
DEF VAR mString   AS CHAR NO-UNDO. /* ��६����� ��� ����権 � ��ப�� */
DEF VAR is-ok     AS INT64  NO-UNDO.
DEF VAR mAddres   AS CHAR NO-UNDO.
DEF VAR mINN      AS CHAR NO-UNDO.
DEF VAR mKPP      AS CHAR NO-UNDO.
DEF VAR mType     AS CHAR NO-UNDO.
DEF VAR mCode     AS CHAR NO-UNDO.
DEF VAR mAcct     AS CHAR NO-UNDO.
DEF VAR mName     AS CHAR NO-UNDO.
DEF VAR mPhone    AS CHAR NO-UNDO.
DEF VAR mCommRateFixed AS LOGICAL NO-UNDO .
DEF VAR mRes      AS CHAR  NO-UNDO.
DEF VAR mIstStrah AS LOG   NO-UNDO.
DEF VAR mStrahComp AS CHAR NO-UNDO.
DEF VAR mDate   AS DATE NO-UNDO. 
DEF VAR mTemp     AS CHAR NO-UNDO. 
DEF VAR mTemp2    AS CHAR NO-UNDO.



DEFINE  VARIABLE out_str AS CHARACTER NO-UNDO. /* ���� �뢮�� �� ����� */

DEF SHARED VAR rid_loan  AS RECID.    /* ��� ��������� */
DEF BUFFER mort-loan  FOR loan.     /* ���������� ����. */
DEF BUFFER loan-obj   FOR loan.     /* ���������� ����. */
DEF BUFFER b-tmp-code FOR tmp-code. /* ���������� ����. */

/*-------------------------------------------------------------------------
** ��� �㭪樨 Trim - �१���� �� ������ �஡���� 
*/
FUNCTION TrimFormula RETURNS CHAR(
   INPUT logTrim    AS LOG, 
   INPUT FormatTrim AS CHAR, 
   INPUT cValue     AS CHAR
):
   IF logTrim THEN
   DO: 
      CASE FormatTrim:
         WHEN "trim"  THEN cValue = TRIM(cValue).
         WHEN "ltrim" THEN cValue = LEFT-TRIM(cValue).
         WHEN "rtrim" THEN cValue = RIGHT-TRIM(cValue).
      END CASE.
   END.
   RETURN cValue.
END.


FIND FIRST loan WHERE
      RECID(loan) EQ rid-p NO-LOCK NO-ERROR.
IF NOT AVAILABLE loan THEN
   IF NOT RetString THEN
   PUT STREAM fil "������� �� ������" FORM "x(18)".

{fndinsur.i}

FIND loan-cond WHERE
      RECID(loan-cond) EQ rid-t NO-LOCK NO-ERROR.

IF strpar MATCHES "*trim*" THEN 
   ASSIGN
      logTrim = YES
      iIndex  = INDEX(strpar,",")
      chTrim  = SUBSTRING(strpar,1,iIndex - 1)
      strpar  = SUBSTRING(strpar,iIndex + 1,LENGTH(strpar) - iIndex)
   .

ASSIGN
   COMMAND = ENTRY(1,STRPAR)
   max_len = INT64(IF NUM-ENTRIES(STRPAR) GE 2 THEN ENTRY(1,STRPAR) ELSE '0')
   offset  = INT64(IF NUM-ENTRIES(STRPAR) GE 3 THEN ENTRY(1,STRPAR) ELSE '0')
   .

/* if COMMAND eq "䨮��1" then COMMAND = "䨮��". */

CASE COMMAND:
   WHEN "��␠��" THEN
   DO:
      FIND LAST loan-acct OF loan WHERE
                loan-acct.acct-type EQ (IF loan.contract EQ "�।��" THEN
                                           "�।����"
                                        ELSE
                                           "�������")
            AND loan-acct.since     LE Xdate NO-LOCK NO-ERROR.

      IF NOT AVAILABLE loan-acct THEN
      DO:

         FIND FIRST cust-corp WHERE
                    cust-corp.cust-id EQ loan.cust-id NO-LOCK NO-ERROR.
         IF NOT AVAILABLE cust-corp THEN
            RETURN.

         FIND FIRST acct WHERE
                    acct.contract EQ "�����"
                AND acct.cust-cat EQ loan.cust-cat
                AND acct.cust-id  EQ loan.cust-id
                AND acct.currency EQ loan.currency
                AND acct.acct-cat EQ "b"
                AND (acct.close-date EQ ? OR
                     acct.close-date GT Xdate) NO-LOCK NO-ERROR.
      END.
      mRetValue = IF AVAIL loan-acct 
            THEN loan-acct.acct 
            ELSE IF AVAIL acct
                 THEN acct.acct
                               ELSE "".
      mRetValue = TRIM(ENTRY(1, mRetValue, "@")).
      IF NOT RetString THEN 
         PUT STREAM fil TrimFormula(logTrim, chTrim, mRetValue) FORM "x(20)".
      ELSE RETURN mRetValue.
   END.
   WHEN "⥫�䮭" THEN
   DO:
      IF loan.cust-cat EQ "�" THEN
      DO:
         FIND FIRST person WHERE
                    person.person-id EQ loan.cust-id NO-LOCK NO-ERROR.

         IF NOT RetString THEN
         PUT STREAM fil UNFORMATTED TrimFormula(logTrim, chTrim, 
            IF AVAIL person THEN (person.phone[1] + (IF person.phone[2] NE "" THEN ", " + person.phone[2] ELSE ""))
                            ELSE "������ �� ������").
         ELSE RETURN IF AVAILABLE person THEN
               (person.phone[1] + (IF person.phone[2] NE "" THEN
                                      ", " + person.phone[2]
                                   ELSE
                                      ""))
            ELSE "".
      END.
      ELSE
      DO:
         FIND FIRST cust-corp WHERE
                    cust-corp.cust-id EQ loan.cust-id NO-LOCK NO-ERROR.
         IF NOT AVAIL cust-corp THEN
            RETURN.

         mSignsVal = GetXAttrValueEx("cust-corp",
                                     STRING(cust-corp.cust-id),
                                     "tel",
                                     ?).
         IF NOT RetString THEN
         PUT STREAM fil UNFORMATTED TrimFormula(logTrim, chTrim, 
            IF mSignsVal <> ? THEN mSignsVal ELSE "�� ��।����").
         ELSE RETURN (IF mSignsVal <> ? THEN
               mSignsVal ELSE "").
      END.
   END.
   WHEN "䠪�" THEN
   DO:
      FIND FIRST cust-corp WHERE
                 cust-corp.cust-id EQ loan.cust-id NO-LOCK NO-ERROR.

      IF NOT RetString THEN
      PUT STREAM fil TrimFormula(logTrim, chTrim, 
         IF AVAIL cust-corp THEN cust-corp.fax
                            ELSE "������ �� ������") FORM "x(20)".
      ELSE RETURN (IF AVAILABLE cust-corp THEN
             cust-corp.fax
          ELSE "").
   END.
   WHEN "email" THEN
   DO:
    IF loan.cust-cat EQ "�" THEN
      DO:
         FIND FIRST person WHERE
                    person.person-id EQ loan.cust-id NO-LOCK NO-ERROR.

         IF NOT AVAIL person THEN
            RETURN.

         mSignsVal = GetXAttrValueEx("person",
                                     STRING(person.person-id),
                                     "e-mail",
                                     ?).
         IF NOT RetString THEN
         PUT STREAM fil UNFORMATTED TrimFormula(logTrim, chTrim, 
            IF mSignsVal <> ? THEN mSignsVal ELSE "�� ��।����").
         ELSE RETURN (IF mSignsVal <> ? THEN
               mSignsVal ELSE "").
      END.
      ELSE
      DO:
         FIND FIRST cust-corp WHERE
                    cust-corp.cust-id EQ loan.cust-id NO-LOCK NO-ERROR.
         IF NOT AVAIL cust-corp THEN
            RETURN.

         mSignsVal = GetXAttrValueEx("cust-corp",
                                     STRING(cust-corp.cust-id),
                                     "e-mail",
                                     ?).
         IF NOT RetString THEN
         PUT STREAM fil UNFORMATTED TrimFormula(logTrim, chTrim, 
            IF mSignsVal <> ? THEN mSignsVal ELSE "�� ��।����").
         ELSE RETURN (IF mSignsVal <> ? THEN
               mSignsVal ELSE "").
      END.
   END.
   WHEN "�����" THEN
   DO:
      IF loan.cust-cat EQ "�" THEN
      DO:
         FIND FIRST person WHERE
                    person.person-id EQ loan.cust-id NO-LOCK NO-ERROR.
         IF AVAIL person THEN
            RUN RetAdr.p(loan.cust-id,loan.cust-cat,"����ய",?,OUTPUT mRes).
         IF NOT RetString THEN
            PUT STREAM fil TrimFormula(logTrim, chTrim,
               IF AVAIL person THEN mRes
                               ELSE "������ �� ������") FORM "x(60)".
         ELSE
         DO:
            RETURN (IF AVAILABLE person THEN TRIM(mRes)
                    ELSE "").
         END.
      END.
      ELSE
         IF loan.cust-cat EQ "�" THEN
         DO:
            FIND FIRST cust-corp WHERE
                       cust-corp.cust-id EQ loan.cust-id NO-LOCK NO-ERROR.
            IF AVAIL cust-corp THEN
               RUN RetAdr.p(loan.cust-id,loan.cust-cat,"�����",?,OUTPUT mRes).
            IF NOT RetString THEN
               PUT STREAM fil TrimFormula(logTrim, chTrim,
                  IF AVAIL cust-corp THEN mRes
                                     ELSE "������ �� ������") FORM "x(60)".
            ELSE RETURN (IF AVAILABLE cust-corp THEN
                            TRIM(mRes)
                         ELSE "").
         END.
   END.
   WHEN "४�_�" THEN
   DO:
      FIND FIRST cust-corp WHERE
                 cust-corp.cust-id EQ loan.cust-id NO-LOCK NO-ERROR.

      IF NOT RetString THEN
         PUT STREAM fil TrimFormula(logTrim, chTrim, 
            IF AVAIL cust-corp THEN "��� " + TRIM(cust-corp.inn)
                               ELSE "") FORM "x(20)".
      ELSE RETURN (IF AVAILABLE cust-corp THEN
                      ("��� " + TRIM(cust-corp.inn))
                   ELSE "").

   END.
   WHEN "४�_�" THEN
   DO:
         /* �� ᤥ��� ⮫쪮 ��� �ਪ��, ������� �஢����, ���� �� 䨧���� ���� ����� ����� */
      IF loan.cust-cat NE "�" THEN
         RETURN.
      FIND LAST loan-acct OF loan WHERE
                loan-acct.acct-type EQ (IF loan.contract EQ "�।��" THEN
                                           "�।����"
                                        ELSE
                                           "�������")
            AND loan-acct.since     LE Xdate NO-LOCK NO-ERROR.

      IF AVAILABLE loan-acct THEN
         FIND FIRST acct OF loan-acct NO-LOCK NO-ERROR.

      FIND FIRST cust-corp WHERE
                 cust-corp.cust-id EQ loan.cust-id NO-LOCK NO-ERROR.
      IF NOT AVAILABLE cust-corp THEN
         RETURN.

      IF NOT AVAILABLE loan-acct OR NOT AVAILABLE acct THEN
      DO:
         FIND FIRST acct WHERE
                    acct.contract EQ "�����"
                AND acct.cust-cat EQ loan.cust-cat
                AND acct.cust-id  EQ loan.cust-id
                AND acct.currency EQ loan.currency
                AND acct.acct-cat EQ "b"
                AND (acct.close-date EQ ? OR
                     acct.close-date GT Xdate) NO-LOCK NO-ERROR.
      END.

      IF AVAILABLE acct THEN
      DO:
         {bank-id.i}

         FIND FIRST banks OF banks-corr WHERE
                    banks.flag-rkc NO-LOCK NO-ERROR.
         IF NOT AVAILABLE banks THEN
            RETURN.

         out_str = "�/� " + delFilFromAcct(acct.acct) + ", �/� " + delFilFromAcct(bank-acct) + "~n� " +
            TRIM(banks.name) + " �. " + banks.town + ", ��� " + bank-mfo-9.

         IF NOT RetString THEN
            PUT STREAM fil TrimFormula(logTrim, chTrim, out_str) FORM "x(130)".
         ELSE RETURN out_str.
      END.
      ELSE
      DO:
         IF cust-corp.bank-code-type NE "���" THEN
         DO:
            FIND banks-code WHERE
                 banks-code.bank-code-type EQ  cust-corp.bank-code-type
             AND banks-code.bank-code      EQ cust-corp.bank-code
            NO-LOCK NO-ERROR.

            IF NOT AVAILABLE banks-code THEN
               RETURN.

            FIND FIRST banks WHERE
                       banks.bank-id EQ banks-code.bank-id NO-LOCK NO-ERROR.
            IF NOT AVAILABLE banks THEN
               RETURN.
         END.
         ELSE
         DO:
            FIND FIRST cust-ident WHERE cust-ident.cust-cat       EQ "�"
                                  AND   cust-ident.cust-code-type EQ "���"
                                  AND   cust-ident.cust-code      EQ cust-corp.bank-code
               NO-LOCK NO-ERROR.
            IF NOT AVAIL cust-ident THEN
               RETURN.
            FIND FIRST banks WHERE banks.bank-id EQ cust-ident.cust-id NO-LOCK NO-ERROR.
            IF NOT AVAIL banks THEN
               RETURN.
         END.

         {getcode.i banks "���-9"}
         IF NOT AVAILABLE banks-code THEN
            RETURN.
         out_str = "�/� " + delFilFromAcct(cust-corp.benacct) + ", �/� " + delFilFromAcct(cust-corp.corr-acct)
            + " � " + TRIM(banks.name) + " �. " + banks.town + ",~n ��� " +
               banks-code.bank-code.

         IF NOT RetString THEN
            PUT STREAM fil TrimFormula(logTrim, chTrim, out_str) FORM "x(100)".
         ELSE RETURN out_str.
      END.
   END.
   WHEN "�����" THEN
   DO:
      FIND FIRST cust-corp WHERE
                 cust-corp.cust-id EQ loan.cust-id NO-LOCK NO-ERROR.
      IF NOT AVAIL cust-corp THEN
         RETURN.

      mSignsVal = GetXAttrValueEx("cust-corp",
                                  STRING(cust-corp.cust-id),
                                  "�����",
                                  ?).

      IF mSignsVal <> ? THEN DO:
         IF NOT RetString THEN
            PUT STREAM fil TrimFormula(logTrim, chTrim, mSignsVal) FORM "x(20)".
         ELSE RETURN TRIM(mSignsVal).
      END.
   END.
   WHEN "䨮��" THEN
   DO:
      FIND FIRST cust-corp WHERE
            cust-corp.cust-id EQ loan.cust-id NO-LOCK NO-ERROR.
      IF NOT AVAIL cust-corp THEN
         RETURN.

      mSignsVal = GetXAttrValueEx("cust-corp",
                                  STRING(cust-corp.cust-id),
                                  "�����",
                                  ?).

      IF mSignsVal <> ? THEN DO:
         IF NOT RetString THEN
            PUT STREAM fil TrimFormula(logTrim, chTrim, mSignsVal) FORM "x(32)".
         ELSE RETURN TRIM(mSignsVal).
      END.
   END.
   WHEN "�᭮��" THEN
   DO:
      FIND FIRST cust-corp WHERE
                 cust-corp.cust-id EQ loan.cust-id NO-LOCK NO-ERROR.
      IF NOT AVAIL cust-corp THEN
         RETURN.

      mSignsVal = GetXAttrValueEx("cust-corp",
                                  STRING(cust-corp.cust-id),
                                  "�᭮��",
                                  ?).

      IF mSignsVal <> ? THEN DO:
         IF NOT RetString THEN
            PUT STREAM fil TrimFormula(logTrim, chTrim, mSignsVal) FORM "x(32)".
         ELSE RETURN TRIM(mSignsVal).
      END.

   END.
   WHEN "����" THEN
   DO:
      RUN GetCustAdr("�",loan.cust-id,xdate,"�������",OUTPUT TABLE ttCustAddress).
      FIND LAST ttCustAddress NO-LOCK NO-ERROR.
      mSignsVal = IF AVAIL ttCustAddress THEN ttCustAddress.fAdrStr ELSE ?.

      IF mSignsVal <> ? THEN DO:
         IF NOT RetString THEN
            PUT STREAM fil TrimFormula(logTrim, chTrim, mSignsVal) FORM "x(32)".
         ELSE RETURN TRIM(mSignsVal).
      END.
   END.
   WHEN "䨮���" THEN
   DO:
      FIND FIRST cust-corp WHERE
                 cust-corp.cust-id EQ loan.cust-id NO-LOCK NO-ERROR.
      IF NOT AVAIL cust-corp THEN
         RETURN.

      mSignsVal = GetXAttrValueEx("cust-corp",
                                  STRING(cust-corp.cust-id),
                                  "䨮���",
                                  ?).

      IF mSignsVal <> ? THEN DO:
         IF NOT RetString THEN
            PUT STREAM fil TrimFormula(logTrim, chTrim, mSignsVal) FORM "x(32)".
         ELSE RETURN TRIM(mSignsVal).
      END.

   END.
   /*䠪��᪠� ��� �뤠� �।��*/
   WHEN "��⠢�" THEN
   DO:
      mSignsVal = STRING(loan.open-date, "99.99.9999").
      find first loan-int of loan 
           where loan-int.id-k = 3
             and loan-int.id-d = 0
             and loan-int.mdate >= loan.open-date
             and loan-int.amt-rub <> 0
      no-lock no-error.
      if avail loan-int then mSignsVal = STRING(loan-int.mdate, "99.99.9999").
      RETURN TRIM(mSignsVal).
   END.
   WHEN "���ᮣ�" THEN
   DO:
      mSignsVal = GetXAttrValueEx("loan",
                                  loan.contract + "," + loan.cont-code,
                                  "���ᮣ�",
                                  ?).

      IF mSignsVal <> ? THEN DO:
         IF NOT RetString THEN
            PUT STREAM fil TrimFormula(logTrim, chTrim, mSignsVal) FORM "x(10)".
         ELSE RETURN TRIM(mSignsVal).
      END.

   END.
   WHEN "���ᮣ�2" THEN
   DO:
      mSignsVal = GetXAttrValueEx("loan",
                                  loan.contract + "," + loan.cont-code,
                                  "���ᮣ�",
                                  ?).

      IF mSignsVal <> ? THEN DO:
         IF NOT RetString THEN
            PUT STREAM fil TrimFormula(logTrim, chTrim, mSignsVal) FORM "99.99.9999".
         ELSE RETURN TRIM(mSignsVal).
      END.

   END.
   /*��������� ⥣� "����ᮣ�" � "���ᮣ�" ��� �뢮�� ���� ᮣ��襭�� � �ଠ� 99 ����� 9999*/
   WHEN "����ᮣ�" THEN
   DO:
      mSignsVal = GetXAttrValueEx("loan",
                                  loan.contract + "," + loan.cont-code,
                                  "���ᮣ�",
                                  ?).

    IF mSignsVal <> ? THEN DO:

    mDate = DATE(mSignsVal).
      mSignsVal = STRING(DAY(mDate)).
    IF NOT RetString THEN
      PUT STREAM fil TrimFormula(logTrim, chTrim, mSignsVal) FORM "x(2)".
    ELSE RETURN TRIM(mSignsVal).
      END.

   END.
   WHEN "���ᮣ�" THEN
   DO:
      mSignsVal = GetXAttrValueEx("loan",
                                  loan.contract + "," + loan.cont-code,
                                  "���ᮣ�",
                                  ?).

    IF mSignsVal <> ? THEN DO:

    mDate = DATE(mSignsVal).
      mSignsVal = ENTRY(MONTH(mDate),{&Months}) + " " + STRING(YEAR(mDate)).
    IF NOT RetString THEN
      PUT STREAM fil TrimFormula(logTrim, chTrim, mSignsVal) FORM "x(10)".
    ELSE RETURN TRIM(mSignsVal).
      END.

   END.
   WHEN "�%����" THEN
   DO:
      IF NOT AVAILABLE loan-cond THEN
         RETURN.

      mCommRate = GET_COMM ("%����",
                            ?,
                            loan.currency,
                            loan.contract + "," + loan.cont-code,
                            0.0,
                            0,
                            loan-cond.since).
      mCommRateFixed = GET_COMM_TYPE ("%����",
                            ?,
                            loan.currency,
                            loan.contract + "," + loan.cont-code,
                            0.0,
                            0,
                            loan-cond.since).


      IF mCommRate <> ? THEN
      DO:
         RUN "amt.p" (mCommRate, OUTPUT out_str).
         out_str = out_str + "|".
         RUN persent ( mCommRate , loan.currency , INPUT-OUTPUT out_str).
         IF NOT RetString THEN
            PUT STREAM fil TrimFormula(logTrim, chTrim, STRING(mCommRate,">>99.99999") +
               "(" + ENTRY(1,out_str, "|") + ")" + ENTRY(2, out_str, "|")) FORMAT "x(60)".
         ELSE RETURN (STRING(mCommRate,">>99.99999") +
               "(" + ENTRY(1,out_str, "|") + ")" +
               ENTRY(2, out_str, "|")).
      END.
   END.
   WHEN "�%����%" THEN
   DO:
      IF NOT AVAILABLE loan-cond THEN
         RETURN.

      mCommRate = GET_COMM ("%����%",
                            ?,
                            loan.currency,
                            loan.contract + "," + loan.cont-code,
                            0.0,
                            0,
                            loan-cond.since).
      mCommRateFixed = GET_COMM_TYPE ("%����%",
                            ?,
                            loan.currency,
                            loan.contract + "," + loan.cont-code,
                            0.0,
                            0,
                            loan-cond.since).


      IF mCommRate <> ? THEN
      DO:
         RUN "amt.p" (mCommRate, OUTPUT out_str).
         RUN persent (mCommRate, mCommRateFixed , loan.currency ,  INPUT-OUTPUT out_str).
         IF NOT RetString THEN
            PUT STREAM fil TrimFormula(logTrim, chTrim, STRING(mCommRate,">>99.99999") +
               "(" + out_str + ")") FORMAT "x(64)".
            ELSE RETURN (STRING(mCommRate,">>99.99999") +
                     "(" + out_str + ")").
      END.
   END.
   WHEN "����������" THEN
   DO:
      IF NOT RetString THEN PUT STREAM fil TrimFormula(logTrim, chTrim, 
         IF loan.class-code EQ "insurance" AND loan.contract   EQ '�����' THEN loan.cont-code ELSE "").
      ELSE RETURN 
         (IF loan.class-code EQ "insurance" AND loan.contract   EQ '�����' THEN loan.cont-code ELSE "").

   END. 
   WHEN "������.����.��������" THEN
   DO:
      IF loan.class-code EQ "insurance" AND loan.contract   EQ '�����' THEN DO:

         mIstStrah = FGetSetting("������", "", "��") EQ "��".
         IF mIstStrah THEN
            RUN RE_CLIENT (loan.cust-cat,
                           loan.cust-id,
                           INPUT-OUTPUT mName).
         ELSE
            ASSIGN
               mStrahComp  = GetXAttrValueEx("loan",loan.contract + "," + loan.cont-code,"���客����","")
               mName       = GetCodeName('���客����', mStrahComp).
         IF NOT RetString THEN PUT STREAM fil TrimFormula(logTrim, chTrim, mName).
         ELSE RETURN mName.
      END.
   END.
   WHEN "��℮�����" THEN
   DO:
      IF NOT RetString THEN PUT STREAM fil TrimFormula(logTrim, chTrim, 
        IF loan.class-code EQ "insurance" AND loan.contract   EQ '�����' THEN string(loan.open-date) ELSE "").
     ELSE RETURN 
        (IF loan.class-code EQ "insurance" AND loan.contract   EQ '�����' THEN string(loan.open-date) ELSE "").
   END.
   WHEN "���������" THEN

   FOR FIRST cust-role WHERE 
             cust-role.FILE-NAME  EQ "loan"   
         AND cust-role.surrogate  EQ "�����," + loan.cont-code
         AND cust-role.class-code EQ "�����客����"
             NO-LOCK:
 
      IF {assigned cust-role.cust-name}
         THEN mName = cust-role.cust-nam.
         ELSE  RUN RE_CLIENT (cust-role.cust-cat,
                              cust-role.cust-id,
                              INPUT-OUTPUT mName).     

      IF NOT RetString THEN PUT STREAM fil TrimFormula(logTrim, chTrim, mName).
      ELSE RETURN mName.
   END.
   WHEN "���த��������" THEN
   DO: 
      FOR  FIRST mort-loan WHERE
           RECID(mort-loan) EQ rid_loan
            AND  mort-loan.class-code eq "loan-mort"
            AND  mort-loan.contract EQ '������' 
                 NO-LOCK,
           FIRST loan-obj WHERE 
                 loan-obj.class-code EQ "deal-mort"
             AND loan-obj.sec-code   EQ mort-loan.cont-code 
             AND loan-obj.contract   EQ '�।��'  
             and loan-obj.deal-type  EQ NO               /* ������ �த��� */
                 NO-LOCK:
      IF NOT RetString THEN PUT STREAM fil TrimFormula(logTrim, chTrim, STRING(loan-obj.open-date)).
         ELSE RETURN STRING(loan-obj.open-date).
      END.
   END.
   WHEN "�����.��ᐥ�.������" THEN
   DO:
      FOR  FIRST mort-loan WHERE
           RECID(mort-loan) EQ rid_loan
            AND  mort-loan.class-code eq "loan-mort"
            AND  mort-loan.contract EQ '������' 
                 NO-LOCK,
           FIRST loan-obj WHERE 
                 loan-obj.class-code EQ "mort-obj"
             AND loan-obj.contract EQ '�����' 
             AND  loan-obj.parent-cont-code EQ mort-loan.cont-code 
             AND loan-obj.parent-contract  EQ '������' NO-LOCK:

      IF NOT RetString THEN PUT STREAM fil TrimFormula(logTrim, chTrim, loan-obj.reg-num).
         ELSE RETURN loan-obj.reg-num.

      END.
   END.
   WHEN "���.��ᐥ�.���������" THEN
   DO:
       FOR  FIRST mort-loan WHERE
           RECID(mort-loan) EQ rid_loan
            AND  mort-loan.class-code eq "loan-mort"
            AND  mort-loan.contract EQ '������' 
                 NO-LOCK,
           FIRST loan-obj WHERE 
                 loan-obj.class-code EQ "mort-obj"
             AND loan-obj.contract EQ '�����' 
             AND loan-obj.parent-cont-code EQ mort-loan.cont-code
             AND loan-obj.parent-contract  EQ '������' NO-LOCK:

      IF NOT RetString THEN PUT STREAM fil TrimFormula(logTrim, chTrim, STRING(loan-obj.open-date)).
         ELSE RETURN STRING(loan-obj.open-date).

      END.
   END.  
   WHEN "�।���.��⠑���" THEN
   DO:
      FOR FIRST mort-loan WHERE
                mort-loan.contract EQ '�।��'
            AND mort-loan.cont-code EQ loan.parent-cont-code
                NO-LOCK:
         IF NOT RetString THEN PUT STREAM fil TrimFormula(logTrim, chTrim, 
            GetXattrValue("loan",mort-loan.contract + "," + mort-loan.cont-code,"��⠑���")).
         ELSE RETURN GetXattrValue("loan",mort-loan.contract + "," + mort-loan.cont-code,"��⠑���").
      END.
   END.
   WHEN "�।���.�����" THEN
   DO:
      IF loan.parent-contract EQ '�।��' THEN
         IF NOT RetString THEN PUT STREAM fil TrimFormula(logTrim, chTrim, loan.parent-cont-code).
         ELSE RETURN loan.parent-cont-code.
   END.
       /* ����� �� ॣ�����쭮�� ������� ����� �㤥� �������, �����
          �㤥� ॠ��������� ������� ������஢ � �� � �।�⭮� ���㫥. */
   WHEN "��.������" THEN
   DO:

   END.
   WHEN "������" THEN
   DO:

   END.
   WHEN "��.������" THEN
   DO:

   END.
   WHEN "��.���" THEN
   DO:

   END.
   WHEN "��.��" THEN
   DO:

   END.
   WHEN "����" THEN
   DO:
      IF NOT RetString THEN
            PUT STREAM fil STRING (DAY(TODAY)).
      ELSE RETURN STRING (DAY(TODAY)).
   END.
   WHEN "���" THEN
   DO:
      IF NOT RetString THEN
            PUT STREAM fil STRING (YEAR(TODAY)).
      ELSE RETURN STRING (YEAR(TODAY)).
   END.
   WHEN "�����ய" THEN
   DO:
      IF NOT RetString THEN
            PUT STREAM fil ENTRY(MONTH(TODAY),
                       {&Months}).
      ELSE RETURN ENTRY(MONTH(TODAY),
                       {&Months}).
   END.
   WHEN "��⠑" THEN
   DO:
      IF NOT RetString THEN
            PUT STREAM fil STRING(TODAY,"99.99.9999").
      ELSE RETURN STRING(TODAY,"99.99.9999").
   END.
   WHEN "������" THEN
   DO:
      FIND FIRST currency WHERE
         currency.currency EQ loan.currency
      NO-LOCK NO-ERROR.
      IF AVAIL currency THEN
         IF NOT RetString THEN
            PUT STREAM fil STRING(currency.name-currenc).
         ELSE
            RETURN string(currency.name-currenc).
   END.
   WHEN "�����" THEN
   DO:                       
      FIND FIRST term-obl WHERE
         term-obl.contract  EQ loan.contract  AND
         term-obl.cont-code EQ loan.cont-code AND
         term-obl.idnt      EQ 2
      NO-LOCK NO-ERROR.
      IF AVAILABLE term-obl THEN
         IF NOT RetString THEN
            PUT STREAM fil TRIM(STRING(term-obl.amt,'->>>,>>>,>>>,>>9.99')).
         ELSE
            RETURN TRIM(STRING(term-obl.amt,'->>>,>>>,>>>,>>9.99')).
   END.
   WHEN "�।���" THEN
   DO:
      RUN GET_COMM_BUF("�।���" , 
                        ?,         
                        loan.currency,
                        loan.contract + "," + loan.cont-code, 
                        0.00,                                 
                        0,                                    
                        Xdate,
                        BUFFER comm-rate).
      IF AVAIL comm-rate THEN
      DO:
         IF comm-rate.rate-fixed EQ YES THEN
         DO:
            FIND FIRST currency WHERE
                    currency.currency EQ loan.currency
            NO-LOCK NO-ERROR.
            IF AVAIL currency THEN DO:
            
               mUnit = currency.name-currenc.
            END.
            ELSE mUnit = "%". 
         END.
         ELSE mUnit = "%".
         mRetValue = STRING(comm-rate.rate-comm) + mUnit.
      END.
      IF NOT RetString THEN
         PUT STREAM fil UNFORMATTED mRetValue.
      ELSE
         RETURN mRetValue. 
   END.
   WHEN "�।���" THEN
   DO:
      RUN GET_COMM_BUF("�।���" , 
                        ?,         
                        loan.currency,
                        loan.contract + "," + loan.cont-code, 
                        0.00,                                 
                        0,                                    
                        Xdate,
                        BUFFER comm-rate).
      IF AVAIL comm-rate THEN
      DO:
         IF comm-rate.rate-fixed EQ YES THEN
         DO:
            FIND FIRST currency WHERE
                    currency.currency EQ loan.currency
            NO-LOCK NO-ERROR.
            IF AVAIL currency THEN
               mUnit = currency.name-currenc.
            ELSE mUnit = "%". 
         END.
         ELSE mUnit = "%".
         mRetValue = STRING(comm-rate.rate-comm) + mUnit.
      END.
      IF NOT RetString THEN
         PUT STREAM fil UNFORMATTED mRetValue.
      ELSE
         RETURN mRetValue. 
   END.
   WHEN "�������" THEN
   DO:
      mCommRate = GET_COMM("����%" ,
                           ?,         
                           loan.currency,
                           loan.contract + "," + loan.cont-code, 
                           0.00,                                 
                           0,                                    
                           Xdate).               
      IF mCommRate EQ ? THEN
         mCommRate = GET_COMM("����-�" ,
                              ?,         
                              loan.currency,
                              loan.contract + "," + loan.cont-code, 
                              0.00,                                 
                              0,                                    
                              Xdate).   
      IF NOT RetString THEN
         PUT STREAM fil UNFORMATTED trim(string(mCommRate,">>>>>>>>>>>>>>9.99")).
      ELSE
         RETURN trim(string(mCommRate,">>>>>>>>>>>>>>9.99")).
   END.
   WHEN "�����%%" THEN
   DO:
      mCommRate = GET_COMM("%����%" , 
                           ?,         
                           loan.currency,
                           loan.contract + "," + loan.cont-code, 
                           0.00,                                 
                           0,                                    
                           Xdate).
      IF mCommRate EQ ? THEN
         mCommRate = GET_COMM("����%�" ,
                              ?,         
                              loan.currency,
                              loan.contract + "," + loan.cont-code, 
                              0.00,                                 
                              0,                                    
                              Xdate).   
      IF NOT RetString THEN
         PUT STREAM fil UNFORMATTED trim(string(mCommRate,">>>>>>>>>>>>>>9.99")).
      ELSE
         RETURN trim(string(mCommRate,">>>>>>>>>>>>>>9.99")).
   END.
   WHEN "�த����" THEN
   DO:
         /* �饬 ������ �� �த��� � �����䨪��� �த��� � �����頥� ��� ������������,
         ** ���� - ����. */
      mString = GetXAttrValueEx("loan",
                                loan.contract + "," + loan.cont-code,
                                "�த���",
                                "").
      FIND LAST tmp-code WHERE
                tmp-code.class    EQ "�த���"
         AND    tmp-code.code     EQ mString
         AND    tmp-code.beg-date LE Xdate
      NO-LOCK NO-ERROR.
      mRetValue = IF AVAIL tmp-code 
                     THEN  tmp-code.name 
                     ELSE  ?.
      IF NOT RetString THEN
         PUT STREAM fil UNFORMATTED mRetValue.
      ELSE
         RETURN mRetValue.
   END.
   WHEN "�த�������" THEN
   DO:
         /* �饬 �� ���� �த�� �।�� � �����䨪��� �த��� ������ �த��, 
         ** ��� �⮩ ����� - த�⥫� � �����頥� ������������ த�⥫�᪮� �����. 
         ** ���� - ����. */
      ASSIGN
         mRetValue = ?
         mString   = GetXAttrValueEx("loan",
                                     loan.contract + "," + loan.cont-code,
                                     "�த���",
                                     "")
      .
      FOR FIRST tmp-code WHERE
                tmp-code.class    EQ '�த���'
         AND    tmp-code.code     EQ mString
         AND    tmp-code.beg-date LE Xdate
         NO-LOCK,
         FIRST code OF tmp-code
         NO-LOCK,
            FIRST b-tmp-code WHERE
                  b-tmp-code.class    EQ '�த���'
            AND   b-tmp-code.code     EQ code.parent
            AND   b-tmp-code.beg-date LE Xdate
      NO-LOCK:
         mRetValue = b-tmp-code.name.
      END.
      IF NOT RetString THEN
         PUT STREAM fil UNFORMATTED mRetValue.
      ELSE
         RETURN mRetValue.
   END.
   WHEN "�த����" THEN
   DO:
      mName = GetXAttrValueEx("loan",
                               loan.contract + "," + loan.cont-code,
                               "�த����",
                               "").
      FIND FIRST code WHERE 
         code.code EQ mName
      NO-LOCK NO-ERROR.
      IF AVAIL code THEN
         IF NOT RetString THEN
            PUT STREAM fil UNFORMATTED code.name.
         ELSE
            RETURN code.name.
      ELSE RETURN "".
   END.
   WHEN "��%���" THEN
   DO:
      FIND FIRST loan-int WHERE loan-int.contract  EQ loan.contract                            AND loan-int.cont-code EQ loan.cont-code 
                            AND loan-int.id-d      EQ 410
                            AND loan-int.id-k      EQ 5
      NO-LOCK NO-ERROR.

      FIND FIRST op-entry OF loan-int NO-LOCK NO-ERROR.

      IF AVAIL op-entry THEN
      DO:
         IF loan.currency EQ "" THEN
            mRetValue = trim(string(op-entry.amt-rub,">>>>>>>>>>>>>>9.99")).
         ELSE
            mRetValue = trim(string(op-entry.amt-cur,">>>>>>>>>>>>>>9.99")).
      END.
      IF mRetValue NE "" THEN
         IF NOT RetString THEN
            PUT STREAM fil UNFORMATTED STRING(mRetValue).
         ELSE
         RETURN STRING(mRetValue).
      ELSE 
      DO:
         FIND FIRST term-obl WHERE
            term-obl.contract  EQ loan.contract  AND
            term-obl.cont-code EQ loan.cont-code AND
            term-obl.idnt      EQ 2
         NO-LOCK NO-ERROR.
         IF AVAIL term-obl THEN
         DO:
            RUN CalcCommStart IN h_loan ("%���",term-obl.amt,loan.contract,loan.cont-code,loan.currency,"?",OUTPUT mCommRate,OUTPUT is-ok).
            /* �᫨ �� �ᯥ譮, � �� �㣠���� � ᠬ�� �㭪樨, � ��⠥� ������� ࠢ��� ��� */
            IF is-ok EQ 0 THEN
               mCommRate = 0.
            IF NOT RetString THEN
               PUT STREAM fil UNFORMATTED STRING(mCommRate).
            RETURN TRIM(STRING(mCommRate,">>>>>>>>>>>>>>9.99")).
         END.
         ELSE
            RETURN "".
      END.
   END.
   WHEN "��%��" THEN
   DO:
      BLCK:
      DO:
         FIND FIRST loan-int WHERE loan-int.contract  EQ loan.contract 
                               AND loan-int.cont-code EQ loan.cont-code 
                               AND loan-int.id-d      EQ 173
                               AND loan-int.id-k      EQ 5
         NO-LOCK NO-ERROR.
    
         IF AVAIL loan-int THEN
            mRetValue = STRING(loan-int.amt-rub).
   
         FIND FIRST op-entry OF loan-int NO-LOCK NO-ERROR.
   
         IF AVAIL op-entry THEN
         DO:
            IF loan.currency EQ "" THEN
               mRetValue = trim(string(op-entry.amt-rub,">>>>>>>>>>>>>>9.99")).
            ELSE
               mRetValue = trim(string(op-entry.amt-cur,">>>>>>>>>>>>>>9.99")).
         END.
         IF mRetValue NE "" THEN
            LEAVE BLCK.
         ELSE 
         DO:
            FIND FIRST term-obl WHERE
               term-obl.contract  EQ loan.contract  AND
               term-obl.cont-code EQ loan.cont-code AND
               term-obl.idnt      EQ 2
            NO-LOCK NO-ERROR.
            IF AVAIL term-obl THEN
            DO:
               RUN CalcCommStart IN h_loan ("%��",term-obl.amt,loan.contract,loan.cont-code,loan.currency,"?",OUTPUT mCommRate,OUTPUT is-ok).
               /* �᫨ �� �ᯥ譮, � �� �㣠���� � ᠬ�� �㭪樨, � ��⠥� ������� ࠢ��� ��� */
               IF is-ok EQ 0 THEN
                  mCommRate = 0.
               mRetValue = STRING(mCommRate).
            END.
         END.
      END.
      IF NOT RetString THEN
         PUT STREAM fil UNFORMATTED TRIM(STRING(DEC(mRetValue),">>>>>>>>>>>>>>9.99")).
      RETURN TRIM(STRING(DEC(mRetValue),">>>>>>>>>>>>>>9.99")).
   END.
   WHEN "��%������" THEN
   DO:
      BLCK:
      DO:
         FIND FIRST loan-int WHERE loan-int.contract  EQ loan.contract 
                               AND loan-int.cont-code EQ loan.cont-code 
                               AND loan-int.id-d      EQ 175
                               AND loan-int.id-k      EQ 5
         NO-LOCK NO-ERROR.
    
         IF AVAIL loan-int THEN
            mRetValue = STRING(loan-int.amt-rub).
   
         FIND FIRST op-entry OF loan-int NO-LOCK NO-ERROR.
   
         IF AVAIL op-entry THEN
         DO:
            IF loan.currency EQ "" THEN
               mRetValue = string(op-entry.amt-rub).
            ELSE
               mRetValue = string(op-entry.amt-cur).
         END.
         IF mRetValue NE "" THEN
            LEAVE BLCK.
         ELSE 
         DO:
            FIND FIRST term-obl WHERE
               term-obl.contract  EQ loan.contract  AND
               term-obl.cont-code EQ loan.cont-code AND
               term-obl.idnt      EQ 2
            NO-LOCK NO-ERROR.
            IF AVAIL term-obl THEN
            DO:
               mRetValue = STRING(term-obl.amt-rub * GET_COMM_LOAN(loan.contract,loan.cont-code,"%������",loan.open-date) / 100).
            END.
         END.
      END.
      IF NOT RetString THEN
         PUT STREAM fil UNFORMATTED TRIM(STRING(DEC(mRetValue),">>>>>>>>>>>>>>9.99")).
      ELSE
         RETURN TRIM(STRING(DEC(mRetValue),">>>>>>>>>>>>>>9.99")).
   END.   




/*���� �ଠ� ���� zss*/
  WHEN "����" THEN
    DO:
 

 RUN pb_newadr.p(loan.cust-cat,loan.cust-id,OUTPUT mString).
       	
   IF NOT RetString THEN
            PUT STREAM fil UNFORMATTED mString.
         ELSE
            RETURN mString.


  END.
   


   /*�������� ⥣ 瀤��ய� ��� ���������� ������ � ���� �ய�᪨*/
   WHEN "瀤��ய�" THEN
   DO:
      RUN RetAdr.p(loan.cust-id,loan.cust-cat,"����ய",Xdate,OUTPUT mString).
     /*message mString view-as alert-box.*/
    IF NOT RetString THEN
            PUT STREAM fil UNFORMATTED mString.
         ELSE
            RETURN mString.
   END.
   /*--------------------------------------------------*/
   WHEN "瀤��ய" THEN
   DO:
      RUN RetAdr.p(loan.cust-id,loan.cust-cat,"����ய",Xdate,OUTPUT mString).
    IF NOT RetString THEN
            PUT STREAM fil UNFORMATTED mString.
         ELSE
            RETURN mString. 
   END.
   WHEN "瀤�����" THEN
   DO:
      RUN RetAdr.p(loan.cust-id,loan.cust-cat,"�������",Xdate,OUTPUT mString).
    IF NOT RetString THEN
            PUT STREAM fil UNFORMATTED mString.
         ELSE
            RETURN mString. 
   END.
   /* ayv - �� ������⢨� 䠪��᪮�� ���� ����뢠�� ���� �ய�᪨ */
   WHEN "瀤�����" THEN
   DO:
      RUN RetAdr.p(loan.cust-id,loan.cust-cat,"�������",Xdate,OUTPUT mString).
      IF mString EQ "" THEN
        RUN RetAdr.p(loan.cust-id,loan.cust-cat,"����ய",Xdate,OUTPUT mString).  
    IF NOT RetString THEN
            PUT STREAM fil UNFORMATTED mString.
         ELSE
            RETURN mString. 
   END.
{lnfrmoms.i}
END CASE.
{intrface.del comm}