{globals.i}
{intrface.get xclass}
{intrface.get strng}

DEF INPUT PARAMETER iCat  AS CHAR  NO-UNDO.
DEF INPUT PARAMETER iId   AS INT64 NO-UNDO.
DEF OUTPUT PARAM sRezult1       AS CHAR  NO-UNDO. 

DEF VAR iAdr1         AS CHAR  NO-UNDO.
DEF VAR mTmp          AS CHAR  NO-UNDO.
DEF VAR vCodReg       AS CHAR  NO-UNDO. /* ॣ��� �� ���� */
DEF VAR vCodCountry   AS CHAR  NO-UNDO.
DEF VAR mIndex        AS CHAR  NO-UNDO. /* ������ */
DEF VAR mArea         AS CHAR  NO-UNDO. /* ࠩ�� */
DEF VAR mCity         AS CHAR  NO-UNDO. /* ��த */
DEF VAR mNasPunkt     AS CHAR  NO-UNDO. /* ��ᥫ���� �㭪� */
DEF VAR mStreet       AS CHAR  NO-UNDO. /* 㫨� */
DEF VAR mHouse        AS CHAR  NO-UNDO. /* ��� */
DEF VAR mCorpus       AS CHAR  NO-UNDO. /* ����� */
DEF VAR mRoom         AS CHAR  NO-UNDO. /* ������ */
DEF VAR mStroen       AS CHAR  NO-UNDO. /* ��஥��� */

/* DEF VAR sRezult1      AS CHAR  NO-UNDO. /* ���� � ����� �ଠ� */  */

FIND LAST cust-ident
  WHERE cust-ident.class-code       = "p-cust-adr"
    AND   cust-ident.cust-code-type = "����ய"
    AND   cust-ident.cust-cat       = iCat
    AND   cust-ident.cust-id        = iId
    AND   cust-ident.open-date      <= TODAY
  NO-LOCK NO-ERROR.

IF AVAIL cust-ident
THEN DO:
  vCodReg = GetXAttrValue("cust-ident",
                          cust-ident.cust-code-type + "," + cust-ident.cust-code + "," + STRING(cust-ident.cust-type-num),
                          "������").
  IF vCodReg NE '' THEN vCodReg = GetCodeName("������", vCodReg). /* ������ */
 
  iAdr1   = cust-ident.issue.
/*  message iAdr1 view-as alert-box. */
   vCodCountry = GetXAttrValue("cust-ident",
                          cust-ident.cust-code-type + "," + cust-ident.cust-code + "," + STRING(cust-ident.cust-type-num),
                          "country-id").
  
  IF vCodCountry NE 'RUS' AND vCodCountry NE '' THEN do:
     FIND FIRST country WHERE country-id EQ vCodCountry NO-LOCK NO-ERROR.
     IF AVAIL country THEN
    vCodCountry = country.country-name.
  END.
  ELSE vCodCountry = ''.

  mIndex  = GetEntries(1,iAdr1,',','').
  mArea   = GetEntries(2, iAdr1,',','').

  mTmp = GetEntries(3,iAdr1,',','').
  IF CAN-DO('* �,* �.,� *,���.*', mTmp) THEN mCity = mTmp.
  ELSE IF(mTmp NE '')                   THEN mCity = '�.' + mTmp.

  /*�᫨ ॣ��� � ��த ����� �������� ��᪢� ��� �����-������ � ����塞 ॣ���*/
  IF CAN-DO('*�����*,*������*',vCodReg) AND CAN-DO('*�����*,*������*',mCity) THEN vCodReg = ''.

  mNasPunkt = GetEntries(4,iAdr1,',','').
  mStreet   = GetEntries(5,iAdr1,',','').

  mTmp = GetEntries(6,iAdr1,',','').
  IF CAN-DO('� *,* �,�.*,* �.,�. *',mTmp) THEN mHouse = mTmp.
  ELSE IF(mTmp NE '')                     THEN mHouse = '�.' + mTmp.

  mTmp = GetEntries(7,iAdr1,',','').
  IF CAN-DO('� *,* �,���. *,�. *',mTmp) THEN mCorpus = mTmp.
  ELSE IF(mTmp NE '')                    THEN mCorpus = '���.' + mTmp.

  mTmp = GetEntries(8,iAdr1,',','').
  IF CAN-DO('� *,�. *,* �.,�� *,��. *,* ��.',mTmp) THEN mRoom = mTmp.
  ELSE IF(mTmp NE '')                              THEN mRoom = '��.' + mTmp.

  mTmp = GetEntries(9,iAdr1,',','').
  IF (mTmp NE '') THEN mStroen = '���.' + mTmp.

  sRezult1 = (IF(mStreet = "")       THEN "" ELSE (mStreet + ', '))
           + (IF (mHouse  = "")      THEN "" ELSE (mHouse))
           + (IF (mStroen = "")      THEN "" ELSE (', ' + mStroen))
           + (IF (mCorpus = "")      THEN "" ELSE (', ' + mCorpus))
           + (IF (mRoom   = "")      THEN "" ELSE (', ' + mRoom))
           + (IF (mNasPunkt   = "")  THEN "" ELSE (', ' + mNasPunkt))
           + (IF (mCity   = "")      THEN "" ELSE (', ' + mCity))
           + (IF (mArea   = "")      THEN "" ELSE (', ' + mArea))
           + (IF (vCodReg = "")      THEN "" ELSE (', ' + vCodReg))
           + (IF (vCodCountry  = "") THEN "" ELSE (', ' + vCodCountry))
           + (IF (mIndex  = "")      THEN "" ELSE (', ' + mIndex)).
END.
/* message sRezult1 view-as alert-box. */
/* sRezult2 = sRezult1. */
{intrface.del}
RETURN  sRezult1.
