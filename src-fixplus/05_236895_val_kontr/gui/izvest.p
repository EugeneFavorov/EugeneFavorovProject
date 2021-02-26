{globals.i}
{intrface.get tmess}

/* +++ izvest.p was humbly modified by (c)blodd converter v.1.09 on 3/27/2015 1:38pm +++ */

/*
               KSV Editor
    Copyright: (C) 2000-2006 Serguey Klimoff (bulklodd)
     Filename: izvest.p
      Comment: ����������� �� �㬬�� �࠭�.��⮢
   Parameters:
         Uses:
      Used by:
      Created: 10.02.2010 IT
*/

{globals.i}
{tmprecid.def}
{tmpobj.def}
{intrface.get count}     /* ������⥪� ��� ࠡ��� � �����⠬�. */
{intrface.get cust}     /* ������⥪� ��� ࠡ��� � �����⠬�. */
{intrface.get xclass}
{intrface.get date}
{sh-defs.i}

{prn-doc.def  &with_proc=YES}
{cust-adr.obj &def-vars =YES}
{globals.i}

DEFINE VARIABLE mName     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mDate2    AS DATE        NO-UNDO.  /* ��� �।����������� ������� */
DEFINE VARIABLE mReestr   AS CHARACTER   NO-UNDO.      /* �ᯮ�.��६����� */
DEFINE VARIABLE mAddress  AS CHARACTER   NO-UNDO.      /* �ᯮ�.��६����� */
DEFINE VARIABLE mCurrI    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mAmt      AS DECIMAL     NO-UNDO.
DEFINE VARIABLE mAmtStr   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mDecStr   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mDocNum   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE i         AS INTEGER     NO-UNDO.
DEFINE VARIABLE mFlag     AS LOGICAL     NO-UNDO.
DEFINE VARIABLE mRePrint  AS LOGICAL     NO-UNDO.

DEFINE TEMP-TABLE ttRep
    FIELD cust-cat   AS CHARACTER
    FIELD cust-id    AS INTEGER     /* ������ */
    FIELD name_klnt  AS CHARACTER   /* ��� ������ */
    FIELD acct       AS CHARACTER   /* ᯨ᮪ ��⮢ �����⮢ �� �����ᮢ�� 40* � �⮫��� */
.
            
{ getdate.i }

{empty TmpObj}
FOR  EACH acct NO-LOCK
    WHERE acct.contract   BEGINS '�࠭�'
      AND NOT acct.acct   BEGINS "409"
      AND ( acct.close-date EQ ? OR
            acct.close-date GT end-date )
      AND acct.open-date  LE end-date
      AND ( acct.filial EQ ShFilial OR
            shModeMulty ) :

    RUN acct-pos IN h_base (acct.acct, acct.currency, end-date, end-date, gop-status).

    IF ( acct.currency EQ "" AND sh-cr NE 0 ) OR ( acct.currency NE "" AND sh-vcr NE 0 ) THEN
    DO:
       CREATE TmpObj.
       ASSIGN TmpObj.rid = recid(acct).
    END.
END.

RUN browseld.p("acct",
               "UseTmpObjInQuery" + CHR(1) +
               "contract",
               string(mTmpObjHand)    + CHR(1) +
               "�࠭�*","",3).

IF {&KEY_FUNCTION}({&LAST_KEY}) EQ "END-ERROR" THEN RETURN.

FIND FIRST tmprecid NO-LOCK NO-ERROR.

DO:
   mblodd_char_Tmp01 = pick-value.
   RUN Fill-AlertSysMes IN h_tmess("","",4,"������ ���㬥��� ����୮?").
   DEFINE VARIABLE ans1 AS LOGICAL NO-UNDO.
   ans1 = (pick-value = "YES").
   pick-value = mblodd_char_Tmp01.
END.


RUN BeginCircle_TTName("izvest").
FOR EACH tmprecid,
    FIRST acct NO-LOCK 
    WHERE RECID(acct) EQ tmprecid.id,
    EACH  op-entry NO-LOCK
    WHERE op-entry.acct-cr  EQ acct.acct
      AND op-entry.currency EQ acct.currency
      AND op-entry.op-date  EQ end-date
      AND ( IF op-entry.currency EQ "" THEN op-entry.amt-rub NE 0 ELSE op-entry.amt-cur NE 0 ),
    FIRST op OF op-entry NO-LOCK 
    BREAK BY op.op :

    IF FIRST-OF(op.op) THEN 
    DO:
       
       FIND FIRST cust-corp 
            WHERE cust-corp.cust-id EQ acct.cust-id NO-LOCK NO-ERROR. 
       
       /* �������� �� 㢥�������� */
       mDocNum = GetXAttrValueEx("op",STRING(op.op),"izvest","").
       mRePrint = {assigned mDocNum}.
       IF (not ans1) and mRePrint THEN NEXT.

       IF mFlag EQ NO 
           THEN mFlag = YES.

       mName = acct.details.
       IF NOT {assigned mName} THEN
          mName = IF AVAIL cust-corp THEN cust-corp.name-short ELSE "������ �� ������". 

       RUN GetCustAdr(acct.cust-cat,acct.cust-id,gend-date,"�������,�������,�����",OUTPUT TABLE ttCustAddress).

       FIND LAST ttCustAddress WHERE 
                 ttCustAddress.fTypeAdr EQ "�������" NO-LOCK NO-ERROR.

       IF NOT AVAIL ttCustAddress THEN 
          FIND LAST ttCustAddress WHERE                
                    ttCustAddress.fTypeAdr EQ "�������" NO-LOCK NO-ERROR.

       IF NOT AVAIL(ttCustAddress) THEN 
          FIND LAST ttCustAddress  WHERE 
                    ttCustAddress.fTypeAdr EQ "�����" NO-LOCK NO-ERROR.

       IF AVAIL(ttCustAddress) THEN
          RUN Insert_TTName("adr_klnt[izvest]",ttCustAddress.fAdrStr).
   
       RUN Insert_TTName("acct[izvest]", DelFilFromAcct(acct.acct)).
       RUN Insert_TTName("name_klnt[izvest]", mName).
       RUN Insert_TTName("date1[izvest]", end-date).

       mDate2 = end-date.
       DO i = 2 TO 15 :
          mDate2 = mDate2 + 1.
          DO WHILE HolidayRu(mDate2):
             mDate2 = mDate2 + 1.
          END.
       END.
       RUN Insert_TTName("date2[izvest]", mDate2).

       FIND FIRST currency
            WHERE currency.currency EQ acct.currency NO-LOCK NO-ERROR.
       mCurrI = IF AVAIL currency THEN currency.i-currency
                                  ELSE acct.currency.
       RUN Insert_TTName("currency[izvest]", mCurrI).
    
       mAmt = IF op-entry.currency EQ "" THEN op-entry.amt-rub ELSE op-entry.amt-cur.
       RUN Insert_TTName("amt[izvest]", mAmt).

       RUN x-amtstr.p (mAmt,
                       op-entry.currency,
                       YES,
                       YES,
                       OUTPUT mAmtStr,     /* ��ப� �㬬� �ய���� */
                       OUTPUT mDecStr).    /* ��ப� ���-�� ������ ��ࠬ� */
       RUN Insert_TTName("amtl[izvest]", mAmtStr + " " + mDecStr).

       if not mRePrint then
       DO TRANSACTION:
          CreateCounterIfNotExist("�����࠭��", "����� 㢥�������� �� �㬬�� �࠭�.��⮢", 1).
          mDocNum = string(GetCounterNextValue("�����࠭��", gend-date)).
          IF not {assigned mDocNum} THEN mDocNum = "1".
       END.

       RUN Insert_TTName("docnum[izvest]", mDocNum).

       RUN NextCircle_TTName("izvest").

       if not mRePrint then UpdateSigns("op",STRING(op.op),"izvest",mDocNum,?).

       CREATE ttRep.
       ASSIGN ttRep.name_klnt = mName
              ttRep.cust-cat  = acct.cust-cat
              ttRep.cust-id   = acct.cust-id
              ttRep.acct      = DelFilFromAcct(acct.acct)
       .
    END.
END.
RUN EndCircle_TTName("izvest").

IF mFlag EQ YES
THEN 
    RUN printvd.p("izvest",INPUT TABLE ttnames). 
ELSE
    RUN Fill-AlertSysMes IN h_tmess("","",1,"�� ����� ���㬥�⠬, 㢥�������� 㦥 ᮧ��������.").


/* --- izvest.p was humbly modified by (c)blodd converter v.1.09 on 3/27/2015 1:38pm --- */
