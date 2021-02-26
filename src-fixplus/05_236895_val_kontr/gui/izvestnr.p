{globals.i}
{intrface.get tmess}

/* +++ izvestnr.p was humbly modified by (c)blodd converter v.1.09 on 3/19/2015 11:54am +++ */

/*
     Filename: izvestnr.p
      Comment: ����������� � ����㯫���� �।�⢠ �� ��१����⮢ 
   Parameters:
         Uses:
      Used by:
      Created: 17.28.2012 KMBIS TT:0174437 �롮ઠ � ᮧ����� ���⭮� ��� �� ᮧ����� 㢥�������� �� �㡫�  

*/
DEFINE INPUT PARAM iParam     AS CHAR    NO-UNDO.

{globals.i}

{tmpobj.def}
{tmprecid.def}


{intrface.get cust}      /* ������⥪� ��� ࠡ��� � �����⠬�. */
{intrface.get count}     /* ������⥪� ��� ࠡ��� � �����⠬�. */
{intrface.get xclass}

{prn-doc.def  &with_proc=YES}

DEF VAR mName     AS CHAR    EXTENT 3    NO-UNDO. /* ������������ ������            */
DEF VAR mFlag     AS LOG                 NO-UNDO. /* ���� �ନ஢���� ᮮ�饭��     */
DEF VAR mDocNum   AS CHAR                NO-UNDO. /* ����� ����饭��                 */

DEF VAR mAcctF    AS LOG                 NO-UNDO. /* ���� �ନ஢���� ᮮ�饭��     */
DEF VAR mAcctNr   AS CHAR                NO-UNDO. /* ��� 2-�� ���浪� ��१����⮢ */

DEF VAR mStat     AS CHAR                NO-UNDO. /* ����� ���㬥�� ��� �⡮�     */
DEF VAR mAcctM    AS CHAR                NO-UNDO. /* ��᪠ ������᪨� ��⮢         */

DEFINE VARIABLE mAmtStr   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mDecStr   AS CHARACTER   NO-UNDO.

DEFINE TEMP-TABLE ttOp-entry
    FIELD acct-id     AS RECID
    FIELD op-entry-id AS RECID
.
IF iParam EQ "" OR NUM-ENTRIES(iParam, ";") NE 2 THEN
DO:
   RUN Fill-AlertSysMes IN h_tmess("","",-1,"�� ��୮ ������ �室�騥 ��ࠬ����" + CHR(32) + "~n" + CHR(32) + "��ଠ�: <��᪠_��⮢>;<�����_���㬥��>" + CHR(32) + "~n").

   RETURN.

END.
            
{getdate.i}

{empty TmpObj}
{empty ttOp-entry}

ASSIGN
   mAcctM  = ENTRY(1,iParam, ";" )
   mStat   = ENTRY(2,iParam, ";" ).
   mAcctNr = ""
.

{justasec}

/* ���᮪ �����ᮢ�� ��⮢ ��ண� ���浪� ��१����⮢ */
FOR EACH signs WHERE signs.file-name  EQ "bal-acct"
                 AND signs.code       EQ "��⍥१"
                 AND signs.code-value EQ "��"
               NO-LOCK:
   IF mAcctNr EQ "" THEN 
      mAcctNr = signs.surrogate + "*".
   ELSE
      mAcctNr = mAcctNr + "," + signs.surrogate + "*".
END.

FOR EACH op-entry WHERE op-entry.op-date   EQ end-date
                    AND op-entry.currency  EQ ""
                    AND op-entry.op-status GE mStat
                    AND CAN-DO(mAcctM, op-entry.acct-cr)
                  NO-LOCK,
    FIRST op OF op-entry WHERE op.op-date   EQ end-date 
                           AND op.op-status GE mStat
                         NO-LOCK,
    FIRST acct WHERE acct.acct EQ op-entry.acct-cr
                 AND acct.currency EQ ""      
                 AND (acct.filial EQ ShFilial OR
                      shModeMulty) 
                NO-LOCK
                BREAK BY acct.acct:


   IF FIRST-OF(acct.acct) THEN mAcctF = NO.

   IF  (op.details   MATCHES '*~{VO*~}*') THEN 
   DO: 
      /* �⡨ࠥ� ���㬥��� � "*{VO*" � ᮤ�ঠ��� ����樨 */
      RUN CreateTTop(RECID(acct), RECID(op-entry)).
      mAcctF = YES.

   END.
   ELSE IF op.ben-acct NE ? AND op.ben-acct NE "" THEN
   DO: 

      IF CAN-DO(mAcctNr, op.ben-acct) THEN
      DO: 
         /* ��� ��ࠢ����� - ��� ��१����� */
         RUN CreateTTop(RECID(acct), RECID(op-entry)).
         mAcctF = YES.
      END.

   END.
   ELSE IF op-entry.acct-db NE ? THEN
   DO: 

      IF CAN-DO(mAcctNr, op-entry.acct-db) THEN
      DO: 
         /* ����७��� ��� ��ࠢ�⥫� - ��� ��१����� */
         RUN CreateTTop(RECID(acct), RECID(op-entry)).
         mAcctF = YES.
      END.

   END.

   IF LAST-OF(acct.acct) AND mAcctF THEN 
   DO: 
      CREATE TmpObj.
      ASSIGN TmpObj.rid = RECID(acct).
      VALIDATE TmpObj.
   END.

END. /* FOR EACH op-entry WHERE op-entry.op-date   EQ end-date */

/* ��㧥� ��⮢ ��� �롮� */
RUN browseld.p("acct",
               "UseTmpObjInQuery",
               string(mTmpObjHand),
               "",
               3).

IF {&KEY_FUNCTION}({&LAST_KEY}) EQ "END-ERROR" THEN RETURN.

DO:
   mblodd_char_Tmp01 = pick-value.
   RUN Fill-AlertSysMes IN h_tmess("","",4,"������ 㢥��������, �� 㦥 ��ࠢ���� ���㬥�⠬?").
   DEFINE VARIABLE ans1 AS LOGICAL NO-UNDO.
   ans1 = (pick-value = "YES").
   pick-value = mblodd_char_Tmp01.
END.


RUN BeginCircle_TTName("izvest").

FOR EACH tmprecid,
   EACH ttOp-entry WHERE ttOp-entry.acct-id EQ tmprecid.id,
   EACH op-entry WHERE RECID(op-entry) EQ ttOp-entry.op-entry-id
                 NO-LOCK,
   FIRST op OF op-entry NO-LOCK
                        BREAK BY op-entry.acct-cr:

   IF FIRST-OF(op-entry.acct-cr) THEN
   DO:
      FIND FIRST acct WHERE acct.acct EQ op-entry.acct-cr
                      NO-LOCK NO-ERROR.
      RELEASE cust-corp.
      RELEASE person.
 
      mName = "".
      
      IF AVAIL(acct) THEN 
      DO:
         RUN GetCustName IN h_base(acct.cust-cat,
                                   acct.cust-id,
                                   ?,
                                   OUTPUT mName[1],
                                   OUTPUT mName[2],
                                   INPUT-OUTPUT mName[3]).
         RUN GetCustAdr(acct.cust-cat,acct.cust-id,gend-date,"�������,�������,�����",OUTPUT TABLE ttCustAddress).

         FIND LAST ttCustAddress WHERE 
                   ttCustAddress.fTypeAdr EQ "�������" NO-LOCK NO-ERROR.
      
         IF NOT AVAIL ttCustAddress THEN 
            FIND LAST ttCustAddress WHERE                
                      ttCustAddress.fTypeAdr EQ "�������" NO-LOCK NO-ERROR.
      
         IF NOT AVAIL(ttCustAddress) THEN 
            FIND LAST ttCustAddress  WHERE 
                      ttCustAddress.fTypeAdr EQ "�����" NO-LOCK NO-ERROR.

         IF AVAIL ttCustAddress THEN 
            RUN Insert_TTName("adr_klnt[izvest]",ttCustAddress.fAdrStr).

      END.
   END.

   /* �������� �� 㢥�������� */
   mDocNum = GetXAttrValueEx("op",STRING(op.op),"izvest","").

   IF (ans1 EQ NO) AND (mDocNum NE "") THEN NEXT.

   IF mDocNum EQ "" THEN
      DO TRANSACTION:
         CreateCounterIfNotExist("�����࠭��", "����� 㢥�������� �� �㬬�� �� ��१����⮢", 1).
         mDocNum = STRING(GetCounterNextValue("�����࠭��", gend-date)).
         IF NOT {assigned mDocNum} THEN mDocNum = "1".
         UpdateSigns("op",STRING(op.op),"izvest",mDocNum,?).
      END.

   RUN Insert_TTName("name_klnt[izvest]", TRIM(TRIM(mName[1]) + " " + TRIM(mName[2]))).

   RUN Insert_TTName("docnum[izvest]", mDocNum).

   RUN Insert_TTName("date1[izvest]", STRING(end-date, "99.99.9999")).

   RUN Insert_TTName("acct[izvest]", DelFilFromAcct(op-entry.acct-cr)).

   RUN Insert_TTName("doc-num[izvest]", op.doc-num).

   RUN Insert_TTName("amt[izvest]", TRIM(STRING(op-entry.amt-rub,"->>,>>>,>>>,>>>,>>9.99"))).

   RUN x-amtstr.p (op-entry.amt-rub,
                   op-entry.currency,
                   YES,
                   YES,
                   OUTPUT mAmtStr,     /* ��ப� �㬬� �ய���� */
                   OUTPUT mDecStr).    /* ��ப� ���-�� ������ ��ࠬ� */

   RUN Insert_TTName("amtl[izvest]", mAmtStr + " " + mDecStr).


   RUN NextCircle_TTName("izvest").

   mFlag = YES.

END. /* FOR EACH tmprecid */

RUN EndCircle_TTName("izvest").
                      
IF mFlag EQ YES
THEN               
    RUN printvd.p("izvestnr",INPUT TABLE ttnames). 
ELSE
    RUN Fill-AlertSysMes IN h_tmess("","",1,"�� ����� ���㬥�⠬, 㢥�������� 㦥 ᮧ��������.").


/*============================================================================*/
/*=== �������� �६����� ⠡����, � �뫪��� �� �⮡࠭�� ���㬥��� ==========*/

PROCEDURE CreateTTop:

DEF INPUT PARAM iAcctId       AS RECID.
DEF INPUT PARAM iOpEntryId    AS RECID.

   CREATE ttOp-entry.
   ASSIGN
     ttOp-entry.acct-id     = iAcctId
     ttOp-entry.op-entry-id = iOpEntryId
   .
   VALIDATE ttOp-entry.

END PROCEDURE.

/*============================================================================*/

/* --- izvestnr.p was humbly modified by (c)blodd converter v.1.09 on 3/19/2015 11:54am --- */
