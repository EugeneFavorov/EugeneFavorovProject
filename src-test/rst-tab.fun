/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2017 �� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: RST-TAB.FUN
      Comment: ��।������ �㬬, ����� ����� � ���-�� 祪�� ��� ������� ॥��� 
   Parameters:
         Uses:
      Used by:
      Created: 16.08.2004 17:49 ligp     31277: ��ࠡ�⪠ � ���㫥 ��� � ᢥ� ������樨 113-� (������ ॥��஢)            
     Modified: 20.08.2004 17:04 ligp     31277: ��ࠡ�⪠ � ���㫥 ��� � ᢥ� ������樨 113-�
                                         (������ ॥��஢)
     Modified: 24.09.2004 15:35 ligp     32092: ��२�������� ��६���� 
     Modified: 24.09.2004 16:02 ligp     32092: �������� � 1433-� (�ਫ������ 3)֑������ �ࠢ�� �
                                         ���ᮢ�� �������
     Modified: 14.01.2005 13:57 ligp     39221: ��� ���������� ।���஢���� ���祭�� ���� "�����"
                                         ��� vok-kas
     Modified: 17.01.2005 11:42 rija     37931: ����������� ����������� ������� �����. ��ࠬ���
                                         ����犠��낎� ��� ���ࠧ������
     Modified: 18.01.2005 17:36 rija     37931: ����������� ����������� ������� �����. ��ࠬ���
                                         ����犠��낎� ��� ���ࠧ������
     Modified: 10.08.2006 18:00 elus     0064978: � �ࠢ�� �뤠������ �� ���㬥��� �த��� �� �㬬�
                                         �ਭ���� ������� 㤢��������
     Modified: 14.08.2006 15:39 ELUS     0064978
     Modified: 25.08.2006 16:58 ELUS     0064978: � �ࠢ�� �뤠������ �� ���㬥��� �த��� �� �㬬�
                                         �ਭ���� ������� 㤢��������
     Modified: 29.08.2006 19:25 ELUS     0064978: � �ࠢ�� �뤠������ �� ���㬥��� �த��� �� �㬬�
                                         �ਭ���� ������� 㤢��������
     Modified: 06.03.2007 15:14 rija     0070660: �� �뢮����� ��� ��࠭� � ॥��� 113-� rst-opn.p
     Modified: 06.04.2007 18:07 rija     <comment>
*/

DEFINE VARIABLE mOperRazm AS CHARACTER   NO-UNDO.
mOperRazm = FGetSetting("������136-�","��������","").

/*------------------------------------------------------------------------------
   �����祭��: ��।������ �㬬, ����� ����� � ���-�� 祪�� ��� ������� ॥���
------------------------------------------------------------------------------*/
PROCEDURE GetCodeCurAndSumm:

   DEFINE INPUT  PARAMETER iOp       AS INT64   NO-UNDO.  /* op.op */
   DEFINE INPUT  PARAMETER iPrSumm   AS CHARACTER NO-UNDO.  /* ��᪨ ��⮢, �������� � ���㬬� */
   DEFINE OUTPUT PARAMETER oCurrDb   AS CHARACTER NO-UNDO.  /* ��� �ਭ�⮩ ������ */
   DEFINE OUTPUT PARAMETER oSummaDb  AS DECIMAL   NO-UNDO.  /* ����祭� ����筮�� */
   DEFINE OUTPUT PARAMETER oCurrCr   AS CHARACTER NO-UNDO.  /* ��� �뤠���� ������ */
   DEFINE OUTPUT PARAMETER oSummaCr  AS DECIMAL   NO-UNDO.  /* �뤠�� ����筮�� */
   DEFINE OUTPUT PARAMETER oQtyCheq  AS DECIMAL   NO-UNDO.  /* ���-�� 祪�� */
   DEFINE OUTPUT PARAMETER oCurrCheq AS CHARACTER NO-UNDO.  /* ��� ������ 祪� */
   DEFINE OUTPUT PARAMETER oSumCheq  AS DECIMAL   NO-UNDO.  /* �㬬� �� 祪�� */
   DEFINE OUTPUT PARAMETER oSumRub   AS DECIMAL   NO-UNDO.  /* �㬬� ����樨 � �㡫�� */
   DEFINE OUTPUT PARAMETER oAcctCli  AS CHARACTER NO-UNDO.  /* ��� ������ */

   DEFINE VARIABLE vCurBuy AS CHARACTER INITIAL "-" NO-UNDO. /* */
   DEFINE VARIABLE vSumBuy AS DECIMAL   INITIAL 0   NO-UNDO. /* */
   DEFINE VARIABLE vCurSel AS CHARACTER INITIAL "-" NO-UNDO. /* */
   DEFINE VARIABLE vSumSel AS DECIMAL   INITIAL 0   NO-UNDO. /* */
   DEFINE VARIABLE vBuyQty AS DECIMAL   INITIAL 0   NO-UNDO. /* ���-�� 祪�� */
   DEFINE VARIABLE vSelQty AS DECIMAL   INITIAL 0   NO-UNDO. /* ���-�� 祪�� */
   DEFINE VARIABLE vBuyRub AS DECIMAL   INITIAL 0   NO-UNDO. /* �㬬� "�뤠��" � �� */
   DEFINE VARIABLE vSelRub AS DECIMAL   INITIAL 0   NO-UNDO. /* �㬬� "����祭�" � �� */

   DEFINE VARIABLE vKindOper AS CHARACTER   NO-UNDO. /* �� ��������� �� ���㬥�� */

   mIsCheckBuy = NO. /* Yes - ����� 祪�� �㯨�� */
   mIsCheckSel = NO. /* Yes - ����� 祪�� �த��� */

   DEFINE BUFFER bOp FOR op.
   DEFINE BUFFER bOp-Entry FOR op-entry.   
   DEFINE BUFFER bAcct     FOR acct.
   DEFINE BUFFER kau-entry FOR kau-entry.

   FIND FIRST bOp WHERE bOp.op = iOp
      NO-LOCK
      NO-ERROR.
   IF NOT AVAILABLE bOp THEN
      RETURN.

   /* ��� ����権 �� �� �������� �㬬� ������� �� ���. �������⮢ */
   vKindOper = GetXattrValueEx("op",STRING(bOp.op),"���������",?).
   IF CAN-DO(mOperRazm,vKindOper) THEN
   DO:
      ASSIGN
         vBuyQty     = 0
         vSelQty     = 0
         mIsCheckBuy = FALSE
         mIsCheckSel = FALSE
         vCurBuy     = GetXattrValueEx("op",STRING(bOp.op),"curr-ex","")
         vSumBuy     = DEC(GetXattrValueEx("op",STRING(bOp.op),"amt-ex" ,"x"))
         vCurSel     = vCurBuy
         vSumSel     = vSumBuy
      NO-ERROR.
      IF ERROR-STATUS:ERROR THEN
         RETURN.
   END.
   ELSE
   DO:
/*--------- ��ᬮ�ਬ, �� � ��� �������� � "�ਭ��" --------------*/
      FIND FIRST bOp-Entry WHERE bOp-Entry.op      =  iOp 
                             AND bOp-Entry.acct-db <> ?                           
         NO-LOCK 
         NO-ERROR.
      IF AVAILABLE bOp-Entry THEN
      DO:
         IF CAN-FIND(FIRST kau-entry WHERE 
                           kau-entry.op       =  bOp-Entry.op
                       AND kau-entry.op-entry =  bOp-Entry.op-entry
                       AND kau-entry.acct     =  bOp-Entry.acct-db
                       AND kau-entry.kau-id   BEGINS "��������")
            AND (NOT CAN-DO(iPrSumm,SUBSTRING(bOp-Entry.acct-cr,1,5)) 
                 OR bOp-Entry.acct-cr =  ?) THEN
         DO:
            {find-act.i &bact=bAcct  &acct=bOp-Entry.acct-db}
            IF AVAILABLE bAcct THEN
            DO:
               vCurBuy = bAcct.currency.
               IF GetXAttrValueEx("acct", 
                                  bAcct.Acct + "," + bAcct.currency, 
                                  "form-type-code",
                                  "-") =  "-" THEN /* �� �� ��⠭����� */
                  mIsCheckBuy = FALSE.
               ELSE
                  mIsCheckBuy = TRUE. /* �� 祪� */
            END.
            ASSIGN
               vSumBuy = IF vCurBuy = "" THEN
                            bOp-Entry.amt-rub
                         ELSE
                            bOp-Entry.amt-cur
               vBuyRub = bOp-Entry.amt-rub
               vBuyQty = bOp-Entry.qty
            .
         END.
         ELSE /* �� ���� ����� */
         DO:
            {find-act.i &bact=bAcct  &acct=bOp-Entry.acct-db}
            IF AVAILABLE bAcct THEN
               oAcctCli = bAcct.acct.
            ELSE 
               oAcctCli = "".
         END.
      END.  
   
   /*--------- ��ᬮ�५�, �� � ��� �������� � "�ਭ��" --------------*/
   
   /*--------- ��ᬮ�ਬ, �� � ��� �������� � "�뤠��" ----------------*/
      FIND FIRST bOp-Entry WHERE bOp-Entry.op      =  iOp 
                             AND bOp-Entry.acct-cr <> ? 
         NO-LOCK 
         NO-ERROR.
      IF AVAILABLE bOp-Entry THEN
      DO:
         IF CAN-FIND(FIRST kau-entry WHERE 
                           kau-entry.op       =  bOp-Entry.op
                       AND kau-entry.op-entry =  bOp-Entry.op-entry
                       AND kau-entry.acct     =  bOp-Entry.acct-cr
                       AND kau-entry.kau-id   BEGINS "��������")
            AND (NOT CAN-DO(iPrSumm,SUBSTRING(bOp-Entry.acct-db,1,5)) 
                 OR bOp-Entry.acct-db =  ?) THEN
         DO:
            {find-act.i &bact=bAcct  &acct=bOp-Entry.acct-cr}
            IF AVAILABLE bAcct THEN
            DO:         
               vCurSel = bAcct.currency.
               IF GetXAttrValueEx("acct", 
                                  bAcct.Acct + "," + bAcct.currency, 
                                  "form-type-code",
                                  "-") =  "-" THEN /* �� �� ��⠭����� */
                  mIsCheckSel = FALSE.
               ELSE
                  mIsCheckSel = TRUE. /* �� 祪� */
            END.
            ASSIGN
               vSumSel = IF vCurSel = "" THEN
                            bOp-Entry.amt-rub
                         ELSE
                            bOp-Entry.amt-cur 
               vSelRub = bOp-Entry.amt-rub
               vSelQty = bOp-Entry.qty
            .
         END.
         ELSE /* �� ���� ����� */
         DO:
            {find-act.i &bact=bAcct  &acct=bOp-Entry.acct-cr}
            IF AVAILABLE bAcct THEN
               oAcctCli = bAcct.acct.
            ELSE 
               oAcctCli = "".
         END.
      END.
   END.
/*--------- ��ᬮ�५�, �� � ��� �������� � "�뤠��" --------------*/

/*-- ��।������ ��室��� ��ࠬ��஢ --*/
   IF vCurBuy =  "" THEN
      oCurrDb = mCodOurCur.
   ELSE
      IF vCurBuy =  "-" THEN /* �.� �� �뫮 ��祣� */
         oCurrDb = "".
      ELSE
         oCurrDb = vCurBuy.

   IF vCurSel =  "" THEN
      oCurrCr = mCodOurCur.
   ELSE
      IF vCurSel =  "-" THEN /* �.� �� �뫮 ��祣� */
         oCurrCr = "".
      ELSE
         oCurrCr = vCurSel.

   ASSIGN
      oSummaDb = vSumBuy
      oSummaCr = vSumSel
   .

   IF mIsCheckBuy =  FALSE AND
      mIsCheckSel =  FALSE THEN
      ASSIGN
         oCurrCheq = ""
         oQtyCheq  = 0
         oSumCheq  = 0
      .
   ELSE 
   DO:
      IF mIsCheckBuy =  TRUE THEN
         ASSIGN
            oCurrCheq = vCurBuy
            oQtyCheq  = vBuyQty
            oSumCheq  = vSumBuy
            oSummaDb  = 0
            oCurrDb   = ""
         .
      IF mIsCheckSel =  TRUE THEN
         ASSIGN
            oCurrCheq = vCurSel
            oQtyCheq  = vSelQty
            oSumCheq  = vSumSel
            oSummaCr  = 0
            oCurrCr   = ""
         .
   END.

   oSumRub = MaxSumInOp(bOp.op-date,vCurBuy,STRING(vSumBuy),vCurSel,STRING(vSumSel)). /* �㡫��� �������� �㬬� ����樨 */
/*-- ��।����� ��室�� ��ࠬ���� --*/

END PROCEDURE. /* GetCodeCurAndSumm */

/* ��।������ ��ࠬ��஢ ������ �� ��⠬ �஢���� */
PROCEDURE GetClParamByAcct:

   DEFINE INPUT  PARAMETER iOp          AS INT64     NO-UNDO.  /* op.op */
   DEFINE INPUT  PARAMETER iIsDocGive   AS LOGICAL   NO-UNDO.  /* �㦭� �� ���.� �뤠� */

   DEFINE OUTPUT PARAMETER oCountry     AS CHARACTER NO-UNDO.  /* ��� ��࠭� */
   DEFINE OUTPUT PARAMETER oFIO         AS CHARACTER NO-UNDO.  /* ��� */
   DEFINE OUTPUT PARAMETER oPassport    AS CHARACTER NO-UNDO.  /* ��ᯮ��� ����� */
   DEFINE OUTPUT PARAMETER oAddress     AS CHARACTER NO-UNDO.  /* ���� */
   DEFINE OUTPUT PARAMETER oResident    AS CHARACTER NO-UNDO.  /* �������� */
   DEFINE OUTPUT PARAMETER oBirthDay    AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER oBirthPlace  AS CHARACTER NO-UNDO.

   DEFINE BUFFER bOp-Entry FOR op-entry.   
   DEFINE BUFFER acct-cr   FOR acct.
   DEFINE BUFFER acct-db   FOR acct.

   DEFINE VARIABLE vPerson-id  AS INT64     NO-UNDO INIT -1.
   DEFINE VARIABLE vDover      AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE vOurCountry AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vDocumentId AS CHARACTER NO-UNDO.

   ASSIGN
      oCountry  = ""
      oFIO      = ""
      oPassport = ""
      oAddress  = ""
   .
   vDover = (GetXAttrValue("op",STRING(iOp),"�����")     =  "��") AND
            (FGetSetting("������136-�","�뢄�����","��") =  "��").

   vOurCountry = FGetSettingEx("������",?,?,NO). /* ��� ��࠭� १�����⢠ */
   IF vOurCountry = ? THEN 
      vOurCountry = "RUS".

   xxx1:
   FOR EACH bOp-Entry WHERE bOp-Entry.op =  iOp 
       NO-LOCK:
      {find-act.i &bact=acct-cr &acct=bOp-Entry.acct-cr}
      IF AVAIL acct-cr AND (acct-cr.cust-cat =  "�" OR acct-cr.cust-cat =  "�") THEN 
      DO:
         IF vDover THEN
         DO:
            FIND FIRST deputy WHERE 
                       deputy.acct       =  acct-cr.acct 
                   AND deputy.currency   =  acct-cr.currency
                   AND deputy.right-priv =  YES               
               NO-LOCK NO-ERROR.
            IF AVAIL deputy THEN
               vPerson-id = deputy.person-id.
         END.
         ELSE
         DO:
            vPerson-id = acct-cr.cust-id.
            LEAVE xxx1.
         END.
      END.
      ELSE 
      DO:
         {find-act.i &bact=acct-db &acct=bOp-Entry.acct-db}
         IF AVAIL acct-db AND (acct-db.cust-cat =  "�" OR acct-db.cust-cat =  "�") THEN 
         DO:
            IF vDover THEN
            DO:
               FIND FIRST deputy WHERE 
                          deputy.acct       =  acct-db.acct 
                      AND deputy.currency   =  acct-db.currency
                      AND deputy.right-priv =  YES
                  NO-LOCK NO-ERROR.
               IF AVAIL deputy THEN
                  vPerson-id = deputy.person-id.
            END.
            ELSE
            DO:
                vPerson-id = acct-db.cust-id.
               LEAVE xxx1.
            END.
         END.
      END.
   END.
   IF vPerson-id <> -1 THEN 
   DO:
      FIND FIRST person WHERE 
                 person.person-id =  vPerson-id 
         NO-LOCK NO-ERROR.
      IF AVAIL person THEN
      DO:
         FIND FIRST country WHERE 
                    country.country-id =  person.country-id 
            NO-LOCK NO-ERROR.
         IF AVAIL country THEN
         DO:
            oCountry  = IF country.country-alt-id =  999 THEN 
                           "-" 
                        ELSE 
                           STRING(country.country-alt-id).
            oResident = IF country.country-alt-id =  999 THEN 
                           "-" 
                        ELSE (IF country.country-id =  vOurCountry THEN 
                                 "�" 
                              ELSE 
                                 "�").
         END.

/* ivv 
  ��� �⢥�: १����⭮��� ��।������ �� ���� �������⭮��� �� ����窨 ������. 
  � ��襬 ��砥 �� RUS. �� �����  �뢮����� � �����. 
- (��娭� �.�.) "�� �� �ࠢ��쭮! � ��� 㦥 �� ⠪�� �����, � �� ����﫨, �� ᮣ��᭮ ��������⥫��⢠, 
  � ������ (� 15 �������) ������ �������� ���ଠ�� �� ���� �ࠦ����⢮ ����窨 ������ � ����."

  ��᪮��� ��楤�� ��뢠���� ��� � 9 ��誠�, � ��⠢�塞 �஢��� ⮫쪮 ��� ॥��� ����権 � ������� �� �멤�� ��� 0310371

*/

         if entry(2,PROGRAM-NAME(1)," ") = "rst-136i.p"  then do:
            if   GetXAttrValue("person",STRING(person.person-id),"country-id2") <> "" 
              or GetXAttrValue("person",STRING(person.person-id),"country-id2") <> ?
            then do:
               oCountry = GetXAttrValue("person",STRING(person.person-id),"country-id2"). 
               FIND FIRST country WHERE  country.country-id EQ oCountry
                                      NO-LOCK NO-ERROR. 
               IF AVAILABLE country THEN do:               
                  oCountry =  STRING(country.country-alt-id,"999") .
               end.
               else do:
                  oCountry = "-".
               end.
            end.
         end.

/* end ivv */


         oFIO      = person.name-last + " " + person.first-names.
         oAddress  = person.address[1] + " " + person.address[2].
         oBirthDay = STRING(person.birthday).
         oBirthPlace = GetXAttrValue("person",STRING(person.person-id),"BirthPlace").
         IF iIsDocGive THEN DO:
            vDocumentId = GetCodeName("�������",person.document-id).
            IF NOT {assigned vDocumentId} THEN
               vDocumentId = person.document-id.
            oPassport = vDocumentId + 
                     " " + person.document + 
                     " " + person.issue +  
                     " " + GetXAttrValue("person",
                                         STRING(person.person-id), 
                                         "Document4Date_vid").
         END.
         ELSE DO:
            oPassport = person.document-id + 
                     " " + person.document.
         END.
      END.
   END. 
END PROCEDURE. /* GetClParamByAcct */
/* $LINTFILE='rst-tab.fun' */
/* $LINTMODE='11,-9,6,3' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='soav' */
/* $LINTDATE='07/03/2017 11:27:45.020+03:00' */
/*prosignbXX2LsbNVibmeCCQ7xdamA*/