DEF VAR MBK_MASK AS CHAR NO-UNDO INIT "30102*,3030*".
DEF VAR NRF_MASK AS CHAR NO-UNDO INIT "30223*,47416*".

DEFINE VARIABLE mNumber    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mOverExist AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mHandle    AS INT64     NO-UNDO.
DEFINE VARIABLE iStat      AS INT64     NO-UNDO.

FUNCTION ClDay RETURN LOGICAL (
   INPUT in-fil      AS CHAR,
   INPUT in-op-date  AS DATE,
   INPUT in-acct-cat AS CHAR):
   DEF BUFFER acct-pos FOR acct-pos.
   FOR LAST acct-pos WHERE 
          acct-pos.filial-id = in-fil
      AND acct-pos.acct-cat  = in-acct-cat
      AND acct-pos.since    >= in-op-date  NO-LOCK:
      RETURN YES.
   END.
   RETURN NO.
END.

FUNCTION OpCurBisStyle RETURN CHARACTER (
   INPUT debacc AS CHAR,
   INPUT credacc AS CHAR):
   IF debacc begins '98' THEN
   DO:
      DEF BUFFER bacc FOR acct.

      FIND FIRST bacc WHERE bacc.acct EQ AddFilToAcct( debacc, shFilial) AND bacc.filial-id EQ shFilial NO-LOCK NO-ERROR.
      IF AVAIL bacc THEN RETURN bacc.currency.
      FIND FIRST bacc WHERE bacc.acct EQ AddFilToAcct( credacc, shFilial) AND bacc.filial-id EQ shFilial NO-LOCK NO-ERROR.
      IF AVAIL bacc THEN RETURN bacc.currency.
   END.
   RETURN 
   (IF SUBSTR(debacc,6,3) NE '810' AND CAN-DO( "!706*,!70701*,!70706*,!90901*,!90902*,!99999*,!99998*,*", debacc)
     THEN SUBSTR(debacc,6,3)
     ELSE
    (IF SUBSTR(credacc,6,3) NE '810' AND CAN-DO( "!706*,!70701*,!70706*,!90901*,!90902*,!99999*,!99998*,*", credacc)
      THEN SUBSTR(credacc,6,3)
      ELSE ''
    )
   ).
END.

FUNCTION sUpdateSigns RETURN LOG(
   INPUT iTable AS CHARACTER,
   INPUT iSurr  AS CHARACTER,
   INPUT iCode  AS CHARACTER,
   INPUT iVal   AS CHARACTER,
   INPUT iIndex AS LOGICAL
   ):
   DEF VAR vOk AS LOGICAL NO-UNDO.

   TR:
   DO
      ON ERROR UNDO TR, RETURN ERROR
      ON STOP  UNDO TR, RETURN ERROR:
      IF GetXattrValueEx( iTable, iSurr, iCode, ?) <> iVal
      THEN vOk = UpdateSigns(iTable, iSurr, iCode, iVal, iIndex).
      ELSE vOk = TRUE.
   END.  /* End of TR BLOCK */
   RETURN vOk.
END FUNCTION.

FUNCTION DocType2BIS RETURNS CHARACTER (INPUT iDocType AS INT, INPUT iCat AS CHAR):
	/* banker.documents */
   CASE iDocType:
      WHEN 101 THEN RETURN '01'.
      WHEN 109 THEN RETURN '04'.
      WHEN 111 THEN RETURN '031'.
      WHEN 134 THEN RETURN '02'.
      WHEN 145 THEN RETURN '037�'.
      WHEN 193 THEN RETURN '016'.
      WHEN 303 THEN RETURN '015'.
      WHEN 601 THEN RETURN '017'.
      WHEN 900 THEN RETURN (IF iCat EQ 'o' THEN '���' ELSE '06').
      WHEN 904 THEN RETURN '037'.
      WHEN 905 THEN RETURN '032'.
      WHEN 908 THEN RETURN '038'.
      WHEN 998 THEN RETURN '09��'.
   END.
   RETURN STRING(iDocType).
END FUNCTION.

DEFINE TEMP-TABLE tt-expdoc LIKE bank.expdoc.
DEFINE TEMP-TABLE tt-post LIKE bank.post-banker.

FUNCTION GetExpDocValue RETURNS CHAR (INPUT ifield_ AS INT, INPUT iDefault AS CHAR):
  DEF BUFFER tt FOR tt-expdoc.
  FIND FIRST tt WHERE tt.field_ EQ ifield_ NO-LOCK NO-ERROR.
  IF AVAIL tt THEN RETURN TRIM( tt.contain).
  RETURN iDefault.
END.

/* ***LoadPostMfr*** ***LoadPostMfr*** ***LoadPostMfr*** ***LoadPostMfr*** ***LoadPostMfr*** ***LoadPostMfr*** ***LoadPostMfr*** ***LoadPostMfr*** ***LoadPostMfr***  */
PROCEDURE LoadPostMfr:
   DEFINE PARAMETER BUFFER post-mfr    FOR bank.post-mfr.
   DEFINE INPUT  PARAMETER iPotok      AS INT64 NO-UNDO.
   DEFINE OUTPUT PARAMETER oCntCreated AS INT64 NO-UNDO.

   DEF BUFFER bpost-mfr      FOR bank.post-mfr.
   DEF BUFFER bpost-mfr-cred FOR bank.post-mfr.
   DEF BUFFER tpost-mfr      FOR bank.post-mfr.
   
   DEF BUFFER blinks         FOR bank.links-mfr.
   DEF BUFFER blinks1        FOR bank.links-mfr.
   DEF BUFFER blinks2        FOR bank.links-mfr.
   
   DEF BUFFER expdoc         FOR bank.expdoc.
   
   DEF BUFFER bbop           FOR op.
   DEF BUFFER signs          FOR signs.
   DEF BUFFER op             FOR op.
   DEF BUFFER op-entry       FOR op-entry.
   DEF BUFFER op-bank        FOR op-bank.
   
   DEF VAR vFdeb      AS CHAR      NO-UNDO.
   DEF VAR vFcred     AS CHAR      NO-UNDO.
   DEF VAR vFil       AS CHAR      NO-UNDO.
   DEF VAR docDID     AS INT64     NO-UNDO.
   DEF VAR tmpStr     AS CHARACTER NO-UNDO.
   DEF VAR tmpStr2    AS CHARACTER NO-UNDO.
   DEF VAR vCust-id   AS INT64     NO-UNDO.
   DEF VAR vOpTransId AS INT64     NO-UNDO.
   
   PUT UNFORMATTED "�஢���� docno = " post-mfr.docno " ��砫� ��ࠡ�⪨" SKIP.   

   oCntCreated = 0.

   vFdeb  = (IF post-mfr.deb_filial_id  EQ '0500'
           AND post-mfr.postdate < DATE( 11, 15, 2014) THEN '0000' ELSE post-mfr.deb_filial_id).
   vFcred = (IF post-mfr.cred_filial_id  EQ '0500'
           AND post-mfr.postdate < DATE( 11, 15, 2014) THEN '0000' ELSE post-mfr.cred_filial_id).
   IF vFdeb EQ vFcred THEN vFil = vFdeb.
   ELSE 
   DO:
      IF post-mfr.postdate NE DATE( 11, 15, 2014) 
         THEN UNDO, THROW NEW Progress.Lang.AppError( "�� ᮢ������ 䨫��� �஢���� �� ������ " + vFdeb + " � �।��� " + vFcred).
      IF      post-mfr.debacc  BEGINS '3030' THEN vFil = vFdeb.
      ELSE IF post-mfr.credacc BEGINS '3030' THEN vFil = vFcred.
      ELSE IF post-mfr.debacc  BEGINS '9999' THEN vFil = vFdeb.
      ELSE IF post-mfr.credacc BEGINS '9999' THEN vFil = vFcred.
      ELSE UNDO, THROW NEW Progress.Lang.AppError( "�� ᮢ������ 䨫��� �஢���� �� ������ " + vFdeb + " � �।��� " + vFcred).
   END.
   
   IF NOT CAN-DO("0000,0300,0500",vFil) THEN
   DO:
      PUT UNFORMATTED "�஢���� " post-mfr.docno ", filial-id=" vFil " �ய�饭�, �� �� 䨫���" SKIP.
      RETURN.
   END.
   
   IF NOT (CAN-DO("1*,2*,3*,4*,5*,6*,7*,9*", post-mfr.debacc) 
      AND  CAN-DO("1*,2*,3*,4*,5*,6*,7*,9*", post-mfr.credacc)    ) THEN
   DO:
      PUT UNFORMATTED "�஢���� docno=" post-mfr.docno " ��� �� �ਭ������� ������� ��� ����������, �ய�饭�" SKIP.
      RETURN.
   END.
   
   IF vFil EQ "0000" /* AND CAN-DO("30102*", post-mfr.debacc) */ 
      THEN UNDO, THROW NEW Progress.Lang.AppError( "���㬥��� �� (0000) �� ��ࠡ�⠭�, ⮫쪮 㤠�����.").
  
   IF vFil EQ "0300"
      AND post-mfr.docno NE 126962388
      AND post-mfr.docno NE 126962635
      AND post-mfr.docno NE 126962636
      AND post-mfr.docno NE 126962637
      AND post-mfr.docno NE 126962638
      AND post-mfr.docno NE 126962639
      AND post-mfr.docno NE 126962640
      AND post-mfr.docno NE 126962641
      AND post-mfr.docno NE 126962642
      AND post-mfr.docno NE 126962643
      AND post-mfr.docno NE 126962644
      AND post-mfr.docno NE 126962645
      AND post-mfr.docno NE 126962646
      AND post-mfr.docno NE 126962647
      AND post-mfr.docno NE 126962648
      AND post-mfr.docno NE 126962649
      AND post-mfr.docno NE 126962650
      AND post-mfr.docno NE 126962651
      AND post-mfr.docno NE 126962652
      AND post-mfr.docno NE 126962653
      AND post-mfr.docno NE 126962654
      AND post-mfr.docno NE 126962655
      AND post-mfr.docno NE 126962656
      AND post-mfr.docno NE 126962657
      AND post-mfr.docno NE 126962658
      AND post-mfr.docno NE 126962659
      AND post-mfr.docno NE 126962660  
      AND post-mfr.docno NE 92295114 
      AND post-mfr.docno NE 92295115
   THEN UNDO, THROW NEW Progress.Lang.AppError( "���㬥��� �� �� (0300) �� ��ࠡ�⠭�, ⮫쪮 㤠�����.").

   DEF VAR entryCount AS INT NO-UNDO.
   DEF VAR curOp AS INT64 NO-UNDO.
   entryCount = 0.
   curOp = ?.
   docDID = post-mfr.did.
   IF docDID EQ ? THEN UNDO, THROW NEW Progress.Lang.AppError( "docDID �� ������ ���� ࠢ�� NULL").
   
   
  
   RELEASE bpost-mfr.
   RELEASE bpost-mfr-cred.
  
   {getomsk-v.i} /*�����*/
  
   IF     (post-mfr.debacc begins '3030' 
      AND (post-mfr.credacc begins '409' or post-mfr.credacc begins '70601')) THEN
   DO:
      /* 10-��� ����� �����⥫� */
      IF GetExpDocValue( 838,?) NE ? THEN
      DO:
         FIND FIRST tt-expdoc WHERE
                 tt-expdoc.field_ EQ 6 
            AND (tt-expdoc.volume EQ ? OR tt-expdoc.volume EQ 1)
         NO-ERROR.
         ASSIGN
            tt-expdoc.contain = GetExpDocValue( 838,?).
      END.
      ELSE
      DO:
         FOR EACH expdoc WHERE
                 expdoc.did    EQ post-mfr.did
            AND (expdoc.volume EQ ? OR expdoc.volume EQ 1)
            AND expdoc.field_  EQ 838
            NO-LOCK QUERY-TUNING ( NO-INDEX-HINT ):
            FIND FIRST tt-expdoc WHERE
               tt-expdoc.field_ EQ 6 
               AND (tt-expdoc.volume EQ ? OR tt-expdoc.volume EQ 1)
            NO-ERROR.
            ASSIGN
               tt-expdoc.contain = expdoc.contain.
         END.
      END.
      
      IF GetExpDocValue( 249,?) NE ? THEN
      DO:
         FIND FIRST tt-expdoc WHERE
                 tt-expdoc.field_  EQ 4 
            AND (tt-expdoc.volume EQ ? OR tt-expdoc.volume EQ 1)
         NO-ERROR.
         ASSIGN
            tt-expdoc.contain = GetExpDocValue( 249,?).
      END.
      ELSE
      DO:
         FOR EACH expdoc WHERE
                expdoc.did EQ post-mfr.did
            AND (expdoc.volume EQ ? OR expdoc.volume EQ 1)
            AND expdoc.field_ EQ 249
            NO-LOCK QUERY-TUNING ( NO-INDEX-HINT ):
            
            FIND FIRST tt-expdoc WHERE tt-expdoc.field_ EQ 4 AND (tt-expdoc.volume EQ ? OR tt-expdoc.volume EQ 1).
            ASSIGN
               tt-expdoc.contain = expdoc.contain.
         END.
      END.
   END.

   IF AVAIL bpost-mfr
      AND CAN-DO( MBK_MASK + ',' + NRF_MASK, post-mfr.credacc) 
      AND GetExpDocValue( 8,?) EQ post-mfr.credacc 
      AND SUBSTR( post-mfr.credacc, 6 , 3) NE '810' THEN
   DO:
      /* ������ ���譨� ४������ ���� �᪠�� � த�⥫�᪮� ���㬥�� � ����⭮� �஢���� */
      DEF BUFFER blinks10 FOR bank.links-mfr.
      DEF BUFFER expdoc10 FOR bank.expdoc.
      FIND FIRST blinks10 WHERE 
             STRING(blinks10.linktype) EQ '17' 
         AND blinks10.id2              EQ bpost-mfr.did 
      NO-LOCK NO-ERROR.
      IF AVAIL(blinks10) THEN
      DO:
         PUT UNFORMATTED " � DID=" bpost-mfr.did " ������ த�⥫�᪨� ���㬥�� �� �裡 ��� ���" SKIP.
         FOR EACH expdoc WHERE
                expdoc.did EQ blinks10.id1
            AND (expdoc.volume EQ ? OR expdoc.volume EQ 1)
            AND (expdoc.field_ EQ 7 OR expdoc.field_ EQ 8 OR expdoc.field_ EQ 10 OR expdoc.field_ EQ 65 OR expdoc.field_ EQ 69)
            NO-LOCK QUERY-TUNING ( NO-INDEX-HINT ):
            
            FIND FIRST tt-expdoc WHERE
               tt-expdoc.field_ EQ expdoc.field_
            NO-ERROR.
            IF NOT AVAIL(tt-expdoc) 
            THEN CREATE tt-expdoc.
            BUFFER-COPY expdoc TO tt-expdoc.
         END.
      END.
      ELSE PUT UNFORMATTED " � DID=" bpost-mfr.did " �� ������ த�⥫�᪨� ���㬥�� �� �裡 ��� ���" SKIP.
    	RELEASE blinks10.
    	RELEASE tt-expdoc.
   END.

   /*ᬮ�ਬ, ����� �� �஢���� �� ���� � �� �� ���� ����㦠�� ----------------------------------------------------*/
   DEFINE BUFFER bopentry FOR op-entry.
   tmpStr  = GetExpDocValue(19, "").
   tmpStr2 = GetExpDocValue(561,"").

   IF tmpStr2 EQ "pk_load_frombis" THEN
   DO:
      PUT UNFORMATTED post-mfr.docno " �஢���� �� ��᪢�� - �ய�饭� (field 561 pk_load_frombis)" SKIP.
      RETURN.
   END.
   
   IF tmpStr <> "" THEN
   DO:
      FIND FIRST bopentry WHERE
             bopentry.op       EQ INTEGER( ENTRY( 1, tmpStr, "."))
         AND bopentry.op-entry EQ INTEGER( ENTRY( 2, tmpStr, "."))
      NO-LOCK NO-ERROR.
      IF AVAIL(bopentry) THEN
      DO:
         IF  post-mfr.debacc  EQ DelFilFromAcct(bopentry.acct-db)
         AND post-mfr.credacc EQ DelFilFromAcct(bopentry.acct-cr) THEN
         DO:
            PUT UNFORMATTED post-mfr.docno " �஢���� �� ��᪢�� - �ய�饭� (field 19)" SKIP.
            RETURN.
         END.
         RELEASE bopentry.
      END.
   END.
   /*ᬮ�ਬ, ����� �� �஢���� �� ���� � �� �� ���� ����㦠�� ----------------------------------------------------*/
   
   shFilial = vFil.

   IF     post-mfr.docno NE 92295114 
      AND post-mfr.docno NE 92295115 THEN
   DO:
      IF  ClDay( vFil, post-mfr.postdate, 
        (IF post-mfr.debacc  BEGINS '98' THEN 'd' ELSE 
        (IF post-mfr.debacc  BEGINS '9' OR post-mfr.credacc BEGINS '9' THEN "o" ELSE "b"))) THEN
      DO:
        UNDO, THROW NEW Progress.Lang.AppError( "���� " + STRING( post-mfr.postdate) + " ������. ��������� ����������.").
      END.
   END.
   
   IF vFil = '0000' AND post-mfr.postdate < date( 2, 9, 2015)
      /*AND NOT CAN-DO("116468157,116468170,116468179,116468159,116468173,116468181",STRING(post-mfr.docno)) */
      THEN UNDO, THROW NEW Progress.Lang.AppError( "���� " + STRING( post-mfr.postdate) + " ������. ��������� ����������.").

   RELEASE op.
   RELEASE op-entry.
   FIND FIRST signs WHERE
          signs.dec-value EQ post-mfr.docno
      AND signs.FILE-NAME EQ 'op-entry'
      AND signs.CODE      EQ 'link-docno'
   NO-LOCK NO-ERROR.
   IF AVAIL(signs) THEN
   DO:
      FIND FIRST op-entry WHERE
             op-entry.op       EQ INTEGER(ENTRY(1,signs.surrogate))
         AND op-entry.op-entry EQ INTEGER(ENTRY(2,signs.surrogate))
      NO-LOCK NO-ERROR.
      IF curOp EQ ? 
      AND AVAIL(op-entry)
      THEN curOp = op-entry.op.
   END.

   FIND FIRST op WHERE op.op EQ curOp NO-ERROR.
   
   IF AVAIL op AND op.filial-id NE shFilial THEN
      UNDO, THROW NEW Progress.Lang.AppError( "�� ᮢ������ 䨫��� ���㬥�� op.op=" + STRING(op.op)).
   IF AVAIL op-entry AND op-entry.filial-id NE shFilial THEN
      UNDO, THROW NEW Progress.Lang.AppError( "�� ᮢ������ 䨫��� �஢���� op-entry.op=" + STRING(op-entry.op)).
   
   IF NOT AVAIL op THEN
   DO:
      IF vOpTransId EQ ? THEN
         vOpTransId = NEXT-VALUE(op-transaction-id).
      
      CREATE op.
      ASSIGN
         op.user-id = 'SYNC'
         op.op-transaction = vOpTransId
         op.filial-id = shFilial
         op.branch-id = shFilial
         op.op-kind = 'absindoc'.
   END.
   
   IF op.op-status EQ CHR(251) + CHR(251) AND post-mfr.docno NE 117771361 THEN
      UNDO, THROW NEW Progress.Lang.AppError("���㬥�� �� ����� ��� ��릠 �� ����� ���� �������").
   ASSIGN
      op.op-date       = IF op.filial-id EQ "0300" AND post-mfr.postdate < DATE("31/12/2013") THEN DATE("31/12/2013") ELSE post-mfr.postdate   
      op.ins-date      = IF op.filial-id EQ "0300" AND post-mfr.postdate < DATE("31/12/2013") THEN DATE("31/12/2013") ELSE post-mfr.postdate
      op.contract-date = IF op.filial-id EQ "0300" AND post-mfr.postdate < DATE("31/12/2013") THEN DATE("31/12/2013") ELSE post-mfr.postdate
      op.acct-cat      = (
      	IF CAN-DO( "93*,94*,95*,96*,97*,99996*,99997*",post-mfr.debacc) THEN 'f'
      	ELSE 
      	IF post-mfr.debacc BEGINS '98' THEN 'd'
      	ELSE 
      	IF post-mfr.debacc BEGINS '9'  OR post-mfr.credacc BEGINS '9'
      	THEN "o"
      	ELSE "b")
      op.op-status = CHR(251) WHEN op.op-status NE CHR(251)
      op.doc-type  = DocType2BIS( post-mfr.doctype, op.acct-cat).
   
   
   
   /* ��।��塞 ⨯ ������᪨� ४����⮢ send,rec,tranz */
   DEF VAR bnk-deb AS LOG NO-UNDO.
   DEF VAR bnk-cred AS LOG NO-UNDO.
   DEF VAR nashBIK AS CHAR NO-UNDO.
   CASE shFilial:
      WHEN '0000' THEN nashBIK = '045209783,044599350,044525129'.
      WHEN '0300' THEN nashBIK = '047106641'.
      WHEN '0400' THEN nashBIK = '044599129'.
      WHEN '0500' THEN nashBIK = '045209884'.
      OTHERWISE UNDO, THROW NEW Progress.Lang.AppError( "�� ���� ��।����� ��� ���").
   END.
   
   bnk-deb  = CAN-DO(MBK_MASK,post-mfr.debacc)  AND GetExpDocValue(4,?) NE post-mfr.debacc.
   bnk-cred = CAN-DO(MBK_MASK,post-mfr.credacc) AND GetExpDocValue(8,?) NE post-mfr.credacc.

   IF CAN-DO( NRF_MASK, post-mfr.debacc)
      AND (NOT CAN-DO( nashBIK, GetExpDocValue( 6,?))) /* ��� ����� ���⥫�騪�  */
   THEN bnk-deb = true.
   IF (CAN-DO( NRF_MASK, post-mfr.credacc) AND
      (NOT CAN-DO( nashBIK, GetExpDocValue( 10,?))) /* ��� ����� �����⥫�  */
      ) OR ( /*shFilial EQ '0000' AND*/ GetExpDocValue( 10,?) EQ '045209783')
   THEN bnk-cred = true.

   DEF BUFFER op-bank-rec  FOR op-bank.
   DEF BUFFER op-bank-send FOR op-bank.
   RELEASE op-bank.
   RELEASE op-bank-rec.
   RELEASE op-bank-send.
  
   IF    bnk-deb  EQ YES
      OR bnk-cred EQ YES THEN
   DO:
      FIND FIRST op-bank WHERE
             op-bank.op           EQ op.op
         AND op-bank.op-bank-type EQ ""
      NO-ERROR.
      IF NOT AVAIL(op-bank) 
         AND bnk-deb NE bnk-cred THEN
      DO:
         CREATE op-bank.
         op-bank.op             = op.op.
         op-bank.bank-code-type = '���-9'.
      END.
   END.
   /* ELSE*/ 
   DO:
      DEF BUFFER bb-op-bank FOR op-bank.
      IF op.op NE ? THEN
      FOR EACH bb-op-bank WHERE
         bb-op-bank.op EQ op.op:
         IF CAN-DO( "rec,send", bb-op-bank.op-bank-type) THEN
         DO:
            IF (bnk-cred EQ false AND bnk-deb EQ false) OR (bnk-cred NE bnk-deb)
            THEN DELETE bb-op-bank.
         END.
         ELSE
         DO:
            IF (bnk-cred EQ false AND bnk-deb EQ false) OR (bnk-cred EQ bnk-deb)
            THEN DELETE bb-op-bank.
         END.
      END.
      RELEASE bb-op-bank.
   END.
   
   op.doc-num = ( /*IF (post-mfr.postype EQ 11) THEN '�1' ELSE*/ GetExpDocValue( 1,?)).
   op.doc-date = DATE( GetExpDocValue( 2, STRING(op.op-date))). 
   op.details = REPLACE( GetExpDocValue( 12,?), '~r~n', '~n').
   op.due-date = DATE( GetExpDocValue( 71,STRING(op.op-date))). 
   op.order-pay = GetExpDocValue( 72, "5"). 

   IF  bnk-cred EQ YES AND bnk-deb EQ NO THEN
   DO: /* �����⥫� � ��㣮� ����� */
      op.name-ben          = GetExpDocValue( 7,?).  /* ������������ �����⥫�                   */
      op.ben-acct          = GetExpDocValue( 8,?).  /* ����� ��� �����⥫� � ����� �����⥫� */
      op-bank.bank-code    = GetExpDocValue( 10,?). /* ��� ����� �����⥫�               */
      op.inn               = GetExpDocValue( 65,?).
      op-bank.corr-acct    = GetExpDocValue( 69,"").
      op-bank.op-bank-type = "".
      op.doc-kind = 'rec'.
   END.
   ELSE IF bnk-deb EQ YES AND bnk-cred EQ NO THEN
   DO: /* ���⥫�騪 � ��㣮� ����� */
      op.name-ben          = GetExpDocValue( 3,?).
      op.ben-acct          = GetExpDocValue( 4,?).
      op-bank.bank-code    = GetExpDocValue( 6,?).
      op.inn               = GetExpDocValue( 64,?).
      op-bank.corr-acct    = GetExpDocValue( 68,"").
      op-bank.op-bank-type = "".
      op.doc-kind = 'send'.
   END.
   ELSE IF bnk-cred EQ YES AND bnk-deb EQ YES THEN
   DO: /* �����⥫� � ���⥫�騪 � ��㣮� ����� */
      FIND FIRST op-bank WHERE
             op-bank.op           EQ op.op
         AND op-bank.op-bank-type EQ ""
      NO-ERROR.
      IF AVAIL(op-bank) THEN DELETE op-bank.

      FIND FIRST op-bank-rec WHERE
             op-bank-rec.op           EQ op.op
         AND op-bank-rec.op-bank-type EQ "rec"
      NO-ERROR.
      IF NOT AVAIL(op-bank-rec) THEN
      DO:
         CREATE op-bank-rec.
         op-bank-rec.op = op.op.
         op-bank-rec.bank-code-type = '���-9'.
      END.
      FIND FIRST op-bank-send WHERE
             op-bank-send.op EQ op.op
         AND op-bank-send.op-bank-type EQ "send"
      NO-ERROR.
      IF NOT AVAIL(op-bank-send) THEN
      DO:
         CREATE op-bank-send.
         op-bank-send.op = op.op.
         op-bank-send.bank-code-type = '���-9'.
      END.
      op.name-ben          = GetExpDocValue( 7,?).  /* ������������ �����⥫�                   */
      op.ben-acct          = GetExpDocValue( 8,?).  /* ����� ��� �����⥫� � ����� �����⥫� */
      op.inn               = GetExpDocValue( 65,?).
      op-bank-rec.bank-code     = GetExpDocValue( 10,?).
      op-bank-rec.corr-acct     = GetExpDocValue( 69,"").
      op-bank-rec.op-bank-type  = "rec".
      op-bank-send.bank-code    = GetExpDocValue( 6,?).
      op-bank-send.corr-acct    = GetExpDocValue( 68,"").
      op-bank-send.op-bank-type = "send".
      op.doc-kind = ''.
   END.
   ELSE
   DO:
      IF (post-mfr.doctype = 904 
         AND post-mfr.debacc BEGINS '202') 
         OR
         ((   post-mfr.doctype = 109 OR post-mfr.doctype = 905) 
         AND post-mfr.credacc BEGINS '202') THEN
      DO:
         FIND FIRST tt-expdoc WHERE tt-expdoc.field_ EQ 50 NO-LOCK NO-ERROR.
         IF AVAIL tt-expdoc THEN
            FIND FIRST clients-abs WHERE
               clients-abs.cid EQ INTEGER(tt-expdoc.contain)
            NO-LOCK NO-ERROR.
         ELSE RELEASE clients-abs.
         IF AVAIL clients-abs THEN
         DO:
            op.name-ben = clients-abs.fname.
            /* ��� ������ 50 ���� �ய��뢠�� � �� */
            PUT UNFORMATTED "�饬 CID=" clients-abs.cid SKIP.
            DEF BUFFER bsigns FOR signs.
            FIND FIRST bsigns WHERE
                   bsigns.file-name EQ 'person'
               AND bsigns.code EQ 'CID'
               AND bsigns.dec-value EQ clients-abs.cid
            NO-LOCK NO-ERROR.
            IF NOT AVAIL bsigns THEN
               PUT UNFORMATTED "�� ��諨 �饬 CID=" clients-abs.cid SKIP.
            ELSE
               PUT UNFORMATTED "��諨 ID=" bsigns.surrogate ", doc-num=" op.doc-num SKIP.
            IF AVAIL bsigns THEN
               sUpdateSigns("opb",STRING(op.op),"IdPers",bsigns.surrogate,?).
         END.
         ELSE op.name-ben = GetExpDocValue( 3,?).
      END. 
      ELSE
      DO:
         op.name-ben = ''.
         op.ben-acct = ''.
         op.inn      = ''.
      END.
   END.
   
   /**/   
   IF AVAIL op-bank-rec 
      AND AVAIL op-bank-send 
      AND op.acct-cat = 'b' 
      THEN op.class-code = 'opbct'.
      ELSE op.class-code = "op" + op.acct-cat.
   IF op.due-date EQ ?
      THEN op.due-date = op.op-date.
   
   VALIDATE op.
   
   PUT UNFORMATTED "ᮧ��� op " op.op SKIP.
   
   IF CAN-DO( MBK_MASK + ',' + NRF_MASK, post-mfr.credacc)
      AND GetExpDocValue( 8,?) NE post-mfr.credacc 
      AND (NOT CAN-DO( MBK_MASK + ',' + NRF_MASK, post-mfr.debacc)) THEN
   DO:
        sUpdateSigns( op.class-code, STRING( op.op), "inn-send", GetExpDocValue( 64,?), ?).
        sUpdateSigns( op.class-code, STRING( op.op), "acct-send", GetExpDocValue( 4,?), ?).
        sUpdateSigns( op.class-code, STRING( op.op), "name-send", GetExpDocValue( 3,?), ?).
        sUpdateSigns( op.class-code, STRING( op.op), "inn-rec",  ?, ?).
        sUpdateSigns( op.class-code, STRING( op.op), "acct-rec", ?, ?).
        sUpdateSigns( op.class-code, STRING( op.op), "name-rec", ?, ?).
   END.
   ELSE IF CAN-DO( MBK_MASK + ',' + NRF_MASK, post-mfr.debacc) AND
      GetExpDocValue( 4,?) NE post-mfr.debacc AND
      (NOT CAN-DO( MBK_MASK + ',' + NRF_MASK, post-mfr.credacc)) THEN
   DO:
      sUpdateSigns( op.class-code, STRING( op.op), "inn-send",  ?, ?).
      sUpdateSigns( op.class-code, STRING( op.op), "acct-send", ?, ?).
      sUpdateSigns( op.class-code, STRING( op.op), "name-send", ?, ?).
      sUpdateSigns( op.class-code, STRING( op.op), "inn-rec", GetExpDocValue( 65,?), ?).
      sUpdateSigns( op.class-code, STRING( op.op), "acct-rec", GetExpDocValue( 8,?), ?).
      sUpdateSigns( op.class-code, STRING( op.op), "name-rec", GetExpDocValue( 7,?), ?).
   END.
   ELSE IF CAN-DO( MBK_MASK + ',' + NRF_MASK, post-mfr.debacc)
	   AND GetExpDocValue( 4,?) NE post-mfr.debacc 
	   AND CAN-DO( MBK_MASK + ',' + NRF_MASK, post-mfr.credacc) 
	   AND GetExpDocValue( 4,?) NE post-mfr.credacc THEN
   DO:
      sUpdateSigns( op.class-code, STRING( op.op), "inn-send", GetExpDocValue( 64,?), ?).
      sUpdateSigns( op.class-code, STRING( op.op), "acct-send", GetExpDocValue( 4,?), ?).
      sUpdateSigns( op.class-code, STRING( op.op), "name-send", GetExpDocValue( 3,?), ?).
      sUpdateSigns( op.class-code, STRING( op.op), "inn-rec", GetExpDocValue( 65,?), ?).
      sUpdateSigns( op.class-code, STRING( op.op), "acct-rec", GetExpDocValue( 8,?), ?).
      sUpdateSigns( op.class-code, STRING( op.op), "name-rec", GetExpDocValue( 7,?), ?).
   END.
   ELSE
   DO:
      sUpdateSigns( op.class-code, STRING( op.op), "inn-send",  ?, ?).
      sUpdateSigns( op.class-code, STRING( op.op), "acct-send", ?, ?).
      sUpdateSigns( op.class-code, STRING( op.op), "name-send", ?, ?).
      sUpdateSigns( op.class-code, STRING( op.op), "inn-rec",  ?, ?).
      sUpdateSigns( op.class-code, STRING( op.op), "acct-rec", ?, ?).
      sUpdateSigns( op.class-code, STRING( op.op), "name-rec", ?, ?).
   END.
   
   IF AVAIL op-bank THEN
   DO:
      ASSIGN
         op-bank.op = op.op
         op-bank.bank-code-type = '���-9'.
      VALIDATE op-bank.
   END.
   IF AVAIL op-bank-send THEN 
      VALIDATE op-bank-send.
   IF AVAIL op-bank-rec THEN 
      VALIDATE op-bank-rec.
   IF NOT AVAIL op-entry THEN
   DO: 
      CREATE op-entry.
      ASSIGN
         op-entry.filial-id = vFil
         op-entry.acct-cat = op.acct-cat.
   END.
   /**/
   IF op-entry.op EQ ? 
   OR op-entry.op NE op.op THEN
   DO:
      DEF BUFFER bop-entry FOR op-entry.
      DEF VAR oldOp AS INT64 NO-UNDO.
      oldOp = op-entry.op.
      FIND LAST bop-entry USE-INDEX op-entry WHERE
         bop-entry.op = op.op
      NO-LOCK NO-ERROR.
      ASSIGN
        op-entry.op       = op.op
        op-entry.op-entry = IF AVAIL(bop-entry) THEN bop-entry.op-entry + 1 ELSE 1.
      RELEASE bop-entry.  
      IF oldOp NE ? AND oldOp NE 0 THEN
      DO:
         FIND LAST bop-entry USE-INDEX op-entry WHERE
            bop-entry.op = oldOp
         NO-LOCK NO-ERROR.
         IF AVAIL bop-entry 
            THEN RELEASE bop-entry.
         ELSE
         DO:
            FIND FIRST bbop WHERE bbop.op EQ oldOp NO-ERROR.
            IF NOT AVAIL(bbop) THEN
            UNDO, THROW NEW Progress.Lang.AppError( "�᪠�� ���� op.op=" + STRING(oldOp)).
            /* ���� 㤠���� ���⮩ ���㬥��????????????????????????????????????*/
            IF AVAIL(bbop) 
            AND bbop.user-id EQ 'SYNC' THEN
            DO:
               FOR EACH op-bank WHERE
                  op-bank.op EQ oldOp:
                  DELETE op-bank.
               END.
               FOR EACH signs WHERE
                      signs.FILE-NAME EQ 'op' 
                  AND signs.surrogate EQ STRING(oldOp):
                  DELETE signs.
               END.
               DELETE bbop.
            END.
            ELSE RELEASE bbop.
         END.
      END.
   END.
   ASSIGN
      op-entry.op-date = IF op.filial-id EQ "0300" 
         AND post-mfr.postdate < DATE("31/12/2013") 
         THEN DATE("31/12/2013") ELSE post-mfr.postdate
      op-entry.op-status = op.op-status
      op-entry.acct-db = AddFilToAcct(post-mfr.debacc, shFilial)
      op-entry.acct-cr =
              (IF SUBSTR(post-mfr.credacc,6,3) EQ SUBSTR(post-mfr.debacc,6,3) 
              OR  SUBSTR(post-mfr.credacc,6,3) EQ '810'
              OR  SUBSTR(post-mfr.debacc,6,3) EQ '810'
              THEN AddFilToAcct(post-mfr.credacc, shFilial)
              ELSE ?)
      op-entry.currency = OpCurBisStyle ( post-mfr.debacc, post-mfr.credacc)
      op-entry.amt-cur = 
         (IF AVAIL bpost-mfr 
         AND TRIM(OpCurBisStyle(post-mfr.debacc,post-mfr.credacc)) <> ""
         THEN bpost-mfr.amount ELSE 0) WHEN op.acct-cat <> 'd'
      op-entry.amt-rub = post-mfr.amount WHEN op.acct-cat <> 'd'
      op-entry.qty     = post-mfr.amount WHEN op.acct-cat EQ 'd'
      op-entry.TYPE    = 
         (IF post-mfr.debacc  BEGINS '2' 
         OR  post-mfr.credacc BEGINS '2' THEN '��' ELSE '��') WHEN op.acct-cat EQ 'b'.
   
   VALIDATE op-entry.
   
   PUT UNFORMATTED "ᮧ��� op-entry" STRING(op-entry.op) + "," + STRING(op-entry.op-entry) SKIP.
   
   sUpdateSigns( "op-entry", 
      STRING( op-entry.op) + "," + STRING(op-entry.op-entry), 
      "link-docno", STRING( post-mfr.docno), ?).
   /*IF AVAIL bpost-mfr THEN*/
   sUpdateSigns( "op-entry", 
      STRING(op-entry.op) + "," + STRING(op-entry.op-entry), 
      "link-vdocno", IF AVAIL bpost-mfr THEN STRING(bpost-mfr.docno) ELSE ?,?).
      
   IF iPotok EQ 5 THEN
   DO:
      sUpdateSigns("op-entry", 
      STRING(op-entry.op) + "," + STRING(op-entry.op-entry), 
      "pc_over","��",?).
      PUT UNFORMATTED "ᮧ��� �� pc_over �� op-entry " STRING(op-entry.op) + "," + STRING(op-entry.op-entry) SKIP.
   END.
   
   DEF BUFFER op-entry2 FOR op-entry.
   IF AVAIL bpost-mfr-cred THEN
   DO:
    	/* ���� ����஢���� ������ᨨ �� �।��� */
    	FIND FIRST op-entry2 WHERE
             op-entry2.op EQ op.op
    	   AND op-entry2.acct-db EQ ? AND op-entry2.acct-cr EQ AddFilToAcct(post-mfr.credacc, shFilial)
    	NO-ERROR.
    	IF NOT AVAIL op-entry2 THEN
      DO:
         FIND LAST bop-entry USE-INDEX op-entry WHERE
            bop-entry.op = op.op
         NO-LOCK NO-ERROR.
    	   CREATE op-entry2.
    	   ASSIGN
    	      op-entry2.op       = op.op
    	      op-entry2.op-entry = if avail bop-entry then bop-entry.op-entry + 1 else 1.
    	   RELEASE bop-entry.
    	END.
    	ASSIGN
         op-entry2.op-date   = IF op.filial-id EQ "0300" AND post-mfr.postdate < DATE("31/12/2013") THEN DATE("31/12/2013") ELSE post-mfr.postdate
         op-entry2.op-status = op.op-status
         op-entry2.currency  = OpCurBisStyle ( post-mfr.credacc, post-mfr.credacc)
	      op-entry2.acct-db   = ?
	      op-entry2.acct-cr   = AddFilToAcct(post-mfr.credacc, shFilial)
	      op-entry2.amt-cur   = bpost-mfr-cred.amount
         op-entry2.amt-rub   = post-mfr.amount WHEN op.acct-cat <> 'd'.
	   
	   VALIDATE op-entry2.
      
      PUT UNFORMATTED "iPotok = " STRING(iPotok) SKIP.
      
      sUpdateSigns( "op-entry", 
          STRING( op-entry2.op) + "," + STRING(op-entry2.op-entry), 
          "link-vdocno", IF AVAIL bpost-mfr-cred THEN STRING( bpost-mfr-cred.docno) ELSE ?, ?).
      IF iPotok EQ 5 THEN
      DO:
         sUpdateSigns("op-entry", 
         STRING(op-entry2.op) + "," + STRING(op-entry2.op-entry), 
         "pc_over","��",?).
         PUT UNFORMATTED "ᮧ��� �� pc_over �� op-entry " STRING(op-entry.op) + "," + STRING(op-entry.op-entry) SKIP.
      END.
   END.
   ELSE
   DO: /* IF AVAIL bpost-mfr-cred */
    	/* �᫨ ���� ����஢����, � �� ���� 㤠���� */
    	FIND FIRST op-entry2 WHERE
             op-entry2.op     EQ op.op
    	   AND op-entry2.acct-db EQ ? 
    	   AND op-entry2.acct-cr EQ AddFilToAcct(post-mfr.credacc,shFilial)
    	NO-ERROR.
    	IF AVAIL op-entry2 THEN
      DO:
    	   DELETE op-entry2.
    	END.
   END. /* IF AVAIL bpost-mfr-cred */

   FOR EACH tt-expdoc NO-LOCK BY tt-expdoc.field_:
      CASE tt-expdoc.field_:
         WHEN 109 THEN sUpdateSigns(op.class-code,STRING(op.op),"�����-�����",tt-expdoc.contain,?).
         WHEN 113 THEN sUpdateSigns(op.class-code,STRING(op.op),"Kpp-send",tt-expdoc.contain,?).
         WHEN 127 THEN sUpdateSigns(op.class-code,STRING(op.op),"Kpp-rec",tt-expdoc.contain,?).
         WHEN 161 THEN sUpdateSigns(op.class-code,STRING(op.op),"�����",tt-expdoc.contain,?).
         WHEN 162 THEN sUpdateSigns(op.class-code,STRING(op.op),"���",tt-expdoc.contain,?).
         WHEN 163 THEN sUpdateSigns(op.class-code,STRING(op.op),"�����",tt-expdoc.contain,?).
         WHEN 164 THEN sUpdateSigns(op.class-code,STRING(op.op),"�����",tt-expdoc.contain,?).
         WHEN 165 THEN sUpdateSigns(op.class-code,STRING(op.op),"�����",tt-expdoc.contain,?).
         WHEN 166 THEN sUpdateSigns(op.class-code,STRING(op.op),"�����",tt-expdoc.contain,?).
         WHEN 167 THEN sUpdateSigns(op.class-code,STRING(op.op),"�����",tt-expdoc.contain,?).
      END CASE.
      
      IF post-mfr.debacc BEGINS '202' THEN
      DO:
         CASE tt-expdoc.field_:
         WHEN 27
            THEN op-entry.symbol = (tt-expdoc.contain).
         END CASE.
      END.
      IF post-mfr.credacc BEGINS '202' THEN
      DO:
         CASE tt-expdoc.field_:
         WHEN 95
            THEN op-entry.symbol = (tt-expdoc.contain).
         END CASE.
      END.
   END.
    
   IF CAN-DO("109,904,905",STRING(post-mfr.doctype))THEN
   DO:
      DEF VAR vdtype AS CHAR NO-UNDO.
      vdtype = GetExpDocValue( 204,?).
      RELEASE bank.glossary.
      IF vdtype NE ? THEN
      DO:
         FIND FIRST bank.glossary WHERE
            glossary.TYPE = 42 
            AND glossary.gid = INTEGER(vdtype) 
         NO-LOCK NO-ERROR.
      END.
      sUpdateSigns(op.class-code,STRING( op.op),"document-id",(IF AVAIL bank.glossary THEN bank.glossary.sname ELSE ""),?).
      sUpdateSigns(op.class-code,STRING( op.op),"����",
         GetExpDocValue(206,?) + " " +  /* doc-ser */ 
         GetExpDocValue(205,?) + ", " + /* doc-no */ 
         GetExpDocValue(139,?) + ", " + /* �뤠� */
         GetExpDocValue(140,?)          /* ��� �뤠� */ 
         ,?).
   END.
   
   /*tmpStr2 = GetExpDocValue(561,"")*/
   PUT UNFORMATTED "tmpStr2 = " tmpStr2 ";" op-entry.acct-db ";" op-entry.acct-cr SKIP.
   IF tmpStr2 EQ "NO_SOFIT" THEN
   DO:
      IF  CAN-DO("40817*,40820*",op-entry.acct-db)
      AND CAN-DO("47423*",op-entry.acct-cr) THEN
      DO:
         sUpdateSigns(op.class-code,STRING(op.op),"W4_export",STRING(NOW,"99.99.9999 HH:MM:SS"),?).
         PUT UNFORMATTED "ᮧ��� �� W4_export �� op = " STRING(op.op) + " ���祭�� = " + STRING(NOW,"99.99.9999 HH:MM:SS") SKIP.
      END.
   END.

   /*
   IF CAN-DO("40817....0599*,40820....0599*",op-entry.acct-cr) THEN
   DO:
      /**/
      mNumber = op-entry.acct-cr.
      mOverExist = NO.
      RUN STORED-PROCEDURE GET_OVERDRAFT_ID mHandle = PROC-HANDLE
         (
         INPUT  PARAM P_ACC            = mNumber,
         INPUT  PARAM P_TYPE_ACC       = 2,
         INPUT  PARAM P_DATE           = TODAY,
         OUTPUT PARAM GET_OVERDRAFT_ID = ?
         ).
      CLOSE STORED-PROC GET_OVERDRAFT_ID iStat = PROC-STATUS.
      
      IF iStat = 0 THEN
      DO:
         IF GET_OVERDRAFT_ID GE 1 THEN
         DO:
            mOverExist = YES.   
         END.
      END.
      /**/
      IF mOverExist EQ YES THEN
      DO:
         op.op-status       = "���".
         op-entry.op-status = "���". 
         IF AVAIL(op-entry2) THEN op-entry2.op-status = "���".
      END.
   END.
   
   VALIDATE op.
   VALIDATE op-entry.
   */
    
   RELEASE op.
   RELEASE op-entry.
    
   PUT UNFORMATTED "docno " post-mfr.docno ", filial-id = " vFil " �ண�㦥�" SKIP.
   oCntCreated = 1.
END PROCEDURE.
