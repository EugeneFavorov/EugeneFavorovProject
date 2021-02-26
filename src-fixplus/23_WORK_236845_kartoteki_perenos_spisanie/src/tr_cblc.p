/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2007 ЗАО "Банковские информационные системы"
     Filename: TR_CBLC.P
      Comment: Постановка документов на картотеку блокированных счетов
   Parameters:
         Uses:
      Used by:
      Created: 10.01.2008 MUTA  0084518  Изменить алгоритм работы с Картотекой 1
                                 при работе с блокированными счетами   
     Modified: 25.03.2008 MUTA 0087581 НП ТипыБлокК перенесен в группу КартБлСч.                                         
     Modified: 
*/


{globals.i}
{intrface.get tmess}
{intrface.get xclass}
{intrface.get strng}
{intrface.get blkob}
{intrface.get acct}
{intrface.get op}

{tmpobj.def}
{sh-defs.i}

DEF VAR vCrt1Acct  AS CHARACTER NO-UNDO. 
DEF VAR vCrt2Acct  AS CHARACTER NO-UNDO. 
DEF VAR vCurrency  AS CHARACTER NO-UNDO.
DEF VAR vBlTypes   AS CHARACTER NO-UNDO.
DEF VAR vAcct      AS CHARACTER NO-UNDO.
DEF VAR mListBlk   AS CHARACTER NO-UNDO.
DEF VAR mbl-pos    AS DECIMAL   NO-UNDO.
DEF VAR mAmt       AS DECIMAL   NO-UNDO.
DEF VAR mOAmt      AS DECIMAL   NO-UNDO.
DEF VAR mAcctFltN  AS INTEGER INIT 1 NO-UNDO.
DEF VAR mUsrBranch AS CHARACTER NO-UNDO.
deF var shb        as decimal no-undo.
deF var shv        as decimal no-undo.

DEFINE VARIABLE mCnt   AS INTEGER NO-UNDO.
DEFINE VARIABLE mTotal AS INTEGER NO-UNDO.

DEF VAR vAcctFlt AS CHARACTER FORMAT "X(23)" INITIAL "По всем счетам"
  VIEW-AS COMBO-BOX INNER-LINES 3
  LIST-ITEMS "По всем счетам","По счетам ОИ","По счетам отделения ОИ" 
  DROP-DOWN-LIST
  NO-UNDO.

DEFINE BUFFER bacct FOR acct.

DEFINE TEMP-TABLE tAcct
    FIELD id_b AS RECID
    FIELD id_o AS RECID
. 

DEFINE QUERY qAcct  FOR tAcct, bacct, acct SCROLLING.

FORM
   vAcctFlt   LABEL  "Фильтр по счетам " 
              FORMAT "x(23)"
              HELP   "Фильтр по счетам картотеки блокированных счетов"

   vAcct      LABEL  "    Лицевой счет" 
              FORMAT "x(20)"
              HELP   "F1 - выбор лицевого счета "
   vCurrency  LABEL  "          Валюта"
              FORMAT "x(3)"
              HELP   "F1 - выбор вылюты"
   vCrt2Acct  LABEL  "Счет картотеки 2"
              FORMAT "x(20)"
   vCrt1Acct  LABEL  "Счет картотеки 1"
              FORMAT "x(20)"
WITH FRAME frAcct OVERLAY CENTERED ROW 8 SIDE-LABELS 1 COL
TITLE "Постановка на картотеку КартБлСч".

PROCEDURE ShowCartAccts:
   DEFINE PARAMETER BUFFER xacct FOR acct.
   IF AVAIL xacct
   THEN DO WITH FRAME frAcct:
      ASSIGN
         vCrt1Acct = GetXattrValue("acct", xacct.acct + "," + xacct.currency, "Карт1ВнСчет")
         vCrt2Acct = GetXattrValue("acct", xacct.acct + "," + xacct.currency, "Карт2ВнСчет")
      NO-ERROR.
      
      RELEASE bacct.
      IF {assigned vCrt1Acct} THEN DO:
         {find-act.i
            &bact = bacct
            &acct  = "ENTRY(1,vCrt1Acct,',')"
            &curr  = "ENTRY(2,vCrt1Acct,',')"
         }
      END.
      IF AVAIL bacct
      THEN ASSIGN
         vCrt1Acct              = bacct.acct
         vCrt1Acct:SCREEN-VALUE = DelFilFromAcct(vCrt1Acct)
      .
      ELSE ASSIGN
         vCrt1Acct              = ""
         vCrt1Acct:SCREEN-VALUE = ""
      .
      
      RELEASE bacct.
      IF {assigned vCrt2Acct} THEN DO:
         {find-act.i
            &bact = bacct
            &acct  = "ENTRY(1,vCrt2Acct,',')"
            &curr  = "ENTRY(2,vCrt2Acct,',')"
         }
      END.
      IF AVAIL bacct
      THEN ASSIGN
         vCrt2Acct              = bacct.acct
         vCrt2Acct:SCREEN-VALUE = DelFilFromAcct(vCrt2Acct)
      .
      ELSE ASSIGN
         vCrt2Acct              = ""
         vCrt2Acct:SCREEN-VALUE = ""
      .
   END.
END PROCEDURE.

ON F1 OF vAcct DO:

   CASE vAcctFlt:SCREEN-VALUE IN FRAME frAcct:
      WHEN "По счетам ОИ" THEN
      DO:
         mAcctFltN = 1.
      END.
      WHEN "По счетам отделения ОИ" THEN
      DO:
         mUsrBranch = GetUserBranchID(userid("bisquit")).
         mAcctFltN = 2.
      END.
      WHEN "По всем счетам" THEN
      DO:
         mAcctFltN = 3.
      END.
   END CASE.
   
   {empty tAcct}

   FOR EACH acct  NO-LOCK
      WHERE acct.kau-id   EQ "Карт-ка1"
        AND acct.acct-cat EQ "o" 
        AND ((acct.user-id EQ USERID("bisquit") AND
              mAcctFltN EQ 1) OR
             (acct.branch-id EQ mUsrBranch AND
              mAcctFltN EQ 2) OR 
              mAcctFltN EQ 3),
       EACH signs NO-LOCK
      WHERE signs.file-name   EQ "acct"
        AND signs.code        EQ "Карт1ВнСчет"
        AND signs.xattr-value EQ acct.acct + "," + acct.currency,
      FIRST bacct NO-LOCK
      WHERE bacct.acct        EQ ENTRY(1,signs.surrogate)
        AND bacct.currency    EQ ENTRY(2,signs.surrogate) :

      pause 0.
      {on-esc return}

      disp bacct.acct with frame l1 col 2 row 18 color messages top-only overlay side-labels 1 col.

      FIND FIRST tAcct
           WHERE tAcct.id_b EQ RECID(bacct) 
             AND tAcct.id_o EQ RECID(acct) NO-ERROR.
      IF NOT AVAIL tAcct THEN
      DO:
          CREATE tAcct.
          ASSIGN tAcct.id_o = RECID(acct)
                 tAcct.id_b = RECID(bacct)
          .
      END.
   END.
   HIDE FRAME l1 NO-PAUSE.

   FOR EACH bal-acct NO-LOCK
      WHERE bal-acct.kau-id BEGINS "Карт-ка1",
       EACH acct  NO-LOCK
      WHERE acct.kau-id   EQ ?
        AND acct.acct-cat EQ "o" 
        AND acct.bal-acct EQ bal-acct.bal-acct 
        AND ((acct.user-id EQ USERID("bisquit") AND
              mAcctFltN EQ 1) OR
             (acct.branch-id EQ mUsrBranch AND
              mAcctFltN EQ 2) OR 
              mAcctFltN EQ 3),
       EACH signs NO-LOCK
      WHERE signs.file-name   EQ "acct"
        AND signs.code        EQ "Карт1ВнСчет"
        AND signs.xattr-value EQ acct.acct + "," + acct.currency,
      FIRST bacct NO-LOCK
      WHERE bacct.acct        EQ ENTRY(1,signs.surrogate)
        AND bacct.currency    EQ ENTRY(2,signs.surrogate) :

      pause 0.
      {on-esc return}

      disp bacct.acct with frame l2 col 2 row 18 color messages top-only overlay side-labels 1 col.

      FIND FIRST tAcct
           WHERE tAcct.id_b EQ RECID(bacct) 
             AND tAcct.id_o EQ RECID(acct) NO-ERROR.
      IF NOT AVAIL tAcct THEN
      DO:
          CREATE tAcct.
          ASSIGN tAcct.id_o = RECID(acct)
                 tAcct.id_b = RECID(bacct)
          .
      END.
   END.
   HIDE FRAME l2 NO-PAUSE.

   FOR EACH acct  NO-LOCK
      WHERE acct.kau-id   BEGINS "Карт-ка2"
        AND acct.acct-cat EQ "o" 
        AND ((acct.user-id EQ USERID("bisquit") AND
              mAcctFltN EQ 1) OR
             (acct.branch-id EQ mUsrBranch AND
              mAcctFltN EQ 2) OR 
              mAcctFltN EQ 3),
       EACH signs NO-LOCK
      WHERE signs.file-name   EQ "acct"
        AND signs.code        EQ "Карт2ВнСчет" 
        AND signs.xattr-value EQ acct.acct + "," + acct.currency,
      FIRST bacct NO-LOCK
      WHERE bacct.acct        EQ ENTRY(1,signs.surrogate)
        AND bacct.currency    EQ ENTRY(2,signs.surrogate) :

      pause 0.
      {on-esc return}

      disp bacct.acct with frame l3 col 2 row 18 color messages top-only overlay side-labels 1 col.

      FIND FIRST tAcct
           WHERE tAcct.id_b EQ RECID(bacct) 
             AND tAcct.id_o EQ RECID(acct) NO-ERROR.
      IF NOT AVAIL tAcct THEN
      DO:
          CREATE tAcct.
          ASSIGN tAcct.id_o = RECID(acct)
                 tAcct.id_b = RECID(bacct)
          .
      END.
   END.
   HIDE FRAME l3 NO-PAUSE.

   FOR EACH bal-acct NO-LOCK
      WHERE bal-acct.kau-id EQ "Карт-ка2",
       EACH acct  NO-LOCK
      WHERE acct.filial-id EQ shFilial
        AND acct.acct-cat  EQ "o"
        AND acct.bal-acct  EQ bal-acct.bal-acct 
        AND ((acct.kau-id EQ ?) OR 
             (acct.kau-id EQ ""))
        AND ((acct.user-id EQ USERID("bisquit") AND
              mAcctFltN EQ 1) OR
             (acct.branch-id EQ mUsrBranch AND
              mAcctFltN EQ 2) OR 
              mAcctFltN EQ 3),
       EACH signs NO-LOCK
      WHERE signs.file-name   EQ "acct"
        AND signs.code        EQ "Карт2ВнСчет" 
        AND signs.xattr-value EQ acct.acct + "," + acct.currency,
      FIRST bacct NO-LOCK
      WHERE bacct.acct        EQ ENTRY(1,signs.surrogate)
        AND bacct.currency    EQ ENTRY(2,signs.surrogate) :

      pause 0.
      {on-esc return}

      disp bacct.acct with frame l4 col 2 row 18 color messages top-only overlay side-labels 1 col.

      FIND FIRST tAcct
           WHERE tAcct.id_b EQ RECID(bacct) 
             AND tAcct.id_o EQ RECID(acct) NO-ERROR.
      IF NOT AVAIL tAcct THEN
      DO:
          CREATE tAcct.
          ASSIGN tAcct.id_o = RECID(acct)
                 tAcct.id_b = RECID(bacct)
          .
      END.
   END.
   HIDE FRAME l4 NO-PAUSE.

   {empty TmpObj}

   OPEN QUERY qAcct
   PRESELECT EACH tAcct,
      FIRST bacct  NO-LOCK
      WHERE RECID(bacct)  EQ tAcct.id_b,
      FIRST acct  NO-LOCK
      WHERE RECID(acct)  EQ tAcct.id_o.

   mCnt = 0.
   mTotal = NUM-RESULTS("qAcct").

   IF mTotal NE 0 THEN 
   DO:
      GET FIRST qAcct.

      {bar-beg2.i &BarTotal     = "mTotal" 
                  &BarMessage   = "'Контроль блокировок'" }

      DO WHILE AVAIL acct 
         TRANSACTION
         ON ERROR  UNDO, RETURN
         ON ENDKEY UNDO, RETURN :

         mCnt = mCnt + 1.
         {bar2.i &BarPointer = "mCnt" }
         
         RUN CalcAvailPos(bacct.acct, 
                          bacct.currency, 
                          gend-date, 
                          gend-date, 
                          FGetSetting("СтандТр", "AccessStatus", "П"), 
                          "П", 
                          "cli-pos", 
                          no,
                          "*",
                          YES,
                          OUTPUT shb, 
                          OUTPUT shv).

         mAmt = IF bacct.currency NE "" 
                THEN ( IF bacct.side EQ "А" THEN shv ELSE - shv)
                ELSE ( IF bacct.side EQ "А" THEN shb ELSE - shb).
  
         RUN acct-pos IN h_base (acct.acct,
                                 acct.currency,
                                 gend-date,
                                 gend-date,
                                 "√").     
      
         mOAmt = IF bacct.currency NE ""
                 THEN ( IF acct.side EQ "А" THEN sh-val ELSE - sh-val)
                 ELSE ( IF acct.side EQ "А" THEN sh-bal ELSE - sh-bal).	          


         mbl-pos = GetBlockPosition(bacct.acct, 
                                    bacct.currency, 
                                    "*",
			            gend-date).

         mListBlk = BlockAcct(bacct.acct + "," + 
                              bacct.currency,
                              IF date(gend-date) EQ TODAY THEN DATETIME(TODAY,MTIME)
                                                          ELSE DATETIME(date(gend-date) + 1) - 1). 

         IF mOAmt                       NE 0 AND
            ( LOOKUP("Блок",    mListBlk) NE 0 OR 
              LOOKUP("БлокКр",  mListBlk) NE 0 OR
              LOOKUP("БлокДб",  mListBlk) NE 0 OR
              LOOKUP("БлокСумм",mListBlk) NE 0 AND
/*              mAmt LE abs(mbl-pos)             AND   */
              abs(mbl-pos)                NE 0 ) THEN 
         DO:
            CREATE TmpObj.
            ASSIGN TmpObj.rid = recid(bacct).
         END.
         
         GET NEXT qAcct.
      END.
   END.
   {del-bar.i}
   CLOSE QUERY qAcct.

   RUN browseld.p ("acct",
                   "UseTmpObjInQuery",
                   STRING(mTmpObjHand),
                   "",
                   7).

   /*
   RUN browseld.p ("acct",
                   "acct-cat" + CHR(1) + "block-type",
                   "b"        + CHR(1) + vBlTypes,
                   "acct-cat",
                   7).
   */

   IF  LAST-EVENT:FUNCTION NE "END-ERROR"
   AND pick-value          NE ? THEN DO: 
      {find-act.i
         &acct = ENTRY(1,pick-value)
         &curr = ENTRY(2,pick-value)
      }
      IF AVAIL acct THEN DO:
         RUN ShowCartAccts (BUFFER acct).
         IF NOT {assigned vCrt1Acct} AND NOT {assigned vCrt2Acct} THEN DO:
            RUN Fill-SysMes ("", "", "-1", "Счет " + acct.number + " не поставлен на картотеку").
            RETURN NO-APPLY.
         END.
         ASSIGN
            vAcct                  = acct.acct
            vAcct:SCREEN-VALUE     = DelFilFromAcct(vAcct)
            vCurrency:SCREEN-VALUE = acct.currency
         NO-ERROR.
      END.       
   END.
END.

ON F1 OF vCurrency DO:

   RUN browseld.p ("currency",
                  "",
                  "",
                  "",             
                  7).

   IF LAST-EVENT:FUNCTION NE "END-ERROR" AND pick-value NE ?  THEN 
    ASSIGN
       vCurrency:SCREEN-VALUE  = pick-value
     .

END.
  
ON GO OF FRAME frAcct DO:

   {find-act.i
      &acct = TRIM(vAcct:SCREEN-VALUE)
      &curr = TRIM(vCurrency:SCREEN-VALUE)
   }
   IF NOT AVAIL acct THEN DO:
      RUN Fill-SysMes ("", "18l", "", "%s=" + GetNullStr(vAcct:SCREEN-VALUE) + " с валютой " + GetNullStr(vCurrency:SCREEN-VALUE)).
      RETURN NO-APPLY.
   END.
   vAcct = acct.acct.

  IF NOT ChkBlkCrd(acct.acct + "," + acct.currency, DATETIME(gend-date), YES, NO) THEN DO:
      RUN Fill-SysMes ("", "", "-1", "Счет " + acct.number + " с валютой " + acct.currency + " не блокирован.").
      RETURN NO-APPLY.
   END.

   RUN ShowCartAccts (BUFFER acct).
   IF NOT {assigned vCrt1Acct} AND NOT {assigned vCrt2Acct} THEN DO:
      RUN Fill-SysMes ("", "", "-1", "Счет " + acct.number + " не поставлен на картотеку").
      RETURN NO-APPLY.
   END.

END.

vBlTypes = FGetSetting ("КартБлСч", "ТипыБлокК", "").

UPD:
DO TRANSACTION ON ERROR  UNDO UPD, RETRY UPD
               ON ENDKEY UNDO UPD, LEAVE UPD:
   IF RETRY THEN DO:
      HIDE FRAME frAcct.
      RETURN ERROR.
   END.

   UPDATE vAcctFlt
          vAcct
          vCurrency
   WITH FRAME frAcct.

   vAcct = AddFilToAcct(vAcct,shFilial).
   pick-value = vAcct + "," + ENTRY(1,vCrt1Acct,",") + "," + ENTRY(1,vCrt2Acct,",") + ","  + vCurrency.

END.
HIDE FRAME frAcct.

{intrface.del}   

RETURN pick-value.

/*prosignw4TzqDTg2P4EMIiEIqXNFQ*/