{globals.i}

/* +++ tr_cblc2.p was humbly modified by (c)blodd converter v.1.09 on 7/15/2015 6:36am +++ */

/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2007 ЗАО "Банковские информационные системы"
     Filename: TR_CBLC2.P
      Comment: Списание документов с картотеки блокированных счетов
               на картотеку 2
   Parameters:
         Uses:
      Used by:
      Created: 22.12.2007 16:14 MUTA 0084518    
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

DEF VAR vBlAcct    AS CHARACTER NO-UNDO.
DEF VAR vCrtAcct   AS CHARACTER NO-UNDO. 
DEF VAR vCurrency  AS CHARACTER NO-UNDO.
DEF VAR vBlTypes   AS CHARACTER NO-UNDO.
DEF VAR vAcct      AS CHARACTER NO-UNDO.
DEF VAR mListBlk   AS CHARACTER NO-UNDO.
DEF VAR mbl-pos    AS DECIMAL   NO-UNDO.
DEF VAR mAmt       AS DECIMAL   NO-UNDO.
DEF VAR mOAmt      AS DECIMAL   NO-UNDO.
DEF VAR mAcctFltN  AS INTEGER INIT 1 NO-UNDO.
DEF VAR mUsrBranch AS CHARACTER NO-UNDO.
deF var shb        as decimal   no-undo.
deF var shv        as decimal   no-undo.
DEF VAR mI         AS INT64     NO-UNDO.
DEF VAR mTmpStr    AS CHARACTER NO-UNDO.


DEF VAR vAcctFlt AS CHARACTER FORMAT "X(23)" INITIAL "По счетам ОИ"
  VIEW-AS COMBO-BOX INNER-LINES 3
  LIST-ITEMS "По счетам ОИ","По счетам отделения ОИ", "По всем счетам" 
  DROP-DOWN-LIST
  NO-UNDO.



DEFINE BUFFER bacct FOR acct.

FORM
   vAcctFlt   LABEL  "Фильтр по счетам " 
              FORMAT "x(23)"
              HELP   "Фильтр по счетам картотеки блокированных счетов"
   vBlAcct    LABEL  "Счет картотеки   " 
              FORMAT "x(20)"
              HELP   "F1 - выбор счета картотеки блокированных счетов"
   vCurrency  LABEL  "Валюта           "
              FORMAT "x(3)"
              HELP   "F1 - выбор вылюты"
   vCrtAcct   LABEL  "Счет картотеки 2 "
              FORMAT "x(20)"
WITH FRAME frAcct OVERLAY CENTERED ROW 8 SIDE-LABELS 1 COL
   TITLE "Списание документов на картотеку 2".

ON F1 OF vBlAcct DO:
   DEFINE VAR vInt  AS INT64 NO-UNDO.
   DEFINE VAR vSurr AS CHAR    NO-UNDO.

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

   {empty TmpObj}
   FOR EACH acct  NO-LOCK
      WHERE acct.kau-id               EQ "КартБлСч"
        AND acct.bal-acct             EQ 90901
        AND acct.acct-cat             EQ "o"
        AND ((acct.user-id EQ USERID("bisquit") AND
              mAcctFltN EQ 1) OR
             (acct.branch-id EQ mUsrBranch AND
              mAcctFltN EQ 2) OR 
             mAcctFltN EQ 3),
       EACH signs NO-LOCK
      WHERE signs.file-name           EQ "acct"
        AND signs.code                EQ "КартБВнСчет"
        AND signs.code-value EQ acct.acct + "," + acct.currency,
      FIRST bacct NO-LOCK
      WHERE bacct.acct                EQ ENTRY(1,signs.surrogate)
        AND bacct.currency            EQ ENTRY(2,signs.surrogate) :

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


      mTmpStr = "".
      DO mI = 1 TO NUM-ENTRIES(mListBlk) :
         FIND FIRST code 
              WHERE code.class EQ "acct-status"
                AND code.code  EQ ENTRY(mI,mListBlk) NO-LOCK NO-ERROR.
         IF AVAIL code AND 
            {assigned code.misc[1]} THEN
         {additem.i mTmpStr code.misc[1]}
      END.
      IF {assigned mTmpStr} THEN
      mListBlk = mTmpStr.


      IF mOAmt                       EQ 0 OR
         LOOKUP("Блок",    mListBlk) NE 0 OR 
         LOOKUP("БлокКр",  mListBlk) NE 0 OR
         LOOKUP("БлокДб",  mListBlk) NE 0 OR
         LOOKUP("БлокСумм",mListBlk) NE 0 AND
         mAmt LE abs(mbl-pos)             AND 
	 abs(mbl-pos)                NE 0 THEN NEXT.

      CREATE TmpObj.
      ASSIGN TmpObj.rid = recid(acct).

   END.

   RUN browseld.p ("acct",
                   "UseTmpObjInQuery",
                   STRING(mTmpObjHand),
                   "",
                   7).

   /*
   RUN browseld.p ("acct",
                   "acct-cat" + chr(1) + "bal-acct" + chr(1) + "kau-id",
                   "o"        + chr(1) + "90901"    + chr(1) + "КартБлСч",
                   "acct-cat" + chr(1) + "bal-acct" + chr(1) + "kau-id",
                   7).
   */

   IF  {&LAST_EVENT_FUNCTION} NE "END-ERROR"
   AND pick-value          NE ? THEN DO:
      {find-act.i
         &acct = ENTRY(1,pick-value)
         &curr = ENTRY(2,pick-value)
      }
      IF AVAIL acct THEN DO:
         RUN FindSignsByVal IN h_xclass ("acct", "КартБВнСчет", pick-value, OUTPUT vInt, OUTPUT vSurr).
         RELEASE bacct.
         IF {assigned vSurr}
         THEN FIND FIRST bacct WHERE bacct.acct     = ENTRY(1,vSurr)
                                 AND bacct.currency = ENTRY(2,vSurr) NO-LOCK NO-ERROR.
         IF NOT AVAIL bacct THEN DO:
            RUN Fill-SysMes ("", "", "-1", "Для счета " + acct.number + " не найден балансовый счет.").
            {return_no_apply.i}
         END.
         vCrtAcct = GetXattrValue("acct", vSurr, "Карт2ВнСчет").
         ASSIGN
            vBlAcct                = acct.acct
            vBlAcct:SCREEN-VALUE   = DelFilFromAcct(acct.acct)
            vCurrency:SCREEN-VALUE = acct.currency
            vCrtAcct:SCREEN-VALUE  = DelFilFromAcct(ENTRY(1,vCrtAcct,","))
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

   IF {&LAST_EVENT_FUNCTION} NE "END-ERROR" AND pick-value NE ?  THEN 
    ASSIGN
       vCurrency:SCREEN-VALUE  = pick-value
     .

END.

ON GO OF FRAME frAcct  DO: 
   DEFINE VAR vInt  AS INT64 NO-UNDO.
   DEFINE VAR vSurr AS CHAR    NO-UNDO.

   {find-act.i
      &acct = TRIM(vBlAcct:SCREEN-VALUE)
      &curr = TRIM(vCurrency:SCREEN-VALUE)
   }
   IF NOT AVAIL acct THEN DO:
      RUN Fill-SysMes ("", "18l", "", "%s=" + GetNullStr(vBlAcct:SCREEN-VALUE) + " с валютой " + GetNullStr(vCurrency:SCREEN-VALUE)).
      {return_no_apply.i}
   END.
   vBlAcct = acct.acct.

   IF acct.kau-id <>  "КартБлСч" THEN DO:
      RUN Fill-SysMes ("", "", "-1", "Код шаблона КАУ счета " + acct.acct + " не  КартБлСч.").
      {return_no_apply.i}
   END.

   RUN FindSignsByVal IN h_xclass ("acct", "КартБВнСчет", acct.acct + "," + acct.currency, OUTPUT vInt, OUTPUT vSurr).
   RELEASE bacct.
   IF {assigned vSurr}
   THEN FIND FIRST bacct WHERE bacct.acct     = ENTRY(1,vSurr)
                           AND bacct.currency = ENTRY(2,vSurr) NO-LOCK NO-ERROR.
   IF NOT AVAIL bacct THEN DO:
      RUN Fill-SysMes ("", "", "-1", "Для счета " + acct.acct + " не найден балансовый счет.").
      {return_no_apply.i}
   END.

   vAcct = bacct.acct.

   IF ChkBlkCrd(bacct.acct + "," + bacct.currency,DATETIME(gend-date),NO,NO) THEN DO:
      RUN Fill-SysMes ("", "", "-1", "Счет " + bacct.number + " с валютой " + bacct.currency + " блокирован.").
      {return_no_apply.i}
   END.

   vCrtAcct = GetXattrValue("acct", vSurr, "Карт2ВнСчет"). 

   IF {assigned vCrtAcct} THEN
      vCrtAcct:SCREEN-VALUE = DelFilFromAcct(ENTRY(1,vCrtAcct,",")) NO-ERROR.

END.

vBlTypes = FGetSetting ("КартБлСч", "ТипыБлокК", "").

UPD:
DO TRANSACTION ON ERROR  UNDO UPD, RETRY UPD
               ON ENDKEY UNDO UPD, LEAVE UPD:
   IF RETRY THEN DO:
      HIDE FRAME frAcct.
      RETURN ERROR.
   END.

   {update.i &THIS_FRAME = "frAcct" &EXFILE = "tr_cblc2.p.st1" {&*}} .

   vBlAcct = AddFilToAcct(vBlAcct,shFilial).
   pick-value = vBlAcct + "," + ENTRY(1,vCrtAcct,",") + "," + vCurrency + "," + vAcct.

END.
HIDE FRAME frAcct.

{intrface.del}

RETURN pick-value.

/* --- tr_cblc2.p was humbly modified by (c)blodd converter v.1.09 on 7/15/2015 6:36am --- */
