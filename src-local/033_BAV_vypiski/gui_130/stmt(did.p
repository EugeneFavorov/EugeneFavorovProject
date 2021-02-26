{globals.i}

/* +++ stmt(did.p was humbly modified by (c)blodd converter v.1.09 on 11/3/2016 9:30am +++ */


{intrface.get tmess}

{globals.i}
{intrface.get print}
{sh-defs.i}
{pick-val.i}
{chkacces.i}
{getdate.i}

DEFINE VARIABLE mdtBegDate LIKE beg-date.
DEFINE VARIABLE mdtEndDate LIKE beg-date.

DEFINE VARIABLE mIsSeparate AS LOGICAL    NO-UNDO.
DEFINE VARIABLE mAvGrList   AS CHARACTER  NO-UNDO.

DEFINE VARIABLE mIsMacro     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mMaxWidthTxt AS INT64     NO-UNDO.
DEFINE VARIABLE vLine        AS CHARACTER NO-UNDO.
DEFINE BUFFER   half-entry   FOR op-entry.   /* буфер для stmt-tst.i */
{initstrp.i}
{op-print.pro}

mIsSeparate = FGetSetting("Выписки", "Порядок", "Счета") EQ "Выписка".
/* получаем список доступных пользователю групп клиентов */
mAvGrList = GetRightPersGroup().

IF end-date = ? THEN UNDO, RETRY.
IF NOT CAN-FIND(FIRST op-entry WHERE op-entry.op-date EQ end-date) THEN DO:
   RUN Fill-SysMes IN h_tmess ("", "", "0",
                               "За этот день не было проводок.").
   UNDO, RETRY.
END.

ASSIGN beg-date   = end-date
       mdtBegDate = end-date /* изолируем даты, чтобы tempacct.i не попортил их */
       mdtEndDate = end-date.
{tempacct.i
   &tempacct_i = "YES"}
ASSIGN beg-date = mdtBegDate /* восстанавливаем испорченное */
       end-date = mdtEndDate.

{stmt.i new}
{getstmt.i}

IF mIsSeparate AND ts.cmode EQ 3 THEN
DO:
   ts.cmode = 1.

   FOR EACH tmprecid NO-LOCK,
       FIRST acct WHERE RECID(acct) = tmprecid.id NO-LOCK
                  BY acct.bal-acct
                  BY SUBSTRING(acct.acct,vSortPoz):

      {on-esc RETURN}

      IF NOT {acctlook.i} THEN
         NEXT.

            /* Проверка права доступа к счету по типу клиента */
      IF     acct.cust-cat         EQ "Ч"
         AND mAvGrList NE "*"
         AND NOT GetPersonPermissionIList(mAvGrList,acct.cust-id)
      THEN NEXT.

      {stmt-prt.i &NEXT    ="NEXT"
                  &begdate = end-date
                  &enddate = end-date
                  &bufacct = acct
      }
   END.
   ts.cmode = 2.
   ts.name-vip = "ВЫПИСКА ИЗ ЛИЦЕВОГО СЧЕТА".
END.
FOR EACH tmprecid NO-LOCK,
   FIRST acct WHERE RECID(acct) = tmprecid.id NO-LOCK
        BY acct.bal-acct
        BY SUBSTR(acct.acct,vSortPoz):

   {on-esc LEAVE}
   IF NOT {acctlook.i} THEN NEXT.

         /* Проверка права доступа к счету по типу клиента */
   IF     acct.cust-cat         EQ "Ч"
      AND mAvGrList NE "*"
      AND NOT GetPersonPermissionIList(mAvGrList,acct.cust-id)
   THEN NEXT.

   {stmt-prt.i &NEXT    ="NEXT"
               &begdate = end-date
               &enddate = end-date
               &bufacct = acct
   }
END.
{stmtview.i}

{intrface.del}

/* --- stmt(did.p was humbly modified by (c)blodd converter v.1.09 on 11/3/2016 9:30am --- */
