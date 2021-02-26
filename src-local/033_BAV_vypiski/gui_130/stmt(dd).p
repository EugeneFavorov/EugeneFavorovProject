{globals.i}
{intrface.get tmess}

/* +++ stmt(dd).p was humbly modified by (c)blodd converter v.1.09 on 11/3/2016 9:30am +++ */

{globals.i}
{intrface.get print}
{sh-defs.i}
{pick-val.i}
{chkacces.i}

{getdates.i}

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

if end-date = ? then undo, retry.

ASSIGN mdtBegDate = beg-date /* изолируем даты, чтобы tempacct.i не попортил их */
       mdtEndDate = end-date.
{tempacct.i
   &tempacct_i = "YES"}
ASSIGN beg-date = mdtBegDate /* восстанавливаем испорченное */
       end-date = mdtEndDate.

{stmt.i new }
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
                  &begdate = beg-date
                  &enddate = end-date
                  &bufacct = acct
      }
   END.
   ts.cmode = 2.
   ts.name-vip = "ВЫПИСКА ИЗ ЛИЦЕВОГО СЧЕТА".
END.

for each tmprecid no-lock,
   first acct where recid(acct) = tmprecid.id no-lock
        by acct.bal-acct
        by substr(acct.acct,vSortPoz):

   {on-esc return}
   if not {acctlook.i} then next.

         /* Проверка права доступа к счету по типу клиента */
   IF     acct.cust-cat         EQ "Ч"
      AND mAvGrList NE "*"
      AND NOT GetPersonPermissionIList(mAvGrList,acct.cust-id)
   THEN NEXT.

   {stmt-prt.i &NEXT    ="NEXT"
               &begdate = beg-date
               &enddate = end-date
               &bufacct = acct
   }
end.

{stmtview.i}
{intrface.del}


/* --- stmt(dd).p was humbly modified by (c)blodd converter v.1.09 on 11/3/2016 9:30am --- */
