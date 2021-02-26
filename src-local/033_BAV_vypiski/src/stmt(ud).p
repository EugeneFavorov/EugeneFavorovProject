/*   Башкирев К.В.
        31/07/97 переход от INT64 -> decimal

*/

{globals.i}
{intrface.get print}
{sh-defs.i}
{pick-val.i}
{chkacces.i}

{getdate.i}
&GLOBAL-DEFINE BreakPageOfUser YES


DEFINE VARIABLE mdtBegDate LIKE beg-date.
DEFINE VARIABLE mdtEndDate LIKE beg-date.

DEFINE VARIABLE mIsSeparate AS LOGICAL    NO-UNDO.
DEFINE VARIABLE mAvGrList   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mIsFirst    AS LOGICAL    NO-UNDO.

DEFINE VARIABLE mIsMacro     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mMaxWidthTxt AS INT64     NO-UNDO.
DEFINE VARIABLE vLine        AS CHARACTER NO-UNDO.
/* Вставка Плюс банк */
DEFINE BUFFER   half-entry   FOR op-entry.   /* буфер для stmt-tst.i */
/* Конец вставки Плюс банк */
{initstrp.i}
{op-print.pro}

mIsSeparate = FGetSetting("Выписки", "Порядок", "Счета") EQ "Выписка".
/* получаем список доступных пользователю групп клиентов */
mAvGrList = GetRightPersGroup().

if end-date = ? then undo, retry.
if not can-find(first op-entry where op-entry.op-date eq end-date) then do:
        {message "|За этот день не было проводок"}
        undo, retry.
end.

ASSIGN mdtBegDate = beg-date /* изолируем даты, чтобы getbac.i не попортил их */
       mdtEndDate = end-date.
{getbac.i} 
ASSIGN beg-date = mdtBegDate /* восстанавливаем испорченное */
       end-date = mdtEndDate.

{getuser2.i}
{stmt.i new "/*"}
{getstmt.i}
mIsFirst = YES.

IF mIsSeparate AND ts.cmode EQ 3 THEN
DO:
   ts.cmode = 1.

   for each bal-acct where bal-acct.acct-cat begins in-acct-cat
                       and can-do(bac,string(bal-acct.bal-acct, "99999")) no-lock,
      each acct of bal-acct where (access EQ "" OR acct.user-id EQ access)
                             and acct.acct-cat begins in-acct-cat
       and (if num-entries(list-id) > 1 then can-do(list-id,acct.user-id) else true)
       and can-do(cur,acct.currency)
       and (acct.close-date eq ? or acct.close-date >= end-date)
       and acct.open-date <= end-date
      no-lock
      break by acct.user-id
      by bal-acct.bal-acct
      by substr(acct.acct,vSortPoz)
      on endkey undo, leave:

      {on-esc return}
      if not {acctlook.i} then next.

            /* Проверка права доступа к счету по типу клиента */
      IF     acct.cust-cat         EQ "Ч"
         AND mAvGrList NE "*"
         AND NOT GetPersonPermissionIList(mAvGrList,acct.cust-id)
      THEN NEXT.

      if first-of(acct.user-id) then do:
         find _user where _user._userid = acct.user-id no-lock no-error.
         put unformatted
            skip(2)
            "***** СОТРУДНИК: " + _user._user-name + " (" + _user._userid
            + ") *****"
            skip.
      end.
      {stmt-prt.i &NEXT    ="NEXT"
                  &begdate = end-date
                  &enddate = end-date
                  &bufacct = acct
      }
   end.
   ts.cmode = 2.
   ts.name-vip = "ВЫПИСКА ИЗ ЛИЦЕВОГО СЧЕТА".
END.
mIsFirst = YES.

for each bal-acct where bal-acct.acct-cat begins in-acct-cat
                    and can-do(bac,string(bal-acct.bal-acct, "99999")) no-lock,
   each acct of bal-acct where (access EQ "" OR acct.user-id EQ access)
                          and acct.acct-cat begins in-acct-cat
    and (if num-entries(list-id) > 1 then can-do(list-id,acct.user-id) else true)
    and can-do(cur,acct.currency)
    and (acct.close-date eq ? or acct.close-date >= end-date)
    and acct.open-date <= end-date
   no-lock
   break by acct.user-id
   by bal-acct.bal-acct
   by substr(acct.acct,vSortPoz)
   on endkey undo, leave:

   {on-esc return}
   if not {acctlook.i} then next.

         /* Проверка права доступа к счету по типу клиента */
   IF     acct.cust-cat         EQ "Ч"
      AND mAvGrList NE "*"
      AND NOT GetPersonPermissionIList(mAvGrList,acct.cust-id)
   THEN NEXT.

   if first-of(acct.user-id) then do:
      find _user where _user._userid = acct.user-id no-lock no-error.

      IF ts.vmode-proc BEGINS "stmtb"
         AND ts.break_user EQ YES THEN
         DO:

            IF mIsFirst THEN
               mIsFirst = NO.
            ELSE
               PAGE.
         END.
      put unformatted
         skip(2)
         "***** СОТРУДНИК: " + _user._user-name + " (" + _user._userid
         + ") *****"
         skip.
   end.
   {stmt-prt.i &NEXT    ="NEXT"
               &begdate = end-date
               &enddate = end-date
               &bufacct = acct
   }
end.

{stmtview.i}
{intrface.del}

