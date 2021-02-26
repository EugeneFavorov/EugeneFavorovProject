/* DEFINE INPUT PARAMETER iParam AS CHARACTER NO-UNDO.
*/

{globals.i}
{sh-defs.i}
{dpsproc.def}
{intrface.get dps}
{intrface.get dpspr}
{intrface.get rights}
{intrface.get tmess}  /* Служба системных сообщений */
/* для множественного выбора из справочников по F1*/
{ttretval.def}

/* подключаем ttnames */
{prn-doc.def &with_proc=YES}


def var beg-date  as date no-undo.
def var toper     as char no-undo.
def var mStrTable as char no-undo.
def var Str       as char no-undo.
def var top-kind  as char no-undo.
DEFINE VARIABLE mFilial       AS CHARACTER   NO-UNDO. /* Номер филиала */
DEFINE VARIABLE mDateB         AS DATE        NO-UNDO. /* Отчетная дата */
DEFINE VARIABLE mDateE         AS DATE        NO-UNDO. /* Отчетная дата */
DEF VAR mFilAvail    AS CHARACTER NO-UNDO.
DEF VAR mUser        AS CHARACTER NO-UNDO.
DEF VAR work-module_ AS CHARACTER NO-UNDO.


beg-date = today.

/*
{getdates.i}
*/

FIND FIRST _user WHERE _user._userid EQ USERID("bisquit")
   NO-LOCK NO-ERROR.
/* Запрос параметров отчетного периода */

ASSIGN 
   mDateB   = today
   mDateE   = gend-date
   mFilial = shfilial
   mUser     = IF AVAIL _User THEN _user._userid ELSE ''
   mFilAvail = GetAllUsrFilial(mUser)
.
PAUSE 0.
DO 
ON ERROR  UNDO, RETURN 
ON ENDKEY UNDO, RETURN
WITH FRAME dateframe2:
   UPDATE
      mDateB LABEL "Отчет C"
             HELP  "F1 - календарь"
      mDateE LABEL "Отчет ПО"
             HELP  "F1 - календарь"
      mFilial LABEL "Код подразделения"
             HELP  "F1 - справочник филиалов"
   WITH CENTERED ROW 10 OVERLAY SIDE-LABELS 1 COL
      COLOR messages TITLE "[ Параметры отчета ]"
   EDITING:
      READKEY.
      IF LASTKEY EQ 301 AND FRAME-FIELD EQ "mDateB"
      THEN DO:
         RUN calend.p.
         IF (LASTKEY EQ 13 OR
             LASTKEY EQ 10) AND
             pick-value NE ? THEN
         DO:
            mDateB = DATE(pick-value).
            DISPLAY mDateB.      
         END.
      END.
      ELSE do:
         IF LASTKEY EQ 301 AND FRAME-FIELD EQ "mDateE"
         THEN DO:
            RUN calend.p.
            IF (LASTKEY EQ 13 OR
               LASTKEY EQ 10) AND
               pick-value NE ? THEN
            DO:
               mDateE = DATE(pick-value).
               DISPLAY mDateE.      
            END.
         END.
         ELSE do:
           IF LASTKEY EQ 301 AND FRAME-FIELD EQ "mFilial" THEN DO:
               mFilial = mFilial:SCREEN-VALUE.
               pick-value = "".
               DO TRANSACTION:
                  work-module_ = work-module.
                  work-module = "base". 
                  RUN browseld.p('branch',
                              'isbank' + '~001' + 'branch-id' + '~001' + 'RetFld',
                              'YES'    + '~001' +  mFilAvail    + '~001' + 'Branch-Id',
                              '',
                              4).   
                  work-module = work-module_. 
               END.
               IF pick-value NE '' AND pick-value NE '?' THEN 
               DO:
                  mFilial = pick-value.   
                  DISPLAY mFilial.    
               END.
            end.
            ELSE do:
               APPLY LASTKEY .
            END.
         END.
      END.
   END.
END.


DEFINE TEMP-TABLE ttOper
   FIELD date-beg       AS DATE
   FIELD date-end       AS DATE
   FIELD filial-id      AS CHARACTER
   FIELD branch-id      AS CHARACTER
   FIELD oper           AS CHARACTER
   FIELD stat           AS CHARACTER
   FIELD op-kind        AS CHARACTER
   FIELD kol-vo         AS int64
   FIELD name           AS char
   FIELD FIO            AS char
   FIELD name-otd       AS char

   INDEX m1 filial-id  branch-id oper op-kind stat .


for each op where op.ins-date >= mDateB 
              and op.ins-date <= mDateE 
              and op.op-status <> "А"
              and op.branch-id = mFilial no-lock.
   if op.user-inspector = "" then do:
      toper = op.user-id.
   end.
   else do:
      toper = op.user-inspector.
   end.
   find first _user where _user._userid = toper no-lock no-error.
   if avail _user then do:
      str = GetUserXAttrValue(_user._userid, "str-branch").
      if str = ? or str = "" then do:
         next. 
      end.
   end.
   else do:
      next.
   end.
   str = GetXattrValueEx("op",STRING(Op.op),"dpr-id",?).
   if str = ? or str = "" then do:
      next. 
   end.
   FIND FIRST code WHERE
   		   code.class = "ОперацииКассы" AND
   		   trim(code.code) = trim(op.op-kind)
   	NO-LOCK NO-ERROR.
   IF AVAIL code THEN DO:
      if code.val = "Исключить" then do:
         next.
      end.
      top-kind = code.parent.
   END.
   ELSE DO:
      top-kind = op.op-kind. 
   END.

   find first ttOper where ttOper.filial-id = op.filial-id
                       and ttOper.branch-id = op.branch-id
                       and ttOper.oper      = toper
                       and ttOper.op-kind   = top-kind
                       no-error.
   if not avail ttOper then do:
      create ttOper.
      assign
         ttOper.date-beg  = mDateB   
         ttOper.date-end  = mDateE    
         ttOper.filial-id = op.filial-id  
         ttOper.branch-id = op.branch-id  
         ttOper.op-kind   = top-kind  
         ttOper.oper      = toper         
         ttoper.fio =  _user._User-Name
      .
   end.
   ttOper.kol-vo   =  ttOper.kol-vo + 1.     
end.
for each ttOper.
   find first op-kind where op-kind.op-kind = ttoper.op-kind no-lock no-error.
   if avail op-kind then do:
      ttoper.name =  op-kind.name-opkind.
   end.
   FIND FIRST code WHERE
   		   code.class = "ОперацииКассы" AND
   		   code.code = ttOper.op-kind
   	NO-LOCK NO-ERROR.
   IF AVAIL code THEN DO:
      ttoper.name =  code.name.
   END.
   find first branch where branch.branch-id = ttoper.branch-id no-lock no-error.
   if avail branch then do:
      ttoper.name-otd =  branch.Short-Name.
   end.
end.

/*
output to "123.txt".
for each ttOper.
   export ttOper.
end.
output close.
*/

RUN BeginCircle_TTName ("opop").

for each ttoper.
   mStrTable =
   TRIM(STRING(ttOper.FIO)) + "~n" +  
   TRIM(STRING(ttOper.name-otd)) + "~n" +  
   TRIM(STRING(ttOper.date-beg)) + "~n" +  
   TRIM(STRING(ttOper.date-end)) + "~n" +  
   TRIM(STRING(ttOper.name)) + "~n" +  
   TRIM(STRING(ttOper.kol-vo))  + "~n" +  
   TRIM(STRING(ttOper.stat))  + "~n" +  
   "`" + TRIM(STRING(ttOper.op-kind))   
   .
   RUN Insert_TTName("mStrTable[opop]",mStrTable).
   RUN NextCircle_TTName("opop").
end.

RUN NextCircle_TTName("opop").
RUN EndCircle_TTName ("opop").
RUN printvd.p ("kass-op1",INPUT TABLE ttnames).


