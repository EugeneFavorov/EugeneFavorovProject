
/* +++ e-ac950.i was humbly modified by (c)blodd converter v.1.11 on 5/25/2017 6:45am +++ */

/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2014 ЗАО "Банковские информационные системы"
     Filename: E-AC950.I
      Comment: Экспорт сообщений МТ940,942,950 формата swift/telex
   Parameters: in-mail-user-num - номер абонента
               in-str-acct      - маска счетов
               in-op-date       - начальная дата
               in-op-date2      - конечная дата
               in-file-name     - имя файла
         Uses: e-tel100.def
               sh-defs.i
               chkacces.i 
               bank-id.i
               wordwrap.def
      Used by: e-tel950.p e-swo950.p e-tel940.p e-swo940.p e-tel942.p
      Created: 21/08/00 Mkv
     Modified: 
*/
Form "~n@(#) E-AC950.I 1.0 Mkv 21/08/00 Экспорт сообщений МТ940,942,950 формата swift/telex"
with frame sccs-id width 250.

{m-u-acct.def}
DEF INPUT        PARAM in-mail-user-num LIKE mail-user.mail-user-num NO-UNDO.
DEF INPUT        PARAM TABLE FOR mail-user-acct.
DEF INPUT        PARAM in-op-date       LIKE op-date.op-date         NO-UNDO.
DEF INPUT        PARAM in-op-date2      LIKE op-date.op-date         NO-UNDO.
DEF INPUT        PARAM in-file-name     AS CHAR                      NO-UNDO.

/* объявление переменных */
{e-tel100.def new}
{intrface.get "xclass"}
{intrface.get count}
{sh-defs.i}
{chkacces.i}
{bank-id.i}    /* все реквизиты нашего банка */
{wordwrap.def} /* делит исходный текст на части */

{debug.equ}

DEF SHARED VAR num-op-stmt AS INT64 NO-UNDO.

DEF VAR buf-ref       AS CHAR NO-UNDO.
DEF VAR cr-first-op   AS INT64  NO-UNDO.
DEF VAR flag-last     AS LOG  NO-UNDO.
DEF VAR flag-ambig    AS LOG  NO-UNDO INIT no.
DEF VAR in-series-num AS CHAR NO-UNDO INIT "1".
DEF VAR in-acct       AS CHAR NO-UNDO.
DEF VAR in-currency   AS CHAR NO-UNDO.
DEF VAR in-op-kind    AS CHAR NO-UNDO. /*для e-swift.72*/
DEF VAR in-user-num   AS INT64  NO-UNDO. /* правило обмена                 */
DEF VAR last-op       AS INT64  NO-UNDO.
DEF VAR mCrAmtRub     AS LOG  NO-UNDO. /*значение др cr-amt-rub на транзакции*/
DEF VAR nomdoc        AS INT64  NO-UNDO. /* кол-во документов              */
DEF VAR num-stmt      AS INT64  NO-UNDO.
DEF VAR num-pach      AS INT64  NO-UNDO.
DEF VAR sv            LIKE op-entry.amt-rub NO-UNDO.
DEF VAR sr            LIKE op-entry.amt-cur NO-UNDO.
DEF VAR vstr          AS CHAR NO-UNDO INIT "link-op-sum".
DEF VAR zop           AS CHAR NO-UNDO.
DEF VAR vIsDayClosing AS LOGICAL      NO-UNDO.
DEF VAR vOpDate       AS DATE         NO-UNDO.

DEFINE VARIABLE mBalance  AS DECIMAL     NO-UNDO.
DEFINE VARIABLE mBalanceS AS DECIMAL     NO-UNDO.
DEFINE VARIABLE mLastDate AS DATE        NO-UNDO.
DEFINE VARIABLE mCurDate  AS DATE        NO-UNDO.

{findcat.i &cat-source = op-kind}      /* для {chkdate1.i */    
IF NOT {assigned mCats} THEN mCats = "b". 

DEF BUFFER cop-entry  FOR op-entry.
DEF BUFFER dop-entry  FOR op-entry.
DEF BUFFER xop-entry  FOR op-entry.
DEF BUFFER z-op-entry FOR op-entry.
/* Вставка Плюс банк */
DEF BUFFER half-entry FOR op-entry.
/* Конец вставки Плюс банк */
DEF BUFFER z-op       FOR op.
DEF BUFFER xop-date   FOR op-date.

{intrface.get strng}

DEF TEMP-TABLE topen no-undo
        field rec-id AS RECID 
        field db     AS LOG 
        field num-op AS INTEGER
        field acct   AS CHAR
INDEX acct IS PRIMARY acct.

DEF STREAM err.

/* иницилизация */
IF "{&format-message}" EQ "SWIFT" THEN
   ASSIGN
      end-file   = "-~}"
      end-msg    = "-~}"
      begin-msg  = "~{2:"
      begin-file = "~{2:".
ELSE
   ASSIGN
      end-file   = "NNNN"
      end-msg    = "QQ"
      begin-msg  = "::"
      begin-file = "YZYZ".

h-tt-app = widget-handle(GetSysConf("bss-app")).

&IF DEFINED (IS-DEBUG) &THEN
   RUN dbgprint.p ("e-ac950.i","in-mail-user-num:" + STRING(in-mail-user-num)).    
&ENDIF

FIND FIRST mail-user WHERE mail-user.mail-user-num EQ in-mail-user-num NO-LOCK NO-ERROR.
FIND FIRST op-kind   WHERE op-kind.op-kind EQ mail-user.op-kind-exp    NO-LOCK NO-ERROR.
FIND FIRST op-templ  OF    op-kind                                     NO-LOCK NO-ERROR.
mCrAmtRub = GetXattrValue("op-kind",op-kind.op-kind,"cr-amt-rub") NE "Нет".


assign
   in-op-kind = op-kind.op-kind
   tmp-status = gop-status.
IF AVAILABLE(op-template)    AND
   op-templ.op-status NE ""  AND
   op-templ.op-status NE ?   AND
   op-templ.op-status NE "?" THEN tmp-status = op-templ.op-status.

in-user-num = mail-user.mail-user-num.

{e-swofile.pro}

&IF DEFINED(IS-DEBUG) &THEN
RUN dbgprint.p ("e-ac950.i","beg-date:" + GetNullDat(in-op-date) +
                            "end-date:" + GetNullDat(in-op-date2)).
&ENDIF

FOR EACH mail-user-acct WHERE
   mail-user-acct.num EQ mail-user.mail-user-num AND
   mail-user-acct.a   EQ YES
/* Вставка Плюс банк */
NO-LOCK,
FIRST acct
   WHERE acct.acct      EQ mail-user-acct.acct
     AND acct.currency  EQ mail-user-acct.currency
/* Конец вставки Плюс банк */
NO-LOCK BY mail-user-acct.acct:

   last-op = 0.
/* Вставка Плюс банк */
   Op-Block:
/* Конец вставки Плюс банк */
   FOR EACH {&pre}op-entry WHERE
     (
     ({&pre}op-entry.op-date   >=     in-op-date               AND
      {&pre}op-entry.op-date   <=     in-op-date2              AND
      {&pre}op-entry.acct-db   EQ     mail-user-acct.acct
      &IF DEFINED(ORACLE) = 0 &THEN
      AND
      {&pre}op-entry.currency  BEGINS mail-user-acct.currency  AND
/* Замена Плюс банк
      {&pre}op-entry.op-status GE     tmp-status
*/    {&pre}op-entry.op-status GE     "Ф"
/* Конец замены Плюс банк */
      &ENDIF
     )
     OR
     ({&pre}op-entry.op-date   >=     in-op-date               AND
      {&pre}op-entry.op-date   <=     in-op-date2              AND
      {&pre}op-entry.acct-cr   EQ     mail-user-acct.acct
      &IF DEFINED(ORACLE) = 0 &THEN
      AND
      {&pre}op-entry.currency  BEGINS mail-user-acct.currency  AND
/* Замена Плюс банк
      {&pre}op-entry.op-status GE     tmp-status
*/    {&pre}op-entry.op-status GE     "Ф"
/* Конец замены Плюс банк */
      &ENDIF
     )
     )
     AND
     ( IF {&pre}op-entry.currency NE "" AND
         {&pre}op-entry.amt-cur EQ 0 THEN NO ELSE YES )

/* Вставка Плюс банк */
      NO-LOCK,
   FIRST op OF {&pre}op-entry
/* Конец вставки Плюс банк */
      NO-LOCK:
/* Вставка Плюс банк */
      IF ({&pre}op-entry.acct-db EQ mail-user-acct.acct)
      THEN DO:
         {e-ac950-tst.i &label=Op-Block &corr=cr {&*}}
      END.
      ELSE DO:
         {e-ac950-tst.i &label=Op-Block &corr=db {&*}}
      END.
/* Конец вставки Плюс банк */
      &IF DEFINED(ORACLE) &THEN
      IF NOT (    {&pre}op-entry.currency  BEGINS mail-user-acct.currency
/* Замена Плюс банк
              AND {&pre}op-entry.op-status GE     tmp-status)
*/            AND {&pre}op-entry.op-status GE     "Ф")
/* Конец замены Плюс банк */
      THEN NEXT.
      &ENDIF
      zop = GetXAttrValue("op",string(op-entry.op),vstr).
      IF zop NE "" THEN DO:
         i = num-link("op",vstr,string(op-entry.op)).

         IF i NE 0 THEN
         FIND FIRST xop-entry WHERE xop-entry.op EQ INT64(zop) NO-LOCK NO-ERROR.
      END.
      ELSE FIND FIRST xop-entry WHERE recid(xop-entry) EQ recid(op-entry) NO-LOCK NO-ERROR.

      IF (i EQ 1 AND zop NE "") OR zop EQ "" THEN
      DO:
         CREATE topen.
         ASSIGN
           topen.db    = {&pre}xop-entry.acct-db EQ mail-user-acct.acct
           topen.acct  = mail-user-acct.acct
           topen.rec-id = recid({&pre}xop-entry)
         .
      END.
   END.
END.

mLastDate = ?.
MAIN:
FOR EACH topen,
   FIRST op-entry WHERE recid(op-entry) EQ topen.rec-id NO-LOCK,
   FIRST op OF op-entry NO-LOCK 
   BREAK 
    BY topen.acct
    BY topen.db desc
    BY op-entry.op-date
    BY ( IF substr(topen.acct,6,3) EQ "{&in-NC-Code}"
           THEN op-entry.amt-rub
           ELSE op-entry.amt-cur):

   &IF DEFINED (IS-DEBUG) &THEN
      RUN dbgprint.p ("e-ac950.i","MAIN 1: topen.acct:" + topen.acct +
         " topen.db:" + STRING(topen.db) +
         " op-entry.op-date:" + GetNullDat(op-entry.op-date)).    
   &ENDIF

   /* проверка на закрытый или заблокированный день                           */


   vOpDate = op-entry.op-date.

   {daylock.i
    &in-op-date = vOpDate
    &lock-type  = SHARE
    &cats       = mCats
    &return-mes = vMsg
    }

   IF {assigned vMsg}  THEN DO:
      IF auto THEN DO:
         UNDO MAIN, LEAVE MAIN.
      END.
      ELSE DO:
         DO:
            mblodd_char_Tmp01 = pick-value.
            RUN Fill-AlertSysMes IN h_tmess("","",4,"~n" + CHR(32) + "Продолжить экспорт выписки ?").
            DEFINE VARIABLE choice AS LOGICAL NO-UNDO.
            choice = (pick-value = "YES").
            pick-value = mblodd_char_Tmp01.
         END.

         IF choice NE YES THEN UNDO MAIN, LEAVE MAIN.
      END. 
   END.

   &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("e-ac950.i"," acct:"    + GetNullStr(topen.acct) +
                            " op-date:" + GetNullDat(op.op-date)).
   &ENDIF

   accum topen.acct (count BY topen.acct).
   last-op = accum count BY topen.acct topen.acct.

   zop = GetXAttrValue("op",string(op.op),vstr).
   IF zop NE "" THEN DO:
      FIND FIRST xop-entry WHERE xop-entry.op EQ INT64(zop) NO-LOCK NO-ERROR.
   END.

   FIND FIRST acct WHERE acct.acct EQ topen.acct NO-LOCK NO-ERROR.
   IF AVAIL acct
      THEN in-currency = acct.currency.
      ELSE in-currency = op-entry.currency.

   IF (last-op modulo num-op-stmt EQ 1) OR num-op-stmt EQ 1 OR
   first-of(topen.acct) THEN DO:
      ASSIGN
         s950-db = 0
         s950-cr = 0.

    /* формирование имени файла экспорта */
      ASSIGN
         in-file-name = SUBSTRING(op-kind.op-kind,5,3)   + 
                        GetFrmtSeriesNum(in-series-num)  +
                        STRING(GetCounterNextValue("SwiftNumFile", gend-date),"999999")  +
                        "." + IF   "{&format-message}" =  "telex"
                              THEN FGetSetting("SWIFT","TelExt","tiv")
                              ELSE FGetSetting("SWIFT","SwoExt","siv")
      .

    /* преобразование суммы в строку */
      ASSIGN
         buf-s = IF in-currency EQ ""
                    THEN TRIM(string(op-entry.amt-rub,"->>>>>>>>>>>9.99"))
                    ELSE TRIM(string(op-entry.amt-cur,"->>>>>>>>>>>9.99"))
      OVERLAY(buf-s,INDEX(buf-s,"."),1) = ",".

    /* преоразование кода валюты */

      FIND FIRST mail-user WHERE
                 mail-user.mail-user-num EQ in-mail-user-num NO-LOCK NO-ERROR.
    /* формирование именя файла экспорта */
      ASSIGN
         outfile = mail-user.file-exp + "/" + in-file-name
         id-rel-mess = "".

      IF NOT session:remote THEN DO:
         OUTPUT TO VALUE(in-file-name).
      END.
      {rel-date.i &in-op-date = vOpDate}
   END.

   {e-950i.i {&*}}

   {sw-msg.cpy &f950=yes}

   IF (last-op modulo num-op-stmt EQ 0) OR
      num-op-stmt                 EQ 1  OR
      last-of(topen.acct)               THEN DO:

   /* Строка окончания */
      IF FGetSetting("NoCaps",?,"") EQ "Да" OR CAN-DO("RUR5,RUR6",VerForm)
         THEN.
         ELSE RUN caps-buf-copy-doc.

      IF id-rel-mess NE "" AND NUM-ENTRIES(id-rel-mess) EQ 4 THEN DO:
         FIND FIRST msg-impexp WHERE
                    msg-impexp.msg-cat   EQ ENTRY(1,id-rel-mess) AND
                    msg-impexp.object-id EQ ENTRY(2,id-rel-mess) AND
                    msg-impexp.msg-type  EQ ENTRY(3,id-rel-mess) AND
                    msg-impexp.msg-batch EQ ENTRY(4,id-rel-mess)
                    EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
         IF AVAIL msg-impexp THEN msg-impexp.msg-text = 32-buf-copy-doc().
      END.
&IF DEFINED(FrmMsg) &THEN
      IF {&FrmMsg} EQ "942" THEN
         RUN put-buf-copy-doc-942.
      ELSE
&ENDIF
         RUN put-buf-copy-doc.

      IF NOT session:remote THEN DO:
         OUTPUT CLOSE.
      
         IF vMsg NE "" AND 
            NOT auto THEN DO:
            {setdest.i &stream="stream err" &append="append"}
            PUT STREAM err UNFORMATTED STRING(TODAY,"99/99/9999") + "."  +
                                       STRING(TIME,"HH:MM:SS")    + "~n" +
                                       vMsg SKIP.
            {preview.i &stream="stream err" &nodef="/*"}
            vMsg = "".
            DO:
               mblodd_char_Tmp01 = pick-value.
               RUN Fill-AlertSysMes IN h_tmess("","",4,"~n" + CHR(32) + "Продолжить экспорт выписки ?").
               choice = (pick-value = "YES").
               pick-value = mblodd_char_Tmp01.
            END.

            IF choice NE YES THEN UNDO MAIN, LEAVE MAIN.
         END.

         IF NOT auto AND FGetSetting("NoPrevDocExp",?,"Нет") EQ "Нет" THEN DO:
            {setdest.i &filename=in-file-name &append=append}
            {preview.i &filename=in-file-name}
         END.
         {cmd-exec.i &file-name=value(in-file-name) &cmd="'postcmd'"}
         IF OPSYS EQ "UNIX" THEN
            unix silent mv VALUE(in-file-name) VALUE(mail-user.file-exp).
         ELSE
            dos silent move VALUE(in-file-name) VALUE(mail-user.file-exp).

      END.
   END.
   {rel-date.i &in-op-date = vOpDate}
END.

IF GetXattrValue("op-kind",string(op-kind.op-kind),"EmptyStmt") EQ "Нет" THEN.
ELSE DO:
   ASSIGN
      vIsDayClosing = NO.

   CHECK-DAY:
   FOR EACH xop-date WHERE xop-date.op-date GE in-op-date
                       AND xop-date.op-date LE in-op-date2 NO-LOCK:      

      {daylock.i
       &in-op-date = xop-date.op-date
       &lock-type  = SHARE
       &cats       = mCats
       &return-mes = vMsg
       }

       {rel-date.i &in-op-date = xop-date.op-date}
       IF {assigned vMsg} THEN DO:
          MESSAGE REPLACE(vMsg,"~n"," ").
          ASSIGN
              vIsDayClosing = YES.
          LEAVE CHECK-DAY. 
       END.
   END.
   IF NOT vIsDayClosing THEN DO:
      {e-950.sp {&*}}
   END.
END.

/*кол-во документов в выписке*/
PROCEDURE GetNumStmt:
      DEF PARAM BUFFER signs FOR signs.

      FIND FIRST signs WHERE
         signs.file EQ "acct"                      AND
         signs.surr EQ acct.acct + "," + acct.curr AND
         signs.code EQ "NumStmt" EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
      IF NOT AVAIL signs AND
         locked signs THEN
      DO WHILE NOT AVAIL signs AND
         locked signs:
         MESSAGE
            "Дополнительный реквизит NumStmt используется другим пользователем!".
         PAUSE 5.
         RELEASE signs.
         FIND FIRST signs WHERE
            signs.file EQ "acct" AND
            signs.surr EQ acct.acct + "," + acct.curr AND
            signs.code EQ "NumStmt" EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
      END.
      ELSE IF NOT AVAIL signs THEN
         UpdateSigns("acct", acct.acct + "," + acct.curr, "NumStmt",
                     string(num-stmt + 1), YES).
      ELSE IF AVAIL signs THEN signs.code-val = string(num-stmt + 1).
END PROCEDURE.
/* $LINTUSER='MIKE' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='07/11/2014 09:45:13.825+04:00' */
/* $LINTFILE='e-ac950.i' */
/*prosignhwDzMbNcQMTQz3q0NRtuUQ*/
/* --- e-ac950.i was humbly modified by (c)blodd converter v.1.11 on 5/25/2017 6:45am --- */
