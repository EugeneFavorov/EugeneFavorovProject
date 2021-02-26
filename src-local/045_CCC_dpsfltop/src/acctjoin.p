/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2015 ЗАО "Банковские информационные системы"
     Filename: acctjoin.p
      Comment: вызов процедур из join-лицевых счетов
   Parameters: нет
         Uses:
      Used by:
      Created:
     Modified: 01.11.00 serge кау динамически
     Modified: 13.02.01 SAP -  Добавлено поле inp-param в ТТ acctjoin  для передачи параметров в процедуру (поддерживается до 5 параметров).
                               Добавлен join "Комиссии и проценты"
     Modified 18.03.2005 13:55 grab - 0041465 - переделано, чтобы быть методом метасхемы JOIN
     Modified: 19.07.2005 11:49 Om Ошибка.
                        Неиндексный поиск по таблице cust-role.
     Modified: 22.09.2005 kraw (0046159) "Связанный клиент" для внутренних счетов.
     Modified: 06.03.2006 ZIAL (0025900) "Показывать договоры, связанные с лицевым счетом"
     Modified: 12.12.2006 MUTA 0068805. В контекстное меню счетов добавлен пункт "Захолдированные суммы"
     Modified: 22.06.2007 16:54 KSV      (0078824) Удалена разводка
                                         SESSION-REMOTE
     Modified: 25.07.2007 14:13 Om       <comment>
     Modified: 30.10.2007 12:44 MUTA    0082120 Реализация количественного учета
                                         ценных бумаг в Базовом модуле

*/

{joinpar.i}
{globals.i}

DEF VAR mShowLine AS LOGICAL NO-UNDO.
DEF VAR mSurr     AS CHAR    NO-UNDO.

DEFINE BUFFER op-int FOR op-int. /* Локализация буфера. */

FIND acct WHERE ROWID(acct) = TO-ROWID(iROWID) NO-LOCK NO-ERROR.

RUN CreateJoin( "Проводки", "op-en(a)", YES ).

IF acct.acct-cat NE "d" THEN
DO:
   RUN CreateJoin( "Движение (валюта) - старое", "acur(a)", CAN-FIND( FIRST
         acct-cur OF acct ) ).
   RUN CreateJoin( "Движение/Прогноз (валюта)", "acur", acct.currency > "" ).
   RUN CreateJoin( "Движение (нац.валюта) - старое", "apos(a)", CAN-FIND( FIRST
         acct-pos OF acct ) ).
   RUN CreateJoin( "Движение/Прогноз (нац.валюта)", "apos", YES ).
END.

RUN CreateJoin( "Позиция по счету", "pos-acct", YES ).
RUN CreateJoinLd("Блокировки счета",
                 "browseld",
                 "BlockAcct",
                 "file-name~001surrogate",
                 "acct~001" + acct.acct + ";" + acct.currency,
                 "file-name~001surrogate",
                 level + 1,
                 YES ).

IF AvailXattr("acct",acct.acct + "," + acct.currency,"sec-code")
   OR acct.acct-cat EQ "d" THEN
   RUN CreateJoin( "Движение/Прогноз (количество)", "aqty", CAN-FIND(FIRST acct-qty OF acct)).

RUN CreateJoin( "Доверенные лица", "dpty(a)", CAN-FIND( FIRST deputy OF acct ) ).

IF CAN-FIND(FIRST loan WHERE loan.cust-cat EQ acct.cust-cat  
                         AND loan.cust-id  EQ acct.cust-id   
                         AND loan.contract EQ "proxy"        
                         AND loan.class-code EQ "proxy-base")
THEN DO:                                                     

   RUN CreateJoin("Доверенности РКО",
                     "proxy_acct`" + acct.cust-cat + "," + STRING(acct.cust-id) + ",acct," + acct.acct + "," +  STRING(level + 1),
                     YES).

END.

RUN CreateJoin( "Комиссии и проценты", "rate(ac)" + "`" + acct.acct + "," +
   acct.currency + "," + acct.contract + "," + STRING( Level + 1 ),
CAN-FIND( FIRST comm-rate WHERE
comm-rate.acct = acct.acct AND comm-rate.currency EQ acct.currency ) ).


DEFINE VARIABLE kau-proc LIKE acct.kau-id INITIAL ? NO-UNDO.
IF acct.kau-id EQ ? OR acct.kau-id EQ "" THEN
DO:
FIND FIRST bal-acct OF acct NO-LOCK NO-ERROR.
IF bal-acct.kau-id NE ? AND bal-acct.kau-id NE "" THEN
kau-proc = bal-acct.kau-id.
END.
ELSE
kau-proc = acct.kau-id.
RUN CreateJoin( "Аналитика", "kauacctv", kau-proc NE ? ).

RUN CreateJoin( "Дополнительные реквизиты", "acctsign", YES ).
RUN CreateJoinLd(
   "Дополнительные связи",
   "browseld",
   "links",
   "class-code"      + CHR(1) +  "surrogate-id"                   + CHR(1) +  "ActionLock",
   acct.class-code   + CHR(1) +  acct.acct + "~002" + acct.currency  + CHR(1) +  "F2",
   "class-code",
   level + 1,
   YES
).
RUN CreateJoinLd(
   "Электронные документы",
   "browseld",
   "eDocument",
   "class-code" + CHR(1) + "contract" + CHR(1) + "file-name" + CHR(1)  + "surrogate",
   "eDocument"  + CHR(1) + "ЭД"        + CHR(1) + "acct"       + CHR(1) +  acct.acct + ";" + acct.currency,
   "class-code" + CHR(1) + "contract" + CHR(1) + "file-name" + CHR(1) + "surrogate",
   level + 1,
   YES
).
/*IF IsUserAdm (USERID ("bisquit")) THEN*/
                        /* В данном случае браузер предназначен для отображения
                        ** групп, назначенных счету. F1 отключена, чтобы не
                        ** было возможности отредактировать описание группы по F1->F9.
                        ** F9 отключена по той же причине. */
   RUN CreateJoinLD ("Группы доступа",
                     "browseld",
                     "acct-group",
                     "acct-surrogate",
                     acct.acct + "~002" + acct.currency,
/*                     "acct-surrogate"                   + CHR (1) + "ActionLock",*/
/*                     acct.acct + "~002" + acct.currency + CHR (1) + "F1~002F9",  */
                     "acct-surrogate",
                     level + 1,
                     YES).

RUN CreateJoin( "Журнал изменений", "hi(acct",
CAN-FIND( FIRST history WHERE
history.file-name EQ "acct" AND
history.field-ref = acct.acct + "," +
acct.currency ) ).

FIND FIRST loan-acct WHERE loan-acct.acct      EQ acct.acct
                       AND loan-acct.currency  EQ acct.currency
                       AND (loan-acct.contract EQ 'card-pers' 
                        OR loan-acct.contract  EQ 'card-corp') NO-LOCK NO-ERROR.
IF AVAILABLE loan-acct THEN
   RUN CreateJoin IN THIS-PROCEDURE( "Журнал привязки счетов",
      "hi(file`?,?," + loan-acct.contract      + ';' +
                       loan-acct.cont-code     + ';' +
                       loan-acct.acct-type     + ';' +
                       STRING(loan-acct.since) + "," +
                       "loan-acct"             + "," + 
                       STRING(level + 1),
      CAN-FIND( FIRST history WHERE 
         history.file-name EQ "loan-acct" AND
         history.field-ref EQ loan-acct.contract  + "," +
                              loan-acct.cont-code + "," +
                              loan-acct.acct-type + "," +
                              STRING(loan-acct.since))).

RUN CreateJoin( "Правила обмена", "acctml-j",YES).

RUN CreateJoin( "Сообщения экспорта-импорта", "acctpack",
CAN-FIND( FIRST PackObject
WHERE PackObject.file-name EQ "acct" AND
PackObject.Surrogate EQ acct.acct + "," +
acct.currency ) ).

RUN CreateJoin( "Связанные субъекты",
                "a_cust",
                CAN-FIND (FIRST cust-role WHERE
                                 cust-role.FILE-NAME EQ "acct"
                          AND    cust-role.surrogate EQ acct.acct + ',' + acct.currency)).

IF acct.cust-cat EQ "В" THEN
   RUN CreateJoin( "Связанный клиент", "link-cli", YES ).


RUN CreateJoin( 
"Договоры", 
"acct2loan" + 
   "`" + acct.acct + 
   "," + acct.currency + 
   "," + STRING( Level + 1),CAN-FIND(FIRST loan-acct WHERE loan-acct.acct EQ acct.acct)).

RUN CreateJoinLd("Счета процессинга",                
                 "browseld", 
                 "acctp",
                 "link-object-id-sks",
                 acct.acct + ";" + acct.currency,
                 "",
                 level + 1,
                 fGetLinkCodeAny ("acctp","СчетПроц",
                                  acct.acct + "," + acct.currency,
                                  ?,
                                  gend-date)
                ).

FOR FIRST xlink WHERE xlink.class-code EQ "acctp"
                  AND xlink.link-code  EQ "СчетПроц"
   NO-LOCK,
    EACH  links WHERE (    links.link-id   EQ xlink.link-id                               
                       AND links.beg-date  LE gend-date
                       AND (   links.end-date GE gend-date         
                            OR links.end-date EQ ?)
                       AND links.target-id BEGINS acct.acct)        
                   OR (    links.link-id   EQ xlink.link-id                            
                       AND links.beg-date  LE gend-date        
                       AND (   links.end-date GE gend-date         
                            OR links.end-date EQ ?)        
                       AND links.source-id BEGINS acct.acct) 
   NO-LOCK:

   mSurr = IF xlink.link-direction BEGINS "s" THEN links.source-id
                                              ELSE links.target-id.

   FIND FIRST op-int WHERE op-int.file-name  EQ "acct"
                       AND op-int.surrogate  EQ mSurr
                       AND op-int.class-code EQ "СчетОстХлд" NO-LOCK NO-ERROR.

   IF NOT AVAIL op-int THEN NEXT.

   mShowLine = YES.
   LEAVE.   
END.

IF AcctLookBuffer(BUFFER acct:HANDLE) THEN
   RUN CreateJoinLd("История состояния счета в ПЦ",                
                    "browseld", 
                    "СчетОстХлд",
                    "mSCSAcct",
                    Acct.acct + ";" + acct.currency,
                    "mSCSAcct",
                    Level + 1,
                    mShowLine).

RUN CreateJoin ("Новые счета по 302-П", 
               "actjoi302`" + acct.acct + ";" + acct.currency + "," + STRING (Level + 1), 
                GetXAttrValueEx("acct", acct.acct + "," + acct.currency, "НовСч302","") NE ""
                ).

RUN CreateJoin ("События", 
                "evntjoi`" + "acct~001" + STRING(acct.acct) + "~002" + STRING(acct.currency) + "," + 
                "СЧЕТ " + acct.acct  + "," +
                STRING (Level + 1), YES).

{ procjoin.i
&prefix = acct
&frametitle = "'[ СЧЕТ ' + acct.number + (if acct.currency = '' then '' else ('/' + acct.currency)) + ' ]'"
&params = "(acct.acct, acct.currency, Level + 1)"
}
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='16/06/2015 09:29:52.818+04:00' */
/* $LINTUSER='soav' */
/* $LINTMODE='1' */
/* $LINTFILE='acctjoin.p' */
/*prosignQPkrr/eKwJD4wY3tIH3dPQ*/