/*
                Банковская интегрированная система БИСквит
    Copyright:  (C) 1992-1996 ТОО "Банковские информационные системы"
     Filename:  GCDCB2_1.P
      Comment:  Процедура расчета курсовых разниц по счетам, переоцениваемым по
                учетному курсу ЦБ. Сделана на основании gcdcb1_3.p, с добавлением
                следующих доработок:
                - произведен возврат к логике подстановки л/с учета курсовых разниц по внебалансовому
                учету, которая использовалась в версии процедуры gcdcb1_1.p.(заявка РБ)

         Uses:
      Used by:  -
      Created:  27/05/1998 eagle
     Modified:  01/07/1998 eagle проверяем закрыт ли предыдущий день; если день закрыт и
                учитывается входящий остаток - лицевой счет не лочится при получении остатка;
                лочим только предыдущий день, чтобы его никто не открыл во время операции;
     Modified:  10/01/1999 eagle совмещение функциональностей текущей и предыдущих версий;
     Modified:  17/02/2000 eagle добавлен анализ дополнительного реквизита
                транзакции - блокировать или не блокировать опердень
     Modified:  29/03/2000 eagle - исправлена ошибка установки режима работы(ПТБ)
     Modified:  13/10/2000 eagle - ускорение работы; при работе процедуры в ОД
                первый раз пропускаются закрытые счета; использование стандартных
                функций;
     Modified:  30/07/2002 NIK GetSigns --> GetXAttrValue
     Modified:  31/07/2003 EAGLE форматирование текста.
     Modified:  08/08/2003 EAGLE  18821
     Modified:  21.11.2005 TSL    Многофилиальность
     Modified:  19/07/2007 kraw (0073795) унификация с g-saldo2.p
     Modified:  18/12/2008 kraw (0095573) Переоценка валюты в соответствии с 302-П
*/

DISABLE TRIGGERS FOR LOAD OF op-entry.

DEFINE INPUT PARAMETER in-op-date LIKE op-date.op-date.
DEFINE INPUT PARAMETER oprid      AS   RECID.

{intrface.get tmess}
{g-defs.i}
{intrface.get xclass}
{details.def}

DEFINE VARIABLE mul  AS INT64 NO-UNDO.
DEFINE VARIABLE dnum AS INT64 INITIAL 0 NO-UNDO.
DEFINE VARIABLE i    AS INT64 INITIAL 0 NO-UNDO.
DEFINE VARIABLE ii   AS INT64 INITIAL 0 NO-UNDO.
DEFINE VARIABLE i1   AS INT64 INITIAL 0 NO-UNDO.
DEFINE VARIABLE i2   AS INT64 INITIAL 0 NO-UNDO.
DEFINE VARIABLE dind AS INT64 INITIAL 0 NO-UNDO.

DEFINE VARIABLE tdb AS INT64 EXTENT 2 INITIAL ? NO-UNDO.
DEFINE VARIABLE tcr AS INT64 EXTENT 2 INITIAL ? NO-UNDO.

DEFINE VARIABLE strbacct   AS CHARACTER NO-UNDO.
DEFINE VARIABLE curstr     AS CHARACTER NO-UNDO.
DEFINE VARIABLE codok      AS CHARACTER NO-UNDO.
DEFINE VARIABLE strmess    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lstacct    AS CHARACTER INITIAL "*" NO-UNDO.
DEFINE VARIABLE lstcurr    AS CHARACTER INITIAL "*" NO-UNDO.
DEFINE VARIABLE rgsbstacct AS CHARACTER INITIAL "0" NO-UNDO.

DEFINE VARIABLE diff  LIKE op-entry.amt-rub NO-UNDO.
DEFINE VARIABLE mdiff LIKE op-entry.amt-rub EXTENT 2 NO-UNDO.
DEFINE VARIABLE v-db   LIKE op-entry.acct-db NO-UNDO.
DEFINE VARIABLE v-cr   LIKE op-entry.acct-cr NO-UNDO.
DEFINE VARIABLE vdbe  LIKE op-entry.acct-db NO-UNDO.
DEFINE VARIABLE vcre  LIKE op-entry.acct-cr NO-UNDO.
DEFINE VARIABLE vcur  LIKE acct.curr        NO-UNDO.
DEFINE VARIABLE vcd   LIKE acct.acct        NO-UNDO.

DEFINE VARIABLE chc          AS LOGICAL NO-UNDO.
DEFINE VARIABLE fl-lck-acct  AS LOGICAL NO-UNDO.
DEFINE VARIABLE clc-cls-acct AS LOGICAL NO-UNDO.

DEFINE VARIABLE mStart      AS LOGICAL INITIAL YES NO-UNDO.
DEFINE VARIABLE mMsg        AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNewSelMode AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mAmtDb      AS DECIMAL   NO-UNDO.
DEFINE VARIABLE mAmtCr      AS DECIMAL   NO-UNDO.
DEFINE VARIABLE mYesNo      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mDblRate    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE mOk         AS LOGICAL. /* UNDO */
DEFINE VARIABLE mDetails    AS CHARACTER   NO-UNDO.

DEFINE BUFFER prev-rate FOR instr-rate.
DEFINE BUFFER xop1 FOR op.
DEFINE BUFFER xop2 FOR op.
DEFINE BUFFER xacct-pos FOR acct-pos.
DEFINE BUFFER xop-templ FOR op-templ.
DEFINE BUFFER bacct FOR acct.
DEFINE BUFFER tacct FOR acct.
DEFINE BUFFER bop-date FOR op-date.
DEFINE BUFFER xbop-date FOR op-date.

{def-wf.i NEW}
DEFINE VARIABLE mErr  AS LOGICAL  NO-UNDO.

&GLOBAL-DEFINE RateType "Учетный"

FUNCTION chklock RETURNS LOGICAL PRIVATE (
   INPUT irecid AS RECID, 
   INPUT istr1  AS CHAR, 
   INPUT istr2  AS CHAR):
  
   DEFINE VARIABLE vString AS CHARACTER  NO-UNDO.

   vString = istr1 + " " + acct.acct + " " + istr2. 
   WhoLocks2(irecid,"acct",
            INPUT-OUTPUT vString).  

   pick-value = "no".
   RUN Fill-SysMes IN h_tmess ("", "", "4", vString + "~n~nПроверить еще раз <Да> или откатить транзакцию <Нет> ?").
   RETURN pick-value = "yes".
END FUNCTION.

PROCEDURE messexit PRIVATE:
   IF NOT auto THEN BELL.
   RUN Fill-SysMes IN h_tmess ("", "", "-1", "В операционном дне " + STRING(in-op-date) + " переоценка не проведена !").
END PROCEDURE.

IF    auto EQ NO
   OR auto EQ ?
THEN
   auto = SESSION:BATCH-MODE.

{checkstart.i &set-code="'КурсЗапуск'" }

{g-acctv1.i 
   &nodef-WorkOnAccts      = YES}

FIND FIRST op-kind WHERE RECID(op-kind) EQ oprid NO-LOCK NO-ERROR.
IF AVAIL(op-kind) THEN
DO:
   IF     op-kind.op-kind EQ "КурсГО"
      AND shFilial        NE "0000" THEN
   DO:
      RUN Fill-SysMes IN h_tmess ("","","-1","Транзакция КурсГО должна запускаться только в Головном офисе (0000).").
      RETURN.
   END.
   IF     op-kind.op-kind EQ "Курс"
      AND shFilial        NE "0300" THEN
   DO:
      RUN Fill-SysMes IN h_tmess ("","","-1","Транзакция Курс должна запускаться только в Тюменском филиале (0300).").
      RETURN.
   END.
   IF     op-kind.op-kind BEGINS "КурсОФ"
      AND shFilial        NE "0500" THEN
   DO:
      RUN Fill-SysMes IN h_tmess ("","","-1","Транзакция Курс должна запускаться только в Омском филиале (0500).").
      RETURN.
   END.
END.

mOk = NO.
_blk1:
DO ON ERROR UNDO, LEAVE:
   IF NOT CAN-FIND (FIRST instr-rate WHERE 
                          instr-rate.instr-cat EQ "currency"
                      AND instr-rate.rate-type EQ {&RateType}
                      AND instr-rate.since     EQ in-op-date) THEN DO:
      pick-value = STRING(GetEntries(2,FGetSetting("ОперДень","КурсУдал","Да,Нет"),",","Нет") = "Да").
      RUN Fill-SysMes IN h_tmess ("", "", "4", "В операционном дне " + STRING(in-op-date) + " не было смены курса ! Продолжим ?").
      IF pick-value <> "yes" THEN
      DO:
         RUN messexit.
         UNDO _blk1, LEAVE _blk1.
      END.
   END.
   cur-op-date = in-op-date.
   FIND FIRST op-kind WHERE RECID(op-kind) EQ oprid NO-LOCK NO-ERROR.
   codok = op-kind.op-kind.

   {findcat.i &cat-source = op-kind}

   ASSIGN
      mDblRate    = GetCodeMisc("Курсы",{&RateType},3) GT ""
      DebugParser = INT64(GetXattrValueEx('op-kind', op-kind.op-kind,'debugparser', '0'))
   .

   mOk = YES.
END. /* TRANS _blk1 */

IF mOk THEN DO:
   mOk = NO.
   _blk2:
   DO TRANSACTION ON ERROR UNDO, LEAVE:
      IF GetXAttrValue("op-kind", op-kind.op-kind, "lck-od") EQ "Нет" THEN .
      ELSE
      DO:
         /* проверка на лочение op-date, а вдруг?.. */
         FIND FIRST bop-date WHERE bop-date.op-date EQ in-op-date 
            NO-LOCK NO-ERROR.
      
      
         {gcdcb2_1.i
            &lock-type         = EXCLUSIVE
            &lock-op-date-only = YES
            &do-on-error       = "RUN Fill-SysMes IN h_tmess ('', '', '-1', mMsg). RUN messexit. UNDO _blk2, LEAVE _blk2."
         }

         RELEASE op-date.   
      END.

      mOk = YES.
   END. /* TRANS _blk2 */
END.

IF mOk THEN DO:
   mOk = NO.
   gen:
   DO TRANSACTION ON ERROR UNDO, LEAVE:
      FIND FIRST bop-date WHERE
                 bop-date.op-date EQ in-op-date NO-LOCK NO-ERROR.
      
      IF GetXAttrValue("op-kind", op-kind.op-kind, "lck-od") EQ "Нет" THEN
      DO:
         {gcdcb2_1.i
            &lock-type   = SHARE
            &do-on-error = "RUN Fill-SysMes IN h_tmess ('', '', '-1', mMsg). RUN messexit. UNDO gen, LEAVE gen."
         }
      END.
      ELSE
      DO:
         {gcdcb2_1.i
            &lock-type   = EXCLUSIVE
            &do-on-error = "RUN Fill-SysMes IN h_tmess ('', '', '-1', mMsg). RUN messexit. UNDO gen, LEAVE gen."
         }
      END.
      
      IF NOT auto THEN
      FORM
         op-templ.op-templ COLUMN-LABEL "НОМ.!ШАБ." FORMAT ">>9"
         code.name COLUMN-LABEL "КАТЕГОРИЯ УЧЕТА" VIEW-AS FILL-IN SIZE 25 BY 1 FORM
         "x(25)"
         rgsbstacct COLUMN-LABEL "РЕЖИМ!ПОДСТАНОВКИ!СЧЕТОВ"
         lstacct COLUMN-LABEL "МАСКА!СЧЕТОВ"
         strmess FORM "x(25)" COLUMN-LABEL "СОСТОЯНИЕ"
         WITH FRAME mes 6 DOWN OVERLAY CENTERED ROW 10 TITLE COLOR bright-white
         "[ Переоценка валютных счетов на " + STRING(in-op-date) + " ]".
      
      {op(ok.del
         &fl-del   = chc
         &date     = in-op-date
         &op-kind  = op-kind.op-kind
         &set-code = "'КурсУдал'"
         &mes1     = "В этом дне уже проводилась переоценка"
         &mes2     = "валютных счетов! Удалить ее?"
         &mes3     = "Удаление документов переоценки..."
         &rel-date = YES
         &cats     = mCats
      }
      
      IF     NOT auto
         AND NOT mPrevOpWasDeleted THEN   /* Удаление не запрашивалось и не производилось*/
         RUN CheckStart (op-kind.op-kind, OUTPUT mStart).
      IF NOT mStart THEN DO:
         mOk = YES.
         LEAVE gen.
      END.
      IF NOT auto THEN PAUSE 0.
      
      /* проверка настроек для транзакции */
      /* проверяем учитывать ли закрытые счета */
      clc-cls-acct = YES.
      IF chc /* !!! должна быть установлена в op(ok.del */ EQ NO THEN
      DO:
         clc-cls-acct = IF GetXAttrValue("op-kind", op-kind.op-kind, "acct-close")
            EQ "Нет" THEN NO ELSE YES.
      
      END.
      /* выбор расчета остатка */
      dind = IF GetXAttrValue("op-kind", op-kind.op-kind, "остаток") EQ "Входящий"
         THEN 1 ELSE 0.
      /* далее следует проверка - лочить или нет лицевой счет */
      fl-lck-acct = IF GetXAttrValue("op-kind", op-kind.op-kind, "lck-acct") EQ
         "Нет" THEN NO ELSE YES.
      
      IF NOT auto THEN
      DO:
         {justamin}
      END.
      i = 0.
      _op-templ:
      FOR EACH op-templ OF op-kind NO-LOCK:
         /* проверка настроек для шаблона */
         IF op-templ.curr NE "" THEN
         DO:
            RUN Fill-SysMes IN h_tmess ("", "", "-1", "Валюта шаблона переоценки должна быть '""""' !").
            RUN messexit.
            UNDO gen, LEAVE gen.
         END.
         IF op-templ.currency EQ "" AND type-curracct AND type-balance THEN
            mul = 2.
         ELSE
            mul = 1.
         
         ASSIGN
            lstacct    = GetXAttrValueEx("op-template",op-kind.op-kind + "," + STRING(op-templ.op-templ),"lstacct", "*")
            rgsbstacct = GetXAttrValueEx("op-template",op-kind.op-kind + "," + STRING(op-templ.op-templ),"rgsbstacct","0")
            lstcurr    = GetXAttrValueEx("op-template",op-kind.op-kind + "," + STRING(op-templ.op-templ),"lstcurr", "*")
         .

         FIND FIRST tacct WHERE CAN-DO(lstacct,tacct.acct)                             
                            AND tacct.acct-cat EQ op-template.acct-cat
                            AND tacct.close-date EQ ?
                            AND tacct.filial-id EQ ShFilial                            
         NO-LOCK NO-ERROR.
         IF NOT AVAIL(tacct) THEN NEXT.

         IF op-templ.curr NE ? THEN
         DO:
            FOR EACH wop WHERE wop.op-kind  = op-kind.op-kind:
               DELETE wop.
            END.
      
            CREATE wop.
            ASSIGN
               wop.currency = op-templ.currency
               wop.op-templ = op-templ.op-templ
               wop.op-kind  = op-kind.op-kind
               tcur         = op-templ.currency
            .
            mNewSelMode = GetXAttrValueEx("op-template",
                                          op-kind.op-kind + "," + STRING(op-templ.op-templ),
                                          "ВыбСчНКР",
                                          "1") EQ "2".

            {g-acctv1.i &nodef-GetAcct = YES
                        &vacct         = v
                        &OFBase        = YES
                        &OFsrch        = YES}

            IF NOT AVAILABLE dacct THEN
            DO:
               RUN Fill-SysMes IN h_tmess ("", "core08", "", "%s=" + DelFilFromAcct(v-db) + ", указанный в шаблоне проводок переоценки" + "%s=" + op-templ.curr).
               DELETE wop.
               NEXT _op-templ.
            END.
            v-db = dacct.acct.
      
            IF dacct.close-date NE ? THEN 
            DO:
               RUN Fill-SysMes IN h_tmess ("", "core10", "" , "%s=" + dacct.number + ", указанный в шаблоне проводок переоценки").
               DELETE wop.
               NEXT _op-templ.
            END.
      
            IF dacct.open-date > in-op-date THEN
            DO:
               RUN Fill-SysMes IN h_tmess ("", "core11", "" , "%t=" + STRING(in-op-date) +
                                                              "%t=" + STRING(dacct.open-date) +
                                                              "%s=" + dacct.number + ", указанный в шаблоне проводок переоценки").
               DELETE wop.
               NEXT _op-templ.
            END.
      
            IF dacct.acct-cat NE op-templ.acct-cat THEN
            DO:
               RUN Fill-SysMes IN h_tmess ("", "acct39", "", "%s=" + dacct.number + ", указанного в шаблоне проводок переоценки").
               UNDO, LEAVE.
            END.
            IF NOT AVAILABLE cacct THEN
            DO:
               RUN Fill-SysMes IN h_tmess ("", "core08", "", "%s=" + DelFilFromAcct(v-cr) + ", указанный в шаблоне проводок переоценки" + "%s=" + op-templ.curr).
               DELETE wop.
               NEXT _op-templ.
            END.
            v-cr = cacct.acct.
      
            IF cacct.close-date NE ? THEN 
            DO:
               RUN Fill-SysMes IN h_tmess ("", "core10", "" , "%s=" + cacct.number + ", указанный в шаблоне проводок переоценки").
               DELETE wop.
               NEXT _op-templ.
            END.
      
            IF cacct.open-date > in-op-date THEN
            DO:
               RUN Fill-SysMes IN h_tmess ("", "core11", "" , "%t=" + STRING(in-op-date) +
                                                              "%t=" + STRING(cacct.open-date) +
                                                              "%s=" + cacct.number + ", указанный в шаблоне проводок переоценки").
               DELETE wop.
               NEXT _op-templ.
            END.
      
            IF cacct.acct-cat NE op-templ.acct-cat  THEN
            DO:
               RUN Fill-SysMes IN h_tmess ("", "acct39", "", "%s=" + cacct.number + ", указанного в шаблоне проводок переоценки").
               UNDO, LEAVE.
            END.
         END.
         
         ASSIGN
            vcur  = ""
            mdiff = 0
         .
         /* Разбор настройки для шаблона */
         CASE rgsbstacct:
            WHEN "0" THEN
            DO:
               /* вроде ничего не проверяем */
            END.
            WHEN "1" THEN
            DO:
               /* контрсчета */
               IF dacct.contr-acct NE cacct.acct OR cacct.contr-acct NE dacct.acct THEN
               DO:
                  RUN Fill-SysMes IN h_tmess ("", "", "-1", "Шаблон № " + STRING(op-templ.op-templ) + ". Режим подстановки счетов - 1, но счета не парные !").
                  UNDO gen, LEAVE gen.
               END.
               RUN acct-pos IN h_base (v-db,vcur,in-op-date - 1,in-op-date - 1,?).
               diff = sh-bal.
               RUN acct-pos IN h_base (v-cr,vcur,in-op-date - 1,in-op-date - 1,?).
               vcd = IF ABSOLUTE(diff) > ABSOLUTE(sh-bal) THEN v-db ELSE v-cr.
            END.
            WHEN "2" THEN
            DO:
               IF dacct.side EQ cacct.side THEN
               DO:
                  RUN Fill-SysMes IN h_tmess ("", "", "-1", "Шаблон № " + STRING(op-templ.op-templ) + ". Режим подстановки счетов - 2, но режимы работы счетов одинаковые !").
                  UNDO gen, LEAVE gen.
               END.
               ASSIGN
                  v-db = IF dacct.side EQ "А" THEN dacct.acct ELSE cacct.acct
                  v-cr = IF cacct.side EQ "П" THEN cacct.acct ELSE dacct.acct
               .
            END.
            OTHERWISE
               DO:
                  RUN Fill-SysMes IN h_tmess ("", "", "-1", "Шаблон № " + STRING(op-templ.op-templ) + ". Режим подстановки счетов - " + rgsbstacct + " не совпадает с обрабатываемыми значениями <0,1,2> !").
                  UNDO gen, LEAVE gen.
               END.
         END.
      
         IF mNewSelMode THEN
         DO:
            RUN acct-pos IN h_base (v-db,
                                    op-templ.currency,
                                    in-op-date - dind,
                                    in-op-date - dind,
                                    op-templ.op-status).
      
            mAmtDb = IF op-templ.currency EQ "" THEN sh-bal
                                                ELSE sh-val.
      
            RUN acct-pos IN h_base (v-cr,
                                    op-templ.currency,
                                    in-op-date - dind,
                                    in-op-date - dind,
                                    op-templ.op-status).
      
            mAmtCr = IF op-templ.currency EQ "" THEN sh-bal
                                                ELSE sh-val.
      
            IF mAmtDb NE 0.0 AND mAmtCr NE 0.0 THEN
            DO:
               pick-value = "NO".
               RUN Fill-SysMes IN h_tmess ("", "", "0", 
                                           "Внимание! На обоих парных счетах " + DelFilFromAcct(v-db) + " и " + DelFilFromAcct(v-cr) + " имеется остаток" + "~n" +
                                           "статус " + op-templ.op-status + "," + "~n" +
                                           STRING (mAmtDb) + " и " + STRING (mAmtCr) + " соответственно" + "~n" +
                                           "Корректный выбор счетов невозможен. Продолжить процедуру переоценки?").
      
               IF pick-value NE "YES" THEN
                  NEXT _op-templ.      
            END.
      
            IF ABS(mAmtDb) EQ 0.00 AND ABS(mAmtCr) EQ 0.00 THEN
               ASSIGN
                  vdbe = v-db
                  vcre = v-cr.
            ELSE DO:
               IF ABSOLUTE(mAmtDb) LE ABSOLUTE(mAmtCr) THEN
                  ASSIGN
                     vdbe = v-cr
                     vcre = v-cr
                  .
               ELSE
                  ASSIGN
                     vdbe = v-db
                     vcre = v-db
                  .
            END.
         END.

         {cr-op(cd.i}

         mDetails = op-templ.details.
         wop.op-recid = RECID(xop1).
         RUN ProcessDetails (RECID(wop), input-output mDetails).
         ASSIGN
            xop1.details = mDetails
            xop2.details = mDetails.

         /* Если тип курса, участвующий в переоценке подвержен двойному вводу
            все курсы должны быть подтверждены  */
         IF mDblRate THEN
            FOR EACH currency FIELDS (currency) WHERE 
                     currency.currency GT ""
                 AND CAN-DO (lstcurr, currency.currency)
             NO-LOCK, 
                LAST instr-rate WHERE
                     instr-rate.instr-cat  EQ "currency"
                 AND instr-rate.instr-code EQ currency.currency
                 AND instr-rate.rate-type  EQ {&RateType}
                 AND instr-rate.since      LE in-op-date 
             NO-LOCK:
               IF GetXattrValue("instr-rate",
                                instr-rate.instr-cat  + ',' + 
                                instr-rate.rate-type  + ',' + 
                                instr-rate.instr-code + ',' + 
                                STRING(instr-rate.since),
                                "Статус") NE 'Подтвержден' 
               THEN 
               DO:
                  RUN Fill-SysMes IN h_tmess ("", "", "-1", "Не подтвержден курс~n"
                     + "Тип: " + {&RateType} + "~n"
                     + "Валюта: " + currency.currency + ")~n"
                     + "Дата: " + STRING(instr-rate.since)).
                  RUN messexit.
                  UNDO gen, LEAVE gen.
               END.
            END.
            
         /* Информационная строка */
         IF NOT auto THEN
         DISPLAY
            op-templ.op-templ
            getCodeName("acct-cat",op-templ.acct-cat) @ code.name
            rgsbstacct
            lstacct
         WITH FRAME mes.
      
         FOR EACH currency FIELDS (currency) WHERE 
                  currency.currency GT ""
              AND CAN-DO (lstcurr, currency.currency)
             NO-LOCK, 
             LAST instr-rate WHERE 
                  instr-rate.instr-cat  EQ "currency"
              AND instr-rate.instr-code EQ currency.currency
              AND instr-rate.rate-type  EQ {&RateType}
              AND instr-rate.since      LE in-op-date
            NO-LOCK,
            EACH acct FIELDS(acct currency close-date side) WHERE 
                 acct.filial-id EQ shFilial
             AND acct.currency  EQ instr-rate.instr-code
             AND acct.acct-cat  EQ op-templ.acct-cat
             AND acct.rate-type EQ {&RateType}
             AND CAN-DO(lstacct,acct.acct)
             AND (clc-cls-acct 
               OR acct.close-date EQ ?
               OR acct.close-date GE in-op-date)
            NO-LOCK:
      
            {on-esc LEAVE gen}
            IF RETRY THEN
               UNDO gen, LEAVE gen.
                
            /* Проверка захвата счета */
            IF fl-lck-acct THEN
            DO:
               FIND FIRST bacct WHERE
                    ROWID(bacct) EQ ROWID(acct) 
                  EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
               IF NOT AVAILABLE bacct THEN
               DO:
                  IF chklock(RECID(acct), "Счет",
                     "редактируется, или по нему проводится операция пользователем")
                     NE TRUE
                  THEN DO:
                     RUN messexit.
                     UNDO gen, LEAVE gen.
                  END.
               END.
            END.
            /* Отстатки по счету */
            RUN acct-pos IN h_base (acct.acct,
                                    acct.curr,
                                    in-op-date - dind,
                                    in-op-date - dind,
                                    ?).
                                    
            diff = ROUND (sh-val * instr-rate.rate-instr / instr-rate.per,2) - sh-bal.
            
            IF diff NE 0 THEN
            DO:
               IF diff > 0 THEN
               DO:
                  i1 = i1 + 1.
                  mdiff[1] = mdiff[1] + diff.
               END.
               ELSE
               DO:
                  i2 = i2 + 1.
                  mdiff[2] = mdiff[2] + diff.
               END.
      
               /* выбор счетов в зависимости от настройки */
               IF NOT mNewSelMode THEN
               DO:
                  CASE rgsbstacct:
                     WHEN "0" THEN
                     DO:
                        ASSIGN
                           vdbe = v-db
                           vcre = v-cr.
                     END.
                     WHEN "1" THEN
                     DO:
                        ASSIGN
                           vdbe = vcd
                           vcre = vcd.
                     END.
                     WHEN "2" THEN
                     DO:
                        ASSIGN
                           vdbe = IF acct.side EQ "А" THEN v-cr ELSE v-db
                           vcre = vdbe.
                     END.
                  END.
               END.
               /* Создаем проводку */
               CREATE op-entry.
               ASSIGN
                  op-entry.op-date      = cur-op-date
                  op-entry.acct-cat     = op-templ.acct-cat
                  op-entry.op-status    = xop1.op-status
                  op-entry.user-id      = xop1.user-id
                  op-entry.filial-id    = xop1.filial-id
                  op-entry.op           = IF diff > 0 THEN xop1.op 
                                                      ELSE xop2.op
                op-entry.op-transaction = IF diff > 0 THEN xop1.op-transaction 
                                                      ELSE xop2.op-transaction
                  op-entry.op-entry     = IF diff > 0 THEN i1 
                                                      ELSE i2
                  op-entry.acct-db      = IF diff > 0 THEN acct.acct 
                                                      ELSE IF mul EQ 1 
                                                           THEN vdbe 
                                                           ELSE ?
                  op-entry.acct-cr      = IF diff < 0 THEN acct.acct 
                                                      ELSE IF mul EQ 1 
                                                           THEN vcre 
                                                           ELSE ?
                  op-entry.currency     = acct.currency
                  op-entry.value-date   = in-op-date
                  op-entry.amt-cur      = 0
                  op-entry.amt-rub      = ABS (diff)
                  op-entry.type         = op-templ.type
                  op-entry.op-cod       = op-templ.op-cod
               .
            END.
            /* Отпускаем захват счета */
            IF fl-lck-acct THEN RELEASE bacct.
         END. /* валюты */
                     
         IF mul = 2 
         THEN
            DO i = 1 TO 2:
               /* если конверс. баланс  и номер счета одинаков в р. и в. */
               IF mdiff[i] NE 0 THEN
               DO:
                  /* надо проанализировать rgsbstacct too */
                  CREATE op-entry.
                  ASSIGN
                     op-entry.op-date      = cur-op-date
                     op-entry.acct-cat     = op.acct-cat
                     op-entry.op-status    = op.op-status
                     op-entry.user-id      = op.user-id
                     op-entry.filial-id    = op.filial-id
                     op-entry.op-transaction = op.op-transaction
                     op-entry.op           = IF mdiff[i] > 0 THEN xop1.op
                                                             ELSE xop2.op
                     op-entry.op-entry     = (IF mdiff[i] > 0 THEN i1 ELSE i2) + 1
                     op-entry.acct-db      = IF mdiff[i] > 0 THEN ? ELSE vdbe
                     op-entry.acct-cr      = IF mdiff[i] < 0 THEN ? ELSE vcre
                     op-entry.currency     = op-templ.currency
                     op-entry.value-date   = in-op-date
                     op-entry.amt-cur      = 0
                     op-entry.amt-rub      = IF mdiff[i] < 0 THEN - mdiff[i]
                                                             ELSE mdiff[i]
                     op-entry.type         = op-templ.type
                     op-entry.op-cod       = op-templ.op-cod
                  .
               END.
            END.
            
         IF CAN-FIND (FIRST xop WHERE
                            xop.filial-id EQ shFilial
                        AND xop.op-kind   EQ codok
                        AND xop.op-date   EQ in-op-date
                        AND xop.acct-cat  EQ op-templ.acct-cat
                       AND CAN-FIND (FIRST op-entry OF xop)
                      NO-LOCK)
         THEN strmess = "Курс. разницы подсчитаны.".
         ELSE strmess = "Курс. разницы нет.".

         IF NOT auto THEN
         DO:
            DISPLAY strmess WITH FRAME mes.
            DOWN WITH FRAME mes.
            PAUSE 0.
         END.
      END. /* for each op-templ */
      
      IF     GetProcSettingByCode("СС_ВыводНаЭкран") = "Да"
         AND NOT auto 
      THEN DO:
         PUT SCREEN COLUMNS 1 ROW SCREEN-LINES + MESSAGE-LINES + 1
            COLOR bright-blink-normal "Нажмите любую клавишу для продолжения.".
         READKEY.
         PUT SCREEN COLUMNS 1 ROW SCREEN-LINES + MESSAGE-LINES + 1
            "                                                        ".
      END.
      FOR EACH xop WHERE
               xop.filial-id EQ shFilial
           AND xop.op-kind   EQ codok
           AND xop.op-date   EQ in-op-date 
           AND NOT CAN-FIND(FIRST op-entry OF xop) 
         NO-LOCK:
         FIND FIRST op WHERE RECID(op) EQ RECID(xop) EXCLUSIVE-LOCK.
         DELETE op. /* нельзя undo - восстановится старая удаленная в начале операция */
      END.

      mOk = YES.
   END.
END. /* TRANS gen */

DO TRANSACTION ON ERROR UNDO, LEAVE:
   /* разблокировать день */
   {rel-date.i &in-op-date = in-op-date}
END.

IF NOT auto THEN
   HIDE FRAME mes.
{intrface.del}          /* Выгрузка инструментария. */ 

IF mOk
THEN RETURN.
ELSE RETURN ERROR RETURN-VALUE.
