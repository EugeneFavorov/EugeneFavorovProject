/* 
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2001 ТОО "Банковские информационные системы"
     Filename: chst(op.i
      Comment: инклюд для смены статуса
   Parameters: BUFFER op, vstatus
         Uses:
      Used BY: many
      Created: 28.03.2001 serge
     Modified: 09.06.2003 NIK Контроль документа в целом
     MODIFIED: 20/05/2003 KOSTIK 0005638  Ввод обязательных реквизитов после смены статуса
     Modified: 07.07.2004 abko   0029145  При смене статуса документа без проводок 
                                          не меняется статус у субаналитики
     Modified: 04/12/2004 kostik 0021328  При смене статуса надо обязательно 
                                          проверять в документе нарушение баланса
     Modified: 17.12.2004 abko   0038777 отключение Контроля 117И при аннулировании
     Modified: 15.03.2007 kraw (0062505) Документ без проводок, но с субаналитикой.
*/

{intrface.get autho}
{intrface.get email}

IF AVAIL op THEN DO:
&IF DEFINED(Def_Var) = 0 
&THEN 
   {intrface.get op}
   {intrface.get xclass}

   DEF VAR vUIN                   AS CHARACTER  NO-UNDO.
   DEF VAR vCodeMes               AS INT64      NO-UNDO.
   DEF VAR vMes                   AS CHARACTER  NO-UNDO.
   DEF VAR vCount1                AS INT64      NO-UNDO.
   DEF VAR vCount2                AS INT64      NO-UNDO.
   DEF VAR cdate                  AS DATE       NO-UNDO.
   DEF VAR ctime                  AS INT64    NO-UNDO.
   DEF VAR v-xattr-cr-CrClassCode AS CHARACTER  NO-UNDO.
   DEF VAR v-xattr-cr-I           AS INT64    NO-UNDO.
   DEF VAR cddif                  AS LOGICAL    NO-UNDO.
   DEF VAR vSelectQuest           AS CHARACTER  INITIAL ? NO-UNDO. /**/
   DEF VAR vDelProc               AS CHARACTER  NO-UNDO.
   DEF VAR vDelProcParam          AS CHARACTER  NO-UNDO.
   DEF VAR vPrevStat              AS CHARACTER  NO-UNDO.
   DEF VAR vFlagAnn               AS LOGICAL    NO-UNDO. /* Документ возвращается из аннулированных*/
   DEF VAR vLinks                 AS CHARACTER  NO-UNDO.
   DEF VAR vLstClass              AS CHARACTER  NO-UNDO.
   DEF VAR vOk                    AS LOGICAL    NO-UNDO. 
   DEF VAR vStrTmp                AS CHARACTER  NO-UNDO.
   DEF VAR mErrMsgDb              AS CHAR       NO-UNDO.
   DEFINE VARIABLE vTypeAutor     AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vOpAuthoStatus    LIKE op.op-status NO-UNDO.
   DEFINE VARIABLE vNoAuthorization  AS LOGICAL NO-UNDO .
   DEFINE VARIABLE vNoMakeDoc        AS LOGICAL NO-UNDO .
   DEFINE VARIABLE vCorrChk          AS CHARACTER INIT "off-corracct"
                                                  NO-UNDO.
   DEFINE VARIABLE vEMail            AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vFileTxt          AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vSubjectText      AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE mEmail            AS CHARACTER  NO-UNDO.

   DEFINE BUFFER gc-op FOR op.

   vOpAuthoStatus = FGetSetting("АвторПЦ", "АвторПЦСтат", gop-status).
   IF FGetSetting("КонтрКореспСтат","","") = "Да" THEN vCorrChk = "".

   &IF DEFINED(visa) <> 0 &THEN
      DEFINE VARIABLE vProcContr   AS CHARACTER   NO-UNDO. /* Процедура контроля     */
      DEFINE VARIABLE vProcContrIn AS CHARACTER   NO-UNDO. /* Процедура контроля     */      
      DEFINE VARIABLE vNewStat   AS CHARACTER   NO-UNDO.
      DEFINE VARIABLE vVisaStat  AS CHARACTER   NO-UNDO. /* Статус для визирования */
      DEFINE VARIABLE oResult    AS LOGICAL     NO-UNDO.
      DEFINE VARIABLE vVisaList  AS CHARACTER   NO-UNDO.
      DEFINE VARIABLE vVisaAll   AS CHARACTER   NO-UNDO.
      DEFINE VARIABLE vVisaN     AS CHARACTER   NO-UNDO.
      DEFINE VARIABLE vStatQuest AS CHARACTER   NO-UNDO. 
      DEFINE VARIABLE vPos       AS INT64       NO-UNDO.
      DEFINE VARIABLE vHistStat  AS CHARACTER   NO-UNDO.
      DEFINE VARIABLE vVisa      AS CHARACTER   NO-UNDO.
      DEFINE VARIABLE vVisaRes   AS LOGICAL     NO-UNDO.
      DEFINE VARIABLE vJ         AS INT64     NO-UNDO.
      DEFINE BUFFER bCode  FOR code.
      DEFINE BUFFER grCode FOR code. 
     
      PROCEDURE RunProcControl:
         DEFINE INPUT  PARAMETER iProc    AS CHARACTER NO-UNDO.
         DEFINE INPUT  PARAMETER iParam   AS CHARACTER NO-UNDO.
         DEFINE OUTPUT PARAMETER oResult  AS LOGICAL NO-UNDO.

         IF SearchPfile(iProc) THEN
     
            RUN VALUE(iProc + ".p")(BUFFER op, iParam, OUTPUT oResult).
     
         ELSE oResult = NO.
      
      END PROCEDURE.
   &ENDIF
   
   &GLOBAL-DEFINE Def_var YES
&ENDIF

   IF NOT (vstatus BEGINS "А") THEN DO:
      FIND op-entry OF op NO-LOCK NO-ERROR.
      IF AVAIL(op-entry) THEN DO:
         RUN ValidateCust115fl(BUFFER op,
                              op-entry.acct-db,
                              op-entry.currency,
                              op-entry.amt-rub)
         NO-ERROR.
         IF ERROR-STATUS:ERROR THEN
            &IF DEFINED(open-undo) <> 0 &THEN
               {&open-undo}.
            &ELSE
               UNDO, RETRY.
            &ENDIF  
      END.
   END.

   IF vstatus BEGINS "А" THEN DO:
      RUN GetClassMethod IN h_xclass (op.class-code, "Delete", "", "", OUTPUT vDelProc, OUTPUT vDelProcParam). 
      IF vDelProc = "pn-op-trans-del" OR vDelProc = "pn-op-svod-del" THEN DO:
         pick-value = "no".
         RUN RunClassMethod in h_xclass (op.class-code, "Delete", "", "", ?, STRING(RECID(op))).
         IF pick-value <> "yes" THEN DO:
            &IF DEFINED(open-undo) <> 0 &THEN
               {&open-undo}.
            &ELSE
               UNDO, RETRY.
            &ENDIF
         END.
      END.

      IF GetXattrValue("op",STRING(op.op),"КодВидаПлатежа") =  "Казначейство" THEN DO:
      
         vLinks = GetLinks(op.class-code,
                           STRING(op.op),
                           "",
                           "dockz_RKC",
                           ",",
                           cur-op-date).

         IF {assigned vLinks} THEN DO:

            vLstClass = Ls-Class("opb-trancfKZ").
          
            IF CAN-DO(vLstClass,op.class-code) THEN DO:
               RUN DelLinksCode(op.class-code,
                                "dockz_RKC",
                                STRING(op.op),
                                "",
                                "",
                                OUTPUT vOK).
            END.
            ELSE DO:
               RUN Fill-SysMes IN h_tmess (
                  "", "", "0",
                  "Аннулирование запрещено, необходимо сначала аннулировать сводный документ и удалить связи.").
               &IF DEFINED(open-undo) <> 0 &THEN
                  {&open-undo}.
               &ELSE
                  UNDO, RETRY.
               &ENDIF
            END.
         END.
      END.
   END.

   cdate = today. ctime = time.
   FIND code WHERE code.class =  "Статус"
               AND code.code  =   vstatus NO-LOCK NO-ERROR.

   vPrevStat = op.op-status.

   /* На аннулированные и без даты возвращаем дату */
   vFlagAnn = NOT (vStatus BEGINS "А")
              AND op.op-status BEGINS "А".
   IF vFlagAnn THEN 
      op.op-date = cur-op-date.
   /* если меняется статус */
   IF vstatus <> op.op-status THEN DO:
      &IF DEFINED(recheck-op-status) > 0 &THEN
         IF vPrevStat <> mOrigStatus THEN DO:
            RUN SetReturnValue IN THIS-PROCEDURE
               (SUBSTITUTE("Статус документа &1 уже был изменён с &2 на &3",
                           op.doc-num,
                           mOrigStatus,
                           vPrevStat)).
            &IF DEFINED(open-undo) = 0 &THEN
               UNDO, RETRY.
            &ELSE
               {&open-undo}.
            &ENDIF
         END.
      &ENDIF
      ASSIGN
      op.user-inspector = userid("bisquit")
      op.op-status      = vstatus
   .
   END.
   IF vstatus BEGINS "А" THEN
      op.op-date = ?.

   &IF DEFINED(visa) <> 0 &THEN

   ASSIGN
      vVisaList = ""
      vVisaAll  = "".

   FOR EACH bcode WHERE
            bCODE.class   =  "Визы" AND
            bCODE.parent  =  "Визы"
            NO-LOCK:

       vVisaN = GetXattrValue("op", STRING(op.op), bCode.CODE).

       IF CAN-DO("Требуется,Не утверждена", vVisaN) THEN
          {additem.i vVisaList bCode.CODE}

       IF {assigned vVisaN} THEN
          {additem.i vVisaAll bCode.CODE}

   END.

   IF {assigned vVisaAll} THEN DO: 
      /* при возврате документа в статус, на котором заданы процедуры контроля, из статуса для визирования или конечного статуса ДР Виза* удаляются  */

      ASSIGN
         vProcContr = GetCodeMisc("Статус", vStatus, 4)
         vVisaStat  = GetCodeMisc("ПроцедурыКонтр", vProcContr, 1) 
      .

      /* будем считать статус исходным, если на нем задана процедура контроля и статус для визирования 
       и возможен переход из этого статуса в текущий статус */
       
      IF {assigned vProcContr} AND {assigned vVisaStat} AND (CAN-DO(GetCode("Статус", vStatus), vPrevStat)  OR vPrevStat =  vVisaStat) THEN 
         hist: 
         DO:
         FOR EACH history WHERE
                  history.file-name =  "op"
              AND history.field-ref =  STRING(op.op)
              AND history.modify    =  "W"
                  NO-LOCK
               BY history.history-id DESCENDING:
        
            vPos = LOOKUP("op-status",history.field-value).
            IF vPos >  0 THEN DO:
               vHistStat = ENTRY(vPos + 1,history.field-value).

               IF vHistStat =  vStatus THEN DO:   /* убедимся, что документ уже был в этом статусе */
                   
                  DO vJ = 1 TO NUM-ENTRIES(vVisaAll):
                     UpdateSigns("op",
                                 STRING (op.op),
                                 ENTRY(vJ, vVisaAll),  
                                 "",
                                 YES).
                  END.  
                  vVisaList = "".
                  LEAVE hist.  
               END.
            END.
         END.
      END.
   END.
                                                                                                     
   IF {assigned vVisaList} THEN DO:

      IF vStatQuest <> "2" THEN DO:
         pick-value = "1". 
         RUN messmenu.p(10 ,
                     "[ СООБЩЕНИЕ ]",
                     "Перевод документа  " + op.doc-num + "~nв статус " + vStatus + " невозможен.~n" + 
                     "На документе отсутствуют визы " + vVisaList + ".",
                     "Пропустить,Пропустить для всех").
         IF pick-value =  "2" THEN
            vStatQuest = pick-value.  

      END.


      &IF DEFINED(open-undo) <> 0 &THEN
         {&open-undo}.
      &ELSE
         UNDO, RETRY.
      &ENDIF
       
   END.

   ASSIGN
      vProcContr   = GetCodeMisc("Статус", vPrevStat, 4)
      vProcContrIn = GetCodeMisc("Статус", vStatus, 8)
   . 

   IF {assigned vProcContr} AND 
       ((NOT vstatus BEGINS "А" )
         OR 
         /* Глухов. Для карточных проводок надо снять блокировку суммы в процессинге даже для крыжа решили */
        (vstatus BEGINS "А" AND GetXattrValueEx("op",STRING(op.op),"БлокСуммКарт","") NE "" ))
            THEN DO:

      FIND FIRST bCode WHERE
                 bCODE.class =  "ПроцедурыКонтр"
             AND bCode.code  =  vProcContr NO-LOCK NO-ERROR.
      IF NOT AVAIL(bCode) THEN DO:

         &IF DEFINED(open-undo) <> 0 &THEN
            {&open-undo}.
         &ELSE
            UNDO, RETRY.
         &ENDIF

      END.
      vVisaStat  = bCode.misc[1].
      IF NOT {assigned vVisaStat} THEN DO:  /* Если статус для визирования не указан, 
                                               процедура контроля выполняется без механизма визирования */

         IF {assigned bCode.val} THEN DO: /* Процедура */
         
            RUN RunProcControl(INPUT bCode.val, INPUT bCode.description[1], OUTPUT oResult).

            IF oResult <> YES THEN
            &IF DEFINED(proc_cntrl_undo) <> 0 &THEN
                  {&proc_cntrl_undo}
            &ELSE
               &IF DEFINED(open-undo) <> 0 &THEN
                  {&open-undo}.
               &ELSE
                  UNDO, RETRY.
               &ENDIF
            &ENDIF
         
         END.
         ELSE DO:   /* Группа процедур */
         
            FOR EACH grCode WHERE 
                     grCode.class  =  "ПроцедурыКонтр"
                 AND grCode.parent =  bCode.CODE
                 AND grCode.val    <> ""
                     NO-LOCK:
         
               RUN RunProcControl(INPUT grCode.val, INPUT grCode.description[1], OUTPUT oResult).
         
               IF oResult <> YES THEN
               &IF DEFINED(proc_cntrl_undo) <> 0 &THEN
                     {&proc_cntrl_undo}
               &ELSE
                  &IF DEFINED(open-undo) <> 0 &THEN
                     {&open-undo}.
                  &ELSE
                     UNDO, RETRY.
                  &ENDIF
               &ENDIF
         
            END.
         END.      

      END.
      ELSE DO:   /* механизм виз */

         IF {assigned bCode.val} AND GetXattrValue("op",STRING(op.op),bCode.misc[2]) <> "Утверждена" THEN DO: /* Процедура */

            RUN RunProcControl(INPUT bCode.val, INPUT bCode.description[1], OUTPUT oResult).
            IF oResult =  ?  THEN DO:
               &IF DEFINED(open-undo) <> 0 &THEN
                  {&open-undo}.
               &ELSE
                  UNDO, RETRY.
               &ENDIF
            END.

            IF oResult <> YES THEN DO:

               op.op-status = vPrevStat. 
               validate op.
               UpdateSigns("op",
                           STRING (op.op),
                           bCode.misc[2],  /* ВизаN*/
                           "Требуется",
                           YES).

               op.op-status = bCode.misc[1]. 
               vVisaRes = YES.
            END.
            ELSE 
               UpdateSigns("op",
                           STRING (op.op),
                           bCode.misc[2],  /* ВизаN*/
                           "Не требуется",
                           YES).
         END.
         ELSE DO:

            FOR EACH grCode WHERE 
                     grCode.class  =  "ПроцедурыКонтр"
                 AND grCode.parent =  bCode.CODE
                 AND grCode.val    <> ""
                     NO-LOCK:

               vVisa = GetXattrValue("op",STRING(op.op),grCode.misc[2]). 
               IF vVisa <> "Утверждена" AND NOT (vVisa =  "Требуется" AND op.op-status =  bCode.misc[1]) THEN DO:

                  RUN RunProcControl(INPUT grCode.val, INPUT grCode.description[1], OUTPUT oResult).
                  IF oResult =  ?  THEN DO:
                     &IF DEFINED(open-undo) <> 0 &THEN
                        {&open-undo}.
                     &ELSE
                        UNDO, RETRY.
                     &ENDIF
                  END.

                  IF oResult <> YES THEN DO:
                 
                     op.op-status = vPrevStat.
                     validate op.
                
                     UpdateSigns("op",
                                 STRING (op.op),
                                 grCode.misc[2],  /* ВизаN*/
                                 "Требуется",
                                 YES).
                     op.op-status = bCode.misc[1].
                     vVisaRes = YES.                 
                  END.
                  ELSE 
                     UpdateSigns("op",
                                 STRING (op.op),
                                 grCode.misc[2],  /* ВизаN*/
                                 "Не требуется",
                                 YES).
               END.  
            END.
         END.          
      END.
   END.
   IF {assigned vProcContrIn} AND NOT vVisaRes THEN DO:
      FIND FIRST bCode WHERE
                 bCODE.class =  "ПроцедурыКонтр"
             AND bCode.code  =  vProcContrIn NO-LOCK NO-ERROR.
      IF NOT AVAIL(bCode) THEN DO:
         &IF DEFINED(open-undo) <> 0 &THEN
            {&open-undo}.
         &ELSE
            UNDO, RETRY.
         &ENDIF
      END.
      vVisaStat  = bCode.misc[1].
      IF NOT {assigned vVisaStat} THEN DO:  /* Если статус для визирования не указан, 
                                               процедура контроля выполняется без механизма визирования */

         IF {assigned bCode.val} THEN DO: /* Процедура */
            RUN RunProcControl(INPUT bCode.val,  INPUT bCode.description[1], OUTPUT oResult).     

            IF oResult <> YES THEN
            &IF DEFINED(proc_cntrl_undo) <> 0 &THEN
                  {&proc_cntrl_undo}
            &ELSE
               &IF DEFINED(open-undo) <> 0 &THEN
                  {&open-undo}.
               &ELSE
                  UNDO, RETRY.
               &ENDIF
            &ENDIF
         END.      
         ELSE DO:   /* Группа процедур */
        
            FOR EACH grCode WHERE 
                     grCode.class  =  "ПроцедурыКонтр"
                 AND grCode.parent =  bCode.CODE
                 AND grCode.val    <> ""
                     NO-LOCK:
        
               RUN RunProcControl(INPUT grCode.val,   INPUT grCode.description[1], OUTPUT oResult).
        
               IF oResult <> YES THEN
               &IF DEFINED(proc_cntrl_undo) <> 0 &THEN
                     {&proc_cntrl_undo}
               &ELSE
                  &IF DEFINED(open-undo) <> 0 &THEN
                     {&open-undo}.
                  &ELSE
                     UNDO, RETRY.
                  &ENDIF
               &ENDIF
        
            END.
         END.
      END.
      ELSE DO:

         IF {assigned bCode.val} AND GetXattrValue("op",STRING(op.op),bCode.misc[2]) <> "Утверждена" THEN DO: /* Процедура */

            RUN RunProcControl(INPUT bCode.val, INPUT bCode.description[1], OUTPUT oResult).
            IF oResult =  ?  THEN DO:
               &IF DEFINED(open-undo) <> 0 &THEN
                  {&open-undo}.
               &ELSE
                  UNDO, RETRY.
               &ENDIF
            END.

            IF oResult <> YES THEN DO:

               op.op-status =  vPrevStat. 
               validate op.

               UpdateSigns("op",
                           STRING (op.op),
                           bCode.misc[2],  /* ВизаN*/
                           "Требуется",
                           YES).
               op.op-status = bCode.misc[1].
            END.
            ELSE 
               UpdateSigns("op",
                           STRING (op.op),
                           bCode.misc[2],  /* ВизаN*/
                           "Не требуется",
                           YES).
         END.
         ELSE DO:

            FOR EACH grCode WHERE 
                     grCode.class  =  "ПроцедурыКонтр"
                 AND grCode.parent =  bCode.CODE
                 AND grCode.val    <> ""
                     NO-LOCK:

               vVisa = GetXattrValue("op",STRING(op.op),grCode.misc[2]). 

               IF vVisa <> "Утверждена" AND NOT (vVisa =  "Требуется" AND op.op-status =  bCode.misc[1]) THEN DO:
                  RUN RunProcControl(INPUT grCode.val, INPUT grCode.description[1], OUTPUT oResult).
                  IF oResult =  ?  THEN DO:
                     &IF DEFINED(open-undo) <> 0 &THEN
                        {&open-undo}.
                     &ELSE
                        UNDO, RETRY.
                     &ENDIF
                  END.

                  IF oResult <> YES THEN DO:
                 
                     op.op-status = vPrevStat.
                     validate op.
                 
                     UpdateSigns("op",
                                 STRING (op.op),
                                 grCode.misc[2],  /* ВизаN*/
                                 "Требуется",
                                 YES). 
                     op.op-status = bCode.misc[1].
                  END.
                  ELSE 
                     UpdateSigns("op",
                                 STRING (op.op),
                                 grCode.misc[2],  /* ВизаN*/
                                 "Не требуется",
                                 YES).
               END.
            END.
         END. 
      END.
      
   END.
   &ENDIF
         
   cddif = NO.
   FIND FIRST op-entry OF op WHERE op-entry.acct-db =  ? NO-LOCK NO-ERROR.
   IF AVAIL op-entry THEN cddif = YES.
   ELSE DO:
      FIND FIRST op-entry OF op WHERE op-entry.acct-cr =  ? NO-LOCK NO-ERROR.
      IF AVAIL op-entry THEN cddif = YES.
   END.

   pick-value = "no".
   RUN RunClassMethod IN h_xclass(op.class-code,"chkupd","","",
                              ?,string(recid(op)) + ",status").
   IF NOT CAN-DO("no-method,no-proc",RETURN-VALUE) AND pick-value <> "yes" THEN DO:
      &IF DEFINED(open-undo) <> 0 &THEN
         {&open-undo}.
      &ELSE
         UNDO, RETRY.
      &ENDIF
   END.

   RUN SetSysConf IN h_base("NoFmsDocChk","YES").
                        /* Проверка документа. */
   IF       code.misc[1]         =  ""
      OR    TRIM(code.misc[1])   =  "да"  THEN 
   DO:
      RUN SetSysConf IN h_base ("ПредСтатус", vPrevStat).
      RUN Check-Op IN h_op (RECID(op),
                            "{&871}",
                            "{&OffKNF}",
                            "{&OFcur-bal}",
                            "{&chkupd}",
                            IF vstatus =  "А" THEN "Off-VO" ELSE "").
      RUN DeleteOldDataProtocol IN h_base ("ПредСтатус").
   END.
   ELSE /*Общие проверки отключены, но баланс проводок проверить надо*/
      RUN Check-Op-balance IN h_op (RECID(op),"{&OFcur-bal}").

   IF RETURN-VALUE <> "" THEN DO:
      RUN DeleteOldDataProtocol IN h_base ("NoFmsDocChk").
      &IF DEFINED(open-undo) <> 0 &THEN
         {&open-undo}.
      &ELSE
         UNDO, RETRY.
      &ENDIF     
   END.
   RUN DeleteOldDataProtocol IN h_base ("NoFmsDocChk").  


   IF vstatus >= gop-status THEN DO: /* доп реквизиты */
      RUN chsigns.p (op.class-code,"OP",STRING(op.op), NO, OUTPUT v-xattr-cr-I).
      IF v-xattr-cr-I = 2 THEN DO: /* если они есть (и требуют редактирования) */
                                     /* то вызываем процедуру редактирования этих реквизитов */
           &IF DEFINED(xattr-undo) <> 0 &THEN
              IF vSelectQuest =  ? THEN
              RUN messmenu.p(10 ,
                          "[ СООБЩЕНИЕ ]",
                          "У документа " + op.doc-num + " не введены~n обязательные доп. реквизиты",
                          "Пропустить,Пропустить для всех,Изменить статус,Изменить статус для всех").
              ASSIGN
              vSelectQuest = pick-value WHEN CAN-DO("2,4",pick-value).
              IF    CAN-DO("1,2",pick-value)
                 OR vSelectQuest =  "2" 
                 OR LASTKEY =  KEYCODE("ESC") THEN DO:
                 {&xattr-undo}.
              END.
           &ELSE
              UNDO,LEAVE.
           &ENDIF
      END.
   END.
&IF DEFINED(BUFF-BOP-ENTRY) &THEN
&ELSE
   &GLOBAL-DEFINE BUFF-BOP-ENTRY YES
   DEFINE BUFFER bop-entry1 FOR op-entry.
   DEFINE BUFFER bkau-entry1 FOR kau-entry.
&ENDIF


   IF CAN-FIND(FIRST op-entry OF op) THEN
      FOR EACH kau-entry OF op NO-LOCK:

         FIND FIRST bkau-entry1 EXCLUSIVE-LOCK WHERE RECID(bkau-entry1) = RECID(kau-entry) NO-WAIT NO-ERROR.
   
         IF LOCKED bkau-entry1 THEN 
         DO:
            RUN wholocks2.p (RECID(kau-entry), "kau-entry", "Запись в kau-entry заблокирована").
            &IF DEFINED(open-undo) <> 0 &THEN
               {&open-undo}.
            &ELSE
            UNDO, RETRY.
            &ENDIF     
         END.
   
         ASSIGN bkau-entry1.op-status = op.op-status
                bkau-entry1.op-date   = op.op-date
         .
      END.
   ELSE
/* есть документы без проводок, но с субаналитикой */
      FOR EACH kau-entry OF op NO-LOCK:
         RUN chst_kau.p(kau-entry.op, kau-entry.kau-entry, kau-entry.op-entry, op.op-status, op.op-date).
      END.
   
   FOR EACH op-entry OF op NO-LOCK:
      FIND FIRST bop-entry1 EXCLUSIVE-LOCK WHERE RECID(bop-entry1) = RECID(op-entry) NO-WAIT NO-ERROR.

      IF LOCKED bop-entry1 THEN 
      DO:
         RUN wholocks2.p (RECID(op-entry), "op-entry", "Запись в op-entry заблокирована").
         &IF DEFINED(open-undo) <> 0 &THEN
            {&open-undo}.
         &ELSE
         UNDO, RETRY.
         &ENDIF     
      END.
      IF NOT cddif                            AND
         cur-op-date      <> op-entry.op-date AND
         bop-entry1.amt-cur <> 0                AND
         bop-entry1.curr    >  ""                    THEN 
      DO:
         bop-entry1.amt-rub    = CurToBase("УЧЕТНЫЙ",
                                           bop-entry1.currency,
                                         cur-op-date,
                                           bop-entry1.amt-cur).
         bop-entry1.value-date = cur-op-date.
      END.

      ASSIGN
         bop-entry1.op-status  = op.op-status
         bop-entry1.op-date    = op.op-date
      .
      FOR EACH kau-entry OF op-entry NO-LOCK:

         FIND FIRST bkau-entry1 EXCLUSIVE-LOCK WHERE RECID(bkau-entry1) = RECID(kau-entry) NO-WAIT NO-ERROR.

         IF LOCKED bkau-entry1 THEN 
         DO:
            RUN wholocks2.p (RECID(kau-entry), "kau-entry", "Запись в kau-entry заблокирована").
            &IF DEFINED(open-undo) <> 0 &THEN
               {&open-undo}.
            &ELSE
            UNDO, RETRY.
            &ENDIF     
         END.

         ASSIGN bkau-entry1.op-status = op.op-status
                bkau-entry1.op-date   = op.op-date.
      END.

      IF code.misc[1]       =  ""   OR
         TRIM(code.misc[1]) =  "да" THEN DO :

         IF vPrevStat > op.op-status AND op.op-status < "П" THEN
            RUN SetSysConf IN h_base ( "ОтменитьПроверкуСобытий",
                                       op.op-kind         + "!" + STRING(RECID(op-entry)) + "#" + "d_dsp"
                                       ).       

         RUN SetSysConf IN h_base ( "ПредСтатус", vPrevStat).    
         RUN SetSysConf IN h_base("NoFmsDocChk","YES").
         RUN Check-Op-Entry IN h_op (BUFFER op-entry,
                                     INPUT  op-entry.op-date,
                                     INPUT  NO,
                                     INPUT  "status",
                                     INPUT  vCorrChk,
                                     OUTPUT flager).

         RUN DeleteOldDataProtocol IN h_base ("ОтменитьПроверкуСобытий").
         RUN DeleteOldDataProtocol IN h_base ("ПредСтатус").
         RUN DeleteOldDataProtocol IN h_base ("NoFmsDocChk").  

         IF flager > 0 THEN DO:
           vStrTmp = GetSysConf("block-acct-msg") NO-ERROR.
           IF vStrTmp = ? THEN DO:
              &IF DEFINED(open-undo) <> 0 &THEN
                 {&open-undo}.
              &ELSE
                 UNDO, RETRY.
              &ENDIF
           END.
           ELSE DO:
              RUN DeleteOldDataProtocol IN h_base ("block-acct-msg").
              RUN putlog(vStrTmp).
              &IF DEFINED(del-undo) NE 0 &THEN
                 {&del-undo}.
              &ELSE
                 UNDO, RETRY.
              &ENDIF
           END.
         END.
      END.
      IF AVAIL op-entry THEN
      RUN Anl-Stb IN h_op (BUFFER op-entry, OUTPUT flager).
      IF flager > 0 THEN DO:
         &IF DEFINED(open-undo) <> 0 &THEN
            {&open-undo}.
         &ELSE
            UNDO, RETRY.
         &ENDIF
      END.

      /* Авторизация при повышении/понижении статуса */
      IF {assigned vPrevStat} THEN
      DO:
         vTypeAutor = ?.
         /* Повышаем статус > минимального статуса авторизации */

         IF (vPrevStat < vOpAuthoStatus) AND (vstatus >= vOpAuthoStatus) THEN
         DO:
            vTypeAutor = "D".
            /* при смены статуса возврат из аннулированных */
            IF vPrevStat BEGINS "А" THEN
            DO:
               vTypeAutor = "U".
   END.
END.
         /* Пнижаем статус < минимального статуса авторизации */
         IF (vPrevStat >= vOpAuthoStatus) AND (vstatus < vOpAuthoStatus) THEN
         DO:
            vTypeAutor = "R".
            /* при смены статуса на аннулированный */
            IF vstatus BEGINS "А" THEN
            DO:
               vTypeAutor = "A".
            END.
         END.

         RUN VerFuturDay  IN h_autho (
             ROWID(op-entry),
             OUTPUT vNoMakeDoc,
             OUTPUT vNoAuthorization).

         IF vNoMakeDoc THEN DO: /* Запрещено создавать документы */
            flager = 9.
            &IF DEFINED(open-undo) <> 0 &THEN
               {&open-undo}.
            &ELSE
               UNDO, RETRY.
            &ENDIF
         END.

         IF {assigned vTypeAutor} AND
             vNoAuthorization = NO THEN   /* нет запрета на авторизацию */
         DO:
            RUN AuthorizationRequest IN h_autho (ROWID(op-entry), vTypeAutor, OUTPUT mVL_Except).
            IF mVL_Except <> "0" THEN DO:
               mVL_Except = 'ERR_AVT'.
               flager = 9.
               &IF DEFINED(open-undo) <> 0 &THEN
                  {&open-undo}.
               &ELSE
                  UNDO, RETRY.
               &ENDIF
            END.
         END.

      END.
      IF op-entry.op-status >= CHR(251) and op-entry.kau-db begins "Депоз" AND NUM-ENTRIES(op-entry.kau-db) >= 2 THEN DO:
         RUN run-chkdb.p(RECID(op-entry),cur-op-date,OUTPUT mErrMsgDb).
         IF mErrMsgDb <> "" THEN DO:
            RUN Fill-SysMes IN h_tmess ("","","0",mErrMsgDb).
              &IF DEFINED(open-undo) <> 0 &THEN
                 {&open-undo}.
              &ELSE
                 UNDO, RETRY.
              &ENDIF
         END.
      END.
   END. /* for each op-entry */
     /* Интеграция с Золотой Короной */
   vUIN   = GetXAttrValue("op",STRING(op.op),"UIN").   
   vCount1 = 0.
   IF {assigned vUIN} THEN
   DO:
      FOR EACH signs WHERE signs.file-name   =  "op"
                       AND signs.code        =  "UIN"
                       AND signs.code-value =  vUIN
         NO-LOCK,
         EACH gc-op WHERE gc-op.op =  INT64(signs.surrogate) AND gc-op.op-status <  CHR(251) NO-LOCK:
         vCount1 = vCount1 + 1.
      END.
   END.
   IF op.op-status >= CHR(251) AND {assigned vUIN} AND vCount1 =  0
   THEN 
   DO:
      DO vCount2 = 1 TO 3:
         RUN confirmation.p (INPUT op.op,Output vCodeMes,Output vMes) NO-ERROR.
         IF vCodeMes <> 0 THEN PAUSE 6 MESSAGE "Передается сообщение в Золотую Корону.".
      END.
      IF {assigned vMes} THEN DO:
         Assign
            mEmail = FGetSetting("LimCtrlParams", "BadEmail", "SStrahov@bis.ru").
         ASSIGN
            vEMail       = mEmail
            vSubjectText = "Ошибка передачи в Золотую Корону"
            vFileTxt     = "Не удалось отправить подтверждение Перевода"  + "~n" +
                           "(Service Confirmation)".
         RUN SendFMail(vEMail, vSubjectText, vFileTxt, "", "sendemail.log").
         MESSAGE vMes VIEW-AS ALERT-BOX.
         &IF DEFINED(open-undo) NE 0 &THEN
            {&open-undo}.
         &ELSE
            UNDO, RETRY.
         &ENDIF
      END.
   END.

   IF vStatus GE CHR(251)  THEN
   DO:
      IF CAN-DO("01БУМ,01КЛ",op.doc-type) THEN
	 FOR EACH op-entry OF op WHERE op-entry.acct-cr BEGINS "30102" NO-LOCK,
            EACH acct WHERE acct.acct     EQ op-entry.acct-cr 
                        AND acct.currency EQ op-entry.currency
            NO-LOCK:
                IF NOT CAN-DO(FGetSetting("ВнешПлатКрыж","",""),USERID("bisquit")) AND NOT IsUserAdm(USERID("bisquit")) THEN
                DO:
                   RUN Fill-SysMes IN h_tmess ("","","",
                      SUBSTITUTE("Пользователь &1 не имеет права переводить внешние платежи в статус √",USERID("bisquit"))).      
                   &IF DEFINED(open-undo) NE 0 &THEN
                      {&open-undo}.
                   &ELSE
                      UNDO, RETRY.
                   &ENDIF
             END.
         END.
   
      FOR EACH op-entry OF op WHERE op-entry.acct-cr BEGINS "407" OR op-entry.acct-cr BEGINS "40802" NO-LOCK,
         EACH acct WHERE acct.acct     EQ op-entry.acct-cr 
                     AND acct.currency EQ op-entry.currency
         NO-LOCK:
           IF CAN-DO("1,7",STRING(WEEKDAY(op.op-date))) THEN
           DO:
                pick-value = "NO".
                RUN Fill-SysMes IN h_tmess ("","","",
                   SUBSTITUTE("ВНИМАНИЕ !!! &1 Платежи на счета ЮЛ/ИП запрещены для статуса √ в выходные дни !!!",CHR(10))).      
                pick-value = "NO".
                IF pick-value NE "YES" THEN
                DO:
                   &IF DEFINED(open-undo) NE 0 &THEN
                      {&open-undo}.
                   &ELSE
                      UNDO, RETRY.
                   &ENDIF
                END.
          END.
      END.
   END.

   IF vStatus EQ "ФБН" 
      AND op.filial-id EQ "0500" 
      AND CAN-FIND(FIRST op-bank OF op WHERE op-bank.bank-name MATCHES '*сбербанк*' USE-INDEX op-bank NO-LOCK) 
      AND STRING(TIME + (360 - TIMEZONE) * 60,"HH:MM:SS") LT REPLACE(SUBSTR(TRIM(FGetSetting('СБРФЭкспортДо',?,"14:00")),1,5),"-",":")
   THEN 
   DO:
      FOR EACH op-entry OF op WHERE op-entry.acct-cr EQ "30102810152090000884     @0500" 
                                AND CAN-DO("!30110*,*",op-entry.acct-db)
                                AND op-entry.amt-rub LE 5000000
      EXCLUSIVE-LOCK:
         op-entry.acct-cr = "30110810101100000047     @0500".
      END.
   END.

   IF vStatus EQ "ФБО" 
   THEN 
   DO:
      FOR EACH op-entry OF op WHERE op-entry.acct-cr BEGINS "30102" 
                                AND op-entry.amt-rub GE 100000000
                                AND op-entry.type NE "НС"
      EXCLUSIVE-LOCK:
         RUN Fill-SysMes IN h_tmess ("","","",
             SUBSTITUTE("ВНИМАНИЕ: сумма по платежному поручению &1 превышает сумму 100 млн рублей. Требуется отправка БЭСП!",op.doc-num)).
      END.
   END.

END.
/* $LINTFILE='chst(op.i' */
/* $LINTMODE='1' */
/* $LINTENV ='2st' */
/* $LINTVSS ='*' */
/* $LINTUSER='krok' */
/* $LINTDATE='31/03/2016 18:26:14.794+04:00' */
/*prosign1v6k3FUSlIMctAHGxSh7iw*/