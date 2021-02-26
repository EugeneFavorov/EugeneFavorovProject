/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2008 ЗАО "Банковские информационные системы"
     Filename: ACCTCLOPR.P
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
      Created: 15.04.2009 18:25 BIS     
     Modified: 15.04.2009 18:25 BIS     
*/

{globals.i}
{intrface.get xclass}
{intrface.get tmess}
{sh-defs.i}
{dps-a-cl.tmp SHARED}
                                                   
DEF INPUT  PARAM in-cont-code LIKE loan.cont-code   NO-UNDO.
DEF INPUT  PARAM in-op-date   LIKE op-date.op-date  NO-UNDO.
DEF OUTPUT PARAM oResult      AS INT64            NO-UNDO.

DEF VAR i                     AS INT64           NO-UNDO.
DEF VAR mess                  AS CHAR EXTENT 100   NO-UNDO.
DEF VAR flagerr               AS LOGICAL           NO-UNDO.
DEF VAR vQuestLog             AS LOGICAL           NO-UNDO. /*Переменная для сообщений с выбором*/
DEF VAR vPosSinceDate         AS DATE              NO-UNDO.
DEF VAR gLogMessage           AS LOG               NO-UNDO.
DEF VAR list_type             AS CHAR              NO-UNDO.
DEF VAR vMainAcct             AS LOGICAL           NO-UNDO.
DEF VAR vClosedMainAcct       AS LOGICAL INIT YES  NO-UNDO.

DEF BUFFER bb-loan-acct FOR loan-acct.
DEF BUFFER bb-loan      FOR loan.
DEF BUFFER b-loan-acct  FOR loan-acct.
DEF BUFFER bAcct        FOR acct.
DEF BUFFER xloan-acct   FOR loan-acct.

MAIN:
DO ON ERROR  UNDO MAIN, LEAVE MAIN
   ON ENDKEY UNDO MAIN, LEAVE MAIN:

   IF GetSysConf("gLogMessage") = "Да" THEN 
      gLogMessage = TRUE.
   
   mess[1]  = "Счет до востребования не найден.".
   mess[2]  = "Срочный счет не найден.".
   mess[3]  = "Счет начисленных процентов не найден" .
   mess[4]  = "Счет начисленных процентов 'Капитал' не найден" .
   mess[5]  = "Счет довложений не найден.".
   mess[6]  = "Счет довложений 'Капитал' не найден.".
   mess[7]  = "Счет просрочки не найден.".
   mess[8]  = "Счет расчетов по налогам не найден.".
   mess[9]  = "Счет 'Расходы будущих периодов' не найден.".
   
   mess[10] = "Есть неакцептованные документы по счету до востребования.".
   mess[11] = "Есть неакцептованные документы по срочному счету.".
   mess[12] = "Есть неакцептованные документы по счету начисленных процентов.".
   mess[13] = "Есть неакцептованные документы по счету начисленных процентов 'Капитал'.".
   mess[14] = "Есть неакцептованные документы по счету довложений.".
   mess[15] = "Есть неакцептованные документы по счету довложений 'Капитал'.".
   mess[16] = "Есть неакцептованные документы по счету просрочки.".
   mess[17] = "Есть неакцептованные документы по счету расчетов по налогам.".
   mess[18] = "Есть неакцептованные документы по счету 'Расходы будущих периодов'.".
   
   mess[19] = "Счет до востребования привязан к другому вкладу.".
   mess[20] = "Срочный счет привязан к другому вкладу.".
   mess[21] = "Счет начисленных процентов привязан к другому вкладу.".
   mess[22] = "Счет начисленных процентов 'Капитал' привязан к другому вкладу.".
   mess[23] = "Cчет довложений привязан к другому вкладу.".
   mess[24] = "Cчет довложений 'Капитал' привязан к другому вкладу.".
   mess[25] = "Cчет просрочки привязан к другому вкладу.".
   mess[26] = "Счет расчетов по налогам привязан к другому вкладу.".
   mess[27] = "Счет 'Расходы будущих периодов' привязан к другому вкладу.".
   
   mess[28] = "Ненулевой остаток на счете до востребования.".
   mess[29] = "Ненулевой остаток на срочном счете.".
   mess[30] = "Ненулевой остаток на счете начисленных процентов.".
   mess[31] = "Ненулевой остаток на счете начисленных процентов 'Капитал'.".
   mess[32] = "Ненулевой остаток на счете довложений.".
   mess[33] = "Ненулевой остаток на счете довложений 'Капитал'.".
   mess[34] = "Ненулевой остаток на счете просрочки.".
   mess[35] = "Ненулевой остаток на счете расчетов по налогам.".
   mess[36] = "Ненулевой остаток на счете 'Расходы будущих периодов'".
   
   mess[37] = "На счете до востребования есть документы после даты закрытия.".
   mess[38] = "На срочном счете есть документы после даты закрытия.".
   mess[39] = "На счете начисленных процентов есть документы после даты закрытия.".
   mess[40] = "На счете начисленных процентов 'Капитал' есть документы после даты закрытия.".
   mess[41] = "На счете довложений есть документы после даты закрытия.".
   mess[42] = "На счете довложений 'Капитал' есть документы после даты закрытия.".
   mess[43] = "На счете просрочки есть документы после даты закрытия.".
   mess[44] = "На счете расчетов по налогам есть документы после даты закрытия.".
   mess[45] = "На счете 'Расходы будущих периодов' есть документы после даты закрытия.".
   
   mess[46] = "Cчет до востребования открыт после даты закрытия. Закрыть его?".
   mess[47] = "Срочный счет открыт после даты закрытия. Закрыть его?".
   mess[48] = "Счет начисленных процентов открыт после даты закрытия. Закрыть его?".
   mess[49] = "Счет начисленных процентов 'Капитал' открыт после даты закрытия. Закрыть его?".
   mess[50] = "Счет довложений открыт после даты закрытия. Закрыть его?".
   mess[51] = "Счет довложений 'Капитал' открыт после даты закрытия. Закрыть его?".
   mess[52] = "Cчет просрочки открыт после даты закрытия. Закрыть его?".
   mess[53] = "Счет расчетов по налогам открыт после даты закрытия. Закрыть его?".
   mess[54] = "Счет 'Расходы будущих периодов' открыт после даты закрытия. Закрыть его?".
   
   mess[55] = "Cчет до востребования кем-то редактируется и будет оставлен без изменений.".
   mess[56] = "Срочный счет кем-то редактируется и будет оставлен без изменений.".
   mess[57] = "Счет начисленных процентов кем-то редактируется и будет оставлен без изменений.".
   mess[58] = "Счет начисленных процентов 'Капитал' кем-то редактируется и будет оставлен без изменений.".
   mess[59] = "Счет довложений кем-то редактируется и будет оставлен без изменений.".
   mess[60] = "Счет довложений 'Капитал' кем-то редактируется и будет оставлен без изменений.".
   mess[61] = "Cчет просрочки кем-то редактируется и будет оставлен без изменений.".
   mess[62] = "Счет расчетов по налогам кем-то редактируется и будет оставлен без изменений.".
   mess[63] = "Счет 'Расходы будущих периодов' кем-то редактируется и будет оставлен без изменений.".

   FIND loan WHERE loan.contract  EQ 'DPS'
               AND loan.cont-code EQ in-cont-code 
      EXCLUSIVE-LOCK NO-ERROR.   
   IF NOT AVAIL loan THEN DO:
      IF LOCKED loan THEN
         IF NOT gLogMessage  THEN
            RUN Fill-SysMes IN h_tmess ("", "", "0", "С этим вкладом работают.").
         ELSE 
            RUN LogMess("С вкладом" + in-cont-code + "работают.").
      ELSE
         IF NOT gLogMessage  THEN
            RUN Fill-SysMes IN h_tmess ("", "", "0", "Вклад не найден.").         
         ELSE
            RUN LogMess("Вклад" + in-cont-code +  "не найден.").
      UNDO MAIN, LEAVE MAIN.
   END.

   TMP_ACCT:
   FOR EACH tmp-acct-cl WHERE tmp-acct-cl.acct-type NE ""
      NO-LOCK,
       FIRST loan-acct WHERE loan-acct.contract  EQ 'DPS'
                         AND loan-acct.cont-code EQ in-cont-code
                         AND loan-acct.acct-type EQ tmp-acct-cl.acct-type
                         AND loan-acct.acct      EQ tmp-acct-cl.acct
                         AND loan-acct.since     EQ tmp-acct-cl.since
      NO-LOCK:

         i = tmp-acct-cl.num.

         FIND FIRST acct WHERE acct.acct     EQ loan-acct.acct
                           AND acct.currency EQ loan-acct.currency
            NO-LOCK NO-ERROR.
         IF NOT AVAIL acct THEN DO:
            IF NOT gLogMessage  THEN
               RUN Fill-SysMes IN h_tmess ("", "", "0", mess[ i ]).            
            ELSE 
               RUN LogMess(mess[i]).
            NEXT TMP_ACCT.
         END.      

      {ac-cloa.i
         &main_cycle = "TMP_ACCT "
         &total      = "tmp-acct-cl.TotalNum "
      }
   END.

   IF NOT vClosedMainAcct THEN
      oResult = -1.
END.

{intrface.del}

PROCEDURE LogMess.
   DEF INPUT PARAMETER iLogMess AS CHAR NO-UNDO.

   DEF VAR vLogMess AS CHAR NO-UNDO.

   vLogMess = GetSysConf("LogMessage").
   IF vLogMess NE ? THEN 
      iLogMess = vLogMess + '~n' + iLogMess.
   RUN SetSysConf IN h_base ("LogMessage",iLogMess).
END PROCEDURE.
