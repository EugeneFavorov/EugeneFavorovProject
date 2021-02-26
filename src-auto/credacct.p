/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-1998 ТОО "Банковские информационные системы"
     Filename: CREDACCT.P
      Comment: Запускает обработку шаблонов счетов указанной транзакции.
   Parameters:
         Uses: shttacct.DEF - Зашареная временная таблица по счетам
               dpsproc.DEF
               dpsproc.p    - Библиотека процедур (list-op-templ - 
                              список шаблонов)
               accttmpl.p   - Обработка одного шаблона счета.
               g-edac.p     - Редактирование(отображение) созданных(найденных)
                              счетов
      Used BY:
      Created: 30.10.2001 Kostik
     Modified: 02.07.2002 18:35 KSV      (0007589) Добавлен индикатор выполнения
     	                                   транзакции.
     Modified: 26.07.2004 19:36 FEPA
     Modified: 07.09.2006 19:36 NIK      Пакетный режим
     Modified: 03.11.2006 16:54 Daru     <comment>
     Modified: 12.12.2007 jadv       (0083662)
*/

{globals.i}

/* {profile.def} */
{intrface.get tmess}    /* Инструменты обработки сообщений. */
{intrface.get loanx}
{intrface.get pbase}
{intrface.get acct}
{intrface.get loan}

DEFINE INPUT  PARAM in-op-date AS DATE      NO-UNDO.
DEFINE INPUT  PARAM iRid-opkind AS RECID     NO-UNDO.
DEFINE INPUT  PARAM irid-loan   AS RECID     NO-UNDO.
DEFINE INPUT  PARAM in-title   AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAM oFl        AS INT64   INIT -1 NO-UNDO.



/*Цикл по шаблонам счета*/
DEF VAR mLst-templ    AS CHARACTER             NO-UNDO.
DEF VAR mPPName      AS CHARACTER             NO-UNDO.

DEF VAR mProcShablMain    AS CHAR             NO-UNDO.
DEF VAR mProcShablOnShabl AS CHAR             NO-UNDO.
DEF VAR mTotal       AS INT64                 NO-UNDO.
DEF VAR mDateSogl    AS CHAR                  NO-UNDO.
DEF VAR mOpenDate    AS DATE                  NO-UNDO.
DEF VAR mProcAfter   AS CHAR                  NO-UNDO.

DEF VAR mph  AS HANDLE NO-UNDO.
DEF VAR mph1 AS HANDLE NO-UNDO.

DEF BUFFER op-kind FOR op-kind. 
DEF BUFFER loan FOR loan .

{dpsproc.def}
{shttacct.def NEW}
{def-wf.i NEW}

{defframe.i NEW}
  


message 'run credacct'.

/*============================================================================*/

{profile CA001}

FIND FIRST op-kind WHERE RECID(op-kind) EQ iRid-opkind
                          NO-LOCK NO-ERROR.
IF NOT AVAIL op-kind THEN RETURN.

FIND FIRST loan WHERE RECID(loan) EQ irid-loan NO-LOCK NO-ERROR.
IF NOT AVAIL loan THEN RETURN.
message 'AVAIL loan'.


/* Сохраним ссылку на договор  для спец обработки в процедурах проверки счетов, 
   в частности для ошибки ACCT19 */
RUN SetSysConf IN h_base ("LoanRecid-Acct19", string(irid-loan)).

{plibinit.i &TransParsLibs = " "}

ASSIGN
   mLst-templ      = list-op-templ(op-kind.op-kind,"acct")
   mProcShablMain = GetXAttrValueEx("op-kind",op-kind.op-kind,"ПроцПодШабл","")
   mDateSogl      = GetXattrValueEx("loan",loan.contract + "," +
                                         loan.cont-code,"ДатаСогл","")
.

IF NOT SearchPFile(mProcShablMain) THEN
   mProcShablMain = "".

cr_frx:
DO TRANSACTION ON ENDKEY UNDO cr_frx,LEAVE cr_frx
               ON ERROR  UNDO cr_frx,LEAVE cr_frx
   WITH FRAME edit-frame:
   {empty tt-editacct}

   IF in-title NE ? THEN DO:           /* Инициализируем индикатор состояния */
      /* Считаем кол-во итераций для индикатора состояния */
      FOR EACH op-template OF op-kind
          WHERE CAN-DO(mLst-templ,STRING(op-template.op-template))
                                                          NO-LOCK:
        mTotal = mTotal + 1.
      END.
/*      {bar-beg2.i &BarTotal = "mTotal" &BarMessage = """Открытие счетов..."""} */
      mTotal = 0.
   END.
   
   mPPName = GetXAttrValue("op-kind", op-kind.op-kind, "parslib").
   IF SearchPFile(mPPName) THEN
      RUN VALUE(mPPName + ".p") PERSISTENT SET mph1.

   SHABL_CYCLE:
   FOR EACH op-template OF op-kind
       WHERE CAN-DO(mLst-templ,STRING(op-template.op-template))
       NO-LOCK:
       {profile CA002} 
      /* Отображаем текущее состояние */
      mTotal = mTotal + 1.

      IF in-title NE ? THEN DO:
/*         {bar2.i &BarPointer = "mTotal" &BarBreak = "UNDO CR_FRX,LEAVE CR_FRX."} */
      END.

      mProcShablOnShabl = GetXAttrValueEx("op-template",
                                          op-template.op-kind + "," +
                                           STRING (op-template.op-template), 
                                          "ПроцПодШабл", 
                                          "").

      IF NOT SearchPFile(mProcShablOnShabl) THEN
         mProcShablOnShabl = "".

      IF mProcShablOnShabl NE "" THEN
      DO:
         RUN VALUE (mProcShablOnShabl + ".p") (RECID(op-template),
                                               irid-loan,
                                               in-op-date).
         IF RETURN-VALUE EQ "NEXT" THEN
         DO:
            oFl = 1.
            NEXT SHABL_CYCLE.
         END.
      END.

      IF mProcShablMain <> "" THEN
      DO:
         RUN VALUE (mProcShablMain + ".p") (RECID(op-template),
                                            irid-loan,
                                            in-op-date).
         IF RETURN-VALUE EQ "NEXT" THEN
         DO:
            oFl = 1.
            NEXT SHABL_CYCLE.
         END.
      END.

      mOpenDate = IF    GetXattrValue("op-template",
                                      op-template.op-kind + "," + 
                                      STRING(op-template.op-template),
                                      "ДатаОткрСчет") EQ "Дата заключения"
                    AND mDateSogl NE "" 
                     THEN DATE(GetXattrValue("loan",loan.contract + "," +
                                                    loan.cont-code,"ДатаСогл"))
                     ELSE in-op-date.

       {profile CA003}
      RUN accttmpl.p (RECID(op-template),
                      irid-loan,
                      mOpenDate).
      IF RETURN-VALUE EQ "-1" THEN do:
    	message loan.cont-code.
         UNDO cr_frx, LEAVE cr_frx.
end.
      {profile CA004}

      FOR LAST tt-editacct,
          FIRST acct WHERE RECID(acct) EQ tt-editacct.rid
      NO-LOCK:
PS:
         DO ON ERROR UNDO PS, LEAVE PS:
            RUN "a-yes.p"   PERSISTENT SET mph.
            mph:PRIVATE-DATA = loan.contract + "," + loan.cont-code + 
            	                   ",loan_flt".
            {profile CA005}

            RUN parssign.p (in-op-date,
                           "op-template",
                           op-kind.op-kind + "," +
                              STRING(op-template.op-template),
                           op-template.class-code,
                           "acct",
                           acct.acct + "," + acct.currency,
                           acct.class-code,
                           ?).
            {profile CA006}
         END.
         IF valid-handle(mph) THEN DELETE PROCEDURE mph.
      END.
   END.
   
   IF valid-handle(mph1) THEN DELETE PROCEDURE mph1.

   PAUSE 0.
   STATUS DEFAULT "".

   /*Редактирование и просмотр созданных счетов*/
   IF in-title NE ?               AND
      CAN-FIND(FIRST tt-editacct) THEN
   DO:
      RUN g-edac.p (in-title, irid-loan,  OUTPUT oFl).
      IF oFl EQ -1 THEN UNDO  cr_frx, LEAVE cr_frx.
      IF oFl EQ  2 THEN UNDO  cr_frx, LEAVE cr_frx.
   END.
   ELSE
      oFl = 0.

/*-------------------------------------------- Привязка счетов к договору --*/
   {profile CA007} 
   IF oFl NE 0 THEN LEAVE cr_frx.
   FOR FIRST loan WHERE RECID(loan) EQ irid-loan
             NO-LOCK:
      FOR EACH tt-editacct
               NO-LOCK,
         FIRST acct WHERE
         RECID(acct) EQ tt-editacct.rid
               NO-LOCK:
         /* Дополнительно после редактирования счетов - инициализация доп.
            реквизитов со счета 2-го порядка 
         ** и из классификатора МаскиНаслед, т.к. счет мог измениться в форме
            редактирования счетов */
         RUN BalToAcct_Xattr(RECID(acct),"*",YES,YES).
         IF RETURN-VALUE EQ "ERROR" THEN
         DO:
            RUN Fill-SysMes IN h_tmess ("","","0","Ошибка при инициализация " + 
                    "доп.реквизитов со счета 2-го порядка и из " + 
                    "классификатора [МаскиНаслед].").
            LEAVE cr_frx.
         END.

         RUN SetKau IN h_loanx (tt-editacct.rid, 
                                irid-loan,
                                tt-editacct.acct-type).
message   "CREATE loan-acct " + acct.acct.
         CREATE loan-acct.
         loan-acct.cont-code = loan.cont-code.
         {lacc.ini
            &loan-acct = loan-acct
            &contract  = loan.contract
            &acct      = acct.acct
            &currency  = acct.currency
            &acct-type = tt-editacct.acct-type
         }
      END.
      /* Если на транзакции определена процедура "Выполнить после" */
      IF {assigned op-kind.after} THEN 
      DO:
         mProcAfter = op-kind.after + '.p'.
         RUN value(mProcAfter) (RECID(loan)).
      END.
   END.
   {profile CA008}
END.

RUN DeleteOldDataProtocol IN h_base ("LoanRecid-Acct19").
 {profile CA010} 
{intrface.del}          /* Выгрузка инструментария. */ 

RETURN.
/******************************************************************************/

/*prosignDxUI10ct6ZTFxuOqPsBgvA*/