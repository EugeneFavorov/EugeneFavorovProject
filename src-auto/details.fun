/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2001 ТОО "Банковские информационные системы"
     Filename: details.fun
      Comment:

  Если Вы решили расширить функциональность парсера, то найдите в  конце этого
  файла процедуру с именем "ПустойШаблон" и комментарием перед ним. Скопируйте
  и то, и другое и пишите себе на здоровье,  но не забудьте прокомментировать,
  что функция делает и как ее вызывать.
                                                                Peter I. Bach.
   Parameters:
         Uses:
      Used by:
      Created: ...
     Modified: 03.12.2001 12:03 SEMA     по заявке 0003724 добавлена функция НаимСчета - возвращает наименование счета
     Modified: 05.12.2001 18:00 SEMA
     Modified: 05.12.2001 19:29 SEMA
     Modified: 20.02.2002 18:21 SEMA     по заявке 0004870 поправлены функции ДогНом и ДатаОтк
     Modified: 13.03.2002 11:42 SEMA     по заявке 0006430 Подъем фикса - создана функция ФизЛицо
     Modified: 13.03.2002 mitr           по заявке 0006472 Подъем фикса - ошибки в догном, грриска, ДатаОтк, Клнт
     Modified: 10.06.2002 18:30 SEMA     по заявке 0007971 перестроены функции ИнвНом, НаимЦенн, GetDetails, NDoc,
                                         Client, NClDoc, NClDocType, NDeposit, DocDate, Клиент2, DocCourse, Классиф,
                                         ВалИСО, КолДок с целью уменьшения кода
     Modified: 28.06.2002 12:49 SEMA     по заявке 0007971 усовершенствование кода
     Modified: 30.10.2002 12:21 SEMA     по заявке 0011574 функционал ф.ФизЛицо убран в основную библиотеку base-pp
     Modified: 11.11.2002 11:05 SEMA     по заявке 0011116 в отдельный файл вынесены процедуры для работы с таблицей
                                         frm-fields, функции ДогНом и NDeposit перенесены в файл parsacct.def
     Modified: 11.11.2002 15:23 SEMA     по заявке 0011932 подъем заявки 0011116
     Modified: 28/04/2004 abko           по 26319 функция "кем, когда выдан документ физлица"
     Modified: 17.09.2004 16:00 laav     заявка 35056. Доработана функциональность парсера Классиф(). Добавлена возможность
                                         работы парсера без вывода броуза указанного классификатора на экран. Для этого
                                         необходимо выбранное значение поля классификатора указать в качестве четвертого
                                         входного параметра.
     Modified: 23.05.2005 18:00 fedm     По заявке 40596 функция Классиф перенесена
                                         из DETAILS.FUN в parsacct.def
     Modified: 23.01.2006 15:02 koav
     Modified: 30.01.2006 18:15 ZIAL     (37171) Парсерная функция ГрРиска()
                                         недоступна для расчета суммы шаблонов
                                         транзакций
     Modified: 31.01.2006 18:15 ZIAL     (37171) Парсерная функция ГрРиска()
                                         недоступна для расчета суммы шаблонов
                                         транзакций
     Modified: 10.05.2006 15:11 DEMO     заявка  0050064. Изменена процедура   НаимСчета
     Modified: 19.05.2006 17:25 MUTA     заявка  0050064. Изменена процедура   НаимСчета
     Modified: 02/11/2007 kraw (0083151) Темпоральные кассовые символы.
     Modified: 21/07/2010 kraw (0090456) GetDetails2, DocDate2, NDoc2
     Modified: 22/12/2010 ches (0136028)  v svaznom   GetSysConf ("in-op-date")   = gend-date uchest pri pod'eme

*/

{intrface.get loan}
{intrface.get i254}
{intrface.get tmcod}
{intrface.get ovl}

PROCEDURE НомерДовер:
    DEF OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* успешно ли выполнена ? */
    DEFINE VARIABLE vProxyNumb   AS CHAR NO-UNDO.
   /* --- */
      is-ok = FALSE.
      IF NOT Pars-ValidParam(0) THEN RETURN.
      vProxyNumb = GetSysConf("ProxyPickVal"). 
      FIND FIRST loan WHERE loan.cont-code EQ vProxyNumb
                      AND   loan.contract  EQ "proxy"
                      NO-LOCK NO-ERROR.
         IF AVAIL loan THEN
            vProxyNumb = loan.doc-num.
               
  RUN Pars-SetCHARResult(vProxyNumb).

   /* --- */
   is-ok = TRUE.
 END PROCEDURE.

/*  **************************************************************************** */
   PROCEDURE Доверенность:
       DEF OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* успешно ли выполнена ? */
       DEFINE VARIABLE vDov   AS CHAR   NO-UNDO.
      /* --- */
      is-ok = FALSE.
      IF NOT Pars-ValidParam(0) THEN RETURN.
      RUN GetLoan.
      IF NOT AVAIL loan THEN
        vDov = "NO".


      DEF BUFFER xloan FOR loan.

      FIND FIRST xloan WHERE xloan.contract  EQ loan.contract
                         AND xloan.cont-code EQ loan.cont-code NO-LOCK NO-ERROR.

      FIND FIRST loan WHERE loan.cust-id     EQ xloan.cust-id
                      AND loan.cust-cat    EQ xloan.cust-cat
                      AND loan.contract    EQ "proxy"
                      NO-LOCK NO-ERROR.
        IF  AVAIL loan THEN
            vDov = "YES".
            ELSE vDov = "NO".

      RUN Pars-SetCHARResult(vDov).
   /* --- */
       is-ok = TRUE.
END PROCEDURE.
/*  **************************************************************************** */

PROCEDURE ДатаДовер:
    DEF OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* успешно ли выполнена ? */
    DEFINE VARIABLE vData   AS CHAR   NO-UNDO.
    DEF VAR vProxyVal AS CHAR NO-UNDO.
   /* --- */
      is-ok = FALSE.
      IF NOT Pars-ValidParam(0) THEN RETURN.
/*      RUN GetLoan.
      IF NOT AVAIL loan THEN
        vData = "".
      DEF BUFFER xloan FOR loan.

      vProxy-code = GetSysConf("_Proxy-code_").
      
      FIND FIRST xloan WHERE xloan.contract  EQ loan.contract
                         AND xloan.cont-code EQ loan.cont-code NO-LOCK NO-ERROR.

      FIND FIRST loan WHERE loan.cust-id   EQ xloan.cust-id
                      AND loan.cust-cat    EQ xloan.cust-cat
                      AND loan.contract    EQ "proxy"
                      AND loan.cont-code   EQ vProxy-code
                      NO-LOCK NO-ERROR.
          IF  AVAIL loan THEN
            vData = STRING(loan.open-date, "99/99/9999").
            ELSE vData = "".
  */
        vProxyVal = GetSysConf("ProxyPickVal").
        FIND FIRST loan WHERE loan.cont-code EQ vProxyVal
                      AND   loan.contract  EQ "proxy"
                      NO-LOCK NO-ERROR.
         IF AVAIL loan THEN
            vData = STRING(loan.open-date, "99/99/9999").

  RUN Pars-SetCHARResult(vData).
   /* --- */
   is-ok = TRUE.
 END PROCEDURE.

/*  ****************************************************************************
  Что делает: Возвращает инвентарный номер ценности в транзакциях ОС, НМА
              системы УМЦ для помещения в поле details
  Синтаксис : ИнвНом()
*/

PROCEDURE ИнвНом:
  DEF OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* успешно ли выполнена ? */
/* --- */
  is-ok = FALSE.
  IF NOT Pars-ValidParam(0) THEN RETURN.
/* --- */
  if chpar1 eq "" or chpar1 eq ? then do:
    message "Не задан инвентарный номер ценности !".
    pause.
    return.
  end.
  RUN Pars-SetCHARResult (chpar1).
/* --- */
  is-ok = TRUE.
END PROCEDURE.
/*   ***************************************************************************
  Что делает: Возвращает наименование ценности в транзакциях ОС, НМА
              системы УМЦ для помещения в поле details
  Синтаксис : НаимЦенн()
*/

PROCEDURE НаимЦенн:
DEF OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* успешно ли выполнена ? */

   DEF VAR i-tmpst    AS INT64  NO-UNDO INIT 1 .
   DEF VAR tmp-chpar1 AS CHAR NO-UNDO.

   is-ok = FALSE.

   MAIN:
   DO ON ERROR  UNDO, LEAVE
      ON ENDKEY UNDO, LEAVE:

      IF NOT Pars-ValidParam(0) THEN
         UNDO MAIN, LEAVE MAIN.

      IF NOT {assigned chpar1} THEN
      DO:
         RUN Fill-SysMes IN h_tmess ("", "", "0", "Не задан инвентарный номер ценности.").
         UNDO MAIN, LEAVE MAIN.
      END.
      IF NUM-ENTRIES(chpar1) GT 1 THEN
      DO:
         DO WHILE i-tmpst LE NUM-ENTRIES(chpar1):
            FIND FIRST loan        WHERE
                       loan.contract  EQ chpar2
                   AND loan.doc-ref   EQ DelFilFromLoan(ENTRY(i-tmpst,chpar1))
                   AND loan.filial-id EQ shFilial
            NO-LOCK.
            FIND FIRST asset OF loan NO-LOCK.
            ASSIGN
               tmp-chpar1 = tmp-chpar1 + ";" + asset.name
               i-tmpst    = i-tmpst + 1
            .
         END.
         tmp-chpar1 = SUBSTRING(tmp-chpar1, 2, LENGTH(tmp-chpar1)). /* для красоты убрать 1 символ */

         /* В наименовании может быть кавычка, т.к. парсер ее срезает, добавляем ее принудительно */
         IF tmp-chpar1 BEGINS """" THEN
            RUN Pars-SetCHARResult (""" " + tmp-chpar1 + " ").
         ELSE
         IF tmp-chpar1 BEGINS "'" THEN
            RUN Pars-SetCHARResult ("' " + tmp-chpar1 + " ").
         ELSE
            RUN Pars-SetCHARResult (tmp-chpar1 + " ").
      END.
      ELSE DO:
         FIND FIRST loan        WHERE
                    loan.contract  EQ chpar2
                AND loan.doc-ref   EQ DelFilFromLoan(chpar1)
                AND loan.filial-id EQ shFilial
         NO-LOCK.
         FIND FIRST asset OF loan NO-LOCK.

         /* В наименовании может быть кавычка, т.к. парсер ее срезает, добавляем ее принудительно */
         IF asset.name BEGINS """" THEN
            RUN Pars-SetCHARResult (""" " + asset.name + " ").
         ELSE
         IF asset.name BEGINS "'" THEN
            RUN Pars-SetCHARResult ("' " + asset.name + " ").
         ELSE
            RUN Pars-SetCHARResult (asset.name + " ").
      END.

   END.

   is-ok = TRUE.
END PROCEDURE.

/* GetDetails: возврат значение, сохраненных из фрейма *
   Использовать: GetDetails (номер_шаблона(id-optempl),Имя_Фрейма (frame-name), Имя_Переменной (frame-field))
             или GetDetails (Имя_Фрейма (frame-name), Имя_Переменной (frame-field)) номер шаблона не учитывается
             или GetDetails (Имя_Переменной (frame-field)) - поле имени фрейма не учитывается

*/
procedure GetDetails:
    DEF OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* успешно ли выполнена ? */

/* --- */
    IF NOT (Pars-ValidParam(1) OR
            Pars-ValidParam(2) OR
            Pars-ValidParam(3))
    THEN RETURN.
/* --- */

    IF pn < 2 THEN
    find first wop where recid(wop) eq rid no-lock no-error.

    def var param-id-optempl as INT64 no-undo.
    def var param-field-name as char no-undo.
    def var param-frame-name as char no-undo.
    DEFINE VARIABLE vResult AS CHARACTER  NO-UNDO.
    ASSIGN
        param-id-optempl = IF pn < 2 THEN
           (IF AVAIL wop THEN wop.op-templ ELSE ? ) ELSE Pars-GetInt ( 0 )
        param-frame-name = IF pn < 1 THEN "" ELSE Pars-GetString ( pn - 1 )
        param-field-name = Pars-GetString ( pn )
        .

    RUN internal-parser-getdetails-form-ttable (param-id-optempl,param-frame-name, param-field-name, OUTPUT vResult).
    IF vResult EQ ? THEN
       RUN ElseTryGetFieldFraMeVal(param-field-name, OUTPUT vResult).
    RUN Pars-SetCHARResult (IF vResult EQ ? THEN "?" ELSE vResult).

/* --- */
    is-ok = TRUE.
end procedure.

procedure GetDetails2:
   DEFINE OUTPUT PARAMETER is-ok AS LOGICAL NO-UNDO. /* успешно ли выполнена ? */

   DEFINE VARIABLE vResult AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vHandle AS HANDLE     NO-UNDO.

/* --- */
   IF NOT (Pars-ValidParam(1) OR
           Pars-ValidParam(2) OR
           Pars-ValidParam(3))
   THEN RETURN.
/* --- */

   FIND FIRST wop WHERE RECID(wop) EQ rid NO-LOCK NO-ERROR.

   IF AVAILABLE wop THEN
       /* Поиск документа. */
      FIND FIRST op WHERE
           RECID(op) EQ wop.op-recid NO-LOCK NO-ERROR.

   vHandle = BUFFER wop:HANDLE:BUFFER-FIELD (Pars-GetString ( pn )) NO-ERROR.

   IF NOT VALID-HANDLE(vHandle) THEN
      vHandle = BUFFER op:HANDLE:BUFFER-FIELD (Pars-GetString ( pn )) NO-ERROR.

   IF VALID-HANDLE(vHandle) THEN
      vResult = vHandle:BUFFER-VALUE.

/*   IF AVAILABLE op THEN
      vResult = STRING(wop:HANDLE).*/

   RUN Pars-SetCHARResult (IF vResult EQ ? THEN "?" ELSE vResult).

/* --- */
   is-ok = TRUE.
end procedure.

/* NDoc: возвращает номер документа
   Использовать: NDoc (Номер_шаблона(id-optempl),Имя_Фрейма (frame-name))
             или NDoc (Имя_Фрейма (frame-name)) - Номер шаблона не учитывается
             или NDoc () - поле имени фрейма не учитывается
*/
procedure NDoc:
    DEF OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* успешно ли выполнена ? */

/* --- */
    IF NOT (Pars-ValidParam(0) OR
            Pars-ValidParam(1) OR
            Pars-ValidParam(2))
    THEN RETURN.
/* --- */
    IF pn < 1 THEN
    find first wop where recid(wop) eq rid no-lock no-error.
    def var param-id-optempl as INT64 no-undo.
    def var param-frame-name as char no-undo.
    DEFINE VARIABLE v-str AS CHARACTER  NO-UNDO.

    param-id-optempl = IF pn < 1 THEN
       (IF AVAIL wop THEN wop.op-templ ELSE ? ) ELSE Pars-GetInt ( pn - 1 ).
    param-frame-name = if pn = -1 THEN "" ELSE Pars-GetString ( pn ).

    RUN internal-parser-getdetails-form-ttable (param-id-optempl,param-frame-name, "doc-num", OUTPUT v-str).
    if v-str eq ? then RUN internal-parser-getdetails-form-ttable (param-id-optempl,param-frame-name, "op-doc-num", OUTPUT v-str).
    RUN Pars-SetCHARResult (IF v-str EQ ? THEN "?" ELSE v-str).

/* --- */
    is-ok = TRUE.
end procedure.

procedure NDoc2:
   DEF OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* успешно ли выполнена ? */

   DEFINE VARIABLE v-str AS CHARACTER  NO-UNDO.

/* --- */
   IF NOT (Pars-ValidParam(0) OR
           Pars-ValidParam(1) OR
           Pars-ValidParam(2))
   THEN RETURN.
/* --- */

   FIND FIRST wop WHERE RECID(wop) EQ rid NO-LOCK NO-ERROR.

   IF AVAILABLE wop THEN
       /* Поиск документа. */
      FIND FIRST op WHERE
           RECID(op) EQ wop.op-recid NO-LOCK NO-ERROR.

   IF AVAILABLE op THEN
      v-str = STRING(op.doc-num).

    RUN Pars-SetCHARResult (IF v-str EQ ? THEN "?" ELSE v-str).

/* --- */
    is-ok = TRUE.
end procedure.

PROCEDURE NParamDeposit:
   DEF OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* успешно ли выполнена ? */
   DEFINE VARIABLE vIdChar AS CHARACTER NO-UNDO. /*Номер параметра*/
   DEFINE VARIABLE vContract AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE vContCode AS CHARACTER NO-UNDO. 

   IF NOT Pars-ValidParam(1) THEN RETURN.
   RUN GetLoan.
   IF NOT AVAIL loan THEN DO:
      vContCode = Get_Loan_Trans().
      IF vContCode EQ ? THEN vContCode = Get_Loan().
      vContract = Get_Loan_contr().
      FIND FIRST loan WHERE loan.contract  EQ vContract
                        AND loan.cont-code EQ vContCode NO-LOCK NO-ERROR.
   END.
   IF AVAIL loan THEN
      CASE Pars-GetString ( 0 ):
         WHEN "1" THEN RUN Pars-SetCHARResult(STRING(loan.open-date,"99/99/9999")).
         WHEN "2" THEN RUN Pars-SetCHARResult(STRING(loan.end-date,"99/99/9999")).
         WHEN "3" THEN RUN Pars-SetCHARResult(STRING(loan.close-date,"99/99/9999")).
         WHEN "4" THEN RUN Pars-SetCHARResult(loan.cont-type).
         OTHERWISE RUN Pars-SetCHARResult("?").
      END CASE.
   ELSE
      RUN Pars-SetResult (0).

   is-ok = TRUE.
END PROCEDURE.

/* Client: возвращает поле клиент введенное во  фрейме
   Использовать: Client (Номер_шаблона(id-optempl),Имя_Фрейма (frame-name))
             или Client (Имя_Фрейма (frame-name)) - Номер шаблона не учитывается
             или Client () - поле имени фрейма не учитывается
*/

procedure Client:
    DEF OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* успешно ли выполнена ? */
    DEFINE VARIABLE v-str AS CHARACTER  NO-UNDO.
/* --- */
    IF NOT (Pars-ValidParam(0) OR
            Pars-ValidParam(1) OR
            Pars-ValidParam(2))
    THEN RETURN.
/* --- */
    IF pn < 1 THEN
    find first wop where recid(wop) eq rid no-lock no-error.
    def var param-id-optempl as INT64 no-undo.
    def var param-frame-name as char no-undo.

    param-id-optempl = IF pn < 1 THEN
       (IF AVAIL wop THEN wop.op-templ ELSE ? ) ELSE Pars-GetInt ( pn - 1 ).
    param-frame-name = if pn = -1 THEN "" ELSE Pars-GetString ( pn ).

    RUN GetLoan.
    IF AVAIL loan AND loan.cust-cat EQ "Ч" THEN DO:
        find first person where person-id = loan.cust-id no-lock no-error.
        if available person then v-str = person.name-last + " " + person.first-names.
    end.

    if v-str eq ?
    then RUN internal-parser-getdetails-form-ttable (param-id-optempl,param-frame-name, "op-doc-num", OUTPUT v-str).

    RUN Pars-SetCHARResult (IF v-str EQ ? THEN "?" ELSE v-str).

/* --- */
    is-ok = TRUE.
end procedure.

/* NClDoc: возвращает номер клиентского документа
   Использовать: NClDoc (Номер_шаблона(id-optempl),Имя_Фрейма (frame-name))
             или NClDoc (Имя_Фрейма (frame-name)) - Номер шаблона не учитывается
             или NClDoc () - поле имени фрейма не учитывается
*/

procedure NClDoc:
    DEF OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* успешно ли выполнена ? */

/* --- */
    IF NOT (Pars-ValidParam(0) OR
            Pars-ValidParam(1) OR
            Pars-ValidParam(2))
    THEN RETURN.
/* --- */
    IF pn < 1 THEN
    find first wop where recid(wop) eq rid no-lock no-error.
    def var param-id-optempl as INT64 no-undo.
    def var param-frame-name as char no-undo.
     DEFINE VARIABLE v-str AS CHARACTER  NO-UNDO.

    param-id-optempl = IF pn < 1 THEN
       (IF AVAIL wop THEN wop.op-templ ELSE ? ) ELSE Pars-GetInt ( pn - 1 ).
    param-frame-name = if pn = -1 THEN "" ELSE Pars-GetString ( pn ).
    RUN GetLoan.
    if AVAIL loan AND loan.cust-cat = "Ч" then
    do:
        find first person where person-id = loan.cust-id no-lock no-error.
        if available person then v-str = person.document.
    end.

    if v-str eq ?
    then RUN internal-parser-getdetails-form-ttable (param-id-optempl,param-frame-name, "op-doc-num", OUTPUT v-str).

    RUN Pars-SetCHARResult (IF v-str EQ ? THEN "?" ELSE v-str).

/* --- */
    is-ok = TRUE.
end procedure.

/* NClDocType: возвращает тип документа
   Использовать: NClDocType (Номер_шаблона(id-optempl),Имя_Фрейма (frame-name))
             или NClDocType (Имя_Фрейма (frame-name)) - Номер шаблона не учитывается
             или NClDocType () - поле имени фрейма не учитывается
*/

procedure NClDocType:
    DEF OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* успешно ли выполнена ? */

/* --- */
    IF NOT (Pars-ValidParam(0) OR
            Pars-ValidParam(1) OR
            Pars-ValidParam(2))
    THEN RETURN.
/* --- */
    IF pn < 1 THEN
    find first wop where recid(wop) eq rid no-lock no-error.
    def var param-id-optempl as INT64 no-undo.
    def var param-frame-name as char no-undo.
     DEFINE VARIABLE v-str AS CHARACTER  NO-UNDO.

    param-id-optempl = IF pn < 1 THEN
       (IF AVAIL wop THEN wop.op-templ ELSE ? ) ELSE Pars-GetInt ( pn - 1 ).
    param-frame-name = if pn = -1 THEN "" ELSE Pars-GetString ( pn ).

    RUN GetLoan.
    if AVAIL loan AND loan.cust-cat = "Ч" then
    do:
       find first person where person-id = loan.cust-id no-lock no-error.
       if available person then v-str = person.document-id.
    end.

    if v-str eq ?
    then RUN internal-parser-getdetails-form-ttable (param-id-optempl,param-frame-name, "op-doc-num", OUTPUT v-str).

    RUN Pars-SetCHARResult (IF v-str EQ ? THEN "?" ELSE v-str).

/* --- */
    is-ok = TRUE.
end procedure.

/* NClDocIss: возвращает "выдан" для документа физического лица
   Использовать: NClDocIss(Номер_шаблона(id-optempl),Имя_Фрейма (frame-name))
             или NClDocIss(Имя_Фрейма (frame-name)) - Номер шаблона не учитывается
             или NClDocIss() - поле имени фрейма не учитывается
*/

PROCEDURE NClDocIss:
   DEF OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* успешно ли выполнена ? */

   DEF VAR clss-name AS CHAR.

/* --- */
    IF NOT (Pars-ValidParam(0) OR
            Pars-ValidParam(1) OR
            Pars-ValidParam(2))
    THEN RETURN.
/* --- */
    IF pn < 1 THEN
       FIND FIRST wop WHERE RECID(wop) EQ rid NO-LOCK NO-ERROR.
    DEF VAR v-id-optempl AS INT64  NO-UNDO.
    DEF VAR v-frame-name AS CHAR NO-UNDO.
    DEF VAR v-str        AS CHAR NO-UNDO.

    v-id-optempl = IF pn < 1 THEN
       (IF AVAIL wop THEN wop.op-templ ELSE ? ) ELSE Pars-GetInt(pn - 1).
    v-frame-name = if pn = -1 THEN "" ELSE Pars-GetString(pn).
    RUN GetLoan.
    IF     AVAIL loan
       AND loan.cust-cat EQ "Ч" THEN
    DO:
       clss-name = 'person'.
        FIND FIRST person WHERE person-id = loan.cust-id NO-LOCK NO-ERROR.
        IF AVAIL person THEN
        DO:
           v-str = person.issue.
           IF INDEX(CAPS(v-str),"ВЫДАН") EQ 0 THEN
              v-str = "выдан " + v-str.
        END.
    END.

    RUN Pars-SetCHARResult (IF v-str EQ ? THEN "?" ELSE v-str).

/* --- */
    is-ok = TRUE.
END PROCEDURE.

/* DocDate: возвращает дату документа (дата выписки расчетно-денежного документа)
   параметры:Номер_шаблона и Формат_Вывода_Даты, none или Формат_Вывода_Даты
   пример:  DocDate (Номер_шаблона(id-optempl),Формат_Вывода_Даты )
            или DocDate() или DocDate("99/99/9999")
*/
PROCEDURE DocDate:
   DEFINE OUTPUT PARAMETER is-ok AS LOGICAL NO-UNDO. /* успешно ли выполнена ? */

   DEFINE VARIABLE param-id-optempl AS INT64   NO-UNDO.
   DEFINE VARIABLE param-format     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE mChResult        AS CHARACTER NO-UNDO.
   DEFINE VARIABLE v-str            AS CHARACTER NO-UNDO.

   IF NOT (   Pars-ValidParam(0)
           OR Pars-ValidParam(1)
           OR Pars-ValidParam(2)
          )
   THEN RETURN.

   FIND FIRST wop WHERE
        RECID(wop) EQ rid NO-LOCK NO-ERROR.

   param-id-optempl = IF pn < 1
                      THEN (IF AVAIL wop
                            THEN wop.op-templ
                            ELSE ?)
                      ELSE Pars-GetInt(pn - 1).
   param-format = IF pn = -1
                  THEN "99/99/9999"
                  ELSE Pars-GetString(pn).

   IF NOT {assigned param-format} THEN
      param-format = "99/99/9999".

   RUN internal-parser-getdetails-form-ttable (param-id-optempl,
                                               "",
                                               "doc-date",
                                               OUTPUT v-str).
   IF v-str EQ ? THEN
      RUN internal-parser-getdetails-form-ttable (param-id-optempl,
                                                  "",
                                                  "op-doc-date",
                                                  OUTPUT v-str).

   IF v-str EQ ? THEN
      RUN ElseTryGetFieldFraMeVal("doc-date", OUTPUT v-str).

  IF AVAIL wop THEN
      /* Поиск документа. */
     FIND FIRST op WHERE
          RECID(op) EQ wop.op-recid NO-LOCK NO-ERROR.

  IF     NOT {assigned v-str}
     AND AVAIL op THEN
     v-str = STRING(op.doc-date).

   RUN Pars-SetCHARResult (IF v-str EQ ?
                           THEN "?"
                           ELSE STRING(DATE(v-str), param-format)).

    is-ok = TRUE.
END PROCEDURE.

PROCEDURE DocDate2:
   DEFINE OUTPUT PARAMETER is-ok AS LOGICAL NO-UNDO. /* успешно ли выполнена ? */

   DEFINE VARIABLE param-id-optempl AS INT64   NO-UNDO.
   DEFINE VARIABLE param-format     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE mChResult        AS CHARACTER NO-UNDO.
   DEFINE VARIABLE v-str            AS CHARACTER NO-UNDO.

   IF NOT (   Pars-ValidParam(0)
           OR Pars-ValidParam(1)
           OR Pars-ValidParam(2)
          )
   THEN RETURN.

   FIND FIRST wop WHERE
        RECID(wop) EQ rid NO-LOCK NO-ERROR.

   param-format = IF pn = -1
                  THEN "99/99/9999"
                  ELSE Pars-GetString(pn).

   IF NOT {assigned param-format} THEN
      param-format = "99/99/9999".

  IF AVAIL wop THEN
      /* Поиск документа. */
     FIND FIRST op WHERE
          RECID(op) EQ wop.op-recid NO-LOCK NO-ERROR.

  IF AVAILABLE op THEN
     v-str = STRING(op.doc-date).

   RUN Pars-SetCHARResult (IF v-str EQ ?
                           THEN "?"
                           ELSE STRING(DATE(v-str), param-format)).

    is-ok = TRUE.
END PROCEDURE.
/* ВалютаИСО, DocDate, Клиент2, DocCourse */

/*******************************************************************************
   Клиент2 - возвращает наименование клиента
   Использовать: Клиент2 (Имя_Фрейма (frame-name))
             или Клиент2 () - поле имени фрейма не учитывается
             или Клиент2(Номер_Шаблона из которого взять клиента)
*/
PROCEDURE Клиент2:

   DEFINE OUTPUT PARAMETER is-ok AS LOGICAL NO-UNDO. /* успешно ли выполнена ? */

   DEFINE VARIABLE param-id-optempl AS INT64   NO-UNDO.
   DEFINE VARIABLE param-frame-name AS CHARACTER NO-UNDO.
   DEFINE VARIABLE mChResult        AS CHARACTER NO-UNDO.

   IF NOT (Pars-ValidParam(0) OR Pars-ValidParam(1)) THEN
      RETURN.

   FIND FIRST wop WHERE RECID(wop) EQ rid NO-LOCK NO-ERROR.

   IF pn EQ -1 THEN
   /* параметры не переданы */
      ASSIGN /* берем из текущ. шаблона */
         param-frame-name = ""
         param-id-optempl = IF AVAILABLE wop THEN wop.op-templ ELSE ?.
   ELSE DO:
      param-frame-name = Pars-GetString(0).
      /* определяем, что передано: номер шаблона или имя фрейма */
      param-id-optempl = INT64(param-frame-name) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN
      /* передано имя фрейма */
         ASSIGN
            param-id-optempl   = ?
            ERROR-STATUS:ERROR = NO.
      ELSE
         param-frame-name = "".
   END.

   RUN internal-parser-getdetails-form-ttable (
          param-id-optempl,
          param-frame-name,
          "name-ben",
          OUTPUT mChResult
       ).

   RUN Pars-SetCHARResult(mChResult).
   is-ok = TRUE.

END PROCEDURE.


/* DocCourse - возвращает строку "Курс заявки на _дата_ _курс_ руб."
   Использовать: DocCourse (Номер_шаблона(id-optempl),Имя_Фрейма (frame-name))
             или DocCourse (Имя_Фрейма (frame-name)) - Номер шаблона не учитывается
             или DocCourse () - поле имени фрейма не учитывается
*/
procedure DocCourse:
  DEF OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* успешно ли выполнена ? */
/* --- */
  is-ok = FALSE.
  IF NOT (Pars-ValidParam(0) OR
          Pars-ValidParam(1) OR
          Pars-ValidParam(2))
  THEN RETURN.
  /*{&type-er} = "".
  IF pn <> 0 THEN DO:
    {&type-er} = {&EGMBadParamCount}.
    RETURN.
  END. ELSE pn = pn - 1.*/
/* --- */
  IF pn < 1 THEN
    find first wop where recid(wop) eq rid no-lock no-error.
  def var param-id-optempl as INT64 no-undo.
  def var param-frame-name as char no-undo.
  param-id-optempl = IF pn < 1 THEN
     (IF AVAIL wop THEN wop.op-templ ELSE ? ) ELSE Pars-GetInt ( pn - 1 ).
  param-frame-name = if pn = -1 THEN "" ELSE Pars-GetString ( pn ).

  /*param-id-optempl = IF pn < 0 THEN wop.op-templ ELSE Pars-GetInt ( pn ).
  if pn = -1
  then param-frame-name = "".
  else param-frame-name = if mvar[pj - pn] eq "" then string(result_l[pj - pn]) else TRIM(mvar[pj - pn], """").
  else param-frame-name = if Pars-GetStr eq "" then string(Pars-GetString ( pn )) else TRIM(Pars-GetString ( pn ), """").
                                                           */

  DEFINE VARIABLE vS AS CHARACTER  NO-UNDO.
  def var l-date    as date no-undo.
  def var l-course  as dec  no-undo.
  def var l-amt-cur as dec  no-undo.
  def var l-amt-rub as dec  no-undo.

  RUN internal-parser-getdetails-form-ttable (param-id-optempl,param-frame-name, "doc-date", OUTPUT vS).
  l-date = date(vS).
  if l-date eq ?
  then DO:
      RUN internal-parser-getdetails-form-ttable (param-id-optempl,param-frame-name, "op-doc-date", OUTPUT vS).
      l-date = date(vS).
  END.
  RUN internal-parser-getdetails-form-ttable (param-id-optempl,param-frame-name, "doc-course", OUTPUT vS).
  l-course = dec(vS).
  if l-course eq ? then do:
      RUN internal-parser-getdetails-form-ttable (param-id-optempl,param-frame-name, "amt-rub", OUTPUT vS).
      l-amt-rub = dec(vS).
      if l-amt-rub eq ?
      then DO:
          RUN internal-parser-getdetails-form-ttable (param-id-optempl,param-frame-name, "op-amt-rub", OUTPUT vS).
          l-amt-rub = dec(vS).
      END.
      RUN internal-parser-getdetails-form-ttable (param-id-optempl,param-frame-name, "amt-cur", OUTPUT vS).
      l-amt-cur = dec(vS).
      if l-amt-cur eq ?
      then DO:
          RUN internal-parser-getdetails-form-ttable (param-id-optempl,param-frame-name, "op-amt-cur", OUTPUT vS).
          l-amt-cur = dec(vS).
      END.
      l-course = l-amt-rub / l-amt-cur.
  end.

  RUN Pars-SetCHARResult ( "Курс на " +
                           (if l-date eq ? then "?" else string(l-date, "99/99/9999")) + ": " +
                           (if l-course eq ? then "?" else trim(string(l-course, "zzz,zzz,zzz,zzz,zzz,zzz,zz9.999999")))
                         ).
/* --- */
  is-ok = TRUE.
/* *** */
end procedure.


/* ВалИСО - возвращает код валюты по ИСО
   параметры: код валюты
   пример: ВалИСО("840") - возврат: USD
           ВалИСО("040") - возврат: ATS
*/
procedure ВалИСО.
  DEF OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* успешно ли выполнена ? */

  /* --- */
  is-ok = FALSE.
  {&type-er} = "".
  IF pn <> 1 THEN DO:
    {&type-er} = {&EGMBadParamCount}.
    RETURN.
  END. ELSE pn = pn - 1.
/* --- */
  def var curr-iso as char no-undo.
  curr-iso = Pars-GetString ( 0 ).
  {curr-iso.i &curr=curr-iso}

  RUN Pars-SetCHARResult ( curr-iso ).

/* --- */
  is-ok = TRUE.
end procedure.


/*-----------------------------------------------------------------------------
    * Что делает: возвращает значение ставки резервирования по договору за дату
    * Синтаксис : КоэфРез()
    * Автор     : amam  01/04/05
    * Пример    : КоэфРез() = 3.5
  -----------------------------------------------------------------------------*/
PROCEDURE КоэфРез:

   DEF OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* успешно ли выполнена ? */

   DEF VAR ph AS HANDLE NO-UNDO.
   DEF VAR vRate AS DEC NO-UNDO.

   is-ok = FALSE.

   IF NOT Pars-ValidParam(0) THEN RETURN.

   /* Определение правильного применения функции,
   ** формат private-data */
   RUN LOAN_VALID_HANDLE(input-output ph).

   IF not valid-handle(ph) THEN RETURN.

   FIND FIRST loan WHERE
       loan.contract  = ENTRY(1, ph:private-data) and
       loan.cont-code = ENTRY(2, ph:private-data)
   NO-LOCK NO-ERROR.
   IF NOT AVAIL loan THEN RETURN.

   vRate = LnRsrvRate(loan.contract, loan.cont-code, in-op-date).

   ASSIGN
       mvar[pj - pn]     = STRING(vRate)
       result_l[pj - pn] = vRate
       is-ok             = TRUE
       .

   RETURN.

END PROCEDURE.


/*
    * Что делает: Возвращает дату откурытия договора.
                  Если передан 1 параметр "Соглаш" то возвращается дата вышестоящего договора
    * Синтаксис : ДатаОтк(["Соглаш"])
    * Автор     : Om  16/10/00
    * Пример    : ДатаОтк(), ДатаОтк("Соглаш")
*/
PROCEDURE ДатаОтк:

    DEF OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* успешно ли выполнена ? */
    def var ph as handle no-undo.

    is-ok = FALSE.

    IF NOT (Pars-ValidParam(0) OR Pars-ValidParam(1)) THEN RETURN.

    /* Определение правильного применения функции,
    ** формат private-data */
    run LOAN_VALID_HANDLE (input-output ph).

    if not valid-handle (ph)
    then return.

    IF pn EQ 0 AND TRIM(Pars-GetString(0), """'") EQ "Соглаш" THEN DO: /* если 1 параметр и он равен "Соглаш" */
        FIND FIRST loan WHERE
            loan.contract  EQ ENTRY(1, ph:private-data) AND
            loan.cont-code EQ SUBSTR(ENTRY(2, ph:private-data), 1, R-INDEX(ENTRY(2, ph:private-data), " ") - 1)
            NO-LOCK NO-ERROR.
    END.
    ELSE DO:
        find first loan where
            loan.contract  eq entry(1, ph:private-data) and
            loan.cont-code eq entry(2, ph:private-data)
            no-lock no-error.
    END.

    if not avail loan then return.

    RUN Pars-SetCHARResult (string(loan.open-date, "99/99/9999")).

    is-ok = TRUE.

END PROCEDURE.

/*
    * Что делает: Возвращает наименование клиента.
    * Синтаксис : Клнт
    * Автор     : Om  16/10/00
    * Пример    : Клнт
*/

PROCEDURE Клнт:

    DEF OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* успешно ли выполнена ? */
    DEF VAR ph    AS HANDLE    NO-UNDO.
    DEF VAR name1 AS CHARACTER NO-UNDO.
    DEF VAR name2 AS CHARACTER NO-UNDO.
    DEF VAR Inn   AS CHARACTER NO-UNDO.
    is-ok = FALSE.

    IF NOT Pars-ValidParam(0) THEN RETURN.
    /* Определение правильного применения функции,
    ** формат private-data */
    run LOAN_VALID_HANDLE (input-output ph).
    if not valid-handle (ph)
    then return.

    find first loan where
        loan.contract  eq entry(1, ph:private-data) and
        loan.cont-code eq entry(2, ph:private-data)
    no-lock no-error.
    if not avail loan then return.

    RUN GetCustName IN h_base (loan.cust-cat,
                               loan.cust-id,
                               "",
                               OUTPUT name1,
                               OUTPUT name2,
                               INPUT-OUTPUT Inn).
   IF name1 + name2 EQ "" 
      AND loan.cust-cat EQ "Ч" THEN
   DO:
      FIND FIRST person WHERE 
         person.person-id EQ loan.cust-id 
      NO-LOCK NO-ERROR.
      IF AVAIL(person) THEN 
         ASSIGN
            name1 = person.name-last
            name2 = person.first-names.
   END.
   IF name1 + name2 EQ "" 
      AND loan.cust-cat EQ "Ю" THEN
   DO:
      FIND FIRST cust-corp WHERE 
         cust-corp.cust-id EQ loan.cust-id 
      NO-LOCK NO-ERROR.
      IF AVAIL(cust-corp) THEN 
         ASSIGN
            name1 = cust-corp.cust-stat
            name2 = cust-corp.name-corp.
   END.

    IF name1 + name2 NE ""
    THEN DO:
      RUN  Pars-SetCHARResult(  name1 + " "
                              + name2 +
                                IF name2 NE "" THEN
                                   (IF    SUBSTRING (name2,LENGTH(name2)) EQ '"'
                                       OR SUBSTRING (name2,LENGTH(name2)) EQ "'"
                                    THEN " "
                                    ELSE "")
                                ELSE "").

      ASSIGN
         result_l[pj - pn] = 0
         is-ok = TRUE
      .
    END.
    RETURN.
END PROCEDURE.

/*
    * Что делает: Возвращает дату (beg-date/end-date) периода (OP_flt.p).
    * Синтаксис : ДатаП ("C/ПО")
    * Автор     : Om 11/03/2001
    * Пример    : ДатаП ("C")
*/
PROCEDURE ДатаП.

    DEF OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* успешно ли выполнена ? */

    define var vSideDate as char no-undo. /* Определяет дату */

    is-ok = FALSE.

    IF NOT Pars-ValidParam(1)
        THEN RETURN.

    vSideDate = if mvar[pj - pn] eq ""
                        then string (result_l[pj - pn])
                        else trim(mvar[pj - pn], """'").

    ASSIGN
        mvar [pj - pn] = string (if vSideDate eq "С"
                                    then beg-date
                                    else end-date, "99/99/9999")
    .

    is-ok = TRUE.

END PROCEDURE.

/*
**  Что делает: Возвращает плановую дату документа.
**  Синтаксис : ПДата ()
**  Автор     : Om 15/11/2001
**  Пример    : ПДата ()
*/
PROCEDURE ПДата.

   DEFINE OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* успешно ли выполнена ? */

   RUN DateDoc ( "П", rid, output is-ok).

   RETURN.

END PROCEDURE.

/*
**  Что делает: Возвращает конец месяца относительно плановой даты документа.
**  Синтаксис : ПДатаКМ ()
**  Автор     : SAP 28/01/2004
**  Пример    : ПДата ()
*/
PROCEDURE ПДатаКМ.
    DEF OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* успешно ли выполнена ? */
    DEF VAR mTmp-date AS DATE NO-UNDO.

    IF NOT Pars-ValidParam(0) THEN RETURN.

    /* Поиск создаваемого документа через временную таблицу */
    find first wop where
        recid(wop) eq rid
    no-lock no-error.

    /* Поиск созданной проводки */
    find first op-entry where
        recid(op-entry) eq rid
    no-lock no-error.
    if avail op-entry
    /* Поиск создаваемого документа */
    then find first op of op-entry
    no-lock no-error.
    /* Тащу дату из документа если он существует,
    ** если нет тащу из временной таблици wop,
    ** иначе "" */
    ASSIGN
        mvar [pj - pn] = string (if avail op
                                    then op.contract-date
                                    else if avail wop
                                        then wop.con-date
                                        else ?, "99/99/9999")
        mvar [pj - pn] = if mvar [pj - pn] eq ?
                         then ""
                         else mvar [pj - pn].

    mTmp-date = DATE(mvar [pj - pn]).
    mvar [pj - pn] = STRING(DATE(IF MONTH(mTmp-date) = 12
                             THEN 1
                             ELSE MONTH(mTmp-date) + 1,
                            1,
                            IF MONTH(mTmp-date) = 12 THEN
                               YEAR(mTmp-date) + 1
                            ELSE YEAR(mTmp-date)) - 1, "99/99/9999" ) .
    is-ok = TRUE.
    RETURN.

END PROCEDURE.


/*
    * Что делает: Возвращает название счета.
    * Синтаксис : НаимСчета ( счет [, валюта ])
    * Автор     : Sema, 07/09/01
    * Пример    : НаимСчета ( Дб(1) )
*/
PROCEDURE НаимСчета.

    DEF OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* успешно ли выполнена ? */

    /* --- */
    is-ok = FALSE.
    {&type-er} = "".
    IF NOT (Pars-ValidParam(1) OR Pars-ValidParam(2)) THEN RETURN.

  /* --- */

    DEFINE VARIABLE vAcct AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE vCurrency AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE vName AS CHARACTER EXTENT 2 NO-UNDO.

    vAcct = Pars-GetString (0).

    IF pn EQ 0 THEN DO:
        {find-act.i
           &bact = acct
           &acct = vAcct
        }
    END.
    ELSE DO:
        vCurrency = Pars-GetStringFormatted (1, "999").

        IF vCurrency EQ FGetSetting("КодНацВал",?,"{&in-NC-Code}") THEN
        vCurrency = "".
        {find-act.i
           &bact = acct
           &acct = vAcct
           &curr = vCurrency
        }
    END.
    IF NOT AVAIL acct THEN DO:
        MESSAGE "Счет" """" + vAcct + (IF vCurrency NE ? THEN "/" + vCurrency ELSE "") + """" "переданный в качестве параметра в функцию НаимСчета не найден!"
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        RETURN.
    END.


       RUN getcust0 in h_base (buffer acct,
                                no, yes,
                                output vName[1], output vName[2]).

        RUN Pars-SetCHARResult ( IF TRIM(TRIM(vName[1]) + " " + TRIM(vName[2])) NE "" THEN
                                    " " + TRIM(TRIM(vName[1]) + " " + TRIM(vName[2])) + " "
                                 ELSE
                                    ""
                               ).



    is-ok = TRUE.

END PROCEDURE.


/*
** Что делает: Возвращает информацию по физлицу, определенному в счете.
               Если вместо счета передать константу "Клиент",
               то источником информации будет клиента, а не счет.
** Синтаксис : ФизЛицо (Тип_информации,Счет[,Валюта])
** Автор     : Sema 28/02/02
** Пример    : ФизЛицо (1, Дб(1)), ФизЛицо (1, Дб(1), Вал(1))
*/
PROCEDURE ФизЛицо.

   DEF OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* успешно ли выполнена ? */
   DEFINE VARIABLE vAcct     AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vCurrency AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vResult   AS CHARACTER  NO-UNDO INIT ?.
   DEFINE VARIABLE vCustCat  AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vCustId   AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE mCustId   AS INT64      NO-UNDO.

   DEF BUFFER person FOR person.

   IF NOT (Pars-ValidParam(2) OR Pars-ValidParam(3)) THEN RETURN.
   ASSIGN
      vAcct     = Pars-GetString ( 1 )
      vCurrency = IF pn EQ 2 THEN Pars-GetStringFormatted ( 2, "999" ) ELSE ?
   .   
                        /* Для получения информации от клиента.
                        ** Используется в коммунальных платежах. */
   IF vAcct EQ "Клиент" THEN
   CASE GetSysConf("tmp-cust-cat"):
      WHEN "Ч" THEN
      DO:
         mCustId = INT64 (GetSysConf ("tmp-person-id")) NO-ERROR.
         FIND FIRST person WHERE
            person.person-id  EQ mCustId
         NO-LOCK NO-ERROR.

         IF AVAIL person THEN
            RUN GetCustInfo IN h_cust (Pars-GetInt (0), "Ч", mCustId, OUTPUT vResult) NO-ERROR.
         ELSE
            RUN Fill-AlertSysMes IN h_tmess (
               "", "", "0",
               "Не найден клиент ФЛ с идентификатором " + QUOTER (GetSysConf ("tmp-person-id")) + "."
            ).
      END.
      OTHERWISE
         RUN Fill-AlertSysMes IN h_tmess (
            "", "", "0",
            "Передан ненверный тип клиента " + QUOTER (GetSysConf ("tmp-cust-cat")) + "."
         ).
   END CASE.

   ELSE DO:
      IF vCurrency NE ? THEN DO:
         {find-act.i
             &bact = acct
             &acct = vAcct
             &curr = vCurrency
         }
      END.
      ELSE DO:
         {find-act.i
             &bact = acct
             &acct = vAcct
         }
      END.
      IF AVAIL acct THEN
         IF acct.cust-cat EQ "Ч" THEN
            RUN GetCustInfo2 IN h_cust (Pars-GetInt ( 0 ),
                                        acct.acct,
                                        acct.currency,
                                        OUTPUT vResult).
         ELSE
            vResult = ?.
      ELSE
         MESSAGE "Счет" """" + vAcct +  """" "переданный в качестве параметра в функцию ФизЛицо не найден!"
         VIEW-AS ALERT-BOX ERROR BUTTONS OK.
   END.
   RUN Pars-SetCHARResult (vResult).

   is-ok = TRUE.

END PROCEDURE.

/*
    * Что делает: Возвращает комиссию по договору
    * Синтаксис : КомДог("<Код Комиссии>")
    * Автор     : Илюха  29/10/2002
    * Пример    : КомДог("%Кред")
*/

PROCEDURE КомДог.

    DEFINE OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* успешно ли выполнена ? */

    DEF VAR vCommName AS CHAR   NO-UNDO.
    DEF VAR h_templ   AS HANDLE NO-UNDO.

    is-ok = NO.

    IF NOT Pars-ValidParam(1)
    THEN RETURN.

    vCommName = Pars-GetString(0).

    /* Определение правильного применения функции, формат private-data */
    RUN LOAN_VALID_HANDLE (INPUT-OUTPUT h_templ).

    IF NOT VALID-HANDLE(h_templ)
    THEN RETURN .

    /*поиск договрра*/
    RUN RE_B_LOAN IN h_loan (ENTRY(1,h_templ:PRIVATE-DATA),
                   ENTRY(2,h_templ:PRIVATE-DATA),
                   BUFFER loan).
    IF NOT AVAIL loan
    THEN RETURN.

    /*условие*/
    RUN RE_L_COND IN h_loan  (loan.contract,
                   loan.cont-code,
                   in-op-date,
                   BUFFER loan-cond).
    IF NOT AVAIL loan-cond
    THEN RETURN.

    FIND LAST comm-rate WHERE
              comm-rate.commi    EQ vCommName
          AND comm-rate.kau      EQ loan-cond.contract + "," + loan-cond.cont-code
          AND comm-rate.currency EQ loan.currency
          AND comm-rate.acct     EQ "0"
          AND comm-rate.since    LE in-op-date  NO-LOCK NO-ERROR.

    IF AVAIL comm-rate
    THEN RUN Pars-SetResult (comm-rate.rate-comm).

    is-ok = TRUE.

END PROCEDURE.

/*
    * Что делает: Возвращает комиссию по договору соглашению, если передан транш
    * Синтаксис : КомДогСогл("<Код Комиссии>")
    * Автор     : Илюха  29/10/2002
    * Пример    : КомДогСогл("%Кред")
*/

PROCEDURE КомДогСогл.

    DEFINE OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* успешно ли выполнена ? */

    DEF VAR vCommName AS CHAR   NO-UNDO.
    DEF VAR h_templ   AS HANDLE NO-UNDO.

    DEF BUFFER xloan FOR loan. /* Локализация буффера. */

    is-ok = NO.

    IF NOT Pars-ValidParam(1)
    THEN RETURN.

    vCommName = Pars-GetString(0).

    /* Определение правильного применения функции, формат private-data */
    RUN LOAN_VALID_HANDLE (INPUT-OUTPUT h_templ).

    IF NOT VALID-HANDLE(h_templ) THEN
       RETURN.

    /*поиск договрра*/
    RUN RE_B_LOAN IN h_loan (ENTRY(1,h_templ:PRIVATE-DATA),
                   ENTRY(2,h_templ:PRIVATE-DATA),
                   BUFFER xloan).
    IF NOT AVAIL xloan THEN
       RETURN.

    FIND FIRST loan WHERE loan.contract  EQ xloan.contract
                      AND loan.cont-code EQ ENTRY(1,xloan.cont-code," ")
    NO-LOCK NO-ERROR.

    IF NOT AVAIL loan THEN
       RETURN.

    /*условие*/
    RUN RE_L_COND IN h_loan  (loan.contract,
                              loan.cont-code,
                              in-op-date,
                              BUFFER loan-cond).
    IF NOT AVAIL loan-cond
    THEN RETURN.

    FIND LAST comm-rate WHERE
              comm-rate.commi    EQ vCommName
          AND comm-rate.kau      EQ loan-cond.contract + "," + loan-cond.cont-code
          AND comm-rate.currency EQ loan.currency
          AND comm-rate.acct     EQ "0"
          AND comm-rate.since    LE in-op-date  NO-LOCK NO-ERROR.

    IF AVAIL comm-rate
    THEN RUN Pars-SetResult (comm-rate.rate-comm).

    is-ok = TRUE.

END PROCEDURE.

/*
    * Что делает: Возвращает дату  календарного дня, следующего за датой
    * хронологически  последней операции с кодом,  принадлежащим
    * некоторому перечню,  переданному функции  в качестве  параметра.
    * Перечень кодов операций перечисляется через ";".
    * Синтаксис : ПослНач("<Перечень_кодов_операций>"[,"<формат_вывода_даты>"])
    * Автор     : Илюха  29/10/2002
    * Модифицировал: Серега 24/10/2003 (pesv)
    * Примеры   : ПослНач("10;15","99/99/9999"), ПослНач("10","99/99/9999"), ПослНач("10;15"), ПослНач("10")
*/
PROCEDURE ПослНач.

    DEFINE OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* успешно ли выполнена ? */

    DEF VAR d-date       AS DATE   NO-UNDO.
    DEF VAR ListParam    AS CHAR   NO-UNDO.
    DEF VAR h_templ      AS HANDLE NO-UNDO.
    DEF VAR param-format AS CHAR   NO-UNDO.

    is-ok = NO.

    IF NOT (Pars-ValidParam(1) OR
            Pars-ValidParam(2))
    THEN RETURN.

    param-format = IF pn = 0
                   THEN "99/99/9999"
                   ELSE Pars-GetString(1).

    ListParam    = Pars-GetString(0).

    /* Определение правильного применения функции, формат private-data */
    RUN LOAN_VALID_HANDLE (INPUT-OUTPUT h_templ).

    IF NOT VALID-HANDLE(h_templ)
    THEN RETURN .

    /*поиск договора*/
    RUN RE_B_LOAN IN h_loan (ENTRY(1,h_templ:PRIVATE-DATA),
                   ENTRY(2,h_templ:PRIVATE-DATA),
                   BUFFER loan).
    IF NOT AVAIL loan
    THEN RETURN.

    FOR EACH loan-int OF loan
       WHERE loan-int.mdate < in-op-date
    NO-LOCK
    BY loan-int.mdate DESC
    :
       FIND FIRST chowhe WHERE
              chowhe.id-d EQ loan-int.id-d
          AND chowhe.id-k EQ loan-int.id-k
       NO-LOCK.
       IF LOOKUP(STRING(chowhe.id-op), ListParam, ";") <> 0 THEN
          LEAVE.
    END.

    d-date  = IF AVAILABLE loan-int
              THEN (loan-int.mdate + 1)
              ELSE loan.open-date.

    RUN Pars-SetCharResult (STRING(d-date,param-format)).

    is-ok = TRUE.

END PROCEDURE.

/*
    * Что делает: Возвращает данные о переданной дате  (номер месяца,
                  название месяца, год)
    * Синтаксис : ДатаИнфо("<дата>","МесНом"|"МесНаз"|"Год"|"МесНазР")
    * Автор     : Илюха  05/11/2002
    * Пример    : ДатаИнфо(ПДата(),"МесНом") - номер месяца плановой даты
                  ДатаИнфо(ПДата(),"МесНаз") - назв. месяца плановой даты
                  ДатаИнфо(ПДата(),"Год")    - год плановой даты
*/

PROCEDURE ДатаИнфо:

    DEFINE OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* успешно ли выполнена ? */

    DEF VAR vDate   AS DATE NO-UNDO.
    DEF VAR vType   AS CHAR NO-UNDO.
    DEF VAR vMonths AS CHAR NO-UNDO
    INIT "январь,февраль,март,апрель,май,июнь,июль,август,сентябрь,октябрь,ноябрь,декабрь".
    DEF VAR vMonthsR AS CHAR NO-UNDO
    INIT "января,февраля,марта,апреля,мая,июня,июля,августа,сентября,октября,ноября,декабря".

    is-ok = NO.

    IF NOT Pars-ValidParam(2)
    THEN RETURN.

    ASSIGN
      vDate = DATE(Pars-GetString(0))
      vType = Pars-GetString(1)
    NO-ERROR.

    IF ERROR-STATUS:ERROR
    THEN RETURN.

    CASE vType:
      WHEN "МесНом" THEN RUN Pars-SetCharResult (STRING(MONTH(vDate),"99")).
      WHEN "МесНаз" THEN RUN Pars-SetCharResult (ENTRY(MONTH(vDate),vMonths)).
      WHEN "МесНазР" THEN RUN Pars-SetCharResult (ENTRY(MONTH(vDate),vMonthsR)).
      WHEN "Год"    THEN RUN Pars-SetCharResult (STRING(YEAR(vDate))).
      OTHERWISE RETURN.
    END CASE.

    is-ok = TRUE.

END.

/* Ищет введенное значение поля in-field-name в текушем фрейме, использовать после
процедуры internal-parser-getdetails-form-ttable, которая  возвратит значение если
по полю уже "пробегали" */

PROCEDURE ElseTryGetFieldFraMeVal :
   DEFINE INPUT  PARAMETER in-field-name AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER opResult AS CHARACTER  NO-UNDO.

   DEF VAR v-wh AS WIDGET-HANDLE NO-UNDO.
   opResult = "".
   v-wh = SELF:HANDLE NO-ERROR.
   IF NOT VALID-HANDLE(v-wh) THEN RETURN.
   IF v-wh:TYPE NE "FRAME" THEN
   DO:
      v-wh = v-wh:FRAME NO-ERROR.
      IF ERROR-STATUS:ERROR THEN
         RETURN.
   END.
   v-wh = v-wh:FIRST-CHILD NO-ERROR.
   v-wh = v-wh:FIRST-CHILD NO-ERROR.
   DO WHILE VALID-HANDLE(v-wh):
      IF v-wh:NAME EQ in-field-name THEN
      DO:
         opResult = v-wh:SCREEN-VALUE.
         LEAVE.
      END.
      v-wh = v-wh:NEXT-SIBLING.
   END.

END PROCEDURE.

/*
    * Что делает: Показывает браузер с паспортами сделок, актуальных для данной
                  операции и возвращает выбранный по Ctrl-Enter номер паспорта
    * Синтаксис : ПаспортСделки()
    * Пример    : ПаспортСделки()
*/

PROCEDURE ПаспортСделки:

   DEFINE OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* успешно ли выполнена ? */

   DEFINE BUFFER op FOR op.
   DEFINE BUFFER op-entry FOR op-entry.

   DEFINE VARIABLE vCat   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vIdLst AS CHARACTER NO-UNDO.

   DEFINE VARIABLE vDb  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vCr  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vCur AS CHARACTER NO-UNDO.

   IF NOT Pars-ValidParam(0) THEN
      RETURN.

   FOR FIRST wop      WHERE RECID(wop)   EQ rid   NO-LOCK
   :
      vDb = wop.acct-db.
      vCr = wop.acct-cr.
      vCur = wop.currency.
   END.

   IF NOT AVAILABLE wop THEN DO:
      FOR FIRST op       WHERE RECID(op)   EQ rid   NO-LOCK,
          FIRST op-entry WHERE op-entry.op EQ op.op NO-LOCK
      :
         vDb = op-entry.acct-db.
         vCr = op-entry.acct-cr.
         vCur = op-entry.currency.
      END.
   END.

   /* запоминаем ИД клиента по дебету */
   FOR FIRST acct WHERE acct.acct     EQ vDb
                    AND acct.currency EQ vCur
                    AND CAN-DO("Ю,Ч", acct.cust-cat)
       NO-LOCK:

      /* не можем фильтровать клиентов разных категорий */
      IF vCat EQ "" THEN
         vCat = acct.cust-cat.
      ELSE IF vCat NE acct.cust-cat THEN DO:
         LEAVE.
      END.

      IF vCat EQ "Ю" AND
         CAN-FIND(FIRST cust-corp WHERE cust-corp.cust-id EQ acct.cust-id
                                    AND (cust-corp.country-id EQ "RUS" OR
                                         NOT {assigned cust-corp.country-id}))
      THEN
         vIdLst = vIdLst + "," + STRING(acct.cust-id).

      IF vCat EQ "Ч" AND
         CAN-FIND(FIRST person WHERE person.person-id   EQ acct.cust-id
                                 AND (person.country-id EQ "RUS" OR
                                      NOT {assigned person.country-id}))
      THEN
         vIdLst = vIdLst + "," + STRING(acct.cust-id).
   END.

   /* запоминаем ИД клиента по кредиту */
   FOR FIRST acct WHERE acct.acct     EQ vCr
                    AND acct.currency EQ vCur
                    AND CAN-DO("Ю,Ч", acct.cust-cat)
       NO-LOCK:

      /* не можем фильтровать клиентов разных категорий */
      IF vCat EQ "" THEN
         vCat = acct.cust-cat.
      ELSE IF vCat NE acct.cust-cat THEN DO:
         LEAVE.
      END.

      IF vCat EQ "Ю" AND
         CAN-FIND(FIRST cust-corp WHERE cust-corp.cust-id EQ acct.cust-id
                                    AND (cust-corp.country-id EQ "RUS" OR
                                         NOT {assigned cust-corp.country-id}))
      THEN
         vIdLst = vIdLst + "," + STRING(acct.cust-id).

      IF vCat EQ "Ч" AND
         CAN-FIND(FIRST person WHERE person.person-id   EQ acct.cust-id
                                 AND (person.country-id EQ "RUS" OR
                                      NOT {assigned person.country-id}))
      THEN
         vIdLst = vIdLst + "," + STRING(acct.cust-id).
   END.

   vIdLst = TRIM(vIdLst, ",").

   pick-value = ?.

   RUN browseld("PS",
                "contract~001cust-cat~001cust-id",
                "ПСКОНТ,ПСКРЕД~001" + vCat + "~001" + vIdLst,
                "contract",
                2
   ).

   IF KEYFUNCTION(LASTKEY) NE "END-ERROR" THEN DO:
      /* в первом ENTRY хранится тип паспорта, во втором - номер */
      RUN Pars-SetCharResult(ENTRY(2, pick-value)).
   END.

   is-ok = TRUE.
END.

/*
   Что делает: Возвращает значение ДР ДогОткрЛС в нужном формате:
                  "1", то значение имеет вид: "N <ХХХХХХХХХ> от <ДД/ММ/ГГГГ>".
                  "2", то значение имеет вид: <ДД/ММ/ГГГГ>
                  "3", то значение имеет вид: <ХХХХХХХХХ>
                  по умолчанию "1".
   Синтаксис : ДогОткрЛС(счет,валюта[,формат])
*/
PROCEDURE ДогОткрЛС:
    DEFINE OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* успешно ли выполнена ? */

    DEFINE VARIABLE vResult AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE vAcct   AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE vCurr   AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE vTmp    AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE vFormat AS INT64     NO-UNDO.

    IF NOT (pn EQ 2 OR pn EQ 3) THEN
      RETURN.
    ASSIGN
      pn      = pn - 1
      vAcct   = Pars-GetString(0)
      vCurr   = Pars-GetString(1)
      vCurr   = IF vCurr EQ "{&in-NC-Code}"
                THEN ""
                ELSE vCurr
      vFormat = IF pn EQ 2
                THEN Pars-GetInt(2)
                ELSE 1
    .

   {find-act.i &acct=vAcct
               &curr=vCurr}

    IF AVAIL(acct) THEN
       vTmp = GetXattrValue("acct",Acct.acct + "," + acct.currency,"ДогОткрЛС").

    IF vTmp NE "" THEN
       CASE vFormat:
          WHEN 1 THEN
             vResult = "N " + (IF NUM-ENTRIES(vTmp) EQ 2 THEN
                                 ENTRY(2,vTmp)
                               ELSE "") +
                       " от " + ENTRY(1,vTmp).
          WHEN 2 THEN
             vResult = ENTRY(1,vTmp).
          WHEN 3 THEN
             vResult = IF NUM-ENTRIES(vTmp) EQ 2 THEN
                          ENTRY(2,vTmp)
                       ELSE "".
          OTHERWISE
              vResult = "N " + (IF NUM-ENTRIES(vTmp) EQ 2 THEN
                                  ENTRY(2,vTmp)
                                ELSE "") +
                        " от " + ENTRY(1,vTmp).
       END CASE.
    ELSE
       vResult = "".

    RUN Pars-SetCHARResult(vResult).

    is-ok = TRUE.

END PROCEDURE.

/*
  Что делает: возвращает наименования кассовых символов из документа
  Синтаксис : НаимКс()
*/
PROCEDURE НаимКс:
  DEF OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* успешно ли выполнена ? */

  DEFINE VARIABLE i       AS INT64     NO-UNDO.
  DEFINE VARIABLE vResult AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE vKSList AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE vKS     AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE vStrTMP AS CHARACTER   NO-UNDO.

  IF {assigned naimks} THEN
     DO i = 1 TO NUM-ENTRIES(naimks):
        vKS = ENTRY(i,naimks).
        /* если уже обрабатывали КС, то пропускаем */
        IF LOOKUP(vKS,vKSList) EQ 0 THEN DO:
           vStrTMP = getTCodeFld("name", "КасСимволы", vKS, gend-date).

           IF vStrTMP NE ? AND vStrTMP NE "" THEN
              vResult = vResult + vStrTMP + "~n".
           /* добавляем как обработанный КС*/
           {additem.i vKSList vKS}
        END.
     END.
  ELSE
     RETURN.

  RUN Pars-SetCHARResult(vResult).

  is-ok  = TRUE.

END PROCEDURE.

/* Что делает: Вычисляет значение даты в формате дд/мм/гггг                */
/*             при необходимости сдвигает дату на требуемое кол-во опердней назад*/
/*     Формат: date(Строка[,кол-во дней]) */
/*  Синтаксис: date("")            - текущая дата                          */
/*             date("ОпДень")      - дата опердня                          */
/*             date("НачПериода")  - начало периода для op_flt && nach2flt */
/*             date("КонПериода")  - конец  периода для op_flt && nach2flt */
/*             date("ГНачПериода") - начало глобального периода            */
/*             date("ГКонПериода") - конец  глобального периода            */
/*             date("ДатаСтрока")  - преобразование строки в дату          */

PROCEDURE DATE:
   DEFINE OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* успешно ли выполнена ? */

   DEFINE VARIABLE vDateStr  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vTmpDate  AS DATE      NO-UNDO.
   DEFINE VARIABLE vQntDay   AS INT64   NO-UNDO.
   /* Разбор входных параметров. */
   IF NOT (   Pars-ValidParam(1)
           OR Pars-ValidParam(2)) THEN DO:
      RETURN.
   END.

   ASSIGN
      vDateStr = TRIM(Pars-GetString(0),"""")
      vQntDay  = Pars-GetInt(1) WHEN pn EQ 1
   .
   CASE vDateStr:
      WHEN ""            THEN
         vTmpDate = TODAY.
      WHEN "ОпДень"      THEN DO:
        IF in-op-date = ? THEN
           in-op-date  = gend-date.
         vTmpDate = in-op-date.
      END.
      WHEN "НачПериода"  THEN
         vTmpDate = beg-date.
      WHEN "КонПериода"  THEN
         vTmpDate = end-date.
      WHEN "ГНачПериода" THEN
         vTmpDate = gbeg-date.
      WHEN "ГКонПериода" THEN
         vTmpDate = gend-date.
      OTHERWISE DO:
         vTmpDate = DATE(vDateStr) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN
         DO:
            MESSAGE "В функции <date> задана неверная дата"
               VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            RETURN.
         END.
      END.
   END CASE.

   IF vQntDay NE 0 THEN
      vTmpDate = AfterOpDays(vTmpDate,-1 * vQntDay).

   ASSIGN
      result_l[pj - pn] = INT64(vTmpDate)
      mvar[pj - pn]     = STRING(vTmpDate, "99/99/9999")
   .
   is-ok = YES.
END PROCEDURE.


/* Что делает: Возвращает начало года для переданной даты                  */
/*     Формат: НачГод(дата)                                                */
/*  Синтаксис: НачГод(ПДата())     - текущая дата                          */
PROCEDURE НачГод:
   DEFINE OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* успешно ли выполнена ? */

   DEF VAR vBegDate  AS DATE NO-UNDO.
   DEF VAR vDate     AS DATE NO-UNDO.

   MAIN:
   DO ON ERROR  UNDO, LEAVE
      ON ENDKEY UNDO, LEAVE:
      /* Разбор входных параметров. */
      IF NOT (   Pars-ValidParam(0)
              OR Pars-ValidParam(1)) THEN
         LEAVE MAIN.

      IF pn EQ -1 THEN
         vBegDate = in-op-date.
      ELSE
         vBegDate = DATE(Pars-GetString(pn)).

      vDate = FirstYearDate (vBegDate).
      RUN Pars-SetCHARResult (STRING(vDate)).
      is-ok = YES.
   END.
END PROCEDURE.


/* Что делает: Возвращает начало месяца, квартала или полугодия            */
/*             относительно даты опер-дня                                  */
/*     Формат: НачПериода(тип[,дата])                                      */
/*  Синтаксис: НачПериода("М"[,дата])  - начало месяца                     */
/*             НачПериода("К"[,дата])  - начало квартала                   */
/*             НачПериода("П"[,дата])  - начало полугодия                  */
/*             НачПериода("Г"[,дата])  - начало года                       */
PROCEDURE НачПериода:
   DEFINE OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* успешно ли выполнена ? */

   DEF VAR vDate     AS DATE NO-UNDO.
   DEF VAR vPeriod   AS CHAR NO-UNDO.

   MAIN:
   DO ON ERROR  UNDO, LEAVE
      ON ENDKEY UNDO, LEAVE:
      /* Разбор входных параметров. */
      IF NOT (   Pars-ValidParam(1)
              OR Pars-ValidParam(2)) THEN
         LEAVE MAIN.

      vPeriod = Pars-GetString(0).
      IF pn GT 0 THEN
         vDate = DATE(Pars-GetString(1)).
      ELSE
         vDate = in-op-date.
      IF vDate = ? THEN LEAVE MAIN.

      CASE vPeriod:
         WHEN "М" OR
         WHEN "M" THEN vDate = FirstMonDate(vDate).
         WHEN "K" OR
         WHEN "К" THEN vDate = kvart_beg(vDate).
         WHEN "П" THEN vDate = FirstHalfYearDate(vDate).
         WHEN "Г" THEN vDate = FirstYearDate(vDate).
         OTHERWISE LEAVE MAIN.
      END CASE.

      RUN Pars-SetCHARResult (STRING(vDate)).
      is-ok = YES.
   END.
END PROCEDURE.

/* Что делает: Возвращает код филиала
**     Формат: Филиал()
**  Синтаксис: Филиал()  - Возвращает код филиала
*/
PROCEDURE Филиал:
   DEFINE OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* успешно ли выполнена ? */

   IF Pars-ValidParam(0) THEN
   DO:
      RUN Pars-SetCHARResult (ShFilial).
      is-ok = YES.
   END.
END PROCEDURE.


/* Что делает: Возвращает код филиала
**     Формат: Филиал()
**  Синтаксис: Филиал()  - Возвращает код филиала
*/
PROCEDURE ДАННЫЕ_ПОРТФЕЛЯ:
   DEFINE OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* успешно ли выполнена ? */

   DEF VAR vLoan   AS HANDLE NO-UNDO.
   DEF VAR VInStr  AS CHAR   NO-UNDO.
   DEF VAR VDate   AS DATE   NO-UNDO.
   DEF VAR VDef    AS CHAR   NO-UNDO.
   DEF VAR VResult AS CHAR   NO-UNDO.

   IF NOT (Pars-ValidParam(1) OR
           Pars-ValidParam(2) OR
           Pars-ValidParam(3)) THEN
      RETURN.
   ASSIGN
      VInStr = Pars-GetString(0)
      VDef   = Pars-GetString(2) WHEN pn GE 2
   .
   IF pn GE 1 AND Pars-GetString(1) EQ "ПД" THEN
      VDate = wop.con-date.
   ELSE
      VDate = in-op-date.
   RUN LOAN_VALID_HANDLE (INPUT-OUTPUT vLoan).
   IF NOT VALID-HANDLE (vLoan) THEN
      RETURN.
   RUN bag-data.p (ENTRY(1, vLoan:PRIVATE-DATA),
                   ENTRY(2, vLoan:PRIVATE-DATA),
                   VInStr,
                   VDate,
                   VDef,
                   OUTPUT VResult).
   RUN Pars-SetCHARResult (VResult).
   is-ok = YES.

END PROCEDURE.

/*
    * Что делает: Возвращает серию и номер ЦБ.
    * Синтаксис : СЕРНОМ_ЦБ
    * Автор     : Ozmi  8/06/2011
    * Пример    : СЕРНОМ_ЦБ
*/
PROCEDURE СЕРНОМ_ЦБ:

   DEFINE OUTPUT PARAMETER is-ok AS LOGICAL     NO-UNDO. /* успешно ли выполнена ? */

   DEFINE VARIABLE ph      AS HANDLE        NO-UNDO.
   DEFINE VARIABLE mResult AS CHARACTER     NO-UNDO.

   DEFINE BUFFER sec-code FOR sec-code.

   is-ok = FALSE.

   IF NOT Pars-ValidParam(0) THEN RETURN.

   /* Определение правильного применения функции, формат private-data */
   RUN LOAN_VALID_HANDLE (INPUT-OUTPUT ph).

   IF NOT VALID-HANDLE(ph) THEN RETURN.

   FIND FIRST loan WHERE loan.contract  EQ ENTRY(1, ph:PRIVATE-DATA)
                     AND loan.cont-code EQ ENTRY(2, ph:PRIVATE-DATA) NO-LOCK NO-ERROR.
   IF NOT AVAIL loan THEN RETURN.

   FIND FIRST sec-code WHERE sec-code.sec-code EQ loan.sec-code NO-LOCK NO-ERROR.
   IF NOT AVAIL sec-code THEN RETURN.

   mResult = sec-code.series + " " + STRING(sec-code.form-nbr).

   RUN Pars-SetCHARResult(mResult).

   is-ok = TRUE.

END PROCEDURE.

/*
    * Что делает: выбор доверенности
    * Синтаксис : БыстрыйВводНазн
    * Автор     : kostik  01/07/2011
    * Пример    : БыстрыйВводНазн
*/
PROCEDURE ВводДовРко:
   DEFINE OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* успешно ли выполнена ? */
   DEFINE VAR vacct AS CHARACTER NO-UNDO.
   DEFINE VAR vRetVal AS CHARACTER INIT "" NO-UNDO.
   DEFINE BUFFER bfacct FOR acct.
   DEFINE BUFFER loan FOR loan.

IF NOT Pars-ValidParam(0) THEN RETURN.

{find-act.i &acct=wop.acct-db &bact=bfacct}
IF AVAIL bfacct AND CAN-FIND(FIRST loan WHERE loan.class-code EQ "proxy-base"
                                          AND loan.cust-cat EQ bfacct.cust-cat 
                                          AND loan.cust-id  EQ bfacct.cust-id)
THEN vacct = bfacct.acct.
ELSE DO:
   {find-act.i &acct=wop.acct-cr &bact=bfacct}
   IF AVAIL bfacct AND CAN-FIND(FIRST loan WHERE loan.class-code EQ "proxy-base"
                                             AND loan.cust-cat EQ bfacct.cust-cat 
                                             AND loan.cust-id  EQ bfacct.cust-id)
   THEN vAcct = bfacct.acct.
END.

is-ok = YES.

IF not {assigned vAcct} THEN 
RETURN.

IF AVAIL bfacct THEN DO TRANSACTION:
   RUN browseld.p("proxy-base",
                  "cust-cat" + CHR(1) + "cust-id"       + CHR(1) + "drower-id",
                  bfacct.cust-cat  + CHR(1) + STRING(bfacct.cust-id) + CHR(1) +  STRING(bfacct.cust-id),
                  "cust-cat" + CHR(1) + "cust-id"       + CHR(1) + "drower-id",
                  3).
   IF LASTKEY EQ 10 AND pick-value NE "" THEN DO:

   FIND FIRST wop WHERE RECID(wop) EQ rid NO-LOCK NO-ERROR.

   IF AVAILABLE wop THEN
       /* Поиск документа. */

       FIND FIRST op WHERE
           RECID(op) EQ wop.op-recid NO-LOCK NO-ERROR.

      
      FIND FIRST loan WHERE loan.contract EQ ENTRY(1,pick-value)
                        AND loan.cont-code EQ ENTRY(2,pick-value)
      NO-LOCK NO-ERROR.

      vRetVal = IF AVAIL loan THEN (loan.doc-num + " от " + STRING(loan.open-date,"99.99.9999") + " г.") ELSE "".
      RUN SetSysConf IN h_base ("ДОВРКО",loan.contract + "," + loan.cont-code).
   END.
   RUN Pars-SetCHARResult(vRetVal).
END.
END PROCEDURE.

/*
    * Что делает: Возвращает Фамилию агента доверенности
    * Синтаксис : ФИОпоДовер()
    * Автор     : ссс  15/05/2014
    * Пример    : ФИОпоДовер()
*/
PROCEDURE ФИОпоДовер:
   
DEFINE OUTPUT PARAM is-ok AS LOGICAL            NO-UNDO. /* успешно ли выполнена ? */
DEFINE VARIABLE vDover    AS CHARACTER          NO-UNDO.
DEFINE VARIABLE vAgent    AS CHARACTER          NO-UNDO.
DEFINE VARIABLE vAcct     AS CHARACTER          NO-UNDO.
DEFINE VARIABLE vAgentID  AS INT64              NO-UNDO.
DEFINE VARIABLE vAgentCat AS CHARACTER          NO-UNDO.
DEFINE VARIABLE vClName   AS CHARACTER EXTENT 3 NO-UNDO.
DEFINE VARIABLE vRetVal   AS CHARACTER INIT ""  NO-UNDO.

DEFINE BUFFER acct FOR acct.
DEFINE BUFFER loan FOR loan.

IF NOT Pars-ValidParam(0) THEN RETURN.

ASSIGN
   vDover = GetSysConf("ДОВРКО")
   is-ok = YES.

IF {assigned vDover} AND NUM-ENTRIES(vDover) EQ 2 THEN
DO:
   FIND FIRST loan WHERE loan.contract  EQ ENTRY(1,vDover) AND 
                         loan.cont-code EQ ENTRY(2,vDover)
   NO-LOCK NO-ERROR.
END.
ELSE
DO:
   {find-act.i &acct=wop.acct-db &bact=acct}
   IF AVAIL acct AND CAN-FIND(FIRST loan WHERE loan.class-code EQ "proxy-base"
                                             AND loan.cust-cat EQ acct.cust-cat 
                                             AND loan.cust-id  EQ acct.cust-id)
   THEN vAcct = acct.acct.
   ELSE DO:
      {find-act.i &acct=wop.acct-cr &bact=acct}
      IF AVAIL acct AND CAN-FIND(FIRST loan WHERE loan.class-code EQ "proxy-base"
                                                AND loan.cust-cat   EQ acct.cust-cat 
                                                AND loan.cust-id    EQ acct.cust-id)
      THEN vAcct = acct.acct.
   END.
   IF AVAIL(acct) AND {assigned vAcct} THEN
   RUN browseld.p("proxy-base",
                  "cust-cat"     + CHR(1) + "cust-id"            + CHR(1) + "drower-id",
                  acct.cust-cat  + CHR(1) + STRING(acct.cust-id) + CHR(1) +  STRING(acct.cust-id),
                  "cust-cat"     + CHR(1) + "cust-id"            + CHR(1) + "drower-id",
                  3).
   IF LASTKEY EQ 10 AND pick-value NE "" THEN
   DO:
      FIND FIRST loan WHERE loan.contract  EQ ENTRY(1,pick-value) AND 
                            loan.cont-code EQ ENTRY(2,pick-value)
      NO-LOCK NO-ERROR.
   END.
END.
/**/
IF AVAIL(loan) THEN
DO:

   vAgentID  = INT64(GetXAttrValueEx("loan",
                                  loan.contract + "," + loan.cont-code,
                                  "agent-id",
                                  "0")) NO-ERROR.

   FIND FIRST signs WHERE signs.file-name  EQ "loan" AND 
                          signs.code       EQ "agent-cat" AND
                          signs.surrogate  EQ  loan.contract + "," + loan.cont-code NO-LOCK NO-ERROR.

   IF AVAIL(signs) THEN vAgentCat = signs.code-value + signs.xattr-value.

   IF NOT ERROR-STATUS:ERROR AND 
      vAgentID NE 0 THEN
   DO:
      RUN GetCustName IN h_base (vAgentCat, vAgentID, "",
                                 OUTPUT       vClName[1],
                                 OUTPUT       vClName[2],
                                 INPUT-OUTPUT vClName[3]).
   
      vAgent = vClName[1] + " " + vClName[2].
   END.

   vRetVal = vAgent.
   /*RUN SetSysConf IN h_base ("ДОВРКО",?).*/
END.

RUN Pars-SetCHARResult(vRetVal).

END PROCEDURE.
/* $LINTUSER='BIS' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='30/01/2015 09:19:12.805+04:00' */
/* $LINTFILE='details.fun' */
/*prosignJ9ZkNd+8Cn+bqCri5TY+HA*/