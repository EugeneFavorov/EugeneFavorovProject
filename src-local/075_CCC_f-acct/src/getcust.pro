/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2001 ТОО "Банковские информационные системы"
     Filename: GETCUST.PRO
      Comment: по заявке 0011574 функционал ф.ФизЛицо убран в основную библиотеку base-pp
   Parameters:
         Uses:
      Used by:
      Created: 18.09.2001 15:24 SAP
     Modified: 20.09.2001 13:01 SAP
     Modified: 21.09.2001 10:34 SAP
     Modified: 31.01.2002 09:58 SAP
     Modified: 29.05.2002 11:02 Gunk     - GetCustNoTown
     Modified: 10.09.2002 kraw (0005744) - Восстановление логики выбора ИНН банка
     Modified: 30.10.2002 12:21 SEMA     по заявке 0011574 функционал ф.ФизЛицо убран в основную библиотеку base-pp
     Modified: 20.11.2002 kraw (0011991) GetCust, если ИНН не присвоено, не ставит в начало названия слово "ИНН"
     Modified: 12.03.2003 yuss (0012300) преобразования форматированной строки адреса физ.лица
     Modified: 03.04.2004 16:32 KSV      (0019947) Изменена процедура
                                         GetCustName для модулей дилинга.
     Modified: 24.01.2006 18:49 ILVI     (48741)  Добавлен анализ настроечного параметра БалСчНаим при формирорвании
                                                  наименования клиента
     Modified: 21.11.2007 jadv (0069015) GetCustNameShort - Получение краткого наименования клиента.
*/

DEF VAR corp-ok            AS LOG  NO-UNDO.
DEF VAR priv-ok            AS LOG  NO-UNDO.
DEF VAR int-ok             AS LOG  NO-UNDO.
DEF VAR bank-ok            AS LOG  NO-UNDO.
DEF VAR userids            AS CHAR NO-UNDO.
DEF VAR rightview          AS CHAR NO-UNDO.
DEF VAR ClassAcctPosView   AS CHAR NO-UNDO.
DEF VAR otdel-lst          AS CHAR NO-UNDO.

/* Проверяет право просмотра остатков по счету. */
FUNCTION AcctLook RETURN LOG (
   BUFFER bbacct FOR acct
):
   RETURN
      (     bbacct.user-id EQ userid('bisquit')
         OR CAN-DO (acct-look, bbacct.acct)
         OR CAN-DO (otdel-lst, bbacct.branch-id)
      )
      AND CAN-DO (ClassAcctPosView, bbacct.class-code).
END.

/* Проверяет право просмотра остатков по счету. */
FUNCTION AcctLookBuffer RETURN LOG (
   INPUT iHAcct AS HANDLE  /* Указатель на буффер счета. */
):
   RETURN
      (     iHAcct:BUFFER-FIELD ("user-id"):BUFFER-VALUE EQ USERID ('bisquit')
         OR CAN-DO (acct-look, iHAcct:BUFFER-FIELD ("acct")       :BUFFER-VALUE)
         OR CAN-DO (otdel-lst, iHAcct:BUFFER-FIELD ("branch-id")  :BUFFER-VALUE)
      )
      AND CAN-DO (ClassAcctPosView, iHAcct:BUFFER-FIELD ("class-code"):BUFFER-VALUE).
END.

procedure GetCust:
   def param buffer GetCust-acct for acct.
   def input param use-inn as logical no-undo.
   def input param use-signs as log no-undo.
   def output param name1 as char no-undo.
   def output param name2 as char no-undo.
   def output param cust-inn as char no-undo.
   /* Argument name must be "x(35)" extent 2 */

   def /*input param*/ VAR in-acct like acct.acct no-undo.
   def /*input param*/ VAR in-currency like acct.currency no-undo.
   def /*input param*/ VAR in-cust-cat like acct.cust-cat no-undo.
   def /*input param*/ VAR in-cust-id LIKE acct.cust-id no-undo.
   
   DEFINE VARIABLE vInn AS CHARACTER no-undo.
   DEFINE VARIABLE vPrnINN AS LOGICAL NO-UNDO.

   IF NOT AVAILABLE GetCust-acct THEN
      RETURN.

   assign
      in-acct      = GetCust-acct.acct
      in-currency  = GetCust-acct.currency
      in-cust-cat  = GetCust-acct.cust-cat
      in-cust-id   = GetCust-acct.cust-id
      cust-inn     = "000000000000"
      name2        = "-"
      vPrnINN      = NOT (GetSysConf("stmtPrt") EQ "YES" AND 
                          CAN-DO(FGetSetting("БалСчНаимВып",?,""),STRING(GetCust-acct.bal-acct,"99999")))
   .


   RUN GetCustName (INPUT in-cust-cat,
                    INPUT in-cust-id,
                    INPUT in-acct,
                    OUTPUT name1,
                    OUTPUT name2,
                    INPUT-OUTPUT cust-inn).
   IF     (    (GetCust-acct.cust-cat EQ "В" AND NOT CAN-DO(FGetSetting("БалСчИНН",?,""),GetCust-acct.acct))
            OR use-signs
            OR (IF GetSysConf("stmtPrt") EQ "YES" THEN
                   CAN-DO(FGetSetting("БалСчНаимВып",?,""),STRING(GetCust-acct.bal-acct,"99999"))
                ELSE
                   CAN-DO(FGetSetting("БалСчНаим",?,""),STRING(GetCust-acct.bal-acct,"99999")))   
          ) 
      AND {assigned GetCust-acct.details} 
   THEN
      ASSIGN
         name1 = /*GetCust-acct.details*/ GetTempXattrValueEx("acct",in-acct + "," + in-currency,"Details",gend-date,"")
         name2 = ""
      .
   if use-signs and ((in-cust-cat eq "Ю" and corp-ok) or
      (in-cust-cat eq "Ч" and priv-ok) or
      (in-cust-cat eq "Б" and bank-ok)) then do:
      if can-do(balschinn, substr(in-acct,1,5)) or
              GetXAttrValueEx("acct", in-acct + "," + in-currency, "bank-inn", ?) = "Да" then DO:
         vInn = GetXAttrValueEx("branch", GetCust-acct.branch-id, "ИНН", ?).

         cust-inn = IF {assigned vInn} THEN
                       vInn
                    ELSE 
                       IF bank-inn <> "" THEN
                          bank-inn
                       ELSE 
                          "000000000000".
      END.  
   end.

   assign
      name1 = (if use-inn and cust-inn <> "000000000000" AND vPrnINN then "ИНН " + cust-inn + " " else "")
            + trim(name1).
      /*по желанию банка наименование счета будет показываться всегда (в печатных документах,при вводе транзакции) */
      name1 = /*IF AcctView(buffer GetCust-acct)  THEN*/  trim(name1) /*ELSE "" */.
      name2 = /*IF AcctView(buffer GetCust-acct)  THEN*/  trim(name2) /*ELSE "НЕТ ДОПУСКА"*/.

end procedure.

procedure GetCust0:
   def param buffer GetCust-acct for acct.
   def input param use-inn as logical no-undo.
   def input param use-signs as log no-undo.
   def output param name1 as char no-undo.
   def output param name2 as char no-undo.
   def var cust-inn as char no-undo.

   run GetCust (buffer GetCust-acct,
                use-inn,use-signs,output name1, output name2, output cust-inn).

end procedure.

procedure GetCustName.
  DEF INPUT PARAM  in-cust-cat like acct.cust-cat no-undo.
  DEF INPUT PARAM  in-cust-id LIKE acct.cust-id no-undo.
  DEF INPUT PARAM  in-acct LIKE acct.acct no-undo.
  DEF OUTPUT PARAM  name1 as char no-undo.
  DEF OUTPUT PARAM  name2 as char no-undo.
  DEF INPUT-OUTPUT PARAM  cust-inn as char no-undo.

  DEFINE VARIABLE vInn AS CHARACTER   NO-UNDO.
  DEFINE BUFFER acct FOR acct.

   case in-cust-cat:
     when "Ю" then if corp-ok then do:
       find cust-corp where cust-corp.cust-id = in-cust-id no-lock no-error.
       if avail cust-corp then assign
          cust-inn = cust-corp.inn when cust-corp.inn ne ?
          name1    = cust-corp.cust-stat
          name2    = cust-corp.name-corp
       .
     end.
     when "Ч" then if priv-ok then do:
       find person where person.person-id = in-cust-id no-lock no-error.
       if avail person then assign
          cust-inn = person.inn when person.inn ne ?
          name1    = person.name-last
          name2    = person.first-names
       .
     end.
     when "Б" then if bank-ok then do:
       find banks where banks.bank-id = in-cust-id no-lock no-error.
       if avail banks then do:
          vInn = GetBankInn ("bank-id", STRING (banks.bank-id)).
          assign
             cust-inn = vInn when {assigned vInn}
             name1    = banks.name
             name2    = ""
          .
       end.
     end.
     when "В" then if int-ok then do:
       {find-act.i
          &acct = in-acct
       }
       IF AVAIL acct THEN
          vInn = GetXAttrValueEx("branch", acct.branch-id, "ИНН", ?).
       IF NOT {assigned vInn} THEN
          vInn = bank-inn.      
       assign
          cust-inn = vInn WHEN {assigned vInn}
          name1    = ""
          name2    = ""
       .
       /* Commented by KSV: Для модулей дилинга, данный идентификатор
       ** интерпретируется как неопредленный контрагент */
       IF CAN-DO("bm,fx,mm,sm,bill",work-module) AND in-cust-id = 0  THEN
          name1 = "НЕОПРЕДЕЛЕННЫЙ СУБЪЕКТ".
     end.
   end.
end procedure.

/* переводит неопределенное значение в знак "вопрос" ("?") */
FUNCTION GetNotEmpty CHAR (ipString AS CHAR):
   RETURN IF ipString EQ ? THEN "?" ELSE ipString.
END FUNCTION.

/* Процедура GetCustNameShort - Получение краткого наименования клиента.
** Параметры:
** input  iCust-cat  - Тип клиента (Ю,Ч,Б)
**        iCust-id   - id клиента
** output oShortName - Краткое наименование клиента.
*/
PROCEDURE GetCustNameShort.
   DEFINE INPUT  PARAMETER iCust-cat  AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER iCust-id   AS INT64   NO-UNDO.
   DEFINE OUTPUT PARAMETER oShortName AS CHARACTER NO-UNDO.

   ASSIGN
      oShortName = "".

   CASE iCust-cat:
      WHEN "Ю" THEN DO:
         FIND cust-corp WHERE cust-corp.cust-id = iCust-id NO-LOCK NO-ERROR.
         IF AVAIL cust-corp THEN 
            ASSIGN 
               oShortName = TRIM(GetNotEmpty(cust-corp.cust-stat)) + " "
                          + TRIM(GetNotEmpty(cust-corp.name-short)).
      END.

      WHEN "Ч" THEN DO:
         FIND FIRST person WHERE person.person-id = iCust-id NO-LOCK NO-ERROR.
         IF AVAIL person THEN
            ASSIGN 
               oShortName = TRIM(GetNotEmpty(person.name-last)) + " " 
                          + TRIM(GetNotEmpty(person.first-names)).
      END.

      WHEN "Б" THEN DO:
         FIND banks WHERE banks.bank-id = iCust-id NO-LOCK NO-ERROR.
         IF AVAIL banks THEN
            ASSIGN 
               oShortName = TRIM(GetNotEmpty(banks.short-name)). 
      END.
   END CASE.

   ASSIGN
      oShortName = TRIM(REPLACE(oShortName, "?", "")).

END PROCEDURE. 
/* $LINTUSER='BIS' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='*' */
/* $LINTDATE='18/09/2014 12:46:52.812+04:00' */
/* $LINTFILE='getcust.pro' */
/*prosignaeVzcRSwOdDPY82cVa/kBQ*/