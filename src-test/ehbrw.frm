/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2010 ЗАО "Банковские информационные системы"
     Filename: ehbrw.FRM
      Comment: Документы для электронного хранилища - формы
   Parameters:
         Uses:
      Used by:
      Created: ayv
     Modified:    
*/

FORM
   code.{&BranchDb}
     VIEW-AS FILL-IN SIZE 20 BY 1
      FORMAT "x(20)"
      LABEL "Код подразделения"
      HELP "Код подразделения счета дебета"
   code.{&AcctDb}
     VIEW-AS FILL-IN SIZE 50 BY 1
      FORMAT "x(200)"
      LABEL "Счета дебета"
      HELP "Маска счетов дебета"
   code.{&BranchCr}
     VIEW-AS FILL-IN SIZE 20 BY 1
      FORMAT "x(20)"
      LABEL "Код подразделения"
      HELP "Код подразделения счета кредита"
   code.{&AcctCr}
     VIEW-AS FILL-IN SIZE 50 BY 1
      FORMAT "x(200)"
      LABEL "Счета кредита"
      HELP "Маска счетов кредита"    
   code.{&DocType}
     VIEW-AS FILL-IN SIZE 30 BY 1
      FORMAT "x(30)"
      LABEL "Тип документа"
      HELP "Тип документа"
   code.{&Currency}
     VIEW-AS FILL-IN SIZE 15 BY 1
      FORMAT "x(15)"
      LABEL "Валюта"
      HELP "Валюта документа"
   code.{&OpKind}
     VIEW-AS FILL-IN SIZE 30 BY 1
      FORMAT "x(100)"
      LABEL "Транзакция"
      HELP "Транзакция, создавшая документ"
   code.{&Details}
     VIEW-AS FILL-IN SIZE 30 BY 1
      FORMAT "x(100)"
      LABEL "Наименование платежа"
      HELP "Наименование платежа"
   code.{&Param}
     VIEW-AS FILL-IN SIZE 30 BY 1
      FORMAT "x(30)"
      LABEL "Параметр"
      HELP "Дополнительный параметр (пр. Интернет-Банк)"
   mStartDate
     VIEW-AS FILL-IN SIZE 10 BY 1
      FORMAT "99/99/9999"
      LABEL "Дата ввода"
      HELP "Дата ввода условия в действие"
   mEndDate
     VIEW-AS FILL-IN SIZE 10 BY 1
      FORMAT "99/99/9999"
      LABEL "Дата окончания"
      HELP "Дата окончания действия условия"
WITH FRAME edit WIDTH 80 side-label
     TITLE COLOR bright-white "[ УСЛОВИЕ ]".

FORM
   str-recid 
	   COLUMN-LABEL "√"
   code.{&BranchDb}
      FORMAT "x(10)"
      COLUMN-LABEL "КОД ПОД."
      HELP "Код подразделения счета дебета"
   code.{&AcctDb}
      FORMAT "x(20)"
      LABEL "Счета дебета"
      COLUMN-LABEL "ДЕБЕТ"
      HELP "Маска счетов дебета" 
   code.{&BranchCr}
      FORMAT "x(10)"
      COLUMN-LABEL "КОД ПОД."
      HELP "Код подразделения счета кредита"
   code.{&AcctCr}
      FORMAT "x(20)"
      LABEL "Счета кредита"
      COLUMN-LABEL "КРЕДИТ"
      HELP "Маска счетов кредита"   
   code.{&DocType}
      FORMAT "x(10)"
      LABEL "Тип документа"
      COLUMN-LABEL "ТИП ДОК."
      HELP "Тип документа"
   code.{&Currency}
      FORMAT "x(10)"
      LABEL "Валюта"
      COLUMN-LABEL "ВАЛЮТА"
      HELP "Валюта документа"
   code.{&OpKind}
      FORMAT "x(10)"
      LABEL "Транзакция"
      COLUMN-LABEL "ТРАНЗАКЦИЯ"
      HELP "Транзакция, создавшая документ"
   code.{&Details}
      FORMAT "x(15)"
      LABEL "Наименование платежа"
      COLUMN-LABEL "НАИМ. ПЛАТЕЖА"
      HELP "Наименование платежа"
   code.{&Param}
      FORMAT "x(15)"
      LABEL "Параметр"
      COLUMN-LABEL "ПАРАМЕТР"
      HELP "Дополнительный параметр (пр. Интернет-Банк)"
   mStartDate
      FORMAT "99/99/9999"
      LABEL "Дата ввода"
      COLUMN-LABEL "ДАТА ВВОДА"
      HELP "Дата ввода условия в действие"
   mEndDate
      FORMAT "99/99/9999"
      LABEL "Дата окончания"
      COLUMN-LABEL "ДАТА ОКОНЧАНИЯ"
      HELP "Дата окончания действия условия"
   /*SPACE(0)*/
WITH FRAME browse1 WIDTH 300
     TITLE COLOR bright-white "[ ДОКУМЕНТЫ ДЛЯ ЭЛЕКТРОННОГО ХРАНЕНИЯ ]".

FORM
   code.code   
      COLUMN-LABEL  "КОД ПАРАМЕТРА" 
      FORMAT "x(13)" 
   code.name   
      COLUMN-LABEL  "НАИМЕНОВАНИЕ ПАРАМЕТРА" 
      FORMAT "x(39)"
      VIEW-AS FILL-IN SIZE 39 BY 1  
   code.val SPACE(0)
   str-recid 
      COLUMN-LABEL "√"
WITH FRAME browse2 WIDTH 190
     TITLE COLOR bright-white "[ ДОКУМЕНТЫ ДЛЯ ЭЛЕКТРОННОГО ХРАНЕНИЯ ]".

FORM
   mFltBranchDb
     VIEW-AS FILL-IN SIZE 50 BY 1
      FORMAT "x(200)"
      LABEL "Код подразделения"
      HELP "Код подразделения счетов дебета" 
   mFltAcctDb
     VIEW-AS FILL-IN SIZE 50 BY 1
      FORMAT "x(200)"
      LABEL "Счета дебета"
      HELP "Маска счетов дебета" 
   mFltBranchCr
     VIEW-AS FILL-IN SIZE 50 BY 1
      FORMAT "x(200)"
      LABEL "Код подразделения"
      HELP "Код подразделения счетов кредита" 
   mFltAcctCr
     VIEW-AS FILL-IN SIZE 50 BY 1
      FORMAT "x(200)"
      LABEL "Счета кредита"
      HELP "Маска счетов кредита"  
   mFltDocType
     VIEW-AS FILL-IN SIZE 30 BY 1
      FORMAT "x(30)"
      LABEL "Тип документа"
      HELP "Тип документа"
   mFltCurrency
     VIEW-AS FILL-IN SIZE 15 BY 1
      FORMAT "x(15)"
      LABEL "Валюта"
      HELP "Валюта документа"
   mFltOpKind
     VIEW-AS FILL-IN SIZE 30 BY 1
      FORMAT "x(100)"
      LABEL "Транзакция"
      HELP "Транзакция, создавшая документ"
   mFltOpKind
     VIEW-AS FILL-IN SIZE 30 BY 1
      FORMAT "x(100)"
      LABEL "Транзакция"
      HELP "Транзакция, создавшая документ"
   mFltDetails
     VIEW-AS FILL-IN SIZE 30 BY 1
      FORMAT "x(100)"
      LABEL "Наимен. платежа"
      HELP "Наименование платежа"
   mFltParam
     VIEW-AS FILL-IN SIZE 30 BY 1
      FORMAT "x(30)"
      LABEL "Параметр"
      HELP "Дополнительный параметр (пр. Интернет-Банк)"
   mFltActual
     VIEW-AS COMBO-BOX LIST-ITEMS "Да", "Нет"
      FORMAT "x(4)"
      LABEL "Актуальные условия"
      HELP "Показывать ли только актуальные условия"
   mFltStartDate1
     VIEW-AS FILL-IN SIZE 10 BY 1
	   FORMAT "99/99/9999"
      LABEL "Дата ввода  С"
	   HELP "Дата ввода ЦФО в действие"
   mFltStartDate2
     VIEW-AS FILL-IN SIZE 10 BY 1
	   FORMAT "99/99/9999"
      LABEL "Дата ввода ПО"
	   HELP "Дата ввода ЦФО в действие"
   mFltEndDate1
     VIEW-AS FILL-IN SIZE 10 BY 1
	   FORMAT "99/99/9999"
      LABEL "Дата оконч.  С"
	   HELP "Дата окончания действия кода ЦФО" 
   mFltEndDate2
     VIEW-AS FILL-IN SIZE 10 BY 1
	   FORMAT "99/99/9999"
      LABEL "Дата оконч. ПО"
	   HELP "Дата окончания действия кода ЦФО" 
WITH FRAME flt-fr 1 COL OVERLAY CENTERED side-label ROW 6
	TITLE COLOR bright-white "[ ФИЛЬТР ]".