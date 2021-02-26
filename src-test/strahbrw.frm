/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2010 ЗАО "Банковские информационные системы"
     Filename: strahbrw.FRM
      Comment: Получатели страховых премий - формы
   Parameters:
         Uses:
      Used by:
      Created:    
     Modified:    
*/

FORM
   mCustCat
      FORMAT "x(1)"
      VIEW-AS COMBO-BOX LIST-ITEMS "Ч", "Ю", "Р","О"
      LABEL "Тип"
      HELP "Тип клиента"
   mCustID
      FORMAT ">>>>>>>>>>>>>9"
      LABEL "Код"
      HELP "Код клиента"
   code.{&WhiteINN}                             /* ИНН */
      VIEW-AS FILL-IN SIZE 12 BY 1
      FORMAT "x(12)"
      LABEL "ИНН"
      HELP "ИНН"
   code.{&ShortName}                            /* Краткое наименование */
      FORMAT "x(100)"
      VIEW-AS FILL-IN SIZE 58 BY 1
      LABEL "Краткое наименование"
      HELP "Краткое наименование клиента"
   code.{&WhiteName}                            /* ФИО/Наименование */
      FORMAT "x(200)"
      VIEW-AS FILL-IN SIZE 58 BY 1
      LABEL "ФИО/Наименование"
      HELP "ФИО или наименование клиента"
   mRaschAcct                                   /* Расчетный счет */
      FORMAT ">>>>>>>>>>>>>>>>>>>9"
      LABEL "Расч.Счет"
      HELP "Номер расчетного счета"
   code.{&BankBIC}                              /* БИК Банка */
      FORMAT "x(20)"
      VIEW-AS FILL-IN SIZE 20 BY 1
      LABEL "БИК Банка"
      HELP "БИК Банка"
   code.{&BankName}                            /* Наименование Банка */
      FORMAT "x(100)"
      VIEW-AS FILL-IN SIZE 58 BY 1
      LABEL "Наименование Банка"
      HELP "Наименование Банка"
   code.{&CorrAcct}                           /* Корр. счет */
      FORMAT "x(20)"
      VIEW-AS FILL-IN SIZE 20 BY 1
      LABEL "КоррСчет"
      HELP "Корреспондентский счет в РКЦ"
   code.{&Telefax}                            /* Телефон,факс */
      FORMAT "x(40)"
      VIEW-AS FILL-IN SIZE 30 BY 1
      LABEL "Телефон, факс"
      HELP "Телефон,факс"
   code.{&DocType}                            /* Код документа */
      FORMAT "x(50)"
      VIEW-AS FILL-IN SIZE 50 BY 1
      LABEL "Код документа"
      HELP "Код документа"
   code.{&DocCustCode}                        /* N документа */
      FORMAT "x(40)"
      VIEW-AS FILL-IN SIZE 30 BY 1
      LABEL "N документа"
      HELP "N документа"
   code.{&DocOpenDate}                        /* дата выдачи документа */
      FORMAT "99.99.9999"
      VIEW-AS FILL-IN SIZE 10 BY 1
      LABEL "Дата выдачи"
      HELP "Дата выдачи документа"
   code.{&DocIssue}                        /* Выдан документ */
      FORMAT "x(300)"
      VIEW-AS FILL-IN SIZE 58 BY 1
      LABEL "Выдан"
      HELP "Документ выдан"
   code.{&BirthDay}                        /* дата рождения */
      FORMAT "99.99.9999"
      VIEW-AS FILL-IN SIZE 10 BY 1
      LABEL "Дата рождения"
      HELP "Дата рождения"
   code.{&Address}                        /* адрес */
      FORMAT "x(300)"
      VIEW-AS FILL-IN SIZE 58 BY 1
      LABEL "Адрес"
      HELP "Адрес"
WITH FRAME edit.

FORM                  
   mCustCat
      FORMAT "x(1)"
      LABEL "Тип"
      COLUMN-LABEL "ТИП"
      HELP "Тип клиента"
   mCustID
      FORMAT ">>>>>>>>>>>>>9"
      LABEL "Код"
      COLUMN-LABEL "КОД"
      HELP "Код клиента"
   code.{&WhiteINN}                             /* ИНН */
      FORMAT "x(12)"
      VIEW-AS FILL-IN SIZE 12 BY 1
      LABEL "ИНН"
      COLUMN-LABEL "ИНН"
      HELP "ИНН"
   code.{&ShortName}                            /* Краткое наименование */
      FORMAT "x(100)"
      VIEW-AS FILL-IN SIZE 20 BY 1
      LABEL "Краткое наименование"
      COLUMN-LABEL "Краткое наименование"
      HELP "Краткое наименование клиента"
   code.{&WhiteName}                            /* ФИО/Наименование */
      FORMAT "x(200)"
      VIEW-AS FILL-IN SIZE 20 BY 1
      LABEL "ФИО/Наименование"
      COLUMN-LABEL "ФИО/НАИМЕНОВАНИЕ"
      HELP "ФИО или наименование клиента"
   mRaschAcct                                   /* Расчетный счет */
      FORMAT ">>>>>>>>>>>>>>>>>>>9"
      LABEL "Расч.Счет"
      COLUMN-LABEL "Расч.Счет"
      HELP "Номер расчетного счета"
   code.{&BankBIC}                              /* БИК Банка */
      FORMAT "x(20)"
      VIEW-AS FILL-IN SIZE 20 BY 1
      LABEL "БИК Банка"
      HELP "БИК Банка"
   code.{&BankName}                            /* Наименование Банка */
      FORMAT "x(100)"
      VIEW-AS FILL-IN SIZE 30 BY 1
      LABEL "Наименование Банка"
      HELP "Наименование Банка"
   code.{&CorrAcct}                           /* Корр. счет */
      FORMAT "x(20)"
      VIEW-AS FILL-IN SIZE 20 BY 1
      LABEL "КоррСчет"
      HELP "Корреспондентский счет в РКЦ"
   code.{&Telefax}                            /* Телефон,факс */
      FORMAT "x(40)"
      VIEW-AS FILL-IN SIZE 30 BY 1
      LABEL "Телефон, факс"
      HELP "Телефон,факс"      
   ch COLUMN-LABEL ""
   SPACE(0)
WITH FRAME browse1 TITLE "" WIDTH 205.

FORM
   code.code   
      COLUMN-LABEL  "КОД ПАРАМЕТРА" 
      FORMAT "x(13)" 
   code.name   
      COLUMN-LABEL  "НАИМЕНОВАНИЕ ПАРАМЕТРА" 
      FORMAT "x(39)"
      VIEW-AS FILL-IN SIZE 39 BY 1  
   code.val SPACE(0)
   ch 
      COLUMN-LABEL ""
WITH FRAME browse2 TITLE COLOR bright-white "".

FORM
   mFltCustCat
      FORMAT "x(1)"
      LABEL "Тип"
      HELP "Тип клиента"
   mFltCustID
      FORMAT ">>>>>>>>>>>>>9"
      LABEL "Код"
      HELP "Код клиента"
   mFltInn
      FORMAT "x(12)"
      VIEW-AS FILL-IN SIZE 12 BY 1
      LABEL "ИНН"
      HELP "ИНН"
   mFltShortName
      FORMAT "x(100)"
      VIEW-AS FILL-IN SIZE 30 BY 1
      LABEL "Краткое наименование"
      HELP "Краткое наименование клиента"
   mFltName
      FORMAT "x(200)"
      VIEW-AS FILL-IN SIZE 30 BY 1
      LABEL "ФИО/Наименование"
      HELP "ФИО или наименование клиента"
   mFltRaschAcct
      FORMAT ">>>>>>>>>>>>>>>>>>>9"
      LABEL "Расч.Счет"
      HELP "Номер расчетного счета"
   mFltBankBIC                              /* БИК Банка */
      FORMAT "x(20)"
      VIEW-AS FILL-IN SIZE 20 BY 1
      LABEL "БИК Банка"
      HELP "БИК Банка"
   mFltBankName                            /* Наименование Банка */
      FORMAT "x(100)"
      VIEW-AS FILL-IN SIZE 30 BY 1
      LABEL "Наименование Банка"
      HELP "Наименование Банка"
   mFltCorrAcct                           /* Корр. счет */
      FORMAT "x(20)"
      VIEW-AS FILL-IN SIZE 20 BY 1
      LABEL "КоррСчет"
      HELP "Корреспондентский счет в РКЦ"
   mFltTelefax                            /* Телефон,факс */
      FORMAT "x(40)"
      VIEW-AS FILL-IN SIZE 30 BY 1
      LABEL "Телефон, факс"
      HELP "Телефон,факс"  
WITH CENTERED 1 COLUMNS 
     FRAME flt-fr OVERLAY ROW 7 SIDE-LABELS TITLE "Фильтр" WIDTH 205 .
      
