/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2010 ЗАО "Банковские информационные системы"
     Filename: cfobrw.FRM
      Comment: ЦФО - формы
   Parameters:
         Uses:
      Used by:
      Created: ayv
     Modified:    
*/

FORM
   mBranchID	
	  VIEW-AS FILL-IN SIZE 4 BY 1
      FORMAT "x(4)"
      LABEL "Код подразделения"
      HELP "Код подразделения"
   mCFO
      VIEW-AS FILL-IN SIZE 15 BY 1
      FORMAT "x(15)"
      LABEL "Код ЦФО"
      HELP "Код ЦФО"
   code.{&ShortName}
	  VIEW-AS FILL-IN SIZE 50 BY 1
      FORMAT "x(150)"
      LABEL "Краткое наим."
      HELP "Краткое наименование подразделения (не ЦФО!)"
  code.{&Status}
      FORMAT "x(3)"
      VIEW-AS COMBO-BOX LIST-ITEMS "ПО", "П", "О"
      LABEL "Участие"
      HELP "Участие ЦФО в Привлечении и Обслуживании"
  mStartDate
      VIEW-AS FILL-IN SIZE 10 BY 1
	  FORMAT "99/99/9999"
      LABEL "Дата ввода"
	  COLUMN-LABEL "ДАТА ВВОДА"
      HELP "Дата ввода ЦФО в действие"
   mEndDate
	  VIEW-AS FILL-IN SIZE 10 BY 1
	  FORMAT "99/99/9999"
      LABEL "Дата окончания"
	  COLUMN-LABEL "ДАТА ОКОНЧАНИЯ"
      HELP "Дата окончания действия кода ЦФО"
   code.{&ProdType}	
      VIEW-AS COMBO-BOX LIST-ITEMS "Автокред", "Прочие", "Автокред,Прочие"
      FORMAT "x(40)"
      LABEL "Типы продуктов"
      HELP "Типы продуктов"
WITH FRAME edit WIDTH 80 side-label
     TITLE COLOR bright-white "[ КОД ЦФО ]".

FORM
   str-recid 
	  COLUMN-LABEL "√"
   mBranchID	
      FORMAT "x(4)"
	  LABEL "Код подразделения"
      COLUMN-LABEL "КОД"
	  HELP "Код подразделения"
   code.{&ShortName}
	  FORMAT "x(90)"
      LABEL "Краткое наим."
	  COLUMN-LABEL "КРАТКОЕ НАИМЕНОВАНИЕ ПОДРАЗДЕЛЕНИЯ"
      HELP "Краткое наименование подразделения"
   mCFO
      FORMAT "x(15)"
      LABEL "Код ЦФО"
	  COLUMN-LABEL "КОД ЦФО"
      HELP "Код ЦФО"
   code.{&Status}
      FORMAT "x(3)"
      LABEL "Участие"
	  COLUMN-LABEL "УЧАСТИЕ"
      HELP "Участие ЦФО в Привлечении и Обслуживании"
   mStartDate
	  FORMAT "99/99/9999"
      LABEL "Дата ввода"
	  COLUMN-LABEL "ДАТА ВВОДА"
      HELP "Дата ввода ЦФО в действие"
   mEndDate
	  FORMAT "99/99/9999"
      LABEL "Дата окончания"
	  COLUMN-LABEL "ДАТА ОКОНЧАНИЯ"
      HELP "Дата окончания действия кода ЦФО"
   code.{&ProdType}	
      FORMAT "x(50)"
      LABEL "Типы продуктов"
	  COLUMN-LABEL "ТИПЫ ПРОДУКТОВ"
      HELP "Типы продуктов"  
   SPACE(0)
WITH FRAME browse1 WIDTH 190
     TITLE COLOR bright-white "[ СПРАВОЧНИК ЦФО ]".

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
     TITLE COLOR bright-white "[ СПРАВОЧНИК ЦФО ]".

FORM
   mFltBranchID	
	  VIEW-AS FILL-IN SIZE 4 BY 1
      FORMAT "x(4)"
      LABEL "Код подразделения"
      HELP "Код подразделения"
   mFltShortName
	  VIEW-AS FILL-IN SIZE 40 BY 1
      FORMAT "x(150)"
      LABEL "Краткое наим."
      HELP "Краткое наименование подразделения"
   mFltCFO
      VIEW-AS FILL-IN SIZE 15 BY 1
      FORMAT "x(15)"
      LABEL "Код ЦФО"
      HELP "Код ЦФО"
   mFltStatus
      FORMAT "x(10)"
      VIEW-AS FILL-IN SIZE 10 BY 1
      LABEL "Участие"
      HELP "Участие ЦФО в Привлечении и Обслуживании"
   mFltStartDate1
      VIEW-AS FILL-IN SIZE 10 BY 1
	  FORMAT "99/99/9999"
      LABEL "Дата ввода  С"
	  COLUMN-LABEL "ДАТА ВВОДА С"
      HELP "Дата ввода ЦФО в действие"
   mFltStartDate2
      VIEW-AS FILL-IN SIZE 10 BY 1
	  FORMAT "99/99/9999"
      LABEL "Дата ввода ПО"
	  COLUMN-LABEL "ДАТА ВВОДА ПО"
      HELP "Дата ввода ЦФО в действие"
   mFltEndDate1
      VIEW-AS FILL-IN SIZE 10 BY 1
	  FORMAT "99/99/9999"
      LABEL "Дата оконч.  С"
	  COLUMN-LABEL "ДАТА ОКОНЧАНИЯ С"
      HELP "Дата окончания действия кода ЦФО" 
   mFltEndDate2
      VIEW-AS FILL-IN SIZE 10 BY 1
	  FORMAT "99/99/9999"
      LABEL "Дата оконч. ПО"
	  COLUMN-LABEL "ДАТА ОКОНЧАНИЯ ПО"
      HELP "Дата окончания действия кода ЦФО" 
   mFltProdType	
      VIEW-AS FILL-IN SIZE 40 BY 1
      FORMAT "x(40)"
      LABEL "Типы продуктов"
      HELP "Типы продуктов"
WITH FRAME flt-fr 1 COL OVERLAY CENTERED side-label ROW 6
	TITLE COLOR bright-white "[ ФИЛЬТР ]".      
