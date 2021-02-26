/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2010 ЗАО "Банковские информационные системы"
     Filename: title_doc.p
      Comment: Классификатор "КаналПривл" Канал привлечения клиента
   Parameters:
         Uses:
      Used by:
      Created: ayv 
     Modified: 
*/
&GLOBAL-DEFINE PClassDRDynamic YES
&GLOBAL-DEFINE SpecClass       "КаналПривл"
&GLOBAL-DEFINE wherecd0        AND DATE(code.misc[1]) LE TODAY AND (code.misc[2] EQ '' OR DATE(code.misc[2]) GT TODAY)
&GLOBAL-DEFINE help-label      "F1|F3 все даты|F6 фильтр|F9|Ins|Del|' ',^A,-,* выдел."

{nextnum.i}

&GLOBAL-DEFINE DRVarCode{&num}     code.code
&GLOBAL-DEFINE DRVarType{&num}     CHAR
&GLOBAL-DEFINE DRVarEFormat{&num}  x(100)
&GLOBAL-DEFINE DRVarViewAs{&num}   FILL-IN SIZE 50 BY 1
&GLOBAL-DEFINE DRVarFormat{&num}   x(13)
&GLOBAL-DEFINE DRVarLabel{&num}    Код!доп.параметра 
&GLOBAL-DEFINE DRVarELabel{&num}   Код 
&GLOBAL-DEFINE DRVarHelp{&num}     Код дополнительного параметра
&GLOBAL-DEFINE DRVarNoUpdate{&num} YES

{nextnum.i}

&GLOBAL-DEFINE DRVarCode{&num}     code.name
&GLOBAL-DEFINE DRVarType{&num}     CHAR
&GLOBAL-DEFINE DRVarEFormat{&num}  x(100)
&GLOBAL-DEFINE DRVarViewAs{&num}   FILL-IN SIZE 50 BY 1
&GLOBAL-DEFINE DRVarFormat{&num}   x(40)
&GLOBAL-DEFINE DRVarLabel{&num}    Наименование!канала
&GLOBAL-DEFINE DRVarELabel{&num}   Наименование
&GLOBAL-DEFINE DRVarHelp{&num}     Наименование канала

{nextnum.i}

&GLOBAL-DEFINE DRVarCode{&num}     code.misc[1]
&GLOBAL-DEFINE DRVarType{&num}     CHAR
&GLOBAL-DEFINE DRVarBFormat{&num}  99/99/9999
&GLOBAL-DEFINE DRVarEFormat{&num}  99/99/9999
&GLOBAL-DEFINE DRVarFormat{&num}   x(10)
&GLOBAL-DEFINE DRVarLabel{&num}    Дата!ввода
&GLOBAL-DEFINE DRVarELabel{&num}   Дата ввода 
&GLOBAL-DEFINE DRVarHelp{&num}     Дата ввода

{nextnum.i}

&GLOBAL-DEFINE DRVarCode{&num}     code.misc[2]
&GLOBAL-DEFINE DRVarType{&num}     CHAR
&GLOBAL-DEFINE DRVarBFormat{&num}  99/99/9999
&GLOBAL-DEFINE DRVarEFormat{&num}  99/99/9999
&GLOBAL-DEFINE DRVarFormat{&num}   x(10)
&GLOBAL-DEFINE DRVarLabel{&num}    Дата!вывода
&GLOBAL-DEFINE DRVarELabel{&num}   Дата вывода 
&GLOBAL-DEFINE DRVarHelp{&num}     Дата вывода

{pclassdr.p}
