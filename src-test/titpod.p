/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2010 ЗАО "Банковские информационные системы"
     Filename: title_doc.p
      Comment: Классификатор "ТитулДокДня" Настройка титульного листа сшива документов дня
   Parameters:
         Uses:
      Used by:
      Created: 14.02.2011 kraa 0130908 
     Modified: 
*/
&GLOBAL-DEFINE PClassDRDynamic YES
&GLOBAL-DEFINE SpecClass       "ТитПодразд"

{nextnum.i}

&GLOBAL-DEFINE DRVarCode{&num}     code.code
&GLOBAL-DEFINE DRVarType{&num}     CHAR
&GLOBAL-DEFINE DRVarEFormat{&num}  x(10)
&GLOBAL-DEFINE DRVarFormat{&num}   x(10)
&GLOBAL-DEFINE DRVarLabel{&num}    Код подр.
&GLOBAL-DEFINE DRVarELabel{&num}   Код подразделен.
&GLOBAL-DEFINE DRVarHelp{&num}     Код подразделения

{nextnum.i}
&GLOBAL-DEFINE DRVarCode{&num}     code.name
&GLOBAL-DEFINE DRVarType{&num}     CHAR
&GLOBAL-DEFINE DRVarEFormat{&num}  x(100)
&GLOBAL-DEFINE DRVarViewAs{&num}   FILL-IN SIZE 50 BY 1
&GLOBAL-DEFINE DRVarFormat{&num}   x(40)
&GLOBAL-DEFINE DRVarLabel{&num}    Наименование
&GLOBAL-DEFINE DRVarHelp{&num}     Наименование подразделения

{nextnum.i}

&GLOBAL-DEFINE DRVarCode{&num}     code.val
&GLOBAL-DEFINE DRVarType{&num}     CHAR
&GLOBAL-DEFINE DRVarEFormat{&num}  x(100)
&GLOBAL-DEFINE DRVarViewAs{&num}   FILL-IN SIZE 45 BY 1
&GLOBAL-DEFINE DRVarFormat{&num}   x(40)
&GLOBAL-DEFINE DRVarLabel{&num}    Сшив
&GLOBAL-DEFINE DRVarHelp{&num}     Сшивы, которые необходимо печатать для подразделения
 
{nextnum.i}

&GLOBAL-DEFINE DRVarCode{&num}     code.misc[1]
&GLOBAL-DEFINE DRVarType{&num}     CHAR
&GLOBAL-DEFINE DRVarEFormat{&num}  x(1)
&GLOBAL-DEFINE DRVarFormat{&num}   x(1)
&GLOBAL-DEFINE DRVarLabel{&num}    Порядок
&GLOBAL-DEFINE DRVarHelp{&num}     Порядок сортировки

{nextnum.i}

&GLOBAL-DEFINE DRVarCode{&num}     code.misc[2]
&GLOBAL-DEFINE DRVarType{&num}     LOGICAL
&GLOBAL-DEFINE DRVarFormat{&num}   Да/Нет
&GLOBAL-DEFINE DRVarLabel{&num}    Печать
&GLOBAL-DEFINE DRVarHelp{&num}     Печатать ли подразделение в собственном титульном листе

{nextnum.i}

&GLOBAL-DEFINE DRVarCode{&num}     code.misc[3]
&GLOBAL-DEFINE DRVarType{&num}     CHAR
&GLOBAL-DEFINE DRVarEFormat{&num}  x(10)
&GLOBAL-DEFINE DRVarFormat{&num}   x(10)
&GLOBAL-DEFINE DRVarLabel{&num}    Филиал
&GLOBAL-DEFINE DRVarELabel{&num}   Филиал
&GLOBAL-DEFINE DRVarHelp{&num}     Филиал, к которому относится подразделение

{pclassdr.p}
