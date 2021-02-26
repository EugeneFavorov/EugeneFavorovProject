/*
               Банковская интегрированная система QBIS
    Copyright: (C) 1992-2015 ЗАО "Банковские информационные системы"
     Filename: brw446psym.p
      Comment: Браузер классификатора "ЦБ446-Симв"
   Parameters: Нет
      Created: 17.09.2015 soav
     Modified: 16.10.2015 krok
     Modified: <date> <who>
*/

&GLOBAL-DEFINE PClassDRAutoAssignCodeCode YES
&GLOBAL-DEFINE startup                    sign_ank.st~032
&GLOBAL-DEFINE mylookup                   sign_ank.nau
&GLOBAL-DEFINE PClassDRFormTrgFile        sign_ank.trg
&GLOBAL-DEFINE sort-oqry0                 BY code.val


{intrface.get xclass} 

/*
{nextnum.i}
&GLOBAL-DEFINE DRVarCode{&num}     code.misc[1]
&GLOBAL-DEFINE DRVarType{&num}     CHARACTER
&GLOBAL-DEFINE DRVarFormat{&num}   x(4)
&GLOBAL-DEFINE DRVarEFormat{&num}  x(4)
&GLOBAL-DEFINE DRVarBFormat{&num}  x(4)
&GLOBAL-DEFINE DRVarViewAs{&num}   FILL-IN SIZE 4 BY 1
&GLOBAL-DEFINE DRVarBViewAs{&num}  FILL-IN SIZE 8 BY 1
&GLOBAL-DEFINE DRVarLabel{&num}    Код!подраз.
&GLOBAL-DEFINE DRVarELabel{&num}   Код подразделения
&GLOBAL-DEFINE DRVarColLabel{&num} Код
&GLOBAL-DEFINE DRVarHelp{&num}     Код

&GLOBAL-DEFINE DRVarViewAs{&num}   EDITOR INNER-CHARS 32 INNER-LINES 4
&GLOBAL-DEFINE DRVarBViewAs{&num}  FILL-IN SIZE 25 BY 1
*/

{nextnum.i}
&GLOBAL-DEFINE DRVarCode{&num}     code.val
&GLOBAL-DEFINE DRVarType{&num}     CHARACTER
&GLOBAL-DEFINE DRVarFormat{&num}   x(4)
&GLOBAL-DEFINE DRVarEFormat{&num}  x(4)
&GLOBAL-DEFINE DRVarBFormat{&num}  x(4)
&GLOBAL-DEFINE DRVarViewAs{&num}   FILL-IN SIZE 4 BY 1
&GLOBAL-DEFINE DRVarBViewAs{&num}  FILL-IN SIZE 8 BY 1
&GLOBAL-DEFINE DRVarLabel{&num}    Код подразделения
&GLOBAL-DEFINE DRVarELabel{&num}   Код подразделения
&GLOBAL-DEFINE DRVarColLabel{&num} Код
&GLOBAL-DEFINE DRVarHelp{&num}     Код

{nextnum.i}
&GLOBAL-DEFINE DRVarCode{&num}     code.misc[1]
&GLOBAL-DEFINE DRVarType{&num}     CHARACTER
&GLOBAL-DEFINE DRVarFormat{&num}   99/99/9999
&GLOBAL-DEFINE DRVarEFormat{&num}  99/99/9999
&GLOBAL-DEFINE DRVarBFormat{&num}  99/99/9999
&GLOBAL-DEFINE DRVarViewAs{&num}   FILL-IN SIZE 10 BY 1
&GLOBAL-DEFINE DRVarBViewAs{&num}  FILL-IN SIZE 12 BY 1
&GLOBAL-DEFINE DRVarLabel{&num}    Дата!начала
&GLOBAL-DEFINE DRVarELabel{&num}   Дата начала
&GLOBAL-DEFINE DRVarColLabel{&num} Дата начала
&GLOBAL-DEFINE DRVarHelp{&num}     Дата начала

{nextnum.i}
&GLOBAL-DEFINE DRVarCode{&num}     code.misc[2]
&GLOBAL-DEFINE DRVarType{&num}     CHARACTER
&GLOBAL-DEFINE DRVarFormat{&num}   99/99/9999
&GLOBAL-DEFINE DRVarEFormat{&num}  99/99/9999
&GLOBAL-DEFINE DRVarBFormat{&num}  99/99/9999
&GLOBAL-DEFINE DRVarViewAs{&num}   FILL-IN SIZE 10 BY 1
&GLOBAL-DEFINE DRVarBViewAs{&num}  FILL-IN SIZE 12 BY 1
&GLOBAL-DEFINE DRVarLabel{&num}    Дата окончания
&GLOBAL-DEFINE DRVarELabel{&num}   Дата окончания
&GLOBAL-DEFINE DRVarColLabel{&num} Дата окончания
&GLOBAL-DEFINE DRVarHelp{&num}     Дата окончания


{nextnum.i}
&GLOBAL-DEFINE DRVarCode{&num}     code.misc[3]
&GLOBAL-DEFINE DRVarType{&num}     CHARACTER
&GLOBAL-DEFINE DRVarFormat{&num}   x(300)
&GLOBAL-DEFINE DRVarEFormat{&num}  x(300)
&GLOBAL-DEFINE DRVarBFormat{&num}  x(300)
&GLOBAL-DEFINE DRVarViewAs{&num}   FILL-IN SIZE 23 BY 1
&GLOBAL-DEFINE DRVarBViewAs{&num}  FILL-IN SIZE 23 BY 1
&GLOBAL-DEFINE DRVarLabel{&num}    Должность
&GLOBAL-DEFINE DRVarELabel{&num}   Должность
&GLOBAL-DEFINE DRVarColLabel{&num} Должность
&GLOBAL-DEFINE DRVarHelp{&num}     Должность

{nextnum.i}
&GLOBAL-DEFINE DRVarCode{&num}     code.name
&GLOBAL-DEFINE DRVarType{&num}     CHARACTER
&GLOBAL-DEFINE DRVarFormat{&num}   x(70))
&GLOBAL-DEFINE DRVarEFormat{&num}  x(70)
&GLOBAL-DEFINE DRVarBFormat{&num}  x(70)
&GLOBAL-DEFINE DRVarViewAs{&num}   FILL-IN SIZE 20 BY 1
&GLOBAL-DEFINE DRVarBViewAs{&num}  FILL-IN SIZE 27 BY 1
&GLOBAL-DEFINE DRVarLabel{&num}    Сотрудник                
&GLOBAL-DEFINE DRVarELabel{&num}   Сотрудник            
&GLOBAL-DEFINE DRVarColLabel{&num} Сотрудник            
&GLOBAL-DEFINE DRVarHelp{&num}     Сотрудник           


DEFINE VARIABLE mSBalAcct   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mFrameIndex AS INT64     NO-UNDO.

{pclassdr.p}

RETURN.
/*
DEFINE VARIABLE ms       AS CHARACTER          NO-UNDO.
  
    ms  = GetCodeMisc("Подписант_ДБС", "0000", 1).
 MESSAGE ms VIEW-AS ALERT-BOX .
*/


FINALLY:
   {intrface.del}
END FINALLY.
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='22/12/2015 18:35:57.715+04:00' */
/* $LINTUSER='krok' */
/* $LINTMODE='1' */
/* $LINTFILE='brw446psym.p' */
/*prosignNR8pPp1hn/55UI/OPSusqw*/