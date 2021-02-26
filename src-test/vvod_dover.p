/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2010 ЗАО "Банковские информационные системы"
     Filename: 
      Comment: 
   Parameters:  
      Created: fev
*/

                                                     

{globals.i}
{setdest.i}
{chkacces.i}
{sh-defs.i}
{intrface.get xclass}
{intrface.get instrum}
{flt-val.i}
{wordwrap.def}
{intrface.get tmess}
{tmprecid.def}

DEF VAR FIO         AS CHAR form "x(58)".
DEF VAR Dover       AS CHAR form "x(12)".
DEF VAR DataDover   AS DATE FORMAT "99/99/9999".
DEF VAR DataDoverS  AS CHAR.

DEF BUFFER bsigns   FOR signs.
DEF BUFFER bbsigns  FOR signs.
DEF BUFFER bbbsigns FOR signs.

pause 0.

form
    FIO   label "ФИО сотрудника"
    Dover label "Номер довер-сти"
DataDover label "Дата довер-сти"

with frame www overlay side-labels 1 col centered row 6 title color bright-white
"[ " + "ВВЕДИТЕ ДАННЫЕ" + " ]" width 78.

do on endkey undo, return on error undo, retry with frame www:
display FIO Dover DataDover.
set 
 FIO
 Dover
 DataDover
editing:
readkey.
apply lastkey.
end.
end.
do on endkey undo, leave on error undo, leave with frame prn:

{setdest.i &col=170}

FIND FIRST bbbsigns WHERE bbbsigns.file-name = '_user'
                      AND bbbsigns.code        = 'ФИОП'
                      AND bbbsigns.xattr-value = FIO
     NO-LOCK NO-ERROR.

/*MESSAGE string(bbbsigns.surrogate) view-as alert-box.*/

IF AVAIL bbbsigns
THEN DO:
         FIND FIRST bbsigns
         WHERE bbsigns.file-name = '_user'
         AND bbsigns.code        = 'tab-no'
         AND bbsigns.surrogate   = bbbsigns.surrogate
         NO-LOCK NO-ERROR.

         IF bbsigns.code-value <> "0" OR bbsigns.code-value <> ""
         THEN MESSAGE "Пользователь не найден!" view-as alert-box.

         FOR EACH bsigns WHERE bsigns.file-name  = '_user'
                           AND bsigns.code       = 'tab-no'
                           AND bsigns.code-value = bbsigns.code-value
                           AND bsigns.code-value <> "0"
                           AND bsigns.code-value <> ""
                         NO-LOCK:

                         IF GetXattrValueEx("_user", bsigns.surrogate, "Blocked", "") = "Не блокирован"
                         THEN DO:
                                  /*MESSAGE string(bsigns.surrogate) view-as alert-box.*/
                      
                                  FOR EACH signs WHERE signs.file-name = '_user'
                                                   AND signs.surrogate = bsigns.surrogate
                                                 EXCLUSIVE-LOCK:
                 
                                                 IF signs.code = 'ДокОснДата'
                                                 THEN DO:
                                                          /*MESSAGE string(signs.surrogate) view-as alert-box.*/
                                                          /*DataDoverS = substring(string(DataDover),1,2) + '.' + substring(string(DataDover),4,2) + '.20' + substring(string(DataDover),7,2).*/
                                                          signs.code-value = string(date(DataDover), "99/99/9999").
                                                          signs.code-value = date(DataDover).
                                                          PUT UNFORMATTED "-------------------------" skip.
                                                          PUT UNFORMATTED "ПОЛЬЗОВАТЕЛЬ: " signs.surrogate " " skip.
                                                          PUT UNFORMATTED "ДОП.РЕКВИЗИТ: " signs.code + "  = " + signs.code-value skip.
                                                 END.

                                                 IF signs.code = 'ДокОснНомер'
                                                 THEN DO: 
                                                          signs.xattr-value = Dover.
                                                          PUT UNFORMATTED "ДОП.РЕКВИЗИТ: " signs.code " = " + signs.xattr-value skip.
                                                 END.

                                                 IF signs.code = 'ДокОснТипРП'
                                                 THEN DO:
                                                          signs.xattr-value = 'доверенности № ' + string(Dover) + ' от ' + string(date(DataDover), "99.99.9999") + ' года'.
                                                          PUT UNFORMATTED "ДОП.РЕКВИЗИТ: " signs.code " = " + signs.xattr-value skip.
                                                          PUT UNFORMATTED "-------------------------" skip.
                                                          PUT UNFORMATTED skip.
                                                 END.
                                  END.
                         END.
         END.
END.
ELSE MESSAGE "Пользователь не существует или заблокирован!" view-as alert-box.

{preview.i &col=170}

END.