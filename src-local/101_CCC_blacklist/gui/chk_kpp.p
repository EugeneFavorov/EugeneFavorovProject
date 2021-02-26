DEF INPUT  PARAM in-cust-id AS CHAR   NO-UNDO.   /* Суррогат клиента */
DEF INPUT  PARAM in-param   AS CHAR   NO-UNDO.   /* Значение редактируемого ДР */
/*
message
in-cust-id skip
in-param  
view-as alert-box.
*/
{globals.i}             /* Глобальные переменные сессии. */
{intrface.get xclass}   /* Библиотека инструментов метасхемы. */
{intrface.get tmess}
{intrface.get strng}      /* Инструменты для работы со строками  */

DEFINE VARIABLE vTmpStr   AS CHAR NO-UNDO.
DEFINE VARIABLE vRes      AS LOG  NO-UNDO.
DEFINE VARIABLE mProvKPP  AS CHARACTER NO-UNDO.

mProvKPP = FGetSetting("Проверки","ВклПровКПП","").

IF mProvKPP <> "Да" THEN
   RETURN.

IF LENGTH(in-param) <> 9 THEN 
DO:
   RUN Fill-SysMes IN h_tmess ("", "", "0", "Внимание! Количество символов КПП должно быть 9.").         
   RETURN ERROR.
END.

/*
Структура КПП: NNNNРРХХХ
NNNN (4 знака) - код налогового органа, значит необходимо производить сверку с классификатором НИ, на наличие там такой записи.
PP (2 знака) - причина постановки на учет (учета сведений). Символ P представляет собой цифру или заглавную букву латинского алфавита от A до Z. 
XXX (3 знака) - порядковый номер постановки на учет (учета сведений) в налоговом органе по соответствующему основанию. Цифры от 0 до 9
*/

vTmpStr = SUBSTR(in-param,1,4).         /*NNNN (4 знака)*/
IF GetCode("НИ",vTmpStr) = "" OR GetCode("НИ",vTmpStr) =  ? THEN
DO:
   RUN Fill-SysMes IN h_tmess ("", "", "0", "Внимание! Нарушена структура КПП.").         
   RETURN ERROR.
END.

vTmpStr = SUBSTR(in-param,5,2).         /*PP (2 знака)*/
RUN Check-Ascii-Set(INPUT vTmpStr,INPUT "3,4",INPUT "", OUTPUT vRes).
IF vRes THEN 
DO:
   RUN Fill-SysMes IN h_tmess ("", "", "0", "Внимание! Нарушена структура КПП.").         
   RETURN ERROR.
END.

vTmpStr = SUBSTR(in-param,7).         /*XXX (3 знака) */
RUN Check-Ascii-Set(INPUT vTmpStr,INPUT "3",INPUT "", OUTPUT vRes).
IF vRes THEN 
DO:
   RUN Fill-SysMes IN h_tmess ("", "", "0", "Внимание! Нарушена структура КПП.").         
   RETURN ERROR.
END.
                           /*










                             */