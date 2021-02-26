{globals.i}
DEFINE INPUT PARAMETER iClass AS character NO-UNDO.
DEFINE INPUT PARAMETER iCat   AS character NO-UNDO.
DEFINE INPUT PARAMETER iSurr  AS character NO-UNDO.
DEFINE INPUT PARAMETER iVal   AS character NO-UNDO.


/*MESSAGE iVal pick-value GetTempXAttrValueEx(iClass,iSurr, "ДатаОбнАнкеты",TODAY,"") VIEW-AS ALERT-BOX .
*/
if (date(iVal) >= (today - 365)) /*and (date(GetTempXAttrValueEx(iClass,iSurr, "ДатаОбнАнкеты",TODAY,"")) lt (today - 365))*/ then   
do:   
    FIND FIRST code WHERE 
             code.class          = 'StopList'
        and  code.parent         = 'StopList'
        and  code.name           = 'OSK'
        and  code.misc[2]       EQ iSurr
        and  code.misc[1]       EQ iCat 
        EXCLUSIVE-LOCK NO-ERROR.
 
    IF AVAIL(CODE) THEN 
        DELETE Code NO-ERROR. {&ON-ERROR}
end.

