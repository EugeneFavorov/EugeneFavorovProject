/*                                               
    Плюс банк
      Comment: Процедура возвращающая реквизит Код ЦФО в зависимости от параметра
   Parameters: vKodRek: О - Обслуживание, П - привлечение , ПО
         Uses: 
      Used by:
      Created: 16/10/2014 KAU
*/
{globals.i}
DEF INPUT PARAM vKodRek AS CHAR NO-UNDO.
DEF OUTPUT PARAM vRezult AS CHAR NO-UNDO.

DEF VAR vUser       AS CHAR NO-UNDO.
DEF VAR vUserOtd    AS CHAR NO-UNDO.

DEF VAR vCodeObsl   AS CHAR NO-UNDO.
DEF VAR vCodePrivl  AS CHAR NO-UNDO.
DEF VAR vCodePO     AS CHAR NO-UNDO. 

/*определяем имя пользователя запустившего процедуру*/
FIND FIRST _user WHERE 
   _user._userid EQ &IF DEFINED(user-code) &THEN {&user-code} &ELSE USERID("bisquit") &ENDIF
NO-LOCK NO-ERROR.

vUser = _user._userid.
vUserOtd = GetXattrValue("_USER",vUser,"ОТДЕЛЕНИЕ").

FIND FIRST code WHERE code.class EQ 'CFO'
                AND code.val EQ 'ПО'
                AND code.code BEGINS vUserOtd /*TODO*/
NO-LOCK NO-ERROR.
IF AVAIL code THEN vCodePO = code.code.
FIND FIRST code WHERE code.class EQ 'CFO'
                AND code.val EQ 'П'
                AND code.code BEGINS vUserOtd /*TODO*/
NO-LOCK NO-ERROR.
IF AVAIL code THEN vCodePrivl = code.code.
FIND FIRST code WHERE code.class EQ 'CFO'
                AND code.val EQ 'О'
                AND code.code BEGINS vUserOtd /*TODO*/
NO-LOCK NO-ERROR.
IF AVAIL code THEN vCodeObsl = code.code.

IF vKodRek EQ 'П'
THEN DO:
    IF vCodePrivl NE ? AND vCodePrivl NE ''
    THEN vRezult = vCodePrivl.
    ELSE IF vCodePO NE ? AND vCodePO NE ''
    THEN vRezult = vCodePO.
    ELSE MESSAGE "Для вашего отделения: " + STRING(vUserOtd) SKIP 
                 "не найден код цфо в классификаторе CFO" VIEW-AS ALERT-BOX. 
END.
ELSE IF vKodRek EQ 'О'
    THEN DO:
        IF vCodeObsl NE ? AND vCodeObsl NE ''
        THEN vRezult = vCodePrivl.
        ELSE IF vCodePO NE ? AND vCodePO NE ''
        THEN vRezult = vCodePO.
        ELSE MESSAGE "Для вашего отделения: " + STRING(vUserOtd) SKIP
                    "не найден код цфо в классификаторе CFO" VIEW-AS ALERT-BOX. 
    END.
    ELSE IF vKodRek EQ 'ПО'
        THEN DO:
            IF vCodePO NE ? AND vCodePO NE ''
            THEN vRezult = vCodePO.
            ELSE MESSAGE "Для вашего отделения: " + STRING(vUserOtd) SKIP
                        "не найден код цфо в классификаторе CFO" VIEW-AS ALERT-BOX. 
        END. 
        ELSE MESSAGE "в процедуру CfoCreateRek.p передан не правильный параметр возможно только П/О/ПО" VIEW-AS ALERT-BOX.
        