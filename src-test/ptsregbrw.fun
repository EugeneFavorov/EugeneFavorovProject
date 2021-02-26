/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2010 ЗАО "Банковские информационные системы"
     Filename: ptsregbrw.fun
      Comment: Движение(регистрация) документов - функции
   Parameters:
         Uses:
      Used by:
      Created: kam 
     Modified:    
*/
&SCOPED-DEFINE SROK_SDACHI "СрокСдачи"
&SCOPED-DEFINE POST_ARCH "ПостАрх"
&SCOPED-DEFINE POST_BANK "ПостБанк"
&SCOPED-DEFINE IZAT_ARCH "ИзъятАрх"

FUNCTION EnableDescription RETURN INTEGER(INPUT nameEv AS CHAR):
    /*IF nameEv = {&POST_ARCH} OR
       nameEv = {&POST_BANK} THEN RETURN 1. */
    IF nameEv = {&IZAT_ARCH} THEN RETURN 2.
    RETURN 0.
END FUNCTION. 
    
FUNCTION NoNull RETURN CHAR(INPUT iChar AS CHARACTER):
    IF iChar = ? THEN iChar = "".
        ELSE iChar = TRIM(iChar).
    RETURN iChar.
END FUNCTION. 

FUNCTION GetFIO RETURN CHAR(INPUT iUser AS CHAR):
    DEFINE VARIABLE FIO AS CHAR NO-UNDO INIT "".
    FIND FIRST _user WHERE _user._userid = iUser NO-LOCK NO-ERROR.
    IF AVAILABLE _user THEN FIO = _user._User-Name.
    ELSE DO:
        FIND FIRST code WHERE code.class EQ 'strahpol' 
            AND code.parent EQ 'strahpol'
            AND code.code = iUser NO-LOCK NO-ERROR.         
        IF AVAILABLE code THEN FIO = code.description[1].
    END.    
    RETURN FIO.
END FUNCTION.    

FUNCTION CheckUser RETURN LOGICAL(INPUT iUser AS CHAR):
    DEFINE VARIABLE sResult AS LOGICAL NO-UNDO.
    FIND FIRST _user WHERE _user._userid = iUser NO-LOCK NO-ERROR.
    IF AVAILABLE _user THEN DO:
        sResult = TRUE.
    END.
    ELSE DO:
        FIND FIRST code WHERE code.class EQ 'strahpol' 
            AND code.parent EQ 'strahpol'
            AND code.code = iUser NO-LOCK NO-ERROR.
        IF AVAILABLE code THEN DO:
            sResult = TRUE.
        END.
        ELSE DO:        
            sResult = FALSE.
            MESSAGE "Не найден пользователь получивший документ (поле Принял)" VIEW-AS ALERT-BOX INFO BUTTONS OK.
        END.
    END.
    RETURN sResult.
END FUNCTION.    

FUNCTION CheckDateValue RETURN LOGICAL(INPUT iDate AS DATE):
    DEF BUFFER bfplindocsreg FOR pl_indocsreg.
    DEF VAR tmpPrgId AS RECID NO-UNDO.
    DEF VAR tmpDate AS DATE NO-UNDO.
    FIND FIRST bfplindocsreg WHERE
        bfplindocsreg.file_name = "term-obl"
        AND bfplindocsreg.surrogate = ContCode 
        AND bfplindocsreg.date_value < iDate
        AND RECID(bfplindocsreg) > RECID(pl_indocsreg) NO-LOCK NO-ERROR.
    IF AVAIL bfplindocsreg THEN DO:
        IF NameEvent <> {&SROK_SDACHI} THEN DO:
            MESSAGE "Существует последующее событие с более ранней датой" VIEW-AS ALERT-BOX TITLE "Ошибка".
            RETURN FALSE.
        END.
        tmpPrgId = RECID(bfplindocsreg). /* это событие ПостБанк */
        FIND FIRST bfplindocsreg WHERE   /* ищем событие после события ПостБанк */
        bfplindocsreg.file_name = "term-obl"
        AND bfplindocsreg.surrogate = ContCode 
        AND bfplindocsreg.date_value < iDate
        AND RECID(bfplindocsreg) > tmpPrgId NO-LOCK NO-ERROR. 
        IF AVAIL bfplindocsreg AND NameEvent <> {&SROK_SDACHI} THEN DO:
            MESSAGE "Существует последующее событие с более ранней датой" VIEW-AS ALERT-BOX TITLE "Ошибка".
            RETURN FALSE.
        END.
    END.
    tmpDate = ?.
    FOR EACH bfplindocsreg WHERE
        bfplindocsreg.file_name = "term-obl"
        AND bfplindocsreg.surrogate = ContCode 
        /* AND bfplindocsreg.date_value > iDate */
        AND RECID(bfplindocsreg) < RECID(pl_indocsreg) NO-LOCK BY RECID(bfplindocsreg) DESCENDING:
        tmpDate = bfplindocsreg.date_value. 
        LEAVE.  
    END.
    IF tmpDate <> ? AND tmpDate > iDate THEN DO:
        IF NameEvent <> {&POST_BANK} THEN DO:
            MESSAGE "Существует предыдущее событие с более поздней датой" VIEW-AS ALERT-BOX TITLE "Ошибка".
            RETURN FALSE.
        END.
        tmpPrgId = RECID(bfplindocsreg). /* это событие СрокСдачи */
        FIND FIRST bfplindocsreg WHERE   /* ищем событие до события СрокСдачи */
        bfplindocsreg.file_name = "term-obl"
        AND bfplindocsreg.surrogate = ContCode 
        AND bfplindocsreg.date_value > iDate
        AND RECID(bfplindocsreg) < tmpPrgId NO-LOCK NO-ERROR. 
        IF AVAIL bfplindocsreg AND NameEvent <> {&POST_BANK} THEN DO:
            MESSAGE "Существует предыдущее событие с более поздней датой" VIEW-AS ALERT-BOX TITLE "Ошибка".
            RETURN FALSE.
        END.
    END.
    
    /*
    FIND FIRST bfplindocsreg WHERE
        bfplindocsreg.file_name = "term-obl"
        AND bfplindocsreg.surrogate = ContCode 
        AND bfplindocsreg.date_value > iDate
        AND RECID(bfplindocsreg) < RECID(pl_indocsreg) NO-LOCK NO-ERROR.
    IF AVAIL bfplindocsreg THEN DO:
        IF NameEvent <> {&POST_BANK} THEN DO:
            
            MESSAGE "Существует предыдущее событие с более поздней датой" VIEW-AS ALERT-BOX TITLE "Ошибка".
            RETURN FALSE.
        END.
        tmpPrgId = RECID(bfplindocsreg). /* это событие СрокСдачи */
        FIND FIRST bfplindocsreg WHERE   /* ищем событие до события СрокСдачи */
        bfplindocsreg.file_name = "term-obl"
        AND bfplindocsreg.surrogate = ContCode 
        AND bfplindocsreg.date_value > iDate
        AND RECID(bfplindocsreg) < tmpPrgId NO-LOCK NO-ERROR. 
        IF AVAIL bfplindocsreg THEN DO:
            MESSAGE "Существует предыдущее событие с более поздней датой" VIEW-AS ALERT-BOX TITLE "Ошибка".
            RETURN FALSE.
        END.
    END.  
    */
    
    
    RETURN TRUE.
END FUNCTION.   
    

FUNCTION CheckNameEvent RETURN LOGICAL(INPUT iNameEvent AS CHARACTER):
    DEF BUFFER bfplindocsreg FOR pl_indocsreg.
    DEFINE BUFFER bloan FOR loan.
    FIND FIRST code WHERE code.class EQ 'СобытРегДокум' AND code.parent EQ 'ПТС' 
        AND code.code = iNameEvent NO-LOCK NO-ERROR.
    IF NOT AVAIL code THEN DO:
        MESSAGE "Нет такого события, выбор по F1" VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN FALSE.
    END.
    tmpChar = code.val.

    tmpEvent = "".
    FOR EACH bfplindocsreg WHERE
        bfplindocsreg.file_name = "term-obl"
        AND bfplindocsreg.surrogate = ContCode 
        AND RECID(bfplindocsreg) < RECID(pl_indocsreg) NO-LOCK
        BY  RECID(bfplindocsreg) DESCENDING :
        tmpEvent = bfplindocsreg.event.
        LEAVE.
    END.
    tmpBool = TRUE.
    IF tmpEvent <> "" THEN DO:
        DO tmpInt = 1 TO NUM-ENTRIES(tmpChar):
            IF INDEX (tmpEvent,ENTRY(tmpInt,tmpChar)) >= 1 THEN tmpBool = FALSE.
        END.
        IF tmpBool THEN DO:
            MESSAGE "Существует предыдущее событие с неподходящим событием" VIEW-AS ALERT-BOX TITLE "Ошибка".
            RETURN FALSE.
        END.
    END.
    ELSE DO:
        tmpBool = TRUE.
        DO tmpInt = 1 TO NUM-ENTRIES(tmpChar):
            IF ENTRY(tmpInt,tmpChar) = "" THEN tmpBool = FALSE.
        END.
        IF tmpBool THEN DO:
            FIND FIRST bloan
                WHERE bloan.contract EQ "Кредит"
                AND bloan.cont-code EQ ENTRY(1, ContCode, " ")
            NO-LOCK NO-ERROR.
            IF AVAILABLE(bloan) THEN
            DO:
                IF bloan.open-date LT DATE("02.06.2015") THEN
                DO:
                    MESSAGE "Нельзя ввести это событие первым" VIEW-AS ALERT-BOX TITLE "Ошибка".
                    RETURN FALSE.
                END.
            END.
        END.
    END.
      
    tmpEvent = "".
    FOR EACH bfplindocsreg WHERE
        bfplindocsreg.file_name = "term-obl"
        AND bfplindocsreg.surrogate = ContCode 
        AND RECID(bfplindocsreg) > RECID(pl_indocsreg) NO-LOCK
        BY  RECID(bfplindocsreg) :
        tmpEvent = bfplindocsreg.event.
        LEAVE.
    END.    
        
    IF tmpEvent <> "" THEN DO:
        FIND FIRST code WHERE code.class EQ 'СобытРегДокум' AND code.parent EQ 'ПТС' 
            AND code.code = tmpEvent NO-LOCK NO-ERROR.
        IF AVAIL code THEN DO:
            IF INDEX (code.val,iNameEvent) < 1 THEN DO:
                MESSAGE "Существует последующее событие с неподходящим событием" VIEW-AS ALERT-BOX TITLE "Ошибка".
                RETURN FALSE.
            END.
        END.
        ELSE DO:
            MESSAGE "Существует последующее событие с неизвестным событием" VIEW-AS ALERT-BOX TITLE "Ошибка".
            RETURN FALSE.
        END.
    END.
    RETURN TRUE.
END FUNCTION. 





