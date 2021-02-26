/*

     Filename: vedprov_r.p
      Comment: Производит расчет таблицы документов дня
   Parameters:
         Uses:
      Used by: vedprov.p, 
      Created: 14.10.2014 KAU
     Modifier: 

*/


DEFINE VARIABLE mTitleSet AS CHARACTER NO-UNDO.
DEFINE VARIABLE mBranch AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mBr AS CHARACTER  NO-UNDO.
DEFINE VARIABLE vBranch AS CHARACTER  NO-UNDO.
DEFINE VARIABLE isCash  AS LOGICAL    NO-UNDO.
DEFINE VARIABLE adb       AS CHARACTER NO-UNDO.
DEFINE VARIABLE acr       AS CHARACTER NO-UNDO.
DEFINE VARIABLE mYes AS LOGICAL    NO-UNDO.
DEFINE VARIABLE mSumm LIKE op-entry.amt-rub  NO-UNDO.
DEFINE VARIABLE mElecSave AS CHARACTER  NO-UNDO.

DEFINE VARIABLE mBalans LIKE op-entry.amt-rub INIT 0.00 NO-UNDO.
DEFINE VARIABLE mVBalans LIKE op-entry.amt-rub INIT 0.00 NO-UNDO.
Define Variable Bank-name        As Character            No-Undo.
Define Variable vDateString      As Character            No-Undo.
DEF VAR     vUsId       like op.user-id     no-undo.
DEF VAR     zUsId       like _user._userid  no-undo.
DEF VAR     zUserName   AS CHAR         NO-UNDO.
DEF VAR     zUserDol    AS CHAR         NO-UNDO.
DEF VAR     cUser       AS CHAR         NO-UNDO.
DEFINE VARIABLE mItog       AS DECIMAL NO-UNDO.
DEFINE VARIABLE mItogbp       AS DECIMAL NO-UNDO.
DEFINE VARIABLE mItogop       AS DECIMAL NO-UNDO.
DEFINE VARIABLE mItogbe       AS DECIMAL NO-UNDO.
DEFINE VARIABLE mItogoe       AS DECIMAL NO-UNDO.
def var fname as char no-undo.

DEFINE BUFFER xop-entry FOR op-entry.
DEFINE BUFFER xop FOR op.
DEFINE BUFFER   TytleCode FOR code.

/*
DEF OUTPUT PARAM TABLE FOR tt-op-day.
DEF OUTPUT PARAM OpDate AS DATE NO-UNDO.
*/

/*run getdate.p(" ", OUTPUT end-date). */
 {getdate.i}
/*OpDate = end-date.*/

FIND FIRST _user WHERE 
   _user._userid EQ &IF DEFINED(user-code) &THEN {&user-code} &ELSE USERID("bisquit") &ENDIF
NO-LOCK NO-ERROR.
/*find first _user where _user._userid = user.*/

/* возможность вводить имя пользователя
pause 0.

Do on error undo, leave on endkey undo, leave with frame ftune:
  Update
    zUsId label "Код пользователя" help "Введите имя пользователя для отчета"   
  with centered row 10 overlay side-labels 1 col
  title "[ Параметры Отчета ]".
End.
Hide frame ftune no-pause.

if LASTKEY EQ KEYCODE("ESC") THEN
    return.

find first _user where _user._userid begins zUsId no-lock no-error.
*/


zUsId = _user._userid.
zUserName = _user._user-name.
zUserDol = getxattrvalue ("_user",_user._userid,"Должность").




FIND FIRST CODE 
    WHERE 
    CODE.class EQ "TitulBr"
    AND code.PARENT EQ "TitulBr"
    AND CODE.CODE EQ ipCity
     NO-LOCK NO-ERROR.
IF NOT AVAILABLE(CODE) THEN
    MESSAGE "No param otc"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
ELSE
    mBranch = CODE.val.
FIND FIRST branch WHERE branch.branch-id EQ mBranch
     NO-LOCK NO-ERROR.
IF AVAILABLE(branch) THEN
DO:
    vBranch = mBranch.
    FOR EACH branch NO-LOCK
        WHERE branch.parent-id EQ vBranch
        BY Branch.branch-id:
        mBranch = mBranch + "," + Branch.branch-id.
    END.
END.
        mBranch = mBranch + ",ЦОУ".
    
{op-cash.def}

FOR EACH op NO-LOCK
    WHERE
    op.op-date EQ end-date
    AND op.op-status BEGINS "√"
    AND CAN-DO(mBranch, op.branch-id):
    FOR EACH op-entry NO-LOCK
        WHERE op-entry.op EQ op.op
        AND op-entry.op-date EQ end-date
        :
        ASSIGN
        adb = op-entry.acct-db
        acr = op-entry.acct-cr.
        IF op.branch-id EQ "ЦОУ" THEN 
            mBr = SUBSTRING(adb, 10, 4) .
        ELSE 
            mBr = op.branch-id.
        
        FIND FIRST CODE 
            WHERE 
            CODE.class EQ "TitulBr"
            AND code.PARENT EQ "TitulBr"
            AND CAN-DO(CODE.val, mBr)
             NO-LOCK NO-ERROR.
        IF NOT AVAILABLE(CODE) THEN 
            MESSAGE "no city"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

        {op-cash.i}
    vUsId = op.user-id.
    IF vUsId BEGINS 'SERV' THEN 
        DO:
            /*kau modif 1*/
            IF NOT op.user-inspector BEGINS 'SERV'
                AND NOT op.user-inspector EQ ''
                AND NOT op.user-inspector EQ ?
            THEN vUsId = op.user-inspector.
            ELSE 
            /*end kau modif 1*/
            DO:
                IF op-entry.acct-cr BEGINS "4" THEN
                    FIND FIRST acct WHERE acct.acct = op-entry.acct-cr NO-LOCK.
                ELSE IF op-entry.acct-db BEGINS "4" THEN
                    FIND FIRST acct WHERE acct.acct = op-entry.acct-db NO-LOCK.
                IF AVAIL acct THEN vUsId = acct.user-id.
            END.
        END.
        CREATE tt-op-day.
        ASSIGN
            tt-op-day.op-cid = RECID(op)
            tt-op-day.ope-cid = RECID(op-entry)
            tt-op-day.city-id = INTEGER(TRIM(CODE.CODE))
            tt-op-day.city = CODE.NAME
            tt-op-day.kko = mBr
            tt-op-day.currency = IF op-entry.currency NE "" AND op-entry.currency NE ? THEN "VAL" ELSE "810"
            tt-op-day.acct-cat = op.acct-cat
            tt-op-day.razdel = "b"
            tt-op-day.acct-db = substring(op-entry.acct-db,1,20)
            tt-op-day.acct-cr = substring(op-entry.acct-cr,1,20)
            tt-op-day.doc-num = op.doc-num
            tt-op-day.amt-rub = op-entry.amt-rub
            tt-op-day.save-type = "p"
        tt-op-day.user-id = vUsId
        tt-op-day.op = op.op
        tt-op-day.op-kind = op.op-kind
        tt-op-day.doc-date = op.doc-date
            .

        IF isCash THEN
            tt-op-day.razdel = "k".
        mElecSave = FGetSetting("elec-type", "",   "01КЛ,01МБ,017,09*,06*,016,ВБО").
       IF CAN-DO(mElecSave, op.doc-type) THEN
           ASSIGN
           tt-op-day.save-type = "e".
        mYes = TRUE.    
    FIND FIRST TytleCode WHERE TytleCode.class EQ "TitTranz" 
            and TytleCode.code EQ op.op-kind NO-LOCK NO-ERROR.
    IF AVAIL tytlecode THEN DO:
    tt-op-day.save-type = TytleCode.val.
    END.
           FOR EACH TytleCode WHERE TytleCode.class EQ "PaperAcct" NO-LOCK:
              IF CAN-DO(TytleCode.code,op-entry.acct-db) AND mYes THEN 
              DO:
                  IF CAN-DO(TytleCode.val,op-entry.acct-cr) AND mYes THEN
                  DO:
                      ASSIGN
                      tt-op-day.save-type = "p".
                      mYes = FALSE.   
                  END.
              END.
           END.                                                                  

    END.
END.