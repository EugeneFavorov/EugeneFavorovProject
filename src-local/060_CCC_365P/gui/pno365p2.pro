&IF DEFINED(PNO365P2_PRO_) = 0 &THEN

&GLOBAL-DEFINE PNO365P2_PRO_ YES

{globals.i}
{intrface.get strng}
{intrface.get xclass}
{intrface.get import}

FUNCTION GetDateOpEntry RETURNS DATE (INPUT iDays AS INT64,
                                          INPUT iDayOpEntry AS DATE).
     DEF VAR ii AS INT64 INIT 0 NO-UNDO.
     DEF VAR Res AS DATE NO-UNDO.
     DEF VAR EndOfTime AS DATE NO-UNDO.
     DEF BUFFER bop-date FOR op-date.
     
     IF iDays EQ 0 THEN Res = iDayOpEntry. /* без смещения */
     ELSE DO:
         FIND FIRST op-date WHERE op-date.op-date EQ iDayOpEntry NO-LOCK NO-ERROR.
         Res = op-date.op-date. /* день zero */
         FIND LAST bop-date NO-LOCK NO-ERROR.
         EndOfTime  = bop-date.op-date. /* последний день БД */
         ii = iDays. /* смещение в днях */
         DO WHILE ii GT 0:
             FIND NEXT op-date NO-LOCK NO-ERROR.
             IF AVAIL op-date THEN DO:
                 IF op-date.op-date EQ EndOfTime THEN ii = 0. /* конец времен */
                 ELSE ii = ii - 1.
                 Res = op-date.op-date.
             END.
             ELSE ii = ii - 1. 
         END.
     END.
     RETURN Res.
END FUNCTION.

PROCEDURE CreateOpPNO.
    DEFINE INPUT  PARAMETER iHExch  AS   HANDLE     NO-UNDO.
    DEFINE INPUT  PARAMETER iOpDate LIKE op.op-date NO-UNDO.
    DEFINE OUTPUT PARAMETER oOp     LIKE op.op      NO-UNDO.

    DEFINE BUFFER op         FOR op.
    DEFINE BUFFER op-entry   FOR op-entry.
    DEFINE BUFFER op-bank    FOR op-bank.
    DEFINE BUFFER acct       FOR acct.
    DEFINE BUFFER banks      FOR banks.
    DEFINE BUFFER banks-corr FOR banks-corr.

    DEFINE VARIABLE vOpSurr     AS   CHARACTER        NO-UNDO.
    DEFINE VARIABLE vStrTmp     AS   CHARACTER        NO-UNDO.
    DEFINE VARIABLE vDateTmp    AS   DATE             NO-UNDO.
    DEFINE VARIABLE vImportUser AS   CHARACTER        NO-UNDO.
    DEFINE VARIABLE vDateShift  AS   INT64            NO-UNDO.
    DEFINE VARIABLE vOpDate     LIKE op.op-date       NO-UNDO.
    DEFINE VARIABLE vAmtRub     LIKE op-entry.amt-rub NO-UNDO.
    
    vDateShift = INT(mShift_365).
    vOpDate = GetDateOpEntry(vDateShift,iOpDate).
    iOpDate = vOpDate. /* переопределим дату */
    RUN CreateObject365p("365-pno", iOpDate, OUTPUT vOpSurr) NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN DO:
        FIND FIRST op WHERE
            op.op = INT64(vOpSurr)
        EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
        IF AVAILABLE op THEN DO:
            FIND FIRST op-entry OF op EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
            FIND FIRST op-bank  OF op EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
            IF AVAILABLE op-entry AND AVAILABLE op-bank THEN DO:
                {365p.get &obj = "iHExch"
                          &fld = "НомСчПл"
                          &val = "op-entry.acct-db"}
                {find-act.i &acct = op-entry.acct-db}
                IF NOT AVAILABLE acct OR {assigned acct.currency} THEN
                    op-entry.acct-db = mAcNSpis.
                ELSE IF  AVAILABLE acct AND NOT {assigned acct.currency} THEN
                   UpdateSigns(op.class-code, vOpSurr, "acct-send", op-entry.acct-db, ?).

                {365p.get &obj = "iHExch"
                          &fld = "ИНННП"
                          &val = "vStrTmp"}
                UpdateSigns(op.class-code, vOpSurr, "inn-send", vStrTmp, ?).
                {365p.get &obj = "iHExch"
                          &fld = "Плательщ"
                          &val = "vStrTmp"}
                UpdateSigns(op.class-code, vOpSurr, "name-send", vStrTmp, ?).
                {365p.get &obj = "iHExch"
                          &fld = "КодПл"
                          &val = "vStrTmp"}
                UpdateSigns(op.class-code, vOpSurr, "УИН", vStrTmp, ?).
               
                RUN GetCorrAcct(OUTPUT op-entry.acct-cr).
                RUN GetDecAttrValue365p(iHExch, "СумПоруч", OUTPUT vAmtRub) NO-ERROR.
                IF ERROR-STATUS:ERROR THEN
                    RETURN ERROR RETURN-VALUE.
                op-entry.amt-rub = vAmtRub / 100.0.

                {365p.get &obj = "iHExch"
                          &fld = "БанкПол"
                          &val = "op-bank.bank-name"}
                op-bank.bank-code-type = "МФО-9".
                {365p.get &obj = "iHExch"
                          &fld = "БИКБПол"
                          &val = "op-bank.bank-code"}
                FIND FIRST banks-corr WHERE
                    banks-corr.bank-corr = INT64(op-bank.bank-code) AND
                    CAN-FIND(FIRST banks OF banks-corr WHERE banks.flag-rkc)
                NO-LOCK NO-ERROR.
                IF AVAILABLE banks-corr THEN
                    op-bank.corr-acct = banks-corr.corr-acct.

                {365p.get &obj = "iHExch"
                          &fld = "НомПоруч"
                          &val = "op.doc-num"}
                {365p.get &obj = "iHExch"
                          &fld = "ДатаПоруч"
                          &val = "op.doc-date"}
                {365p.get &obj = "iHExch"
                          &fld = "ИННПол"
                          &val = "op.inn"}
                {365p.get &obj = "iHExch"
                          &fld = "НаимПол"
                          &val = "op.name-ben"}
                {365p.get &obj = "iHExch"
                          &fld = "НомСчПол"
                          &val = "op.ben-acct"}
                {365p.get &obj = "iHExch"
                          &fld = "ОчерПл"
                          &val = "op.order-pay"}
                {365p.get &obj = "iHExch"
                          &fld = "НазнПл"
                          &val = "op.details"}
                {365p.get &obj = "iHExch"
                          &fld = "НомПорВал"
                          &val = "vStrTmp"}
                UpdateSigns(op.class-code, vOpSurr, "НомПорВал", vStrTmp, ?).
                {365p.get &obj = "iHExch"
                          &fld = "ДатаПорВал"
                          &val = "vDateTmp"}
                UpdateSigns(op.class-code, vOpSurr, "ДатаПоруч", STRING(vDateTmp, "99/99/9999"), ?).
                {365p.get &obj = "iHExch"
                          &fld = "НомВалСч"
                          &val = "vStrTmp"}
                          
               vImportUser = TRNSettingValue ("","ImportUser","ImportUser").
               op.user-id = GetUserImportEx(INPUT INT64(iHExch::mail-user-num),
                                            INPUT vImportUser,
                                            INPUT op-entry.acct-db,                                                              
                                            INPUT USERID("BISQUIT"),
                                            INPUT "",
                                            INPUT op-entry.acct-db,
                                            INPUT op-entry.acct-cr).

                /* -------------------------------------------------------
                   0163411 Просьба при загрузке PNO всегда устанавливать
                           Op.due-date      = Op.op-date
                           Op.contract-date = Op.op-date
                -------------------------------------------------------- */
                op.due-date = op.op-date.
                op.contract-date = op.op-date.
                
                UpdateSigns(op.class-code, vOpSurr, "НомВалСч", vStrTmp, ?).
                RUN FillOpTaxDetails365p(iHExch, BUFFER op) {&RAISE-ERROR}.
                VALIDATE op NO-ERROR.
                IF ERROR-STATUS:ERROR THEN
                    RETURN ERROR.
                VALIDATE op-entry NO-ERROR.
                IF ERROR-STATUS:ERROR THEN
                    RETURN ERROR.
                oOp = op.op.
                RELEASE op.
                RELEASE op-entry.
                RELEASE op-bank.
                RETURN.
            END.
        END.
    END.
    RETURN ERROR.
END PROCEDURE.

&ENDIF /* PNO365P2_PRO_ */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='23/07/2015 12:38:33.675+04:00' */
/* $LINTUSER='ahra' */
/* $LINTMODE='1' */
/* $LINTFILE='pno365p2.pro' */
/*prosignFo11sSe1cskYrCEU5rY+Jw*/