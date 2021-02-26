
/* +++ spec365p.pro was humbly modified by (c)blodd converter v.1.09 on 10/16/2015 7:13am +++ */

/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2012 ЗАО "Банковские информационные системы"
     Filename: spec365p.pro
      Comment: Положение 365-П. Функциональность спецверсий.
   Parameters:
      Created: 01.03.2012 krok
     Modified: <date> <who>
*/

&IF DEFINED(SPEC365P_PRO_) = 0 &THEN

&GLOBAL-DEFINE SPEC365P_PRO_ YES

{globals.i}
{intrface.get op}
{details.i}

FUNCTION getAcctNumber RETURN CHARACTER (INPUT iAcct AS CHARACTER):
    RETURN DelFilFromAcct(iAcct).
END FUNCTION.

PROCEDURE GetThisFilialId.
    DEFINE OUTPUT PARAMETER oFilialId AS CHARACTER NO-UNDO.

    oFilialId = shFilial.
END PROCEDURE.

PROCEDURE GetDocTypeDigital.
    DEFINE INPUT  PARAMETER iDocType LIKE doc-type.doc-type NO-UNDO.
    DEFINE OUTPUT PARAMETER oDigital LIKE doc-type.digital  NO-UNDO.

    RUN GetDocTypeDigital IN h_op (iDocType, ?, OUTPUT oDigital).
END PROCEDURE.

PROCEDURE GetOpDetails365p.
    DEFINE PARAMETER BUFFER op       FOR  op.
    DEFINE OUTPUT PARAMETER oDetails LIKE op.details NO-UNDO.

    DEFINE VARIABLE vI AS INT64 NO-UNDO.
    DEFINE VARIABLE vS AS CHARACTER NO-UNDO.

    RUN GetOpDetails(BUFFER op, OUTPUT oDetails) NO-ERROR.
    IF {assigned oDetails} THEN DO:
        vS = mZamRVSO_365.
        CASE vS:
            WHEN "" THEN
                RETURN.
            WHEN {&OP-DETAILS-CHAR-WHITESPACE} THEN
                vS = " ".
            WHEN {&OP-DETAILS-CHAR-NONE} THEN
                vS = "".
        END.
        oDetails = REPLACE(oDetails, {&SEPARATOR-365P}, vS).
    END.
    ELSE
        oDetails = "".
END PROCEDURE.

PROCEDURE SetOpTaxAttr365p.
    DEFINE PARAMETER BUFFER op         FOR  op.
    DEFINE INPUT  PARAMETER iAttrName  LIKE signs.code        NO-UNDO.
    DEFINE INPUT  PARAMETER iAttrValue LIKE signs.xattr-value NO-UNDO.

    DEFINE VARIABLE vErrorMsg AS CHARACTER NO-UNDO.

    IF NOT AVAILABLE op THEN
        RETURN ERROR "Документ не найден".
    IF UpdateSigns(op.class-code,
                   Surrogate(BUFFER op:HANDLE),
                   iAttrName,
                   iAttrValue,
                   ?)
    THEN
        RETURN.
    RETURN ERROR "Ошибка заполнения налогового реквизита " +
                 GetNullStr(iAttrName)                     +
                 " документа класса "                      +
                 GetNullStr(op.class-code)                 +
                 " значением "                             +
                 QUOTER(GetNullStr(iAttrValue)).
END PROCEDURE.

PROCEDURE FillOpTaxDetails365p.
    DEFINE INPUT  PARAMETER iHReqInfo AS  HANDLE NO-UNDO.
    DEFINE PARAMETER BUFFER op        FOR op.

    DEFINE VARIABLE vBudgClassCode AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vDocNum        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vKppRec        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vKppSend       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vNalPeriod     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vNalStat       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vOkatoNalog    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vPayArg        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vPayDate       AS DATE      NO-UNDO.
    DEFINE VARIABLE vPayType       AS CHARACTER NO-UNDO.

    RUN CheckReqInfo365p(iHReqInfo) {&RAISE-ERROR}.
    IF NOT AVAILABLE op THEN
        RETURN ERROR "Документ не найден".
    RUN GetCharAttrValue365p(iHReqInfo,
                             "КБК",
                             OUTPUT vBudgClassCode)
    {&RAISE-ERROR}.
    RUN GetCharAttrValue365p(iHReqInfo,
                             "НомТреб",
                             OUTPUT vDocNum)
    NO-ERROR.
    RUN GetCharAttrValue365p(iHReqInfo,
                             "КПППол",
                             OUTPUT vKppRec)
    {&RAISE-ERROR}.
    RUN GetCharAttrValue365p(iHReqInfo,
                             "КППНП",
                             OUTPUT vKppSend)
    NO-ERROR.
    RUN GetCharAttrValue365p(iHReqInfo,
                             "СрокУплТр",
                             OUTPUT vNalPeriod)
    NO-ERROR.
    RUN GetCharAttrValue365p(iHReqInfo,
                             "Статус",
                             OUTPUT vNalStat)
    NO-ERROR.
    vOkatoNalog = "".
    RUN GetCharAttrValue365p(iHReqInfo,
                             "ОКАТО",
                             OUTPUT vOkatoNalog)
    NO-ERROR.
    IF NOT {assigned vOkatoNalog} THEN
    RUN GetCharAttrValue365p(iHReqInfo,
                             "ОКТМО",
                             OUTPUT vOkatoNalog)
    {&RAISE-ERROR}.
    RUN GetCharAttrValue365p(iHReqInfo,
                             "ОснПл",
                             OUTPUT vPayArg)
    NO-ERROR.
    RUN GetDateAttrValue365p(iHReqInfo,
                             "ДатаТреб",
                             OUTPUT vPayDate)
    NO-ERROR.
    RUN GetCharAttrValue365p(iHReqInfo,
                             "ТипПлат",
                             OUTPUT vPayType)
    {&RAISE-ERROR}.
    IF NOT {assigned vKppSend} THEN
        vKppSend = "0".
    RUN SetOpTaxAttr365p(BUFFER op,
                         "КБК",
                         vBudgClassCode)
    {&RAISE-ERROR}.
    RUN SetOpTaxAttr365p(BUFFER op,
                         "ПокНД",
                         vDocNum)
    {&RAISE-ERROR}.
    RUN SetOpTaxAttr365p(BUFFER op,
                         "kpp-rec",
                         vKppRec)
    {&RAISE-ERROR}.
    RUN SetOpTaxAttr365p(BUFFER op,
                         "kpp-send",
                         vKppSend)
    {&RAISE-ERROR}.
    RUN SetOpTaxAttr365p(BUFFER op,
                         "ПокНП",
                         vNalPeriod)
    {&RAISE-ERROR}.
    RUN SetOpTaxAttr365p(BUFFER op,
                         "ПокСТ",
                         vNalStat)
    {&RAISE-ERROR}.
    RUN SetOpTaxAttr365p(BUFFER op,
                         "ОКАТО-НАЛОГ",
                         vOkatoNalog)
    {&RAISE-ERROR}.
    RUN SetOpTaxAttr365p(BUFFER op,
                         "ПокОП",
                         vPayArg)
    {&RAISE-ERROR}.
    RUN SetOpTaxAttr365p(BUFFER op,
                         "ПокДД",
                         date2str(vPayDate))
    {&RAISE-ERROR}.
    RUN SetOpTaxAttr365p(BUFFER op,
                         "ПокТП",
                         vPayType)
    {&RAISE-ERROR}.
END PROCEDURE.

PROCEDURE GetRealCustName365p.
   DEFINE INPUT        PARAMETER iCustCat  LIKE acct.cust-cat NO-UNDO.
   DEFINE INPUT        PARAMETER iCustId   LIKE acct.cust-id  NO-UNDO.
   DEFINE OUTPUT       PARAMETER oCustName AS   CHARACTER     NO-UNDO.
   DEFINE INPUT-OUTPUT PARAMETER ioCustINN AS   CHARACTER     NO-UNDO.

   DEFINE VARIABLE vCustName1 AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vCustName2 AS CHARACTER NO-UNDO.

   RUN GetCustName IN h_base (iCustCat,
                              iCustId,
                              "",
                              OUTPUT vCustName1,
                              OUTPUT vCustName2,
                              INPUT-OUTPUT ioCustINN).
   oCustName = TRIM(( IF iCustCat          = "Ю" AND
                        LENGTH(ioCustINN) = {&INN-LENGTH-PERSON}
                     THEN ""
                     ELSE TRIM(vCustName1))
                    + " " +
                    TRIM(vCustName2)).
END PROCEDURE.

&ENDIF /* SPEC365P_PRO_ */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='23/07/2015 12:38:53.227+04:00' */
/* $LINTUSER='ahra' */
/* $LINTMODE='1' */
/* $LINTFILE='spec365p.pro' */
/*prosignEZEukbyqewzD6L1ynMhTew*/
/* --- spec365p.pro was humbly modified by (c)blodd converter v.1.09 on 10/16/2015 7:13am --- */
