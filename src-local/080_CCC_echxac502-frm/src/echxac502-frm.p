/*              
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2016 ЗАО "Банковские информационные системы"
     Filename: ECHXAC502-FRM.P
      Comment: Подготовка данных об изменениях по счету
   Parameters: iClass      - код класса
               iInstance   - содержимое класса iClass
         Uses:
      Used BY:
      Created: 02.07.2014 zhua
     Modified: 13.07.2014 Mike - клон FX-ORDER.P
     Modified: 24.08.2014 zhua - формы для организаций и ИП
*/

{globals.i}
{intrface.get xclass}
{intrface.get pbase}
{intrface.get cust}
{intrface.get trans}
{intrface.get count}
{intrface.get tmess}
{intrface.get acctn}
{debug.equ}
{tax-refusal.fun}

DEFINE INPUT PARAMETER iParams AS CHARACTER NO-UNDO.
DEFINE VARIABLE mMode          AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAcct          AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCliT          AS CHARACTER NO-UNDO.
/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/
FUNCTION GetTAGSFC RETURNS CHAR (INPUT  iBuf   AS CHAR,
                                 INPUT  iTag   AS CHAR):
   DEF VAR vValue  AS CHAR   NO-UNDO.
   DEF VAR vI     AS INT64   NO-UNDO.

   vI     = INDEX(iBuf,iTag + "=").
   IF vI EQ 0 
   THEN 
      vValue = "".
   ELSE
      ASSIGN
         vValue = SUBSTRING(iBuf,vI + LENGTH(iTag) + 1)
         vValue = ENTRY(2,vValue,'"')
      .
   &IF DEFINED (IS-DEBUG) &THEN
      RUN dbgprint.p("GetTAGSFC","iTAg:" + iTAG + 
                                 " vValue:" + vValue).
   &ENDIF

   RETURN vValue.
END FUNCTION.
/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/
FUNCTION LastSFC RETURNS LOGICAL (INPUT  iAcct     AS CHAR,
                                  INPUT  iCurrency AS CHAR,
                                  OUTPUT oINN      AS CHAR,
                                  OUTPUT oBRTD     AS DATE,
                                  OUTPUT oMBRD     AS CHAR,
                                  OUTPUT oCDUL     AS CHAR,
                                  OUTPUT oSERN     AS CHAR,
                                  OUTPUT oDDOC     AS DATE,
                                  OUTPUT oSURN     AS CHAR,
                                  OUTPUT oFNAM     AS CHAR,
                                  OUTPUT oSNAM     AS CHAR):

DEF VAR vResult AS LOGICAL NO-UNDO INIT NO.
DEF VAR vBuf    AS CHAR    NO-UNDO.
DEF VAR vI1     AS INT64   NO-UNDO.
DEF VAR vI2     AS INT64   NO-UNDO.

DEFINE BUFFER PackObject FOR PackObject.
DEFINE BUFFER Packet     FOR Packet.
DEFINE BUFFER PacketText FOR PacketText.

TEXTLOOP:
   FOR EACH PackObject WHERE
            PackObject.file-name EQ 'acct'
        AND PackObject.Surrogate EQ iAcct + "," + iCurrency  NO-LOCK,
       EACH Packet     WHERE
            Packet.PacketID      EQ PackObject.PacketID
        AND Packet.Kind          EQ "TaxExpCommon5X"  
            NO-LOCK,
      EACH  PacketText WHERE 
            PacketText.PacketID  EQ Packet.PacketID
            NO-LOCK
            BY Packet.PackDate DESCENDING
            BY Packet.PackTime DESCENDING:
      ASSIGN
         vI1  = INDEX(PacketText.Contents,"<═╧╘╦")
         vI2  = INDEX(PacketText.Contents,"/═╧╘╦")
      .
      IF vI1 EQ 0 OR vI2 EQ 0 THEN NEXT TEXTLOOP.

      ASSIGN
         vBuf    =  SUBSTRING(PacketText.Contents,vI1 + 6, vI2 - vI1)
         vResult =  YES
         oINN    =  CODEPAGE-CONVERT(GetTAGSFC(vBuf,"╚══╘╦"),
                                                     SESSION:CHARSET,"1251")
         oBRTD   =  DATE(GetTAGSFC(vBuf,"─рЄр╨юцф"))
         oMBRD   =  CODEPAGE-CONVERT(GetTAGSFC(vBuf,"╠хёЄю╨юцф"),
                                                     SESSION:CHARSET,"1251")
         oCDUL   =  CODEPAGE-CONVERT(GetTAGSFC(vBuf,"╩юф─╙╦"),
                                                     SESSION:CHARSET,"1251")
         oSERN   =  CODEPAGE-CONVERT(GetTAGSFC(vBuf,"╤хЁ═юь─юъ"),
                                                     SESSION:CHARSET,"1251")
         oDDOC   =  DATE(GetTAGSFC(vBuf,"─рЄр─юъ"))
         oSURN   =  CODEPAGE-CONVERT(GetTAGSFC(vBuf,"╘рьшыш "),
                                                     SESSION:CHARSET,"1251")
         oFNAM   =  CODEPAGE-CONVERT(GetTAGSFC(vBuf,"╚ь "),
                                                     SESSION:CHARSET,"1251")
         oSNAM   =  CODEPAGE-CONVERT(GetTAGSFC(vBuf,"╬ЄўхёЄтю"),
                                                     SESSION:CHARSET,"1251")
      NO-ERROR.

      &IF DEFINED (IS-DEBUG) &THEN
      RUN dbgprint.p("LastSFC","oINN :" + STRING(oINN)).
      RUN dbgprint.p("LastSFC","oBRTD:" + STRING(oBRTD)).
      RUN dbgprint.p("LastSFC","oMBRD:" + STRING(oMBRD)).
      RUN dbgprint.p("LastSFC","oCDUL:" + STRING(oCDUL)).
      RUN dbgprint.p("LastSFC","oSERN:" + STRING(oSERN)).
      RUN dbgprint.p("LastSFC","oDDOC:" + STRING(oDDOC)).
      RUN dbgprint.p("LastSFC","oSURN:" + STRING(oSURN)).
      RUN dbgprint.p("LastSFC","oFNAM:" + STRING(oFNAM)).
      RUN dbgprint.p("LastSFC","oSNAM:" + STRING(oSNAM)).
      &ENDIF

      LEAVE TEXTLOOP.
   END.

   RETURN vResult.
END FUNCTION.

/*----------------------------------------------------------------------------*/
/*Поиск тега (iTag) по history таблицы iFile по суррогату iSurr               */
/*----------------------------------------------------------------------------*/
FUNCTION GetTagFromHist RETURNS LOGICAL (INPUT  iFile AS CHAR,
                                         INPUT  iSurr AS CHAR,
                                         INPUT  iTag  AS CHAR,
                                         OUTPUT oVal  AS CHAR
):

   DEFINE BUFFER   history FOR history.

   DEFINE VARIABLE vHistF  AS CHAR    NO-UNDO INIT "".
   DEFINE VARIABLE vI      AS INT64   NO-UNDO.

   /*SORT-ACCESS history*/
   FOR EACH history WHERE history.file-name  EQ iFile AND
                          history.field-ref  EQ iSurr AND
                          history.modif-date GE 07/01/2014 AND /*259955: 01.07.2014*/
                          history.modify     EQ "W"   NO-LOCK
                       BY history.modif-date DESC
                       BY history.modif-time DESC:
      /*Убираем, если есть, запятую в конце history.field-value*/
      ASSIGN
         vHistF = history.field-value
         vHistF = IF   SUBSTRING(vHistF,LENGTH(vHistF)) EQ ","
                  THEN SUBSTRING(vHistF,1,LENGTH(vHistF) - 1)
                  ELSE vHistF
      NO-ERROR.
                       
      /*Т.к. в history.field-value данные лежат в формате
      "поле,значение,...поле,значение,", то нам нужно использовать в сравнении
       только нечетные вхождения (поле)
      */

      DO vI = 1 TO ROUND(NUM-ENTRIES(vHistF) / 2, 0):
         IF iTag EQ ENTRY(2 * vI - 1,vHistF)
         THEN
         DO:
            ASSIGN oVal = IF ENTRY(2 * vI,vHistF) EQ CHR(3) + CHR(4) + CHR(5) 
                          THEN "" 
                          ELSE ENTRY(2 * vI,vHistF) NO-ERROR.
            &IF DEFINED (IS-DEBUG) &THEN
            RUN dbgprint.p("GetTagFromHist","iTAg:" + iTAG + 
                                       " vValue:" + oVal).
            RUN dbgprint.p("GetTagFromHist","history.modif-date:" + STRING(history.modif-date) +
                           " history.modif-time:" + string(history.modif-time, "HH:MM:SS")).
            &ENDIF
            {tolist.i oVal}

            RETURN YES.
         END. /*if*/
      END. /*do*/
   END. /*for each*/
   RETURN NO.
END FUNCTION.

/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/
FUNCTION GetCliData RETURNS CHARACTER (INPUT  iCustCat   AS CHAR,
                                       INPUT  iCustID    AS INT64,
                                       OUTPUT oINN       AS CHAR,
                                       OUTPUT oKPP       AS CHAR,
                                       OUTPUT oOGRN      AS CHAR):
DEF VAR vAddr AS CHAR NO-UNDO.
DEF VAR vType AS CHAR NO-UNDO.
DEF VAR vCode AS CHAR NO-UNDO.
DEF VAR vCorr AS CHAR NO-UNDO.
DEF VAR vName AS CHAR NO-UNDO.
DEFINE BUFFER cust-corp FOR cust-corp.
DEFINE BUFFER person    FOR person.
vName = GetCliName (iCustCat,
             STRING(iCustID),
             OUTPUT vAddr,
             OUTPUT oINN,
             OUTPUT oKPP,
       INPUT-OUTPUT vType,
             OUTPUT vCode,
             OUTPUT vCorr).
CASE iCustCat:
   WHEN "Ю" THEN
      FOR FIRST cust-corp WHERE cust-corp.cust-id EQ iCustID NO-LOCK:
         oOGRN = GetXattrValue("cust-corp",STRING(iCustID),"ОГРН").
      END.
   WHEN "Ч" THEN
      FOR FIRST person    WHERE person.person-id  EQ iCustID NO-LOCK:
         oOGRN = GetXattrValue("person",STRING(iCustID),"ОГРН").
      END.
END CASE.
RETURN vName.
END FUNCTION.
/*----------------------------------------------------------------------------*/
/*Модифицированная FGetSetting для работы в многофилиальном режиме не с       */
/*текущим подразделением, а указанным во входном параметре iFilial            */
/*----------------------------------------------------------------------------*/

FUNCTION FGetSetFil RETURNS CHAR
   (INPUT iCode     AS CHAR,    /* Код параметра             */
    INPUT iSubCode  AS CHAR,    /* Код подпараметра          */
    INPUT iFilial   AS CHAR,
    INPUT iDefValue AS CHAR):   /* Значение по умолчанию     */
  
   DEFINE BUFFER tSetting FOR setting.
   DEFINE VARIABLE vResult AS CHARACTER  NO-UNDO.
   
   IF iSubCode = ? THEN
      iSubCode = "".
      /* Если МФ режим, то ищем с учетом iFilial. */
      IF shMode THEN
         FIND FIRST tSetting WHERE
                  tSetting.code        EQ iCode
            AND   tSetting.sub-code    EQ iSubCode
            AND   tSetting.filial-id   EQ iFilial
         NO-LOCK NO-ERROR.

      IF NOT AVAIL tSetting THEN
         FIND FIRST tSetting WHERE
                  tSetting.code     EQ iCode
            AND   tSetting.sub-code EQ iSubCode
         NO-LOCK NO-ERROR.

      IF AVAILABLE(tSetting) THEN
         vResult = RIGHT-TRIM(tSetting.Val).
      ELSE
         vResult = ?.

   IF vResult EQ ? THEN
      vResult = iDefValue.
   RETURN vResult.

END FUNCTION.
/******************************************************************************/

ASSIGN
   mMode = ENTRY(1,iParams,CHR(1))
   mAcct = ENTRY(2,iParams,CHR(1))
   mCliT = ENTRY(3,iParams,CHR(1))
NO-ERROR.
CASE mMode:
 WHEN "1" THEN RUN RequestOldAcct (INPUT mAcct, OUTPUT pick-value).
 WHEN "2" THEN RUN RequestOldBank (             OUTPUT pick-value).
 WHEN "3" THEN RUN RequestOldPers (INPUT mAcct, INPUT mCliT, OUTPUT pick-value).
 WHEN "4" THEN RUN RequestOldFirm (INPUT mAcct, INPUT mCliT, OUTPUT pick-value).
 WHEN "5" THEN RUN RequestOldIndP (INPUT mAcct, INPUT mCliT, OUTPUT pick-value).
 WHEN "6" THEN RUN RequestBranchB (             OUTPUT pick-value).
 WHEN "7" THEN RUN RequestBranchBCancel (       OUTPUT pick-value).
END CASE.
{intrface.del}
RETURN pick-value.

/******************************************************************************/
/*Если Признак изменения номера счета = Да, то выводим форму с запросом данных*/
/******************************************************************************/
PROCEDURE RequestOldAcct:

   DEFINE INPUT  PARAMETER iAcct      AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER oPickValue AS CHARACTER NO-UNDO.
   {echxac502-frm.def}
   DEF VAR vSurrNewAcct AS CHAR NO-UNDO.
   DEF VAR vSurrOldAcct AS CHAR NO-UNDO.
   DEF VAR vOldAcOpDt   AS DATE NO-UNDO.
   DEF VAR vDateFromBuf AS DATE NO-UNDO INIT ?.

   DEFINE BUFFER bacct FOR acct.

   FORM
     vOldAcct vDATE
   WITH FRAME vfrm1.

   ON F1 OF vOldAcct IN FRAME vfrm1
   DO:
      RUN browseld.p ("acct",
                      "bal-acct" + CHR(1) + "cust-cat" + CHR(1) + "cust-id", 
                      "!40802,*" + CHR(1) +  vCustCat  + CHR(1) + STRING(vCustID), "", 2).
      IF pick-value <> ? THEN SELF:SCREEN-VALUE = pick-value.
   END.
   {echxac502-frm.nau vfrm1 vDATE}
   ON GO OF FRAME vfrm1
   DO:
      {echxac502-frm.chk &vchar = vOldAcct &text = "Счет"}
      {echxac502-frm.chk &vdate = vDATE    &text = "Дата изменений"}
      RUN AddAttr2TableEx IN h_trans ("", 0, -1, "", 0,
                                      "AcChDt",STRING(vDATE:SCREEN-VALUE)).
      {find-act.i &acct = vOldAcct:SCREEN-VALUE}
      IF AVAIL acct THEN
      DO:
         ASSIGN
            vOldAcOpDt   = acct.open-date
            vSurrOldAcct = acct.acct + "," + acct.currency
         NO-ERROR.
         UpdateSigns("acct",vSurrNewAcct,"СНИНачДата",
                     GetXAttrValueEx("acct",vSurrOldAcct,"СНИНачДата",""),?).
         UpdateSigns("acct",vSurrNewAcct,"СНИНом",
                     GetXAttrValueEx("acct",vSurrOldAcct,"СНИНом",""),?).
         UpdateSigns("acct",vSurrNewAcct,"СНИНомПП",
                     GetXAttrValueEx("acct",vSurrOldAcct,"СНИНомПП",""),?).
         UpdateSigns("acct",vSurrNewAcct,"СНИРегн",
                     GetXAttrValueEx("acct",vSurrOldAcct,"СНИРегн",""),?).
      END.
      ELSE
      DO:
         RUN Fill-AlertSysMes IN h_tmess ("","","1",
                 "Указанный счет не существует !~n" +
                 "Выберите счет по F1").
         RETURN NO-APPLY.
      END.
   END.
   {echxac502-frm.i vfrm1}

   {find-act.i 
      &acct = iAcct 
      &bact = "bacct"}
   vCustCat = bacct.cust-cat.

   ASSIGN
     vOldAcct     = iAcct
     vDateFromBuf = DATE(GetAttrValue2("",0,"AcChDt"))
   NO-ERROR.

   IF vDateFromBuf NE ? THEN vDATE = vDateFromBuf.

   FIND FIRST acct WHERE acct.acct = iAcct NO-LOCK NO-ERROR.
   IF AVAIL acct THEN
      ASSIGN
         vCustID      = acct.cust-id
         vSurrNewAcct = acct.acct + "," + acct.currency
      .

   DO TRANS ON ERROR UNDO, LEAVE ON ENDKEY UNDO, LEAVE:
      UPDATE
         vOldAcct 
         vDATE
      WITH FRAME vfrm1 SIDE-LABEL ROW 11 TITLE COLOR brigth-white 
      "[ ВВЕДИТЕ ДАННЫЕ ]" 1 COL OVERLAY CENTERED.
   END.
   HIDE FRAME vfrm1 NO-PAUSE.
   IF vChoice THEN
      oPickValue = STRING(vChoice).
   ELSE
      oPickValue = vOldAcct + CHR(1) + STRING(vDATE) + CHR(1) + 
                                       STRING(vOldAcOpDt).
END PROCEDURE.

/******************************************************************************/
/*Если Признак изменения свед.о банке = Да, то выводим форму с запросом данных*/
/******************************************************************************/
PROCEDURE RequestOldBank:

   DEFINE OUTPUT PARAMETER oPickValue AS CHARACTER NO-UNDO.

  {echxac502-frm.def}
   ASSIGN
      vCodF = FGetSetting("КодФил", ?, "")
      vBIK  = FGetSetting("БанкМФО",?, "")
      vINN  = FGetSetting("ИНН", ?, "")
      vKPP  = ENTRY(1,FGetSetting("БанкКПП",?,?))
      vOGRN = FGetSetting("ОГРН", ?, "")
      vRNOM = ENTRY(1,GetXAttrValue("branch", vCodF, "REGN"),"/")
      vNAME = FGetSetting("ГНИ", "НаимКО","")
      vBRAN = GetXAttrValue("branch", vCodF, "REGN")
      vBRAN = IF NUM-ENTRIES(vBRAN,"/") GE 2 THEN ENTRY(2,vBRAN,"/") ELSE ""
      vAdr  = FGetSetting("Адрес_структ", ?, "")
      vINDX = ENTRY(2,vAdr)
      vCREG = ENTRY(3,vAdr)
      vDIST = ENTRY(4,vAdr)
      vCITY = ENTRY(5,vAdr)
      vNASP = ENTRY(6,vAdr)
      vSTRT = ENTRY(7,vAdr)
      vHOUS = ENTRY(8,vAdr)
      vCORP = ENTRY(9,vAdr)
      vAPRT = ENTRY(10,vAdr)
   NO-ERROR.

   IF NOT {assigned vBIK}  THEN vBIK = GetXAttrValue("branch",vCodF, "БанкМФО").
   IF NOT {assigned vINN}  THEN vINN = GetXAttrValue("branch", vCodF, "ИНН").
   IF NOT {assigned vOGRN} THEN vOGRN = GetXAttrValue("branch", vCodF, "ОГРН").

   CASE vNAME:
      WHEN "Банкс" THEN 
         vNAME = FGetSetting("Банкс", ?, "").
      WHEN "Банк"  OR
      WHEN ""      THEN
      DO:
         vNAME = FGetSetting("Банк", ?, "").
         IF NOT {assigned vName} THEN DO:
            FIND FIRST branch WHERE branch.branch-id EQ vCodF NO-LOCK NO-ERROR.
            vNAME = IF AVAIL branch THEN branch.name ELSE "".
         END.
      END.
   END CASE.

   FORM
     vBIK  vINN  vKPP  vOGRN vRNOM vNAME vINDX vCREG
     vDIST vCITY vNASP vSTRT vHOUS vCORP vAPRT vBRAN vDATE
   WITH FRAME vfrm1.

   ON F1 OF vBIK IN FRAME vfrm1
   DO:
      RUN browseld.p ("banks", "", "", "", 2).

      IF pick-value <> ? THEN
      DO:
         FIND FIRST banks-code WHERE banks-code.bank-id EQ INT64(pick-value) AND
                                     banks-code.bank-code-type EQ "МФО-9"
         NO-LOCK NO-ERROR.
         IF AVAIL banks-code THEN
            SELF:SCREEN-VALUE = banks-code.bank-code.
      END.
   END.
   {echxac502-frm.nau vfrm1 vDATE}
   ON GO OF FRAME vfrm1
   DO:
      {echxac502-frm.chk &vchar = vBIK  &text = "БИК"}
      {echxac502-frm.chk &vchar = vINN  &text = "ИНН"}
      {echxac502-frm.chk &vchar = vKPP  &text = "КПП"}
      {echxac502-frm.chk &vchar = vOGRN &text = "ОГРН"}
      {echxac502-frm.chk &vchar = vRNOM &text = "Рег. НОМ"}
      {echxac502-frm.chk &vchar = vNAME &text = "Наименование"}
      {echxac502-frm.chk &vchar = vINDX &text = "Почт. индекс"}
      {echxac502-frm.chk &vchar = vCREG &text = "Код региона"}
      {echxac502-frm.chk &vchar = vBRAN &text = "Ном. Филиала"}
      {echxac502-frm.chk &vdate = vDATE &text = "Дата изменений"}
   END.
   {echxac502-frm.i vfrm1}
   DO TRANS ON ERROR UNDO, LEAVE ON ENDKEY UNDO, LEAVE:
      UPDATE 
         vBIK  vINN  vKPP  vOGRN vRNOM vNAME VIEW-AS FILL-IN SIZE 60 BY 1
         vINDX vCREG vDIST vCITY vNASP vSTRT
         vHOUS vCORP vAPRT vBRAN vDATE 
      WITH FRAME vfrm1 SIDE-LABEL ROW 3 TITLE COLOR brigth-white 
         "[ СВЕДЕНИЯ О БАНКЕ ДО ВНЕСЕНИЯ ИЗМЕНЕНИЙ ]" 1 COL OVERLAY 
         CENTERED.
   END.
   HIDE FRAME vfrm1 NO-PAUSE.
   IF vChoice THEN
      oPickValue = STRING(vChoice).
   ELSE
      oPickValue = vBIK  + CHR(1) + vINN  + CHR(1) + vKPP  + CHR(1) + 
                   vOGRN + CHR(1) + vRNOM + CHR(1) + vNAME + CHR(1) + 
                   vINDX + CHR(1) + vCREG + CHR(1) + vDIST + CHR(1) + 
                   vCITY + CHR(1) + vNASP + CHR(1) + vSTRT + CHR(1) +
                   vHOUS + CHR(1) + vCORP + CHR(1) + vAPRT + CHR(1) +
                   vBRAN + CHR(1) + STRING(vDATE).
END PROCEDURE.

/******************************************************************************/
/*Если Признак изменения свед. ФЛ = Да, то выводим форму с запросом данных ФЛ**/
/******************************************************************************/
PROCEDURE RequestOldPers:

   DEFINE INPUT  PARAMETER iAcct      AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER iCliType   AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER oPickValue AS CHARACTER NO-UNDO.

   {echxac502-frm.def}   

   DEFINE VARIABLE vBRTDc   AS CHARACTER NO-UNDO INIT "".
   DEFINE VARIABLE vDDOCc   AS CHARACTER NO-UNDO INIT "".
   DEFINE VARIABLE vDocFlag AS LOGICAL   NO-UNDO INIT NO.
   DEFINE VARIABLE vDocEnable AS LOGICAL NO-UNDO INIT YES.


   FORM
     vINN vBRTD vMBRD vCDUL vSERN vDDOC vSURN vFNAM vSNAM vDATE
   WITH FRAME vfrm1.

   FIND FIRST acct WHERE acct.acct EQ iAcct NO-LOCK NO-ERROR.
   {find-act.i
      &acct   = iAcct
   }

   IF AVAIL acct THEN
   DO:
      ASSIGN
         vCustID  = acct.cust-id
         vCustCat = acct.cust-cat
      .
      FIND FIRST person WHERE person.person-id EQ acct.cust-id NO-LOCK NO-ERROR.
      IF AVAIL person THEN
      DO:
                   /*ищем предыдущее сообщение - функционал заявки 232710*/
         IF NOT LastSFC (INPUT  acct.acct,
                         INPUT  acct.currency,
                         OUTPUT vINN,
                         OUTPUT vBRTD,
                         OUTPUT vMBRD,
                         OUTPUT vCDUL,
                         OUTPUT vSERN,
                         OUTPUT vDDOC,
                         OUTPUT vSURN,
                         OUTPUT vFNAM,
                         OUTPUT vSNAM) 
         THEN
         DO:
                   /*если не находим, то начинаем искать по history:*/
                   /*ИНН*/
            IF NOT GetTagFromHist (INPUT  "person",
                                   INPUT  STRING(person.person-id),
                                   INPUT  "inn",
                                   OUTPUT vINN) THEN
                   /*если не нашли - поставляем текущее значение*/
               ASSIGN vINN  = IF {assigned person.inn} THEN person.inn ELSE "".

                   /*День рождения*/
            IF NOT GetTagFromHist (INPUT  "person",
                                   INPUT  STRING(person.person-id),
                                   INPUT  "birthday",
                                   OUTPUT vBRTDc) THEN
                   /*если не нашли - поставляем текущее значение*/
               ASSIGN vBRTD = person.birthday.
                   /*если нашли - преобразуем в дату*/
            ELSE ASSIGN vBRTD = DATE(vBRTDc) NO-ERROR.

                   /*Место рождения*/
            IF NOT GetTagFromHist (INPUT  "person",
                                   INPUT  STRING(person.person-id),
                                   INPUT  "*BirthPlace",
                                   OUTPUT vMBRD) THEN
            DO:
                   /*если не нашли - поставляем текущее значение*/
               &IF DEFINED (IS-DEBUG) &THEN
               RUN dbgprint.p("RequestOldPers","hist vMBRD:" + STRING(vMBRD)).
               &ENDIF
               ASSIGN vMBRD = GetXattrValueEx("person",
                                              STRING(person.person-id),
                                              "birthplace",
                                              "неизвестно").
               &IF DEFINED (IS-DEBUG) &THEN
               RUN dbgprint.p("RequestOldPers","dr vMBRD:" + STRING(vMBRD)).
               &ENDIF
            END.
            ELSE
               IF NOT {assigned vMBRD} THEN vMBRD = "неизвестно".
                   /*Фамилия*/
            &IF DEFINED (IS-DEBUG) &THEN
            RUN dbgprint.p("RequestOldPers","hist2 vMBRD:" + STRING(vMBRD)).
            &ENDIF
            IF NOT GetTagFromHist (INPUT  "person",
                                   INPUT  STRING(person.person-id),
                                   INPUT  "name-last",
                                   OUTPUT vSURN) THEN
                   /*если не нашли - поставляем текущее значение*/
              ASSIGN vSURN = person.name-last.

                   /*Имя, Отчество*/
            IF NOT GetTagFromHist (INPUT  "person",
                                   INPUT  STRING(person.person-id),
                                   INPUT  "first-names",
                                   OUTPUT vSNAM) THEN
                   /*если не нашли - поставляем текущее значение*/
               ASSIGN vSNAM = person.first-names.

                   /*разбиваем на Имя и Отчество*/
            ASSIGN
               vFNAM = ENTRY(1,vSNAM," ")
               ENTRY(1,vSNAM," ") = ""
               vSNAM = TRIM(vSNAM)
            .

                   /*Документ (серия-номер)*/
            IF GetTagFromHist (INPUT  "person",
                               INPUT  STRING(person.person-id),
                               INPUT  "document",
                               OUTPUT vSERN) THEN
            DO:
                   /*ищем ДУЛ и заполняем Код ДУЛ и Дату выдачи из него*/
               FIND FIRST cust-ident WHERE
                          cust-ident.class-code EQ "p-cust-ident"   AND
                          cust-ident.cust-cat   EQ "Ч"              AND
                          cust-ident.cust-id    EQ person.person-id AND
                          cust-ident.cust-code  EQ vSERN 
               NO-LOCK NO-ERROR.
               IF AVAIL cust-ident THEN
                  ASSIGN
                     vCDUL    = GetCode("КодДокум", cust-ident.cust-code-type)
                     vDDOC    = cust-ident.open-date
                     vDocFlag = YES
                  .
               ELSE
                   /*vDocFlag - остается NO*/
                   /*если нашли по history, но такого ДУЛ (cust-ident) нет*/
                   /*кейс - новый ДУЛ не создавался, а редактировался текущий*/
                  vDocEnable = NO.
            END.
                   /*если не нашлось в history, или нашлось в history,
                     но ДУЛ не нашелся, то заполняем текущими значениями*/
            IF NOT vDocFlag THEN
            DO:
               ASSIGN
                  vCDUL = GetCode("КодДокум", person.document-id)
                  vDDOC = DATE(GetXattrValueEx("person",
                                               STRING(acct.cust-id),
                                               "Document4Date_vid",
                                               "")
                              )
               .
               /*серия-номер:*/
               /*если в history не нашлось, то подставляем текущее значение*/
               IF vDocEnable THEN
                  vSERN = person.document.
               /*а если нашлось, но ДУЛ отсутствует, то подставляем то,
                 что нашли в history.*/
            END.

                   /*возможна и такая ситуация - документ не менялся, а 
                     дата его выдачи изменилась (например, ошибка пользователя
                     при первичном вводе*/

            IF GetTagFromHist (INPUT  "person",
                               INPUT  STRING(person.person-id),
                               INPUT  "*Document4Date_vid",
                               OUTPUT vDDOCc) THEN
                   /*если находим - переопределяем 
                     уже сформированный vDDOC из ДР*/
               ASSIGN vDDOC = DATE (vDDOCc).
         END.
      END.
   END.

   {echxac502-frm.nau vfrm1 vBRTD}
   {echxac502-frm.nau vfrm1 vDDOC}
   {echxac502-frm.nau vfrm1 vDATE}
   ON GO OF FRAME vfrm1
   DO:
      {echxac502-frm.chk &vdate = vBRTD &text = "Дата рождения"}
      {echxac502-frm.chk &vchar = vMBRD &text = "Место рождения"}
      {echxac502-frm.chk &vchar = vCDUL &text = "Код ДУЛ"}
      {echxac502-frm.chk &vchar = vSERN &text = "Серия, Номер"}
      {echxac502-frm.chk &vdate = vDDOC &text = "Дата выдачи документа"}
      {echxac502-frm.chk &vchar = vSURN &text = "Фамилия"}
      {echxac502-frm.chk &vchar = vFNAM &text = "Имя"}
      {echxac502-frm.chk &vdate = vDATE &text = "Дата изменений"}
   END.
   {echxac502-frm.i vfrm1}
      &IF DEFINED (IS-DEBUG) &THEN
      RUN dbgprint.p("RequestOldPers","frm vMBRD:" + STRING(vMBRD)).
      &ENDIF

   IF GetBaseOpKind() NE "GRCXacA" THEN
      DO TRANS ON ERROR UNDO, LEAVE ON ENDKEY UNDO, LEAVE:
         UPDATE 
            vINN vBRTD vMBRD vCDUL vSERN vDDOC vSURN vFNAM vSNAM vDATE
         WITH FRAME vfrm1 SIDE-LABEL ROW 8 TITLE COLOR brigth-white 
            "[ РЕКВИЗИТЫ ФЛ ДО ВНЕСЕНИЯ ИЗМЕНЕНИЙ ]" 1 COL OVERLAY 
            CENTERED.
      END.

   HIDE FRAME vfrm1 NO-PAUSE.
   IF vChoice THEN
      oPickValue = STRING(vChoice).
   ELSE
   DO:
      Set-tt-taxcli-FL(vCustID,vCustCat,iCliType,
                    vINN,vBRTD,vMBRD,vCDUL,vSERN,vDDOC,vSURN,vFNAM,vSNAM,vDATE).
      oPickValue = vINN  + CHR(1) + STRING(vBRTD) + CHR(1) + vMBRD + CHR(1) +
                   vCDUL + CHR(1) + vSERN + CHR(1) + STRING(vDDOC) + CHR(1) + 
                   vSURN + CHR(1) + vFNAM + CHR(1) + vSNAM + CHR(1) + 
                   STRING(vDATE).
   END.
END PROCEDURE.
/******************************************************************************/
/*Если Признак изменения свед. организации = Да, то                           */
/*выводим форму с запросом данных организации                                 */
/******************************************************************************/
PROCEDURE RequestOldFirm:

   DEFINE INPUT  PARAMETER iAcct      AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER iCliType   AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER oPickValue AS CHARACTER NO-UNDO.

   {echxac502-frm.def}

   IF vDate511 GT TODAY THEN
      FORM vPrIzOld vINN vKPP vOGRN vNAME vDATE WITH FRAME vfrmOld.
   ELSE
      FORM vPrIz    vINN vKPP vOGRN vNAME vDATE WITH FRAME vfrm1.

   FIND FIRST acct WHERE acct.acct EQ iAcct NO-LOCK NO-ERROR.
   IF AVAIL acct THEN
   DO:
      ASSIGN
         vCustID  = acct.cust-id
         vCustCat = acct.cust-cat
         vNAME    = GetCliData (INPUT  acct.cust-cat,
                                INPUT  acct.cust-id,
                               OUTPUT vINN,
                               OUTPUT vKPP,
                               OUTPUT vOGRN)
      NO-ERROR.
   END.
   {echxac502-frm.nau vfrm1 vDATE}
   ON GO OF FRAME vfrm1
   DO:
      {echxac502-frm.chk &vchar = vINN  &text = "ИНН"}
      {echxac502-frm.chk &vchar = vNAME &text = "Наименование"}
      {echxac502-frm.chk &vdate = vDATE &text = "Дата изменений"}
   END.

   IF vDate511 GT TODAY THEN DO:
      {echxac502-frm.i vfrmOld}

      DO TRANS ON ERROR UNDO, LEAVE ON ENDKEY UNDO, LEAVE:
         UPDATE  
            vPrIzOld FORMAT "X(32)" LABEL "ПРИЗНАК ИЗМ." 
            vINN vKPP vOGRN
            vNAME VIEW-AS FILL-IN SIZE 60 BY 1
            vDATE
         WITH FRAME vfrmOld SIDE-LABEL ROW 8 TITLE COLOR brigth-white 
            "[ РЕКВИЗИТЫ ОРГАНИЗАЦИИ ДО ВНЕСЕНИЯ ИЗМЕНЕНИЙ ]" 1 COL OVERLAY 
            CENTERED.
      END.
      HIDE FRAME vfrmOld.

      IF vChoice THEN
         oPickValue = STRING(vChoice).
      ELSE
      DO:
         Set-tt-taxcli-OR(vCustID,vCustCat,iCliType,
                   SUBSTRING(vPrIzOld,1,1),vINN,vKPP,vOGRN,vNAME,vDATE).
         oPickValue = SUBSTRING(vPrIzOld,1,1) + CHR(1) + vINN  + CHR(1) +
                   vKPP                 + CHR(1) + vOGRN + CHR(1) +
                   vNAME                + CHR(1) + STRING(vDATE).
      END.
   END.
   ELSE DO:
      {echxac502-frm.i vfrm1}

      DO TRANS ON ERROR UNDO, LEAVE ON ENDKEY UNDO, LEAVE:
         UPDATE  
            vPrIz FORMAT "X(35)" LABEL "ПРИЗНАК ИЗМ." 
            vINN vKPP vOGRN
            vNAME VIEW-AS FILL-IN SIZE 60 BY 1
            vDATE
         WITH FRAME vfrm1 SIDE-LABEL ROW 8 TITLE COLOR brigth-white 
            "[ РЕКВИЗИТЫ ОРГАНИЗАЦИИ ДО ВНЕСЕНИЯ ИЗМЕНЕНИЙ ]" 1 COL OVERLAY 
            CENTERED.
      END.
      HIDE FRAME vfrm1.

      IF vChoice THEN
         oPickValue = STRING(vChoice).
      ELSE
      DO:
         Set-tt-taxcli-OR(vCustID,vCustCat,iCliType,
                   SUBSTRING(vPrIz,1,1),vINN,vKPP,vOGRN,vNAME,vDATE).
         oPickValue = SUBSTRING(vPrIz,1,1) + CHR(1) + vINN  + CHR(1) +
                   vKPP                 + CHR(1) + vOGRN + CHR(1) +
                   vNAME                + CHR(1) + STRING(vDATE).
      END.
   END.

END PROCEDURE.
/******************************************************************************/
/*Если Признак изменения свед. организации = Да, то                           */
/*выводим форму с запросом данных организации, УТИТ                           */
/******************************************************************************/
PROCEDURE RequestOldIndP:

   DEFINE INPUT  PARAMETER iAcct      AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER iCliType   AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER oPickValue AS CHARACTER NO-UNDO.

   {echxac502-frm.def}

   IF vDate511 GT TODAY THEN
      FORM vPrIzOld vINN vOGRN vSURN vFNAM vSNAM vDATE WITH FRAME vfrmOld.
   ELSE
      FORM vPrIz vINN vOGRN vSURN vFNAM vSNAM vDATE WITH FRAME vfrm1.
  
   FIND FIRST acct WHERE acct.acct EQ iAcct NO-LOCK NO-ERROR.
   IF AVAIL acct THEN
   DO:
      ASSIGN
         vCustID  = acct.cust-id
         vCustCat = acct.cust-cat
         vNAME    = GetCliData (INPUT  acct.cust-cat,
                                INPUT  acct.cust-id,
                               OUTPUT vINN,
                               OUTPUT vKPP,
                               OUTPUT vOGRN)
      NO-ERROR.
   END.
   ASSIGN
      vSURN = ENTRY(1,vNAME, " ")
      vFNAM = ENTRY(2,vNAME, " ")
      vSNAM = ENTRY(3,vNAME, " ")
   NO-ERROR.
   {echxac502-frm.nau vfrm1 vDate}
   ON GO OF FRAME vfrm1
   DO:
      {echxac502-frm.chk &vchar = vINN  &text = "ИНН"}
      {echxac502-frm.chk &vchar = vSURN &text = "Фамилия"}
      {echxac502-frm.chk &vchar = vFNAM &text = "Имя"}
      {echxac502-frm.chk &vdate = vDATE &text = "Дата изменений"}
      
   END.
   IF vDate511 GT TODAY THEN DO:
      {echxac502-frm.i vfrmOld}

      DO TRANS ON ERROR UNDO, LEAVE ON ENDKEY UNDO, LEAVE:
         UPDATE  
            vPrIzOld FORMAT "X(32)" LABEL "ПРИЗНАК ИЗМ." vINN vOGRN
            vSURN vFNAM vSNAM vDATE
         WITH FRAME vfrmOld SIDE-LABEL ROW 8 TITLE COLOR brigth-white 
            "[ РЕКВИЗИТЫ ИП ДО ВНЕСЕНИЯ ИЗМЕНЕНИЙ ]" 1 COL OVERLAY 
            CENTERED.
      END.
      HIDE FRAME vfrmOld.

      IF vChoice THEN
         oPickValue = STRING(vChoice).
      ELSE
      DO:
         Set-tt-taxcli-OR(vCustID,vCustCat,iCliType,
                   SUBSTRING(vPrIzOld,1,1),vINN,vKPP,vOGRN,vNAME,vDATE).
         oPickValue = SUBSTRING(vPrIzOld,1,1) + CHR(1) + vINN  + CHR(1) +
                   vKPP                 + CHR(1) + vOGRN + CHR(1) +
                   vNAME                + CHR(1) + STRING(vDATE).
      END.
   END.
   ELSE DO:
      {echxac502-frm.i vfrm1}

      DO TRANS ON ERROR UNDO, LEAVE ON ENDKEY UNDO, LEAVE:
         UPDATE  
            vPrIz FORMAT "X(35)" LABEL "ПРИЗНАК ИЗМ." vINN vOGRN
            vSURN vFNAM vSNAM vDATE
         WITH FRAME vfrm1 SIDE-LABEL ROW 8 TITLE COLOR brigth-white 
            "[ РЕКВИЗИТЫ ИП ДО ВНЕСЕНИЯ ИЗМЕНЕНИЙ ]" 1 COL OVERLAY 
            CENTERED.
      END.
      HIDE FRAME vfrm1.

      IF vChoice THEN
         oPickValue = STRING(vChoice).
      ELSE
      DO:
         Set-tt-taxcli-OR(vCustID,vCustCat,iCliType,
                   SUBSTRING(vPrIz,1,1),vINN,vKPP,vOGRN,vNAME,vDATE).
         oPickValue = SUBSTRING(vPrIz,1,1) + CHR(1) + vINN  + CHR(1) +
                   vKPP                 + CHR(1) + vOGRN + CHR(1) +
                   vNAME                + CHR(1) + STRING(vDATE).
      END.
   END.

END PROCEDURE.
/******************************************************************************/
/*Реквизиты банка                                                             */
/*                                                                            */
/******************************************************************************/
PROCEDURE RequestBranchB:

   DEFINE OUTPUT PARAMETER oPickValue AS CHARACTER NO-UNDO.

   {echxac502-frm.def}

   ASSIGN
   vNStmp   = GetXAttrValueEx("branch",FGetSetting("КодФил",?,""),"REGN","0")
   vNomSoob = STRING(INT64(ENTRY(1,vNStmp,"/")),"9999")
   vNStmp   = STRING(
                 INT64(IF NUM-ENTRIES(vNStmp,"/") GE 2 THEN ENTRY(2,vNStmp,"/")
                                                       ELSE "0"),"9999")
   vNomSoob = vNomSoob + vNStmp + SUBSTRING(STRING(YEAR(TODAY),"9999"),3,2) +
              STRING(INT64(GetCounterNextValue
              (FGetSetting("ГНИ","КодНом1114301","ГНИ4"), TODAY)),"99999999")
   .
   
   FORM
     vBRNC
   WITH FRAME vfrm1
   .

   ON F1 OF vBRNC IN FRAME vfrm1
   DO:
      DO TRANSACTION:
         RUN browseld.p ("branch",                         
             "parent-id" + CHR(1) + 
             "title"     + CHR(1) + 
             "isbank",
             'Top' + CHR(1) + 
             "Организационная структура банка" + CHR(1) +
             'yes',
             ?,
             5).
      END.
      IF (LASTKEY EQ 10   OR 
          LASTKEY  EQ 13) AND
         pick-value NE ?  THEN
      DO:
         FIND FIRST branch WHERE branch.branch-id EQ pick-value
                                                               NO-LOCK NO-ERROR.
         IF AVAIL branch THEN
            ASSIGN
               vBRNC:SCREEN-VALUE = pick-value
            .
         RELEASE branch.
      END.
   END.
   ON GO OF FRAME vfrm1
   DO:
      {echxac502-frm.chk &vchar = vBRNC &text = "Подразделение-правопреемник"}
      FIND FIRST branch WHERE branch.branch-id EQ vBRNC:SCREEN-VALUE
                                                               NO-LOCK NO-ERROR.
      IF NOT AVAIL branch THEN
      DO:
         RUN Fill-AlertSysMes IN h_tmess ("","","1",
                    "Указанное подразделение не существует!~n" +
                    "Выберите подразделение-правопреемник по F1").
         RETURN NO-APPLY.
      END.
   END.
   {echxac502-frm.i vfrm1}
   DO TRANS ON ERROR UNDO, LEAVE ON ENDKEY UNDO, LEAVE:
      UPDATE  
         vBRNC LABEL "Подразделение"
      WITH FRAME vfrm1 SIDE-LABEL ROW 8 TITLE COLOR brigth-white 
         "[ ВЫБОР ПРАВОПРЕЕМНИКА ]" 1 COL OVERLAY 
         CENTERED.

   END.
   HIDE FRAME vfrm1 NO-PAUSE.

   IF vChoice THEN
      oPickValue = STRING(vChoice).
   ELSE
   DO:
      FIND FIRST branch WHERE branch.branch-id EQ vBRNC NO-LOCK NO-ERROR.
      IF AVAIL branch THEN
      DO:
         ASSIGN
            vCodF = vBRNC
            vBIK  = FGetSetFil("БанкМФО",?,vCodF, "")
            vINN  = FGetSetFil("ИНН", ?,vCodF, "")
            vKPP  = ENTRY(1,FGetSetFil("БанкКПП",?,vCodF,?))
            vOGRN = FGetSetFil("ОГРН", ?,vCodF, "")
            vRNOM = ENTRY(1,GetXAttrValue("branch", vCodF, "REGN"),"/")
            vNAME = FGetSetFil("ГНИ", "НаимКО",vCodF,"")
            vBRAN = GetXAttrValue("branch", vCodF, "REGN")
            vBRAN = IF NUM-ENTRIES(vBRAN,"/") GE 2 THEN ENTRY(2,vBRAN,"/")
                                                   ELSE ""
            vAdr  = FGetSetFil("Адрес_структ", ?,vCodF, "")
            vINDX = ENTRY(2,vAdr)
            vCREG = ENTRY(3,vAdr)
            vDIST = ENTRY(4,vAdr)
            vCITY = ENTRY(5,vAdr)
            vNASP = ENTRY(6,vAdr)
            vSTRT = ENTRY(7,vAdr)
            vHOUS = ENTRY(8,vAdr)
            vCORP = ENTRY(9,vAdr)
            vAPRT = ENTRY(10,vAdr)
         NO-ERROR.

         CASE vNAME:
            WHEN "Банкс" THEN 
               vNAME = FGetSetFil("Банкс", ?, vCodF,"").
            WHEN "Банк"  OR
            WHEN ""      THEN
            DO:
               vNAME = FGetSetFil("Банк", ?, vCodF,"").
               IF NOT {assigned vName} THEN
               DO:
                  FIND FIRST branch WHERE branch.branch-id EQ vCodF
                                                               NO-LOCK NO-ERROR.
                  vNAME = IF AVAIL branch THEN branch.name ELSE "".
               END.
            END.
         END CASE.
         oPickValue = vBIK  + CHR(1) + vINN  + CHR(1) + vKPP  + CHR(1) + 
                      vOGRN + CHR(1) + vRNOM + CHR(1) + vNAME + CHR(1) + 
                      vINDX + CHR(1) + vCREG + CHR(1) + vDIST + CHR(1) + 
                      vCITY + CHR(1) + vNASP + CHR(1) + vSTRT + CHR(1) +
                      vHOUS + CHR(1) + vCORP + CHR(1) + vAPRT + CHR(1) +
                      vBRAN + CHR(1) + STRING(vDATE).
         FORM
            vPrIzB vBIK  vINN  vKPP  vOGRN vRNOM vNAME vAdr  vINDX vCREG
            vDIST  vCITY vNASP vSTRT vHOUS vCORP vAPRT vBRAN vDATE
         WITH FRAME vfrm2
            OVERLAY CENTERED 1 COL SIDE-LABELS
            TITLE "[ СВЕДЕНИЯ О БАНКЕ ДО ВНЕСЕНИЯ ИЗМЕНЕНИЙ ]"
         .
         ON F1 OF vBIK IN FRAME vfrm2
         DO:
            DO TRANSACTION:
               RUN browseld.p ("banks", "SetFirstFrm", "4", "", 2).
            END.
            CLEAR FRAME vfrm2.
            IF (LASTKEY EQ 10   OR 
                LASTKEY  EQ 13) AND
                pick-value NE ?  THEN
            DO:
               FIND FIRST banks-code WHERE
                                         banks-code.bank-id EQ INT64(pick-value)
                                     AND banks-code.bank-code-type EQ "МФО-9"
               NO-LOCK NO-ERROR.
               IF AVAIL banks-code THEN
               DO:
/*
                  CLEAR FRAME vfrm2.
*/                ASSIGN
                     SELF:SCREEN-VALUE  = banks-code.bank-code
                     vDATE:SCREEN-VALUE = STRING(vDATE)
                  NO-ERROR.
                  FIND FIRST banks WHERE banks.bank-id EQ banks-code.bank-id
                  NO-LOCK NO-ERROR.
                  IF AVAIL banks THEN
                  DO:
                     FIND FIRST branch WHERE branch.bank-id EQ banks.bank-id
                     NO-LOCK NO-ERROR.

                     IF AVAIL branch THEN
                     DO:
                        vINNtmp = GetXAttrValue("branch",
                                          STRING(branch.branch-id), "ИНН").
                        IF NOT {assigned vINNtmp} THEN
                           IF {assigned banks.inn} THEN
                              vINNtmp = banks.inn.
                        vKPPtmp = GetXAttrValue("branch",
                                          STRING(branch.branch-id), "КПП").
                        IF NOT {assigned vKPPtmp} THEN
                           vKPPtmp = GetXAttrValue("banks",
                                             STRING(banks.bank-id), "КПП").
                         vOGRNtmp = GetXAttrValue("branch",
                                            STRING(branch.branch-id), "ОГРН").
                        IF NOT {assigned vOGRNtmp} THEN
                           vOGRNtmp = GetXAttrValue("banks", 
                                              STRING(banks.bank-id), "ОГРН").
                     END.
                     /*если не нашли в branch и banks, то ищем в cust-ident*/
                     IF NOT AVAIL branch       OR
                        NOT {assigned vINNtmp} THEN
                     DO:
                        FIND FIRST cust-ident
                             WHERE cust-ident.cust-id EQ banks.bank-id
                               AND cust-ident.cust-code-type EQ "ИНН"
                        NO-LOCK NO-ERROR.
                        IF AVAIL cust-ident THEN
                           IF {assigned cust-ident.cust-code} THEN
                              vINNtmp = cust-ident.cust-code.
                     END.
                     IF NOT AVAIL branch       OR
                        NOT {assigned vKPPtmp} THEN
                     DO:
                        FIND FIRST cust-ident
                             WHERE cust-ident.cust-id EQ banks.bank-id
                               AND cust-ident.cust-code-type EQ "КПП"
                        NO-LOCK NO-ERROR.
                        IF AVAIL cust-ident THEN
                           IF {assigned cust-ident.cust-code} THEN
                              vKPPtmp = cust-ident.cust-code.
                     END.
                     ASSIGN
                        vINN:SCREEN-VALUE  = 
                                   IF {assigned vINNtmp} THEN vINNtmp ELSE ""
                        vKPP:SCREEN-VALUE  =
                                   IF {assigned vKPPtmp} THEN vKPPtmp ELSE ""
                        vOGRN:SCREEN-VALUE =
                                   IF {assigned vOGRNtmp} THEN vOGRNtmp ELSE ""
                        vNAME:SCREEN-VALUE = banks.name
                        vAdr:SCREEN-VALUE  = banks.law-address
                     NO-ERROR.
                     FIND FIRST banks-code 
                                       WHERE banks-code.bank-id EQ banks.bank-id
                                         AND banks-code.bank-code-type EQ "REGN"
                     NO-LOCK NO-ERROR.
                     IF AVAIL banks-code THEN
                     DO:
                        ASSIGN
                           vRNOM:SCREEN-VALUE =
                                    ENTRY(1,banks-code.bank-code,"/")
                           vBRAN:SCREEN-VALUE =
                              IF NUM-ENTRIES(banks-code.bank-code,"/") GE 2 THEN
                                     ENTRY(2,banks-code.bank-code,"/")
                              ELSE ""
                        NO-ERROR.
                     END.
                  END.
               END. /*IF AVAIL banks-code*/
            END.    /*IF pick-value*/
         END.       /*ON F1 OF vBIK*/
         {echxac502-frm.nau vfrm2 vDATE}
         ON GO OF FRAME vfrm2
         DO:
            {echxac502-frm.chk &vchar = vBIK  &text = "БИК"}
            {echxac502-frm.chk &vchar = vINN  &text = "ИНН"}
            {echxac502-frm.chk &vchar = vKPP  &text = "КПП"}
            {echxac502-frm.chk &vchar = vOGRN &text = "ОГРН"}
            {echxac502-frm.chk &vchar = vRNOM &text = "Рег. НОМ"}
            {echxac502-frm.chk &vchar = vNAME &text = "Наименование"}
            {echxac502-frm.chk &vchar = vINDX &text = "Почт. индекс"}
            {echxac502-frm.chk &vchar = vCREG &text = "Код региона"}
            {echxac502-frm.chk &vchar = vBRAN &text = "Ном. Филиала"}
            {echxac502-frm.chk &vdate = vDATE &text = "Дата изменений"}
         END.
         {echxac502-frm.i vfrm2}
         DO TRANS ON ERROR UNDO, LEAVE ON ENDKEY UNDO, LEAVE:
            UPDATE 
               vPrIzB FORMAT "X(36)" LABEL "ПРИЗНАК ИЗМ."
               vBIK  vINN  vKPP  vOGRN vRNOM
               vNAME VIEW-AS FILL-IN SIZE 60 BY 1
               vAdr  VIEW-AS FILL-IN SIZE 60 BY 1
               vINDX vCREG vDIST vCITY vNASP vSTRT vHOUS vCORP vAPRT vBRAN 
               vDATE
            WITH FRAME vfrm2.
         END.
         HIDE FRAME vfrm2 NO-PAUSE.
         IF vChoice THEN
            oPickValue = STRING(vChoice).
         ELSE
            oPickValue = oPickValue + CHR(1) + 
                         vBIK  + CHR(1) + vINN  + CHR(1) + vKPP  + CHR(1) + 
                         vOGRN + CHR(1) + vRNOM + CHR(1) + vNAME + CHR(1) + 
                         vINDX + CHR(1) + vCREG + CHR(1) + vDIST + CHR(1) + 
                         vCITY + CHR(1) + vNASP + CHR(1) + vSTRT + CHR(1) +
                         vHOUS + CHR(1) + vCORP + CHR(1) + vAPRT + CHR(1) +
                         vBRAN + CHR(1) + STRING(vDATE) + CHR(1) + 
                         vCodF + CHR(1) + SUBSTRING(vPrIzB,1,1) + CHR(1) + 
                         vNomSoob.
      END. /*IF AVAIL branch THEN*/
   END.
   IF vChoice THEN
      oPickValue = STRING(vChoice).
END PROCEDURE.
/******************************************************************************/
/*Реквизиты банка (отмена)                                                    */
/*                                                                            */
/******************************************************************************/
PROCEDURE RequestBranchBCancel:

   DEFINE OUTPUT PARAMETER oPickValue AS CHARACTER NO-UNDO.

   {echxac502-frm.def}
   DEFINE VARIABLE vText   AS CHAR  NO-UNDO.
   DEFINE VARIABLE vIndDoc AS INT64 NO-UNDO.
   DEFINE VARIABLE vFile   AS CHAR  NO-UNDO.
   DEFINE VARIABLE vDocu   AS CHAR  NO-UNDO.

   FORM
     vBRNC
   WITH FRAME vfrm1
   .

   ON F1 OF vBRNC IN FRAME vfrm1
   DO:
      DO TRANSACTION:
         RUN browseld.p ("branch",                         
             "parent-id" + CHR(1) + 
             "title"     + CHR(1) + 
             "isbank",
             'Top' + CHR(1) + 
             "Организационная структура банка" + CHR(1) +
             'yes',
             ?,
             5).
      END.
      IF (LASTKEY EQ 10   OR 
          LASTKEY  EQ 13) AND
         pick-value NE ?  THEN
      DO:
         FIND FIRST branch WHERE branch.branch-id EQ pick-value
                                                               NO-LOCK NO-ERROR.
         IF AVAIL branch THEN
            ASSIGN
               vBRNC:SCREEN-VALUE = pick-value
            .
         RELEASE branch.
      END.
   END.
   ON GO OF FRAME vfrm1
   DO:
      {echxac502-frm.chk &vchar = vBRNC &text = "Подразделение-правопреемник"}
      FIND FIRST branch WHERE branch.branch-id EQ vBRNC:SCREEN-VALUE
                                                               NO-LOCK NO-ERROR.
      IF NOT AVAIL branch THEN
      DO:
         RUN Fill-AlertSysMes IN h_tmess ("","","1",
                    "Указанное подразделение не существует!~n" +
                    "Выберите подразделение-правопреемник по F1").
         RETURN NO-APPLY.
      END.
      ELSE
         IF NOT IsLastTaxExpWithImp("branch",
                                    vBRNC:SCREEN-VALUE,
                                    "TaxExpCommon5X",
                                    "TaxImp",
                                    vText,
                                    vFNam) THEN
         DO:
            RUN Fill-AlertSysMes IN h_tmess ("","","1",
                       "У данного подразделения отсутствуют~n" + 
                       "успешные сообщения экспорта").
            RETURN NO-APPLY.
         END.
         ELSE
         DO:
            ASSIGN
               vIndDoc = INDEX(vText,"<Документ")
               vFile   = SUBSTRING(vText,1,vIndDoc - 1)
               vDocu   = SUBSTRING(vText,vIndDoc,LENGTH(vText) - (vIndDoc - 1))
            NO-ERROR.
         END.
   END.
   {echxac502-frm.i vfrm1}
   DO TRANS ON ERROR UNDO, LEAVE ON ENDKEY UNDO, LEAVE:
      UPDATE  
         vBRNC LABEL "Подразделение"
      WITH FRAME vfrm1 SIDE-LABEL ROW 8 TITLE COLOR brigth-white 
         "[ ВЫБОР ПРАВОПРЕЕМНИКА ]" 1 COL OVERLAY 
         CENTERED.
   END.
   HIDE FRAME vfrm1 NO-PAUSE.

   IF vChoice THEN
      oPickValue = STRING(vChoice).
   ELSE
      oPickValue = vBRNC + CHR(1) + vFile + CHR(1) + vDocu + CHR(1) + vFNam.
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* $LINTFILE='echxac502-frm.p' */
/* $LINTMODE='1,5,6,3' */
/* $LINTENV ='dpl' */
/* $LINTVSS ='$/ws3-dpl/bq' */
/* $LINTUSER='ivrg' */
/* $LINTDATE='24/10/2016 10:23:34.778+03:00' */
/*prosigno/om3JZ6aWTLddHzJK5XgQ*/