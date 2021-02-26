/* 
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2017 АО "Банковские информационные системы"
     Filename: pp-acctn.p
      Comment: Библиотека МНС
   Parameters: нет
         Uses:
      Used by:
      Created: 01.07.2004 MIKE
     Modified: 08.02.2013 14:39 MUTA     <comment>
*/
{globals.i}

{intrface.get exch}
{intrface.get fx}
{intrface.get db2l}
{intrface.get strng}
{g-trans.equ}
{intrface.get xclass}
{intrface.get trans}
{exchange.equ}
{intrface.get count}
{intrface.get tmess}
{intrface.get pbase}
{intrface.get parsr}    /* Интерпретатор Bisquit Script (парсер). */
/* Вставка Плюс банк */
{sh-defs.i}
/* Конец вставки Плюс банк */

{pfuncdef
&DefLib="acctn"
&Description="Библиотека МНС"}

&GLOB NO-BASE-PROC YES

DEF VAR cl-acct-cat AS CHAR INIT "b,o,f,t,d" NO-UNDO. /* Список категорий учета */
DEF VAR vAcctChange AS CHAR NO-UNDO.
DEF VAR vNewAcct    AS CHARACTER NO-UNDO.
DEFINE VAR mIsCorrect   AS LOGICAL           NO-UNDO.

DEFINE TEMP-TABLE tt-taxcli NO-UNDO
   FIELD ClientId  AS INT64
   FIELD ClientCat AS CHARACTER
   FIELD ClientTyp AS CHARACTER
   FIELD INN       AS CHARACTER
   FIELD BRTD      AS DATE
   FIELD MBRD      AS CHARACTER
   FIELD CDUL      AS CHARACTER
   FIELD SERN      AS CHARACTER
   FIELD DDOC      AS DATE
   FIELD SURN      AS CHARACTER
   FIELD FNAM      AS CHARACTER
   FIELD SNAM      AS CHARACTER
   FIELD PrIz      AS CHARACTER
   FIELD KPP       AS CHARACTER
   FIELD OGRN      AS CHARACTER
   FIELD CLNAME    AS CHARACTER
   FIELD CHDATE    AS DATE
.

/* TT для восстановления старых значений ДР */
DEFINE TEMP-TABLE tt-del-pack NO-UNDO
   FIELD PacketId  AS INT64
   FIELD Acct      AS CHARACTER
   FIELD Currency  AS CHARACTER
   FIELD DateBeg   AS CHARACTER /*СНИНачДата*/
   FIELD Num       AS CHARACTER /*СНИНомПП*/
   FIELD Regn      AS CHARACTER /*СНИРегн*/
INDEX acct Acct Currency
INDEX packet PacketId
.


{pp-acctn.def}

vAcctChange = TRNSettingValue("","AcctChange","").
mIsCorrect  = TRNSettingValue("","Correction","НЕТ") =  "YES".

/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/

FUNCTION GetRefValue RETURN INT64 (INPUT iDate  AS DATE,
                                     INPUT iClass-Code  AS CHAR):
   FIND LAST  Reference WHERE
              Reference.op-date    =  iDate
          AND Reference.class-code =  iClass-Code
              NO-LOCK NO-ERROR.
   RETURN IF AVAIL Reference THEN INT64(Reference.RefValue) 
          ELSE 0.

END FUNCTION.


FUNCTION LoanDateFromStr RETURN DATE (INPUT iStr      AS CHAR,
                                     OUTPUT oLoanDate AS DATE,
                                     OUTPUT oLoanNum  AS CHAR): 

   oLoanDate = DATE(ENTRY(1,iStr))  NO-ERROR.
   oLoanNum  = IF NUM-ENTRIES(iStr) >= 2
                  THEN ENTRY(2,iStr)
                  ELSE "".
   oLoanNum = TRIM(oLoanNum).
END.

FUNCTION GetVers RETURN CHAR ():

   DEF VAR vVersForm      AS CHAR     NO-UNDO.
   DEF VAR hTaxExpCommon  AS handle   NO-UNDO.

   ASSIGN
      hTaxExpCommon  = GetTransObject("TaxExpCommon5")
      hTaxExpCommon  = hTaxExpCommon:default-buffer-handle
      NO-ERROR.
   IF NOT valid-handle(hTaxExpCommon) THEN 
   ASSIGN
      hTaxExpCommon  = GetTransObject("TaxExpCommonCH")
      hTaxExpCommon  = hTaxExpCommon:default-buffer-handle
      NO-ERROR.

   ASSIGN
      vVersForm      = hTaxExpCommon:buffer-field(GetMangledName("ВерсФорм")):buffer-value
      NO-ERROR.

   RETURN vVersForm.
END.

FUNCTION GetVersX RETURN CHAR (INPUT iObj AS CHAR):
   DEFINE VARIABLE vVersForm      AS CHAR   NO-UNDO.
   DEFINE VARIABLE hTaxExpCommon  AS HANDLE NO-UNDO.
   IF NOT {assigned iObj} THEN
      vVersForm = GetVers().
   ELSE
   DO:
      ASSIGN
         hTaxExpCommon = GetTransObject(iObj)
         hTaxExpCommon = hTaxExpCommon:DEFAULT-BUFFER-HANDLE
      NO-ERROR.
      IF VALID-HANDLE(hTaxExpCommon) THEN
         vVersForm =
          hTaxExpCommon:BUFFER-FIELD(GetMangledName("ВерсФорм")):BUFFER-VALUE.
      ELSE
         vVersForm = 
          GetAttrValue2(GetBaseOpKind(),GetBaseTemplate(),"$VersForm").
   END.
   RETURN vVersForm.
END.

FUNCTION LastTaxImp RETURN CHAR (INPUT iAcct      AS CHAR,
                              INPUT iCurrency  AS CHAR):

   DEF VAR vResult AS CHAR INIT "" NO-UNDO.

   FOR LAST PackObject WHERE
            PackObject.file-name =  'acct'
        AND PackObject.Surrogate =  iAcct + "," + iCurrency  NO-LOCK,
       EACH Packet     WHERE
            Packet.PacketID      =  PackObject.PacketID
        AND Packet.Kind          =  "TaxImp"
            NO-LOCK:

            FIND FIRST  FileExch   WHERE
                        FileExch.FileExchID  =  packet.FileExchID  NO-LOCK NO-ERROR.

      IF AVAIL FileExch   THEN
         vResult =  SUBSTRING(FileExch.name,3,1).
      LEAVE.
   END.

   RETURN vResult.
END FUNCTION.

/*----------------------------------------------------------------------------*/
/* Возвращает ID и Cust-Cat клиента                                           */
/*----------------------------------------------------------------------------*/
FUNCTION GetClientIDCAT RETURN LOGICAL (INPUT  iAcct      AS CHAR,
                                        OUTPUT oClientId  AS INT64,
                                        OUTPUT oClientCat AS CHAR):

   DEFINE VARIABLE vResult AS LOGICAL NO-UNDO INIT NO.
   DEFINE BUFFER cust-corp FOR cust-corp.
   DEFINE BUFFER person    FOR person.
   DEFINE BUFFER banks     FOR banks.
   {find-act.i
      &acct = iAcct
   }
   IF AVAIL acct THEN
   DO:
      CASE acct.cust-cat:
         WHEN "Ю" THEN
         DO:
            FIND FIRST cust-corp WHERE cust-corp.cust-id =  acct.cust-id
            NO-LOCK NO-ERROR.
            IF AVAIL cust-corp THEN
               ASSIGN
                  oClientId = cust-corp.cust-id
                  vResult   = YES
               NO-ERROR.
         END.
         WHEN "Ч" THEN
         DO:
            FIND FIRST person WHERE person.person-id =  acct.cust-id
            NO-LOCK NO-ERROR.
            IF AVAIL person THEN
               ASSIGN
                  oClientId = person.person-id
                  vResult   = YES
               NO-ERROR.
         END.
         WHEN "Б" THEN
         DO:
            FIND FIRST banks WHERE banks.bank-id =  acct.cust-id
            NO-LOCK NO-ERROR.
            IF AVAIL banks THEN
               ASSIGN
                  oClientId = banks.bank-id
                  vResult   = YES
               NO-ERROR.
         END.
      END CASE.
      oClientCat = acct.cust-cat.
   END.
   RETURN vResult.
END FUNCTION.
/*----------------------------------------------------------------------------*/
/* Создает запись ФЛ в tt-taxcli                                              */
/*----------------------------------------------------------------------------*/
FUNCTION Set-tt-taxcli-FL RETURN LOGICAL (INPUT iCustID     AS INT64,
                                          INPUT iCustCat    AS CHAR,
                                          INPUT iCliType    AS CHAR,
                                          INPUT iINN        AS CHAR,
                                          INPUT iBRTD       AS DATE,
                                          INPUT iMBRD       AS CHAR,
                                          INPUT iCDUL       AS CHAR,
                                          INPUT iSERN       AS CHAR,
                                          INPUT iDDOC       AS DATE,
                                          INPUT iSURN       AS CHAR,
                                          INPUT iFNAM       AS CHAR,
                                          INPUT iSNAM       AS CHAR,
                                          INPUT iDATE       AS DATE):
   DEFINE VARIABLE vResult AS LOGICAL NO-UNDO INIT YES.
   CREATE tt-taxcli.
   ASSIGN
      tt-taxcli.ClientId  = iCustID
      tt-taxcli.ClientCat = iCustCat
      tt-taxcli.ClientTyp = iCliType
      tt-taxcli.INN       = iINN
      tt-taxcli.BRTD      = iBRTD
      tt-taxcli.MBRD      = iMBRD
      tt-taxcli.CDUL      = iCDUL
      tt-taxcli.SERN      = iSERN
      tt-taxcli.DDOC      = iDDOC
      tt-taxcli.SURN      = iSURN
      tt-taxcli.FNAM      = iFNAM
      tt-taxcli.SNAM      = iSNAM
      tt-taxcli.CHDATE    = iDATE
   NO-ERROR.
   RETURN vResult.
END FUNCTION.
/*----------------------------------------------------------------------------*/
/* Создает запись Организации в tt-taxcli                                     */
/*----------------------------------------------------------------------------*/
FUNCTION Set-tt-taxcli-OR RETURN LOGICAL (INPUT iCustID     AS INT64,
                                          INPUT iCustCat    AS CHAR,
                                          INPUT iCliType    AS CHAR,
                                          INPUT iPrIz       AS CHAR,
                                          INPUT iINN        AS CHAR,
                                          INPUT iKPP        AS CHAR,
                                          INPUT iOGRN       AS CHAR,
                                          INPUT iNAME       AS CHAR,
                                          INPUT iDATE       AS DATE):
   DEFINE VARIABLE vResult AS LOGICAL NO-UNDO INIT YES.
   CREATE tt-taxcli.
   ASSIGN
      tt-taxcli.ClientId  = iCustID
      tt-taxcli.ClientCat = iCustCat
      tt-taxcli.ClientTyp = iCliType
      tt-taxcli.PrIz      = iPrIz
      tt-taxcli.INN       = iINN
      tt-taxcli.KPP       = iKPP
      tt-taxcli.OGRN      = iOGRN
      tt-taxcli.CLNAME    = iNAME
      tt-taxcli.CHDATE    = iDATE
   NO-ERROR.
   RETURN vResult.
END FUNCTION.
/*----------------------------------------------------------------------------*/
/* Создает запись ИП, адвоката, нотариуса, УТИТ в tt-taxcli                   */
/*----------------------------------------------------------------------------*/
FUNCTION Set-tt-taxcli-IP RETURN LOGICAL (INPUT iCustID     AS INT64,
                                          INPUT iCustCat    AS CHAR,
                                          INPUT iCliType    AS CHAR,
                                          INPUT iPrIz       AS CHAR,
                                          INPUT iINN        AS CHAR,
                                          INPUT iOGRN       AS CHAR,
                                          INPUT iSURN       AS CHAR,
                                          INPUT iFNAM       AS CHAR,
                                          INPUT iSNAM       AS CHAR,
                                          INPUT iDATE       AS DATE):
   DEFINE VARIABLE vResult AS LOGICAL NO-UNDO INIT YES.
   CREATE tt-taxcli.
   ASSIGN
      tt-taxcli.ClientId  = iCustID
      tt-taxcli.ClientCat = iCustCat
      tt-taxcli.ClientTyp = iCliType
      tt-taxcli.PrIz      = iPrIz
      tt-taxcli.INN       = iINN
      tt-taxcli.OGRN      = iOGRN
      tt-taxcli.SURN      = iSURN
      tt-taxcli.FNAM      = iFNAM
      tt-taxcli.SNAM      = iSNAM
      tt-taxcli.CHDATE    = iDATE
   NO-ERROR.
   RETURN vResult.
END FUNCTION.
/*----------------------------------------------------------------------------*/
/* Считывает запись из tt-taxcli                                              */
/*----------------------------------------------------------------------------*/
FUNCTION IsClientInTT RETURN LOGICAL (INPUT iCustID     AS INT64,
                                      INPUT iCustCat    AS CHAR,
                                      INPUT iCliType    AS CHAR):

   FIND FIRST tt-taxcli WHERE tt-taxcli.ClientId  =  iCustID
                          AND tt-taxcli.ClientCat =  iCustCat
                          AND tt-taxcli.ClientTyp =  iCliType
   NO-LOCK NO-ERROR.
   RETURN AVAIL (tt-taxcli).
END FUNCTION.
/*----------------------------------------------------------------------------*/
/* Считывает запись из tt-taxcli                                              */
/*----------------------------------------------------------------------------*/
FUNCTION Get-tt-taxcli RETURN LOGICAL (INPUT iCustID     AS INT64,
                                       INPUT iCustCat    AS CHAR,
                                       INPUT iCliType    AS CHAR,
                                      OUTPUT oPick-Value AS CHAR):
   DEFINE VARIABLE vResult AS LOGICAL NO-UNDO INIT NO.

   IF IsClientInTT(iCustID,iCustCat,iCliType) THEN
   DO:
      IF iCliType =  "7" THEN
         oPick-Value = tt-taxcli.INN  + CHR(1) + STRING(tt-taxcli.BRTD) + CHR(1)
                     + tt-taxcli.MBRD + CHR(1) + tt-taxcli.CDUL         + CHR(1)
                     + tt-taxcli.SERN + CHR(1) + STRING(tt-taxcli.DDOC) + CHR(1)
                     + tt-taxcli.SURN + CHR(1) + tt-taxcli.FNAM         + CHR(1)
                     + tt-taxcli.SNAM + CHR(1) + STRING(tt-taxcli.CHDATE).
      ELSE
         IF iCliType =  "1" OR iCliType =  "6" THEN
            oPick-Value = tt-taxcli.PrIz   + CHR(1) + tt-taxcli.INN +  CHR(1)
                        + tt-taxcli.KPP    + CHR(1) + tt-taxcli.OGRN + CHR(1)
                        + tt-taxcli.CLNAME + CHR(1) + STRING(tt-taxcli.CHDATE). 
         ELSE
            oPick-Value = tt-taxcli.PrIz + CHR(1) + tt-taxcli.INN +  CHR(1)
                        + tt-taxcli.OGRN + CHR(1) + tt-taxcli.SURN + CHR(1)
                        + tt-taxcli.FNAM + CHR(1) + tt-taxcli.SNAM + CHR(1)
                        + STRING(tt-taxcli.CHDATE). 
      vResult = YES.
   END.
   RETURN vResult.
END FUNCTION.
/*----------------------------------------------------------------------------*/
/* Был ли уже экспорт с такой транзакцией                                    */
/*----------------------------------------------------------------------------*/
FUNCTION IsTaxExp RETURN LOGICAL (INPUT iAcct      AS CHAR,
                                  INPUT iCurrency  AS CHAR):
   

   DEF VAR vResult AS LOGICAL INIT NO NO-UNDO.
   DEF VAR vOpKind AS CHAR            NO-UNDO. 

   vOpKind = GetBaseOpKind().

   FOR FIRST PackObject WHERE
             PackObject.file-name =  'acct'
         AND PackObject.Surrogate =  iAcct + "," + iCurrency  NO-LOCK,
        EACH Packet     WHERE
             Packet.PacketID      =  PackObject.PacketID
         AND Packet.Kind          =  "TaxExp"
             NO-LOCK,
        EACH Seance WHERE Seance.SeanceID =  Packet.SeanceID
         AND Seance.op-kind       =  vOpKind
             NO-LOCK,
             FIRST FileExch WHERE
                   FileExch.FileExchID  =  packet.FileExchID  
             NO-LOCK:
         vResult =  YES.
      LEAVE.
   END.

   RETURN vResult.
END FUNCTION.
/*----------------------------------------------------------------------------*/
/* Является ли клиент предпринимателем                                        */
/*----------------------------------------------------------------------------*/
FUNCTION IsCliBusi RETURN LOGICAL (INPUT iAcct  AS CHAR):

   DEFINE VARIABLE        vCode          AS CHARACTER NO-UNDO.
   DEFINE VARIABLE        vIsPred        AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE        vIndivBusi     AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE        vAcctPred      AS CHARACTER NO-UNDO.

   vIndivBusi = TRNSettingValue("ExchSET-BNK", "IndivBusi", "Предпр") BEGINS "Пред".
   vIsPred    = NO.

   {find-act.i
      &acct = iAcct
   }

   IF AVAIL acct THEN DO:
      vAcctPred = GetXAttrValueEx("acct",
                                  acct.acct + "," + acct.currency,
                                  "Предпр", "").

      CASE acct.cust-cat:
          WHEN "Ч"  THEN DO:
             FIND FIRST person WHERE  person.person-id =  acct.cust-id NO-LOCK NO-ERROR.
             IF AVAIL person THEN DO:
                vIsPred = vAcctPred BEGINS "Пред".

                IF NOT vIsPred AND
                   vIndivBusi THEN
                   vIsPred = GetXAttrValueEx("person",
                                             STRING(person.person-id),
                                            "Предпр", "") BEGINS "Пред".
             END.
          END.
          WHEN "Ю"  THEN DO:
             FIND FIRST cust-corp WHERE  cust-corp.cust-id =  acct.cust-id NO-LOCK NO-ERROR.
             IF AVAIL cust-corp THEN DO:
                vIsPred = vAcctPred BEGINS "Пред".
                IF vIndivBusi THEN DO:
                   IF NOT vIsPred THEN
                      vIsPred = GetXAttrValueEx("cust-corp",
                                                STRING(cust-corp.cust-id),
                                                "Предпр", "") BEGINS "Пред".
                END.
                ELSE
                   ASSIGN
                      vCode   = GetCodeVal ("КодПредп",cust-corp.cust-stat)
                      vIsPred = {assigned GetCodeMisc('КодПредп',vCode,2)}.
             END.
          END.
          WHEN "Б"  THEN DO:
             FIND FIRST banks WHERE  banks.bank-id =  acct.cust-id NO-LOCK NO-ERROR.
                ASSIGN
                   vIsPred = NO.
          END.
          OTHERWISE.
      END CASE.
   END.
   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("IsCliBusi"," iAcct:"      + GetNullStr(iAcct)  +
                               " vIndivBusi:" + STRING(vIndivBusi) +  
                               " vIsPred:"    + STRING(vIsPred)).  
   &ENDIF

   RETURN vIsPred.

END FUNCTION.

FUNCTION GetCliType RETURN CHAR (INPUT iAcctSurr AS CHAR, INPUT iObj AS CHAR):
   DEF VAR vVidITD      AS CHAR  NO-UNDO.
   DEF VAR vCliType     AS CHAR  NO-UNDO.
   DEF VAR vPredpr      AS CHAR  NO-UNDO.
   DEF VAR vAlgorSt     AS INT64 NO-UNDO.
   DEF VAR vFLMask      AS CHAR  NO-UNDO.

   vFLMask = fGetSetting("ГНИ","СчетаФЛ","423*,40817*,426*,40820*").
 
   {find-act.i
      &acct     = ENTRY(1,iAcctSurr)
      &currency = ENTRY(2,iAcctSurr)
   }
   IF AVAIL acct THEN
   IF GetVersX(iObj) >= "5.10" THEN
   DO:
      CASE acct.cust-cat:
         WHEN "Ч"  THEN DO:
            vVidITD = GetXAttrValueEx("person",STRING(acct.cust-id),"ВидИТД", "").
            vPredpr = GetXAttrValueEx("person",STRING(acct.cust-id),"Предпр", "").
            vCliType = IF (NOT {assigned vVidITD} AND NOT {assigned vPredpr}) OR 
                          CAN-DO(vFLMask,acct.acct)  THEN "7"
                       ELSE IF vVidITD =  "Нотариус" THEN "3"
                       ELSE IF vVidITD =  "Адвокат"  THEN "4"
                       ELSE IF vVidITD =  "УТИТ"     THEN "5"
                       ELSE IF IsCliBusi(acct.acct)  THEN "2"
                       ELSE "0".
          END.
          WHEN "Ю"  THEN DO:
            vVidITD = GetXAttrValueEx("cust-corp",STRING(acct.cust-id),"ВидИТД", "").
            vPredpr = GetXAttrValueEx("cust-corp",STRING(acct.cust-id),"Предпр", "").
            FIND FIRST cust-corp WHERE cust-corp.cust-id = acct.cust-id NO-LOCK NO-ERROR.
            IF AVAIL cust-corp THEN
               IF NOT {assigned vVidITD} AND
                  NOT {assigned vPredpr} THEN
                  vCliType =      IF cust-corp.cust-stat  =  "УТИТ" THEN "5"
                             ELSE IF cust-corp.country-id <> "RUS"  THEN "6"
                             ELSE "1".
               ELSE
               DO:
                  vAlgorSt = INT64(FGetSetting("ГНИ","АлгоритмСт","")).
                  IF vAlgorSt = 2 THEN
                     vCliType =      IF cust-corp.cust-stat =  "ИП"       OR
                                        cust-corp.cust-stat =  "ГКФК"     OR
                                        cust-corp.cust-stat =  "АрбитУпр" THEN "2"
                                ELSE IF cust-corp.cust-stat =  "Нотариус" THEN "3"
                                ELSE IF cust-corp.cust-stat =  "Адвокат"  THEN "4"
                                ELSE IF {assigned vPredpr}                AND
                                        cust-corp.cust-stat =  "УТИТ"     THEN "5"
                                ELSE "2".
                  ELSE       /*если НП не определен, то используем алгоритм 1*/
                     vCliType =      IF vVidITD =  "Нотариус"  THEN "3"
                                ELSE IF vVidITD =  "Адвокат"   THEN "4"
                                ELSE IF {assigned vPredpr}     AND
                                        vVidITD =  "УТИТ"      THEN "5"
                                ELSE IF vPredpr BEGINS "Пред"  AND
                                        NOT {assigned vVidITD} THEN "2"
                                ELSE "2".
                             /*спецификация не оговаривает значение vCliType*/
                             /*если ни одно условие не выполнится*/
                             /*решение для ELSE vCliType = "2" (ИП)*/
               END.
          END.
          WHEN "Б" THEN DO:
             FIND FIRST banks WHERE banks.bank-id =  acct.cust-id NO-LOCK NO-ERROR.
             IF AVAIL banks THEN
                vCliType = IF banks.country-id <> "RUS" THEN "6" ELSE "1".
          END.
          OTHERWISE
             vCliType = "1".
      END CASE.
   END.
   ELSE /*Если версия не 5.10*/
   DO:
      CASE acct.cust-cat:
         WHEN "Ч"  THEN DO:
            vVidITD = GetXAttrValueEx("person",
                                      STRING(acct.cust-id),
                                      "ВидИТД", "").

            vCliType =      IF vVidITD =  "Нотариус" THEN "3"
                       ELSE IF vVidITD =  "Адвокат"  THEN "4"
                       ELSE IF IsCliBusi(acct.acct)  THEN "2" 
                       ELSE "0".
          END.
          WHEN "Ю"  THEN DO:
            vVidITD = GetXAttrValueEx("cust-corp",
                                      STRING(acct.cust-id),
                                      "ВидИТД", "").

            vCliType =      IF vVidITD =  "Нотариус" THEN "3"
                       ELSE IF vVidITD =  "Адвокат"  THEN "4"
                       ELSE IF IsCliBusi(acct.acct)  THEN "2"
                       ELSE "1".
          END.
          OTHERWISE
             vCliType = "1".
      END CASE.
   END.
   RETURN vCliType.
END FUNCTION.

PROCEDURE eИдФайл:
   DEFINE INPUT PARAMETER iIns   AS HANDLE  NO-UNDO.
   DEFINE INPUT PARAMETER iTag   AS CHAR    NO-UNDO.
   DEFINE INPUT PARAMETER iValue AS CHAR    NO-UNDO.

   DEF VAR vText     AS CHAR NO-UNDO.
   DEF VAR vNumFile  AS INT64  NO-UNDO.

   iTag     = GetMangledName(iTag).

   vNumFile  = GetRefValue(TODAY,"RTaxFile") + 1.

   vText = SUBSTRING(FGetSetting("ИНН",?,""),1,10) + 
           "**" + FGetSetting("БанкКПП",?,"")      +
           STRING(YEAR(TODAY))                     + 
           STRING(MONTH(TODAY))                    +
           STRING(DAY(TODAY))                      +
           SUBSTRING(STRING(TIME,"hh:mm:ss"),1,2)  +     
           SUBSTRING(STRING(TIME,"hh:mm:ss"),4,2)  +     
           SUBSTRING(STRING(TIME,"hh:mm:ss"),7,2)  +
           STRING(vNumFile,"999999"). 

   RUN SetValue in h_exch (iIns, iTag,vText, 0).
END PROCEDURE.

/*----------------------------------------------------------------------------*/
PROCEDURE eНомерДог:
   DEFINE INPUT PARAMETER iIns   AS HANDLE  NO-UNDO.
   DEFINE INPUT PARAMETER iTag   AS CHAR    NO-UNDO.
   DEFINE INPUT PARAMETER iValue AS CHAR    NO-UNDO.

   DEF VAR oLoanDate    AS DATE   NO-UNDO.
   DEF VAR oLoanDateCls AS DATE   NO-UNDO.
   DEF VAR oLoanNum     AS CHAR   NO-UNDO.
   DEF VAR vLoanCode    AS CHAR   NO-UNDO.

   RUN LoanNum (INPUT iIns, 
                INPUT iTag,
                INPUT iValue,
                "",
                OUTPUT oLoanDate,
                OUTPUT oLoanDateCls,
                OUTPUT vLoanCode 
                ).

   RUN SetValue in h_exch (iIns,GetMangledName("ДатаДог"),STRING(oLoanDate,"99.99.9999"), 0) NO-ERROR.

END PROCEDURE.

/*----------------------------------------------------------------------------*/

PROCEDURE eНомерДог5:
   DEFINE INPUT PARAMETER iIns   AS HANDLE  NO-UNDO.
   DEFINE INPUT PARAMETER iTag   AS CHAR    NO-UNDO.
   DEFINE INPUT PARAMETER iValue AS CHAR    NO-UNDO.

   DEF VAR oLoanDate    AS DATE   NO-UNDO.
   DEF VAR oLoanDateCls AS DATE   NO-UNDO.
   DEF VAR oLoanNum     AS CHAR   NO-UNDO.
   DEF VAR vLoanKind    AS CHAR   NO-UNDO.
   DEF VAR vLoanCode    AS CHAR   NO-UNDO.
   DEF VAR vKPPNP       AS CHAR   NO-UNDO.
   DEF VAR vAcct        AS CHAR   NO-UNDO.
   DEF VAR vExchMain    AS CHAR   NO-UNDO.
   DEF VAR vVersForm    AS CHAR   NO-UNDO.

   vExchMain   = GetValue(iIns,GetMangledName("ExchMain")).
   vVersForm   = GetVersX(vExchMain).

   RUN LoanNum (INPUT iIns,
                INPUT iTag,
                INPUT iValue,
                INPUT vVersForm,
                OUTPUT oLoanDate,
                OUTPUT oLoanDateCls,
                OUTPUT vLoanCode).

IF vVersForm >= "5.10" THEN
DO:
   IF GetAttrValue2("",0,"OpKindKind") =  "CLOSE" THEN
   DO:
      /*условие по 233550*/
      IF NOT {assigned vLoanCode}               AND
         oLoanDateCls                <> ?       THEN 
            vLoanCode = "По заявлению клиента".

      CASE vLoanCode:
         WHEN ""                      THEN vLoanKind = "0".
         WHEN "По заявлению клиента"  THEN vLoanKind = "1".
         WHEN "Банком в одн. порядке" THEN vLoanKind = "2".
         WHEN "По решению суда"       THEN vLoanKind = "3".
      END CASE.
      RUN SetValue in h_exch (iIns,GetMangledName("КодСостДог"),vLoanKind,0).
      RUN SetValue in h_exch (iIns,GetMangledName("ДатаЗаклДог"),"",0) NO-ERROR.
    
      IF vLoanKind <> "0" THEN
         RUN SetValue in h_exch (iIns,GetMangledName("ДатаРастДог"),STRING(oLoanDateCls,"99.99.9999"), 0) NO-ERROR.
      ELSE
         RUN SetValue in h_exch (iIns,GetMangledName("ДатаРастДог"),"", 0) NO-ERROR.
   END.
   ELSE
   DO:
      RUN SetValue in h_exch (iIns,GetMangledName("КодСостДог"),"0",0).
      RUN SetValue in h_exch (iIns,GetMangledName("ДатаЗаклДог"),STRING(oLoanDate,"99.99.9999"),0) NO-ERROR.
      RUN SetValue in h_exch (iIns,GetMangledName("ДатаРастДог"),"", 0) NO-ERROR.
   END.
END.
ELSE /*ВЕРСИИ ДО 5.10*/
DO:
   IF GetAttrValue2("",0,"OpKindKind") =  "CLOSE" THEN DO:
      RUN SetValue in h_exch (iIns,GetMangledName("ДатаЗаклДог"),"",0) NO-ERROR.
      IF oLoanDateCls <> ? THEN  
         RUN SetValue in h_exch (iIns,GetMangledName("ДатаРастДог"),STRING(oLoanDateCls,"99.99.9999"), 0) NO-ERROR.
      ELSE 
         RUN SetValue in h_exch (iIns,GetMangledName("ДатаРастДог"),"", 0) NO-ERROR.
   END.
   ELSE DO:
      RUN SetValue in h_exch (iIns,GetMangledName("ДатаЗаклДог"),STRING(oLoanDate,"99.99.9999"),0) NO-ERROR.
      RUN SetValue in h_exch (iIns,GetMangledName("ДатаРастДог"),"", 0) NO-ERROR.
   END.

   RUN SetValue in h_exch (iIns,GetMangledName("ДатаРастДог"),STRING(oLoanDateCls,"99.99.9999"), 0) NO-ERROR.

   IF  oLoanDateCls <> ? THEN DO:
        vLoanKind = IF GetVersX(vExchMain) >= "5.01" THEN "0" ELSE "2". 
   END.
   ELSE vLoanKind = "1".
    
   IF GetVersX(vExchMain) =  "5.01" THEN RUN SetValue in h_exch (iIns,GetMangledName("КодСостДог"),vLoanKind,0).
                          ELSE RUN SetValue in h_exch (iIns,GetMangledName("ПризнДог"),  vLoanKind,0).
   IF GetVersX(vExchMain) =  "5.02" THEN
   DO:
       /*235787, 239733: Для версии формата 5.02!
         Если формируется сообщение о закрытии счета, то в реквизит
         "КодСостДог" надо проставлять значение 
         <1> - для электронного сообщения о закрытии счета по заявлению клиента,
         ДатаРастДог=ДатаЗакрСч
         Т.о. ушло условие по oLoanDateCls, т.к. ситуация справедлива, как
         для пролонгации (oLoanDateCls EQ ?), так и для закрытого договора
         (oLoanDateCls NE ?)*/
      IF GetAttrValue2("",0,"OpKindKind") =  "CLOSE" AND
         NOT {assigned vLoanCode}               THEN 
      ASSIGN
         vLoanCode = "По заявлению клиента".

      IF vAcctChange =  "Нет" THEN vLoanKind = "".
      ELSE
      CASE vLoanCode:
         WHEN ""                      THEN vLoanKind = "0".
         WHEN "По заявлению клиента"  THEN vLoanKind = "1".
         WHEN "Банком в одн. порядке" THEN vLoanKind = "2".
         WHEN "По решению суда"       THEN vLoanKind = "3".
      END CASE.
      RUN SetValue in h_exch (iIns,GetMangledName("КодСостДог"),vLoanKind,0).
   END.
END.

IF GetVersX(vExchMain) =  "5.02" OR GetVersX(vExchMain) >= "5.10" THEN DO:

      IF GetAttrValue2("",0,"OpKindKind") =  "CLOSE" THEN DO:
         /*алгоритм формирования поля КППНП*/
         vKPPNP = "".
         vAcct  = GetValue(iIns,GetMangledName("НомСч")).
         {find-act.i
            &acct   = vAcct
         }
         IF AVAIL acct AND acct.cust-cat =  "Ю" THEN DO:
            FIND FIRST cust-corp WHERE cust-corp.cust-id =  acct.cust-id NO-LOCK NO-ERROR.
            IF LENGTH(cust-corp.inn) <= 10 OR
               GetXAttrValueEX("cust-corp",STRING(acct.cust-id),"КИО", "") <> "" OR
               vLoanKind =  "1"
            THEN
            vKPPNP = GetXAttrValueEx("cust-corp",
                                     STRING(acct.cust-id),
                                     "КПП", vKPPNP).
         END.
         IF AVAIL acct AND acct.cust-cat =  "Б" THEN DO:
            FIND FIRST banks WHERE banks.bank-id =  acct.cust-id NO-LOCK NO-ERROR.
            IF LENGTH(banks.inn) <= 10 OR
               GetXAttrValueEX("banks",STRING(acct.cust-id),"КИО", "") <> "" OR
               vLoanKind =  "1"
            THEN
            vKPPNP = GetXAttrValueEx("banks",
                                     STRING(acct.cust-id),
                                     "КПП", vKPPNP).
         END.
         RUN SetValue in h_exch (iIns,GetMangledName("КППНП"),vKPPNP,0).
      END.
      IF IsCliBusi(acct.acct)  THEN
         RUN SetValue in h_exch (iIns,GetMangledName("КППНП"),"",0).
   END.

END PROCEDURE.

/*----------------------------------------------------------------------------*/
PROCEDURE LoanNum:
   DEFINE INPUT  PARAMETER iIns         AS HANDLE  NO-UNDO.
   DEFINE INPUT  PARAMETER iTag         AS CHAR    NO-UNDO.
   DEFINE INPUT  PARAMETER iValue       AS CHAR    NO-UNDO.
   DEFINE INPUT  PARAMETER iVersForm    AS CHAR    NO-UNDO.
   DEFINE OUTPUT PARAMETER vLoanDate    AS CHAR    NO-UNDO.
   DEFINE OUTPUT PARAMETER vLoanDateCls AS CHAR    NO-UNDO.
   DEFINE OUTPUT PARAMETER vLoanCode    AS CHAR    NO-UNDO.
   
   DEF VAR hAcct        AS HANDLE NO-UNDO.
   DEF VAR vAcct        AS CHAR   NO-UNDO.
   DEF VAR vAcct-surr   AS CHAR   NO-UNDO.
   DEF VAR vOutDate     AS DATE   NO-UNDO.
   DEF VAR vLoanNum     AS CHAR   NO-UNDO.
   DEF VAR vText        AS CHAR   NO-UNDO.
   DEF VAR vExchMain    AS CHAR    NO-UNDO.
   DEF VAR vVersion     AS CHAR    NO-UNDO.
   DEF VAR vPostfix     AS CHAR   NO-UNDO.
/* Вставка Плюс банк */
   DEFINE VARIABLE mNom      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE mSymb     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vContract AS CHARACTER NO-UNDO.
   DEFINE VARIABLE mInt      AS INT64     NO-UNDO.
/* Конец вставки Плюс банк */

/* 56955
Необходимо проанализировать дополнительный реквизит счета <ДогОткрЛС>.
ЕСЛИ Он заполнен, то информация о договоре должна печататься на основании этого
     дополнительного реквизита.
ИНАЧЕ Необходимо определить эту информацию из связанных со счетом договоров
      по следующему алгоритму.
      Определяется список ролей счетов из значения нового (см гл.3.1)
      дополнительного реквизита отчета <РолиИзДог>.
        Для счета ищется связанная запись в таблице договоров (loan).
        Сначала ищется запись в таблице счетов договоров (loan-acct).
        Она ищется по одному из трех возможных условий:
        1. loan-acct.acct = <номер обрабатываемого счета>
           AND loan-acct.contract = <Кредит>
           AND loan-acct.acct-type = одному из значений, перечисленных
               в дополнительном реквизите отчета <РолиИзДог>
           AND наибольшее значение даты loan-acct.since.
        2. loan-acct.acct = <номер обрабатываемого счета>
           AND loan-acct.contract = <Депоз>
           AND loan-acct.acct-type = одному из значений, перечисленных
               в дополнительном реквизите отчета <РолиИзДог>
           AND наибольшее значение даты loan-acct.since.
        3. loan-acct.acct = <номер обрабатываемого счета>
           AND loan-acct.contract = <dps>
           AND loan-acct.acct-type = одному из значений, перечисленных
               в дополнительном реквизите отчета <РолиИзДог>
           AND наибольшее значение даты loan-acct.since.
        ЕСЛИ запись найдена, то номер договора это loan-acct.cont-code.
             По паре полей loan-acct.cont-code и loan-acct.contract
             ищется связанная запись в таблице loan.
             Оттуда берется дата открытия loan.open-date.
        КОНЕЦ_ЕСЛИ
КОНЕЦ_ЕСЛИ
*/
   ASSIGN
      vExchMain   = GetValue(iIns,GetMangledName("ExchMain"))
      vVersion    = GetVersX(vExchMain)
      iTag      = GetMangledName(iTag)
      vAcct     = GetValue(iIns,GetMangledName("НомСч"))
      vLoanNum  = iValue
      vPostfix  = TRNSettingValue("ExchSET-BNK","Postfix","")
   NO-ERROR.

   {find-act.i
      &acct   = vAcct
   }
   IF AVAIL acct THEN DO:
      ASSIGN
         vLoanDate    = ""
         vLoanDateCls = ""
         vLoanDate    = STRING(acct.open-date,"99/99/9999")
                  WHEN acct.open-date <> ?
         vLoanDateCls = STRING(acct.close-date,"99/99/9999")
                  WHEN acct.close-date <> ?
         vAcct-surr = acct.acct + "," + acct.currency
         vText      = TRIM( GetXAttrValueEx("acct",vAcct-Surr,"ДогОткрЛС",""))
      .
   
      IF {assigned vText} THEN DO:
         LoanDateFromStr(INPUT  vText,
                         OUTPUT vOutDate,
                         OUTPUT vLoanNum).
   
         vLoanDate = STRING(vOutDate,"99.99.9999").
      END.
      IF NOT {assigned vText} OR iVersForm >= "5.10" THEN DO:
         FOR EACH loan-acct OF acct WHERE CAN-DO(mContAcct, loan-acct.contract)
                                      AND CAN-DO(mRuleAcct, loan-acct.acct-type)
            NO-LOCK,
            FIRST loan OF loan-acct NO-LOCK BY loan-acct.since DESCENDING:

            IF NOT {assigned vText} THEN    
               ASSIGN
                  vLoanDate = STRING(loan.open-date,"99/99/9999")
/* Вставка Плюс банк */
                  vContract = loan.contract
/* Конец вставки Плюс банк */
                  vLoanNum  = loan.cont-code.

            vLoanDateCls = STRING(loan.close-date,"99/99/9999").

            LEAVE.
         END.
      END.
   END.
   ASSIGN
      vLoanCode = TRIM(GetXAttrValueEx("acct",vAcct-Surr,"СНИПрич","")).
      vLoanNum  = TRIM(vLoanNum)
   .
/* Вставка Плюс банк */
   IF shFilial EQ "0300" THEN
   DO:
      IF     vContract        EQ "DPS"
         AND LENGTH(vLoanNum) GT 25 THEN
      DO:
         mNom = ENTRY(1,vLoanNum,"@").
         REPEAT mInt = 1 TO 100:
            IF LENGTH(mNOm) EQ 1 THEN LEAVE.
            mSymb = SUBSTRING(mNom,LENGTH(mNOm)).
            IF mSymb NE "/"
            THEN mNom = SUBSTRING(mNom,1,LENGTH(mNOm) - 1 ). 
            ELSE LEAVE.
         END.       
         mNom = TRIM(mNom,"/").
         ENTRY(1,vLoanNum,"@") = mNom.
      END.
   END.
   
   IF shFilial EQ "0500" THEN
   DO:
      IF vContract EQ "DPS" THEN
      DO:
         mNom = ENTRY(1,vLoanNum,"@").
         REPEAT mInt = 1 TO 100:
            IF LENGTH(mNOm) EQ 1 THEN LEAVE.
            mSymb = SUBSTRING(mNom,LENGTH(mNOm)).
            IF mSymb NE "/"
            THEN mNom = SUBSTRING(mNom,1,LENGTH(mNOm) - 1 ). 
            ELSE LEAVE.
         END.       
         mNom = TRIM(mNom,"/").
         ENTRY(1,vLoanNum,"@") = mNom.
      END.
   END.
/* Конец вставки Плюс банк */

   IF {assigned vPostfix} THEN
         /*чтобы исключить, обнаружение постфикса в середине: 012xx12*/
      IF R-INDEX(vLoanNum, vPostfix) =  ((LENGTH(vLoanNum) - LENGTH(vPostFix)) + 1) THEN
         ASSIGN vLoanNum = SUBSTRING(vLoanNum, 1, R-INDEX(vLoanNum, vPostfix) - 1).

   IF NOT {assigned vLoanNum} THEN vLoanNum = "".
   IF GetVersX(vExchMain) >= "5.10" THEN
      RUN SetValue in h_exch (iIns, iTag,DelFilFromAcct(vLoanNum), 0).
   ELSE
      RUN SetValue in h_exch (iIns, iTag,vLoanNum, 0).
END PROCEDURE.

/*----------------------------------------------------------------------------*/
PROCEDURE еНомСооб:
   DEFINE INPUT PARAMETER iIns   AS HANDLE  NO-UNDO.
   DEFINE INPUT PARAMETER iTag   AS CHAR    NO-UNDO.
   DEFINE INPUT PARAMETER iValue AS CHAR    NO-UNDO.

   DEF VAR vNumMess     AS CHAR    NO-UNDO.
   DEF VAR vNumAcct     AS CHAR    NO-UNDO.
   DEF VAR vNumYear     AS CHAR    NO-UNDO.
   DEF VAR vBegDate     AS DATE    NO-UNDO.
   DEF VAR vFirstDate   AS DATE    NO-UNDO.
   DEF VAR vAcct        AS CHAR    NO-UNDO.
   DEF VAR vAcct-surr   AS CHAR    NO-UNDO.
   DEF VAR vMessTmp     AS CHAR    NO-UNDO.
   DEF VAR vRRRR        AS CHAR    NO-UNDO.
   DEF VAR vRegn        AS CHAR    NO-UNDO.
   DEF VAR vRegn1       AS CHAR    NO-UNDO.
   DEF VAR vRegn2       AS CHAR    NO-UNDO.
   DEF VAR codFil       AS CHAR    NO-UNDO.
   DEF VAR vLastTaxImp  AS CHAR    NO-UNDO.
   DEF VAR vError       AS CHAR    NO-UNDO.
   DEF VAR vErrorList   AS CHAR    NO-UNDO.
   DEF VAR vMMM         AS CHAR    NO-UNDO.
   DEF VAR vChoice      AS LOGICAL NO-UNDO.
   DEF VAR vSniNomPP    AS CHAR    NO-UNDO.
   DEF VAR vCliType     AS CHAR    NO-UNDO.
   DEF VAR mFlErr       AS INT64 NO-UNDO. /* Флаг ошибки. */
   DEF VAR mResult      AS CHAR    NO-UNDO. /* Результатработы парсера. */
   DEF VAR mLibPars     AS CHAR    INIT "ptax"  NO-UNDO.  /* Библиотеки парсера. */
   DEF VAR vReturn      AS CHAR    NO-UNDO.
   DEF VAR vFormatN     AS CHAR    NO-UNDO.
   DEF VAR vAddInd      AS INT64 NO-UNDO.
   DEF VAR vExchMain    AS CHAR    NO-UNDO.
   DEF VAR vCurrCnt     AS CHAR    NO-UNDO INIT "".

   DEF VAR mRefusal     AS LOGICAL NO-UNDO.

   DEF BUFFER bSigns FOR signs.
   DEF BUFFER cSigns FOR signs.
   DEF BUFFER dSigns FOR signs.
   DEF BUFFER bAcct  FOR acct.

   mIsCorrect  = TRNSettingValue("ExchSET-BNK","Correction","НЕТ") =  "YES".
   iTag        = GetMangledName(iTag).
   vAcct       = GetValue(iIns,GetMangledName("НомСч")).
   vReturn     = "".
   vFormatN    = FGetSetting("ГНИ","ФорматНомера","5.01").
   mRefusal    = TRNSettingValue("","Refusal","НЕТ") =  "ДА".
   vNewAcct    = GetValue(iIns,GetMangledName("НомИзмСч")) NO-ERROR.
   vExchMain   = GetValue(iIns,GetMangledName("ExchMain")) NO-ERROR.
   {find-act.i &acct=vAcct}
   vAcct    = DelFilFromAcct(vAcct).
   RUN SetValue in h_exch (iIns,GetMangledName("НомСч"), vAcct, 0) NO-ERROR.
   IF GetVersX(vExchMain) >= "5.10" THEN 
      RUN SetValue in h_exch (iIns,GetMangledName("Choice"),
                              STRING(YES),0) NO-ERROR.
   IF {assigned vNewAcct} THEN DO:

      {find-act.i 
          &acct=vNewAcct
          &bact=bacct}
      IF AVAIL(bacct) THEN vNewAcct = bacct.acct + "," + bacct.currency.
      ELSE vNewAcct = "".

   END.
&IF DEFINED(IS-DEBUG) &THEN
RUN dbgprint.p ("еНомСооб","vAcct:" + vAcct +
                " Avail:"           +  STRING(AVAIL acct) +
                " mRefusal:"        + STRING(mRefusal) + 
                " vRegn:" + string(vRegn) +  
                " vMessTmp:" + string(vMessTmp) +  
                " codFil:" + string(codFil)).
&ENDIF

   codFil   = FGetSetting("КодФил", ?, "").
   vRegn    = GetXAttrValueEx("branch", codFil, "REGN","0").
   vNumAcct = STRING(INT64(ENTRY(1,vRegn,"/")),"9999"). 
   vMessTmp = STRING(INT64(IF NUM-ENTRIES(vRegn,"/") >= 2 THEN ENTRY(2,vRegn,"/")
                                             ELSE "0"),"9999"). 
    codFil  = vMessTmp.

&IF DEFINED(IS-DEBUG) &THEN
RUN dbgprint.p ("еНомСооб", "vRegn:"     +  vRegn   +
                            " vMessTmp:" + vMessTmp +
                            " vNumAcct:" + GetNULLSTR(vNumAcct) +
                            " codFil:" + string(codFil)).
&ENDIF
   IF AVAIL acct THEN DO:
      ASSIGN 
          vAcct-surr = acct.acct + "," + acct.currency.
   END.
 
  RUN fill-acct-tt-dp IN THIS-PROCEDURE 
     (IF {assigned vNewAcct} THEN
         GetEntries(1, vNewAcct, ",", "")
      ELSE GetEntries(1, vAcct-surr, ",", ""),
      IF {assigned vNewAcct} THEN
         GetEntries(2, vNewAcct, ",", "")
      ELSE GetEntries(2, vAcct-surr, ",", "")
     ).
   IF AVAIL acct THEN DO:

      vRRRR = SUBSTRING(GetXAttrValue("acct",vAcct-Surr,"СНИРегн"),1,4).
      IF {assigned vRRRR} THEN DO:
         vNumAcct = vRRRR.
         vMessTmp = SUBSTRING(GetXAttrValue("acct",vAcct-Surr,"СНИРегн"),5,4).
      END.
      ELSE DO:
         ASSIGN
            vRegn1 = STRING(INT64(ENTRY(1,vRegn,"/")),"9999")
            vRegn2 = STRING(INT64(IF NUM-ENTRIES(vRegn,"/") >= 2
                                   THEN ENTRY(2,vRegn,"/")
                                   ELSE "0"),"9999").


         IF NOT UpdateSigns(acct.class-code,
                           IF {assigned vNewAcct} THEN vNewAcct ELSE
                            vAcct-surr,
                            "СНИРегн",
                            vRegn1 + vRegn2,
                            ?) THEN LEAVE.
      END.

      vNumAcct    = vNumAcct + STRING(INT64(vMessTmp),"9999").
      vLastTaxImp = LastTaxImp(acct.acct, acct.currency).
      vAddInd = 0.

      IF mIsCorrect         OR
         vLastTaxImp =  "K" OR
         vLastTaxImp =  "N" OR
         vLastTaxImp =  "T" THEN vAddInd = 1.
&IF DEFINED(IS-DEBUG) &THEN
RUN dbgprint.p ("еНомСооб","codFil:" + GetNULLSTR(codFil)         +
                " vNumAcct:"         +  GetNULLSTR(vNumAcct)      +
                " class-code:"       +  acct.class-code           +
                " vMessTmp:"         +  GetNULLSTR(vMessTmp)      +
                " СНИРегн:"          +  GetXAttrValue("acct",vAcct-Surr,"СНИРегн") +
                " vAddInd:"          + STRING(vAddInd)
                ).
&ENDIF

      IF vFormatN =  "5.01" THEN DO:
         ASSIGN 
            vBegDate = DATE(FGetSetting("ГНИ",
                                        "НачДата",
                                        STRING(acct.open-date)))
            vFirstDate = DATE(GetXAttrValue("acct",vAcct-Surr,"СНИНачДата"))
                                        NO-ERROR.
   
   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("еНомСооб"," acct.open-date:" + GetNullDat(acct.open-date)  +
                              " vBegDate:"       + GetNullDat(vBegDate) +
                              " FirstDate:"      + GetNullDat(vFirstDate )).
   &ENDIF
   
         IF vFirstDate <> ? 
         THEN
            ASSIGN
               vNumYear = SUBSTRING(STRING(YEAR(vFirstDate),"9999"),3,2).
         ELSE DO:
            ASSIGN
               vNumYear = SUBSTRING(STRING(YEAR(TODAY),"9999"),3,2).             
            UpdateSigns("acct",IF {assigned vNewAcct} THEN vNewAcct ELSE vAcct-Surr,"СНИНачДата",STRING(TODAY),YES).
         END.
      END.
      ELSE DO:                         /* 5.02 */
         IF vLastTaxImp =  "K"  OR
            vLastTaxImp =  "N"  OR
            vLastTaxImp =  "T"  THEN DO:
            vFirstDate = DATE(GetXAttrValue("acct",vAcct-Surr,"СНИНачДата")).
            vNumYear = SUBSTRING(STRING(YEAR(vFirstDate),"9999"),3,2).
         END.
         ELSE DO:
            vNumYear = SUBSTRING(STRING(YEAR(TODAY),"9999"),3,2).
            UpdateSigns("acct",IF {assigned vNewAcct} THEN vNewAcct ELSE vAcct-Surr,"СНИНачДата",STRING(TODAY),YES).
         END.
      END.

      vNumAcct = vNumAcct + vNumYear.

&IF DEFINED(IS-DEBUG) &THEN
RUN dbgprint.p ("еНомСооб", " vNumYear:"   + GetNullStr(vNumYear) +
                            " vNumAcct:"   + GetNullStr(vNumAcct)).
&ENDIF

      vAcct-surr  = acct.acct + "," + acct.currency.
      vNumMess    = GetXAttrValueEx("acct",vAcct-Surr,"СНИНомПП","").

      ASSIGN vCurrCnt = STRING(GetCounterCurrentValue(
                                  FGetSetting("ГНИ","КодНом1114301","ГНИ4"),
                                  TODAY)).

      IF vFormatN =  "5.01" THEN DO:
         IF vLastTaxImp =  ""  OR
            vLastTaxImp =  "F" THEN
            vNumMess   = STRING(INT64(vNumMess) + 1).
   
         vMessTmp   = GetXAttrValueEx("acct",vAcct-Surr,"СНИНом","0").
         IF INT64(vMessTmp) =  0  THEN
         DO:
            vMessTmp = STRING(GetCounterNextValue(FGetSetting("ГНИ","КодНом1114301","ГНИ4"), TODAY)).
            IF {assigned vMessTmp} THEN
            DO:
               RUN AddAttr2TableEx IN h_trans ("", 0, -1, "", 0, "PrvCntVal",
                                               vCurrCnt).
               ASSIGN vCurrCnt = "".
            END.
         END.
      END.
      ELSE DO:                         /* 5.02 */

         IF  NOT mIsCorrect       AND
             ((vLastTaxImp =  ""   OR
               vLastTaxImp =  "F"  OR
               vLastTaxImp =  "R"  OR
               vLastTaxImp =  "P"  OR
               vLastTaxImp =  "N"  OR
               vLastTaxImp =  "E") AND
               NOT mRefusal)
            THEN DO:
               vMessTmp = STRING(GetCounterNextValue(FGetSetting("ГНИ","КодНом1114301","ГНИ4"), TODAY)).
               IF NOT {assigned vMessTmp} THEN DO:
                  vError = 'В классификаторе "counters" счетчик "ГНИ4" не изменяется !'.
                  RUN Fill-SysMes("","","",vError).
                  vError = "Tax-028" + CHR(1)  + vError + " СНИНом = 0! ".
                  {additem.i vErrorList vError}
               END. 
               ELSE
               DO:
                  RUN AddAttr2TableEx IN h_trans ("", 0, -1, "", 0, "PrvCntVal",
                                                  vCurrCnt).
                  ASSIGN vCurrCnt = "".
               END.
            END.
         ELSE DO: 
            vMessTmp = GetXAttrValueEx("acct",vAcct-Surr,"СНИНом","").
            IF NOT {assigned vMessTmp} THEN DO:
               vError = 'СНИНом = 0!'.
               RUN Fill-SysMes("","","",vError).
               {additem.i vErrorList vError}
            END. 
         END.
      END.
&IF DEFINED(IS-DEBUG) &THEN
RUN dbgprint.p ("еНомСооб","counter:" + GetNULLSTR(FGetSetting("ГНИ","КодНом1114301","ГНИ4")) +
                " vMessTmp:" +  GetNULLSTR(vMessTmp)
                ).
&ENDIF

      ASSIGN
         vMessTmp =  STRING(INT64(vMessTmp),IF GetVersX(vExchMain) >= "5.10" 
                        THEN "99999999" ELSE "999999")
         vRegn    =  GetXAttrValue("acct",vAcct-Surr,"СНИРегн")
      .

      FOR EACH  signs WHERE
                signs.file       =  "acct"
           AND  signs.code       =  "СНИНом"
           AND  signs.surr       <> vAcct-surr
           AND  signs.code-val   =  vMessTmp NO-LOCK,    
          EACH bsigns WHERE
               bsigns.file       =  "acct"
           AND bsigns.code       =  "СНИРегн"
           AND bsigns.surr       =  signs.surr
           AND (bsigns.code-val  =  vRegn OR
                bsigns.xattr-val =  vRegn)
               NO-LOCK,
          EACH csigns WHERE
               csigns.file       =  "acct"
           AND csigns.code       =  "СНИНачДата"
           AND csigns.surr       =  signs.surr
               NO-LOCK,
          EACH dsigns WHERE
               dsigns.file       =  "acct"
           AND dsigns.code       =  "СНИНомПП"
           AND dsigns.surr       =  signs.surr
               NO-LOCK,
          EACH acct WHERE 
               acct.acct         =  ENTRY(1,signs.surr)
           AND acct.currency     =  ENTRY(2,signs.surr)
           AND acct.filial-id    =  shFilial
               NO-LOCK:

           IF  SUBSTRING(STRING(YEAR(DATE(csigns.code-val)),"9999"),3,2)
                                 =  vNumYear OR
               SUBSTRING(STRING(YEAR(DATE(csigns.xattr-val)),"9999"),3,2)
                                 =  vNumYear THEN DO:
              IF INT64(dsigns.code-val) MODULO 100 =  0 THEN
              DO:
                 IF mIsCorrect AND       /*0267159: исключаем ситуацию, когда */
                 INT64(vNumMess) MODULO 100 =  0 THEN . /*корректирующее сообщение*/
                                                    /*и при этом - первое*/
                 ELSE
                 DO:
                     vError = "Номер " + vMessTmp + vNumYear + vRegn +
                              " уже существует на счете " +
                              ENTRY(1,signs.surr) + " !".
                     RUN Fill-SysMes("","","",vError).
                     vError = "Tax-029" + CHR(1)  + vError.
                     {additem.i vErrorList vError}
                 END.
              END.
           END.
      END.
&IF DEFINED(IS-DEBUG) &THEN
RUN dbgprint.p ("еНомСооб","vErrorList:" + GetNULLSTR(vErrorList)).
&ENDIF
      ASSIGN
         iIns:BUFFER-FIELD("ErrorList"):BUFFER-VALUE  = vErrorList
         vNumAcct   = vNumAcct + STRING(INT64(vMessTmp),
                        IF GetVersX(vExchMain) >= "5.10" THEN "99999999" ELSE "999999")
      NO-ERROR.

      IF GetVersX(vExchMain) =  "5.02" OR GetVersX(vExchMain) >= "5.10" THEN DO:
         vCliType = GetCliType(vAcct-Surr,vExchMain).

         IF GetVersX(vExchMain) >= "5.10" THEN DO:
            RUN SetValue in h_exch (iIns, "CliType", vCliType, 0) NO-ERROR.
            RUN SetValue in h_exch (iIns, GetMangledName("КодЛица"), vCliType, 0) NO-ERROR.
         END.

         IF mIsCorrect THEN mResult = "2".
         ELSE IF NOT mRefusal THEN 
            RUN ParsMain IN h_parsr ("ПЯТЫЙЭЛЕМЕНТ('" + "5.02','"  +
                                      ENTRY(1,vAcct-surr) + "'," +
                                      (IF ENTRY(2,vAcct-surr) =  "" THEN "''"
                                       ELSE ENTRY(2,vAcct-surr))
                                      + ")", ?, mLibPars, OUTPUT mFlErr, OUTPUT mResult).
         ELSE mResult = "3".
         RUN SetValue in h_exch (iIns, "file-name", mResult, 0).

         IF mResult =  "1" THEN DO:
            vMMM = vCliType + "00".
            IF GetXAttrValueEx("acct",vAcct-Surr,"СНИНомПП","") =  "" THEN DO:
               RUN GetXAttr IN h_xclass ("acct", "СНИНомПП", BUFFER xattr).
               UpdateSigns("acct",
                           IF {assigned vNewAcct} THEN vNewAcct ELSE
                           acct.acct + "," + acct.currency,
                           "СНИНомПП",
                           STRING(INT64(vMMM),xattr.data-format),
                           xattr.Indexed).
            END.
            ELSE DO:
            IF IsTaxExp(acct.acct,acct.currency) THEN DO:
                  MESSAGE COLOR NORMAL
                     "Внимание! Счет <" + acct.acct + ">: Осуществляется попытка экспортировать " +
                      "первичное сообщение по счету, по которому ранее были сообщения." SKIP
                      "Продолжить ?" SKIP
                      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
                  SET vChoice.
                  IF GetVersX(vExchMain) >= "5.10" THEN 
                     RUN SetValue in h_exch (iIns,GetMangledName("Choice"),
                                             STRING(vChoice),0) NO-ERROR.
                  IF vChoice THEN DO:

                     IF vLastTaxImp =  "K" OR
                        vLastTaxImp =  "N" OR
                        vLastTaxImp =  "T"  THEN
                        vMMM = GetXattrVAlue("acct",
                                             acct.acct + "," + acct.currency,
                                             "СНИНомПП").
                     vMMM = STRING(INT64(vMMM) + vAddInd).
                     RUN GetXAttr IN h_xclass ("acct", "СНИНомПП", BUFFER xattr).
                     UpdateSigns("acct",
                                 IF {assigned vNewAcct} THEN vNewAcct ELSE
                                 acct.acct + "," + acct.currency,
                                 "СНИНомПП",
                                 STRING(INT64(vMMM),xattr.data-format),
                                 xattr.Indexed).
                  END.
                  ELSE DO:
                     Return  "SKIP".
                  END.
               END.
            END.
         END.
         ELSE IF mResult =  "2" THEN DO:
            vSniNomPP = GetXAttrValueEx("acct",vAcct-Surr,"СНИНомПП","").

            vMMM = vCliType + "01".

            IF vSniNomPP =  "" THEN DO:
               MESSAGE COLOR NORMAL
                  "Внимание! <" + acct.acct + ">: Производится попытка экспортировать " +
                   "повторное сообщение по счету, по которому не было " +
                   "первичного сообщения."   SKIP
                   "Продолжить ?" SKIP
                   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
               SET vChoice.
               IF GetVersX(vExchMain) >= "5.10" THEN 
                  RUN SetValue in h_exch (iIns,GetMangledName("Choice"),
                                          STRING(vChoice),0) NO-ERROR.
               IF vChoice THEN DO:
                  vMMM = STRING(INT64(vMMM) + vAddInd).
                  RUN GetXAttr IN h_xclass ("acct", "СНИНомПП", BUFFER xattr).
                  UpdateSigns("acct",
                              IF {assigned vNewAcct} THEN vNewAcct ELSE
                              acct.acct + "," + acct.currency,
                              "СНИНомПП",
                              STRING(INT64(vMMM),xattr.data-format),
                              xattr.Indexed).
               END.
            END.
            ELSE DO:

               IF (mIsCorrect          OR  
                   vLastTaxImp =  "K"  OR
                   vLastTaxImp =  "N"  OR
                   vLastTaxImp =  "T") AND
                  INT64(SUBSTRING(vSniNomPP,2)) + 1 > 76 THEN DO:
                  MESSAGE COLOR NORMAL
                          "Внимание! Корректирующих сообщений не может быть больше 76!" SKIP
                          VIEW-AS ALERT-BOX. 
                  vReturn    = "SKIP".
                  LEAVE.
               END.

               vMMM = STRING(INT64(vSniNomPP) + vAddInd).

               RUN GetXAttr IN h_xclass ("acct", "СНИНомПП", BUFFER xattr).
               UpdateSigns("acct",
                           IF {assigned vNewAcct} THEN vNewAcct ELSE 
                           acct.acct + "," + acct.currency,
                           "СНИНомПП",
                           STRING(INT64(vMMM),xattr.data-format),
                           xattr.Indexed).
            END.
         END.
         ELSE IF mResult =  "3" THEN DO:
               vMMM = GetCliType(vAcct-Surr,vExchMain) + "77".
               RUN GetXAttr IN h_xclass ("acct", "СНИНомПП", BUFFER xattr).
               UpdateSigns("acct",
                           IF {assigned vNewAcct} THEN vNewAcct ELSE
                           acct.acct + "," + acct.currency,
                           "СНИНомПП",
                           STRING(INT64(vMMM),xattr.data-format),
                           xattr.Indexed).

         END.
         vNumAcct   = vNumAcct + "," + STRING(INT64(vMMM),"999").
      END.
      ELSE DO:
         vNumAcct   = vNumAcct + "," + STRING(INT64(vNumMess),"999").
      END.
   END.
   ELSE DO:
      ASSIGN vCurrCnt = STRING(GetCounterCurrentValue(
                                  FGetSetting("ГНИ","КодНом1114301","ГНИ4"),
                                  TODAY))
             vMessTmp = STRING(GetCounterNextValue(
                                  FGetSetting("ГНИ","КодНом1114301","ГНИ4"), 
                                  TODAY))
      NO-ERROR.

      IF {assigned vMessTmp} THEN
      DO:
         RUN AddAttr2TableEx IN h_trans ("", 0, -1, "", 0, "PrvCntVal",
                                         vCurrCnt).
         ASSIGN vCurrCnt = "".
      END.

&IF DEFINED(IS-DEBUG) &THEN
RUN dbgprint.p ("еНомСооб", "vMessTmp:" + vMessTmp +
                            " vNumAcct:" + GetNULLSTR(vNumAcct)).
&ENDIF
                      /* Номер сообщения: RRRRFFFFGGNNNNNN,MMM
                      (где RRRRFFFF - регистрационный  номер банка и
                      порядковый номер филиала; в элементах GG,
                      NNNNNN и MMM  указываются нули) */
      vNumAcct   = vNumAcct + CodFil + SUBSTRING(STRING(YEAR(TODAY),"9999"),3,2)
                            + STRING(INT64(vMessTmp),IF GeTVersX(vExchMain) >= "5.10" THEN 
                                 "99999999" ELSE "999999") + "," +
                   (IF GetVersX(vExchMain) =  "5.02" OR GetVersX(vExchMain) >= "5.10" THEN "500"
                                           ELSE "000").
   END.
&IF DEFINED(IS-DEBUG) &THEN
RUN dbgprint.p ("еНомСооб", "vMessTmp:" + vMessTmp +
                            " vNumAcct:" + GetNULLSTR(vNumAcct)).
RUN dbgprint.p ("еНомСооб","NoSetНомСооб:" + CAPS(GetAttrValue2("",0,"NoSetНомСооб"))).
&ENDIF
   IF GetVersX(vExchMain) >= "5.10" THEN 
   DO:
      IF CAPS(GetAttrValue2("",0,"NoSetНомСооб")) <> "ДА" THEN DO:
         RUN SetValue in h_exch (iIns, iTag, ENTRY(1,vNumAcct), 0).
      END.
   END.
   ELSE
      RUN SetValue in h_exch (iIns, iTag, vNumAcct, 0).
   IF GetVersX(vExchMain) >= "5.10" THEN 
      RUN SetValue in h_exch (iIns, GetMangledName("ТипСооб"), vMMM, 0) NO-ERROR.
   RETURN vReturn.
END PROCEDURE.

PROCEDURE TaxStatExport:
   DEFINE INPUT PARAMETER iPacketID    AS INT64  NO-UNDO.

   DEF VAR vMessSer  AS CHAR    NO-UNDO.
   DEF VAR vMessNum  AS CHAR    NO-UNDO.
   DEF VAR vMessTmp  AS CHAR    NO-UNDO.
   DEF VAR vFlagImp  AS LOGICAL NO-UNDO.
   DEF VAR vFileItem AS CHAR    NO-UNDO.
   DEF VAR vTranForm AS CHAR    NO-UNDO.
   DEF VAR vKind     AS CHAR    NO-UNDO.
   DEF BUFFER xattr       FOR xattr.
   DEF BUFFER bPackObject FOR PackObject.

&IF DEFINED(IS-DEBUG) &THEN
RUN dbgprint.p ("TaxStatExport","PacketID:" + GetNullINT(iPacketID)).
&ENDIF

MAIN:
DO TRANSACTION ON ERROR UNDO MAIN, RETRY MAIN:
   {do-retry.i MAIN}

   ASSIGN
   vTranForm = GetAttrValue2(GetBaseOpKind(),GetBaseTemplate(),"$Kind")
   .
   FOR EACH  PackObject      WHERE 
             PackObject.PacketID    =  iPacketID 
        AND  PackObject.File-Name   =  "acct" 
             NO-LOCK,
        EACH acct            WHERE 
             acct.acct              =  ENTRY(1,PackObject.Surrogate) 
         AND acct.currency          =  ENTRY(2,PackObject.Surrogate) 
         NO-LOCK:

&IF DEFINED(IS-DEBUG) &THEN
RUN dbgprint.p ("TaxStatExport 1 ","acct:"      + GetNullStr(acct.acct) +
                                   " StateOBJ:" + GetNullStr(vMessNum)).
&ENDIF
      ASSIGN
         vFlagImp  = NO
         vFileItem = ""
         vKind = IF GetVersX(vTranForm) >= "5.10" THEN "TaxExpCommon5X" ELSE "TaxImp"
      .

      FOR LAST bPackObject WHERE
               bPackObject.file-name =  'acct'
           AND bPackObject.Surrogate =  ENTRY(1,PackObject.Surrogate) + "," + ENTRY(2,PackObject.Surrogate)  NO-LOCK,
          EACH Packet     WHERE
               Packet.PacketID      =  bPackObject.PacketID
           AND Packet.Kind          =  vKind
               NO-LOCK:
                   FIND FIRST FileExch   WHERE
                              FileExch.FileExchID  =  packet.FileExchID        NO-LOCK NO-ERROR.
                        IF AVAIL FileExch THEN 
                           vFileItem = SUBSTRING(FileExch.name,3,1).

            vFlagImp = vFileItem  =  "K" OR
                       vFileItem  =  "N" OR
                       vFileItem  =  "T". 
            LEAVE.
      END.

      ASSIGN
         vMessTmp = GetXAttrValueEx("Packet",        
                                    STRING(PackObject.PacketID),
                                    "StateOBJ",
                                    "")

         vMessSer = IF GetVersX(vTranForm) >= "5.10"
                      THEN SUBSTRING(vMessTmp,20,3)
                      ELSE SUBSTRING(vMessTmp,18,3)
         vMessNum = IF GetVersX(vTranForm) >= "5.10"
                      THEN SUBSTRING(vMessTmp,11,8)
                      ELSE SUBSTRING(vMessTmp,11,6)

      NO-ERROR.

&IF DEFINED(IS-DEBUG) &THEN
RUN dbgprint.p ("TaxStatExport 2 ","acct:"      + GetNullStr(acct.acct) +
                                   " StateOBJ:" + GetNullStr(vMessNum)).
&ENDIF

         IF NOT  UpdateSigns(acct.class-code,
                     IF {assigned vNewAcct} THEN vNewAcct ELSE
                     acct.acct + "," + acct.currency,
                     "СНИНомПП",
                     STRING(INT64(vMessSer),"999"),
                     ?) THEN
            UNDO MAIN, RETRY MAIN.

&IF DEFINED(IS-DEBUG) &THEN
RUN dbgprint.p ("TaxStatExport 3 ","acct:"      + GetNullStr(acct.acct) +
                                   " StateOBJ:" + GetNullStr(vMessNum)  +
                                   " СНИНомПП:" + GetNullINT(INT64(vMessSer)) +
                                   " vFlagImp:" + STRING(vFlagImp)).
&ENDIF

      RUN GetXAttr IN h_xclass ("acct", "СНИНом", BUFFER xattr).

      IF NOT vFlagImp THEN DO:
         IF NOT  UpdateSigns("acct",
                             IF {assigned vNewAcct} THEN vNewAcct ELSE
                             acct.acct + "," + acct.currency,
                            "СНИНом",
                             STRING(INT64(vMessNum),xattr.data-format),
                             xattr.Indexed) THEN
           UNDO MAIN, RETRY MAIN.
      END.

&IF DEFINED(IS-DEBUG) &THEN
RUN dbgprint.p ("TaxStatExport","acct:"      + GetNullStr(acct.acct) +
                                " StateOBJ:" + GetNullStr(vMessNum)  +
                                " СНИНомПП:" + GetNullInt(INT64(vMessSer) + 1) +
                                " СНИНом:"   + GetNullInt(INT64(vMessNum))).
&ENDIF
      vNewAcct = "".
   END.
END.
END PROCEDURE.

/*----------------------------------------------------------------------------*/
{pfuncdef 
   &DefProc     = "fill-acct-tt-dp"
   &PARAMETERS  = "iAcct, iCurrency"
   &Description = "Установить для счетов" }

PROCEDURE fill-acct-tt-dp:
   DEF INPUT PARAM iAcct     AS CHAR NO-UNDO.
   DEF INPUT PARAM iCurrency AS CHAR NO-UNDO.

   DEF VAR vFlagSet AS LOGICAL INIT ? NO-UNDO.

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("pp-acctn.p fill-acct-tt-dp",
                   "Start iacct:" + GetNullStr(iacct) +
                   " icurr:" + GetNullStr(icurrency)
                   ).
   &ENDIF

   MAIN:
   DO TRANSACTION ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}
      FIND FIRST tt-del-pack WHERE
                 tt-del-pack.Acct     =  iAcct AND
                 tt-del-pack.Currency =  iCurrency NO-ERROR.

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("pp-acctn.p fill-acct-tt-dp",
                      "AVAIL tt-del-pack:" + STRING(AVAIL tt-del-pack)).
      &ENDIF
      IF NOT AVAIL tt-del-pack THEN
      CREATE tt-del-pack.
      ASSIGN
         tt-del-pack.Acct     = iAcct
         tt-del-pack.Currency = iCurrency
         tt-del-pack.DateBeg  = GetXattrValueEx("acct",
                                                iAcct + "," + iCurrency,
                                                "СНИНачДата", "")
         tt-del-pack.Num      = GetXattrValueEx("acct",
                                                iAcct + "," + iCurrency,
                                                "СНИНомПП", "")
         tt-del-pack.Regn     = GetXattrValueEx("acct",
                                                iAcct + "," + iCurrency,
                                                "СНИРегн", "")
      NO-ERROR. {&ON-ERROR}

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("pp-acctn.p fill-acct-tt-dp",
                      "tt-del-pack.Date:" + GetNullStr(tt-del-pack.Date) +
                      "tt-del-pack.Num:" + GetNullStr(tt-del-pack.Num) +
                      "tt-del-pack.Regn:" + GetNullStr(tt-del-pack.Regn)
                      ).
      &ENDIF
      vFlagSet = YES.
   END.
   {doreturn.i vFlagSet}
END PROCEDURE.

/*----------------------------------------------------------------------------*/
{pfuncdef 
   &DefProc     = "fill-pack-tt-dp"
   &PARAMETERS  = "iAcct, iCurrency, iPacketId"
   &Description = "Установить код сообщения" }
PROCEDURE fill-pack-tt-dp:
   DEF INPUT PARAM iAcct     AS CHAR  NO-UNDO.
   DEF INPUT PARAM iCurrency AS CHAR  NO-UNDO.
   DEF INPUT PARAM iPacketId AS INT64 NO-UNDO.

   DEF VAR vFlagSet AS LOGICAL INIT ? NO-UNDO.

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("pp-acctn.p fill-pack-tt-dp",
                   "Start iacct:" + GetNullStr(iacct) +
                   " icurr:" + GetNullStr(icurrency) +
                   " iPacketId:" + STRING(iPacketId)
                   ).
   &ENDIF

   MAIN:
   DO TRANSACTION ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}
      FIND FIRST tt-del-pack WHERE
                 tt-del-pack.Acct     =  iAcct AND
                 tt-del-pack.Currency =  iCurrency NO-ERROR.
      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("pp-acctn.p fill-pack-tt-dp",
                      "AVAIL tt-del-pack:" + STRING(AVAIL tt-del-pack)).
      &ENDIF
      IF AVAIL tt-del-pack THEN
      ASSIGN
         tt-del-pack.PacketId = iPacketId
      NO-ERROR. {&ON-ERROR}

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("pp-acctn.p fill-pack-tt-dp",
                      "tt-del-pack.PacketId:" + STRING(tt-del-pack.PacketId)).
      &ENDIF
      vFlagSet = YES.
   END.
   {doreturn.i vFlagSet}
END PROCEDURE.
/*----------------------------------------------------------------------------*/
{pfuncdef 
   &DefProc     = "del-pack-tt-dp"
   &PARAMETERS  = "-"
   &Description = "Удаление дрсчета по откаченым сообщениям" }
PROCEDURE del-pack-tt-dp:

   DEF VAR vFlagSet AS LOGICAL INIT ? NO-UNDO.
   DEF BUFFER Packet FOR Packet.
   DEF BUFFER acct FOR acct.

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("pp-acctn.p del-pack-tt-dp", "Start ").
   &ENDIF

   MAIN:
   DO TRANSACTION ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      FOR EACH tt-del-pack: 
         &IF DEFINED(IS-DEBUG) &THEN
         RUN dbgprint.p ("pp-acctn.p del-pack-tt-dp",
                         "AVAIL tt-del-pack:" + STRING(tt-del-pack.PacketId)).
         &ENDIF

         FIND FIRST Packet WHERE 
                    Packet.PacketId =  tt-del-pack.PacketId NO-LOCK NO-ERROR.
         &IF DEFINED(IS-DEBUG) &THEN
         RUN dbgprint.p ("pp-acctn.p del-pack-tt-dp",
                         "AVAIL pack:" + STRING(AVAIL packet)).
         RUN dbgprint.p ("pp-acctn.p del-pack-tt-dp",
                         " id:" + STRING(tt-del-pack.PacketId)).
         &ENDIF
         IF NOT AVAIL Packet THEN DO:

            &IF DEFINED(IS-DEBUG) &THEN
            RUN dbgprint.p ("pp-acctn.p del-pack-tt-dp",
                            "до СНИНачДата:" + GetXattrValueEx("acct",
                                tt-del-pack.acct + "," + tt-del-pack.currency,
                                "СНИНачДата",
                                "")
                            ).
            RUN dbgprint.p ("pp-acctn.p del-pack-tt-dp",
                            "до СНИНомПП:" + GetXattrValueEx("acct",
                                tt-del-pack.acct + "," + tt-del-pack.currency,
                                "СНИНомПП",
                                "")
                            ).
            RUN dbgprint.p ("pp-acctn.p del-pack-tt-dp",
                            "до СНИРегн:" + GetXattrValueEx("acct",
                                tt-del-pack.acct + "," + tt-del-pack.currency,
                                "СНИРегн",
                                "")
                            ).
            &ENDIF


            {find-act.i 
                &acct = tt-del-pack.acct
                &curr = tt-del-pack.currency
            }
            IF AVAIL acct THEN DO:
               UpdateSigns(acct.class-code,
                           tt-del-pack.acct + "," + tt-del-pack.currency,
                           "СНИНачДата",
                           tt-del-pack.DateBeg,
                           ?).
               UpdateSigns(acct.class-code,
                           tt-del-pack.acct + "," + tt-del-pack.currency,
                           "СНИНомПП",
                           tt-del-pack.Num,
                           ?).
               UpdateSigns(acct.class-code,
                           tt-del-pack.acct + "," + tt-del-pack.currency,
                           "СНИРегн",
                           tt-del-pack.Regn,
                           ?) no-error.
            END.
            &IF DEFINED(IS-DEBUG) &THEN
            RUN dbgprint.p ("pp-acctn.p del-pack-tt-dp",
                            "error-status:error:" + string(error-status:error) +
                            "error-status:get-message(1):" + error-status:get-message(1)
                            ).
            RUN dbgprint.p ("pp-acctn.p del-pack-tt-dp",
                            "СНИНачДата:" + GetXattrValueEx("acct",
                                tt-del-pack.acct + "," + tt-del-pack.currency,
                                "СНИНачДата",
                                "")
                            ).
            RUN dbgprint.p ("pp-acctn.p del-pack-tt-dp",
                            "СНИНомПП:" + GetXattrValueEx("acct",
                                tt-del-pack.acct + "," + tt-del-pack.currency,
                                "СНИНомПП",
                                "")
                            ).
            RUN dbgprint.p ("pp-acctn.p del-pack-tt-dp",
                            "СНИРегн:" + GetXattrValueEx("acct",
                                tt-del-pack.acct + "," + tt-del-pack.currency,
                                "СНИРегн",
                                "")
                            ).
            &ENDIF

         END.
      END.
      {empty tt-del-pack}
      vFlagSet = YES.
   END.
   {doreturn.i vFlagSet}
END PROCEDURE.
/* $LINTFILE='pp-acctn.p' */
/* $LINTMODE='11,-9,6,3' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='soav' */
/* $LINTDATE='07/03/2017 11:27:43.902+03:00' */
/*prosign6K5lbX/IRqrBfgaVtN/5IQ*/