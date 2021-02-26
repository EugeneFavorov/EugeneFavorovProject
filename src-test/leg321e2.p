/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2017 АО "Банковские информационные системы"
     Filename: leg321e2.p
      Comment: Фoрма редактирования данных класса legal321
               ПЛАТЕЛЬЩИК
   Parameters: нет
         Uses:
      Used by:
      Created: 03/12/08 ruva
     Modified: 26.12.08 ler 0103840 - клавиша F1 для AMR и ADRESS (КОД СУБЪЕКТА) - классификатор "КодРег" вместо "КодРегГНИ".
     Modified: 15.06.09 ler 0099461 - 321-П. механизм импорта и хранения ИЭС.
*/
/******************************************************************************/
&GLOBAL-DEFINE LEG321P YES

DEFINE INPUT PARAMETER iRecID    AS RECID   NO-UNDO.
DEFINE INPUT PARAMETER iNumFrame AS INT64 NO-UNDO.

DEFINE VAR h_Debug       AS HANDLE  NO-UNDO. /* Чтобы не появлялся фрейм дебаггера */
DEFINE VARIABLE ipDataID AS INT64 NO-UNDO. /* для leg321f.i */
{globals.i}
{intrface.get xclass}
{intrface.get instrum}
{intrface.get strng}    /* Библиотека для работы со строками. */
{leg321p.def}
{leg321p.fun}

{path-cmp.get DataLine iRecID YES}

&GLOBAL-DEFINE SEND-LINE-DISP "ЛицоСоверОпер"
&GLOBAL-DEFINE SPRX-LINE-DISP "Пред. ЛицСоверОпер"

&SCOPED-DEFINE FRAME1 FRAME edit1
&SCOPED-DEFINE FRAME2 FRAME edit2
&SCOPED-DEFINE FRAME3 FRAME edit3

DEFINE VARIABLE vType     AS CHARACTER NO-UNDO.
DEFINE VARIABLE vTypeDisp AS CHARACTER NO-UNDO
   VIEW-AS COMBO-BOX LIST-ITEMS {&SEND-LINE-DISP},{&SPRX-LINE-DISP},{&RPRX-LINE},{&RECV-LINE},{&ORDR-LINE}.

DEFINE VARIABLE vTU       AS CHARACTER NO-UNDO.

DEFINE VARIABLE vTUcb     AS CHARACTER NO-UNDO
   VIEW-AS COMBO-BOX LIST-ITEMS
      "0 - Лицо отсутствует",
      "1 - Юридическое лицо",
      "2 - Физическое лицо",
      "3 - Физическое лицо (ИП/ПБОЮЛ)",
      "4 - Прочее".
DEFINE VARIABLE vPRU         AS CHARACTER NO-UNDO INIT "0".
DEFINE VARIABLE vNameU       AS CHARACTER NO-UNDO INIT "0"
                             VIEW-AS EDITOR INNER-CHARS 50 INNER-LINES 3 FORMAT "x(254)".
DEFINE VARIABLE vKodCR       AS CHARACTER NO-UNDO INIT "0".
DEFINE VARIABLE vKodCN       AS CHARACTER NO-UNDO INIT "0".
DEFINE VARIABLE vAMRS        AS CHARACTER NO-UNDO FORMAT "x(2)" INIT "00".
DEFINE VARIABLE vAMRR        AS CHARACTER NO-UNDO FORMAT "x(40)"
                             VIEW-AS FILL-IN SIZE 40 BY 1.
DEFINE VARIABLE vAMRG        AS CHARACTER NO-UNDO FORMAT "x(40)"
                             VIEW-AS FILL-IN SIZE 40 BY 1.
DEFINE VARIABLE vAMRU        AS CHARACTER NO-UNDO FORMAT "x(40)"
                             VIEW-AS FILL-IN SIZE 40 BY 1.
DEFINE VARIABLE vAMRD        AS CHARACTER NO-UNDO FORMAT "x(7)".
DEFINE VARIABLE vAMRK        AS CHARACTER NO-UNDO FORMAT "x(7)".
DEFINE VARIABLE vAMRO        AS CHARACTER NO-UNDO FORMAT "x(7)".
DEFINE VARIABLE vADRESSS     AS CHARACTER NO-UNDO FORMAT "x(2)" INIT "00".
DEFINE VARIABLE vADRESSR     AS CHARACTER NO-UNDO FORMAT "x(40)"
                             VIEW-AS FILL-IN SIZE 40 BY 1.
DEFINE VARIABLE vADRESSG     AS CHARACTER NO-UNDO FORMAT "x(40)"
                             VIEW-AS FILL-IN SIZE 40 BY 1.
DEFINE VARIABLE vADRESSU     AS CHARACTER NO-UNDO FORMAT "x(40)"
                             VIEW-AS FILL-IN SIZE 40 BY 1.
DEFINE VARIABLE vADRESSD     AS CHARACTER NO-UNDO FORMAT "x(7)".
DEFINE VARIABLE vADRESSK     AS CHARACTER NO-UNDO FORMAT "x(7)".
DEFINE VARIABLE vADRESSO     AS CHARACTER NO-UNDO FORMAT "x(7)".
DEFINE VARIABLE vKD          AS CHARACTER NO-UNDO INIT "0".
DEFINE VARIABLE vSD          AS CHARACTER NO-UNDO INIT "0".
DEFINE VARIABLE vRG          AS CHARACTER NO-UNDO INIT "0".
DEFINE VARIABLE vND          AS CHARACTER NO-UNDO INIT "0".
DEFINE VARIABLE vVD1         AS CHARACTER NO-UNDO FORMAT "x(13)" INIT "0".
DEFINE VARIABLE vVD2         AS CHARACTER NO-UNDO FORMAT "x(254)"
                             VIEW-AS FILL-IN SIZE 55 BY 1.
DEFINE VARIABLE vVD3         AS DATE      NO-UNDO INIT 01/01/2099.
DEFINE VARIABLE vVD4         AS CHARACTER NO-UNDO FORMAT "x(2)" INIT "0".
DEFINE VARIABLE vVD5         AS CHARACTER NO-UNDO FORMAT "x(30)" INIT "0".
DEFINE VARIABLE vVD6         AS DATE      NO-UNDO INIT 01/01/2099.
DEFINE VARIABLE vVD7         AS DATE      NO-UNDO INIT 01/01/2099.
DEFINE VARIABLE vMC1         AS CHARACTER NO-UNDO FORMAT "x(15)" INIT "0".
DEFINE VARIABLE vMC2         AS DATE      NO-UNDO INIT 01/01/2099.
DEFINE VARIABLE vMC3         AS DATE      NO-UNDO INIT 01/01/2099.
DEFINE VARIABLE vGR          AS DATE      NO-UNDO.
DEFINE VARIABLE vBP          AS CHARACTER NO-UNDO FORMAT "x(254)" VIEW-AS FILL-IN SIZE 60 BY 1.
DEFINE VARIABLE vVP          AS CHARACTER NO-UNDO FORMAT "9".
DEFINE VARIABLE vVPcb        AS CHARACTER NO-UNDO
                             VIEW-AS COMBO-BOX LIST-ITEMS
                             "0 - иные случаи",
                             "1 - идентификация выгодоприобретателя завершена",
                             "2 - идентификация выгодоприобретателя не завершена".
DEFINE VARIABLE vReserv2     AS CHARACTER NO-UNDO.
DEFINE VARIABLE vAcctB       AS CHARACTER NO-UNDO INIT "0".
DEFINE VARIABLE vAcctCorB    AS CHARACTER NO-UNDO INIT "0".
DEFINE VARIABLE vNameIsB     AS CHARACTER NO-UNDO INIT "0" FORMAT "x(254)" VIEW-AS FILL-IN SIZE 50 BY 2.
DEFINE VARIABLE vBIKIsB      AS CHARACTER NO-UNDO INIT "0" FORMAT "x(12)".
DEFINE VARIABLE vCardB       AS CHARACTER NO-UNDO FORMAT "9".
DEFINE VARIABLE vCardBcb     AS CHARACTER NO-UNDO FORMAT "x(63)"
                             VIEW-AS COMBO-BOX LIST-ITEMS
                             "0 - без использования банковской карты",
                             "1 - владелец банковской карты - клиент кредитной организации",
                             "2 - владелец банковской карты - не клиент кредитной организации",
                             "3 - иное".
DEFINE VARIABLE vNameB       AS CHARACTER NO-UNDO INIT "0"
                             VIEW-AS EDITOR INNER-CHARS 30 INNER-LINES 3 FORMAT "x(254)".
DEFINE VARIABLE vKodCNB      AS CHARACTER NO-UNDO INIT "0".
DEFINE VARIABLE vBIKB        AS CHARACTER NO-UNDO INIT "0".
DEFINE VARIABLE vNameR       AS CHARACTER NO-UNDO INIT "0"
                             VIEW-AS EDITOR INNER-CHARS 28 INNER-LINES 3 FORMAT "x(254)".
DEFINE VARIABLE vKodCNR      AS CHARACTER NO-UNDO INIT "0".
DEFINE VARIABLE vBIKR        AS CHARACTER NO-UNDO INIT "0".
DEFINE VARIABLE vTemp        AS CHARACTER NO-UNDO.
DEFINE VARIABLE vNewDL       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE vI           AS INT64   NO-UNDO.
DEFINE VARIABLE vContr       AS LOGICAL   NO-UNDO VIEW-AS TOGGLE-BOX.
DEFINE VARIABLE vCustCat     AS CHARACTER NO-UNDO VIEW-AS COMBO-BOX LIST-ITEMS "Ю","Ч","Б".
DEFINE VARIABLE vCustID      AS INT64   NO-UNDO.

DEFINE VARIABLE vAMR         AS CHARACTER NO-UNDO.
DEFINE VARIABLE vADRESS      AS CHARACTER NO-UNDO.
DEFINE VARIABLE vVD-Label    AS CHARACTER NO-UNDO FORMAT "x(78)" INIT
                             "─────────────────ДОКУМЕНТ, ПОДТВЕРЖДАЮЩИЙ ПРАВО НА ПРЕБЫВАНИЕ─────────────────".
DEFINE VARIABLE vMC-Label    AS CHARACTER NO-UNDO FORMAT "x(78)" INIT
                             "──────────────────────────────МИГРАЦИОННАЯ КАРТА──────────────────────────────".
DEFINE VARIABLE vLine        AS CHARACTER NO-UNDO FORMAT "x(78)" INIT
                             "──────────────────────────────────────────────────────────────────────────────".

DEFINE VARIABLE mTitle AS CHARACTER NO-UNDO EXTENT 3 INIT
   ["ДАННЫЕ УЧАСТНИКОВ - ОСНОВНЫЕ", "ДАННЫЕ УЧАСТНИКОВ - ДОКУМЕНТЫ", "ДАННЫЕ УЧАСТНИКОВ - СЧЕТА"].


DEFINE VARIABLE mOk AS LOGICAL NO-UNDO.

DEFINE BUFFER bDataLine FOR DataLine.

FORM
   vTypeDisp FORMAT "x(23)"
             LABEL  "Участник"
             HELP   "Положение участника операции"

   vContr    LABEL  " Контрагент"
             HELP   "Является ли участник контрагентом"
   SKIP
   vCustCat  LABEL  "Кат"
             FORMAT "x(1)"
             HELP   "Юридическое лицо, Частное лицо, Банк"
   vCustID   FORMAT ">>>>>>>9"
             LABEL  "Номер"
   vTUcb     LABEL  "Тип"
             FORMAT "x(33)"
   SKIP
   SPACE(23)
   vPRU      FORMAT "x(6)"
             LABEL  "Признак"
             HELP   "Признак участника операции (PRU)"
   SKIP
   "Наименование:"
   vNameU    HELP    "Наименование участника операции (NAMEU)"
             NO-LABEL
   SKIP
   "──────────────────────────────МЕСТО РЕГИСТРАЦИИ───────────────────────────────"
   SKIP
   vKodCR    FORMAT "x(5)"
             LABEL  "Код страны"
             HELP   "Код места регистрации участника операции (KODCR)"
   vAMRS     LABEL "Код субъекта"
             HELP  "Код субъекта РФ (AMR_S)"
   SKIP
   vAMRR     LABEL "Район (регион) "
             HELP  "Район (регион) (AMR_R)"
   SKIP
   vAMRG     LABEL "Населенный пункт"
             HELP  "Населенный пункт (AMR_G)"
   SKIP
   vAMRU     LABEL "Улица"
             HELP  "Наименование улицы (AMR_U)"
   SKIP
   vAMRD     LABEL "Дом"
             HELP  "Номер дома (номер владения) (AMR_D)"
   vAMRK     LABEL "Корп."
             HELP  "Номер корпуса (номер строения) (AMR_K)"
   vAMRO     LABEL "Кв."
             HELP  "Номер офиса (номер квартиры) (AMR_O)"

   SKIP
   "───────────────────────────────МЕСТО НАХОЖДЕНИЯ───────────────────────────────"
   SKIP
   vKodCN    FORMAT "x(5)"
             LABEL  "Код страны"
             HELP   "Код места нахождения участника операции (KODCN)"

   vADRESSS  LABEL "Код субъекта"
             HELP  "Код субъекта РФ (ADRESS_S)"

   SKIP
   vADRESSR  LABEL "Район (регион) "
             HELP  "Район (регион) (ADRESS_R)"

   SKIP
   vADRESSG  LABEL "Населенный пункт"
             HELP  "Населенный пункт (ADRESS_G)"
   SKIP
   vADRESSU  LABEL "Улица"
             HELP  "Наименование улицы (ADRESS_U)"
   SKIP
   vADRESSD  LABEL "Дом"
             HELP  "Номер дома (номер владения) (ADRESS_D)"
   vADRESSK  LABEL "Корп."
             HELP  "Номер корпуса (номер строения) (ADRESS_K)"
   vADRESSO  LABEL "Кв."
             HELP  "Номер офиса (номер квартиры) (ADRESS_O)"
WITH {&FRAME1} OVERLAY CENTERED
     TITLE COLOR bright-white mTitle[1] ROW 3 SIDE-LABELS.

FORM
   vTypeDisp FORMAT "x(23)"
             LABEL  "Участник"
             HELP   "Положение участника операции"
   SKIP
   "Наименование:"
   vNameU    HELP    "Наименование участника операции (NAMEU)"
             NO-LABEL
   SKIP
   vKD      FORMAT "x(2)"
            LABEL  "Документ"
            HELP   "Код документа (KD)"
   vSD      FORMAT "x(10)"
            LABEL  "Серия"
            HELP   "Серия документа (SD)"
   vVD1     FORMAT "x(13)"
            LABEL  "НомерДокУдостЛичн"
            HELP   "Номер документа, удостоверяющего личность (VD1)"
   vND      FORMAT "x(12)"
            LABEL  "ИНН"
            HELP   "Идентификацонный номер налогоплательщика (ND)"
   vRG      FORMAT "x(20)"
            LABEL  "ОГРН"
            HELP   "ОГРН (RG)"
   SKIP
   vVD2     LABEL  "Орган, выдав.документ"
            HELP   "Наименование органа, выдавшего документ (VD2)"
   vVD3     FORMAT "99/99/9999"
            LABEL  "Дата выдачи документа"
            HELP   "Дата выдачи документа, удостоверяющего личность (VD3)"
   vVD-Label NO-LABEL
   vVD4     FORMAT "x(2)"
            LABEL  "Код вида документа"
            HELP   "Код вида документа, подтв. право на пребывание в РФ (VD4)"
   vVD5     FORMAT "x(30)"
            LABEL  "Серия и номер документа"
            HELP   "Серия и номер документа, подтв. право на пребывание в РФ (VD5)"
   SKIP
   vVD6     FORMAT "99/99/9999"
            LABEL  "Дата начала"
            HELP   "Дата начала срока действия права на пребывание в РФ (VD6)"
   vVD7     FORMAT "99/99/9999"
            LABEL  "Дата окончания"
            HELP   "Дата окончания срока действия права на пребывание в РФ (VD7)"
   vMC-Label NO-LABEL
   vMC1     FORMAT "x(15)"
            LABEL  "Номер мигр.карты"
            HELP   "Номер миграционной карты (MC1)"
   SKIP
   vMC2     FORMAT "99/99/9999"
            LABEL  "Дата начала"
            HELP   "Дата начала срока пребывания в РФ (MC2)"
   vMC3     FORMAT "99/99/9999"
            LABEL  "Дата окончания"
            HELP   "Дата окончания срока пребывания в РФ (MC3)"
   vLine    NO-LABEL
   SKIP
   vGR      FORMAT "99/99/9999"
            LABEL  "Дата регистрации/рождения"
            HELP   "Дата регистрации/рождения (GR)"
   vBP      LABEL  "Место рождения"
            HELP   "Место рождения (BP)"
   vVPcb    FORMAT "x(50)"
            LABEL  "Признак идентификации"
            HELP   "Признак идентификации выгодоприобретателя (VP)"
WITH {&FRAME2} OVERLAY CENTERED
     TITLE COLOR bright-white mTitle[2] ROW 3 SIDE-LABELS.


FORM
   vTypeDisp FORMAT "x(23)"
             LABEL  "Участник"
             HELP   "Положение участника операции"
   SKIP
   "Наименование:"
   vNameU    HELP    "Наименование участника операции (NAMEU)"
             NO-LABEL
   SKIP

   vAcctB    FORMAT "x(34)"
             LABEL  "Счет"
             HELP   "Номер счета участника в обслуживающем его банке (ACC_B)"
   vAcctCorB FORMAT "x(34)"
             LABEL  "Корр.счет"
             HELP   "Номер корреспондирующего счета (ACC_COR_B)"
   SKIP
   vBIKIsB   LABEL  "БИК КРЕД.ОРГ"
             HELP   "БИК кредитной организации - эмитента банковской карты (BIK_IS_B)"
   SKIP
   vNameIsB  LABEL  "Наим.кред.организации"
             HELP   "Наименование кредитной организации - эмитента банковской карты (NAME_IS_B)"
   SKIP
   vCardBcb  LABEL  "КЛ"
             HELP   "Принадлежность владельца банк.карты к кредитной организации (CARD_B)"
   "─────────────────УЧАСТНИК───────────────┬───────────КОРРЕСПОНДЕНТ─────────────"
   vKodCNB   FORMAT "x(5)"
             LABEL  "Место"
             HELP   "Код места нахождения банка, обслуживающего участника операции (KODCN_B)"
   "│" AT 41
   vKODCNR   FORMAT "x(5)"
             LABEL  "Место"
             HELP   "Код места нахождения банка - корреспондента (KODCN_R)"
   SKIP
   vBIKB     FORMAT "x(12)"
             LABEL  "БИК"
             HELP   "БИК банка, обслуживающего участника операции (BIK_B)"
   "│" AT 41
   vBIKR     FORMAT "x(12)"
             LABEL  "БИК"
             HELP   "БИК банка - корреспондента (BIK_R)"
   SKIP
   vNameB    LABEL  "Наим."
             HELP   "Наименование обслуживающего банка (NAME_B)"
   "│" AT 41
   vNameR    LABEL  "Наим."
             HELP   "Наименование банка - корреспондента (NAME_R)"
   "│"  AT COLUMN 41 ROW 15
   "│"  AT COLUMN 41 ROW 16
WITH {&FRAME3} OVERLAY CENTERED
     TITLE COLOR bright-white mTitle[3] ROW 3 SIDE-LABELS.

/*----------------------------------------------------------------------------*/
/*                            FUNCTIONS                                       */
/*----------------------------------------------------------------------------*/
FUNCTION RefreshClientData RETURNS LOG
    ( INPUT iCat AS CHARACTER,
      INPUT iID  AS INT64):
   DEFINE VARIABLE vAmr     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vAddress AS CHARACTER NO-UNDO.

   IF iId =  0 THEN RETURN NO.
   /* Релиз записей */
   RUN PrepareCustBuffer.

   RUN GetCustBuffer (iID,iCat).
   IF RETURN-VALUE <> "" THEN DO:
      MESSAGE COLOR ERROR
              "Клиент, имеющий тип ~"" + iCat +
              "~" и номер "            + string(iID) + "," SKIP
              "не существует."                             SKIP
         VIEW-AS ALERT-BOX ERROR.
      RETURN NO.
   END.
   RUN VD-SENS(vTU <> "1").

   ASSIGN
      vTU      = GetChckAttr(iID,iCat,"TU")
      vTUcb    = ENTRY (INT64(vTU) + 1, vTUcb:LIST-ITEMS IN {&FRAME1})
      vPRU     = GetChckAttr(iId,iCat,"PRU")
      vNameU   = GetChckAttr(iID,iCat,"NAMEU")
      vKodCR   = getchckattr(iID,iCat,"KODCR")
      vKodCN   = GetChckAttr(iID,iCat,"KODCN")
      vAmr     = GetChckAttr(iID,iCat,"AMR")
      vAMRS    = GetEntries(1, vAmr, "~n", "")
      vAMRR    = GetEntries(2, vAmr, "~n", "")
      vAMRG    = GetEntries(3, vAmr, "~n", "")
      vAMRU    = GetEntries(4, vAmr, "~n", "")
      vAMRD    = GetEntries(5, vAmr, "~n", "")
      vAMRK    = GetEntries(6, vAmr, "~n", "")
      vAMRO    = GetEntries(7, vAmr, "~n", "")
      vAdress  = GetChckAttr(iID,iCat,"ADRESS")
      vADRESSS = GetEntries(1, vADRESS, "~n", "")
      vADRESSR = GetEntries(2, vADRESS, "~n", "")
      vADRESSG = GetEntries(3, vADRESS, "~n", "")
      vADRESSU = GetEntries(4, vADRESS, "~n", "")
      vADRESSD = GetEntries(5, vADRESS, "~n", "")
      vADRESSK = GetEntries(6, vADRESS, "~n", "")
      vADRESSO = GetEntries(7, vADRESS, "~n", "")
      vKD      = GetChckAttr(iID,iCat,"KD")
      vSD      = GetChckAttr(iID,iCat,"SD")
      vRG      = GetChckAttr(iID,iCat,"RG")
      vND      = GetChckAttr(iID,iCat,"ND")
      vVD1     = GetChckAttr(iID,iCat,"VD_1")
      vVD2     = GetChckAttr(iID,iCat,"VD_2")
      vVD3     = DATE(GetChckAttr(iID,iCat,"VD_3"))
      vVD4     = GetChckAttr(iID,iCat,"VD_4")
      vVD5     = GetChckAttr(iID,iCat,"VD_5")
      vVD6     = DATE(GetChckAttr(iID,iCat,"VD_6"))
      vVD7     = DATE(GetChckAttr(iID,iCat,"VD_7"))
      vMC1     = GetChckAttr(iID,iCat,"MC_1")
      vMC2     = DATE(GetChckAttr(iID,iCat,"MC_2"))
      vMC3     = DATE(GetChckAttr(iID,iCat,"MC_3"))
      vGR      = DATE (GetChckAttr(iID,iCat,"GR"))
      vBP      = GetChckAttr(iID,iCat,"BP")
      NO-ERROR.

   PAUSE 0.
   DISPLAY
      iId @ vCustID
      vPRU
      vTUcb
      vNameU
      vKodCR
      vKodCN
      vAMRS
      vAMRR
      vAMRG
      vAMRU
      vAMRD
      vAMRK
      vAMRO
      vADRESSS
      vADRESSR
      vADRESSG
      vADRESSU
      vADRESSD
      vADRESSK
      vADRESSO
   WITH {&FRAME1}.
   RETURN YES.
END FUNCTION.
/*----------------------------------------------------------------------------*/
/*                            TRIGGERS                                        */
/*----------------------------------------------------------------------------*/

ON ENTRY OF {&FRAME1}, {&FRAME2}, {&FRAME3} DO:
   DEFINE VARIABLE vCustCatTemp AS CHARACTER NO-UNDO.
   APPLY "VALUE-CHANGED" TO vTypeDisp.
   RUN pEnableDisable (vContr).
   IF iNumFrame = 1 THEN
      APPLY "VALUE-CHANGED" TO vTUcb.
   vCustCatTemp = IF vNewDl OR iNumFrame = 1 THEN vCustCat:SCREEN-VALUE ELSE vCustCat.
   RUN VD-SENS(vCustCatTemp = "Ч").

END.

ON VALUE-CHANGED OF vContr IN {&FRAME1} DO:
   ASSIGN vContr.
   RUN pEnableDisable(vContr).
END.
ON RETURN OF vTypeDisp IN {&FRAME1} APPLY "TAB" TO SELF.
ON RETURN OF vCustCat, vTUcb, vContr IN {&FRAME1} APPLY "TAB" TO SELF.
ON RETURN OF vVPcb IN {&FRAME2} APPLY "TAB" TO SELF.
ON RETURN OF vCardBcb IN {&FRAME3} APPLY "TAB" TO SELF.

ON LEAVE OF vKodCR IN {&FRAME1}, vKodCN IN {&FRAME1}, vKodCNB IN {&FRAME3}, vKodCNR IN {&FRAME3} DO:
   CASE SELF:NAME:
      WHEN "vKodCR"  THEN
         ASSIGN {&FRAME1} vKodCR .
      WHEN "vKodCN"  THEN
         ASSIGN {&FRAME1} vKodCN.
      WHEN "vKodCNB" THEN
         ASSIGN {&FRAME3} vKodCNB.
      WHEN "vKodCNR" THEN
         ASSIGN {&FRAME3} vKodCNR.
   END.
   IF SELF:SCREEN-VALUE <> "0" THEN DO:
      FIND FIRST country WHERE country.country-alt-id = INT64(SUBSTRING(SELF:SCREEN-VALUE, 1, 3)) NO-LOCK NO-ERROR.
      IF NOT AVAIL country THEN DO:
         MESSAGE "Нет страны с кодом" SUBSTRING(SELF:SCREEN-VALUE, 1, 3)
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         RETURN NO-APPLY.
      END.
      ELSE
         SELF:SCREEN-VALUE = FStrPadC(SELF:SCREEN-VALUE, 5, YES, "0").
   END.
   RETURN.
END.

ON VALUE-CHANGED OF vTUcb IN {&FRAME1} DO:
   ASSIGN vTUcb.
   RUN Assign-vTU.
   RUN VD-SENS(LOOKUP(SELF:SCREEN-VALUE, SELF:LIST-ITEMS) <> 1).
 
END.

ON VALUE-CHANGED OF vCustCat IN {&FRAME1} DO:
   vCustID = 0.
   PAUSE 0.
   DISPLAY vCustID WITH {&FRAME1}.
END.

ON LEAVE OF vTypeDisp IN {&FRAME1} DO:
   ASSIGN vTypeDisp.
   RUN Assign-vType.
   IF CAN-FIND (FIRST bDataLine WHERE
                      bDataLine.Data-ID = DataLine.data-Id
                  AND bDataLine.Sym1    = DataLine.Sym1
                  AND bDataLine.Sym2    = vType)
   AND vNewDL THEN DO:
      MESSAGE vTypeDisp "уже существует в данной операции"
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
   END.
END.

ON VALUE-CHANGED OF vTypeDisp IN {&FRAME1} DO:
   ASSIGN vTypeDisp.
   RUN Assign-vType.
   IF CAN-DO({&SEND-LINE} + "," + {&RECV-LINE}, vType) THEN
      ASSIGN
         vContr:HIDDEN    = NO
         vContr:SENSITIVE = YES
      .
   ELSE DO:
      ASSIGN
         vContr:SENSITIVE = NO
         vContr:HIDDEN    = YES
         vContr           = NO
         vVP = "0"
      .
      RUN pEnableDisable (NO).
   END.
   RUN OTHER-SENS(CAN-DO({&SEND-LINE} + "," + {&RECV-LINE}, vType)).
   RETURN NO-APPLY.
END.

ON LEAVE OF vND IN {&FRAME2} DO:
   IF SELF:SCREEN-VALUE <> "0" THEN DO:
      vND = FormatINN (SELF:SCREEN-VALUE, vTU).
      PAUSE 0.
      DISPLAY vND WITH {&FRAME2}.
   END.
END.

ON LEAVE OF vVD3,vVD6,vVD7,vMC2,vMC3,vGr IN {&FRAME2} DO:
   DEFINE VARIABLE vDate AS DATE NO-UNDO.
   ASSIGN
      vDate = DATE(SELF:SCREEN-VALUE) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN DO:
      MESSAGE "Неверно указана дата"
         VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
   END.
END.

ON LEAVE OF vCustCat IN {&FRAME1} DO:
   IF SELF:MODIFIED THEN DO:
      SELF:MODIFIED = NO.
      IF NOT vCustID:MODIFIED OR
         RefreshClientData (SELF:SCREEN-VALUE,INT64(vCustID:SCREEN-VALUE)) THEN
         ASSIGN vCustCat.
      ELSE DO:
         vCustID = 0.
         PAUSE 0.
         DISPLAY vCustID WITH {&FRAME1}.
      END.
   END.
   RUN VD-SENS(SELF:SCREEN-VALUE = "Ч").
END.

ON LEAVE OF vCustID IN {&FRAME1} DO:
   ASSIGN vCustCat.
   IF SELF:MODIFIED THEN DO:
      SELF:MODIFIED = NO.
      IF RefreshClientData (vCustCat:SCREEN-VALUE,INT64 (SELF:SCREEN-VALUE))
      THEN
         ASSIGN vCustID.
      ELSE DO:
         vCustID = 0.
         PAUSE 0.
         DISPLAY vCustID WITH {&FRAME1}.
         RETURN.
      END.
   END.
   RUN VD-SENS(vCustCat:SCREEN-VALUE = "Ч").

END.

ON LEAVE OF vBIKB, vBIKR, vBIKIsB IN {&FRAME3} DO:
   IF SELF:SCREEN-VALUE = "0" THEN RETURN.
   { getbank.i banks SELF:SCREEN-VALUE }
   IF NOT AVAIL banks
      OR (SELF:NAME = "vBIKB" AND vKODCNB BEGINS "643" AND pick-value <> "МФО-9")
      OR (SELF:NAME = "vBIKR" AND vKODCNR BEGINS "643" AND pick-value <> "МФО-9")
   THEN DO:
      MESSAGE "Не существует банка с кодом" SELF:SCREEN-VALUE
         VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      RETURN NO-APPLY.
   END.
   ELSE DO:
      CASE SELF:NAME:
         WHEN "vBIKB" THEN DO:
            vNameB = banks.name.
            PAUSE 0.
            DISPLAY vNameB WITH {&FRAME3}.
         END.
         WHEN "vBIKR" THEN DO:
            vNameR = banks.name.
            PAUSE 0.
            DISPLAY vNameR WITH {&FRAME3}.
         END.
         WHEN "vBIKIsB" THEN DO:
            vNameIsB = banks.name.
            PAUSE 0.
            DISPLAY vNameIsB WITH {&FRAME3}.
         END.
      END.
   END.
END.

ON "F1" OF vCustID IN {&FRAME1} DO:
   ASSIGN vCustCat.
   CASE vCustCat:
      WHEN "Ю" THEN
         RUN browseld.p ("cust-corp",                              /* Класс объекта                */
                         "crClass-Code", "ImaginClient",           /* Коды. Поля для предустановки */
                         ?,                                        /* Поля для блокировки          */
                         4).                                       /* Строка отображения фрейма    */
      WHEN "Ч" THEN
         RUN browseld.p ("person",                                 /* Класс объекта                */
                         "crClass-Code" + CHR(1) + "SetFirstFrm",  /* Коды. Поля для предустановки */
                         "*" + CHR(1) + "1",
                         ?,                                        /* Поля для блокировки          */
                         4).                                       /* Строка отображения фрейма    */
      WHEN "Б" THEN RUN bank-cli.p (4).
      OTHERWISE DO:
         APPLY "entry" TO vCustCat.
         RETURN NO-APPLY.
      END.
   END CASE.
   IF KEYFUNCTION(LASTKEY) <> "end-error" AND
      pick-value           <> ?           AND
      RefreshClientData(vCustCat,INT64(pick-value))
   THEN
      vCustID = INT64(pick-value).
   RETURN NO-APPLY.
END.

ON "F1" OF vKodCR, vKodCN, vKodCNB, vKODCNR ANYWHERE DO:
   RUN count2.p(4).
   IF KEYFUNCTION(LASTKEY) <> "end-error" THEN DO:
      SELF:SCREEN-VALUE = STRING (INT64(pick-value),"999") + "00".
      CASE SELF:NAME:
         WHEN "vKodCR"  THEN
            ASSIGN {&FRAME1} vKodCR .
         WHEN "vKodCN"  THEN
            ASSIGN {&FRAME1} vKodCN.
         WHEN "vKodCNB" THEN
            ASSIGN {&FRAME3} vKodCNB.
         WHEN "vKodCNR" THEN
            ASSIGN {&FRAME3} vKodCNR.
      END.
   END.
   RETURN NO-APPLY.
END.

ON "F1" OF vBIKB, vBIKR, vBIKIsB IN {&FRAME3} DO:
   RUN banks.p (4).
   IF KEYFUNCTION(LASTKEY) <> "end-error" THEN DO:
      SELF:SCREEN-VALUE = GetEntries(2, pick-value, ",", SELF:SCREEN-VALUE).
      ASSIGN vBIKB vBIKR vBIKIsB.
   END.
   RETURN NO-APPLY.
END.

ON "F1" OF vAMRS, vADRESSS IN {&FRAME1} DO:
    RUN codelay.p("КодРег", ?, ?, 3).
   IF KEYFUNCTION(LASTKEY) <> "end-error" THEN DO:
      SELF:SCREEN-VALUE = SUBSTRING(pick-value, 4).
      ASSIGN vAMRS vADRESSS.
   END.
   RETURN NO-APPLY.
END.

ON "F1" OF vKD IN {&FRAME2} DO:
   RUN codelay.p("КодДокум", ?, ?, 3).
   IF KEYFUNCTION(LASTKEY) <> "end-error" THEN DO:
      SELF:SCREEN-VALUE = GetCodeMisc("КодДокум", pick-value, 6).
      ASSIGN vKD.
   END.
   RETURN NO-APPLY.
END.

ON "F1" OF vVD4 IN {&FRAME2} DO:
   RUN codelay.p("VisaType", ?, ?, 3).
   IF KEYFUNCTION(LASTKEY) <> "end-error" THEN DO:
      SELF:SCREEN-VALUE = GetCode("VisaType", pick-value).
      ASSIGN vVD4.
   END.
   RETURN NO-APPLY.
END.

/*----------------------------------------------------------------------------*/
/*                            MAIN BLOCK                                      */
/*----------------------------------------------------------------------------*/
{leg321v2.fnd}

vNewDl = NEW(DataLine).

IF NOT vNewDl AND iNumFrame = 3 AND NOT CAN-DO({&SEND-LINE} + "," + {&RECV-LINE}, vType)  THEN
   RETURN.

IF INT64(vTU) = 5 THEN       /* для неверных значений - вместо конвертации */
   vTU = "4".

ASSIGN
   vCustCat = GetEntries(1, DataLine.Sym3, ",", "")
   vCustID  = INT64(GetEntries (2, DataLine.Sym3, ",", "0"))
   vContr   = (DataLine.Sym3 = "" AND CAN-DO ({&SEND-LINE} + "," + {&RECV-LINE}, vType))
   vTUcb    = ENTRY(MAXIMUM(INT64(vTU) + 1,1), vTucb:LIST-ITEMS IN {&FRAME1})
   vVPcb    = ENTRY(INT64(vVP) + 1, vVPcb:LIST-ITEMS IN {&FRAME2})
   vCardBcb = ENTRY(INT64(vCardB) + 1, vCardBcb:LIST-ITEMS IN {&FRAME3})
.

IF vNewDl THEN DO:
   L1:
   REPEAT:
      RUN update-frame1(OUTPUT mOk).
      IF NOT mOk THEN LEAVE L1.
      L2:
      REPEAT:
         RUN update-frame2(OUTPUT mOk).
         IF NOT mOk THEN LEAVE L2.
         IF CAN-DO({&SEND-LINE} + "," + {&RECV-LINE}, vType) THEN DO:
            RUN update-frame3(OUTPUT mOk).
         END.
         IF mOk THEN LEAVE L2.
      END.
      IF mOk THEN LEAVE L1.
   END.
END.
ELSE DO:
   RUN VALUE("update-frame" + STRING(iNumFrame)) (OUTPUT mOk).
END.

IF mOk THEN
DO:
   {&TstStatEd} RUN StoreDataLine.
END.
IF mOk THEN
DO:
   {&MsgStatEd}
END.

HIDE {&FRAME3} NO-PAUSE.
HIDE {&FRAME2} NO-PAUSE.
HIDE {&FRAME1} NO-PAUSE.

/*----------------------------------------------------------------------------*/
/*                            PROCEDURES                                      */
/*----------------------------------------------------------------------------*/
PROCEDURE update-frame1:
   DEFINE OUTPUT PARAMETER Ok AS LOGICAL NO-UNDO.
   DO ON ERROR  UNDO, LEAVE
      ON ENDKEY UNDO, LEAVE:
      PAUSE 0.
      DISPLAY vTypeDisp vContr WITH {&FRAME1}.
      UPDATE UNLESS-HIDDEN
         vTypeDisp WHEN vNewDl
         vContr
         vCustCat
         vCustID
         vTUcb
         vPRU
         vNameU
         vKodCR
         vAMRS
         vAMRR
         vAMRG
         vAMRU
         vAMRD
         vAMRK
         vAMRO
         vKodCN
         vADRESSS
         vADRESSR
         vADRESSG
         vADRESSU
         vADRESSD
         vADRESSK
         vADRESSO
      WITH FRAME edit1.
      Ok = YES.
   END.
   HIDE {&FRAME1} NO-PAUSE.
END PROCEDURE.

PROCEDURE update-frame2:
   DEFINE OUTPUT PARAMETER Ok AS LOGICAL NO-UNDO.
   DO ON ERROR  UNDO, LEAVE
      ON ENDKEY UNDO, LEAVE:
      PAUSE 0.
      DISPLAY
         vTypeDisp
         vNameU
         WITH {&FRAME2}.
      UPDATE UNLESS-HIDDEN
         vKD
         vSD
         vVD1
         vND
         vRG
         vVD2
         vVD3
         vVD4
         vVD5
         vVD6
         vVD7
         vMC1
         vMC2
         vMC3
         vGR
         vBP
         vVPcb
      WITH FRAME edit2.
      Ok = YES.
   END.
   HIDE {&FRAME2} NO-PAUSE.
END PROCEDURE.

PROCEDURE update-frame3:
   DEFINE OUTPUT PARAMETER Ok AS LOGICAL NO-UNDO.
   DO ON ERROR  UNDO, LEAVE
      ON ENDKEY UNDO, LEAVE:
      PAUSE 0.
      DISPLAY
         vTypeDisp
         vNameU
         WITH {&FRAME3}.
      UPDATE UNLESS-HIDDEN
         vAcctB
         vAcctCorB
         vBIKIsB
         vNameIsB
         vCardBcb
         vKodCNB
         vBIKB
         vNameB
         vKODCNR
         vBIKR
         vNameR
      WITH FRAME edit3.
      Ok = YES.
   END.
   HIDE {&FRAME3} NO-PAUSE.
END PROCEDURE.

PROCEDURE pEnableDisable:
   DEFINE INPUT  PARAMETER iFlag AS LOGICAL NO-UNDO.
   DO WITH {&FRAME1}:
      ASSIGN
         vCustCat:SENSITIVE = NOT iFlag
         vCustId:SENSITIVE  = NOT iFlag
      .
   END.
   DO WITH {&FRAME3}:
      ASSIGN
         vBIKB:SENSITIVE    = iFlag
         vNameB:SENSITIVE   = iFlag
         vKodCNB:SENSITIVE  = iFlag
         vBIKR:SENSITIVE    = iFlag
         vNameR:SENSITIVE   = iFlag
         vKodCNR:SENSITIVE  = iFlag
      .
   END.
END PROCEDURE.

PROCEDURE Assign-vType:
   CASE vTypeDisp:
      WHEN {&SEND-LINE-DISP} THEN
         vType = {&SEND-LINE}.
      WHEN {&SPRX-LINE-DISP} THEN
         vType = {&SPRX-LINE}. 
      OTHERWISE 
         vType = vTypeDisp.
   END CASE.
END PROCEDURE.

PROCEDURE Assign-vTU:
   vTU = STRING(LOOKUP(vTucb, vTuCb:LIST-ITEMS IN {&FRAME1}) - 1).
END PROCEDURE.


PROCEDURE VD-SENS:
   DEFINE INPUT PARAMETER iSens AS LOGICAL NO-UNDO.
/*
   IF vNewDl OR iNumFrame = 2 THEN
   DO WITH {&FRAME2}:
      ASSIGN
         vVD1:SENSITIVE = iSens
         vVD2:SENSITIVE = iSens
         vVD3:SENSITIVE = iSens
      .
   END.
   */
END PROCEDURE.

PROCEDURE OTHER-SENS:
   DEFINE INPUT PARAMETER iSens AS LOGICAL NO-UNDO.

   IF vNewDl OR iNumFrame = 2 THEN
   DO WITH {&FRAME2}:
      ASSIGN
         vVPcb:HIDDEN        = NOT iSens
         vVPcb:SENSITIVE     = iSens
      .
   END.

   IF vNewDl OR iNumFrame = 3 THEN
   DO WITH {&FRAME3}:
      ASSIGN
         vAcctB:HIDDEN       = NOT iSens
         vAcctCorB:HIDDEN    = NOT iSens
         vNameIsB:HIDDEN     = NOT iSens
         vBIKIsB:HIDDEN      = NOT iSens
         vCardBcb:HIDDEN     = NOT iSens
         vAcctB:SENSITIVE    = iSens
         vAcctCorB:SENSITIVE = iSens
         vNameIsB:SENSITIVE  = iSens
         vBIKIsB:SENSITIVE   = iSens
         vCardBcb:SENSITIVE  = iSens
      .
   END.
END PROCEDURE.

PROCEDURE StoreDataLine:
   DEFINE VARIABLE vTxt    AS CHARACTER NO-UNDO EXTENT 49.
   DEFINE VARIABLE vI      AS INT64   NO-UNDO.
   FIND CURRENT DataLine EXCLUSIVE-LOCK.
   /* Для клиентов этого ничего нет */
   IF NOT vContr THEN
      ASSIGN
         vKodCNB  = "0"
         vBIKB    = "0"
         vNameB   = "0"
         vKODCNR  = "0"
         vBIKR    = "0"
         vNameR   = "0"
      .

   ASSIGN
      DataLine.Sym2 = vType
      DataLine.Sym3 = IF vContr THEN "" ELSE (vCustCat + "," + STRING (vCustID))
   .
   ASSIGN
      vTU      = STRING(LOOKUP(vTucb, vTuCb:LIST-ITEMS IN {&FRAME1}) - 1)
      vVP      = STRING(LOOKUP(vVPcb, vVPcb:LIST-ITEMS IN {&FRAME2}) - 1)
      vCardB   = STRING(LOOKUP(vCardBcb, vCardBcb:LIST-ITEMS IN {&FRAME3}) - 1)
      vTxt[1]  = vTU
      vTxt[2]  = FStrNVL(vPRU,"0")
      vTxt[3]  = FStrNVL(REPLACE (vNameU, "~n", ""),"0")
      vTxt[4]  = FStrNVL(vKodCR,"0")
      vTxt[5]  = FStrNVL(vKodCN,"0")
      vTxt[6]  = FStrNVL(vAMRS,"0")
      vTxt[7]  = FStrNVL(vAMRR,"0")
      vTxt[8]  = FStrNVL(vAMRG,"0")
      vTxt[9]  = FStrNVL(vAMRU,"0")
      vTxt[10] = FStrNVL(vAMRD,"0")
      vTxt[11] = FStrNVL(vAMRK,"0")
      vTxt[12] = FStrNVL(vAMRO,"0")
      vTxt[13] = FStrNVL(vADRESSS,"0")
      vTxt[14] = FStrNVL(vADRESSR,"0")
      vTxt[15] = FStrNVL(vADRESSG,"0")
      vTxt[16] = FStrNVL(vADRESSU,"0")
      vTxt[17] = FStrNVL(vADRESSD,"0")
      vTxt[18] = FStrNVL(vADRESSK,"0")
      vTxt[19] = FStrNVL(vADRESSO,"0")
      vTxt[20] = FStrNVL(vKD,"0")
      vTxt[21] = FStrNVL(vSD,"0")
      vTxt[22] = FStrNVL(vRG,"0")
      vTxt[23] = FStrNVL(vND,"0")
      vTxt[24] = FStrNVL(vVD1,"0")
      vTxt[25] = FStrNVL(vVD2,"0")
      vTxt[26] = IF vVD3 = ? THEN EmptyDate (?) ELSE STRING (vVD3, "99/99/9999")
      vTxt[27] = FStrNVL(vVD4,"0")
      vTxt[28] = FStrNVL(vVD5,"0")
      vTxt[29] = IF vVD6 = ? THEN EmptyDate (?) ELSE STRING (vVD6, "99/99/9999")
      vTxt[30] = IF vVD7 = ? THEN EmptyDate (?) ELSE STRING (vVD7, "99/99/9999")
      vTxt[31] = FStrNVL(vMC1,"0")
      vTxt[32] = IF vMC2 = ? THEN EmptyDate (?) ELSE STRING (vMC2, "99/99/9999")
      vTxt[33] = IF vMC3 = ? THEN EmptyDate (?) ELSE STRING (vMC3, "99/99/9999")
      vTxt[34] = IF vGr = ? THEN EmptyDate (?) ELSE STRING (vGr, "99/99/9999")
      vTxt[35] = FStrNVL(vBP,"0")
      vTxt[36] = vVP
      vTxt[37] = "0"
      vTxt[38] = FStrNVL(vAcctB,"0")
      vTxt[39] = FStrNVL(vAcctCorB,"0")
      vTxt[40] = FStrNVL(REPLACE(vNameIsB, "~n", ""),"0")
      vTxt[41] = FStrNVL(vBIKIsB,"0")
      vTxt[42] = vCardB
      vTxt[43] = FStrNVL(REPLACE(vNameB, "~n", ""),"0")
      vTxt[44] = FStrNVL(vKODCNB,"0")
      vTxt[45] = FStrNVL(vBIKB,"0")
      vTxt[46] = FStrNVL(REPLACE(vNameR, "~n", ""),"0")
      vTxt[47] = FStrNVL(vKODCNR,"0")
      vTxt[48] = FStrNVL(vBIKR,"0")
      vTxt[49] = "0"
      .
   DataLine.Txt = "".
   DO vI = 1 TO EXTENT(vTxt):
      DataLine.Txt = DataLine.Txt + (IF vI = 1 THEN "" ELSE "~n") + vTxt[vI].
   END.
END PROCEDURE.
/* $LINTFILE='leg321e2.p' */
/* $LINTMODE='1,6,6,3' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='anba' */
/* $LINTDATE='15/08/2017 19:26:22.294+03:00' */
/*prosignunU6FQEEMAs5aJkXpj41kw*/