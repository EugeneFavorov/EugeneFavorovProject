{globals.i}
/*
ROUTINE-LEVEL ON ERROR UNDO, THROW.
*/
{intrface.get count}
{intrface.get xclass}
{intrface.get db2l}
{intrface.get pack}
{intrface.get cust}

DEFINE INPUT PARAMETER ipParam AS CHARACTER.
DEFINE INPUT-OUTPUT PARAMETER ioRet AS INT64.
/* 'Сообщение в нотариат о появлении, изменении и прекращении залога' */
DEFINE BUFFER NF FOR PLEDGENOTIFICATION.
DEFINE BUFFER NF1 FOR PLEDGENOTIFICATION.
/* 2.  Выгрузить адреса для залогодателя и залогодержателя в таблицы PledgeAddressRF / PledgeAddressForeign.*/
DEF BUFFER AR FOR PLEDGEADDRESSRF-1.
DEF BUFFER AR1 FOR PLEDGEADDRESSRF-1.
DEF BUFFER AF FOR PLEDGEADDRESSFOREIGN-1.
/* 3. Выгрузить залогодателя(-ей) и залогодержателя(-ей) в таблицы PledgeRussianOrganization / PledgeForeignOrganization / PledgePrivatePerson, при выгрузке привязать по идентификаторам ранее выгруженные адреса.*/
DEF BUFFER RO FOR PLEDGERUSSIANORGANIZATION-1.
DEF BUFFER FO FOR PLEDGEFOREIGNORGANIZATION-1.
DEF BUFFER PP FOR PLEDGEPRIVATEPERSON-1.
/* 4. Привязать залогодателя(-ей) к сообщению через таблицу PledgeMessagePledgors с указанием типа залогодержателя. */
DEF BUFFER PG FOR PLEDGEMESSAGEPLEDGORS-1.
/* 5. Привязать залогодержателя(-ей) к сообщению через таблицу PledgeMessagePledgees с указанием типа залогодержателя. */
DEF BUFFER MP FOR PLEDGEMESSAGEPLEDGEES-1.
/* 6. Выгрузить данные о залоге в таблицу PersonalProperties, для автомобилей указывать PropertyType=1 и в поле ID указывать VIN-код автомобиля.*/
DEF BUFFER PJ FOR PERSONALPROPERTIES-1.
DEFINE BUFFER PO FOR PLEDGENOTIFICATIONAPPLICANT-1.

DEFINE BUFFER loan FOR loan.
DEFINE BUFFER term-obl FOR term-obl.
DEFINE BUFFER op-entry FOR op-entry.
DEFINE BUFFER signs FOR signs.
DEFINE BUFFER code FOR code.
DEFINE BUFFER ccode FOR code.

DEFINE VARIABLE mNotifCreNumber AS CHARACTER NO-UNDO.
DEFINE VARIABLE TMP_CHEL    AS CHARACTER NO-UNDO.
DEFINE VARIABLE TMP_CHEL2   AS CHARACTER NO-UNDO.
DEFINE VARIABLE vTCbody AS CHARACTER NO-UNDO.
DEFINE VARIABLE vTCmotor AS CHARACTER NO-UNDO.
DEFINE VARIABLE vTCchassis AS CHARACTER NO-UNDO.
DEFINE VARIABLE CNT_AR AS INT64 INIT 1 NO-UNDO.
DEFINE VARIABLE CNT_AF AS INT64 INIT 1 NO-UNDO.
/* счетчики */
DEFINE VARIABLE CNT_NF AS INT64 INIT 1 NO-UNDO.
/**/
DEFINE VARIABLE CNT_RO AS INT64 INIT 1 NO-UNDO.
DEFINE VARIABLE CNT_FO AS INT64 INIT 1 NO-UNDO.
/* DEFINE VARIABLE CNT_PM AS INT64 INIT 1 NO-UNDO. */
DEFINE VARIABLE CNT_PP AS INT64 INIT 1 NO-UNDO.
DEFINE VARIABLE CNT_MP AS INT64 INIT 1 NO-UNDO.
DEFINE VARIABLE CNT_PJ AS INT64 INIT 1 NO-UNDO.
DEFINE VARIABLE ZALOG_DERZH AS INT64 INIT 0 NO-UNDO.

DEFINE VARIABLE mAPPLICANTID AS INT64 INIT 1 NO-UNDO.
DEFINE VARIABLE mStatus AS INT64 INIT 1 NO-UNDO.

IF NUM-ENTRIES(ipParam) EQ 7 THEN 
    mAPPLICANTID = INT64(ENTRY(7, ipParam)). 
IF NUM-ENTRIES(ipParam) GE 6 THEN 
    mStatus = INT64(ENTRY(6, ipParam)). 

DEFINE TEMP-TABLE HZ NO-UNDO
    /* данные по КД */
    FIELD jNN           AS INT64    /* порядковый номер */
    FIELD jCONT_CODE    AS CHAR     /* номер КД */
    FIELD jFILIAL_ID    AS CHAR
    FIELD jCOMMENT      AS CHAR     /* описание */
    FIELD jOPEN_DATE    AS DATE     /* дата открытия КД */
    FIELD jEND_DATE     AS DATE     /* дата закрытия КД */
    /* информация о залогодателе */
    FIELD iTYPE         AS CHAR     /* тип клиента */
    FIELD iFIRSTNAME    AS CHAR     /* Фамилия */
    FIELD iLASTNAME     AS CHAR     /* Имя */
    FIELD iMIDDLENAME   AS CHAR     /* Отчество */ 
    FIELD iBIRTHDATE    AS DATE     /* Дата рождения */ 
    FIELD iDOCUMENTCODE AS CHAR     /* Код вида документа */
    FIELD iDOCUMENTNAME AS CHAR     /* Вид документа */ 
    FIELD iDOCUMENTSERIESNUMBER AS CHAR /* Серия и номер документа */ 
    /* адрес залогодателя */
    FIELD aTYPE         AS INT64    /* тип адреса */
    FIELD aREG_CODE     AS CHAR     /* код региона */
    FIELD aREG_NAME     AS CHAR     /* наименование региона */
    FIELD aDISTRICT     AS CHAR     /* Район */
    FIELD aCITY         AS CHAR     /* Город */
    FIELD aLOCALITY     AS CHAR     /* Населенный пункт */
    FIELD aSTREET       AS CHAR     /* Улица */
    FIELD aHOUSE        AS CHAR     /* Дом */
    FIELD aBUILDING     AS CHAR     /* Корпус */
    FIELD aAPARTMENT    AS CHAR     /* Квартира */
    /* описание залога */
    FIELD wZALOG_TYPE   AS INT64    /* Для автомобиля - 1, для другого типа залога - 2 */
    FIELD wZALOG_ID     AS CHAR     /* Для автомобиля - VIN-код, для другого типа залога - некий идентификатор */
    FIELD wZALOG_DESC   AS CHAR     /* Описание предмета залога */
    FIELD wZALOG_SF     AS INT64    /* Модель и марка  или год выпуска пустые - нам не нужно */
    FIELD wZALOG_SURR   AS CHAR     /* суррогат договора залога для установки ДР */
    /* new */
    FIELD wNOTICE       AS INT64    /* Принятие - 0, Списание - 1 */
    FIELD term-obl-surrogate AS CHAR
    FIELD NOTIFICATIONREFERENCENUMBER AS CHAR
.

/* Обьявляем переменные считывания адреса*/
{cust-adr.obj  &def-vars     = YES}


/* Заполняем таблицу
COMMENT ON TABLE NOTARY_ONLINE.PLEDGENOTIFICATION IS 'Сообщение в нотариат о появлении, изменении и прекращении залога';
COMMENT ON COLUMN NOTARY_ONLINE.PLEDGENOTIFICATION.CONTRACTNAME IS 'Наименование договора или другой сделки';
COMMENT ON COLUMN NOTARY_ONLINE.PLEDGENOTIFICATION.CONTRACTDATE IS 'Дата договора';
COMMENT ON COLUMN NOTARY_ONLINE.PLEDGENOTIFICATION.CONTRACTNUMBER IS 'Номер договора';
COMMENT ON COLUMN NOTARY_ONLINE.PLEDGENOTIFICATION.CONTRACTTERMOF IS 'Срок завершения контракта';
COMMENT ON COLUMN NOTARY_ONLINE.PLEDGENOTIFICATION.APPLICANTID IS 'Идентификатор сведений о заявителе (ссылка на таблицу PledgeNotificationApplicant)';
COMMENT ON COLUMN NOTARY_ONLINE.PLEDGENOTIFICATION.NOTIFICATIONREFERENCENUMBER IS 'Регистрационный номер уведомления о возникновении залога';
*/
    ioRet = 0.
    MAIN:
    DO ON ERROR UNDO, THROW:
        IF RETRY THEN DO:
            PUT UNFORMATTED "ошибка создания пакета" SKIP.
            UNDO, RETURN ERROR.
        END.
/* Находим договор */        
        FIND FIRST loan
            WHERE RECID(loan) EQ INT64(ENTRY(3,ipParam))
        NO-LOCK NO-ERROR.
        IF NOT AVAILABLE(loan) THEN
        DO:
            PUT UNFORMATTED "Не найдет кредитный договор " + ENTRY(2, ipParam) SKIP.
            UNDO, RETURN ERROR.           
        END.
        ELSE
        DO:
            TMP_CHEL = loan.cust-cat + "," + TRIM(STRING(loan.cust-id)).
        END.
/* Находим договор обеспечения */        
        FIND FIRST term-obl
            WHERE RECID(term-obl) EQ INT64(ENTRY(4,ipParam))
        NO-LOCK NO-ERROR.
        IF NOT AVAILABLE(term-obl) THEN
        DO:
            PUT UNFORMATTED "Не найден договор обеспечения для кредита " + ENTRY(2, ipParam) SKIP.
            UNDO, RETURN ERROR.           
        END.
        
        FOR EACH signs
            WHERE signs.file-name = 'term-obl'
            AND signs.code = 'ВидОб'
            AND signs.xattr-value = 'Автомобиль'
            AND signs.surrogate EQ term-obl.contract + "," + term-obl.cont-code + ",5,"  + STRING(term-obl.end-date, "99/99/99") + "," + STRING(term-obl.nn)
            NO-LOCK:
            LEAVE.              
        END.              
        IF NOT AVAILABLE(signs) THEN
        DO:
            PUT UNFORMATTED "Не найден вид обеспечения - Автомобиль для кредита " + ENTRY(2, ipParam) SKIP.
            UNDO, RETURN ERROR.           
        END.
          
/* Выбираем следующий PLEDGEID секвенция SEQ_PLEDGEID */        
        DO WHILE TRUE:
            CNT_NF = NEXT-VALUE(SEQ_PLEDGEID).
            FIND FIRST NF WHERE NF.PLEDGEID EQ CNT_NF NO-LOCK NO-ERROR.
            IF NOT AVAILABLE(NF) THEN LEAVE.
        END.
   
        CREATE HZ.
        ASSIGN
            HZ.jNN = 1
            HZ.jCONT_CODE = LOAN.DOC-REF
            HZ.jFILIAL_ID = loan.filial-id
            HZ.jCOMMENT = LOAN.CONT-TYPE
            HZ.jOPEN_DATE = LOAN.OPEN-DATE
            HZ.jEND_DATE  = LOAN.END-DATE
            HZ.wNOTICE = ?
            HZ.term-obl-surrogate = SIGNS.SURROGATE
            .
            
        TMP_CHEL2 = GetXAttrValueEx ("term-obl", signs.surrogate, "CustSurr", "").

        IF TMP_CHEL2 <> "" AND NUM-ENTRIES(TMP_CHEL2) > 1 THEN TMP_CHEL = TMP_CHEL2.
            
        /* ФЛ */
        IF ENTRY(1, TMP_CHEL) = "Ч" THEN
        DO:
            /**/
            FIND FIRST PERSON
                WHERE PERSON.PERSON-ID = INT64( ENTRY(2, TMP_CHEL) )
                NO-LOCK NO-ERROR.
            /**/
            IF AVAILABLE(PERSON) THEN
            DO:
                /* инфо */
                HZ.iTYPE = "Ч".
                HZ.iLASTNAME  = PERSON.NAME-LAST.           /* Фамилия */
                HZ.iFIRSTNAME = ENTRY(1, PERSON.FIRST-NAMES, " ").  /* Имя */
                HZ.iMIDDLENAME = SUBSTRING(PERSON.FIRST-NAMES, INDEX(PERSON.FIRST-NAMES, " ") + 1). /* Отчество */ 
                HZ.iBIRTHDATE = PERSON.BIRTHDAY.        /* Дата рождения */ 
                /**/
                FIND FIRST CCODE
                    WHERE CCODE.CLASS = "КодДокум"
                    AND CCODE.PARENT = "КодДокум"
                    AND CCODE.CODE = PERSON.DOCUMENT-ID
                    NO-LOCK NO-ERROR.
                /**/
                IF AVAILABLE(CCODE) THEN
                    HZ.iDOCUMENTCODE = CCODE.VAL.  /* Код вида документа */
                /**/
                HZ.iDOCUMENTNAME = PERSON.DOCUMENT-ID. /* Вид документа */ 

                /* формат серия номер для паспорта */
                IF  HZ.iDOCUMENTCODE                    EQ '21' AND
                    NUM-ENTRIES( PERSON.DOCUMENT)       EQ 3 AND
                    LENGTH( ENTRY( 1, PERSON.DOCUMENT)) EQ 2 AND
                    LENGTH( ENTRY( 2, PERSON.DOCUMENT)) EQ 2 THEN
                    HZ.iDOCUMENTSERIESNUMBER = ENTRY( 1, PERSON.DOCUMENT) + ENTRY( 2, PERSON.DOCUMENT) + ' ' + ENTRY( 3, PERSON.DOCUMENT).
                ELSE HZ.iDOCUMENTSERIESNUMBER = PERSON.DOCUMENT.
                /**/
                /* адрес */
                HZ.aTYPE = 0.
                HZ.aREG_CODE = GetXAttrValueEx ("person", STRING(PERSON.PERSON-ID), "КодРегГНИ", ""). /* код региона */
                /**/
                HZ.aREG_NAME = GetCodeName( "КодРегГНИ", HZ.aREG_CODE).
                {cust-adr.obj
                    &addr-to-vars = YES
                    &tablefield   = "TRIM(person.address[1] + ' ' + person.address[2])"
                }
                HZ.aDISTRICT  = IF TRIM(vOblChar)   NE "" THEN TRIM( vOblChar) ELSE ?.  /* Район */
                HZ.aCITY      = IF TRIM(vGorChar)   NE "" THEN TRIM( vGorChar) ELSE ?.  /* Город */
                HZ.aLOCALITY  = IF TRIM(vPunktChar) NE "" THEN TRIM( vPunktChar) ELSE ?. /* Населенный пункт */
                HZ.aSTREET    = IF TRIM(vUlChar)    NE "" THEN TRIM( vUlChar) ELSE ?.   /* Улица */
                HZ.aHOUSE     = IF TRIM(vDomChar)   NE "" THEN TRIM( vDomChar) ELSE ?.  /* Дом */
                HZ.aBUILDING  = IF TRIM(vKorpChar)  NE "" THEN TRIM( vKorpChar) ELSE ?. /* Корпус */
                HZ.aAPARTMENT = IF TRIM(vKvChar)    NE "" THEN TRIM( vKvChar) ELSE ?.   /* Квартира */
                
            END.
            /**/
        END.
        /**/    
        /* ЮЛ */
        IF ENTRY(1, TMP_CHEL) = "Ю" THEN
        DO:
            /**/
            FIND FIRST CUST-CORP
                WHERE CUST-CORP.CUST-ID = INT64( ENTRY(2, TMP_CHEL) )
                NO-LOCK NO-ERROR.
            /**/
        END.
        /**/
        /*----------------- данные об предмете залога -----------------*/
/*        IF SIGNS.XATTR-VALUE = 'Автомобиль' THEN */
        DO:
            /**/
            HZ.wZALOG_TYPE = 1.
            HZ.wZALOG_SURR = signs.surrogate.
            /* найдем TCVIN */
            HZ.wZALOG_ID = GetXAttrValueEx ("term-obl", signs.surrogate, "TCVIN", "").
            
            /* ОТСУТСТВУЕТ  - не нужно */
            IF INDEX( CAPS(HZ.wZALOG_ID), "ОТСУ") > 0 OR
               TRIM( HZ.wZALOG_ID ) = "-" THEN
            DO:
                /* нужно было ? */
                HZ.wZALOG_ID = ?. 
                /**/
            END.
/*            
MESSAGE signs.surrogate
VIEW-AS ALERT-BOX.
*/
            /* модель и марка */
            HZ.wZALOG_DESC = "Модель автомобиля: " + 
                GetXAttrValueEx ("term-obl", signs.surrogate, "TCbrand", "")
                + " " +
                GetXAttrValueEx ("term-obl", signs.surrogate, "TCmodel", "")
                + ", год выпуска:"
                + GetXAttrValueEx ("term-obl", signs.surrogate, "TCyear", "").
                
            vTCbody    = GetXAttrValueEx ("term-obl", signs.surrogate, "TCbody", "").
            vTCmotor   = GetXAttrValueEx ("term-obl", signs.surrogate, "TCmotor", "").
            vTCchassis = GetXAttrValueEx ("term-obl", signs.surrogate, "TCchassis", "").
            IF vTCbody    NE "" AND NOT vTCbody BEGINS "ОТСУТСТВ"
              THEN HZ.wZALOG_DESC = HZ.wZALOG_DESC + ', № кузова: ' + vTCbody.
            IF vTCmotor   NE "" AND NOT vTCmotor BEGINS "ОТСУТСТВ"
              THEN HZ.wZALOG_DESC = HZ.wZALOG_DESC + ', модель и № двигателя: ' + vTCmotor.
            IF vTCchassis NE "" AND NOT vTCchassis BEGINS "ОТСУТСТВ"
              THEN HZ.wZALOG_DESC = HZ.wZALOG_DESC + ', № шасси: ' + vTCchassis.
            /**/
            IF TRIM( GetXAttrValueEx ("term-obl", signs.surrogate, "TCbrand", "") + GetXAttrValueEx ("term-obl", signs.surrogate, "TCmodel", "") ) = ""
                 THEN
            DO:
                /**/
                HZ.wZALOG_SF = 1.
                /**/
            END.
            ELSE
            DO:
                /**/
                HZ.wZALOG_SF = 0.
                /**/
            END.
            
/* Создаем запись в PLEDGENOTIFICATION */   

        mNotifCreNumber = GetXAttrValueEx("term-obl", 
                            (IF INT64(ENTRY(5, ipParam)) EQ 3 AND NUM-ENTRIES(ipParam) LT 7 THEN    
                                term-obl.contract + "," + term-obl.cont-code + ",5,"  + STRING(term-obl.end-date, "99/99/99") + "," + STRING(term-obl.nn - 1)
                            ELSE signs.surrogate),
                            "NotifRefNumber",
                            ?).
        IF mNotifCreNumber EQ ? THEN
        DO:
            mNotifCreNumber = GetXAttrValueEx("term-obl",
                                (IF INT64(ENTRY(5, ipParam)) EQ 3 AND NUM-ENTRIES(ipParam) LT 7 THEN    
                                    term-obl.contract + "," + term-obl.cont-code + ",5,"  + STRING(term-obl.end-date, "99/99/99") + "," + STRING(term-obl.nn - 1)
                                ELSE signs.surrogate),
                                "reg-zalog",
                                ?).
            IF mNotifCreNumber NE ? THEN
                mNotifCreNumber = ENTRY(2, mNotifCreNumber).
        END.
        IF INT64(ENTRY(5, ipParam)) NE 1 THEN
            FOR EACH NF1
                WHERE NF1.CONTRACTNUMBER EQ ENTRY(1, loan.cont-code, "@")
                AND NF1.NOTIFICATIONTYPE = 1
                AND NF1.STATUS_ = 7
                NO-LOCK
                BY NF1.MODIFYTIME DESC:
                LEAVE.
        END.
        IF AVAILABLE(NF1) THEN
            mNotifCreNumber = NF1.NOTIFICATIONREFERENCENUMBER.
     
/*             NF.APPLICANTID                  = 1 mAPPLICANTID */
        CREATE NF.
        ASSIGN 
            NF.APPLICANTID                  = 1
            NF.CONTRACTDATE                 = loan.open-date
            NF.CONTRACTNAME                 = loan.cont-type
            NF.CONTRACTNUMBER               = ENTRY(1, loan.cont-code, "@")
            NF.CONTRACTTERMOF               = loan.end-date
            NF.CREATETIME                   = NOW
/*            NF.CREATIONREFERENCENUMBER      = (IF INT64(ENTRY(5, ipParam)) EQ 1 THEN ? ELSE mNotifCreNumber) */ 
            NF.CREATIONREFERENCENUMBER      = (IF INT64(ENTRY(5, ipParam)) EQ 1 THEN ? ELSE mNotifCreNumber) 
/*вставка тест потом раскоментарить */ 
/*            NF.CREATIONREFERENCENUMBER      = mNotifCreNumber */ 
            NF.MODIFYTIME                   = NOW
/*            NF.NOTIFICATIONREFERENCENUMBER  = (IF INT64(ENTRY(5, ipParam)) NE 3 THEN ? ELSE mNotifCreNumber) */
            NF.NOTIFICATIONTYPE             = INT64(ENTRY(5, ipParam))
            NF.PLEDGEID                     = CNT_NF
            NF.REGISTRATIONTIME             = ?
            NF.SOURCESYSTEM                 = 1 /* АБС Бисквит */
            NF.STATUS_                      = mStatus /* новое */
            .
            IF INT64(ENTRY(5, ipParam)) EQ 1 THEN
                NF.NOTIFICATIONREFERENCENUMBER  = ?.
        END.    
            VALIDATE NF.
/* */       
    /* вставка информации о банке = залогодержателе */
        /* адрес залогодержателя */
        DO WHILE TRUE:
            CNT_AR = NEXT-VALUE( SEQ_ADDRESSID).
            FIND FIRST AR WHERE AR.ADDRESSID EQ CNT_AR NO-LOCK NO-ERROR.
            IF NOT AVAIL AR THEN LEAVE.
        END.
        RELEASE AR.
        FIND FIRST PO
            WHERE PO.APPLICANTID EQ mAPPLICANTID
        NO-LOCK NO-ERROR.
        FIND FIRST AR1
            WHERE AR1.ADDRESSID EQ PO.ADDRESSRFID
        NO-LOCK NO-ERROR.
        CREATE AR.
         /* IF mAPPLICANTID EQ 1 THEN */ 
            ASSIGN 
                AR.ADDRESSID    = CNT_AR            /* SEQ_ ADDRESSID */
                AR.REGIONCODE   = "77"              /* код региона */
                AR.REGION       = "Москва г"       /* наименование региона */
                AR.DISTRICT     = ?                 /* Район */
                AR.CITY         = ""                /* Город */
                AR.LOCALITY     = ?                 /* Населенный пункт */
                AR.STREET       = "Дербеневская набережная" /* Улица */
                AR.HOUSE        = "11"             /* Дом */
                AR.APARTMENT    = "94"
            .
            /*
        ELSE
            ASSIGN 
                AR.ADDRESSID    = CNT_AR            /* SEQ_ ADDRESSID */
                AR.REGIONCODE   = AR1.REGIONCODE              /* код региона */
                AR.REGION       = AR1.REGION       /* наименование региона */
                AR.DISTRICT     = ?                 /* Район */
                AR.CITY         = ""                /* Город */
                AR.LOCALITY     = ?                 /* Населенный пункт */
                AR.STREET       = AR1.STREET /* Улица */
                AR.HOUSE        = AR1.HOUSE             /* Дом */
            .
            */
        /**/
        VALIDATE AR.
/*        
MESSAGE "AR.ADDRESSID" AR.ADDRESSID 
VIEW-AS ALERT-BOX.        
*/        
        /**/        
        /* информация о залогодержателе */
        DO WHILE TRUE:
            CNT_RO = NEXT-VALUE( SEQ_CLIENTID).
            FIND FIRST RO WHERE RO.CLIENTID EQ CNT_RO NO-LOCK NO-ERROR.
            IF NOT AVAIL RO THEN LEAVE.
        END.
        
        /**/
        CREATE RO.
        /**/
        IF mAPPLICANTID EQ 1 THEN 
        ASSIGN
            RO.CLIENTID  = CNT_RO
            RO.NAMEFULL  = 'Публичное акционерное общество "Плюс Банк"'
            RO.OGRN      = "1025500000624"
            RO.INN       = "5503016736"
            RO.ADDRESSID = CNT_AR
        .
        
        ELSE
        ASSIGN
            RO.CLIENTID  = CNT_RO
            RO.NAMEFULL  = TRIM(PO.NAME)
            RO.OGRN      = TRIM(PO.URN)
            RO.INN       = TRIM(PO.UINN)
            RO.ADDRESSID = CNT_AR
        .
        
        /**/
        VALIDATE RO.
        
        ZALOG_DERZH = CNT_RO.
/* 2. Выгрузить адреса для залогодателя и залогодержателя в таблицы PledgeAddressRF / PledgeAddressForeign.*/
/* следующий идентификатор */
                /**/
                DO WHILE TRUE:
                    CNT_AR = NEXT-VALUE( SEQ_ADDRESSID).
                    FIND FIRST AR WHERE AR.ADDRESSID EQ CNT_AR NO-LOCK NO-ERROR.
                    IF NOT AVAIL AR THEN LEAVE.
                END.
                /**/    
                RELEASE AR.
                /**/
                CREATE AR.
                /**/
                ASSIGN
                    AR.ADDRESSID    = CNT_AR /* SEQ_ ADDRESSID */
                    AR.REGISTRATION = "Y"
                    AR.REGIONCODE   = SUBSTRING(HZ.aREG_CODE, 1, 2)     /* код региона */
                    AR.REGION       = SUBSTRING(HZ.aREG_NAME, 1, 60)    /* наименование региона */
                    AR.DISTRICT     = SUBSTRING(HZ.aDISTRICT, 1, 60)    /* Район */
                    AR.CITY         = SUBSTRING(HZ.aCITY, 1, 60)        /* Город */
                    AR.LOCALITY     = SUBSTRING(HZ.aLOCALITY, 1, 60)    /* Населенный пункт */
                    AR.STREET       = SUBSTRING(HZ.aSTREET, 1, 60)      /* Улица */
                    AR.HOUSE        = SUBSTRING(HZ.aHOUSE, 1, 8)        /* Дом */
                    AR.BUILDING     = SUBSTRING(HZ.aBUILDING, 1, 8)     /* Корпус */
                    AR.APARTMENT    = SUBSTRING(HZ.aAPARTMENT, 1, 8)    /* Квартира */
                .
                /**/
                VALIDATE AR.
                /**/
/*                
MESSAGE ar.city skip AR.STREET
VIEW-AS ALERT-BOX.                
*/
            /**/
            /* 3. Выгрузить залогодателя(-ей) и залогодержателя(-ей) в таблицы PledgeRussianOrganization / PledgeForeignOrganization / PledgePrivatePerson, при выгрузке привязать по идентификаторам ранее выгруженные адреса.*/
            
            DO WHILE TRUE:
                CNT_PP = NEXT-VALUE(SEQ_CLIENTID).
                FIND FIRST PP WHERE PP.CLIENTID EQ CNT_PP NO-LOCK NO-ERROR.
                IF NOT AVAIL PP THEN LEAVE.
            END.
            RELEASE PP.
            
            /* данные о залогодателе */
            CREATE PP.
            /**/
            ASSIGN
                PP.CLIENTID = CNT_PP /* SEQ_CLIENTID */
                PP.FIRSTNAME = SUBSTRING(HZ.iFIRSTNAME, 1, 60)
                PP.LASTNAME = SUBSTRING(HZ.iLASTNAME, 1, 60)
                PP.MIDDLENAME = SUBSTRING(HZ.iMIDDLENAME, 1, 60)
                PP.BIRTHDATE = HZ.iBIRTHDATE
                PP.DOCUMENTCODE = SUBSTRING(HZ.iDOCUMENTCODE, 1, 2)
                PP.DOCUMENTNAME = SUBSTRING(HZ.iDOCUMENTNAME, 1, 255)
                PP.DOCUMENTSERIESNUMBER = SUBSTRING(HZ.iDOCUMENTSERIESNUMBER, 1, 25)
                /* идентификатор адреса залогодателя */
                PP.ADDRESSRFID = CNT_AR
            .
            /**/
            VALIDATE PP.
/*            
MESSAGE PP.LASTNAME skip PP.FIRSTNAME PP.MIDDLENAME
VIEW-AS ALERT-BOX.
*/            
            /**/
            /* 4. Привязать залогодателя(-ей) к сообщению через таблицу PledgeMessagePledgors с указанием типа залогодержателя.*/
            /**/
            CREATE PG.
            /**/
            ASSIGN
                /* идентификатор сообщения */
                PG.PLEDGEID = NF.PLEDGEID
                /* идентификатор залогодателя */
                PG.CLIENTID = PP.CLIENTID
            .
                /* тип клиента */
                IF HZ.iTYPE = "Ч" THEN
                    PG.CLIENTTYPE = "P".
                ELSE    
                    PG.CLIENTTYPE = "L".
                /**/

            /**/
            VALIDATE PG.
            /**/
            /* 5. Привязать залогодержателя(-ей) к сообщению через таблицу PledgeMessagePledgees с указанием типа залогодержателя. */
            /**/
            CREATE MP.
            /**/
            ASSIGN
                /* идентификатор сообщения */
                MP.PLEDGEID = NF.PLEDGEID
                /* идентификатор залогодержателя */
                MP.CLIENTID = ZALOG_DERZH
                /**/
                MP.CLIENTTYPE = "L"
            .
            /**/
            VALIDATE MP.
            /**/
            /* 6. Выгрузить данные о залоге в таблицу PersonalProperties, для автомобилей указывать PropertyType=1 и в поле ID указывать VIN-код автомобиля.*/
            
            DO WHILE TRUE:
                CNT_PJ = NEXT-VALUE(SEQ_PERSONALPROPERTIES).
                FIND FIRST PJ WHERE PJ.PERSONALPROPERTYID EQ CNT_PJ NO-LOCK NO-ERROR.
                IF NOT AVAIL PJ THEN LEAVE.
            END.
            RELEASE PJ.
            
            /**/
            CREATE PJ.
            /**/
            ASSIGN
                PJ.PERSONALPROPERTYID = CNT_PJ /* SEQ_PERSONALPROPERTIES */
                PJ.PLEDGEID = NF.PLEDGEID
                PJ.PROPERTYTYPE = HZ.wZALOG_TYPE
                PJ.ID = SUBSTRING(HZ.wZALOG_ID, 1, 25)
                PJ.DESCRIPTION = SUBSTRING(HZ.wZALOG_DESC, 1, 200)
            .
            VALIDATE PJ.

/*            IF HZ.wNOTICE = 0 THEN
                UpdateSigns("term-obl-gar", HZ.wZALOG_SURR, "reg-zalog-no",
                    STRING( NF.PLEDGEID), ?).
*/
        END.
        ioRet = NF.PLEDGEID.
/*    END.
    END.
*/
/*
CATCH eAnyError AS Progress.Lang.Error:
 PUT UNFORMATTED "CATCH: " + RETURN-VALUE + " " + eAnyError:GetMessage(1) SKIP.
END CATCH.
*/
{intrface.del}
