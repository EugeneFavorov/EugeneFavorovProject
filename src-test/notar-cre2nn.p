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
/* '����饭�� � ���ਠ� � ������, ��������� � �४�饭�� ������' */
DEFINE BUFFER NF FOR PLEDGENOTIFICATION.
DEFINE BUFFER NF1 FOR PLEDGENOTIFICATION.
/* 2.  ���㧨�� ���� ��� ��������⥫� � ��������ঠ⥫� � ⠡���� PledgeAddressRF / PledgeAddressForeign.*/
DEF BUFFER AR FOR PLEDGEADDRESSRF-1.
DEF BUFFER AR1 FOR PLEDGEADDRESSRF-1.
DEF BUFFER AF FOR PLEDGEADDRESSFOREIGN-1.
/* 3. ���㧨�� ��������⥫�(-��) � ��������ঠ⥫�(-��) � ⠡���� PledgeRussianOrganization / PledgeForeignOrganization / PledgePrivatePerson, �� ���㧪� �ਢ易�� �� �����䨪��ࠬ ࠭�� ���㦥��� ����.*/
DEF BUFFER RO FOR PLEDGERUSSIANORGANIZATION-1.
DEF BUFFER FO FOR PLEDGEFOREIGNORGANIZATION-1.
DEF BUFFER PP FOR PLEDGEPRIVATEPERSON-1.
/* 4. �ਢ易�� ��������⥫�(-��) � ᮮ�饭�� �१ ⠡���� PledgeMessagePledgors � 㪠������ ⨯� ��������ঠ⥫�. */
DEF BUFFER PG FOR PLEDGEMESSAGEPLEDGORS-1.
/* 5. �ਢ易�� ��������ঠ⥫�(-��) � ᮮ�饭�� �१ ⠡���� PledgeMessagePledgees � 㪠������ ⨯� ��������ঠ⥫�. */
DEF BUFFER MP FOR PLEDGEMESSAGEPLEDGEES-1.
/* 6. ���㧨�� ����� � ������ � ⠡���� PersonalProperties, ��� ��⮬������ 㪠�뢠�� PropertyType=1 � � ���� ID 㪠�뢠�� VIN-��� ��⮬�����.*/
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
/* ���稪� */
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
    /* ����� �� �� */
    FIELD jNN           AS INT64    /* ���浪��� ����� */
    FIELD jCONT_CODE    AS CHAR     /* ����� �� */
    FIELD jFILIAL_ID    AS CHAR
    FIELD jCOMMENT      AS CHAR     /* ���ᠭ�� */
    FIELD jOPEN_DATE    AS DATE     /* ��� ������ �� */
    FIELD jEND_DATE     AS DATE     /* ��� ������� �� */
    /* ���ଠ�� � ��������⥫� */
    FIELD iTYPE         AS CHAR     /* ⨯ ������ */
    FIELD iFIRSTNAME    AS CHAR     /* ������� */
    FIELD iLASTNAME     AS CHAR     /* ��� */
    FIELD iMIDDLENAME   AS CHAR     /* ����⢮ */ 
    FIELD iBIRTHDATE    AS DATE     /* ��� ஦����� */ 
    FIELD iDOCUMENTCODE AS CHAR     /* ��� ���� ���㬥�� */
    FIELD iDOCUMENTNAME AS CHAR     /* ��� ���㬥�� */ 
    FIELD iDOCUMENTSERIESNUMBER AS CHAR /* ���� � ����� ���㬥�� */ 
    /* ���� ��������⥫� */
    FIELD aTYPE         AS INT64    /* ⨯ ���� */
    FIELD aREG_CODE     AS CHAR     /* ��� ॣ���� */
    FIELD aREG_NAME     AS CHAR     /* ������������ ॣ���� */
    FIELD aDISTRICT     AS CHAR     /* ����� */
    FIELD aCITY         AS CHAR     /* ��த */
    FIELD aLOCALITY     AS CHAR     /* ��ᥫ���� �㭪� */
    FIELD aSTREET       AS CHAR     /* ���� */
    FIELD aHOUSE        AS CHAR     /* ��� */
    FIELD aBUILDING     AS CHAR     /* ����� */
    FIELD aAPARTMENT    AS CHAR     /* ������ */
    /* ���ᠭ�� ������ */
    FIELD wZALOG_TYPE   AS INT64    /* ��� ��⮬����� - 1, ��� ��㣮�� ⨯� ������ - 2 */
    FIELD wZALOG_ID     AS CHAR     /* ��� ��⮬����� - VIN-���, ��� ��㣮�� ⨯� ������ - ����� �����䨪��� */
    FIELD wZALOG_DESC   AS CHAR     /* ���ᠭ�� �।��� ������ */
    FIELD wZALOG_SF     AS INT64    /* ������ � ��ઠ  ��� ��� ���᪠ ����� - ��� �� �㦭� */
    FIELD wZALOG_SURR   AS CHAR     /* ���ண�� ������� ������ ��� ��⠭���� �� */
    /* new */
    FIELD wNOTICE       AS INT64    /* �ਭ�⨥ - 0, ���ᠭ�� - 1 */
    FIELD term-obl-surrogate AS CHAR
    FIELD NOTIFICATIONREFERENCENUMBER AS CHAR
.

/* ���塞 ��६���� ���뢠��� ����*/
{cust-adr.obj  &def-vars     = YES}


/* ������塞 ⠡����
COMMENT ON TABLE NOTARY_ONLINE.PLEDGENOTIFICATION IS '����饭�� � ���ਠ� � ������, ��������� � �४�饭�� ������';
COMMENT ON COLUMN NOTARY_ONLINE.PLEDGENOTIFICATION.CONTRACTNAME IS '������������ ������� ��� ��㣮� ᤥ���';
COMMENT ON COLUMN NOTARY_ONLINE.PLEDGENOTIFICATION.CONTRACTDATE IS '��� �������';
COMMENT ON COLUMN NOTARY_ONLINE.PLEDGENOTIFICATION.CONTRACTNUMBER IS '����� �������';
COMMENT ON COLUMN NOTARY_ONLINE.PLEDGENOTIFICATION.CONTRACTTERMOF IS '�ப �����襭�� ����ࠪ�';
COMMENT ON COLUMN NOTARY_ONLINE.PLEDGENOTIFICATION.APPLICANTID IS '�����䨪��� ᢥ����� � ��⥫� (��뫪� �� ⠡���� PledgeNotificationApplicant)';
COMMENT ON COLUMN NOTARY_ONLINE.PLEDGENOTIFICATION.NOTIFICATIONREFERENCENUMBER IS '�������樮��� ����� 㢥�������� � ������������� ������';
*/
    ioRet = 0.
    MAIN:
    DO ON ERROR UNDO, THROW:
        IF RETRY THEN DO:
            PUT UNFORMATTED "�訡�� ᮧ����� �����" SKIP.
            UNDO, RETURN ERROR.
        END.
/* ��室�� ������� */        
        FIND FIRST loan
            WHERE RECID(loan) EQ INT64(ENTRY(3,ipParam))
        NO-LOCK NO-ERROR.
        IF NOT AVAILABLE(loan) THEN
        DO:
            PUT UNFORMATTED "�� ������ �।��� ������� " + ENTRY(2, ipParam) SKIP.
            UNDO, RETURN ERROR.           
        END.
        ELSE
        DO:
            TMP_CHEL = loan.cust-cat + "," + TRIM(STRING(loan.cust-id)).
        END.
/* ��室�� ������� ���ᯥ祭�� */        
        FIND FIRST term-obl
            WHERE RECID(term-obl) EQ INT64(ENTRY(4,ipParam))
        NO-LOCK NO-ERROR.
        IF NOT AVAILABLE(term-obl) THEN
        DO:
            PUT UNFORMATTED "�� ������ ������� ���ᯥ祭�� ��� �।�� " + ENTRY(2, ipParam) SKIP.
            UNDO, RETURN ERROR.           
        END.
        
        FOR EACH signs
            WHERE signs.file-name = 'term-obl'
            AND signs.code = '�����'
            AND signs.xattr-value = '��⮬�����'
            AND signs.surrogate EQ term-obl.contract + "," + term-obl.cont-code + ",5,"  + STRING(term-obl.end-date, "99/99/99") + "," + STRING(term-obl.nn)
            NO-LOCK:
            LEAVE.              
        END.              
        IF NOT AVAILABLE(signs) THEN
        DO:
            PUT UNFORMATTED "�� ������ ��� ���ᯥ祭�� - ��⮬����� ��� �।�� " + ENTRY(2, ipParam) SKIP.
            UNDO, RETURN ERROR.           
        END.
          
/* �롨ࠥ� ᫥���騩 PLEDGEID ᥪ����� SEQ_PLEDGEID */        
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
            
        /* �� */
        IF ENTRY(1, TMP_CHEL) = "�" THEN
        DO:
            /**/
            FIND FIRST PERSON
                WHERE PERSON.PERSON-ID = INT64( ENTRY(2, TMP_CHEL) )
                NO-LOCK NO-ERROR.
            /**/
            IF AVAILABLE(PERSON) THEN
            DO:
                /* ��� */
                HZ.iTYPE = "�".
                HZ.iLASTNAME  = PERSON.NAME-LAST.           /* ������� */
                HZ.iFIRSTNAME = ENTRY(1, PERSON.FIRST-NAMES, " ").  /* ��� */
                HZ.iMIDDLENAME = SUBSTRING(PERSON.FIRST-NAMES, INDEX(PERSON.FIRST-NAMES, " ") + 1). /* ����⢮ */ 
                HZ.iBIRTHDATE = PERSON.BIRTHDAY.        /* ��� ஦����� */ 
                /**/
                FIND FIRST CCODE
                    WHERE CCODE.CLASS = "�������"
                    AND CCODE.PARENT = "�������"
                    AND CCODE.CODE = PERSON.DOCUMENT-ID
                    NO-LOCK NO-ERROR.
                /**/
                IF AVAILABLE(CCODE) THEN
                    HZ.iDOCUMENTCODE = CCODE.VAL.  /* ��� ���� ���㬥�� */
                /**/
                HZ.iDOCUMENTNAME = PERSON.DOCUMENT-ID. /* ��� ���㬥�� */ 

                /* �ଠ� ��� ����� ��� ��ᯮ�� */
                IF  HZ.iDOCUMENTCODE                    EQ '21' AND
                    NUM-ENTRIES( PERSON.DOCUMENT)       EQ 3 AND
                    LENGTH( ENTRY( 1, PERSON.DOCUMENT)) EQ 2 AND
                    LENGTH( ENTRY( 2, PERSON.DOCUMENT)) EQ 2 THEN
                    HZ.iDOCUMENTSERIESNUMBER = ENTRY( 1, PERSON.DOCUMENT) + ENTRY( 2, PERSON.DOCUMENT) + ' ' + ENTRY( 3, PERSON.DOCUMENT).
                ELSE HZ.iDOCUMENTSERIESNUMBER = PERSON.DOCUMENT.
                /**/
                /* ���� */
                HZ.aTYPE = 0.
                HZ.aREG_CODE = GetXAttrValueEx ("person", STRING(PERSON.PERSON-ID), "���������", ""). /* ��� ॣ���� */
                /**/
                HZ.aREG_NAME = GetCodeName( "���������", HZ.aREG_CODE).
                {cust-adr.obj
                    &addr-to-vars = YES
                    &tablefield   = "TRIM(person.address[1] + ' ' + person.address[2])"
                }
                HZ.aDISTRICT  = IF TRIM(vOblChar)   NE "" THEN TRIM( vOblChar) ELSE ?.  /* ����� */
                HZ.aCITY      = IF TRIM(vGorChar)   NE "" THEN TRIM( vGorChar) ELSE ?.  /* ��த */
                HZ.aLOCALITY  = IF TRIM(vPunktChar) NE "" THEN TRIM( vPunktChar) ELSE ?. /* ��ᥫ���� �㭪� */
                HZ.aSTREET    = IF TRIM(vUlChar)    NE "" THEN TRIM( vUlChar) ELSE ?.   /* ���� */
                HZ.aHOUSE     = IF TRIM(vDomChar)   NE "" THEN TRIM( vDomChar) ELSE ?.  /* ��� */
                HZ.aBUILDING  = IF TRIM(vKorpChar)  NE "" THEN TRIM( vKorpChar) ELSE ?. /* ����� */
                HZ.aAPARTMENT = IF TRIM(vKvChar)    NE "" THEN TRIM( vKvChar) ELSE ?.   /* ������ */
                
            END.
            /**/
        END.
        /**/    
        /* �� */
        IF ENTRY(1, TMP_CHEL) = "�" THEN
        DO:
            /**/
            FIND FIRST CUST-CORP
                WHERE CUST-CORP.CUST-ID = INT64( ENTRY(2, TMP_CHEL) )
                NO-LOCK NO-ERROR.
            /**/
        END.
        /**/
        /*----------------- ����� �� �।��� ������ -----------------*/
/*        IF SIGNS.XATTR-VALUE = '��⮬�����' THEN */
        DO:
            /**/
            HZ.wZALOG_TYPE = 1.
            HZ.wZALOG_SURR = signs.surrogate.
            /* ������ TCVIN */
            HZ.wZALOG_ID = GetXAttrValueEx ("term-obl", signs.surrogate, "TCVIN", "").
            
            /* �����������  - �� �㦭� */
            IF INDEX( CAPS(HZ.wZALOG_ID), "����") > 0 OR
               TRIM( HZ.wZALOG_ID ) = "-" THEN
            DO:
                /* �㦭� �뫮 ? */
                HZ.wZALOG_ID = ?. 
                /**/
            END.
/*            
MESSAGE signs.surrogate
VIEW-AS ALERT-BOX.
*/
            /* ������ � ��ઠ */
            HZ.wZALOG_DESC = "������ ��⮬�����: " + 
                GetXAttrValueEx ("term-obl", signs.surrogate, "TCbrand", "")
                + " " +
                GetXAttrValueEx ("term-obl", signs.surrogate, "TCmodel", "")
                + ", ��� ���᪠:"
                + GetXAttrValueEx ("term-obl", signs.surrogate, "TCyear", "").
                
            vTCbody    = GetXAttrValueEx ("term-obl", signs.surrogate, "TCbody", "").
            vTCmotor   = GetXAttrValueEx ("term-obl", signs.surrogate, "TCmotor", "").
            vTCchassis = GetXAttrValueEx ("term-obl", signs.surrogate, "TCchassis", "").
            IF vTCbody    NE "" AND NOT vTCbody BEGINS "��������"
              THEN HZ.wZALOG_DESC = HZ.wZALOG_DESC + ', � �㧮��: ' + vTCbody.
            IF vTCmotor   NE "" AND NOT vTCmotor BEGINS "��������"
              THEN HZ.wZALOG_DESC = HZ.wZALOG_DESC + ', ������ � � �����⥫�: ' + vTCmotor.
            IF vTCchassis NE "" AND NOT vTCchassis BEGINS "��������"
              THEN HZ.wZALOG_DESC = HZ.wZALOG_DESC + ', � ���: ' + vTCchassis.
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
            
/* ������� ������ � PLEDGENOTIFICATION */   

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
/*��⠢�� ��� ��⮬ �᪮������� */ 
/*            NF.CREATIONREFERENCENUMBER      = mNotifCreNumber */ 
            NF.MODIFYTIME                   = NOW
/*            NF.NOTIFICATIONREFERENCENUMBER  = (IF INT64(ENTRY(5, ipParam)) NE 3 THEN ? ELSE mNotifCreNumber) */
            NF.NOTIFICATIONTYPE             = INT64(ENTRY(5, ipParam))
            NF.PLEDGEID                     = CNT_NF
            NF.REGISTRATIONTIME             = ?
            NF.SOURCESYSTEM                 = 1 /* ��� ��᪢�� */
            NF.STATUS_                      = mStatus /* ����� */
            .
            IF INT64(ENTRY(5, ipParam)) EQ 1 THEN
                NF.NOTIFICATIONREFERENCENUMBER  = ?.
        END.    
            VALIDATE NF.
/* */       
    /* ��⠢�� ���ଠ樨 � ����� = ��������ঠ⥫� */
        /* ���� ��������ঠ⥫� */
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
                AR.REGIONCODE   = "77"              /* ��� ॣ���� */
                AR.REGION       = "��᪢� �"       /* ������������ ॣ���� */
                AR.DISTRICT     = ?                 /* ����� */
                AR.CITY         = ""                /* ��த */
                AR.LOCALITY     = ?                 /* ��ᥫ���� �㭪� */
                AR.STREET       = "��ࡥ���᪠� ����०���" /* ���� */
                AR.HOUSE        = "11"             /* ��� */
                AR.APARTMENT    = "94"
            .
            /*
        ELSE
            ASSIGN 
                AR.ADDRESSID    = CNT_AR            /* SEQ_ ADDRESSID */
                AR.REGIONCODE   = AR1.REGIONCODE              /* ��� ॣ���� */
                AR.REGION       = AR1.REGION       /* ������������ ॣ���� */
                AR.DISTRICT     = ?                 /* ����� */
                AR.CITY         = ""                /* ��த */
                AR.LOCALITY     = ?                 /* ��ᥫ���� �㭪� */
                AR.STREET       = AR1.STREET /* ���� */
                AR.HOUSE        = AR1.HOUSE             /* ��� */
            .
            */
        /**/
        VALIDATE AR.
/*        
MESSAGE "AR.ADDRESSID" AR.ADDRESSID 
VIEW-AS ALERT-BOX.        
*/        
        /**/        
        /* ���ଠ�� � ��������ঠ⥫� */
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
            RO.NAMEFULL  = '�㡫�筮� ��樮��୮� ����⢮ "���� ����"'
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
/* 2. ���㧨�� ���� ��� ��������⥫� � ��������ঠ⥫� � ⠡���� PledgeAddressRF / PledgeAddressForeign.*/
/* ᫥���騩 �����䨪��� */
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
                    AR.REGIONCODE   = SUBSTRING(HZ.aREG_CODE, 1, 2)     /* ��� ॣ���� */
                    AR.REGION       = SUBSTRING(HZ.aREG_NAME, 1, 60)    /* ������������ ॣ���� */
                    AR.DISTRICT     = SUBSTRING(HZ.aDISTRICT, 1, 60)    /* ����� */
                    AR.CITY         = SUBSTRING(HZ.aCITY, 1, 60)        /* ��த */
                    AR.LOCALITY     = SUBSTRING(HZ.aLOCALITY, 1, 60)    /* ��ᥫ���� �㭪� */
                    AR.STREET       = SUBSTRING(HZ.aSTREET, 1, 60)      /* ���� */
                    AR.HOUSE        = SUBSTRING(HZ.aHOUSE, 1, 8)        /* ��� */
                    AR.BUILDING     = SUBSTRING(HZ.aBUILDING, 1, 8)     /* ����� */
                    AR.APARTMENT    = SUBSTRING(HZ.aAPARTMENT, 1, 8)    /* ������ */
                .
                /**/
                VALIDATE AR.
                /**/
/*                
MESSAGE ar.city skip AR.STREET
VIEW-AS ALERT-BOX.                
*/
            /**/
            /* 3. ���㧨�� ��������⥫�(-��) � ��������ঠ⥫�(-��) � ⠡���� PledgeRussianOrganization / PledgeForeignOrganization / PledgePrivatePerson, �� ���㧪� �ਢ易�� �� �����䨪��ࠬ ࠭�� ���㦥��� ����.*/
            
            DO WHILE TRUE:
                CNT_PP = NEXT-VALUE(SEQ_CLIENTID).
                FIND FIRST PP WHERE PP.CLIENTID EQ CNT_PP NO-LOCK NO-ERROR.
                IF NOT AVAIL PP THEN LEAVE.
            END.
            RELEASE PP.
            
            /* ����� � ��������⥫� */
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
                /* �����䨪��� ���� ��������⥫� */
                PP.ADDRESSRFID = CNT_AR
            .
            /**/
            VALIDATE PP.
/*            
MESSAGE PP.LASTNAME skip PP.FIRSTNAME PP.MIDDLENAME
VIEW-AS ALERT-BOX.
*/            
            /**/
            /* 4. �ਢ易�� ��������⥫�(-��) � ᮮ�饭�� �१ ⠡���� PledgeMessagePledgors � 㪠������ ⨯� ��������ঠ⥫�.*/
            /**/
            CREATE PG.
            /**/
            ASSIGN
                /* �����䨪��� ᮮ�饭�� */
                PG.PLEDGEID = NF.PLEDGEID
                /* �����䨪��� ��������⥫� */
                PG.CLIENTID = PP.CLIENTID
            .
                /* ⨯ ������ */
                IF HZ.iTYPE = "�" THEN
                    PG.CLIENTTYPE = "P".
                ELSE    
                    PG.CLIENTTYPE = "L".
                /**/

            /**/
            VALIDATE PG.
            /**/
            /* 5. �ਢ易�� ��������ঠ⥫�(-��) � ᮮ�饭�� �१ ⠡���� PledgeMessagePledgees � 㪠������ ⨯� ��������ঠ⥫�. */
            /**/
            CREATE MP.
            /**/
            ASSIGN
                /* �����䨪��� ᮮ�饭�� */
                MP.PLEDGEID = NF.PLEDGEID
                /* �����䨪��� ��������ঠ⥫� */
                MP.CLIENTID = ZALOG_DERZH
                /**/
                MP.CLIENTTYPE = "L"
            .
            /**/
            VALIDATE MP.
            /**/
            /* 6. ���㧨�� ����� � ������ � ⠡���� PersonalProperties, ��� ��⮬������ 㪠�뢠�� PropertyType=1 � � ���� ID 㪠�뢠�� VIN-��� ��⮬�����.*/
            
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
