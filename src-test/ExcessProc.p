/*                                               
        ��� "���� ����"
      Comment: ����� ����譥 ���᫥���� ��業⮢ 
   Parameters:  �室�騥: cContCode ����� ������; dateOp ��� ����樮����� ���; dateLastNach ��� ��᫥����� ���᫥���
               ��室�騥: dPrev �㬬� ����譥 ���� �� �।��騥 ����; dCurr �㬬� ����譥 ���� �� ⥪�騩 ���
         Uses:
      Used by: extrpars.fun
      Created: � ���� ⠪ 2013 KAM
     Modifier: 23/10/2014 KAU ��䠪�ਭ� ���� 
                24/10/2014 KAU ������� �㭪樮��� ��� ������ ��業⮢ ��稭�� � ���䭮�� ��ਮ��.
*/

    DEFINE INPUT PARAM cContCode    AS CHAR NO-UNDO.    /* ����� ������ */
    DEFINE INPUT PARAM dateOp       AS DATE NO-UNDO.    /* ��� ����樮����� ���  */
    DEFINE INPUT PARAM dateLastNach AS DATE NO-UNDO.    /* ��� ��᫥����� ���᫥���  */
    DEFINE OUTPUT PARAM dPrev       AS DEC NO-UNDO.     /* �㬬� ����譥 ���� �� �।��騥 ���� */
    DEFINE OUTPUT PARAM dCurr       AS DEC NO-UNDO.     /* �㬬� ����譥 ���� �� ⥪�騩 ��� */

    {globals.i}
    {intrface.get loan}
    {intrface.get i254}
    {intrface.get comm}
    {client.i}
    {tmprecid.def}
    {wordwrap.def}
    {navigate.def}
    {flt-file.i}
    {sh-defs.i}
    {ksh-defs.i new}
    {intrface.get dps}
    {dpsproc.def}
    {pp-corr.p}

    {svednew.i}

dPrev = 0.
dCurr = 0.

DEF VAR dPrevReal       AS DEC INIT 0 NO-UNDO.
DEF VAR dCurrReal       AS DEC INIT 0 NO-UNDO.

DEF VAR dateCurrYear    AS DATE     NO-UNDO.    /*��� ��砫� ����*/
DEF VAR ostatok         AS DEC      NO-UNDO.    /*���⮪ ��筮�� ������*/
DEF VAR lastDate        AS DATE     NO-UNDO.    /*��� ��᫥���� ����樨 �� ����*/
DEF VAR dateKap         AS DATE     NO-UNDO.    /*1) ����砫쭮 ᬮ��� ���� ���� ����権. � ���쭥�襬 ��⠥� ��������� ���� ����⠫���樨*/
DEF VAR shtraf          AS INT64    NO-UNDO.    /*�ᯮ������ � case ��� ���।������ ��।������ ������ �� ⨯� ��� ���䮢����*/ 
                                                /* 1 = ��� �������, � ������ �� �� ��⨨ �㤥� ���䭠� �⠢�� */
                                                /* 2 = ��� �������, � ������ �� ��⨨ �� ��ࢮ� ����⠫���樨 �㤥� ���䭠� �⠢�� */
                                                /* 3 = ��� �������, � ������ ���䭠� �⠢�� ������ �� ���� ���� */
                                                /* 4 = ��� ⨯�� �������, � ������ �� ��⨨ �� 1 ����⠫���樨 - �⠢�� �㤥� 0.1%, ����� ���䭠� �⠢�� ������ �� ���� ���� */
                                                /* 5 = ���ᨮ��� */
DEF VAR snatie          AS LOG      NO-UNDO.    /*䫠� ��᫥���� ������ �� ������ �����ᥭ�� ��� ��⨥*/
DEF VAR osnComm         AS CHAR     NO-UNDO.    /*��� �᭮���� �����ᨨ ������*/
DEF VAR shtrComm        AS CHAR     NO-UNDO.    /*��� ���䭮� �����ᨨ ������*/
DEF VAR summProc        AS DEC      NO-UNDO.
DEF VAR raschProc       AS DEC      NO-UNDO.
DEF VAR acct42          AS CHAR     NO-UNDO.    /*�᭮���� ��� ������*/
DEF VAR acct474         AS CHAR     NO-UNDO.    /*��� ��業⮢ �ਢ易��� � ������*/
DEF VAR acct706         AS CHAR     NO-UNDO.    /*��� ��室�� �ਢ易��� � ������*/
DEF VAR acctTr          AS CHAR     NO-UNDO.    /*��� ��� ����᫥��� ��業⮢*/
DEF VAR notIndivid      AS LOG      NO-UNDO.    /*䫠� �㫥��� �㬬� ���㬥��.... � � ��㣮� ���� 䫠� ������⢨� �������㠫쭮�� ��� ��� ��� ������*/
DEF VAR pPeriod         AS DEC      NO-UNDO.
DEF VAR fullPeriod      AS DEC      NO-UNDO.
DEF VAR periodSnatie    AS DEC      NO-UNDO.
DEF VAR perShtraf       AS DEC      NO-UNDO.
DEF VAR stavkaShtraf    AS DEC      NO-UNDO.    /*��業� ���䭮� �⠢�� ��� ������ ��� ��ਮ�� � ���஬ ��묠��*/
DEF VAR stavkaOsnovn    AS DEC      NO-UNDO.
DEF VAR lastOstatok     AS DEC      NO-UNDO.
DEF VAR lastOstatokRasch AS DEC     NO-UNDO.    /*���⮪ ��᫥ �।��饩 ����樨 ��� ��� ���᫥���*/
DEF VAR tempDate        AS DATE     NO-UNDO.    /*��६����� � ������ ����娢��� ��� �����*/
DEF VAR tempDate2       AS DATE     NO-UNDO.    /*���� ��� ������ �� �ᯮ��㥬 ����� �� ����� �ᯮ�짮���� tempDate*/
DEF VAR daysInYear      AS INT64    NO-UNDO.    /*������⢮ ���� � ����*/
DEF VAR myCur           LIKE loan.currency NO-UNDO.
DEF VAR firstOstatok    AS DEC      NO-UNDO.    /*���⮪ ������ � ���� ������*/
DEF VAR prich474_42     AS LOG      NO-UNDO.    /*䫠� ����稥 ���� ������ ���᫥��� �� ��� ������*/
DEF VAR amt_rub_cur     AS DEC      NO-UNDO.    /*�㬬� ������ �⮩ �஢���� ��� ���ᥭ�� � ⠡���� tt-dviacct*/
DEF VAR procRaschAdd    AS LOG      NO-UNDO.    /*䫠� ����� ���㬥�� � ���� ��業⮢ FALSE - 㦥 ������*/
DEF VAR dateFirstSnatie AS DATE     NO-UNDO.    /*��� ��ࢮ�� ���䭮�� ������ ������*/
DEF VAR status_vkl      AS CHAR     NO-UNDO.    /*����� �⮡࠭���� ������*/
DEF VAR countdps_t      AS INT64    NO-UNDO.    /*������⢮ �᭮���� ��⮢ ������ � �������*/
DEF VAR isVost          AS LOG      NO-UNDO.    /*����室��� �� ���� � ������� ������ �⠢�� ��*/
DEF VAR tmpOstatok      AS DEC      NO-UNDO.    /*� ����� ���⮪ �᭮����� ��� �� ���� ������ ����樨 �� ����*/
   
/* � tt-dviacct - ࠧ��⨥ ��ਮ��� �� �⫨��騬�� �㬬�� */
DEF TEMP-TABLE tt-dviacct
    FIELD cont-code LIKE loan.cont-code /*����� ������� ������*/
    FIELD cont-type LIKE loan.cont-type /*⨯ ������*/
    FIELD ostatokAcct AS DEC INIT 0     /*���⮪ ��� ��室�騩*/
    FIELD ostatokAcct-Proc AS DEC INIT 0 /*���⮪ ��� ����� ��業��*/
    FIELD cur LIKE acct.currency        /*����� ������*/
    FIELD acct LIKE acct.acct           /*��� ������, ��� ���� �� ⮫쪮 �᭮���� ��� ������ �뢠��*/
    FIELD debet AS LOG INIT FALSE       /*����� 䫠� �⮩ ����樨. ���⨥ TRUE*/
    FIELD posDate AS DATE               /*��� ����権 �� ��������*/
    FIELD nachProc AS LOG INIT FALSE    /*䫠� ⮣� �� � ��� ���� �뫮 ���᫥��� ��業⮢*/
    FIELD amt_rub_cur AS DEC INIT 0     /*�㬬� ����権 �� �⮬� �������� � �⮬ ���*/
    .

/* � tt-stavka - ࠧ��⨥ ��ਮ��� �� �⫨��騬�� �㬬�� � �⫨��騬�� �⠢��� */ 
DEF TEMP-TABLE tt-stavka
    FIELD cont-code LIKE loan.cont-code     /*����� ������� ������*/
    FIELD cont-type LIKE loan.cont-type     /*⨯ ������*/
    FIELD ostatokAcct AS DEC                /*���⮪ ��� �� ����*/
    FIELD ostatokAcctRasch AS DEC           /*���⮪ ����� ������ ����*/
    FIELD acct LIKE acct.acct               /*��� ������*/
    FIELD startDate AS DATE                 /*��� �।��饣� ���㬥��*/
    FIELD endDate AS DATE                   /*��� ������� ���㬥��*/
    FIELD closeDate AS DATE                 /*��� ������� ������*/
    FIELD period AS DEC                     /*ࠧ��� ����� ��⮩ �।��饣� ���㬥�� � ��⮩ �뭥譥�� ���㬥��*/
    FIELD cur LIKE acct.currency            /*����� ������*/
    FIELD stavkaOsn LIKE comm-rate.rate-comm    /*% �⠢�� �᭮����*/
    FIELD stavkaShtr LIKE comm-rate.rate-comm   /*% �⠢�� ���䭮�*/
    FIELD procRasch AS DEC                  /*���⠭�� ��業�� �� ��ਮ�*/
    FIELD shtraf AS INT64                   /*⨯ ���䭮�� ������ � ������*/
    FIELD summProc AS DEC                   /*�㬬� ���᫥���� ��業⮢. ����쭠� �� ��� �६�*/
    FIELD daysInYear AS INT64               /*������⢮ ���� � ���� � ���஬ �������� �� ������*/
    FIELD fullPeriod AS INT64               /*���� ��ਮ� �� ������ �� ���� ���㬥��*/
    FIELD nachProc AS LOG INIT FALSE        /*�뫮 �� � ��� ���� ���᫥��� ��業⮢*/
    FIELD procRaschAdd AS LOG INIT FALSE        /*䫠� ����� ���㬥�� � ���� ��業⮢ FALSE - 㦥 ������, TRUE � �������襥 ���᫥��� ����� ���⢮����*/
    .

/* � tt-comm-rate ����������� �⠢�� �� ������� ������ �� �ᥬ ��ਮ��� �� �ᥬ ⨯�� ���䮢*/    
DEF TEMP-TABLE tt-comm-rate     
    FIELD rate-comm LIKE comm-rate.rate-comm    /*��業� �⠢��*/
    FIELD period LIKE comm-rate.period          /*��ਮ�*/
    FIELD shtraf AS INT64 INIT 0                /*⨯ ���� 0 - �᭠����*/
    .                                           /*1 - ���䭠�*/
                                                /*2 - ����*/
    
FIND FIRST loan WHERE loan.contract  EQ "dps"
                        AND loan.cont-code = cContCode NO-LOCK NO-ERROR.

IF AVAIL loan THEN DO:
    
    {empty tt-dviacct}
    {empty tt-stavka}
    {empty tt-comm-rate}

    dateCurrYear = DATE(1, 1, YEAR(dateOp)).
    tempDate = dateOp.
    dateOp = dateOp .
    IF LOOKUP(loan.cont-type,listContType) > 0  
        AND loan.open-date < dateOp THEN DO:
            /*��⠥� ������⢮ ��⮢ �᫨ ����� 1, ����室�� ��筮� ����஫�*/
        SELECT COUNT(*) INTO countdps_t FROM loan-acct WHERE loan-acct.cont-code = loan.cont-code AND loan-acct.acct-type = 'loan-dps-t'.
      IF countdps_t = 1 THEN DO: 
        status_vkl = loan-status.
        acct474 = ''.
        acct706 = ''.
        acctTr = ''.
        summProc = 0.
        lastOstatok = 0.
        lastOstatokRasch = 0.
        prich474_42 = FALSE.
        dateFirstSnatie = loan.open-date.
        shtraf = 0.
        RUN get-summ-beg-ost IN h_dpspc (RECID (loan),loan.open-date,loan.open-date, OUTPUT firstOstatok).
        
        /*   ��� �����᫥��� ��業⮢ */
        FIND FIRST loan-acct WHERE loan-acct.contract = loan.contract
        AND loan-acct.cont-code = loan.cont-code
        AND loan-acct.acct-type = "loan-dps-tr"
        NO-LOCK NO-ERROR.
        IF AVAIL loan-acct THEN DO:
            acctTr = loan-acct.acct.
        END.
        
        /*   ��� % 474   */
        FIND FIRST loan-acct WHERE loan-acct.contract = loan.contract
        AND loan-acct.cont-code = loan.cont-code
        AND loan-acct.acct-type = "loan-dps-int"
        NO-LOCK NO-ERROR.
        IF AVAIL loan-acct THEN DO:
            acct474 = loan-acct.acct.
        END.
        
        /*   ��� % 706   */
        FIND FIRST loan-acct WHERE loan-acct.contract = loan.contract
        AND loan-acct.cont-code = loan.cont-code
        AND loan-acct.acct-type = "loan-expens"
        NO-LOCK NO-ERROR.
        IF AVAIL loan-acct THEN DO:
            acct706 = loan-acct.acct.
        END.
        
        /*   ��� �������   */
        FIND FIRST loan-acct WHERE loan-acct.contract = loan.contract
            AND loan-acct.cont-code = loan.cont-code
            AND loan-acct.acct-type = "loan-dps-t"
            NO-LOCK NO-ERROR.
        IF AVAIL loan-acct THEN DO:
            FIND FIRST acct OF loan-acct NO-LOCK NO-ERROR.
            IF AVAIL acct THEN DO:
                acct42 = acct.acct.
                ostatok = 0.
                lastDate = loan.open-date.
                dateKap = lastDate.

                /*ᬮ�ਬ ���㬥��� �� �᭮����� ���� ������
                ���।��塞 �� ����樨 �� �᭮����� ���� ������ � ࠧ���뢠�� �� �� ���*/                                
                FOR EACH op-entry WHERE 
                        (op-entry.op-status = "�" OR op-entry.op-status = "��")
                        AND (op-entry.acct-db = acct.acct OR op-entry.acct-cr = acct.acct),
                FIRST op OF op-entry WHERE op.contract-date >= lastDate 
                        AND op.contract-date <= dateOp
                NO-LOCK BY op.contract-date:
                    amt_rub_cur = 0.
                    dateKap = op.contract-date.
                    notIndivid = TRUE.
                    
                    IF (op-entry.acct-db = acct.acct) THEN DO: /*�᫨ ����⨥ � ������*/
                         IF SUBSTRING(op-entry.acct-cr,1,3) <> '706' THEN  DO:  /* �� ��⠥� ��ࠢ�⥫�� �஢���� ������ % */
                            IF TRIM(loan.currency) = '' THEN DO:
                                ostatok  = ostatok - ABS(op-entry.amt-rub). 
                                amt_rub_cur = ABS(op-entry.amt-rub).
                            END.
                            ELSE DO:
                                IF op-entry.amt-cur = 0 THEN notIndivid = FALSE.
                                ostatok  = ostatok - abs(op-entry.amt-cur).
                                amt_rub_cur = abs(op-entry.amt-cur).
                            END.
                            snatie = TRUE.
                        END.
                    END.
                    ELSE DO: /*�᫨ �����ᥭ�� ��� ���᫥��� �� �����*/
                        IF (op-entry.acct-db = acct474) THEN DO:
                            prich474_42 = TRUE.
                            /*TODO ��� �筮�� ���� �������� ����� ����� - 4 ���*/
                            IF DAY(dateKap) > 24 THEN DO: /* �᫨ � ���� �����, � ᪮॥ �ᥣ� ����. */
                                dateKap = DATE_CORRECT(MONTH(dateKap),0,31,YEAR(dateKap)). /*�����頥� ��᫥���� ���� �����*/
                                amt_rub_cur = 0.
                            END.
                        END.
                        ELSE DO:
                            IF loan.currency = '' THEN DO:
                                ostatok  = ostatok + abs(op-entry.amt-rub).
                                amt_rub_cur = abs(op-entry.amt-rub).
                            END.
                            ELSE DO:
                            IF op-entry.amt-cur = 0 THEN notIndivid = FALSE.
                            ostatok  = ostatok + ABS(op-entry.amt-cur).
                            amt_rub_cur = ABS(op-entry.amt-cur).
                            END.
                        END.
                        snatie = FALSE.
                    END.
                 
                    IF notIndivid THEN DO:  /* �᫨ �㬬� �஢���� �� 0 */
                        FIND FIRST tt-dviacct WHERE tt-dviacct.cont-code = loan.cont-code
                            AND tt-dviacct.cont-type = loan.cont-type
                            AND tt-dviacct.posDate = dateKap
                            NO-ERROR.
                        IF AVAIL tt-dviacct THEN DO: /*�᫨ 㦥 ᮧ������ ����� � ⠡��� �� �⮬� �������� �� ������� ���भ�*/
                            IF snatie AND NOT tt-dviacct.debet THEN DO: 
                                amt_rub_cur = tt-dviacct.amt_rub_cur - amt_rub_cur.
                                IF amt_rub_cur < 0 THEN tt-dviacct.debet = TRUE. /*�������� ���塞 䫠� ������*/
                                tt-dviacct.amt_rub_cur = abs(amt_rub_cur).
                            END.
                            ELSE DO:
                                IF (snatie EQ tt-dviacct.debet) THEN DO:
                                    tt-dviacct.amt_rub_cur = tt-dviacct.amt_rub_cur + amt_rub_cur.
                                END.
                                ELSE DO:
                                    IF NOT snatie AND tt-dviacct.debet THEN DO:
                                        amt_rub_cur = amt_rub_cur - tt-dviacct.amt_rub_cur.
                                        IF amt_rub_cur > 0 THEN tt-dviacct.debet = FALSE.
                                        tt-dviacct.amt_rub_cur = ABS(amt_rub_cur).
                                    END.
                                END.
                            END.
                            tt-dviacct.ostatokAcct-Proc = ostatok.
                            RUN my_acct_pos(INPUT acct42,INPUT loan.open-date,INPUT dateKap,INPUT loan.currency,OUTPUT tmpOstatok).
                            tt-dviacct.ostatokAcct = tmpOstatok.
  
                        END.
                        ELSE DO:
                        RUN my_acct_pos(INPUT acct42,INPUT loan.open-date,INPUT dateKap,INPUT loan.currency,OUTPUT lastOstatok).

                            CREATE tt-dviacct.
                            ASSIGN 
                            tt-dviacct.cont-code = loan.cont-code
                            tt-dviacct.cont-type = loan.cont-type
                            tt-dviacct.ostatokAcct-Proc = ostatok
                            tt-dviacct.ostatokAcct = lastOstatok
                            tt-dviacct.cur = loan.currency
                            tt-dviacct.acct = acct42
                            tt-dviacct.posDate = dateKap
                            tt-dviacct.debet = snatie
                            tt-dviacct.amt_rub_cur = amt_rub_cur
                            .
                        END.
                    END.
                END. /*FOR EACH op-entry*/

                /*��� ���� �� ����⭮ �� �ந�室��... ��祬-� ᮧ��� ������ � ᥣ����譥� ��� �� ���� �㡫�
                ����� ����� �� ���������?*/
                IF dateKap < dateOp THEN DO:
                        RUN my_acct_pos(INPUT acct42,INPUT loan.open-date,INPUT dateOp,INPUT loan.currency,OUTPUT lastOstatok).
 
                        CREATE tt-dviacct.
                        ASSIGN 
                        tt-dviacct.cont-code = loan.cont-code
                        tt-dviacct.cont-type = loan.cont-type
                        tt-dviacct.ostatokAcct-Proc = ostatok
                        tt-dviacct.ostatokAcct = lastOstatok
                        tt-dviacct.cur = loan.currency
                        tt-dviacct.acct = acct42
                        tt-dviacct.posDate = dateOp
                        tt-dviacct.debet = TRUE
                        tt-dviacct.amt_rub_cur = 1
                        .
                END.
                
                /* ࠧ������ �� ��ਮ�� ���᫥��� % */
                tempDate = DATE ( MONTH(loan.open-date) , 1 , YEAR(loan.open-date) ).
                DO WHILE tempDate <= dateOp:
                    dateKap = DATE_CORRECT(MONTH(tempDate),0,31,YEAR(tempDate)).
                    /*ᬥ頥� �� �����++*/
                    tempDate = DATE_CORRECT(MONTH(tempDate),1,1,YEAR(tempDate)).
                     
                    IF dateKap <= dateOp AND (dateKap <= loan.close-date OR loan.close-date = ?) THEN DO:
                        tempDate2 = loan.open-date - 1.

                        /*�஡����� �� ᮧ������ ⠡��� �饬 ���� ����⠫���樨 ��業⮢ ᮮ⢥�����騩 dateKap*/
                        FOR EACH tt-dviacct WHERE tt-dviacct.posDate <= dateKap BY tt-dviacct.posDate DESC:
                            IF tt-dviacct.posDate > tempDate2 THEN DO:
                                tempDate2 = tt-dviacct.posDate.
                                lastOstatok = tt-dviacct.ostatokAcct.
                                lastOstatokRasch = tt-dviacct.ostatokAcct-Proc.
                                myCur = tt-dviacct.cur.
                                IF tempDate2 EQ dateKap THEN tt-dviacct.nachProc = TRUE.  
                                LEAVE.
                            END.
                        END.
                        /*�᫨ ���-⠪� ��諨 ���祭�� � ⠡��� tt-dviacct �� ��� �� ��������� ���᫥����
                        ���� ��� ������ ���� ���᫥���... �� ��� �뫮 �� ���� �㡫��*/
                        IF tempDate2 >= (loan.open-date - 1) AND tempDate2 <> dateKap THEN DO:
                            CREATE tt-dviacct.
                            ASSIGN 
                            tt-dviacct.cont-code = loan.cont-code
                            tt-dviacct.cont-type = loan.cont-type
                            tt-dviacct.ostatokAcct = lastOstatok
                            tt-dviacct.ostatokAcct-Proc = lastOstatokRasch
                            tt-dviacct.cur = myCur
                            tt-dviacct.acct = acct42
                            tt-dviacct.posDate = dateKap
                            tt-dviacct.debet = FALSE
                            tt-dviacct.nachProc = TRUE
                            tt-dviacct.amt_rub_cur = 0
                            .
                        END.
                    END.
                END.

                dateKap = loan.open-date.
                /* ��।������ ���� ��ࢮ� ����⠫���樨 */
                IF acct474 <> '' THEN DO:
                    FIND FIRST op-entry WHERE (op-entry.op-status = "�" OR op-entry.op-status = "��")
                            AND op-entry.acct-db = acct474
                            AND (op-entry.acct-cr EQ acctTr OR op-entry.acct-cr EQ acct42)
                /*FIX ���� 㡥�� �� ��-����� �।��騩 ��ਠ�� ���� AND (SUBSTRING(op-entry.acct-cr,1,2) = '40' OR SUBSTRING(op-entry.acct-cr,1,2) = '42')*/
                            AND op-entry.op-date > loan.open-date 
                            AND op-entry.op-date <= dateOp
                    NO-LOCK NO-ERROR.
                    IF AVAIL op-entry THEN DO:
                        dateKap = op-entry.op-date.
                    END.
                END.
                /*�� ������ �� ��直� ��砩*/
                IF dateKap EQ loan.open-date THEN DO:
                    dateKap = DATE_CORRECT(MONTH(dateKap),1,31,YEAR(dateKap)).
                END.
                
                IF acct706 <> '' AND acct474 <> '' THEN DO:
                    /*��諨 �� ���㬥�⠬ ���᫥��� ��業⮢*/
                    FOR EACH op-entry WHERE 
                        (op-entry.op-status = "�" OR op-entry.op-status = "��")
                        AND op-entry.acct-cr = acct474,
                        FIRST op OF op-entry WHERE
                        op.contract-date >= loan.open-date AND op.contract-date <= dateOp
                        NO-LOCK:
                        IF loan.currency = '' THEN DO:
                            summProc = summProc + abs(op-entry.amt-rub).
                            IF op.contract-date < dateCurrYear THEN
                                dPrevReal = dPrevReal + abs(op-entry.amt-rub).
                            ELSE
                                dCurrReal = dCurrReal + abs(op-entry.amt-rub).
                        END.
                        ELSE DO:
                            summProc = summProc + abs(op-entry.amt-cur).
                            IF op.contract-date < dateCurrYear THEN
                                dPrevReal = dPrevReal + abs(op-entry.amt-cur).
                            ELSE
                                dCurrReal = dCurrReal + abs(op-entry.amt-cur).
                        END.
                    END. /*FOR EACH op-entry*/
                    
                    /* 09/09/2013 ���뢠�� ��ࠢ�⥫�� �஢���� ����譥 ���� */
                    FOR EACH op-entry WHERE 
                        (op-entry.op-status = "�" OR op-entry.op-status = "��")
                        AND op-entry.acct-cr = acct706
                        AND (op-entry.acct-db = acct474 OR op-entry.acct-db = acct42),
                        FIRST op OF op-entry WHERE
                        op.contract-date >= loan.open-date AND op.contract-date <= dateOp
                        NO-LOCK:
                        IF loan.currency = '' THEN DO:
                            summProc = summProc - abs(op-entry.amt-rub).
                            IF op.contract-date < dateCurrYear THEN
                                dPrevReal = dPrevReal - abs(op-entry.amt-rub).
                            ELSE
                                dCurrReal = dCurrReal - abs(op-entry.amt-rub).
                        END.
                        ELSE DO:
                            summProc = summProc - abs(op-entry.amt-cur).
                            IF op.contract-date < dateCurrYear THEN
                                dPrevReal = dPrevReal - abs(op-entry.amt-cur).
                            ELSE
                                dCurrReal = dCurrReal - abs(op-entry.amt-cur).
                        END.
                    END. /*FOR EACH op-entry*/
                    
                END.
                
                /* ��室�� ��ࢮ� ���䭮� ����⨥ */
                IF IsShtrIzVkl(RECID(loan), dateOp, acct42) 
                THEN dateFirstSnatie =  dateOp. /*��� ��� �� ���䭮�*/
                
                FOR EACH tt-dviacct WHERE tt-dviacct.posDate <= dateOp 
                        AND ( tt-dviacct.posDate < loan.close-date  OR loan.close-date = ? )
                        AND tt-dviacct.debet 
                NO-LOCK BY tt-dviacct.posDate:
                        IF IsShtrIzVkl(RECID(loan), tt-dviacct.posDate, acct42) 
                        THEN DO:
                            dateFirstSnatie = tt-dviacct.posDate. /*��� ��� �� ���䭮�*/
                            LEAVE.
                        END.
                END.

                periodSnatie = dateFirstSnatie - loan.open-date.
                
                /* ��� �������, � ������ �� �� ��⨨ �㤥� ���䭠� �⠢�� */
                IF LOOKUP(loan.cont-type,listContType_str) > 0  
                    AND dateFirstSnatie > loan.open-date THEN DO:
                    shtraf = 1.
                END.
                
                /* ��� �������, � ������ �� ��⨨ �� ��ࢮ� ����⠫���樨 �㤥� ���䭠� �⠢�� (����) */
                IF LOOKUP(loan.cont-type,listContType_str01) > 0  
                    AND dateFirstSnatie > loan.open-date 
                    AND dateFirstSnatie < dateKap THEN DO:
                    shtraf = 2.
                END.
                
                /* ��� �������, � ������ ���䭠� �⠢�� ������ �� ���� ���� */
                IF LOOKUP(loan.cont-type,listContType_invest) > 0  
                    AND dateFirstSnatie > loan.open-date THEN DO:
                    shtraf = 3.
                END.

                /* ��� ⨯�� �������, � ������ �� ��⨨ �� 1 ����⠫���樨 - �⠢�� �㤥� 0.1%, ����� ���䭠� �⠢�� ������ �� ���� ���� */
                IF LOOKUP(loan.cont-type,listContType_gold) > 0  
                    AND dateFirstSnatie > loan.open-date THEN DO:
                    IF dateFirstSnatie < dateKap THEN shtraf = 2.
                    ELSE shtraf = 4.
                END.    

                /*���ᨮ��� */
                IF LOOKUP(loan.cont-type,listContType_hitr2) > 0
                    AND dateFirstSnatie > loan.open-date THEN DO:
                    IF dateFirstSnatie < dateKap THEN shtraf = 2.
                    ELSE shtraf = 4.
                END.
                
                /* ��।��塞 �⠢�� */
                RUN Get_Last_Param IN h_dpspc (RECID(loan),
                                loan.open-date,
                                loan.open-date,
                                "commission",
                                OUTPUT osnComm).  /* ��� �᭮���� �����ᨨ */
                
                RUN Get_Last_Param IN h_dpspc (RECID(loan),
                                loan.open-date,
                                loan.open-date,
                                "pen-commi",
                                OUTPUT shtrComm). /* ��� ���䭮� �����ᨨ */

                {empty tt-comm-rate}
                /* �᭮���� */
                notIndivid = TRUE. /*� ��� ��� �� ���塞 ��� �⮣� 䫠��*/
                /*�饬 �������㠫��� �᭮���� �⠢�� ��� ���*/
                FOR EACH comm-rate WHERE comm-rate.commission EQ osnComm
                    AND comm-rate.since <= loan.open-date
                    AND comm-rate.acct EQ acct42
                    AND comm-rate.currency EQ loan.currency
                    AND comm-rate.min-value < firstOstatok
                    NO-LOCK
                    BREAK BY comm-rate.period BY comm-rate.min-value BY comm-rate.since DESC:
                    IF LAST-OF (comm-rate.since) AND LAST-OF (comm-rate.min-value) AND FIRST-OF(comm-rate.period) THEN DO:
                        notIndivid = FALSE.
                        CREATE tt-comm-rate.
                        ASSIGN 
                        tt-comm-rate.rate-comm = comm-rate.rate-comm
                        tt-comm-rate.period = comm-rate.period
                        tt-comm-rate.shtraf = 0
                        .
                    END.
                END.
                IF notIndivid THEN DO:
                    /*�饬 �᭮���� �⠢�� ��� ������*/
                    FOR EACH comm-rate WHERE comm-rate.commission EQ osnComm
                        AND comm-rate.since <= loan.open-date
                        AND comm-rate.acct EQ '0'
                        AND comm-rate.currency EQ loan.currency
                        NO-LOCK
                        BREAK BY comm-rate.period BY comm-rate.since DESCENDING:
                        IF LAST-OF (comm-rate.since) AND FIRST-OF(comm-rate.period) THEN DO:
                            CREATE tt-comm-rate.
                            ASSIGN 
                            tt-comm-rate.rate-comm = comm-rate.rate-comm
                            tt-comm-rate.period = comm-rate.period
                            tt-comm-rate.shtraf = 0
                            .
                        END.
                    END.
                END.
                
                /* ���䭠� */
                notIndivid = TRUE.
                /*�饬 ������ �������㠫��� �⠢��*/
                FOR EACH comm-rate WHERE comm-rate.commission EQ shtrComm
                    AND comm-rate.since <= loan.open-date
                    AND comm-rate.acct EQ acct42
                    AND comm-rate.currency EQ loan.currency
                    NO-LOCK
                    BREAK BY comm-rate.period BY comm-rate.since DESCENDING:
                    IF LAST-OF (comm-rate.since) AND FIRST-OF(comm-rate.period) THEN DO:
                        notIndivid = FALSE.
                        CREATE tt-comm-rate.
                        ASSIGN 
                        tt-comm-rate.rate-comm = comm-rate.rate-comm
                        tt-comm-rate.period = comm-rate.period
                        tt-comm-rate.shtraf = 1
                        .
                    END.
                END.
                IF notIndivid THEN DO:
                    /*�饬 ������ �⠢��*/
                    FOR EACH comm-rate WHERE comm-rate.commission EQ shtrComm
                        AND comm-rate.since <= loan.open-date
                        AND comm-rate.acct EQ '0'
                        AND comm-rate.currency EQ loan.currency
                        NO-LOCK
                        BREAK BY comm-rate.period BY comm-rate.since DESCENDING:
                        IF LAST-OF (comm-rate.since) AND FIRST-OF(comm-rate.period) THEN DO:
                            CREATE tt-comm-rate.
                            ASSIGN 
                            tt-comm-rate.rate-comm = comm-rate.rate-comm
                            tt-comm-rate.period = comm-rate.period
                            tt-comm-rate.shtraf = 1
                            .
                        END.
                    END.
                END.
                
                /* ���䭠� ���� (0.1) */
                notIndivid = TRUE.
                /*�饬 ���� �������㠫��� �⠢��*/
                FOR EACH comm-rate WHERE comm-rate.commission = "����"
                    AND comm-rate.acct EQ acct42
                    AND comm-rate.currency EQ loan.currency
                    NO-LOCK BY comm-rate.since:
                        notIndivid = FALSE.
                        CREATE tt-comm-rate.
                        ASSIGN 
                        tt-comm-rate.rate-comm = comm-rate.rate-comm
                        tt-comm-rate.period = comm-rate.since - loan.open-date
                        tt-comm-rate.shtraf = 2
                        .
                END.
                IF notIndivid THEN DO:
                    /*�饬 ���� �⠢��*/
                    FOR EACH comm-rate WHERE comm-rate.commission EQ "����"
                        AND comm-rate.since <= loan.open-date
                        AND comm-rate.acct EQ '0'
                        AND comm-rate.currency EQ loan.currency
                        NO-LOCK
                        BREAK BY comm-rate.period BY comm-rate.since DESCENDING:
                        IF LAST-OF (comm-rate.since) AND FIRST-OF(comm-rate.period) THEN DO:
                            CREATE tt-comm-rate.
                            ASSIGN 
                            tt-comm-rate.rate-comm = comm-rate.rate-comm
                            tt-comm-rate.period = comm-rate.period
                            tt-comm-rate.shtraf = 2
                            .
                        END.
                    END.
                END.
                
                /* �������⥫쭮 ࠧ������, �᫨ ��������� �⠢�� �� �६� ����� ������ */
                FOR EACH tt-comm-rate NO-LOCK BY tt-comm-rate.period:
                    tempDate = loan.open-date + tt-comm-rate.period - 1.
                    IF tt-comm-rate.period > 0 AND tempDate <= dateOp 
                        AND (tempDate < loan.close-date OR loan.close-date EQ ?) 
                    THEN DO:
                        notIndivid = FALSE. /*����� ����� �ᯮ��㥬 ��� ��㣨� 楫�� ��� 䫠�*/
                        /*��室�� ��᫥���� ������ ��� ��� �⠢�� �� ��� ��ਮ��� ����� 0*/
                        FOR EACH tt-dviacct WHERE tt-dviacct.posDate <= tempDate 
                        NO-LOCK BY tt-dviacct.posDate DESC:
                            IF tt-dviacct.posDate < tempDate 
                            THEN notIndivid = TRUE.
                            ELSE notIndivid = FALSE.
                            lastOstatok = tt-dviacct.ostatokAcct.
                            lastOstatokRasch = tt-dviacct.ostatokAcct-Proc.
                            myCur = tt-dviacct.cur.
                            LEAVE.
                        END.
                        IF notIndivid THEN DO: /*�᫨ ������ �� � ���� ����砭�� ��ਮ��*/
                        /*� ᮧ��� �㫥��� ������ ��� ࠧ������� ����� ��ਮ���� ��⮩ ����砭�� ��ਮ��*/
                            CREATE tt-dviacct.
                            ASSIGN 
                            tt-dviacct.cont-code = loan.cont-code
                            tt-dviacct.cont-type = loan.cont-type
                            tt-dviacct.ostatokAcct = lastOstatok
                            tt-dviacct.ostatokAcct-Proc = lastOstatokRasch
                            tt-dviacct.cur = myCur
                            tt-dviacct.acct = acct42
                            tt-dviacct.posDate = tempDate
                            tt-dviacct.debet = FALSE
                            tt-dviacct.amt_rub_cur = 0
                            .
                        END.
                    END.
                END. /*FOR EACH tt-comm-rate*/
                
                stavkaShtraf = 0.   
                                
                CASE shtraf: 
                    /* ��� �������, � ������ �� �� ��⨨ �㤥� ���䭠� �⠢�� */
                    WHEN 1 THEN DO:
                        FIND FIRST tt-comm-rate WHERE tt-comm-rate.shtraf EQ 1 NO-LOCK NO-ERROR.
                        IF AVAIL tt-comm-rate 
                        THEN DO:
                            perShtraf = 0.
                            stavkaShtraf = tt-comm-rate.rate-comm.
                        END.
                    END. 

                    /* ��� �������, � ������ �� ��⨨ �� ��ࢮ� ����⠫���樨 �㤥� ���䭠� �⠢�� (����) */
                    WHEN 2 THEN DO:
                        isVost = TRUE.
                    END.
                     /*TODO ࠧ������� ��� ������ ��묠���� ������ invest*/
                    /* ��� �������, � ������ ���䭠� �⠢�� ������ �� ���� ���� */
                    WHEN 3 THEN DO:
                        FOR EACH tt-comm-rate WHERE tt-comm-rate.shtraf EQ 1 
                                AND tt-comm-rate.period <= periodSnatie 
                        NO-LOCK BY tt-comm-rate.period DESC:
                            stavkaShtraf = tt-comm-rate.rate-comm.
                            LEAVE.
                        END.
                        /*�� ������ ���� �������쭮�� ���⪠ ������ �� ��祬�-� 10 �����*/
                        FOR EACH tt-dviacct WHERE tt-dviacct.cont-code EQ loan.cont-code 
                                AND tt-dviacct.debet
                                AND tt-dviacct.posDate > loan.open-date
                                AND (tt-dviacct.posDate < loan.close-date OR loan.close-date EQ ?) 
                        NO-LOCK BY tt-dviacct.posDate:
                            IF tt-dviacct.ostatokAcct < 10000 THEN DO:
                                isVost = TRUE.
                            END.
                        END.
                    END. 
                    
                    /* ��� ⨯�� �������, � ������ �� ��⨨ �� 1 ����⠫���樨 - �⠢�� �㤥� 0.1%, ����� ���䭠� �⠢�� ������ �� ���� ���� */
                    WHEN 4 THEN DO:
                        FOR EACH tt-comm-rate WHERE tt-comm-rate.shtraf EQ 1 
                                AND tt-comm-rate.period <= periodSnatie 
                        NO-LOCK BY tt-comm-rate.period DESC:
                            perShtraf = tt-comm-rate.period.
                            stavkaShtraf = tt-comm-rate.rate-comm.
                            LEAVE.
                        END.
                    END. 
                END.

                /* ������塞 �६����� ⠡���� tt-stavka */
                pPeriod = 0.
                fullPeriod = 0.
                stavkaOsnovn = 0.
                lastOstatokRasch = 0.
                
                FOR EACH tt-dviacct WHERE tt-dviacct.cont-code EQ loan.cont-code 
                        AND (tt-dviacct.posDate <= loan.close-date OR loan.close-date EQ ?) NO-LOCK 
                BY tt-dviacct.posDate:
                    /*᪮॥ �ᥣ� ⮫쪮 �᫨ ��ࢠ� ������ �� ������ 
                    � ���� ������ ��ࢮ� ���ᥭ�� �।�� �� �����*/
                    IF fullPeriod EQ 0 THEN DO:
                        fullPeriod = 1.
                        lastDate = tt-dviacct.posDate.
                        tempDate = DATE_CORRECT(2,0,31,YEAR(tt-dviacct.posDate)). 
                        IF DAY(tempDate) > 28 THEN daysInYear = 36600.
                        ELSE daysInYear = 36500.
                        CREATE tt-stavka.
                            ASSIGN
                                tt-stavka.cont-code = loan.cont-code
                                tt-stavka.cont-type = loan.cont-type
                                tt-stavka.ostatokAcct = tt-dviacct.ostatokAcct
                                tt-stavka.ostatokAcctRasch = tt-dviacct.ostatokAcct-Proc
                                tt-stavka.acct = acct42
                                tt-stavka.startDate = lastDate
                                tt-stavka.endDate = tt-dviacct.posDate
                                tt-stavka.closeDate = loan.close-date
                                tt-stavka.period = pPeriod
                                tt-stavka.cur = loan.currency
                                tt-stavka.stavkaOsn = 0
                                tt-stavka.stavkaShtr = 0
                                tt-stavka.procRasch = 0
                                tt-stavka.shtraf = shtraf
                                tt-stavka.daysInYear = daysInYear
                                tt-stavka.fullPeriod = 0
                                tt-stavka.nachProc = tt-dviacct.nachProc
                                .
                            lastOstatokRasch = tt-dviacct.ostatokAcct-Proc.
                    END.
                    ELSE DO:
                        fullPeriod = tt-dviacct.posDate - loan.open-date.
                        pPeriod = tt-dviacct.posDate - lastDate .
                        
                        /*��室�� ��業� �᭮���� �⠢�� ������*/
                        FOR EACH tt-comm-rate WHERE tt-comm-rate.shtraf = 0 
                                AND tt-comm-rate.period <= fullPeriod 
                        NO-LOCK BY tt-comm-rate.period DESC:
                            stavkaOsnovn = tt-comm-rate.rate-comm.
                            LEAVE.
                        END.
                        /*�᫨ � ������ ����室��� ����� �⠢�� ��. ������塞 ������ �⠢��*/
                        IF isVost THEN DO:
                        FOR EACH tt-comm-rate WHERE tt-comm-rate.shtraf = 2 
                                AND tt-comm-rate.period <= fullPeriod 
                        NO-LOCK BY tt-comm-rate.period DESC:
                                stavkaShtraf = tt-comm-rate.rate-comm.
                                LEAVE.
                            END.
                        END.

                        tempDate = DATE_CORRECT(2,0,31,YEAR(tt-dviacct.posDate)). 
                        IF DAY(tempDate) > 28 THEN daysInYear = 36600.
                        ELSE daysInYear = 36500.
                        
                        procRaschAdd = TRUE.
                        
                        IF shtraf > 0 THEN DO:
                            IF shtraf EQ 4 THEN DO:
                                FOR EACH tt-comm-rate WHERE tt-comm-rate.shtraf EQ 1
                                        AND tt-comm-rate.period <= fullperiod 
                                NO-LOCK BY tt-comm-rate.period DESC:
                                    stavkaShtraf = tt-comm-rate.rate-comm.
                                    LEAVE.
                                END.
                            END.
                            
                            IF fullPeriod >= perShtraf
                            THEN raschProc = (lastOstatokRasch * stavkaShtraf * pPeriod) / DEC(daysInYear).
                            ELSE raschProc = (lastOstatokRasch * stavkaOsnovn * pPeriod) / DEC(daysInYear).
                            
                            IF lastOstatokRasch < 0 THEN raschProc = 0.
                        
                            /*��� ᫥���饣� ��㣠 ����뢠�� ���⮪ ������*/    
                            IF tt-dviacct.debet 
                            THEN lastOstatokRasch = lastOstatokRasch - tt-dviacct.amt_rub_cur.
                            ELSE lastOstatokRasch = lastOstatokRasch + tt-dviacct.amt_rub_cur.
                                
                            /*�᫨ � ������ ���� ���᫥��� � �� ��� ���᫥��� ��業⮢. 
                            � 㦥 ��᫥ ��ࢮ� ����⠫���樨 � �� �ਡ���� ���⠭�� ���� ��業��*/
                            IF prich474_42 AND tt-dviacct.nachProc AND tt-dviacct.posDate >= dateKap  
                            THEN DO:
                                /*���᫨� ���⠭�� ���� ��業�� �� ��� ���*/
                                lastOstatokRasch = lastOstatokRasch + raschProc.
                                /*�஡����� �� ᮧ����� ���⠬ ��業⮢ ����� ��� �� ���뢠�� � �����*/
                                FOR EACH tt-stavka WHERE tt-stavka.cont-code = loan.cont-code 
                                        AND tt-stavka.cont-type = loan.cont-type 
                                        AND tt-stavka.procRaschAdd 
                                NO-LOCK:
                                    lastOstatokRasch = lastOstatokRasch + tt-stavka.procRasch. 
                                    tt-stavka.procRaschAdd = FALSE. /*�⬥砥� �� ����� ���㬥�� 㦥 ���⢮��� � ����*/
                                END. 
                                procRaschAdd = FALSE. /*�⬥砥� �� ����� ���㬥�� 㦥 ���⢮��� � ����*/
                            END.
                        END. /*IF shtraf > 0*/
                        
                        ELSE DO: /*��� ���� ��業⮢ �᫨ ���䭮�� ������ �� �뫮*/
                            raschProc = ROUND((lastOstatokRasch * stavkaOsnovn * pPeriod) / DEC(daysInYear),2).
                            IF lastOstatokRasch < 0 THEN raschProc = 0.
                            IF prich474_42 AND tt-dviacct.nachProc 
                                    AND NOT (
                                                YEAR(loan.open-date) EQ YEAR(tt-dviacct.posDate) 
                                            AND MONTH(loan.open-date) EQ MONTH(tt-dviacct.posDate)
                                            )
                            THEN DO:
                                IF tt-dviacct.debet 
                                THEN lastOstatokRasch = lastOstatokRasch - tt-dviacct.amt_rub_cur + raschProc.
                                ELSE lastOstatokRasch = lastOstatokRasch + tt-dviacct.amt_rub_cur + raschProc.
                                
                                FOR EACH tt-stavka WHERE tt-stavka.cont-code = loan.cont-code 
                                        AND tt-stavka.cont-type = loan.cont-type 
                                        AND tt-stavka.procRaschAdd 
                                NO-LOCK:
                                    lastOstatokRasch = lastOstatokRasch + tt-stavka.procRasch.
                                    tt-stavka.procRaschAdd = FALSE.
                                END. 
                                procRaschAdd = FALSE.
                            END.
                            ELSE DO:
                                IF tt-dviacct.debet THEN lastOstatokRasch = lastOstatokRasch - tt-dviacct.amt_rub_cur.
                                    ELSE lastOstatokRasch = lastOstatokRasch + tt-dviacct.amt_rub_cur.
                            END.
                        END.
                        
                        CREATE tt-stavka.
                        ASSIGN
                            tt-stavka.cont-code = loan.cont-code
                            tt-stavka.cont-type = loan.cont-type
                            tt-stavka.ostatokAcct = tt-dviacct.ostatokAcct
                            tt-stavka.ostatokAcctRasch = lastOstatokRasch
                            tt-stavka.acct = acct42
                            tt-stavka.startDate = lastDate
                            tt-stavka.endDate = tt-dviacct.posDate
                            tt-stavka.closeDate = loan.close-date
                            tt-stavka.period = pPeriod
                            tt-stavka.cur = loan.currency
                            tt-stavka.stavkaOsn = stavkaOsnovn
                            tt-stavka.stavkaShtr = stavkaShtraf
                            tt-stavka.procRasch = raschProc
                            tt-stavka.procRaschAdd  = procRaschAdd
                            tt-stavka.shtraf = shtraf
                            tt-stavka.summProc = summProc
                            tt-stavka.daysInYear = daysInYear
                            tt-stavka.fullPeriod = fullPeriod
                            .
                        lastDate = tt-dviacct.posDate.
                    END.
                END. /*FOR EACH tt-dviacct*/
            END. /* IF AVAIL acct THEN DO: */
            
            /*� �� �뫮 � �।��騥 ����*/ 
            FOR EACH tt-stavka WHERE tt-stavka.endDate < dateCurrYear AND tt-stavka.endDate <= dateLastNach NO-LOCK:
                dPrev = dPrev + tt-stavka.procRasch.
            END. /* FOR EACH tt-stavka */
            
            /*� �⮬ ����*/
            FOR EACH tt-stavka WHERE tt-stavka.endDate >= dateCurrYear AND tt-stavka.endDate <= dateLastNach NO-LOCK:
                dCurr = dCurr + tt-stavka.procRasch.
            END. /* FOR EACH tt-stavka */
            
        END. /* IF AVAIL loan-acct */
      END. /* IF countdps_t = 1 THEN DO: */
      ELSE DO:
        MESSAGE '����� ������ ��筮�� ���, �������� �訡�� � ���� ��業⮢' VIEW-AS ALERT-BOX.
      END.
    END. /* IF LOOKUP */
    ELSE DO:
        MESSAGE '����� ���ॢ訩 �ॡ���� ��筠� �஢�ઠ ��業⮢ ' + STRING(loan.cont-type) VIEW-AS ALERT-BOX.
    END.
END. /* IF AVAIL loan */

    {intrface.del}






