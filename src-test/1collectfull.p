/*
	���� ��� ��ࢮ�� ��������᪮��
	kam
*/

{globals.i}
{tmprecid.def}
{sh-defs.i}
{ksh-defs.i NEW}
{intrface.get xclass}
{intrface.get comm}
{intrface.get i254}
{intrface.get blkob}

DEFINE TEMP-TABLE otch
        FIELD fio          AS CHAR                 /* �������, ���, ����⢮ �������� */
        FIELD region       AS CHAR                 /* ������ �������� */
        FIELD birthday     AS DATE                 /* ��� ஦����� �������� */
        FIELD regionbirth  AS CHAR                 /* ���� ஦����� �������� */
        FIELD sernompasp   AS CHAR                 /* ��� ����� ��ᯮ�� �������� */
        FIELD datepasp     AS DATE                 /* ��� �뤠� ��ᯮ�� �������� */        
        FIELD whopasp      AS CHAR                 /* �࣠� �뤠� ��ᯮ�� �������� */
        FIELD gender       AS CHAR                 /* ��� �������� */
        FIELD inn          AS CHAR                 /* ��� �������� */
        FIELD adrfact      AS CHAR                 /* ���� 䠪��᪮�� �஦������ ��������  */      
        FIELD adrreg       AS CHAR                 /* ���� ॣ����樨 �������� */
        FIELD teldom       AS CHAR                 /* ����䮭 ����譨� �������� (�� ����稨) */
        FIELD telmob       AS CHAR                 /* ����䮭 ������� �������� (�� ����稨) */        
        FIELD telrab       AS CHAR                 /* ����䮭 ࠡ�稩 �������� (�� ����稨) */         
        FIELD cont_code    AS CHAR                 /* ����� �� */        
        FIELD open_date    AS DATE                 /* ��� �।�� */
        FIELD open_date2   AS DATE                 /* ��� �।��⠢����� �।�� */
        FIELD typeprod     AS CHAR                 /* ��� �த�� (��� ������: ������, ����, ���� � �.�.) */
        FIELD sumcred      AS DECIMAL              /* �㬬� �।�� */
        FIELD currency     AS CHAR                 /* ����� �� */
        FIELD plan_date    AS DATE                 /* �������� ��� ������ �।�� (��� ������������� ������������) */
        FIELD srok_cred    AS INT                  /* �ப �।�� (� ����) */
        FIELD proc_st      AS DECIMAL              /* ��業⭠� �⠢�� */        
        FIELD sumpogod     AS DECIMAL              /* �㬬� ����襭���� �᭮����� ����� �� ���� ��।�� */
        FIELD sumpogproc   AS DECIMAL              /* �㬬� ����襭��� ��業⮢ �� ���� ��।�� */
        FIELD sumpogfull   AS DECIMAL              /* ���� ����襭��� �㬬� �� ���� ��।�� */        
        FIELD datelastpog  AS DATE                 /* ��� ��᫥����� ����襭�� */        
        FIELD sumlastpog   AS DECIMAL              /* �㬬� ��᫥����� ���⥦� */         
        FIELD sumod        AS DECIMAL              /* �㬬� ������������ �� �᭮����� ����� �� ���� ��।��  */
        FIELD sumproc      AS DECIMAL              /* �㬬� ������������ �� ��業⠬ �� ���� ��।�� */        
        FIELD sumpeni      AS DECIMAL              /* �㬬� ������������ �� ���䠬 (����, ����⮩��) */         
        FIELD fullsum      AS DECIMAL              /* ���� �㬬� ���᪨������ ������������ */ 
        FIELD sumgosp      AS DECIMAL              /* �㬬� ��ᯮ諨�� */          
        FIELD srokprosr    AS INT                  /* �ப ����祭��� ������������ */        
        FIELD kolporuch    AS INT                  /* ������⢮ �����⥫�� (�� ����稨) */        
        FIELD nomdogpor    AS CHAR                 /* ����� ������� �����⥫��⢠ */        
        FIELD datedogpor   AS DATE                 /* ��� ������� �����⥫��⢠ */        
        FIELD fiopor       AS CHAR                 /* ��� �����⥫� */        
        FIELD regionpor    AS CHAR                 /* ���� ஦����� �����⥫�  */        
        FIELD sernompor    AS CHAR                 /* ���� � ����� ��ᯮ�� �����⥫� */
        FIELD datepor      AS DATE                 /* ��� �뤠� ��ᯮ�� �����⥫� */        
        FIELD whopor       AS CHAR                 /* ������������ �࣠��, �뤠�襣� ��ᯮ�� �����⥫� */
        FIELD innpor       AS CHAR                 /* ��� �����⥫� (�� ����稨) */
        FIELD adrfactpor   AS CHAR                 /* ���� 䠪��᪮�� �஦������ �����⥫� */      
        FIELD adrregpor    AS CHAR                 /* ���� ॣ����樨 �����⥫� */
        FIELD telpor       AS CHAR                 /* ����䮭� �����⥫� */
        FIELD kolzalog     AS INT                  /* ������⢮ ��������⥫�� (�� ����稨) */ 
        FIELD nomdogzal    AS CHAR                 /* ����� ������� ������ */        
        FIELD datedogzal   AS DATE                 /* ��� ������� ������ */        
        FIELD predmzal     AS CHAR                 /* �।��� ������ */        
        FIELD mestozal     AS CHAR                 /* ���⮭�宦����� �।��� ������ */        
        FIELD svedzal      AS CHAR                 /* ��� ᢥ�����, ��������騥 �������஢��� �।��� ������ (�� ����稨) */        
        FIELD fiozal       AS CHAR                 /* �������, ���, ����⢮/������������ ��������⥫� (�� ����稨) */        
        FIELD sernomzal    AS CHAR                 /* ���� � ����� ��ᯮ�� ��������⥫� */
        FIELD datezal      AS DATE                 /* ��� �뤠� ��ᯮ�� ��������⥫� */        
        FIELD whozal       AS CHAR                 /* ������������ �࣠��, �뤠�襣� ��ᯮ�� ��������⥫� */
        FIELD innzal       AS CHAR                 /* ��� ��������⥫� (�� ����稨) */
        FIELD adrfactzal   AS CHAR                 /* ���� 䠪��᪮�� �஦������ ��������⥫� */      
        FIELD adrregzal    AS CHAR                 /* ���� ॣ����樨 ��������⥫� */
        FIELD telzal       AS CHAR                 /* ����䮭� ��������⥫� */
        FIELD bkiname      AS CHAR                 /* �������� ���, �㤠 ��।������� �।�⭠� ����� */
        FIELD firstcred    AS CHAR                 /* ��ࢨ�� �।��� */
        FIELD nomcess      AS CHAR                 /* ��� � ����� ������� ��ᨨ � ��ࢨ�� �।��஬ */
        
        FIELD email        AS CHAR                 /* email */
        FIELD zalstoim     AS DECIMAL              /* ��������� �⮨����� */
        FIELD mark         AS CHAR                 /* ��ઠ ��� */
        FIELD model        AS CHAR                 /* ������ ��� */
        FIELD colour       AS CHAR                 /* 梥� ��� */
        FIELD yearAuto     AS CHAR                 /* ��� ��� */        
        FIELD vin          AS CHAR                 /* vin ��� */ 
        FIELD ptsSer       AS CHAR                 /* ptsSer ��� */               
        FIELD ptsNum       AS CHAR                 /* ptsNum ��� */ 
        FIELD ptsDate      AS CHAR                 /* ptsDate ��� */ 
          
        INDEX cont_code cont_code       
    .

{empty otch}

   
DEF VAR vin AS CHAR NO-UNDO.
DEF VAR mark AS CHAR NO-UNDO.
DEF VAR yearAuto AS CHAR NO-UNDO.   
DEF VAR model AS CHAR NO-UNDO.
DEF VAR ptsSer AS CHAR NO-UNDO.
DEF VAR ptsNum AS CHAR NO-UNDO.
DEF VAR ptsDate AS CHAR NO-UNDO.
DEF VAR sumpogod AS DECIMAL NO-UNDO.
DEF VAR sumpogproc AS DECIMAL NO-UNDO.   
DEF VAR datelastpog AS DATE NO-UNDO.
DEF VAR sumlastpog AS DECIMAL NO-UNDO.
DEF VAR sumod AS DECIMAL NO-UNDO.
DEF VAR sumproc AS DECIMAL NO-UNDO. 
DEF VAR datekredpr AS DATE NO-UNDO.
DEF VAR tmpDate AS DATE NO-UNDO.       
DEF VAR datekredproc AS DATE NO-UNDO.
DEF VAR tmpDate2 AS DATE NO-UNDO.

DEF VAR regionbirth AS CHAR NO-UNDO.
DEF VAR email AS CHAR NO-UNDO.
DEF VAR zalstoim AS DECIMAL NO-UNDO.
DEF VAR colour AS CHAR NO-UNDO.
DEF VAR engine AS CHAR NO-UNDO.
DEF VAR chassis AS CHAR NO-UNDO.
/*
DEF VAR modelauto AS CHAR NO-UNDO.
DEF VAR markauto AS CHAR NO-UNDO.
DEF VAR vinauto AS CHAR NO-UNDO.
DEF VAR serpts AS CHAR NO-UNDO.
DEF VAR numpts AS CHAR NO-UNDO.
DEF VAR datepts AS CHAR NO-UNDO.
*/

DEFINE VARIABLE mFileName AS CHARACTER NO-UNDO.
DEF VAR tmpInt AS INT NO-UNDO.

DEF VAR fstr AS CHAR INIT '' NO-UNDO.
DEF NEW SHARED STREAM vvs.
 
mFileName = "./1collect" + ".txt".


 {getdate.i}

/* �� �⬥祭�� �����⠬ */
FOR EACH tmprecid NO-LOCK,
	FIRST loan WHERE RECID(loan) = tmprecid.id NO-LOCK:

    FIND FIRST PERSON 
        WHERE PERSON.PERSON-ID = loan.cust-id
        NO-LOCK NO-ERROR.
    IF AVAIL PERSON THEN DO:
        CREATE otch.
        otch.fio = PERSON.NAME-LAST + ' ' + PERSON.FIRST-NAMES.
        IF person.gender THEN 
        otch.gender = "�".
        ELSE otch.gender = "�".
        otch.birthday = person.birthday.
        FOR EACH cust-ident WHERE cust-ident.close-date EQ ?
            AND cust-ident.class-code EQ 'p-cust-ident'
            AND cust-ident.cust-cat EQ '�'
            AND cust-ident.cust-id EQ person.person-id 
            AND cust-ident.cust-code-type EQ '��ᯮ��'
            NO-LOCK BY cust-ident.open-date DESC:
                otch.sernompasp = cust-ident.cust-code.
                otch.datepasp = cust-ident.open-date.        
                otch.whopasp = cust-ident.issue.
            LEAVE.
        END.
        IF person.inn <> ? THEN otch.inn = person.inn.
        FOR EACH cust-ident WHERE cust-ident.close-date EQ ?
            AND cust-ident.class-code EQ 'p-cust-adr'
            AND cust-ident.cust-cat EQ '�'
            AND cust-ident.cust-id EQ person.person-id
            AND cust-ident.cust-code-type EQ '����ய'
            NO-LOCK BY cust-ident.open-date DESC:
                otch.adrreg = cust-ident.issue.
            LEAVE.    
        END.
        FOR EACH cust-ident WHERE cust-ident.close-date EQ ?
            AND cust-ident.class-code EQ 'p-cust-adr'
            AND cust-ident.cust-cat EQ '�'
            AND cust-ident.cust-id EQ person.person-id 
            AND cust-ident.cust-code-type EQ '�������'
            NO-LOCK BY cust-ident.open-date DESC:
                otch.adrfact = cust-ident.issue.
            LEAVE.    
        END.
        
        otch.teldom = TRIM(person.phone[1],",").
        otch.telmob = TRIM(person.phone[2],",").
        
        FIND FIRST signs where signs.file-name = 'person'
            and signs.surrogate = STRING(person.person-id)
            and signs.code = 'BirthPlace'
            no-lock no-error.
        if avail signs then regionbirth = signs.xattr-value.
        else regionbirth = ''.
        
        FIND FIRST signs where signs.file-name = 'person'
            and signs.surrogate = STRING(person.person-id)
            and signs.code = 'e-mail'
            no-lock no-error.
        if avail signs then email = signs.xattr-value.
        else email = ''.
        
    END.
    otch.regionbirth = regionbirth.
    otch.cont_code = loan.doc-ref.
    otch.open_date = loan.open-date.
    
    FIND FIRST loan-int OF loan WHERE 
    loan-int.id-d = 0 
    AND loan-int.id-k = 3 
    AND loan-int.mdate < end-date
    AND loan-int.amt-rub <> 0
    NO-LOCK NO-ERROR.
    IF AVAIL loan-int THEN DO:
       otch.open_date2 = loan-int.mdate.
       otch.sumcred = loan-int.amt-rub. 
    END.


    sumcred = 0.
    FOR EACH term-obl WHERE term-obl.amt-rub <> 0 AND
            term-obl.contract = '�।��'
        AND term-obl.cont-code = loan.cont-code
        AND term-obl.idnt = 2
    	AND term-obl.end-date <> ?
	AND term-obl.sop-date = ?
        NO-LOCK
        BY term-obl.end-date:
     otch.sumcred = term-obl.amt-rub.
         LEAVE.
    END.
    otch.open_date2 = loan.open-date.


    otch.typeprod = '����'.
    otch.currency = 'RUB'.
    otch.srok_cred = loan.end-date - loan.open-date.
    otch.sumpeni = 0.
    FOR EACH comm-rate
         WHERE comm-rate.kau EQ loan.contract + "," + loan.cont-code
         AND comm-rate.commission EQ '%�।'
         AND comm-rate.since <= end-date
         NO-LOCK
         BY comm-rate.since DESC:
         otch.proc_st = comm-rate.rate-comm.   
         LEAVE.
    END. 
    otch.sumgosp = 0.
    otch.bkiname = '����,����䠪�'.
    
    
        FOR EACH term-obl WHERE
        term-obl.cont-code EQ loan.cont-code
        AND term-obl.contract EQ loan.contract
        AND term-obl.idnt EQ 5
        /* AND (term-obl.end-date = ? OR term-obl.end-date >= end-date) */ 
        NO-LOCK BY term-obl.fop-date:
          
           
        FIND FIRST signs
            WHERE signs.file-name = 'term-obl'
            AND signs.code = '�����'
            AND signs.xattr-value = '��⮬�����'
            AND signs.surrogate EQ term-obl.contract + "," + term-obl.cont-code + ",5,"  + STRING(term-obl.end-date, "99/99/99") + "," + STRING(term-obl.nn)
            NO-LOCK NO-ERROR.       
        IF NOT AVAILABLE(signs) THEN NEXT.
        
        
        otch.nomdogzal = GetXAttrValueEx("term-obl",
        loan.contract + "," + loan.cont-code + ',5,' + STRING(term-obl.end-date,"99/99/99") + ',' + STRING(term-obl.nn),
        "��������",
        "").
        otch.datedogzal = term-obl.fop-date.

        vin = GetXAttrValueEx("term-obl",
        loan.contract + "," + loan.cont-code + ',5,' + STRING(term-obl.end-date,"99/99/99") + ',' + STRING(term-obl.nn),
        "TCVIN",
        "").
        mark = GetXAttrValueEx("term-obl",
        loan.contract + "," + loan.cont-code + ',5,' + STRING(term-obl.end-date,"99/99/99") + ',' + STRING(term-obl.nn),
        "TCbrand",
        "").
        colour = GetXAttrValueEx("term-obl",
        loan.contract + "," + loan.cont-code + ',5,' + STRING(term-obl.end-date,"99/99/99") + ',' + STRING(term-obl.nn),
        "TCcolor",
        "").
        yearAuto = GetXAttrValueEx("term-obl",
        loan.contract + "," + loan.cont-code + ',5,' + STRING(term-obl.end-date,"99/99/99") + ',' + STRING(term-obl.nn),
        "TCyear",
        "").        
        model = GetXAttrValueEx("term-obl",
        loan.contract + "," + loan.cont-code + ',5,' + STRING(term-obl.end-date,"99/99/99") + ',' + STRING(term-obl.nn),
        "TCmodel",
        "").
        ptsSer = GetXAttrValueEx("term-obl",
        loan.contract + "," + loan.cont-code + ',5,' + STRING(term-obl.end-date,"99/99/99") + ',' + STRING(term-obl.nn),
        "TCSER",
        "").
        ptsNum = GetXAttrValueEx("term-obl",
        loan.contract + "," + loan.cont-code + ',5,' + STRING(term-obl.end-date,"99/99/99") + ',' + STRING(term-obl.nn),
        "TCNUMB",
        "").
        
        FIND FIRST signs WHERE signs.file-name = 'term-obl'
            AND signs.surrogate = loan.contract + "," + loan.cont-code + ',5,' + STRING(term-obl.end-date,"99/99/99") + ',' + STRING(term-obl.nn)
            AND signs.code = 'TCDATE' NO-LOCK NO-ERROR.
        IF AVAIL signs THEN DO:
            ptsDate = STRING(DAY(signs.date-value), "99") + '.' + STRING(MONTH(signs.date-value), "99") + '.' + STRING(YEAR(signs.date-value), "9999").
        END.
        ELSE DO:
            ptsDate = ''.
        END.

        zalstoim = term-obl.amt-rub.
        
        engine = GetXAttrValueEx("term-obl",
        loan.contract + "," + loan.cont-code + ',5,' + STRING(term-obl.end-date,"99/99/99") + ',' + STRING(term-obl.nn),
        "TCmotor",
        "").
   
        otch.svedzal = "�����⥫�: " + engine + ', 梥�: ' + colour + ', ��� ���᪠ ���: ' + yearAuto.
        
        otch.email = email.
        otch.zalstoim = zalstoim.
        otch.mark = mark.
        otch.model = model.
        otch.colour = colour.
        otch.yearAuto = yearAuto.
        otch.vin = vin.
        otch.ptsSer = ptsSer.
        otch.ptsNum = ptsNum.        
        otch.ptsDate = ptsDate.         

        LEAVE.
     END.   
    
        datelastpog = loan.open-date.
        sumlastpog = 0.
        sumpogod = 0.
        FOR EACH loan-acct WHERE loan-acct.cont-code = loan.cont-code
            AND loan-acct.contract = loan.contract
            AND loan-acct.acct-type = '�।��'
            NO-LOCK:
                FOR EACH op-entry WHERE
                    op-entry.acct-cr = loan-acct.acct 
                    AND SUBSTRING(op-entry.acct-db,1,5) = '40817'
                    AND op-entry.op-status >= "�"
                    AND op-entry.op-date < end-date
                    NO-LOCK:
                        IF datelastpog < op-entry.op-date THEN DO:
                            datelastpog = op-entry.op-date.
                            sumlastpog = ABS(op-entry.amt-rub).
                        END.
                        IF datelastpog = op-entry.op-date THEN DO:
                            sumlastpog = sumlastpog + ABS(op-entry.amt-rub).
                        END.
                        sumpogod = sumpogod + ABS(op-entry.amt-rub).
                END.
        END.
            
        FOR EACH loan-acct WHERE loan-acct.cont-code = REPLACE(loan.cont-code,'@0000','@0400')
            AND loan-acct.contract = loan.contract
            AND loan-acct.acct-type = '�।��'
            NO-LOCK:
                FOR EACH op-entry WHERE
                    op-entry.acct-cr = loan-acct.acct 
                    AND SUBSTRING(op-entry.acct-db,1,5) = '40817'
                    AND op-entry.op-status >= "�"
                    AND op-entry.op-date < end-date
                    NO-LOCK:
                        IF datelastpog < op-entry.op-date THEN DO:
                            datelastpog = op-entry.op-date.
                            sumlastpog = ABS(op-entry.amt-rub).
                        END.
                        IF datelastpog = op-entry.op-date THEN DO:
                            sumlastpog = sumlastpog + ABS(op-entry.amt-rub).
                        END.
                        sumpogod = sumpogod + abs(op-entry.amt-rub).
                END.
        END.
    
        FOR EACH loan-acct WHERE loan-acct.cont-code = loan.cont-code
            AND loan-acct.contract = loan.contract
            AND loan-acct.acct-type = '�।��'
            NO-LOCK:
                FOR EACH op-entry WHERE
                    op-entry.acct-cr = loan-acct.acct 
                    AND SUBSTRING(op-entry.acct-db,1,5) = '40817'
                    AND op-entry.op-status >= "�"
                    AND op-entry.op-date < end-date
                    NO-LOCK:
                        IF datelastpog < op-entry.op-date THEN DO:
                            datelastpog = op-entry.op-date.
                            sumlastpog = ABS(op-entry.amt-rub).
                        END.
                        IF datelastpog = op-entry.op-date THEN DO:
                            sumlastpog = sumlastpog + ABS(op-entry.amt-rub).
                        END.
                    sumpogod = sumpogod + abs(op-entry.amt-rub).
                END.
        END.
        
        FOR EACH loan-acct WHERE loan-acct.cont-code = REPLACE(loan.cont-code,'@0000','@0400')
            AND loan-acct.contract = loan.contract
            AND loan-acct.acct-type = '�।��'
            NO-LOCK:
                FOR EACH op-entry WHERE
                    op-entry.acct-cr = loan-acct.acct 
                    AND SUBSTRING(op-entry.acct-db,1,5) = '40817'
                    AND op-entry.op-status >= "�"
                    AND op-entry.op-date < end-date
                    NO-LOCK:
                        IF datelastpog < op-entry.op-date THEN DO:
                            datelastpog = op-entry.op-date.
                            sumlastpog = ABS(op-entry.amt-rub).
                        END.
                        IF datelastpog = op-entry.op-date THEN DO:
                            sumlastpog = sumlastpog + ABS(op-entry.amt-rub).
                        END.
                        sumpogod = sumpogod + abs(op-entry.amt-rub).
                END.
        END.
        otch.sumpogod = sumpogod.

        sumpogproc = 0.
        FOR EACH loan-acct WHERE loan-acct.cont-code = loan.cont-code
            AND loan-acct.contract = loan.contract
            AND loan-acct.acct-type = '�।�'
            NO-LOCK:
                FOR EACH op-entry WHERE
                    op-entry.acct-cr = loan-acct.acct 
                    AND SUBSTRING(op-entry.acct-db,1,5) = '40817'
                    AND op-entry.op-status >= "�"
                    AND op-entry.op-date < end-date
                    NO-LOCK:
                        IF datelastpog < op-entry.op-date THEN DO:
                            datelastpog = op-entry.op-date.
                            sumlastpog = ABS(op-entry.amt-rub).
                        END.
                        IF datelastpog = op-entry.op-date THEN DO:
                            sumlastpog = sumlastpog + ABS(op-entry.amt-rub).
                        END.                        
                    sumpogproc = sumpogproc + abs(op-entry.amt-rub).
                END.
        END.
            
        FOR EACH loan-acct WHERE loan-acct.cont-code = REPLACE(loan.cont-code,'@0000','@0400')
            AND loan-acct.contract = loan.contract
            AND loan-acct.acct-type = '�।�'
            NO-LOCK:
                FOR EACH op-entry WHERE
                    op-entry.acct-cr = loan-acct.acct 
                    AND SUBSTRING(op-entry.acct-db,1,5) = '40817'
                    AND op-entry.op-status >= "�"
                    AND op-entry.op-date < end-date
                    NO-LOCK:
                        IF datelastpog < op-entry.op-date THEN DO:
                            datelastpog = op-entry.op-date.
                            sumlastpog = ABS(op-entry.amt-rub).
                        END.
                        IF datelastpog = op-entry.op-date THEN DO:
                            sumlastpog = sumlastpog + ABS(op-entry.amt-rub).
                        END.                         
                    sumpogproc = sumpogproc + abs(op-entry.amt-rub).
                END.
        END.
    
        FOR EACH loan-acct WHERE loan-acct.cont-code = loan.cont-code
            AND loan-acct.contract = loan.contract
            AND loan-acct.acct-type = '�।��%'
            NO-LOCK:
                FOR EACH op-entry WHERE
                    op-entry.acct-cr = loan-acct.acct 
                    AND SUBSTRING(op-entry.acct-db,1,5) = '40817'
                    AND op-entry.op-status >= "�"
                    AND op-entry.op-date < end-date
                    NO-LOCK:
                        IF datelastpog < op-entry.op-date THEN DO:
                            datelastpog = op-entry.op-date.
                            sumlastpog = ABS(op-entry.amt-rub).
                        END.
                        IF datelastpog = op-entry.op-date THEN DO:
                            sumlastpog = sumlastpog + ABS(op-entry.amt-rub).
                        END.                         
                    sumpogproc = sumpogproc + abs(op-entry.amt-rub).
                END.
        END.
                
        FOR EACH loan-acct WHERE loan-acct.cont-code = REPLACE(loan.cont-code,'@0000','@0400')
            AND loan-acct.contract = loan.contract
            AND loan-acct.acct-type = '�।��%'
            NO-LOCK:
                FOR EACH op-entry WHERE
                    op-entry.acct-cr = loan-acct.acct 
                    AND SUBSTRING(op-entry.acct-db,1,5) = '40817'
                    AND op-entry.op-status >= "�"
                    AND op-entry.op-date < end-date
                    NO-LOCK:
                        IF datelastpog < op-entry.op-date THEN DO:
                            datelastpog = op-entry.op-date.
                            sumlastpog = ABS(op-entry.amt-rub).
                        END.
                        IF datelastpog = op-entry.op-date THEN DO:
                            sumlastpog = sumlastpog + ABS(op-entry.amt-rub).
                        END.                         
                    sumpogproc = sumpogproc + abs(op-entry.amt-rub).
                END.
        END.
        otch.sumpogproc = sumpogproc.
        
        otch.sumpogfull = 0.
        FOR EACH loan-int WHERE loan-int.contract = loan.contract
            AND loan-int.cont-code = loan.cont-code
            AND loan-int.id-k = 377
            AND loan-int.mdate < end-date NO-LOCK:
            otch.sumpogfull = otch.sumpogfull + abs(loan-int.amt-rub).
        END.
        otch.sumpogfull = otch.sumpogfull + sumpogproc + sumpogod.
        otch.datelastpog = datelastpog.
        otch.sumlastpog = sumlastpog.
        
    sumod = 0.
    FIND LAST loan-acct OF loan WHERE 
        loan-acct.since <= end-date 
        AND loan-acct.acct-type = '�।��'
    NO-LOCK NO-ERROR.
    IF AVAIL loan-acct THEN DO:
        RUN acct-pos IN h_base (loan-acct.acct, loan-acct.currency, (end-date - 1), (end-date - 1), ?).
        IF loan-acct.currency = '' THEN
            sumod = abs(sh-bal).
        ELSE sumod = abs(sh-val).
    END.  
    
    tmpDate = loan.open-date.
    datekredpr = end-date.
    FIND LAST loan-acct OF loan WHERE 
        loan-acct.since <= end-date 
        AND loan-acct.acct-type = '�।��'
    NO-LOCK NO-ERROR.
    IF AVAIL loan-acct THEN DO:
        FIND LAST acct-pos WHERE acct-pos.acct = loan-acct.acct
            AND acct-pos.since < end-date
            AND acct-pos.balance = 0
            NO-LOCK NO-ERROR.
        IF AVAIL acct-pos THEN tmpDate = acct-pos.since.
        
        FIND FIRST acct-pos WHERE acct-pos.acct = loan-acct.acct
            AND acct-pos.since < end-date
            AND acct-pos.since >= tmpDate
            AND acct-pos.balance > 0
            NO-LOCK NO-ERROR.
        IF AVAIL acct-pos THEN datekredpr = acct-pos.since.
        
        RUN acct-pos IN h_base (loan-acct.acct, loan-acct.currency, (end-date - 1), (end-date - 1), ?).
        IF loan-acct.currency = '' THEN
            sumod = sumod + abs(sh-bal).
        ELSE sumod = sumod + (sh-val).
    END.  
    otch.sumod = sumod.
    
    sumproc = 0.   
    tmpDate2 = loan.open-date.
    datekredproc = tmpDate2.
    FIND LAST loan-acct OF loan WHERE 
        loan-acct.since <= end-date 
        AND loan-acct.acct-type = '�।��%'
    NO-LOCK NO-ERROR.
    IF AVAIL loan-acct THEN DO:
        FIND LAST acct-pos WHERE acct-pos.acct = loan-acct.acct
            AND acct-pos.since < end-date
            AND acct-pos.balance = 0
            NO-LOCK NO-ERROR.
        IF AVAIL acct-pos THEN tmpDate2 = acct-pos.since.
        FIND FIRST acct-pos WHERE acct-pos.acct = loan-acct.acct
            AND acct-pos.since < end-date
            AND acct-pos.since >= tmpDate2
            AND acct-pos.balance > 0
            NO-LOCK NO-ERROR.
        IF AVAIL acct-pos THEN datekredproc = acct-pos.since.
        
        RUN acct-pos IN h_base (loan-acct.acct, loan-acct.currency, end-date, end-date, ?).
        IF loan-acct.currency = '' THEN
            sumproc = abs(sh-bal).
        ELSE sumproc = abs(sh-val).
    END.
    FIND LAST loan-acct OF loan WHERE 
        loan-acct.since <= end-date 
        AND loan-acct.acct-type = '�।��%�'
    NO-LOCK NO-ERROR.
    IF AVAIL loan-acct THEN DO:
        RUN acct-pos IN h_base (loan-acct.acct, loan-acct.currency, end-date, end-date, ?).
        IF loan-acct.currency = '' THEN
            sumproc = sumproc + abs(sh-bal).
        ELSE sumproc = sumproc + abs(sh-val).
    END.
    otch.sumproc = sumproc.
    
    otch.fullsum = sumproc + sumod.

   IF datekredpr > datekredproc AND datekredproc <> loan.open-date THEN datekredpr = datekredproc. 
    otch.plan_date = datekredpr.
    otch.srokprosr = (end-date) - datekredpr.
END.

/*
RUN instview.p(TEMP-TABLE otch:HANDLE).                 
*/

/*
{setdest.i}
*/
OUTPUT TO VALUE(mFileName) CONVERT TARGET "1251".

PUT UNFORMATTED
        "�������, ���, ����⢮ ��������"
        "^������ ��������"
        "^��� ஦����� ��������"
        "^���� ஦����� ��������"
        "^��� ����� ��ᯮ�� ��������"
        "^��� �뤠� ��ᯮ�� ��������"        
        "^�࣠� �뤠� ��ᯮ�� ��������"
        "^��� ��������"
        "^��� ��������"
        "^���� 䠪��᪮�� �஦������ ��������"      
        "^���� ॣ����樨 ��������"
        "^����䮭 ����譨� �������� (�� ����稨)"
        "^����䮭 ������� �������� (�� ����稨)"        
        "^����䮭 ࠡ�稩 �������� (�� ����稨)"         
        "^����� ��"        
        "^��� �।��"
        "^��� �।��⠢����� �।��"
        "^��� �த�� (��� ������: ������, ����, ���� � �.�.)"
        "^�㬬� �।��"
        "^����� ��"
        "^�������� ��� ������ �।�� (��� ������������� ������������)"
        "^�ப �।�� (� ����)"
        "^��業⭠� �⠢��"        
        "^�㬬� ����襭���� �᭮����� ����� �� ���� ��।��"
        "^�㬬� ����襭��� ��業⮢ �� ���� ��।��"
        "^���� ����襭��� �㬬� �� ���� ��।��"        
        "^��� ��᫥����� ����襭��"        
        "^�㬬� ��᫥����� ���⥦�"         
        "^�㬬� ������������ �� �᭮����� ����� �� ���� ��।��"
        "^�㬬� ������������ �� ��業⠬ �� ���� ��।��"        
        "^�㬬� ������������ �� ���䠬 (����, ����⮩��)"         
        "^���� �㬬� ���᪨������ ������������" 
        "^�㬬� ��ᯮ諨��"          
        "^�ப ����祭��� ������������"        
        "^������⢮ �����⥫�� (�� ����稨)"        
        "^����� ������� �����⥫��⢠"        
        "^��� ������� �����⥫��⢠"        
        "^��� �����⥫�"        
        "^���� ஦����� �����⥫�"        
        "^���� � ����� ��ᯮ�� �����⥫�"
        "^��� �뤠� ��ᯮ�� �����⥫�"        
        "^������������ �࣠��, �뤠�襣� ��ᯮ�� �����⥫�"
        "^��� �����⥫� (�� ����稨)"
        "^���� 䠪��᪮�� �஦������ �����⥫�"      
        "^���� ॣ����樨 �����⥫�"
        "^����䮭� �����⥫�"
        "^������⢮ ��������⥫�� (�� ����稨)" 
        "^����� ������� ������"        
        "^��� ������� ������"              
        "^���⮭�宦����� �।��� ������"        
        "^��� ᢥ�����, ��������騥 �������஢��� �।��� ������ (�� ����稨)"        
        "^�������, ���, ����⢮/������������ ��������⥫� (�� ����稨)"        
        "^���� � ����� ��ᯮ�� ��������⥫�"
        "^��� �뤠� ��ᯮ�� ��������⥫�"        
        "^������������ �࣠��, �뤠�襣� ��ᯮ�� ��������⥫�"
        "^��� ��������⥫� (�� ����稨)"
        "^���� 䠪��᪮�� �஦������ ��������⥫�"      
        "^���� ॣ����樨 ��������⥫�"
        "^����䮭� ��������⥫�"
        "^�������� ���, �㤠 ��।������� �।�⭠� �����"
        "^��ࢨ�� �।���"
        "^��� � ����� ������� ��ᨨ � ��ࢨ�� �।��஬"
        
        "^E-mail �������� (�� ����稨)"
        "^�।��� ������" 
        "^���⮭�宦����� �।��� ������(� ��������, � �����, ॠ�������)"
        "^��������� �⮨�����"
        "^������, ��ઠ ���"
        "^VIN"
        "^��� ���"
        "^�����  ���"
        "^��� �।��⠢����� ��� � ����"                
        "^����稥 ��� � �����"
        "^����稥 ��� � �����"
        "^��� �뤠� ���"
        "^��� ᢥ�����, ��������騥 �������஢��� �।��� ������ (�� ����稨)"
        "^�������, ���, ����⢮/������������ ��������⥫� (�� ����稨)"
        
SKIP.

FOR EACH otch NO-LOCK BY otch.cont_code:
    PUT UNFORMATTED 
       STRING(otch.fio          ) "^"
       STRING(otch.region       ) "^"
       STRING(otch.birthday     ) "^"
       STRING(otch.regionbirth  ) "^"
       STRING(otch.sernompasp   ) "^"
       STRING(otch.datepasp     ) "^"        
       STRING(otch.whopasp      ) "^"
       STRING(otch.gender       ) "^"
       STRING(otch.inn          ) "^"
       STRING(otch.adrfact      ) "^"      
       STRING(otch.adrreg       ) "^"
       STRING(otch.teldom       ) "^"
       STRING(otch.telmob       ) "^"        
       STRING(otch.telrab       ) "^"         
       STRING(otch.cont_code    ) "^"        
       STRING(otch.open_date    ) "^"
       STRING(otch.open_date2   ) "^"
       STRING(otch.typeprod     ) "^"
       STRING(otch.sumcred      ) "^"
       STRING(otch.currency     ) "^"
       STRING(otch.plan_date    ) "^"
       STRING(otch.srok_cred    ) "^"
       STRING(otch.proc_st      ) "^"        
       STRING(otch.sumpogod     ) "^"
       STRING(otch.sumpogproc   ) "^"
       STRING(otch.sumpogfull   ) "^"        
       STRING(otch.datelastpog  ) "^"        
       STRING(otch.sumlastpog   ) "^"         
       STRING(otch.sumod        ) "^"
       STRING(otch.sumproc      ) "^"        
       STRING(otch.sumpeni      ) "^"         
       STRING(otch.fullsum      ) "^" 
       STRING(otch.sumgosp      ) "^"          
       STRING(otch.srokprosr    ) "^"        
       STRING(otch.kolporuch    ) "^"        
       STRING(otch.nomdogpor    ) "^"        
       STRING(otch.datedogpor   ) "^"        
       STRING(otch.fiopor       ) "^"        
       STRING(otch.regionpor    ) "^"        
       STRING(otch.sernompor    ) "^"
       STRING(otch.datepor      ) "^"        
       STRING(otch.whopor       ) "^"
       STRING(otch.innpor       ) "^"
       STRING(otch.adrfactpor   ) "^"      
       STRING(otch.adrregpor    ) "^"
       STRING(otch.telpor       ) "^"
       STRING(otch.kolzalog     ) "^" 
       STRING(otch.nomdogzal    ) "^"        
       STRING(otch.datedogzal   ) "^"  
       STRING(otch.mestozal     ) "^"        
       STRING(otch.svedzal      ) "^"        
       STRING(otch.fiozal       ) "^"        
       STRING(otch.sernomzal    ) "^"
       STRING(otch.datezal      ) "^"        
       STRING(otch.whozal       ) "^"
       STRING(otch.innzal       ) "^"
       STRING(otch.adrfactzal   ) "^"      
       STRING(otch.adrregzal    ) "^"
       STRING(otch.telzal       ) "^"
       STRING(otch.bkiname      ) "^"
       STRING(otch.firstcred    ) "^"
       STRING(otch.nomcess      ) "^"
       
       STRING(otch.email        ) "^"
       STRING("��⮬�����"      ) "^"
       "^"
       STRING(otch.zalstoim     ) "^"
       STRING(otch.model        )  "^"                        
               
       STRING(otch.vin          )  "^"                 
       
                            
        
        STRING(otch.ptsSer       )  "^"                               
        STRING(otch.ptsNum       )  "^"                 
        "^"
        "^"
        "^"
        STRING(otch.ptsDate      )  "^"                 
        STRING(otch.svedzal)    "^"
        "^"
       
    SKIP.
END.
/*
{preview.i}
*/
{intrface.del}

/*
    tmpInt = tmpInt + 1.
    PUT STREAM vvs UNFORMATTED '<Row ss:AutoFitHeight="0">'.
    PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
    PUT STREAM vvs UNFORMATTED STRING(tmpInt) + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
    PUT STREAM vvs UNFORMATTED otch.famil + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
    PUT STREAM vvs UNFORMATTED otch.tel + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
    PUT STREAM vvs UNFORMATTED otch.cont_code + '</Data></Cell>\n'.

    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="Number">'.
    PUT STREAM vvs UNFORMATTED STRING(otch.sumcred) + '</Data></Cell>\n'.

    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="DateTime">'.
    PUT STREAM vvs UNFORMATTED ISO-DATE(DATETIME(otch.plan_date)) + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="Number">'.
    PUT STREAM vvs UNFORMATTED STRING(otch.rateperc) + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="Number">'.
    PUT STREAM vvs UNFORMATTED otch.cred-date + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="Number">'.
    PUT STREAM vvs UNFORMATTED STRING(otch.annuit) + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="String">'.
    PUT STREAM vvs UNFORMATTED otch.acct40817 + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="Number">'.
    PUT STREAM vvs UNFORMATTED STRING(otch.ost40817) + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="String">'.
    PUT STREAM vvs UNFORMATTED otch.block + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="Number">'.
    PUT STREAM vvs UNFORMATTED STRING(otch.sumblock) + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="Number">'.
    PUT STREAM vvs UNFORMATTED STRING(otch.ostRasch1) + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="Number">'.
    PUT STREAM vvs UNFORMATTED STRING(otch.ost455) + '</Data></Cell>\n'.
	PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="Number">'.
    PUT STREAM vvs UNFORMATTED STRING(otch.ost458) + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="Number">'.
    PUT STREAM vvs UNFORMATTED STRING(otch.ost91315) + '</Data></Cell>\n'.
	PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="Number">'.
    PUT STREAM vvs UNFORMATTED STRING(otch.ost91312) + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="Number">'.
    PUT STREAM vvs UNFORMATTED STRING(otch.ost913121) + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="Number">'.
    PUT STREAM vvs UNFORMATTED STRING(otch.ost47427) + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="Number">'.
    PUT STREAM vvs UNFORMATTED STRING(otch.ost90909) + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="Number">'.
    PUT STREAM vvs UNFORMATTED STRING(otch.ost459) + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="Number">'.
    PUT STREAM vvs UNFORMATTED STRING(otch.ost91604Pr) + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="Number">'.
    PUT STREAM vvs UNFORMATTED STRING(otch.ost91604) + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="String">'.
    PUT STREAM vvs UNFORMATTED otch.acct47401 + '</Data></Cell>\n'.    
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="String">'.
    PUT STREAM vvs UNFORMATTED otch.priznak + '</Data></Cell>\n'.    
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="String">'.
    PUT STREAM vvs UNFORMATTED otch.sud + '</Data></Cell>\n'.    
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="String">'.
    PUT STREAM vvs UNFORMATTED otch.acct60323 + '</Data></Cell>\n'.    
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="Number">'.
    PUT STREAM vvs UNFORMATTED STRING(otch.ost60323) + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="Number">'.
    PUT STREAM vvs UNFORMATTED STRING(otch.ost45515) + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="Number">'.
    PUT STREAM vvs UNFORMATTED STRING(otch.ost45818) + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="Number">'.
    PUT STREAM vvs UNFORMATTED STRING(otch.ost60324) + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="Number">'.
    PUT STREAM vvs UNFORMATTED STRING(otch.ost47425) + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="Number">'.
    PUT STREAM vvs UNFORMATTED STRING(otch.ost45918) + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="Number">'.
    PUT STREAM vvs UNFORMATTED STRING(otch.ostKredGar) + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="Number">'.
    PUT STREAM vvs UNFORMATTED STRING(otch.ostKredGar1) + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="Number">'.
    PUT STREAM vvs UNFORMATTED STRING(otch.ostKredBudKom) + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="Number">'.
    PUT STREAM vvs UNFORMATTED STRING(otch.ostKredBudRko) + '</Data></Cell>\n'.
    
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="Number">'.
    PUT STREAM vvs UNFORMATTED STRING(otch.ostKredPrOv) + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="Number">'.
    PUT STREAM vvs UNFORMATTED STRING(otch.ostKredShtOv) + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="Number">'.
    PUT STREAM vvs UNFORMATTED STRING(otch.ostKredTov) + '</Data></Cell>\n'.
    
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="Number">'.
    PUT STREAM vvs UNFORMATTED STRING(otch.raterez) + '</Data></Cell>\n'.
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="String">'.
    PUT STREAM vvs UNFORMATTED otch.pos + '</Data></Cell>\n'.  
    
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="Number">'.
    PUT STREAM vvs UNFORMATTED STRING(otch.peni) + '</Data></Cell>\n'. 
    
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="String">'.
    IF otch.datenewzal <> ? THEN PUT STREAM vvs UNFORMATTED STRING(otch.datenewzal,"99.99.9999").
    PUT STREAM vvs UNFORMATTED '</Data></Cell>\n'. 
    
    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="String">'.
    IF otch.closedate NE ? THEN PUT STREAM vvs UNFORMATTED STRING(otch.closedate,"99.99.9999"). 
  PUT STREAM vvs UNFORMATTED '</Data></Cell>\n'.


    PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="String">'.
    IF otch.datelast NE ? THEN PUT STREAM vvs UNFORMATTED STRING(otch.datelast,"99.99.9999"). 
   PUT STREAM vvs UNFORMATTED '</Data></Cell>\n'.

    PUT STREAM vvs UNFORMATTED '</Row>\n'.
END.    

PUT STREAM vvs UNFORMATTED 
'  </Table>
 </Worksheet>
</Workbook>
' .

OUTPUT STREAM vvs CLOSE.
RUN sndbispc ("file=" + fname + ";class=bq").
{intrface.del}


*/



