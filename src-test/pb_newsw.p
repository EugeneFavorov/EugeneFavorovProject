/**
����᪨� �ࠢ� �ਭ�������: ��� ���� ����
�᭮�����:      ����.1578 - �ନ஢���� ������� ��ॢ���� � ��᪢��
�� ������:     ������� �஢���� �� 蠡���� SWIFT
��ࠬ����:
���� ����᪠:
������:         26.05.2017 ���ᮢ �.�.
*/

DEFINE INPUT PARAM  in-op-date  LIKE op.op-date.
DEFINE INPUT PARAM  in-recid    AS RECID.

{globals.i}             /** �������� ��।������ */
{intrface.get tmess}
{intrface.get swi}      /** sw-trans */
{intrface.get instrum}  /** �㭪樨 ��� ࠡ��� � ���ᠬ� */
{intrface.get xclass}   /** �㭪樨 ��� ࠡ��� � ����奬�� */
{intrface.get refer}    /** indicate */
{intrface.get count}
{sh-defs.i}
{topkind.def}           /* ����� �� �� ��楤��� */
{pb_kladdrsw.fun}

DEFINE VARIABLE cTmp        AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cBr         AS CHARACTER    NO-UNDO.
DEFINE VARIABLE I           AS INTEGER      NO-UNDO.
DEFINE VARIABLE J           AS INTEGER      NO-UNDO.
DEFINE VARIABLE cDocNum     AS CHARACTER    NO-UNDO FORMAT "x(10)".
DEFINE VARIABLE cAcctDb     AS CHARACTER    NO-UNDO FORMAT "x(20)"  VIEW-AS TEXT.
DEFINE VARIABLE cAcctCr     AS CHARACTER    NO-UNDO FORMAT "x(20)"  VIEW-AS TEXT.
DEFINE VARIABLE cAcctCr2    AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cMFil       AS LOGICAL      NO-UNDO. /* ���䨫 ? */
DEFINE VARIABLE cAcctComm   AS CHARACTER    NO-UNDO FORMAT "x(20)".
DEFINE VARIABLE cCurrComm   AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cCommT      AS CHARACTER    NO-UNDO FORMAT "x(11)"  VIEW-AS TEXT INIT "  ��������:".
DEFINE VARIABLE cCrMask     AS CHARACTER    NO-UNDO.
DEFINE VARIABLE dSumPere    AS DECIMAL      NO-UNDO FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE dSumPereR   AS DECIMAL      NO-UNDO.
DEFINE VARIABLE cValPere    AS CHARACTER    NO-UNDO FORMAT "x(3)".
DEFINE VARIABLE cValComm    AS CHARACTER    NO-UNDO.
DEFINE VARIABLE dSumComm    AS DECIMAL      NO-UNDO FORMAT ">>>,>>>,>>9.99" INIT 0.
DEFINE VARIABLE dSumComR    AS DECIMAL      NO-UNDO FORMAT ">>>,>>>,>>9.99" INIT 0.
DEFINE VARIABLE dSumSpis    AS DECIMAL      NO-UNDO FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE dSumSpisR   AS DECIMAL      NO-UNDO.
DEFINE VARIABLE cValSpis    AS CHARACTER    NO-UNDO FORMAT "x(3)"   VIEW-AS TEXT.
DEFINE VARIABLE dKurs       AS DECIMAL      NO-UNDO FORMAT ">>>9.999999"    INIT 1.
DEFINE VARIABLE dKursU      AS DECIMAL      NO-UNDO FORMAT ">>>9.999999"    INIT 1.
DEFINE VARIABLE cN1         AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cN2         AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cINNTxt     AS CHARACTER    NO-UNDO FORMAT "x(12)"  VIEW-AS TEXT INIT "���".
DEFINE VARIABLE cSndINN     AS CHARACTER    NO-UNDO FORMAT "x(35)"  VIEW-AS FILL-IN SIZE 35 BY 1.
DEFINE VARIABLE cSndNameR   AS CHARACTER    NO-UNDO.  /* ��� ��ࠢ�⥫� ��-���᪨ ��� �/� */
DEFINE VARIABLE cSndAdrR    AS CHARACTER    NO-UNDO.  /* ���� ��ࠢ�⥫� ��-���᪨ ��� �/� */
DEFINE VARIABLE cSndName    AS CHARACTER    NO-UNDO FORMAT "x(70)"  VIEW-AS FILL-IN SIZE 50 BY 1.
DEFINE VARIABLE cSndAdr     AS CHARACTER    NO-UNDO FORMAT "x(70)"  VIEW-AS FILL-IN SIZE 50 BY 1.
/*
DEFINE VARIABLE cSndName2   AS CHARACTER    NO-UNDO                 VIEW-AS FILL-IN SIZE 35 BY 1.
DEFINE VARIABLE cSndName3   AS CHARACTER    NO-UNDO                 VIEW-AS FILL-IN SIZE 35 BY 1.
DEFINE VARIABLE cSndTxt2    AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cSndTxt3    AS CHARACTER    NO-UNDO.
*/
DEFINE VARIABLE iSndCnt1    AS INTEGER      NO-UNDO FORMAT ">9"     INIT 0.
DEFINE VARIABLE iSndCnt2    AS INTEGER      NO-UNDO FORMAT ">9"     INIT 0.
DEFINE VARIABLE cDrTxt      AS CHARACTER    NO-UNDO FORMAT "x(13)"  INIT " �������".
DEFINE VARIABLE cDrTel      AS CHARACTER    NO-UNDO FORMAT "x(20)".
DEFINE VARIABLE cSwIntB     AS CHARACTER    NO-UNDO FORMAT "x(12)".
DEFINE VARIABLE cIntBName   AS CHARACTER    NO-UNDO FORMAT "x(34)"  VIEW-AS TEXT.
DEFINE VARIABLE cIntBCntr   AS CHARACTER    NO-UNDO FORMAT "x(5)"   VIEW-AS TEXT.
DEFINE VARIABLE cSwBenB     AS CHARACTER    NO-UNDO FORMAT "x(12)".
DEFINE VARIABLE cKsBenB     AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cBenBName   AS CHARACTER    NO-UNDO FORMAT "x(34)"  VIEW-AS TEXT.
DEFINE VARIABLE cBenBCntr   AS CHARACTER    NO-UNDO FORMAT "x(5)"   VIEW-AS TEXT.
DEFINE VARIABLE cBenAcct    AS CHARACTER    NO-UNDO FORMAT "x(35)".
DEFINE VARIABLE cBenName    AS CHARACTER    NO-UNDO FORMAT "x(70)"  VIEW-AS FILL-IN SIZE 50 BY 1.
DEFINE VARIABLE cBenAdr     AS CHARACTER    NO-UNDO FORMAT "x(70)"  VIEW-AS FILL-IN SIZE 50 BY 1.
DEFINE VARIABLE iBenCnt1    AS INTEGER      NO-UNDO FORMAT ">9"     INIT 0.
DEFINE VARIABLE iBenCnt2    AS INTEGER      NO-UNDO FORMAT ">9"     INIT 0.
DEFINE VARIABLE cBenCntr    AS CHARACTER    NO-UNDO FORMAT "x(3)".
DEFINE VARIABLE cBenRF      AS LOGICAL      NO-UNDO FORMAT "��/���" INIT YES.
DEFINE VARIABLE cBenUL      AS LOGICAL      NO-UNDO FORMAT "��/��"  INIT YES.
DEFINE VARIABLE cBenCntrT   AS CHARACTER    NO-UNDO FORMAT "x(21)"  VIEW-AS TEXT    INIT "            ������".
DEFINE VARIABLE cBenRFT     AS CHARACTER    NO-UNDO FORMAT "x(13)"  VIEW-AS TEXT    INIT "  �������� ��".
DEFINE VARIABLE cBenULT     AS CHARACTER    NO-UNDO FORMAT "x(17)"  VIEW-AS TEXT    INIT "  ��.��� ���.����".
DEFINE VARIABLE cDetails    AS CHARACTER    NO-UNDO FORMAT "x(210)" VIEW-AS FILL-IN SIZE 62 BY 1.
DEFINE VARIABLE iDetCnt     AS INTEGER      NO-UNDO FORMAT ">>9"    INIT 0.
DEFINE VARIABLE cCharge     AS CHARACTER    NO-UNDO FORMAT "x(3)"   INIT "OUR".
DEFINE VARIABLE cSndInfo    AS CHARACTER    NO-UNDO FORMAT "x(210)" VIEW-AS EDITOR SIZE 35 BY 2 BUFFER-LINES 6 NO-WORD-WRAP.
DEFINE VARIABLE iOp         AS INT64        NO-UNDO.
DEFINE VARIABLE cOp         AS CHARACTER    NO-UNDO.
DEFINE VARIABLE iOpTr       AS INT64        NO-UNDO.
DEFINE BUFFER   accom       FOR acct.
DEFINE BUFFER   acccr       FOR acct.
DEFINE BUFFER   accno       FOR acct.

FUNCTION XmlCurr RETURN CHAR
    ( iCurr AS CHAR ):

    FIND FIRST currency
        WHERE currency.currency EQ iCurr
        NO-LOCK NO-ERROR.
    RETURN IF AVAIL currency THEN currency.i-currency ELSE "".
END FUNCTION.

FUNCTION Swift72    RETURNS CHARACTER
   (INPUT  iInfo    AS CHARACTER ).

    DEFINE VARIABLE cT1 AS CHAR     NO-UNDO INIT "".
    DEFINE VARIABLE cT2 AS CHAR     NO-UNDO.
    DEFINE VARIABLE cT3 AS CHAR     NO-UNDO.
    DEFINE VARIABLE I   AS INTEGER  NO-UNDO.
    DEFINE VARIABLE J   AS INTEGER  NO-UNDO.
    
    IF (iInfo NE "")
    THEN DO:
        DO I = 1 TO NUM-ENTRIES(iInfo, "~n"):
            cT2 = TRIM(ENTRY(I, iInfo, "~n")).
            J = 1.
            DO WHILE (J <= LENGTH(cT2)):
                IF (J = 1)
                THEN DO:
                    cT1 = cT1 + (IF (cT1 = "") THEN "" ELSE "~n")
                        + STRING(SUBSTRING((IF (SUBSTRING(cT2, 1, 1) = "/") THEN "" ELSE "//") + cT2, 1, 35), "x(35)")
                        + (IF (I = 1) THEN "" ELSE "    ").
                    J   = J + (IF (SUBSTRING(cT2, 1, 1) = "/") THEN 35 ELSE 33).
                END.
                ELSE DO:
                    DO WHILE (SUBSTRING(cT2, J, 1) = " "):
                        J = J + 1.
                    END.
                    IF (SUBSTRING(cT2, J, 2) = "//") THEN J = J + 2.

                    cT1 = cT1 + "~n//"
                        + STRING(SUBSTRING(cT2, J, 33), "x(37)").
                    J   = J + 33.
                END.
            END.
        END.
    END.

    RETURN cT1.
END FUNCTION.

/* ���ᠭ�� ��� ����� */
{pb_newsw.frm}

/* **************************************************************************** */
/* ���� ���㬥�� SWIFT ******************************************************* */
FIND FIRST op-kind
    WHERE (RECID(op-kind)   EQ in-recid)
    NO-LOCK.
FIND FIRST op-template OF op-kind
    WHERE (op-template.op-template  = 1)
    NO-LOCK.

pick-value = ?.

/* ��� ������ */
DO TRANSACTION:
    RUN browseld.p("acct",
        "acct-cat" + CHR(1) + "bal-acct"                  + CHR(1) + "contract"                + CHR(1) + "currency",
        "b"        + CHR(1) + "40*,423*,426*,60314,60323" + CHR(1) + "�����,�����,�����,dps," + CHR(1) + op-template.currency,
        "bal-acct", 5).

    IF (KEYFUNCTION(LASTKEY) EQ "GO")
    THEN DO:
        cAcctDb     = ENTRY(1, pick-value).
        cValSpis    = ENTRY(2, pick-value).
        cValPere    = cValSpis.
        cAcctComm   = IF (cAcctDb BEGINS "603") THEN "" ELSE cAcctDb.
        cCurrComm   = cValSpis.

        /* ��� �।�� */
        cAcctCr = GetRefVal("SWkorr", in-op-date, cValPere).

        /* ���⥫�騪 */
        FIND FIRST acct
            WHERE (acct.acct        EQ cAcctDb)
              AND (acct.currency    EQ cValSpis)
            NO-LOCK NO-ERROR.
/*
        FIND FIRST accom
            WHERE (accom.acct       EQ cAcctComm)
              AND (accom.currency   EQ cCurrComm)
            NO-LOCK NO-ERROR.
*/
        IF (cAcctComm = "")
        THEN DO:
            cSndNameR = "��� ���� ����".
            cSndName  = "PAO PLUS BANK".
            cSndAdr   = "MOSKVA IZVESTKOVYi PER, DOM 7, STR.1".
            cDrTxt    = " �������".
            cDrTel    = "".
            cSndINN   = "5503016736".
            DO WITH FRAME swdoc:
                cBenRF:HIDDEN = YES.
                cBenUL:HIDDEN = YES.
                cBenRFT:HIDDEN = YES.
                cBenULT:HIDDEN = YES.
                cCommT = "".
                cAcctComm:HIDDEN = YES.
            END.
        END.
        ELSE DO:
            RUN GetCustName IN h_base (acct.cust-cat, acct.cust-id, ?, OUTPUT cN1, OUTPUT cN2, INPUT-OUTPUT cTmp).
            cSndNameR = TRIM(cN1) + " " + TRIM(cN2).
            cSndName  = SUBSTRING(REPLACE(sw-trans(cSndNameR, YES, "RUR5"), "  ", " "), 1, 70).
            cSndAdrR  = KladrSWK(acct.cust-cat, acct.cust-id).
            cSndAdr   = SUBSTRING(REPLACE(sw-trans(cSndAdrR, YES, "RUR5"), "  ", " "), 1, 70).

            IF (acct.cust-cat EQ "�")
            THEN DO:
                FIND FIRST cust-corp
                    WHERE (cust-corp.cust-id EQ acct.cust-id)
                    NO-LOCK NO-ERROR.
                cDrTxt  = " �������".
                cDrTel  = GetXAttrValue("cust-corp", STRING(acct.cust-id), "tel").
                cSndINN = cust-corp.inn /* + "/" + GetXAttrValue("cust-corp", STRING(acct.cust-id), "���") */ .
            END.
            ELSE DO:
                FIND FIRST person
                    WHERE (person.person-id EQ acct.cust-id)
                    NO-LOCK NO-ERROR.
                cDrTxt  = "���� ��������".
                cDrTel  = IF (AVAIL person) THEN STRING(person.birthday, "99.99.9999") ELSE "".
                cSndINN = IF (AVAIL person) THEN person.inn ELSE "".
            END.

            IF (acct.cust-cat EQ "�") OR (acct.acct BEGINS "40802")
            THEN DO WITH FRAME swdoc:
                    cBenRF:HIDDEN = YES.
                    cBenUL:HIDDEN = YES.
                    cBenRFT:HIDDEN = YES.
                    cBenULT:HIDDEN = YES.
                 END.
            ELSE DO WITH FRAME swdoc:
                    cCommT = "".
                    cAcctComm:HIDDEN = YES.
                 END.
        END.
        iSndCnt1  = LENGTH(cSndName).
        iSndCnt2  = LENGTH(cSndAdr).
    END.
    ELSE RETURN.
END.

/* ���� ��ࠬ��஢ ��ॢ��� *************************************************** */
REPEAT:
    DISPLAY
        cCommT dSumSpis dKurs
        cINNTxt
        iSndCnt1 iSndCnt2 cDrTxt
        iBenCnt1 iBenCnt2 iDetCnt
        WITH FRAME swdoc.
    UPDATE  UNLESS-HIDDEN
        cDocNum cAcctDb cAcctCr
        cValPere dSumPere cAcctComm cValSpis
        cSndINN cSndName cSndAdr cDrTel
        cSwIntB cIntBName cIntBCntr
        cSwBenB cBenBName cBenBCntr
        cBenAcct cBenName cBenAdr
        cBenCntrT cBenCntr cBenRFT cBenRF cBenULT cBenUL
        cDetails cCharge cSndInfo
        WITH FRAME swdoc.
    ASSIGN.
/*  cSndAdr = cSndAdr
            + IF (cDrTxt  = "���� ��������") THEN (", " + cDrTel) ELSE "". */

    /* �஢�ન �ࠢ��쭮�� ���������� ����� ��� */
    IF (cDocNum = "")
    THEN DO:
        RUN Fill-SysMes IN h_tmess ("","","1", "������ ����� ���㬥��!").
        UNDO, RETRY.
    END.

    IF (dSumPere = 0)
    THEN DO:
        RUN Fill-SysMes IN h_tmess ("","","1", "������ �㬬� ��ॢ���!").
        UNDO, RETRY.
    END.

    IF (cSwBenB = "")
    THEN DO:
        RUN Fill-SysMes IN h_tmess ("","","1", "�� 㪠��� ���� �����樠�!").
        UNDO, RETRY.
    END.

    IF (cBenAcct = "")
    THEN DO:
        RUN Fill-SysMes IN h_tmess ("","","1", "�� 㪠��� ��� �����樠�!").
        UNDO, RETRY.
    END.

    IF (cBenName = "")
    THEN DO:
        RUN Fill-SysMes IN h_tmess ("","","1", "�� 㪠���� ������������ �����樠�!").
        UNDO, RETRY.
    END.

    IF (cBenCntr = "")
    THEN DO:
        RUN Fill-SysMes IN h_tmess ("","","1", "�� 㪠���� ��࠭� �����樠�!").
        UNDO, RETRY.
    END.

    IF (cDetails = "")
    THEN DO:
        RUN Fill-SysMes IN h_tmess ("","","1", "�� 㪠���� �����祭�� ��ॢ���!").
        UNDO, RETRY.
    END.

    cN1 = Swift72(cSndInfo).
    DO I = 1 TO NUM-ENTRIES(cN1, "~n"):
        cTmp = ENTRY(I, cN1, "~n").
        IF     (SUBSTRING(cTmp, 1, 1) NE "/")
            OR (INDEX(cTmp, "/", 2) EQ 0)
        THEN DO:
            RUN Fill-SysMes IN h_tmess ("","","1", "�訡�� � ���� ���������� ����������� � ��ப� " + STRING(I, "9")).
            UNDO, RETRY.
        END.
    END.
    cSndInfo = cN1.

    LEAVE.
END.
HIDE FRAME swdoc  NO-PAUSE.


/* ��ନ஢���� ���㬥�� ***************************************************** */
IF (KEYFUNCTION(LASTKEY) EQ "GO")
THEN DO TRANSACTION:
    FIND FIRST acccr
        WHERE (acccr.acct       = cAcctCr)
          AND (acccr.close-date = ?)
        NO-LOCK NO-ERROR.
    cMFil = (acccr.filial-id <> shFilial).

    /* ��� �।�� */
    IF (cMFil)
    THEN DO:
        cAcctCr2 = "".
        cN1      = acccr.filial-id.
        cN2      = acccr.currency.
        FOR EACH acccr
            WHERE (acccr.acct  BEGINS (IF (acct.cust-cat EQ "�") THEN "30305" ELSE "30301"))
              AND (acccr.currency   = cN2)
              AND (acccr.close-date = ?)
              AND (acccr.filial-id  = shFilial)
            NO-LOCK,
        EACH c-nostro
            WHERE (c-nostro.acct    = acccr.acct)
            NO-LOCK,
        FIRST accno
            WHERE (accno.acct  BEGINS c-nostro.corr-acct)
              AND (accno.close-date = ?)
              AND (accno.filial-id  = cN1
                OR accno.filial-id  = "0000")
            NO-LOCK:

            cAcctCr2 = acccr.acct.
        END.

        IF (cAcctCr2 = "")
        THEN DO:
            RUN Fill-SysMes IN h_tmess ("", "", "1", '�� ������ ��� 30301* ��� ������ ' + cN2 + "!").
            RETURN.
        END.
    END.
    ELSE cAcctCr2 = cAcctCr.

    CREATE op.
    ASSIGN
        op.Class-Code       = op-template.cr-class-code
        op.op-status        = op-template.op-status
        op.doc-type         = op-template.doc-type
        op.op-transaction   = NEXT-VALUE(op-transaction-id)
        iOpTr               = op.op-transaction
        cBr                 = GetXAttrValue("_user", USERID('bisquit'), "�⤥���������")
        cBr                 = IF (LENGTH(cBr) = 4) THEN cBr ELSE GetXAttrValue("_user", USERID('bisquit'), "�⤥�����")
        cBr                 = IF (LENGTH(cBr) = 4) THEN cBr ELSE shFilial
        op.branch-id        = cBr
        op.filial-id        = shFilial
        op.op-kind          = op-kind.op-kind
        op.doc-num          = cDocNum
        op.doc-kind         = "rec"
        op.op-date          = in-op-date
        op.doc-date         = in-op-date
        op.ins-date         = in-op-date
        op.op-value-date    = in-op-date
        op.due-date         = in-op-date
        op.contract-date    = in-op-date
        op.order-pay        = "5"
        op.details          = "��ॢ�� ��. ������ ᮣ��᭮ ������ �� ��ॢ�� � " + cDocNum
                            + " �� " + STRING(in-op-date, "99.99.9999")
                            + " ������������ ������: " + cBenName
                            + " ���:" + cSwBenB + " �/�:" + cBenAcct
                            + " ����ঠ��� ����樨:" + cDetails
        op.user-id          = USERID('bisquit')
        op.name-ben         = STRING(cBenName, IF (iBenCnt1 < 36) THEN "x(35)" ELSE "x(70)") + cBenAdr
        op.ben-acct         = cBenAcct
        op.inn              = ""
        NO-ERROR.
    iOp = op.op.
    cOp = STRING(iOp).
    RELEASE op.

    UpdateSigns ("op", cOp, "acct-send",      SUBSTRING(cAcctDb, 1, 20), NO).
    IF (cSndINN <> "") THEN
    UpdateSigns ("op", cOp, "inn-send",       cSndINN, NO).
    UpdateSigns ("op", cOp, "name-send",      cSndNameR, NO).
    UpdateSigns ("op", cOp, "acctcorr",       SUBSTRING(cAcctCr, 1, 20), NO).
/*  UpdateSigns ("op", cOp, "acct-rec",       SUBSTRING(cAcctCr, 1, 20), NO).
*/  UpdateSigns ("op", cOp, "sw-name-send",   STRING(cSndName, IF (iSndCnt1 < 36) THEN "x(35)" ELSE "x(70)") + cSndAdr, NO).
/*  UpdateSigns ("op", cOp, "name-rec",       STRING(cBenName, IF (iBenCnt1 < 36) THEN "x(35)" ELSE "x(70)") + cBenAdr, NO).
*/  UpdateSigns ("op", cOp, "sw-benef-cntr",  cBenCntr, NO).
    UpdateSigns ("op", cOp, "sw-bank-op-code","CRED", YES).
/*  UpdateSigns ("op", cOp, "sw-trans-code",  "1", YES).
*/  UpdateSigns ("op", cOp, "sw-charge-acct", cAcctComm, NO).
    UpdateSigns ("op", cOp, "sw-details",     cDetails, NO).
    UpdateSigns ("op", cOp, "swift-det-inf",  "||||||", NO).
    UpdateSigns ("op", cOp, "swift-det-cod",  "||||||", NO).
    IF (cCharge NE "OUR") THEN
    UpdateSigns ("op", cOp, "swift-det-pay", cCharge, YES).
    IF (cValPere NE cValSpis)
    THEN DO:
        UpdateSigns ("op", cOp, "sw-benef-amt",   XmlCurr(cValPere) + TRIM(TRIM(TRIM(STRING(dSumPere, ">>>>>>>>9.99"), "0"), ".")), NO).
        UpdateSigns ("op", cOp, "sw-instr-amt",   XmlCurr(cValSpis) + TRIM(TRIM(TRIM(STRING(dSumSpis, ">>>>>>>>9.99"), "0"), ".")), NO).
        UpdateSigns ("op", cOp, "sw-exch-rate",   TRIM(TRIM(TRIM(STRING(dKurs, ">>>9.999999"), "0"), ".")), NO).
    END.
    IF (acct.cust-cat EQ "�")
    THEN DO:
        UpdateSigns ("op", cOp, "sw-benef-res", IF cBenRF THEN "१�����" ELSE "��१�����", NO).
        UpdateSigns ("op", cOp, "sw-benef-ul",  IF cBenUL THEN "��" ELSE "��", NO).
        UpdateSigns ("op", cOp, "swift-50f-4",  cDrTel, NO).
    END.
    ELSE
        UpdateSigns ("op", cOp, "sw-send-tel",  cDrTel, NO).

    cN1 = "||||||".
    cN2 = "||||||".
    DO I = 1 TO NUM-ENTRIES(cSndInfo, "~n"):
        cTmp = TRIM(ENTRY(I, cSndInfo, "~n")).
        J    = INDEX(cTmp, "/", 2).
        ENTRY(I, cN1, "|") = SUBSTRING(cTmp, 2, J - 2).
        ENTRY(I, cN2, "|") = SUBSTRING(cTmp, J + 1).
    END.
    UpdateSigns ("op", cOp, "swift-det-cod", cN1, NO).
    UpdateSigns ("op", cOp, "swift-det-inf", cN2, NO).

    IF    (cValSpis = cValPere)
        OR NOT CAN-DO(op-template.currency, cValPere)   /* ���� ����� ��ॢ��� */
    THEN DO:
        CREATE op-entry.
        ASSIGN
            op-entry.op         = iOp
            op-entry.op-entry   = 1
            op-entry.op-status  = op-template.op-status
            op-entry.filial-id  = shFilial
            op-entry.type       = "SF"
            op-entry.op-cod     = "000000"
            op-entry.qty        = 0
            op-entry.op-date    = in-op-date
            op-entry.value-date = in-op-date
            op-entry.acct-db    = cAcctDb
            op-entry.acct-cr    = cAcctCr2
            op-entry.currency   = cValSpis
            op-entry.amt-rub    = dSumSpisR
            op-entry.amt-cur    = dSumSpis
            NO-ERROR.
        RELEASE op-entry.
    END.
    ELSE DO:
        CREATE op-entry.
        ASSIGN
            op-entry.op         = iOp
            op-entry.op-entry   = 1
            op-entry.op-status  = op-template.op-status
            op-entry.filial-id  = shFilial
            op-entry.type       = "SF"
            op-entry.op-cod     = "000000"
            op-entry.qty        = 0
            op-entry.op-date    = in-op-date
            op-entry.value-date = in-op-date
            op-entry.acct-db    = cAcctDb
            op-entry.acct-cr    = ?
            op-entry.currency   = cValSpis
            op-entry.amt-rub    = dSumSpisR
/*          op-entry.amt-rub    = dSumPereR
*/          op-entry.amt-cur    = dSumSpis
            NO-ERROR.
        RELEASE op-entry.

        CREATE op-entry.
        ASSIGN
            op-entry.op         = iOp
            op-entry.op-entry   = 2
            op-entry.op-status  = op-template.op-status
            op-entry.filial-id  = shFilial
            op-entry.type       = "SF"
            op-entry.op-cod     = "000000"
            op-entry.qty        = 0
            op-entry.op-date    = in-op-date
            op-entry.value-date = in-op-date
            op-entry.acct-db    = ?
            op-entry.acct-cr    = cAcctCr2
            op-entry.currency   = cValPere
            op-entry.amt-rub    = dSumPereR
            op-entry.amt-cur    = dSumPere
            NO-ERROR.
        RELEASE op-entry.

        cTmp = GetRefVal("��᪨�犐", TODAY, "002,��,�," + cValPere + ",��") + " *".
        FIND FIRST acccr
            WHERE (acccr.bal-acct   = 70601)
              AND (acccr.filial-id  = shFilial)
              AND (acccr.acct MATCHES cTmp)
            NO-LOCK NO-ERROR.
        CREATE op-entry.
        ASSIGN
            op-entry.op         = iOp
            op-entry.op-entry   = 3
            op-entry.op-status  = op-template.op-status
            op-entry.filial-id  = shFilial
            op-entry.type       = "SF"
            op-entry.op-cod     = "000000"
            op-entry.qty        = 0
            op-entry.op-date    = in-op-date
            op-entry.value-date = in-op-date
            op-entry.acct-db    = ?
/*          op-entry.acct-db    = cAcctDb
*/          op-entry.acct-cr    = acccr.acct
            op-entry.currency   = ""
            op-entry.amt-rub    = dSumSpisR - dSumPereR
            op-entry.amt-cur    = 0
            NO-ERROR.
        RELEASE op-entry.
    END.

    IF (cSwIntB NE "")
    THEN DO:
        CREATE op-bank.
        ASSIGN
            op-bank.op              = iOp
            op-bank.op-bank-type    = "intermed"
            op-bank.bank-code-type  = "BIC"
            op-bank.bank-code       = cSwIntB
            op-bank.bank-name       = cIntBName
            .
        UpdateSigns("op-bank", string(op-bank.op) + "," + op-bank.op-bank-type, "ver-form", "A", NO).
        RELEASE op-bank.
    END.

    IF (cSwBenB NE "")
    THEN DO:
        CREATE op-bank.
        ASSIGN
            op-bank.op              = iOp
            op-bank.op-bank-type    = ""
            op-bank.bank-code-type  = "BIC"
            op-bank.bank-code       = cSwBenB
            op-bank.bank-name       = cBenBName
            op-bank.corr-acct       = cKsBenB
            .
        UpdateSigns("op-bank", string(op-bank.op) + "," + op-bank.op-bank-type, "ver-form", "A", NO).
        RELEASE op-bank.
    END.

    FIND FIRST branch
        WHERE (branch.branch-Id = shFilial)
        NO-LOCK NO-ERROR.
    CREATE op-bank.
    ASSIGN
        op-bank.op              = iOp
        op-bank.op-bank-type    = "send"
        op-bank.bank-code-type  = "BIC"         /* "���-9" */
        op-bank.bank-code       = "COMSRUSMXXX" /* GetXAttrValue("branch", shFilial, "�������") */
        op-bank.bank-name       = "PLUS BANK"   /* branch.name */
        .
    UpdateSigns("op-bank", string(op-bank.op) + "," + op-bank.op-bank-type, "Cod-BIC", "COMSRUSMXXX", NO).
    UpdateSigns("op-bank", string(op-bank.op) + "," + op-bank.op-bank-type, "ver-form", "A", NO).
    RELEASE op-bank.

    /* ��ॢ�� ᮧ���, ������� ������� */
    IF (cAcctComm <> "")
    THEN DO:
        RUN Fill-SysMes IN h_tmess ("", "", "1", "���ᠭ�� �����ᨨ = " + TRIM(STRING(dSumComm, ">>>,>>>,>>9.99")) + " " + GetISOCode(cValPere)
                                               + " (" + TRIM(STRING(dSumComR, ">>>,>>>,>>9.99")) + " ��)").
        FIND FIRST op-template OF op-kind
            WHERE (op-template.op-template  = 2)
            NO-LOCK.
        CREATE op.
        ASSIGN
            op.Class-Code       = op-template.cr-class-code
            op.op-status        = op-template.op-status
            op.doc-type         = op-template.doc-type
            op.op-transaction   = iOpTr
            op.branch-id        = cBr
            op.filial-id        = shFilial
            op.op-kind          = op-kind.op-kind
            op.doc-num          = STRING(SetCounterValue("��㡏��",?,in-op-date))
            op.op-date          = in-op-date
            op.doc-date         = in-op-date
            op.ins-date         = in-op-date
            op.op-value-date    = in-op-date
            op.due-date         = in-op-date
            op.contract-date    = in-op-date
            op.order-pay        = "5"
            op.details          = (IF CAN-DO("!.....810*,406*,407*,40802*", cAcctComm) THEN "~{VO80150} " ELSE "")
                                + (IF CAN-DO("!.....810*,40807*",           cAcctComm) THEN "~{VO99090} " ELSE "")
                                + (IF CAN-DO("40807810*",                   cAcctComm) THEN "~{VO80050} " ELSE "")
                                + "������� �� ���⭮� ���㦨����� "
                                + (IF (acct.cust-cat EQ "�") OR (cAcctDb BEGINS "40802") THEN "�ਤ" ELSE "䨧")
                                + "��᪨� ���, ����祭�� � ��.�����. ��� �� ����������"
            op.user-id          = USERID('bisquit')
            NO-ERROR.
        UpdateSigns ("op", STRING(op.op), "�������",    "����", NO).
        UpdateSigns ("op", STRING(op.op), "rate_curr_u", TRIM(STRING(FindRate("�������", cValPere, in-op-date), ">>>,>>9.9999")), NO).

        CREATE op-entry.
        ASSIGN
            op-entry.op         = op.op
            op-entry.op-entry   = 1
            op-entry.op-status  = op-template.op-status
            op-entry.filial-id  = shFilial
            op-entry.type       = op-template.type
            op-entry.op-cod     = "000000"
            op-entry.qty        = 0
            op-entry.op-date    = in-op-date
            op-entry.value-date = in-op-date
            op-entry.acct-db    = cAcctComm
            op-entry.acct-cr    = GetRefVal("��吠��", in-op-date, shFilial + ",*,"
                                + (IF (acct.cust-cat EQ "�") THEN "�" ELSE (IF (cAcctDb BEGINS "40802") THEN "�" ELSE "�")) + ",TAG_103,�")
            op-entry.currency   = cCurrComm
            op-entry.amt-rub    = dSumComR
            op-entry.amt-cur    = IF (cCurrComm = cValComm) THEN dSumComm ELSE 
                                 (IF (cCurrComm = "")       THEN 0.0
                                  ELSE ROUND(CurFromBaseTime("�������", cCurrComm, shFilial, in-op-date, TIME, dSumComR), 2))
            NO-ERROR.
        RELEASE op-entry.
        RELEASE op.
    END.
END.
{intrface.del}

/* �஢�ઠ �����筮�� �।�� �� ���� ******************************************************* */
PROCEDURE CheckPOS:
    DEFINE VARIABLE cMess       AS CHARACTER    NO-UNDO INIT "".

    IF (cAcctComm <> "")
    THEN DO:
        IF (cAcctDb = cAcctComm)
        THEN DO:
            RUN acct-pos IN h_base(cAcctDb, cValSpis, in-op-date, in-op-date, "�").
            IF (- sh-bal - dSumComR - CurToBase("�������", cValSpis, in-op-date, dSumSpis) < 0.0)
            THEN DO:
                RUN Fill-SysMes IN h_tmess ("", "", "1", "�� ��� " + SUBSTRING(cAcctDb,1,20) + " �������筮 �।�� ��� ᯨᠭ�� ��ॢ���"
                                                       + (IF (cAcctComm = "") THEN "." ELSE " � �����ᨨ.")).
                RETURN "ERR".
            END.
        END.
        ELSE DO:
            RUN acct-pos IN h_base(cAcctDb, cValSpis, in-op-date, in-op-date, "�").
            IF (- sh-val - dSumSpis < 0.0)
            THEN cMess = "�� ��� " + SUBSTRING(cAcctDb,1,20)   + " �������筮 �।�� ��� ᯨᠭ�� ��ॢ���.".

            RUN acct-pos IN h_base(cAcctComm, cCurrComm, in-op-date, in-op-date, "�").
            IF (- sh-bal - dSumComR < 0.0)
            THEN cMess = cMess + (IF (cMess = "") THEN "" ELSE "~n")
                       + "�� ��� " + SUBSTRING(cAcctComm,1,20) + " �������筮 �।�� ��� ᯨᠭ�� �����ᨨ.".
            IF (cMess <> "")
            THEN DO:
                RUN Fill-SysMes IN h_tmess ("", "", "1", cMess).
                RETURN "ERR".
            END.
        END.
    END.

    RETURN "".
END PROCEDURE.
