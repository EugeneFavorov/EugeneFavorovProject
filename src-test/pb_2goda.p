/**
����᪨� �ࠢ� �ਭ�������: ��� ���� ����
�᭮�����:      �� �����.421
�� ������:
��� ࠡ�⠥�:
��ࠬ����:      type=���᮪/���쬠  - ⨯ ����,
                cust-cat=�/�;       - ⨯ �����⮢,
                letters=50;         - ������⢮ ��ᥬ �� 1 䠩�,
���� ����᪠:
������:         15.03.2017 ���ᮢ �.�.
*/

DEFINE INPUT PARAMETER iParam       AS CHARACTER NO-UNDO.

{globals.i}             /** �������� ��।������ */
{parsin.def}
{intrface.get netw}     /* ��ࠢ�� � bispc */
{intrface.get xclass}   /* �㭪樨 ��� ࠡ��� � ����奬�� */
{intrface.get blkob}    /* �����஢�� */
{sh-defs.i}
{pb_exf_exl.i}          /** ������⥪� ��� ���㧪� � XL */
{pb_kladdr.fun}

end-date = TODAY.
/* {getdate.i}             / * ��� �⫠��� */
beg-date = DATE(MONTH(end-date), DAY(end-date), YEAR(end-date) - 2).

/******************************************* ��।������ ��६����� � ��. */
DEFINE VARIABLE cXL     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFl     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTyp    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCat    AS CHARACTER NO-UNDO.
DEFINE VARIABLE iLet    AS INTEGER   NO-UNDO.
DEFINE VARIABLE lastrs  AS DATE      NO-UNDO.
DEFINE VARIABLE lasttr  AS DATE      NO-UNDO.
DEFINE VARIABLE npostr  AS DECIMAL   NO-UNDO.
DEFINE VARIABLE I       AS INT64     NO-UNDO INIT 0.
DEFINE VARIABLE J       AS INTEGER   NO-UNDO.   /* ���浪��� ����� ॥��� */
DEFINE VARIABLE Iu      AS INTEGER   NO-UNDO.   /* ����� 㢥�������� �� ��࠭�� 1-3 */
DEFINE VARIABLE Iost    AS INTEGER   NO-UNDO.   /* ��⠫��� ��ᥬ */
DEFINE VARIABLE Nree    AS INTEGER   NO-UNDO.   /* ��ᥬ � ⥪�饬 ॥��� */
DEFINE VARIABLE cTranz  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTmpl1  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTmpl2  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRee_s  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRee_e  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cUvd_b  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cUvd_e  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cUvd_p  AS CHARACTER NO-UNDO.
DEFINE VARIABLE iCnt1   AS INT64     NO-UNDO.   /* ���稪� ��� �ଠ�஢���� 㢥�������� */
DEFINE VARIABLE iCnt2   AS INT64     NO-UNDO.
DEFINE VARIABLE iCnt3   AS INT64     NO-UNDO.
DEFINE VARIABLE iCnt4   AS INT64     NO-UNDO.
DEFINE VARIABLE iCnt5   AS INT64     NO-UNDO.
DEFINE VARIABLE cTxt    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTmp    AS CHARACTER NO-UNDO.
DEFINE VARIABLE iTmp    AS INTEGER   NO-UNDO.
DEFINE BUFFER   actr    FOR acct.
DEFINE TEMP-TABLE ttacc     NO-UNDO
    FIELD idx       AS INTEGER
    FIELD cgroup    AS CHARACTER
    FIELD cacct     AS CHARACTER
    FIELD cactr     AS CHARACTER    /* �࠭���� ��� */
    FIELD ccurr     AS CHARACTER
    FIELD ccontr    AS CHARACTER
    FIELD cdet      AS CHARACTER
    FIELD ckl       AS CHARACTER
    FIELD cklname   AS CHARACTER
    FIELD cadress   AS CHARACTER
    FIELD cindex    AS CHARACTER
    FIELD cadrnoi   AS CHARACTER    /* ���� ��� ������ */
    FIELD ckldir    AS CHARACTER
    FIELD dopen     AS DATE
    FIELD dat       AS DATE
    FIELD nposrs    AS DECIMAL
    FIELD c47423    AS CHARACTER
    FIELD npos474   AS DECIMAL
    FIELD bankrot   AS CHARACTER
    .
DEFINE STREAM sletter.
DEFINE STREAM sreestr.
DEFINE STREAM suvedom.

cTyp = GetParamByNameAsChar   (iParam, "type",     "���᮪").
cCat = GetParamByNameAsChar   (iParam, "cust-cat", "�").
iLet = GetParamByNameAsDecimal(iParam, "letters",  50).

/******************************************* �㭪樨  **** */
FUNCTION FromOABS   RETURNS DATE
   (INPUT  iacct    AS CHARACTER ).

   DEFINE VARIABLE mHandle     AS INT64    NO-UNDO.
   DEFINE VARIABLE iStat       AS INT64    NO-UNDO.

   RUN STORED-PROCEDURE GET_LAST_DATEPAY mHandle = PROC-HANDLE
     (INPUT  PARAM P_ACC             = iAcct,
      INPUT  PARAM P_DATELESS        = TODAY,
      OUTPUT PARAM GET_LAST_DATEPAY  = ? ).
   CLOSE STORED-PROC GET_LAST_DATEPAY iStat = PROC-STATUS.

   IF iStat = 0
   THEN RETURN GET_LAST_DATEPAY.
   ELSE RETURN ?.
END FUNCTION.

/* ��⠥� 蠡���� */
FUNCTION TmplImport RETURNS CHARACTER
   (INPUT  iTmplNam AS CHARACTER ).

    DEFINE VARIABLE cTmpl       AS CHARACTER    NO-UNDO.
    DEFINE VARIABLE cTmp        AS CHARACTER    NO-UNDO.

    INPUT FROM VALUE(SEARCH(iTmplNam)).
    cTmpl = "".
    DO WHILE TRUE
        ON ENDKEY UNDO, LEAVE:

        IMPORT UNFORMATTED cTmp.
        cTmpl = cTmpl + cTmp + "~r~n".
    END.
    INPUT  CLOSE.

    RETURN cTmpl.
END FUNCTION.

/* �뢮� ��������� ॥��� � ������� */
PROCEDURE OutReeBeg:
    DEFINE INPUT  PARAMETER iVal    AS CHARACTER    NO-UNDO.    /* ����� ॥��� */

    DEFINE VARIABLE cTmp        AS CHARACTER    NO-UNDO.

    INPUT FROM VALUE(SEARCH("pb_re2g_beg.rtf")).
    DO WHILE TRUE
        ON ENDKEY UNDO, LEAVE:

        IMPORT UNFORMATTED cTmp.
        cTmp = REPLACE(cTmp, "numtabl", iVal).
        PUT STREAM sreestr UNFORMATTED (cTmp + "~r~n").
    END.
    INPUT  CLOSE.
END PROCEDURE.

/* �뢮� 蠡���� 㢥�������� � ������� */
PROCEDURE OutUved:
    DEFINE INPUT  PARAMETER iAdr    AS CHARACTER    NO-UNDO.    /* ����  */
    DEFINE INPUT  PARAMETER iFio    AS CHARACTER    NO-UNDO.    /* ���    */
    DEFINE INPUT  PARAMETER iInd    AS CHARACTER    NO-UNDO.    /* ������ */

    DEFINE VARIABLE cTmp        AS CHARACTER    NO-UNDO.
    DEFINE VARIABLE cTxt        AS LONGCHAR     NO-UNDO INIT "".
    DEFINE VARIABLE I           AS INTEGER      NO-UNDO.

    INPUT FROM VALUE(SEARCH("pb_uv2g_one.rtf")).
    DO WHILE TRUE
        ON ENDKEY UNDO, LEAVE:

        IMPORT UNFORMATTED cTmp.
        cTxt = cTxt + cTmp + "~n".
    END.
    INPUT CLOSE.

    cTxt = REPLACE(cTxt, "#Counter11", STRING(iCnt1)).
    cTxt = REPLACE(cTxt, "#Counter12", STRING(iCnt1 + 1)).
    cTxt = REPLACE(cTxt, "#Counter13", STRING(iCnt1 + 2)).
    cTxt = REPLACE(cTxt, "#Counter21", STRING(iCnt2)).
    cTxt = REPLACE(cTxt, "#Counter22", STRING(iCnt2 + 1)).
    cTxt = REPLACE(cTxt, "#Counter23", STRING(iCnt2 + 2)).
    cTxt = REPLACE(cTxt, "#Counter31", STRING(iCnt3)).
    cTxt = REPLACE(cTxt, "#Counter32", STRING(iCnt3 + 1)).
    cTxt = REPLACE(cTxt, "#Counter33", STRING(iCnt3 + 2)).
    cTxt = REPLACE(cTxt, "#Counter41", STRING(iCnt4)).
    cTxt = REPLACE(cTxt, "#Counter42", STRING(iCnt4 + 1024)).
    cTxt = REPLACE(cTxt, "#Counter43", STRING(iCnt4 + 2048)).
    cTxt = REPLACE(cTxt, "#Counter51", STRING(iCnt5)).
    cTxt = REPLACE(cTxt, "#Counter52", STRING(iCnt5 + 1)).
    cTxt = REPLACE(cTxt, "#Counter53", STRING(iCnt5 + 2)).
    cTxt = REPLACE(cTxt, "#adress", iAdr).
    cTxt = REPLACE(cTxt, "#fio",    iFio).
    cTxt = REPLACE(cTxt, "#1", SUBSTRING(iInd, 1, 1)).
    cTxt = REPLACE(cTxt, "#2", SUBSTRING(iInd, 2, 1)).
    cTxt = REPLACE(cTxt, "#3", SUBSTRING(iInd, 3, 1)).
    cTxt = REPLACE(cTxt, "#4", SUBSTRING(iInd, 4, 1)).
    cTxt = REPLACE(cTxt, "#5", SUBSTRING(iInd, 5, 1)).
    cTxt = REPLACE(cTxt, "#6", SUBSTRING(iInd, 6, 1)).

    DO I = 1 TO NUM-ENTRIES(cTxt, "~n"):
        cTmp = ENTRY(I, cTxt, "~n").
        PUT STREAM suvedom UNFORMATTED (cTmp + "~r~n").
    END.
    ASSIGN
        iCnt1   = iCnt1 + 3
        iCnt2   = iCnt2 + 3
        iCnt3   = iCnt3 + 65536
        iCnt4   = iCnt4 + 4096
        iCnt5   = iCnt5 + 3
        .
END PROCEDURE.

/******************************************* ���᮪ ��⮢ */
FOR EACH acct
    WHERE CAN-DO(IF (cCat EQ "�") THEN "405*,406*,407*,40802*,40821*,40807*" ELSE "40817*,40820*,423*,426*", acct.acct)
      AND (acct.open-date   LE end-date)
      AND (acct.close-date  EQ ?)
      AND (acct.filial-id   EQ shFilial)
      AND NOT (acct.contract BEGINS "�࠭�")
/*    AND CAN-DO("40802810304000030924*,40702810204000004820*", acct.acct)  / * <<<====    ************************************************************************ */
    NO-LOCK
    BY acct.cust-cat
    BY acct.cust-id:

    put screen col 1 row 24 "��ࠡ��뢠���� " + acct.cust-cat + "_" + STRING(acct.cust-id) + " " + STRING(I) + "          ".
    RUN acct-pos IN h_base(acct.acct, acct.currency, acct.open-date, end-date, "�").
    lastmove = IF (lastmove EQ ?) AND (shFilial EQ "0500") THEN FromOABS(acct.acct) ELSE lastmove.
    lastmove = IF (lastmove EQ ?) THEN acct.open-date      ELSE lastmove.
    lastrs   = lastmove.
    lasttr   = lastmove.    /* �᫨ ��� �࠭��⭮�� ��� */
    npostr   = sh-bal.

    IF      (sh-bal EQ 0.00)
        AND (lastrs LE beg-date)
    THEN DO:
        cTranz = "".

        IF      (acct.currency NE "")
            AND (cCat EQ "�")
        THEN DO:
            FIND FIRST actr
                WHERE CAN-DO(SUBSTRING(acct.acct, 1, 8) + "*", actr.acct)
                  AND (actr.currency    EQ acct.currency)
                  AND (actr.open-date   EQ acct.open-date)
                  AND (actr.close-date  EQ ?)
                  AND (actr.cust-cat    EQ acct.cust-cat)
                  AND (actr.cust-id     EQ acct.cust-id)
                  AND (actr.filial-id   EQ shFilial)
                  AND (actr.contract    BEGINS "�࠭�")
                NO-LOCK NO-ERROR.
            IF (AVAIL actr)
            THEN DO:
                RUN acct-pos IN h_base(actr.acct, actr.currency, actr.open-date, end-date, "�").
                lastmove = IF (lastmove EQ ?) AND (shFilial EQ "0500") THEN FromOABS(actr.acct) ELSE lastmove.
                lasttr   = IF (lastmove EQ ?) THEN actr.open-date ELSE lastmove.
                npostr   = sh-bal.

                IF      (npostr EQ 0.00)
                    AND (lasttr LE beg-date)
                THEN RUN Addtt(BUFFER actr, lasttr).

                cTranz = actr.number.
            END.
        END.

        IF      (npostr EQ 0.00)
            AND (lasttr LE beg-date)
        THEN RUN Addtt(BUFFER acct, lastrs).
    END.
END.
put screen col 1 row 24 color normal STRING(" ","X(80)").

/******************************************* ����� */
IF (I = 0)
THEN MESSAGE "�� ������� �� ������ ���."
    VIEW-AS ALERT-BOX MESSAGE BUTTONS OK.
ELSE DO:
    IF (cTyp EQ "���᮪")
    THEN DO:    /*************************** Excel */
        cFl = "./acc" + (IF (cCat EQ "�") THEN "u" ELSE "f") + "l-"
            + STRING(YEAR(end-date)) + STRING(MONTH(end-date), "99") + STRING(DAY(end-date), "99") + "-" + shFilial + ".xml".
        OUTPUT TO VALUE(cFl).

        IF (shFilial EQ "0500")
        THEN PUT UNFORMATTED XLHead("acct", "CCDCDCCCNCN", "53,65,114,213,71,87,86,350,64,150,79").
        ELSE PUT UNFORMATTED XLHead("acct", "CDCDCCCNCN", "65,114,213,71,87,86,350,64,150,79").

        cXL = (IF (shFilial EQ "0500") THEN XLCellHead("��㯯�",0,0,0) ELSE "")
            + XLCellHead("��� ������",0,0,0)
            + XLCellHead("��� ��᫥���� ����樨",0,0,0)
            + XLCellHead("����� ���",0,0,0)
            + XLCellHead("��� ������",0,0,0)
            + XLCellHead("�����祭��",0,0,0)
            + XLCellHead("�����஢��",0,0,0)
            + XLCellHead("������������",0,0,0)
            + XLCellHead("���⮪ �� ���",0,0,0)
            + (IF (cCat EQ "�") THEN XLCellHead("��� 47423 �� ���",0,0,0) ELSE "")
            + (IF (cCat EQ "�") THEN XLCellHead("���⮪ �� 47423",0,0,0)  ELSE "")
            .
        PUT UNFORMATTED XLRow(0) cXL XLRowEnd().

        FOR EACH ttacc
            BY ttacc.cgroup
            BY ttacc.idx:

            cXL = (IF (shFilial EQ "0500") THEN XLCell(ttacc.cgroup) ELSE "")
                + XLCell(ttacc.ckl)
                + XLDateCell(ttacc.dat)
                + XLCell(ttacc.cacct)
                + XLDateCell(ttacc.dopen)
                + XLCell(ttacc.ccontr)
                + XLCell(ttacc.bankrot)
                + XLCell(ttacc.cdet)
                + XLNumCell(ttacc.nposrs)
                + (IF (ttacc.c47423 EQ "") THEN "" ELSE XLCell(ttacc.c47423))
                + (IF (ttacc.c47423 EQ "") THEN "" ELSE XLNumCell(ttacc.npos474))
                .
            PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
        END.

        PUT UNFORMATTED XLEnd().
        OUTPUT CLOSE.

        /* ��। ��ࠢ��� ��⮪��� �஢�ਬ, ����饭 �� bispc */
        DEFINE VARIABLE mRet        AS CHARACTER    NO-UNDO INIT "".
        DO WHILE (mRet EQ ""):
            RUN IsUserServReady IN h_netw ("", OUTPUT mRet).
            IF (mRet EQ "")
            THEN MESSAGE "������� �ணࠬ�� bispc � ������ ��" VIEW-AS ALERT-BOX.
        END.
        /* ��ࠢ�塞 ��⮪�� */
        RUN sndbispc.p ("file=" + cFl + ";class=bq").
    END.
    ELSE DO:    /*************************** ���쬠 */
        Iost = 0.
        I    = iLet.    /* ���稪 ��ᥬ � ����� ॥��� */
        J    = 0.       /* ���稪 ॥��஢ */
        FOR EACH ttacc
            WHERE NOT (ttacc.ccontr BEGINS "�࠭�")
              AND (ttacc.bankrot    EQ "")
            NO-LOCK:

            Iost = Iost + 1.
        END.

        /* ��⠥� 蠡���� */
        cTmpl1 = TmplImport("pb_2goda-1.rtf").
        cTmpl2 = TmplImport("pb_2goda-2.rtf").
        cRee_s = TmplImport("pb_re2g_str.rtf").
        cRee_e = TmplImport("pb_re2g_end.rtf").
        cUvd_b = TmplImport("pb_uv2g_beg.rtf").
        cUvd_p = TmplImport("pb_uv2g_page.rtf").
        cUvd_e = TmplImport("pb_uv2g_end.rtf").

        FOR EACH ttacc
            WHERE NOT (ttacc.ccontr BEGINS "�࠭�")
              AND (ttacc.bankrot    EQ "")
            BREAK BY ttacc.cgroup
                  BY ttacc.idx:

            IF (I EQ iLet)  /* ��稭��� ���� ॥��� */
            THEN DO:
                Nree = IF (Iost < iLet) THEN Iost ELSE iLet.
                Iost = Iost - iLet.
                I    = 0.
                J    = J + 1.
                Iu   = 0.
                cFl  = (IF (cCat EQ "�") THEN "ul-" ELSE "fl-")
                     + STRING(YEAR(end-date)) + STRING(MONTH(end-date), "99") + STRING(DAY(end-date), "99")
                     + "-" + shFilial + "-" + STRING(J) + ".rtf".
                OUTPUT STREAM sletter TO VALUE("./acc-" + cFl).
                OUTPUT STREAM sreestr TO VALUE("./ree-" + cFl).
                OUTPUT STREAM suvedom TO VALUE("./uvd-" + cFl).
                PUT STREAM sletter UNFORMATTED cTmpl1.
                RUN OutReeBeg(STRING(Nree)).
                iCnt1   = 0.
                iCnt2   = 1026.
                iCnt3   = 65536.
                iCnt4   = 251658240.
                iCnt5   = 8192.
                PUT STREAM suvedom UNFORMATTED cUvd_b.
            END.

            I    = I  + 1.
            Iu   = Iu + 1.

            /* ���쬮 */
            cTxt = cTmpl2.
            cTmp = STRING(DAY(end-date)) + " "
                 + ENTRY(MONTH(end-date),"ﭢ���,䥢ࠫ�,����,��५�,���,���,���,������,ᥭ����,������,�����,�������") + " "
                 + STRING(YEAR(end-date)) + "�".
            cTxt = REPLACE(cTxt, "ltrdate",     CODEPAGE-CONVERT(cTmp, "1251")).
            cTxt = REPLACE(cTxt, "klname",      CODEPAGE-CONVERT(ttacc.cklname, "1251")).
            cTxt = REPLACE(cTxt, "kladress",    CODEPAGE-CONVERT(ttacc.cadress, "1251")).
            cTxt = REPLACE(cTxt, "kldirektor",  CODEPAGE-CONVERT(ttacc.ckldir,  "1251")).
            cTmp = IF (ttacc.cactr EQ "") THEN " ���" ELSE " � �࠭��⭮� ����".
            cTmp = ENTRY(LOOKUP(ttacc.ccontr, "�����,�����,�����,dps,���ப"),
                   "����饬" + cTmp + ",������⭮� ���,����⭮� ���,��� ������,���樠�쭮� �ப��᪮�" + cTmp) NO-ERROR.
            cTxt = REPLACE(cTxt, "acccontr",    CODEPAGE-CONVERT(cTmp, "1251")).
            cTxt = REPLACE(cTxt, "accnumber",   SUBSTRING(ttacc.cacct, 1, 20)
                         + IF (ttacc.cactr EQ "") THEN "" ELSE (", " + ttacc.cactr)).
            cTmp = GetXAttrValue("acct", ttacc.cacct + "," + ttacc.ccurr, "��������").
            cTmp = IF (NUM-ENTRIES(cTmp) LT 2) THEN cTmp ELSE (TRIM(ENTRY(2, cTmp)) + " �� " + STRING(DATE(ENTRY(1, cTmp)), "99.99.9999") + "�") NO-ERROR.
            cTmp = IF (cTmp EQ "") THEN (SUBSTRING(ttacc.cacct, 1, 20) + " �� " + STRING(ttacc.dopen, "99.99.9999") + "�") ELSE cTmp.
            cTxt = REPLACE(cTxt, "accdogovor",  CODEPAGE-CONVERT(cTmp, "1251")).
            PUT STREAM sletter UNFORMATTED cTxt.

            /* ��ப� ॥��� */
            cTxt = REPLACE(cRee_s, "numstr",       STRING(I)).
            cTxt = REPLACE(cTxt,   "klientfio",    CODEPAGE-CONVERT(ttacc.cklname, "1251")).
            cTxt = REPLACE(cTxt,   "klientadress", " " + CODEPAGE-CONVERT(ttacc.cadress, "1251")).
            PUT STREAM sreestr UNFORMATTED cTxt.

            /* ����������� */
            cTxt   = ttacc.cadress.
            cTmp   = ENTRY(1, cTxt).
            iTmp   = INTEGER(cTmp) NO-ERROR.
            IF (LENGTH(cTmp) = 6) AND NOT ERROR-STATUS:ERROR
            THEN cTxt = TRIM(SUBSTRING(cTxt, 8)).   /* ���� ��� ������ */
            ELSE cTmp = "".                         /* ������ �訡���  */
            RUN OutUved(CODEPAGE-CONVERT(ttacc.cklname, "1251"), CODEPAGE-CONVERT(cTxt, "1251"), cTmp).

            IF (I EQ iLet) OR LAST(ttacc.idx)  /* ����뢠�� 䠩�� */
            THEN DO:
                PUT STREAM sletter UNFORMATTED "}~r~n ".
                PUT STREAM sreestr UNFORMATTED REPLACE(cRee_e, "numtabl", STRING(Nree)).
                PUT STREAM suvedom UNFORMATTED cUvd_e.
                OUTPUT STREAM sletter CLOSE.
                OUTPUT STREAM sreestr CLOSE.
                OUTPUT STREAM suvedom CLOSE.
            END.
            ELSE DO:
                PUT STREAM sletter UNFORMATTED "~\cf0~\page~r~n".

                IF (Iu EQ 3)
                THEN DO:
                    Iu  = 0.
                    PUT STREAM suvedom UNFORMATTED cUvd_p.
                END.
            END.
        END.

        MESSAGE "��ନ஢��� " J " 䠩��� � ���쬠��."
            VIEW-AS ALERT-BOX MESSAGE BUTTONS OK.
    END.
END.
{intrface.del}

/* ��楤���  ***************************************************************** */
PROCEDURE Addtt:
    DEFINE PARAMETER BUFFER ac      FOR acct.
    DEFINE INPUT PARAMETER  lmdat   AS DATE     NO-UNDO.

    DEFINE VARIABLE cN1     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cN2     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cINN    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLink   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCred   AS CHARACTER NO-UNDO INIT "".
    DEFINE VARIABLE iIndex  AS INTEGER   NO-UNDO.
    DEFINE BUFFER   bAc     FOR acct.
    DEFINE BUFFER   vAc     FOR acct.
    DEFINE BUFFER   sroch   FOR loan-acct.

    IF (ac.cust-cat EQ "�") AND (cCat NE "�")
    THEN DO:
        FOR EACH bAc
            WHERE CAN-DO("455*,458*,459*", bAc.acct)
              AND (bAc.close-date   EQ ?)
              AND (bAc.cust-cat     EQ "�")
              AND (bAc.cust-id      EQ ac.cust-id)
            NO-LOCK:

            RUN acct-pos IN h_base(bAc.acct, bAc.currency, end-date, end-date, "�").
            IF (sh-bal NE 0.00)
            THEN DO:
                cCred = "�।��".
                LEAVE.
            END.
        END.
    END.

    RUN GetCustName IN h_base (ac.cust-cat, ac.cust-id, ?, OUTPUT cN1, OUTPUT cN2, INPUT-OUTPUT cINN).
    cLink = GetLinks("acctb", ac.acct + "," + ac.currency, "S", "acct47423", ",", ?).
    IF (cLink NE "")
    THEN RUN acct-pos IN h_base(ENTRY(1, cLink), ENTRY(2, cLink), end-date, end-date, "�").

    CREATE ttacc.
    ASSIGN
        I               = I + 1
        ttacc.idx       = I
        ttacc.cacct     = ac.acct
        ttacc.ccurr     = ac.currency
        ttacc.cgroup    = IF (shFilial EQ "0500") THEN GetXAttrValue("acct", ttacc.cacct + "," + ttacc.ccurr, "groupOABS") ELSE ""
        ttacc.ccontr    = ac.contract
        ttacc.ckl       = ac.cust-cat + "_" + STRING(ac.cust-id)
        ttacc.dat       = lmdat
        ttacc.cactr     = cTranz
        ttacc.cklname   = TRIM(cN1 + " " + cN2)
        ttacc.cdet      = IF {assigned ac.details} THEN ac.details ELSE ttacc.cklname
        ttacc.cadress   = KlientAdr(ac.cust-cat, ac.cust-id, "�")
        ttacc.cadress   = IF (ttacc.cadress EQ "") THEN KlientAdr(ac.cust-cat, ac.cust-id, "�") ELSE ttacc.cadress
        ttacc.cadress   = IF (ttacc.cadress EQ "") THEN KlientAdr(ac.cust-cat, ac.cust-id, "�") ELSE ttacc.cadress
        ttacc.cadress   = REPLACE(ttacc.cadress, ",", ", ")
        ttacc.ckldir    = IF (ac.cust-cat  EQ "�") THEN GetXAttrValue("cust-corp", STRING(ac.cust-id), "�����") ELSE ttacc.cklname
        ttacc.dopen     = ac.open-date
        ttacc.c47423    = SUBSTRING(cLink, 1, 20)
        ttacc.npos474   = IF (cLink EQ "") THEN 0.00 ELSE sh-bal
        ttacc.bankrot   = BlockAcct(ac.acct + "," + ac.currency, end-date)
        ttacc.bankrot   = IF CAN-DO(ttacc.bankrot, "������") THEN "������" ELSE (
                          IF CAN-DO(ttacc.bankrot, "������")  THEN "������"  ELSE cCred)
        .
    /* ����� ������� ? */
    IF (ac.acct BEGINS "40817")
    THEN DO:
        FOR EACH bAc
            WHERE CAN-DO("45509*", bAc.acct)
              AND (bAc.close-date   = ?)
              AND (bAc.cust-cat     = ac.cust-cat)
              AND (bAc.cust-id      = ac.cust-id)
            NO-LOCK:

            RUN acct-pos IN h_base(bAc.acct, bAc.currency, end-date, end-date, "�").
            IF (sh-bal = 0.00)
            THEN DO:
                FOR EACH vAc
                    WHERE CAN-DO("91317*", vAc.acct)
                      AND (vAc.close-date   = ?)
                      AND (vAc.cust-cat     = ac.cust-cat)
                      AND (vAc.cust-id      = ac.cust-id)
                    NO-LOCK:

                    RUN acct-pos IN h_base(vAc.acct, vAc.currency, end-date, end-date, "�").
                    IF (sh-bal <> 0.00)
                    THEN ttacc.bankrot = "����".
                END.
            END.
        END.
    END.

    /* ���� �� ���� ����� ? */
    IF CAN-DO("42301*,42601*", ac.acct)
    THEN DO:
        FOR EACH loan-acct
            WHERE (loan-acct.acct       = ac.acct)
              AND (loan-acct.currency   = ac.currency)
            NO-LOCK,
        EACH loan OF loan-acct
            WHERE (loan.close-date      = ?)
            NO-LOCK,
        EACH sroch OF loan
            WHERE CAN-DO("!42301*,!42601*,4230*,4260*", sroch.acct)
            NO-LOCK,
        FIRST bAc
            WHERE (bAc.acct             = sroch.acct)
              AND (bAc.currency         = sroch.currency)
              AND (bAc.close-date       = ?)
            NO-LOCK:

            ttacc.bankrot = "�����".
            LEAVE.
        END.
    END.

    RUN acct-pos IN h_base(ac.acct, ac.currency, end-date, end-date, "�").
    ttacc.nposrs    = - sh-bal.
END PROCEDURE.
