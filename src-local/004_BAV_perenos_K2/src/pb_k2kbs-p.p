/**
����᪨� �ࠢ� �ਭ�������: ��� "���� ����"
�� ������:     �ਭ㤨⥫�� ��७�� ���㬥�⮢ �2 - ���
��ࠬ����:      
���� ����᪠:  ���� Ctrl-G  ��⮢
������:         12.04.2016 ���ᮢ �.�.
*/

{globals.i}             /** �������� ��।������ */
{intrface.get tmess}
{tmprecid.def}
{sh-defs.i}             /* ���⮪ �� ��� */
{topkind.def}
{pb_logit.i}            /* ������ � ��� */
{pb_k2init.i}

DEFINE VARIABLE cAcct       AS CHARACTER    NO-UNDO INIT "".    /* �/� */
DEFINE VARIABLE cCurr       AS CHARACTER    NO-UNDO.            /* ��� */
DEFINE VARIABLE cAcctK2     AS CHARACTER    NO-UNDO.            /* K2  */
DEFINE VARIABLE cAcctKbs    AS CHARACTER    NO-UNDO.            /* ��� */
DEFINE VARIABLE nK2Pos      AS DECIMAL      NO-UNDO INIT 0.     /* ���⮪ �� �2 */
DEFINE VARIABLE nKbsPos     AS DECIMAL      NO-UNDO INIT 0.     /* ���⮪ �� ��� */
DEFINE VARIABLE cKrcv       AS CHARACTER    NO-UNDO INIT "".    /* ����⥪� - �����⥫� */
DEFINE VARIABLE cDet        AS CHARACTER    NO-UNDO.            /* �����祭�� ���⥦� ���.���. */
DEFINE VARIABLE cTranz      AS CHARACTER    NO-UNDO.            /* ����� ��-樨 sbk_2kbs */
DEFINE VARIABLE lOk         AS LOGICAL      NO-UNDO.
DEFINE VARIABLE cErrMsg     AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cMess       AS CHARACTER    NO-UNDO.
DEFINE VARIABLE iNum        AS INTEGER      NO-UNDO.            /* ���稪 ��७�ᥭ��� ���㬥�⮢ */
DEFINE BUFFER ac     FOR acct.

cMess = "====== ��७�� ���㬥�⮢ �2 - ���".
RUN LogIt(cMess, cLog).

FOR FIRST tmprecid 
    NO-LOCK,
FIRST acct
    WHERE (RECID(acct) EQ tmprecid.id)
    NO-LOCK:

    cMess = "  �⬥祭 ��� " + acct.acct.
    RUN LogIt(cMess, cLog).

    IF (acct.acct BEGINS "9090")
    THEN DO:    /* ��࠭ ��� ����⥪� */
        IF NOT CAN-DO("����2,���⁫", acct.contract)
        THEN DO:
            cMess = "��࠭�� ��� �� ���� �2 ��� ��� !".
            RUN LogIt("???  " + cMess, cLog).
            MESSAGE cMess VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            RETURN.
        END.

        RUN acct-pos IN h_base(acct.acct, acct.currency, TODAY, TODAY, "�").
        IF (sh-bal EQ 0)
        THEN DO:
            cMess = "�� ��࠭��� ����⥪� ��� ���㬥�⮢ !".
            RUN LogIt("???  " + cMess, cLog).
            MESSAGE cMess VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            RETURN.
        END.

        cKrcv = ENTRY(LOOKUP(acct.contract, "����2,���⁫"), "���,�2").

        FOR EACH signs
            WHERE (signs.file-name   EQ "acct")
              AND (signs.code        EQ ENTRY(LOOKUP(acct.contract, "����2,���⁫"), "����2�����,���⁂����"))
              AND (IF (acct.contract EQ "����2") THEN
                  (signs.xattr-value EQ (acct.acct + "," + acct.currency)) ELSE
                  (signs.code-value  EQ (acct.acct + "," + acct.currency)))
            NO-LOCK,
        FIRST ac
            WHERE (ac.acct           EQ ENTRY(1, signs.surrogate))
              AND (ac.currency       EQ ENTRY(2, signs.surrogate))
              AND (ac.close-date     EQ ?)
            NO-LOCK:

            cAcct = ac.acct.
            cCurr = ac.currency.
        END.

        IF (cAcct EQ "")
        THEN DO:
            cMess = "��࠭��� ����⥪� �� �ਢ易�� �� � ������ �/� !".
            RUN LogIt("???  " + cMess, cLog).
            MESSAGE cMess VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            RETURN.
        END.
    END.
    ELSE DO:    /* ��࠭ �/� */
        cAcct = acct.acct.
        cCurr = acct.currency.
    END.
END.

/* �饬 �����஢�� ������ ��� ������ ***************************************** */
FIND FIRST blockobject
    WHERE (blockobject.file-name     EQ 'acct')
      AND (blockobject.class-code    EQ 'BlockAcct')
      AND CAN-DO("������,������", blockobject.block-type)
      AND (blockobject.beg-datetime  LE DATETIME(TODAY, MTIME))
      AND ((blockobject.end-datetime EQ ?)
        OR (blockobject.end-datetime GE DATETIME(TODAY, MTIME)))
      AND (blockobject.surrogate     EQ cAcct + "," + cCurr)
    NO-LOCK NO-ERROR.
IF (NOT AVAIL blockobject)
THEN DO:
    cMess = "�� �/� " + DelFilFromAcct(cAcct) + " ���������� �����஢�� ������ ��� ������ !".
    RUN LogIt("???  " + cMess, cLog).
    MESSAGE cMess VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    RETURN.
END.

cAcctK2  = ENTRY(1, GetXAttrValue("acct", cAcct + "," + cCurr, "����2�����")).
cAcctKbs = ENTRY(1, GetXAttrValue("acct", cAcct + "," + cCurr, "���⁂����")).

IF (cAcctK2 EQ "")
THEN DO:
    cMess = "�� �/� " + DelFilFromAcct(cAcct) + " ��������� ����⥪� 2 !".
    RUN LogIt("???  " + cMess, cLog).
    MESSAGE cMess VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    RETURN.
END.

/* �᫨ ��� ���, � ��७�� � �2 �� ��� */
IF (cAcctKbs EQ "")
THEN cKrcv = "���".
ELSE DO:
    RUN acct-pos IN h_base(cAcctKbs, "", TODAY, TODAY, "�").
    nKbsPos = sh-bal.
END.

RUN acct-pos IN h_base(cAcctK2, "", TODAY, TODAY, "�").
nK2Pos = sh-bal.

IF (nK2Pos + nKbsPos EQ 0)
THEN DO:
    cMess = "�� ����⥪�� �/� " + DelFilFromAcct(cAcct) + " ��� ���㬥�⮢ !".
    RUN LogIt("???  " + cMess, cLog).
    MESSAGE cMess VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    RETURN.
END.

IF (cKrcv EQ "")
THEN DO:
    IF (nKbsPos EQ 0)
    THEN cKrcv = "���".
    IF (nK2Pos  EQ 0)
    THEN cKrcv = "�2".

    IF (cKrcv EQ "")
    THEN DO:            /* ���訢���, ��㤠 � �㤠 ��७���� */
        RUN Fill-SysMes IN h_tmess ("", "", "3",
            "� ����� ���ࠢ����� �㤥� ��७���� ���㬥���?|� �2 �� ���,� ��� �� �2").
        CASE pick-value:
            WHEN "0" THEN RETURN.
            WHEN "1" THEN cKrcv = "���".
            WHEN "2" THEN cKrcv = "�2".
        END CASE.
    END.
END.

/* ����砥� ���㬥��� �� ��室��� ����⥪� *********************************** */
cMess = "    �⮡ࠦ��� �����⨪� " + cKrcv.
RUN LogIt(cMess, cLog).
cAcctK2 = IF (cKrcv EQ "�2") THEN cAcctKbs ELSE cAcctK2.
run "crd.p" (cAcctK2, "", 4).
iNum = 0.

FOR EACH tmprecid 
    NO-LOCK,
FIRST kau
    WHERE (RECID(kau) EQ tmprecid.id)
    NO-LOCK:

    cDet = GetXAttrValue("op", ENTRY(1, kau.kau), "op-bal").    /* ��室�� ���㬥��, ���⠢����� �� ����⥪� */
    IF (cDet NE "")
    THEN DO:
        FIND FIRST op
            WHERE (op.op    EQ INT64(cDet))
            NO-LOCK NO-ERROR.

        IF (AVAIL op)
        THEN cDet = op.details.
        ELSE cDet = "".
    END.

    /* ����� �࠭���樨  sbk_2kbs - SB: ��६�饭�� �2 <--> ��� */
    cTranz = "sbk_2kbs".
    {empty tOpKindParams}     /* ������ ⠡���� ��ࠬ��஢ */
    ASSIGN
        lOk =   TDAddParam("iSrcCrt"   , cAcctK2)
            AND TDAddParam("iCrtType"  , cKrcv)
            AND TDAddParam("iAcct"     , cAcct)
            AND TDAddParam("iCurrency" , cCurr)
            AND TDAddParam("iKau"      , kau.kau)
            AND TDAddParam("iDetails"  , cDet)
            AND TDAddParam("iUser"     , USERID('bisquit'))
            AND TDAddParam("iLog"      , cLog)
        NO-ERROR.
    IF NOT lOk
    THEN DO:
        cMess = "�訡�� ��।�� ��ࠬ��஢ � �࠭����� " + cTranz.
        RUN LogIt("???  " + cMess, cLog).
        MESSAGE cMess VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    END.
    ELSE DO:
        RUN ex-trans.p (cTranz, TODAY, TABLE tOpKindParams, OUTPUT lOk, OUTPUT cErrMsg).
        IF NOT lOk
        THEN DO:
            cMess = cErrMsg.
            RUN LogIt("???  " + cMess, cLog).
            MESSAGE cMess VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        END.
        ELSE DO:
            cMess = "    ���㬥�� ��७�ᥭ �� " + cKrcv.
            RUN LogIt(cMess, cLog).
            iNum  = iNum + 1.
        END.
    END.
END.

cMess = "��७�ᥭ� " + STRING(iNum) + " ���㬥�⮢ �� " + cKrcv.
RUN LogIt(cMess, cLog).
MESSAGE cMess VIEW-AS ALERT-BOX MESSAGE BUTTONS OK.
