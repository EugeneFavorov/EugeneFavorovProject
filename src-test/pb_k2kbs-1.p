/**
����᪨� �ࠢ� �ਭ�������: ��� "���� ����"
�᭮�����:      
�� ������:     ��楤�� �ॣ㫨஢���� �2 - ��� �� ����� �/�
��� ࠡ�⠥�:   
��ࠬ����:      <�/�>
���� ����᪠:  
������:         25.03.2016 ���ᮢ �.�.
*/

{globals.i}             /* �������� ��६���� ��ᨨ. */
{topkind.def}           /* ����� �� �� ��楤��� */
{sh-defs.i}             /* ���⮪ �� ��� */
{pb_logit.i}            /* ������ � ��� */

DEFINE INPUT        PARAM iAcct    AS CHARACTER NO-UNDO.    /* ��楢�� ��� */
DEFINE INPUT        PARAM iCurr    AS CHARACTER NO-UNDO.    /* ����� �/�   */
DEFINE INPUT        PARAM iPrtAll  AS CHARACTER NO-UNDO.    /* ������ ��� ���ଠ�� - "��", ��� ⮫쪮 ��������� - "���" */
DEFINE INPUT        PARAM iUser    AS CHARACTER NO-UNDO.    /* ���짮��⥫�, �� �쥣� ����� ᮧ������ �஢���� */
                                                            /* �᫨ 㪠��� => �� �����஢騪� ��� ������       */
DEFINE INPUT        PARAM iLog     AS CHARACTER NO-UNDO.    /* ���-䠩�     */
DEFINE INPUT        PARAM iPrt     AS CHARACTER NO-UNDO.    /* ��⮪��     */
DEFINE INPUT-OUTPUT PARAM lFirst   AS LOGICAL   NO-UNDO.    /* ���� ���? */

/* �஢�ન *************************************************************************************** */
DEFINE VARIABLE cMess       AS CHARACTER NO-UNDO.   /* ����饭�� � ��� */
IF (iCurr NE "") THEN RETURN.   /* <<< =============  ���� ������ ��� �ய�᪠�� ================================================================ */

FIND FIRST acct
    WHERE (acct.acct        = iAcct)
      AND (acct.currency    = iCurr)
    NO-LOCK NO-ERROR.
IF (acct.cust-cat = "�") AND NOT CAN-DO("40802*,40821*", iAcct)
THEN DO:
    cMess = "???   �����㦥�� ����⥪� �� ��� �� : " + iAcct.
    RUN LogIt(cMess, iLog).
    RUN pb_mail.p ("a.borisov,y.tsisnevich", "����⥪� �� ��� �� : " + SUBSTRING(iAcct,1,20), cMess, "").
    RETURN.
END.
RELEASE acct.

/* ��६����  ************************************************************************************ */
DEFINE VARIABLE cUser       AS CHARACTER NO-UNDO.   /* ���짮��⥫� */
DEFINE VARIABLE cBlOrdL     AS CHARACTER NO-UNDO.   /* �����஢�� �� �/� */
DEFINE VARIABLE lArest      AS LOGICAL   NO-UNDO.   /* ���� �����஢�� ���� */
DEFINE VARIABLE cAllOrd     AS CHARACTER NO-UNDO.   /* ���᮪ �����஢�� �� �/� ��� ��⮪��� */
DEFINE VARIABLE cAcctK2     AS CHARACTER NO-UNDO.   /* ����⥪� K2 */
DEFINE VARIABLE cCurrK2     AS CHARACTER NO-UNDO.   /* ����� K2 - �ᥣ�� �㡫� */
DEFINE VARIABLE cAcctKbs    AS CHARACTER NO-UNDO.   /* ����⥪� ��� */
DEFINE VARIABLE cCurrKbs    AS CHARACTER NO-UNDO.   /* ����� ��� - �ᥣ�� �㡫� */
DEFINE VARIABLE nAcPos      AS DECIMAL   NO-UNDO.   /* ���⮪ �� �/� (� ��⮬ �������襭��� ᯨᠭ�� - �) */
DEFINE VARIABLE nAcPosTmp   AS DECIMAL   NO-UNDO.   /* ���⮪ �� ᯨᠭ�� */
DEFINE VARIABLE nK2Pos      AS DECIMAL   NO-UNDO.   /* ���⮪ �� �2 */
DEFINE VARIABLE nKbsPos     AS DECIMAL   NO-UNDO.   /* ���⮪ �� ��� */
DEFINE VARIABLE cTranz      AS CHARACTER NO-UNDO.   /* ����� ��-権: ��� ��-樨 */
DEFINE VARIABLE lOk         AS LOGICAL   NO-UNDO.   /* ����� ��-権: ���.��ࠬ��� */
DEFINE VARIABLE cErrMsg     AS CHARACTER NO-UNDO.   /* ����� ��-権: �訡�� */
DEFINE VARIABLE nPorog      AS DECIMAL   NO-UNDO.   /* ��ண ������ = 0 ��� �2, blocksum ��� ��� */
DEFINE VARIABLE cPartial    AS CHARACTER NO-UNDO.   /* ����筠� ����� */
DEFINE VARIABLE cOp         AS CHARACTER NO-UNDO.   /* ���㬥�� ������  */

/* �६����� ⠡��� ��� �࠭���� ���㬥�⮢ �� ����⥪�� */
DEFINE TEMP-TABLE ttKauCrt  NO-UNDO
    FIELD crt       AS CHARACTER        /* ��� ����⥪�: K2 ��� ��� */
    FIELD crtn      AS CHARACTER        /* ����⥪� ��᫥ ��७��  */
    FIELD kau       AS CHARACTER        /* ���㬥�� �� ����⥪� <op>,<oe> */
    FIELD sort      AS CHARACTER        /* ����஢�� � ����⥪� <�ਮ���>,<���>,<�㬬�> = kau.sort */
    FIELD opdate    AS DATE             /* ��� ���⠭���� �� ����⥪� */
    FIELD numop     AS INTEGER          /* = kau.numop */
    FIELD balance   AS DECIMAL          /* ���� �㬬� ���㬥��, ����� �� ����⥪ = kau.balance */
    FIELD firstsum  AS DECIMAL          /* �㬬� ���⠭���� �� ����⥪� */
    FIELD summa     AS DECIMAL          /* �㬬� ������ (�.�.���筮�) */
    FIELD order-pay AS INTEGER          /* ��।����� ���.���. ��� ��७��(5(�����) = 3) */
    FIELD order-sym AS CHARACTER        /* ��।����� ���.���. ��� ��⮪���  */
    FIELD order-new AS INTEGER          /* ��।����� ���.���. ��� ������: (4-5(�����),5-4,6-5) */
    FIELD details   AS CHARACTER        /* �����祭�� ���⥦� ���.���. */
    FIELD action    AS CHARACTER        /* ����⢨�: ��६�饭�� / ����� */
    FIELD eerror    AS CHARACTER        /* �訡�� */
    FIELD block     AS CHARACTER        /* ��� �����஢�� ��� ���㬥�� �� cBlOrdL */
    FIELD blocksum  AS DECIMAL          /* �㬬� �����஢�� �� �����㬬 */
    FIELD priost    AS LOGICAL          /* ���㬥�� �ਮ�⠭����� �� �2 */
    FIELD oplata    AS CHARACTER        /* op ���㬥�� ������ */
    FIELD comissia  AS DECIMAL          /* �������, ���᫥���� �� ���㬥�� ������ */
    .

{pb_k2kbs.pro}

cMess = USERID("bisquit") + "  == �ॣ㫨஢���� �2 - ��� �� ��� " + iAcct.
RUN LogIt(cMess, iLog).
cUser = IF (iUser EQ "") THEN USERID('bisquit') ELSE iUser.

/* ���⮪ �� �/� (� ��⮬ �������襭��� ᯨᠭ��) */
RUN acct-pos IN h_base(iAcct, iCurr, TODAY, TODAY, "�").
nAcPos    = IF (iCurr EQ "") THEN (sh-db - sh-bal) ELSE (sh-vdb - sh-val).
RUN acct-pos IN h_base(iAcct, iCurr, TODAY, TODAY, "�").
nAcPos    = nAcPos - (IF (iCurr EQ "") THEN sh-db ELSE sh-vdb).
nAcPos    = IF (nAcPos < 0.00) THEN 0.00 ELSE nAcPos.           /* ���쪮 ��� ���� */
cMess = "  �ࠢ���� ���⮪ �� ��� = " + STRING(nAcPos, "->>,>>>,>>>,>>9.99").
RUN LogIt(cMess, iLog).

nAcPosTmp    = - (IF (iCurr EQ "") THEN sh-bal ELSE sh-val).
IF (nAcPosTmp NE nAcPos)
THEN DO:
    cMess = "  ����� �ᯮ�짮���� ���⮪ = " + STRING(nAcPosTmp, "->>,>>>,>>>,>>9.99").
    RUN LogIt(cMess, iLog).
    cMess = "  ���宦�����                 = " + STRING(nAcPosTmp - nAcPos, "->>,>>>,>>>,>>9.99").
    RUN LogIt(cMess, iLog).
END.
nAcPosTmp = nAcPos.

/* �����஢�� �� �/� ********************************************************** */
cBlOrdL = BlockArr(iAcct + "," + iCurr).
cAllOrd = ENTRY(2, cBlOrdL, ";").
cBlOrdL = ENTRY(1, cBlOrdL, ";").
lArest  = INDEX(cAllOrd, "����") NE 0.
cMess = "  - �����஢�� �� ��� " + cBlOrdL.
RUN LogIt(cMess, iLog).

/* ����⥪� */
nK2Pos      = 0.
cCurrK2     = "".
cAcctK2     = GetXAttrValue("acct", iAcct + "," + iCurr, "����2�����").
cMess = "  - �2  = " + cAcctK2.
RUN LogIt(cMess, iLog).
IF (cAcctK2 NE "")
THEN DO:
    ASSIGN
        cCurrK2     = ENTRY(2, cAcctK2).
        cAcctK2     = ENTRY(1, cAcctK2).
        .
    RUN acct-pos IN h_base(cAcctK2, cCurrK2, TODAY, TODAY, "�").
    nK2Pos  = (IF (cCurrK2 EQ "") THEN sh-bal ELSE sh-val).
END.

{empty ttKauCrt}

/* ������㥬 �2 ��� ��७�� �� ��� ******************************************* */
IF (nK2Pos GT 0)    /* �᫨ ���� ���㬥��� �� �2 */
THEN DO:
    /* ��� ������ �����⨪� �2 */
    FOR EACH kau
        WHERE (kau.acct         EQ cAcctK2)
          AND (kau.currency     EQ cCurrK2)
          AND (kau.zero-bal     EQ no)
        NO-LOCK,
    FIRST op
        WHERE (op.op            EQ INT64(ENTRY(1, kau.kau)))  /* �ய�᪠�� ��� ��� ���㬥�⮢ */
        NO-LOCK,
    FIRST op-entry OF op
        NO-LOCK
        BY kau.sort
        BY op.op:

        RUN New_ttKauCrt("�2").
    END.

    FOR EACH ttKauCrt
        WHERE (ttKauCrt.crt EQ "�2")
        BY ttKauCrt.order-pay
        BY ttKauCrt.sort:

        IF       NOT (cBlOrdL BEGINS "������")                  /* �� ������᭮� �ந�����⢥ � �������⢥... */
            AND  NOT (cBlOrdL BEGINS "������")                 /*     �� ��⠢�塞 �� ᢮�� �����            */
            AND (NOT ttKauCrt.priost AND (ttKauCrt.block NE "") /* �� �ਮ�⠭����� � ���� �����஢�� */
                  OR ttKauCrt.priost AND lArest)                /* ��� �ਮ�⠭�����, �� ���� ����   */
        THEN DO:

            cMess = "  - ��७�� �� ���".
            RUN LogIt(cMess, iLog).
            /* ����� �࠭���樨  sbk_2kbs - SB: ��६�饭�� �2 <--> ��� */
            cTranz = "sbk_2kbs".
            {empty tOpKindParams}     /* ������ ⠡���� ��ࠬ��஢ */
            ASSIGN
                lOk =   TDAddParam("iSrcCrt"   , cAcctK2)
                    AND TDAddParam("iCrtType"  , "���")
                    AND TDAddParam("iAcct"     , iAcct)
                    AND TDAddParam("iCurrency" , iCurr)
                    AND TDAddParam("iKau"      , ttKauCrt.kau)
                    AND TDAddParam("iDetails"  , ttKauCrt.details)
                    AND TDAddParam("iUser"     , cUser)
                    AND TDAddParam("iLog"      , iLog)
                NO-ERROR.
            IF NOT lOk
            THEN DO:
                cMess = "???   �訡�� ��।�� ��ࠬ��஢ � �࠭����� " + cTranz.
                RUN LogIt(cMess, iLog).
                ttKauCrt.eerror = "�訡�� ��।�� ��ࠬ��஢ � �࠭����� " + cTranz.
            END.
            ELSE DO:
                RUN ex-trans.p (cTranz, TODAY, TABLE tOpKindParams, OUTPUT lOk, OUTPUT cErrMsg).
                IF NOT lOk
                THEN DO:
                    cMess = "???   �訡�� �� �2 -> ��� � �࠭���樨 " + cTranz + " : " + cErrMsg.
                    RUN LogIt(cMess, iLog).
                    RUN pb_mail.p ("a.borisov", "�訡�� �2-���", cMess, SEARCH("sysmess.log")).
                    ttKauCrt.eerror = cErrMsg.
                    ttKauCrt.action = "�訡��".
                END.
                ELSE DO:
                    ttKauCrt.action = "��७�ᥭ �� ���".  /* �� ��� ��� ������ ᪠��஢��� �� �㤥� */
                    ttKauCrt.crtn   = "���".
                    IF ttKauCrt.priost
                    THEN ttKauCrt.eerror = "�ਮ�⠭�����".
                END.
            END.
        END. /* IF NOT (cBlOrdL BEGINS "������") */
        ELSE ttKauCrt.action = IF (iPrtAll EQ "��") THEN "��⠢��� �� �2" ELSE "".

    END. /* FOR EACH ttKauCrt */
END. /* IF (nK2Pos GT 0) */

/* ������㥬 ��� ��� ��७�� �� �2 ******************************************* */
cCurrKbs    = "".
cAcctKbs    = GetXAttrValue("acct", iAcct + "," + iCurr, "���⁂����").    /* ��� ����� ������ ⮫쪮 �� �� �2 -> ��� */
cMess = "  - ��� = " + cAcctKbs.
RUN LogIt(cMess, iLog).

IF (cAcctKbs NE "")
THEN DO:
    ASSIGN
        cCurrKbs    = ENTRY(2, cAcctKbs)
        cAcctKbs    = ENTRY(1, cAcctKbs)
        .
    RUN acct-pos IN h_base(cAcctKbs, cCurrKbs, TODAY, TODAY, "�").
    nKbsPos     = (IF (cCurrKbs EQ "") THEN sh-bal ELSE sh-val).

    IF (nKbsPos GT 0)    /* �᫨ ���� ���㬥��� �� ��� */
    THEN DO:
        /* ��� ������ �����⨪� ��� */
        FOR EACH kau
            WHERE (kau.acct         EQ cAcctKbs)
              AND (kau.currency     EQ cCurrKbs)
              AND (kau.zero-bal     EQ no)
            NO-LOCK,
        FIRST op
            WHERE (op.op            EQ INT64(ENTRY(1, kau.kau)))  /* �ய�᪠�� ��� ��� ���㬥�⮢ */
            NO-LOCK,
        FIRST op-entry OF op
            NO-LOCK
            BY kau.sort
            BY op.op:

            IF NOT CAN-FIND(FIRST ttKauCrt
                                WHERE (ttKauCrt.kau     EQ kau.kau)
                                  AND (ttKauCrt.action  EQ "��७�ᥭ �� ���"))
            THEN RUN New_ttKauCrt("���").
        END.

        FOR EACH ttKauCrt
            WHERE (ttKauCrt.crt EQ "���")
            BY ttKauCrt.order-pay
            BY ttKauCrt.sort:

            IF       NOT (cBlOrdL BEGINS "������")                  /* �� ������᭮� �ந�����⢥ � �������⢥... */
                AND  NOT (cBlOrdL BEGINS "������")                 /*     �� ��⠢�塞 �� ᢮�� �����            */
                AND (NOT ttKauCrt.priost AND (ttKauCrt.block EQ "") /* �� �ਮ�⠭����� � ��� �����஢�� */
                  OR     ttKauCrt.priost AND NOT lArest)            /* ��� �ਮ�⠭����� � ��� ���� */
            THEN DO:

                cMess = "  - ��७�� �� �2".
                RUN LogIt(cMess, iLog).
                /* ����� �࠭���樨  sbk_2kbs - SB: ��६�饭�� �2 <--> ��� */
                cTranz = "sbk_2kbs".
                {empty tOpKindParams}     /* ������ ⠡���� ��ࠬ��஢ */
                ASSIGN
                    lOk =   TDAddParam("iSrcCrt"   , cAcctKbs)
                        AND TDAddParam("iCrtType"  , "�2")
                        AND TDAddParam("iAcct"     , iAcct)
                        AND TDAddParam("iCurrency" , iCurr)
                        AND TDAddParam("iKau"      , ttKauCrt.kau)
                        AND TDAddParam("iDetails"  , ttKauCrt.details)
                        AND TDAddParam("iUser"     , cUser)
                        AND TDAddParam("iLog"      , iLog)
                    NO-ERROR.
                IF NOT lOk
                THEN DO:
                    cMess = "???   �訡�� ��।�� ��ࠬ��஢ � �࠭����� " + cTranz.
                    RUN LogIt(cMess, iLog).
                    ttKauCrt.eerror = "�訡�� ��।�� ��ࠬ��஢ � �࠭����� " + cTranz.
                END.
                ELSE DO:
                    RUN ex-trans.p (cTranz, TODAY, TABLE tOpKindParams, OUTPUT lOk, OUTPUT cErrMsg).
                    IF NOT lOk
                    THEN DO:
                        cMess = "???   �訡�� �� ��� -> �2 � �࠭���樨 " + cTranz + " : " + cErrMsg.
                        RUN LogIt(cMess, iLog).
                        RUN pb_mail.p ("a.borisov", "�訡�� �2-���", cMess, SEARCH("sysmess.log")).
                        ttKauCrt.eerror = cErrMsg.
                        ttKauCrt.action = "�訡��".
                    END.
                    ELSE DO:
                        ttKauCrt.action = "��७�ᥭ �� �2".
                        ttKauCrt.crtn   = "�2".
                        IF ttKauCrt.priost
                        THEN ttKauCrt.eerror = "�ਮ�⠭�����".
                    END.
                END.
            END. /* IF NOT (cBlOrdL BEGINS "������") */
            ELSE ttKauCrt.action = IF (iPrtAll EQ "��") THEN "��⠢��� �� ���" ELSE "".

        END. /* FOR EACH ttKauCrt */
    END. /* IF (nKbsPos GT 0) */
END. /* IF (cAcctKbs NE "") */

/* �஢��塞 �����⨪� �� ����������� ������ ********************************** */
cCurrK2     = "".
cAcctK2     = GetXAttrValue("acct", iAcct + "," + iCurr, "����2�����").    /* �2 ����� ������ ⮫쪮 �� �� ��� -> �2 */
IF (cAcctK2 NE "")
THEN ASSIGN
        cCurrK2     = ENTRY(2, cAcctK2).
        cAcctK2     = ENTRY(1, cAcctK2).
        .

IF          (iUser EQ "")               /* ��楤�� ����饭� �� �� �����஢騪� */
    AND NOT (cBlOrdL BEGINS "������")   /* � ��� ������᭮�� �ந�����⢠        */
    AND NOT (cBlOrdL BEGINS "������")  /* � ��� �������⢠ => ����稢��� ����� */
THEN DO:
    FOR EACH ttKauCrt
/*      WHERE NOT ttKauCrt.priost   / * ⮫쪮 ���ਮ�⠭������� */
        NO-LOCK
        BY ttKauCrt.crtn                            /* �2, ��⥬ ��� */
        BY ttKauCrt.order-new                       /* � ���浪� "�����" ��।����  */
        BY ttKauCrt.sort                            /* �� ��� ���⠭���� � ����⥪� */
        BY (ttKauCrt.balance - ttKauCrt.firstsum):  /* ���筮 ����祭�� - ���묨  */

        nPorog  = IF (ttKauCrt.crtn EQ "�2") THEN 0 ELSE ttKauCrt.blocksum.

        IF     (ttKauCrt.block NE "��")     /* ��� �2 �믮������ ��⮬���᪨ */
           AND (nAcPosTmp      GT nPorog)
        THEN DO:                            /* ����� ��-� ������� */
            IF     (iCurr NE cCurrK2)
                OR (iCurr NE "")
            THEN DO:                        /* ��� ��� ��� �� ����⥪� => ��筠� ��ࠡ�⪠ */
                cMess = "  - ����� ������".
                RUN LogIt(cMess, iLog).
                ttKauCrt.action = ttKauCrt.action + (IF (ttKauCrt.action EQ "") THEN "" ELSE "|")
                                + "����� ������".
            END.
            ELSE IF ttKauCrt.priost         /* �ਮ�⠭������� �� ����稢���, �� �����뢠�� � ��⮪�� */
            THEN DO:                        /* ��� ��� ��� �� ����⥪� => ��筠� ��ࠡ�⪠ */
                cMess = "  - �ਮ�⠭������� ���㬥��".
                RUN LogIt(cMess, iLog).
                ttKauCrt.action = ttKauCrt.action + (IF (ttKauCrt.action EQ "") THEN "" ELSE "|") + "�� ����祭".
                ttKauCrt.eerror = ttKauCrt.eerror + (IF (ttKauCrt.eerror EQ "") THEN "" ELSE "|") + "�ਮ�⠭�����".
            END.
            ELSE DO:                                            /* � �/� � ����⥪� - �㡫��� */
                cPartial = "yes".

                IF (nAcPosTmp - nPorog GE ttKauCrt.balance)
                THEN DO:                                        /* ����� ������� ��� �����⨪� ��������� */
                    IF (ttKauCrt.balance EQ ttKauCrt.firstsum)
                    THEN cPartial   = "no".                /* ����� �� �뫮 ���筮� ������ */
                    ttKauCrt.summa  = ttKauCrt.balance.
                END.
                ELSE ttKauCrt.summa = nAcPosTmp - nPorog.

                cMess = "  - " + (IF (cPartial EQ "yes") THEN "����筠� �����" ELSE "�����").
                RUN LogIt(cMess, iLog).
                /* ����� �࠭���樨  sbk_pay - SB: �������� ���㬥�⮢ ������ */
                RUN SetSysConf IN h_base("����⠑����⥪�","").
                cTranz = "sbk_pay".
                {empty tOpKindParams}     /* ������ ⠡���� ��ࠬ��஢ */
                ASSIGN
                    lOk =   TDAddParam("iSrcCrt"   , IF (ttKauCrt.crtn EQ "�2") THEN cAcctK2 ELSE cAcctKbs)
                        AND TDAddParam("iCrtType"  , ttKauCrt.crtn)
                        AND TDAddParam("iAcct"     , iAcct)
                        AND TDAddParam("iCurrency" , iCurr)
                        AND TDAddParam("iKau"      , ttKauCrt.kau)
                        AND TDAddParam("iSumma"    , STRING(ttKauCrt.summa))
                        AND TDAddParam("partial"   , cPartial)
                        AND TDAddParam("inalog"    , (IF (INDEX(ttKauCrt.order-sym, "�����") NE 0) THEN "yes" ELSE "no"))
                        AND TDAddParam("iUser"     , cUser)
                        AND TDAddParam("iLog"      , iLog)
                    NO-ERROR.
                IF NOT lOk
                THEN DO:
                    cMess = "???   �訡�� ��।�� ��ࠬ��஢ � �࠭����� " + cTranz.
                    RUN LogIt(cMess, iLog).
                    ttKauCrt.eerror = "�訡�� ��।�� ��ࠬ��஢ � �࠭����� " + cTranz.
                END.
                ELSE DO:
                    RUN ex-trans.p (cTranz, TODAY, TABLE tOpKindParams, OUTPUT lOk, OUTPUT cErrMsg).
                    IF NOT lOk
                    THEN DO:
                        cMess = "???   �訡�� �� ����� � �࠭���樨 " + cTranz + " : " + cErrMsg.
                        RUN LogIt(cMess, iLog).
                        RUN pb_mail.p ("a.borisov", "�訡�� �2-���", cMess, SEARCH("sysmess.log")).
                        ttKauCrt.eerror = ttKauCrt.eerror + (IF (ttKauCrt.eerror EQ "") THEN "" ELSE "|") + "�஢���� ���஢������ ���㬥���".
                        ttKauCrt.action = ttKauCrt.action + (IF (ttKauCrt.action EQ "") THEN "" ELSE "|") + "�訡�� ������".
                    END.
                    ELSE DO:
                        nAcPosTmp       = nAcPosTmp - ttKauCrt.summa.
                        ttKauCrt.action = ttKauCrt.action + (IF (ttKauCrt.action EQ "") THEN "" ELSE "|")
                                        + (IF (cPartial EQ "no") THEN "�����" ELSE "����筠� �����").
                        ttKauCrt.oplata = GetSysConf("����⠑����⥪�").
                        RUN LogIt("    ���㬥�� ������ - " + ttKauCrt.oplata, iLog).
                    END.
                END.
            END. /* IF (iCurr NE cCurrK2) */
        END. /* IF (ttKauCrt.block NE "��") */
    END. /* FOR EACH ttKauCrt */
END. /* IF (iUser EQ "") */

/* ����塞 �����ᨨ �� ���㬥��� ������ � ����⥪� */
RUN LogIt("���᫥��� �����ᨩ :", iLog).
FOR EACH ttKauCrt
    WHERE (ttKauCrt.oplata  NE "")
    NO-LOCK:

    RUN LogIt("  - ���㬥�� ������ - " + ttKauCrt.oplata, iLog).
    cTranz = "CreateComPT".
    {empty tOpKindParams}     /* ������ ⠡���� ��ࠬ��஢ */
    lOk = TDAddParam("CurOp", ttKauCrt.oplata) NO-ERROR.
    RUN ex-trans.p (cTranz, TODAY, TABLE tOpKindParams, OUTPUT lOk, OUTPUT cErrMsg).
    IF lOk
    THEN DO:
        ttKauCrt.comissia = DEC(GetSysConf("���������⥪�")).
        RUN LogIt("    ������� - " + STRING(ttKauCrt.comissia, ">>>,>>>,>>9.99"), iLog).
    END.
    ELSE DO:
        cMess = "???   �訡�� ���� �����ᨨ : " + cErrMsg.
        RUN LogIt(cMess, iLog).
        RUN pb_mail.p ("a.borisov", "�訡�� �2-���", cMess, SEARCH("sysmess.log")).
        ttKauCrt.action = ttKauCrt.action + (IF (ttKauCrt.action EQ "") THEN "" ELSE "|") + "�訡�� �����ᨨ".
    END.
END.

/* ���⠥� ��⮪��, �᫨ ���� ��� ���� ���㬥�� */
IF CAN-FIND(FIRST ttKauCrt
                WHERE (IF (iPrtAll EQ "��") THEN YES ELSE (ttKauCrt.action NE "")))
THEN DO:
    RUN XLProtokol(iPrt, lFirst).
    lFirst = NO.
END.
