/**
����᪨� �ࠢ� �ਭ�������: 
���������:     pr_chk_val.p
�᭮�����:      �� ������.3805, .3806, .3825
�� ������:     �� �����䨪���� ���낎��� ��� ��� ��, ᮮ⢥�����騩 ���㬥���
���� ����᪠:  ��楤�� ����஫� � �����䨪��� ��楤��늮���
������:         31.08.2016 ���ᮢ �.�.
*/

DEFINE PARAMETER BUFFER Op FOR op.
DEFINE INPUT  PARAMETER iParam  AS CHARACTER NO-UNDO.   /* ��� �ந�室�� �஢�ઠ: hand / <����> �� ᬥ�� ����� */
DEFINE OUTPUT PARAMETER oResult AS LOGICAL   NO-UNDO INIT YES.

{globals.i}
{pb_logit.i}

DEFINE VARIABLE iLog        AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cMess       AS CHARACTER    NO-UNDO.
/*
DEFINE BUFFER   oedb        FOR op-entry.
DEFINE BUFFER   oecr        FOR op-entry.
*/
iLog = "/home2/bis/quit41d/log/code_vo/" + STRING(YEAR(TODAY)) + STRING(MONTH(TODAY), "99") + STRING(DAY(TODAY), "99") + ".log".

FOR EACH op-entry OF op
NO-LOCK:
    FOR EACH code
        WHERE (code.class   EQ '���낎���')
          AND (code.parent  EQ '���낎���')
          AND (iParam       EQ ""
            OR code.val     NE "")
        NO-LOCK
        BY code.code:

        IF (NUM-ENTRIES(code.description[1], "|") EQ 2)     /* �᫨ � �� 㪠���� �����祭�� ��� */
        THEN DO:
            FIND FIRST acct
                WHERE (acct.acct    EQ op-entry.acct-db)
                NO-LOCK NO-ERROR.
            IF NOT CAN-DO(ENTRY(2, code.description[1], "|"), acct.contract)
            THEN NEXT.
        END.

        IF (NUM-ENTRIES(code.description[2], "|") EQ 2)     /* �᫨ � �� 㪠���� �����祭�� ��� */
        THEN DO:
            FIND FIRST acct
                WHERE (acct.acct    EQ op-entry.acct-cr)
                NO-LOCK NO-ERROR.
            IF NOT CAN-DO(ENTRY(2, code.description[2], "|"), acct.contract)
            THEN NEXT.
        END.

        IF      CAN-DO(ENTRY(1, code.description[1], "|"), op-entry.acct-db)
            AND CAN-DO(ENTRY(1, code.description[2], "|"), op-entry.acct-cr)
            AND CAN-DO(code.description[3], op-entry.currency)
        THEN DO:

            cMess = " (" + DelFilFromAcct(op-entry.acct-db) + " - " + DelFilFromAcct(op-entry.acct-cr) + ") : �ࠢ��� ".
            RUN ChangeVO.
            RETURN.
        END.
    END.
END.
/*
FOR EACH code
    WHERE (code.class       EQ '���낎���2')
      AND (code.parent      EQ '���낎���2')
    NO-LOCK,
FIRST oedb OF op
    WHERE CAN-DO(code.description[1], oedb.acct-db)
      AND (oedb.acct-cr     EQ ?)
    NO-LOCK,
FIRST oecr OF op
    WHERE CAN-DO(code.description[2], oecr.acct-cr)
      AND (oecr.acct-db     EQ ?)
      AND (oecr.currency    NE oedb.currency)
    NO-LOCK:

    cMess = " (" + DelFilFromAcct(oedb.acct-db) + " - " + DelFilFromAcct(oecr.acct-cr) + ") : �ࠢ��� 2-".
    RUN ChangeVO.
    RETURN.
END.
*/

/* �ࠢ�� �� */
PROCEDURE ChangeVO:
    DEFINE VARIABLE cDet        AS CHARACTER    NO-UNDO.
    DEFINE VARIABLE ibeg        AS INTEGER      NO-UNDO.
    DEFINE VARIABLE iend        AS INTEGER      NO-UNDO.
    DEFINE BUFFER   bop         FOR op.

    cMess = shFilial + " " + STRING(USERID("bisquit"),"x(9)") + " ���." + STRING(op.op,">>>>>>>>9") + " �� " + STRING(op.op-date, "99.99.9999")
          + cMess + code.code + "-" + STRING(code.name,"x(6)") + " : ".

    cDet = TRIM(op.details).
    ibeg = INDEX (cDet, "~{").
    iend = INDEX (cDet, "}").

    IF (code.val EQ "*")
    THEN cMess = cMess + "��⠢�塞 "
               + (IF ((ibeg EQ 0) OR (ibeg GE iend)) THEN "��� ���� VO"
                                                     ELSE ("��� VO" + SUBSTRING(cDet, ibeg + 3, iend - ibeg - 3))).
    ELSE DO:
        IF (code.val NE "")
        THEN DO:
            /* �⠢�� ��� �� */
            IF (ibeg EQ 0) OR (ibeg GE iend)
            THEN DO:
                cMess = cMess + "�⠢�� ��� VO" + code.val.
                cDet  = "~{VO" + code.val + "} " + cDet.
            END.
            ELSE DO:
                IF (code.val NE SUBSTRING(cDet, ibeg + 3, iend - ibeg - 3))
                THEN DO:
                    cMess = cMess + "���塞 ��� VO" + SUBSTRING(cDet, ibeg + 3, iend - ibeg - 3) + " �� VO" + code.val.
                    SUBSTRING(cDet, ibeg + 3, iend - ibeg - 3) = code.val.
                END.
                ELSE cMess = cMess + "��⠢�塞 ��� VO" + SUBSTRING(cDet, ibeg + 3, iend - ibeg - 3).
            END.
        END.
        ELSE DO:
            /* ���ࠥ� ��� �� */
            IF (ibeg NE 0) AND (ibeg LT iend)
            THEN DO:
                cMess = cMess + "���ࠥ� ��� VO" + SUBSTRING(cDet, ibeg + 3, iend - ibeg - 3).
                SUBSTRING(cDet, ibeg, iend - ibeg + 1) = "".
                cDet = TRIM(cDet).
            END.
            ELSE cMess = cMess + "���� VO �� �뫮 � �� ����".
        END.
    END.

    IF (cDet NE op.details)
    THEN DO:
        FOR FIRST bop 
            WHERE (bop.op   EQ op.op)
            EXCLUSIVE-LOCK:

            bop.details = cDet.
        END.
    END.

    oResult = (code.name EQ "���").
    RUN LogIt(cMess, iLog).
    OS-COMMAND SILENT VALUE("chmod 666 " + iLog).
END PROCEDURE.