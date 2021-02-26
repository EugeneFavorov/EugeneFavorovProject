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
DEFINE OUTPUT PARAMETER oResult AS LOGICAL   NO-UNDO INIT YES.  /* �᫨ �� ���� �ࠢ��� �� �ࠡ�⠫�, � ᬥ�� ����� */

{globals.i}
{pb_logit.i}
{intrface.get xclass}

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

        IF      CAN-DO(ENTRY(1, code.description[1], "|"), op-entry.acct-db)
            AND CAN-DO(ENTRY(1, code.description[2], "|"), op-entry.acct-cr)
            AND CAN-DO(code.description[3], op-entry.currency)
        THEN DO:

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

    RUN ChangeVO.
    RETURN.
END.
*/
{intrface.del}


/* �ࠢ�� �� */
PROCEDURE ChangeVO:
    DEFINE VARIABLE cDet        AS CHARACTER    NO-UNDO.
    DEFINE VARIABLE ibeg        AS INTEGER      NO-UNDO.
    DEFINE VARIABLE iend        AS INTEGER      NO-UNDO.
    DEFINE VARIABLE cVOn        AS CHARACTER    NO-UNDO.    /* ��� �� ��� �����祭�� ���⥦� */
    DEFINE VARIABLE cVOd        AS CHARACTER    NO-UNDO.    /* ��� �� ��� �� ��������117 */
    DEFINE BUFFER   bop         FOR op.

    oResult = NO.   /* � ��砥 �訡�� - �� ������ ����஫� */
    cMess = shFilial + " " + STRING(USERID("bisquit"),"x(9)") + " ���." + STRING(op.op,">>>>>>>>9") + " �� " + STRING(op.op-date, "99.99.9999")
          + " (" + DelFilFromAcct(op-entry.acct-db) + " - " + DelFilFromAcct(op-entry.acct-cr)
          + ") : �ࠢ��� " + code.code + "-" + STRING(code.name,"x(6)") + " : ".
    cDet  = TRIM(op.details).
    ibeg  = INDEX (cDet, "~{").
    iend  = INDEX (cDet, "}").
    cVOn  = ENTRY(1, code.val, "|").
    cVOd  = IF (NUM-ENTRIES(code.val, "|") EQ 2) THEN ENTRY(2, code.val, "|") ELSE "".

    IF (cVOn EQ "*")
    THEN cMess = cMess + "��⠢�塞 "
               + (IF ((ibeg EQ 0) OR (ibeg GE iend))
                  THEN "��� ���� VO"
                  ELSE ("��� ~{" + SUBSTRING(cDet, ibeg + 1, iend - ibeg - 1)) + "}").
    ELSE DO:
        IF (cVOn NE "")
        THEN DO:
            /* �⠢�� ��� �� */
            IF (ibeg EQ 0) OR (ibeg GE iend)
            THEN DO:
                cMess = cMess + "�⠢�� ��� VO" + cVOn.
                cDet  = "~{VO" + cVOn + "} " + cDet.
            END.
            ELSE DO:
                IF (("~{VO" + cVOn) NE SUBSTRING(cDet, ibeg + 1, iend - ibeg - 1))
                THEN DO:
                    cMess = cMess + "���塞 ��� " + SUBSTRING(cDet, ibeg + 1, iend - ibeg - 1) + " �� VO" + cVOn.
                    SUBSTRING(cDet, ibeg + 1, iend - ibeg - 1) = "VO" + cVOn.
                END.
                ELSE cMess = cMess + "��⠢�塞 ��� " + SUBSTRING(cDet, ibeg + 1, iend - ibeg - 1).
            END.
        END.
        ELSE DO:
            /* ���ࠥ� ��� �� */
            IF (ibeg NE 0) AND (ibeg LT iend)
            THEN DO:
                cMess = cMess + "���ࠥ� ��� " + SUBSTRING(cDet, ibeg + 1, iend - ibeg - 1).
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

    IF (cVOd NE "")     /* ���� ���⠢��� �� ��������117 */
    THEN DO:
        cDet = STRING(IF (op-entry.currency EQ "") THEN op-entry.amt-rub ELSE op-entry.amt-cur).
        cVOd = cVOd + ",," + cDet + ",0," + op-entry.currency + "," + cDet + ",1,1,�,".
        UpdateSigns ("op", STRING(op.op), "��������117", cVOd, NO).
        cMess = cMess + " | " + "�⠢�� �� ��������117 = " + cVOd.
    END.

    cDet = SEARCH(iLog).
    RUN LogIt(cMess, iLog).
    IF (cDet EQ ?) THEN OS-COMMAND SILENT VALUE("chmod 666 " + iLog).   /* �᫨ ��� ⮫쪮 �� ᮧ���, ࠧ�蠥� ������ �ᥬ */

    oResult = (code.name EQ "���").
END PROCEDURE.