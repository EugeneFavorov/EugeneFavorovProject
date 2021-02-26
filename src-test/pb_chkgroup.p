/**
����᪨� �ࠢ� �ਭ�������: ��� ���� ����
�᭮�����:      ���쬮 ����ન���
�� ������:     �⠢�� �� groupOABS �� 2-� � ��᫥����� �/� = 1-��� �/�
��� ࠡ�⠥�:   ��⮤ ExtraChkUpd �� acctb
��ࠬ����:      
���� ����᪠:  
������:         11.09.2017 ���ᮢ �.�.
*/

{globals.i}
{intrface.get xclass}
{pb_logit.i}

DEFINE INPUT PARAMETER iSurr AS CHARACTER NO-UNDO.

DEFINE VARIABLE cGR         AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cAccLst     AS CHARACTER    NO-UNDO INIT "407*,406*,40802*,40807*,40821*".
DEFINE VARIABLE cConLst     AS CHARACTER    NO-UNDO INIT "�࠭�1,�����,�����,���揀,������,������,���138".
DEFINE BUFFER ac1           FOR acct.
DEFINE VARIABLE cLog        AS CHARACTER    NO-UNDO.
DEFINE VARIABLE lNewLog     AS LOGICAL      NO-UNDO.
DEFINE VARIABLE tCorr       AS INTEGER      NO-UNDO.    /* ������ �� �६��� */
tCorr = (TIMEZONE - TIMEZONE("+03:00")) * 60.

cLog = STRING(YEAR(TODAY)) + STRING(MONTH(TODAY), "99") + STRING(DAY(TODAY), "99").
cLog = "/home2/bis/quit41d/log/chkgroup/pb_chkgroup-" + cLog + ".log".
lNewLog = (SEARCH(cLog) = ?).
RUN LogIt('���� ���: "' + iSurr + '" - ' + USERID("bisquit") + ", �����=" + STRING(tCorr, "HH:MM:SS") + ", TIME-60-�����=" + STRING(TIME - 60 - tCorr, "HH:MM:SS"), cLog).
IF lNewLog THEN OS-COMMAND SILENT VALUE("chmod 777 " + cLog).   /* �᫨ ��� ⮫쪮 �� ᮧ���, ࠧ�蠥� ������ �ᥬ */

FIND LAST history
    WHERE (history.modify = "C")
      AND (history.file-name    = 'acct')
      AND (history.field-ref    = iSurr)
    NO-LOCK NO-ERROR.
IF (AVAIL history)
THEN RUN LogIt("  ������: " + STRING(history.modif-date, "99.99.9999") + " "
             + STRING(history.modif-time, "HH:MM:SS"), cLog).
ELSE RUN LogIt("  ��� ᮧ����� �� �������", cLog).

FOR FIRST acct
    WHERE (acct.acct            = ENTRY(1,iSurr))
      AND (acct.currency        = ENTRY(2,iSurr))
      AND (acct.filial-id       = "0500")
      AND CAN-DO(cAccLst, acct.acct)
      AND CAN-DO(cConLst, acct.contract)
      AND (acct.cust-cat       <> "�")
      AND (acct.class-code     <> "acctw4")
      AND CAN-DO("!0599,!0598,*", acct.branch-id)
    EXCLUSIVE-LOCK,
LAST history
    WHERE (history.modify = "C")
      AND (history.file-name    = 'acct')
      AND (history.field-ref    = iSurr)
      AND (history.modif-date   = TODAY)
      AND (history.modif-time   > TIME - 60 - tCorr)    /* ��� ᮧ��� ⮫쪮 �� */
    NO-LOCK:

    cGR = "".
    FOR EACH ac1
        WHERE (ac1.filial-id    = acct.filial-id)
          AND (ac1.cust-cat     = acct.cust-cat)
          AND (ac1.cust-id      = acct.cust-id)
          AND CAN-DO(cAccLst, ac1.acct)
          AND CAN-DO(cConLst, ac1.contract)
          AND (ac1.close-date   = ?)
          AND (ac1.acct        <> ENTRY(1,iSurr))
        NO-LOCK
        BY ac1.open-date:

        cGR = GetXAttrValue("acct", ac1.acct + "," + ac1.currency, "groupOABS").
        IF (LENGTH(cGR) = 3) AND CAN-DO("!599,!598,*", cGR)
        THEN LEAVE.
        ELSE cGR = "".
    END.

    IF (cGR = "")
    THEN DO:    /* ���� ��� �� ������ */
        DO WHILE TRUE:
            RUN g-prompt.p ("integer", "��㯯� ᮯ஢�������", "999", "",
                            "������ ��㯯� ������ ���", 40, ?, ? /* iProcs */,
                            ?,?,OUTPUT cGR).
            IF      (cGR <> ?)
                AND CAN-DO(FGetSetting("�����⮢", "��㯯���", "581,582,583,584,585,586"), cGR)
            THEN LEAVE.
        END.
        RUN LogIt("  ���� ��� �� ������. ��࠭� ��㯯�: " + cGR, cLog).
        UpdateSigns("acct", iSurr, "groupOABS", cGR, YES).
    END.
    ELSE DO:
        /* ��७�� ��ࠬ��஢ � ac1 �� acct */
        UpdateSigns("acct", iSurr, "groupOABS", cGR, YES).
        acct.branch-id = ac1.branch-id.
        RUN LogIt("  ���� ��� ������: " + ac1.number + ". ��㯯�=" + cGR + ", branch-id=" + ac1.branch-id, cLog).
        LEAVE.
    END.
END.

{intrface.del}          /* ���㧪� �����㬥����. */ 
