/**
����᪨� �ࠢ� �ਭ�������: ��� ���� ����
�᭮�����:      �� �����.1252
�� ������:     ������� ॣ���� ���⥦� �� �����䨪���� ���㫏���
���� ����᪠:  �����஢騪
������:         21.05.2018 ���ᮢ �.�.
*/

{intrface.get date}
{intrface.get tmess}
{topkind.def}
IF HolidayRu(TODAY) THEN RETURN.

DEFINE VARIABLE dDat        AS DATE      NO-UNDO.
DEFINE VARIABLE cTranz      AS CHARACTER NO-UNDO INIT "���㫏���".
DEFINE VARIABLE lOk         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cErrMsg     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSubj       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMail       AS CHARACTER NO-UNDO.

RUN Fill-SysMes IN h_tmess ("", "", "1", "����� pb_regulplat.p").
FOR EACH code
    WHERE (code.class   = '���㫏���')
      AND (code.parent  = '���㫏���')
      AND NOT (code.name BEGINS "-")
    NO-LOCK:

    dDat  = DATE(MONTH(TODAY), INT(ENTRY(3, code.val)), YEAR(TODAY)).
    cMail = code.description[3].

    DO WHILE HolidayRu(dDat):
        dDat = dDat + 1.
    END.

    IF (dDat = TODAY)
    THEN DO:
        /* ����� �࠭���樨  �� */
        RUN Fill-SysMes IN h_tmess ("", "", "1", "����� �� " + cTranz + "��� �ࠢ��� " + code.code).
        {empty tOpKindParams}     /* ������ ⠡���� ��ࠬ��஢ */
        ASSIGN
            lOk =   TDAddParam("__icode" , code.code)
            NO-ERROR.
        IF NOT lOk
        THEN RUN LogIt("�������� �訡�� �� ��।�� ��ࠬ��஢ � �࠭����� " + cTranz).
        ELSE DO:
            RUN ex-trans.p (cTranz, TODAY, TABLE tOpKindParams, OUTPUT lOk, OUTPUT cErrMsg).

            IF NOT lOk
            THEN DO:
                RUN Fill-SysMes IN h_tmess ("", "", "1", "�訡�� � �� " + cTranz + " : " + cErrMsg).
                cErrMsg = "�������� �訡�� � �࠭���樨 " + cTranz + " : " + cErrMsg
                        + "~n~r�ࠢ��� " + code.code + " ��� ��� " + code.name.
                cSubj   = "Regular payment : ERROR".
            END.
            ELSE DO:
                RUN Fill-SysMes IN h_tmess ("", "", "1", "�� " + cTranz + " ��ࠡ�⠫� ��� �訡��").
                cErrMsg = "������ ॣ���� ���⥦ � ��� " + code.name.
                cSubj   = "Regular payment : OK".
            END.

            OUTPUT TO VALUE("/tmp/date.txt").
            PUT UNFORMATTED CODEPAGE-CONVERT(cErrMsg, "1251") SKIP.
            OUTPUT CLOSE.
            RUN pb_mail.p (cMail, cSubj, "", "/tmp/date.txt")).
            RUN Fill-SysMes IN h_tmess ("", "", "1", "���쬮 ��ࠢ����").
        END.
    END.
END.
RUN Fill-SysMes IN h_tmess ("", "", "1", "����砭�� pb_regulplat.p").
{intrface.del}
