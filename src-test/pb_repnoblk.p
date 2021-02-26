/**
����᪨� �ࠢ� �ਭ�������: ������� ����
�᭮�����:      �����.779 �������� ���� �� ����⨨ ��᫥����� ��⮢ � �����⮢
�� ������:     ��ନ��� ���� � ��ࠢ��� ��� �� ����
��ࠬ����:      ���᮪ �/� ��� ���뫪� ��⮪���
���� ����᪠:  �����஢騪
������:         12.10.2017 ���ᮢ �.�.
*/

DEFINE INPUT  PARAMETER iMail   AS CHARACTER NO-UNDO.   /* ���᮪ �/� ��� ���뫪� ��⮪��� */

RUN pb_tstwork.p.
IF NOT (RETURN-VALUE BEGINS "bis-work") THEN RETURN.

{globals.i}             /** �������� ��।������ */
{intrface.get xclass}   /** �㭪樨 ��� ࠡ��� � ����奬�� */
{pb_exf_exl.i}          /** ������⥪� ��� ���㧪� � XL */
{pb_logit.i}

/******************************************* ��।������ ��६����� � ��. */
DEFINE VARIABLE cXL     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFl     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLog    AS CHARACTER NO-UNDO INIT "/home2/bis/quit41d/log/bankrot/bankrot.log".
DEFINE VARIABLE I       AS INTEGER   NO-UNDO INIT 0.
DEFINE VARIABLE dNow    AS DATETIME  NO-UNDO.
DEFINE BUFFER   accn    FOR acct.
DEFINE BUFFER   blockn  FOR blockobject.
dNow = DATETIME(TODAY, MTIME).

/******************************************* ��������� */
FOR EACH acct
    WHERE CAN-DO("405*,406*,407*,40802*,40807*,40821*,40817*,40820*,420*,421*,422*,4230*,4250*,4260*", acct.acct)
      AND (acct.close-date          = ?)
    NO-LOCK,
FIRST blockobject
    WHERE (blockobject.class-code   = 'BlockAcct')
      AND (blockobject.file-name    = 'acct')
      AND (blockobject.surrogate    = acct.acct + "," + acct.currency)
      AND CAN-DO('������,������', TRIM(blockobject.block-type))
      AND (blockobject.end-datetime = ?
        OR blockobject.end-datetime >= dNow)
    NO-LOCK
    BREAK BY acct.cust-cat
          BY acct.cust-id:

    IF FIRST-OF(acct.cust-id)
    THEN DO:
        FOR EACH accn
            WHERE CAN-DO("405*,406*,407*,40802*,40807*,40821*,40817*,40820*,420*,421*,422*,4230*,4250*,4260*", accn.acct)
              AND (accn.close-date  = ?)
              AND (accn.cust-cat    = acct.cust-cat)
              AND (accn.cust-id     = acct.cust-id)
              AND (accn.acct        <> acct.acct)
            NO-LOCK:

/*          RUN LogIt(acct.cust-cat + STRING(acct.cust-id) + " " + accn.acct, cLog).
*/          FIND FIRST blockn
                WHERE (blockn.class-code    = 'BlockAcct')
                  AND (blockn.file-name     = 'acct')
                  AND (blockn.surrogate     = accn.acct + "," + accn.currency)
                  AND CAN-DO('������,������', TRIM(blockn.block-type))
                  AND (blockn.end-datetime  = ?
                    OR blockn.end-datetime  >= dNow)
                NO-LOCK NO-ERROR.
/*          RUN LogIt(STRING(AVAIL blockn), cLog).
*/          IF (NOT AVAIL blockn)
            THEN DO:
                IF (acct.cust-cat = "�")
                THEN FIND FIRST person
                        WHERE (person.person-id = acct.cust-id)
                        NO-LOCK NO-ERROR.
                ELSE FIND FIRST cust-corp
                        WHERE (cust-corp.cust-id = acct.cust-id)
                        NO-LOCK NO-ERROR.
                I = I + 1.
                IF (I = 1)
                THEN DO:
                    cFl = STRING(YEAR(TODAY)) + STRING(MONTH(TODAY), "99") + STRING(DAY(TODAY), "99").
                    cFl = "/home2/bis/quit41d/log/bankrot/repnoblk-" + cFl + ".xml".
                    OUTPUT TO VALUE(cFl).

                    PUT UNFORMATTED XLHead("��� �����஢��", "ICCCD", "28,112,250,150,105").

                    cXL = XLCellHead("�",0,0,0)
                        + XLCellHead("���ࠧ�������",0,0,0)
                        + XLCellHead("������",0,0,0)
                        + XLCellHead("����� ���",0,0,0)
                        + XLCellHead("��� ������ ���",0,0,0)
                        .
                    PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
                END.

                cXL = XLNumCell(I)
                    + XLCell(accn.branch-id)
                    + XLCell(IF (acct.cust-cat = "�") THEN (person.name-last + " " + person.first-names) ELSE cust-corp.name-short)
                    + XLCell(accn.number)
                    + XLDateCell(accn.open-date)
                    .
                PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
            END.
        END.
    END.
END.

/* **************************************************************************** */
IF (I EQ 0)
THEN RUN pb_mail.p (iMail, "Net schetov bankrotov bez blokirovok", "", "").
ELSE DO:
    PUT UNFORMATTED XLEnd().
    OUTPUT CLOSE.
    RUN pb_mail.p (iMail, "Scheta bankrotov bez blokirovok", "", cFl).
END.

{intrface.del}
