{globals.i}
{intrface.get tmess}

/* +++ pb_depinet.p was humbly modified by (c)blodd converter v.1.09 on 10/13/2016 2:17pm +++ */

/**
����᪨� �ࠢ� �ਭ�������: ��� ���� ����
�᭮�����:      ���쬮 ��客����� �.�.
�� ������:     ��⮢�� ���� �� �ਢ��祭�� ������⮢ �� �१ ���୥�
��� ࠡ�⠥�:   
��ࠬ����:      
���� ����᪠:  �����஢騪 �� �������쭨��� � 8.00
������:         12.10.2016 ���ᮢ �.�.
*/

DEFINE INPUT  PARAMETER iMail   AS CHARACTER NO-UNDO.   /* ���᮪ �/� ��� ���뫪� ��⮪��� */

{globals.i}             /** �������� ��।������ */
{intrface.get xclass}   /** �㭪樨 ��� ࠡ��� � ����奬�� */
{sh-defs.i}
{pb_exf_exl.i}          /** ������⥪� ��� ���㧪� � XL */

/******************************************* ��।������ ��६����� � ��. */
DEFINE VARIABLE cXL         AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cFile       AS CHARACTER    NO-UNDO.
DEFINE VARIABLE dSumm       AS DECIMAL      NO-UNDO.
DEFINE VARIABLE lInet       AS LOGICAL      NO-UNDO.
DEFINE TEMP-TABLE ttRep   NO-UNDO
    FIELD fil       AS CHARACTER
    FIELD cur       AS CHARACTER
    FIELD summ      AS DECIMAL
    .

beg-date = TODAY - 7.
FOR EACH loan
    WHERE (loan.contract    EQ 'dps')
      AND (loan.cont-type   NE "��")
      AND (loan.cust-cat    EQ "�")
      AND (loan.close-date  EQ ?)
      AND (loan.open-date   GE beg-date)
      AND (loan.open-date   LT TODAY)
    NO-LOCK
    BREAK BY loan.filial-id
          BY loan.currency:

    IF FIRST-OF(loan.currency)
    THEN dSumm = 0.

    lInet = NO.
    IF (GetXAttrValue("loan", "dps," + loan.cont-code, "������ਢ�") MATCHES "*���୥�")
    THEN lInet = YES.
    ELSE
    IF (GetXAttrValue("person", STRING(loan.cust-id),  "������ਢ�") MATCHES "*���୥�")
    THEN lInet = YES.

    IF lInet
    THEN DO:
        FIND FIRST loan-acct OF loan
            WHERE (loan-acct.acct-type  EQ "loan-dps-t")
            NO-LOCK NO-ERROR.
        IF (AVAIL loan-acct)
        THEN DO:
            RUN acct-pos IN h_base(loan-acct.acct, loan-acct.currency, TODAY, TODAY, "�").
            dSumm = dSumm - ( IF (loan.currency EQ "") THEN sh-bal ELSE sh-val).
        END.
    END.

    IF LAST-OF(loan.currency)
    THEN DO:
        CREATE ttRep.
        ASSIGN
            ttRep.fil   = loan.filial-id
            ttRep.cur   = loan.currency
            ttRep.summ  = dSumm
            .
    END.
END.

IF CAN-FIND(FIRST ttRep)
THEN DO:
    cFile = "/tmp/dep-" + STRING(YEAR(TODAY)) + STRING(MONTH(TODAY), "99") + STRING(DAY(TODAY), "99") + ".xml".
    OUTPUT TO VALUE(cFile).

    /******************************************* ��������� */
    PUT UNFORMATTED XLHead("dep", "CCN", "120,120,175").
    PUT UNFORMATTED XLRow(0) XLCellHat("���� �� �ਢ��祭�� ������⮢ �१ ���୥�", 2) XLRowEnd().
    PUT UNFORMATTED XLRow(0) XLCellHat("�� ��ਮ� � " + STRING(beg-date, "99.99.9999") + " �� " + STRING(TODAY - 1, "99.99.9999"), 2) XLRowEnd().

    cXL = XLCellHead("������",0,0,0)
        + XLCellHead("�����",0,0,0)
        + XLCellHead("�㬬�",0,0,0)
        .
    PUT UNFORMATTED XLRow(0) cXL XLRowEnd().

    FOR EACH ttRep
        NO-LOCK
        BY ttRep.fil
        BY ttRep.cur:

        cXL = XLCell(ttRep.fil)
            + XLCell(IF (ttRep.cur EQ "") THEN "810" ELSE ttRep.cur)
            + XLNumCell(ttRep.summ)
            .
        PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
    END.
    PUT UNFORMATTED XLEnd().
    OUTPUT CLOSE.

    RUN pb_mail.p (iMail, "Deposits from ���୥�", "", SEARCH(cFile)).
    OS-DELETE VALUE ( {&RELATIVE_2_ABSOLUTE}( cFile ) ).
END.
ELSE 
    RUN pb_mail.p (iMail, "NO deposits from ���୥�", "", "").

{intrface.del}

/* --- pb_depinet.p was humbly modified by (c)blodd converter v.1.09 on 10/13/2016 2:17pm --- */
