/**
����᪨� �ࠢ� �ਭ�������: ��� ���� ����
�᭮�����:      �� ��ଠ�
�� ������:     ������� ���� ��ࠢ����� �� ���� ��� ������ � �������
��� ࠡ�⠥�:   
���� ����᪠:  �����஢騪 ��஬.
������:         25.05.2017 ���ᮢ �.�.
*/

DEFINE INPUT  PARAMETER iParam  AS CHARACTER NO-UNDO.   /* days=5;mail=<ᯨ᮪> */

RUN pb_tstwork.p.
IF NOT (RETURN-VALUE BEGINS "bis-work") THEN RETURN.

{globals.i}             /** �������� ��।������ */
{intrface.get xclass}   /** �㭪樨 ��� ࠡ��� � ����奬�� */
/*
{intrface.get netw}     /** ��ࠢ�� � bispc */
*/
{intrface.get date}
{pb_exf_exl.i}          /** ������⥪� ��� ���㧪� � XL */
{parsin.def}

IF HolidayRu(TODAY) THEN RETURN.

DEFINE VARIABLE iMail       AS CHARACTER    NO-UNDO.
DEFINE VARIABLE iDays       AS INTEGER      NO-UNDO.
iMail  = GetParamByNameAsChar(iParam, "mail", "").          /* ���᮪ �/� ��� ���뫪� ���� */
iDays  = INT(GetParamByNameAsChar(iParam, "days", "5")).

end-date = TODAY - 1.
beg-date = rAfterWorkDays(TODAY, - iDays).

/******************************************* ��।������ ��६����� � ��. */
DEFINE VARIABLE cXL         AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cFl         AS CHARACTER    NO-UNDO.
DEFINE VARIABLE dNewDate    AS DATE         NO-UNDO.
DEFINE VARIABLE d2NiDate    AS DATE         NO-UNDO.
DEFINE VARIABLE d2NiSrok    AS DATE         NO-UNDO.
DEFINE VARIABLE cTmp        AS CHARACTER    NO-UNDO.
DEFINE VARIABLE iNum        AS INTEGER      NO-UNDO INIT 0.
DEFINE VARIABLE cAcctL      AS CHARACTER    NO-UNDO INIT
    "30109*,30111*,405*,406*,407*,40802*,40807*,40821*,40817*,40820*,420*,421*,422*,4230*,4250*,4260*".
DEFINE TEMP-TABLE ttIzm     NO-UNDO
    FIELD cFil      AS CHARACTER
    FIELD cAcct     AS CHARACTER
    FIELD cSurr     AS CHARACTER
    FIELD dOpen     AS DATE
    FIELD dClose    AS DATE
    FIELD d2NI      AS DATE
    FIELD dIspr     AS DATETIME
    FIELD cFild     AS CHARACTER
    FIELD dOld      AS DATE
    FIELD dNew      AS DATE
    FIELD cUser     AS CHARACTER
    FIELD cComm     AS CHARACTER
    .
DEFINE BUFFER   hist        FOR history.

/* * ���, ᮮ�饭��� � �� **************************************************** */
FUNCTION Date2NI    RETURNS DATE
   (INPUT  iTxt     AS CHARACTER ) FORWARD.

/* ����⨥ ��� ************************************************************* */
FOR EACH history
    WHERE (history.file-name    EQ 'acct')
      AND (history.modify       EQ 'C')
      AND (history.modif-date   GE beg-date)
      AND (history.modif-date   LE end-date)
    NO-LOCK,
FIRST acct
    WHERE (acct.acct            EQ ENTRY(1, history.field-ref))
      AND (acct.currency        EQ ENTRY(2, history.field-ref))
      AND CAN-DO(cAcctL, acct.acct)
      AND NOT (acct.contract    BEGINS "�࠭�")
    NO-LOCK
    BY history.modif-date:

    /* �᫨ ��� �� ����� ��८����, � �ய�᪠�� */
    FIND FIRST hist
        WHERE (hist.file-name   EQ 'acct')
          AND (hist.field-ref   EQ history.field-ref)
          AND (hist.modify      EQ 'C')
          AND (hist.modif-date  GE history.modif-date)
          AND (hist.modif-date  GT history.modif-date
            OR hist.modif-time  GT history.modif-time)
        NO-LOCK NO-ERROR.
    IF (AVAIL hist) THEN NEXT.

    /* �᫨ � ��������� ��ࠢ���� ��� �� ����窨 ���, � �ய�᪠�� */
    IF (Date2NI("��⠎����") EQ acct.open-date) THEN NEXT.
    /* �᫨ � �� �� �� ��ࠢ���, �� 3 ��� �� �� ��諮, � �ய�᪠�� */
    IF (d2NiDate EQ ?) AND (d2NiSrok GT TODAY) THEN NEXT.

    RUN NextChange("*open-date*", acct.open-date).
    /* �᫨ �� ᮧ����� ��� �뫠 㪠���� ⥪��� ��� ᮧ�����, � �ய�᪠�� * /
    IF (dNewDate EQ history.modif-date) THEN NEXT. */
    /* �᫨ � �� �� �� ��ࠢ���, �� ��� ������ = ᥣ����, � �ய�᪠�� * /
    IF (d2NiDate EQ ?) AND (dNewDate EQ history.modif-date) THEN NEXT. */

    RUN CreateTT("open-date").
END.

/* ��ࠢ����� ���� ������ ��� ******************************************** */
FOR EACH history
    WHERE (history.file-name    EQ 'acct')
      AND (history.modify       EQ 'W')
      AND CAN-DO("*open-date*", history.field-value)
      AND (history.modif-date   GE beg-date)
      AND (history.modif-date   LE end-date)
    NO-LOCK,
FIRST acct
    WHERE (acct.acct            EQ ENTRY(1, history.field-ref))
      AND (acct.currency        EQ ENTRY(2, history.field-ref))
      AND CAN-DO(cAcctL, acct.acct)
      AND NOT (acct.contract    BEGINS "�࠭�")
    NO-LOCK
    BY history.modif-date:

    /* �᫨ � ��������� ��ࠢ���� ��� �� ����窨 ���, � �ய�᪠�� */
    IF (Date2NI("��⠎����") EQ acct.open-date) THEN NEXT.

    RUN NextChange("*open-date*", acct.open-date).
    RUN CreateTT("open-date").
END.

/* ��ࠢ����� ���� ������� ��� ******************************************** */
FOR EACH history
    WHERE (history.file-name    EQ 'acct')
      AND (history.modify       EQ 'W')
      AND CAN-DO("*close-date*", history.field-value)
      AND (history.modif-date   GE beg-date)
      AND (history.modif-date   LE end-date)
    NO-LOCK,
FIRST acct
    WHERE (acct.acct            EQ ENTRY(1, history.field-ref))
      AND (acct.currency        EQ ENTRY(2, history.field-ref))
      AND CAN-DO(cAcctL, acct.acct)
      AND NOT (acct.contract    BEGINS "�࠭�")
    NO-LOCK
    BY history.modif-date:

    /* �᫨ � ��������� ��ࠢ���� ��� �� ����窨 ���, � �ய�᪠�� */
    IF (Date2NI("��⠇�����") EQ acct.close-date) THEN NEXT.
    /* �᫨ � �� �� �� ��ࠢ���, �� 3 ��� �� �� ��諮, � �ய�᪠�� */
    IF (d2NiDate EQ ?) AND (d2NiSrok GT TODAY) THEN NEXT.

    RUN NextChange("*close-date*", acct.close-date).
    /* �᫨ ��� �� ������ ⥪�饩 ��⮩ ᮧ�����, � �ய�᪠�� * /
    IF (dNewDate NE ?) AND (dNewDate EQ history.modif-date) THEN NEXT. */
    /* �᫨ � �� �� �� ��ࠢ���, �� ��� ������� = ᥣ����, � �ய�᪠�� * /
    IF (d2NiDate EQ ?) AND (dNewDate EQ history.modif-date) THEN NEXT. */

    RUN CreateTT("close-date").
END.

IF (iNum EQ 0)
THEN DO:
    RUN pb_mail.p (iMail, "Open / Close-date - OK !", "", "").
    {intrface.del}
    RETURN.
END.

cFl = STRING(YEAR(beg-date)) + STRING(MONTH(beg-date), "99") + STRING(DAY(beg-date), "99") + "-"
    + STRING(YEAR(end-date)) + STRING(MONTH(end-date), "99") + STRING(DAY(end-date), "99").
cFl = "/tmp/acctoc-" + cFl + ".xml".
OUTPUT TO VALUE(cFl).

/* ���� ********************************************************************** */
PUT UNFORMATTED XLHead("open", "CCDDDDCDDCC", "59,150,118,118,115,124,131,71,71,170,190").
cXL = XLCellHead("������",0,0,0)
    + XLCellHead("���",0,0,0)
    + XLCellHead("��� ������ � ����窥 ���",0,0,0)
    + XLCellHead("��� ������� � ����窥 ���",0,0,0)
    + XLCellHead("��� ��ࠢ�� ᮮ�饭�� � ��",0,0,0)
    + XLCellHead("��� ������, ᮮ�饭��� � ��",0,0,0)
    + XLCellHead("��� � �६� ��ࠢ�����",0,0,0)
    + XLCellHead("�뫮",0,0,0)
    + XLCellHead("�⠫�",0,0,0)
    + XLCellHead("��ࠢ�� ���㤭��",0,0,0)
    + XLCellHead("�������਩",0,0,0)
    .
PUT UNFORMATTED XLRow(0) cXL XLRowEnd().

FOR EACH ttIzm
    WHERE (ttIzm.cFild EQ "open-date")
    NO-LOCK
    BREAK BY ttIzm.cFil
          BY ttIzm.cAcct
          BY ttIzm.dIspr:

    cXL = XLCell(ttIzm.cFil)
        + XLCell(ttIzm.cAcct)
        + XLDateCell(ttIzm.dOpen)
        + XLDateCell(ttIzm.dClose)
        + XLCell(GetXAttrValue("acct", ttIzm.cSurr, "��⠑���鋑"))
        + XLDateCell(ttIzm.d2NI)
        + XLCell(STRING(ttIzm.dIspr, "99.99.9999  HH:MM:SS"))
        + XLDateCell(ttIzm.dOld)
        + XLDateCell(ttIzm.dNew)
        + XLCell(ttIzm.cUser)
        + XLCell(ttIzm.cComm)
        .
    PUT UNFORMATTED XLRow(IF FIRST-OF(ttIzm.cFil) THEN 1 ELSE 0) cXL XLRowEnd().
END.

PUT UNFORMATTED XLNextList("close", "CCDDDDCDDCC", "59,150,118,118,115,124,131,71,71,170,190").
cXL = XLCellHead("������",0,0,0)
    + XLCellHead("���",0,0,0)
    + XLCellHead("��� ������ � ����窥 ���",0,0,0)
    + XLCellHead("��� ������� � ����窥 ���",0,0,0)
    + XLCellHead("��� ��ࠢ�� ᮮ�饭�� � ��",0,0,0)
    + XLCellHead("��� �������, ᮮ�饭��� � ��",0,0,0)
    + XLCellHead("��� � �६� ��ࠢ�����",0,0,0)
    + XLCellHead("�뫮",0,0,0)
    + XLCellHead("�⠫�",0,0,0)
    + XLCellHead("��ࠢ�� ���㤭��",0,0,0)
    + XLCellHead("�������਩",0,0,0)
    .
PUT UNFORMATTED XLRow(0) cXL XLRowEnd().

FOR EACH ttIzm
    WHERE (ttIzm.cFild EQ "close-date")
    NO-LOCK
    BREAK BY ttIzm.cFil
          BY ttIzm.cAcct
          BY ttIzm.dIspr:

    cXL = XLCell(ttIzm.cFil)
        + XLCell(ttIzm.cAcct)
        + XLDateCell(ttIzm.dOpen)
        + XLDateCell(ttIzm.dClose)
        + XLCell(GetXAttrValue("acct", ttIzm.cSurr, "��⠑���釠�"))
        + XLDateCell(ttIzm.d2NI)
        + XLCell(STRING(ttIzm.dIspr, "99.99.9999  HH:MM:SS"))
        + XLDateCell(ttIzm.dOld)
        + XLDateCell(ttIzm.dNew)
        + XLCell(ttIzm.cUser)
        + XLCell(ttIzm.cComm)
        .
    PUT UNFORMATTED XLRow(IF FIRST-OF(ttIzm.cFil) THEN 1 ELSE 0) cXL XLRowEnd().
END.

PUT UNFORMATTED XLEnd().
OUTPUT CLOSE.

/* ��। ��ࠢ��� ���� �஢�ਬ, ����饭 �� bispc * /
DEFINE VARIABLE mRet        AS CHARACTER    NO-UNDO INIT "".
DO WHILE (mRet EQ ""):
    RUN IsUserServReady IN h_netw ("", OUTPUT mRet).
    IF (mRet EQ "")
    THEN MESSAGE "������� �ணࠬ�� bispc � ������ ��" VIEW-AS ALERT-BOX.
END.
/ * ��ࠢ�塞 ��⮪�� * /
RUN sndbispc.p ("file=" + cFl + ";class=bq").
*/

RUN pb_mail.p (iMail, "Otchet open/close-date", "", cFl).
OS-DELETE VALUE(cFl).
{intrface.del}

/* **************************************************************************** */
/* �饬 ����, �� ������ �뫮 �������� ��஥ ���祭�� ************************ */
PROCEDURE NextChange:
    DEFINE INPUT  PARAMETER iMask   AS CHARACTER    NO-UNDO.
    DEFINE INPUT  PARAMETER iDate   AS DATE         NO-UNDO.

    /* �饬 ������襥 ��ࠢ����� */
    FIND FIRST hist
        WHERE (hist.file-name   EQ 'acct')
          AND (hist.field-ref   EQ history.field-ref)
          AND (hist.modify      EQ 'W')
          AND CAN-DO(iMask, hist.field-value)
          AND (hist.modif-date  GE history.modif-date)
          AND (hist.modif-date  GT history.modif-date
            OR hist.modif-time  GT history.modif-time)
        NO-LOCK NO-ERROR.
    IF (AVAIL hist)
    THEN DO: 
        cTmp     = ENTRY(LOOKUP(REPLACE(iMask, "*", ""), hist.field-value) + 1, hist.field-value).
        dNewDate = IF (cTmp EQ "" OR cTmp EQ "~003~004~005") THEN ? ELSE DATE(cTmp).
    END.
    ELSE dNewDate = iDate.  /* �᫨ �� ����� �ࠢ�� �� �뫮, � ��� � ��� */
END PROCEDURE.

/* ************************************** */
PROCEDURE CreateTT:
    DEFINE INPUT  PARAMETER iDat    AS CHARACTER    NO-UNDO.
    DEFINE VARIABLE dOldDate        AS DATE         NO-UNDO.

    IF (LOOKUP(iDat, history.field-value) EQ 0)
    THEN dOldDate   = ?.
    ELSE DO:
        cTmp        = ENTRY(LOOKUP(iDat, history.field-value) + 1, history.field-value).
        dOldDate    = IF (cTmp EQ "" OR cTmp EQ "~003~004~005") THEN ? ELSE DATE(cTmp).
    END.

    IF (dNewDate EQ ? AND dOldDate EQ ?) THEN RETURN.

    FIND FIRST _user
        WHERE (_user._userid    EQ history.user-id)
        NO-LOCK NO-ERROR.
    CREATE ttIzm.
    ASSIGN
        ttIzm.cFil   = acct.filial-id
        ttIzm.cAcct  = acct.number
        ttIzm.cSurr  = acct.acct + "," + acct.currency
        ttIzm.dOpen  = acct.open-date
        ttIzm.dClose = acct.close-date
        ttIzm.d2NI   = d2NiDate
        ttIzm.dIspr  = DATETIME(history.modif-date, history.modif-time * 1000)
        ttIzm.cFild  = iDat
        ttIzm.dNew   = dNewDate
        ttIzm.dOld   = dOldDate
        ttIzm.cUser  = IF (AVAIL _user) THEN _user._user-name ELSE history.user-id
        ttIzm.cComm  = IF (d2NiDate NE ?) THEN "" ELSE (
                       IF (d2NiSrok EQ TODAY) THEN "������� ���� ��ࠢ��� � ��" ELSE (
                       IF (d2NiSrok LT TODAY) THEN "�ய�饭� ��ࠢ�� � ��"     ELSE ""))
        iNum         = iNum + 1.
        .
END PROCEDURE.

/* ************************************** */
FUNCTION Date2NI    RETURNS DATE
   (INPUT  iTxt     AS CHARACTER ).

    DEFINE VARIABLE I       AS INTEGER  NO-UNDO.

    d2NiDate = ?.
    FOR EACH PackObject
        WHERE (PackObject.file-name EQ 'acct')
          AND (PackObject.Surrogate EQ acct.acct + "," + acct.currency)
        NO-LOCK,
    FIRST Packet
        WHERE (Packet.PacketID      EQ PackObject.PacketID)
          AND (Packet.Class-Code    EQ "PTaxX")
          AND (Packet.mail-format   EQ IF (iTxt EQ "��⠎����") THEN "XFNSAcctOpen" ELSE "XFNSAcctClose")
        NO-LOCK,
    FIRST PacketText
        WHERE (PacketText.PacketID  EQ PackObject.PacketID)
        NO-LOCK:

        I = INDEX(PacketText.Contents, CODEPAGE-CONVERT(iTxt, "1251")).
        IF (I NE 0)
        THEN 
            d2NiDate = DATE(SUBSTRING(PacketText.Contents, I + 12, 10)) NO-ERROR.
    END.

    d2NiSrok = rAfterWorkDays(IF (iTxt EQ "��⠎����") THEN acct.open-date ELSE acct.close-date, 3).

    RETURN d2NiDate.
END FUNCTION.
