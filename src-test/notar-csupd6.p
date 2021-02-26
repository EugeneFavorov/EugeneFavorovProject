/*
��楤�� ᮡ�ࠥ� �६����� ⠡���� ��� ����᪠ 
��楤��� ᮧ����� 㢥�������� � ���ਠ� (notar-cre.p)
�롮ઠ �������� ���� �� tmprecid.id �।���� ������஢
���� �� 䠩�� kd.txt
��ࠬ����:
    iDate - ��� ���भ�, � ���஬ ���� �롨����� �஢����
*/
{globals.i}
{intrface.get xclass}
{sh-defs.i}
/*
DEFINE INPUT  PARAMETER iFile AS CHARACTER NO-UNDO.
*/
DEFINE BUFFER op-entry FOR op-entry.
DEFINE BUFFER op-entry1 FOR op-entry.
DEFINE BUFFER loan-acct FOR loan-acct.
DEFINE BUFFER loan FOR loan.
DEFINE BUFFER term-obl FOR term-obl.
DEFINE BUFFER NF FOR PLEDGENOTIFICATION.
DEFINE BUFFER NFOLD FOR NOTIFICATION.

/* '����饭�� � ���ਠ� � ������, ��������� � �४�饭�� ������' */
/* DEFINE BUFFER NF FOR PLEDGENOTIFICATION. */
/* ��ப� ��ࠬ��஢ ��� ��।�� � ��楤��� ᮧ����� ��� ��������� ������*/
DEFINE VARIABLE mParStr AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNotifRet AS INT64 NO-UNDO.
DEFINE VARIABLE mreg-zalog-no AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNN AS INT64 NO-UNDO.
DEFINE VARIABLE mTermSurr AS CHARACTER NO-UNDO.
DEFINE VARIABLE mVidOb AS CHARACTER NO-UNDO.
DEFINE VARIABLE mStr AS CHARACTER NO-UNDO.
DEFINE VARIABLE mOst91312 AS DECIMAL NO-UNDO.
DEFINE VARIABLE mNotifCreNumber AS CHARACTER NO-UNDO.

{tmprecid.def}

DEFINE TEMP-TABLE ttNotif
    FIELD OpEntryDbOp AS INT64
    FIELD OpEntryCrOp AS INT64
    FIELD acct-db AS CHARACTER
    FIELD acct-cr AS CHARACTER
    FIELD contract AS CHARACTER
    FIELD cont-code AS CHARACTER
    FIELD RecLoan AS INT64
    FIELD RecTermObl AS INT64
    INDEX contract cont-code contract
    . 
    
/* 
��ࠡ�⪠ ᮮ�饭��
� ������������� ������
�� ��������� ������
*/
{setdest.i}
PUT UNFORMATTED "��ࠡ�⠭�� ��������" SKIP.
FOR EACH tmprecid,
    FIRST loan
    WHERE RECID(loan) EQ tmprecid.id
    NO-LOCK:
    IF loan.close-date NE ? THEN /* ������� ������ */
        PUT UNFORMATTED 
        ENTRY(1, loan.cont-code, "@") 
        " ; "
        " ������� ������, �� ᮧ���� 㢥��������."
        SKIP.
    ELSE
    DO:
        FIND FIRST NF 
            WHERE NF.CONTRACTNUMBER EQ TRIM(ENTRY(1, loan.cont-code, "@")) 
            AND NF.NOTIFICATIONTYPE EQ 3
            AND NF.STATUS_ GE 6 
            AND NF.STATUS_ LE 7 
        NO-LOCK NO-ERROR.
        IF NOT AVAILABLE(NF) THEN
            PUT UNFORMATTED 
            ENTRY(1, loan.cont-code, "@") 
            " ; "
            "��� 㢥�������� � ������"
            SKIP.
    END.    
END.

{intrface.del}
{preview.i}

