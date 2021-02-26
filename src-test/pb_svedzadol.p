/**
����᪨� �ࠢ� �ਭ�������: ��� ���� ����
�᭮�����:     
�� ������:    ����� �ࠢ�� � ������������ ��� ᢥ����� (��䨪�) ��� ��࠭���� ������� ��࠭���� ������
��� ࠡ�⠥�:  
���� ����᪠: 
������:        13.12.2017 ���ᮢ �.�.
*/

{globals.i}             /** �������� ��।������ */
{tmprecid.def}
{intrface.get tmess}

DEFINE INPUT PARAMETER iParam AS CHARACTER NO-UNDO.
/*
DEFINE VARIABLE iParam     AS CHARACTER   NO-UNDO INIT "��ࠢ�� �� �����.���.������������ v02 2�.,precrdprint,sprssudv02|,��ࠢ�� � ��㤭�� ������������ v02 2�.,precrdprvvv,pr_os_3_v02,�������� � ��㤭�� ������������ (��䨪),print-graphs,templ=graphzadol".
*/
DEFINE VARIABLE cClient    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iClNum     AS INT64       NO-UNDO INIT 0.
DEFINE VARIABLE cDogNum    AS CHARACTER   NO-UNDO VIEW-AS COMBO-BOX.
DEFINE VARIABLE cListDog   AS CHARACTER   NO-UNDO INIT "".
DEFINE VARIABLE cListDoc   AS CHARACTER   NO-UNDO INIT ",".
DEFINE VARIABLE cListPrg   AS CHARACTER   NO-UNDO INIT "".
DEFINE VARIABLE cListPar   AS CHARACTER   NO-UNDO INIT "".
DEFINE VARIABLE I          AS INTEGER     NO-UNDO.
DEFINE VARIABLE cDocum     AS CHARACTER   NO-UNDO VIEW-AS COMBO-BOX.
DEFINE VARIABLE cDogNum2   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cDocum2    AS CHARACTER   NO-UNDO.

DO I = 1 TO NUM-ENTRIES(iParam) / 3:
    cListDoc = cListDoc + "," + ENTRY((I - 1) * 3 + 1, iParam) + "," + STRING(I + 1).
    cListPrg = cListPrg + "," + ENTRY((I - 1) * 3 + 2, iParam).
    cListPar = cListPar + "," + ENTRY((I - 1) * 3 + 3, iParam).
END.

/* ���ᠭ�� ��� ����� */
FORM
    cClient LABEL "������"   FORMAT "x(50)" HELP "F1 - �롮� ������"
    cDogNum LABEL "�������"  FORMAT "x(50)"
    cDocum  LABEL "��������" FORMAT "x(50)"
    WITH FRAME svedzadol
    KEEP-TAB-ORDER OVERLAY CENTERED SIDE-LABELS ROW 8 1 COL
    TITLE COLOR BRIGHT-WHITE "[ ������ ��������� �� �������� ������� ]".

/* ON ******************************************* */
/* �����誨. ���� �ଠ ����뢠���� */
ON F3, F4 ANYWHERE RETURN NO-APPLY.

/* ON ******************************************* */
ON VALUE-CHANGED OF cClient IN FRAME svedzadol
DO:
    RUN Fill-SysMes IN h_tmess ("", "", "0", "�ᯮ���� F1 ��� �롮�.").
    DISPLAY cClient cDogNum WITH FRAME svedzadol.
END.

/* ON ******************************************* */
ON F1 OF cClient IN FRAME svedzadol
DO:
    DO TRANSACTION:
        RUN browseld.p("person",
            "date-out1"   + CHR(1) + "Or&And",
            STRING(TODAY) + CHR(1) + "����",
            "", 5).
        IF (KEYFUNCTION(LASTKEY) EQ "GO")
        THEN DO:
            FOR FIRST person
                WHERE (person.person-id = INT(pick-value))
                NO-LOCK:

                cListDog = ",".
                FOR EACH loan
                    WHERE (loan.contract    = "�।��")
                      AND (loan.cust-cat    = "�")
                      AND (loan.cust-id     = person.person-id)
/*                    AND (loan.close-date  = ?)
*/                    AND CAN-DO("!0400,*", loan.filial-id)
                      AND (NUM-ENTRIES(loan.cont-code, " ") = 1)
                    NO-LOCK
                    BY loan.filial-id
                    BY loan.cont-code:

                    cListDog = cListDog + ","
                             + loan.filial-id + " - " + loan.doc-ref + (IF (loan.close-date = ?) THEN "" ELSE " ������")
                             + "," + loan.cont-code.
                END.

                iClNum   = person.person-id.
                cClient  = person.name-last + " " + person.first-names.
                cDogNum  = "".

                IF (cListDog = ",")
                THEN RUN Fill-SysMes IN h_tmess ("", "", "0", '� ������ "' + cClient + '" ��� �।���� ������஢.').
                ELSE DO:
                    cDogNum:INNER-LINES IN FRAME svedzadol = NUM-ENTRIES(cListDog) / 2.
                    cDogNum:LIST-ITEM-PAIRS IN FRAME svedzadol = cListDog.
                END.

                DISPLAY cClient cDogNum WITH FRAME svedzadol.
            END.
        END.
    END.
END.

/* ON ******************************************* */
ON VALUE-CHANGED OF cDogNum IN FRAME svedzadol
DO:
    ASSIGN cDogNum.
    cDogNum2 = cDogNum.
END.

/* ON ******************************************* */
ON VALUE-CHANGED OF cDocum IN FRAME svedzadol
DO:
    ASSIGN cDocum.
    cDocum2 = cDocum.
END.

/* ���� ��ࠬ��஢ ���㬥��  ************************************************* */
cDocum:LIST-ITEM-PAIRS IN FRAME svedzadol = cListDoc.
cDocum:INNER-LINES IN FRAME svedzadol = NUM-ENTRIES(cListDoc) / 2.

PAUSE 0.
REPEAT:
    UPDATE cClient cDogNum cDocum
        WITH FRAME svedzadol.

    IF (iClNum = 0)
    THEN DO:
        RUN Fill-SysMes IN h_tmess ("", "", "0", "�� ��࠭ ������.").
        UNDO, RETRY.
    END.

    IF (cDogNum2 = "")
    THEN DO:
        RUN Fill-SysMes IN h_tmess ("", "", "0", "�� ��࠭ �������.").
        UNDO, RETRY.
    END.

    IF (cDocum2 = "")
    THEN DO:
        RUN Fill-SysMes IN h_tmess ("", "", "0", "�� ��࠭ ���㬥��.").
        UNDO, RETRY.
    END.

    LEAVE.
END.
HIDE FRAME svedzadol NO-PAUSE.


IF (cDogNum2 <> "")
THEN DO:
    FOR FIRST loan
        WHERE (loan.cont-code   = cDogNum2)
        NO-LOCK:

        CREATE tmprecid.
        tmprecid.id = RECID(loan).
    END.

    RUN VALUE(ENTRY(INT(cDocum2), cListPrg) + ".p") (ENTRY(INT(cDocum2), cListPar)).
END.

{intrface.del}          /* ���㧪� �����㬥����. */ 
