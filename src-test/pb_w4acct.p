/**
����᪨� �ࠢ� �ਭ�������: ��� ���� ����
�� ������:     ��४������ �ଠ� ��⮢ WAY4 � ⥣��� �ଠ� ��᪢��
��ࠬ����:      file=<��.䠩�>;arch-dir=<��娢>;ans-dir=<���.��⠫��>
���� ����᪠:  ��ᬠ�
������:         07.04.2017 ���ᮢ �.�.
*/

DEFINE INPUT PARAMETER iParam       AS CHARACTER    NO-UNDO.

{globals.i}           /** �������� ��।������ */
{parsin.def}
{intrface.get filex}
{pb_logit.i}

DEFINE VARIABLE cInFile     AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cOutFile    AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cArcDir     AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cStr        AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cAcct       AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cClose      AS CHARACTER    NO-UNDO.
DEFINE VARIABLE lSend       AS LOGICAL      NO-UNDO INIT NO.
DEFINE VARIABLE lCorp       AS LOGICAL      NO-UNDO.
DEFINE VARIABLE cCust       AS CHARACTER    NO-UNDO.
DEFINE VARIABLE iCnt        AS INTEGER      NO-UNDO INIT 0.
DEFINE VARIABLE cSep        AS CHARACTER    NO-UNDO INIT "~t".
DEFINE TEMP-TABLE ttinf     NO-UNDO
    FIELD str       AS CHARACTER
    .
DEFINE STREAM imp.
DEFINE STREAM txt.

/* ��娢 ********************************************************************** */
cInFile = GetParamByNameAsChar(iParam, "file", "").
cArcDir = GetParamByNameAsChar(iParam, "arch-dir", "./")
        + STRING( YEAR(TODAY))       + "-"
        + STRING(MONTH(TODAY), "99") + "-"
        + STRING(  DAY(TODAY), "99") + "/".
IF NOT ExistFolder(cArcDir)
THEN DO:
    SurelyCreateFolder(cArcDir).
    OS-COMMAND SILENT VALUE("chmod 777 " + cArcDir).
END.

INPUT  STREAM imp FROM VALUE(cInFile).
IMPORT STREAM imp UNFORMATTED cStr.     /* � ��ࢮ� ��ப� - ��������� �⮫�殢 */
REPEAT:
    IMPORT STREAM imp UNFORMATTED cStr.
    CREATE ttinf.
    ttinf.str = cStr.
    iCnt = iCnt + 1.
END.
INPUT  STREAM imp CLOSE.

IF (iCnt NE 0) THEN OS-COPY VALUE(cInFile)  VALUE(cArcDir).
cInFile  = SUBSTRING(cInFile, R-INDEX(cInFile, "/") + 1).

IF (iCnt NE 0)
THEN DO:
    cOutFile = cArcDir + SUBSTRING(cInFile, 1, LENGTH(cInFile) - 4) + ".txt".
    OUTPUT STREAM txt TO VALUE(cOutFile).

    FOR EACH ttinf
        NO-LOCK:

        cStr   = ttinf.str.
        cAcct  = ENTRY(3, cStr, cSep).
        cClose = IF (NUM-ENTRIES(cStr, cSep) >= 5) THEN ENTRY(5, cStr, cSep) ELSE "".
        cCust  = IF (cAcct BEGINS "40802") THEN SUBSTRING(ENTRY(2, cStr, cSep), 4) /* �� ��⠥��� �� � ��� ����� ��।����� � �㪢�� "U" */
                                           ELSE SUBSTRING(ENTRY(2, cStr, cSep), 3).
        lCorp  = (cAcct BEGINS "407") AND (SUBSTRING(cAcct,10,4) = "0577").
        IF (cClose = "") THEN lSend = YES.

        IF      (cClose = "")
            AND (DATE(ENTRY(4, cStr, cSep)) <> TODAY)
        THEN DO:
            OUTPUT TO VALUE("date.txt").
            PUT UNFORMATTED CODEPAGE-CONVERT("��� ������ ��� " + cAcct + " = " + ENTRY(4, cStr, cSep)
                + " �� ᮢ������ � ⥪�饩: " + STRING(TODAY, "99.99.9999"), "1251") SKIP.
            OUTPUT CLOSE.
            RUN mail-add.p ("pb_w4acct").
            RUN pb_mail.p (RETURN-VALUE, "WAY4 - BIS: Acct open date!", "", "date.txt")).
        END.

        PUT STREAM txt UNFORMATTED
            (IF (cAcct BEGINS "9") THEN "%������W4" ELSE "%����W4") + "~n" +
            "������   :0500"                                    + "~n" +
            "������   :" + SUBSTRING(cAcct, 6, 3)               + "~n" +
            "���/���� :" + SUBSTRING(cAcct, 1, 5)               + "~n" +
            "����     :" + cAcct                                + "~n" +
            "������1  :" + cCust                                + "~n" +
            "���1     :" + (IF (NUM-ENTRIES(cStr, cSep) GE 8)
                           THEN ENTRY(8, cStr, cSep) ELSE "")   + "~n" +
            "������   :" + ENTRY(4, cStr, cSep)                 + "~n" +
            (IF (cClose = "") THEN "" ELSE (
            "������   :" + cClose                               + "~n" )) +
            "�������  :" + (IF (cAcct BEGINS "9")
                           THEN "���������" ELSE "������")      + "~n" +
            "���      :"                                        + "~n" +
            (IF (cClose <> "") THEN "" ELSE (
            "groupOABS:" + (IF (SUBSTRING(cAcct,10,4) = "0577")
                           THEN "599" ELSE "598")               + "~n" +
            (IF (cAcct BEGINS "40") THEN (
            "��������:" + ENTRY(4, cStr, cSep) + ","
                         + (IF lCorp THEN (SUBSTRING(cCust,2) + "/��") ELSE
                           (IF (cCust BEGINS "U")
                            THEN GetXAttrValue("cust-cat", SUBSTRING(cCust,2), "CID")
                            ELSE GetXAttrValue("person", cCust, "CID"))) + "~n" +
            (IF lCorp THEN (
            "��ொ���:��"                                       + "~n" ) ELSE "")) ELSE "") +
            "���������:" + USERID("bisquit")                   + "~n" +
            (IF (NUM-ENTRIES(cStr, cSep) GE 6) AND (ENTRY(6, cStr, cSep) NE "") THEN (
            "W4basicacct:" + ENTRY(6, cStr, cSep)               + "~n" ) ELSE "") +
            (IF (NUM-ENTRIES(cStr, cSep) GE 7) AND (ENTRY(7, cStr, cSep) NE "") THEN (
            "W4tarif:" + ENTRY(7, cStr, cSep)                   + "~n" ) ELSE "")
            )) +
            "%END"                                              + "~n"
            .
    END.

    OUTPUT STREAM txt CLOSE.
    IF lSend THEN
/*  RUN pb_mail.p ("a.borisov,v.ignatchenko,t.stenina", "WAY4 - acct control", "", cOutFile)). */
    RUN pb_mail.p ("a.borisov", "WAY4 - acct control", "", cOutFile)).
    OS-COPY VALUE(cOutFile) VALUE(GetParamByNameAsChar(iParam, "ans-dir", ".")).
END.

RUN LogIt(cInFile + " - " + (IF (iCnt EQ 0) THEN "���⮩" ELSE STRING(iCnt)), cArcDir + "Account.log").
{intrface.del}
