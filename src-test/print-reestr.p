/**
����᪨� �ࠢ� �ਭ�������: ��� "���� ����"
���������:     print-reestr.p
�᭮�����:      �� ����.036
�� ������:     ����� ॥��� (��� ��ࠢ�� �ࠢ�� � ������������) �� �⬥祭�� ������ࠬ
��ࠬ����:      templ=<蠡��� XL>;dogtype=<⨯ �������>;payments=ALL/�����  (templ=graphm0914;dogtype=4;payments=ALL) 
���� ����᪠:  �।��� - Ctrl-G - ������ ��������� � ������� EXCEL
������:         04.05.2018 ���ᮢ �.�.
*/

&GLOB nodate YES

{globals.i}
{tmprecid.def}
{intrface.get xclass}
{intrface.get tmess}
{intrface.get netw}     /** ��ࠢ�� � bispc */

DEFINE VARIABLE cFl     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTxt    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRee_s  AS CHARACTER NO-UNDO.
DEFINE VARIABLE I       AS INTEGER   NO-UNDO.
DEFINE VARIABLE Nree    AS INTEGER   NO-UNDO.
DEFINE VARIABLE cN1     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cN2     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cINN    AS CHARACTER NO-UNDO.
DEFINE VARIABLE Rezult    AS CHARACTER NO-UNDO.
DEFINE STREAM sreestr.

/* ��⠥� 蠡��� */
FUNCTION TmplImport RETURNS CHARACTER
   (INPUT  iTmplNam AS CHARACTER ).

    DEFINE VARIABLE cTmpl       AS CHARACTER    NO-UNDO.
    DEFINE VARIABLE cTmp        AS CHARACTER    NO-UNDO.

    INPUT FROM VALUE(SEARCH(iTmplNam)).
    cTmpl = "".
    DO WHILE TRUE
        ON ENDKEY UNDO, LEAVE:

        IMPORT UNFORMATTED cTmp.
        cTmpl = cTmpl + cTmp + "~r~n".
    END.
    INPUT  CLOSE.

    RETURN cTmpl.
END FUNCTION.

/* �뢮� ��������� ॥��� � ������� */
PROCEDURE OutReeBeg:
    DEFINE INPUT  PARAMETER iTmpl   AS CHARACTER    NO-UNDO.    /* ������ */
    DEFINE INPUT  PARAMETER iVal    AS CHARACTER    NO-UNDO.    /* ����� ॥��� */

    DEFINE VARIABLE cTmp        AS CHARACTER    NO-UNDO.

    INPUT FROM VALUE(SEARCH(iTmpl)).
    DO WHILE TRUE
        ON ENDKEY UNDO, LEAVE:

        IMPORT UNFORMATTED cTmp.
        cTmp = REPLACE(cTmp, "numtabl", iVal).
        PUT STREAM sreestr UNFORMATTED (cTmp + "~r~n").
    END.
    INPUT  CLOSE.
END PROCEDURE.

/* ��⠥� 蠡��� */
cRee_s = TmplImport("pb_respr_str.rtf").

cFl  = "./reespr-"
     + STRING(YEAR(TODAY)) + STRING(MONTH(TODAY), "99") + STRING(DAY(TODAY), "99")
     + "-" + shFilial + ".rtf".
OUTPUT STREAM sreestr TO VALUE(cFl).

/* �� �⬥祭�� ������ࠬ */
Nree = 0.
FOR EACH tmprecid
    NO-LOCK,
FIRST loan
    WHERE (RECID(loan)  = tmprecid.id)
    NO-LOCK
    BY loan.doc-ref:

    Nree = Nree + 1.
END.
RUN OutReeBeg("pb_respr_beg.rtf", STRING(Nree)).

I = 0.
FOR EACH tmprecid
    NO-LOCK,
FIRST loan
    WHERE (RECID(loan)      = tmprecid.id)
    NO-LOCK
    BY loan.doc-ref:

    /* ��ப� ॥��� */
    I = I + 1.
    cTxt = REPLACE(cRee_s, "numstr",       STRING(I)).
    RUN GetCustName IN h_base (loan.cust-cat, loan.cust-id, ?, OUTPUT cN1, OUTPUT cN2, INPUT-OUTPUT cINN).
    cTxt = REPLACE(cTxt,   "klientfio",    CODEPAGE-CONVERT(TRIM(cN1 + " " + cN2), "1251")).
    RUN pb_newadr.p (loan.cust-cat, loan.cust-id,OUTPUT Rezult).
    cTxt = REPLACE(cTxt,   "klientadress", " " + CODEPAGE-CONVERT(RETURN-VALUE, "1251")).
    PUT STREAM sreestr UNFORMATTED cTxt.
END.

RUN OutReeBeg("pb_respr_end.rtf", STRING(Nree)).
OUTPUT STREAM sreestr CLOSE.

/* ��। ��ࠢ��� ���� �஢�ਬ, ����饭 �� bispc */
DEFINE VARIABLE mRet        AS CHARACTER    NO-UNDO INIT "".
DO WHILE (mRet EQ ""):
    RUN IsUserServReady IN h_netw ("", OUTPUT mRet).
    IF (mRet EQ "")
    THEN MESSAGE "������� �ணࠬ�� bispc � ������ ��" VIEW-AS ALERT-BOX.
END.
/* ��ࠢ�塞 ��⮪�� */
RUN sndbispc.p ("file=" + cFl + ";class=bq").
{intrface.del}
