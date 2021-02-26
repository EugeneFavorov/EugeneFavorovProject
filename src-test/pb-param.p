/**
����᪨� �ࠢ� �ਭ�������: ��� ���� ����
�� ������:     ��������� �१ ��ᬠ� � �ଠ� PB-PARAM
��� ࠡ�⠥�:   � �ࠢ��� ������ �� 4-� ��⠫���� � �� PBparam �ନ��� 
��ࠬ����:      �ࠢ��� ������, ��⠫�� ImpArch - ��४��� ��娢�;
                �ࠢ��� ������, ��⠫�� ��ᯮ�� - ��४��� �⢥� (�᫨ ����);
                �ࠢ��� ������, ��⠫�� Export  - ��४��� �訡�� (�᫨ ����);
                �ࠢ��� ������, ���.४����� PBparam - ��ப�. ��ଠ�:
                    <��楤��>|<��ࠬ����>
                � <��楤���> ��।����� ��ࠬ���:
                file=<��.䠩�>;arch-dir=<���.��娢�>;ans-dir=<���.�⢥�>;error-dir=<���.�訡��>;<��ࠬ���� �� PBparam>
���� ����᪠:  ��ᬠ� (mimfile.p)
������:         27/09/2016 ���ᮢ �.�. 
*/

{globals.i}

DEFINE INPUT PARAMETER in-mail-user-num LIKE mail-user.mail-user-num NO-UNDO.
DEFINE INPUT PARAMETER inFileName       AS CHARACTER                 NO-UNDO.

/* �饬 ��⠫�� arch-dir */
DEFINE VARIABLE iArchDir    AS CHARACTER NO-UNDO INIT "".
FOR FIRST catalog
    WHERE (catalog.mail-user-num EQ in-mail-user-num)
      AND (catalog.Kind          EQ "ImpArch")
    NO-LOCK:

    iArchDir = catalog.path.
END.
IF (iArchDir EQ "") THEN RETURN.

/* �饬 ��⠫�� ans-dir */
DEFINE VARIABLE iOutDir     AS CHARACTER NO-UNDO INIT "".
FOR FIRST catalog
    WHERE (catalog.mail-user-num EQ in-mail-user-num)
      AND (catalog.Kind          EQ "��ᯮ��")
    NO-LOCK:

    iOutDir = catalog.path.
END.

/* �饬 ��⠫�� error-dir */
DEFINE VARIABLE iErrDir     AS CHARACTER NO-UNDO INIT "".
FOR FIRST catalog
    WHERE (catalog.mail-user-num EQ in-mail-user-num)
      AND (catalog.Kind          EQ "Export")
    NO-LOCK:

    iErrDir = catalog.path.
END.

/* ��ନ�㥬 ��ࠬ��� � ����᪠�� ��楤��� ��ࠡ�⪨ */
DEFINE VARIABLE iParam      AS CHARACTER NO-UNDO.
DEFINE VARIABLE iProc       AS CHARACTER NO-UNDO.
iParam = GetXAttrValue("mail-user", STRING(in-mail-user-num), "PBparam").
iProc  = ENTRY(1, iParam, "|").
iParam = "file="      + inFileName + ";"
       + "arch-dir="  + iArchDir   + ";"
       + (IF (iOutDir NE "") THEN ("ans-dir="   + iOutDir  + ";") ELSE "")
       + (IF (iErrDir NE "") THEN ("error-dir=" + iErrDir  + ";") ELSE "")
       + ENTRY(2, iParam, "|").

RUN VALUE(iProc) (iParam) NO-ERROR.
