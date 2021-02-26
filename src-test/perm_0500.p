{globals.i}
{setdest.i}
{tmprecid.def}
{chkacces.i}
{sh-defs.i}
{card.i}
{clg-cr.err}          /* �訡�� �� ����⨨ ������஢ �� */
{intrface.get count}  /* �⮡� ࠡ�⠫ GetCounterNextValue */
{intrface.get xclass} /* �⮡� ࠡ�⠫� ����祭�� ��砫쭮�� ���祭�� ४����� */
{intrface.get corr}
{intrface.get date}
{intrface.get jloan}
{intrface.get instrum}
{intrface.get db2l}
{intrface.get tmess}
{intrface.get dpspc}
{intrface.get acct}
{intrface.get trnsl}
{intrface.get loan}
{intrface.get refer}    /* ������⥪� �㦡� �ࠢ�筨���. */
{intrface.get tmcod}
{intrface.get rights}   /* ������⥪� ��� ࠡ��� � �ࠢ��� � ��஫ﬨ. */

DEF VAR User_SURR   AS CHAR NO-UNDO.             /* �� ���� �।��⠢��� �ࠢ� */
DEF VAR i           AS DEC NO-UNDO.
DEF VAR j           AS DEC INIT 0 NO-UNDO.
DEF VAR k           AS DEC INIT 0 NO-UNDO.
DEF VAR vChv        AS LOGICAL INIT FALSE NO-UNDO.
DEF VAR vFilial     AS CHAR NO-UNDO.
DEF VAR users       AS CHAR INIT "" NO-UNDO.
DEF VAR User_ID     AS CHAR NO-UNDO INIT
'
BIS,
END
'.



form
     User_SURR label "UserID     "

with frame www overlay side-labels 1 col centered row 6 title color bright-white
"[ " + "������� ������������" + " ]" width 28.

do on endkey undo, return on error undo, retry with frame www:
display User_SURR.
set 
 User_SURR
editing:
readkey.
apply lastkey.
end.
end.
do on endkey undo, leave on error undo, leave with frame prn:
END.



MESSAGE "�� 㢥७� �� ��� �������� ����஢���� �ࠢ �� ���짮��⥫�" User_SURR "?" view-as alert-box QUESTION BUTTONS YES-NO title "������" set vChv.
IF vChv
THEN DO:

DO i = 1 TO LENGTH(User_ID):
   IF ((LENGTH(ENTRY(INT(i), User_ID, ',')) < 2) OR (ENTRY(INT(i), User_ID, ',') = "END"))
   THEN LEAVE.
   ELSE DO: 
            j = j + 1. 
        END.
END.



i = 0.
FOR EACH permission WHERE permission.class-code EQ "slave"
                      AND permission.surrogate  EQ User_SURR
         EXCLUSIVE-LOCK:
         IF AVAIL permission AND GetXattrValueEx("_user", STRING(permission.user-id), "filial-id", "") = '0500'
            THEN DO:
                     i = i + 1.
                     delete permission.
                 END.
END.
MESSAGE "������� ����ᥩ" string(int(i / 3)) "!" view-as alert-box title "���������".



FOR EACH _user WHERE _userid <> "" NO-LOCK:
    IF AVAIL _user 
         AND GetXattrValueEx("_user", STRING(_userid), "Blocked", "") = "�� �����஢��"
         AND GetXattrValueEx("_user", STRING(_userid), "filial-id", "") = '0500'
    THEN DO:
             k = k + 1.

             create permission.
             assign permission.class-code = "slave"
             permission.surrogate  = User_SURR
             permission.user-id    = _userid
             permission.method-id  = "r"
             permission.allow      = yes
             .
 
             create permission.
             assign permission.class-code = "slave"
             permission.surrogate  = User_SURR
             permission.user-id    = _userid
             permission.method-id  = "w"
             permission.allow      = yes
             .

             create permission.
             assign
             permission.class-code = "slave"
             permission.surrogate  = User_SURR
             permission.user-id    = _userid
             permission.method-id  = "d"
             permission.allow      = yes
             .
         END.
END.



MESSAGE "��������� ����ᥩ" string(k) "!" view-as alert-box title "���������".



END.
ELSE RETURN "ERROR".