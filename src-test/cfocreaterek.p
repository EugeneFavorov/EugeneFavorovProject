/*                                               
    ���� ����
      Comment: ��楤�� ��������� ४����� ��� ��� � ����ᨬ��� �� ��ࠬ���
   Parameters: vKodRek: � - ���㦨�����, � - �ਢ��祭�� , ��
         Uses: 
      Used by:
      Created: 16/10/2014 KAU
*/
{globals.i}
DEF INPUT PARAM vKodRek AS CHAR NO-UNDO.
DEF OUTPUT PARAM vRezult AS CHAR NO-UNDO.

DEF VAR vUser       AS CHAR NO-UNDO.
DEF VAR vUserOtd    AS CHAR NO-UNDO.

DEF VAR vCodeObsl   AS CHAR NO-UNDO.
DEF VAR vCodePrivl  AS CHAR NO-UNDO.
DEF VAR vCodePO     AS CHAR NO-UNDO. 

/*��।��塞 ��� ���짮��⥫� �����⨢襣� ��楤���*/
FIND FIRST _user WHERE 
   _user._userid EQ &IF DEFINED(user-code) &THEN {&user-code} &ELSE USERID("bisquit") &ENDIF
NO-LOCK NO-ERROR.

vUser = _user._userid.
vUserOtd = GetXattrValue("_USER",vUser,"���������").

FIND FIRST code WHERE code.class EQ 'CFO'
                AND code.val EQ '��'
                AND code.code BEGINS vUserOtd /*TODO*/
NO-LOCK NO-ERROR.
IF AVAIL code THEN vCodePO = code.code.
FIND FIRST code WHERE code.class EQ 'CFO'
                AND code.val EQ '�'
                AND code.code BEGINS vUserOtd /*TODO*/
NO-LOCK NO-ERROR.
IF AVAIL code THEN vCodePrivl = code.code.
FIND FIRST code WHERE code.class EQ 'CFO'
                AND code.val EQ '�'
                AND code.code BEGINS vUserOtd /*TODO*/
NO-LOCK NO-ERROR.
IF AVAIL code THEN vCodeObsl = code.code.

IF vKodRek EQ '�'
THEN DO:
    IF vCodePrivl NE ? AND vCodePrivl NE ''
    THEN vRezult = vCodePrivl.
    ELSE IF vCodePO NE ? AND vCodePO NE ''
    THEN vRezult = vCodePO.
    ELSE MESSAGE "��� ��襣� �⤥�����: " + STRING(vUserOtd) SKIP 
                 "�� ������ ��� �� � �����䨪��� CFO" VIEW-AS ALERT-BOX. 
END.
ELSE IF vKodRek EQ '�'
    THEN DO:
        IF vCodeObsl NE ? AND vCodeObsl NE ''
        THEN vRezult = vCodePrivl.
        ELSE IF vCodePO NE ? AND vCodePO NE ''
        THEN vRezult = vCodePO.
        ELSE MESSAGE "��� ��襣� �⤥�����: " + STRING(vUserOtd) SKIP
                    "�� ������ ��� �� � �����䨪��� CFO" VIEW-AS ALERT-BOX. 
    END.
    ELSE IF vKodRek EQ '��'
        THEN DO:
            IF vCodePO NE ? AND vCodePO NE ''
            THEN vRezult = vCodePO.
            ELSE MESSAGE "��� ��襣� �⤥�����: " + STRING(vUserOtd) SKIP
                        "�� ������ ��� �� � �����䨪��� CFO" VIEW-AS ALERT-BOX. 
        END. 
        ELSE MESSAGE "� ��楤��� CfoCreateRek.p ��।�� �� �ࠢ���� ��ࠬ��� �������� ⮫쪮 �/�/��" VIEW-AS ALERT-BOX.
        