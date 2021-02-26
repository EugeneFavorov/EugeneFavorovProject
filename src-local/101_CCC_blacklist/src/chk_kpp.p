DEF INPUT  PARAM in-cust-id AS CHAR   NO-UNDO.   /* ���ண�� ������ */
DEF INPUT  PARAM in-param   AS CHAR   NO-UNDO.   /* ���祭�� ।����㥬��� �� */
/*
message
in-cust-id skip
in-param  
view-as alert-box.
*/
{globals.i}             /* �������� ��६���� ��ᨨ. */
{intrface.get xclass}   /* ������⥪� �����㬥�⮢ ����奬�. */
{intrface.get tmess}
{intrface.get strng}      /* �����㬥��� ��� ࠡ��� � ��ப���  */

DEFINE VARIABLE vTmpStr   AS CHAR NO-UNDO.
DEFINE VARIABLE vRes      AS LOG  NO-UNDO.
DEFINE VARIABLE mProvKPP  AS CHARACTER NO-UNDO.

mProvKPP = FGetSetting("�஢�ન","����஢���","").

IF mProvKPP <> "��" THEN
   RETURN.

IF LENGTH(in-param) <> 9 THEN 
DO:
   RUN Fill-SysMes IN h_tmess ("", "", "0", "��������! ������⢮ ᨬ����� ��� ������ ���� 9.").         
   RETURN ERROR.
END.

/*
������� ���: NNNN�����
NNNN (4 �����) - ��� ���������� �࣠��, ����� ����室��� �ந������� ᢥ�� � �����䨪��஬ ��, �� ����稥 ⠬ ⠪�� �����.
PP (2 �����) - ��稭� ���⠭���� �� ��� (��� ᢥ�����). ������ P �।�⠢��� ᮡ�� ���� ��� ��������� �㪢� ��⨭᪮�� ��䠢�� �� A �� Z. 
XXX (3 �����) - ���浪��� ����� ���⠭���� �� ��� (��� ᢥ�����) � ��������� �࣠�� �� ᮮ⢥�����饬� �᭮�����. ����� �� 0 �� 9
*/

vTmpStr = SUBSTR(in-param,1,4).         /*NNNN (4 �����)*/
IF GetCode("��",vTmpStr) = "" OR GetCode("��",vTmpStr) =  ? THEN
DO:
   RUN Fill-SysMes IN h_tmess ("", "", "0", "��������! ����襭� ������� ���.").         
   RETURN ERROR.
END.

vTmpStr = SUBSTR(in-param,5,2).         /*PP (2 �����)*/
RUN Check-Ascii-Set(INPUT vTmpStr,INPUT "3,4",INPUT "", OUTPUT vRes).
IF vRes THEN 
DO:
   RUN Fill-SysMes IN h_tmess ("", "", "0", "��������! ����襭� ������� ���.").         
   RETURN ERROR.
END.

vTmpStr = SUBSTR(in-param,7).         /*XXX (3 �����) */
RUN Check-Ascii-Set(INPUT vTmpStr,INPUT "3",INPUT "", OUTPUT vRes).
IF vRes THEN 
DO:
   RUN Fill-SysMes IN h_tmess ("", "", "0", "��������! ����襭� ������� ���.").         
   RETURN ERROR.
END.
                           /*










                             */