/*
                ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright:  (C) 1992-2017 �� "������᪨� ���ଠ樮��� ��⥬�"
     Filename:  chk_ogrn.p
      Comment:  ��⮤ update �� ४����� ���� �� 䨧. ���� 
   Parameters:  ���ண�� ������,���祭�� ।����㥬��� ��
         Uses:  
      Used BY:
      Created:  26.10.17 KSBIS
     Modified:
*/

DEF INPUT  PARAM iN-pers-id AS CHAR   NO-UNDO.   /* ���ண�� ������ */
DEF INPUT  PARAM iN-param   AS CHAR   NO-UNDO.   /* ���祭�� ।����㥬��� �� */

{globals.i}             /* �������� ��६���� ��ᨨ. */
{intrface.get xclass}   /* ������⥪� �����㬥�⮢ ����奬�. */
{intrface.get tmess}
{intrface.get strng}      /* �����㬥��� ��� ࠡ��� � ��ப���  */


DEFINE VARIABLE mPredpr   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mProvOGRN AS CHARACTER NO-UNDO.

mProvOGRN = FGetSetting("�஢�ન","����஢����","").

IF mProvOGRN <> "��" THEN
   RETURN.

FIND FIRST person WHERE person.person-id = INT64(iN-pers-id) NO-LOCK NO-ERROR.
mPredpr = GetXAttrValueEx("person", STRING(person.person-id), "�।��", "").

IF mPredpr = "�।��" AND LENGTH(in-param) <> 15 THEN 
DO:
   RUN Fill-SysMes IN h_tmess ("", "", "0", "��������! ��ଠ� ���� ��� �।�ਭ���⥫� 15 ᨬ�����").
   RETURN ERROR.
END.

