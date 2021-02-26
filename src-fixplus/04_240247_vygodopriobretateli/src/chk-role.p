/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2015 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: chk-role.p
      Comment: TT:0240247 ������. �룮���ਮ���⥫�, �����樠�� � ���������� ������ ������. 
   Parameters:
         Uses:
      Used by:
      Created: 22/05/2015 15:15 KMBIS ������. ����஫� ��������� �易���� ஫��.
     Modified: 
*/
{globals.i}
DEF INPUT PARAM iTable  AS  CHAR  NO-UNDO.
DEF INPUT PARAM iRid    AS  RECID NO-UNDO.

{intrface.get db2l}
{intrface.get xclass}

DEF VAR mRoleKontr AS LOG  NO-UNDO. /* �㦭� ����஫�஢��� ஫� ��ꥪ� */
DEF VAR mRoleMask  AS CHAR NO-UNDO. /* ���᮪ ����஫��㥬�� ஫��        */
DEF VAR mSurr      AS CHAR NO-UNDO. 

DEF BUFFER bRole  FOR cust-role.

ASSIGN
   mRoleKontr = NO
   mRoleMask  = fGetSetting("�����������", "���ᐮ��",  "")
   mSurr      = GetSurrogateByRecid(iTable, iRid).
.

IF iTable EQ "person" THEN
DO:
   IF GetXAttrValueEx(iTable, mSurr, "�।��", "") NE "�।��" THEN
      RETURN "".

END. /* IF iTable EQ "person" THEN */

MESSAGE "���� � ������ �룮���ਮ���⥫� ��� �����樠�� ��������?"
VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE mRoleKontr.

IF mRoleKontr EQ YES THEN
DO:

   RUN browseld.p (iTable      + CHR(1) + "cust-cust",
                   "in-class"  + chr(1) + "in-surr" + chr(1) + "in-xattr"  + chr(1) + "class-code",
                   iTable      + chr(1) + mSurr     + chr(1) + "cust-cust" + chr(1) + mRoleMask,
                   "class-code",
                   4).
   FIND FIRST bRole WHERE bRole.file-name EQ iTable
                      AND bRole.surrogate EQ mSurr
                      AND CAN-DO(mRoleMask, bRole.class-code)
                    NO-LOCK NO-ERROR.
   IF NOT AVAIL(bRole) THEN
      RETURN "������ �룮���ਮ���⥫� ��� �����樠୮�� ��������!".

END. /* IF vRoleKontr EQ YES THEN */

RETURN "".