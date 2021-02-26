/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2015 ЗАО "Банковские информационные системы"
     Filename: chk-role.p
      Comment: TT:0240247 Миграция. выгодоприобретатели, бенефициары и обновление анкеты клиента. 
   Parameters:
         Uses:
      Used by:
      Created: 22/05/2015 15:15 KMBIS Миграция. Контроль заведения связанных ролей.
     Modified: 
*/
{globals.i}
DEF INPUT PARAM iTable  AS  CHAR  NO-UNDO.
DEF INPUT PARAM iRid    AS  RECID NO-UNDO.

{intrface.get db2l}
{intrface.get xclass}

DEF VAR mRoleKontr AS LOG  NO-UNDO. /* Нужно контролировать роли субъекта */
DEF VAR mRoleMask  AS CHAR NO-UNDO. /* Список контролируемых ролей        */
DEF VAR mSurr      AS CHAR NO-UNDO. 

DEF BUFFER bRole  FOR cust-role.

ASSIGN
   mRoleKontr = NO
   mRoleMask  = fGetSetting("КонтрКлВвод", "СписРоли",  "")
   mSurr      = GetSurrogateByRecid(iTable, iRid).
.

IF iTable EQ "person" THEN
DO:
   IF GetXAttrValueEx(iTable, mSurr, "Предпр", "") NE "Предпр" THEN
      RETURN "".

END. /* IF iTable EQ "person" THEN */

MESSAGE "Есть у клиента выгодоприобретатель или бенефициарный владелец?"
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
      RETURN "Укажите выгодоприобретателя или бенефициарного владелеца!".

END. /* IF vRoleKontr EQ YES THEN */

RETURN "".