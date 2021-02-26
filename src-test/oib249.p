{globals.i}
/*
{intrface.get tmess}
{intrface.get xclass}
*/
DEF        PARAM BUFFER bop      FOR op.
DEF INPUT  PARAM        iParam  AS  CHAR NO-UNDO.
DEF OUTPUT PARAM        oResult AS  LOG  NO-UNDO INIT ?.
         

DEFINE VARIABLE iParam-249   AS CHARACTER NO-UNDO.
DEFINE VARIABLE iProc        AS CHARACTER NO-UNDO.
DEFINE VARIABLE mSeek_       AS CHARACTER NO-UNDO.
DEFINE VARIABLE vErrorText   AS CHARACTER NO-UNDO.
vErrorText = "".

FOR each code WHERE                   
   code.class EQ "ДопКонтрКЛБ"        
   AND code.parent EQ "ДопКонтрКЛБ"   
   and code.val  = "действует"
   NO-LOCK .
   iParam-249 = "".
   vErrorText = "".
   iParam-249 = string(bop.op).
   iProc  = code.name + ".p".
   if code.name = "" or code.name = ? then next.
   mSeek_ = search(iProc).
   if mSeek_ = ? then do :
      message "Процедура обработки " + iProc + " из классификатора ДопКонтрКЛБ не найдена." view-as alert-box.
      next.
   end.
   RUN VALUE(iProc) (iParam-249,OUTPUT vErrorText) NO-ERROR.                    
   if vErrorText = "" then do:
      oResult = yes.
   end.
   else do:
      oResult = no.
/*      leave. */
     return.
   end.
end. 