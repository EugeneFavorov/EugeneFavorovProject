   DEFINE INPUT  PARAMETER iProcName AS CHARACTER NO-UNDO. /*Первый параметр поиска в классификаторе*/
/*   DEFINE OUTPUT PARAMETER UserStr   AS CHARACTER NO-UNDO. /**/
*/ 
   {globals.i}
   {justasec}
   {intrface.get refer}    /* Библиотека службы справочников. */
   {prn-doc.def &with_proc=YES}
   /* подключаем tmprecid */
   {tmprecid.def}
   {intrface.get cust}
   {intrface.get tmess}

   DEF var UserStr  AS CHAR  NO-UNDO.
   def var i        as int64 NO-UNDO.
   def var str      as char  NO-UNDO.
   def var str_menu as char  NO-UNDO.
   DEF var iBranch  AS CHAR  NO-UNDO.
   DEF var user_   AS CHAR  NO-UNDO.
   UserStr = "".
   user_ =  USERID("bisquit").   
   iBranch = GetXattrValueEx("_user",user_,"Отделение",?).
   if iBranch  = ? then do:
     message "Не найден ДР Отделение для пользователя " user_  view-as alert-box.
     return .
   end.   
   TRE:
   do i = 1 to 9 :

      str = trim(iProcName) + ","  + trim(iBranch)  + "," + trim(string(i)).
      UserStr = GetRefVal ("mail-add",today, str ).
      if UserStr = "" or UserStr = ? then next.      
      if i = 1 then do:
         str_menu = entry(1,GetXattrValueEx("_user",UserStr,"e-mail",""),"@").
      end.
      else  do:
         str_menu = str_menu + "," + entry(1,GetXattrValueEx("_user",UserStr,"e-mail",""),"@").
      end.
   end. 
   UserStr = str_menu.
   return UserStr. 
   
   
   
   
   
   