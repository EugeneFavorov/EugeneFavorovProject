   DEFINE INPUT  PARAMETER iProcName AS CHARACTER NO-UNDO. /*Первый параметр поиска в классификаторе*/
   DEFINE INPUT  PARAMETER iVarName  AS CHARACTER NO-UNDO. /*Второй параметр поиска в классификаторе*/
   DEFINE INPUT  PARAMETER iTitle   AS CHARACTER NO-UNDO.  /* Заголовок менюшки*/
   DEFINE OUTPUT PARAMETER UserStr   AS CHARACTER NO-UNDO. /**/
 
   {globals.i}
   {justasec}
   {intrface.get refer}    /* Библиотека службы справочников. */
   {prn-doc.def &with_proc=YES}
   /* подключаем tmprecid */
   {tmprecid.def}
   {intrface.get cust}
   {intrface.get tmess}

   DEFINE TEMP-TABLE tFIO  NO-UNDO
      FIELD fFIO         AS CHARACTER
      FIELD fNum         AS int64
      INDEX iFIO fFIO
   .
      
   def var i        as int64 NO-UNDO.
   def var str      as char  NO-UNDO.
   def var str_menu as char  NO-UNDO.
   def var str_num  as char  NO-UNDO.
   DEF var iBranch  AS CHAR  NO-UNDO.
   DEF var user_   AS CHAR  NO-UNDO.
   UserStr = "".
   user_ =  "BIS".   
   iBranch = GetXattrValueEx("_user",user_,"Отделение",?).
   if iBranch  = ? then do:
     return .
   end.   
   TRE:
   do i = 1 to 1 :
      str = trim(iProcName) + "," + trim(iVarName)  + "," + trim(iBranch)  + "," + trim(string(i)).
      UserStr = GetRefVal ("signat",today, str ).
      if UserStr NE "" AND UserStr NE ? then      
      DO:
         if i = 1 then do:
            IF NUM-ENTRIES(UserStr,"|") = 1 THEN
        	str_menu = GetXattrValueEx("_user",entry(1,UserStr,"|"),"ФИОП",?).
        	ELSE str_menu = entry(2,UserStr,"|"). 
         end.
         else do:
            IF NUM-ENTRIES(UserStr,"|") = 1 THEN
            str_menu = GetXattrValueEx("_user",entry(1,UserStr,"|"),"ФИОП",?).
        	ELSE str_menu = entry(2,UserStr,"|").
         end.
         create tFIO.
         ASSIGN
            tFIO.fFIO = str_menu
            tFIO.fNum = i.
      END.
   end. 
   for each tFio :
       str_menu = tFIO.fFIO.
       str_num  = string(tFIO.fNum).
   end. 
   if str_menu = "" then do:
      return.
   end.
   str = trim(iProcName) + "," + trim(iVarName)  + "," + trim(iBranch)  + "," + trim(str_num).
   UserStr = GetRefVal ("signat",today, str ).
   UserStr = trim(entry(1,UserStr,"|")).
   return. 
   
   
   
   
   
   