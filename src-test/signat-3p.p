   DEFINE INPUT  PARAMETER iProcName AS CHARACTER NO-UNDO. /*���� ��ࠬ��� ���᪠ � �����䨪���*/
   DEFINE INPUT  PARAMETER iVarName  AS CHARACTER NO-UNDO. /*��ன ��ࠬ��� ���᪠ � �����䨪���*/
   DEFINE INPUT  PARAMETER iBranch   AS CHARACTER NO-UNDO. /*��⨩ ��ࠬ��� ���᪠ � �����䨪���*/
   DEFINE INPUT  PARAMETER iTitle    AS CHARACTER NO-UNDO.  /* ��������� ����誨*/
   DEFINE OUTPUT PARAMETER UserStr   AS CHARACTER NO-UNDO. /**/
 
   {globals.i}
   {justasec}
   {intrface.get refer}    /* ������⥪� �㦡� �ࠢ�筨���. */
   {prn-doc.def &with_proc=YES}
   /* ������砥� tmprecid */
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
/*
   DEF var iBranch  AS CHAR  NO-UNDO.
*/
   DEF var user_   AS CHAR  NO-UNDO.
   UserStr = "".
   user_ =  USERID("bisquit").   
/*
   iBranch = GetXattrValueEx("_user",user_,"�⤥�����",?).
*/
   if iTitle = "" or iTitle = ? then do:
       iTitle = "[ �롥�� �����ᠭ� (" + user_ + " " + iBranch + ")]".
   end.

/*
   if iBranch  = ? then do:
     message "�� ������ �� �⤥����� ��� ���짮��⥫� " user_  view-as alert-box.
     return .
   end.   
*/
   TRE:
   do i = 1 to 100 :
      str = trim(iProcName) + "," + trim(iVarName)  + "," + trim(iBranch)  + "," + trim(string(i)).
      UserStr = GetRefVal ("signat",today, str ).
      if UserStr NE "" AND UserStr NE ? then      
      DO:
         if i = 1 then do:
            IF NUM-ENTRIES(UserStr,"|") = 1 THEN
        	str_menu = GetXattrValueEx("_user",entry(1,UserStr,"|"),"����",?).
        	ELSE str_menu = entry(2,UserStr,"|"). 
         end.
         else do:
            IF NUM-ENTRIES(UserStr,"|") = 1 THEN
            str_menu = GetXattrValueEx("_user",entry(1,UserStr,"|"),"����",?).
        	ELSE str_menu = entry(2,UserStr,"|").
         end.
         create tFIO.
         ASSIGN
            tFIO.fFIO = str_menu
            tFIO.fNum = i.
      END.
  end. 

   i = 1.
   for each tFio :
      if i = 1 then do:
         str_menu = tFIO.fFIO.
         str_num  = string(tFIO.fNum).
      end.
      else  do:
         str_menu = str_menu + "," + tFIO.fFIO.
         str_num  = str_num + "," + string(tFIO.fNum).
      end.
     i = i + 1.
   end. 
   if str_menu = "" then do:
    /* message "�����ᠭ⮢ ��� ���ࠧ������� " + iBranch +  " ��।��񭭮�� �� �� �⤥����� ���짮��⥫� " +  user_ + " �� �������."  view-as alert-box.*/
     return.
  end.
  RUN messmenu.p (7,iTitle ,"",str_menu).
  if int64(pick-value) = 0 then do:
     UserStr = ?.
  end.
  else do:
     str = trim(iProcName) + "," + trim(iVarName)  + "," + trim(iBranch)  + "," + trim(entry(int64(pick-value),str_num)).
     UserStr = GetRefVal ("signat",today, str ).
     UserStr = trim(entry(1,UserStr,"|")).
  end.
  return. 
   
   
   
   
   
   