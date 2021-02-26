   DEFINE INPUT  PARAMETER iProcName AS CHARACTER NO-UNDO. /*���� ��ࠬ��� ���᪠ � �����䨪���*/
   DEFINE INPUT  PARAMETER iVarName  AS CHARACTER NO-UNDO. /*��ன ��ࠬ��� ���᪠ � �����䨪���*/
   DEFINE INPUT  PARAMETER iTitle   AS CHARACTER NO-UNDO.  /* ��������� ����誨*/
   DEFINE OUTPUT PARAMETER UserStr   AS CHARACTER NO-UNDO. /**/
 
   {globals.i}
   {justasec}
   {intrface.get refer}    /* ������⥪� �㦡� �ࠢ�筨���. */
   {prn-doc.def &with_proc=YES}
   /* ������砥� tmprecid */
   {tmprecid.def}
   {intrface.get cust}
   {intrface.get tmess}
   
   def var i        as int64 NO-UNDO.
   def var str      as char  NO-UNDO.
   def var str_menu as char  NO-UNDO.
   DEF var iBranch  AS CHAR  NO-UNDO.
   DEF var user_   AS CHAR  NO-UNDO.
   UserStr = "".
   user_ =  USERID("bisquit").   
   iBranch = GetXattrValueEx("_user",user_,"office",?).
   if iTitle = "" or iTitle = ? then do:
       "[ �롨�� �����ᠭ� (" + user_ + " " + iBranch + ")]".
   end.

   if iBranch  = ? then do:
     message "�� ������ office ��� ���짮��⥫� " user_  view-as alert-box.
     return .
   end.   
   TRE:
   do i = 1 to 50 :
      str = trim(iProcName) + "," + trim(iVarName)  + "," + trim(iBranch)  + "," + trim(string(i)).
      UserStr = GetRefVal ("signat",today, str ).
      if UserStr = "" or UserStr = ? then LEAVE TRE.      
      if i = 1 then do:
         str_menu = entry(2,UserStr,"|").
      end.
      else  do:
         str_menu = str_menu + "," + entry(2,UserStr,"|").
      end.
  end. 
  if str_menu = "" then do:
     message "�����ᠭ⮢ ��� ���� " + iBranch +  " ��।��񭭮�� �� �� office ���짮��⥫� " +  user_ + " �� �������."  view-as alert-box.     
     return.
  end.
  RUN messmenu.p (7,iTitle ,"",str_menu).
  str = trim(iProcName) + "," + trim(iVarName)  + "," + trim(iBranch)  + "," + trim(string(pick-value)).
  UserStr = GetRefVal ("signat",today, str ).
  UserStr = entry(1,UserStr,"|").
  return. 












