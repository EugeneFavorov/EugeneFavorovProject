/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2010 ЗАО "Банковские информационные системы"
     Filename: cfobrw.eh
      Comment: ЦФО - триггеры формы
   Parameters:
         Uses:
      Used by:
      Created: ayv   
     Modified:    
*/

ON LEAVE OF mBranchID IN FRAME edit
DO:
    mDummy = mBranchID:SCREEN-VALUE.
	IF SUBSTRING(mDummy,1,1) NE "0" THEN
		MESSAGE "Неправильный код подразделения!" VIEW-AS ALERT-BOX.
END.