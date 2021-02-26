DEF INPUT  PARAM iOpTemplRec AS RECID   NO-UNDO.
DEF OUTPUT PARAM oResultVal  AS INT64 NO-UNDO INIT 0.

DEF VAR mLoanHandle AS HANDLE.

DEF VAR mContCode	AS CHAR NO-UNDO.
DEF VAR mContract	AS CHAR NO-UNDO.
DEF VAR mShablName	AS CHAR	NO-UNDO.
DEF VAR pp	AS LOGICAL NO-UNDO.
{globals.i}
{loan_sn.i}
/*{lshpr.pro}*/
/*{loantran.pro}*/

pp = yes.




FIND op-template WHERE RECID(op-template) = iOpTemplRec NO-LOCK.

FIND op-kind OF op-template NO-LOCK.

IF op-kind.op-kind = 'ВыдАвто+' then DO:
	mShablName = 'raspvydap|'.
	MESSAGE "Печатать распоряжение на выдачу?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE pp.
END.


/*IF op-kind.op-kind = 'ВыдОпл' then DO:
	mShablName = 'raspperapp|'.
	MESSAGE "Печатать распоряжение на перечисление продавцу?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE pp.
END.

IF op-kind.op-kind = 'ВыдСК2' THEN DO:
	run messmenu.p(10,"","","Распоряжение СК Жизнь, Распоряжение СК Каско, Не печатать" ).
	
	CASE INT64(pick-value):
		WHEN 1 THEN MESSAGE "Жизнь" VIEW-AS ALERT-BOX.
		WHEN 2 THEN MESSAGE "Каско" VIEW-AS ALERT-BOX.
		WHEN 3 THEN MESSAGE "НЕТ" VIEW-AS ALERT-BOX.
	END.
		
END.
*/



RUN LOAN_VALID_HANDLE (INPUT-OUTPUT mLoanHandle).



IF NOT VALID-HANDLE(mLoanHandle)
THEN DO:
   MESSAGE "Не установлены назначение и номер договора!"
     VIEW-AS ALERT-BOX.
   oResultVal = 0.
END.

ASSIGN
   mContract = ENTRY(1,mLoanHandle:PRIVATE-DATA)
   mContCode = ENTRY(2,mLoanHandle:PRIVATE-DATA)
   .




/*
RUN RE_B_LOAN (mContract,
               mContCode,
               BUFFER loan).
*/



oResultVal = 1.


IF pp THEN RUN precrdprint.p (mShablName + mContract + "," + mContCode).

                                   








oResultVal = 1.


















