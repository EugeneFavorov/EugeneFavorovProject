/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2010 ЗАО "Банковские информационные системы"
     Filename: dog_bk.p
      Comment: Формирование договора оказания услуг электронного банкинга в системе <iBank 2> для физ.лиц
   Parameters:  
      Created: ayv
*/

{globals.i}
{chkacces.i}
{sh-defs.i}
{intrface.get xclass}
{intrface.get instrum}
{flt-val.i}
{intrface.get tmess}
{tmprecid.def}
{prn-doc.def &with_proc=YES}

DEF VAR mTempC AS CHAR NO-UNDO.
DEF VAR mT	   AS CHAR NO-UNDO.
DEF VAR dblank AS DATE NO-UNDO.
DEF VAR sblank AS INT  NO-UNDO.
DEF VAR mblank AS INT  NO-UNDO INITIAL 1.
DEF VAR iblank AS CHAR NO-UNDO FORMAT "x(64)".
DEF VAR tblank AS CHAR NO-UNDO FORMAT "x(64)".
DEF VAR eblank AS CHAR NO-UNDO INITIAL "1" FORMAT "x(3)".
DEF VAR pblank AS CHAR NO-UNDO FORMAT "x(50)".
DEF VAR nblank AS CHAR NO-UNDO FORMAT "x(50)".
DEF VAR oblank AS CHAR NO-UNDO FORMAT "x(50)".
DEF VAR	FlExst AS LOG  NO-UNDO.
DEF VAR rblank1 AS LOG 
	LABEL "Internet-банкинг"
	VIEW-AS TOGGLE-BOX
    NO-UNDO.

DEF VAR rblank2 AS LOG
	LABEL "PC-банкинг"
	VIEW-AS TOGGLE-BOX
    NO-UNDO.
	
DEF VAR tmpL AS LOG NO-UNDO INITIAL FALSE.	
DEF STREAM vvs.
DEF STREAM ws.
DEF VAR txtLine AS CHAR NO-UNDO.
DEF VAR i AS INT NO-UNDO.	
DEF VAR fname AS CHAR NO-UNDO.
DEF VAR delim AS CHAR INITIAL ": " FORMAT "x(2)" NO-UNDO.
DEF VAR eol AS CHAR FORMAT "x(2)" NO-UNDO.
eol = chr(13) + chr(10).

DEF TEMP-TABLE kb
	FIELD ib   AS CHAR INITIAL "НЕТ"
	FIELD pcb  AS CHAR INITIAL "НЕТ"
	FIELD auth AS CHAR
	FIELD ipf  AS CHAR
	FIELD phon AS CHAR
	FIELD ecp  AS CHAR
	FIELD ip   AS CHAR
	FIELD foun AS CHAR
	FIELD accm AS CHAR
	FIELD acca AS CHAR
	FIELD date AS CHAR.
	
/*выбор подписантов*/
{sign_select.i}

FOR EACH tmprecid,
	FIRST person WHERE
		tmprecid.id = RECID(person)
NO-LOCK:
	
	mTempC = GetXAttrValue("person",string(person.person-id),"CID").
	IF mTempC EQ '' 
	OR mTempC EQ '-1' THEN
	DO:
    	mTempC = GetXAttrValueEx("person",string(person.person-id),"CIDIP","").
		MESSAGE 'Неправильно введен доп.реквизит "Код клиента в БД Омска"' VIEW-AS ALERT-BOX.
	END.

	fname = "/home2/bis/quit41d/imp-exp/0000/bss/reg/" + mTempC + ".txt".
	
	FlExst = SEARCH(fname) <> ?.
	IF FlExst = TRUE 
	AND mTempC NE '' 
   	AND mTempC NE '-1' THEN 
   	DO:
		INPUT STREAM vvs FROM VALUE(fname)
			CONVERT  TARGET "IBM866"  SOURCE "1251".
		DO i=1 TO 9 BY 1:
			IMPORT STREAM vvs UNFORMATTED txtLine.
			mTempC = SUBSTRING(txtLine,33,LENGTH(txtLine) - 31).
			CASE i:
				WHEN 1 THEN DO:
					IF mTempC EQ "ДА" THEN rblank1 = TRUE.
					ELSE rblank1 = FALSE.
				END.
				WHEN 2 THEN DO:
					IF mTempC EQ "ДА" THEN rblank2 = TRUE.
					ELSE rblank2 = FALSE.
				END.
				WHEN 3 THEN
					IF mTempC EQ "Не использовать" THEN mblank = 1. 
					ELSE IF mTempC EQ "50000 руб." THEN mblank = 2.
						 ELSE mblank = 3.
				WHEN 4 THEN 
					iblank = mTempC.
				WHEN 5 THEN 
					tblank = mTempC.
				WHEN 6 THEN 
					eblank = mTempC.
				WHEN 7 THEN 
					nblank = mTempC.
				WHEN 8 THEN
					oblank = mTempC.
				OTHERWISE DO:
					mT = ENTRY(1,mTempC,".") + "." + ENTRY(2,mTempC,".") + "." + ENTRY(3,mTempC,".").
					dblank = DATE(mT).
					tmpL = TRUE.
				END.
			END CASE.
		END.
		INPUT STREAM vvs CLOSE.
	END.
	ELSE DO:
		nblank = person.name-last + " " + person.first-names.
		IF GetXAttrValue("person",string(person.person-id),"ДатаОГРН") <> "" THEN DO:
			mTempC = entry(1,GetXAttrValue("person",string(person.person-id),"ДатаОГРН"),"/") + "." + entry(2,GetXAttrValue("person",string(person.person-id),"ДатаОГРН"),"/") + "." + entry(3,GetXAttrValue("person",string(person.person-id),"ДатаОГРН"),"/").
			oblank = "Свидетельства №" + GetXAttrValue("person",string(person.person-id),"ОГРН") + " от " + mTempC + " г.".
		END.
	END.

IF tmpL EQ FALSE THEN dblank = TODAY.
	
PAUSE 0.

DEFINE FRAME frame-set
   dblank 			  LABEL 		"Дата              " SKIP
   "Режим работы :"
   rblank1 SKIP
   rblank2 AT ROW 3 COL 16 SKIP
   mblank VIEW-AS RADIO-SET VERTICAL
                      RADIO-BUTTONS "Не использовать",1, "50000 руб.",2, "более 50000 руб.",3
                      LABEL         "Многофакторная аутентификация" SKIP   
   iblank 			  LABEL 		"IP-фильтр" SKIP
   tblank             LABEL         "Телефоны " SKIP
   eblank			  LABEL			"Количество групп ЭЦП   "SKIP
   nblank			  LABEL			"ИП            "SKIP
   oblank			  LABEL			"На основании  "SKIP
   WITH OVERLAY SIDE-LABEL CENTERED ROW 9
        TITLE COLOR BRIGHT-WHITE "[ Установка параметров договора ]" .

DO ON ERROR UNDO, LEAVE ON ENDKEY UNDO, LEAVE WITH FRAME frame-set:
	DISPLAY dblank rblank1 rblank2 mblank iblank tblank eblank nblank oblank.
	ENABLE dblank rblank1 rblank2 mblank iblank tblank eblank nblank oblank WITH FRAME frame-set.
	
	ON VALUE-CHANGED OF dblank,rblank1,rblank2,mblank,iblank,tblank,eblank,nblank,oblank IN FRAME frame-set DO:
		ASSIGN rblank1 rblank2 mblank iblank tblank eblank nblank oblank.
		IF dblank <> ? THEN ASSIGN dblank.
	END.
	
	WAIT-FOR GO OF FRAME frame-set.

END.

HIDE FRAME frame-set.

DEF VAR pr AS LOG 
	LABEL "Договор с приложениями"
	VIEW-AS TOGGLE-BOX
    NO-UNDO.
DEF VAR pr1 AS LOG 
	LABEL "Приложение 1"
	VIEW-AS TOGGLE-BOX
    NO-UNDO.
DEF VAR pr2 AS LOG 
	LABEL "Приложение 2"
	VIEW-AS TOGGLE-BOX
    NO-UNDO.
DEF VAR pr3 AS LOG 
	LABEL "Приложение 3"
	VIEW-AS TOGGLE-BOX
    NO-UNDO.
DEF VAR pr4 AS LOG 
	LABEL "Приложение 4"
	VIEW-AS TOGGLE-BOX
    NO-UNDO.
DEF VAR pr5 AS LOG 
	LABEL "Приложение 5"
	VIEW-AS TOGGLE-BOX
    NO-UNDO.
DEF VAR pr6 AS LOG 
	LABEL "Приложение 6"
	VIEW-AS TOGGLE-BOX
    NO-UNDO.
DEF VAR pr7 AS LOG 
	LABEL "Приложение 7"
	VIEW-AS TOGGLE-BOX
    NO-UNDO.

DEFINE FRAME printset
   pr SKIP
   pr1 SKIP
   pr2 SKIP
   pr3 SKIP
   pr4 SKIP
   pr5 SKIP
   pr6 SKIP
   pr7 SKIP
   WITH OVERLAY SIDE-LABEL CENTERED ROW 9
        TITLE COLOR BRIGHT-WHITE "[ Выберите документы на печать ]" .

DO ON ERROR UNDO, LEAVE ON ENDKEY UNDO, LEAVE WITH FRAME printset:
	DISPLAY pr pr1 pr2 pr3 pr4 pr5 pr6 pr7.
	ENABLE pr pr1 pr2 pr3 pr4 pr5 pr6 pr7 WITH FRAME printset.
	
	ON VALUE-CHANGED OF pr,pr1,pr2,pr3,pr4,pr5,pr6,pr7 IN FRAME printset DO:
		ASSIGN pr pr1 pr2 pr3 pr4 pr5 pr6 pr7.
	END.
	
	WAIT-FOR GO OF FRAME printset.

END.
	
	CREATE kb.

	IF rblank1 EQ TRUE THEN DO:
		RUN Insert_TTName ("Work1","X").
		kb.ib = "ДА".
	END.
	
	IF rblank2 EQ TRUE THEN DO:
		RUN Insert_TTName ("Work2","X").
		kb.pcb = "ДА".
	END.
	
	RUN Insert_TTName ("ECP",eblank).
	kb.ecp = eblank.
	
	IF tblank = "" THEN tblank = "-".
	RUN Insert_TTName ("Phone",tblank).
	kb.pho = tblank.
	
	IF iblank = "" THEN iblank = "-".
	RUN Insert_TTName ("IPFil",iblank).
	kb.ipf = iblank.

	CASE mblank:
		WHEN 1 THEN DO:
			RUN Insert_TTName ("Auth1","X").
			kb.auth = "Не использовать".
		END.
		WHEN 2 THEN DO:
			RUN Insert_TTName ("Auth2","X").
			kb.auth = "50000 руб.".
		END.
		WHEN 3 THEN DO:
			RUN Insert_TTName ("Auth3","X").
			kb.auth = "Более 50000 руб.".
		END.
	END CASE.
	
	mTempC = "Индивидуальный предприниматель".
	RUN Insert_TTName ("ClientSh",mTempC).
	
	IF oblank = "" THEN DO:
		mTempC = entry(1,GetXAttrValue("person",string(person.person-id),"ДатаОГРН"),"/") + "." + entry(2,GetXAttrValue("person",string(person.person-id),"ДатаОГРН"),"/") + "." + entry(3,GetXAttrValue("person",string(person.person-id),"ДатаОГРН"),"/").
		mTempC = "Свидетельства №" + GetXAttrValue("person",string(person.person-id),"ОГРН") + " от " + mTempC + " г.".
	END.
	ELSE  mTempC = oblank.
	RUN Insert_TTName ("CliFound",mTempC).
	kb.foun = mTempC.

	
	mTempC = GetXAttrValue("person",string(person.person-id),"ОГРН").
	RUN Insert_TTName ("OGRN",mTempC).
	
	mTempC = GetXAttrValue("person",string(person.person-id),"КПП").
	IF mTempC NE "000000000" THEN RUN Insert_TTName ("KPP",mTempC).
	
	mTempC = STRING(person.inn).
	RUN Insert_TTName ("INN",mTempC).
	
	mTempC = GetXAttrValue("person",string(person.person-id),"CIDIP").
	IF mTempC EQ '-1' THEN
	    mTempC = '______'.
	RUN Insert_TTName ("DogNum",mTempC).
	
	IF nblank = "" THEN 
		mTempC = person.name-last + " " + person.first-names.
	ELSE
		mTempC = nblank.
	kb.ip = mTempC.
	
	IF mTempC NE "" THEN 
	DO:
		IF NUM-ENTRIES(mTempC, " ") EQ 3 THEN 
		DO:
			mT = SUBSTRING(ENTRY(2,mTempC," "),1,1,"CHARACTER") + ". " +
				 SUBSTRING(ENTRY(3,mTempC," "),1,1,"CHARACTER") + ". " +
				 ENTRY(1,mTempC," ").
			RUN Insert_TTName ("CliRepSh",mT).
		END.
		ELSE RUN Insert_TTName ("CliRepSh",mTempC).
	END.
	ELSE RUN Insert_TTName ("CliRepSh","").
	
	RUN Insert_TTName ("CliPostViza","Индивидуальный предприниматель").
	
	mTempC = "".
	RUN RetAdr.p(person.person-id,"Ч","АдрПроп",?,OUTPUT mTempC).
    RUN Insert_TTName("AddReg",mTempC). 
	
	RUN RetAdr.p(person.person-id,"Ч","АдрФакт",?,OUTPUT mTempC).
	IF mTempC = "" THEN
		RUN RetAdr.p(person.person-id,"Ч","АдрПроп",?,OUTPUT mTempC).
    RUN Insert_TTName("AddFact",mTempC).
	
	mTempC = person.name-last + " " + person.first-names.
	RUN Insert_TTName ("CliRep",mTempC).

	mTempC = person.name-last + " " + person.first-names.
	RUN Insert_TTName ("Client",mTempC).
	
	mTempC = STRING(DAY(dblank)) +  " " + 
	ENTRY(MONTH(dblank),"января,февраля,марта,апреля,мая,июня,июля,августа,сентября,октября,ноября,декабря") 
	+ STRING(YEAR(dblank)," 9999г.").
	RUN Insert_TTName ("Date",mTempC).
	kb.date = STRING(DAY(dblank)) + "." + STRING(MONTH(dblank)) + "." + STRING(YEAR(dblank)).

	FIND FIRST acct WHERE
		acct.cust-id EQ person.person-id
		AND acct.contract EQ "Расчет"
		NO-LOCK NO-ERROR.
	IF AVAILABLE acct THEN DO:
		mTempC = acct.number.
		RUN Insert_TTName ("AccMain",mTempC).
		kb.accm = mTempC.
	END.
	
	mTempC = "".
	FOR EACH acct WHERE	acct.cust-id EQ person.person-id
		AND ( acct.contract EQ "Текущ" OR acct.contract MATCHES "Транз*" )
		AND acct.currency NE "" NO-LOCK 
		BY acct.cust-id:
			mTempC = mTempC + "," + acct.number.
	END.
	IF mTempC = "" THEN mTempC = "-".
	RUN Insert_TTName ("AccAdd",mTempC).
	kb.acca = mTempC.

	OS-DELETE VALUE(fname).
	
	OUTPUT STREAM ws TO VALUE(fname) 
		CONVERT  TARGET "1251"  SOURCE "IBM866".
		
		PUT STREAM ws UNFORMATTED
			"Internet-банкинг              "+ delim + kb.ib   + eol +
			"PC-банкинг                    "+ delim + kb.pcb  + eol +
			"Многофакторная аутентификация "+ delim + kb.auth + eol +
			"IP-фильтр                     "+ delim + kb.ipf  + eol +
			"Телефоны                      "+ delim + kb.pho  + eol +
			"Количество групп ЭЦП          "+ delim + kb.ecp  + eol +
			"Индивидуальный предприниматель"+ delim + kb.ip   + eol +
 			"Основание                     "+ delim + kb.foun + eol +
			"Дата                          "+ delim + kb.date + eol +
			"Расчетные счета               "+ delim + kb.accm + eol +
			"Текущие и транзитные счета    "+ delim + kb.acca + eol.
			
	OUTPUT STREAM ws CLOSE.
	
	IF pr  EQ TRUE THEN RUN printvd.p ("dog_kbip",INPUT TABLE ttnames).   
	IF pr1 EQ TRUE THEN RUN printvd.p ("dog_kbip1",INPUT TABLE ttnames).   
	IF pr2 EQ TRUE THEN RUN printvd.p ("dog_kbip2",INPUT TABLE ttnames).   
	IF pr3 EQ TRUE THEN RUN printvd.p ("dog_kbip3",INPUT TABLE ttnames).   
	IF pr4 EQ TRUE THEN RUN printvd.p ("dog_kbip4",INPUT TABLE ttnames).   
	IF pr5 EQ TRUE THEN RUN printvd.p ("dog_kbip5",INPUT TABLE ttnames).   
	IF pr6 EQ TRUE THEN RUN printvd.p ("dog_kbip6",INPUT TABLE ttnames).
	IF pr7 EQ TRUE THEN RUN printvd.p ("dog_kbip7",INPUT TABLE ttnames).
	
END.	