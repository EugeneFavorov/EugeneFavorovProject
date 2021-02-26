/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-1998 ТОО "Банковские информационные системы"
     Filename: crd2pos.p
      Comment: ведомость остатков по картотеке
   Parameters:
         Uses:
      Used by:
      Created: Взято и модифицированно KAU 12.2013

*/
{globals.i}
{sh-defs.i}
{kautools.lib}

DEFINE INPUT PARAMETER in-param    AS CHARACTER NO-UNDO.
DEFINE VAR in-kau-id    AS CHARACTER NO-UNDO.
DEFINE VAR in-currency  AS CHARACTER NO-UNDO.
DEFINE VAR in-bal-acct LIKE acct.acct NO-UNDO.
DEFINE VAR in-acct-cat AS CHARACTER INITIAL "o" NO-UNDO.
DEFINE VAR xamt-rub    LIKE op-entry.amt-rub FORMAT ">>,>>>,>>9.99" NO-UNDO.
DEFINE VAR amt-rub     LIKE op-entry.amt-rub FORMAT ">>,>>>,>>9.99" NO-UNDO.
DEFINE VAR kBlockAc       AS dec NO-UNDO.
DEFINE VAR BlockAc       AS char NO-UNDO.
DEFINE VAR lst-user    AS   CHARACTER NO-UNDO.
DEFINE VAR tmpacct    AS   CHARACTER NO-UNDO.
DEFINE VAR total-amt    AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99" NO-UNDO.
DEFINE VAR total-xamt   AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99" NO-UNDO.
DEFINE VAR mFlag	AS LOGICAL NO-UNDO. mFlag = NO.
DEFINE VAR mCustName    AS   CHARACTER FORMAT "x(35)" NO-UNDO.

DEFINE BUFFER xacct FOR acct.

DEFINE STREAM out-doc.
ASSIGN
   in-kau-id   = IF NUM-ENTRIES(in-param) GE 1 THEN TRIM(ENTRY(1,in-param))
                                               ELSE ""
.
PAUSE 0.
/*Вызываем ворму для определения будем печатать в Word или нет*/
FORM 
   mFlag  LABEL "Выводить пустые" VIEW-AS TOGGLE-BOX 
   WITH FRAME fIs CENTERED KEEP-TAB-ORDER OVERLAY ROW 10 SIDE-LABELS.

UPDATE  mFlag WITH FRAME fIs
EDITING:
   READKEY.

   IF KEYFUNCTION(LASTKEY) EQ "END-ERROR" THEN 
   DO:
      HIDE FRAME fIs NO-PAUSE.
      RETURN.
   END.
   ELSE
      APPLY LASTKEY.
END.
HIDE FRAME fIs.



/*MESSAGE string(in-kau-id) VIEW-AS ALERT-BOX.*/
IF in-kau-id EQ "" THEN RETURN.
in-currency = "".
pause 0.
UPDATE in-currency   LABEL "ВАЛЮТА"
       end-date      LABEL "ДАТА РАСЧЕТА"
WITH FRAME enter-cond
     WIDTH 40
     SIDE-LABELS
     CENTERED
     ROW 10
     TITLE "[ Начальные условия ]"
     OVERLAY
EDITING:
       READKEY.
       IF KEYLABEL(LASTKEY) = "F1" AND FRAME-FIELD EQ "END-DATE" THEN DO:
          RUN calend.p.
          IF (LASTKEY = 13 OR LASTKEY = 10) AND pick-value <> ? THEN do:
            FRAME-VALUE = pick-value.
            assign end-date.
            disp end-date WITH FRAME enter-cond.
          end.
       END.
       ELSE IF KEYLABEL(LASTKEY) = "F1" AND FRAME-FIELD EQ "IN-CURRENCY" THEN DO TRANS:
          RUN cur-code.p ("currency", 10).
          IF (LASTKEY = 13 OR LASTKEY = 10) AND pick-value <> ? THEN
          DISPLAY pick-value @ in-currency WITH FRAME enter-cond.
       END.
       ELSE
          APPLY LASTKEY.
END.
HIDE FRAME enter-cond.

FORM
    xacct.acct       COLUMN-LABEL "Счет"
    xamt-rub         COLUMN-LABEL "Остаток" 
    mCustName	     COLUMN-LABEL "Наименование" 
    BlockAc	     COLUMN-LABEL "Кол-во блокир" SKIP
    acct.acct        NO-LABEL
    amt-rub          NO-LABEL
    acct.details     no-label
WITH FRAME out-doc
     WIDTH 93
     DOWN.

{setdest.i &stream="STREAM out-doc" &cols=93}
/*{setdest.i &stream="STREAM out-doc"}*/


rights-pos  = getThisUserXAttrValue("ПросмотрОст")  eq "Да".
rights-user = getThisUserXAttrValue("ПросмотрСотр") eq "Да".

if rights-user then do:
   userids = getSlaves().
   if not can-do(userids, userid("bisquit")) then do:
      {additem.i userids userid(""bisquit"")}
   end.
   IF userids NE '*' THEN DO:
      lst-user = "".
      for each _user where can-do(userids, _user._userid)
                                                  no-lock:
         {additem.i lst-user _user._userid}
      end.
   END.
   ELSE lst-user = userids.
end.
ELSE lst-user = "*".
/**/
&SCOP PRINT-KAU RUN fdbacct (BUFFER acct,"Да",in-kau-id).               ~
                FIND FIRST ttKau WHERE ttKau.ftbName EQ "ACCTB"         ~
                                                             NO-ERROR.  ~
                RELEASE xacct.                                          ~
                IF avail(ttKau) THEN                                    ~
                   FIND FIRST xacct WHERE recid(xacct) = ttKau.fRecId   ~
                                                      NO-LOCK NO-ERROR. ~
                IF NOT avail(xacct) THEN                                ~
                DO:                                                     ~
			FIND FIRST signs WHERE signs.file-name = "acct" ~
                             AND signs.xattr-value = acct.acct          ~
                             AND signs.code = IF in-kau-id='карт-ка2' THEN "Карт2ВнСчет" ELSE IF in-kau-id='карт-ка1' THEN "Карт1ВнСчет" else /* in-kau-id='КартБлСч' THEN*/ "КартБВнСчет" /*"Карт2ВнСчет"*/             ~
                             NO-LOCK NO-ERROR.                          ~
	                IF AVAIL signs THEN DO:                         ~
				FIND FIRST xacct WHERE xacct.acct-cat EQ "b"            ~
                                      AND (xacct.contract EQ "Расчет" OR xacct.contract EQ "ОсРеж")       ~
                                      AND xacct.currency EQ acct.currency  ~
                                      AND xacct.cust-cat EQ acct.cust-cat  ~
                                      AND xacct.cust-id  EQ acct.cust-id   ~
                                      AND xacct.acct EQ Substring(signs.surrogate,1,20) ~
                                                         NO-LOCK NO-ERROR. ~
	                END.                                            ~
	                IF NOT avail(xacct) THEN                                ~
				FIND FIRST xacct WHERE xacct.acct-cat EQ "b"            ~
                                      AND (xacct.contract EQ "Расчет" OR xacct.contract EQ "ОсРеж")       ~
                                      AND xacct.currency EQ acct.currency  ~
                                      AND xacct.cust-cat EQ acct.cust-cat  ~
                                      AND xacct.cust-id  EQ acct.cust-id   ~
                                                         NO-LOCK NO-ERROR. ~
                END.                                                    ~
		RUN acct-pos IN h_base(acct.acct,                       ~
                                       acct.currency,                   ~
                                       end-date,                        ~
                                       end-date,                        ~
                                      ?).                               ~
                amt-rub = IF acct.currency EQ "" THEN sh-bal            ~
                                                 ELSE sh-val.           ~
                IF AVAIL xacct THEN DO:                                 ~
                   RUN acct-pos IN h_base(xacct.acct,                   ~
                                          xacct.currency,               ~
                                          end-date,                     ~
                                          end-date,                     ~
                                          ?).                           ~
                   xamt-rub = IF xacct.currency EQ "" THEN sh-bal       ~
                                                      ELSE sh-val.      ~
                END.                                                    ~
	IF AVAIL xacct AND ((amt-rub <> 0 AND xamt-rub <> 0) OR ((xacct.contract NE "Расчет" OR xacct.contract NE "ОсРеж") and mFlag and amt-rub <> 0)) THEN DO:      ~
                ASSIGN                                                  ~
                  total-amt  = total-amt  + ABS(amt-rub)                     ~
                  total-xamt = total-xamt + IF AVAIL xacct THEN xamt-rub~
                                                           ELSE 0       ~
                .                                                       ~
kBlockAc = 0.											~
	For each BlockObject where BlockObject.end-datetime = ?					~
/*					or blockObject.end-datetime < end-date*/		~
				and  BlockObject.file-name = "acct"				~
				and BlockObject.surrogate = xacct.acct + "," + xacct.currency	~
				no-lock: 							~
					kBlockAc = kBlockAc + 1.				~
				end.								~
if kBlockAc = 0 then blockAc = "НЕТ". else blockac = string(kBlockAc).				~
IF (xacct.contract NE "Расчет" OR xacct.contract NE "ОсРеж") THEN FIND FIRST cust-corp where cust-corp.cust-id = xacct.cust-id NO-LOCK NO-ERROR. ~
if AVAIL cust-corp then mCustName = cust-corp.name-short. else mCustName = "". 	~
                DISPLAY STREAM out-doc                                  ~
                  xacct.acct WHEN     AVAIL xacct  @ xacct.acct         ~
                  "-"        WHEN NOT AVAIL xacct  @ xacct.acct         ~
                  xamt-rub   WHEN     AVAIL xacct  @ xamt-rub           ~
		  mCustName  when     avail xacct  @ mCustName     ~
		  blockAc when avail xacct @ blockac   ~
                  acct.acct                        @ acct.acct          ~
                  amt-rub                          @ amt-rub            ~
                WITH FRAME out-doc.                                     ~
                DOWN STREAM out-doc 1 WITH FRAME out-doc.               ~
	END.
IF in-kau-id='карт-ка2' THEN
PUT STREAM out-doc UNFORMATTED
 "КАРТОТЕКА НА ВНЕБАЛАНСОВОМ СЧЕТЕ 90902 ЗА " end-date SKIP(2).
ELSE 
PUT STREAM out-doc UNFORMATTED
 "КАРТОТЕКА НА ВНЕБАЛАНСОВОМ СЧЕТЕ 90901 ЗА " end-date SKIP(2).
				

FOR EACH bal-acct WHERE bal-acct.kau-id EQ in-kau-id NO-LOCK,
    EACH acct OF bal-acct WHERE (acct.kau-id    EQ ""
                                 OR acct.kau-id EQ ?)
                            AND CAN-DO(lst-user,acct.user-id)
                            AND acct.open-date LE end-date
                            and (acct.close-date eq ? or acct.close-date >= end-date)
                            NO-LOCK
                              BREAK  BY bal-acct.bal-acct
                                     BY acct.currency
               BY substring(trim(acct.acct),
                            length(trim(acct.acct)) - 7)
                                     :
   IF NOT CAN-DO(in-currency,acct.currency) THEN NEXT.
   {&PRINT-KAU}
END.
FOR EACH acct WHERE acct.kau-id    EQ in-kau-id
                AND acct.acct-cat  EQ in-acct-cat
                AND CAN-DO(lst-user,acct.user-id)
                AND acct.open-date LE end-date NO-LOCK
                  BREAK BY acct.currency
                        BY substring(trim(acct.acct),length(trim(acct.acct)) - 7)

                  :
   IF NOT CAN-DO(in-currency,acct.currency) THEN NEXT.
   {&PRINT-KAU}
END.
IF total-amt NE 0 OR total-xamt NE 0 THEN DO:
   DISPLAY STREAM out-doc
      "ИТОГО ПО БАЛАНСУ"                           @ xacct.acct
                  total-xamt                       @ xamt-rub
      "ИТОГО ПО ВНЕБАЛАНСУ"                        @ acct.acct
       total-amt                                   @ amt-rub
                WITH FRAME out-doc.
                DOWN STREAM out-doc 2 WITH FRAME out-doc.

END.
{signatur.i &department = branch &stream="STREAM out-doc"
            &user-only  ="YES"
}
{preview.i  &stream="stream out-doc"}
