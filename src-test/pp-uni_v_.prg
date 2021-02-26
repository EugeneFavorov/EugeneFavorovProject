/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-1998 ТОО "Банковские информационные системы"
     Filename: pp-new1.prg
      Comment: Библиотека процедур используемых при печати платежного поручения
   Parameters:
         Uses: banknm.lf bankct.lf x-amtstr.p wordwrap.i
      Used by: pp-new1.p
      Created: 20.12.1999 Kostik
     Modified: 09.02.2000 Kostik Добавлена новая ветка
                                                       (счет плательщика = счету дебета
                                                        счет получателя  = счету кредита)
     Modified: 28.07.2003 Guva заявка 15378 добавлено поле дата перечисления платежа
     Modified: 25/07/2003 kraw (0018163) Если нет счета в банке, то брать из реквизита получателя
     Modified: 13/08/2003 Gorm (0018681) Спецпатч С09. Перенесена функциональность из стандартной версии
                                         (з. 18163), оставлена спец. функциональность - з. 15378.
     Modified: 16.09.2003 12:15 DEMA     (0019306) Поднято в DPL-Intesa из 41C-09
                                         для внесения изменений.
     Modified: 16.09.2003 13:50 DEMA     (0019306) Изменен алгоритм печати поля
                                         "Списано со счета плательщика": если
                                         задан допреквизит user-proc.СписСчета,
                                         поле печатается в любот случае, если
                                         пустое - подставляется op.op-date.
     Modified: 15/06/2004 kraw (0021242) Рез. поле для банков-клиентов
     Modified: 09/07/2007 kraw (0078707) "Срочно" в "вид платежа"
     Modified: 05/10/2007 kraw (0081500) другая печать плательщика, если перечисляют из филиала с БИК в филиал без БИК (наш)
     Modified: 25/01/2008 kraw (0082055) PayType = "   электронно   " по умолчанию.
     Modified: 25/01/2008 kraw (0092259) PayType = "   электронно   " по умолчанию только для т. н. "беспроводочных"
     Modified: 26/03/2009 kraw (0091707) печать транзитных платежей.
     Modified: 17/10/2010 kraa (0133901) печати инкассового поручения.
     Modified: 25/07/2011 stred (0151939) печатm подписей документа сертификатами.
     Modified: 25/07/2011 ccc   (0184102) печать назначения из alt-details
     Modified: 27/05/2014 kraw (0227236) УИН может быть больше 20 символов
*/
Form "~n@(#) pp-new1.prg 1.0 Kostik 20.12.1999 Kostik 20.12.1999 Библиотека процедур для печати платежных поручений" with frame sccs-id width 250.

{op-ident.i}
{exchange.equ}
{intrface.get xclass}

{opcertif.pro} /* Подписи документа сертификатами */

/*    форма для печати в test.pp*/
DEFINE FRAME testprn
      Info-Store.info-id       FORMAT "x(14)"
      Info-Store.inn           FORMAT "x(14)"
      Info-Store.flag          FORMAT "x(14)"
      Info-Store.code          FORMAT "x(14)"
      Info-Store.acct          FORMAT "x(30)"
      Info-Store.corr-acct     FORMAT "x(30)"
      Info-Store.acct-type     FORMAT "x(10)"
      Info-Store.acct-cat      FORMAT "x(1)"
      Info-Store.bank-id       FORMAT ">>>>>>>>>9"
      Info-Store.name          FORMAT "x(40)"
      Info-Store.Sh-name       FORMAT "x(40)"
      Info-Store.address       FORMAT "x(60)"
      Info-Store.corr-acct1    FORMAT "x(60)"
      Info-Store.flag-rkc      FORMAT "x(60)"
      Info-Store.flag-client   FORMAT "x(60)"
      Info-Store.flag-balchinn FORMAT "x(60)"
      Info-Store.category      FORMAT "x(60)"
   WITH WIDTH 100.
/********       Добавления для инктрукции 1256 У        ***********************/

/* Использовать ли op.op-date в поле "Списано со счета" расчетных документов */
FUNCTION UseOpDate LOGICAL:
   DEFINE VARIABLE vPrName AS CHARACTER NO-UNDO.

   vPrName = program-name(1).
   vPrName = ENTRY(NUM-ENTRIES(vPrName," "),vPrName," ").
   vPrName = ENTRY(NUM-ENTRIES(vPrName,"/"),vPrName,"/").
   vPrName = ENTRY(NUM-ENTRIES(vPrName,"~\"),vPrName,"~\").
   vPrName = ENTRY(1,vPrName,".").

   FIND FIRST user-proc
      WHERE user-proc.procedure EQ vPrName
        AND user-proc.partition EQ "ПЕЧДОКУМ"
      NO-LOCK NO-ERROR.

   IF NOT AVAIL(user-proc)
      THEN RETURN NO.
      ELSE RETURN GetXAttrValue("user-proc", STRING(user-proc.public-number), "СписСчета") = "Да".
END FUNCTION.

PROCEDURE GetDopParam:

   DEF VAR vTime       AS CHAR NO-UNDO.
   DEF VAR vBatch      AS CHAR NO-UNDO.
   DEF VAR vNumRkc     AS CHAR NO-UNDO.
   DEF VAR vElDocDate  AS CHAR NO-UNDO.
   DEF VAR vSendId     AS CHAR NO-UNDO.

   
   /*Инициализация дополнительных параметров добавленных в соответствии с требованиями 1256-У*/
   ASSIGN
      mKBK    = GetXAttrValueEx("op",STRING(op.op),"КБК"," ")
      mOKATO  = GetXAttrValueEx("op",STRING(op.op),"ОКАТО-НАЛОГ","")
      mPokOp  = GetXAttrValueEx("op",STRING(op.op),"ПокОП","")
      mPokNP  = GetXAttrValueEx("op",STRING(op.op),"ПокНП","")
      mPokND  = GetXAttrValueEx("op",STRING(op.op),"ПокНД","")
      mPokDD  = GetXAttrValueEx("op",STRING(op.op),"ПокДД","")
      mPokTP  = GetXAttrValueEx("op",STRING(op.op),"ПокТП","")
      mPokST  = GetXAttrValueEx("op",STRING(op.op),"ПокСТ","")
      mSpisPl = GetXAttrValueEx("op",STRING(op.op),"СписСчета","")
   .

   mUIN1 = TRIM(GetXAttrValueEx("op",STRING(op.op),"УИН","")).

   IF LENGTH(mUIN1) GT 21 THEN
   DO:
      mUIN3 = SUBSTRING(mUIN1, 19).
      mUIN2 = SUBSTRING(mUIN1, 10, 9).
      mUIN1 = SUBSTRING(mUIN1, 1,  9).
   END.
   ELSE
   DO:
      mUIN3 = SUBSTRING(mUIN1, 15).
      mUIN2 = SUBSTRING(mUIN1, 8, 7).
      mUIN1 = SUBSTRING(mUIN1, 1, 7).
   END.

   IF mUIN1 EQ "" THEN
      IF mPokST NE "" AND CAN-DO(FGetSetting("ГНИ","bal-smev",""),op.ben-acct) THEN
         mUIN2 = "0". 
   IF mUIN1 EQ "0" THEN 
      ASSIGN
         mUIN1 = ""
         mUIN2 = "0" 
      .     

   mSpisPl = STRING(DATE(mSpisPl),"99.99.9999") NO-ERROR.
   IF ERROR-STATUS:ERROR THEN mSpisPl = ?.

   IF mSpisPl = ? AND UseOpDate() THEN
      mSpisPl = STRING(op.op-date, "99.99.9999").

   IF mSpisPl = ? THEN mSpisPl = "".

   &IF DEFINED(uni1) NE 0 &THEN
   IF op.user-id = "SERV" THEN
   DO:
      FIND FIRST acct WHERE acct.acct = &IF defined(tt-op-entry) = 0 &THEN op-entry.acct-cr 
                                                                     &ELSE tt-op-entry.acct-cr 
                                        &ENDIF
      NO-LOCK NO-ERROR.
      FIND FIRST _user WHERE _user._userid = acct.user-id NO-LOCK.
   END.
   ELSE
      FIND FIRST _user WHERE _user._userid = op.user-id NO-LOCK.

   FIND FIRST op-impexp OF op NO-LOCK NO-ERROR.
   IF AVAIL op-impexp THEN 
      ASSIGN
         vTime  =  IF op-impexp.imp-date <> ? THEN
                      STRING(op-impexp.imp-time, "hh:mm")
                   ELSE "?"
         vBatch =  STRING(op-impexp.imp-batch).
   ELSE DO:
      RUN GetFieldFromPacket
                       (INPUT        YES,  
                        INPUT        "RKCReturn",
                        INPUT        "ExchRKCDoc",
                        INPUT         {&STATE-FIN}, 
                        INPUT        "REPDReturn",
                        INPUT-OUTPUT vNumRkc,    
                        INPUT-OUTPUT vElDocDate,
                        INPUT-OUTPUT vSendId,
                        INPUT-OUTPUT vTime, 
                        INPUT-OUTPUT vBatch).
   END.
   ASSIGN
      theHeader = FGetSetting("Банк",?,"") + " " +
                  "ВРЕМЯ ВВОДА " + vTime + " " +
                  "НОМЕР РЕЙСА " + vBatch
      theBank = FGetSetting("БанкСПлат",?,FGetSetting("БанкС",?,""))
      theBank = FILL(" ", INT64((22 - LENGTH(theBank)) / 2)) + theBank
      theCity = FGetSetting("БанкГород",?,"")
      theCity = FILL(" ", INT64((22 - LENGTH(theCity)) / 2)) + theCity
      .
   &ENDIF
   /*Инициализация дополнительных параметров добавленных в соответствии с требованиями ВТБ*/
   &IF DEFINED(uni2) NE 0 &THEN
   IF op.user-id = "SERV" THEN
   DO:
      FIND FIRST acct WHERE acct.acct = &IF defined(tt-op-entry) = 0 &THEN op-entry.acct-cr 
                                                                     &ELSE tt-op-entry.acct-cr 
                                        &ENDIF
      NO-LOCK NO-ERROR.
      FIND FIRST _user WHERE _user._userid = acct.user-id NO-LOCK.
   END.
   ELSE
      FIND FIRST _user WHERE _user._userid = op.user-id NO-LOCK.
   ASSIGN
      theUserName = FILL(" ", INT64((22 - LENGTH(_user._user-name)) / 2)) +
                   _user._user-name
      theBank = FGetSetting("БанкСПлат",?,FGetSetting("БанкС",?,""))
      theBank = FILL(" ", INT64((22 - LENGTH(theBank)) / 2)) + theBank
      theCity = FGetSetting("БанкГород",?,"")
      theCity = FILL(" ", INT64((22 - LENGTH(theCity)) / 2)) + theCity.
   &ENDIF
   
     
/*-------------------------------------- Штамп, для СПБИ ---------------------------------------*/
   &IF DEFINED(uni3) NE 0 &THEN  
   
   ASSIGN
      BankName[1] = FGetSetting("Банк",   ?,FGetSetting("БанкС",?,""))
      BankMFO[1]  = FGetSetting("БанкМФО",?,FGetSetting("БанкС",?,""))
      BankAcct[1] = FGetSetting("КорСч",  ?,FGetSetting("БанкС",?,""))
      mIsUni3     = YES
   .
    
   IF op.user-inspector NE "" THEN
   DO:
      FIND FIRST _user WHERE _user._userid = op.user-inspector NO-LOCK.
   END.

   IF AVAILABLE _user THEN
      Inspector[1] = _user._user-name.
   ELSE
      Inspector[1] = "".
   
   {getbank.i bank3 BankMFO[1]}

   IF AVAILABLE bank3 THEN
      FIND banks-corr2 WHERE banks-corr2.bank-corr EQ bank3.bank-id
                         AND banks-corr2.corr-acct EQ BankAcct[1]
         NO-LOCK NO-ERROR.

   IF AVAILABLE banks-corr2 THEN
      FIND bank2 WHERE bank2.bank-id EQ banks-corr2.bank-id
         NO-LOCK NO-ERROR.

   IF AVAILABLE bank2 THEN
      RKCName = bank2.name.
   ELSE
      RKCName = "ГРКЦ ГУ ЦБ РФ".

   BankAcct[1] = "К/С " + BankAcct[1].
   BankMFO[1]  = "БИК " + BankMFO[1] + " В " + RKCName.

   {wordwrap.i &s=BankName  &n=3 &l=30 &CENTERED=YES}
   {wordwrap.i &s=BankMFO   &n=2 &l=30 &CENTERED=YES}
   {wordwrap.i &s=Inspector &n=1 &l=30 &CENTERED=YES}
   {wordwrap.i &s=BankAcct  &n=1 &l=30 &CENTERED=YES}

   &ENDIF
/*----------------------------------------------------------------------------------------------*/   
   run GetSignCertif("op",STRING(op.op)). /* Подписи документа сертификатами */
END PROCEDURE.

FUNCTION ConvertNameINN RETURNS CHAR(INPUT iINN     AS CHARACTER,
                                     INPUT iRecSend AS LOGICAL):
   DEFINE VARIABLE vKPP AS CHARACTER INIT "" NO-UNDO. /*?*/
   &IF DEFINED(NEW_1256) EQ 0 &THEN
      ASSIGN
         vKPP = GetXAttrValueEx("op",STRING(op.op),"Kpp-send","") WHEN NOT iRecSend
         vKPP = GetXAttrValueEx("op",STRING(op.op),"Kpp-rec","")  WHEN iRecSend
      .
      IF iINN EQ ? OR iINN EQ "" THEN
      RETURN ("").
      ELSE
      RETURN ("ИНН " +
              iINN   + " " +
              (IF vKPP NE "" THEN ("КПП " + vKPP + " ") ELSE "")
             ).
   &ELSE
      ASSIGN
         PlINN = iINN WHEN NOT iRecSend
         PoINN = iINN WHEN iRecSend
         PlKPP = GetXAttrValueEx("op",STRING(op.op),"Kpp-send","")
         PoKPP = GetXAttrValueEx("op",STRING(op.op),"Kpp-rec","")

      .
      RETURN "".
   &ENDIF
END.
/******************************************************************************/
                  /* наличие описания процедуры */

FUNCTION AvailProc RETURN LOGICAL (INPUT inProc AS CHAR):
  RETURN LOOKUP(inProc, THIS-PROCEDURE:INTERNAL-ENTRIES) <> 0.
END.

/* выбор функции пользователя или по умолчанию */

FUNCTION GetProcName RETURN CHAR (INPUT inProc AS CHAR):
  RETURN (IF AvailProc("Get" + inProc) THEN "Get" ELSE (IF AvailProc("Def" + inProc) THEN "Def" ELSE "")) + inProc.
END.

FUNCTION ProcAvail RETURN LOGICAL (INPUT inProc AS CHAR, INPUT inRoot AS CHAR, OUTPUT NameProc AS CHAR):
  NameProc = "".
  IF AvailProc("Get" + inRoot + inProc) THEN  NameProc = "Get" + inRoot + inProc.
  ELSE IF AvailProc("Get" + inProc) THEN NameProc = "Get" + inProc.
  ELSE IF AvailProc("Def" + inRoot + inProc) THEN NameProc = "Def" + inRoot + inProc.
  ELSE IF AvailProc("Def" + inProc) THEN NameProc = "Def" + inProc.
  RETURN NameProc <> "".
END.

/* выполнить стандартную или пользовательскую функцию */

PROCEDURE RunAvailProc:
  DEF INPUT PARAM inProc AS CHAR NO-UNDO.
  DEF VAR Proc2Run AS CHAR NO-UNDO.
/* --- */
  Proc2Run = GetProcName(inProc).
  IF Proc2Run <> inProc THEN RUN VALUE(Proc2Run).
END PROCEDURE.

/* стандартный заголовок, дата, тип платежа */

PROCEDURE DefHeader:
  &GLOB perm-uer "1,3,4"
  DEF VAR is-electro AS LOGICAL NO-UNDO.
  DEF VAR mfo-code     AS CHARACTER NO-UNDO.
  DEF VAR set-electro  AS CHARACTER NO-UNDO.
  DEF VAR set-mail     AS CHARACTER NO-UNDO.
  DEF VAR set-telegraf AS CHARACTER NO-UNDO.
  DEF VAR set-cito     AS CHARACTER NO-UNDO.

  DEFINE BUFFER cacct FOR acct.

  set-electro  = FGetSetting("ТехПлат","ТПЭлектронный",?).
  set-mail     = FGetSetting("ТехПлат","ТППочта",?).
  set-telegraf = FGetSetting("ТехПлат","ТПТелеграф",?).
  set-cito     = FGetSetting("ТехПлат","ТПСрочный",?).

  ASSIGN
    NameOrder[1]  = "ПЛАТЕЖНОЕ ПОРУЧЕНИЕ В N"
	NameOrder[2]  = "ИНОСТРАННОЙ ВАЛЮТЕ"
    NumberForm = "0401060".
  theDate = IF op.doc-date <> ? THEN STRING(op.doc-date, "99.99.9999")
                                ELSE STRING(op.op-date, "99.99.9999").
  &IF DEFINED(MORDER ) NE 0 &THEN
  
  DEFINE VARIABLE vDateMoBo AS LOG NO-UNDO.
  vDateMoBo = FGetSetting("ПлатДок","ДатаМОБО", "") EQ "op-date".

  IF    op.op-status EQ "А"
     OR CAN-DO(op.op-status, FGetSetting("СтандТр","КартСтат", "А"))
     OR op.op-date = ?
  THEN DO TRANSACTION:
     FOR EACH bo-op
        WHERE bo-op.op-transaction EQ op.op-transaction
          AND bo-op.acct-cat       EQ "o"
     NO-LOCK:
        IF GetXattrValue("op",STRING(bo-op.op),"op-bal") EQ STRING(op.op) THEN
        LEAVE.
     END.
     IF NOT AVAIL bo-op THEN
     FIND FIRST bo-op
          WHERE bo-op.op-transaction EQ op.op-transaction
            AND bo-op.acct-cat       EQ "o"
     NO-LOCK NO-ERROR.
  
     IF NOT AVAIL bo-op THEN DO:
        MESSAGE "Не найден связанный внебалансовый документ"
        VIEW-AS ALERT-BOX ERROR.
        RETURN.
     END.
     theDate = IF vDateMoBo THEN STRING(bo-op.op-date, "99.99.9999")  
                            ELSE theDate.
  END.
  ELSE
     theDate = IF vDateMoBo THEN STRING(op.op-date, "99.99.9999")  
                            ELSE theDate.
  &ENDIF
/*  is-electro = NO.
  FIND Info-Store WHERE Info-Store.Info-ID EQ "БАНКПОЛ" NO-LOCK NO-ERROR.
  IF AVAIL Info-Store AND Info-Store.code NE ? AND Info-Store.code NE "" THEN DO:
     IF Info-Store.bank-id NE ? THEN
        FIND banks WHERE banks.bank-id EQ Info-Store.bank-id NO-LOCK NO-ERROR.
     ELSE DO:
        FIND banks-code WHERE banks-code.bank-code-type EQ "МФО-9"
                          AND banks-code.bank-code      EQ Info-Store.code
                                                        NO-LOCK NO-ERROR.
        FIND banks OF banks-code NO-LOCK NO-ERROR.
     END.

     FIND FIRST signs WHERE signs.file = "banks"
                        AND signs.surr = string(banks.bank-id)
                        AND signs.code = "uer" NO-LOCK NO-ERROR.
     IF AVAIL signs AND (LOOKUP(signs.xattr,FGetSetting("МЦИ","perm-uer",{&perm-uer})) <> 0) THEN DO:
        IF CAN-DO(FGetSetting("МЦИ","reg-mask","NotAvail"), Info-Store.code) 
        THEN
           is-electro = yes.
        ELSE DO:
           IF NOT CAN-DO(FGetSetting("МЦИ","dt-always","NotAvail"), op.doc-type) THEN
           is-electro=yes.
        END.
     END.
  END.*/
/*  IF NOT is-electro THEN DO:*/
     IF CAN-DO(set-mail,&IF defined(tt-op-entry) <> 0 &THEN tt-op-entry.type &ELSE op-entry.type &ENDIF) THEN PayType = "     почтой     ".
     ELSE IF CAN-DO(set-telegraf,&IF defined(tt-op-entry) <> 0 &THEN tt-op-entry.type &ELSE op-entry.type &ENDIF) THEN PayType = "   телеграфом   ".
     ELSE IF CAN-DO(set-electro,&IF defined(tt-op-entry) <> 0 &THEN tt-op-entry.type &ELSE op-entry.type &ENDIF)  THEN PayType = "   электронно   ".
     ELSE IF CAN-DO(set-cito,&IF defined(tt-op-entry) <> 0 &THEN tt-op-entry.type &ELSE op-entry.type &ENDIF)  THEN PayType = "     срочно     ".
     ELSE PayType = "".

&IF DEFINED(FRM_PRN) NE 0 &THEN

     IF TRIM("{&FRM_PRN}")  EQ "pp-uni_1.frm" AND PayType EQ "" THEN
     DO:

        IF NOT AVAILABLE op-entry 
           OR op-entry.op-entry EQ -1 THEN
           PayType = "   электронно   ".
     END.
&ENDIF
/*  END.
  ELSE
     PayType = "   электронно   ".*/
  IF FGetSetting("ВидПлатОбяз",?,"НЕТ") EQ "НЕТ" THEN DO:
     IF  CAN-DO(FGetSetting("МЦИ","reg-mask","NotAvail"),PoMFO)
     AND CAN-DO(FGetSetting("МЦИ","reg-mask","NotAvail"),PlMFO)
     AND TRIM(PayType) EQ "электронно" THEN
        PayType = "".
  END.
  mPPDate = "".
  
  FIND FIRST cacct WHERE cacct.acct     EQ op-entry.acct-cr 
                     AND cacct.currency EQ op-entry.currency
     NO-LOCK NO-ERROR.

  IF AVAILABLE cacct THEN

     IF cacct.cust-cat EQ "Б" THEN

        FOR FIRST banks WHERE banks.bank-id EQ cacct.cust-id
           AND NOT banks.flag-rkc NO-LOCK:

           IF CAN-DO(FGetSetting("НазнСчМБР", "", "*empty*"), cacct.contract) AND
              op-entry.acct-db NE FGetSetting("КорСч","","") THEN
              mPPDate = STRING(op.op-value-date, "99.99.9999").
        END.
   mPPDate = "".
END PROCEDURE.

/* стандартный расчет суммы */

PROCEDURE DefAmt:
  DEFINE VAR in-amt   AS DECIMAL   			NO-UNDO.
  DEFINE VAR in-amt-c AS DECIMAL   			NO-UNDO.
  DEFINE VAR in-curr  AS CHARACTER 			NO-UNDO.
  DEFINE VAR vAmtStr  AS CHARACTER EXTENT 2 NO-UNDO.
  
 
  ASSIGN
	&IF defined(tt-op-entry) = 0 &THEN
		in-amt-c = op-entry.amt-cur
		in-amt   = op-entry.amt-rub
		in-curr  = op-entry.currency
	&ELSE
 		in-amt-c = tt-op-entry.amt-cur
		in-amt   = tt-op-entry.amt-rub
		in-curr  = tt-op-entry.currency
	&ENDIF
	.

  RUN x-amtstr.p(in-amt-c,in-curr, TRUE, TRUE, OUTPUT vAmtStr[1], OUTPUT vAmtStr[2]).
  ASSIGN
      Val       = STRING(STRING(in-amt-c * 100, "-zzzzzzzzzz999"), "x(12)-x(2)")
      AmtStr[1] = vAmtStr[1] + ' ' + vAmtStr[2]
    .
  AmtStr[1] = AmtStr[1] + " / ".
  RUN x-amtstr.p(in-amt,'', TRUE, TRUE, OUTPUT vAmtStr[1], OUTPUT vAmtStr[2]).
  ASSIGN
      Rub       = STRING(STRING(in-amt * 100, "-zzzzzzzzzz999"), "x(12)-x(2)")
      AmtStr[1] = AmtStr[1] + vAmtStr[1] + ' ' + vAmtStr[2]
    .

END PROCEDURE.

/* назначения платежа */

PROCEDURE DefDetail:
   DEF VAR vAltDet  AS CHARACTER. 
   DEF VAR vAltTyp  AS CHARACTER. 
   DEF VAR vTypTMP1 AS CHARACTER. 
   DEF VAR vTypTMP2 AS CHARACTER. 
   vAltDet = GetXAttrValueEx("op",STRING(op.op),"alt-details","").
   vAltTyp = GetXAttrValueEx("op",STRING(op.op),"alt-type","").
   FIND FIRST doc-type WHERE doc-type.doc-type EQ op.doc-type NO-LOCK NO-ERROR.
   IF AVAIL(doc-type) THEN vTypTMP1 = doc-type.digital.
   IF vAltDet NE "" AND vTypTMP1 eq "09" THEN 
   Do:
      FIND FIRST doc-type WHERE doc-type.doc-type EQ vAltTyp NO-LOCK NO-ERROR.
      IF AVAIL(doc-type) THEN vTypTMP2 = doc-type.digital.
      If vTypTMP2 EQ "09"
      Then Detail[1] = IF vAltDet    <> ? THEN vAltDet    ELSE "".
      Else Detail[1] = IF op.details <> ? THEN op.details ELSE "".
   End.
   ELSE Detail[1] = IF op.details <> ? THEN op.details ELSE "".
END PROCEDURE.

/* реквизиты плательщика */

PROCEDURE DefPayer:
   RUN for-pay("ДЕБЕТ,ПЛАТЕЛЬЩИК,БАНКПЛ,БАНКГО,БАНКФИЛ",
               "ПП",
               OUTPUT PlName[1],
               OUTPUT PlLAcct,
               OUTPUT PlRKC[1],
               OUTPUT PlCAcct,
               OUTPUT PlMFO).
               
                
END PROCEDURE.

/* реквизиты получателя */

PROCEDURE DefRecipient:
   RUN for-rec("КРЕДИТ,ПОЛУЧАТЕЛЬ,БАНКПОЛ,БАНКГО,БАНКФИЛ",
               "ПП",
               OUTPUT PoName[1],
               OUTPUT PoAcct,
               OUTPUT PoRKC[1],
               OUTPUT PoCAcct,
               OUTPUT PoMFO).         
END PROCEDURE.

/* всякие дествия после вычислений */

PROCEDURE DefWrap:
  {wordwrap.i &s=Detail &n=5 &l=80}
  {wordwrap.i &s=AmtStr &n=3 &l=71}
  {wordwrap.i &s=PlRKC  &n=2 &l=46}
  {wordwrap.i &s=PlName &n=5 &l=46}
  {wordwrap.i &s=PoRKC  &n=2 &l=46}
  {wordwrap.i &s=PoName &n=5 &l=46}
END PROCEDURE.

/* счета по умолчанию из проводки */

PROCEDURE DefAcct:
  DEF INPUT PARAM isPay AS LOGICAL NO-UNDO.
  DEF INPUT-OUTPUT PARAM inAcct LIKE acct.acct NO-UNDO.
/* --- */
  &IF defined(tt-op-entry) = 0 &THEN
    FIND FIRST acct WHERE acct.acct = (IF isPay THEN op-entry.acct-db ELSE op-entry.acct-cr) AND acct.currency = "" NO-LOCK NO-ERROR.
  &ELSE
    FIND FIRST acct WHERE acct.acct = (IF isPay THEN tt-op-entry.acct-db ELSE tt-op-entry.acct-cr) AND acct.currency = "" NO-LOCK NO-ERROR.
  &ENDIF
  inAcct = IF AVAIL acct THEN acct.acct ELSE "".
END PROCEDURE. /* ? */

/* МФО по умолчанию */

PROCEDURE DefMFO:
  DEF INPUT PARAM isPay AS LOGICAL NO-UNDO.
  DEF INPUT-OUTPUT PARAM inMFO AS CHAR NO-UNDO.
/* --- */
  inMFO   = bank-mfo-9.
END PROCEDURE.

/* РКЦ по умолчанию */
PROCEDURE DefRKC:
  DEF INPUT PARAM isPay AS LOGICAL NO-UNDO.
  DEF INPUT-OUTPUT PARAM inRKC AS CHAR NO-UNDO.
/* --- */
  {getbank.i bank1 bank-mfo-9}
  inRKC = IF AVAIL bank1 THEN BankNameCity(BUFFER Bank1) ELSE "".
END PROCEDURE.

/* к/с по умолчанию */

PROCEDURE DefCAcct:
  DEF INPUT PARAM isPay AS LOGICAL NO-UNDO.
  DEF INPUT-OUTPUT PARAM inCAcct AS CHAR NO-UNDO.
/* --- */
  inCAcct = bank-acct.
END PROCEDURE.

/* ИНН по умолчанию */

PROCEDURE DefINN:
  DEF INPUT PARAM isPay AS LOGICAL NO-UNDO.
  DEF INPUT-OUTPUT PARAM inINN AS CHAR NO-UNDO.
/* --- */
  IF AVAIL acct AND (acct.cust-cat = 'В' OR LOOKUP(STRING(acct.bal-acct), mSetBalSCHinn) <> 0 ) THEN DO:
    inINN = FGetSetting("ИНН",?,"").
  END.
  ELSE inINN = "".
END PROCEDURE.

/* наименование по умолчанию */

PROCEDURE DefName:
END PROCEDURE.

/* Сбор информации во временную таблицу. Если указан параметр &TEST, то
   собранная информация выводится в файл test.pp */
PROCEDURE Collection-Info:
   RUN identify-Client("SEND").
   RUN identify-Client("REC").
&IF defined(tt-op-entry) = 0 &THEN
   RUN Identify-Acct("ДЕБЕТ",op-entry.acct-db,op-entry.currency).
   RUN Identify-Acct("КРЕДИТ",op-entry.acct-cr,op-entry.currency).
&ELSE
   RUN Identify-Acct("ДЕБЕТ",tt-op-entry.acct-db,tt-op-entry.currency).
   RUN Identify-Acct("КРЕДИТ",tt-op-entry.acct-cr,tt-op-entry.currency).
&ENDIF
   RUN Identify-Banks("SEND").
   RUN Identify-Banks("REC").
   RUN Our-Bank.
   RUN General-Bank.

   &IF "{&TEST}" EQ "YES" &THEN
      FOR EACH Info-Store :
         DISP STREAM test
            Info-Store.info-id
            Info-Store.inn
            Info-Store.flag
            Info-Store.code
            Info-Store.acct
            Info-Store.corr-acct
            Info-Store.acct-type
            Info-Store.acct-cat
            Info-Store.bank-id
            Info-Store.name
            Info-Store.Sh-name
            Info-Store.address
            Info-Store.corr-acct1
            Info-Store.flag-rkc
            Info-Store.flag-client
            Info-Store.flag-balchinn
            Info-Store.category
            WITH FRAME testprn WITH 1 COL.
         DOWN STREAM test
         WITH FRAME testprn.
      END.
   &ENDIF
END.

PROCEDURE for-pay:
   DEFINE INPUT PARAMETER  in-param   AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER  in-doc-type AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER out-Name   AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER out-Acct   AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER out-RKC    AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER out-CAcct  AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER out-MFO    AS CHARACTER NO-UNDO.

   DEFINE VARIABLE vPNINN AS CHARACTER   NO-UNDO.

   DEF BUFFER Inf-acct    FOR Info-Store.
   DEF BUFFER Inf-acct-cr FOR Info-Store.
   DEF BUFFER Inf-cl      FOR Info-Store.
   DEF BUFFER Inf-bank    FOR Info-Store.
   DEF BUFFER Inf-fil     FOR Info-Store.

   vPNINN = GetXAttrValueEx("op",STRING(op.op),"ПНИНН","").

   IF in-doc-type EQ "ПП" THEN
   FIND inf-acct-cr WHERE inf-acct-cr.info-id   EQ "КРЕДИТ"      NO-LOCK NO-ERROR.
   FIND inf-acct WHERE inf-acct.info-id   EQ ENTRY(1,in-param) NO-LOCK NO-ERROR.
   IF in-doc-type NE "ВО" THEN
   FIND inf-cl   WHERE inf-cl.info-id     EQ ENTRY(2,in-param) NO-LOCK NO-ERROR.
   IF in-doc-type NE "ВО" THEN
   FIND inf-bank WHERE inf-bank.info-id   EQ ENTRY(3,in-param)     NO-LOCK NO-ERROR.
   FIND Info-Store WHERE info-store.info-id   EQ ENTRY(4,in-param) NO-LOCK NO-ERROR.
   FIND Inf-fil WHERE inf-fil.info-id   EQ ENTRY(5,in-param) NO-LOCK NO-ERROR.

   RELEASE banks.
   IF AVAIL inf-acct AND NOT AVAIL inf-cl AND NOT AVAIL inf-bank THEN DO:
      ASSIGN
         out-RKC  = Info-Store.name
         out-MFO    = Info-Store.code
         out-CAcct  = Info-Store.acct
      .
      out-Acct  = Inf-acct.acct.
      IF Inf-acct.acct-cat EQ "В" OR Inf-acct.flag-balchinn EQ "БалСчИНН" THEN DO:  /* 1.1.1 */
         IF NOT AVAIL inf-fil THEN DO:
            IF in-doc-type EQ "ПП" THEN
            DO:
               IF vPNINN NE "" THEN 
                  Info-Store.inn = vPNINN.
                  out-Name =  ConvertNameINN(Info-Store.inn,NO) +
                     Info-Store.Sh-Name.
            END.
            ELSE
            out-Name = ConvertNameINN(Inf-acct.inn,NO) +
                       Inf-acct.Name .
            IF  AVAIL inf-acct-cr AND inf-acct-cr.flag-client EQ "БанкКлиент" THEN DO:
               IF inf-acct-cr.category EQ "t" THEN
                  out-Name =  ConvertNameINN(Info-Store.inn,NO) +
                              Info-Store.Sh-Name + " ДОВЕРИТЕЛЬНЫЙ УПРАВЛЯЮЩИЙ".
               ELSE
                  out-Name =  ConvertNameINN(Info-Store.inn,NO) +
                              Info-Store.Sh-Name.
               ASSIGN
                  out-RKC  = Inf-acct-cr.Sh-name
                  out-MFO    = Inf-acct-cr.code
                  out-CAcct  = Inf-acct-cr.corr-acct
                  out-Acct  = Inf-acct-cr.corr-acct1
               .
            END.
         END.
         ELSE                                                                       /* 1.1.2 */
            IF in-doc-type EQ "ПП" THEN
            out-Name =  ConvertNameINN(Inf-Fil.inn,NO) +
                        Inf-Fil.Sh-Name.
            ELSE
            out-Name = ConvertNameINN(Inf-acct.inn,NO) +
                       Inf-acct.Name .
      END.
      ELSE DO:
         out-Name =  ConvertNameINN(Inf-acct.inn,NO) +
                     Inf-acct.Name.
         IF NOT AVAIL Inf-Fil THEN DO:
            IF AVAIL inf-acct-cr AND inf-acct-cr.flag-client EQ "БанкКлиент" THEN DO:
               out-Name =  ConvertNameINN(Inf-acct.inn,NO) +
                           Inf-acct.Name + " р/c " + SUBSTRING(inf-acct.acct, 1, 25) + " в " +
                           Info-Store.Name.
               ASSIGN
                  out-RKC    = Inf-acct-cr.Sh-name
                  out-MFO    = Inf-acct-cr.code
                  out-CAcct  = Inf-acct-cr.corr-acct
                  out-Acct  = Inf-acct-cr.corr-acct1
               .
            END.
         END.
         ELSE
            out-Name = out-Name + " в " + inf-fil.name.
      END.
   END.
   ELSE IF AVAIL inf-acct AND AVAIL inf-cl AND NOT AVAIL inf-bank THEN DO:      /* 1.2 */
      ASSIGN
         out-RKC  = Info-Store.name
         out-MFO    = Info-Store.code
         out-CAcct  = Info-Store.acct
      .
      IF inf-cl.acct EQ inf-acct.acct THEN DO:                                  /* 1.2.1 */
         ASSIGN
            out-Name   = ConvertNameINN(inf-cl.inn,NO) +
                         inf-cl.name
            out-Acct  = inf-acct.acct
            out-RKC  = Info-Store.name
            out-MFO    = Info-Store.code
            out-CAcct  = Info-Store.acct
         .
         IF AVAIL inf-acct-cr AND inf-acct-cr.flag-client EQ "БанкКлиент" THEN DO:
               out-Name =  ConvertNameINN(inf-cl.inn,NO) +
                           inf-cl.name                +
                           (IF inf-cl.acct NE "" THEN (" р/c " + SUBSTRING(inf-cl.acct,1, 25))
                                                 ELSE "") + " в " +
                           Info-Store.Name.
               ASSIGN
                  out-RKC    = Inf-acct-cr.Sh-name
                  out-MFO    = Inf-acct-cr.code
                  out-CAcct  = Inf-acct-cr.corr-acct
                  out-Acct   = Inf-acct-cr.corr-acct1
               .
         END.
      END.
      ELSE IF (inf-acct.acct-type EQ "МежБанк" OR (inf-acct.acct-type EQ "Филиал"
                                             AND inf-acct.code  NE ?)) AND inf-acct.flag NE "МежФилиалБезБИК"
                                            THEN DO:                                     /* 1.2.2 */
          IF inf-acct.acct-cat EQ "Б" AND  inf-acct.bank-id NE ? THEN                    /* 1.2.2.1 */
          FIND banks WHERE banks.bank-id EQ inf-acct.bank-id NO-LOCK NO-ERROR.
          out-Acct = inf-acct.acct.
          IF NOT AVAIL Inf-Fil THEN DO:
             out-Name  =  ConvertNameINN(inf-cl.inn,NO) +
                          inf-cl.name +
                          (IF inf-cl.acct NE "" THEN (" р/с " + SUBSTRING(inf-cl.acct, 1, 25))
                                                ELSE "") +
                          IF AVAIL banks THEN (" в " + BankNameCity(BUFFER Banks))
                                         ELSE "".
             IF inf-acct.acct-cat EQ "Ю" THEN DO:
                out-Name  =  ConvertNameINN(inf-cl.inn,NO) +
                             inf-cl.name +
                             (IF inf-cl.acct NE "" THEN (" р/с " + SUBSTRING(inf-cl.acct, 1, 25))
                                                   ELSE "") +
                             " в " +
                             Info-Store.Name.
             END.
             IF AVAIL inf-acct-cr AND inf-acct-cr.flag-client EQ "БанкКлиент" THEN DO:
                out-Name  =  ConvertNameINN(inf-cl.inn,NO) +
                             inf-cl.name +
                             (IF inf-cl.acct NE "" THEN (" р/с " + SUBSTRING(inf-cl.acct, 1, 25))
                                                   ELSE "") +
                             (IF inf-acct.name NE "" THEN (" в " + inf-acct.name)
                                                     ELSE "") +
                             " к/с " + inf-acct.acct + " в " +
                             Info-Store.Name.
                ASSIGN
                   out-RKC  = Inf-acct-cr.Sh-name
                   out-MFO    = Inf-acct-cr.code
                   out-CAcct  = Inf-acct-cr.corr-acct
                   out-Acct  = Inf-acct-cr.corr-acct1
                .
             END.

          END.
          ELSE DO:
             out-Name  =  ConvertNameINN(inf-cl.inn,NO) +
                          inf-cl.name +
                          (IF inf-cl.acct NE "" THEN (" р/с " + SUBSTRING(inf-cl.acct, 1, 25))
                                                ELSE "") +
                          IF AVAIL banks THEN (" в " + BankNameCity(BUFFER Banks))
                                         ELSE "".
             out-Name = out-Name + " в " + inf-fil.name.
          END.
      END.
      ELSE IF inf-acct.flag EQ "МежФилиалБезБИК" THEN DO:                              /* 1.2.3. */
         FIND banks WHERE banks.bank-id EQ inf-acct.bank-id NO-LOCK NO-ERROR.
         out-Name =  ConvertNameINN(inf-cl.inn,NO) +
                     inf-cl.name /* +
                     IF AVAIL banks THEN (" в " + BankNameCity(BUFFER Banks))
                                    ELSE "" */ .
         out-Acct = inf-cl.acct.
         IF AVAIL inf-acct-cr AND inf-acct-cr.flag-client EQ "БанкКлиент" THEN DO:
               out-Name =  ConvertNameINN(inf-cl.inn,NO) +
                           inf-cl.name +
                           (IF inf-cl.acct NE "" THEN (" р/c " + SUBSTRING(inf-cl.acct,1 , 25))
                                                 ELSE "") + " в " +
                           Info-Store.Name.
               ASSIGN
                  out-RKC  = Inf-acct-cr.Sh-name
                  out-MFO    = Inf-acct-cr.code
                  out-CAcct  = Inf-acct-cr.corr-acct
                  out-Acct  = Inf-acct-cr.corr-acct1
               .
         END.
         IF AVAIL inf-fil THEN DO: /* счет дебета - счет филиала в ГО */
            IF inf-acct.acct-cat EQ "Б"
               AND inf-acct.bank-id EQ Info-Store.bank-id THEN DO:
               out-Name =  ConvertNameINN(inf-cl.inn,NO) +
                           inf-cl.name.
            END. /***************************/
         END.
      END.
      ELSE DO:                                                                              /* 1.2.4. */
         ASSIGN
            out-Name = ConvertNameINN(inf-cl.inn,NO) +
                       inf-cl.name
            out-Acct = inf-cl.acct
         .
         IF AVAIL inf-acct-cr AND inf-acct-cr.flag-client EQ "БанкКлиент" THEN DO:
               out-Name =  ConvertNameINN(inf-cl.inn,NO) +
                           inf-cl.name +
                           (IF inf-cl.acct NE "" THEN (" р/c " + SUBSTRING(inf-cl.acct, 1, 25))
                                                 ELSE "") + " в " +
                           Info-Store.Name.
               ASSIGN
                  out-RKC  = Inf-acct-cr.Sh-name
                  out-MFO    = Inf-acct-cr.code
                  out-CAcct  = Inf-acct-cr.corr-acct
                  out-Acct  = Inf-acct-cr.corr-acct1
               .
         END.
         IF AVAIL Inf-Fil THEN
         out-Name = out-Name /* + " в " + inf-fil.name */.
      END.
   END.
   ELSE IF AVAIL inf-acct AND AVAIL inf-cl AND AVAIL inf-bank THEN DO:                     /* 1.3. */
      IF inf-acct.acct-cat EQ "Б" AND inf-acct.flag-rkc = "РКЦ" THEN DO:                   /* 1.3.1. */
         ASSIGN
            out-Name   = ConvertNameINN(inf-cl.inn,NO) +
                         inf-cl.name

            out-Acct  = inf-cl.acct
            out-RKC  = inf-bank.name
            out-MFO    = inf-bank.code
            out-CAcct  = inf-bank.acct
         .
      END.
      ELSE IF inf-acct.bank-id NE ? AND inf-acct.bank-id EQ inf-bank.bank-id THEN DO:      /* 1.3.2. */
         ASSIGN
            out-RKC  = Info-Store.name
            out-MFO    = Info-Store.code
            out-CAcct  = Info-Store.acct
         .
         IF (inf-acct.acct-type EQ "МежБанк" OR (inf-acct.acct-type EQ "Филиал"
                                                AND inf-acct.code  NE ?)) AND inf-acct.flag NE "МежФилиалБезБИК"
                                               THEN DO:
             IF inf-acct.acct-cat EQ "Б" AND  inf-acct.bank-id NE ? THEN
             FIND banks WHERE banks.bank-id EQ inf-acct.bank-id NO-LOCK NO-ERROR.
             out-Acct = inf-acct.acct.
             IF NOT AVAIL Inf-Fil THEN DO:
                out-Name  =  ConvertNameINN(inf-cl.inn,NO) +
                             inf-cl.name +
                             (IF inf-cl.acct NE "" THEN (" р/с " + SUBSTRING(inf-cl.acct, 1, 25))
                                                   ELSE "") +
                             IF AVAIL banks THEN (" в " + BankNameCity(BUFFER Banks))
                                            ELSE "".
                IF inf-acct.acct-cat EQ "Ю" THEN DO:
                   out-Name  =  ConvertNameINN(inf-cl.inn,NO) +
                                inf-cl.name +
                                (IF inf-cl.acct NE "" THEN (" р/с " + SUBSTRING(inf-cl.acct, 1, 25))
                                                      ELSE "") +
                                " в " +
                                Info-Store.Name.
                END.
                IF AVAIL inf-acct-cr AND inf-acct-cr.flag-client EQ "БанкКлиент" THEN DO:
                   out-Name  =  ConvertNameINN(inf-cl.inn,NO) +
                                inf-cl.name +
                                (IF inf-cl.acct NE "" THEN (" р/с " + SUBSTRING(inf-cl.acct, 1, 25))
                                                      ELSE "") +
                                (IF inf-acct.name NE "" THEN (" в " + inf-acct.name)
                                                        ELSE "") +
                                " к/с " + inf-acct.acct + " в " +
                                Info-Store.Name.
                   ASSIGN
                      out-RKC  = Inf-acct-cr.Sh-name
                      out-MFO    = Inf-acct-cr.code
                      out-CAcct  = Inf-acct-cr.corr-acct
                      out-Acct  = Inf-acct-cr.corr-acct1
                   .
                END.
             END.
             ELSE DO:
               FIND FIRST branch WHERE branch.branch-id EQ shfilial NO-LOCK NO-ERROR.

               IF AVAILABLE branch 
                  AND NOT CAN-FIND(FIRST banks-code WHERE banks-code.bank-id        EQ branch.bank-id
                                                      AND banks-code.bank-code-type EQ "МФО-9")
                  AND inf-bank.code NE ?
               THEN
               DO:
                  ASSIGN
                     out-Name  = ConvertNameINN(inf-cl.inn,NO) + inf-cl.name
                     out-Acct  = inf-cl.acct
                  .
               
               END.
               ELSE
               DO:
                  out-Name  =  ConvertNameINN(inf-cl.inn,NO) +
                               inf-cl.name +
                               (IF inf-cl.acct NE "" THEN (" р/с " + SUBSTRING(inf-cl.acct, 1, 25))
                                                     ELSE "") +
                               IF AVAIL banks THEN (" в " + BankNameCity(BUFFER Banks))
                                              ELSE "".
                  out-Name = out-Name + " в " + inf-fil.name.
               END.
             END.
         END.
         ELSE IF inf-acct.flag EQ "МежФилиалБезБИК" THEN DO:
            FIND banks WHERE banks.bank-id EQ inf-acct.bank-id NO-LOCK NO-ERROR.
            out-Name =  ConvertNameINN(inf-cl.inn,NO) +
                        inf-cl.name /* +
                        IF AVAIL banks THEN (" в " + BankNameCity(BUFFER Banks))
                                       ELSE "" */ .
            out-Acct = inf-cl.acct.
            IF AVAIL inf-acct-cr AND inf-acct-cr.flag-client EQ "БанкКлиент" THEN DO:
                  out-Name =  ConvertNameINN(inf-cl.inn,NO) +
                              inf-cl.name +
                              (IF inf-cl.acct NE "" THEN (" р/c " + SUBSTRING(inf-cl.acct, 1, 25))
                                                    ELSE "") + " в " +
                              Info-Store.Name.
                  ASSIGN
                     out-RKC  = Inf-acct-cr.Sh-name
                     out-MFO    = Inf-acct-cr.code
                     out-CAcct  = Inf-acct-cr.corr-acct
                     out-Acct  = Inf-acct-cr.corr-acct1
                  .
            END.
            IF AVAIL inf-fil THEN DO: /* счет дебета - счет филиала в ГО */
               IF inf-acct.acct-cat EQ "Б"
                  AND inf-acct.bank-id EQ Info-Store.bank-id THEN DO:
                  out-Name =  ConvertNameINN(inf-cl.inn,NO) +
                              inf-cl.name.
               END. /***************************/
            END.
         END.
      END.
      ELSE IF (inf-acct.acct-type EQ "МежБанк" OR inf-acct.acct-type EQ "Филиал")       /* 1.3.3. */
         AND inf-acct.code NE ? AND NOT AVAIL inf-fil THEN DO:
         IF inf-acct.bank-id NE ? THEN DO:
            ASSIGN
               out-RKC  = Info-Store.name
               out-MFO    = Info-Store.code
               out-CAcct  = Info-Store.acct
            .
            IF (inf-bank.code EQ ? AND inf-bank.acct NE ?) OR inf-bank.corr-acct NE ? THEN DO:     /* 1.3.3.1. */
               FIND banks WHERE banks.bank-id EQ inf-acct.bank-id NO-LOCK NO-ERROR.
               out-Name = ConvertNameINN(inf-cl.inn,NO) +
                        inf-cl.name +
                        (IF inf-cl.acct NE "" THEN (" р/с " + SUBSTRING(inf-cl.acct, 1, 25))
                                              ELSE "") +
                        (IF inf-bank.name NE "" AND inf-bank.name NE ? THEN
                        (" в " + inf-bank.name)
                        ELSE "") +
                        " к/c "  +
                        (IF inf-bank.corr-acct NE ? THEN inf-bank.corr-acct
                                                    ELSE inf-bank.acct) +
                        (IF AVAIL banks THEN (" в " + BankNameCity(BUFFER Banks))
                                        ELSE "").

               out-Acct = inf-acct.acct.
               IF AVAIL inf-acct-cr AND inf-acct-cr.flag-client EQ "БанкКлиент" THEN DO:
                  out-Name = ConvertNameINN(inf-cl.inn,NO) +
                           inf-cl.name +
                           (IF inf-cl.acct NE "" THEN (" р/с " + SUBSTRING(inf-cl.acct, 1, 25))
                                                 ELSE "") +
                           (IF inf-bank.name NE "" AND inf-bank.name NE ? THEN
                           (" в " + inf-bank.name)
                           ELSE "") +
                           " к/c "  +
                           (IF inf-bank.corr-acct NE ? THEN inf-bank.corr-acct
                                                       ELSE inf-bank.acct) +
                           (IF AVAIL banks THEN (" в " + BankNameCity(BUFFER Banks))
                                           ELSE "") +
                           " к/с " + inf-acct.acct + " в " + Info-Store.Name
                           .
                  ASSIGN
                     out-RKC  = Inf-acct-cr.Sh-name
                     out-MFO    = Inf-acct-cr.code
                     out-CAcct  = Inf-acct-cr.corr-acct
                     out-Acct  = Inf-acct-cr.corr-acct1
                  .
               END.
            END.
            ELSE IF inf-bank.code NE ? THEN DO:                               /* 1.3.3.2. */

            &IF DEFINED(NoUsePTransDoc) EQ 0 &THEN 
               IF    FGetSetting("ПТранзДок", "", "Нет") EQ "Нет"
                  OR FGetSetting("ПТранзДок", "", "Нет") EQ "No" THEN
               DO:
            &ENDIF
                  ASSIGN
                     out-Name   = ConvertNameINN(inf-cl.inn,NO) +
                                  inf-cl.name

                     out-Acct  = inf-cl.acct
                     out-RKC  = inf-bank.name
                     out-MFO    = inf-bank.code
                     out-CAcct  = inf-bank.acct
                  .

            &IF DEFINED(NoUsePTransDoc) EQ 0 &THEN 
               END.
               ELSE
               DO:
                  ASSIGN
                     out-Name   = ConvertNameINN(inf-cl.inn,NO)
                                + inf-cl.name
                                + " р/с " + inf-cl.acct
                                + " в " + inf-bank.name

                     out-Acct   = inf-acct.acct
                     out-RKC    = Info-Store.name
                     out-MFO    = Info-Store.code
                     out-CAcct  = Info-Store.acct
                  .
               END.
            &ENDIF
/****************
               FIND banks WHERE banks.bank-id EQ inf-acct.bank-id NO-LOCK NO-ERROR.
               out-Name = (IF inf-cl.inn NE "" THEN ("ИНН" + " " + inf-cl.inn + " ")
                                               ELSE "") +
                        inf-cl.name +
                        (IF inf-cl.acct NE "" THEN (" р/с " + SUBSTRING(inf-cl.acct, 1, 25))
                                              ELSE "") +
                        (IF inf-bank.name NE "" AND inf-bank.name NE ? THEN
                        (" в " + inf-bank.name)
                        ELSE "") +
                        (IF AVAIL banks THEN (" в " + BankNameCity(BUFFER Banks))
                                        ELSE "").
               out-Acct = inf-acct.acct. */
               IF AVAIL inf-acct-cr AND inf-acct-cr.flag-client EQ "БанкКлиент" THEN DO:
                  out-Name = ConvertNameINN(inf-cl.inn,NO) +
                           inf-cl.name +
                           (IF inf-cl.acct NE "" THEN (" р/с " + SUBSTRING(inf-cl.acct, 1, 25))
                                                 ELSE "") +
                           (IF inf-bank.name NE "" AND inf-bank.name NE ? THEN
                           (" в " + inf-bank.name)
                           ELSE "") +
                           (IF AVAIL banks THEN (" в " + BankNameCity(BUFFER Banks))
                                           ELSE "") +
                           " к/с " + inf-acct.acct + " в " + Info-Store.Name
                           .
                  ASSIGN
                     out-RKC  = Inf-acct-cr.Sh-name
                     out-MFO    = Inf-acct-cr.code
                     out-CAcct  = Inf-acct-cr.corr-acct
                     out-Acct  = Inf-acct-cr.corr-acct1
                  .
               END.

            END.
         END.
      END.
      ELSE IF inf-acct.flag EQ "МежФилиалБезБИК" THEN DO:               /* 1.3.4. */
         IF inf-bank.code EQ ? THEN DO:
            IF NOT AVAIL inf-fil THEN DO:
               FIND banks WHERE banks.bank-id EQ inf-acct.bank-id NO-LOCK NO-ERROR.
               ASSIGN
                  out-RKC  = Info-Store.name
                  out-MFO    = Info-Store.code
                  out-CAcct  = Info-Store.acct
               .
               out-Name =  ConvertNameINN(inf-cl.inn,NO) +
                           inf-cl.name +
                           (IF inf-cl.acct NE "" THEN (" р/с " + SUBSTRING(inf-cl.acct, 1, 25))
                                                    ELSE "") +
                           (IF inf-bank.name NE "" AND inf-bank.name NE ? THEN
                              (" в " + inf-bank.name)
                            ELSE "") +
                           (IF AVAIL banks THEN (" в " + BankNameCity(BUFFER Banks))
                                           ELSE "").
                           .
               out-Acct  = inf-bank.acct.
               IF AVAIL inf-acct-cr AND inf-acct-cr.flag-client EQ "БанкКлиент" THEN DO:
                  out-Name =  ConvertNameINN(inf-cl.inn,NO) +
                              inf-cl.name +
                              (IF inf-cl.acct NE "" THEN (" р/с " + SUBSTRING(inf-cl.acct, 1, 25))
                                                    ELSE "") +
                              (IF inf-bank.name NE "" AND inf-bank.name NE ? THEN
                                  (" в " + inf-bank.name)
                               ELSE "") +
                              (IF AVAIL banks THEN (" в " + BankNameCity(BUFFER Banks))
                                              ELSE "") +
                              (IF inf-cl.acct NE "" THEN (" к/с " + inf-bank.acct)
                                                    ELSE "") + " в " + Info-Store.Name
                              .
                     ASSIGN
                        out-RKC  = Inf-acct-cr.Sh-name
                        out-MFO    = Inf-acct-cr.code
                        out-CAcct  = Inf-acct-cr.corr-acct
                        out-Acct  = Inf-acct-cr.corr-acct1
                     .
               END.
            END.
            ELSE DO:
               FIND acct WHERE acct.acct     EQ inf-acct.acct
                           AND acct.currency EQ "" NO-LOCK NO-ERROR.
               IF acct.branch-id EQ inf-fil.code AND inf-acct.acct-cat EQ "Б"
                  AND inf-acct.bank-id EQ Info-Store.bank-id THEN DO:
                  ASSIGN
                     out-Name =  ConvertNameINN(inf-cl.inn,NO) +
                                 inf-cl.name
                     out-Acct  = inf-cl.acct
                     out-RKC  = inf-bank.name
                     out-MFO    = inf-bank.code
                     out-CAcct  = inf-bank.acct
                  .
               END.
            END.
         END.
         ELSE DO:
            ASSIGN
               out-Name = ConvertNameINN(inf-cl.inn,NO) +
                          inf-cl.name
               out-Acct  = inf-cl.acct
               out-RKC  = inf-bank.name
               out-MFO    = inf-bank.code
               out-CAcct  = inf-bank.acct
            .
         END.
      END.
      ELSE DO:                                                     /* 1.3.5. */

      &IF DEFINED(NoUsePTransDoc) EQ 0 &THEN 
         IF    FGetSetting("ПТранзДок", "", "Нет") EQ "Нет"
            OR FGetSetting("ПТранзДок", "", "Нет") EQ "No" THEN
         DO:
      &ENDIF
            ASSIGN
               out-Name   = ConvertNameINN(inf-cl.inn,NO) +
                            inf-cl.name

               out-Acct  = inf-cl.acct
               out-RKC  = inf-bank.name
               out-MFO    = inf-bank.code
               out-CAcct  = inf-bank.acct
            .
      &IF DEFINED(NoUsePTransDoc) EQ 0 &THEN 
         END.
         ELSE
         DO:
            ASSIGN
               out-Name   = ConvertNameINN(inf-cl.inn,NO)
                          + inf-cl.name
                          + " р/с " + inf-cl.acct
                          + " в " + inf-bank.name

               out-Acct   = inf-acct.acct
               out-RKC    = Info-Store.name
               out-MFO    = Info-Store.code
               out-CAcct  = Info-Store.acct
            .
         END.
      &ENDIF
      END.
   END.
   ELSE IF AVAIL inf-acct AND NOT AVAIL inf-cl AND AVAIL inf-bank THEN DO:
      IF inf-acct.acct-cat EQ "Б" AND inf-acct.flag-rkc = "РКЦ" THEN DO:
         ASSIGN
            out-Name   = ConvertNameINN(inf-acct.inn,NO) +
                         inf-acct.name

            out-Acct   = inf-acct.acct
            out-RKC    = inf-bank.name
            out-MFO    = inf-bank.code
            out-CAcct  = inf-bank.acct
         .
      END.
      ELSE IF inf-acct.bank-id NE ? AND inf-acct.bank-id EQ inf-bank.bank-id THEN DO:
         ASSIGN
            out-RKC    = Info-Store.name
            out-MFO    = Info-Store.code
            out-CAcct  = Info-Store.acct
         .
         IF (inf-acct.acct-type EQ "МежБанк" OR (inf-acct.acct-type EQ "Филиал"
                                                AND inf-acct.code  NE ?)) AND inf-acct.flag NE "МежФилиалБезБИК"
                                               THEN DO:
             IF inf-acct.acct-cat EQ "Б" AND  inf-acct.bank-id NE ? THEN
             FIND banks WHERE banks.bank-id EQ inf-acct.bank-id NO-LOCK NO-ERROR.
             out-Acct = inf-acct.acct.
             IF NOT AVAIL Inf-Fil THEN DO:
                out-Name  =  ConvertNameINN(inf-acct.inn,NO) +
                             inf-acct.name +
                             (IF inf-acct.acct NE "" THEN (" р/с " + SUBSTRING(inf-acct.acct, 1, 25))
                                                   ELSE "") +
                             IF AVAIL banks THEN (" в " + BankNameCity(BUFFER Banks))
                                            ELSE "".
                IF inf-acct.acct-cat EQ "Ю" THEN DO:
                   out-Name  =  ConvertNameINN(inf-acct.inn,NO) +
                                inf-acct.name +
                                (IF inf-acct.acct NE "" THEN (" р/с " + SUBSTRING(inf-acct.acct, 1, 25))
                                                      ELSE "") +
                                " в " +
                                Info-Store.Name.
                END.
                IF AVAIL inf-acct-cr AND inf-acct-cr.flag-client EQ "БанкКлиент" THEN DO:
                   out-Name  =  ConvertNameINN(inf-acct.inn,NO) +
                                inf-acct.name +
                                (IF inf-acct.acct NE "" THEN (" р/с " + SUBSTRING(inf-acct.acct, 1, 25))
                                                      ELSE "") +
                                (IF inf-acct.name NE "" THEN (" в " + inf-acct.name)
                                                        ELSE "") +
                                " к/с " + inf-acct.acct + " в " +
                                Info-Store.Name.
                   ASSIGN
                      out-RKC  = Inf-acct-cr.Sh-name
                      out-MFO    = Inf-acct-cr.code
                      out-CAcct  = Inf-acct-cr.corr-acct
                      out-Acct  = Inf-acct-cr.corr-acct1
                   .
                END.
             END.
             ELSE DO:
                out-Name  =  ConvertNameINN(inf-acct.inn,NO) +
                             inf-acct.name +
                             (IF inf-acct.acct NE "" THEN (" р/с " + SUBSTRING(inf-acct.acct, 1, 25))
                                                   ELSE "") +
                             IF AVAIL banks THEN (" в " + BankNameCity(BUFFER Banks))
                                            ELSE "".
                out-Name = out-Name + " в " + inf-fil.name.
             END.
         END.
         ELSE IF inf-acct.flag EQ "МежФилиалБезБИК" THEN DO:
            FIND banks WHERE banks.bank-id EQ inf-acct.bank-id NO-LOCK NO-ERROR.
            out-Name =  ConvertNameINN(inf-acct.inn,NO) +
                        inf-acct.name.
            out-Acct = inf-acct.acct.
            IF AVAIL inf-acct-cr AND inf-acct-cr.flag-client EQ "БанкКлиент" THEN DO:
                  out-Name =  ConvertNameINN(inf-acct.inn,NO) +
                              inf-acct.name +
                              (IF inf-acct.acct NE "" THEN (" р/c " + SUBSTRING(inf-acct.acct, 1, 25))
                                                    ELSE "") + " в " +
                              Info-Store.Name.
                  ASSIGN
                     out-RKC  = Inf-acct-cr.Sh-name
                     out-MFO    = Inf-acct-cr.code
                     out-CAcct  = Inf-acct-cr.corr-acct
                     out-Acct  = Inf-acct-cr.corr-acct1
                  .
            END.
            IF AVAIL inf-fil THEN DO: /* счет дебета - счет филиала в ГО */
               IF inf-acct.acct-cat EQ "Б"
                  AND inf-acct.bank-id EQ Info-Store.bank-id THEN DO:
                  out-Name =  ConvertNameINN(inf-acct.inn,NO) +
                              inf-acct.name.
               END. /***************************/
            END.
         END.
      END.
      ELSE IF (inf-acct.acct-type EQ "МежБанк" OR inf-acct.acct-type EQ "Филиал")
         AND inf-acct.code NE ? AND NOT AVAIL inf-fil THEN DO:
         IF inf-acct.bank-id NE ? THEN DO:
            ASSIGN
               out-RKC  = Info-Store.name
               out-MFO    = Info-Store.code
               out-CAcct  = Info-Store.acct
            .
            IF (inf-bank.code EQ ? AND inf-bank.acct NE ?) OR inf-bank.corr-acct NE ? THEN DO:
               FIND banks WHERE banks.bank-id EQ inf-acct.bank-id NO-LOCK NO-ERROR.
               out-Name = ConvertNameINN(inf-acct.inn,NO) +
                        inf-acct.name +
                        (IF inf-acct.acct NE "" THEN (" р/с " + SUBSTRING(inf-acct.acct, 1, 25))
                                              ELSE "") +
                        (IF inf-bank.name NE "" AND inf-bank.name NE ? THEN
                        (" в " + inf-bank.name)
                        ELSE "") +
                        " к/c "  +
                        (IF inf-bank.corr-acct NE ? THEN inf-bank.corr-acct
                                                    ELSE inf-bank.acct) +
                        (IF AVAIL banks THEN (" в " + BankNameCity(BUFFER Banks))
                                        ELSE "").

               out-Acct = inf-acct.acct.
               IF AVAIL inf-acct-cr AND inf-acct-cr.flag-client EQ "БанкКлиент" THEN DO:
                  out-Name = ConvertNameINN(inf-acct.inn,NO) +
                           inf-acct.name +
                           (IF inf-acct.acct NE "" THEN (" р/с " + SUBSTRING(inf-acct.acct, 1, 25))
                                                 ELSE "") +
                           (IF inf-bank.name NE "" AND inf-bank.name NE ? THEN
                           (" в " + inf-bank.name)
                           ELSE "") +
                           " к/c "  +
                           (IF inf-bank.corr-acct NE ? THEN inf-bank.corr-acct
                                                       ELSE inf-bank.acct) +
                           (IF AVAIL banks THEN (" в " + BankNameCity(BUFFER Banks))
                                           ELSE "") +
                           " к/с " + inf-acct.acct + " в " + Info-Store.Name
                           .
                  ASSIGN
                     out-RKC  = Inf-acct-cr.Sh-name
                     out-MFO    = Inf-acct-cr.code
                     out-CAcct  = Inf-acct-cr.corr-acct
                     out-Acct  = Inf-acct-cr.corr-acct1
                  .
               END.
            END.
            ELSE IF inf-bank.code NE ? THEN DO:
               ASSIGN
                  out-Name   = ConvertNameINN(inf-acct.inn,NO) +
                               inf-acct.name

                  out-Acct  = inf-acct.acct
                  out-RKC  = inf-bank.name
                  out-MFO    = inf-bank.code
                  out-CAcct  = inf-bank.acct
               .
               IF AVAIL inf-acct-cr AND inf-acct-cr.flag-client EQ "БанкКлиент" THEN DO:
                  out-Name = ConvertNameINN(inf-acct.inn,NO) +
                           inf-acct.name +
                           (IF inf-acct.acct NE "" THEN (" р/с " + SUBSTRING(inf-acct.acct, 1, 25))
                                                 ELSE "") +
                           (IF inf-bank.name NE "" AND inf-bank.name NE ? THEN
                           (" в " + inf-bank.name)
                           ELSE "") +
                           (IF AVAIL banks THEN (" в " + BankNameCity(BUFFER Banks))
                                           ELSE "") +
                           " к/с " + inf-acct.acct + " в " + Info-Store.Name
                           .
                  ASSIGN
                     out-RKC  = Inf-acct-cr.Sh-name
                     out-MFO    = Inf-acct-cr.code
                     out-CAcct  = Inf-acct-cr.corr-acct
                     out-Acct  = Inf-acct-cr.corr-acct1
                  .
               END.

            END.
         END.
      END.
      ELSE IF inf-acct.flag EQ "МежФилиалБезБИК" THEN DO:
         IF inf-bank.code EQ ? THEN DO:
            IF NOT AVAIL inf-fil THEN DO:
               FIND banks WHERE banks.bank-id EQ inf-acct.bank-id NO-LOCK NO-ERROR.
               ASSIGN
                  out-RKC  = Info-Store.name
                  out-MFO    = Info-Store.code
                  out-CAcct  = Info-Store.acct
               .
               out-Name =  ConvertNameINN(inf-acct.inn,NO) +
                           inf-acct.name +
                           (IF inf-acct.acct NE "" THEN (" р/с " + SUBSTRING(inf-acct.acct, 1, 25))
                                                    ELSE "") +
                           (IF inf-bank.name NE "" AND inf-bank.name NE ? THEN
                              (" в " + inf-bank.name)
                            ELSE "") +
                           (IF AVAIL banks THEN (" в " + BankNameCity(BUFFER Banks))
                                           ELSE "").
                           .
               out-Acct  = inf-bank.acct.
               IF AVAIL inf-acct-cr AND inf-acct-cr.flag-client EQ "БанкКлиент" THEN DO:
                  out-Name =  ConvertNameINN(inf-acct.inn,NO) +
                              inf-acct.name +
                              (IF inf-acct.acct NE "" THEN (" р/с " + SUBSTRING(inf-acct.acct, 1, 25))
                                                    ELSE "") +
                              (IF inf-bank.name NE "" AND inf-bank.name NE ? THEN
                                  (" в " + inf-bank.name)
                               ELSE "") +
                              (IF AVAIL banks THEN (" в " + BankNameCity(BUFFER Banks))
                                              ELSE "") +
                              (IF inf-acct.acct NE "" THEN (" к/с " + inf-bank.acct)
                                                    ELSE "") + " в " + Info-Store.Name
                              .
                     ASSIGN
                        out-RKC  = Inf-acct-cr.Sh-name
                        out-MFO    = Inf-acct-cr.code
                        out-CAcct  = Inf-acct-cr.corr-acct
                        out-Acct  = Inf-acct-cr.corr-acct1
                     .
               END.
            END.
            ELSE DO:
               FIND acct WHERE acct.acct     EQ inf-acct.acct
                           AND acct.currency EQ "" NO-LOCK NO-ERROR.
               IF acct.branch-id EQ inf-fil.code AND inf-acct.acct-cat EQ "Б"
                  AND inf-acct.bank-id EQ Info-Store.bank-id THEN DO:
                  ASSIGN
                     out-Name =  ConvertNameINN(inf-acct.inn,NO) +
                                 inf-acct.name
                     out-Acct  = inf-acct.acct
                     out-RKC  = inf-bank.name
                     out-MFO    = inf-bank.code
                     out-CAcct  = inf-bank.acct
                  .
               END.
            END.
         END.
         ELSE DO:
            ASSIGN
               out-Name = ConvertNameINN(inf-acct.inn,NO) +
                          inf-acct.name
               out-Acct  = inf-acct.acct
               out-RKC  = inf-bank.name
               out-MFO    = inf-bank.code
               out-CAcct  = inf-bank.acct
            .
         END.
      END.
      ELSE DO:
         ASSIGN
            out-Name   = ConvertNameINN(inf-acct.inn,NO) +
                         inf-acct.name

            out-Acct  = inf-acct.acct
            out-RKC  = inf-bank.name
            out-MFO    = inf-bank.code
            out-CAcct  = inf-bank.acct
         .
      END.
   END.
END PROCEDURE.

PROCEDURE for-rec:
   DEFINE INPUT PARAMETER  in-param   AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER  in-doc-type AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER out-Name   AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER out-Acct   AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER out-RKC    AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER out-CAcct  AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER out-MFO    AS CHARACTER NO-UNDO.

   DEF BUFFER Inf-acct FOR Info-Store.
   DEF BUFFER Inf-cl   FOR Info-Store.
   DEF BUFFER Inf-bank FOR Info-Store.
   DEF BUFFER Inf-fil  FOR Info-Store.

   FIND inf-acct WHERE inf-acct.info-id   EQ ENTRY(1,in-param) NO-LOCK NO-ERROR.
   IF NOT avail inf-acct THEN /* Счет не в нашем банке, но надо распечатать */
   FIND inf-acct WHERE inf-acct.info-id   EQ ENTRY(2,in-param) NO-LOCK NO-ERROR.
   IF in-doc-type NE "ВО" THEN
   FIND inf-cl   WHERE inf-cl.info-id     EQ ENTRY(2,in-param) NO-LOCK NO-ERROR.
   IF in-doc-type NE "ВО" THEN
   FIND inf-bank WHERE inf-bank.info-id   EQ ENTRY(3,in-param)     NO-LOCK NO-ERROR.
   FIND Info-Store WHERE info-store.info-id   EQ ENTRY(4,in-param) NO-LOCK NO-ERROR.
   FIND Inf-fil WHERE inf-fil.info-id   EQ ENTRY(5,in-param) NO-LOCK NO-ERROR.

   RELEASE banks.
   IF AVAIL inf-acct AND NOT AVAIL inf-cl AND NOT AVAIL inf-bank THEN DO:
      ASSIGN
         out-RKC  = Info-Store.name
         out-MFO    = Info-Store.code
         out-CAcct  = Info-Store.acct
         out-Acct   = inf-acct.acct
      .
      out-Acct  = Inf-acct.acct.
      IF Inf-acct.acct-cat EQ "В" OR inf-acct.flag-balchinn EQ "БалСчИНН" THEN DO:
         IF NOT AVAIL Inf-Fil THEN
         out-Name =  ConvertNameINN(Info-Store.inn,YES) +
                     Info-Store.Sh-Name.
         ELSE
         out-Name =  ConvertNameINN(Inf-Fil.inn,YES) +
                     Inf-Fil.Sh-Name.
      END.
      ELSE DO:
         out-Name =  ConvertNameINN(Inf-acct.inn,YES) +
                     Inf-acct.Name.
         IF AVAIL Inf-Fil THEN
         out-Name = out-Name + " в " + inf-fil.name.
      END.
   END.
   IF AVAIL inf-acct AND AVAIL inf-cl AND NOT AVAIL inf-bank THEN DO:
      ASSIGN
         out-RKC  = Info-Store.name
         out-MFO    = Info-Store.code
         out-CAcct  = Info-Store.acct
      .
      IF inf-cl.acct EQ inf-acct.acct THEN DO:
         ASSIGN
            out-Name   = ConvertNameINN(inf-cl.inn,YES) +
                         inf-cl.name
            out-Acct   = inf-acct.acct
            out-RKC  = Info-Store.name
            out-MFO    = Info-Store.code
            out-CAcct  = Info-Store.acct
         .
         
           
      END.
      ELSE IF (inf-acct.acct-type EQ "МежБанк" OR (inf-acct.acct-type EQ "Филиал"
                                             AND inf-acct.code  NE ?)) AND inf-acct.flag NE "МежФилиалБезБИК"
                                            THEN DO:
                                            
                                          
         IF inf-acct.acct-cat EQ "Б" AND  inf-acct.bank-id NE ? THEN
         FIND banks WHERE banks.bank-id EQ inf-acct.bank-id NO-LOCK NO-ERROR.
         FIND banks-code OF banks WHERE banks-code.bank-code-type EQ "МФО-9"
                                                                       NO-LOCK NO-ERROR.
         FIND FIRST banks-corr WHERE banks-corr.bank-corr EQ banks.bank-id
                                 AND CAN-FIND (banks OF banks-corr WHERE banks.flag-rkc)
                                                                        NO-LOCK NO-ERROR.

         ASSIGN
            out-Name  =  ConvertNameINN(inf-cl.inn,YES) +
                         inf-cl.name
            out-Acct     = inf-cl.acct
            out-RKC    = (IF AVAIL banks THEN BankNameCity(BUFFER Banks) ELSE "")
            out-MFO      = (IF AVAIL banks-code THEN banks-code.bank-code ELSE "")
            out-CAcct    = (IF AVAIL banks-corr THEN banks-corr.corr-acct ELSE "")
         .
         
                         
      END.      
      
      ELSE IF inf-acct.flag EQ "МежФилиалБезБИК" THEN DO:
         IF NOT AVAIL Inf-Fil THEN DO:
            FIND banks WHERE banks.bank-id EQ inf-acct.bank-id NO-LOCK NO-ERROR.
            out-Name =  ConvertNameINN(inf-cl.inn,YES) +
                        inf-cl.name /* +
                        IF AVAIL banks THEN (" в " + BankNameCity(BUFFER Banks))
                                       ELSE "" */ .
                                       
                                             
         END.
         ELSE DO:
            IF inf-acct.acct-cat EQ "Б"
               AND inf-acct.bank-id EQ Info-Store.bank-id THEN DO:

               out-Name = ConvertNameINN(inf-cl.inn,YES) +
                          inf-cl.name.

            END.
         END.
         out-Acct = inf-cl.acct.
          
      END.
      ELSE DO:
         ASSIGN
            out-Name =  ConvertNameINN(inf-cl.inn,YES) +
                        inf-cl.name
            out-Acct    = inf-cl.acct
         .
         IF AVAIL Inf-Fil THEN DO:
            out-Name = out-Name /* + " в " + inf-fil.name */ .

         END.
      END.
      
   END.       
    
   IF AVAIL inf-acct AND AVAIL inf-cl AND AVAIL inf-bank THEN DO:
      ASSIGN
         out-RKC  = Info-Store.name
         out-MFO    = Info-Store.code
         out-CAcct  = Info-Store.acct
      .
      IF inf-acct.acct-cat EQ "Б" AND inf-acct.flag-rkc EQ "РКЦ" THEN DO:
         ASSIGN
            out-Name   = ConvertNameINN(inf-cl.inn,YES) +
                         inf-cl.name
            out-Acct   = inf-cl.acct
            out-RKC    = inf-bank.name
            out-MFO    = inf-bank.code
            out-CAcct  = inf-bank.acct
         .
         
             
      END.
      ELSE IF inf-acct.bank-id EQ inf-bank.bank-id THEN DO:
         IF (inf-acct.acct-type EQ "МежБанк" OR (inf-acct.acct-type EQ "Филиал"
                                                AND inf-acct.code  NE ?)) AND inf-acct.flag NE "МежФилиалБезБИК"
                                               THEN DO:
            IF inf-acct.acct-cat EQ "Б" AND  inf-acct.bank-id NE ? THEN
            FIND banks WHERE banks.bank-id EQ inf-acct.bank-id NO-LOCK NO-ERROR.
            FIND banks-code OF banks WHERE banks-code.bank-code-type EQ "МФО-9"
                                                                NO-LOCK NO-ERROR.
            FIND FIRST banks-corr WHERE banks-corr.bank-corr EQ banks.bank-id
                                    AND CAN-FIND (banks OF banks-corr WHERE banks.flag-rkc)
                                                                           NO-LOCK NO-ERROR.

            ASSIGN
               out-Name     = ConvertNameINN(inf-cl.inn,YES) +
                              inf-cl.name
               out-Acct     = inf-cl.acct
               out-RKC      = (IF AVAIL banks THEN BankNameCity(BUFFER Banks) ELSE "")
               out-MFO      = (IF AVAIL banks-code THEN banks-code.bank-code ELSE "")
               out-CAcct    = (IF AVAIL banks-corr THEN banks-corr.corr-acct ELSE "")
            .
             
         END.
         ELSE IF inf-acct.flag EQ "МежФилиалБезБИК" THEN DO:
            IF NOT AVAIL Inf-Fil THEN DO:
               FIND banks WHERE banks.bank-id EQ inf-acct.bank-id NO-LOCK NO-ERROR.
               out-Name =  ConvertNameINN(inf-cl.inn,YES) +
                           inf-cl.name +
                           IF AVAIL banks THEN (" в " + BankNameCity(BUFFER Banks))
                                          ELSE "".
                                          
                              
            END.
            ELSE DO:
           
               IF inf-acct.acct-cat EQ "Б"
                  AND inf-acct.bank-id EQ Info-Store.bank-id THEN DO:
                  out-Name = ConvertNameINN(inf-cl.inn,YES) +
                             inf-cl.name.
               END.
            END.
            out-Acct = inf-cl.acct.
         END.
         
      END.
      ELSE DO:
         ASSIGN
            out-Name    = ConvertNameINN(inf-cl.inn,YES) +
                          inf-cl.name
            out-Acct    = inf-cl.acct
            out-RKC     = Inf-bank.name
            out-MFO     = Inf-bank.code
            out-CAcct   = IF Inf-bank.code EQ ? OR Inf-bank.code EQ ""  /* Это обработка кривой ситуации */
                        THEN ""
                        ELSE Inf-bank.acct
         .         
         /* MESSAGE   "123"     Plmfo              VIEW-AS ALERT-BOX .*/
          out-MFO     = Plmfo. 
          out-Name    = PlRKC[1].
          out-RKC     = PlRKC[1].
          out-Acct    = op-entry.acct-cr.
          out-CAcct   = PlCAcct.
          PoINN = FGetSetting("БанкМФО",?,FGetSetting("БанкС",?,"")).
      END.
      
     
   END.
   IF AVAIL inf-acct AND NOT AVAIL inf-cl AND AVAIL inf-bank THEN DO:
      ASSIGN
         out-RKC  = Info-Store.name
         out-MFO    = Info-Store.code
         out-CAcct  = Info-Store.acct
      .
      IF inf-acct.acct-cat EQ "Б" AND inf-acct.flag-rkc EQ "РКЦ" THEN DO:
         ASSIGN
            out-Name   = ConvertNameINN(inf-acct.inn,YES) +
                         inf-acct.name
            out-Acct   = inf-acct.acct
            out-RKC    = inf-bank.name
            out-MFO    = inf-bank.code
            out-CAcct  = inf-bank.acct
         .
      END.
      ELSE IF inf-acct.bank-id EQ inf-bank.bank-id THEN DO:
         IF (inf-acct.acct-type EQ "МежБанк" OR (inf-acct.acct-type EQ "Филиал"
                                                AND inf-acct.code  NE ?)) AND inf-acct.flag NE "МежФилиалБезБИК"
                                               THEN DO:
            IF inf-acct.acct-cat EQ "Б" AND  inf-acct.bank-id NE ? THEN
            FIND banks WHERE banks.bank-id EQ inf-acct.bank-id NO-LOCK NO-ERROR.
            FIND banks-code OF banks WHERE banks-code.bank-code-type EQ "МФО-9"
                                                                NO-LOCK NO-ERROR.
            FIND FIRST banks-corr WHERE banks-corr.bank-corr EQ banks.bank-id
                                    AND CAN-FIND (banks OF banks-corr WHERE banks.flag-rkc)
                                                                           NO-LOCK NO-ERROR.

            ASSIGN
               out-Name     = ConvertNameINN(inf-acct.inn,YES) +
                              inf-acct.name
               out-Acct     = inf-acct.acct
               out-RKC      = (IF AVAIL banks THEN BankNameCity(BUFFER Banks) ELSE "")
               out-MFO      = (IF AVAIL banks-code THEN banks-code.bank-code ELSE "")
               out-CAcct    = (IF AVAIL banks-corr THEN banks-corr.corr-acct ELSE "")
            .
         END.
         ELSE IF inf-acct.flag EQ "МежФилиалБезБИК" THEN DO:
            IF NOT AVAIL Inf-Fil THEN DO:
               FIND banks WHERE banks.bank-id EQ inf-acct.bank-id NO-LOCK NO-ERROR.
               out-Name =  ConvertNameINN(inf-acct.inn,YES) +
                           inf-acct.name +
                           IF AVAIL banks THEN (" в " + BankNameCity(BUFFER Banks))
                                          ELSE "".
            END.
            ELSE DO:
               IF inf-acct.acct-cat EQ "Б"
                  AND inf-acct.bank-id EQ Info-Store.bank-id THEN DO:
                  out-Name = ConvertNameINN(inf-acct.inn,YES) +
                             inf-acct.name.
               END.
            END.
            out-Acct = inf-acct.acct.
         END.
      END.
      ELSE DO:
         ASSIGN
            out-Name    = ConvertNameINN(inf-acct.inn,YES) +
                          inf-acct.name
            out-Acct    = inf-acct.acct
            out-RKC     = Inf-bank.name
            out-MFO     = Inf-bank.code
            out-CAcct   = IF Inf-bank.code EQ ? OR Inf-bank.code EQ ""  /* Это обработка кривой ситуации */
                        THEN ""
                        ELSE Inf-bank.acct
         .
      END.
   END.
   
   
END PROCEDURE.

PROCEDURE FillElectroField:

   DEF PARAM BUFFER op FOR op.
   DEF OUTPUT PARAM oNumRkc    AS CHAR NO-UNDO.
   DEF OUTPUT PARAM oElDocDate AS CHAR NO-UNDO.
   DEF OUTPUT PARAM oSendId    AS CHAR NO-UNDO.

   DEF VAR vTime  AS CHAR NO-UNDO.
   DEF VAR vBatch AS CHAR NO-UNDO.

   FIND FIRST op-entry OF op NO-LOCK NO-ERROR.
   FIND FIRST op-bank  OF op  
        WHERE op-bank.op-bank-type   EQ "" 
          AND op-bank.bank-code-type EQ "МФО-9"  NO-LOCK NO-ERROR.

   ASSIGN
      oNumRkc    = GetXAttrValueEx("op", STRING(op.op), "NUM-RKC"    , "")
      oElDocDate = GetXAttrValueEx("op", STRING(op.op), "EL-DOC-DATE", "")
      oSendId    = GetXAttrValueEx("op", STRING(op.op), "УИС-ЭД"     , "").

   RUN GetFieldFromPacket
                    (INPUT        YES,  
                     INPUT        "RKCReturn",
                     INPUT        "ExchRKCDoc",
                     INPUT         {&STATE-FIN}, 
                     INPUT        "REPDReturn",
                     INPUT-OUTPUT oNumRkc,    
                     INPUT-OUTPUT oElDocDate,
                     INPUT-OUTPUT oSendId,
                     INPUT-OUTPUT vTime, 
                     INPUT-OUTPUT vBatch).

   RUN GetFieldFromPacket  
                    (INPUT        NO,
                     INPUT        "RKCBegin",
                     INPUT        "ExchRKCDoc",
                     INPUT        {&STATE-FIN}, 
                     INPUT        "REPDBegin",
                     INPUT-OUTPUT oNumRkc,    
                     INPUT-OUTPUT oElDocDate,
                     INPUT-OUTPUT oSendId,
                     INPUT-OUTPUT vTime, 
                     INPUT-OUTPUT vBatch).
END PROCEDURE.


PROCEDURE  GetFieldFromPacket.
   DEF INPUT PARAM        isImport            AS LOGICAL NO-UNDO.
   DEF INPUT PARAM        iPackObjectKind     AS CHAR    NO-UNDO.
   DEF INPUT PARAM        iPacketKind         AS CHAR    NO-UNDO.
   DEF INPUT PARAM        iPacketState        AS CHAR    NO-UNDO.
   DEF INPUT PARAM        iReferenceClassCode AS CHAR    NO-UNDO.
   DEF INPUT-OUTPUT PARAM oNumRkc             AS CHAR    NO-UNDO.
   DEF INPUT-OUTPUT PARAM oElDocDate          AS CHAR    NO-UNDO.
   DEF INPUT-OUTPUT PARAM oSendId             AS CHAR    NO-UNDO.
   DEF INPUT-OUTPUT PARAM oSeanceTime         AS CHAR    NO-UNDO.
   DEF INPUT-OUTPUT PARAM oNumber             AS CHAR    NO-UNDO.

   FOR FIRST PackObject 
       WHERE PackObject.Kind      EQ iPackObjectKind
         AND PackObject.file-name EQ "op-entry" 
         AND PackObject.Surrogate EQ STRING(op-entry.op) + "," +
                                     STRING(op-entry.op-entry) NO-LOCK,
      FIRST Packet WHERE 
            Packet.PacketID EQ PackObject.PacketID 
        AND Packet.Kind     EQ iPacketKind
        AND Packet.State    EQ iPacketState 
            NO-LOCK,
         FIRST Reference WHERE 
               Reference.PacketID   EQ Packet.PacketID 
           AND Reference.Class-Code EQ iReferenceClassCode NO-LOCK, 
             FIRST Seance WHERE
                   Seance.SeanceID  EQ Packet.SeanceID NO-LOCK:

          ASSIGN
             oNumRkc     = Reference.RefValue 
             oElDocDate  = STRING(Packet.PackDate)  
             oSendId     = SUBSTRING(op-bank.bank-code,3,7) + "000"
             oNumber     = Seance.Number
             oSeanceTime = STRING(Seance.SeanceTime,"hh:mm:ss").
          LEAVE.
   END.

   IF isImport AND
      NOT {assigned oNumRkc}  THEN 
      FOR FIRST PackObject 
          WHERE PackObject.Kind      EQ iPackObjectKind
            AND PackObject.file-name EQ "op-entry" 
            AND PackObject.Surrogate EQ STRING(op-entry.op) + "," +
                                        STRING(op-entry.op-entry) NO-LOCK,
         FIRST Packet WHERE 
               Packet.PacketID EQ PackObject.PacketID 
           AND Packet.Kind     EQ iPacketKind
               NO-LOCK,
            FIRST Reference WHERE 
                  Reference.PacketID   EQ Packet.PacketID 
              AND Reference.Class-Code EQ iReferenceClassCode NO-LOCK, 
                 FIRST Seance WHERE
                       Seance.SeanceID  EQ Packet.SeanceID NO-LOCK:
   
             ASSIGN
                oNumRkc    = Reference.RefValue 
                oElDocDate = STRING(Packet.PackDate)  
                oSendId    = SUBSTRING(op-bank.bank-code,3,7) + "000"
                oNumber     = Seance.Number
                oSeanceTime = STRING(Seance.SeanceTime,"hh:mm:ss").
             LEAVE.
      END.
   IF NUM-ENTRIES(oNumRkc,"|") GE 2 THEN  DO:
      oSendId = ENTRY(1,oNumRkc,"|").
      oNumRkc = ENTRY(2,oNumRkc,"|").
   END.
END PROCEDURE.
