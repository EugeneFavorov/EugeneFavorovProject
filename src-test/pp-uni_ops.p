/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2009 ЗАО "Банковские информационные системы"
     Filename: pp-uni_ops.p
      Comment: Универсальная процедура печати платежных поручений
               адаптированная для доверительного управления.
               (Открытая Система Печати)
      Comment:
   Parameters: Input: RID -  RecID(op)
         Uses:
      Used by:
      Created: 11/06/2009 kraw (0112572)
     Modified: 24/09/2009 kraw (0117341) Добавили отметки банка и теги PlName_TeX_Font, PoName_TeX_Font
     Modified: 08/09/2010 kraw (0117604) Факсимиле штампа
     Modified: 12/05/2010 kraw (0120519) Факсимиле печати (РСХБ)
     Modified: 27/10/2010 kraw (0135495) Оригинальная разбивка details
     Modified: 19/04/2011 kraw (0144455) Проверка параметров (штампа) на документе
*/

{globals.i}                                 /* глобальные переменные         */
{intrface.get xclass}
{chkacces.i}
{prn-doc.def &with_proc=YES}
{parsin.def}

&GLOB tt-op-entry yes
&GLOBAL-DEFINE OFFSIGNS YES
&GLOBAL-DEFINE NEW_1256 YES 

&IF defined(NEW_1256) NE 0 &THEN
   &SCOP NFORM    601
   &SCOP NEW_1256 YES
&ELSE
   &SCOP NFORM    60
&ENDIF

&IF defined(ELIT_POL) EQ 0 &THEN
   &SCOP ELIT_POL NO
&ELSE
   &SCOP ELIT_POL YES
&ENDIF

DEFINE VARIABLE i        AS INT64               NO-UNDO.
DEFINE VARIABLE PlatName AS CHARACTER    EXTENT 6 NO-UNDO.
DEFINE VARIABLE PolName  AS CHARACTER    EXTENT 6 NO-UNDO.

DEFINE VARIABLE mStrTMP AS CHARACTER NO-UNDO.


&SCOP TEST YES
&IF "{&TEST}" EQ "YES" &THEN
   DEFINE STREAM test.
   OUTPUT STREAM test TO "test.pp".
&ENDIF
{pp-uni.var {&*}}                                /* определение переменных        */
{pp-uni.not &ACTIVE      = YES
            &MAX-NUM-STR = 4
            &LENGTH      = 30}

{pp-uni.err}                                /* сообщения об ошибках          */
{pp-uni.prg {&*}}                     /* описание стандартных процедур */

{pp-uni.chk}                                /* проверка входных данных       */

{pp-uni.run}                                /* непосредственно расчет        */

{pp-uni.not &BANK-CLIENT = YES}

&IF "{&TEST}" EQ "YES" &THEN
   OUTPUT STREAM test CLOSE.
&ENDIF

{nal_name.i}

RUN Insert_TTName("NoteBanks1", note-banks-str[1]). 
RUN Insert_TTName("NoteBanks2", note-banks-str[2]). 
RUN Insert_TTName("NoteBanks3", note-banks-str[3]). 
RUN Insert_TTName("NoteBanks4", note-banks-str[4]). 
RUN Insert_TTName("ins-DATE", STRING(op.ins-date,"99.99.9999")).
RUN Insert_TTName("SpisPl", mSpisPl).
RUN Insert_TTName("op-DATE", theDate).
/*RUN Insert_TTName("NameOrder
RUN Insert_TTName("NumberForm*/
RUN Insert_TTName("docnum", op.doc-num).
RUN Insert_TTName("PokST", mPokST).
RUN Insert_TTName("AmtStr", AmtStr[1] + " " + AmtStr[2] + " " + AmtStr[3]).
RUN Insert_TTName("Amt", TRIM(Rub)).
RUN DefDetail.
RUN Insert_TTName("detail", Detail[1]).
RUN Insert_TTName("PayType", PayType).
RUN Insert_TTName("CorrPl", DelFilFromAcct(PlCAcct)).
RUN Insert_TTName("AcctDb", DelFilFromAcct(PlLAcct)).
RUN Insert_TTName("MFOPl", PlMFO).

mStrTMP = TRIM(PlName[1] + " " + PlName[2] + " " + PlName[3] + " " + PlName[4] + " " + PlName[5]).

IF LENGTH(mStrTMP) GT 160 THEN
   RUN Insert_TTName("PlName_TeX_Font", CHR(1) + "~\footnotesize").
ELSE
   RUN Insert_TTName("PlName_TeX_Font", CHR(1) + "~\large").

RUN Insert_TTName("PlName", mStrTMP).

RUN Insert_TTName("BankPl", PlRKC[1] + " " + PlRKC[2]).
RUN Insert_TTName("AcctCr", DelFilFromAcct(PoAcct)).
/*RUN Insert_TTName("PoBankName*/
RUN Insert_TTName("CorrPo", DelFilFromAcct(PoCAcct)).
RUN Insert_TTName("MFOPo", PoMFO).

mStrTMP = TRIM(PoName[1] + " " + PoName[2] + " " + PoName[3] + " " + PoName[4] + " " + PoName[5]).

IF LENGTH(mStrTMP) GT 160 THEN
   RUN Insert_TTName("PoName_TeX_Font", CHR(1) + "~\footnotesize").
ELSE
   RUN Insert_TTName("PoName_TeX_Font", CHR(1) + "~\large").

RUN Insert_TTName("PoName", mStrTMP).

RUN Insert_TTName("BankPo", PoRKC[1] + " " + PoRKC[2]).
RUN Insert_TTName("INNPl", PlINN).
RUN Insert_TTName("KPPPl", PlKPP).
RUN Insert_TTName("INNPo", PoINN).
RUN Insert_TTName("KBK", mKBK).
RUN Insert_TTName("OKATO", mOKATO).
RUN Insert_TTName("PokOP", mPokOp).
RUN Insert_TTName("PokNP", mPokNP).
RUN Insert_TTName("PokND", mPokND).
RUN Insert_TTName("PokDD", mPokDD).
RUN Insert_TTName("PokTP", mPokTP).
RUN Insert_TTName("KPPPo", PoKPP).

IF AVAILABLE doc-type THEN
   RUN Insert_TTName("DocType", doc-type.digital).
ELSE
   RUN Insert_TTName("DocType", STRING(op.doc-type, "x(2)")).
/*RUN Insert_TTName("DueDate", STRING(op.due-date,"99.99.9999")).*/

IF SUBSTRING(op.order-pay,1,1) EQ "0" AND NOT CAN-DO("00,0,",op.order-pay) THEN
   RUN Insert_TTName("OrderPay", SUBSTRING(op.order-pay,2,1)).
ELSE IF SUBSTRING(op.order-pay,1,1) NE "0" AND NOT CAN-DO("00,0,",op.order-pay) THEN 
   RUN Insert_TTName("OrderPay", op.order-pay).

RUN Insert_TTName("PPDate", mPPDate).

   ASSIGN
     mUIN1   = SUBSTRING(GetXAttrValueEx("op",STRING(op.op),"УИН",""),1,7)
     mUIN2   = SUBSTRING(GetXAttrValueEx("op",STRING(op.op),"УИН",""),8,7)
     mUIN3   = SUBSTRING(GetXAttrValueEx("op",STRING(op.op),"УИН",""),15).
   IF mUIN1 EQ "" THEN
      IF mPokST NE "" THEN
         mUIN2 = "0". 
   IF mUIN1 EQ "0" THEN 
      ASSIGN
         mUIN1 = ""
         mUIN2 = "0" 
      .     

   RUN Insert_TTName("UINa",  mUin1).
   RUN Insert_TTName("UINb",  mUin2).
   RUN Insert_TTName("UINc",  mUin3).


/*-------------------------------------- Штамп, для СПБИ ---------------------------------------*/
/*                                   Потом здесь будет генерация красивого штампа               */
IF mIsUni3 THEN
DO:
   RUN Insert_TTName("BankNameS1", TRIM(BankName[1])).
   RUN Insert_TTName("BankNameS2", TRIM(BankName[2])).
   RUN Insert_TTName("BankNameS3", TRIM(BankName[3])).
   RUN Insert_TTName("BankAcct",   TRIM(BankAcct[1])).
   RUN Insert_TTName("op-dates",   STRING(OP.op-date, "99/99/9999")).

   RUN Insert_TTName("BankMFO1",   TRIM(BankMFO[1])).
   RUN Insert_TTName("BankMFO2",   TRIM(BankMFO[2])).
   RUN Insert_TTName("proved",     "ПРОВЕДЕНО").
   RUN Insert_TTName("Inspector",  TRIM(Inspector[1])).
END.
/*----------------------------------------------------------------------------------------------*/   

/*    Штамп    */

DEFINE VARIABLE mStampUser AS CHARACTER NO-UNDO.
DEFINE VARIABLE mStampStat AS CHARACTER NO-UNDO.

mStampUser = FGetSetting("StampUser", "", "").
mStampStat = GetCodeEx("StampCodes", "pp", "").

{stamp_o.i
   &StampDate    = "op.op-date"
   &StampUser    = "mStampUser"
   &Stamp        = "'PPStamp'"
   &StampStat    = "mStampStat"
   &StampObjType = "op"
}

IF op.op-date < DATE("31.03.2014") THEN  RUN printvd.p("pp-uni", INPUT TABLE ttnames).
ELSE RUN printvd.p("pp-uni107", INPUT TABLE ttnames).


