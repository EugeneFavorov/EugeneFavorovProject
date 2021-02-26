{globals.i}
{intrface.get tmess}

/* +++ pt-uni_ops.p was humbly modified by (c)blodd converter v.1.09 on 5/30/2016 7:28am +++ */

/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2009 ЗАО "Банковские информационные системы"
     Filename: PT-UNI_OPS.P
      Comment: Универсальная процедура печати платежных требований
               (Открытая Система Печати)
      Comment:
   Parameters: Input: RID -  RecID(op)
         Uses:
      Used by:
      Created: 03/02/2010 kraw (0114875)
     Modified: 02/03/2011 kraw (0143231) Вид платежа больше не заполняется.
     Modified: 19/04/2011 kraw (0144455) Факсимиле штампа
*/
{globals.i}                                 /* глобальные переменные         */
{chkacces.i}
{prn-doc.def &with_proc=YES}
{parsin.def}

&GLOB tt-op-entry yes
&GLOBAL-DEFINE OFFSIGNS YES
&IF defined(NEW_1256) NE 0 &THEN
   &SCOP NFORM    611
&ELSE
   &SCOP NFORM    61
&ENDIF
{intrface.get xclass}
&SCOP TEST YES
&IF "{&TEST}" EQ "YES" &THEN
   DEF STREAM test.
   OUTPUT STREAM test TO "test.pp".
&ENDIF

{pp-uni.var}                                /* определение переменных        */
{pt-uni.var}

{pp-uni.err}                                /* сообщения об ошибках          */

{pp-uni.prg}                     /* описание стандартных процедур */
{pp-uni.pro}
{pt-uni.prg}

{pp-uni.chk &allcur=YES &multy-op-ontry=YES}                                /* проверка входных данных       */

{pp-uni.run}                                /* непосредственно расчет        */
{pt-uni.run}

&IF "{&TEST}" EQ "YES" &THEN
   OUTPUT STREAM test CLOSE.
&ENDIF
PROCEDURE GetHeader:
   RUN DefHeader.
   ASSIGN
      NameOrder  = "ПЛАТЕЖНОЕ ТРЕБОВАНИЕ N"
      NumberForm = "0401061"
   .

END PROCEDURE.

{nal_name.i}

/* ************************************* */

   DEFINE VAR lzr-Cond-Pay       AS CHARACTER NO-UNDO.
   DEFINE VAR lzr-Amt-Rub        AS CHARACTER NO-UNDO.
   DEFINE VAR lzr-Plat-Name      AS CHARACTER NO-UNDO.
   DEFINE VAR lzr-Plat-Bank      AS CHARACTER NO-UNDO.
   DEFINE VAR lzr-Pol-Name       AS CHARACTER NO-UNDO.
   DEFINE VAR lzr-Pol-Bank       AS CHARACTER NO-UNDO.
   DEFINE VAR lzr-Details        AS CHARACTER NO-UNDO.

   DEFINE VARIABLE vIsLongPolName AS LOGICAL NO-UNDO.
   DEFINE VARIABLE vIsLongPlaName AS LOGICAL NO-UNDO.

   DEFINE VARIABLE mStrTMP      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE mFontSizeStr AS CHARACTER NO-UNDO.

   ASSIGN
      lzr-Cond-Pay  = TRIM(Cond-Pay[1]) + " " +
                      TRIM(Cond-Pay[2]) + " " +
                      TRIM(Cond-Pay[3]).
      lzr-Amt-Rub   = TRIM(AmtStr[1]) + " " +
                      TRIM(AmtStr[2]) + " " +
                      TRIM(AmtStr[3]).
      lzr-Plat-Name = TRIM(PlName[1]) + " " +
                      TRIM(PlName[2]) + " " +
                      TRIM(PlName[3]) + " " +
                      TRIM(PlName[4]) + " " +
                      TRIM(PlName[5]).
      lzr-Plat-Bank = TRIM(PlRKC[1]) + " " +
                      TRIM(PlRKC[2]).
      lzr-Pol-Bank  = TRIM(PoRKC[1]) + " " +
                      TRIM(PoRKC[2]).
      lzr-Pol-Name  = TRIM(PoName[1]) + " " +
                      TRIM(PoName[2]) + " " +
                      TRIM(PoName[3]) + " " +
                      TRIM(PoName[4]) + " " +
                      TRIM(PoName[5]).
      lzr-Details   =   op.details.
      lzr-Plat-Name = TRIM(lzr-Plat-Name).
      lzr-Pol-Name  = TRIM(lzr-Pol-Name ).

   IF lzr-Pol-Name BEGINS "ИНН " THEN
      lzr-Pol-Name = SUBSTRING(lzr-Pol-Name,4).

   IF lzr-Plat-Name BEGINS "ИНН " THEN
      lzr-Plat-Name = SUBSTRING(lzr-Plat-Name,4).

/* ************************************* */

   RUN GetCustFontSize(lzr-Plat-Name, OUTPUT mFontSizeStr).
   IF NOT {assigned mFontSizeStr} THEN
      mFontSizeStr = "large".
   RUN Insert_TTName("PlName_TeX_Font", CHR(1) + "~\" + mFontSizeStr).

   RUN GetCustFontSize(lzr-Pol-Name, OUTPUT mFontSizeStr).
   IF NOT {assigned mFontSizeStr} THEN
      mFontSizeStr = "large".
   RUN Insert_TTName("PoName_TeX_Font", CHR(1) + "~\" + mFontSizeStr).

   RUN Insert_TTName("ins-DATE", IF    op.ins-date EQ ?
                                    OR PlMFO       NE bank-mfo-9 THEN ""
                                                                 ELSE STRING(op.ins-date,"99.99.9999")).

   RUN Insert_TTName("SpisPl", mSpisPl).

   RUN Insert_TTName("UslOpl", IF GetXAttrValueEx( "op",
                                                   STRING(op.op),
                                                   'УслОпл',
                                                   "--") BEGINS 'С акцеп' THEN STRING(op.Due-Date,"99.99.9999")
                                                                          ELSE "").

   RUN Insert_TTName("docnum", op.doc-num).

   RUN Insert_TTName("op-DATE", theDate).

   RUN Insert_TTName("PayType", PayType).

   RUN Insert_TTName("CondPay", lzr-Cond-Pay).

   RUN Insert_TTName("NumDay", Num-Day).

   RUN Insert_TTName("AmtStr", lzr-Amt-Rub).

   RUN Insert_TTName("PlName", lzr-Plat-Name).

   RUN Insert_TTName("Amt", TRIM(Rub)).

   RUN Insert_TTName("AcctDb", DelFilFromAcct(PlLAcct)).

   RUN Insert_TTName("BankPl", lzr-Plat-Bank).

   RUN Insert_TTName("MFOPl", PlMFO).

   RUN Insert_TTName("CorrPl", DelFilFromAcct(PlCAcct)).

   RUN Insert_TTName("BankPo", lzr-Pol-Bank).

   RUN Insert_TTName("MFOPo", PoMFO).

   RUN Insert_TTName("CorrPo", DelFilFromAcct(PoCAcct)).

   RUN Insert_TTName("AcctCr", DelFilFromAcct(PoAcct)).

   RUN Insert_TTName("PoName", lzr-Pol-Name).

   IF AVAILABLE doc-type THEN
      RUN Insert_TTName("DocType", doc-type.digital).
   ELSE
      RUN Insert_TTName("DocType", STRING(op.doc-type, "x(2)")).

   IF INT64(op.order-pay) GT 0 THEN
   DO:

      IF SUBSTRING(op.order-pay,1,1) EQ "0" THEN
         RUN Insert_TTName("OrderPay", SUBSTRING(op.order-pay,2,1)).
      ELSE
         RUN Insert_TTName("OrderPay", op.order-pay).
   END.

   RUN Insert_TTName("detail", lzr-Details ).
   RUN Insert_TTName("DataMarkDBank", mDataMarkDBank ).
   RUN Insert_TTName("DataCartIn",    mDataCartIn).

   RUN Insert_TTName("UIN1", mUIN1).
   RUN Insert_TTName("UIN2", mUIN2).
   RUN Insert_TTName("UIN3", mUIN3).
   RUN Insert_TTName("Electron", GetXAttrValue( "op", STRING(op.op), 'СпособПолуч')).

/* ************************************* */

/*    Штамп    */

DEFINE VARIABLE mStampUser AS CHARACTER NO-UNDO.
DEFINE VARIABLE mStampStat AS CHARACTER NO-UNDO.
DEFINE VARIABLE mStampTmpl AS CHARACTER NO-UNDO.

mStampUser = FGetSetting("StampUser", "", "").
mStampStat = GetCodeEx("StampCodes", "pp", "").
mStampTmpl = GetSysConf("StampTmpl").

IF NOT {assigned mStampTmpl} THEN 
   mStampTmpl = "PPStamp".


{stamp_o.i
   &StampDate    = "op.op-date"
   &StampUser    = "mStampUser"
   &Stamp        = "mStampTmpl"
   &StampStat    = "mStampStat"
   &StampObjType = "op"
}

RUN printvd.p(IF op.op-date ge date("31.03.14") OR 
                (op.op-date EQ ? AND op.doc-date GE DATE("31.03.14")) THEN  "pt-uni_new" ELSE "pt-uni", 
              INPUT TABLE ttnames).

/* --- pt-uni_ops.p was humbly modified by (c)blodd converter v.1.09 on 5/30/2016 7:28am --- */
