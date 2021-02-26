/*
               KSV Editor
    Copyright: (C) 2000-2009 Serguey Klimoff (bulklodd)
     Filename: LRCOND-JOI.P
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
      Created: 01.10.2009 MUTA 0080978 Тарифные планы
     Modified: 
*/

{joinpar.i}

DEFINE VARIABLE vTitle AS CHARACTER NO-UNDO.
DEFINE VARIABLE vRate  AS CHARACTER NO-UNDO.
DEFINE VARIABLE vCond  AS CHARACTER NO-UNDO.
DEFINE BUFFER loan-cond     FOR loan-cond.

FIND loan-cond WHERE ROWID(loan-cond) EQ TO-ROWID(iROWID) 
   NO-LOCK NO-ERROR.

IF NOT AVAIL loan-cond THEN
   RETURN.

vRate =  GetXattrValue("loan-cond", loan-cond.contract + "," + loan-cond.cont-code + "," + STRING(loan-cond.since),"ТарифПлан").
vCond =  GetXAttrValue("loan-cond", loan-cond.contract + "," + loan-cond.cont-code + "," + STRING(loan-cond.since),"НомерСогл").   

RUN CreateJoinLd ("Комиссии и тарифы", 
                  "browseld",
                  "comm-rate",
                   "contract" + CHR(1) +  "kau" + CHR(1) + "SetFirstFrm" + CHR(1) + "Parent", 
                   "base"     + CHR(1) +  "ТарифыРКО;"  + vRate + CHR(1) + "14" + CHR(1) + "comm",
                  "contract",
                  STRING (Level + 1),
                  YES).

RUN CreateJoin("Дополнительные реквизиты",
               "ccond-sign`"
               + loan-cond.contract + ","
               + loan-cond.cont-code + ","
               + STRING(loan-cond.since) + ","
               + STRING(Level + 1),
               YES ).

RUN CreateJoinLd(
   "Электронные документы",
   "browseld",
   "eDocument",
   "class-code" + CHR(1) + "contract" + CHR(1) + "file-name" + CHR(1) + "surrogate",
   "eDocument"  + CHR(1) + "ЭД"        + CHR(1) + "loan"      + CHR(1) + loan-cond.contract + ";" + loan-cond.cont-code + ";" + vCond,
   "class-code" + CHR(1) + "contract" + CHR(1) + "file-name" + CHR(1) + "surrogate",
   STRING (level + 1),
   YES
).

       
vTitle = "[ УСЛОВИЯ  qqqq ДОГОВОРА " + STRING(loan-cond.cont-code) + " ]". 

{procjoin.i
    &prefix = loan-cond
    &frametitle = vTitle
}

