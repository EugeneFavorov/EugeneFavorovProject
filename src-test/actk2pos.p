/*
               KSV Editor
    Copyright: (C) 2000-2006 Serguey Klimoff (bulklodd)
     Filename: actk2pos.p
      Comment: отчет сверки остатков на внебалансовых счетах с остатками по аналитике, работает из браузера счетов по отмеченным  
   Parameters:
         Uses:
      Used by:
      Created: 
*/

{globals.i}
{tmprecid.def}
{sh-defs.i}

{setdest.i}

DEFINE VARIABLE mAmtKau   AS DECIMAL     NO-UNDO.
DEFINE VARIABLE mAmtAcct  AS DECIMAL     NO-UNDO.
DEFINE VARIABLE mAmtDiff  AS DECIMAL     NO-UNDO.
  
          
{ getdate.i }


PUT UNFORMATTED "Номер лицевого счета" format "x(25)" "    Остаток на счете" format "x(20)" "  Сумма на аналитике" format "x(20)" "         Расхождение" format "x(20)"  SKIP.
PUT UNFORMATTED fill("-", 85)  SKIP.

FOR EACH tmprecid,
    FIRST acct NO-LOCK 
    WHERE RECID(acct) EQ tmprecid.id:

    mAmtAcct = 0.
    RUN acct-pos IN h_base (acct.acct,
                            acct.currency,
                            end-date,
                            end-date,
                            "П").

    IF acct.side EQ "П" THEN
       mAmtAcct = IF trim(acct.currency) EQ ""
                    THEN - sh-bal
                    ELSE - sh-val.
    ELSE IF acct.side EQ "А" THEN
       mAmtAcct = IF trim(acct.currency) EQ ""
                    THEN sh-bal
                    ELSE sh-val.

    

   mAmtKau = 0.

   FOR EACH kau WHERE kau.acct EQ acct.acct AND
            kau.currency EQ acct.currency AND 
            kau.zero-bal EQ no NO-LOCK,
      FIRST op WHERE op.op EQ INT64(ENTRY(1, kau.kau)) NO-LOCK:

      mAmtKau = mAmtKau + kau.balance.

   END.

   mAmtDiff = mAmtAcct - mAmtKau.

   if mAmtDiff ne 0 then 
      PUT UNFORMATTED acct.number format "x(25)" string(mAmtAcct, "->>>>>>>>>>>>>>>9.99") string(mAmtKau, "->>>>>>>>>>>>>>>9.99") string(mAmtDiff, "->>>>>>>>>>>>>>>9.99") SKIP.

END.

{preview.i}
