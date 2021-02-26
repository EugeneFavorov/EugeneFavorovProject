/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2008 ЗАО "Банковские информационные системы"
     Filename: ORGNAME318P.I
      Comment: Печатает наименования организаций (318П)
   Parameters: CurBranchName - наименование ВСП
               Otstup        - длинна отступа от края
               OrgNameDef    - для повторного использования
               BranchExt     - на сколько строк делим наименования
               Примечания:
               1) i as INT64, тут определять не стал
               2) требует wordwrap.def
         Uses:
      Used by:
      Created: 29.07.2008 17:17 elus
     Modified: 29.07.2008 17:17 elus
*/

&IF DEFINED(BranchExt) EQ 0 &THEN
   &SCOPED-DEFINE BranchExt 3
&ENDIF

&IF DEFINED(OrgNameDef) EQ 0 &THEN   

   &SCOPED-DEFINE OrgNameDef YES
   
   DEFINE VARIABLE mBranchNameL LIKE branch.name NO-UNDO EXTENT {&BranchExt}. /* наименование ВСП */ 
   DEFINE VARIABLE mBranchNameM LIKE branch.name NO-UNDO EXTENT {&BranchExt}. /* наименование филиала */ 
   DEFINE VARIABLE mBranchNameH LIKE branch.name NO-UNDO EXTENT {&BranchExt}. /* Фирменное наименование кредитной организации */ 
   DEFINE VARIABLE mOtstup      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE mRasshifNaim AS LOGICAL   NO-UNDO.
   
   RUN GetBankNamesN(mCuBrchID, OUTPUT mBranchNameH[1], OUTPUT mBranchNameM[1], OUTPUT mBranchNameL[1]).
   mRasshifNaim = FGetSetting("РасшифНаим",?,"Да") EQ "Да".
   
&ENDIF

&IF DEFINED(Otstup) EQ 0 &THEN
   &SCOPED-DEFINE Otstup 0
&ENDIF

   mOtstup = FILL(" ",{&Otstup}).

&IF DEFINED(cols) NE 0 &THEN
   IF {&cols} > 82 THEN
   mOtstup = FILL(" ",INT64(({&cols} - 82) / 2)).
&ENDIF

/* Разбиваем длинные наименования и выводим в поток */

        {put-name.i &pref = "L" &*}
    if mBranchNameL[1] = '' then do:
        {put-name.i &pref = "M" &*}
    end.
    if mBranchNameL[1] = '' and mBranchNameM[1] = '' then do:
        {put-name.i &pref = "H" &*}
    end.

   PUT UNFORMATTED
     mOtstup FILL('─', 82) SKIP.

   IF mRasshifNaim THEN
   DO:
      PUT UNFORMATTED
        mOtstup " полное фирменное (сокращенное фирменное) наименование кредитной организации или  " SKIP
        mOtstup "  полное (сокращенное) наименование филиала, или наименование и (или) номер ВСП   " SKIP
        mOtstup "(при наличии) либо иные идентифицирующие признаки ВСП (при отсутствии наименования" SKIP
        mOtstup "   и номера) с указанием на его принадлежность кредитной организации (филиалу)    " SKIP(1).
   END.
/* $LINTUSER='STRE' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='14/11/2014 10:33:47.823+04:00' */
/* $LINTFILE='orgname318p.i' */
/*prosign2NSHmHJWGTqJMGWxFi4Bow*/