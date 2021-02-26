/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2016 ЗАО "Банковские информационные системы"
     Filename: chksgnnaldoc.p
      Comment: Процедура проверки реквизитов налоговых реквизитов документа
   Parameters: INPUT налоговые реквизиты, oOk - результат проверки
         Uses:
      Used by:
      Created: 
     Modified:
*/
{globals.i}
{intrface.get xclass}
{intrface.get date}
{intrface.get tmess}
{intrface.get op}

 DEFINE INPUT  PARAMETER iPokST AS CHARACTER   NO-UNDO.
 DEFINE INPUT  PARAMETER iKBK   AS CHARACTER   NO-UNDO.
 DEFINE INPUT  PARAMETER iPokOP AS CHARACTER   NO-UNDO.
 DEFINE INPUT  PARAMETER iPokNP AS CHARACTER   NO-UNDO.
 DEFINE INPUT  PARAMETER iPokND AS CHARACTER   NO-UNDO.
 DEFINE INPUT  PARAMETER iPokDD AS CHARACTER   NO-UNDO.
 DEFINE INPUT  PARAMETER iBenAcct AS CHARACTER NO-UNDO.
 DEFINE OUTPUT PARAMETER oOk AS LOGICAL NO-UNDO INIT YES.

{148n.i}

 DEFINE VARIABLE mKBKNalog  AS CHARACTER NO-UNDO.
 DEFINE VARIABLE mKBKCustom AS CHARACTER NO-UNDO.
 DEFINE VARIABLE mDescPokOP AS CHARACTER NO-UNDO.
 DEFINE VARIABLE mErrorMsg  AS CHARACTER NO-UNDO.

/*
   Если ПокСт (101) не заполнено или содержит "0",
   либо при отключении контроля посредством НП ГНИ.ДопКонтр148,
   никакие проверки не производятся
*/
IF NOT isAssigned148n(iPokST) OR
   FGetSetting("ГНИ", "ДопКонтр148", "Да") = "Нет"
THEN DO:
   oOK = NO.
   {intrface.del}
   RETURN.
END.

 ASSIGN
  mKBKNalog  = FGetSetting("ГНИ","КБКНалог","")
  mKBKCustom = FGetSetting("ГНИ","КБКТамож","")
  mDescPokOP = GetCodeDesc("Нал:ОП",iPokOP,1,"").

  CREATE ttNalRec.
  ASSIGN
     ttNalRec.ben-acct = iBenAcct
     ttNalRec.KBK      = iKBK
     ttNalRec.PokOP    = iPokOP
     ttNalRec.PokNP    = iPokNP.

  IF iKBK NE "0" AND
     (iPokST EQ "0" OR
      iPokST EQ "00")
     THEN DO:
        RUN Fill-SysMes IN h_tmess ("","","-1",
              "Не пройден перекрестный контроль полей (101) Статус плательщика и (104) КБК").
        RETURN.
     END.

    IF mDescPokOP NE "" AND 
     ((CAN-DO(mKBKNalog,iKBK)  AND NOT CAN-DO(mDescPokOP,"н")) OR
      (CAN-DO(mKBKCustom,iKBK) AND NOT CAN-DO(mDescPokOP,"т")))       
/*
 Пока отключаем.
 Из налоговой приходят некорректные платежи, который надо принять.
*/    
  /* OR
     ( NOT CAN-DO(mKBKNalog,iKBK)  AND
       NOT CAN-DO(mKBKCustom,iKBK) AND
       iPokOP     NE "0"))
       */
       THEN DO:
          RUN Fill-SysMes IN h_tmess ("","","-1",
                   "Не пройден перекрестный контроль полей (104) КБК и (106) Основание платежа").
          RETURN.
       END.



  /* Весь перекрестный контроль (106) и (107) полей */
  RUN check148n-106-107 IN THIS-PROCEDURE (TABLE ttNalRec BY-REFERENCE,
                                           OUTPUT mErrorMsg).
  IF {assigned mErrorMsg} THEN DO:
      RUN Fill-SysMes IN h_tmess ("", "", "-1", mErrorMsg).
      RETURN.
  END.
         /* Требуется уточнение !!! */
/*
  IF iPokOP = "ТП" AND
     (iPokND <> "0" OR Str2Date(REPLACE(iPokDD,".","/")) = ?)
  THEN DO:
     RUN Fill-SysMes IN h_tmess ("",
                                 "",
                                 "-1",
                                 "Не пройден перекрестный контроль полей " +
                                 "(108) Номер документа и (109) Дата документа").
     RETURN.
  END.
*/

  IF CAN-DO(FGetSetting("ГНИ", "Осн106ПокНД0", ""), iPokOP) AND
     iPokND <> "0"
  THEN DO:
     RUN Fill-SysMes IN h_tmess ("",
                                 "",
                                 "-1",
                                 "Не пройден перекрестный контроль полей " +
                                 "(106) Основание платежа и (108) Номер документа").
     RETURN.
  END.

/*
Исправлено в банке:
Не проходил случай, когда

Основание (106): ТП
    Номер (108): 0
     Дата (109): 11.11.2016

*/

         /* Требуется уточнение !!! */
/*
  IF ((iPokND NE "0"          AND
      INDEX(iPokND,";") EQ 0 AND
      Str2Date(REPLACE(iPokDD,".","/")) EQ ?)  OR
     ((iPokND EQ "0"          OR
       INDEX(iPokND,";") > 0) AND
      iPokDD NE "0"))
   AND NOT (iPokOP = "ТП" AND
         iPokND = "0" AND Str2Date(REPLACE(iPokDD,".","/")) <> ?)
   THEN DO:
       RUN Fill-SysMes IN h_tmess ("","","-1",
            "Не пройден перекрестный контроль полей (108) Номер документа и (109) Дата документа").
       RETURN.
  END.
*/

  oOk = NO.
{intrface.del}
/* $LINTENV ='dpl' */
/* $LINTVSS ='$/ws3-dpl/bq' */
/* $LINTDATE='31/03/2016 18:49:01.709+04:00' */
/* $LINTUSER='krok' */
/* $LINTMODE='1' */
/* $LINTFILE='chksgnnaldoc.p' */
/*prosignWjIcctXO+yNyMgIwZdiGzg*/