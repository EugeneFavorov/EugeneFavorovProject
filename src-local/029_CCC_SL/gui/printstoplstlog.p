/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2016 ЗАО "Банковские информационные системы"
     Filename: printstoplstlog.p
      Comment: Отчет "Проверка документов по стоп листам"
   Parameters: iOp oNoCont
         Uses:
      Used by:
      Created: 29.08.2013 
     Modified: 29.08.2013 
*/

{globals.i}     
{intrface.get xclass}
{wordwrap.def}
{intrface.get tmess}
{stoplist.fun}

DEFINE INPUT  PARAMETER iOp     AS INT64 NO-UNDO.
DEFINE OUTPUT PARAMETER oNoCont AS LOG   NO-UNDO.

DEFINE VARIABLE vSLType         AS CHAR          NO-UNDO.
DEFINE VARIABLE vNum            AS INT64         NO-UNDO.
DEFINE VARIABLE vI              AS INT64         NO-UNDO.
DEFINE VARIABLE vKritval        AS CHAR EXTENT 5 NO-UNDO.
DEFINE VARIABLE vSL             AS CHAR EXTENT 5 NO-UNDO.
DEFINE VARIABLE vRez            AS LOG           NO-UNDO.
DEFINE VARIABLE mFlag           AS LOG           NO-UNDO.

mFlag = yes.
IF GetSysConf ("IMPEXPSL") NE "YES" THEN DO:
   FIND FIRST op WHERE op.op EQ iOp NO-LOCK NO-ERROR.
   IF AVAIL op THEN
      vSLType = GetXattrValueEx("op-kind",op.op-kind,"КонтрВводРед_Док","").
   ELSE
      vSLType = "".
   IF vSLType EQ "Нет" THEN
      RETURN.
   IF vSLType EQ "Да" THEN 
      mFlag = no.
END.


vSLType = IF GetSysConf("IMPEXPSL") EQ "YES" THEN FGetSetting("Стоп-листы","ЭкспИмп_Док","")
                                             ELSE FGetSetting("Стоп-листы","КонтрВводРед_Док","").

IF mFlag AND ENTRY(1,vSLType, ";") EQ "Нет" THEN
   RETURN.

vSLType = ENTRY(2,vSLType, ";") NO-ERROR.   
vSLType =  IF  {assigned  TRIM(vSLType) } THEN  vSLType ELSE "*".
{empty tt-view-sl}
RUN ChkStopListOp(iOp, vSLType, OUTPUT vRez).
FIND FIRST tt-view-sl NO-ERROR.
IF AVAIL tt-view-sl THEN
DO:
   /* если вывод на экран отключен */
   IF GetProcSettingByCode("СС_ВыводНаЭкран") EQ "Нет" THEN
   /* вывод сообщения (в протокол) */
   RUN Fill-SysMes("","core3301","","").
   ELSE  DO: /* иначе запрос пользователю */
       RUN Fill-SysMes("","core59","","").
       IF (pick-value = "yes") THEN 
       DO:
          oNoCont = NO.
          RETURN.       
       END.
       ELSE
       DO:
          oNoCont = YES.
          {setdest.i &stream = "stream rep" &filename = "'chkstoplist.tmp'"  &cols = 170 &nodef   = "/*"}
          /*Вывод отчета*/
          FIND FIRST tt-view-sl NO-ERROR.
          IF AVAIL tt-view-sl THEN
          DO:
             /*Вывод отчета*/
          
             PUT STREAM rep UNFORMATTED " Документы с фигурантами справочника 'Стоп-листы'"  SKIP .
             PUT STREAM rep UNFORMATTED SKIP(1).
             PUT STREAM rep UNFORMATTED "┌──────┬────────────────┬──────────────┬──────────────────────────┬──────────────────────────────┬───────────┬──────────────────────────────┐" SKIP.
             PUT STREAM rep UNFORMATTED "│№ п/п │ Дата документа │ № документа  │   Критерий               │    Наименование критерия     │  № п/п из │          Примечание          │" SKIP.
             PUT STREAM rep UNFORMATTED "│      │                │              │                          │                              │ Стоп-листа│                              │" SKIP.
          
            FOR EACH tt-view-sl NO-LOCK 
                             BREAK BY tt-view-sl.fld3 
                                   BY tt-view-sl.fld2 
                                   BY tt-view-sl.fld1 
                                   BY tt-view-sl.fld7:
            IF       FIRST-OF(tt-view-sl.fld3)
               AND   FIRST-OF(tt-view-sl.fld2)
               AND   FIRST-OF(tt-view-sl.fld1)
               AND   FIRST-OF(tt-view-sl.fld7)
            THEN
            DO:
                ASSIGN
                   vNum        = vNum + 1
                   vKritval[1] =  tt-view-sl.fld4
                   vSL[1]      =  tt-view-sl.fld14.
          
          
          
                {wordwrap.i &s=vKritval  &n=5 &l=30 }
                {wordwrap.i &s=vSL    &n=5 &l=30 }
                PUT STREAM rep UNFORMATTED "├──────┼────────────────┼──────────────┼──────────────────────────┼──────────────────────────────┼───────────┼──────────────────────────────┤" SKIP.          
                PUT STREAM rep UNFORMATTED "│" string(vNum)      FORMAT "X(6)"  +     /*номер*/
                                "│" tt-view-sl.fld2     FORMAT "X(16)"  +     /*дата документа*/
                                "│" tt-view-sl.fld1     FORMAT "X(14)"  +     /*номер документа*/
                                "│" tt-view-sl.fld3     FORMAT "X(26)"  +     /*критерий*/
                                "│" vKritval[1]         FORMAT "X(30)"  +     /*значение критерия*/
                                "│" tt-view-sl.fld6     FORMAT "X(11)"  +     /*номер стоп лита*/
                                "│" vSL[1]              FORMAT "X(30)"  +     /*вид стоп листа*/
                                "│"
                SKIP. 
          
                DO vI = 2 TO 5:
                   IF   vKritval[vI] NE ""
                      OR vSL[vI]  NE ""
                   THEN
                      PUT STREAM rep UNFORMATTED "│" " "       FORMAT "X(6)"   + 
                                      "│" " "       FORMAT "X(16)"  + 
                                      "│" " "       FORMAT "X(14)"  + 
                                      "│" " "       FORMAT "X(26)"  + 
                                      "│" IF vKritval[vI]  NE "" THEN vKritval[vI] ELSE " " FORMAT "X(30)"  + 
                                      "│" " "       FORMAT "X(11)"  + 
                                      "│" IF vSL[vI]       NE "" THEN vSL[vI]      ELSE " " FORMAT "X(30)"  + 
                                      "│"
                      SKIP.
                END.
               END.
               
             END.
             PUT STREAM rep UNFORMATTED "└──────┴────────────────┴──────────────┴──────────────────────────┴──────────────────────────────┴───────────┴──────────────────────────────┘" SKIP.
          END.
          {preview.i  &stream = "stream rep" &filename = "'chkstoplist.tmp'"} 
       END.
   END.
END.
/* $LINTFILE='printstoplstlog.p' */
/* $LINTMODE='1,4,6,3' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='shoi' */
/* $LINTDATE='22/06/2016 18:38:25.615+03:00' */
/*prosignwGKnShFqTtGEbbMGLlrFCA*/