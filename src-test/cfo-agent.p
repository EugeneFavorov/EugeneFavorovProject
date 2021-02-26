/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2006 ЗАО "Банковские информационные системы"
     Filename: BRDGOTLS.P
      Comment: Процедура форматированного ввода ДР CFO-AGENT
   Parameters:
         Uses:
      Used by:
      Created: 07.10.2014 KAU
     Modified: 
*/
{globals.i}             /* Глобальные переменные сессии. */
{intrface.get tmess}    /* Инструменты обработки сообщений. */
{client.i}              /*re_CLIENT*/

DEFINE INPUT PARAMETER iLevel AS INT64 NO-UNDO.

DEF VAR mTypeCl     AS CHAR NO-UNDO.
DEF VAR mNumCl      AS CHAR NO-UNDO.
DEF VAR mNameCl     AS CHAR NO-UNDO.

DEFINE VARIABLE mNum      AS INT64   NO-UNDO.

&SCOPED-DEFINE dateformat 99/99/9999

DEFINE FRAME edit
   mTypeCl
      LABEL "Тип клиента"
      HELP  "Код агента в базе БИС"
      FORMAT "x(1)" SKIP
      
   mNumCl
      LABEL "Код агента "
      HELP  "Код агента в базе БИС"
      FORMAT "x(20)" 
      VIEW-AS FILL-IN SIZE 30 BY 1       
    mNameCl
      LABEL "Имя агента "
      HELP  "ФИО агента, для организации Краткое наименование"
      FORMAT "x(20)"
      VIEW-AS FILL-IN SIZE 30 BY 1
WITH OVERLAY SIDE-LABELS CENTERED ROW iLevel TITLE "Значение реквизита ЦФО - Агент".

IF NUM-ENTRIES(pick-value) EQ 2 THEN
   DO mNum = 1 TO NUM-ENTRIES(pick-value):
      ASSIGN
        mTypeCl     = ENTRY(1,pick-value)
        mNumCl      = ENTRY(2,pick-value)
      NO-ERROR.
      RUN RE_CLIENT (mTypeCl,mNumCl,INPUT-OUTPUT mNameCl).
   END.



MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK WITH FRAME edit:


   ON 'F1':U OF mTypeCl
   DO:
       
        IF mTypeCl:SCREEN-VALUE EQ "Ч"
        THEN mTypeCl:SCREEN-VALUE = "Ю".
        ELSE mTypeCl:SCREEN-VALUE = "Ч".
        /*mTypeCl:SCREEN-VALUE = mTypeCl.*/
        RETURN NO-APPLY.
       /*
      RUN calend.p.
      IF (   LASTKEY EQ 10
          OR LASTKEY EQ 13)
      AND pick-value NE ?
      THEN 
         mDate:SCREEN-VALUE = pick-value.
      RETURN NO-APPLY.
      */
   END.
   
   ON 'F1':U OF mNumCl
   DO:
        IF mTypeCl:SCREEN-VALUE EQ "Ю" THEN
        DO TRANS:
           RUN browseld.p ("cust-corp","sc-1" + chr(1) + "sv-1","cfoagent" + chr(1) + "ДА",?,4).
        END.
        
        IF mTypeCl:SCREEN-VALUE EQ "Ч" THEN
        DO TRANS:
           RUN browseld.p ("person","sc-1" + chr(1) + "sv-1","cfoagent" + chr(1) + "ДА",?,4).
        END.
        
        IF {assigned pick-value} THEN
        DO:
           SELF:SCREEN-VALUE = pick-value.
           RUN RE_CLIENT (mTypeCl:SCREEN-VALUE,mNumCl:SCREEN-VALUE,INPUT-OUTPUT mNameCl).
           mNameCl:SCREEN-VALUE = mNameCl.
        END.
        RETURN NO-APPLY.
   END.

   ON 'ENTER':U OF mNumCl
   DO:
      APPLY "ENTRY" TO mTypeCl.
      RETURN NO-APPLY.
   END.
   
   ON 'leave':U OF mNumCl
   DO:
       DISABLE mNameCl WITH FRAME edit.
       /*
      IF mContCode:SCREEN-VALUE NE "" THEN
         IF INDEX(mContCode:SCREEN-VALUE,",") NE 0 
         OR INDEX(mContCode:SCREEN-VALUE,";") NE 0 THEN DO:
             RUN Fill-SysMes ("","","0","Поле 'Номер договора' не должно содержать символов ',' и ';'").
             RETURN NO-APPLY "ERROR".
         END.
         */
         
      RETURN.
   END.

   DISPLAY
      mTypeCl          
      mNumCl
      mNameCl
   WITH FRAME edit.
   ENABLE ALL WITH FRAME edit.
   DISABLE mNameCl WITH FRAME edit.
   
   WAIT-FOR GO,END-ERROR OF FRAME edit FOCUS mTypeCl.
   ASSIGN
      mTypeCl    
      mNumCl
      mNameCl
   .
   IF LAST-EVENT:FUNCTION EQ "GO" THEN
      pick-value = mTypeCl + ',' + mNumCl.

END.
HIDE FRAME edit.
