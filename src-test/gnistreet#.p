/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2017 ЗАО "Банковские информационные системы"
     Filename: gnistreet#.p
      Comment: Просмотр списка GNIStreet
   Parameters: iParent,iTitle,iGniSym1
         Uses:
      Created: 10.06.2016 KOZV
*/
{intrface.get tmess}

DEF INPUT PARAMETER iParent  AS CHARACTER  NO-UNDO. /*  родитель классификатора           */
DEF INPUT PARAMETER iTitle   AS CHARACTER  NO-UNDO. /*  заголовок справочника             */
DEF INPUT PARAMETER iGniSym1 AS CHARACTER  NO-UNDO. /*  код ГНИ                           */

DEFINE TEMP-TABLE xDataLine
   FIELD Sym1 AS CHARACTER
   FIELD Sym2 AS CHARACTER LABEL "НАИМЕНОВАНИЕ"
   FIELD Sym3 AS CHARACTER 
   FIELD Sym4 AS CHARACTER LABEL "ТИП"
   FIELD Txt1 AS CHARACTER LABEL "ИНДЕКС"
   FIELD Txt2 AS CHARACTER
   INDEX Sym2 IS PRIMARY Sym2.

DEFINE VARIABLE mDataID         AS INT64     NO-UNDO.
DEFINE VARIABLE mSym2_srch      AS CHAR      NO-UNDO.
DEFINE VARIABLE mSym2_old_srch  AS CHAR      NO-UNDO.
DEFINE VARIABLE mFlagAll        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mHelp           AS CHAR      NO-UNDO.

/*============================================================================*/
FIND FIRST Code WHERE
           Code.class EQ ""
       AND Code.code  EQ iParent
           NO-LOCK NO-ERROR.

IF NOT AVAILABLE(Code) THEN RETURN.

ASSIGN mDataID = INT64(Code.misc[8]) NO-ERROR.

FIND FIRST DataBlock WHERE
           DataBlock.Data-ID EQ mDataID
           NO-LOCK NO-ERROR.

IF mDataID                EQ 0         OR
   mDataID                EQ ?         OR
   NOT AVAILABLE DataBlock             OR
   DataBlock.DataClass-ID NE iParent THEN DO:
   RUN Fill-SysMes IN h_tmess ("",
                               "",
                               "-1",
                               "Отсутствует справочник " + iTitle + " !" + "~n" +
                               "Обратитесь к Администратору!").   
   RETURN.
END.
/* SORT-ACCESS DataLine из-за вынужденной сортировки */
FOR EACH DataLine WHERE
         DataLine.Data-ID EQ mDataID   AND
         DataLine.Sym1 BEGINS SUBSTR(iGniSym1,1,11)
         NO-LOCK  BY DataLine.Data-ID BY DataLine.Sym2:

   CREATE xDataLine.
   ASSIGN
      xDataLine.Sym1 = DataLine.Sym1
      xDataLine.Sym2 = DataLine.Sym2
      xDataLine.Sym3 = DataLine.Sym3
      xDataLine.Sym4 = DataLine.Sym4
      xDataLine.Txt1 = ENTRY(1,DataLine.Txt,CHR(1))
      xDataLine.Txt2 = IF NUM-ENTRIES(DataLine.Txt,CHR(1)) GE 2 THEN ENTRY(2,DataLine.Txt,CHR(1))
                                                                ELSE ""
      .
END.

FORM
   xDataLine.Sym2 VIEW-AS FILL-IN size 50 BY 1 FORMAT "x(50)"
   xDataLine.Sym4 FORMAT "x(8)"
   xDataLine.Txt1 FORMAT "x(10)"
   WITH FRAME browse1
        TITLE "[ " + caps(iTitle) + " ]".


&GLOB defquery DEFINE QUERY qry0 FOR xDataLine scrolling.

&GLOB oqry0 OPEN QUERY qry0 FOR EACH xDataLine USE-INDEX Sym2 NO-LOCK.

&GLOB oqry1 OPEN QUERY qry0 FOR EACH xDataLine WHERE CAN-DO("*0",xDataLine.Sym1)~
            USE-INDEX Sym2 NO-LOCK.

&GLOB local-recid YES
&GLOB local-keep  YES
&GLOB local-rest  YES

FIND FIRST xDataLine NO-LOCK NO-ERROR.

IF NOT AVAIL xDataLine THEN
DO:
   RUN Fill-SysMes IN h_tmess ("",
                               "",
                               "1",
                               "ДАННЫХ НЕТ"). 
   RETURN.
END.
RELEASE xDataLine.

ASSIGN
   mFlagAll = yes
   mHelp    = "F1"
   .

{navigate.cqr
   &file    = "xDataLine"
   &maxoq   = 2
   &avfile  = "xDataLine"

   &qry     = "qry0"

   &return  = "gniret.cqr &rfld=Sym1 &rfld2=Sym2 &rfld3=Sym4 &rfld4=Txt2 &rfld5=Txt1 "
   &pickable = "yes"
   &look    = "gnistreet#.nav " 

   &maxfrm  = 1
   &bf1     = "xDataLine.Sym2 xDataLine.Sym4 xDataLine.Txt1 "
   &first-frm = 1

   &workfile = "/*"
   &nodel    = "/*"

   &help-label = mHelp

   &oth6     = "gni#.flt "

   &oth2       = "fastsrch.alt "
     &fs_field       = "Sym2"
     &current_srch   = mSym2_srch
     &old_srch       = mSym2_old_srch
     &fs_keys        ="(lookup(key-label(lastkey),'-, ,BACKSPACE') <> 0 OR
                       (LASTKEY GE 128 AND LASTKEY LE 175) OR
                       (LASTKEY GE 224 AND LASTKEY LE 239) OR
                       (LASTKEY GE 48  AND LASTKEY LE 57)) "

     &go_on = "'А','Б','В','Г','Д','Е','Ё','Ж','З','И','Й','К','Л','М','Н','О','П','Р','С','Т','У','Ф','Х','Ц','Ч','Ш','Щ','Ъ','Ы','Ь','Э','Ю','Я',               'а','б','в','г','д','е','ё','ж','з','и','й','к','л','м','н','о','п','р','с','т','у','ф','х','ц','ч','ш','щ','ъ','ы','ь','э','ю','я',               '0','1','2','3','4','5','6','7','8','9'"
   &find="gni.fnd "

}

PROCEDURE Select-Query:
  IF mFlagAll THEN n-oqry = 0.
  ELSE n-oqry = 1.
END PROCEDURE.

{intrface.del}
RETURN.
/* $LINTFILE='gnistreet#.p' */
/* $LINTMODE='1' */
/* $LINTENV ='2st' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='kozv' */
/* $LINTDATE='22/06/2016 13:05:26.867+04:00' */
/*prosignxs6V8iIq1KyAmm5t5mxiVg*/