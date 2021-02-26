/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2005 ЗАО "Банковские информационные системы"
     Filename: CODVLBRW.P
      Comment: Стандартный справочник значений классификаторов
   Parameters:
         Uses:
      Used by:
      Created: 22.12.2005 ILVI (50277)   
     Modified:
*/

{globals.i}             /* Глобальные переменные сессии. */
{flt-file.i}            /* Определение структуры динамического фильтра. */
{intrface.get xclass}   /* Библиотека инструментов метасхемы. */
{navigate.def}          /* Переменные для navigate.cqr. */

DEFINE VARIABLE in-class  AS CHARACTER NO-UNDO. /*  класс данных (код классификатора) */
DEFINE VARIABLE in-parent AS CHARACTER NO-UNDO. /*  родитель классификатора           */
DEFINE VARIABLE in-title  AS CHARACTER NO-UNDO. /*  заголовок справочника             */

DEFINE VARIABLE ch            AS CHARACTER NO-UNDO.
DEFINE VARIABLE vMaxFrm       AS INT64   NO-UNDO.
DEFINE VARIABLE vFirstFrm     AS INT64   NO-UNDO.
DEFINE VARIABLE vExclFrm      AS CHARACTER NO-UNDO.
DEFINE VARIABLE list-rec      AS CHARACTER NO-UNDO.
DEFINE VARIABLE list-mov      AS CHARACTER NO-UNDO. /* Список recid перемещаемых подпараметров */
DEFINE VARIABLE list-subparam AS CHARACTER NO-UNDO. /* Список кодов перемещаемых подпараметров */
DEFINE VARIABLE fl-level      AS LOGICAL   NO-UNDO. /* признак доступности перемещения */
DEFINE VARIABLE mClass        AS CHARACTER NO-UNDO. /* Класс */

DEFINE BUFFER bufcode FOR CODE.

IF GetFltVal("title") EQ "*" THEN DO:
   RUN SetFltFieldList("title",GetCodeName("",GetFltVal("class"))).
   RUN SetFltField    ("title",GetFltVal("title")).
END.

ASSIGN 
   mClass    = IF {assigned in-class}
                  THEN in-class 
                  ELSE GetFltVal("class")
   in-parent = GetFltVal("parent")
   in-class  = GetFltVal("class")
   in-title  = GetFltVal("title")
   mFrmLst   = IF mFrmLst EQ ""
               THEN "1,2"
               ELSE mFrmLst
   vMaxFrm   = NUM-ENTRIES (mFrmLst)
   vFirstFrm = IF GetFltVal("SetFirstFrm") EQ "0"
               THEN 1
               ELSE INT64(GetFltVal("SetFirstFrm"))
.

FIND FIRST class WHERE 
           class.Class-Code EQ mClass
NO-LOCK NO-ERROR.
IF NOT AVAIL class THEN
   mClass = "code".

IF  {assigned mClass}
AND NOT CAN-DO(GetXclassAllParents(mClass),"code")
THEN mClass = "".

IF NOT {assigned mClass}
THEN mClass = "code".

ch = {submenu.chr}.

{codvlbrw.frm}
{codvlbrw.qry
   &OQ = OpenQuery}

&GLOBAL-DEFINE tmprecid YES

{navigate.cqr
   &file          = code
   &filt          = YES
   &class         = mClass
   
   &access-class  = "'code'"
   &access-surr   = "code.class + ',' + code.code"
   &access-read   = r

   &maxfrm        = vMaxFrm
   &exclfrm       = vExclFrm
   &first-frm     = vFirstFrm
   &bf1           = "code.code code.name code.val "
   &bf2           = "code.code code.name code.val "
   &bf3           = "code.code code.name code.parent "

   &edit          = "bis-tty.ef "
                       &before-run-method = "codvlbrw.bfe "
   &postfind      = "pclass2.fnd "
   &delete        = "pclass.del "
   &look          = "bis-tty.nav "
   &NOier         = "/*"
   &NOparent      = "/*"

   &return        = "return.cqr 
                       &rfld = code "

   &oh3           = """ + (IF    NUM-ENTRIES (mFrmLst)   GT 1
                                 AND ENTRY (1, mFrmLst)  NE ENTRY (2, mFrmLst)
                              THEN ""│F3 форма""
                              ELSE """") + """
   &oth3          = "frames.cqr "
                       &user-frames_cqr = "RUN RELQ.  ~
                                           n-str = 1. ~
                                           IF n-frm = 1 THEN DO:~
                                              RUN SetFltField('parent',in-parent).~
                                              RUN SetFltFieldList('parent',in-parent).~
                                           END.~
                                           ELSE DO:~
                                              RUN SetFltField('parent','*').~
                                              RUN SetFltFieldList('parent','*').~
                                           END."
   
   &oh5           = """ + (if n-frm eq 1 then ""│F5 переместить"" else """") + """
   &oth5          = "codvlbrw.vi "
   
   &need-tmprecid = YES
   &local-recid   = YES
   &local-rest    = YES
   &local-keep    = YES
}

{intrface.del}          /* Выгрузка инструментария. */ 

{codvlbrw.qry
   &SQ = "PostSelect"}

PROCEDURE find-descendants. /* поиск потомков */
   DEFINE INPUT        PARAMETER iClass  AS CHARACTER NO-UNDO. /* класс параметра классификатора */
   DEFINE INPUT        PARAMETER iParent AS CHARACTER NO-UNDO. /* родитель параметра классификатора */
   DEFINE INPUT-OUTPUT PARAMETER oList   AS CHARACTER NO-UNDO.
   
   DEFINE BUFFER xc FOR CODE.
   
   FOR EACH xc WHERE
            xc.class  EQ iClass 
        AND xc.parent EQ iParent NO-LOCK:
      
      RUN find-descendants(xc.class,xc.code,INPUT-OUTPUT oList).
      {additem.i oList "'!' + xc.code"}       
   END.
END PROCEDURE.

