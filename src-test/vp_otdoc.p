/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2012 ЗАО "Банковские информационные системы"
     Filename: vp_otdoc.p
      Comment: классификатор "ВП_ОтборДок"
   Parameters:
         Uses:
      Used by:
      Created: 18.03.2013     
     Modified: 18.03.2013   
*/

DEFINE INPUT PARAMETER in-class  LIKE code.class NO-UNDO.
DEFINE INPUT PARAMETER in-parent LIKE code.code  NO-UNDO.
DEFINE INPUT PARAMETER in-title  LIKE code.name  NO-UNDO.

{vp_otdoc.def NEW}
{globals.i}

DEFINE VARIABLE valid    LIKE code.code                 NO-UNDO.
DEFINE VARIABLE old-code LIKE code.code                 NO-UNDO.
DEFINE VARIABLE ch       AS CHAR FORMAT "x(1)" INIT '>' NO-UNDO.

DEFINE BUFFER xxcode  FOR code.
DEFINE BUFFER bufcode FOR code.

ch = {submenu.chr}.

FORM
   code.code COLUMN-LABEL "КОД ПАРАМЕТРА" FORMAT "x(13)"
   code.name COLUMN-LABEL "НАИМЕНОВАНИЕ ПАРАМЕТРА1" VIEW-AS FILL-IN SIZE 60 BY 1
   ch        NO-LABEL
WITH FRAME browse1 TITLE COLOR bright-white "[ " + CAPS(in-title) + " ]".

FORM
   code.code
   code.name VIEW-AS FILL-IN SIZE 60 BY 1
   code.description[1] LABEL "Описание" VIEW-AS EDITOR SIZE 60 BY 5
WITH FRAME edit.

&glob oqry0 OPEN QUERY qry0 FOR EACH code WHERE code.class  EQ in-class                                            AND code.parent EQ in-parent NO-LOCK.

&GLOBAL-DEFINE tmprecid YES

{navigate.cqr
    &file     = code

    &qry      = "qry0"
    &maxoq    = 1
    &avfile   = "code"
    &defquery = "def query qry0 for code scrolling."

    &bf1      = "code.code code.name "
    &ef       = "vp_otdoc.uf "
    &edit     = "edit-ef.cqr "
    &oth1     = "vp_otdoc.mnu "
    &oh1      = "│F2 - Установка фильтра"
    &create   = "link2 
       &lfld  = class 
       &lfld2 = parent "
    &befupd   = "pclass2.bup "
    &postfind = "pclass2.fnd "
    &update   = "pclass.upd "
    &delete   = "pclass.del "
    &look     = "vp_otdoc.nav "
    &NOier    = "/*"
    &NOparent = "/*"
    &return   = "return.cqr &rfld=code "
    &oth7     = "find.cqr "
       &find1 = "seek.cqr  &sfld=code "
       &find2 = "match.cqr &sfld=name "
       &find3 = "match.cqr &sfld=val "
       

}



