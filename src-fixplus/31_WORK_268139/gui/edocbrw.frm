
/* +++ edocbrw.frm was humbly modified by (c)blodd converter v.1.09 on 10/16/2015 7:15am +++ */

FORM
   indocs.create-date
      COLUMN-LABEL "ДАТА!ДОБАВЛ."
      HELP "Дата добавления ЭД"
      FORMAT "99/99/9999"
   indocs.exp-date
      COLUMN-LABEL "СРОК!ХРАНЕНИЯ"
      HELP "Срок хранения ЭД"
      FORMAT "99/99/9999"
   mName
      FORMAT "x(38)"
      COLUMN-LABEL "НАИМЕНОВАНИЕ"
      HELP "Наименование документа"
      SPACE(0)
   mMark
      FORMAT ">/"
      COLUMN-LABEL " "
      HELP "Иерархия"  
      NO-TAB-STOP 
   indocs.doc-cat
      FORMAT "x(8)"
      COLUMN-LABEL "КОД!ОКУД"
      HELP "Код по ОКУД"
   indocs.doc-type
      COLUMN-LABEL "ТИП!ДОКУМ"
      FORMAT "x(5)"
      HELP "Тип документа"
WITH FRAME browse1 TITLE COLOR bright-white 
    "[ ЭЛЕКТРОННЫЕ ДОКУМЕНТЫ " + mFrmLabel + "]".

FORM
   indocs.create-date
      COLUMN-LABEL "ДАТА!ДОБАВЛ."
      HELP "Дата добавления ЭД"
      FORMAT "99/99/9999"
   indocs.exp-date
      COLUMN-LABEL "СРОК!ХРАНЕНИЯ"
      HELP "Срок хранения ЭД"
      FORMAT "99/99/9999"
   mName
      FORMAT "x(20)"
      COLUMN-LABEL "НАИМЕНОВАНИЕ"
      HELP "Наименование документа"
      SPACE(0)
   mMark
      FORMAT ">/"    
      COLUMN-LABEL " "
      HELP "Иерархия"  
      NO-TAB-STOP 
   indocs.doc-cat
      FORMAT "x(8)"
      COLUMN-LABEL "КОД!ОКУД"
      HELP "Код по ОКУД"
   indocs.doc-type
      COLUMN-LABEL "ТИП!ДОКУМ"
      FORMAT "x(5)"
      HELP "Тип документа"
   mClassname
      COLUMN-LABEL "ПРИВЯЗКА!ДОКУМ"
      FORMAT "x(17)"
      HELP "Привязка документа к таблице"      
WITH FRAME browse2 TITLE COLOR bright-white 
    "[ ЭЛЕКТРОННЫЕ ДОКУМЕНТЫ ]".
/*prosignbR6f617lnb701RDJNQPkfA*/
/* --- edocbrw.frm was humbly modified by (c)blodd converter v.1.09 on 10/16/2015 7:15am --- */
