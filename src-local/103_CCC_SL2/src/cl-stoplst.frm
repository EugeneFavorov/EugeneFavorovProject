
FORM
   str-recid            
      COLUMN-LABEL "√"
      HELP         "√"
   &IF DEFINED(LOOK-STOPLIST) <> 0 &THEN
   tt-look-sl.matchstr   
      COLUMN-LABEL  "Искомое значение" 
      FORMAT "x(30)"
      HELP   "Искомое значение"   
   &ENDIF
   code.description[1]   
      COLUMN-LABEL  "№ п/п" 
      FORMAT "x(7)"
      HELP   "Номер"
   code.misc[1]   
      COLUMN-LABEL  "Тип" 
      FORMAT "x(1)"   
      HELP "Тип клиента"
   code.misc[2]   
      COLUMN-LABEL  "Код" 
      FORMAT "x(7)" 
      HELP "Внутренний номер для клиентов банка"
  code.misc[3]  
      COLUMN-LABEL  "ФИО~/ Наименование" 
      FORMAT "x(30)"
      HELP "ФИО/Наименование"
   code.misc[4]   
      COLUMN-LABEL  "Страна"
      FORMAT "x(3)"
      HELP "Символьный код страны"
   code.misc[5]   
      COLUMN-LABEL  "ИНН"
      FORMAT "x(12)"
      HELP "ИНН"
   code.misc[6]
      COLUMN-LABEL  "Паспорт" 
      FORMAT "x(10)"
      HELP "Серия и номер документа удостоверяющего личность"
   mBirthDay
      COLUMN-LABEL  "Дата рождения"
      FORMAT "99/99/9999"
      HELP "Дата рождения"
   mBirthPlace
      COLUMN-LABEL  "Место рождения"
      FORMAT "x(30)"
      HELP "Место рождения"
    code.misc[7]
      COLUMN-LABEL  "Примечание" 
      FORMAT "x(20)" 
      HELP "Примечание"
   code.name
      COLUMN-LABEL  "Стоп-лист" 
      FORMAT "x(20)"  
      HELP "Вид стоп-листа"
      VIEW-AS FILL-IN SIZE 20 BY 1
   code.val
      COLUMN-LABEL  "Номер резолюции" 
      FORMAT "x(20)"
      HELP "Номер резолюции"
   mDataRez
      COLUMN-LABEL  "Дата резолюции"
     FORMAT "99/99/9999"    
      HELP "Дата резолюции"
   mDataAdd
      COLUMN-LABEL  "Дата добавления"
      FORMAT "99/99/9999"
      HELP "Дата добавления субъекта в стоп-лист"
  mDataCls
      COLUMN-LABEL  "Дата закрытия"
      FORMAT "99/99/9999"
      HELP "Дата закрытия стоп-листа"
  code.misc[8]
      COLUMN-LABEL  "Пользователь"
      FORMAT "x(12)"    
      HELP "Пользователь, добавивший/изменивший запись в стоп-листе"

WITH FRAME browse1 WIDTH 270 TITLE COLOR bright-white "[  ]".

FORM
   code.description[1]     
      LABEL  "№ п/п" 
      FORMAT "x(7)" 
      HELP   "Номер"
      AT 12 SKIP
   code.misc[1]    
      LABEL  "Тип" 
      VIEW-AS COMBO-BOX LIST-ITEMS "Ч", "Ю", "Б"
      FORMAT "x(1)"  
      HELP "Тип клиента"
      AT 14 SKIP
   code.misc[2]   
      LABEL  "Код" 
      FORMAT "x(7)"  
      HELP "Код клиента"
      AT 14 SKIP
   code.misc[3]    
      LABEL  "ФИО~/Наименование" 
      FORMAT "x(4000)"
      VIEW-AS EDITOR SIZE 50 BY 2  
      HELP  "Наименование"
      SKIP
   code.misc[4]   
      LABEL  "Страна"
      FORMAT "x(3)" 
      HELP "Символьный код страны"
      AT 11 SKIP  
   code.misc[5]   
      LABEL  "ИНН"
      FORMAT "x(12)"   
      HELP "ИНН"
      AT  14   SKIP 
   code.misc[6]
      LABEL  "Паспорт" 
      FORMAT "x(50)"
      HELP "Серия и номер документа удостоверяющего личность"
      AT  10   SKIP
   mBirthDay
      LABEL  "Дата рождения"
      FORMAT "99/99/9999"    
      HELP "Дата рождения"
      AT 4
   mBirthPlace
      LABEL  "Место рождения" 
      FORMAT "x(150)" 
      VIEW-AS FILL-IN SIZE 50 BY 1
      HELP "Место рождения"
      AT 3
   code.misc[7] 
      LABEL  "Примечание" 
      FORMAT "x(4000)"
      VIEW-AS EDITOR SIZE 50 BY 2 
      HELP "Примечание"
      AT  7   SKIP
   code.name
      LABEL  "Стоп-лист"   
      FORMAT "x(20)"    
      HELP "Вид стоп-листа"
      AT  8   SKIP
   code.val
      LABEL  "Номер резолюции"  
      FORMAT "x(100)"
      VIEW-AS FILL-IN SIZE 50 BY 1
      HELP "Номер резолюции"  
      AT 2   SKIP
   mDataRez 
      LABEL  "Дата резолюции"  
      FORMAT "99/99/9999"  
      HELP  "Дата резолюции"  
      AT  3  SKIP
   mDataAdd  
      LABEL  "Дата добавления"    
      FORMAT "99/99/9999"
      HELP   "Дата добавления субъекта в стоп-лист"
      AT  2   SKIP
   mDataCls  
      LABEL  "Дата закрытия"    
      FORMAT "99/99/9999"
      HELP   "Дата закрытия стоп-листа"
      AT  4   SKIP      
   code.misc[8]
      LABEL  "Пользователь"   
      FORMAT "x(12)"    
      HELP "Пользователь, добавивший/изменивший запись в стоп-листе"
      AT  5   SKIP 
WITH FRAME edit SIDE-LABELS CENTERED TITLE COLOR BRIGHT-WHITE "[  ]" .
/* $LINTFILE='cl-stoplst.frm' */
/* $LINTMODE='11,-9,6,3' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='soav' */
/* $LINTDATE='07/03/2017 11:27:36.378+03:00' */
/*prosignhoi9vaXqjMwjpUYUiixjEw*/