/*

7.	Функциональные требования [Подробная постановка задачи (по шагам, с описанием алгоритма работы системы, требований к бух учету, интерфейсу и пр.)]
В связи с вступлением с 01.03.2018г. Инструкции Банка России 181- И  О порядке представления резидентами и нерезидентами уполномоченным банкам подтверждающих документов и информации при осуществлении валютных операций, о единых формах учета и отчетности по валютным операциям, порядке и сроках их представления, необходимо в ИБС Бисквит выполнить следующие доработки:
7.1.	Создать  отчет, который позволит выявлять операции клиентов, попадающих под определенные критерии, а именно в содержании операции должен быть указан код вида операции {VO}
7.2.	Запуск отчета выполнять с Клиента - процедура печати - выбор отчета - формат отчета Excel
7.3.	При запуске отчета должны быть проанализированы счета клиента:
- маски счета: 406*, 407*, 40802*.
- назначение счета: Расчет, Текущ, Транз1.
 в случае выявления нужных операций отразить их в отчете.
7.4.	Форма отчета: 
        Контроль сумм 200 000 рублей                                                 							

Код филиала/ГБ	Наименование Клиента ЮЛ/ИП	Счет Клиента	Дата платежа	Валюта платежа	Сумма в валюте перевода	Эквивалент в нац.валюте	Направление платежа:    1 - Зачисление,2 - Списание	Назначение платежа
0500/0300/0000	ООО Ромашка	40702-840-0-0000-0027356	01.03.18г.	Цифровое или буквенное обозначение валюты (840/USD)	1000.00		В случае если счет Клиента указан по Дт. счета направление платежа - 2, по Кт. счета- 1	Содержание операции с кодам {VO}

*/

{globals.i}
{tmprecid.def}          /** Используем информацию из броузера */

{sh-defs.i}
{pb_exf_exl.i}          /** Библиотека для выгрузки в XL */


DEFINE VARIABLE List-bal     AS CHARACTER NO-UNDO.    /* Список балансовых счетов */
DEFINE VARIABLE List-cont    AS CHARACTER NO-UNDO.    /* Список назначения счетов */
DEFINE VARIABLE cXL          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFl          AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAmt         AS decimal   NO-UNDO.


def temp-table op_  no-undo
    field op          as int
    field filial-id   as char
    field name-ben    as char
    field acct        as char
    field op-date     as date
    field curr        as char
    field amt-cur     as dec
    field amt-rub     as dec
    field direct      as char
    field details     as char
    index date op-date filial-id acct amt-rub
    .

list-bal  = "406*,407*,40802*".
list-cont = "Расчет,Текущ,Транз1".
{getdates.i}

FOR EACH tmprecid NO-LOCK, 
   FIRST cust-corp     WHERE RECID(cust-corp) EQ tmprecid.id NO-LOCK :

   for each acct where  acct.cust-cat EQ 'Ю'
                   and  acct.cust-id  EQ cust-corp.cust-id
                   and  acct.acct-cat EQ 'b'
                   and  CAN-DO(list-bal,string(acct.bal-acct))
                   NO-LOCK:
      if not CAN-DO(list-cont,acct.contract) then next.

      for each op-entry where  op-entry.acct-cr    = acct.acct
                          and  op-entry.op-date   >= beg-date
                          and  op-entry.op-date   <= end-date
                          and  op-entry.op-status >= chr(251)
                          no-lock.
         FIND FIRST op OF op-entry NO-LOCK NO-ERROR.
         if substr(op.details,2,2) <> "VO" then next.

         create op_.
         assign
            op_.op          = op-entry.op  
            op_.filial-id   = op-entry.filial-id
            op_.name-ben    = cust-corp.name-short
            op_.acct        = acct.number
            op_.op-date     = op-entry.op-date
            op_.curr        = if acct.curr = "" then "810" else acct.curr
            op_.amt-cur     = if acct.curr = "" then op-entry.amt-rub else op-entry.amt-cur 
            op_.amt-rub     = op-entry.amt-rub
            op_.direct      = "Зачисление"
            op_.details     = op.details
         .
      end.
      for each op-entry where  op-entry.acct-db    = acct.acct
                          and  op-entry.op-date   >= beg-date
                          and  op-entry.op-date   <= end-date
                          and  op-entry.op-status >= chr(251)
                          no-lock.
         FIND FIRST op OF op-entry NO-LOCK NO-ERROR.
         if substr(op.details,2,2) <> "VO" then next.

         create op_.
         assign
            op_.op          = op-entry.op  
            op_.filial-id   = op-entry.filial-id
            op_.name-ben    = cust-corp.name-short
            op_.acct        = acct.number
            op_.op-date     = op-entry.op-date
            op_.curr        = if acct.curr = "" then "810" else acct.curr
            op_.amt-cur     = if acct.curr = "" then op-entry.amt-rub else op-entry.amt-cur 
            op_.amt-rub     = op-entry.amt-rub
            op_.direct      = "Списание"
            op_.details     = op.details
         .
      end.
   end.
end.

output to "op.txt".
for each op_.
   export op_.
end.
output close.



cFl = "./sl_039.xml".
output to VALUE(cFl).


/*
Код филиала/ГБ	Наименование Клиента ЮЛ/ИП	Счет Клиента	Дата платежа	Валюта платежа	Сумма в валюте перевода	Эквивалент в нац.валюте	
Направление платежа:    1 - Зачисление,2 - Списание	Назначение платежа
0500/0300/0000	ООО Ромашка	40702-840-0-0000-0027356	01.03.18г.	Цифровое или буквенное обозначение валюты (840/USD)	1000.00		В случае если счет Клиента указан по Дт. счета направление платежа -2, по Кт. счета- 1	Содержание операции с кодам {VO}
*/               

PUT UNFORMATTED XLHead("tmp", "CCCCCNNCCC", "90,200,150,70,70,110,110,100,80,500").
cXL = XLCellHead("Код филиала/ГБ",0,0,0)
    + XLCellHead("Наименование Клиента ЮЛ/ИП",0,0,0)
    + XLCellHead("Счет Клиента",0,0,0)
    + XLCellHead("Дата платежа",0,0,0)
    + XLCellHead("Валюта платежа",0,0,0)
    + XLCellHead("Сумма в валюте перевода",0,0,0)
    + XLCellHead("Эквивалент в нац.валюте",0,0,0)
    + XLCellHead("Направление платежа",0,0,0)
    + XLCellHead("ИД документа",0,0,0)
    + XLCellHead("Назначение платежа",0,0,0)
    .
PUT UNFORMATTED XLRow(0) cXL XLRowEnd().

FOR EACH op_ where op_.amt-rub >= mAmt
   NO-LOCK.
   cXL = XLCell(string(op_.filial-id ))
       + XLCell(STRING(op_.name-ben  ))
       + XLCell(STRING(op_.acct      ))
       + XLCell(STRING(op_.op-date   ))  
       + XLCell(STRING(op_.curr      ))
       + XLNumCell    (op_.amt-cur   )
       + XLNumCell    (op_.amt-rub   )
       + XLCell(STRING(op_.direct    ))
       + XLCell(STRING(op_.op        ))
       + XLCellWrap(STRING(op_.details   ))
       .
   PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
END.


PUT UNFORMATTED XLEnd().
OUTPUT CLOSE.

/*
    RUN mail-add.p ("FM-Report").
    RUN pb_mail.p ("v.ignatchenko", "FM Report", "FM Report", cFl).
    RUN pb_mail.p (RETURN-VALUE, "FM Report", "FM Report", cFl).
*/

  
    RUN sndbispc ("file=" + cFl + ";class=bq").



