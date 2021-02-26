[DataClass 4.1d] // Экспорт данных произвел 0000BAV 15/12/2016 11:20:40 БД "-db /home2/bis/quit41d/db/bisquit,-ld bisquit,-L 200000,-n 500,-B 8000,-omsize 1400,-spin 10000,-U qbis,-P,-n 500"
no "REFERENCE" "" "СПРАВОЧНИКИ" 910 yes "Картотека справочников." no "" 1 "" "" "" "" "" "" "" "" "" "СПРАВОЧНИКИ ДЛЯ ЦЕННЫХ БУМАГ" 101
.
[DataClass 4.1d] // Экспорт данных произвел 0000BAV 15/12/2016 11:20:40 БД "-db /home2/bis/quit41d/db/bisquit,-ld bisquit,-L 200000,-n 500,-B 8000,-omsize 1400,-spin 10000,-U qbis,-P,-n 500"
no "Sec" "Reference" "СПРАВОЧНИКИ ДЛЯ ЦЕННЫХ БУМАГ" 101 yes "" no "*" 1 "" "" "" "" "" "" "" "OLTP,oltpb" "*" "СПРАВОЧНИКИ ДЛЯ ЦЕННЫХ БУМАГ" 101
.
[DataClass 4.1d] // Экспорт данных произвел 0000BAV 15/12/2016 11:20:40 БД "-db /home2/bis/quit41d/db/bisquit,-ld bisquit,-L 200000,-n 500,-B 8000,-omsize 1400,-spin 10000,-U qbis,-P,-n 500"
no "ОргРепо" "Sec" "Организации для сделок РЕПО" 20 no "" no "*" 1 "" "" "" "" "" "" "" "OLTP,oltpb" "*" "СПРАВОЧНИКИ ДЛЯ ЦЕННЫХ БУМАГ" 101
.
"OLAPDataBlock" "BISquit 4.1D124" "ОргРепо" "" "01/01/2016" "01/01/2016" ? "" 113706 "Организации" "" 61 " // Экспорт данных произвел 0000BAV 15/12/2016 11:20:40 БД '-db /home2/bis/quit41d/db/bisquit,-ld bisquit,-L 200000,-n 500,-B 8000,-omsize 1400,-spin 10000,-U qbis,-P,-n 500'"
"" "0000" "0000010207" "1" 0 0 0 0 0 0 0 0 0 0 0 0 ""
"0000010207" "" "0000010208" "2" 0 0 0 0 0 0 0 0 0 0 0 0 ""
"0000010207" "840" "0000010211" "2" 0 0 0 0 0 0 0 0 0 0 0 0 ""
"0000010207" "978" "0000010214" "2" 0 0 0 0 0 0 0 0 0 0 0 0 ""
"0000010208" "Б" "0000010209" "3" 0 0 0 0 0 0 0 0 0 0 0 0 ""
"0000010209" "                   139690" "0000010210" "4" 0 0 0 0 0 0 0 0 0 0 0 0 "30424810801300010001"
"0000010211" "Б" "0000010212" "3" 0 0 0 0 0 0 0 0 0 0 0 0 ""
"0000010212" "                   139690" "0000010213" "4" 0 0 0 0 0 0 0 0 0 0 0 0 "30424840300000000002"
"0000010214" "Б" "0000010215" "3" 0 0 0 0 0 0 0 0 0 0 0 0 ""
"0000010215" "                   139690" "0000010216" "4" 0 0 0 0 0 0 0 0 0 0 0 0 "30424978600000000001"
.
[IndicationAttr] // Экспорт данных произвел 0000BAV 15/12/2016 11:20:40 БД "-db /home2/bis/quit41d/db/bisquit,-ld bisquit,-L 200000,-n 500,-B 8000,-omsize 1400,-spin 10000,-U qbis,-P,-n 500"
"ОргРепо|113706" "filial-id" "Код филиала" "character" "x(8)" "ФИЛИАЛ" "Sum" "" "" "" "" "" "" "" "" "" "" "" "" "" "acct.filial-id" 1 no 2 "" 12/12/16 ?
"ОргРепо|113706" "currency" "Код валюты" "character" "xxx" "Код валюты" "Sum" "" "" "" "" "" "" "" "" "" "" "" "" "" "acct.currency" 2 no 2 "" 12/12/16 ?
"ОргРепо|113706" "cust-cat" "Тип клиента" "character" "X" "Тип клиента" "Sum" "" "" "" "" "" "" "" "" "" "" "" "" "" "acct.cust-cat" 3 no 2 "" 12/12/16 ?
"ОргРепо|113706" "cust-id" "Клиент N" "integer" "9999999" "Клиент N" "Sum" "" "" "" "" "" "" "" "" "" "" "" "" "" "acct.cust-id" 4 no 2 "" 12/12/16 ?
"ОргРепо|113706" "acct" "Лицевой счет" "character" "x(25)" "Лицевой счет" "Sum" "" "" "" "" "" "" "" "" "" "" "" "" "" "acct.acct" 5 yes 2 "" 12/12/16 ?
.
[Signs] // Экспорт данных произвел 0000BAV 15/12/2016 11:20:40 БД "-db /home2/bis/quit41d/db/bisquit,-ld bisquit,-L 200000,-n 500,-B 8000,-omsize 1400,-spin 10000,-U qbis,-P,-n 500"
"class-code" "Indication" "ОргРепо" "dataclass" "" ? ?
"История" "Да" "ОргРепо" "dataclass" "" ? ?
"module" "" "REFERENCE" "dataclass" "base" ? ?
"class-code" "Indication" "Sec" "dataclass" "" ? ?
"История" "Да" "Sec" "dataclass" "" ? ?
"module" "" "Sec" "dataclass" "base" ? ?
.
[Signs.IndicationAttr] // Экспорт данных произвел 0000BAV 15/12/2016 11:20:40 БД "-db /home2/bis/quit41d/db/bisquit,-ld bisquit,-L 200000,-n 500,-B 8000,-omsize 1400,-spin 10000,-U qbis,-P,-n 500"
"class-code" "" "ОргРепо|113706,filial-id,12/12/16" "DataAttr" "IndicationAttr" ? ?
"Маска" "0" "ОргРепо|113706,filial-id,12/12/16" "DataAttr" "" 0 ?
"ФильтрИнтервал" "" "ОргРепо|113706,filial-id,12/12/16" "DataAttr" "Да" ? ?
"class-code" "" "ОргРепо|113706,currency,12/12/16" "DataAttr" "IndicationAttr" ? ?
"Маска" "0" "ОргРепо|113706,currency,12/12/16" "DataAttr" "" 0 ?
"ФильтрИнтервал" "" "ОргРепо|113706,currency,12/12/16" "DataAttr" "Да" ? ?
"class-code" "" "ОргРепо|113706,cust-cat,12/12/16" "DataAttr" "IndicationAttr" ? ?
"Маска" "0" "ОргРепо|113706,cust-cat,12/12/16" "DataAttr" "" 0 ?
"ФильтрИнтервал" "" "ОргРепо|113706,cust-cat,12/12/16" "DataAttr" "Да" ? ?
"class-code" "" "ОргРепо|113706,cust-id,12/12/16" "DataAttr" "IndicationAttr" ? ?
"Маска" "0" "ОргРепо|113706,cust-id,12/12/16" "DataAttr" "" 0 ?
"ФильтрИнтервал" "" "ОргРепо|113706,cust-id,12/12/16" "DataAttr" "Да" ? ?
"class-code" "" "ОргРепо|113706,acct,12/12/16" "DataAttr" "IndicationAttr" ? ?
"Маска" "0" "ОргРепо|113706,acct,12/12/16" "DataAttr" "" 0 ?
"ФильтрИнтервал" "" "ОргРепо|113706,acct,12/12/16" "DataAttr" "Да" ? ?
.
[Signs.IndicationBlock] // Экспорт данных произвел 0000BAV 15/12/2016 11:20:40 БД "-db /home2/bis/quit41d/db/bisquit,-ld bisquit,-L 200000,-n 500,-B 8000,-omsize 1400,-spin 10000,-U qbis,-P,-n 500"
"class-code" "IndicationBlock" "113706" "DataBlock" "" ? ?
.
