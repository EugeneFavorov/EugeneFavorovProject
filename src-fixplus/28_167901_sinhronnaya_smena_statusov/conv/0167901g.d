[DataClass 4.1d] // Экспорт данных произвел GSBIS 08/09/2015 10:06:47 БД "-db /home2/bis/quit41d/db-spb/bisquit,-ld bisquit,-Mm 16384,-spin 5000"
no "REFERENCE" "" "СПРАВОЧНИКИ" 910 yes "Картотека справочников." no "" 1 "" "" "" "" "" "" "" "" "" "СПРАВОЧНИКИ , ИСПОЛЬЗУЕМЫЕ ВО ВСЕХ МОДУЛЯХ" 1
.
[DataClass 4.1d] // Экспорт данных произвел GSBIS 08/09/2015 10:06:47 БД "-db /home2/bis/quit41d/db-spb/bisquit,-ld bisquit,-Mm 16384,-spin 5000"
no "ВсеМодули" "Reference" "СПРАВОЧНИКИ , ИСПОЛЬЗУЕМЫЕ ВО ВСЕХ МОДУЛЯХ" 1 yes "" no "*" 1 "" "" "" "" "" "" "" "OLTP" "*" "СПРАВОЧНИКИ , ИСПОЛЬЗУЕМЫЕ ВО ВСЕХ МОДУЛЯХ" 1
.
[DataClass 4.1d] // Экспорт данных произвел GSBIS 08/09/2015 10:06:47 БД "-db /home2/bis/quit41d/db-spb/bisquit,-ld bisquit,-Mm 16384,-spin 5000"
no "СинхСтат" "ВсеМодули" "Синхронная смена статусов" 999 no "TT:0167901
" no "*" 1 "" "" "" "" "" "" "" "OLTP,oltpb" "*" "СПРАВОЧНИКИ , ИСПОЛЬЗУЕМЫЕ ВО ВСЕХ МОДУЛЯХ" 1
.
"OLAPDataBlock" "BISquit 4.1D116" "СинхСтат" "" "01/01/2015" "01/01/2015" ? "" 103118 "Взаимосвязь статусов" "" 61 " // Экспорт данных произвел GSBIS 08/09/2015 10:06:47 БД '-db /home2/bis/quit41d/db-spb/bisquit,-ld bisquit,-Mm 16384,-spin 5000'"
"" "*" "0000002853" "1" 0 0 0 0 0 0 0 0 0 0 0 0 ""
"0000002853" "В" "0000002854" "2" 0 0 0 0 0 0 0 0 0 0 0 0 "В"
"0000002853" "√" "0000002859" "2" 0 0 0 0 0 0 0 0 0 0 0 0 "√"
"0000002853" "√√" "0000002860" "2" 0 0 0 0 0 0 0 0 0 0 0 0 "√"
.
[IndicationAttr] // Экспорт данных произвел GSBIS 08/09/2015 10:06:47 БД "-db /home2/bis/quit41d/db-spb/bisquit,-ld bisquit,-Mm 16384,-spin 5000"
"СинхСтат|103118" "com-status" "Конечный статус комиссионного документа" "CHARACTER" "x(8)" "" "Sum" "" "" "" "" "" "" "" "" "" "" "" "" "" "com-status" 3 yes 2 "" 31/08/15 ?
"СинхСтат|103118" "op-kind" "Код транзакции" "CHARACTER" "x(25)" "" "Sum" "" "" "" "" "" "" "" "" "" "" "" "" "" "op-kind" 1 no 2 "" 31/08/15 ?
"СинхСтат|103118" "status" "Конечный статус основного документа" "CHARACTER" "x(8)" "" "Sum" "" "" "" "" "" "" "" "" "" "" "" "" "" "status" 2 no 2 "" 31/08/15 ?
.
[Signs] // Экспорт данных произвел GSBIS 08/09/2015 10:06:47 БД "-db /home2/bis/quit41d/db-spb/bisquit,-ld bisquit,-Mm 16384,-spin 5000"
"История" "Да" "ВсеМодули" "dataclass" "" ? ?
"class-code" "Indication" "ВсеМодули" "dataclass" "" ? ?
"module" "" "ВсеМодули" "dataclass" "base" ? ?
"module" "" "REFERENCE" "dataclass" "base" ? ?
"class-code" "Indication" "СинхСтат" "dataclass" "" ? ?
"История" "Да" "СинхСтат" "dataclass" "" ? ?
.
[Signs.IndicationAttr] // Экспорт данных произвел GSBIS 08/09/2015 10:06:47 БД "-db /home2/bis/quit41d/db-spb/bisquit,-ld bisquit,-Mm 16384,-spin 5000"
"class-code" "" "СинхСтат|103118,com-status,31/08/15" "DataAttr" "IndicationAttr" ? ?
"Маска" "0" "СинхСтат|103118,com-status,31/08/15" "DataAttr" "" 0 ?
"ФильтрИнтервал" "" "СинхСтат|103118,com-status,31/08/15" "DataAttr" "Да" ? ?
"class-code" "" "СинхСтат|103118,op-kind,31/08/15" "DataAttr" "IndicationAttr" ? ?
"Маска" "1" "СинхСтат|103118,op-kind,31/08/15" "DataAttr" "" 1 ?
"ФильтрИнтервал" "" "СинхСтат|103118,op-kind,31/08/15" "DataAttr" "Да" ? ?
"class-code" "" "СинхСтат|103118,status,31/08/15" "DataAttr" "IndicationAttr" ? ?
"Маска" "0" "СинхСтат|103118,status,31/08/15" "DataAttr" "" 0 ?
"ФильтрИнтервал" "" "СинхСтат|103118,status,31/08/15" "DataAttr" "Да" ? ?
.
[Signs.IndicationBlock] // Экспорт данных произвел GSBIS 08/09/2015 10:06:47 БД "-db /home2/bis/quit41d/db-spb/bisquit,-ld bisquit,-Mm 16384,-spin 5000"
"class-code" "IndicationBlock" "103118" "DataBlock" "" ? ?
.
