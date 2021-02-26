[Menu] // Экспорт данных произвел KMBIS 06/11/2015 17:45:02 БД "-db /home2/bis/quit41d/db/bisquit,-ld bisquit,-Mm 16384,-spin 5000"
800 "mainmenu" "БАЗОВЫЙ МОДУЛЬ" "base" 0 "" 1 yes "" ""
0 ? "ОТЧЕТЫ ГЕНЕРАТОРА" "Base" 1 "RepGen" 2968 yes "Корневой раздел генератора отчетов" ""
0 ? "СИСТЕМА ПЕЧАТИ" "base" 2968 "RepGen" 111912 yes "" ""
0 ? "ОСП. Шаблоны pfrotch1" "base" 111912 "RepGen" 175957 yes "" ""
0 "pfrotch1" "Отчет по пенсионерам (236841)" "base" 175957 "RepGen" 175956 yes "" ""
.
[Reports] // Экспорт данных произвел KMBIS 06/11/2015 17:45:02 БД "-db /home2/bis/quit41d/db/bisquit,-ld bisquit,-Mm 16384,-spin 5000"
"pfrotch1" 2 "[table]:1"
"pfrotch1" 3 "<!REPEAT[line],BEGIN!>"
"pfrotch1" 4 "<#fio[line]#>"
"pfrotch1" 5 "<#birth[line]#>"
"pfrotch1" 6 "<#proxy[line]#>"
"pfrotch1" 7 "<#proxybeg[line]#>"
"pfrotch1" 8 "<#proxyend[line]#>"
"pfrotch1" 9 "<#proxyfio[line]#>"
"pfrotch1" 10 "<#lastop[line]#>"
"pfrotch1" 11 "<#pfr[line]#>"
"pfrotch1" 12 "<!REPEAT[line],END!>"
"pfrotch1" 13 "[table/]:1 "
"pfrotch1" 15 ""
.
[Report-Fields] // Экспорт данных произвел KMBIS 06/11/2015 17:45:02 БД "-db /home2/bis/quit41d/db/bisquit,-ld bisquit,-Mm 16384,-spin 5000"
.
[Signs] // Экспорт данных произвел KMBIS 06/11/2015 17:45:02 БД "-db /home2/bis/quit41d/db/bisquit,-ld bisquit,-Mm 16384,-spin 5000"
"ВидДок" "" "175956" "user-proc" "pfrotch1" ? ?
"ПчтРеж" "" "175956" "user-proc" "XlsVar" ? ?
.
