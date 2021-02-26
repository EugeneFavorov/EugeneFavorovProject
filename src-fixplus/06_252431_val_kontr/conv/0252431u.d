[User-Proc 4.1d] // Экспорт данных произвел KSBIS 18/03/2015 17:45:47 БД "-db /home2/bis/quit41d/db-demo/bisquit,-ld bisquit,-Mm 16384,-spin 5000"
0 ? "ОТЧЕТЫ ГЕНЕРАТОРА" "Base" 1 "RepGen" 2968 yes "Корневой раздел генератора отчетов" ""
0 "izvestxl" "Отчет по поступлениям на транзитные счета и рублей от нерезидентов" "base" 2968 "RepGen" 176636 yes "" ""
.
[Reports] // Экспорт данных произвел KSBIS 18/03/2015 17:45:47 БД "-db /home2/bis/quit41d/db-demo/bisquit,-ld bisquit,-Mm 16384,-spin 5000"
"izvestxl" 1 "enddate:<#enddate#>                                                                             "
"izvestxl" 2 "begdate:<#begdate#>                                                                             "
"izvestxl" 3 "[table]:1                                                                           "
"izvestxl" 4 "<!REPEAT[izvest],BEGIN>"
"izvestxl" 5 "<#dateuved[izvest]#> "
"izvestxl" 6 "<#dateopentry[izvest]#>  "
"izvestxl" 7 "<#docnum[izvest]#>  "
"izvestxl" 8 "<#currency[izvest]#>"
"izvestxl" 9 "<#sum[izvest]#>"
"izvestxl" 10 "<#acct[izvest]#>"
"izvestxl" 11 "<#name[izvest]#>"
"izvestxl" 12 "<#details[izvest]#>"
"izvestxl" 13 "<!REPEAT[izvest],END>"
"izvestxl" 14 "[table/]:1                                                                           "
.
[Report-Fields] // Экспорт данных произвел KSBIS 18/03/2015 17:45:47 БД "-db /home2/bis/quit41d/db-demo/bisquit,-ld bisquit,-Mm 16384,-spin 5000"
.
[Signs] // Экспорт данных произвел KSBIS 18/03/2015 17:45:47 БД "-db /home2/bis/quit41d/db-demo/bisquit,-ld bisquit,-Mm 16384,-spin 5000"
"ВидДок" "" "176636" "user-proc" "izvestxl" ? ?
"ПчтРеж" "" "176636" "user-proc" "XlsVar" ? ?
.
