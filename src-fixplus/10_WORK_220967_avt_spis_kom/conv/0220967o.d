[Op-kind 4.1d] // Экспорт данных произвел GSBIS 16/12/2014 12:00:08 БД "-db /home2/bis/quit41d/db-demo/bisquit,-ld bisquit,-Mm 16384,-spin 5000"
"CreateCom" "Автоматическое создание комиссий при повышении статуса" "g-trans" "WideProc" "BASE" "Универсальная транзакция для автоматического создания комиссий при повышении статуса  и установке связей
" "common-op-kind" "" "@_ResultCom = @flag;
SetSysConf(""_ResultCom"", ""1"");" "" "_ResultComflag(GETVAR(1(SETVAR(2""_ResultCom""""1""(SetSysConf(2"
.
[Op-kind-tmpl 4.1d] // Экспорт данных произвел GSBIS 16/12/2014 12:00:08 БД "-db /home2/bis/quit41d/db-demo/bisquit,-ld bisquit,-Mm 16384,-spin 5000"
71371 "CreateCom" "op-kind-tmpl" "opb" "// Флаг, определяющий, создан документ комиссии или нет, //1- создан, 0 - не создан

@flag = 1;
" "//проверка счета плательщика, кор.счета, вида документа
if can-do('405*,406*,407*,40802,40804,40807',substr(@acct-db(15),1,5)) &    can-do('30102,30223',substr(@acct-cr(15),1,5)) &    can-do('01,02',@doc-type(10))     
  then @flag = 1;
   else @flag = 0;                                                    
endif;                                                                                                                       
//Проверка очередности документа, технологии и налогов
if @flag == 1  
  then 
  (if can-do('1,2',@order-pay(10))||can-do('ВН,""""',@type(15))||БЮДЖ_ПЛАТЕЖ(@op(10) == ""ДА"")||(@order-pay(10) == '3') & can-do('401*',substr(@ben-acct(10),1,5))
        then @flag = 0;
         else 1;
       endif);
 else 1;     
endif;
                                                           
//Проверка БИКов банков филиалов при платежах через РКЦ на филиал
//if @bank-code(16) == ""044030790""
// then @flag = 0;                   
//  else 1;                          
//endif;                             " "flag1(SETVAR(2" "'405*,406*,407*,40802,40804,40807'acct-db15(GETVAR(215(substr(3(can-do(2'30102,30223'acct-cr15(GETVAR(215(substr(3(can-do(2(&'01,02'doc-type10(GETVAR(2(can-do(2(&(jumpcon(+5flag1(SETVAR(2(jumpnoc(+4flag0(SETVAR(2(nop(0flag(GETVAR(11(==(jumpcon(+42'1,2'order-pay10(GETVAR(2(can-do(2'ВН,""""'type15(GETVAR(2(can-do(2(||op10(GETVAR(2""ДА""(==(БЮДЖ_ПЛАТЕЖ(1(||order-pay10(GETVAR(2'3'(==(||'401*'ben-acct10(GETVAR(215(substr(3(can-do(2(&(jumpcon(+5flag0(SETVAR(2(jumpnoc(+21(nop(0(jumpnoc(+21(nop(0" 0 10 "Ввод данных" "Поиск с ош." 0 ""
71376 "CreateCom" "op-kind-tmpl" "op-entry" "" "" "" "" 71371 15 "op-entry" "Поиск с ош." 0 "Проводки"
71377 "CreateCom" "op-kind-tmpl" "op-bank" "" "" "" "" 71371 16 "op-bank" "Поиск с ош." 0 "Банковские реквизиты"
71378 "CreateCom" "op-kind-tmpl" "op-bank" "" "" "" "" 71371 17 "op-bank" "Поиск с ош." 0 "Банковские реквизиты валюта"
71372 "CreateCom" "op-kind-tmpl" "acctb" "" "" "" "" 0 20 "Ввод данных" "Поиск с ош." 0 "Поиск счета Дт из основного документа"
71373 "CreateCom" "op-kind-tmpl" "opb" "if @flag == 1
then
@acct-inc = СправочникЗн(""СчДохРасх"",""ЧекКниж,"" + @currency(15) + "","" +ФИЛИАЛ(),Дата(),нет);
if (@СпособПолуч(10) == """") || (@СпособПолуч(10) == ?) 
 then                             
   @com = ""ЮРПЛБУМ"";
  else
   @com = ""ЮРПЛКЛБ"";
endif;
if @currency(15) == """"
then
 @sum-val = 0;
 @sum-rub = КОМТАРИФПЛАН(@amt-rub(15),@com,@acct-db(15),"""",""Расчет"",""*"");   else
 @sum-val =КОМТАРИФПЛАН(@amt-cur(15),@com,@acct-db(15),@currency(15),""Расчет"",""*""); @sum-rub = ПЕРЕСЧЕТ(@sum-val,@currency(15),"""");
endif;
@doc-stat = ""Ф"";
@iop-kind = ""CreateCom"";  

if can-do('40807',substr(@acct-db(15),1,5))
then  
@doc-details = ""{VO99090} Комиссия за прием и исполнение п/п, предоставл. в банк согласно тарифам банка. НДС не облагается."";   
else 
@doc-details = ""Комиссия за прием и исполнение п/п, предоставл. в банк согласно тарифам банка. НДС не облагается."";       
endif;     
@curr = @currency(15);
else
0;
endif;" "" "flag(GETVAR(11(==(jumpcon(+115acct-inc""СчДохРасх""""ЧекКниж,""currency15(GETVAR(2(+"",""(+(ФИЛИАЛ(0(+(Дата(0нет(СправочникЗн(4(SETVAR(2СпособПолуч10(GETVAR(2""""(==СпособПолуч10(GETVAR(2?(==(||(jumpcon(+5com""ЮРПЛБУМ""(SETVAR(2(jumpnoc(+4com""ЮРПЛКЛБ""(SETVAR(2(nop(0currency15(GETVAR(2""""(==(jumpcon(+19sum-val0(SETVAR(2sum-rubamt-rub15(GETVAR(2com(GETVAR(1acct-db15(GETVAR(2""""""Расчет""""*""(КОМТАРИФПЛАН(6(SETVAR(2(jumpnoc(+26sum-valamt-cur15(GETVAR(2com(GETVAR(1acct-db15(GETVAR(2currency15(GETVAR(2""Расчет""""*""(КОМТАРИФПЛАН(6(SETVAR(2sum-rubsum-val(GETVAR(1currency15(GETVAR(2""""(ПЕРЕСЧЕТ(3(SETVAR(2(nop(0doc-stat""Ф""(SETVAR(2iop-kind""CreateCom""(SETVAR(2'40807'acct-db15(GETVAR(215(substr(3(can-do(2(jumpcon(+5doc-details""{VO99090} Комиссия за прием и исполнение п/п, предоставл. в банк согласно тарифам банка. НДС не облагается.""(SETVAR(2(jumpnoc(+4doc-details""Комиссия за прием и исполнение п/п, предоставл. в банк согласно тарифам банка. НДС не облагается.""(SETVAR(2(nop(0currcurrency15(GETVAR(2(SETVAR(2(jumpnoc(+20(nop(0" "" 0 50 "Ввод данных" "Создание" 0 "Комиссия"
71379 "CreateCom" "op-kind-tmpl" "op-entry" "" "" "" "" 71373 55 "op-entry" "Создание" 0 "Проводки"
71374 "CreateCom" "op-kind-tmpl" "xlink" "@flag;" "" "flag(GETVAR(1" "" 0 60 "Ввод данных" "Поиск с ош." 0 "Поиск связи"
71375 "CreateCom" "op-kind-tmpl" "links" "@flag;" "" "flag(GETVAR(1" "" 0 70 "Ввод данных" "Создание" 0 "Создание связи"
.
[Op-kind-tmpl-ln 4.1d] // Экспорт данных произвел GSBIS 16/12/2014 12:00:08 БД "-db /home2/bis/quit41d/db-demo/bisquit,-ld bisquit,-Mm 16384,-spin 5000"
401104 71371 "opb" "op" "" "search(@_OpMain)" "_OpMain(GETVAR(1(search(1" 1 "Выражение"
401125 71376 "op-entry" "op" "" "search(@op(10))" "op10(GETVAR(2(search(1" 1 "Выражение"
401126 71377 "op-bank" "op" "" "search(@op(10))" "op10(GETVAR(2(search(1" 1 "Выражение"
401127 71377 "op-bank" "op-bank-type" "" "search("""")" """""(search(1" 2 "Выражение"
401128 71377 "op-bank" "bank-code-type" "" "search(""МФО-9"")" """МФО-9""(search(1" 3 "Выражение"
401129 71378 "op-bank" "op" "" "search(@op(10))" "op10(GETVAR(2(search(1" 1 "Выражение"
401130 71378 "op-bank" "op-bank-type" "" "search(""ben"")         " """ben""(search(1" 2 "Выражение"
401131 71378 "op-bank" "bank-code-type" "" "search(""BIC"")" """BIC""(search(1" 3 "Выражение"
401105 71372 "acctb" "bal-acct" "" "search(substr(@acct-db(15),1,5))" "acct-db15(GETVAR(215(substr(3(search(1" 1 "Выражение"
401106 71372 "acctb" "acct" "" "search(@acct-db(15))" "acct-db15(GETVAR(2(search(1" 2 "Выражение"
401107 71372 "acctb" "currency" "" "search(@currency(15)) " "currency15(GETVAR(2(search(1" 3 "Выражение"
401108 71373 "opb" "doc-type" "017" "" "" 1 "Значение"
401109 71373 "opb" "doc-num" "" "счетчик(""мемордер"") " """мемордер""(счетчик(1" 2 "Выражение"
401110 71373 "opb" "doc-date" "" "Дата() " "(Дата(0" 3 "Выражение"
401111 71373 "opb" "due-date" "" "Дата() " "(Дата(0" 4 "Выражение"
401112 71373 "opb" "op-date" "" "Дата() " "(Дата(0" 5 "Выражение"
401113 71373 "opb" "ins-date" "" "Дата() " "(Дата(0" 6 "Выражение"
401114 71373 "opb" "order-pay" "5" "" "" 7 "Значение"
401115 71373 "opb" "op-status" "" "@doc-stat" "doc-stat(GETVAR(1" 8 "Выражение"
401116 71373 "opb" "op-kind" "" "@iop-kind" "iop-kind(GETVAR(1" 9 "Выражение"
401117 71373 "opb" "details" "" "@doc-details" "doc-details(GETVAR(1" 10 "Выражение"
401118 71373 "opb" "op-transaction" "" "@op-transaction(10)" "op-transaction10(GETVAR(2" 11 "Выражение"
401132 71379 "op-entry" "op-status" "" "@op-status(50)" "op-status50(GETVAR(2" 1 "Выражение"
401133 71379 "op-entry" "op-cod" "000000" "" "" 2 "Значение"
401134 71379 "op-entry" "op-date" "" "@op-date(50)" "op-date50(GETVAR(2" 3 "Выражение"
401135 71379 "op-entry" "acct-db" "" "@acct-db(15)" "acct-db15(GETVAR(2" 4 "Выражение"
401136 71379 "op-entry" "acct-cr" "" "@acct-inc
" "acct-inc(GETVAR(1" 5 "Выражение"
401137 71379 "op-entry" "currency" "" "@curr" "curr(GETVAR(1" 6 "Выражение"
401138 71379 "op-entry" "amt-cur" "" "@sum-val;
                   
 " "sum-val(GETVAR(1" 7 "Выражение"
401139 71379 "op-entry" "amt-rub" "" "@sum-rub;" "sum-rub(GETVAR(1" 8 "Выражение"
401140 71379 "op-entry" "type" "ВН" "" "" 9 "Значение"
401141 71379 "op-entry" "op-transaction" "" "@op-transaction(50);" "op-transaction50(GETVAR(2" 10 "Выражение"
401119 71374 "xlink" "link-code" "" "search(""КомГр"")
" """КомГр""(search(1" 1 "Выражение"
401120 71374 "xlink" "target-class" "op" "" "" 2 "Значение"
401121 71375 "links" "beg-date" "" "Дата()" "(Дата(0" 1 "Выражение"
401122 71375 "links" "link-id" "" "@link-id(60)" "link-id60(GETVAR(2" 2 "Выражение"
401123 71375 "links" "source-id" "" "@op(10)" "op10(GETVAR(2" 3 "Выражение"
401124 71375 "links" "target-id" "" "@op(50)" "op50(GETVAR(2" 4 "Выражение"
.
[Signs.op-kind] // Экспорт данных произвел GSBIS 16/12/2014 12:00:08 БД "-db /home2/bis/quit41d/db-demo/bisquit,-ld bisquit,-Mm 16384,-spin 5000"
"сс_выводнаэкран" "" "CreateCom" "op-kind" "Да" ? ?
"Executable" "Нет" "CreateCom" "op-kind" "" ? ?
"parslib" "" "CreateCom" "op-kind" "pacct,pcrd,pop,pcomm" ? ?
"ProgressBar" "Да" "CreateCom" "op-kind" "" ? ?
"TmplOrder" "70" "CreateCom" "op-kind" "" 70 ?
"Transaction" "" "CreateCom" "op-kind" "Да" ? ?
.
[Signs.op-template] // Экспорт данных произвел GSBIS 16/12/2014 12:00:08 БД "-db /home2/bis/quit41d/db-demo/bisquit,-ld bisquit,-Mm 16384,-spin 5000"
"AttrOptimized" "Нет" "71371" "op-kind-tmpl" "" ? ?
"Transaction" "" "71371" "op-kind-tmpl" "Да" ? ?
"AttrOptimized" "Нет" "71372" "op-kind-tmpl" "" ? ?
"Transaction" "" "71372" "op-kind-tmpl" "Да" ? ?
"AttrOptimized" "Нет" "71373" "op-kind-tmpl" "" ? ?
"Transaction" "" "71373" "op-kind-tmpl" "Да" ? ?
"AttrOptimized" "Нет" "71374" "op-kind-tmpl" "" ? ?
"Transaction" "" "71374" "op-kind-tmpl" "Да" ? ?
"AttrOptimized" "Нет" "71375" "op-kind-tmpl" "" ? ?
"Transaction" "" "71375" "op-kind-tmpl" "Да" ? ?
.
