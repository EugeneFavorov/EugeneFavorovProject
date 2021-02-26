[Class 4.1d] // Экспорт данных произвел KMBIS 27/05/2015 18:39:53 БД "-db /home2/bis/quit41d/db-spbD112/bisquit,-ld bisquit,-Mm 16384,-spin 5000"
"ОнлОбмен" "code" "Системы онлайн переводов" no yes no "code" "" ? no
"ExchPaySys" "" "Обмен с платежными системами" no no yes "Exchange" "" ? yes
"EXCHCyber" "" "Обмен с КиберПлат" no no yes "ExchPaySys" "" ? yes
"PPaySys" "Packet" "Обмен с платежными системами" no yes yes "Packet" "" ? yes
"PCyber" "Packet" "Киберплат: Платеж" no yes no "PPaySys" "" ? yes
"RPaySys" "Reference" "Обмен с платежными системами" no yes yes "Reference" "" ? yes
"RCyber" "Reference" "Обмен с КиберПлат" no yes no "RPaySys" "" ? yes
"ErrorPSys" "" "Ошибки обмена с плат.системами (Online)" no no no "OperError,RealError,PSysError" "" ? yes
"PSysError" "" "Ошибки онлайн обмена с плат.системами" no no no "GroupError" "" ? yes
"EXCHCyberRee" "" "Обмен с КиберПлат (Реестр)" no no no "EXCHCyber" "" ? yes
"PCyberRee" "Packet" "КиберПлат: Реестр платежей" no yes no "PPaySys" "" ? yes
"ExchSEPCPlat" "" "Обмен с КиберПлат" no no yes "ExchSEP" "" ? yes
"SepCPlatRee" "" "КиберПлат: Итоговый реестр" no no no "ExchSEPCPlat" "" ? yes
"EXCHUSIBREE" "" "Обмен с УралСиб (Реестр)" no no no "ExchPaySys" "" ? yes
"RUSibRee" "Reference" "Обмен с УралСиб" no yes no "RPaySys" "" ? no
"PUsibRee" "Packet" "УралСиб: Реестр платежей" no yes no "PPaySys" "" ? yes
"ExchSEPUSib" "" "Обмен с УралСиб" no no yes "ExchSEP" "" ? yes
"SepUSibRee" "" "Уралсиб: Итоговый реестр" no no yes "ExchSEPUSib" "" ? yes
"SepUSIBReeDT" "" "Данные итогового реестра" no no no "SepUSibRee" "" ? yes
"SepUSIBReeFH" "" "Заголовок итогового реестра" no no no "SepUSibRee" "" ? yes
"SepUSIBReeFF" "" "Итоговая строка" no no no "SepUSibRee" "" ? yes
"PUsibOpCr" "Packet" "Уралсиб: Импорт документа по реестру" no yes no "PPaySys" "" ? yes
"ExchSEPRapid" "" "Обмен с Рапидой" no no yes "ExchSEP" "" ? yes
"SepRapidRee" "" "Итоговый реестр Рапида" no no yes "ExchSEPRapid" "" ? yes
"SepRapidReeDT" "" "Рапида: Реестр, платежи клиентов" no no no "SepRapidRee" "" ? yes
"SepRapidReeFH" "" "Рапида: Реестр, заголовок" no no no "SepRapidRee" "" ? yes
"SepRapidReeFF" "" "Рапида: реестр, хвостовик" no no no "SepRapidRee" "" ? yes
"EXCHRapidRee" "" "Обмен с Рапидой (Реестр)" no no no "ExchPaySys" "" ? yes
"PRapidRee" "Packet" "Рапида: Реестр платежей" no yes no "PPaySys" "" ? yes
"RRapidRee" "Reference" "Рапида: Реестр платежей" no yes no "RPaySys" "" ? yes
"PRapidOpCr" "Packet" "Рапида. Реестр: создание платежа" no yes no "PPaySys" "" ? yes
"SepRapidOutDT" "" "Рапида: Возвратный реестр" no no no "SepRapidRee" "" ? yes
"ExchSEPGoldCrown" "" "Обмен с Золотой короной" no no yes "ExchSEP" "" ? yes
"SepGCrownRee" "" "Золотая корона: Реестр платежей" no no yes "ExchSEPGoldCrown" "" ? yes
"GoldCrownReeFH" "" "Заголовок реестр платежей" no no no "SepGCrownRee" "" ? yes
"EXCHGCrownRee" "" "Обмен с Золтой короной (Реестр)" no no no "ExchPaySys" "" ? yes
"GoldCrownReeDT" "" "Строки с платежами" no no no "SepGCrownRee" "" ? yes
"GoldCrownReeFF" "" "Хвостовик реестра" no no no "SepGCrownRee" "" ? yes
"PGCrownRee" "Packet" "Золотая корона: Реестр платежей" no yes no "PPaySys" "" ? yes
"RGCrownRee" "Reference" "Золотая короне: Реестр платежей" no yes no "RPaySys" "" ? yes
"PGCrownPay" "Packet" "Золотая корона: Платеж" no yes no "PPaySys" "" ? no
.
[Xattr 4.1d] // Экспорт данных произвел KMBIS 27/05/2015 18:39:53 БД "-db /home2/bis/quit41d/db-spbD112/bisquit,-ld bisquit,-Mm 16384,-spin 5000"
"acctb" "PriemKiberplat" no "Принимать платежи через Киберплат" "" no "logical" "Да/" "" no no "*" "" no no yes no "с" 0 "" "" 0 "CAN-DO" no no no ""
"ОнлОбмен" "code-frm" no "формат для code.code" "" no "character" "x(12)" "" no no "*" "" yes yes no no "п" 0 "" "" 0 "CAN-DO" no no no ""
"ОнлОбмен" "sortby" no "выражение сортировки для браузера" "" no "character" "x(1000)" "" no no "*" "" yes yes no no "п" 0 "" "" 0 "CAN-DO" no no no ""
"ОнлОбмен" "БИК" no "Список разрешенных БИК" "" no "character" "x(250)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"ОнлОбмен" "ЗакрКл" no "Закрытый (private) ключ" "" no "character" "x(2000)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"ОнлОбмен" "МаскСч" no "Маска счетов получателей" "" no "character" "x(250)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"ОнлОбмен" "ОткрКл" no "Открытый (public) ключ" "" no "character" "x(2000)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"ОнлОбмен" "ТрЗач" no "Транзакция зачисления платежей" "" no "character" "x(150)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"ExchPaySys" "Direct" no "Направление обмена" "Направление обмена" no "character" "x(12)" "" no no "*" "" yes no no no "п" 0 "Направление" "НАПРАВЛЕНИЕ" 0 "CAN-DO" no no no ""
"ExchPaySys" "КБК" no "Код бюджетной классификации" "" no "character" "x(20)" "КБК" no no "*" "" no yes yes no "с" 0 "Код бюджетной классификации" "Код бюджетной классификации" 0 "CAN-DO" no no no ""
"ExchPaySys" "ОКАТО-НАЛОГ" no "Код ОКАТО" "" no "character" "x(11)" "" no no "*" "" no no no no "с" 0 "Код ОКАТО" "Код ОКАТО" 2 "CAN-DO" no no no ""
"ExchPaySys" "ПокДД" no "Показатель даты документа" "" no "character" "x(10)" "" no no "*" "" no no no no "с" 0 "Показатель даты документа" "Показатель даты документа" 2 "CAN-DO" no no no ""
"ExchPaySys" "ПокНД" no "Показатель номера документа" "" no "character" "x(15)" "" no no "*" "" no yes yes no "с" 0 "Показатель номера документа" "Показатель номера документа" 2 "CAN-DO" no no no ""
"ExchPaySys" "ПокНП" no "Показатель налогового периода" "" no "character" "x(10)" "" no no "*" "" no no yes no "с" 0 "Показатель налогового периода" "Показатель налогового периода" 2 "CAN-DO" no no no ""
"ExchPaySys" "ПокОП" no "Показатель оснований платежа" "" no "character" "x(2)" "Нал:ОП" no no "*" "" no yes yes no "с" 0 "Показатель оснований платежа" "Показатель оснований платежа" 0 "CAN-DO" no no no ""
"ExchPaySys" "ПокСт" no "Показатель статуса" "" no "character" "x(12)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"ExchPaySys" "ПокТП" no "Показатель типа платежа" "" no "character" "x(2)" "" no no "*" "" no yes yes no "с" 0 "Показатель типа платежа" "Показатель типа платежа" 0 "CAN-DO" no no no ""
"ExchPaySys" "acct-cat" no "Категория" "Категория счета" no "character" "x" "" no no "*" "b" no no no no "с" 0 "Категория" "Категория" 2 "CAN-DO" no no no ""
"ExchPaySys" "acct-cr" no "Счет по кредиту" "" no "character" "x(12)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"ExchPaySys" "acct-cur" no "Текущий валютный счет" "" no "character" "x(20)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"ExchPaySys" "acct-db" no "Счет по дебету" "" no "character" "x(20)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"ExchPaySys" "acct-rec" no "Счет получателя" "" no "character" "x(40)" "" no no "*" "" yes no no no "с" 120 "" "" 0 "CAN-DO" no no no ""
"ExchPaySys" "acct-send" no "Счет отправителя" "" no "character" "x(40)" "" no no "*" "" yes no no no "с" 110 "" "" 0 "CAN-DO" no no no ""
"ExchPaySys" "acct-send-out" no "Номер счета для операций обмена" "" no "character" "x(20)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"ExchPaySys" "addr-rec" no "Адрес Бенефициара" "" no "character" "x(100)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"ExchPaySys" "amt-contr" no "Сумма в валюте контракта" "" no "decimal" ">>>>>>>>>>>>>>9.99" "" no no "*" "" no no yes no "с" 0 "" "" 2 "CAN-DO" no no no ""
"ExchPaySys" "amt-cur" no "Сумма проводки в валюте" "Сумма проводки в валюте" no "decimal" "-99999999999999999999999.99" "" no no "*" "00000000000000000000000.00" no no no no "с" 0 "" "" 2 "CAN-DO" no no no ""
"ExchPaySys" "amt-cur-all" no "Общая сумма валютной выручки" "" no "decimal" ">>>>>>>>>>>>>>9.99" "" no no "*" "" no no yes no "с" 0 "" "" 2 "CAN-DO" no no no ""
"ExchPaySys" "amt-rub" no "Сумма документа рублевая" "" no "decimal" "-zzz,zzz,zzz,zz9.99" "" no no "*" "0.00" no no no no "с" 0 "Сумма документа рублевая" "Сумма документа рублевая" 2 "CAN-DO" no no no ""
"ExchPaySys" "amt-trans" no "Сумма для зачисления на текущий счет" "" no "decimal" ">>>>>>>>>>>>>>9.99" "" no no "*" "" no no yes no "с" 0 "" "" 2 "CAN-DO" no no no ""
"ExchPaySys" "bamt-cur" no "Сумма покупаемой валюты" "" no "character" "x(20)" "" no no "*" "" no no yes no "с" 0 "Сумма покупаемой валюты" "Сумма покупаемой валюты" 2 "CAN-DO" no no no ""
"ExchPaySys" "bank-city-ben" no "Город банка бенефициара" "" no "character" "x(50)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"ExchPaySys" "bank-city-intermed" no "Населенный пункт банка посредника" "" no "character" "x(50)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"ExchPaySys" "bank-code-intermed" no "Код банка посредника" "Код банка посредника" no "character" "x(11)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"ExchPaySys" "bank-code-rec" no "Банк получателя" "" no "character" "x(20)" "" no no "*" "" yes no no no "с" 20 "" "" 0 "CAN-DO" no no no ""
"ExchPaySys" "bank-code-send" no "Банк плательщика" "" no "character" "x(20)" "" no no "*" "" yes no no no "с" 10 "" "" 0 "CAN-DO" no no no ""
"ExchPaySys" "bank-corr-acct-intermed" no "Корсчет в банке посреднике" "" no "character" "x(35)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"ExchPaySys" "bank-corr-acct-rec" no "Корсчет банка получателя" "" no "character" "x(20)" "" no no "*" "" yes no no no "с" 40 "" "" 0 "CAN-DO" no no no ""
"ExchPaySys" "bank-corr-acct-send" no "Коррсчет банка плательщика" "" no "character" "x(20)" "" no no "*" "" yes no no no "с" 30 "" "" 0 "CAN-DO" no no no ""
"ExchPaySys" "bank-name-ben" no "Название банка бенефициара" "" no "character" "x(160)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"ExchPaySys" "bank-name-intermed" no "Наименование банка посредника" "" no "character" "x(160)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"ExchPaySys" "bank-name-send" no "Наименование банка плательщика" "" no "character" "x(160)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"ExchPaySys" "ben-acct" no "Р/с" "Номер лицевого счета клиента в другом банке" no "character" "x(20)" "" no no "*" "" no no no no "с" 0 "Р/с" "Р/с" 0 "CAN-DO" no no no ""
"ExchPaySys" "buh" no "Гл. бухгалтер" "" no "character" "x(120)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"ExchPaySys" "chif" no "Руководитель" "" no "character" "x(120)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"ExchPaySys" "Class-Code" no "Класс объекта" "Класс объекта" no "character" "X(12)" "" no no "*" "op" no no no no "с" 0 "Класс объекта" "Класс объекта" 0 "CAN-DO" no no no ""
"ExchPaySys" "contract-date" no "Дата договора" "Дата договора" no "date" "99/99/9999" "" no no "*" "" yes no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"ExchPaySys" "country-ben" no "Код страны банка бенефициара" "" no "character" "x(3)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"ExchPaySys" "country-intermed" no "Код страны банка посредника" "" no "character" "x(3)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"ExchPaySys" "country-rec" no "Символьный код страны получателя" "Символьный код страны получателя(не клиента банка)" no "character" "x(3)" "country" no no "*" "" no no yes no "с" 0 "Символьный код страны получателя" "Символьный код страны получателя" 2 "CAN-DO" no no no ""
"ExchPaySys" "CrBankCode" no "Код банка зачисления." "" no "character" "x(20)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"ExchPaySys" "cur-contr" no "Валюта контракта" "" no "character" "x(12)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"ExchPaySys" "curgent" no "Срочность (s-срочный,n-несрочный)" "" no "character" "x(12)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"ExchPaySys" "currency" no "Код валюты" "Валюта" no "character" "xxx" "" no no "*" "" no no no no "с" 0 "Код валюты" "Код валюты" 0 "CAN-DO" no no no ""
"ExchPaySys" "DbAcct" no "Счет по дебету" "" no "character" "x(20)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"ExchPaySys" "DbAmtCur" no "Сумма дебета в валюте" "Сумма дебета в валюте" no "decimal" "->>>>>>,>>>,>>9.99" "" no no "*" "" yes no no no "с" 50 "" "" 2 "CAN-DO" no no no ""
"ExchPaySys" "DbBankCode" no "Код банка списания." "" no "character" "x(20)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"ExchPaySys" "dbf-num" no "Номер докуента в DBF" "" no "character" "x(16)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"ExchPaySys" "DealType" no "Тип сделки." "Тип сделки. 1-На бирже, 2-На межбанковском рынке, 3-У банка, 4-У ЦБ" no "integer" "9" "" no no "*" "" no no yes no "с" 0 "" "" 0 "CAN-DO" no no no ""
"ExchPaySys" "depdate" no "покупка валюты" "0/1 = не более суммы/ровно сумму" no "character" "x(54)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"ExchPaySys" "depnum" no "Основание покупки" "0/1 = резидент/нерезидент" no "character" "x(54)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"ExchPaySys" "details" no "Содержание" "Содержание операции" no "character" "x(350)" "" no no "ereg()" "" yes no no no "с" 0 "Содержание" "Содержание" 0 "CAN-DO" no no no ""
"ExchPaySys" "doc-date" no "Документ от" "Дата выписки расчетно-денежного документа" no "date" "99/99/9999" "" no no "*" "" yes no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"ExchPaySys" "doc-kind" no "Тип документа" "Тип документа" no "character" "x(1)" "" no no "*" "" no no no no "с" 0 "Тип документа" "Тип документа" 2 "CAN-DO" no no no ""
"ExchPaySys" "doc-num" no "Номер докум." "Номер расчетно-денежного документа" no "character" "x(6)" "" no no "ereg()" "" yes no no no "с" 0 "" "" 2 "CAN-DO" no no no ""
"ExchPaySys" "doc-type" no "Код докум." "Код расчетно-денежного документа" no "character" "x(5)" "" no no "*" "" yes no no no "с" 0 "ДОКУМЕНТ" "Код докум." 0 "CAN-DO" no no no ""
"ExchPaySys" "documents" no "Документы" "" no "character" "x(512)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"ExchPaySys" "due-date" no "Срок платежа" "Дата(срок) платежа" no "date" "99/99/9999" "" no no "*" "p^today()" no no no no "с" 0 "Срок платежа" "Срок платежа" 2 "CAN-DO" no no no ""
"ExchPaySys" "elnum" no "Электронный номер" "" no "character" "x(16)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"ExchPaySys" "history-id" no "Идентификатор последнего изменения" "" no "integer" "->>>>>>>>>>9" "" no no "*" "" no no yes no "с" 0 "" "" 0 "CAN-DO" no no no ""
"ExchPaySys" "indoc-id" no "Идентификатор заявки на покуп./прод. вал" "" no "integer" ">>>>>>>>>>>9" "" no no "*" "" no no yes no "с" 0 "" "" 0 "CAN-DO" no no no ""
"ExchPaySys" "inn" no "ИНН" "Индивидуальный номер налогоплательщика" no "character" "x(12)" "" no no "ereg(^[0-9F]{0,12}$)" "" no no no no "с" 0 "ИНН" "ИНН" 2 "CAN-DO" no no no ""
"ExchPaySys" "inn-rec" no "ИНН Получателя" "" no "character" "x(12)" "" no no "*" "" yes no no no "с" 80 "" "" 0 "CAN-DO" no no no ""
"ExchPaySys" "inn-send" no "ИНН плательщика" "" no "character" "x(12)" "" no no "*" "" yes no no no "с" 60 "" "" 0 "CAN-DO" no no no ""
"ExchPaySys" "ins-date" no "Дата приема" "Дата приема документа от клиента" no "date" "99/99/9999" "" no no "*" "p^today()" no no no no "с" 0 "Поступил" "Дата приема" 0 "CAN-DO" no no no ""
"ExchPaySys" "kau-cr" no "Код анал.уч." "Код аналитического учета по кредиту" no "character" "x(8)" "" no no "*" "" no no no no "с" 0 "Код анал.уч." "Код анал.уч." 2 "CAN-DO" no no no ""
"ExchPaySys" "kau-db" no "Код анал.уч." "Код аналитического учета по дебету" no "character" "x(8)" "" no no "*" "" no no no no "с" 0 "Код анал.уч." "Код анал.уч." 2 "CAN-DO" no no no ""
"ExchPaySys" "kontr" no "Контракт, дата контракта" "" no "character" "x(254)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"ExchPaySys" "Kpp-rec" no "КПП получателя" "" no "character" "x(9)" "" no no "*" "" yes no no no "с" 0 "КПП получателя" "КПП получателя" 0 "CAN-DO" no no no ""
"ExchPaySys" "Kpp-send" no "КПП плательщика" "" no "character" "x(9)" "" no no "*" "" yes no no no "с" 0 "КПП плательщика" "КПП плательщика" 0 "CAN-DO" no no no ""
"ExchPaySys" "kvp" no "Расходы зарубежного банка" "0-списать со счета, 1-отнести на счет бенефициара" no "character" "x(12)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"ExchPaySys" "LegTerr" no "Список совпадений" "Список совпадений" no "character" "x(4000)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"ExchPaySys" "name-ben" no "Наим.клиента" "Наименование клиента в другом банке" no "character" "x(256)" "" no no "*" "" no no no no "с" 0 "Клиент" "Наим.клиента" 0 "CAN-DO" no no no ""
"ExchPaySys" "name-rec" no "Наименование получателя" "" no "character" "x(160)" "" no no "*" "" yes no no no "с" 100 "" "" 0 "CAN-DO" no no no ""
"ExchPaySys" "name-send" no "Наименование отправителя" "Наименование отправителя" no "character" "x(160)" "" no no "*" "" yes no no no "с" 90 "" "" 0 "CAN-DO" no no no ""
"ExchPaySys" "nomer" no "Номер заявления о резервировании" "" no "character" "x(30)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"ExchPaySys" "not-date" no "Дата уведомления" "" no "date" "99/99/9999" "" no no "*" "" no no yes no "с" 0 "" "" 0 "CAN-DO" no no no ""
"ExchPaySys" "op" no "Номер операции" "Номер операции" no "integer" ">>>>>>>>>9" "" no no "*" "0" yes no no no "с" 0 "Номер операции" "Номер операции" 0 "CAN-DO" no no no ""
"ExchPaySys" "op-cod" no "Код операции" "Код операции" no "character" "x(8)" "" no no "*" "" no no no no "с" 0 "Код операции" "Код операции" 2 "CAN-DO" no no no ""
"ExchPaySys" "op-date" no "Дата" "Дата проводки" no "date" "99/99/9999" "" no no "*" "" yes no no no "с" 0 "Дата" "Дата" 0 "CAN-DO" no no no ""
"ExchPaySys" "op-entry" no "Номер проводки" "Номер проводки" no "integer" "-9999999999" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"ExchPaySys" "op-error" no "Ошибки" "Ошибки загрузки документа" no "character" "x(350)" "" no no "*" "" no no no no "с" 0 "Ошибки" "Ошибки" 2 "CAN-DO" no no no ""
"ExchPaySys" "op-status" no "Статус документа" "" no "character" "x(12)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"ExchPaySys" "op-value-date" no "Дата валютирования" "Дата валютирования" no "date" "99/99/9999" "" no no "*" ? no no no no "с" 0 "Дата валютирования" "Дата валютирования" 2 "CAN-DO" no no no ""
"ExchPaySys" "order-doc" no "Очередность платежа в документе" "Очередность платежа в документе" no "character" "x(2)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"ExchPaySys" "order-pay" no "Очередность платежа" "Очередность платежа" no "integer" "999999" "" no no "*" "" yes no no no "с" 0 "" "MAKE-INDEX" 0 "CAN-DO" no no no ""
"ExchPaySys" "order-pre" no "Предварительная сортировка" "Предварительная сортировка" no "integer" "999999" "" no no "*" "" yes no no no "с" 0 "" "MAKE-INDEX" 0 "CAN-DO" no no no ""
"ExchPaySys" "pay-until-date" no "Дата окончания действия поручения." "" no "date" "99/99/9999" "" no no "*" "" no no yes no "с" 0 "" "" 0 "CAN-DO" no no no ""
"ExchPaySys" "psi" no "Паспорт сделки" "" no "character" "x(254)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"ExchPaySys" "qty" no "Количество" "Количество" no "decimal" ">,>>>,>>9.99" "" no no "*" "0" no no no no "с" 0 "Количество" "Количество" 2 "CAN-DO" no no no ""
"ExchPaySys" "rashod" no "Расоды банка" "0-списать со счета, 1-из суммы перевода" no "character" "x(12)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"ExchPaySys" "Rate" no "Курс операции" "" no "character" "x(38)" "" no no "*" "" yes no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"ExchPaySys" "res-date" no "Дата заявления о резервирования" "" no "date" "99/99/9999" "" no no "*" "" no no yes no "с" 0 "" "" 0 "CAN-DO" no no no ""
"ExchPaySys" "rstate" no "Статус запроса" "" no "character" "x(12)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"ExchPaySys" "sell-bank" no "Признак продажи банку" "Если 1 - для продажи банку" no "decimal" ">>>>>>>>>>>>>>9.99" "" no no "*" "" no no yes no "с" 0 "" "" 2 "CAN-DO" no no no ""
"ExchPaySys" "sell-trade" no "Признак продажи на бирже" "Если 1 - для продажи на бирже" no "decimal" ">>>>>>>>>>>>>>9.99" "" no no "*" "" no no yes no "с" 0 "" "" 2 "CAN-DO" no no no ""
"ExchPaySys" "SendDate" no "Дата экспорта" "" no "character" "x(8)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"ExchPaySys" "SendID" no "Идентификатор отправителя" "Идентификатор отправителя" no "character" "x(12)" "" no no "*" "" yes no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"ExchPaySys" "SendREF" no "Уникальный ссылка отправителя" "" no "character" "x(12)" "" no no "*" "" yes no no no "с" 140 "" "MAKE-INDEX" 0 "CAN-DO" no no no ""
"ExchPaySys" "spacctn" no "Номер валютного спецсчета" "" no "character" "x(25)" "" no no "*" "" no no yes no "с" 0 "Номер валютного спецсчета" "Номер валютного спецсчета" 2 "CAN-DO" no no no ""
"ExchPaySys" "sstatus" no "Статус составителя" "" no "character" "x(2)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"ExchPaySys" "State" no "Состояние" "Состояние" no "character" "x(12)" "" no no "*" "" yes no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"ExchPaySys" "status" no "Статус" "" no "character" "x(12)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"ExchPaySys" "symbol" no "Касс. символ" "Кассовый символ" no "character" "xx" "" no no "*" "" no no no no "с" 0 "Касс. символ" "Касс. символ" 2 "CAN-DO" no no no ""
"ExchPaySys" "SystemCode" no "Признак обработки" "Признак обработки" no "character" "x(2)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"ExchPaySys" "Text" no "Содержание строки реестра" "" no "character" "x(4000)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"ExchPaySys" "type" no "Техн. платежа" "Условный код технологии исполнения платежа" no "character" "x(3)" "" no no "*" "" no no no no "с" 0 "Техн. платежа" "Техн. платежа" 2 "CAN-DO" no no no ""
"ExchPaySys" "Urgency" no "Срочность перевода" "0 - несрочный, 1 - срочный" no "integer" "9" "" no no "0,1" "" no no yes no "с" 0 "срочность платежа" "срочность платежа" 0 "CAN-DO" no no no ""
"ExchPaySys" "user-id" no "Сотрудник" "Код сотрудника, вводившего данные" no "character" "x(8)" "" no no "*" "BIS" no no no no "с" 0 "Сотрудник" "Сотрудник" 2 "CAN-DO" no no no ""
"ExchPaySys" "value-date" no "Курс на" "Дата курса, на которую расчитан эквивалент нац. валюты" no "date" "99/99/9999" "" no no "*" ? no no no no "с" 0 "Курс на" "Курс на" 2 "CAN-DO" no no no ""
"ExchPaySys" "vidop" no "Код ВО" "" no "character" "x(12)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"ExchPaySys" "XMLNameSpace" no "Пространство имен" "Пространство имен" no "character" "x(40)" "" no no "*" "" yes no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"EXCHCyberRee" "number" no "Уникальный идентификатор абонента" "" no "character" "x(50)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"EXCHCyberRee" "abtype" no "Тип абонента" "" no "character" "x(12)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"EXCHCyberRee" "DTpay" no "Дата и время завершения операции" "" no "character" "x(20)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"EXCHCyberRee" "receipt" no "Уникальный номер платежа в КиберПлат" "" no "character" "x(20)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"SepCPlatRee" "sep001" no "Дата и время платежа" "ATTR" no "character" "x(19)" "" no no "" "" yes no no yes "с" 1 "EXCHCyberRee" "DTPay" 2 "0" no no no ""
"SepCPlatRee" "sep002" no "Тип" "ATTR" no "character" "x(20)" "" no no "" "" yes no no yes "с" 2 "EXCHCyberRee" "number" 2 "0" no no no ""
"SepCPlatRee" "sep003" no "Дата и время" "ATTR" no "character" "x(20)" "" no no "" "" yes no no yes "с" 3 "EXCHCyberRee" "bank-code-rec" 2 "0" no no no ""
"SepCPlatRee" "sep004" no "Сумма платежа" "ATTR" no "character" "x(9)" "" no no "" "" yes no no yes "с" 4 "EXCHCyberRee" "abtype" 2 "0" no no no ""
"SepCPlatRee" "sep005" no "Номер платежа" "ATTR" no "character" "x(99)" "" no no "" "" yes no no yes "с" 5 "EXCHCyberRee" "amt-rub" 2 "0" no no no ""
"ОнлОбмен" "ТрСчСпис" no "Транзитный счет списания" "" no "character" "x(99)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"SepCPlatRee" "sep006" no "Бик банка получателя" "ATTR" no "character" "x(99)" "" no no "" "" yes no no yes "с" 6 "EXCHCyberRee" "receipt" 2 "0" no no no ""
"PCyberRee" "SummAmt" no "Общая сумма зачислений" "" no "character" "x(99)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"ОнлОбмен" "СтатБезПодтв" no "Статус документов" "Для документов без подтверждения" no "character" "x(12)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"SepUSIBReeDT" "sep001" no "Номер перевода" "ATTR" no "character" "x(99)" "" no no "" "" yes no no yes "с" 1 "EXCHUSIBREE" "SendREF" 2 "0" no no no ""
"SepUSIBReeDT" "sep002" no "Дата перевода" "ATTR" no "character" "x(10)" "" no no "" "" yes no no yes "с" 2 "EXCHUSIBREE" "OrderDate" 2 "0" no no no ""
"SepUSIBReeDT" "sep003" no "Сумма перевода" "ATTR" no "character" "x(99)" "" no no "" "" yes no no yes "с" 3 "EXCHUSIBREE" "Amt" 2 "0" no no no ""
"SepUSIBReeDT" "sep004" no "Сумма комиссионного вознаграждения" "ATTR" no "character" "x(99)" "" no no "" "" yes no no yes "с" 4 "EXCHUSIBREE" "AmtCom" 2 "0" no no no ""
"SepUSIBReeDT" "sep005" no "Счет Получателя" "ATTR" no "character" "x(99)" "" no no "" "" yes no no yes "с" 5 "EXCHUSIBREE" "OrderAcct" 2 "0" no no no ""
"SepUSIBReeDT" "sep006" no "БИК филиала Банка" "ATTR" no "character" "x(99)" "" no no "" "" yes no no yes "с" 6 "EXCHUSIBREE" "bank-code-rec" 2 "0" no no no ""
"SepUSIBReeDT" "sep007" no "ФИО Получателя" "ATTR" no "character" "x(250)" "" no no "" "" yes no no yes "с" 7 "EXCHUSIBREE" "name-rec" 2 "0" no no no ""
"SepUSIBReeDT" "sep008" no "ФИО Отправителя" "ATTR" no "character" "x(250)" "" no no "" "" yes no no yes "с" 8 "EXCHUSIBREE" "name-send" 2 "0" no no no ""
"SepUSIBReeFF" "sep001" no "Количество переводов" "ATTR" no "character" "x(99)" "" no no "" "" yes no no yes "с" 1 "EXCHUSIBREE" "TotCount" 2 "0" no no no ""
"SepUSIBReeFF" "sep002" no "Сумма переводов" "ATTR" no "character" "x(99)" "" no no "" "" yes no no yes "с" 2 "EXCHUSIBREE" "TotAmt" 2 "0" no no no ""
"SepUSIBReeFF" "sep004" no "Сумма комиссионного вознаграждения" "ATTR" no "character" "x(99)" "" no no "" "" yes no no yes "с" 4 "EXCHUSIBREE" "TotCom" 2 "0" no no no ""
"EXCHUSIBREE" "IDNum" no "Номер перевода" "" no "character" "x(99)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"EXCHUSIBREE" "OrderDate" no "Дата перевода" "" no "character" "x(12)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"EXCHUSIBREE" "Amt" no "Сумма перевода" "" no "character" "x(99)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"EXCHUSIBREE" "AmtCom" no "Сумма комиссионного вознаграждения" "" no "character" "x(99)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"EXCHUSIBREE" "OrderAcct" no "Счет получателя" "" no "character" "x(99)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"EXCHUSIBREE" "tel-send" no "Номер телефона Отправителя" "" no "character" "x(99)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"EXCHUSIBREE" "TotCount" no "Количество переводов" "" no "character" "x(99)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"EXCHUSIBREE" "TotAmt" no "Сумма переводов" "" no "character" "x(99)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"EXCHUSIBREE" "TotCom" no "Сумма комиссионного вознаграждения" "" no "character" "x(99)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"ExchPaySys" "ExchMain" no "Хэндл охватывающего сообщения" "Хэндл охватывающего сообщения" no "character" "x(99)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"SepUSIBReeDT" "sep009" no "Номер телефона Отправителя" "ATTR" no "character" "x(99)" "" no no "" "" yes no no yes "с" 9 "EXCHUSIBREE" "tel-send" 2 "0" no no no ""
"SepUSIBReeFH" "sep001" no "Номер перевода" "ATTR" no "character" "x(99)" "" no no "" "" yes no no yes "с" 1 "EXCHUSIBREE" "SendREF" 2 "0" no no no ""
"SepUSIBReeFH" "sep002" no "Дата перевода" "ATTR" no "character" "x(10)" "" no no "" "" yes no no yes "с" 2 "EXCHUSIBREE" "OrderDate" 2 "0" no no no ""
"SepUSIBReeFH" "sep003" no "Сумма перевода" "ATTR" no "character" "x(99)" "" no no "" "" yes no no yes "с" 3 "EXCHUSIBREE" "Amt" 2 "0" no no no ""
"SepUSIBReeFH" "sep004" no "Сумма комиссионного вознаграждения" "ATTR" no "character" "x(99)" "" no no "" "" yes no no yes "с" 4 "EXCHUSIBREE" "AmtCom" 2 "0" no no no ""
"SepUSIBReeFH" "sep005" no "Счет Получателя" "ATTR" no "character" "x(99)" "" no no "" "" yes no no yes "с" 5 "EXCHUSIBREE" "OrderAcct" 2 "0" no no no ""
"SepUSIBReeFH" "sep006" no "БИК филиала Банка" "ATTR" no "character" "x(99)" "" no no "" "" yes no no yes "с" 6 "EXCHUSIBREE" "bank-code-rec" 2 "0" no no no ""
"SepUSIBReeFH" "sep007" no "ФИО Получателя" "ATTR" no "character" "x(250)" "" no no "" "" yes no no yes "с" 7 "EXCHUSIBREE" "name-rec" 2 "0" no no no ""
"SepUSIBReeFH" "sep008" no "ФИО Отправителя" "ATTR" no "character" "x(250)" "" no no "" "" yes no no yes "с" 8 "EXCHUSIBREE" "name-send" 2 "0" no no no ""
"SepUSIBReeFH" "sep009" no "Номер телефона Отправителя" "ATTR" no "character" "x(99)" "" no no "" "" yes no no yes "с" 9 "EXCHUSIBREE" "tel-send" 2 "0" no no no ""
"ExchPaySys" "CounterName" no "Имя счетчика документов" "" no "character" "x(12)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"EXCHRapidRee" "CliSys" no "Уникальный код Участника Системы в НКО" "" no "character" "x(12)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"EXCHRapidRee" "ReeRef" no "Уникальный номер реестра" "" no "character" "x(12)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"SepRapidReeFH" "sep000" no "Уникальный код Участника Системы в НКО" "ATTR" no "character" "x(8)" "" no no "" "" yes no no yes "с" 2 "EXCHRapidRee" "CliSys" 2 "0" no no no ""
"EXCHRapidRee" "DTReeBeg" no "Дата и время начала отчетного периода" "" no "character" "x(20)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"SepRapidReeFH" "sep001" no "Тип строки" "STATIC" no "character" "xxx" "" no no "" "sum" yes no no yes "с" 1 "" "" 2 "1" no no no ""
"SepRapidReeFH" "sep003" no "Уникальный номер реестра" "ATTR" no "character" "x(12)" "" no no "" "" yes no no yes "с" 3 "EXCHRapidRee" "ReeRef" 2 "0" no no no ""
"SepRapidReeFH" "sep004" no "Дата и время начала отчетного периода" "ATTR" no "character" "x(20)" "" no no "" "" yes no no yes "с" 4 "EXCHRapidRee" "DTReeBeg" 2 "0" no no no ""
"SepRapidReeFH" "sep005" no "Дата и время окончания отчетного периода" "ATTR" no "character" "x(20)" "" no no "" "" yes no no yes "с" 5 "EXCHRapidRee" "DTReeEnd" 2 "0" no no no ""
"EXCHRapidRee" "DTReeEnd" no "Дата и время окончания отчетного периода" "" no "character" "x(20)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"SepRapidReeFH" "sep006" no "Количество записей типа pay, вошедших в реестр" "ATTR" no "character" "x(999)" "" no no "" "" yes no no yes "с" 6 "EXCHRapidRee" "TotCount" 2 "0" no no no ""
"EXCHRapidRee" "TotCount" no "Количество записей типа pay, вошедших в" "" no "character" "x(999)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"SepRapidReeFH" "sep007" no "Общая сумма исполненных переводов в рублях" "ATTR" no "character" "x(250)" "" no no "" "" yes no no yes "с" 7 "EXCHRapidRee" "TotAmt" 2 "0" no no no ""
"EXCHRapidRee" "TotAmt" no "Общая сумма исполненных переводов" "в рублях" no "character" "x(250)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"SepRapidReeDT" "sep001" no "Тип строки" "STATIC" no "CHARACTER" "x(3)" "" no no "" "pay" yes no no yes "с" 1 "" "" 2 "1" no no no ""
"SepRapidReeDT" "sep002" no "Дата и время регистрации перевода" "ATTR" no "character" "x(20)" "" no no "" "" yes no no yes "с" 2 "EXCHRapidRee" "DTPay" 2 "0" no no no ""
"EXCHRapidRee" "DTPay" no "Дата и время регистрации перевода" "" no "character" "x(20)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"SepRapidReeDT" "sep003" no "Уникальный номер перевода в НКО" "ATTR" no "character" "x(12)" "" no no "" "" yes no no yes "с" 3 "EXCHRapidRee" "SendREF" 2 "0" no no no ""
"SepRapidReeDT" "sep000" no "Сумма перевода" "ATTR" no "character" "x(20)" "" no no "" "" yes no no yes "с" 4 "EXCHRapidRee" "amt-rub" 2 "0" no no no ""
"SepRapidReeDT" "sep005" no "Номер счета" "ATTR" no "character" "x(90)" "" no no "" "" yes no no yes "с" 5 "EXCHRapidRee" "OrderAcct" 2 "0" no no no ""
"EXCHRapidRee" "OrderAcct" no "Номер счета" "" no "character" "x(30)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"SepRapidReeDT" "sep006" no "БИК" "ATTR" no "character" "x(90)" "" no no "" "" yes no no yes "с" 6 "EXCHRapidRee" "bank-code-rec" 2 "0" no no no ""
"SepRapidReeDT" "sep008" no "ФИО отправителя" "ATTR" no "character" "x(250)" "" no no "" "" yes no no yes "с" 8 "EXCHRapidRee" "name-send" 2 "0" no no no ""
"SepRapidReeDT" "sep007" no "ФИО получателя" "ATTR" no "character" "x(250)" "" no no "" "" yes no no yes "с" 7 "EXCHRapidRee" "name-rec" 2 "0" no no no ""
"EXCHRapidRee" "addr-send" no "Адрес регистрации отправителя" "" no "character" "x(900)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"SepRapidReeDT" "sep009" no "Адрес регистрации отправителя" "ATTR" no "character" "x(900)" "" no no "" "" yes no no yes "с" 9 "EXCHRapidRee" "addr-send" 2 "0" no no no ""
"PRapidRee" "CliSys" no "Уникальный код Участника Системы в НКО" "" no "character" "x(12)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"PRapidRee" "ReeRef" no "Уникальный номер реестра" "" no "character" "x(12)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"PRapidRee" "DTReeBeg" no "Дата и время начала отчетного периода" "" no "character" "x(20)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"PRapidRee" "DTReeEnd" no "Дата и время окончания отчетного периода" "" no "character" "x(20)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"PRapidRee" "TotCount" no "Количество записей типа pay" "" no "character" "x(999)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"PRapidRee" "TotAmt" no "Общая сумма исполненных переводов" "в рублях" no "character" "x(250)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"PRapidRee" "DTPay" no "Дата и время регистрации перевода" "" no "character" "x(20)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"PRapidRee" "SendRef" no "Уникальный ссылка отправителя" "" no "character" "x(150)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"PRapidRee" "amt-rub" no "Сумма перевода" "" no "character" "x(21)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"PRapidRee" "OrderAcct" no "Номер счета" "" no "character" "x(20)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"PRapidRee" "bank-code-rec" no "Банк получателя" "" no "character" "x(20)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"PRapidRee" "name-rec" no "Наименование получателя" "" no "character" "x(260)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"PRapidRee" "name-send" no "Наименование отправителя" "" no "character" "x(260)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"PRapidRee" "addr-send" no "Адрес регистрации отправителя" "" no "character" "x(900)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"EXCHRapidRee" "TotAmtWOCom" no "Сумма переводов без комиссии" "" no "character" "x(20)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"SepRapidReeFH" "sep008" no "Общая сумма без комиссии" "ATTR" no "character" "x(20)" "" no no "" "" yes no no yes "с" 8 "EXCHRapidRee" "TotAmtWOCom" 2 "0" no no no ""
"PRapidRee" "ReeError" no "Ошибка реестра" "" no "character" "x(12)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"EXCHRapidRee" "FilialId" no "Код подразделения для зачисления" "" no "character" "x(99)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"EXCHRapidRee" "RapidaErr" no "Ошибки в реестра платежей" "" no "character" "x(250)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"PRapidRee" "SendId" no "Идентификатор отправителя" "" no "character" "x(12)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"PRapidRee" "RapidaErr" no "Ошибки в строках платежей реестра" "" no "character" "x(150)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"EXCHRapidRee" "ReeDate" no "Дата реестра" "" no "character" "x(12)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"EXCHRapidRee" "ReeTime" no "Время реестра" "" no "character" "x(12)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"GoldCrownReeFH" "sep001" no "Номер реестра" "ATTR" no "character" "x(12)" "" no no "" "" yes no no yes "с" 1 "EXCHGCrownRee" "ReeNom" 2 "0" no no no ""
"EXCHGCrownRee" "ReeNom" no "Номер реестра" "" no "character" "x(12)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"EXCHGCrownRee" "ReeAmt" no "Сумма передаваемого реестра" "" no "character" "x(99)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"EXCHGCrownRee" "ReePeny" no "Пеня" "" no "character" "x(12)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"EXCHGCrownRee" "ReeHoldAmt" no "Удержанная сумма" "" no "character" "x(99)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"EXCHGCrownRee" "ReePayAmt" no "Сумма к перечислению" "" no "character" "x(99)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"EXCHGCrownRee" "ReeCountTot" no "Количество записей в реесре" "" no "character" "x(12)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"EXCHGCrownRee" "ReeAgent" no "Код агента (РНКО)" "" no "character" "x(100)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"EXCHGCrownRee" "ReeNomUsl" no "Номер услуги" "" no "character" "x(12)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"EXCHGCrownRee" "ReeDate" no "Дата формирования реестра" "" no "character" "x(20)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"EXCHGCrownRee" "ReeBegDate" no "Начала диапазона дат документов" "" no "character" "x(20)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"EXCHGCrownRee" "ReeEndDate" no "Конец диапазона дат документов" "" no "character" "x(20)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"EXCHGCrownRee" "ReePrim" no "Примечание" "" no "character" "x(3000)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"EXCHGCrownRee" "FIOCli" no "ФИО Клиета" "" no "character" "x(1500)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"EXCHGCrownRee" "AddrCli" no "Адрес" "" no "character" "x(1500)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"EXCHGCrownRee" "AcctCli" no "Лицевой счет" "" no "character" "x(20)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"EXCHGCrownRee" "DopInfo" no "Дополнительные поля" "" no "character" "x(3000)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"EXCHGCrownRee" "Amt" no "Сумма перевода" "" no "character" "x(99)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"EXCHGCrownRee" "PayDate" no "Дата внесения перевода" "" no "character" "x(10)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"EXCHGCrownRee" "Peny" no "Пеня" "" no "character" "x(12)" "" no no "*" "" no no no no "с" 0 "" "" 0 "CAN-DO" no no no ""
"GoldCrownReeFH" "sep002" no "Сумма передаваемого реестра" "ATTR" no "character" "x(99)" "" no no "" "" yes no no yes "с" 2 "EXCHGCrownRee" "ReeAmt" 2 "0" no no no ""
"GoldCrownReeFH" "sep003" no "Пеня" "ATTR" no "character" "x(12)" "" no no "" "" yes no no yes "с" 3 "EXCHGCrownRee" "ReePeny" 2 "0" no no no ""
"GoldCrownReeFH" "sep004" no "Удержанная сумма" "ATTR" no "character" "x(99)" "" no no "" "" yes no no yes "с" 4 "EXCHGCrownRee" "ReeHoldAmt" 2 "0" no no no ""
"GoldCrownReeFH" "sep005" no "Сумма к перечислению" "ATTR" no "character" "x(99)" "" no no "" "" yes no no yes "с" 5 "EXCHGCrownRee" "ReePayAmt" 2 "0" no no no ""
"GoldCrownReeFH" "sep000" no "Количество записей в реестра" "ATTR" no "character" "x(12)" "" no no "" "" yes no no yes "с" 6 "EXCHGCrownRee" "ReeCountTot" 2 "0" no no no ""
"GoldCrownReeFH" "sep007" no "Код агента" "ATTR" no "CHARACTER" "x(100)" "" no no "" "" yes no no yes "с" 7 "EXCHGCrownRee" "ReeAgent" 2 "0" no no no ""
"GoldCrownReeFH" "sep008" no "Номер услуги" "ATTR" no "character" "x(12)" "" no no "" "" yes no no yes "с" 8 "EXCHGCrownRee" "ReeNomUsl" 2 "0" no no no ""
"GoldCrownReeFH" "sep009" no "Дата формирования реестра" "ATTR" no "character" "x(20)" "" no no "" "" yes no no yes "с" 9 "EXCHGCrownRee" "ReeDate" 2 "0" no no no ""
"GoldCrownReeFH" "sep010" no "Начало диапазона дат документов" "ATTR" no "character" "x(20)" "" no no "" "" yes no no yes "с" 10 "EXCHGCrownRee" "ReeBegDate" 2 "0" no no no ""
"GoldCrownReeFH" "sep011" no "Конец дат документов" "ATTR" no "character" "x(20)" "" no no "" "" yes no no yes "с" 11 "EXCHGCrownRee" "ReeEndDate" 2 "0" no no no ""
"GoldCrownReeFH" "sep012" no "Примечание" "ATTR" no "character" "x(3000)" "" no no "" "" yes no no yes "с" 12 "EXCHGCrownRee" "ReePrim" 2 "0" no no no ""
"GoldCrownReeDT" "sep001" no "ФИО Клиента" "ATTR" no "character" "x(250)" "" no no "" "" yes no no yes "с" 1 "EXCHGCrownRee" "FIOCli" 2 "0" no no no ""
"GoldCrownReeDT" "sep002" no "Адрес" "ATTR" no "character" "x(500)" "" no no "" "" yes no no yes "с" 2 "EXCHGCrownRee" "AddrCli" 2 "0" no no no ""
"GoldCrownReeDT" "sep000" no "Лицевой счет" "ATTR" no "character" "x(20)" "" no no "" "" yes no no yes "с" 3 "EXCHGCrownRee" "AcctCli" 2 "0" no no no ""
"GoldCrownReeDT" "sep004" no "Сумма перевода" "ATTR" no "character" "x(99)" "" no no "" "" yes no no yes "с" 4 "EXCHGCrownRee" "Amt" 2 "0" no no no ""
"GoldCrownReeDT" "sep006" no "Пустоя поле" "STATIC" no "character" "x(12)" "" no no "" "" yes no no yes "с" 5 "" "" 2 "0" no no no ""
"GoldCrownReeDT" "" no "Пустое поле" "STATIC" no "character" "x(12)" "" no no "" "" yes no no yes "с" 6 "" "" 2 "0" no no no ""
"GoldCrownReeDT" "sep007" no "Пустое поле" "STATIC" no "character" "x(12)" "" no no "" "" yes no no yes "с" 7 "" "" 2 "0" no no no ""
"GoldCrownReeDT" "sep008" no "Дополнительные поля" "ATTR" no "character" "x(3000)" "" no no "" "" yes no no yes "с" 8 "EXCHGCrownRee" "DopInfo" 2 "0" no no no ""
"GoldCrownReeDT" "sep010" no "Остаток на счете" "STATIC" no "character" "x(12)" "" no no "" "" yes no no yes "с" 10 "" "" 2 "0" no no no ""
"GoldCrownReeDT" "sep011" no "Уникальные номер операции" "ATTR" no "character" "x(12)" "" no no "" "" yes no no yes "с" 11 "EXCHGCrownRee" "SendRef" 2 "0" no no no ""
"GoldCrownReeDT" "sep012" no "Пеня" "ATTR" no "character" "x(12)" "" no no "" "" yes no no yes "с" 12 "EXCHGCrownRee" "Peny" 2 "0" no no no ""
"GoldCrownReeDT" "sep009" no "PayDate" "ATTR" no "character" "x(12)" "" no no "" "" yes no no yes "с" 9 "EXCHGCrownRee" "PayDate" 2 "0" no no no ""
"ExchPaySys" "id273" no "" "" no "integer" ">>>>>>>>>>9" "" no no "*" "" no no yes no "с" 0 "" "" 0 "CAN-DO" no no no ""
.
[Class-Method 4.1d] // Экспорт данных произвел KMBIS 27/05/2015 18:39:53 БД "-db /home2/bis/quit41d/db-spbD112/bisquit,-ld bisquit,-Mm 16384,-spin 5000"
"EXCHCyber" "" "CPlatOnlImport,pays" "" "" "п" "Import" 01/01/04
"PCyber" "" "OperationImport,iwop" "" "" "п" "Import" 01/01/1900
"PCyber" "" "pck-iops" "" "" "с" "ImportSet" 01/01/1900
"PCyber" "" "pck-op-uim" "" "" "с" "ImpUndo" 01/01/1900
"PSysError" "" "ChkOpRef,pays" "" "onlp001" "с" "validate" 01/01/1900
"EXCHCyberRee" "" "i-posone" "" "" "п" "Import" 01/01/04
"SepCPlatRee" "" "CPlatReeIni,pays" "" "" "с" "Initial" 01/01/1900
"SepCPlatRee" "" "CPlatReeImp,pays" "" "" "с" "Import" 01/01/1900
"PCyberRee" "" "ree-onl-uim" "" "" "с" "ImpUndo" 01/01/1900
"SepCPlatRee" "" "CPlatReeCalc,pays" "" "" "с" "ShutDown" 01/01/1900
"EXCHUSIBREE" "" "crdiucs" "" "" "п" "Import" 01/01/04
"SepUSIBReeDT" "" "USIBReeImpDT,pays" "" "" "с" "Import" 01/01/1900
"PUsibRee" "" "pck-op-uim" "" "" "с" "ImpUndo" 01/01/1900
"SepUSIBReeFF" "" "USIBReeImpFF,pays" "" "" "с" "Import" 01/01/1900
"SepUSIBReeFH" "" "USIBReeImpDT,pays" "" "" "с" "Import" 01/01/1900
"PUsibOpCr" "" "OperationImport,iwop" "" "" "п" "Import" 01/01/1900
"EXCHRapidRee" "" "crdiresp" "" "" "п" "Import" 01/01/04
"PRapidRee" "" "file-exp" "" "" "п" "Export" 01/01/04
"SepRapidReeDT" "" "RapidReeImpDT,pays" "" "" "с" "Import" 01/01/1900
"SepRapidReeFF" "" "RapidReeImpFF,pays" "" "" "с" "Import" 01/01/1900
"SepRapidReeFH" "" "RapidReeImpFH,pays" "" "" "с" "Import" 01/01/1900
"PRapidRee" "" "pck-op-uim" "" "" "с" "ImpUndo" 01/01/1900
"PRapidOpCr" "" "OperationImport,iwop" "" "" "п" "Import" 01/01/1900
"PRapidOpCr" "" "pck-iops" "" "" "с" "ImportSet" 01/01/1900
"EXCHRapidRee" "" "RapidReeOutDT,pays" "" "" "с" "ExportSet" 01/01/1900
"EXCHGCrownRee" "" "i-gcrown" "" "" "п" "Import" 01/01/04
"GoldCrownReeFF" "" "CRownReeImpFF,pays" "" "" "с" "Import" 01/01/1900
"GoldCrownReeFH" "" "CRownReeImpFH,pays" "" "" "с" "Import" 01/01/1900
"GoldCrownReeDT" "" "CRownReeImpDT,pays" "" "" "с" "Import" 01/01/1900
"PGCrownPay" "" "OperationImport,iwop" "" "" "п" "Import" 01/01/1900
"PGCrownRee" "" "pck-op-uim" "" "" "с" "ImpUndo" 01/01/1900
"SepUSIBReeFH" "" "USIBReeIniFH,pays" "" "" "с" "Initial" 01/01/1900
.
