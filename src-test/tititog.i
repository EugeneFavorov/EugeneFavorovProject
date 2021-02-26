/* инклюдник для расчета итогов дня по отделению                     */
/* принимает 2 параметра city-id и kko для поиска итогов по отделению*/

/* обнуляем переменные*/
ASSIGN
    mSum-b-all       = 0.00 /* общая сумма баланс*/
    mSum-o-all       = 0.00 /* общая сумма внебаланс*/
    mSum-val-b-all   = 0.00 /* общая сумма валюта баланс*/
    mSum-val-o-all   = 0.00 /* общая сумма валюта внебаланс*/

    mSum-e-b-all     = 0.00 /* общая сумма электронно баланс*/
    mSum-e-o-all     = 0.00 /* общая сумма электронно внебаланс*/
    mSum-e-val-b-all = 0.00 /* общая сумма электронно валюта баланс*/
    mSum-e-val-o-all = 0.00 /* общая сумма электронно валюта внебаланс*/

    mKas-b-all       = 0.00 /* общая сумма кассовые баланс*/
    mKas-o-all       = 0.00 /* общая сумма кассовые внебаланс*/
    mKas-val-b-all   = 0.00 /* общая сумма кассовые валюта баланс*/
    mKas-val-o-all   = 0.00 /* общая сумма кассовые валюта внебаланс*/

    mDd-b-all       = 0.00 /* общая сумма документы дня баланс*/
    mDd-o-all       = 0.00 /* общая сумма документы дня внебаланс*/
    mDd-val-b-all   = 0.00 /* общая сумма документы дня валюта баланс*/
    mDd-val-o-all   = 0.00 /* общая сумма документы дня валюта внебаланс*/ 

    mAg-p-b-all      = 0.00 /* общая сумма агентские бумага баланс*/
    mAg-p-o-all      = 0.00 /* общая сумма агентские бумага внебаланс*/
    mAg-p-val-b-all  = 0.00 /* общая сумма агентские бумага валюта баланс*/
    mAg-p-val-o-all  = 0.00 /* общая сумма агентские бумага валюта внебаланс*/

    mOt-p-b-all      = 0.00 /* общая сумма отчет бумага баланс*/
    mOt-p-o-all      = 0.00 /* общая сумма отчет бумага внебаланс*/
    mOt-p-val-b-all  = 0.00 /* общая сумма отчет бумага валюта баланс*/
    mOt-p-val-o-all  = 0.00 /* общая сумма отчет бумага валюта внебаланс*/

    mHoz-p-b-all     = 0.00 /* общая сумма хоз.дог. бумага баланс*/
    mHoz-p-o-all     = 0.00 /* общая сумма хоз.дог. бумага внебаланс*/
    mHoz-p-val-b-all = 0.00 /* общая сумма хоз.дог. бумага валюта баланс*/
    mHoz-p-val-o-all = 0.00 /* общая сумма хоз.дог. бумага валюта внебаланс*/

    mAg-e-b-all      = 0.00 /* общая сумма агентские электронно баланс*/
    mAg-e-o-all      = 0.00 /* общая сумма агентские электронно внебаланс*/
    mAg-e-val-b-all  = 0.00 /* общая сумма агентские электронно валюта баланс*/
    mAg-e-val-o-all  = 0.00 /* общая сумма агентские электронно валюта внебаланс*/

    mOt-e-b-all      = 0.00 /* общая сумма отчет электронно баланс*/
    mOt-e-o-all      = 0.00 /* общая сумма отчет электронно внебаланс*/
    mOt-e-val-b-all  = 0.00 /* общая сумма отчет электронно валюта баланс*/
    mOt-e-val-o-all  = 0.00 /* общая сумма отчет электронно валюта внебаланс*/

    mHoz-e-b-all     = 0.00 /* общая сумма хоз.дог. электронно баланс*/
    mHoz-e-o-all     = 0.00 /* общая сумма хоз.дог. электронно внебаланс*/
    mHoz-e-val-b-all = 0.00 /* общая сумма хоз.дог. электронно валюта баланс*/
    mHoz-e-val-o-all = 0.00 /* общая сумма хоз.дог. электронно валюта внебаланс*/

    mBuh-b-all       = 0.00 /* общая сумма бугалтерские баланс*/
    mBuh-o-all       = 0.00 /* общая сумма бугалтерские внебаланс*/
    mBuh-val-b-all   = 0.00 /* общая сумма бугалтерские валюта баланс*/
    mBuh-val-o-all   = 0.00 /* общая сумма бугалтерские валюта внебаланс*/

    mBuh-aut-b-all   = 0.00 /* общая сумма автоматические транзакции баланс*/
    mBuh-aut-o-all   = 0.00 /* общая сумма автоматические транзакции внебаланс*/
    mBuh-auv-b-all   = 0.00 /* общая сумма автоматические транзакции валюта баланс*/
    mBuh-auv-o-all   = 0.00 /* общая сумма автоматические транзакции валюта внебаланс*/
    .

/*рассчитываем итоги*/

/* общие итоги */
FOR EACH tt-day-itog 
    WHERE tt-day-itog.acct-cat EQ "b"
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mSum-b-all = mSum-b-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog
    WHERE tt-day-itog.acct-cat EQ "o"
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mSum-o-all = mSum-o-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog 
    WHERE tt-day-itog.acct-cat EQ "b"
    AND   tt-day-itog.currency EQ 'VAL'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mSum-val-b-all = mSum-val-b-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog
    WHERE tt-day-itog.acct-cat EQ "o"
    AND   tt-day-itog.currency EQ 'VAL'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mSum-val-o-all = mSum-val-o-all + tt-day-itog.itog.
END.
/**/


/* общие итоги электронно */
FOR EACH tt-day-itog 
    WHERE tt-day-itog.acct-cat EQ "b"
    AND   tt-day-itog.save-type EQ 'e'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mSum-e-b-all = mSum-e-b-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog
    WHERE tt-day-itog.acct-cat EQ "o"
    AND   tt-day-itog.save-type EQ 'e'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mSum-e-o-all = mSum-e-o-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog 
    WHERE tt-day-itog.acct-cat EQ "b"
    AND   tt-day-itog.save-type EQ 'e'
    AND   tt-day-itog.currency EQ 'VAL'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mSum-e-val-b-all = mSum-e-val-b-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog
    WHERE tt-day-itog.acct-cat EQ "o"
    AND   tt-day-itog.save-type EQ 'e'
    AND   tt-day-itog.currency EQ 'VAL'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mSum-e-val-o-all = mSum-e-val-o-all + tt-day-itog.itog.
END.
/**/


/* кассовые документы */
FOR EACH tt-day-itog 
    WHERE tt-day-itog.acct-cat EQ "b"
    AND   tt-day-itog.razdel EQ 'k'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mKas-b-all = mKas-b-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog
    WHERE tt-day-itog.acct-cat EQ "o"
    AND   tt-day-itog.razdel EQ 'k'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mKas-o-all = mKas-o-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog 
    WHERE tt-day-itog.acct-cat EQ "b"
    AND   tt-day-itog.razdel EQ 'k'
    AND   tt-day-itog.currency EQ 'VAL'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mKas-val-b-all = mKas-val-b-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog
    WHERE tt-day-itog.acct-cat EQ "o"
    AND   tt-day-itog.razdel EQ 'k'
    AND   tt-day-itog.currency EQ 'VAL'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mKas-val-o-all = mKas-val-o-all + tt-day-itog.itog.
END.
/**/


/* документы дня */
FOR EACH tt-day-itog 
    WHERE tt-day-itog.acct-cat EQ "b"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'p'
    AND   tt-day-itog.shiv EQ 'Докдн'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mDd-b-all = mDd-b-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog
    WHERE tt-day-itog.acct-cat EQ "o"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'p'
    AND   tt-day-itog.shiv EQ 'Докдн'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mDd-o-all = mDd-o-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog 
    WHERE tt-day-itog.acct-cat EQ "b"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'p'
    AND   tt-day-itog.shiv EQ 'Докдн'
    AND   tt-day-itog.currency EQ 'VAL'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mDd-val-b-all = mDd-val-b-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog
    WHERE tt-day-itog.acct-cat EQ "o"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'p'
    AND   tt-day-itog.shiv EQ 'Докдн'
    AND   tt-day-itog.currency EQ 'VAL'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mDd-val-o-all = mDd-val-o-all + tt-day-itog.itog.
END.
/**/


/* агентские бумага */
FOR EACH tt-day-itog 
    WHERE tt-day-itog.acct-cat EQ "b"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'p'
    AND   tt-day-itog.shiv EQ 'Агент'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mAg-p-b-all = mAg-p-b-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog
    WHERE tt-day-itog.acct-cat EQ "o"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'p'
    AND   tt-day-itog.shiv EQ 'Агент'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mAg-p-o-all = mAg-p-o-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog 
    WHERE tt-day-itog.acct-cat EQ "b"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'p'
    AND   tt-day-itog.shiv EQ 'Агент'
    AND   tt-day-itog.currency EQ 'VAL'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mAg-p-val-b-all = mAg-p-val-b-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog
    WHERE tt-day-itog.acct-cat EQ "o"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'p'
    AND   tt-day-itog.shiv EQ 'Агент'
    AND   tt-day-itog.currency EQ 'VAL'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mAg-p-val-o-all = mAg-p-val-o-all + tt-day-itog.itog.
END.
/**/


/*подотчетные суммы бумага*/
FOR EACH tt-day-itog 
    WHERE tt-day-itog.acct-cat EQ "b"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'p'
    AND   tt-day-itog.shiv EQ 'Подотчет'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mOt-p-b-all = mOt-p-b-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog
    WHERE tt-day-itog.acct-cat EQ "o"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'p'
    AND   tt-day-itog.shiv EQ 'Подотчет'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mOt-p-o-all = mOt-p-o-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog 
    WHERE tt-day-itog.acct-cat EQ "b"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'p'
    AND   tt-day-itog.shiv EQ 'Подотчет'
    AND   tt-day-itog.currency EQ 'VAL'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mOt-p-val-b-all = mOt-p-val-b-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog
    WHERE tt-day-itog.acct-cat EQ "o"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'p'
    AND   tt-day-itog.shiv EQ 'Подотчет'
    AND   tt-day-itog.currency EQ 'VAL'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mOt-p-val-o-all = mOt-p-val-o-all + tt-day-itog.itog.
END.
/**/


/*хоздокументы бумага*/
FOR EACH tt-day-itog 
    WHERE tt-day-itog.acct-cat EQ "b"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'p'
    AND   tt-day-itog.shiv EQ 'Хоздок'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mHoz-p-b-all = mHoz-p-b-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog
    WHERE tt-day-itog.acct-cat EQ "o"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'p'
    AND   tt-day-itog.shiv EQ 'Хоздок'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mHoz-p-o-all = mHoz-p-o-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog 
    WHERE tt-day-itog.acct-cat EQ "b"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'p'
    AND   tt-day-itog.shiv EQ 'Хоздок'
    AND   tt-day-itog.currency EQ 'VAL'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mHoz-p-val-b-all = mHoz-p-val-b-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog
    WHERE tt-day-itog.acct-cat EQ "o"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'p'
    AND   tt-day-itog.shiv EQ 'Хоздок'
    AND   tt-day-itog.currency EQ 'VAL'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mHoz-p-val-o-all = mHoz-p-val-o-all + tt-day-itog.itog.
END.
/**/


/*агентские электронно*/
FOR EACH tt-day-itog 
    WHERE tt-day-itog.acct-cat EQ "b"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'e'
    AND   tt-day-itog.shiv EQ 'Агент'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mAg-e-b-all = mAg-e-b-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog
    WHERE tt-day-itog.acct-cat EQ "o"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'e'
    AND   tt-day-itog.shiv EQ 'Агент'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mAg-e-o-all = mAg-e-o-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog 
    WHERE tt-day-itog.acct-cat EQ "b"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'e'
    AND   tt-day-itog.shiv EQ 'Агент'
    AND   tt-day-itog.currency EQ 'VAL'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mAg-e-val-b-all = mAg-e-val-b-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog
    WHERE tt-day-itog.acct-cat EQ "o"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'e'
    AND   tt-day-itog.shiv EQ 'Агент'
    AND   tt-day-itog.currency EQ 'VAL'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mAg-e-val-o-all = mAg-e-val-o-all + tt-day-itog.itog.
END.
/**/


/*подотчетные электронно*/
FOR EACH tt-day-itog 
    WHERE tt-day-itog.acct-cat EQ "b"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'e'
    AND   tt-day-itog.shiv EQ 'Подотчет'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mOt-e-b-all = mOt-e-b-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog
    WHERE tt-day-itog.acct-cat EQ "o"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'e'
    AND   tt-day-itog.shiv EQ 'Подотчет'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mOt-e-o-all = mOt-e-o-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog 
    WHERE tt-day-itog.acct-cat EQ "b"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'e'
    AND   tt-day-itog.shiv EQ 'Подотчет'
    AND   tt-day-itog.currency EQ 'VAL'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mOt-e-val-b-all = mOt-e-val-b-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog
    WHERE tt-day-itog.acct-cat EQ "o"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'e'
    AND   tt-day-itog.shiv EQ 'Подотчет'
    AND   tt-day-itog.currency EQ 'VAL'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mOt-e-val-o-all = mOt-e-val-o-all + tt-day-itog.itog.
END.
/**/


/*хоздок электронно*/
FOR EACH tt-day-itog 
    WHERE tt-day-itog.acct-cat EQ "b"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'e'
    AND   tt-day-itog.shiv EQ 'Хоздок'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mHoz-e-b-all = mHoz-e-b-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog
    WHERE tt-day-itog.acct-cat EQ "o"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'e'
    AND   tt-day-itog.shiv EQ 'Хоздок'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mHoz-e-o-all = mHoz-e-o-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog 
    WHERE tt-day-itog.acct-cat EQ "b"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'e'
    AND   tt-day-itog.shiv EQ 'Хоздок'
    AND   tt-day-itog.currency EQ 'VAL'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mHoz-e-val-b-all = mHoz-e-val-b-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog
    WHERE tt-day-itog.acct-cat EQ "o"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'e'
    AND   tt-day-itog.shiv EQ 'Хоздок'
    AND   tt-day-itog.currency EQ 'VAL'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mHoz-e-val-o-all = mHoz-e-val-o-all + tt-day-itog.itog.
END.
/**/


/* бухгалтерские документы */
FOR EACH tt-day-itog 
    WHERE tt-day-itog.acct-cat EQ "b"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'e'
    AND   tt-day-itog.shiv EQ 'Докдн'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mBuh-b-all = mBuh-b-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog
    WHERE tt-day-itog.acct-cat EQ "o"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'e'
    AND   tt-day-itog.shiv EQ 'Докдн'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mBuh-o-all = mBuh-o-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog 
    WHERE tt-day-itog.acct-cat EQ "b"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'e'
    AND   tt-day-itog.shiv EQ 'Докдн'
    AND   tt-day-itog.currency EQ 'VAL'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mBuh-val-b-all = mBuh-val-b-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog
    WHERE tt-day-itog.acct-cat EQ "o"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'e'
    AND   tt-day-itog.shiv EQ 'Докдн'
    AND   tt-day-itog.currency EQ 'VAL'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mBuh-val-o-all = mBuh-val-o-all + tt-day-itog.itog.
END.
/**/


/* автоматические транзакции */
/*
FOR EACH tt-day-itog 
    WHERE tt-day-itog.acct-cat EQ "b"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'e'
    AND   tt-day-itog.shiv EQ 'Докдн'
    AND   tt-day-itog.auto-t
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mBuh-aut-b-all = mBuh-aut-b-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog
    WHERE tt-day-itog.acct-cat EQ "o"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'e'
    AND   tt-day-itog.shiv EQ 'Докдн'
    AND   tt-day-itog.auto-t
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mBuh-aut-o-all = mBuh-aut-o-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog 
    WHERE tt-day-itog.acct-cat EQ "b"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'e'
    AND   tt-day-itog.shiv EQ 'Докдн'
    AND   tt-day-itog.auto-t
    AND   tt-day-itog.currency EQ 'VAL'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
   :
    mBuh-auv-b-all = mBuh-auv-b-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog
    WHERE tt-day-itog.acct-cat EQ "o"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'e'
    AND   tt-day-itog.shiv EQ 'Докдн'
    AND   tt-day-itog.auto-t
    AND   tt-day-itog.currency EQ 'VAL'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mBuh-auv-o-all = mBuh-auv-o-all + tt-day-itog.itog.
END.*/
/**/