/* Ручное формирвоание WHERE выражения.*/
PROCEDURE AcctManual.

   DEF PARAM BUFFER flt-attr FOR flt-attr.   /* Описание поля. */
   DEF OUTPUT PARAM MyWhere AS CHAR NO-UNDO. /* WHERE выражение для поля. */

   CASE flt-attr.attr-basic:
      WHEN "cust-id"
      THEN DO:
         IF GetFltVal ("cust-id") NE "Список" THEN
         DO:
            MyWhere = StdWhere(recid(flt-attr),YES).
  
            IF LOOKUP ("EQ", MyWhere, " ") EQ 0
            THEN MyWhere = DYNAMIC-FUNCTION ("GetDynWhr" IN h_dynqr,
                              "",
                              "STRING (acct." + flt-attr.attr-basic + ")",
                              flt-attr.attr-code-value + CHR (1) + CHR (1),
                              flt-attr.attr-datatype).
         END.
      END.
   END CASE.

   RETURN.
END PROCEDURE.

/* Установка номера фрейма и заголовка */
PROCEDURE Select-Browse:
   
   ASSIGN
      n-frm =  INT64 (GetFltVal ('view-type')) + 1
      vDate =  IF n-frm EQ 5 OR n-frm EQ 6 OR n-frm EQ 8
                  THEN mSldDate
                  ELSE closedate
      h-frm[n-frm]:TITLE = IF GetFltVal("extra-title") EQ ""
                           THEN "[ ЛИЦЕВЫЕ СЧЕТА, ОСТАТОК ЗА " + STRING(vDate)
                                 + " (~"" + ENTRY(3,user-config-info) + "~") ]"
                           ELSE "[ " + GetFltVal("extra-title") + " ]"
   .
   RETURN.
END PROCEDURE.

/* Процедура отображения остатка по счету. */
PROCEDURE b-acct_fnd:

   DEF VAR vAPRight AS LOG    NO-UNDO. /* Право просматра остатка. */
   DEF VAR mColor   AS CHAR   NO-UNDO.
                        /* Получение флага по правам доступа. */
   vAPRight = {acctlook.i}. 
                        
            /* Формирование цвета для валютного остатка. */
   IF     AVAIL acct-cur
      AND (    acct.close-date GE vDate
           OR  acct.close-date EQ ?)
   THEN  /* Формирование цвета для рублевого остатка и счета.  */
      COLOR DISPLAY
         VALUE (GetBalColorBuffer ((BUFFER acct:HANDLE), acct-cur.balance))
         acct-cur.balance
      WITH FRAME browse2.

   IF     AVAIL acct-pos
      AND (    acct.close-date GE vDate
           OR  acct.close-date EQ ?)
   THEN 
      COLOR DISPLAY
         VALUE (GetBalColorBuffer ((BUFFER acct:HANDLE), acct-pos.balance))
         acct-pos.balance
      WITH FRAME browse2.

   COLOR DISPLAY
      VALUE (GetAcctColorBuffer(BUFFER acct:HANDLE,vDate))
      mAcct
   WITH FRAME browse2.
                        /* Отображение данных. */
   DISPLAY
      mAcct
      acct-cur.balance
         FORMAT ">>>>>>,>>>,>>9.99 Д "
      WHEN AVAIL acct-cur
         AND acct-cur.balance GT 0
         AND vAPRight
      @ acct-cur.balance

      - acct-cur.balance
         FORMAT ">>>>>>,>>>,>>9.99  К"
         WHEN  AVAIL acct-cur
            AND acct-cur.balance LT 0
            AND vAPRight
      @ acct-cur.balance

      ""
         WHEN acct.close-date EQ ? @ acct-pos.balance

      acct-pos.balance
         FORMAT ">>>>>>,>>>,>>>,>>9.99 Д "
         WHEN  AVAILABLE acct-pos 
            AND acct-pos.balance GT 0 
            AND vAPRight
         @ acct-pos.balance

      - acct-pos.balance
         FORMAT ">>>>>>,>>>,>>>,>>9.99  К"
         WHEN  AVAILABLE acct-pos
            AND acct-pos.balance LT 0
            AND vAPRight
         @ acct-pos.balance

      "ЗАКРЫТ"
         WHEN     acct.close-date NE ?
              AND NOT mColCloAc
         @ acct-pos.balance

      "       НЕТ ДОПУСКА"
         WHEN NOT vAPRight
      @ acct-pos.balance
   WITH FRAME browse2.
END.

/* Отображение остатка по счету. */
PROCEDURE APDisp PRIVATE.
   DEF VAR vAPRight AS LOG    NO-UNDO. /* Право просматра остатка. */
                        /* Получение флага по правам доступа. */
   vAPRight = {acctlook.i}.
                        /* Формирование цвета для валютного остатка. */
   IF mPosVal NE 0
   THEN COLOR DISPLAY
      VALUE (GetBalColorBuffer ((BUFFER acct:HANDLE), mPosVal))
      mPosVal
   WITH FRAME browse5.
                        /* Формирование цвета для рублевого остатка. */
   IF mPosBal NE 0
   THEN COLOR DISPLAY
      VALUE (GetBalColorBuffer ((BUFFER acct:HANDLE), mPosBal))
      mPosBal
   WITH FRAME browse5.
   COLOR DISPLAY
      VALUE (GetAcctColorBuffer(BUFFER acct:HANDLE,vDate))
      mAcct
   WITH FRAME browse5.
                        /* Отображение данных. */
   DISPLAY
      mAcct

      mPosVal
         FORMAT ">>>>>>,>>>,>>9.99 Д "
      WHEN mPosVal GT 0
         AND vAPRight
      @ mPosVal

      - mPosVal
         FORMAT ">>>>>>,>>>,>>9.99  К"
         WHEN  mPosVal LT 0
            AND vAPRight
      @ mPosVal

      ""
         WHEN acct.close-date EQ ? @ mPosBal

      mPosBal
         FORMAT ">>>>>>,>>>,>>>,>>9.99 Д "
         WHEN  mPosBal GT 0
            AND vAPRight
         @ mPosBal

      - mPosBal
         FORMAT ">>>>>>,>>>,>>>,>>9.99  К"
         WHEN  mPosBal LT 0
            AND vAPRight
         @ mPosBal

      "ЗАКРЫТ"
         WHEN      acct.close-date NE ?
               AND NOT mColCloAc
         @ mPosBal

      "       НЕТ ДОПУСКА"
         WHEN NOT vAPRight
      @ mPosBal
   WITH FRAME browse5.
   RETURN.
END PROCEDURE.

/* Отображение остатка по счету. */
PROCEDURE APDisp8 PRIVATE.
   DEF VAR vAPRight AS LOG    NO-UNDO. /* Право просматра остатка. */
                        /* Получение флага по правам доступа. */
   vAPRight = {acctlook.i}. 
                        /* Формирование цвета для валютного остатка. */
   IF mAvVal NE 0 THEN
    COLOR DISPLAY
      VALUE (GetBalColorBuffer ((BUFFER acct:HANDLE), mAvVal))
      mAvVal
   WITH FRAME browse8.

                        /* Формирование цвета для рублевого остатка. */
   IF mAvBal NE 0
   THEN 
   COLOR DISPLAY
      VALUE (GetBalColorBuffer ((BUFFER acct:HANDLE), mAvBal))
      mAvBal
   WITH FRAME browse8.

   COLOR DISPLAY
      VALUE (GetAcctColorBuffer(BUFFER acct:HANDLE,vDate))
      mAcct
   WITH FRAME browse8.

                        /* Отображение данных. */
   DISPLAY
      mAcct

      mAvVal
         FORMAT ">>>>>>,>>>,>>9.99 Д "
      WHEN mAvVal GT 0
         AND vAPRight
      @ mAvVal

      - mAvVal
         FORMAT ">>>>>>,>>>,>>9.99  К"
         WHEN  mAvVal LT 0
            AND vAPRight
      @ mAvVal

      ""
         WHEN acct.close-date EQ ? @ mAvBal

      mAvBal
         FORMAT ">>>>>>,>>>,>>>,>>9.99 Д "
         WHEN  mAvBal GT 0
            AND vAPRight
         @ mAvBal

      - mAvBal
         FORMAT ">>>>>>,>>>,>>>,>>9.99  К"
         WHEN  mAvBal LT 0
            AND vAPRight
         @ mAvBal

      "ЗАКРЫТ"
         WHEN      acct.close-date NE ?
               AND NOT mColCloAc
         @ mAvBal

      "       НЕТ ДОПУСКА"
         WHEN NOT vAPRight
      @ mAvBal
   WITH FRAME browse8.
   RETURN.
END PROCEDURE.

/* Отображение остатка по счету. */
PROCEDURE APDisp6 PRIVATE.
   DEF VAR vAPRight AS LOG    NO-UNDO. /* Право просматра остатка. */
                        /* Получение флага по правам доступа. */
   vAPRight = {acctlook.i}.
                        /* Формирование цвета для рублевого остатка. */
   IF mPosQty NE 0 THEN
      COLOR DISPLAY VALUE (GetBalColorBuffer ((BUFFER acct:HANDLE), mPosQty)) mPosQty
   WITH FRAME browse6.
   COLOR DISPLAY
      VALUE (GetAcctColorBuffer(BUFFER acct:HANDLE,vDate))
      mAcct
   WITH FRAME browse6.
                        /* Отображение данных. */
   DISPLAY
      mAcct

      mPosQty
         WHEN     mPosQty GT 0
            AND   vAPRight
         @ mPosQty
      - mPosQty
         WHEN     mPosQty LT 0
            AND   vAPRight
         @ mPosQty
      mPosBal
         FORMAT ">>>>>>,>>>,>>>,>>9.99 Д"
         WHEN     mPosBal GT 0
            AND   vAPRight
         @ mPosBal
      - mPosBal
         FORMAT ">>>>>>,>>>,>>>,>>9.99 К" 
         WHEN     mPosBal LT 0
            AND   vAPRight
         @ mPosBal
      "ЗАКРЫТ"
         WHEN     acct.close-date NE ?
              AND NOT mColCloAc
         @ mPosBal
      "       НЕТ ДОПУСКА"
         WHEN NOT vAPRight
      @ mPosBal
   WITH FRAME browse6.
   RETURN.
END PROCEDURE.


/* Процедура дополнительной проверки при изменении значений основных/дополнительных реквизитов
   с использованием мех-ма Tmprecid */
PROCEDURE pAddProcCheck:
   DEFINE INPUT PARAMETER iClass   AS CHARACTER NO-UNDO. /* Класс объекта. */         
   DEFINE INPUT PARAMETER iCode    AS CHARACTER NO-UNDO. /* Код реквизита. */         
   DEFINE INPUT PARAMETER iSurrObj AS CHARACTER NO-UNDO. /* Идентификатор объекта. */ 
   DEFINE INPUT PARAMETER iValue   AS CHARACTER NO-UNDO. /* Значение реквизита. */    
   DEFINE INPUT PARAMETER iBaseVal AS CHARACTER NO-UNDO. /* Значение реквизита,к которому осуществляется привязка. */    
    
   DEF BUFFER acct FOR acct. /* Локализация буфера. */
   DEFINE VARIABLE vAcct     AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vCurrency AS CHARACTER  NO-UNDO.

   ASSIGN 
      vAcct     = ENTRY(1,iSurrObj)
      vCurrency = ENTRY(2,iSurrObj)
   NO-ERROR.

   IF ERROR-STATUS:ERROR THEN
      RETURN ERROR.

   {find-act.i
      &acct = vAcct
      &curr = vCurrency
   }   

   IF NOT AVAILABLE acct THEN
      RETURN ERROR.
   CASE iCode:
      WHEN "kau-id" THEN DO:
         IF     {assigned acct.kau-id}
            AND acct.kau-id NE iBaseVal THEN
            RETURN "Этот счет используется с другим шаблоном!".
      END.
   END CASE.

   RETURN "".

END PROCEDURE.

/* действия выполняемые после применения фильтра
** вынесены из CalcVar (acctbrw.cv), для ускорения,
** т.к. эти действия не требуется выполнять для каждой записи */
PROCEDURE AcctAfterFilter:
   RUN PostSelectQuery.
   /* Проверка на корректность задания условий фильтрации.
   ** Если задана хотя бы одна из границ интервалов суммы оборотов,
   ** но не задан интервал дат, то при таких условиях фильтрация невозможна.
   ** Выдаем сообщение об этом и записываем в vFldReturn код поля фильтра
   ** и движок фильтра запускает заново форму фильтра и встает на указанное поле */
   IF     NOT vFiltTurn
      AND (   IsFieldChange("sh-v1")
           OR IsFieldChange("sh-v2")
           OR IsFieldChange("sh-b1")
           OR IsFieldChange("sh-b2"))
   THEN DO:
      RUN Fill-SysMes IN h_tmess ("", "", "0", "Для фильтрации по оборотам задайте обе даты интервала для оборотов.").
      vFldReturn = "TurnDate1".
   END.
   RETURN.
END PROCEDURE.

PROCEDURE PostSelectQuery:
   /* Обнуление флагов фильтрации. */
   ASSIGN
      vFiltTurn   = NO
      vFiltSld    = NO
   .
   ASSIGN
      /* осуществлять ли фильтрацию по оборотам? (если пользователь задал
      ** даты начала и конца интервала расчета оборотов в фильтре
      ** и хотя бы одну из границ интервалов значений оборотов */
      vFiltTurn =       IsFieldChange("TurnDate1")
                  AND   IsFieldChange("TurnDate2")
                  AND (   IsFieldChange("sh-v1")
                       OR IsFieldChange("sh-v2")
                       OR IsFieldChange("sh-b1")
                       OR IsFieldChange("sh-b2"))
      /* осуществлять ли фильтрацию по остаткам? (если пользователем задано
      ** хотя бы одно поле фильтра, относящееся к разделу фильтрации по остаткам */
      vFiltSld =     IsFieldChange("SldDate")      /* "Остатки на:" */
                  OR IsFieldChange("SldType")      /* "Тип сальдо:" */
                  OR IsFieldChange("mPosVal1")     /* "Сальдо в ин.вал. ОТ:" */
                  OR IsFieldChange("mPosVal2")     /* "Сальдо в ин.вал. ДО:" */
                  OR IsFieldChange("mPosBal1")     /* "Сальдо в нац.вал. ОТ:" */
                  OR IsFieldChange("mPosBal2")     /* "Сальдо в нац.вал. ДО:" */
      /* дата начала интервала расчета оборотов по счетам (из фильтра) */
      mTurnBeg = DATE(GetFltVal("TurnDate1"))
      /* дата окончания интервала расчета оборотов по счетам (из фильтра) */
      mTurnEnd = DATE(GetFltVal("TurnDate2"))
      /* тип оборотов */
      mTurnType = GetFltVal("TurnType")
      mSldDate = DATE(GetFltVal("SldDate"))
      mGrpFltLst = GetFltValEx("GroupList","*")     /* список групп счетов */
      mFltGrpType = GetFltValEx("GroupFltType","any")  /* способ фильтрации по группам */
   NO-ERROR.
                        /* Включены ли ограничения на чтение групп счетов */
   mAcctGrOn = UserAcctGroupLimitsEnabled ('r', USERID("bisquit")).
                        /* Заполняем врем.таблицу для фильтрации по группам счетов (только для админов) */
   IF     mBrwRole EQ "Group"
      /*AND IsUserAdm(USERID("bisquit"))*/ THEN
   RUN FillttGroupAcct.
END PROCEDURE.

PROCEDURE APDisp9 PRIVATE.
  DEF VAR vAPRight AS LOG    NO-UNDO. /* Право просматра остатка. */
                        /* Получение флага по правам доступа. */
   vAPRight = {acctlook.i}.
                        /* Формирование цвета для валютного остатка. */

  COLOR DISPLAY
      VALUE (GetAcctColorBuffer(BUFFER acct:HANDLE,vDate))
      mAcct
   WITH FRAME browse9.    
                        /* Отображение данных. */
   DISPLAY
      mAcct
      currency
      mDbTurnVal WHEN mTurnBeg NE ? AND mTurnEnd NE ? AND vAPRight  @ mDbTurnVal
      mCrTurnVal WHEN mTurnBeg NE ? AND mTurnEnd NE ? AND vAPRight  @ mCrTurnVal
      mDbTurn    WHEN mTurnBeg NE ? AND mTurnEnd NE ? AND vAPRight  @ mDbTurn
      mCrTurn    WHEN mTurnBeg NE ? AND mTurnEnd NE ? AND vAPRight  @ mCrTurn
      ""         WHEN mTurnBeg EQ ? OR  mTurnEnd EQ ? @ mDbTurnVal
      ""         WHEN mTurnBeg EQ ? OR  mTurnEnd EQ ? @ mCrTurnVal
      ""         WHEN mTurnBeg EQ ? OR  mTurnEnd EQ ? @ mDbTurn
      ""         WHEN mTurnBeg EQ ? OR  mTurnEnd EQ ? @ mCrTurn

   WITH FRAME browse9.
   RETURN.
END PROCEDURE.

/* Заполнение временной таблицы ROWID'ов счетов при фильтрации по группам доступа */
PROCEDURE FillttGroupAcct PRIVATE.
   DEF VAR vGroupList    AS CHAR   NO-UNDO.
   DEF VAR vi            AS INT64  NO-UNDO.
   DEF VAR vOK           AS LOG    NO-UNDO.
   DEF VAR vLinkID       AS INT64  NO-UNDO.

   DEF BUFFER flt-attr FOR flt-attr.
   DEF BUFFER acct     FOR acct.
   DEF BUFFER links    FOR links.

   EMPTY TEMP-TABLE LocalTmpObj.
                        /* Если заданы группы для фильтрации */
   IF     mBrwRole   EQ "Group"
      AND mGrpFltLst NE "*"
   THEN DO:
      vLinkID = GetXLinkID("acct","acct-group").
      FOR EACH links WHERE links.link-id   EQ vLinkID
                       AND links.target-id EQ mGrpFltLst 
      NO-LOCK,
      FIRST acct WHERE acct.acct     EQ ENTRY (1,links.source-id)
                   AND acct.currency EQ ENTRY (2,links.source-id)
      NO-LOCK:
                              /* "Отбирать счета принадлежащие: (X)Только указанным группам" -
                              ** Все группы счета принадлежат списку */
         IF mFltGrpType EQ "all" THEN
         DO:
            vGroupList = GetLinks ("acct",acct.acct + "," + acct.currency,"S","acct-group",",",?).
            GRLST:
            DO vi = 1 TO NUM-ENTRIES(vGroupList):
               IF NOT CAN-DO(mGrpFltLst, ENTRY (vi,vGroupList)) THEN
               DO:
                  vGroupList = ?.
                  LEAVE GRLST.
               END.
            END.
         END.

         IF    vGroupList    NE ?
            OR mFltGrpType NE "all"
         THEN DO:
            CREATE LocalTmpObj.
            ASSIGN LocalTmpObj.rid = RECID (acct).
         END.
      END.

                        /* Устанавливаем фильтрацию по временной таблице LocalTmpObj */
      FIND FIRST flt-attr WHERE
         flt-attr.attr-code EQ "UseTmpObjInQuery"
      NO-ERROR.
      IF NOT AVAIL flt-attr THEN
      DO:
         CREATE flt-attr.
         ASSIGN
            flt-attr.attr-code      = "UseTmpObjInQuery"
            flt-attr.attr-hidden    = YES
            flt-attr.attr-sensitive = NO
            flt-attr.attr-initial   = "*"
         .
      END.
      flt-attr.attr-code-value = STRING (mLocalTmpObjHand).
                        /* Сбрасываем значение поля "Группы" для того, чтобы 
                        ** не осуществлялась фильтрация по вычисляемому полю */
      RUN SetFltField ("GroupList","*").
   END.
   RETURN.
END PROCEDURE.
