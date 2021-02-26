/*
Отчет.
Находит использование указанного во входном параметре текста в транзакциях 

Входные параметры:
1 Текст который нужно найти			
2 Модуль, транзакции которого проверяются (если не указать, то будет card)
Разделение праметров символом ";" 

Пример1: ПКДохРасх;card,base
Пример2: ПКДохРасх

*/

DEFINE INPUT PARAMETER iParams AS CHARACTER NO-UNDO .

DEFINE VARIABLE mMod 	  AS CHARACTER 	NO-UNDO.
DEFINE VARIABLE mTmpStr   AS CHARACTER 	NO-UNDO.
DEFINE VARIABLE mFullPath AS CHARACTER 	NO-UNDO.
DEFINE VARIABLE mParent   AS CHARACTER 	NO-UNDO.
DEFINE VARIABLE mAnswer   AS LOG NO-UNDO INIT YES FORMAT "Да/Нет". /* для запроса формы отчета */
DEFINE VARIABLE mInd      AS INT64 NO-UNDO. 
DEFINE VARIABLE mLen      AS INT64 NO-UNDO. 


DEFINE TEMP-TABLE tt-results NO-UNDO 
   FIELD module 	AS CHARACTER  /* Модуль */ 
   FIELD path 		AS CHARACTER  /* Папка */
   FIELD fullpath 	AS CHARACTER  /* Полный путь */
   FIELD opkind 	AS CHARACTER  /* Транзакция */
   FIELD name 		AS CHARACTER  /* Наименование транзакции */
   FIELD class 		AS CHARACTER  /* Класс транзакции */
   FIELD order		AS INT64      /* Номер шаблона транзкции */
   FIELD place 		AS CHARACTER  /* Место в транзакции */
   FIELD formula	AS CHARACTER  /* Текст (выражение) */
   .

DEFINE STREAM fout.

/* ----- */

DEF FRAME mFrOption
   mAnswer NO-LABEL
WITH TITLE " Формировать только список транзакций ? "
   SIDE-LABELS NO-UNDERLINE VIEW-AS DIALOG-BOX.

ON F1 OF mAnswer IN FRAME mFrOption
DO:
   mAnswer = NOT mAnswer.
   DISP mAnswer WITH FRAME mFrOption.
END.

/* ---- */

{globals.i}
{intrface.get tmess}
{setdest.i
   &STREAM   = "STREAM fout "
   &filename = "'search_txt.log'"
   &custom   = "if true then 0 else"
}

/* Проверки */
IF num-entries(iParams,";") LT 1  OR num-entries(iParams,";") GT 2 THEN  
  DO:
    message "Ошибка. Указано неверное количество параметров" view-as alert-box.
    RETURN.
  END.

mTmpStr = TRIM(entry(1,iParams,";")).
IF mTmpStr EQ "" THEN
  DO:
    message "Ошибка. Не задан текст для поиска" view-as alert-box.
    RETURN.
  END.

IF num-entries(iParams,";") GT 1 THEN  
   mMod = entry(2,iParams,";").
ELSE 
   mMod = "card".

/* Основной цикл */

UPDATE
   mAnswer
WITH FRAME mFrOption.


loop:
FOR EACH op-kind WHERE CAN-DO(mMod ,op-kind.module) NO-LOCK:

    /* --- Универсальные транзакции --- */

    /* Выполнить ДО транзакции */
    IF INDEX(op-kind.before,mTmpStr) GT 0 THEN
    DO:
       CREATE tt-results.
       ASSIGN
          tt-results.module   = op-kind.module
          tt-results.path     = op-kind.parent
          tt-results.opkind   = op-kind.op-kind
          tt-results.name     = op-kind.name-opkind
          tt-results.class    = op-kind.class-Code
          tt-results.place    = "Выполнить ДО транзакции"
          tt-results.formula  = TRIM(op-kind.before)

          mInd                = INDEX(op-kind.before,mTmpStr)
          mLen                = LENGTH(op-kind.before)
       .

       /* Коррекция формулы */
       IF mLen GT 600 THEN
          IF mInd LE 300 THEN  
             tt-results.formula  = SUBSTRING(tt-results.formula, 1, mInd + 300) + " ... ".
          ELSE
             IF mLen - mInd LT 300 THEN  
                tt-results.formula  = " ... " + SUBSTRING(tt-results.formula, mInd - 300, 300 + (mLen - mInd) + 1). 
             ELSE
                tt-results.formula  = " ... " + SUBSTRING(tt-results.formula, mInd - 300, 600 + 1) + " ... ". 

       IF mAnswer THEN NEXT loop.
    END.

    /* Выполнить ПОСЛЕ транзакции */
    IF INDEX(op-kind.after,mTmpStr) GT 0 THEN
    DO:
       CREATE tt-results.
       ASSIGN
          tt-results.module   = op-kind.module
          tt-results.path     = op-kind.parent
          tt-results.opkind   = op-kind.op-kind
          tt-results.name     = op-kind.name-opkind
          tt-results.class    = op-kind.class-Code
          tt-results.place    = "Выполнить ПОСЛЕ транзакции"
          tt-results.formula  = TRIM(op-kind.after)

          mInd                = INDEX(op-kind.after,mTmpStr)
          mLen                = LENGTH(op-kind.after)
       .

       /* Коррекция формулы */
       IF mLen GT 600 THEN
          IF mInd LE 300 THEN  
             tt-results.formula  = SUBSTRING(tt-results.formula, 1, mInd + 300) + " ... ".
          ELSE
             IF mLen - mInd LT 300 THEN  
                tt-results.formula  = " ... " + SUBSTRING(tt-results.formula, mInd - 300, 300 + (mLen - mInd) + 1). 
             ELSE
                tt-results.formula  = " ... " + SUBSTRING(tt-results.formula, mInd - 300, 600 + 1) + " ... ". 

       IF mAnswer THEN NEXT loop.
    END.

    FOR EACH op-kind-tmpl WHERE op-kind-tmpl.op-kind EQ op-kind.op-kind no-lock:

       /* Выполнить ДО шаблона транзакции */
       IF INDEX(op-kind-tmpl.before,mTmpStr) GT 0 THEN
       DO:
          CREATE tt-results.
          ASSIGN
             tt-results.module   = op-kind.module
             tt-results.path     = op-kind.parent
             tt-results.opkind   = op-kind.op-kind
             tt-results.name     = op-kind.name-opkind
             tt-results.class    = op-kind.class-Code
             tt-results.order    = op-kind-tmpl.order
             tt-results.place    = "Выполнить ДО шаблона транзакции"
             tt-results.formula  = TRIM(op-kind-tmpl.before)

             mInd                = INDEX(op-kind-tmpl.before,mTmpStr)
             mLen                = LENGTH(op-kind-tmpl.before)
          .

          /* Коррекция формулы */
          IF mLen GT 600 THEN
             IF mInd LE 300 THEN  
                tt-results.formula  = SUBSTRING(tt-results.formula, 1, mInd + 300) + " ... ".
             ELSE
                IF mLen - mInd LT 300 THEN  
                   tt-results.formula  = " ... " + SUBSTRING(tt-results.formula, mInd - 300, 300 + (mLen - mInd) + 1). 
                ELSE
                   tt-results.formula  = " ... " + SUBSTRING(tt-results.formula, mInd - 300, 600 + 1) + " ... ". 

          IF mAnswer THEN NEXT loop.
       END.


       /* Выполнить ПОСЛЕ шаблона транзакции */
       IF INDEX(op-kind-tmpl.after,mTmpStr) GT 0 THEN
       DO:
          CREATE tt-results.
          ASSIGN
             tt-results.module   = op-kind.module
             tt-results.path     = op-kind.parent
             tt-results.opkind   = op-kind.op-kind
             tt-results.name     = op-kind.name-opkind
             tt-results.class    = op-kind.class-Code
             tt-results.order    = op-kind-tmpl.order
             tt-results.place    = "Выполнить ПОСЛЕ шаблона транзакции"
             tt-results.formula  = TRIM(op-kind-tmpl.after)

             mInd                = INDEX(op-kind-tmpl.after,mTmpStr)
             mLen                = LENGTH(op-kind-tmpl.after)
          .

          /* Коррекция формулы */
          IF mLen GT 600 THEN
             IF mInd LE 300 THEN  
                tt-results.formula  = SUBSTRING(tt-results.formula, 1, mInd + 300) + " ... ".
             ELSE
                IF mLen - mInd LT 300 THEN  
                   tt-results.formula  = " ... " + SUBSTRING(tt-results.formula, mInd - 300, 300 + (mLen - mInd) + 1). 
                ELSE
                   tt-results.formula  = " ... " + SUBSTRING(tt-results.formula, mInd - 300, 600 + 1) + " ... ". 

          IF mAnswer THEN NEXT loop.
       END.

       FOR EACH op-kind-tmpl-ln WHERE op-kind-tmpl-ln.tmpl-id EQ op-kind-tmpl.tmpl-id NO-LOCK:

          /* Реквизит шаблона транзакции (выражение) */
          IF INDEX(op-kind-tmpl-ln.xattr-formula,mTmpStr) GT 0 THEN
          DO:
             CREATE tt-results.
             ASSIGN
                tt-results.module   = op-kind.module
                tt-results.path     = op-kind.parent
                tt-results.opkind   = op-kind.op-kind
                tt-results.name     = op-kind.name-opkind
                tt-results.class    = op-kind.class-Code
                tt-results.order    = op-kind-tmpl.order
                tt-results.place    = "Реквизит: " + op-kind-tmpl-ln.Xattr-Code
                tt-results.formula  = op-kind-tmpl-ln.xattr-formula
             .
             IF mAnswer THEN NEXT loop.
          END.
      END.
    END.


    /* --- Стандартные транзакции --- */
    FOR EACH op-template WHERE op-template.op-kind EQ op-kind.op-kind NO-LOCK:

       /* Дебет счета */ 
       IF INDEX(op-template.acct-db,mTmpStr) GT 0 THEN
       DO:
          CREATE tt-results.
          ASSIGN
             tt-results.module   = op-kind.module
             tt-results.path     = op-kind.parent
             tt-results.opkind   = op-kind.op-kind
             tt-results.name     = op-kind.name-opkind
             tt-results.class    = op-kind.class-Code
             tt-results.order    = op-template.op-template
             tt-results.place    = "Дебет счета"
             tt-results.formula  = op-template.acct-db
          .
          IF mAnswer THEN NEXT loop.
       END.

       /* Кредит счета */
       IF INDEX(op-template.acct-cr,mTmpStr) GT 0  THEN
       DO:
          CREATE tt-results.
          ASSIGN
             tt-results.module   = op-kind.module
             tt-results.path     = op-kind.parent
             tt-results.opkind   = op-kind.op-kind
             tt-results.name     = op-kind.name-opkind
             tt-results.class    = op-kind.class-Code
             tt-results.order    = op-template.op-template
             tt-results.place    = "Кредит счета"
             tt-results.formula  = op-template.acct-cr
          .
          IF mAnswer THEN NEXT loop.
       END.

       /* Расчет суммы */
       IF INDEX(op-template.amt-rub,mTmpStr) GT 0  THEN
       DO:
          CREATE tt-results.
          ASSIGN
             tt-results.module   = op-kind.module
             tt-results.path     = op-kind.parent
             tt-results.opkind   = op-kind.op-kind
             tt-results.name     = op-kind.name-opkind
             tt-results.class    = op-kind.class-Code
             tt-results.order    = op-template.op-template
             tt-results.place    = "Расчет суммы"
             tt-results.formula  = op-template.amt-rub
          .
          IF mAnswer THEN NEXT loop.
       END.

       /* Расчет эквив. */
       IF INDEX(op-template.amt-natcur,mTmpStr) GT 0  THEN
       DO:
          CREATE tt-results.
          ASSIGN
             tt-results.module   = op-kind.module
             tt-results.path     = op-kind.parent
             tt-results.opkind   = op-kind.op-kind
             tt-results.name     = op-kind.name-opkind
             tt-results.class    = op-kind.class-Code
             tt-results.order    = op-template.op-template
             tt-results.place    = "Расчет эквив."
             tt-results.formula  = op-template.amt-natcur
          .
          IF mAnswer THEN NEXT loop.
       END.

       /* Содержание */
       IF INDEX(op-template.details,mTmpStr) GT 0  THEN
       DO:
          CREATE tt-results.
          ASSIGN
             tt-results.module   = op-kind.module
             tt-results.path     = op-kind.parent
             tt-results.opkind   = op-kind.op-kind
             tt-results.name     = op-kind.name-opkind
             tt-results.class    = op-kind.class-Code
             tt-results.order    = op-template.op-template
             tt-results.place    = "Содержание"
             tt-results.formula  = op-template.details
          .
          IF mAnswer THEN NEXT loop.
       END.

/* ДР пока не смотрим ...
          /* ДР на транзакции ? */
          FOR EACH signs WHERE signs.file-name EQ "op-template"
                           AND signs.surrogate EQ op-template.op-kind + "," +
                                                  STRING(op-template.op-template) NO-LOCK:
             IF INDEX(signs.code-formula,mTmpStr) GT 0 THEN
             DO:
                CREATE tt-results.
                ASSIGN
                   tt-results.module   = op-kind.module
                   tt-results.path     = op-kind.parent
                   tt-results.opkind   = op-kind.op-kind
                   tt-results.name     = op-kind.name-opkind
                   tt-results.class    = op-kind.class-Code
                .
                IF mAnswer THEN NEXT loop.
             END.
          END.
*/

   END.

/* ДР пока не смотрим ...
   /* ДР  ? */
   FOR EACH signs WHERE signs.file-name EQ "op-kind"
                    AND signs.surrogate EQ op-kind.op-kind NO-LOCK:
      IF INDEX(signs.code-formula,mTmpStr) GT 0 THEN
      DO:
          CREATE tt-results.
          ASSIGN
             tt-results.module   = op-kind.module
             tt-results.path     = op-kind.parent
             tt-results.opkind   = op-kind.op-kind
             tt-results.name     = op-kind.name-opkind
             tt-results.class    = op-kind.class-Code
          .
         IF mAnswer THEN NEXT loop.
      END.
   END.
*/

END.

/* Выводим отчет */

IF CAN-FIND(FIRST tt-results) THEN 
   DO:

      /* Шапка */
      PUT STREAM fout UNFORMATTED "Отчет по транзакциям, содежащим заданный текст. " STRING(TODAY) SKIP(2).
      PUT STREAM fout UNFORMATTED "Настройки:" SKIP.
      PUT STREAM fout UNFORMATTED " - текст для поиска: " mTmpStr  SKIP.
      PUT STREAM fout UNFORMATTED " - поиск в модулях : " CAPS(mMod) SKIP(1).

      /* Определим полный путь к транзакциям */
      FOR EACH tt-results 
         EXCLUSIVE-LOCK BREAK BY tt-results.path: 

         IF FIRST-OF(tt-results.path) THEN
            DO:
               FIND FIRST op-kind WHERE op-kind.op-kind EQ tt-results.path 
                  NO-LOCK NO-ERROR.
 
              IF AVAIL(op-kind) THEN 
                  DO:

                     ASSIGN
                        mFullPath = tt-results.path + " - " + op-kind.name-opkind
                        mParent   = op-kind.parent
                     .

                     DO WHILE {assigned mParent}:

                        FIND FIRST op-kind WHERE op-kind.op-kind EQ mParent 
                           NO-LOCK NO-ERROR.
 
                        IF AVAIL(op-kind) THEN 
                           ASSIGN
                              mFullPath = op-kind.op-kind + "/" + mFullPath
                              mParent   = op-kind.parent
                           .
                     END.
                  END.
            END.

            tt-results.fullpath = mFullPath.
      END.

      IF mAnswer THEN
         DO:
            /* Короткий отчет */
            FOR EACH tt-results 
               NO-LOCK BREAK BY tt-results.module BY tt-results.fullpath BY tt-results.opkind: 
      
               IF FIRST-OF(tt-results.fullpath) THEN
                  DO:
                     PUT STREAM fout UNFORMATTED "------------------------------------------------------------------------------" SKIP.
                     PUT STREAM fout UNFORMATTED STRING(CAPS(tt-results.module) + ": " + tt-results.fullpath,"x(78)") SKIP.
                     PUT STREAM fout UNFORMATTED "------------------------------------------------------------------------------" SKIP.
                  END.
      
               PUT STREAM fout UNFORMATTED STRING(tt-results.opkind,"x(17)") + tt-results.name    SKIP.
      
               IF LAST-OF(tt-results.fullpath) THEN
                  PUT STREAM fout UNFORMATTED " " SKIP.
            END.
         END.
      ELSE
         DO:
            /* Подробный отчет */

            FOR EACH tt-results 
               NO-LOCK BREAK BY tt-results.module BY tt-results.fullpath BY tt-results.opkind BY tt-results.order BY tt-results.place: 
      
               IF FIRST-OF(tt-results.fullpath) THEN
                  DO:
                     PUT STREAM fout UNFORMATTED "------------------------------------------------------------------------------" SKIP.
                     PUT STREAM fout UNFORMATTED STRING(CAPS(tt-results.module) + ": " + tt-results.fullpath,"x(78)") SKIP.
                     PUT STREAM fout UNFORMATTED "------------------------------------------------------------------------------" SKIP.
                  END.

               PUT STREAM fout UNFORMATTED " >>> " + tt-results.opkind + " - " + tt-results.name SKIP.
               PUT STREAM fout UNFORMATTED " >>> "  
                                           + (IF tt-results.order GE 1 THEN ("Шаблон: " + STRING(tt-results.order) + ", ") ELSE "")
                                           + tt-results.place SKIP.
               PUT STREAM fout UNFORMATTED "------------------------------------------------------------------------------" SKIP.
               PUT STREAM fout UNFORMATTED tt-results.formula SKIP.
               PUT STREAM fout UNFORMATTED "------------------------------------------------------------------------------" SKIP.
      
               IF LAST-OF(tt-results.fullpath) THEN
                  PUT STREAM fout UNFORMATTED " " SKIP.

            END.
         END.

      PUT STREAM fout UNFORMATTED "--- КОНЕЦ ОТЧЕТА ---" SKIP.

      RUN Fill-SysMes IN h_tmess ("","","0", "Сформирован файл отчета search_txt.log" ).

      {preview.i &stream="stream fout" 
          &STREAM   = "STREAM fout "
          &filename = "'search_txt.log'"
          &nodef=/**/ 
      }

   END.
ELSE
   message "Текст <" + mTmpStr + "> не обнаружен в транзакциях модулей " + CAPS(mMod) view-as alert-box.

{intrface.del}
