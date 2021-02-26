
                   /*******************************************
                    *                                         *
                    *  ГОСПОДА ПРОГРАММИСТЫ И СОЧУВСТВУЮЩИЕ!  *
                    *                                         *
                    *  РЕДАКТИРОВАТЬ ДАННЫЙ ФАЙЛ БЕСПОЛЕЗНО,  *
                    *  Т.К. ОН СОЗДАЕТСЯ ГЕНЕРАТОРОМ ОТЧЕТОВ  *
                    *             АВТОМАТИЧЕСКИ!              *
                    *                                         *
                    *******************************************/

/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2011 ТОО "Банковские информационные системы"
     Filename: lsa1.p
      Comment: Отчет, созданный генератором отчетов
      Comment:
   Parameters: Input: RID -  RecID(op)
         Uses: Globals.I ChkAcces.I SetDest.I PreView.I
      Used by:
      Created: 15/02/11 16:26:28
     Modified:
*/
Form "~n@(#) lsa1.p 1.0 RGen 15/02/11 RGen 15/02/11 [ AutoReport By R-Gen ]"
     with frame sccs-id stream-io width 250.

{globals.i}
{chkacces.i}
/*-------------------- Входные параметры --------------------*/
Define Input Param RID as RecID no-undo.

/*-------------------- Объявление переменных --------------------*/

Define Buffer buf_0_op               For op.

/*--------------- Буфера для полей БД: ---------------*/

/*--------------- Переменные для специальных полей: ---------------*/
Define Variable Bank-name        As Character            No-Undo.
Define Variable drag-b           As Decimal              No-Undo.
Define Variable drag-o           As Decimal              No-Undo.
Define Variable k-b              As Decimal              No-Undo.
Define Variable k-drag-b         As Decimal              No-Undo.
Define Variable k-drag-o         As Decimal              No-Undo.
Define Variable k-o              As Decimal              No-Undo.
Define Variable kinv-b           As Decimal              No-Undo.
Define Variable kinv-o           As Decimal              No-Undo.
Define Variable tot-b            As Decimal              No-Undo.
Define Variable tot-o            As Decimal              No-Undo.
Define Variable val-b            As Decimal              No-Undo.
Define Variable val-o            As Decimal              No-Undo.
Define Variable vBalSum          As Decimal              No-Undo.
Define Variable vDateString      As Character            No-Undo.
Define Variable vNBalSum         As Decimal              No-Undo.

/*--------------- Определение форм для циклов ---------------*/

/* Начальные действия */
{lsa.i}

/*-----------------------------------------
   Проверка наличия записи главной таблицы,
   на которую указывает Input Param RID
-------------------------------------------*/
Find op Where RecID(op) = RID no-lock no-error.
If Not Avail(op) then do:
  message "Нет записи <op>".
  Return.
end.

/*------------------------------------------------
   Выставить buffers на записи, найденные
   в соответствии с заданными в отчете правилами
------------------------------------------------*/
/* Т.к. не задано правило для выборки записей из главной таблицы,
   просто выставим его buffer на input RecID                    */
find buf_0_op where RecID(buf_0_op) = RecID(op) no-lock.

/*------------------------------------------------
   Вычислить значения специальных полей
   в соответствии с заданными в отчете правилами
------------------------------------------------*/
/* Вычисление значения специального поля Bank-name */
{get_set.i "Банк"}

    assign

       bank-name = setting.val

    .

/* Вычисление значения специального поля drag-b */
/* Смотри начальные действия */

/* Вычисление значения специального поля drag-o */
/* Смотри начальные действия */

/* Вычисление значения специального поля k-b */
/* Смотри начальные действия */

/* Вычисление значения специального поля k-drag-b */
/* Смотри начальные действия */

/* Вычисление значения специального поля k-drag-o */
/* Смотри начальные действия */

/* Вычисление значения специального поля k-o */
/* Смотри начальные действия */

/* Вычисление значения специального поля kinv-b */
/* Смотри начальные действия */

/* Вычисление значения специального поля kinv-o */
/* Смотри начальные действия */

/* Вычисление значения специального поля tot-b */
/* Смотри начальные действия */

/* Вычисление значения специального поля tot-o */
/* Смотри начальные действия */

/* Вычисление значения специального поля val-b */
/* Смотри начальные действия */

/* Вычисление значения специального поля val-o */
/* Смотри начальные действия */

/* Вычисление значения специального поля vBalSum */
VBalSum = vBal.

/* Вычисление значения специального поля vDateString */
vDateString = GetDateString(end-date).

/* Вычисление значения специального поля vNBalSum */
VNBalSum = vNBal.

/*-------------------- Формирование отчета --------------------*/
{strtout3.i &cols=83 &option=Paged}

put unformatted "Срок хранения   __________________________________________________" skip.
put unformatted " " skip.
put unformatted "Архивный индекс __________________________________________________" skip.
put unformatted " " skip.
put skip(1).
put unformatted "" Bank-name Format "x(82)"
                "" skip.
put unformatted "   (полное или сокращенное фирменное наименование кредитной организации и(или)" skip.
put unformatted "                              наименование филиала)" skip.
put skip(3).
put unformatted "Документы за " vDateString Format "x(27)"
                "" skip.
put skip(3).
put unformatted "                                по балансовым счетам       по внебалансовым счетам" skip.
put skip(1).
put unformatted " " skip.
put unformatted "Сумма                       " tot-b Format "->>>,>>>,>>>,>>9.99"
                "  руб.     " tot-o Format "->>>,>>>,>>>,>>9.99"
                "  руб." skip.
put skip(2).
put unformatted "Из них:" skip.
put unformatted "хранятся на бумажном носителе и находятся в отдельных папках:" skip.
put skip(1).
put unformatted "кассовые документы          " k-b Format "->>>,>>>,>>>,>>9.99"
                "  руб.     " k-o Format "->>>,>>>,>>>,>>9.99"
                "  руб." skip.
put skip(2).
put unformatted "    по операциям с иностранной валютой:" skip.
put skip(1).
put unformatted "бухгалтерские документы     " val-b Format "->>>,>>>,>>>,>>9.99"
                "  руб.     " val-o Format "->>>,>>>,>>>,>>9.99"
                "  руб." skip.
put skip(1).
put unformatted "     срок хранения  ____________" skip.
put unformatted " " skip.
put unformatted "кассовые документы          " kinv-b Format "->>>,>>>,>>>,>>9.99"
                "  руб.     " kinv-o Format "->>>,>>>,>>>,>>9.99"
                "  руб." skip.
put skip(1).
put unformatted "     срок хранения  ____________" skip.
put unformatted " " skip.
put skip(1).
put unformatted "    по операциям с драгоценными металлами:" skip.
put unformatted " " skip.
put unformatted "бухгалтерские документы     " drag-b Format "->>>,>>>,>>>,>>9.99"
                "  руб.     " drag-o Format "->>>,>>>,>>>,>>9.99"
                "  руб." skip.
put skip(1).
put unformatted "     срок хранения  ____________" skip.
put skip(1).
put unformatted "кассовые документы          " k-drag-b Format "->>>,>>>,>>>,>>9.99"
                "  руб.     " k-drag-o Format "->>>,>>>,>>>,>>9.99"
                "  руб." skip.
put skip(1).
put unformatted "     срок хранения  ____________" skip.
put unformatted " " skip.
put unformatted "хранятся в электронном виде:" skip.
put unformatted " " skip.
put unformatted "кассовые документы          _______________________________________________________" skip.
put skip(1).
put unformatted "бухгалтерские документы     " vBalSum Format "->>>,>>>,>>>,>>9.99"
                "  руб.     " vNBalSum Format "->>>,>>>,>>>,>>9.99"
                "  руб." skip.
put skip(1).
put unformatted " " skip.
put skip(1).
put unformatted "Документы сброшюрованы и подшиты __________________________________________________" skip.
put skip(1).
put unformatted "___________________________________________________________________________________" skip.
put unformatted "     (подпись бухгалтерского работника, осуществившего сшив и проверку полноты" skip.
put unformatted "                            сброшюрованных документов)" skip.
put skip(1).
put unformatted "С данными бухгалтерского учета сверено ___________________" skip.
put unformatted "                                            (подпись)" skip.

/* Конечные действия */
{lb_dtl.i}


{endout3.i &nofooter=yes}

