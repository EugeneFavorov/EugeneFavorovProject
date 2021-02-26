/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2015 ЗАО "Банковские информационные системы"
     Filename: ext-fillop.p
      Comment: TT:0259807 Миграция. 365-П предоставление общей выписки по счетам ТФ и ОФ
   Parameters:
         Uses:
      Used by:
      Created: 22/03/2014 15:15 KMBIS Переходник для выгрузки данных из внешней системы.
                                      Документы по счету.
     Modified: 
*/
ROUTINE-LEVEL ON ERROR UNDO, THROW.
{globals.i}
{extexch.def} /* Содержит описание временной таблицы ttExtAcct */

DEF INPUT  PARAM iHRepOp   AS  HANDLE NO-UNDO.   /* Указатель на таблицу для данных по операциям */
DEF INPUT  PARAM iBegDate  AS  DATE   NO-UNDO.   /* Дата начала выгрузки                         */ 
DEF INPUT  PARAM iEndDate  AS  DATE   NO-UNDO.   /* Дата окончания выгрузки                      */
DEF INPUT  PARAM iUpID     AS  INT64  NO-UNDO.   /* ID выгружаемого счета                        */
DEF INPUT  PARAM iAllFil   AS  LOG    NO-UNDO.   /* По всем филиалам или только текущий          */ 
DEF INPUT  PARAM TABLE     FOR ttExtAcct.        /* Таблица с данными по счету                   */
DEF OUTPUT PARAM oNumDocs  AS  INT64  NO-UNDO.   /* Количество документов                        */

/* Не смотря на то что идет передача таблицы, в ней всегда будет 1 счет */

{intrface.get strng}

DO ON ERROR UNDO, RETURN ERROR
   ON STOP UNDO, RETURN ERROR:

IF NOT CONNECTED("bank")
  THEN CONNECT -pf VALUE("/home2/bis/quit41d/conf/bismfr.pf").

FOR FIRST ttExtAcct NO-LOCK:
   /*
   /* Создаем запись в таблице операций */
   iHRepOp:BUFFER-CREATE().
   ASSIGN
      /* oNumDocs увеличиваем с каждым созданным документом на 1 */
      oNumDocs = oNumDocs + 1
   
      /* Обязательные системные поля */
      iHRepOp:BUFFER-FIELD(GetMangledName("ID")):BUFFER-VALUE       = STRING(oNumDocs)
      iHRepOp:BUFFER-FIELD(GetMangledName("UpId")):BUFFER-VALUE     = STRING(iUpID)
   
      /* Поля для выгрузки, имена соответсвуют тэгам файле */
      iHRepOp:BUFFER-FIELD(GetMangledName("ДатаОпер")):BUFFER-VALUE = "01.01.1900"
      iHRepOp:BUFFER-FIELD(GetMangledName("ВидДок")):BUFFER-VALUE   = "01"
      iHRepOp:BUFFER-FIELD(GetMangledName("НомДок")):BUFFER-VALUE   = "123"
      iHRepOp:BUFFER-FIELD(GetMangledName("ДатаДок")):BUFFER-VALUE  = "01.01.1900"
      iHRepOp:BUFFER-FIELD(GetMangledName("НомКорСч")):BUFFER-VALUE = "99999999999999999999"
      iHRepOp:BUFFER-FIELD(GetMangledName("НаимБП")):BUFFER-VALUE   = "НаимБП - выгрузка из внешней системы"
      iHRepOp:BUFFER-FIELD(GetMangledName("ИННПП")):BUFFER-VALUE    = "9999999999"
      iHRepOp:BUFFER-FIELD(GetMangledName("КПППП")):BUFFER-VALUE    = "999999999"
      iHRepOp:BUFFER-FIELD(GetMangledName("БИКБП")):BUFFER-VALUE    = "888888888"
      iHRepOp:BUFFER-FIELD(GetMangledName("НаимПП")):BUFFER-VALUE   = "НаимПП - выгрузка из внешней системы"
      iHRepOp:BUFFER-FIELD(GetMangledName("НомСчПП")):BUFFER-VALUE  = "88888888888888888888"
      iHRepOp:BUFFER-FIELD(GetMangledName("Дебет")):BUFFER-VALUE    = "1.11"
      iHRepOp:BUFFER-FIELD(GetMangledName("Кредит")):BUFFER-VALUE   = "2.22"
      iHRepOp:BUFFER-FIELD(GetMangledName("НазнПл")):BUFFER-VALUE   = SUBST("Тестовая выгрузка из внешней системы. Счет: &1",
                                                                            ttExtAcct.number)
      iHRepOp:BUFFER-FIELD(GetMangledName("ФИОПП")):BUFFER-VALUE    = "ФИОПП - выгрузка из внешней системы"
  .
  /* Записываем запись и освобождаем ее */
  iHRepOp:BUFFER-RELEASE().
  */
  FOR EACH tobis365p
   WHERE tobis365p.postime >= iBegDate
     AND tobis365p.postime <  (iEndDate + 1)
     AND tobis365p.account EQ DelFilFromAcct( ttExtAcct.acct)
   NO-LOCK:  
     /* Создаем запись в таблице операций */
     iHRepOp:BUFFER-CREATE().
    ASSIGN
        /* oNumDocs увеличиваем с каждым созданным документом на 1 */
        oNumDocs = oNumDocs + 1
     
        /* Обязательные системные поля */
        iHRepOp:BUFFER-FIELD(GetMangledName("ID")):BUFFER-VALUE       = STRING(oNumDocs)
        iHRepOp:BUFFER-FIELD(GetMangledName("UpId")):BUFFER-VALUE     = STRING(iUpID)
     
        /* Поля для выгрузки, имена соответсвуют тэгам файле */
        iHRepOp:BUFFER-FIELD(GetMangledName("ДатаОпер")):BUFFER-VALUE = tobis365p.data_oper
        iHRepOp:BUFFER-FIELD(GetMangledName("ВидДок")):BUFFER-VALUE   = tobis365p.vid_doc
        iHRepOp:BUFFER-FIELD(GetMangledName("НомДок")):BUFFER-VALUE   = tobis365p.nom_doc
        iHRepOp:BUFFER-FIELD(GetMangledName("ДатаДок")):BUFFER-VALUE  = tobis365p.data_doc
        iHRepOp:BUFFER-FIELD(GetMangledName("НомКорСч")):BUFFER-VALUE = tobis365p.nom_kor_sch /* номер кор.счета банка плательщика */
        iHRepOp:BUFFER-FIELD(GetMangledName("НаимБП")):BUFFER-VALUE   = tobis365p.naim_bp     /* наименование банка плательщика */
        iHRepOp:BUFFER-FIELD(GetMangledName("БИКБП")):BUFFER-VALUE    = tobis365p.bik_bp      /* БИК банка плательщика */
        iHRepOp:BUFFER-FIELD(GetMangledName("ИННПП")):BUFFER-VALUE    = tobis365p.inn_pp      /* ИНН/КИО плательщика/получателя */
        iHRepOp:BUFFER-FIELD(GetMangledName("КПППП")):BUFFER-VALUE    = tobis365p.kpp_pp      /* КПП плательщика/получателя */
        iHRepOp:BUFFER-FIELD(GetMangledName("НаимПП")):BUFFER-VALUE   = tobis365p.naim_pp     /* наименование плательщика/получателя */
        iHRepOp:BUFFER-FIELD(GetMangledName("НомСчПП")):BUFFER-VALUE  = tobis365p.nom_sch_pp  /* счет плательщика/получателя */
        iHRepOp:BUFFER-FIELD(GetMangledName("Дебет")):BUFFER-VALUE    = tobis365p.debet
        iHRepOp:BUFFER-FIELD(GetMangledName("Кредит")):BUFFER-VALUE   = tobis365p.kredit
        iHRepOp:BUFFER-FIELD(GetMangledName("НазнПл")):BUFFER-VALUE   = tobis365p.nazn_pl
        iHRepOp:BUFFER-FIELD(GetMangledName("ФИОПП")):BUFFER-VALUE    = tobis365p.fio_pp      /* ФИО плательщика/получателя */
    .
    /* Записываем запись и освобождаем ее */
    iHRepOp:BUFFER-RELEASE().
  END.
END. /* FOR FIRST ttExtAcct NO-LOCK */
CATCH eAnyError AS Progress.Lang.Error:
  RETURN ERROR RETURN-VALUE + " " + eAnyError:GetMessage(1).
END CATCH.
END.
