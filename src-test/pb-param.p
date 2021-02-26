/**
Авторские права принадлежат: ПАО Плюс банк
Что делает:     Импортирует через Бисмарк в формате PB-PARAM
Как работает:   В правиле обмена из 4-х каталогов и ДР PBparam формирует 
Параметры:      Правило обмена, каталог ImpArch - директория архива;
                Правило обмена, каталог Экспорт - директория ответа (если есть);
                Правило обмена, каталог Export  - директория ошибок (если есть);
                Правило обмена, доп.реквизит PBparam - строка. Формат:
                    <процедура>|<параметры>
                В <процедуру> передается параметр:
                file=<вх.файл>;arch-dir=<дир.архива>;ans-dir=<дир.ответа>;error-dir=<дир.ошибок>;<параметры из PBparam>
Место запуска:  Бисмарк (mimfile.p)
Создан:         27/09/2016 Борисов А.В. 
*/

{globals.i}

DEFINE INPUT PARAMETER in-mail-user-num LIKE mail-user.mail-user-num NO-UNDO.
DEFINE INPUT PARAMETER inFileName       AS CHARACTER                 NO-UNDO.

/* Ищем каталог arch-dir */
DEFINE VARIABLE iArchDir    AS CHARACTER NO-UNDO INIT "".
FOR FIRST catalog
    WHERE (catalog.mail-user-num EQ in-mail-user-num)
      AND (catalog.Kind          EQ "ImpArch")
    NO-LOCK:

    iArchDir = catalog.path.
END.
IF (iArchDir EQ "") THEN RETURN.

/* Ищем каталог ans-dir */
DEFINE VARIABLE iOutDir     AS CHARACTER NO-UNDO INIT "".
FOR FIRST catalog
    WHERE (catalog.mail-user-num EQ in-mail-user-num)
      AND (catalog.Kind          EQ "Экспорт")
    NO-LOCK:

    iOutDir = catalog.path.
END.

/* Ищем каталог error-dir */
DEFINE VARIABLE iErrDir     AS CHARACTER NO-UNDO INIT "".
FOR FIRST catalog
    WHERE (catalog.mail-user-num EQ in-mail-user-num)
      AND (catalog.Kind          EQ "Export")
    NO-LOCK:

    iErrDir = catalog.path.
END.

/* Формируем параметр и запускаем процедуру обработки */
DEFINE VARIABLE iParam      AS CHARACTER NO-UNDO.
DEFINE VARIABLE iProc       AS CHARACTER NO-UNDO.
iParam = GetXAttrValue("mail-user", STRING(in-mail-user-num), "PBparam").
iProc  = ENTRY(1, iParam, "|").
iParam = "file="      + inFileName + ";"
       + "arch-dir="  + iArchDir   + ";"
       + (IF (iOutDir NE "") THEN ("ans-dir="   + iOutDir  + ";") ELSE "")
       + (IF (iErrDir NE "") THEN ("error-dir=" + iErrDir  + ";") ELSE "")
       + ENTRY(2, iParam, "|").

RUN VALUE(iProc) (iParam) NO-ERROR.
