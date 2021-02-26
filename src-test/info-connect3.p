DEFINE NEW GLOBAL SHARED VARIABLE h_cache AS HANDLE     NO-UNDO.
RUN pp-cache.p PERSIST SET h_cache.

{globals.i}
{intrface.get xclass}   /* Библиотека инструментов метасхемы. */
{intrface.get rights}
{intrface.get osyst}
{tmprecid.def}

def var fname as char  init "info-"  no-undo.
DEF VAR cfile as char  no-undo.
DEF VAR hSAXWriter  AS HANDLE. /* NO-UNDO.*/
DEF VAR lOK         AS LOGICAL  NO-UNDO.

def new shared stream vvs.
def buffer bbbloan for loan.
fname = fname + string(year(today)) + string(month(today),"99") + string(day(today),"99") + ".log".
output to value (fname) APPEND UNBUFFERED.
PUT UNFORMATTED "начало выгрузки данных в филиале " + shFilial + " - " + STRING(NOW,"99/99/99 HH:MM:SS") SKIP.

cfile = "info" + STRING(YEAR(TODAY)) +  STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") + REPLACE( STRING(TIME,"HH:MM:SS"), ':', '') + ".xml".
CREATE SAX-WRITER hSAXWriter.
hSAXWriter:FORMATTED = TRUE.
hSAXWriter:ENCODING = "windows-1251".
lOK = hSAXWriter:SET-OUTPUT-DESTINATION("file", cfile).
lOK = hSAXWriter:START-DOCUMENT( ).
lOK = hSAXWriter:START-ELEMENT("ImportCreditRegistry").
lOK = hSAXWriter:DECLARE-NAMESPACE ("http://www.creditregistry.ru/schema/import").


/* запускаем формирование xml для выделенных договоров */
DO  ON ERROR UNDO, RETURN ERROR
    ON STOP UNDO, LEAVE :

    for each tmprecid no-lock:
        RUN info-export3.p(tmprecid.id, hSAXWriter).
    end.
    CATCH eAnyError AS Progress.Lang.Error:
        PUT UNFORMATTED "CATCH: " + RETURN-VALUE + " " + eAnyError:GetMessage(1) SKIP.
    END CATCH.
END. /* DO */

lOK = hSAXWriter:END-ELEMENT("ImportCreditRegistry").
lOK = hSAXWriter:END-DOCUMENT( ).

PUT UNFORMATTED "окончание выгрузки данных в филиале " + shFilial + " - " + STRING(NOW,"99/99/99 HH:MM:SS") SKIP(2).
OUTPUT CLOSE.
