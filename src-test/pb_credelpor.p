/**
Авторские права принадлежат: ПАО Плюс банк
Основание:      
Что делает:     Создает xml-файл для удаления кредитной истории из CRE
Как работает:   
Параметры:      
Место запуска:  
Создан:         19.04.2018 Борисов А.В.
*/

{globals.i}             /** Глобальные определения */
{tmprecid.def}          /** Используем информацию из броузера */
{intrface.get xclass}

&GLOB NBKICODE  "NU01BB000001"
&GLOB FLPREFIX  "ФЛ"
&GLOB ULPREFIX  "ЮЛ"
DEFINE VARIABLE hSAXWriter  AS HANDLE.
DEFINE VARIABLE lOK         AS LOGICAL      NO-UNDO.
DEFINE VARIABLE cAcc        AS CHARACTER    NO-UNDO.

CREATE SAX-WRITER hSAXWriter.
hSAXWriter:FORMATTED = TRUE.
hSAXWriter:ENCODING = "windows-1251".
lOK = hSAXWriter:SET-OUTPUT-DESTINATION("file",
      "cre" + STRING(YEAR(TODAY)) +  STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") +
      REPLACE( STRING(TIME,"HH:MM:SS"), ':', '') + ".xml").
lOK = hSAXWriter:START-DOCUMENT( ).
lOK = hSAXWriter:START-ELEMENT("ImportCreditRegistry").
lOK = hSAXWriter:DECLARE-NAMESPACE ("http://www.creditregistry.ru/schema/import").

FOR EACH tmprecid 
    NO-LOCK,
FIRST loan
    WHERE (RECID(loan) EQ tmprecid.id)
    NO-LOCK,
EACH signs
    WHERE signs.file-name   = 'term-obl'
      AND signs.surrogate   BEGINS loan.contract + "," + loan.cont-code + ',5'
      AND signs.code        = 'видоб'
      AND signs.xattr-value = 'Поручит'
    NO-LOCK,
FIRST term-obl
    WHERE term-obl.contract     =      ENTRY(1,signs.surrogate)
      AND term-obl.cont-code    =      ENTRY(2,signs.surrogate)
      AND term-obl.idnt         = INT (ENTRY(3,signs.surrogate))
      AND term-obl.end-date     = DATE(ENTRY(4,signs.surrogate))
      AND term-obl.nn           = INT (ENTRY(5,signs.surrogate))
    NO-LOCK:

    cAcc = REPLACE( loan.cont-code, '@', '/').
    CASE loan.filial-id:
        WHEN "0000" THEN DO:
            IF (loan.open-date < 01/01/2014)
            THEN cAcc = loan.doc-ref + "@0400".
            ELSE IF (loan.open-date < 05/01/2015)
            THEN cAcc = loan.doc-ref + "/0400".
        END.
        WHEN "0300" THEN DO:
            IF (loan.open-date < 09/04/2015)
            THEN cAcc = loan.doc-ref.
        END.
        WHEN "0400" THEN DO:
            IF (loan.open-date < 01/01/2014)
            THEN cAcc = loan.cont-code.
        END.
        WHEN "0500" THEN DO:
            IF (loan.open-date < 10/01/2016)
            THEN cAcc = loan.doc-ref.
        END.
    END CASE.

    lOK = hSAXWriter:START-ELEMENT("Delete").
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("ReferenceCode", (IF (term-obl.symbol = "Ю") THEN {&ULPREFIX} ELSE {&FLPREFIX}) + STRING(term-obl.fop)).
    lOK = hSAXWriter:START-ELEMENT("Trades").
    lOK = hSAXWriter:START-ELEMENT("Trade").
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("MemberCode", {&NBKICODE}).
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("Account",    cAcc).
    lOK = hSAXWriter:END-ELEMENT("Trade").
    lOK = hSAXWriter:END-ELEMENT("Trades").
    lOK = hSAXWriter:END-ELEMENT("Delete").
END.

lOK = hSAXWriter:END-ELEMENT("ImportCreditRegistry").
lOK = hSAXWriter:END-DOCUMENT().
{intrface.del}

