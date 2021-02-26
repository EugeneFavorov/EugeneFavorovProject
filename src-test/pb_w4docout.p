/**
Авторские права принадлежат: ПАО Плюс банкЧто делает:     Выгружает документы из Бисквита для WAY4Как работает:   
Параметры:      
Место запуска:  Процедура контроля на выходе статуса ФКС
Создан:         29.09.2016 Борисов А.В.*/

DEFINE INPUT  PARAMETER iop     AS INT64    NO-UNDO.    /* Документ */
DEFINE INPUT  PARAMETER iParam  AS INT64    NO-UNDO.    /* <Правило обмена с каталогами Экспорт и ExpArch > */

RUN pb_tstwork.p.
IF NOT (RETURN-VALUE BEGINS "bis-work") THEN RETURN.
{globals.i}{intrface.get count}
{intrface.get xclass}
{intrface.get filex}
{intrface.get refer}

DEFINE VARIABLE cFile       AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cArcDir     AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cOutDir     AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cCnt        AS CHARACTER    NO-UNDO.    /* Счетчик файлов экспорта */
DEFINE VARIABLE nAmt        AS DECIMAL      NO-UNDO.
DEFINE VARIABLE cAcDb       AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cAcCr       AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cCurr       AS CHARACTER    NO-UNDO.
DEFINE BUFFER   oe          FOR op-entry.
DEFINE BUFFER   acdb        FOR acct.
DEFINE BUFFER   accr        FOR acct.
DEFINE STREAM   txt.{pb_w4docout.i}

FOR FIRST op
    WHERE (op.op    EQ iop)
    NO-LOCK,
EACH oe OF op
    NO-LOCK:

    IF {assigned oe.acct-db}
    THEN FIND FIRST acdb
            WHERE (acdb.acct    EQ oe.acct-db)
            NO-LOCK NO-ERROR.
    IF {assigned oe.acct-cr}
    THEN FIND FIRST accr
            WHERE (accr.acct    EQ oe.acct-cr)
            NO-LOCK NO-ERROR.

    IF ForExp2W4()
    THEN DO:        /* Надо выгружать */
        FOR EACH Catalog
            WHERE (Catalog.mail-user-num EQ iParam)
              AND (Catalog.Kind          EQ "ExpArch")
            NO-LOCK:

            cArcDir = Catalog.path + W4Date(TODAY) + "/".
            IF NOT ExistFolder(cArcDir)
            THEN DO:
                SurelyCreateFolder(cArcDir).
                OS-COMMAND SILENT VALUE("chmod 777 " + cArcDir).
            END.
        END.

        FOR EACH Catalog
            WHERE (Catalog.mail-user-num EQ iParam)
              AND (Catalog.Kind          EQ "Экспорт")
            NO-LOCK:

            cOutDir = Catalog.path.
        END.

        cCnt  = STRING(SetCounterValue("ДокумWAY4",?,TODAY)).
        cFile = cArcDir + "IIC_Documents_000095_000095_"
              + REPLACE(W4Date(TODAY), "-", "") + "_" + cCnt + ".xml".
        OUTPUT STREAM txt TO VALUE(cFile) CONVERT TARGET "UTF-8".

        cAcDb = Acct2W4(BUFFER acdb, accr.currency).
        cAcCr = Acct2W4(BUFFER accr, acdb.currency).
        cCurr = SUBSTRING(cAcDb,6,3).
        cCurr = IF (cCurr = SUBSTRING(cAcCr,6,3)) THEN (IF (cCurr = "810") THEN "" ELSE cCurr) ELSE oe.currency.
        nAmt  = IF (cCurr EQ "") THEN oe.amt-rub ELSE oe.amt-cur.

        IF      (cAcDb BEGINS "40")
            AND (cAcCr BEGINS "40")
            AND (acdb.currency <> accr.currency)
        THEN PUT STREAM txt UNFORMATTED
                '<?xml version="1.0" encoding="UTF-8"?>'
                + '~n<DocFile xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">'
                + '~n  <FileHeader>'
                + '~n    <FileLabel>PAYMENT</FileLabel>'
                + '~n    <FormatVersion>2.2</FormatVersion>'
                + '~n    <Sender>000095</Sender>'
                + '~n    <CreationDate>' + W4Date(TODAY) + '</CreationDate>'
                + '~n    <CreationTime>' + STRING(TIME, "HH:MM:SS") + '.000</CreationTime>'
                + '~n    <FileSeqNumber>' + cCnt + '</FileSeqNumber>'
                + '~n    <Receiver>000095</Receiver>'
                + '~n  </FileHeader>'
                + '~n  <DocList>'
                + '~n    <Doc>'
                + '~n      <TransType>'
                + '~n        <TransCode>'
                + '~n          <MsgCode>PAYACC</MsgCode>'
                + '~n        </TransCode>'
                + '~n      </TransType>'
                + '~n      <DocRefSet>'
                + '~n        <Parm>'
                + '~n          <ParmCode>SRN</ParmCode>'
                + '~n          <Value>A' + STRING(op.op) + '</Value>'
                + '~n        </Parm>'
                + '~n        <Parm>'
                + '~n           <ParmCode>ARN</ParmCode>'
                + '~n           <Value>BIS</Value>'
                + '~n        </Parm>'
                + '~n      </DocRefSet>'
                + '~n      <LocalDt>' + W4Date(op.op-date) + ' 00:00:00</LocalDt>'
                + '~n      <Description>' + op.details + '</Description>'
                + '~n      <Originator>'
                + '~n        <ContractNumber>' + cAcDb + '</ContractNumber>'
                + '~n        <MemberId>0001</MemberId>'
                + '~n      </Originator>'
                + '~n      <Destination>'
                + '~n        <ContractNumber>' + GetRefVal("W4subst", TODAY, IF (acdb.currency = "") THEN "810" ELSE acdb.currency) + '</ContractNumber>'
                + '~n        <MemberId>0001</MemberId>'
                + '~n      </Destination>'
                + '~n      <Transaction>'
                + '~n        <Currency>' + (IF (acdb.currency EQ "") THEN "810" ELSE acdb.currency) + '</Currency>'
                + '~n        <Amount>'   + (IF (acdb.currency EQ "") THEN STRING(oe.amt-rub) ELSE STRING(nAmt)) + '</Amount>'
                + '~n      </Transaction>'
                + '~n    </Doc>'
                + '~n    <Doc>'
                + '~n      <TransType>'
                + '~n        <TransCode>'
                + '~n          <MsgCode>PAYACC</MsgCode>'
                + '~n        </TransCode>'
                + '~n      </TransType>'
                + '~n      <DocRefSet>'
                + '~n        <Parm>'
                + '~n          <ParmCode>SRN</ParmCode>'
                + '~n          <Value>B' + STRING(op.op) + '</Value>'
                + '~n        </Parm>'
                + '~n        <Parm>'
                + '~n           <ParmCode>ARN</ParmCode>'
                + '~n           <Value>BIS</Value>'
                + '~n        </Parm>'
                + '~n      </DocRefSet>'
                + '~n      <LocalDt>' + W4Date(op.op-date) + ' 00:00:00</LocalDt>'
                + '~n      <Description>' + op.details + '</Description>'
                + '~n      <Originator>'
                + '~n        <ContractNumber>' + GetRefVal("W4subst", TODAY, IF (accr.currency = "") THEN "810" ELSE accr.currency) + '</ContractNumber>'
                + '~n        <MemberId>0001</MemberId>'
                + '~n      </Originator>'
                + '~n      <Destination>'
                + '~n        <ContractNumber>' + cAcCr + '</ContractNumber>'
                + '~n        <MemberId>0001</MemberId>'
                + '~n      </Destination>'
                + '~n      <Transaction>'
                + '~n        <Currency>' + (IF (accr.currency EQ "") THEN "810" ELSE accr.currency) + '</Currency>'
                + '~n        <Amount>'   + (IF (accr.currency EQ "") THEN STRING(oe.amt-rub) ELSE STRING(nAmt)) + '</Amount>'
                + '~n      </Transaction>'
                + '~n    </Doc>'
                + '~n  </DocList>'
                + '~n  <FileTrailer>'
                + '~n    <CheckSum>'
                + '~n      <RecsCount>2</RecsCount>'
                + '~n      <HashTotalAmount>' + STRING(nAmt + oe.amt-rub) + '</HashTotalAmount>'
                + '~n    </CheckSum>'
                + '~n  </FileTrailer>'
                + '~n</DocFile>~n'
                .
        ELSE PUT STREAM txt UNFORMATTED
                '<?xml version="1.0" encoding="UTF-8"?>'
                + '~n<DocFile xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">'
                + '~n  <FileHeader>'
                + '~n    <FileLabel>PAYMENT</FileLabel>'
                + '~n    <FormatVersion>2.2</FormatVersion>'
                + '~n    <Sender>000095</Sender>'
                + '~n    <CreationDate>' + W4Date(TODAY) + '</CreationDate>'
                + '~n    <CreationTime>' + STRING(TIME, "HH:MM:SS") + '.000</CreationTime>'
                + '~n    <FileSeqNumber>' + cCnt + '</FileSeqNumber>'
                + '~n    <Receiver>000095</Receiver>'
                + '~n  </FileHeader>'
                + '~n  <DocList>'
                + '~n    <Doc>'
                + '~n      <TransType>'
                + '~n        <TransCode>'
                + '~n          <MsgCode>PAYACC</MsgCode>'
                + '~n        </TransCode>'
                + '~n      </TransType>'
                + '~n      <DocRefSet>'
                + '~n        <Parm>'
                + '~n          <ParmCode>SRN</ParmCode>'
                + '~n          <Value>' + STRING(op.op) + '</Value>'
                + '~n        </Parm>'
                + '~n        <Parm>'
                + '~n           <ParmCode>ARN</ParmCode>'
                + '~n           <Value>BIS</Value>'
                + '~n        </Parm>'
                + '~n      </DocRefSet>'
                + '~n      <LocalDt>' + W4Date(op.op-date) + ' 00:00:00</LocalDt>'
                + '~n      <Description>' + op.details + '</Description>'
                + '~n      <Originator>'
                + '~n        <ContractNumber>' + cAcDb + '</ContractNumber>'
                + '~n        <MemberId>0001</MemberId>'
                + '~n      </Originator>'
                + '~n      <Destination>'
                + '~n        <ContractNumber>' + cAcCr + '</ContractNumber>'
                + '~n        <MemberId>0001</MemberId>'
                + '~n      </Destination>'
                + '~n      <Transaction>'
                + '~n        <Currency>' + (IF (cCurr EQ "") THEN "810" ELSE cCurr) + '</Currency>'
                + '~n        <Amount>' + STRING(nAmt) + '</Amount>'
                + '~n      </Transaction>'
                + '~n    </Doc>'
                + '~n  </DocList>'
                + '~n  <FileTrailer>'
                + '~n    <CheckSum>'
                + '~n      <RecsCount>1</RecsCount>'
                + '~n      <HashTotalAmount>' + STRING(nAmt) + '</HashTotalAmount>'
                + '~n    </CheckSum>'
                + '~n  </FileTrailer>'
                + '~n</DocFile>~n'
                .
        OUTPUT STREAM txt CLOSE.
        OS-COPY VALUE(cFile) VALUE(cOutDir).
        RELEASE acdb.
        RELEASE accr.

        UpdateSigns ("op", STRING(op.op), "W4_export", STRING(NOW, "99.99.9999 HH:MM:SS"), YES).
    END.
END.

{intrface.del}
