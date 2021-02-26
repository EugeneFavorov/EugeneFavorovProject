/**
Авторские права принадлежат: ПАО Плюс банкЧто делает:     Выгружает выбранные документы из Бисквита для WAY4
Как работает:
Параметры:
Место запуска:  Ctrl-G
Создан:         29.09.2016 Борисов А.В.*/

DEFINE INPUT  PARAMETER iParam  AS INT64    NO-UNDO.    /* <Правило обмена с каталогами Экспорт и ExpArch > = 12104 */

{globals.i}
{pb_logit.i}{intrface.get count}
{intrface.get xclass}
{intrface.get filex}
{intrface.get refer}
{tmprecid.def}          /** Используем информацию из броузера */

DEFINE VARIABLE cFile       AS CHARACTER    NO-UNDO.DEFINE VARIABLE cArcDir     AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cOutDir     AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cCnt        AS CHARACTER    NO-UNDO.    /* Счетчик файлов экспорта */
DEFINE VARIABLE iDoc        AS INTEGER      NO-UNDO.    /* Счетчик документов в файле */
DEFINE VARIABLE nAmt        AS DECIMAL      NO-UNDO.
DEFINE VARIABLE nSum        AS DECIMAL      NO-UNDO.
DEFINE BUFFER   oe          FOR op-entry.
DEFINE BUFFER   acdb        FOR acct.
DEFINE BUFFER   accr        FOR acct.
DEFINE STREAM txt.DEFINE TEMP-TABLE ttDoc     NO-UNDO
    FIELD op        AS CHARACTER
    FIELD details   LIKE op.details
    FIELD acct-db   LIKE op-entry.acct-db
    FIELD acct-cr   LIKE op-entry.acct-cr
    FIELD curr      LIKE op-entry.currency
    FIELD amt       LIKE op-entry.amt-rub
    FIELD acct      LIKE op-entry.acct-db   /* Для упорядочивания проводок */
    FIELD cr-db     AS INTEGER              /* по кл.счетам: Кр - потом Дб */
    .
{pb_w4docout.i}

iDoc = 0.
FOR EACH tmprecid 
    NO-LOCK,
FIRST op
    WHERE (RECID(op) EQ tmprecid.id)
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
        CREATE ttDoc.
        ASSIGN
            ttDoc.op        = STRING(op.op)
            ttDoc.details   = op.details
            ttDoc.acct-db   = Acct2W4(BUFFER acdb)
            ttDoc.acct-cr   = Acct2W4(BUFFER accr)
            ttDoc.curr      = oe.currency
            ttDoc.amt       = IF (oe.currency EQ "") THEN oe.amt-rub ELSE oe.amt-cur
            ttDoc.acct      = IF CAN-DO("40817*,40820*,40802*,40807*,40702*,40703*", ttDoc.acct-db) THEN ttDoc.acct-db ELSE ttDoc.acct-cr
            ttDoc.cr-db     = IF (ttDoc.acct = ttDoc.acct-cr) THEN 0 ELSE 1
            .
    END.

    RELEASE acdb.
    RELEASE accr.
END.

FOR EACH ttDoc
    NO-LOCK
    BREAK BY ttDoc.curr
          BY ttDoc.acct
          BY ttDoc.cr-db:

    IF FIRST-OF(ttDoc.curr)
    THEN DO:
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

        iDoc  = 0.
        nSum  = 0.0.
        cCnt  = STRING(SetCounterValue("ДокумWAY4",?,TODAY)).
        cFile = cArcDir + "IIC_Documents_000095_000095_"
              + REPLACE(W4Date(TODAY), "-", "") + "_" + cCnt + ".xml".
        OUTPUT STREAM txt TO VALUE(cFile) CONVERT TARGET "UTF-8".
        PUT STREAM txt UNFORMATTED
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
            .
    END.

    iDoc  = iDoc + 1.
    nSum  = nSum + ttDoc.amt.
    PUT STREAM txt UNFORMATTED
          '~n    <Doc>'
        + '~n      <TransType>'
        + '~n        <TransCode>'
        + '~n          <MsgCode>PAYACC</MsgCode>'
        + '~n        </TransCode>'
        + '~n      </TransType>'
        + '~n      <DocRefSet>'
        + '~n        <Parm>'
        + '~n          <ParmCode>SRN</ParmCode>'
        + '~n          <Value>' + ttDoc.op + '</Value>'
        + '~n        </Parm>'
        + '~n        <Parm>'
        + '~n           <ParmCode>ARN</ParmCode>'
        + '~n           <Value>BIS</Value>'
        + '~n        </Parm>'
        + '~n      </DocRefSet>'
        + '~n      <LocalDt>' + W4Date(TODAY) + ' 00:00:00</LocalDt>'
        + '~n      <Description>' + ttDoc.details + '</Description>'
        + '~n      <Originator>'
        + '~n        <ContractNumber>' + ttDoc.acct-db + '</ContractNumber>'
        + '~n        <MemberId>0001</MemberId>'
        + '~n      </Originator>'
        + '~n      <Destination>'
        + '~n        <ContractNumber>' + ttDoc.acct-cr + '</ContractNumber>'
        + '~n        <MemberId>0001</MemberId>'
        + '~n      </Destination>'
        + '~n      <Transaction>'
        + '~n        <Currency>' + (IF (ttDoc.curr EQ "") THEN "810" ELSE ttDoc.curr) + '</Currency>'
        + '~n        <Amount>' + STRING(ttDoc.amt) + '</Amount>'
        + '~n      </Transaction>'
        + '~n    </Doc>'
        .
    UpdateSigns ("op", ttDoc.op, "W4_export", STRING(NOW, "99.99.9999 HH:MM:SS"), YES).

    IF LAST-OF(ttDoc.curr)
    THEN DO:
        PUT STREAM txt UNFORMATTED
              '~n  </DocList>'
            + '~n  <FileTrailer>'
            + '~n    <CheckSum>'
            + '~n      <RecsCount>' + STRING(iDoc) + '</RecsCount>'
            + '~n      <HashTotalAmount>' + STRING(nSum) + '</HashTotalAmount>'
            + '~n    </CheckSum>'
            + '~n  </FileTrailer>'
            + '~n</DocFile>~n'
            .
        OUTPUT STREAM txt CLOSE.
        OS-COPY VALUE(cFile) VALUE(cOutDir).
    END.
END.

{intrface.del}
