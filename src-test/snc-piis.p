/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2014 ЗАО "Банковские информационные системы"
     Filename: snc-piis.p
      Comment: 0062724  ОЭД. Создание отчета по загруженным
                  служебно-информационным сообщениям.
   Parameters: none
         Uses:
      Used BY:
      Created: 07.06.2006 15:56 MUTA
     Modified: 07.11.2006 11:20 MUTA Для каждого отчета создается файл с
                                     уникальным именем.
     Modified: 26.12.2006 15:56 NIK  Перевод строки в служебных сообщениях
     Modified: 05.03.2007 13:00 MUTA 0074036  Отображение информации для 
                                     ошибок rkl20 и rkl21.
     Modified: 01.06.2007 13:00 Mike 0076243 Исправлена ошибка вывода
                                     наименования из базы данных  в протокол
     Modified: 26.06.2007 17.00 Muta 0077510  Подключена обработка доп.
                                     реквизита Подписи.
     Modified: 20.07.2007       Muta 0079896  Исправлена печать выписки
*/
{globals.i}
{snc-print.prm}

{exchange.equ}
{intrface.get strng}
{intrface.get exch}
{intrface.get pack}
{intrface.get import} 
{intrface.get cust} 

{wordwrap.def}

&SCOPE mPackTxtNumStr 11

DEFINE BUFFER filPacket FOR Packet.
DEFINE BUFFER docPacket FOR Packet.
DEFINE BUFFER stmPacket FOR Packet.
DEFINE BUFFER edPacket  FOR Packet.

DEFINE VARIABLE mHeader       AS CHAR     NO-UNDO.
DEFINE VARIABLE mTypeT        AS CHAR     NO-UNDO.

DEFINE VARIABLE mComment      AS CHAR     NO-UNDO.
DEFINE VARIABLE mMistake      AS CHAR     NO-UNDO.
DEFINE VARIABLE mNumber       AS CHAR     NO-UNDO.
DEFINE VARIABLE mItm          AS INT64    NO-UNDO.
DEFINE VARIABLE mJtm          AS INT64    NO-UNDO.
DEFINE VARIABLE mParentID     AS INT64    NO-UNDO.
DEFINE VARIABLE mParentStr    AS CHAR     NO-UNDO.
DEFINE VARIABLE mI            AS INT64    NO-UNDO.
DEFINE VARIABLE mNextStr      AS LOGICAL  NO-UNDO.
DEFINE VARIABLE mDispl        AS LOGICAL  NO-UNDO.
DEFINE VARIABLE mCorNum       AS INT64    NO-UNDO.
DEFINE VARIABLE mMistNum      AS INT64    NO-UNDO.
DEFINE VARIABLE mFileName     AS CHAR     NO-UNDO.
DEFINE VARIABLE mCodeMsg      AS CHAR     NO-UNDO.

DEFINE VARIABLE mRecidOp      AS RECID    NO-UNDO.
DEFINE VARIABLE mRecidOpEntry AS RECID    NO-UNDO.

DEFINE TEMP-TABLE mResults NO-UNDO
   FIELD seance   AS INT64
   FIELD fname    AS CHARACTER
   FIELD kind     AS CHARACTER
   FIELD CorNum   AS INT64
   FIELD MistNum  AS INT64.

FORM
  PackObject.Kind     COLUMN-LABEL "  С О!ВИД"
                      FORMAT "x(5)"
  mNumber             COLUMN-LABEL "О Б Щ Е Н!НОМЕР"
                      FORMAT "x(9)"
  docPacket.State     COLUMN-LABEL "И Е!СТАТ"
                      FORMAT "x(4)"
  op.op-status        COLUMN-LABEL " Д О!СТАТ"
                      FORMAT "x(4)"
  op.doc-num          COLUMN-LABEL "К У М Е Н!НОМЕР"
                      FORMAT "x(7)"
  op-entry.amt-rub    COLUMN-LABEL "Т                  ! СУММА НАЦ. ВАЛ."
  mComment            COLUMN-LABEL "КОММЕНТАРИЙ"
                      FORMAT "x(60)"
  HEADER SKIP(1)
            mHeader   FORMAT "x(80)"
         SKIP(1)
WITH FRAME frOper1 WIDTH 120.

FORM
  PackObject.Kind     COLUMN-LABEL "ВИД"
                      FORMAT "x(5)"
  mNumber             COLUMN-LABEL "НОМЕР"
                      FORMAT "x(9)"
  docPacket.State     COLUMN-LABEL "СТАТ"
                      FORMAT "x(4)"
  mMistake            COLUMN-LABEL "ОШИБКИ"
                      FORMAT "x(30)"
  HEADER SKIP(1)
         mHeader      FORMAT "x(80)"
         SKIP(1)
  "СООБЩЕНИЯ "
WITH FRAME frOper2 WIDTH 120.

FORM
  mNumber             COLUMN-LABEL "НОМЕР"
                      FORMAT "x(9)"
  docPacket.State     COLUMN-LABEL "СТАТ"
                      FORMAT "x(4)"
  op.op-status        COLUMN-LABEL "СТАТ"
                      FORMAT "x(4)"
  op.doc-num          FORMAT "x(6)"
  op-entry.amt-rub
  op.ben-acct         FORMAT "x(20)"
                      COLUMN-LABEL "СЧЕТ ПЛАТЕЛЬЩИКА"
  op-entry.acct-cr    FORMAT "x(20)"
                      COLUMN-LABEL "СЧЕТ ПОЛУЧАТЕЛЯ"
  op-bank.bank-code   FORMAT "x(9)"
  HEADER SKIP(1)
            mHeader   FORMAT "x(80)"
         SKIP(1)
WITH FRAME frOper3 WIDTH 150.

FORM
   mNumber             LABEL "Номер сообщения "
                       FORMAT "x(9)"
   docPacket.State
 WITH FRAME frOper5 WIDTH 100 SIDE-LABEL 1 COL NO-UNDERLINE.

FORM
   Seance.SeanceDate LABEL "Дата сеанса  "
   Seance.Number     LABEL "Номер сеанса "
WITH FRAME frOper7 WIDTH 150 SIDE-LABEL 1 COL  NO-UNDERLINE.

FORM
  FileExch.Path       LABEL   "Файл"
                      FORMAT "x(80)"
 WITH FRAME frOper6 WIDTH 100 SIDE-LABEL 1 COL NO-UNDERLINE.


FORM
  mResults.Kind     COLUMN-LABEL "ВИД"
                    FORMAT "x(10)"
  mResults.CorNum   COLUMN-LABEL "ОБРБ"
                    FORMAT ">>>>>>>9"
  mResults.MistNum  COLUMN-LABEL "ОШБК"
                    FORMAT ">>>>>>>9"
  HEADER SKIP(1)
         mHeader      FORMAT "x(80)"
         SKIP(1)
  WITH FRAME frOper8 WIDTH 100.

FORM
  mResults.Kind     COLUMN-LABEL "ВИД"
                    FORMAT "x(10)"
  mCorNum   COLUMN-LABEL "ОБРБ"
                    FORMAT ">>>>>>>9"
  mMistNum  COLUMN-LABEL "ОШБК"
                    FORMAT ">>>>>>>9"
  HEADER SKIP(1)
         mHeader      FORMAT "x(80)"
         SKIP(1)
  WITH FRAME frOper9 WIDTH 100.

/*============================================================================*/
{setdest.i &filename='report.txt'}

if auto then
   mFileName = "report"                                    +
               string(TODAY,"999999")                      +
               FormatValueEx(string(time),"TIME","HHMMSS") +
               ".txt".
else
   mFileName = "report.txt".

{setdest.i &filename=mFileName}

PUT UNFORMATTED dept.name FORMAT "x(100)" SKIP(1).

FOR EACH tmprecid,
   FIRST Seance WHERE
   recid(Seance) EQ tmprecid.id
         NO-LOCK,
    EACH filPacket WHERE
         filPacket.SeanceID EQ
         Seance.SeanceID
         NO-LOCK,
   FIRST FileExch WHERE
         FileExch.FileExchID EQ filPacket.FileExchID
         NO-LOCK
   BREAK BY Seance.op-kind
         BY Seance.SeanceDate
         BY Seance.Number
         BY FileExch.Name:

   IF FIRST-OF(Seance.op-kind) THEN
      PUT "Результат выполнения транзакции " Seance.op-kind SKIP.

   IF FIRST-OF(Seance.SeanceDate) OR
      FIRST-OF(Seance.Number) THEN
      DISPLAY
      Seance.SeanceDate
      Seance.Number
      WITH FRAME frOper7.

   IF FIRST-OF(FileExch.Name) THEN
      DISPLAY fileExch.Path WITH FRAME frOper6.

/*----------------------------------------------------------------------------*/
   mHeader = PADC("СЛУЖЕБНЫЕ СООБЩЕНИЯ",80).

   mCodeMsg = IF INDEX(filPacket.mail-format,"BSP") EQ 0
                 THEN "EXCH-MSG"
                 ELSE "EXCH-MSGBSP".

   FOR EACH docPacket WHERE
            docPacket.ParentID   EQ filPacket.PacketID
       AND  docPacket.Class-Code EQ "PCONF"
            NO-LOCK,
      FIRST Code WHERE
            Code.Class EQ mCodeMsg
        AND Code.Code  EQ docPacket.mail-format
            NO-LOCK,
      FIRST Reference WHERE
            Reference.PacketID   EQ docPacket.PacketID
        AND Reference.Class-Code EQ Code.Misc[{&RKC-REPLY}]
            NO-LOCK
      BREAK BY docPacket.State
            BY docPacket.mail-format
            BY Reference.RefValue
       WITH FRAME frOper1 DOWN:

      mNumber = ENTRY(2,Reference.RefValue,"|").

      mTypeT = SUBSTR(GetEntries(2,docPacket.mail-format,"-",""),1,5).
      IF mTypeT EQ ? THEN mTypeT = "".
               
      DISPLAY mTypeT @ PackObject.Kind
              mNumber
              docPacket.State
      WITH FRAME frOper1.

      mNextStr = NO.
      FOR EACH PackObject WHERE
               PackObject.PacketID  EQ docPacket.PacketID
           AND PackObject.File-Name EQ "op-entry"
               NO-LOCK,
         FIRST op-entry WHERE
               op-entry.op       EQ INT64(ENTRY(1,PackObject.Surrogate))
           AND op-entry.op-entry EQ INT64(ENTRY(2,PackObject.Surrogate))
               NO-LOCK,
         FIRST op WHERE op.op EQ op-entry.op
               NO-LOCK
               BREAK BY op.doc-num:

         ASSIGN
            mRecidOp      =  RECID(op)
            mRecidOpEntry =  RECID(op-entry).

         IF mNextStr = YES THEN DOWN WITH FRAME frOper1.
         DISPLAY PackObject.Kind
                 op.doc-num
                 op.op-status
                 op-entry.amt-rub
            WITH FRAME frOper1.
         mNextStr = YES.

         RUN mResultsTable(BUFFER mResults).
      END.                                       /* FOR EACH PackObject       */

      IF PacketReadOpen(docPacket.PacketID) THEN DO:
         REPEAT:
            IF NOT PacketReadLine(docPacket.PacketID, OUTPUT mComment) THEN DO:
               IF NOT {assigned mComment} THEN DOWN WITH FRAME frOper1.
               LEAVE.
            END.

            DISPLAY
               mComment
            WITH FRAME frOper1.
            DOWN WITH FRAME frOper1.
         END.
         PacketReadClose(docPacket.PacketID).
      END.
      ELSE 
         DOWN WITH FRAME frOper1. 

      RUN FillErrorTable (INPUT        docPacket.ClassError,
                          INPUT        docPacket.PackError,
                          OUTPUT TABLE ttError).

      FOR EACH ttError:
         PUT UNFORMATTED
         FILL(" ",9)
         string(ttError.Code,"x(12)") "      "
         string(ttError.Name,"x(50)") " "
         string(ttError.Type,"x(15)")  SKIP.

         IF    ttError.CODE EQ "rkl20"
            OR ttError.CODE EQ "rkl20k"  
            OR ttError.CODE EQ "rkl21" THEN
            RUN mINNorName (mRecidOp,
                            mRecidOpEntry,
                            INPUT ttError.CODE).
      END.
      IF NOT CAN-FIND(FIRST ttError) THEN  DOWN WITH FRAME frOper1.
   END.
/*----------------------------------------------------------------------------*/
   mHeader = PADC("ИНФОРМАЦИОННЫЕ СООБЩЕНИЯ",80).
   FOR EACH docPacket WHERE
            docPacket.ParentID   EQ      filPacket.PacketID
       AND  docPacket.Class-Code BEGINS "PRQ"
            NO-LOCK,
      FIRST Code WHERE
            Code.Class EQ "EXCH-MSG"
        AND Code.Code  EQ docPacket.mail-format
            NO-LOCK,
      FIRST Reference WHERE
            Reference.PacketID   EQ docPacket.PacketID
        AND Reference.Class-Code EQ Code.Misc[{&RKC-REPLY}]
            NO-LOCK
            BREAK BY docPacket.State
                  BY Reference.RefValue
            WITH FRAME frOper2 DOWN:

      mNumber = ENTRY(2,Reference.RefValue,"|").
      DISPLAY mNumber
              docPacket.State
              SUBSTRING(DocPacket.mail-format,5,5) @ PackObject.Kind
      WITH FRAME frOper2.

      FOR EACH PackObject WHERE
               PackObject.PacketID  EQ docPacket.PacketID
           AND PackObject.File-Name EQ "op-entry"
               NO-LOCK:

         DISPLAY SUBSTR(DocPacket.mail-format,5,5) @ PackObject.Kind
         WITH FRAME frOper2.

         RUN mResultsTable(BUFFER mResults).

      END.

      IF DocPacket.mail-format BEGINS "XML-ED330" THEN
         FOR EACH PacketText OF DocPacket NO-LOCK:
            RUN mPutMsgTxt(INPUT PacketText.contents).
         END.

      RUN FillErrorTable (INPUT        docPacket.ClassError,
                          INPUT        docPacket.PackError,
                          OUTPUT TABLE ttError).
      FIND FIRST ttError NO-ERROR.
      IF NOT AVAIL(ttError) THEN DOWN WITH  FRAME frOper2.

      FOR EACH ttError:
          mJtm  = 0.
          mItm  = 1.
          DO WHILE (AVAIL(ttError) AND mItm LE NUM-ENTRIES(ttError.NAME)):
             mJtm = mJtm + 1.
             IF AVAIL(ttError) THEN mMistake = GetPartStr(ENTRY(mItm,ttError.NAME),30,mJtm).
                               ELSE mMistake = "".
             IF NOT {assigned mMistake} THEN DO:
                mItm = mItm + 1.
                mJtm = 0.
             END.
             ELSE DO:
                DISPLAY mMistake
                WITH FRAME frOper2.
                DOWN WITH  FRAME frOper2.
             END.
          END.
      END.                                       /* FOR EACH ttError          */

   END.                                          /* FOR EACH docPacket WHERE  */

   mHeader = PADC("ИНФОРМАЦИОННЫЕ СООБЩЕНИЯ",80).

   IF filPacket.mail-format BEGINS "XML-ED333" THEN 
   DO:
      mCodeMsg = IF INDEX(filPacket.mail-format,"BSP") EQ 0
                    THEN "EXCH-MSG"
                    ELSE "EXCH-MSGBSP".

      FOR FIRST Code WHERE
                Code.Class EQ mCodeMsg
            AND Code.Code  EQ filPacket.mail-format
                NO-LOCK,
        FIRST Reference WHERE
                Reference.PacketID   EQ filPacket.PacketID
            AND Reference.Class-Code EQ Code.Misc[{&RKC-REPLY}]
                NO-LOCK
                BREAK BY Reference.RefValue
                WITH FRAME frOper2 DOWN:
   
         mNumber = ENTRY(2,Reference.RefValue,"|").

         DISPLAY mNumber
                 filPacket.State @ docPacket.State
                 SUBSTRING(filPacket.mail-format,5,5) @ PackObject.Kind
         WITH FRAME frOper2.
   
         FOR EACH PacketText OF filPacket NO-LOCK:
            RUN mPutMsgTxt(INPUT PacketText.contents).
         END.
   
         DOWN WITH  FRAME frOper2.
      END.
   END. /* filPacket.mail-format BEGINS "XML-ED333"  */
   ELSE IF filPacket.mail-format EQ "XML-ED330BSP" THEN
   DO:
      DISPLAY filPacket.State @ docPacket.State
              SUBSTRING(filPacket.mail-format, 5, 5) @ PackObject.Kind
      WITH FRAME frOper2.
      FOR EACH PacketText OF filPacket NO-LOCK:
         RUN mPutMsgTxt(INPUT PacketText.contents). 
      END.             
   END.                                             

/*----------------------------------------------------------------------------*/
   mHeader = PADC("ОТВЕТНЫЕ ДОКУМЕНТЫ",80).
   mParentID = filPacket.PacketID.

   mParentStr = STRING(mParentID).
   FOR EACH edPacket WHERE
      edPacket.ParentID EQ mParentID AND
      edPacket.mail-format EQ "XML-ED273" NO-LOCK:
      mParentStr = mParentStr + "," + STRING(edPacket.PacketID).
   END.

   DO mI = 1 TO NUM-ENTRIES(mParentStr):
      mParentID = INT64(ENTRY(mI,mParentStr)).

      FOR EACH docPacket WHERE
               docPacket.ParentID   EQ mParentID
          AND  docPacket.Class-Code EQ "PRKCXML"
          AND  docPacket.Kind EQ "ExchRKCDoc"
               NO-LOCK,
         FIRST Code WHERE
               Code.Class EQ "EXCH-MSG"
           AND Code.Code  EQ docPacket.mail-format
               NO-LOCK,
         FIRST Reference WHERE
               Reference.PacketID   EQ docPacket.PacketID
           AND Reference.Class-Code EQ Code.Misc[{&RKC-REPLY}]
               NO-LOCK
               BREAK BY docPacket.State
                     BY Reference.RefValue
               WITH FRAME frOper3 DOWN:
      
         mNumber = ENTRY(2,Reference.RefValue,"|").
         DISPLAY
            docPacket.State
            mNumber
         WITH FRAME frOper3.
      
         FOR EACH PackObject WHERE
                  PackObject.PacketID  EQ docPacket.PacketID
              AND PackObject.File-Name EQ "op-entry"
                  NO-LOCK,
            FIRST op-entry WHERE
                  op-entry.op       EQ INT64(ENTRY(1,PackObject.Surrogate))
              AND op-entry.op-entry EQ INT64(ENTRY(2,PackObject.Surrogate))
                  NO-LOCK,
            FIRST op WHERE op.op EQ op-entry.op
                  NO-LOCK,
            FIRST op-bank  WHERE
                  op-bank.op EQ op.op NO-LOCK
                  BREAK BY op.doc-num:
      
            ASSIGN
               mRecidOp      =  RECID(op)
               mRecidOpEntry =  RECID(op-entry).
      
            IF op.ben-acct <>"" THEN
                 DISPLAY op.ben-acct WITH FRAME frOper3.
            ELSE DISPLAY op-entry.acct-db @ op.ben-acct WITH FRAME frOper3.
      
            DISPLAY op.doc-num
                    op.op-status
                    op-entry.amt-rub
                    op-entry.acct-cr
                    op-bank.bank-code
            WITH FRAME frOper3.
      
            RUN mResultsTable(BUFFER mResults).
      
         END.
      
         RUN FillErrorTable (INPUT        docPacket.ClassError,
                             INPUT        docPacket.PackError,
                             OUTPUT TABLE ttError).
      
         FOR EACH ttError:  
            PUT UNFORMATTED
               FILL(" ",9)
               string(ttError.Code,"x(12)") "      "
               string(ttError.Name,"x(50)") " "
               string(ttError.Type,"x(15)")  SKIP.
      
            IF    ttError.CODE EQ "rkl20"
               OR ttError.CODE EQ "rkl20k" 
               OR ttError.CODE EQ "rkl21" THEN 
               RUN mINNorName (mRecidOp,
                               mRecidOpEntry,
                               INPUT ttError.CODE).
      
         END.
         IF NOT CAN-FIND(FIRST ttError) THEN  DOWN WITH FRAME frOper3.
      END.                                          /* FOR EACH DocPacket        */
   END.

/*----------------------------------------------------------------------------*/
   mHeader = PADC("ВЫПИСКА",80).
   PUT UNFORMATTED SKIP(1).
   FOR EACH docPacket WHERE
            docPacket.ParentID   EQ filPacket.PacketID
       AND  docPacket.Class-Code EQ "PSTMT"
       AND  docPacket.Kind EQ "ExchRKCStm"
            NO-LOCK,
      FIRST Code WHERE
            Code.Class EQ "EXCH-MSG"
        AND Code.Code  EQ docPacket.mail-format
            NO-LOCK,
      FIRST Reference WHERE
            Reference.PacketID   EQ docPacket.PacketID
        AND Reference.Class-Code EQ Code.Misc[{&RKC-REPLY}]
            NO-LOCK
            BREAK BY docPacket.State
            BY Reference.RefValue
       WITH FRAME frOper3 DOWN:

      IF PacketReadOpen(docPacket.PacketID) THEN DO:
         REPEAT:
            IF NOT PacketReadLine(docPacket.PacketID, OUTPUT mComment) THEN
               LEAVE.
            PUT UNFORMATTED mComment SKIP.
         END.
      PacketReadClose(docPacket.PacketID).
      END.

      mNumber = ENTRY(2,Reference.RefValue,"|").
      DISPLAY mNumber
              docPacket.State
              WITH FRAME frOper5.

      FOR EACH stmPacket WHERE
               stmPacket.ParentID EQ docPacket.PacketID
                NO-LOCK
               BREAK BY stmPacket.State:

         FOR EACH PackObject WHERE
                  PackObject.PacketID  EQ stmPacket.PacketID
              AND PackObject.File-Name EQ "op-entry"
                  NO-LOCK,
            FIRST op-entry WHERE
                  op-entry.op       EQ INT64(ENTRY(1,PackObject.Surrogate))
              AND op-entry.op-entry EQ INT64(ENTRY(2,PackObject.Surrogate))
                  NO-LOCK,
            FIRST op WHERE
                  op.op EQ op-entry.op
                  NO-LOCK,
            FIRST op-bank  WHERE
                  op-bank.op EQ op.op NO-LOCK
            BREAK BY op.doc-num:

         ASSIGN
            mRecidOp      =  RECID(op)
            mRecidOpEntry =  RECID(op-entry).

            DISPLAY stmPacket.State @ docPacket.State
                    op.doc-num
                    op.op-status
                    op-entry.amt-rub
                    op-bank.bank-code
            WITH FRAME frOper3.

            IF op.doc-kind EQ  "rec" THEN        /* Начальные докум           */
               DISPLAY
                    GetXAttrValueEx("op",
                                  string(op.op),
                                  "acct-send",
                                  op-entry.acct-db) @ op.ben-acct
                    (IF {assigned op.ben-acct}
                     THEN op.ben-acct
                     ELSE op-entry.acct-cr)         @ op-entry.acct-cr
               WITH FRAME frOper3.
            ELSE
               DISPLAY
                    (IF {assigned op.ben-acct}
                     THEN op.ben-acct
                     ELSE op-entry.acct-db)         @ op.ben-acct
                    GetXAttrValueEx("op",
                                  string(op.op),
                                  "acct-rec",
                                  op-entry.acct-cr) @ op-entry.acct-cr
               WITH FRAME frOper3.

            DOWN WITH  FRAME frOper3.

            RUN mResultsTable(BUFFER mResults).

         END.                                    /* FOR EACH PackObject       */

         RUN FillErrorTable (INPUT        stmPacket.ClassError,
                             INPUT        stmPacket.PackError,
                             OUTPUT TABLE ttError).
         FOR EACH ttError:
            PUT UNFORMATTED
               FILL(" ",9)
               STRING(ttError.Code,"x(12)") "      "
               STRING(ttError.Name,"x(50)") " "
               STRING(ttError.Type,"x(15)")  SKIP.

            IF    ttError.CODE EQ "rkl20"
               OR ttError.CODE EQ "rkl20k"
               OR ttError.CODE EQ "rkl21" THEN 
               RUN mINNorName (mRecidOp,
                               mRecidOpEntry, 
                               INPUT ttError.CODE).

         END. 
         IF NOT CAN-FIND(FIRST ttError) THEN  DOWN WITH FRAME frOper3.

      END.                                       /*  FOR EACH stmPacket       */
   END.                                          /* FOR EACH docPacket (STMP) */

   IF LAST-OF(FileExch.Name) THEN DO:
      mHeader =  "ИТОГО ПО ФАЙЛУ " + FileExch.NAME.
      FOR EACH mResults WHERE
         mResults.fName EQ fileExch.NAME
         WITH FRAME frOper8 DOWN
         BREAK BY mResults.kind:
         DISPLAY
            mResults.kind
            mResults.CorNum
            mResults.MistNum
         WITH FRAME frOper8.
         DOWN WITH FRAME frOper8.
      END.
   END.

   mDispl = NO.
   IF LAST-OF(Seance.Number) THEN DO:
      mHeader =  "ИТОГО ПО  СЕАНСУ " + STRING(Seance.Number).
      FOR EACH mResults WHERE
         mResults.seance EQ Seance.SeanceID
         WITH FRAME frOper9 DOWN
         BREAK BY mResults.kind:

         IF mResults.fname <> FileExch.NAME THEN mDispl = YES.

         IF FIRST-OF(mResults.kind) THEN DO:
         mCorNum  = 0.
         mMistNum = 0.
         END.
         mCorNum  = mCorNum  + mResults.CorNum.
         mMistNum = mMistNum + mResults.MistNum.
         IF (LAST-OF(mResults.kind)  AND mDispl = YES) THEN
         DISPLAY
            mResults.kind
            mCorNum
            mMistNum
         WITH FRAME frOper9.
         DOWN WITH FRAME frOper9.
      END.
   END.
END.                     /* FOR EACH tmprecid  */

IF auto AND NOT {assigned mHeader} THEN
   OS-DELETE VALUE(mFileName).

FIND FIRST user-proc WHERE user-proc.procedure EQ "snc-piis"  
                       AND user-proc.partition EQ "РАЗНОЕ"   NO-LOCK NO-ERROR.
IF AVAIL user-proc THEN DO:
   RUN SetSysConf IN h_base ("user-proc-id",RECID(user-proc)).
   {signatur.i}
END.
ELSE DO:
   {signatur.i &user-only=yes}
END.

{preview.i &filename=mFileName}
/*----------------------------------------------------------------------------*/
/* Заполнение таблицы для вывода итогов                                       */
/*----------------------------------------------------------------------------*/
PROCEDURE mResultsTable:
   DEFINE PARAMETER BUFFER mResults FOR mResults.

   IF NOT CAN-FIND (mResults WHERE
                    mResults.kind   EQ PackObject.Kind
                AND mResults.seance EQ Seance.SeanceID
                AND mResults.fname  EQ FileExch.NAME) THEN DO:
      CREATE mResults.
      ASSIGN
         mResults.kind   = PackObject.Kind
         mResults.seance = Seance.SeanceID
         mResults.fname  = FileExch.NAME.
         IF docPacket.State EQ "ОБРБ" THEN  mResults.CorNum  = 1.
         IF docPacket.State EQ "ОШБК" THEN  mResults.MistNum = 1.
   END.
   ELSE DO:
      FIND FIRST mResults WHERE
                 mResults.kind   EQ PackObject.Kind
             AND mResults.seance EQ Seance.SeanceID
             AND mResults.fname  EQ FileExch.NAME NO-ERROR.
      IF docPacket.State EQ "ОБРБ" THEN mResults.CorNum  = mResults.CorNum  + 1.
      IF docPacket.State EQ "ОШБК" THEN mResults.MistNum = mResults.MistNum + 1.
   END.

END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* Отображение информации для ошибок rkl20 и rkl21                            */
/*----------------------------------------------------------------------------*/
PROCEDURE mINNorName:
   DEFINE INPUT PARAMETER iRecidOp          AS RECID     NO-UNDO.
   DEFINE INPUT PARAMETER iRecidOpEntry     AS RECID     NO-UNDO.
   DEFINE INPUT PARAMETER iErrCode          AS CHARACTER NO-UNDO.

   DEFINE VAR vNameBase                     AS CHARACTER NO-UNDO.
   DEFINE VAR vNameShort                    AS CHARACTER NO-UNDO.
   DEFINE VAR vINNBase                      AS CHARACTER NO-UNDO.
   DEFINE VAR vAcct                         AS CHARACTER NO-UNDO.

   DEF BUFFER bOp      FOR op.
   DEF BUFFER bOpEntry FOR op-entry.

   FIND FIRST bOp      WHERE RECID(bOp)      EQ iRecidOp      NO-LOCK NO-ERROR.
   FIND FIRST bOpEntry WHERE RECID(bOpEntry) EQ iRecidOpEntry NO-LOCK NO-ERROR.

   IF AVAIL bOp AND AVAIL bOpEntry THEN DO:
      vAcct =    IF bOp.doc-kind EQ  "rec" THEN       /* Начальные документы */
                    GetXAttrValueEx("op",
                                     STRING(bOp.op),
                                     "acct-send",
                                     bOpEntry.acct-db)
                 ELSE                                 /* Ответные  документы */
                    GetXAttrValueEx("op",
                                     STRING(bOp.op),
                                     "acct-rec",
                                     bOpEntry.acct-cr).
   
   {find-act.i
      &acct = vAcct
   }

      IF AVAIL(acct) THEN DO:
         RUN GetINNAndName ((bOp.doc-kind = "rec"),
                            BUFFER Acct,
                            OUTPUT vINNBase,
                            OUTPUT vNameBase).

         RUN GetCustShortName(INPUT  Acct.cust-cat,
                              INPUT  Acct.cust-id,
                              OUTPUT vNameShort).
      END.

      PUT UNFORMATTED
      FILL(" ",12) 
      "Наименование из базы данных : " ENTRY(1,vNameBase,CHR(250)) SKIP.

      IF {assigned vNameShort} THEN
      PUT UNFORMATTED
      FILL(" ",12)
      "Краткое наименование из базы: " ENTRY(1,vNameShort,CHR(250)) SKIP.
      PUT UNFORMATTED
      FILL(" ",12)
      "Наименование из файла       : " GetXattrValueEx('op',STRING(bOp.op),'name-rec','') SKIP.
   
      IF iErrCode EQ "rkl21" THEN 
         PUT UNFORMATTED
         FILL(" ",12)  
         "ИНН из базы данных: " vINNBase  SKIP
         FILL(" ",12)
         "ИНН из файла      : " GetXattrValueEx('op',STRING(bOp.op),'inn-rec','') SKIP.
   END.
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* Вывод текста информационного сообщения */
/*----------------------------------------------------------------------------*/
PROCEDURE mPutMsgTxt:
   
   DEFINE INPUT PARAMETER iTxt AS CHAR NO-UNDO.
   
   DEFINE VARIABLE mTxt AS CHAR  EXTENT {&mPackTxtNumStr} NO-UNDO.
   DEFINE VARIABLE mI   AS INT64                          NO-UNDO.
   
   mTxT[1] = iTxt /* PacketText.contents */.
   {wordwrap.i &s="mTxT" &n=~{&mPackTxtNumStr~} &l=80}
   DO mI = 1 TO {&mPackTxtNumStr} - 1:
      IF {assigned mTxT[mI]} THEN
         PUT UNFORMATTED mTxT[mI] SKIP.
   END.
   /* если строк больше */
   IF {assigned mTxT[mI]} THEN 
      PUT UNFORMATTED "..." SKIP
                      "Слишком большой объем информации для отображения в протоколе." SKIP
                      "Полностью весь текст просмотрите в сообщении ED330." SKIP.   
END PROCEDURE.
/******************************************************************************/
/* $LINTUSER='MIKE' */
/* $LINTENV ='dpl' */
/* $LINTVSS ='$/ws3-dpl/bq' */
/* $LINTDATE='08/12/2014 20:19:56.344+04:00' */
/* $LINTFILE='snc-piis.p' */
/*prosign6A9Te9aycLZ7r7cOnbKMaw*/