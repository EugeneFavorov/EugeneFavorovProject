/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2015 ЗАО "Банковские информационные системы"
     Filename: pfr-ved.p
      Comment: 0161576 Создать функционал. обмен с ПФР - договор о доставке пенсий
   Parameters: SeanceId
         Uses:
      Used by:
      Created: 16/05/2013 KMBIS 0161576 Создать функционал. обмен с ПФР - договор о доставке пенсий
                                        Выводим на экран файлы с логами обработки
     Modified: 
                                    
*/
DEF INPUT PARAM iSeanceID AS INT64  NO-UNDO.

{globals.i}
{intrface.get filex}
{intrface.get xclass}


DEF VAR mFileLog AS CHAR NO-UNDO.

{setdest.i}

FOR EACH Packet WHERE Packet.SeanceID EQ iSeanceID
                  AND Packet.ParentID EQ 0
                 NO-LOCK,
   FIRST FileExch WHERE FileExch.FileExchID EQ Packet.FileExchID
                  NO-LOCK:
   mFileLog = GetXAttrValueEx("packet", STRING(Packet.PacketID), "LogName", "").

   IF NOT {assigned mFileLog} THEN
   DO:
      ASSIGN
         mFileLog = CatalogGetPath(Packet.Mail-user-num,"LogArch","Path")
         mFileLog = MakeFileName(mFileLog, FileExch.Name) + ".log"
      .
   END. /* IF NOT {assigned mFileLog} THEN */

   IF {assigned mFileLog} THEN
      mFileLog = SEARCH(mFileLog).

   IF {assigned mFileLog} THEN
   DO:
      {preview.i &filename=mFileLog }
   END. /* IF {assigned mFileLog} THEN */
END. /* FOR FIRST Packet WHERE Packet.SeanceID EQ iSeanceID */
