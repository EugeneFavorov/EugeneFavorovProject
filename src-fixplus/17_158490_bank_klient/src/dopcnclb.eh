/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2015 ЗАО "Банковские информационные системы"
     Filename: dopcnclb.eh
      Comment: Собственная форма просмотра классификатора 
               "ДопКонтрКЛБ"
   Parameters:
         Uses:
      Used by:
      Created: 06.08.2015 IT    
*/

code.val:SCREEN-VALUE IN FRAME edit = "действует".

ON LEAVE OF code.code IN FRAME edit
DO:
   DEFINE VARIABLE vScreenCode AS CHARACTER   NO-UNDO.
   DEFINE BUFFER bCode FOR code.
   {&BEG_BT_LEAVE}
   IF NOT {assigned SELF:SCREEN-VALUE } THEN
   DO:
      RUN Fill-SysMes IN h_tmess ("","","-1","Поле должно быть заполнено.").
      RETURN NO-APPLY {&RET-ERROR}.
   END.
   vScreenCode = code.code:SCREEN-VALUE.
   IF CAN-FIND(FIRST bCode WHERE
                     bCode.code   EQ vScreenCode
                 AND bCode.class  EQ in-class
                 AND RECID(bCode) NE RECID(code)) THEN
   DO:
      RUN Fill-SysMes IN h_tmess ("","","-1","Параметр с указанным кодом уже существует!").
      RETURN NO-APPLY {&RET-ERROR}.
   END.
   {&END_BT_LEAVE}
END.

ON LEAVE OF code.misc[2] IN FRAME edit
DO:
   {&BEG_BT_LEAVE}
   IF NOT {assigned SELF:SCREEN-VALUE }      AND
      NOT {assigned code.name:SCREEN-VALUE } THEN
   DO:
      RUN Fill-SysMes IN h_tmess ("","","-1",
      "Должно быть заполнено или поле 'Процедура' или поле 'Фильтр'.").
      RETURN NO-APPLY {&RET-ERROR}.
   END.
   {&END_BT_LEAVE}
END.

ON LEAVE OF code.misc[3] IN FRAME edit
DO:
   {&BEG_BT_LEAVE}
   IF NOT {assigned SELF:SCREEN-VALUE } THEN
   DO:
      RUN Fill-SysMes IN h_tmess ("","","-1","Поле должно быть заполнено.").
      RETURN NO-APPLY {&RET-ERROR}.
   END.
   {&END_BT_LEAVE}
END.

ON LEAVE OF code.val IN FRAME edit
DO:
   {&BEG_BT_LEAVE}
   IF code.val:SCREEN-VALUE NE "действует" THEN
      code.val:SCREEN-VALUE = "не действует".
   {&END_BT_LEAVE}
END.

ON " " OF code.val IN FRAME edit
DO:
   code.val:SCREEN-VALUE = (IF code.val:SCREEN-VALUE EQ "действует" THEN "не действует" ELSE "действует").
   RETURN NO-APPLY.
END.

ON GO OF code.code    IN FRAME edit,
         code.name    IN FRAME edit,
         code.misc[1] IN FRAME edit,
         code.misc[2] IN FRAME edit,
         code.misc[3] IN FRAME edit,
         code.val     IN FRAME edit
DO:
   APPLY "LEAVE" TO code.code    IN FRAME edit.
   IF ERROR-STATUS:ERROR OR
      RETURN-VALUE EQ {&RET-ERROR} THEN
      RETURN NO-APPLY {&RET-ERROR}.
   APPLY "LEAVE" TO code.name    IN FRAME edit.
   IF ERROR-STATUS:ERROR OR
      RETURN-VALUE EQ {&RET-ERROR} THEN
      RETURN NO-APPLY {&RET-ERROR}.
   APPLY "LEAVE" TO code.misc[1] IN FRAME edit.
   IF ERROR-STATUS:ERROR OR
      RETURN-VALUE EQ {&RET-ERROR} THEN
      RETURN NO-APPLY {&RET-ERROR}.
   APPLY "LEAVE" TO code.misc[2] IN FRAME edit.
   IF ERROR-STATUS:ERROR OR
      RETURN-VALUE EQ {&RET-ERROR} THEN
      RETURN NO-APPLY {&RET-ERROR}.
   APPLY "LEAVE" TO code.misc[3] IN FRAME edit.
   IF ERROR-STATUS:ERROR OR
      RETURN-VALUE EQ {&RET-ERROR} THEN
      RETURN NO-APPLY {&RET-ERROR}.
   APPLY "LEAVE" TO code.val     IN FRAME edit.
   IF ERROR-STATUS:ERROR OR
      RETURN-VALUE EQ {&RET-ERROR} THEN
      RETURN NO-APPLY {&RET-ERROR}.
END.

