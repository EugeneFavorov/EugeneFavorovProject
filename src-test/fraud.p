/*
               Банковская интегрированная система QBIS
    Copyright: (C) 1992-2017 АО "Банковские информационные системы"
     Filename: POSTJKU.P
      Comment: Справочник ПоставщикиЖКУ
   Parameters: iClass  - классификатор
               iParent - родитель
               iTitle  - заголовок
               iLevel  - уровень
      Created: 29.04.2016 VASOV
     Modified: 
*/


DEFINE INPUT PARAMETER iClass  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iParent AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iTitle  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iLevel  AS INT64     NO-UNDO.

RUN browseld.p ("DataBlock", "DataClass-Id", "fraud", "", iLevel).
