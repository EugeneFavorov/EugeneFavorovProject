/*
               ������᪠� ��⥣�஢����� ��⥬� QBIS
    Copyright: (C) 1992-2017 �� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: POSTJKU.P
      Comment: ��ࠢ�筨� ���⠢騪����
   Parameters: iClass  - �����䨪���
               iParent - த�⥫�
               iTitle  - ���������
               iLevel  - �஢���
      Created: 29.04.2016 VASOV
     Modified: 
*/


DEFINE INPUT PARAMETER iClass  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iParent AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iTitle  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iLevel  AS INT64     NO-UNDO.

RUN browseld.p ("DataBlock", "DataClass-Id", "fraud", "", iLevel).
