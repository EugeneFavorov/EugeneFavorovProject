/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2010 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: ptsregbrw.EH
      Comment: ��������(ॣ������) ���㬥�⮢ - �ਣ���� ���
   Parameters:
         Uses:
      Used by:
      Created:    
     Modified:    
*/

DEF BUFFER bfplindocsreg FOR pl_indocsreg.

ON LEAVE OF Branch_ IN FRAME edit
DO:
    IF LASTKEY <> KEYCODE("UP") THEN DO:
        FIND FIRST branch WHERE branch.branch-id = Branch_:SCREEN-VALUE IN FRAME edit NO-LOCK NO-ERROR.
        IF NOT AVAIL branch THEN DO:
            MESSAGE "��� ⠪��� ���ࠧ�������, �롮� �� F1" VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN NO-APPLY {&RET-ERROR}.
        END.
    END.
END.
 
ON LEAVE OF NameEvent IN FRAME edit
DO:
    CASE EnableDescription(NameEvent:SCREEN-VALUE IN FRAME edit):
        WHEN 0 THEN DO:
            DISABLE Descriptions WITH FRAME edit.
  /*          PrinalFIO:LABEL = "". 
            Descriptions:LABEL = "".  */ 
        END.
        WHEN 1 THEN DO:
            ENABLE Descriptions WITH FRAME edit.
            PrinalFIO:LABEL = "�ਭ� ���". 
            Descriptions:LABEL = "�ਭ�". 
        END.
        WHEN 2 THEN DO:
            ENABLE Descriptions WITH FRAME edit.
/*            PrinalFIO:LABEL = "�뤠� ���".
            Descriptions:LABEL = "�뤠�".  */ 
            PrinalFIO:LABEL = "�ਭ� ���". 
            Descriptions:LABEL = "�ਭ�".
        END.
    END CASE.
    IF LASTKEY <> KEYCODE("UP") THEN DO:
        IF CheckNameEvent(NameEvent:SCREEN-VALUE IN FRAME edit) = FALSE THEN RETURN NO-APPLY {&RET-ERROR}.
    END.
END.   

ON LEAVE OF Descriptions IN FRAME edit
DO:
    IF LASTKEY <> KEYCODE("UP") THEN DO:
        IF EnableDescription(NameEvent:SCREEN-VALUE IN FRAME edit) > 0 AND CheckUser(Descriptions:SCREEN-VALUE IN FRAME edit) = FALSE THEN DO:
            RETURN NO-APPLY {&RET-ERROR}.
        END.
    END. 
END.
/* �⪫�祭� � �裡 � ������������ ����� ���� ����㯫���� �� ����砭�� �ப� ᤠ� 
�� �஢�ન ��� � ptsregbrw.upd
 
ON LEAVE OF DateValue IN FRAME edit
DO:    
    IF CheckDateValue(DATE(DateValue:SCREEN-VALUE IN FRAME edit)) = FALSE THEN RETURN NO-APPLY {&RET-ERROR}.
END.    

*/
