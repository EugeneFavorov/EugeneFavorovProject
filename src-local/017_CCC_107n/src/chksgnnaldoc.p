/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2016 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: chksgnnaldoc.p
      Comment: ��楤�� �஢�ન ४����⮢ ��������� ४����⮢ ���㬥��
   Parameters: INPUT �������� ४������, oOk - १���� �஢�ન
         Uses:
      Used by:
      Created: 
     Modified:
*/
{globals.i}
{intrface.get xclass}
{intrface.get date}
{intrface.get tmess}
{intrface.get op}

 DEFINE INPUT  PARAMETER iPokST AS CHARACTER   NO-UNDO.
 DEFINE INPUT  PARAMETER iKBK   AS CHARACTER   NO-UNDO.
 DEFINE INPUT  PARAMETER iPokOP AS CHARACTER   NO-UNDO.
 DEFINE INPUT  PARAMETER iPokNP AS CHARACTER   NO-UNDO.
 DEFINE INPUT  PARAMETER iPokND AS CHARACTER   NO-UNDO.
 DEFINE INPUT  PARAMETER iPokDD AS CHARACTER   NO-UNDO.
 DEFINE INPUT  PARAMETER iBenAcct AS CHARACTER NO-UNDO.
 DEFINE OUTPUT PARAMETER oOk AS LOGICAL NO-UNDO INIT YES.

{148n.i}

 DEFINE VARIABLE mKBKNalog  AS CHARACTER NO-UNDO.
 DEFINE VARIABLE mKBKCustom AS CHARACTER NO-UNDO.
 DEFINE VARIABLE mDescPokOP AS CHARACTER NO-UNDO.
 DEFINE VARIABLE mErrorMsg  AS CHARACTER NO-UNDO.

/*
   �᫨ ����� (101) �� ��������� ��� ᮤ�ন� "0",
   ���� �� �⪫�祭�� ����஫� ���।�⢮� �� ���.��������148,
   ������� �஢�ન �� �ந��������
*/
IF NOT isAssigned148n(iPokST) OR
   FGetSetting("���", "��������148", "��") = "���"
THEN DO:
   oOK = NO.
   {intrface.del}
   RETURN.
END.

 ASSIGN
  mKBKNalog  = FGetSetting("���","��������","")
  mKBKCustom = FGetSetting("���","��������","")
  mDescPokOP = GetCodeDesc("���:��",iPokOP,1,"").

  CREATE ttNalRec.
  ASSIGN
     ttNalRec.ben-acct = iBenAcct
     ttNalRec.KBK      = iKBK
     ttNalRec.PokOP    = iPokOP
     ttNalRec.PokNP    = iPokNP.

  IF iKBK NE "0" AND
     (iPokST EQ "0" OR
      iPokST EQ "00")
     THEN DO:
        RUN Fill-SysMes IN h_tmess ("","","-1",
              "�� �ன��� ��४���� ����஫� ����� (101) ����� ���⥫�騪� � (104) ���").
        RETURN.
     END.

    IF mDescPokOP NE "" AND 
     ((CAN-DO(mKBKNalog,iKBK)  AND NOT CAN-DO(mDescPokOP,"�")) OR
      (CAN-DO(mKBKCustom,iKBK) AND NOT CAN-DO(mDescPokOP,"�")))       
/*
 ���� �⪫�砥�.
 �� ��������� ��室�� �����४�� ���⥦�, ����� ���� �ਭ���.
*/    
  /* OR
     ( NOT CAN-DO(mKBKNalog,iKBK)  AND
       NOT CAN-DO(mKBKCustom,iKBK) AND
       iPokOP     NE "0"))
       */
       THEN DO:
          RUN Fill-SysMes IN h_tmess ("","","-1",
                   "�� �ன��� ��४���� ����஫� ����� (104) ��� � (106) �᭮����� ���⥦�").
          RETURN.
       END.



  /* ���� ��४���� ����஫� (106) � (107) ����� */
  RUN check148n-106-107 IN THIS-PROCEDURE (TABLE ttNalRec BY-REFERENCE,
                                           OUTPUT mErrorMsg).
  IF {assigned mErrorMsg} THEN DO:
      RUN Fill-SysMes IN h_tmess ("", "", "-1", mErrorMsg).
      RETURN.
  END.
         /* �ॡ���� ��筥��� !!! */
/*
  IF iPokOP = "��" AND
     (iPokND <> "0" OR Str2Date(REPLACE(iPokDD,".","/")) = ?)
  THEN DO:
     RUN Fill-SysMes IN h_tmess ("",
                                 "",
                                 "-1",
                                 "�� �ன��� ��४���� ����஫� ����� " +
                                 "(108) ����� ���㬥�� � (109) ��� ���㬥��").
     RETURN.
  END.
*/

  IF CAN-DO(FGetSetting("���", "��106�����0", ""), iPokOP) AND
     iPokND <> "0"
  THEN DO:
     RUN Fill-SysMes IN h_tmess ("",
                                 "",
                                 "-1",
                                 "�� �ன��� ��४���� ����஫� ����� " +
                                 "(106) �᭮����� ���⥦� � (108) ����� ���㬥��").
     RETURN.
  END.

/*
��ࠢ���� � �����:
�� ��室�� ��砩, �����

�᭮����� (106): ��
    ����� (108): 0
     ��� (109): 11.11.2016

*/

         /* �ॡ���� ��筥��� !!! */
/*
  IF ((iPokND NE "0"          AND
      INDEX(iPokND,";") EQ 0 AND
      Str2Date(REPLACE(iPokDD,".","/")) EQ ?)  OR
     ((iPokND EQ "0"          OR
       INDEX(iPokND,";") > 0) AND
      iPokDD NE "0"))
   AND NOT (iPokOP = "��" AND
         iPokND = "0" AND Str2Date(REPLACE(iPokDD,".","/")) <> ?)
   THEN DO:
       RUN Fill-SysMes IN h_tmess ("","","-1",
            "�� �ன��� ��४���� ����஫� ����� (108) ����� ���㬥�� � (109) ��� ���㬥��").
       RETURN.
  END.
*/

  oOk = NO.
{intrface.del}
/* $LINTENV ='dpl' */
/* $LINTVSS ='$/ws3-dpl/bq' */
/* $LINTDATE='31/03/2016 18:49:01.709+04:00' */
/* $LINTUSER='krok' */
/* $LINTMODE='1' */
/* $LINTFILE='chksgnnaldoc.p' */
/*prosignWjIcctXO+yNyMgIwZdiGzg*/