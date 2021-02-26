/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: 
     Filename: bamk-cn.p
      Comment: �饬 ������ � ���� BANK � ᮧ���� � ����
   Parameters: 
         Uses:
      Used by:
      Created: kam 
     Modified:    
*/

{globals.i}
{intrface.get xclass}   /* ������⥪� �����㬥�⮢ ����奬�. */
{intrface.get rights}
{intrface.get osyst}

DEF INPUT PARAMETER iOper AS CHAR.

DEFINE VARIABLE mStr AS CHARACTER NO-UNDO.
{justasec}

DO ON ERROR UNDO, RETURN ERROR
   ON STOP UNDO, LEAVE :
  IF NOT CONNECTED("bank") THEN DO:
    CONNECT -pf VALUE("/home2/bis/quit41d/conf/bismfr.pf").
   &IF DEFINED( SESSION-REMOTE ) = 0 &THEN
     put screen col 1 row screen-lines + message-lines + 1
   				color bright-blink-normal "������祭�� � ���...".
   &ELSE
    mblodd_char_Tmp03 = ?.
    RUN bloddProgressBar( INPUT-OUTPUT mblodd_char_Tmp03, ?, ?, ?, "������祭�� � ���...", ? ).
   &ENDIF
  END.
  IF iOper EQ ? THEN
    MESSAGE "�� 㪠���� ����᪠���� ��楤��." VIEW-AS ALERT-BOX.
  ELSE
    RUN VALUE(iOper + ".p").

  CATCH eAnyError AS Progress.Lang.Error:
    PUT UNFORMATTED "CATCH: " + RETURN-VALUE + " " + eAnyError:GetMessage(1) SKIP.
  END CATCH.
  FINALLY:
    IF CONNECTED("bank") THEN DISCONNECT bank.
    IF CONNECTED("bismfr") THEN DISCONNECT bismfr.
  END FINALLY.
END. /* DO */
hide message no-pause.


