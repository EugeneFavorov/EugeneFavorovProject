/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2016 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: nbkiprint.p
      Comment: 
   Parameters: 
         Uses:
      Used by: 
      Created: 
     Modified: 
*/

{globals.i}
{norm.i NEW}
{tmprecid.def}
{prn-doc.def &with_proc=YES}

DEFINE INPUT PARAMETER iOp     AS INT64     NO-UNDO. 
DEFINE VARIABLE        i       AS INT64     NO-UNDO. /* ���稪 横�� */
DEFINE VARIABLE        mStrPar AS CHARACTER NO-UNDO. 
DEFINE VARIABLE        tStr    AS CHARACTER NO-UNDO. 
DEFINE VARIABLE        tPrint  AS CHARACTER NO-UNDO. /* ��।���� 蠡��� ���� */ 

FOR EACH op WHERE 
         op.op   EQ iOp
     AND op-kind EQ "nbki-qry"
NO-LOCK:
  IF AVAIL op THEN
  tStr = GetXattrValueEx("op", 
                         STRING(op.op), 
                         "����ፁ��",
                         "").
  DO i = 6 TO NUM-ENTRIES(tStr, CHR(1)):
    mStrPar = ENTRY(i, tStr, CHR(1)).
    RUN Insert_TTName(i, mStrPar).
    mStrPar = "".
  END.
END.

{norm-end.i &nofil=YES &nopreview=YES}

CASE ENTRY(1, tStr, CHR(1)):
  WHEN "1" THEN
    tPrint = "zapr_ki".   /* ����� ��ꥪ� �।�⭮� ���ਨ �� ����祭�� �।�⭮�� ���� �� ᢮�� �।�⭮� ���ਨ */
  WHEN "2" THEN
    tPrint = "zapr_ckki".  /* ����� ��ꥪ� �।�⭮� ���ਨ �� ����祭�� ᢥ����� �� ����ࠫ쭮�� ��⠫��� �।���� ���਩ */
  WHEN "3" THEN
    tPrint = "zapr_auto".
  WHEN "4" THEN
    tPrint = "zapr_kid". /* ����� ��ꥪ� �।�⭮� ���ਨ �� �ନ஢���� �������⥫쭮�� ����/������/���㫨஢���� ���� ��ꥪ� �।�⭮� ���ਨ */
END CASE.  

/* �뢮� ������ � 䠩� ���� */
RUN printvd.p (tPrint, INPUT TABLE ttnames).
{empty ttnames}
{intrface.del} /* ���㧪� �����㬥���� */
