/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2008 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: ORGNAME318P.I
      Comment: ���⠥� ������������ �࣠����権 (318�)
   Parameters: CurBranchName - ������������ ���
               Otstup        - ������ ����㯠 �� ���
               OrgNameDef    - ��� ����୮�� �ᯮ�짮�����
               BranchExt     - �� ᪮�쪮 ��ப ����� ������������
               �ਬ�砭��:
               1) i as INT64, ��� ��।����� �� �⠫
               2) �ॡ�� wordwrap.def
         Uses:
      Used by:
      Created: 29.07.2008 17:17 elus
     Modified: 29.07.2008 17:17 elus
*/

&IF DEFINED(BranchExt) EQ 0 &THEN
   &SCOPED-DEFINE BranchExt 3
&ENDIF

&IF DEFINED(OrgNameDef) EQ 0 &THEN   

   &SCOPED-DEFINE OrgNameDef YES
   
   DEFINE VARIABLE mBranchNameL LIKE branch.name NO-UNDO EXTENT {&BranchExt}. /* ������������ ��� */ 
   DEFINE VARIABLE mBranchNameM LIKE branch.name NO-UNDO EXTENT {&BranchExt}. /* ������������ 䨫���� */ 
   DEFINE VARIABLE mBranchNameH LIKE branch.name NO-UNDO EXTENT {&BranchExt}. /* ��ଥ���� ������������ �।�⭮� �࣠����樨 */ 
   DEFINE VARIABLE mOtstup      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE mRasshifNaim AS LOGICAL   NO-UNDO.
   
   RUN GetBankNamesN(mCuBrchID, OUTPUT mBranchNameH[1], OUTPUT mBranchNameM[1], OUTPUT mBranchNameL[1]).
   mRasshifNaim = FGetSetting("����䍠��",?,"��") EQ "��".
   
&ENDIF

&IF DEFINED(Otstup) EQ 0 &THEN
   &SCOPED-DEFINE Otstup 0
&ENDIF

   mOtstup = FILL(" ",{&Otstup}).

&IF DEFINED(cols) NE 0 &THEN
   IF {&cols} > 82 THEN
   mOtstup = FILL(" ",INT64(({&cols} - 82) / 2)).
&ENDIF

/* ��������� ������ ������������ � �뢮��� � ��⮪ */

        {put-name.i &pref = "L" &*}
    if mBranchNameL[1] = '' then do:
        {put-name.i &pref = "M" &*}
    end.
    if mBranchNameL[1] = '' and mBranchNameM[1] = '' then do:
        {put-name.i &pref = "H" &*}
    end.

   PUT UNFORMATTED
     mOtstup FILL('�', 82) SKIP.

   IF mRasshifNaim THEN
   DO:
      PUT UNFORMATTED
        mOtstup " ������ �ଥ���� (᮪�饭��� �ଥ����) ������������ �।�⭮� �࣠����樨 ���  " SKIP
        mOtstup "  ������ (᮪�饭���) ������������ 䨫����, ��� ������������ � (���) ����� ���   " SKIP
        mOtstup "(�� ����稨) ���� ��� ����������騥 �ਧ���� ��� (�� ������⢨� ������������" SKIP
        mOtstup "   � �����) � 㪠������ �� ��� �ਭ���������� �।�⭮� �࣠����樨 (䨫����)    " SKIP(1).
   END.
/* $LINTUSER='STRE' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='14/11/2014 10:33:47.823+04:00' */
/* $LINTFILE='orgname318p.i' */
/*prosign2NSHmHJWGTqJMGWxFi4Bow*/