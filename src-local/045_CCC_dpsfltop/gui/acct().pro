/* ��筮� �ନࢮ���� WHERE ��ࠦ����.*/
PROCEDURE AcctManual.

   DEF PARAM BUFFER flt-attr FOR flt-attr.   /* ���ᠭ�� ����. */
   DEF OUTPUT PARAM MyWhere AS CHAR NO-UNDO. /* WHERE ��ࠦ���� ��� ����. */

   CASE flt-attr.attr-basic:
      WHEN "cust-id"
      THEN DO:
         IF GetFltVal ("cust-id") NE "���᮪" THEN
         DO:
            MyWhere = StdWhere(recid(flt-attr),YES).
  
            IF LOOKUP ("EQ", MyWhere, " ") EQ 0
            THEN MyWhere = DYNAMIC-FUNCTION ("GetDynWhr" IN h_dynqr,
                              "",
                              "STRING (acct." + flt-attr.attr-basic + ")",
                              flt-attr.attr-code-value + CHR (1) + CHR (1),
                              flt-attr.attr-datatype).
         END.
      END.
   END CASE.

   RETURN.
END PROCEDURE.

/* ��⠭���� ����� �३�� � ��������� */
PROCEDURE Select-Browse:
   
   ASSIGN
      n-frm =  INT64 (GetFltVal ('view-type')) + 1
      vDate =  IF n-frm EQ 5 OR n-frm EQ 6 OR n-frm EQ 8
                  THEN mSldDate
                  ELSE closedate
      h-frm[n-frm]:TITLE = IF GetFltVal("extra-title") EQ ""
                           THEN "[ ������� �����, ������� �� " + STRING(vDate)
                                 + " (~"" + ENTRY(3,user-config-info) + "~") ]"
                           ELSE "[ " + GetFltVal("extra-title") + " ]"
   .
   RETURN.
END PROCEDURE.

/* ��楤�� �⮡ࠦ���� ���⪠ �� ����. */
PROCEDURE b-acct_fnd:

   DEF VAR vAPRight AS LOG    NO-UNDO. /* �ࠢ� ��ᬠ�� ���⪠. */
   DEF VAR mColor   AS CHAR   NO-UNDO.
                        /* ����祭�� 䫠�� �� �ࠢ�� ����㯠. */
   vAPRight = {acctlook.i}. 
                        
            /* ��ନ஢���� 梥� ��� ����⭮�� ���⪠. */
   IF     AVAIL acct-cur
      AND (    acct.close-date GE vDate
           OR  acct.close-date EQ ?)
   THEN  /* ��ନ஢���� 梥� ��� �㡫����� ���⪠ � ���.  */
      COLOR DISPLAY
         VALUE (GetBalColorBuffer ((BUFFER acct:HANDLE), acct-cur.balance))
         acct-cur.balance
      WITH FRAME browse2.

   IF     AVAIL acct-pos
      AND (    acct.close-date GE vDate
           OR  acct.close-date EQ ?)
   THEN 
      COLOR DISPLAY
         VALUE (GetBalColorBuffer ((BUFFER acct:HANDLE), acct-pos.balance))
         acct-pos.balance
      WITH FRAME browse2.

   COLOR DISPLAY
      VALUE (GetAcctColorBuffer(BUFFER acct:HANDLE,vDate))
      mAcct
   WITH FRAME browse2.
                        /* �⮡ࠦ���� ������. */
   DISPLAY
      mAcct
      acct-cur.balance
         FORMAT ">>>>>>,>>>,>>9.99 � "
      WHEN AVAIL acct-cur
         AND acct-cur.balance GT 0
         AND vAPRight
      @ acct-cur.balance

      - acct-cur.balance
         FORMAT ">>>>>>,>>>,>>9.99  �"
         WHEN  AVAIL acct-cur
            AND acct-cur.balance LT 0
            AND vAPRight
      @ acct-cur.balance

      ""
         WHEN acct.close-date EQ ? @ acct-pos.balance

      acct-pos.balance
         FORMAT ">>>>>>,>>>,>>>,>>9.99 � "
         WHEN  AVAILABLE acct-pos 
            AND acct-pos.balance GT 0 
            AND vAPRight
         @ acct-pos.balance

      - acct-pos.balance
         FORMAT ">>>>>>,>>>,>>>,>>9.99  �"
         WHEN  AVAILABLE acct-pos
            AND acct-pos.balance LT 0
            AND vAPRight
         @ acct-pos.balance

      "������"
         WHEN     acct.close-date NE ?
              AND NOT mColCloAc
         @ acct-pos.balance

      "       ��� �������"
         WHEN NOT vAPRight
      @ acct-pos.balance
   WITH FRAME browse2.
END.

/* �⮡ࠦ���� ���⪠ �� ����. */
PROCEDURE APDisp PRIVATE.
   DEF VAR vAPRight AS LOG    NO-UNDO. /* �ࠢ� ��ᬠ�� ���⪠. */
                        /* ����祭�� 䫠�� �� �ࠢ�� ����㯠. */
   vAPRight = {acctlook.i}.
                        /* ��ନ஢���� 梥� ��� ����⭮�� ���⪠. */
   IF mPosVal NE 0
   THEN COLOR DISPLAY
      VALUE (GetBalColorBuffer ((BUFFER acct:HANDLE), mPosVal))
      mPosVal
   WITH FRAME browse5.
                        /* ��ନ஢���� 梥� ��� �㡫����� ���⪠. */
   IF mPosBal NE 0
   THEN COLOR DISPLAY
      VALUE (GetBalColorBuffer ((BUFFER acct:HANDLE), mPosBal))
      mPosBal
   WITH FRAME browse5.
   COLOR DISPLAY
      VALUE (GetAcctColorBuffer(BUFFER acct:HANDLE,vDate))
      mAcct
   WITH FRAME browse5.
                        /* �⮡ࠦ���� ������. */
   DISPLAY
      mAcct

      mPosVal
         FORMAT ">>>>>>,>>>,>>9.99 � "
      WHEN mPosVal GT 0
         AND vAPRight
      @ mPosVal

      - mPosVal
         FORMAT ">>>>>>,>>>,>>9.99  �"
         WHEN  mPosVal LT 0
            AND vAPRight
      @ mPosVal

      ""
         WHEN acct.close-date EQ ? @ mPosBal

      mPosBal
         FORMAT ">>>>>>,>>>,>>>,>>9.99 � "
         WHEN  mPosBal GT 0
            AND vAPRight
         @ mPosBal

      - mPosBal
         FORMAT ">>>>>>,>>>,>>>,>>9.99  �"
         WHEN  mPosBal LT 0
            AND vAPRight
         @ mPosBal

      "������"
         WHEN      acct.close-date NE ?
               AND NOT mColCloAc
         @ mPosBal

      "       ��� �������"
         WHEN NOT vAPRight
      @ mPosBal
   WITH FRAME browse5.
   RETURN.
END PROCEDURE.

/* �⮡ࠦ���� ���⪠ �� ����. */
PROCEDURE APDisp8 PRIVATE.
   DEF VAR vAPRight AS LOG    NO-UNDO. /* �ࠢ� ��ᬠ�� ���⪠. */
                        /* ����祭�� 䫠�� �� �ࠢ�� ����㯠. */
   vAPRight = {acctlook.i}. 
                        /* ��ନ஢���� 梥� ��� ����⭮�� ���⪠. */
   IF mAvVal NE 0 THEN
    COLOR DISPLAY
      VALUE (GetBalColorBuffer ((BUFFER acct:HANDLE), mAvVal))
      mAvVal
   WITH FRAME browse8.

                        /* ��ନ஢���� 梥� ��� �㡫����� ���⪠. */
   IF mAvBal NE 0
   THEN 
   COLOR DISPLAY
      VALUE (GetBalColorBuffer ((BUFFER acct:HANDLE), mAvBal))
      mAvBal
   WITH FRAME browse8.

   COLOR DISPLAY
      VALUE (GetAcctColorBuffer(BUFFER acct:HANDLE,vDate))
      mAcct
   WITH FRAME browse8.

                        /* �⮡ࠦ���� ������. */
   DISPLAY
      mAcct

      mAvVal
         FORMAT ">>>>>>,>>>,>>9.99 � "
      WHEN mAvVal GT 0
         AND vAPRight
      @ mAvVal

      - mAvVal
         FORMAT ">>>>>>,>>>,>>9.99  �"
         WHEN  mAvVal LT 0
            AND vAPRight
      @ mAvVal

      ""
         WHEN acct.close-date EQ ? @ mAvBal

      mAvBal
         FORMAT ">>>>>>,>>>,>>>,>>9.99 � "
         WHEN  mAvBal GT 0
            AND vAPRight
         @ mAvBal

      - mAvBal
         FORMAT ">>>>>>,>>>,>>>,>>9.99  �"
         WHEN  mAvBal LT 0
            AND vAPRight
         @ mAvBal

      "������"
         WHEN      acct.close-date NE ?
               AND NOT mColCloAc
         @ mAvBal

      "       ��� �������"
         WHEN NOT vAPRight
      @ mAvBal
   WITH FRAME browse8.
   RETURN.
END PROCEDURE.

/* �⮡ࠦ���� ���⪠ �� ����. */
PROCEDURE APDisp6 PRIVATE.
   DEF VAR vAPRight AS LOG    NO-UNDO. /* �ࠢ� ��ᬠ�� ���⪠. */
                        /* ����祭�� 䫠�� �� �ࠢ�� ����㯠. */
   vAPRight = {acctlook.i}.
                        /* ��ନ஢���� 梥� ��� �㡫����� ���⪠. */
   IF mPosQty NE 0 THEN
      COLOR DISPLAY VALUE (GetBalColorBuffer ((BUFFER acct:HANDLE), mPosQty)) mPosQty
   WITH FRAME browse6.
   COLOR DISPLAY
      VALUE (GetAcctColorBuffer(BUFFER acct:HANDLE,vDate))
      mAcct
   WITH FRAME browse6.
                        /* �⮡ࠦ���� ������. */
   DISPLAY
      mAcct

      mPosQty
         WHEN     mPosQty GT 0
            AND   vAPRight
         @ mPosQty
      - mPosQty
         WHEN     mPosQty LT 0
            AND   vAPRight
         @ mPosQty
      mPosBal
         FORMAT ">>>>>>,>>>,>>>,>>9.99 �"
         WHEN     mPosBal GT 0
            AND   vAPRight
         @ mPosBal
      - mPosBal
         FORMAT ">>>>>>,>>>,>>>,>>9.99 �" 
         WHEN     mPosBal LT 0
            AND   vAPRight
         @ mPosBal
      "������"
         WHEN     acct.close-date NE ?
              AND NOT mColCloAc
         @ mPosBal
      "       ��� �������"
         WHEN NOT vAPRight
      @ mPosBal
   WITH FRAME browse6.
   RETURN.
END PROCEDURE.


/* ��楤�� �������⥫쭮� �஢�ન �� ��������� ���祭�� �᭮����/�������⥫��� ४����⮢
   � �ᯮ�짮������ ���-�� Tmprecid */
PROCEDURE pAddProcCheck:
   DEFINE INPUT PARAMETER iClass   AS CHARACTER NO-UNDO. /* ����� ��ꥪ�. */         
   DEFINE INPUT PARAMETER iCode    AS CHARACTER NO-UNDO. /* ��� ४�����. */         
   DEFINE INPUT PARAMETER iSurrObj AS CHARACTER NO-UNDO. /* �����䨪��� ��ꥪ�. */ 
   DEFINE INPUT PARAMETER iValue   AS CHARACTER NO-UNDO. /* ���祭�� ४�����. */    
   DEFINE INPUT PARAMETER iBaseVal AS CHARACTER NO-UNDO. /* ���祭�� ४�����,� ���஬� �����⢫���� �ਢ離�. */    
    
   DEF BUFFER acct FOR acct. /* ���������� ����. */
   DEFINE VARIABLE vAcct     AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vCurrency AS CHARACTER  NO-UNDO.

   ASSIGN 
      vAcct     = ENTRY(1,iSurrObj)
      vCurrency = ENTRY(2,iSurrObj)
   NO-ERROR.

   IF ERROR-STATUS:ERROR THEN
      RETURN ERROR.

   {find-act.i
      &acct = vAcct
      &curr = vCurrency
   }   

   IF NOT AVAILABLE acct THEN
      RETURN ERROR.
   CASE iCode:
      WHEN "kau-id" THEN DO:
         IF     {assigned acct.kau-id}
            AND acct.kau-id NE iBaseVal THEN
            RETURN "��� ��� �ᯮ������ � ��㣨� 蠡�����!".
      END.
   END CASE.

   RETURN "".

END PROCEDURE.

/* ����⢨� �믮��塞� ��᫥ �ਬ������ 䨫���
** �뭥ᥭ� �� CalcVar (acctbrw.cv), ��� �᪮७��,
** �.�. �� ����⢨� �� �ॡ���� �믮����� ��� ������ ����� */
PROCEDURE AcctAfterFilter:
   RUN PostSelectQuery.
   /* �஢�ઠ �� ���४⭮��� ������� �᫮��� 䨫���樨.
   ** �᫨ ������ ��� �� ���� �� �࠭�� ���ࢠ��� �㬬� ����⮢,
   ** �� �� ����� ���ࢠ� ���, � �� ⠪�� �᫮���� 䨫����� ����������.
   ** �뤠�� ᮮ�饭�� �� �⮬ � �����뢠�� � vFldReturn ��� ���� 䨫���
   ** � ������ 䨫��� ����᪠�� ������ ��� 䨫��� � ��⠥� �� 㪠������ ���� */
   IF     NOT vFiltTurn
      AND (   IsFieldChange("sh-v1")
           OR IsFieldChange("sh-v2")
           OR IsFieldChange("sh-b1")
           OR IsFieldChange("sh-b2"))
   THEN DO:
      RUN Fill-SysMes IN h_tmess ("", "", "0", "��� 䨫���樨 �� ����⠬ ������ ��� ���� ���ࢠ�� ��� ����⮢.").
      vFldReturn = "TurnDate1".
   END.
   RETURN.
END PROCEDURE.

PROCEDURE PostSelectQuery:
   /* ���㫥��� 䫠��� 䨫���樨. */
   ASSIGN
      vFiltTurn   = NO
      vFiltSld    = NO
   .
   ASSIGN
      /* �����⢫��� �� 䨫����� �� ����⠬? (�᫨ ���짮��⥫� �����
      ** ���� ��砫� � ���� ���ࢠ�� ���� ����⮢ � 䨫���
      ** � ��� �� ���� �� �࠭�� ���ࢠ��� ���祭�� ����⮢ */
      vFiltTurn =       IsFieldChange("TurnDate1")
                  AND   IsFieldChange("TurnDate2")
                  AND (   IsFieldChange("sh-v1")
                       OR IsFieldChange("sh-v2")
                       OR IsFieldChange("sh-b1")
                       OR IsFieldChange("sh-b2"))
      /* �����⢫��� �� 䨫����� �� ���⪠�? (�᫨ ���짮��⥫�� ������
      ** ��� �� ���� ���� 䨫���, �⭮��饥�� � ࠧ���� 䨫���樨 �� ���⪠� */
      vFiltSld =     IsFieldChange("SldDate")      /* "���⪨ ��:" */
                  OR IsFieldChange("SldType")      /* "��� ᠫ줮:" */
                  OR IsFieldChange("mPosVal1")     /* "���줮 � ��.���. ��:" */
                  OR IsFieldChange("mPosVal2")     /* "���줮 � ��.���. ��:" */
                  OR IsFieldChange("mPosBal1")     /* "���줮 � ���.���. ��:" */
                  OR IsFieldChange("mPosBal2")     /* "���줮 � ���.���. ��:" */
      /* ��� ��砫� ���ࢠ�� ���� ����⮢ �� ��⠬ (�� 䨫���) */
      mTurnBeg = DATE(GetFltVal("TurnDate1"))
      /* ��� ����砭�� ���ࢠ�� ���� ����⮢ �� ��⠬ (�� 䨫���) */
      mTurnEnd = DATE(GetFltVal("TurnDate2"))
      /* ⨯ ����⮢ */
      mTurnType = GetFltVal("TurnType")
      mSldDate = DATE(GetFltVal("SldDate"))
      mGrpFltLst = GetFltValEx("GroupList","*")     /* ᯨ᮪ ��㯯 ��⮢ */
      mFltGrpType = GetFltValEx("GroupFltType","any")  /* ᯮᮡ 䨫���樨 �� ��㯯�� */
   NO-ERROR.
                        /* ����祭� �� ��࠭�祭�� �� �⥭�� ��㯯 ��⮢ */
   mAcctGrOn = UserAcctGroupLimitsEnabled ('r', USERID("bisquit")).
                        /* ������塞 �६.⠡���� ��� 䨫���樨 �� ��㯯�� ��⮢ (⮫쪮 ��� �������) */
   IF     mBrwRole EQ "Group"
      /*AND IsUserAdm(USERID("bisquit"))*/ THEN
   RUN FillttGroupAcct.
END PROCEDURE.

PROCEDURE APDisp9 PRIVATE.
  DEF VAR vAPRight AS LOG    NO-UNDO. /* �ࠢ� ��ᬠ�� ���⪠. */
                        /* ����祭�� 䫠�� �� �ࠢ�� ����㯠. */
   vAPRight = {acctlook.i}.
                        /* ��ନ஢���� 梥� ��� ����⭮�� ���⪠. */

  COLOR DISPLAY
      VALUE (GetAcctColorBuffer(BUFFER acct:HANDLE,vDate))
      mAcct
   WITH FRAME browse9.    
                        /* �⮡ࠦ���� ������. */
   DISPLAY
      mAcct
      currency
      mDbTurnVal WHEN mTurnBeg NE ? AND mTurnEnd NE ? AND vAPRight  @ mDbTurnVal
      mCrTurnVal WHEN mTurnBeg NE ? AND mTurnEnd NE ? AND vAPRight  @ mCrTurnVal
      mDbTurn    WHEN mTurnBeg NE ? AND mTurnEnd NE ? AND vAPRight  @ mDbTurn
      mCrTurn    WHEN mTurnBeg NE ? AND mTurnEnd NE ? AND vAPRight  @ mCrTurn
      ""         WHEN mTurnBeg EQ ? OR  mTurnEnd EQ ? @ mDbTurnVal
      ""         WHEN mTurnBeg EQ ? OR  mTurnEnd EQ ? @ mCrTurnVal
      ""         WHEN mTurnBeg EQ ? OR  mTurnEnd EQ ? @ mDbTurn
      ""         WHEN mTurnBeg EQ ? OR  mTurnEnd EQ ? @ mCrTurn

   WITH FRAME browse9.
   RETURN.
END PROCEDURE.

/* ���������� �६����� ⠡���� ROWID'�� ��⮢ �� 䨫���樨 �� ��㯯�� ����㯠 */
PROCEDURE FillttGroupAcct PRIVATE.
   DEF VAR vGroupList    AS CHAR   NO-UNDO.
   DEF VAR vi            AS INT64  NO-UNDO.
   DEF VAR vOK           AS LOG    NO-UNDO.
   DEF VAR vLinkID       AS INT64  NO-UNDO.

   DEF BUFFER flt-attr FOR flt-attr.
   DEF BUFFER acct     FOR acct.
   DEF BUFFER links    FOR links.

   EMPTY TEMP-TABLE LocalTmpObj.
                        /* �᫨ ������ ��㯯� ��� 䨫���樨 */
   IF     mBrwRole   EQ "Group"
      AND mGrpFltLst NE "*"
   THEN DO:
      vLinkID = GetXLinkID("acct","acct-group").
      FOR EACH links WHERE links.link-id   EQ vLinkID
                       AND links.target-id EQ mGrpFltLst 
      NO-LOCK,
      FIRST acct WHERE acct.acct     EQ ENTRY (1,links.source-id)
                   AND acct.currency EQ ENTRY (2,links.source-id)
      NO-LOCK:
                              /* "�⡨��� ��� �ਭ������騥: (X)���쪮 㪠����� ��㯯��" -
                              ** �� ��㯯� ��� �ਭ������� ᯨ�� */
         IF mFltGrpType EQ "all" THEN
         DO:
            vGroupList = GetLinks ("acct",acct.acct + "," + acct.currency,"S","acct-group",",",?).
            GRLST:
            DO vi = 1 TO NUM-ENTRIES(vGroupList):
               IF NOT CAN-DO(mGrpFltLst, ENTRY (vi,vGroupList)) THEN
               DO:
                  vGroupList = ?.
                  LEAVE GRLST.
               END.
            END.
         END.

         IF    vGroupList    NE ?
            OR mFltGrpType NE "all"
         THEN DO:
            CREATE LocalTmpObj.
            ASSIGN LocalTmpObj.rid = RECID (acct).
         END.
      END.

                        /* ��⠭�������� 䨫����� �� �६����� ⠡��� LocalTmpObj */
      FIND FIRST flt-attr WHERE
         flt-attr.attr-code EQ "UseTmpObjInQuery"
      NO-ERROR.
      IF NOT AVAIL flt-attr THEN
      DO:
         CREATE flt-attr.
         ASSIGN
            flt-attr.attr-code      = "UseTmpObjInQuery"
            flt-attr.attr-hidden    = YES
            flt-attr.attr-sensitive = NO
            flt-attr.attr-initial   = "*"
         .
      END.
      flt-attr.attr-code-value = STRING (mLocalTmpObjHand).
                        /* ����뢠�� ���祭�� ���� "��㯯�" ��� ⮣�, �⮡� 
                        ** �� �����⢫﫠�� 䨫����� �� ����塞��� ���� */
      RUN SetFltField ("GroupList","*").
   END.
   RETURN.
END PROCEDURE.
