
/* +++ cntac100.i was humbly modified by (c)blodd converter v.1.09 on 11/10/2016 7:23am +++ */

{intrface.get acct}     /* ������⥪� ��� ࠡ��� � ��⠬�. */
                        /* ���� ��� ��ண� ���浪�. */
FIND FIRST bal-acct WHERE
   bal-acct.bal-acct EQ iBalAcct
NO-LOCK.

&IF DEFINED(nodef) = 0 &THEN
   {acct-sh.def &SHARE_MODE="NEW SHARED" &nodefbranch=YES}
&ELSE
   {acct-sh.def &SHARE_MODE="SHARED"}
&ENDIF

&IF DEFINED (nodef) = 0
&THEN
   DEF VAR acctmask     AS CHAR  NO-UNDO  FORMAT "x(25)".
   DEF VAR vTokIdx      LIKE tokidx NO-UNDO.
   DEF VAR vacct-cat    AS CHAR  NO-UNDO.
   DEF VAR vKodDoxRash  AS CHAR  NO-UNDO.
   DEF VAR sTokidx      AS CHAR  NO-UNDO.
   DEF VAR sToklen      AS CHAR  NO-UNDO.
   DEF VAR i            AS INT64   NO-UNDO.

   vacct-cat = bal-acct.acct-cat.

   RUN "FindAcctMask" IN h_acct (
      vclass, 
      inbal-acct,
      INPUT-OUTPUT acctmask,
      INPUT-OUTPUT vKodDoxRash 
   ) NO-ERROR.
   IF    ERROR-STATUS:ERROR
      OR {&RETURN_VALUE} EQ "error"
      THEN RETURN "exit".

   RUN GetAcctMask IN h_acct (acctmask,
                              OUTPUT tokacct,    
                              OUTPUT sTokidx,    
                              OUTPUT sToklen
   ) NO-ERROR.      
   IF ERROR-STATUS:ERROR
   OR {&RETURN_VALUE} EQ "error"
      THEN RETURN "exit".

   DO i = 1 TO EXTENT(vTokIdx):
      vTokIdx[i] = INT64(ENTRY(i,sTokidx)).
      IF i LE EXTENT(toklen)
         THEN toklen[i] = INT64(ENTRY(i,sToklen)).
   END.
   tmp-branch-id = TRIM(GetUserBranchId(USERID("bisquit"))).
&ENDIF
                        /* ��ନࢮ���� ����� ���. */
RUN counacct (iBalAcct, iCurrency, tokacct, IF toklen[{&TOK_BRANCH_IDX}] NE 0 THEN tmp-branch-id ELSE dept.branch, num).

/* �஢�ઠ ��� � १�ࢥ,
** �᫨ � १�ࢥ ��� ���, ��࠭���� ����� � १�ࢥ.
** �����頥� YES - � ��砥 ��࠭���� ��� � १�ࢥ. */
FUNCTION AcctChckRsrv RETURN LOG PRIVATE (
   INPUT iSurrogate AS CHAR /* ��� १�ࢠ. */
):
   DEF VAR vOk AS LOG. /* �ਧ��� ��࠭���� ��� � १�ࢥ NO-UNDO ���. */

   DEF BUFFER bis-temp-table FOR bis-temp-table. /* ���������� ����. */
   FIND FIRST bis-temp-table WHERE
      bis-temp-table.surrogate-id EQ iSurrogate
   NO-LOCK NO-ERROR.
   IF NOT AVAIL bis-temp-table
   THEN
   BLCK_RSRV:
   DO
   ON ERROR  UNDO BLCK_RSRV, LEAVE BLCK_RSRV
   ON ENDKEY UNDO BLCK_RSRV, LEAVE BLCK_RSRV:
      /* �஢�ઠ �� �����஢��.
      ** � ������ �����⬥ ������� �訡�� ᨭ�஭���� ����㯠.
      ** �᫨ ��१�ࢨ஢����� ��⮢ ��� (bis-tt ����), � �ந������
      ** १�ࢨ஢���� ����� �࠭���樨 �� ᮧ����� ���. �� �� ��� ����
      ** �࠭����� �� �㤥� �����襭� (��� �� �㥤� ᮧ���), ᮧ�����
      ** १�ࢮ� ��ᯮ�짮������ �㤥� ����� ��-�� �����஢�� �����.
      ** �.�. �� ᮧ����� ��� �� ��ன ��ᨨ (��ࠫ���쭮), �㤥� �ந��������
      ** ����୮� १�ࢨ஢���� ����஢ ���.
      ** ������, ������ �����஢�� �� �ந��������, �� �ਢ����� � �訡�� ᮢ���⭮��
      ** ����㯠 � ������ � ⠡��� bis-tt. */
      FIND FIRST bis-temp-table WHERE
         bis-temp-table.surrogate-id EQ ( IF shModeMulty THEN shFilial + "@" ELSE "") + iSurrogate
      EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
      IF       NOT AVAIL bis-temp-table
         AND   NOT LOCKED (bis-temp-table)
      THEN DO:
         CREATE bis-temp-table.
         ASSIGN
            bis-temp-table.surrogate-id   = ( IF shModeMulty THEN shFilial + "@" ELSE "") + iSurrogate
            vOk                           = YES
         .
      END.
   END.
   RETURN vOk.
END FUNCTION.

/* ��楤�� �ନ஢���� ����� ��� � ������������ ���������� "��ப". */
PROCEDURE counacct PRIVATE.
   DEF INPUT  PARAM inbal-acct   AS INT64    NO-UNDO. /* ��� ��ண� ���浪�. */
   DEF INPUT  PARAM incur        AS CHAR   NO-UNDO. /* ����� ���. */
   DEF INPUT  PARAM tokacct      AS CHAR   NO-UNDO. /* ��᪠ ���. */
   DEF INPUT  PARAM iBranch      AS CHAR   NO-UNDO. /* ��� ���ࠧ�������. */
   DEF INPUT  PARAM num          AS INT64    NO-UNDO. /* ������⢮ १�ࢨ�㥬�� ��⮢. */

   DEF VAR ac        AS CHAR   NO-UNDO.
   DEF VAR j         AS INT64    NO-UNDO.
   DEF VAR i         AS INT64    NO-UNDO.
   DEF VAR ii        AS DEC    NO-UNDO. /* ���稪 ��������� ��⮢. */
   DEF VAR jj        AS DEC    NO-UNDO. /* ������⢮ ��१�ࢨ஢����� ��⮢. */
   DEF VAR mm        AS DEC    NO-UNDO.
   DEF VAR kk        AS INT64    NO-UNDO. /* ���. ���-�� ��⮢, ��� १�ࢠ.*/
   DEF VAR curnum    AS DEC    NO-UNDO. /* ���祭�� ���稪� �� ���������� �����. */
   DEF VAR st        AS CHAR   NO-UNDO. /* ���⨯ ��� ��� १�ࢠ. */
                        /* ����樨 ���稪� � ��᪥ ���.
                        ** ����來���� ���稪� ��࠭�祭� 11 �����ﬨ. */
   DEF VAR cnt-srt   AS INT64    NO-UNDO EXTENT 11.

   DEF BUFFER acct FOR acct. /* ���������� ����. */

   ASSIGN
                        /* ������塞 ��ﬨ ���, �� ���-�� ����権 � ��᪥. */
      ac       = FILL ("0", LENGTH (tokacct))
      j        = 0
      cnt-srt  = 100
   .
                        /* ��ନ�㥬 ࠧ�來���� ���稪�, ᮣ��᭮ ��᪥. */
   DO i = 1 TO LENGTH (ac):
      IF SUBSTR (tokacct, i, 1) EQ "�"
      THEN ASSIGN
         j           = j + 1
         cnt-srt [j] = i
      .
   END.
                        /* ��ॡ�ࠥ� �� ���
                        ** �� ���� ��ண� ���浪� � �����,
                        ** � ���஢��� �� ������ ���稪�. */
   SRCH_BLCK:
   FOR EACH acct where
            acct.bal-acct  EQ inbal-acct
      AND   acct.currency  EQ incur
      AND   acct.acct      NE ?
   NO-LOCK
   BY SUBSTR (acct.acct, cnt-srt [1],  1)
   BY SUBSTR (acct.acct, cnt-srt [2],  1)
   BY SUBSTR (acct.acct, cnt-srt [3],  1)
   BY SUBSTR (acct.acct, cnt-srt [4],  1)
   BY SUBSTR (acct.acct, cnt-srt [5],  1)
   BY SUBSTR (acct.acct, cnt-srt [6],  1)
   BY SUBSTR (acct.acct, cnt-srt [7],  1)
   BY SUBSTR (acct.acct, cnt-srt [8],  1)
   BY SUBSTR (acct.acct, cnt-srt [9],  1)
   BY SUBSTR (acct.acct, cnt-srt [10], 1)
   BY SUBSTR (acct.acct, cnt-srt [11], 1):

      ASSIGN
                        /* ������ ������⢠ ��������� ��⮢. */
         ii       = ii + 1
                        /* ��ନ஢���� ⥪�饣� ���祭�� ���稪� � ���. */
         curnum   = DEC (
                     SUBSTR (acct.acct, cnt-srt[1],  1) +
                     SUBSTR (acct.acct, cnt-srt[2],  1) +
                     SUBSTR (acct.acct, cnt-srt[3],  1) + 
                     SUBSTR (acct.acct, cnt-srt[4],  1) +
                     SUBSTR (acct.acct, cnt-srt[5],  1) + 
                     SUBSTR (acct.acct, cnt-srt[6],  1) +
                     SUBSTR (acct.acct, cnt-srt[7],  1) + 
                     SUBSTR (acct.acct, cnt-srt[8],  1) +
                     SUBSTR (acct.acct, cnt-srt[9],  1) + 
                     SUBSTR (acct.acct, cnt-srt[10], 1) +
                     SUBSTR (acct.acct, cnt-srt[11], 1)
                   )
         mm       = IF curnum - ii GT num
                     THEN num
                     ELSE curnum - ii + 1
      NO-ERROR.
                        /* �᫨ ⥪�饥 ���祭�� ���稪� �����
                        ** ⥪�饣� ������⢠ ��������� ��⮢, �... */
      IF curnum GT ii
      THEN DO kk = 1 TO IF mm EQ 1 THEN 1 ELSE mm - 1 :
                        /* ��ନ஢���� ����� ���. */
         st =  STRING (inbal-acct)  + "," + 
               incur                + "," + 
               iBranch              + "," + 
               STRING (DECIMAL(ii), FILL("9", toklen[{&TOK_CNT_IDX}]))
         NO-ERROR.
         IF AcctChckRsrv (st)
            THEN jj = jj + 1.
                        /* �᫨ ������⢮ ��१�ࢨ஢����� ��⮢ �ॢ�ᨫ�
                        ** ��⠭��������, � �४�頥� �ନ஢��� १��. */
         IF jj EQ num
            THEN LEAVE SRCH_BLCK.
         IF kk NE mm
            THEN ii = ii + 1.
      END.
      ELSE IF curnum LT ii
         THEN ii = ii - 1.
   END.
                        /* �����稢��� ���稪 �ॡ㥬�� ��⮢. */
   mm = jj + 1.
                        /* �᫨ ������⢮ ��१�ࢨ஢����� ��⮢
                        ** �������筮, �... */
   IF jj LT num THEN 
   GEN_NUMBER:
   DO kk = mm TO num:
      ii = ii + 1.
                        /* �஢��塞 �� ࠧ��୮��� ���稪� ᮮ⢥����
                        ** ����祭���� ������. */
      IF LENGTH(STRING(ii)) GT toklen[{&TOK_CNT_IDX}]
         THEN LEAVE GEN_NUMBER.
                        /* ��䨪�஢��� �஡����, ���⠢��� �६���� ��室 �
                        ** �ᯮ�짮������ NO-ERROR,
                        ** �� ����� ���稪� ࠢ�� 6 � toklen [6] ����� 2. */
      st =  STRING (inbal-acct)  + "," +
            incur                + "," + 
            iBranch              + "," +
            STRING (DECIMAL(ii), FILL("9", toklen[{&TOK_CNT_IDX}]))
      NO-ERROR.
                        /* �᫨ १�ࢨ஢���� �� 㤠����,
                        ** � 㢥��稢��� ���孨� �।��. */
      IF NOT AcctChckRsrv (st)
         THEN num = num + 1.
   END.
   RETURN.
END PROCEDURE.


/*prosignPCzG1CM5jEHUjmiMGGkNrw*/
/* --- cntac100.i was humbly modified by (c)blodd converter v.1.09 on 11/10/2016 7:23am --- */
