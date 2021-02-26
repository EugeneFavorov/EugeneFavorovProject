/*
     ������᪠� ��⥣�஢����� ��⥬� �������
      Filename: svodlst.p
      Comment: ��������� ���㬥�⮢ ���짮��⥫�\���஭���� �࠭����\���ࠧ�������
      Parameters:
      Uses:
      Used by:
      Created: ayv
      ����஥�� ��ࠬ����: �����(b), �����(o)
      �����䨪����: elhran(elhran.i/def), ������(hozdoc.i/def), ��⏮�ࠧ�  
*/

{globals.i}

DEF INPUT PARAM iStr AS CHAR NO-UNDO.
DEF VAR iUserInp AS CHAR NO-UNDO. /*���짮��⥫� ��� ��������*/
DEF VAR iHozS    AS CHAR NO-UNDO. /*⨯ �訢� ��� 宧��⢥���� ���㬥�⮢ */
/* ���� �訢��:                                                */
/* ����� - �����᪨� ������ࠦ�����                            */
/* ������ - 宧��⢥��� ���㬥���                            */
/* ������� - �������� �㬬�                                */
/* ����� - ���㬥��� ���                                       */

DEF VAR fname  AS CHAR NO-UNDO.
DEF NEW SHARED STREAM vvs.

DEF VAR iFil    AS CHAR            NO-UNDO. /*��� ���ࠧ�������*/
DEF VAR iUserID AS CHAR  INIT ''   NO-UNDO. /*��� ���짮��⥫�*/
DEF VAR tUserID AS CHAR            NO-UNDO. 
DEF VAR tShiv   AS CHAR            NO-UNDO.
DEF VAR tNull   AS LOG   INIT TRUE NO-UNDO. /*���� �� �஢������ ���짮��⥫�� ���㬥���*/
DEF VAR tNotA   AS LOG   INIT TRUE NO-UNDO. /*�஢���� ������ �࠭���� � �㬠��, �� ᮧ���� SERV'��*/
DEF VAR tStr    AS CHAR            NO-UNDO.
DEF VAR i       AS INT64           NO-UNDO.

DEF VAR tServ   AS CHAR            NO-UNDO.

DEF VAR vUserName AS CHAR NO-UNDO.
DEF VAR vUserOtd  AS CHAR NO-UNDO.

DEF TEMP-TABLE tt-op LIKE op.
DEF BUFFER b-code FOR code.

/* ��� ࠡ��� op-cash*/
DEF VAR adb     AS CHAR    NO-UNDO. 
DEF VAR acr     AS CHAR    NO-UNDO.
DEF VAR isCash  AS LOGICAL NO-UNDO.

/*��� svodxls.i*/
DEF VAR vSum-b     LIKE op-entry.amt-rub INIT 0.00 NO-UNDO. /*���஭��� �࠭���� - �����ᮢ�*/
DEF VAR vSum-o     LIKE op-entry.amt-rub INIT 0.00 NO-UNDO. /*���஭��� �࠭���� - ��������ᮢ�*/
DEF VAR vSum-b-val LIKE op-entry.amt-rub INIT 0.00 NO-UNDO. /*���஭��� �࠭���� - �����ᮢ�    - �����*/
DEF VAR vSum-o-val LIKE op-entry.amt-rub INIT 0.00 NO-UNDO. /*���஭��� �࠭���� - ��������ᮢ� - �����*/

/*��� svodallxls.i*/
DEF VAR vSum-b-p     LIKE op-entry.amt-rub INIT 0.00 NO-UNDO. /*���㬥��� �� �㬠��  - �����ᮢ�*/
DEF VAR vSum-o-p     LIKE op-entry.amt-rub INIT 0.00 NO-UNDO. /*���㬥��� �� �㬠��  - ��������ᮢ�*/
DEF VAR vSum-b-p-v   LIKE op-entry.amt-rub INIT 0.00 NO-UNDO. /*���㬥��� �� �㬠�� � ����� - �����ᮢ�*/
DEF VAR vSum-o-p-v   LIKE op-entry.amt-rub INIT 0.00 NO-UNDO. /*���㬥��� �� �㬠�� � ����� - ��������ᮢ�*/
DEF VAR vSum-b-e     LIKE op-entry.amt-rub INIT 0.00 NO-UNDO. /*���஭��� �࠭���� - �����ᮢ�*/
DEF VAR vSum-o-e     LIKE op-entry.amt-rub INIT 0.00 NO-UNDO. /*���஭��� �࠭���� - ��������ᮢ�*/
DEF VAR vSum-b-e-v   LIKE op-entry.amt-rub INIT 0.00 NO-UNDO. /*���஭��� �࠭���� � ����� - �����ᮢ�*/
DEF VAR vSum-o-e-v   LIKE op-entry.amt-rub INIT 0.00 NO-UNDO. /*���஭��� �࠭���� � ����� - ��������ᮢ�*/

DEF TEMP-TABLE tt-op-day NO-UNDO     /*�஢���� �� ����樮��� ����*/
  FIELD op-cid    AS   RECID              /*op*/
  FIELD ope-cid   AS   RECID              /*op-entry*/
  FIELD city-id   AS   INTEGER            /*�� ��� ���*/
  FIELD city      AS   CHARACTER          /*��� �⤥�����*/
  FIELD kko       AS   CHARACTER          /*��� �⤥�����*/
  FIELD currency  LIKE op-entry.currency  /*�����*/
  FIELD acct-cat  LIKE op-entry.acct-cat  /*������\���������*/
  FIELD razdel    AS   CHARACTER          /*���ᮢ� �� ���㬥��*/
  FIELD save-type AS   CHARACTER          /*��� �࠭���� (1 - �㬠��, 2 - ���஭��)*/
  FIELD hozdoc    AS   CHARACTER          /*�ਭ���������� � 宧.���㬥�⠬ (��।������ ⨯ ��� �� ���㬥���)*/
  FIELD acct-dbf  AS   CHARACTER          /*��� ��ண� ���浪� �����(��� ���஢��)*/
  FIELD acct-dbl  AS   CHARACTER          /*��᫥���� 5 ��� �����(��� ���஢��)*/
  FIELD acct-db   LIKE op-entry.acct-db   /*�����*/
  FIELD acct-cr   LIKE op-entry.acct-cr   /*�।��*/
  FIELD doc-num   LIKE op.doc-num         /*����� ���㬥��*/
  FIELD doc-numt  LIKE op.doc-num         /*����� ���㬥�� ��� ���஢��*/
  FIELD doc-type  LIKE op.doc-type        /*⨯ ���㬥��*/
  FIELD amt-rub   LIKE op-entry.amt-rub   /*�㬬� � �㡫��*/
  FIELD amt-cur   LIKE op-entry.amt-cur   /*�㬬� � �㡫��*/
  FIELD op        LIKE op.op              /**/
  FIELD op-kind   LIKE op.op-kind         /*��� �࠭���樨*/
  FIELD op-trans  LIKE op.op-transaction  /*����� �࠭���樨*/
  FIELD doc-date  LIKE op.doc-date        /*��� ᮧ�����*/
  FIELD op-date   LIKE op.op-date         /*��� �஢����*/
  FIELD user-id   LIKE op.user-id         /*���짮��⥫�*/
  FIELD user-name AS   CHARACTER          /*��� ���짮��⥫�*/
  FIELD branch-id LIKE op.branch-id       /*branch-id �஢����*/
INDEX graph save-type currency acct-db acct-dbf acct-dbl doc-numt.

/* ������塞 �室�騥 ��ࠬ���� */
iUserInp = ENTRY(1,iStr,';').
iHozS    = ENTRY(2,iStr,';').

{getdate.i}

/* ��६���� ��� ��।������ ���ᮢ��� ���㬥�� */
{op-cash.def}

/* ���樠������ �ࠢ�筨�� �.���-�� */
{elhran.def}

/* ���樠������ �ࠢ�筨�� 宧.���-�� */
{hozdoc.def}

/* �롨ࠥ� ���짮��⥫� ��� �������� */
CASE iUserInp:

  WHEN 'serv' THEN DO:

  /* ���ᮢ뢠�� ��� */
    PAUSE 0.

    DO ON ERROR UNDO, LEAVE ON ENDKEY UNDO, LEAVE WITH FRAME ftune3:
    UPDATE
      iFil LABEL "��� ���ࠧ�������" HELP "������ ��� ���ࠧ�������"   
    WITH CENTERED ROW 10 OVERLAY SIDE-LABELS 1 COL
    TITLE "[ ��ࠬ���� ���� ]".
    END.
    HIDE FRAME ftune3 NO-PAUSE.

    IF LASTKEY EQ KEYCODE("ESC") THEN
      RETURN.

    FIND FIRST code WHERE code.class EQ '��⏮�ࠧ�'
                      AND   code.code  EQ  iFil 
    NO-LOCK NO-ERROR.
      IF AVAIL(code) THEN 
      DO:
        IF code.misc[3] EQ iFil THEN
          vUserOtd = code.name.
        ELSE DO:
          FIND FIRST b-code WHERE b-code.class EQ '��⏮�ࠧ�'
                    AND   b-code.code EQ iFil.
          IF AVAIL(b-code) THEN
            vUserOtd = b-code.name.
        END.
      END.

   
    /* ayv 01.09.2015 ������塞 IRBIS, BIS � _SERV � SERV */
    ASSIGN
    iUserID = '*SERV*,IRBIS,BIS,SYNC'
    tUserID = 'SERV' + shFilial.
  
  END. /*serv */

  /* �롮� ���짮��⥫� */
  WHEN 'user' THEN DO:

    /* ���ᮢ뢠�� ��� */
    PAUSE 0.
    
    DO ON ERROR UNDO, LEAVE ON ENDKEY UNDO, LEAVE WITH FRAME ftune:
    UPDATE
      iUserID LABEL "��� ���짮��⥫�" HELP "������ ��� ���짮��⥫� ��� ����"   
    WITH CENTERED ROW 10 OVERLAY SIDE-LABELS 1 COL
    TITLE "[ ��ࠬ���� ���� ]".
    END.
    HIDE FRAME ftune NO-PAUSE.

    IF LASTKEY EQ KEYCODE("ESC") THEN
      RETURN.

    /*������塞*/
    FIND FIRST _user WHERE _user._userid BEGINS iUserID NO-LOCK NO-ERROR.
    IF AVAIL(_user) THEN DO:
      ASSIGN
        vUserName = _user._user-name
        iUserID   = _user._userid + ',*SERV*,SYNC'.
        tUserID   = _user._userid.

      /*�饬 ���ࠧ������� ���짮��⥫�*/
      iFil      = GetXattrValueEx("_user",STRING(_user._userid),"�⤥�����","").

      FIND FIRST code WHERE code.class EQ '��⏮�ࠧ�'
                      AND   code.code EQ  iFil 
          NO-LOCK NO-ERROR.
      IF AVAIL(code) THEN DO:
        IF code.misc[3] EQ shFilial THEN
          vUserOtd = code.name.
        ELSE DO:
          FIND FIRST b-code WHERE b-code.class EQ '��⏮�ࠧ�'
                    AND   b-code.code EQ shFilial.
          IF AVAIL(b-code) THEN
            vUserOtd = b-code.name.
        END.
      END.

    END.

  END. /* user */

  /* ���짮��⥫�, �맢��訩 ��楤��� */
  WHEN 'self' THEN DO:

    FIND FIRST _user WHERE _user._userid EQ userid("bisquit") NO-LOCK NO-ERROR.
    IF AVAIL(_user) THEN DO:
      ASSIGN
        vUserName = _user._user-name
        iUserID   = _user._userid + ',*SERV*,SYNC'.
        tUserID   = _user._userid.

      /*�饬 ���ࠧ������� ���짮��⥫�*/
      iFil      = GetXattrValueEx("_user",STRING(_user._userid),"�⤥�����","").

      FIND FIRST code WHERE code.class EQ '��⏮�ࠧ�'
                      AND   code.code  EQ  iFil 
          NO-LOCK NO-ERROR.
      IF AVAIL(code) THEN DO:
        IF code.misc[3] EQ shFilial THEN
          vUserOtd = code.name.
        ELSE DO:
          FIND FIRST b-code WHERE b-code.class EQ '��⏮�ࠧ�'
                    AND   b-code.code EQ shFilial.
          IF AVAIL(b-code) THEN
            vUserOtd = b-code.name.
        END.
      END.

    END.

  END. /*self*/

  /* ᢮���� ��������� ��� ���ࠧ������� */
  WHEN 'all' THEN DO:

    /* ���ᮢ뢠�� ��� */
    PAUSE 0.

    DO ON ERROR UNDO, LEAVE ON ENDKEY UNDO, LEAVE WITH FRAME ftune2:
    UPDATE
      iFil LABEL "��� ���ࠧ�������" HELP "������ ��� ���ࠧ�������"   
    WITH CENTERED ROW 10 OVERLAY SIDE-LABELS 1 COL
    TITLE "[ ��ࠬ���� ���� ]".
    END.
    HIDE FRAME ftune2 NO-PAUSE.

    IF LASTKEY EQ KEYCODE("ESC") THEN
      RETURN.

    /* �饬 ��� ���ࠧ������� */
    FIND FIRST code WHERE code.class EQ '��⏮�ࠧ�'
                    AND   code.code  EQ iFil
    NO-LOCK NO-ERROR.  
    IF NOT AVAIL(code) THEN
      MESSAGE '������ ��� ���ࠧ�������!' VIEW-AS ALERT-BOX ERROR.

    FIND FIRST branch WHERE branch.branch-id EQ iFil NO-LOCK NO-ERROR.
    IF AVAIL(branch) THEN
      vUserOtd = branch.name.

  END. /* all */

END CASE.

/* ������� �६����� ⠡���� tt-op */
FOR EACH op
   WHERE op.op-date EQ end-date
   AND   CAN-DO("�*,���",op.op-status)
   AND   op.acct-cat NE 'd'
   AND   op.filial-id EQ shFilial
NO-LOCK:
  CREATE tt-op.
  BUFFER-COPY op TO tt-op.
END.

/*===========================================================================*/
FOR EACH tt-op 
   WHERE tt-op.user-inspector NE ''
NO-LOCK:
  FOR EACH  op-entry 
      WHERE op-entry.op EQ tt-op.op
      AND   op-entry.op-date EQ end-date
  NO-LOCK:

    ASSIGN
      adb = op-entry.acct-db
      acr = op-entry.acct-cr
    .
        
    /* ��।������ ���ᮢ��� ���㬥�� */
    {op-cash.i}
    
    /* ��।������ ���஭���� ���㬥�� */
    {elhran.i}  
      
    /* ��७�ᨬ ���㬥�� SERV � ����஫����, �᫨ �� � �㬠�� */
    IF isAuto AND NOT isElH AND tt-op.user-inspector NE '' THEN
      DO:
        ASSIGN
          tt-op.user-id = tt-op.user-inspector
          tt-op.branch-id = GetTempXattrValueEx('_user', tt-op.user-id, '�⤥���������', end-date, '')
        .
        IF tt-op.branch-id EQ '' THEN 
          tt-op.branch-id = GetXattrValueEx('_user', tt-op.user-id, 'office', '').
      END.
  END.
END.

FOR EACH tt-op 
   WHERE tt-op.filial-id EQ '0500'
   AND   tt-op.branch-id EQ '0000'
   OR    CAN-DO('*SERV*,IRBIS,BIS,SYNC', tt-op.user-id)
NO-LOCK:
  ASSIGN
    tt-op.branch-id = tt-op.filial-id
  .
END.

/*
IF end-date EQ DATE(01/23/17) THEN 
DO:
  FOR EACH tt-op 
     WHERE CAN-DO('41740800,41742137', STRING(tt-op.op))
  NO-LOCK:
    ASSIGN
      tt-op.branch-id = '0503'
    .
  END.
END.

IF end-date EQ DATE(01/10/17) THEN 
DO:
  FOR EACH tt-op 
     WHERE tt-op.user-id EQ 'NSK_LTA'
     AND   tt-op.op-date EQ end-date
  NO-LOCK:
    ASSIGN
      tt-op.branch-id = '0517'
    .
  END.
END.
*/

FOR EACH tt-op 
  WHERE CAN-DO('0500LEK,0500FTA,0500RVG,0500STA,OU_CNV,OU_DOP,OU_GTV,OU_KNA,0500GEF,0500SUTA,0500SES,A0503KKB,0500MVY', tt-op.user-id) 
  AND   CAN-DO('0518,0598', tt-op.branch-id)
NO-LOCK:
  ASSIGN
    tt-op.branch-id = '0500'
  .
END.
/*===========================================================================*/

/* �⡨ࠥ� �஢���� */
FOR EACH  tt-op 
    WHERE tt-op.filial-id EQ shFilial
    AND   CAN-DO(iUserID,tt-op.user-id)
    AND   iUserInp NE "ALL"
     OR
     (tt-op.branch-id EQ iFil 
     AND iUserInp EQ "ALL")
NO-LOCK:
  
  FOR EACH  op-entry 
      WHERE op-entry.op EQ tt-op.op
      AND   op-entry.op-date EQ end-date
  NO-LOCK:

    ASSIGN
      adb = op-entry.acct-db
      acr = op-entry.acct-cr
    .
        
    /* ��।������ ���ᮢ��� ���㬥�� */
    {op-cash.i}
    
    /* ��।������ ���஭���� ���㬥�� */
    {elhran.i}  
      
    /* ��७�ᨬ ���㬥�� SERV � ����஫����, �᫨ �� � �㬠�� */
    IF isAuto AND NOT isElH AND tt-op.user-inspector NE '' THEN
      DO:
        ASSIGN
          tNotA = TRUE
          tStr  = tt-op.user-inspector
          tt-op.branch-id = GetTempXattrValueEx('_user', tStr, '�⤥���������', end-date, '')
        .
        IF tt-op.branch-id EQ '' THEN 
          tt-op.branch-id = GetXattrValueEx('_user', tStr, 'office', '').
      END.
    ELSE
      ASSIGN
        tNotA = FALSE
        tStr = tt-op.user-id
      .

    /* �ନ�㥬 ᯨ᮪ ���짮��⥫�� �� ���㬥�⠬ ��� ��饣� ॥���*/
    IF iUserInp EQ "ALL" THEN
    DO:
       IF LOOKUP(tStr, tUserID) = 0 THEN tUserID = tUserID + "," + tStr.
    END.

    /* ��室�� 䨮 ���㤭��� ��� ᢮���� �������� */
    FIND FIRST _user 
      WHERE _user._userid EQ tStr 
    NO-LOCK NO-ERROR.
    IF AVAIL(_user) THEN
      
      IF CAN-DO('*SERV*,IRBIS,BIS,QBIS,SYNC',tStr) THEN
        tStr = '��⮬���᪨� �࠭���樨'.
      ELSE
        tStr = _user._user-name.

    CREATE tt-op-day.
    ASSIGN
      tt-op-day.op-cid    = RECID(op)
      tt-op-day.ope-cid   = RECID(op-entry)
      tt-op-day.razdel    = IF isCash THEN "k" ELSE "b"
      tt-op-day.currency  = op-entry.currency
      tt-op-day.acct-cat  = IF tt-op.acct-cat EQ 'b' THEN tt-op.acct-cat ELSE 'o'
      tt-op-day.acct-dbf  = SUBSTRING(op-entry.acct-db,1,5)
      tt-op-day.acct-dbl  = SUBSTRING(op-entry.acct-db,16,5)
      tt-op-day.acct-db   = op-entry.acct-db
      tt-op-day.acct-cr   = op-entry.acct-cr
      tt-op-day.doc-num   = tt-op.doc-num
      tt-op-day.doc-numt  = FILL('0',20 - LENGTH(tt-op.doc-num)) + tt-op.doc-num
      tt-op-day.doc-type  = tt-op.doc-type
      tt-op-day.amt-rub   = IF op-entry.acct-db EQ ? THEN 0.00 ELSE op-entry.amt-rub
      tt-op-day.amt-cur   = IF op-entry.acct-db EQ ? THEN 0.00 ELSE op-entry.amt-cur
      tt-op-day.save-type = IF isElH THEN "2" ELSE "1"                      
      tt-op-day.hozdoc    = '-'
      tt-op-day.op        = tt-op.op
      tt-op-day.op-kind   = tt-op.op-kind
      tt-op-day.op-trans  = tt-op.op-transaction
      tt-op-day.doc-date  = tt-op.doc-date
      tt-op-day.op-date   = tt-op.op-date
      tt-op-day.user-id   = IF tNotA THEN tt-op.user-inspector ELSE (IF iUserInp EQ 'serv' THEN 'SERV' + shFilial ELSE tt-op.user-id)
      tt-op-day.user-name = IF AVAIL(_user) THEN tStr ELSE tt-op.user-id
      tt-op-day.branch-id = tt-op.branch-id
    .
  
    /* ��।������ �訢� */
    DO i = 1 TO NUM-ENTRIES(iHozS):
  
      {hozdoc.i &hoz-type = ENTRY(i,iHozS)}
      /* �� ����� �⬥砥� �� ���㬥���, �� �室�騥 � �����䨪��� ������ */
      IF ENTRY(i,iHozS) = '�����' THEN DO:
        IF NOT isHoz THEN DO:
          tt-op-day.hozdoc = ENTRY(i,iHozS).
          LEAVE.
        END.
      END.
      ELSE DO:
        IF isHoz THEN DO:
          tt-op-day.hozdoc = ENTRY(i,iHozS).
          LEAVE.
        END.
      END.
    
    END. /* do */

  END. /* for each op-entry */

END. /* for each op */

/* {setdest.i &file-name = "111.log"}
FOR EACH tt-op-day NO-LOCK:
  PUT UNFORMATTED tt-op-day.op ";" tt-op-day.user-id ";" tt-op-day.user-name ";" tt-op-day.branch-id ";" tt-op-day.currency ";" tt-op-day.acct-cat ";" tt-op-day.razdel ";" tt-op-day.save-type ";" tt-op-day.hozdoc ";" tt-op-day.amt-rub SKIP.
END.
{preview.i &file-name = "111.log"} */

/* {setdest.i &file-name = "111.log"}
FOR EACH tt-op-day NO-LOCK:
  PUT UNFORMATTED tt-op-day.razdel ";" tt-op-day.currency ";" tt-op-day.acct-cat ";" tt-op-day.acct-dbf ";" tt-op-day.acct-dbl ";" tt-op-day.acct-db ";" tt-op-day.acct-cr ";" tt-op-day.doc-num ";" tt-op-day.doc-numt ";" tt-op-day.doc-type ";" tt-op-day.amt-rub ";" tt-op-day.amt-cur ";" tt-op-day.save-type ";" tt-op-day.hozdoc ";" tt-op-day.op ";" tt-op-day.op-kind ";" tt-op-day.op-trans ";" tt-op-day.doc-date ";" tt-op-day.op-date ";" tt-op-day.user-id ";" tt-op-day.user-name ";" tt-op-day.branch-id SKIP.
END.
{preview.i &file-name = "111.log"} */

/* ���⠥� ᢮���� ��������� ��� �� ���짮��⥫� */
IF iUserInp EQ 'all' THEN DO:
  ASSIGN
    tUserID = TRIM(tUserID,",")
    iUserID = tUserID
    iUserID = iUserID + ',*SERV*'
    tServ = 'SERV' + shFilial
  .

  {svodallxls.i}
END.
ELSE DO:
  {svodxls0500.i}
END.