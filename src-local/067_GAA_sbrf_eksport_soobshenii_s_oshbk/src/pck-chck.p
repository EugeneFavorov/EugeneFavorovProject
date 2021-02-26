/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2003 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: PCK-CHCK.P
      Comment: ��ᯮ�� �����⮢������ ����⮢ ������
               ������� ��⮤�� (��� Report) �����, 㭠᫥�������� �� Packet

               �믮��塞� ����⢨�:
               1. �뢮����� ��� ��ᬮ�� ��, ᮧ����� � ⥪�饬 ᥠ��
                  ������, ᮮ�饭��, ����騥 �訡��. �� ��� ����� ����ந��
                  ����.
               2. �뢮����� ��� ��ᬮ�� ��, ᮧ����� � ⥪�饬 ᥠ��
                  ������,ᮧ����� ᮮ�饭��, �� ����騥 �訡��. �� ��� �����
                  ����ந�� ����.
               3. �᫨ ������ ��� ࠧ����� ���ﭨ� (�१ �������), �
                  �⬥祭�� ᮮ�饭�� �� ���ﭨ� 1 ��ॢ������ � ���ﭨ� 2.

               ��������� � ���� �࠭���樨 �� �ந��������
   Parameters: iClass      - ��� �����
               iInstance   - ᮤ�ন��� ����� iClass
         Uses:
      Used BY:
      Created: 30.03.2004 NIK
     Modified: 17.05.2004 NIK �������� �訡���� � ����ࠢ������ ����⮢
     Modified: 23.12.2005 NIK �᪫�祭�� �ᯮ�� ������
     Modified: 28.06.2005 NIK ������ ०��
     Modified: 05.08.2005 NIK �ᯮ�짮����� ����஥� �࠭���樨
     Modified: 26.05.2006 NIK PROGRESS v10.
     Modified:
*/
{globals.i}
{tmprecid.def}

DEFINE NEW GLOBAL SHARED STREAM debug-stream.

DEFINE INPUT PARAMETER iClass       AS CHAR     NO-UNDO.
DEFINE INPUT PARAMETER iInstance    AS handle   NO-UNDO.

DEFINE VAR mFlagSet     AS LOGICAL INIT ? NO-UNDO.
DEFINE VAR mOk          AS LOGICAL        NO-UNDO.

DEFINE VAR hPack        AS handle   NO-UNDO.
DEFINE VAR hFilterTable AS handle   NO-UNDO.
DEFINE VAR hQuery       AS handle   NO-UNDO.
DEFINE VAR mWhere       AS CHAR     NO-UNDO.

DEFINE VAR mSeanceID    AS INT64  NO-UNDO.
DEFINE VAR mKind        AS CHAR     NO-UNDO.
DEFINE VAR mState       AS CHAR     NO-UNDO.
DEFINE VAR mStateSrc    AS CHAR     NO-UNDO.
DEFINE VAR mStateTrg    AS CHAR     NO-UNDO.
DEFINE VAR mClass       AS CHAR     NO-UNDO.
DEFINE VAR mCount       AS INT64  NO-UNDO.

DEFINE VAR mOpDate      AS DATE     NO-UNDO.
DEFINE VAR mOpKind      AS CHAR     NO-UNDO.
DEFINE VAR mOpKindPre   AS CHAR     NO-UNDO.
DEFINE VAR mTmplID      AS INT64  NO-UNDO.
DEFINE VAR mRoleMessage AS CHAR     NO-UNDO.
DEFINE VAR mRequest     AS INT64  NO-UNDO.

DEFINE VAR mProcView    AS CHAR     NO-UNDO.
DEFINE VAR mFake        AS CHAR     NO-UNDO.
DEFINE VAR mBrowse      AS CHAR     NO-UNDO.
DEFINE VAR mFields      AS CHAR     NO-UNDO.
DEFINE VAR mValues      AS CHAR     NO-UNDO.

DEFINE VAR mFilter      AS CHAR     NO-UNDO.
DEFINE VAR hFilter      AS HANDLE   NO-UNDO.

DEFINE VAR mRetryErr    AS CHAR     NO-UNDO.
DEFINE VAR mTitle       AS CHAR     NO-UNDO.

{form.def}
{g-trans.equ}
{exchange.equ}

{intrface.get strng}
{intrface.get xclass}

{intrface.get data}
{intrface.get tmess}
{intrface.get pbase}
{intrface.get trans}

{intrface.get exch}
{intrface.get pack}

&GLOB RETRY-ERROR mRetryErr
/*============================================================================*/
MAIN:
DO ON ERROR UNDO MAIN, RETRY MAIN:
   {do-retry.i MAIN}
/*-------------------------------------------- �⥭�� ����஥� � ४����⮢ --*/
   IF auto THEN DO:
      mFlagSet = YES.
      LEAVE MAIN.
   END.

   ASSIGN
      hPack     = iInstance:default-buffer-handle
      mSeanceID = hPack:buffer-field("SeanceID"):buffer-value
      mState    = hPack:buffer-field("State"):buffer-value
      mClass    = hPack:buffer-field("Class-Code"):buffer-value
      mOpKind   = GetBaseOpKind()
      mTmplID   = GetBaseTemplate()
   NO-ERROR.
   IF ERROR-STATUS:ERROR THEN UNDO MAIN, RETRY MAIN.

   mStateSrc = GetEntries(1,mState,",","").

   ASSIGN
      mStateSrc    = {&STATE-CRT} WHEN NOT {assigned mStateSrc}
      mStateTrg    = GetEntries(2,mState,",",mStateSrc)
      mClass       = iClass WHEN NOT {assigned mClass}
      mRoleMessage = TRNSettingValue("","RoleMessage",{&RET-ERROR})
      mTitle       = GetAttrValue2(mOpKind,mTmplID,"$Title")
   NO-ERROR.
   IF ERROR-STATUS:ERROR THEN UNDO MAIN, RETRY MAIN.

   mRequest = PacketDefineRequest(hPack).        /* ��� �롮ન               */

   IF mRoleMessage EQ {&RET-ERROR} THEN DO:
      mRetryErr = "�� ������ ���祭�� ��ਡ�� ~"RoleMessage~"".
      UNDO MAIN, RETRY MAIN.
   END.
   IF mTitle       EQ {&RET-ERROR} THEN mTitle = "".

   IF mSeanceID NE 0 AND mSeanceID NE ? THEN DO:
      FIND FIRST Seance WHERE Seance.SeanceID EQ mSeanceID NO-LOCK NO-ERROR.
      IF ERROR-STATUS:ERROR THEN UNDO MAIN, RETRY MAIN.
   END.

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("PCK-CHCK.P","mOpKind:" + GetNullStr(mOpKind)           +
                              " mRequest:" + GetNullNum(decimal(mRequest)) +
                          " mRoleMessage:" + GetNullStr(mRoleMessage)).
   &ENDIF

/*--------------------------------------------����஫� ᮮ�饭�� � �訡���� --*/
   IF mRequest EQ {&REQ-SNC} THEN                /* ���� �࠭�����           */
      RUN pck-verr.p (mClass,
                      mSeanceID,
                      mRoleMessage).

/*------------------------------------------------ ��⮤ ��ᬮ�� ����権 --*/
   RUN GetClassMethod IN h_xclass (INPUT  mClass,
                                   INPUT  "View-OP",
                                   INPUT  "",
                                   INPUT  "",
                                   OUTPUT mProcView,
                                   OUTPUT mFake) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN UNDO MAIN, RETRY MAIN.


/*------------------------------------------------------ �����⮢�� �롮ન --*/
   CASE mRequest:
      WHEN {&REQ-CMP} THEN DO:                   /* ������ �����            */
         RUN GetEmptyInstance(INPUT  "filter",
                              INPUT  ?,
                              INPUT  0,
                              INPUT "",
                              OUTPUT hFilter,
                              OUTPUT mOk).
         IF NOT valid-handle(hFilter) THEN DO:
            mRetryErr = "���������� ᮧ���� ��ꥪ� ����� ~"filter~"".
            UNDO MAIN, RETRY MAIN.
         END.
         mBrowse = "FilterTable" + chr(2) + string(hFilter).
      END.
      WHEN {&REQ-PCK} THEN                       /* ���쪮 �� ᮮ�饭��      */
         mBrowse = PacketCreateBrowse (hPack, ?).
      WHEN {&REQ-SNC} THEN                       /* � ⥪�饬 ᥠ��          */
         mBrowse = PacketCreateBrowse (hPack, mSeanceID).
      WHEN {&REQ-FLT} THEN DO:                   /* ����� 䨫���              */
         mFilter = GetAttrValue(mOpKind,mTmplID,"$FilterTable").
         IF NOT {assigned mFilter} THEN DO:
            mRetryErr = "�����⮢����� 䨫��� ����".
            mFlagSet  = YES.
            UNDO MAIN, RETRY MAIN.
         END.
         ASSIGN
            hFilter = widget-handle(mFilter)
         NO-ERROR.
         IF ERROR-STATUS:ERROR        OR
            NOT valid-handle(hFilter) THEN UNDO MAIN, RETRY MAIN.
         mBrowse = "FilterTable"   + chr(1) + "ParentID" + chr(2) +
                   string(hFilter) + chr(1) + "-1".
      END.
   END CASE.

   IF NOT {assigned mBrowse}           OR
      NUM-ENTRIES(mBrowse,chr(2)) NE 2 THEN DO:
      mRetryErr = "����୮ ������ ���� 䨫��� ~"" + GetNullStr(mBrowse) + "~"".
      UNDO MAIN, RETRY MAIN.
   END.
   ELSE ASSIGN
      mFields = ENTRY(1,mBrowse,chr(2))
      mValues = ENTRY(2,mBrowse,chr(2))
   .
/*--------------------------------------- �����⮢�� ᮮ�饭�� ��� �ᯮ�� --*/
PCK-ST:
   DO WHILE YES ON ERROR UNDO PCK-ST, RETRY PCK-ST:
      IF RETRY THEN UNDO MAIN, RETRY MAIN.

      IF mRequest EQ {&REQ-CMP} THEN DO:
         CREATE QUERY hQuery.
         RUN PacketCreateQuery (hPack,
                                mRequest,
                                hQuery).

         IF NOT hQuery:query-open() THEN DO:
            mRetryErr = "���������� �����⮢��� �롮��".
            UNDO PCK-ST, RETRY PCK-ST.
         END.

         IF NOT hQuery:GET-FIRST()  THEN LEAVE PCK-ST.     /* ��� ����ᥩ     */
         IF hQuery:QUERY-OFF-END    THEN LEAVE PCK-ST.

         RUN g-fltr.p (INPUT  hFilter,           /* �⠭����� 䨫���        */
                       INPUT  hQuery,
                       INPUT  "",
                       OUTPUT mOk).
         IF NOT mOk THEN UNDO MAIN, RETRY MAIN.
      END.

      ASSIGN
         mFields = mFields + chr(1) + "ridrest"
         mValues = mValues + chr(1) + "YES"
      .

                             /* ������� ��楤�� ��ᬮ�� ����権          */
      IF {assigned mProcView} THEN ASSIGN
         mFields = mFields + chr(1) + "ViewObject"
         mValues = mValues + chr(1) + mProcView
      .

      IF {assigned mTitle}    THEN ASSIGN
         mFields = mFields + chr(1) + "Title"
         mValues = mValues + chr(1) + mTitle
      .

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("PCK-CHCK.P","mFileds:" + GetNullStr(mFields) +
                                  " mValues:" + GetNullStr(mValues)).
      &ENDIF

/*--------------------------------------------- ��ᬮ�� ᮧ����� ᮮ�饭�� --*/
      {empty tmprecid}
      RUN browseld.p ("Packet",
                      mFields,
                      mValues,
                      "",
                      4) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN UNDO MAIN, RETRY MAIN.
      RUN PacketUpdateStatus ("����", "����") NO-ERROR.
      IF mStateTrg EQ mStateSrc THEN             /* �� �㦭� ��������� �����*/
         LEAVE PCK-ST.

      mCount = 0.
      FOR EACH tmprecid:
         mCount = mCount + 1.
      END.

      IF mCount GT 0 THEN DO:
         MESSAGE COLOR NORMAL
            "�⬥祭�" mCount "ᮮ�饭��."                                    SKIP
            "�������� ����� �⬥祭�� ᮮ�饭�� �� ~"" + mStateTrg + "~"?" SKIP
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
            SET vChoice AS LOGICAL.
         CASE vChoice:
            WHEN YES THEN DO:
               RUN PacketUpdateStatus (mStateSrc, mStateTrg) NO-ERROR.
               IF ERROR-STATUS:ERROR THEN UNDO MAIN, RETRY MAIN.
            END.
            WHEN NO  THEN DO:
               MESSAGE COLOR NORMAL
                  "� 蠡����" mOpKind + "." + iClass "������ ��������� �����"  SKIP
                  "� ~"" + mStateSrc + "~" �� ~"" + mStateTrg + "~""             SKIP
                  "�� ��࠭� �� ������ ᮮ�饭�� ��� ��������� �����."        SKIP
                  "������ ��ਠ��:                                            " SKIP
                  "��  - �த������ �믮������ �࠭���樨 ��� ��������� �����" SKIP
                  "��� - �४���� �믮������ �࠭���樨                      " SKIP
                  "ESC - ������� �롮� ᮮ�饭��                             " SKIP
                  VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
                  SET vChoice1 AS LOGICAL.
               CASE vChoice1:
                  WHEN NO  THEN DO:
                     mRetryErr = "�⪠� ���짮��⥫� �� �믮������".
                     UNDO PCK-ST, RETRY PCK-ST.
                  END.
                  WHEN YES THEN LEAVE PCK-ST.
                  WHEN ?   THEN.
               END CASE.
            END.
         END CASE.
      END.
      ELSE
         LEAVE PCK-ST.

   END.                                          /* PCK-ST: DO WHILE YES:     */

   {empty tmprecid}
   mFlagSet = YES.
END.                                             /* MAIN: DO:                 */

IF valid-handle(hQuery)    THEN DELETE object hQuery.

{intrface.del}

{doreturn.i mFlagSet}
/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/
PROCEDURE PacketUpdateStatus:
   DEFINE INPUT PARAMETER iStateSrc AS CHAR NO-UNDO.
   DEFINE INPUT PARAMETER iStateTrg AS CHAR NO-UNDO.

   DEFINE VAR vFlagSet AS LOGICAL INIT ? NO-UNDO.

   RUN Init-SysMes({&AUTOR-UPDSTAT},"","").
   RUN Fill-SysMes({&AUTOR-UPDSTAT},"ComnExc50","","").
MAIN:
   DO TRANSACTION ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      FOR EACH tmprecid,
         FIRST Packet WHERE
               recid(Packet) EQ tmprecid.id
           AND Packet.State  EQ iStateSrc
               EXCLUSIVE-LOCK:

         ASSIGN Packet.State = iStateTrg NO-ERROR.
         IF ERROR-STATUS:ERROR THEN UNDO MAIN, RETRY MAIN.

         RUN Fill-SysMes({&AUTOR-UPDSTAT},
                         "ComnExc51",
                         "",
                         "%s=" + string(Packet.PacketID,"   >>>>>>>>>9") +
                         "%s=" + string(Packet.Kind,    "x(12)")         +
                         "%s=" + string(iStateSrc,      "x(9)")          +
                         "%s=" + string(iStateTrg,      "x(9)")).
      END.

      vFlagSet = YES.
   END.

   RUN End-SysMes.

   {doreturn.i vFlagSet}
END PROCEDURE.
/******************************************************************************/
