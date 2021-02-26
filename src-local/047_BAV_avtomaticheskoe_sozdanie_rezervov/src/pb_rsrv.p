/**
����᪨� �ࠢ� �ਭ�������: ��� ���� ����
���������:     g-rsrv.p (130d)
�᭮�����:      �����.428
�� ������:     ������ १��� �� ��⠬ "�� ���", �ନ��� ���� XL � ��ࠢ��� ��� �� ����.
��� ࠡ�⠥�:
��ࠬ����:      ��� ���भ�, �࠭�����
���� ����᪠:  �࠭����� rsrv_pr51 - �����஢騪
������:         20.12.2016 ���ᮢ �.�.
*/

{globals.i}
{sh-defs.i}
{intrface.get rsrv}
{intrface.get date}     /* �����㬥��� ��� ࠡ��� � ��⠬�. */
{intrface.get tmess}
{intrface.get xclass}
{def-wf.i NEW}
/* ��⠢�� ���� ���� */
{pb_exf_exl.i}          /** ������⥪� ��� ���㧪� � XL */
{pb_logit.i}
/* ����� ��⠢�� ���� ���� */

DEFINE INPUT PARAMETER in-op-date LIKE op.op-date.
DEFINE INPUT PARAMETER oprid      AS   RECID.
/* ��⠢�� ���� ���� */
IF HolidayRu(in-op-date) THEN RETURN.
/* ����� ��⠢�� ���� ���� */

DEFINE VARIABLE vCnt         AS INT64   NO-UNDO.
DEFINE VARIABLE vTotal       AS INT64   NO-UNDO. /* ��饥 ������⢮ ��ࠡ��뢠���� ��⮢ */
DEFINE VARIABLE vTotalAll    AS INT64   NO-UNDO. /* ��饥 ������⢮ ��࠭��� ��⮢ */
DEFINE VARIABLE vNtWrkCnt    AS INT64   NO-UNDO. /* ���-�� ��⮢ �� ����� �� �ॡ���� �ॣ㫨஢���� */
DEFINE VARIABLE vErrCnt      AS INT64   NO-UNDO. /* ���-�� ��⮢ �� ����� �ॣ㫨஢���� ��諮 � �訡���� */
DEFINE VARIABLE vOk          AS LOGICAL.
DEFINE VARIABLE vAcctOpen    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE vAcct        AS CHARACTER NO-UNDO.
DEFINE VARIABLE vLog         AS CHARACTER NO-UNDO.
/* ��⠢�� ���� ���� */
DEFINE VARIABLE cLog         AS CHARACTER NO-UNDO.
/* ����� ��⠢�� ���� ���� */
DEFINE VARIABLE vOkCnt       AS INT64   NO-UNDO. /* ������⢮ ��⮢ �� ����� �ॣ㫨஢���� 
                                                      ��諮 �ᯥ譮 */
DEFINE VARIABLE vAcctClass   AS CHARACTER NO-UNDO.
DEFINE VARIABLE vFRsrv       AS DECIMAL   NO-UNDO.
DEFINE VARIABLE vCRsrv       AS DECIMAL   NO-UNDO.
DEFINE VARIABLE mCont-Code   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAcct-Risk   AS CHARACTER NO-UNDO.
DEFINE VARIABLE vOldAutoNum  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE vOpD         AS DATE.
DEFINE VARIABLE vAftProc     AS CHARACTER NO-UNDO.
DEFINE VARIABLE vAcctDetails AS CHARACTER NO-UNDO.
DEFINE VARIABLE mUserID      AS CHARACTER NO-UNDO.
DEFINE VARIABLE vHandleProc  AS HANDLE NO-UNDO.
DEFINE VARIABLE nprrsrv      AS CHARACTER NO-UNDO.
DEFINE VARIABLE vProcParam   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDoc-num     AS CHARACTER NO-UNDO.
DEFINE VARIABLE vSomeBranch  AS LOGICAL   NO-UNDO.    /* ���� ���ࠧ������� ��� ��.१�ࢠ � �᪠ */
DEFINE VARIABLE mAutoMode    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDate446P    AS DATE      NO-UNDO.
DEFINE VARIABLE mOldPickValue LIKE pick-value NO-UNDO.
/* ��⠢�� ���� ���� */
DEFINE VARIABLE cMail        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cAcct        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cXL          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTmp         AS CHARACTER NO-UNDO.
DEFINE VARIABLE nTmp         AS DECIMAL   NO-UNDO.
DEFINE VARIABLE I            AS INT64     NO-UNDO.
/* ����� ��⠢�� ���� ���� */

DEFINE BUFFER bAcct FOR acct.
DEFINE BUFFER bf_tProv FOR tProv.
DEFINE STREAM sLog.

DEFINE TEMP-TABLE tt-acct-revexp NO-UNDO
   FIELD fNom         LIKE tProv.fNom
   FIELD acct-reserve LIKE acct.acct
   FIELD acct-revenue LIKE acct.acct
   FIELD acct-expense LIKE acct.acct

   INDEX fNom
      fNom
.

{details.def}
/* ��⠢�� ���� ���� */
FIND FIRST op-kind WHERE RECID(op-kind) = oprid NO-LOCK NO-ERROR.
RUN Init-SysMes (op-kind.op-kind, "", "").      /* ����� �� ��_�뢮�����࠭ */
/* ����� ��⠢�� ���� ���� */

{chkblock.i
  &surr=string(in-op-date)
  &msg="�� �� ����� �ࠢ� ࠡ���� � �������஢����� ����樮���� ���!"
  &action= "return."
}

/* ������� ���� ����
FIND FIRST op-kind WHERE RECID(op-kind) = oprid NO-LOCK NO-ERROR.
����� 㤠������� �ࠣ���� ���� ���� */

ASSIGN
   vAcctOpen    = GetXAttrValue("op-kind",op-kind.op-kind,"�����137�") =  "��"
   vAcctClass   = GetXAttrValue("op-kind",op-kind.op-kind,"ACCT-CLASS-CODE")
   nprrsrv      = GetXattrValueEx("op-kind",op-kind.op-kind,"nprrsrv","")
   vSomeBranch  = GetXattrValueEx("op-kind", op-kind.op-kind, "����珮�����",
                                  GetXattrInit("base-op-rsrv", "����珮�����")) =  "��"
/* ��⠢�� ���� ���� */
   cMail        = GetXAttrValue("op-kind",op-kind.op-kind,"e-mail")     /* ���� ���뫪� ���� XL */
/* ����� ��⠢�� ���� ���� */
.

IF NOT {assigned vAcctClass} THEN vAcctClass = "acctb".

vAftProc = GetXattrValue("op-kind",op-kind.op-kind,"aft-fill-prog").

RUN SetAfterProcedure IN h_rsrv(vAftProc).

ASSIGN
   end-date  = LastWorkDay(in-op-date - 1)
   mDate446P = DATE(FGetSetting("��446�", "���446�", "01/01/2016"))
NO-ERROR.

/* ����㧪� �����⥭⭮� ������⥪� */
RUN "g-rsrlib.p" PERSISTENT SET vHandleProc NO-ERROR.

mAutoMode = GetXAttrValue("op-kind",op-kind.op-kind,"��␥���").
IF mAutoMode <> "��" THEN
DO:
   bl:
   REPEAT ON ERROR  UNDO,RETURN
          ON ENDKEY UNDO,RETURN:
      {getdate.i
         &DateLabel = "�।�.�ॣ㫨�."
         &DateHelp  = "��� �।��饣� �ॣ㫨஢���� १�ࢠ (F1 - ���������)"
         &noinit    = YES
      }
      IF end-date >= in-op-date THEN DO:
         RUN Fill-sysmes IN h_tmess ("","","0","��� �।��饣� �ॣ㫨஢���� ������ ���� ����� ���� ���भ�!").
         NEXT bl.
      END.
      ELSE
         LEAVE bl.
   END.
END.

/* ����� �।���⥫쭮� ��楤��� */
IF nprrsrv <> ""
THEN DO:
   /* �᫨ ��ࠬ���� �� ������ �� ��, � �㤥� ��।������� ���
   ** ���� ��ࠬ���� 㪠����� �१ "((".
   ** ����室��� ��� ���४⭮�� ��।������ ���⮪�� � ��楤�� setacdrv.p */
   vProcParam = SUBSTR(nprrsrv, (IF INDEX(nprrsrv, "((") =  0
                                 THEN ?
                                 ELSE INDEX(nprrsrv, "((") + 2)).

   RUN VALUE(SUBSTR(nprrsrv, 1, IF INDEX(nprrsrv, "((") =  0
                                THEN 255
                                ELSE INDEX(nprrsrv, "((") - 1))
            (IF vProcParam =  ? THEN STRING(in-op-date)
                                ELSE vProcParam ).
   
   IF RETURN-VALUE <> "yes" THEN 
   DO:
      {intrface.del}
      RETURN.
   END.
END.

IF mAutoMode =  "��" THEN
DO TRANSACTION:
   RUN FillTProv IN h_rsrv ("�����137�",
                            in-op-date,
                            end-date,
                            GetXAttrValue("op-kind",op-kind.op-kind,"��᪠���"),
                            GetXAttrValue("op-kind",op-kind.op-kind,"�⢥��⢥���") = "��",
                            GetXAttrValue("op-kind",op-kind.op-kind,"������"),
                            YES, 
                            OUTPUT TABLE tProv).
END.
ELSE
DO TRANSACTION:
   RUN brwrsrv.p(in-op-date,
                 end-date,
                 YES,
                 GetXAttrValue("op-kind",op-kind.op-kind,"��᪠���"),
                 GetXAttrValue("op-kind",op-kind.op-kind,"�⢥��⢥���") = "��",
                 GetXAttrValue("op-kind",op-kind.op-kind,"������"),
                 OUTPUT TABLE tProv,4).
END.
/* ��⠢�� ���� ���� */
/* ��⠢��� ⮫쪮 ��� "�� ���", � ������ ���� ��� acct47423 */
FOR EACH tProv:
    IF (GetLinks("acctb", tProv.acct-risk + "," + tProv.currency, "", "acct47423", "!", TODAY) EQ "")    /* ��� �裡 � �/� */
    THEN DELETE tProv.
END.
/* ����� ��⠢�� ���� ���� */
IF KEYFUNCTION(LASTKEY) =  "END-ERROR" THEN DO:
   DELETE PROCEDURE vHandleProc NO-ERROR.
   {intrface.del}
   RETURN.
END.

/* ��⠢�� ���� ���� */
IF mAutoMode NE "��" THEN DO:
/* ����� ��⠢�� ���� ���� */
    {logset.i}

    vLog = op-kind.op-kind + ".log".
    OUTPUT STREAM sLog TO VALUE(vLog).
    PUT STREAM sLog UNFORMATTED
       "�������� �������� �������������� ������� (137-�).~n".
    PUT STREAM sLog UNFORMATTED STRING(TODAY,"99/99/9999") + " " +
       STRING(TIME,"HH:MM:SS") "~n".
    PUT STREAM sLog UNFORMATTED "~n".

/* ��⠢�� ���� ���� */
END.
ELSE DO:    /* ���뢠�� ��⮪�� � XL */
    vLog = STRING(YEAR(in-op-date)) + STRING(MONTH(in-op-date), "99") + STRING(DAY(in-op-date), "99").
    cLog = "/home2/bis/quit41d/log/rsrv/" + op-kind.op-kind + "-" + vLog + ".txt".
    vLog = "/home2/bis/quit41d/log/rsrv/" + op-kind.op-kind + "-" + vLog + "-" + shFilial + ".xml".
    OUTPUT STREAM sLog TO VALUE(vLog).

    IF (shFilial EQ "0500")
    THEN PUT STREAM sLog UNFORMATTED XLHead("rsrv", "CDCCCCNNC", "60,70,65,213,213,213,121,125,130").
    ELSE PUT STREAM sLog UNFORMATTED XLHead("rsrv", "DCCCCNNC", "70,65,213,213,213,121,125,130").

    cXL = (IF (shFilial EQ "0500") THEN XLCellHead("����� ��㯯�",0,0,0) ELSE "")
        + XLCellHead("��� �஢����",0,0,0)
        + XLCellHead("��� ������",0,0,0)
        + XLCellHead("����� �/���",0,0,0)
        + XLCellHead("����� ��� 47423*",0,0,0)
        + XLCellHead("����� ��� 47425*",0,0,0)
        + XLCellHead("% ����ࢨ஢����",0,0,0)
        + XLCellHead("�㬬� ᮧ������ �஢����",0,0,0)
        + XLCellHead("�ਬ�砭��",0,0,0)
        .
    PUT STREAM sLog UNFORMATTED XLRow(0) cXL XLRowEnd().
    RUN LogIt("������: " + shFilial, cLog).
    RUN LogIt((IF (shFilial EQ "0500") THEN "������  " ELSE "")
            + "����        ������     ����� �/�����         ����� ����� 47423*    ����� ����� 47425*    % ���   ����� ��������  ����������", cLog).
END.
/* ����� ��⠢�� ���� ���� */

IF LASTKEY <> 27 THEN
DO:
   TR:
   DO TRANSACTION 
      ON ERROR UNDO TR,LEAVE TR
      ON STOP UNDO TR,LEAVE TR:
      
      FOR EACH tProv NO-LOCK:
               
         IF    tProv.rsrv-amt > 0     
            OR (    tProv.rsrv-amt = 0     
                AND tProv.acct-rsrv-bal > 0) THEN
            vTotal = vTotal + 1.
         vTotalAll = vTotalAll + 1.
      END.

/* ��⠢�� ���� ���� */
      IF mAutoMode NE "��" THEN DO:
/* ����� ��⠢�� ���� ���� */
          PUT STREAM sLog UNFORMATTED
             "����������         ����� ������� ������       " vTotalAll SKIP.
          PUT STREAM sLog UNFORMATTED
             "����������         ����� ������ ��� ��������� " vTotal    SKIP(1).

          {bar-beg2.i 
             &BarTotal = "vTotal" 
             &BarMessage = """��ࠡ�⪠ ������..."""}
/* ��⠢�� ���� ���� */
      END.
/* ����� ��⠢�� ���� ���� */

      mOldPickValue = pick-value.

      LOOP:
      FOR EACH tProv WHERE
               tProv.rsrv-amt > 0     
            OR (    tProv.rsrv-amt = 0     
                AND tProv.acct-rsrv-bal > 0)
      ON ERROR UNDO LOOP, NEXT LOOP 
      ON STOP UNDO LOOP,NEXT LOOP:

/* ��⠢�� ���� ���� */
         IF mAutoMode NE "��" THEN DO:
/* ����� ��⠢�� ���� ���� */
         vCnt = vCnt + 1.
         {bar2.i 
            &BarPointer = "vCnt" 
            &BarBreak = "UNDO TR,LEAVE TR."}
         PUT STREAM sLog UNFORMATTED
            "����������         �������������� ���� � " tProv.acct-risk SKIP.
/* ��⠢�� ���� ���� */
         END.
/* ����� ��⠢�� ���� ���� */

         IF     tProv.acct-reserve = "" 
            AND vAcctOpen THEN
         DO:
/* ��⠢�� ���� ���� */
            IF mAutoMode NE "��" THEN
/* ����� ��⠢�� ���� ���� */
            PUT STREAM sLog UNFORMATTED
               "����������         ��� ����� ����� ����������� ���� �������" SKIP.

            /* ��।������ �� AcctReservName ������ ���� ���, �.�. ���� ���� ��������
            ** ���⠢���� �ᥬ ᮧ����� ��⠬. � ⠪, ����� ࠧ ��� ������� ��� �㤥�
            ** ���᫥� �� �᭮����� �����. */
            ASSIGN
               vAcctDetails = GetXattrValue("op-kind",op-kind.op-kind,"AcctReservName").
               mUserID = GetXattrValue("op-kind",op-kind.op-kind,"AcctReservUserID").

            IF {assigned mUserID} AND mUserID <> "��␨᪠" THEN DO:
               FIND FIRST _user WHERE _user._userid =  mUserID NO-LOCK NO-ERROR.
               IF AVAIL(_user) THEN DO:
                  IF GetXattrValue("_user",mUserID,"Blocked") =  "�����஢��"
                  THEN mUserID = "".
               END.
               ELSE mUserID = "".
            END.

            /* ��ନ�㥬 ���祭�� ४����� "���������������" */
            RUN SetCurrentRec IN h_rsrv (tProv.fNom,TABLE tProv).
            RUN ProcessDetails (?, INPUT-OUTPUT vAcctDetails).
            RUN CrAcctRsrv IN h_rsrv (tProv.fNom,
                                      vAcctClass,
                                      vAcctDetails,
                                      vSomeBranch,
                                      mUserID,
                                      TABLE tProv,
                                      OUTPUT vAcct).
            IF vAcct = ? THEN
            DO:
/* ��⠢�� ���� ���� */
               IF mAutoMode NE "��" THEN
/* ����� ��⠢�� ���� ���� */
               PUT STREAM sLog UNFORMATTED
                  "������             ������ �������� ����� �������" SKIP.
/* ��⠢�� ���� ���� */
               ELSE DO:
                   cXL = (IF (shFilial EQ "0500") THEN XLEmptyCell() ELSE "")
                       + XLDateCell(in-op-date)
                       + XLEmptyCells(2)
                       + XLCell(Tprov.acct-risk)
                       + XLEmptyCells(3)
                       + XLCell("�訡�� ������ ��� १�ࢠ")
                       .
                   PUT STREAM sLog UNFORMATTED XLRow(0) cXL XLRowEnd().
                   RUN LogIt((IF (shFilial EQ "0500") THEN "        " ELSE "")
                           + STRING(in-op-date, "99.99.9999")  + STRING(" ", "x(35)")
                           + SUBSTRING(Tprov.acct-risk, 1, 20) + STRING(" ", "x(48)")
                           + "������ �������� ����� �������", cLog).
               END.
/* ����� ��⠢�� ���� ���� */
               vErrCnt = vErrCnt + 1.
               NEXT.
            END.
            tProv.acct-reserve = vAcct.
            RUN SaveRsrvParameters IN h_rsrv (tProv.fNom,in-op-date,TABLE tProv).
/* ��⠢�� ���� ���� */
            IF mAutoMode NE "��" THEN
/* ����� ��⠢�� ���� ���� */
            PUT STREAM sLog UNFORMATTED
               "����������         ������ ���� ������� � " tProv.acct-reserve SKIP.
/* ��⠢�� ���� ���� */
            ELSE DO:
                cXL = (IF (shFilial EQ "0500") THEN XLEmptyCell() ELSE "")
                    + XLDateCell(in-op-date)
                    + XLEmptyCells(2)
                    + XLCell(Tprov.acct-risk)
                    + XLCell(Tprov.acct-reserve)
                    + XLEmptyCells(2)
                    + XLCell("����� ��� १�ࢠ")
                    .
                PUT STREAM sLog UNFORMATTED XLRow(0) cXL XLRowEnd().
                RUN LogIt((IF (shFilial EQ "0500") THEN "        " ELSE "")
                        + STRING(in-op-date, "99.99.9999")     + STRING(" ", "x(35)")
                        + SUBSTRING(Tprov.acct-risk,    1, 20) + "  "
                        + SUBSTRING(Tprov.acct-reserve, 1, 20) + STRING(" ", "x(26)")
                        + "������ ���� �������", cLog).
            END.
/* ����� ��⠢�� ���� ���� */
         END.
            /* 
               ��堭��� ��㯯�஢�� �� ��⠬ �� ᮧ����� ᢮���� ���㬥�⮢
               �㤥� ������� �� ⮣�, ���㯨�� �� � ᨫ� ��������� 446-�.
               �� ���� ���㯫���� � ᨫ� ��㯯�஢�� ���� �� ��஬�: ⮫쪮
               �� ��⠬ १�ࢠ. ��᫥ ���㯫���� � ᨫ� - �� ��⠬ १�ࢠ
               � �� ��⠬ ��室��-��室��, ����� ⥯��� ���� ������� ��
               ⨯� ������.
            */
            CREATE tt-acct-revexp.
            ASSIGN
               tt-acct-revexp.fNom         = tProv.fNom
               tt-acct-revexp.acct-reserve = tProv.acct-reserve
            .
            IF in-op-date >= mDate446P THEN
               RUN GetAcctRevExp446P IN THIS-PROCEDURE
                  (vHandleProc,
                   OUTPUT tt-acct-revexp.acct-revenue,
                   OUTPUT tt-acct-revexp.acct-expense).
            RELEASE tt-acct-revexp.


         ASSIGN 
            vCRsrv = tProv.rsrv-amt
            vFRsrv = 0
            vOpD   = DYNAMIC-FUNCTION("GetOpDate" IN h_rsrv)
         .

         {find-act.i
            &bact = bAcct
            &acct = tProv.acct-reserve
            &curr = ""
         }   

         IF AVAIL bAcct THEN
         DO:
            IF     bAcct.open-date <= vOpD 
               AND (   bAcct.close-date =  ? 
                    OR bAcct.close-date >  vOpD ) THEN DO:
   
               RUN acct-pos IN h_base (bAcct.acct,
                                       bAcct.currency,
                                       vOpD,
                                       vOpD,
                                       CHR(251)
               ).
               vFRsrv = ABS(sh-bal).
            END.
         END. 
         ELSE DO:
/* ��⠢�� ���� ���� */
            IF mAutoMode NE "��" THEN
/* ����� ��⠢�� ���� ���� */
            PUT STREAM sLog UNFORMATTED
               "������             �� ������ ����� ������� " tProv.acct-reserve SKIP.

            vErrCnt = vErrCnt + 1.   
            NEXT.
         END.

         IF vCRsrv = vFRsrv THEN
         DO:
/* ��⠢�� ���� ���� */
            IF mAutoMode NE "��" THEN
/* ����� ��⠢�� ���� ���� */
            PUT STREAM sLog UNFORMATTED
              "����������         ��� ����� ����� " tProv.acct-risk " �������������� �� ���������." SKIP.
/* ��⠢�� ���� ���� */
            ELSE DO:
                cXL = (IF (shFilial EQ "0500") THEN XLEmptyCell() ELSE "")
                    + XLDateCell(in-op-date)
                    + XLEmptyCells(2)
                    + XLCell(Tprov.acct-risk)
                    + XLCell(Tprov.acct-reserve)
                    + XLNumCell(Tprov.pers-rsrv)
                    + XLEmptyCell()
                    + XLCell("�ॣ㫨஢���� �� �ॡ����")
                    .
                PUT STREAM sLog UNFORMATTED XLRow(0) cXL XLRowEnd().
                RUN LogIt((IF (shFilial EQ "0500") THEN "        " ELSE "")
                        + STRING(in-op-date, "99.99.9999")     + STRING(" ", "x(35)")
                        + SUBSTRING(Tprov.acct-risk,    1, 20) + "  "
                        + SUBSTRING(Tprov.acct-reserve, 1, 20) + "  "
                        + STRING(Tprov.pers-rsrv, ">>9.99")    + STRING(" ", "x(18)")
                        + "�������������� �� ���������", cLog).
            END.
/* ����� ��⠢�� ���� ���� */
            vNtWrkCnt = vNtWrkCnt + 1.
            NEXT.
         END.


/* ��⠢�� ���� ���� */
         IF mAutoMode NE "��" THEN
/* ����� ��⠢�� ���� ���� */
         PUT STREAM sLog UNFORMATTED
            "����������         ���� ����� � " tprov.acct-risk " ������� ���������" SKIP(1).
         vOkCnt = vOkCnt + 1.
      END.

      ASSIGN
         vOk        = YES
         pick-value = mOldPickValue
      .
   END.  /* End of TR BLOCK */
END.

/* ������ ���� ����
FIND LAST tProv NO-LOCK NO-ERROR.
vCnt = tProv.fNom + 1.
*/
FOR LAST tProv:
    vCnt = tProv.fNom + 1.
END.
/* ����� ������ ���� ���� */
FOR EACH tProv
   WHERE
      tProv.rsrv-amt >  0        OR
      (tProv.rsrv-amt      =  0  AND
       tProv.acct-rsrv-bal >  0)
NO-LOCK,
FIRST tt-acct-revexp WHERE
   tt-acct-revexp.fNom = tProv.fNom
NO-LOCK
BREAK BY tt-acct-revexp.acct-reserve
      BY tt-acct-revexp.acct-revenue
      BY tt-acct-revexp.acct-expense:
   {find-act.i
      &acct = tProv.acct-reserve
      &curr = ""
   }   
   IF NOT AVAIL acct           OR
      tProv.acct-reserve =  ?  OR
      tProv.acct-reserve =  ""
   THEN DO:
      DELETE tProv.
      NEXT.
   END.

   IF FIRST-OF(tt-acct-revexp.acct-reserve) THEN
   ASSIGN
      mAcct-Risk = ""
      mCont-code = "0"
   .

   IF mCont-code      =  "0"  AND
      tProv.cont-code <> ?    AND
      tProv.cont-code <> ""
   THEN
   mCont-code = tProv.cont-code.

   IF NUM-ENTRIES(mAcct-Risk) <= 50 THEN
   {additem.i mAcct-Risk tProv.acct-risk}

   ACCUMULATE tProv.rsrv-amt (TOTAL BY tt-acct-revexp.acct-reserve).

   IF LAST-OF(tt-acct-revexp.acct-reserve) THEN DO:
      CREATE bf_tProv.
      bf_tProv.fNom = vCnt.
      BUFFER-COPY tProv
           EXCEPT fNom
                  rsrv-amt
                  cont-code
           TO bf_tProv.
      ASSIGN
         bf_tProv.rsrv-amt       = ACCUM TOTAL BY tt-acct-revexp.acct-reserve tProv.rsrv-amt
         bf_tProv.cont-code      = mCont-code
         bf_tProv.lst-acct-risk  = mAcct-Risk
         mAcct-Risk              = ""
         mCont-code              = "0"
         vCnt                    = vCnt + 1
      .
   END.
   DELETE tProv.
END.

FOR EACH tProv WHERE
    tProv.rsrv-amt >  0     OR
   (tProv.rsrv-amt =  0     AND
    tProv.acct-rsrv-bal >  0)
NO-LOCK:
   RUN SaveRsrvParameters IN h_rsrv (tProv.fNom,in-op-date,TABLE tProv).
   RUN SetCurrentRec IN h_rsrv (tProv.fNom,TABLE tProv).
   ASSIGN
      vOldAutoNum = autonumdoc
      autonumdoc  = YES
      mDoc-num    = ?
   .
   RUN g-rsrv2.p(in-op-date, 
                 oprid,
                 OUTPUT mDoc-num).
   autonumdoc = vOldAutoNum.

/* ��⠢�� ���� ���� */
    IF mAutoMode NE "��" THEN DO:
/* ����� ��⠢�� ���� ���� */
       IF mDoc-num <> ? THEN
          PUT STREAM sLog UNFORMATTED
             "����������         ��� ����� ����� � "
             tProv.acct-risk 
             " ������ �������� � "
             mDoc-num 
          SKIP.
       ELSE
          PUT STREAM sLog UNFORMATTED
             "��������������     ��� ����� ����� � "
             Tprov.acct-risk 
             " �������� �� ������"
          SKIP.
    END.
/* ��⠢�� ���� ���� */
    ELSE DO:    /* ���������� ��⮪��� � XL */
        {find-act.i
            &bact = bAcct
            &acct = tProv.acct-risk
            &curr = tProv.currency
        }
        vAcct = GetLinks("acctb", tProv.acct-risk + "," + tProv.currency, "", "acct47423", "!", TODAY).
        IF (vAcct NE "")
        THEN DO:
            DO I = 1 TO NUM-ENTRIES(vAcct, "!"):
                cTmp = ENTRY(I, vAcct, "!").
                IF (ENTRY(2, cTmp) EQ "")
                THEN DO:
                    vAcct = cTmp.
                    LEAVE.
                END.
            END.

            IF (NUM-ENTRIES(vAcct, "!") NE 1)
            THEN vAcct = ENTRY(1, vAcct, "!").
        END.

        IF (mDoc-num NE ?)
        THEN
            FOR FIRST op
                WHERE (op.op-date   EQ in-op-date)
                  AND (op.op-kind   EQ op-kind.op-kind)
                  AND (op.doc-num   EQ mDoc-num)
                NO-LOCK,
            FIRST op-entry OF op
                NO-LOCK:

                nTmp = IF (tProv.currency EQ "") THEN op-entry.amt-rub ELSE op-entry.amt-cur.
            END.

        cTmp = IF (vAcct NE "") THEN GetXAttrValue("acct", vAcct, "groupOABS") ELSE "".
        cXL = (IF (shFilial EQ "0500") THEN XLCell(cTmp) ELSE "")
            + XLDateCell(in-op-date)
            + XLCell(bAcct.cust-cat + "_" + STRING(bAcct.cust-id))
            + XLCell(ENTRY(1, vAcct))
            + XLCell(Tprov.acct-risk)
            + XLCell(Tprov.acct-reserve)
            + XLNumCell(Tprov.pers-rsrv)
            + (IF (mDoc-num NE ?) THEN XLNumCell(nTmp) ELSE XLEmptyCell())
            + XLCell(IF (mDoc-num EQ ?) THEN "���㬥�� �� ᮧ���" ELSE "")
            .
        PUT STREAM sLog UNFORMATTED XLRow(0) cXL XLRowEnd().
        RUN LogIt((IF (shFilial EQ "0500") THEN STRING(cTmp, "x(8)") ELSE "")
                + STRING(in-op-date, "99.99.9999")     + "  "
                + CODEPAGE-CONVERT(STRING(bAcct.cust-cat + "_" + STRING(bAcct.cust-id), "x(9)"),"IBM866","1251") + "  "
                + SUBSTRING(ENTRY(1, vAcct),    1, 20) + "  "
                + SUBSTRING(Tprov.acct-risk,    1, 20) + "  "
                + SUBSTRING(Tprov.acct-reserve, 1, 20) + "  "
                + STRING(Tprov.pers-rsrv, ">>9.99")    + "  "
                + (IF (mDoc-num NE ?) THEN STRING(nTmp, ">>>,>>>,>>9.99") ELSE "              ") + "  "
                + (IF (mDoc-num EQ ?) THEN "�������� �� ������" ELSE ""), cLog).
    END.
END.

IF mAutoMode NE "��" THEN DO:
/* ����� ��⠢�� ���� ���� */
    IF vOk <> YES THEN
      PUT STREAM sLog UNFORMATTED
       "������             ���������� <" op-kind.op-kind
       "> �������� �������������" SKIP.
    ELSE
      PUT STREAM sLog UNFORMATTED "����������         ���������� ������ - "
         vOkCnt + vErrCnt + vNtWrkCnt " �� ��� � �������� - " vErrCnt 
         ", �������������� �� ��������� - " vNtWrkCnt SKIP.

    {logreset.i}

    MESSAGE "".
    PAUSE 0.

    {setdest.i &STREAM   = "STREAM sLog "
               &APPEND   = " APPEND "
               &FILENAME = vLog
    }
/* ������� ���� ����
IF mAutoMode <> "��" THEN
DO:
����� 㤠������� �ࠣ���� ���� ���� */
   {preview.i &STREAM= "STREAM sLog " &FILENAME = vLog}
END.
/* ��⠢�� ���� ���� */
ELSE DO:    /* ��ࠢ�� ��⮪��� �� ���� */
    PUT STREAM sLog UNFORMATTED XLEnd().
    OUTPUT STREAM sLog CLOSE.
    RUN LogIt(" ", cLog).
    RUN LogIt(" ", cLog).
    RUN pb_mail.p (cMail, "RESERV " + shFilial, "", vLog + "," + cLog).
END.
/* ����� ��⠢�� ���� ���� */

DELETE PROCEDURE vHandleProc NO-ERROR.
{intrface.del}

/*------------------------------------------------------------------------------
  Purpose:     Callback - ��楤��. ����஢���� ᮮ�饭�� �� ������ �㦡
  Parameters:  pMess  - ⥪�� ᮮ�饭��
               pEvnt  - ��� ᮮ�饭��
               pCont  - �ਧ��� ���������� �த������� �믮������
               opFlag - ��� ������
                  0       - �த������ ��ࠡ���
                  �� ���� - ��ࢠ�� ��ࠡ���
  Notes:
------------------------------------------------------------------------------*/
PROCEDURE LogMessage:
  DEFINE INPUT  PARAMETER pMess AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER pEvnt AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER pCont AS CHARACTER  NO-UNDO.
  DEFINE OUTPUT PARAMETER opFlag AS INT64    NO-UNDO.

  IF pCont <> "YES" THEN
  DO:
    PUT STREAM sLog UNFORMATTED "������             " pMess SKIP.
    opFlag = 1.
  END. ELSE
  DO:
    PUT STREAM sLog UNFORMATTED "��������������     " pMess SKIP.
    opFlag = 0.
  END.
END PROCEDURE.

/* ���᫥��� ��⮢ ��室� � ��室� ��� ��⠭�������� tProv ᮣ��᭮ 446-� */
PROCEDURE GetAcctRevExp446P PRIVATE:
   DEFINE INPUT  PARAMETER iHRsrLib AS   HANDLE                      NO-UNDO.
   DEFINE OUTPUT PARAMETER oAcctRev LIKE tt-acct-revexp.acct-revenue NO-UNDO.
   DEFINE OUTPUT PARAMETER oAcctExp LIKE tt-acct-revexp.acct-expense NO-UNDO.

   IF VALID-HANDLE(iHRsrLib) THEN DO:
      RUN VALUE("�焮吠��") IN iHRsrLib (?, ?, ?, "[�]").
      oAcctRev = pick-value.
      RUN VALUE("�焮吠��") IN iHRsrLib (?, ?, ?, "[�]").
      oAcctExp = pick-value.
   END.
END PROCEDURE.
/* $LINTFILE='g-rsrv.p' */
/* $LINTMODE='11,-9,6,3' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='soav' */
/* $LINTDATE='07/03/2017 11:27:40.737+03:00' */
/*prosignaF8FvsaUZFIGE+Z6y3CiQA*/