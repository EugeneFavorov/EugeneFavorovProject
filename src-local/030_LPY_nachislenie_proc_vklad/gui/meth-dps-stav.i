
/* +++ meth-dps-stav.i was humbly modified by (c)blodd converter v.1.09 on 10/5/2016 2:03pm +++ */

/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2006 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: meth-dps-stav.i
      Comment: ��⮤� ��� ���᫥��� ���� �⠢�� ��� ���⠭���⭮�� ���᫥���. �࠭���� � ��⮤�����
   Parameters:
         Uses:
      Used by:
      Created: 01.12.2006  Ariz
*/

/* ��।������ ��������� �⠢�� ��� ������ "�஦����" 
** ��楤�� �����頥� ��� ���������� �⠢�� (���祭�� �� ������⠢),
** �᫨ �뫠 ����襭� ��ਮ��筮��� ���������� �� ������,
** ���, �᫨ ����襭�� �� �뫮, ���⮥ ���祭�� "" */
PROCEDURE stav_yield:
   DEF INPUT  PARAM iLoan     AS RECID  NO-UNDO.   /* RECID ������ */
   DEF INPUT  PARAM iDate     AS DATE   NO-UNDO.   /* ��� ����樨 */
   DEF INPUT  PARAM iDateEnd  AS DATE   NO-UNDO.   /* ��� ����樨 */
   DEF OUTPUT PARAM oComm     AS CHAR   NO-UNDO.   /* �����頥�� ��� ���������� �⠢�� */

   DEF VAR mIdOp     AS CHAR   NO-UNDO.    /* id ���㬥�� */
   DEF VAR mDComm    AS CHAR   NO-UNDO.   /* ���������� �⠢�� */
   DEF VAR fl-close  AS LOG    NO-UNDO.   /* �ந�������� �� �����⨥ ������ */
   DEF VAR mBegDate  AS DATE   NO-UNDO.   /* ��� ��砫� ��ਮ�� �஢�ન ������. */
   DEF VAR mMonths   AS INT64    NO-UNDO.   /* ���稪 ������⢠ ����楢 ��� �஢�ન ������. */
   DEF VAR mMoveDate AS DATE   NO-UNDO.   /* ��� ������襣� �������� �� ���� ������ */
   DEF VAR fl-NoMove AS LOG    NO-UNDO.   /* YES - �᫨ �� ᮡ��� ��ਮ��筮��� ����������� */

   PROC:
   DO:
      /* ��।������ id  ���㬥�� */
      mIdOp = GetSysConf("o p-id").
      /* ���� ���㬥�� */
      FIND FIRST op WHERE op.op EQ INT64(ENTRY(1,mIdOp)) NO-LOCK NO-ERROR.
      IF    NOT {assigned mIdOp}
         OR NOT AVAILABLE op
      THEN LEAVE PROC.

      /* ��।������ ���������� �⠢�� */
      RUN Get_Last_Param in h_dpspc (iLoan,
                                             iDate,
                                             iDate,
                                             "������⠢",
                                             OUTPUT mDComm).
      IF mDComm EQ "" THEN LEAVE PROC.

      FIND FIRST loan WHERE RECID(loan) EQ iLoan NO-LOCK NO-ERROR.
      /* ��।������ �ந�������� �� ������� ������ */
      RUN chk_close IN h_dpspc (op.op-kind,
                                loan.Class-Code,
                                loan.loan-status,
                                OUTPUT fl-close).
      /* �����⨥ - ��� */
      IF fl-close EQ NO THEN LEAVE PROC.

               /* ����஫� �믮������ ��ਮ��筮�� ���������� - 
               ** �஢������, �� �� ������ ����� ����� � �祭�� 10 ����楢
               ** �뫨 ���������� */

      mBegDate = loan.open-date.
      /* ��� ��砫� ��ਮ�� �஢�ન ���������� -
      ** ����� ᫥���騩 � ���� ������ ������ */
      mBegDate = GoMonth(mBegDate,1).
      mBegDate = DATE(MONTH(mBegDate), 1, YEAR(mBegDate)).
      /* �஢�ઠ �������� � �祭�� ������� �� 10 ��᫥����� ����楢 */
      MONTHS:
      DO mMonths = 1 TO 10:
         RUN get-last-dps-move(iLoan,                 /* RECID ������ */
                               mBegDate,               /* ��ࢮ� �᫮ ����� */
                               LastMonDate(mBegDate),  /* ��᫥���� �᫮ ����� */
                               OUTPUT mMoveDate).      /* ��� �������� �� ���� � ���ࢠ�� ��� */
         /* �᫨ �� �뫮 �������� */
         IF mMoveDate EQ ? THEN DO:
            /* �����頥� 䫠� ⮣�, �� ��ਮ��筮��� ���������� ����襭� */
            fl-NoMove = YES.
            LEAVE MONTHS.
         END.
         /* ᫥���騩 ����� */
         mBegDate = GoMonth(mBegDate,1).
      END.

      /* �᫨ ����஫� �믮������ ��ਮ��筮�� ���������� �� ��襫
      ** �����頥� ��� ���������� �⠢�� */
      IF fl-NoMove THEN oComm = mDComm.
   END.
   RETURN.
END PROCEDURE.

PROCEDURE stav_solid:
   DEF INPUT  PARAM iLoan     AS RECID       NO-UNDO.
   DEF INPUT  PARAM iDate     AS DATE        NO-UNDO.
   DEF INPUT  PARAM iDateEnd  AS DATE        NO-UNDO.   
   DEF OUTPUT PARAM oComm     AS CHARACTER   NO-UNDO.

   DEFINE VARIABLE vDateNarush AS DATE        NO-UNDO.
   DEFINE VARIABLE vCommi      AS CHARACTER   NO-UNDO.


   MAIN:
   DO ON ERROR  UNDO, LEAVE
      ON ENDKEY UNDO, LEAVE:

      /* ��।������ ���� ����襭�� �������쭮�� ���⪠ */
      RUN DateBreakMinOst IN h_dpspc (iLoan,
                                      iDateEnd,
                                      OUTPUT vDateNarush).
      IF vDateNarush NE ? THEN /* ���� ����襭�� �������쭮�� ���⪠ */
      DO:
         /* ��।������ ���䭮� �����ᨨ �� ४����� "�⠢������" */
         RUN Get_last_param IN h_dpspc (iLoan,
                                        iDate,
                                        iDateEnd,
                                        "�⠢������",
                                        OUTPUT vCommi).
         /* �᫨ ��⠭����� �ਧ��� - ���� �ᯮ��㥬 ������
         ** ������� �� ������ */
         IF vCommi EQ "!�ᯘ�����" THEN
            RUN Get_Last_Pen-Commi IN h_dpspc (iLoan,
                                               iDate,
                                               iDateEnd,
                                               OUTPUT vCommi).
      END.
   END.
   oComm = vCommi.
END PROCEDURE.


PROCEDURE stav_udob:
   DEF INPUT  PARAM iLoan     AS RECID       NO-UNDO.
   DEF INPUT  PARAM iDate     AS DATE        NO-UNDO.
   DEF INPUT  PARAM iDateEnd  AS DATE        NO-UNDO.
   DEF OUTPUT PARAM oComm     AS CHARACTER   NO-UNDO.

   DEFINE VARIABLE vAcct       AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vAcctRole   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vKodOst     AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vCommi      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vBegDate    AS DATE        NO-UNDO.
   DEFINE VARIABLE vEndDate    AS DATE        NO-UNDO.

   DEFINE BUFFER b-loan FOR loan.

   MAIN:
   DO ON ERROR  UNDO, LEAVE
      ON ENDKEY UNDO, LEAVE:

      FIND FIRST b-loan WHERE RECID(b-loan) EQ iLoan
         NO-LOCK NO-ERROR.
      IF NOT AVAIL b-loan THEN
         UNDO MAIN, LEAVE MAIN.

      /* ��।������ �᭮����� ��� �� ������ */          
      RUN GetBaseAcct IN h_dps (b-loan.contract,
                                b-loan.cont-code,
                                iDateEnd,
                                OUTPUT vAcct).
      IF NOT ({assigned vAcct}) THEN
         UNDO MAIN, LEAVE MAIN.
      /* ��।������ ஫� �᭮����� ��� */                             
      RUN GetBaseAcctRole IN h_dps (iLoan,
                                    iDateEnd,
                                    OUTPUT vAcctRole).
      IF NOT ({assigned vAcctRole}) THEN
         UNDO MAIN, LEAVE MAIN.

      /* ��।������ ���� ��.���⪠ */                                  
      RUN GetBaseKodOst IN h_dps (vAcctRole,
                                  OUTPUT vKodOst).
      IF NOT ({assigned vKodOst}) THEN
         UNDO MAIN, LEAVE MAIN.
      RUN get-beg-date-prol IN h_dpspc (RECID(b-loan),
                                        iDateEnd,
                                        OUTPUT vBegDate,
                                        OUTPUT vEndDate).
      /* �饬 ��������� ����⨥ */
      FIND FIRST kau-entry WHERE kau-entry.acct     EQ ENTRY(1, vAcct)
                             AND kau-entry.currency EQ ENTRY(2, vAcct)
                             AND kau-entry.debit 
                             AND kau-entry.op-date  GE vBegDate
                             AND kau-entry.op-date  LE iDateEnd 
                             AND kau-entry.kau      EQ b-loan.contract + "," + b-loan.cont-code + "," + vKodOst
         NO-LOCK NO-ERROR.
      IF AVAIL kau-entry THEN
      DO:
         /* ��।������ ���䭮� �����ᨨ �� ४����� "�⠢������" */
         RUN Get_last_param IN h_dpspc (iLoan,
                                        iDate,
                                        iDateEnd,
                                        "�⠢������",
                                        OUTPUT vCommi).
         /* �᫨ ��⠭����� �ਧ��� - ���� �ᯮ��㥬 ������
         ** ������� �� ������ */
         IF vCommi EQ "!�ᯘ�����" THEN
            RUN Get_Last_Pen-Commi IN h_dpspc (iLoan,
                                               iDate,
                                               iDateEnd,
                                               OUTPUT vCommi).
      END.
      oComm = vCommi.
   END.
END PROCEDURE.

PROCEDURE stav_invst:
   DEF INPUT  PARAM iLoan     AS RECID       NO-UNDO.
   DEF INPUT  PARAM iDate     AS DATE        NO-UNDO.
   DEF INPUT  PARAM iDateEnd  AS DATE        NO-UNDO.   
   DEF OUTPUT PARAM oComm     AS CHARACTER   NO-UNDO.

   DEFINE VARIABLE vTypeComm AS CHARACTER   NO-UNDO.

   RUN stav_invstType (iLoan,
                       iDate,
                       iDateEnd,
                       OUTPUT oComm,
                       OUTPUT vTypeComm).

END PROCEDURE.

PROCEDURE stav_invstType:
   DEF INPUT  PARAM iLoan     AS RECID       NO-UNDO.
   DEF INPUT  PARAM iDate     AS DATE        NO-UNDO.
   DEF INPUT  PARAM iDateEnd  AS DATE        NO-UNDO.   
   DEF OUTPUT PARAM oComm     AS CHARACTER   NO-UNDO. /* �⠢�� */
   DEF OUTPUT PARAM oTypeComm AS CHARACTER   NO-UNDO. /* ��㤠 ���� �⠢�� Pen-Commi/�⠢������ */

   DEFINE VARIABLE vDateNarush AS DATE        NO-UNDO.
   DEFINE VARIABLE vCommiShtr  AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vShtrKapOst AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vMess       AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vDatePer    AS DATE        NO-UNDO.
   DEFINE VARIABLE vEndDate    AS DATE        NO-UNDO.
   DEFINE VARIABLE vBegDate    AS DATE        NO-UNDO.
   DEFINE VARIABLE vDateNarysh AS DATE        NO-UNDO.
   DEFINE VARIABLE vBegDateSt  AS DATE        NO-UNDO.
   DEFINE VARIABLE vBegDateEnd AS DATE        NO-UNDO.
   DEFINE VARIABLE vNarysh     AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE vInvestType AS CHARACTER   NO-UNDO.
   DEFINE BUFFER b-loan FOR loan.

   MAIN:
   DO ON ERROR  UNDO, LEAVE
      ON ENDKEY UNDO, LEAVE:

      FIND FIRST b-loan WHERE RECID (b-loan) EQ iloan
         NO-LOCK NO-ERROR.
      IF NOT AVAIL b-loan THEN
         UNDO MAIN, LEAVE MAIN.

      RUN Get_last_param IN h_dpspc (iLoan,
                                     iDate,
                                     iDateEnd,
                                     "�⠢������",
                                     OUTPUT vShtrKapOst). /* �⠢�� ��� ����襭�� �� �ॢ��� ���. ��業⮢ */

      RUN Get_Last_Pen-Commi IN h_dpspc (iLoan,
                                         iDate,
                                         iDateEnd,
                                         OUTPUT vCommiShtr). /* �⠢�� ��� ����襭�� �� ������ */

      RUN get-beg-date-prol IN h_dpspc (iLoan,
                                        iDate,
                                        OUTPUT vBegDateSt,
                                        OUTPUT vBegDateEnd).

      /* �஢�ઠ ����襭�� ������ �� ���� ��ਮ� ����� ������ (� b-loan.open-date) */
      RUN DateBreakMinOstPer IN h_dpspc (iLoan,
                                         b-loan.open-date,
                                         MAX (iDateEnd,vBegDateEnd),
                                         OUTPUT vDateNarush).
      vInvestType = GetXAttrValueEx("op-kind", b-loan.op-kind, "�������", "").
      IF vDateNarush NE ? THEN
         /* ����襭�� ������ ���� � ��� ��ਮ�. */
         ASSIGN
            oComm     = vCommiShtr
            oTypeComm = "Pen-Commi"
            oComm     = vShtrKapOst      WHEN {assigned vInvestType}
            oTypeComm = "�⠢������"     WHEN {assigned vInvestType}
         .
      IF NOT {assigned oComm} THEN
      DO:
         /* ����襭�� ������ ���, �஥ਬ �ॢ�襭�� ����⨩ �㬬� ���. ��業⮢. */
         RUN GetPrichProcIz IN THIS-PROCEDURE (iLoan,
                                               b-loan.open-date,
                                               iDateEnd,
                                               OUTPUT vNarysh,
                                               OUTPUT vDateNarysh,
                                               OUTPUT vMess).
         IF vNarysh THEN
         DO:
            ASSIGN
               oComm     = vShtrKapOst
               oTypeComm = "�⠢������"
               oComm     = vCommiShtr    WHEN {assigned vInvestType}
               oTypeComm = "Pen-Commi"   WHEN {assigned vInvestType}
            .

         END.
      END. /* IF NOT {assigned oComm} THEN */
   END.
   /* oComm = vCommi. */
END PROCEDURE.

/* ���� �ॢ�襭�� �㬬� ����⨩ ��� ������1 �� ��ਮ�*/
PROCEDURE GetPrichProcIz.
   DEF INPUT  PARAM iLoanRID    AS RECID       NO-UNDO.
   DEF INPUT  PARAM iDateStart  AS DATE        NO-UNDO.
   DEF INPUT  PARAM iDateEnd    AS DATE        NO-UNDO.
   DEF OUTPUT PARAM oNarysh     AS LOGICAL     NO-UNDO. /* �뫮 �� �ॢ�襭�� */
   DEF OUTPUT PARAM oDateNarysh AS DATE        NO-UNDO. /* ��� �ॢ�襭�� */
   DEF OUTPUT PARAM oMess       AS CHAR        NO-UNDO. /* ᮮ�饭�� */


   DEFINE VARIABLE vIn-inter      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vStr-Acct      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vIn-kau        AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vKodOst        AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vI             AS INT64       NO-UNDO.
   DEFINE VARIABLE vBegDate       AS DATE        NO-UNDO.
   DEFINE VARIABLE vEndDate       AS DATE        NO-UNDO.
   DEFINE VARIABLE vSummPrichProc AS DECIMAL     NO-UNDO.
   DEFINE VARIABLE mAmtSummIz     AS DECIMAL     NO-UNDO.
   DEFINE VARIABLE vFlErr         AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE vMigrCode      AS CHARACTER   NO-UNDO. /* ����� - ����� �� ��㣮� ����/��⥬� */

   DEFINE BUFFER b-loan      FOR loan.
   DEFINE BUFFER b-acct      FOR acct.
   DEFINE BUFFER b-kau-entry FOR kau-entry.
   DEFINE BUFFER b-loan-acct FOR loan-acct.


   MAIN:
   DO ON ERROR  UNDO, LEAVE
      ON ENDKEY UNDO, LEAVE:

      FIND FIRST b-loan WHERE RECID (b-loan) EQ iLoanRid
         NO-LOCK NO-ERROR.
      IF NOT AVAIL b-loan THEN
         UNDO MAIN, LEAVE MAIN.

      RUN Get_Last_Param IN h_dpspc (RECID(b-loan),
                                     iDateEnd,
                                     iDateEnd,
                                     "����������",
                                     OUTPUT vIn-inter).

      RUN GET_acct_per IN h_dpspc (RECID (b-loan),
                                   iDateStart,
                                   iDateEnd,
                                   OUTPUT vStr-Acct). /* ���᮪ �᭮���� ��⮢ ������ */
      /* ��� �� ������ ��� */
      IF NOT {assigned vStr-acct} THEN 
      DO:
         oMess = "�� ������ ��� ����� ".
         UNDO MAIN, LEAVE MAIN.
      END.

      vMigrCode = GetXAttrValueEx("loan",
                                  SUBST("&1,&2", b-loan.contract, b-loan.cont-code),
                                  "�������",
                                  "").

      vBegDate = iDateStart.
      DO_ACCT:
      DO vI = 1 TO NUM-ENTRIES(vStr-acct) BY 2
         ON ERROR  UNDO, LEAVE
         ON ENDKEY UNDO, LEAVE:

         /* ���� ��� */
         {find-act.i
            &acct = "ENTRY(vI, vStr-acct) "
            &curr = b-loan.currency 
            &bact = b-acct 
         }
         IF NOT AVAIL b-acct THEN
            UNDO DO_ACCT, NEXT DO_ACCT.
         /* ���樠������ ��� */
         IF vI GT 1 THEN
            vBegDate = DATE(ENTRY(vI + 1,vStr-acct)).

         IF NUM-ENTRIES(vStr-acct) GE vI + 3 THEN 
            vEndDate = DATE(ENTRY(vI + 3,vStr-acct)).
         ELSE
            vEndDate = iDateEnd.   

         FIND LAST b-loan-acct WHERE b-loan-acct.contract  EQ b-loan.contract 
                                 AND b-loan-acct.cont-code EQ b-loan.cont-code 
                                 AND b-loan-acct.acct      EQ b-acct.acct 
                                 AND b-loan-acct.currency  EQ b-acct.currency
                                 AND b-loan-acct.acct-type EQ "loan-dps-t"
         NO-LOCK NO-ERROR.
         
         IF NOT AVAILABLE b-loan-acct THEN
            FIND LAST b-loan-acct WHERE b-loan-acct.contract  EQ b-loan.contract 
                                    AND b-loan-acct.cont-code EQ b-loan.cont-code 
                                    AND b-loan-acct.acct      EQ b-acct.acct 
                                    AND b-loan-acct.currency  EQ b-acct.currency
                                    AND b-loan-acct.acct-type EQ "loan-dps-p"
         NO-LOCK NO-ERROR. 

         IF AVAIL b-loan-acct THEN
         DO:
            RUN GetBaseKodOst IN h_dps (b-loan-acct.acct-type,
                                        OUTPUT vKodOst).

            vIn-kau = b-loan.contract + "," + b-loan.cont-code + "," +  vKodOst.
            /* �� ������ */
            Izyat:
            FOR EACH b-kau-entry WHERE b-kau-entry.acct     EQ b-acct.acct 
                                   AND b-kau-entry.currency EQ b-acct.currency
                                   AND b-kau-entry.kau      EQ vIn-kau 
                                   AND b-kau-entry.debit 
                                   AND b-kau-entry.op-date  GE vBegDate
                                   AND b-kau-entry.op-date  LE vEndDate 
               NO-LOCK:

               IF    CAN-FIND(op-entry OF b-kau-entry WHERE op-entry.op-cod = vIn-inter)
                  OR ({assigned vMigrCode} AND b-kau-entry.op-code EQ vIn-inter) THEN
               DO:
                  /* ��।���� �㬬� ��珐�1 �� ��ਮ� */
                  RUN Get_Interest_KauEntry /*IN h_dpspr*/ (BUFFER b-loan,
                                                            b-acct.acct,
                                                            b-acct.currency,
                                                            b-loan.contract + "," + b-loan.cont-code + ",�����1",
                                                            b-loan.open-date,
                                                            b-kau-entry.op-date,
                                                            OUTPUT vSummPrichProc,
                                                            OUTPUT vFlErr).

                  IF b-acct.currency EQ "" THEN
                     mAmtSummIz = b-kau-entry.amt-rub.
                  ELSE
                     mAmtSummIz = b-kau-entry.amt-cur. /* �㬬� ������ */

                  /* ���� �ॢ�襭�� ����⨩ ��� �㬬�� ���᫥���� ��業⮢ */
                  IF vSummPrichProc LT mAmtSummIz THEN
                  DO:
                     ASSIGN
                        oNarysh     = YES
                        oDateNarysh = b-kau-entry.op-date
                     .
                     LEAVE Izyat.
                  END.
               END. /*IF CAN-FIND(op-entry OF b-kau-entry*/
            END. /* FOR EACH b-kau-entry */
         END.
      END. /* DO vI = 1 TO NUM-ENTRIES(vStr-acct) BY 2 */
   END.
END PROCEDURE.
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='13/02/2015 08:56:36.186+04:00' */
/* $LINTUSER='BIS' */
/* $LINTMODE='1' */
/* $LINTFILE='meth-dps-stav.i' */
/*prosignHtIaX5yV5o5MVGetSVjvWw*/
/* --- meth-dps-stav.i was humbly modified by (c)blodd converter v.1.09 on 10/5/2016 2:03pm --- */
