/* ��⠢�� ��������� ���� ����
  ayv ���ࠢ���� �㭪樨 GetFirstDueDate � BKICredLimitAmt(ᬮ�� FOR EACH)
  ��-�� OR CreditRegistry ���㦠���� � ~10 ࠧ ���������
����� ��������� ���� ���� */
/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2017 �� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: pp-bki.p
      Comment: �����㬥�� ��� �।��⠢����� ���ଠ樨 �� �।�⠬ ��� ����
   Parameters: ���
         Uses:
      Used BY:
      Created: 27/04/2006 Gorm (59661)
     Modified: 07.11.2006 16:00 Daru     <comment>
     Modified: 09/12/2006 NIK ��䨫�஢����
     Modified: 23.05.2008 MUTA 0093248 �� ���� ����� �믫�� ��/��業⮢
               �᫨ �᫮��� �� ���� ���㧪� �� ������� - ������ ���� �᫮���.
     Modified: 25.07.2008 MUTA 0095193 �᫨ ����_���℠� �� �������� ��� ����� ���� ���㧪�, � ����_����ﭨ� �� ���뢠����

F BKIAccType            ��� ��� (4)
F BKIAccRelation        �⭮襭�� � ���� (5)
F BKIDateOFLstPay       ��� ��᫥���� �믫��� (7)
P BKIAccRating          ����ﭨ� ��� (8) � ��� ����ﭨ� ��� (9)
F BKICredLimitAmt       ����� �।��/ ��室��� �㬬� �।�� (11)
F BKICredBalance        ������ (12)
F BKIPastDue            ����窠 (13)
F BKINextPayment        ������騩 ���⥦ (14)
F BKICredPayFreq        ����� �믫�� (15)
F BKIMOP                �����६������� ���⥦�� (16)
F BKICollatCode         ��� ������  (18)
F BKIDatePayDue         ��� 䨭��쭮�� ���⥦� (20)
F BKIDateInterPayDue    ��� 䨭��쭮� �믫��� ��業⮢ (21)
F BKIInterPayFreq       ����� �믫�� ��業⮢ (22)
F BKIAmountOutst        ������ ������������� (25)
*/
{globals.i}
{intrface.get xclass}
{intrface.get lv}
{intrface.get loan}
{intrface.get refer}
{intrface.get i254}
{intrface.get date}
{intrface.get db2l}
{intrface.get pogcr}
{intrface.get trans}
{lshpr.pro}
{debug.equ}
{profile.def}
{pick-var.i}

{pfuncdef
   &LIBDEF        = "YES"
   &NAME          = "bki"
   &LIBNAME       = "������⥪� �㭪権 ��� �।��⠢����� ���ଠ樨 �� �।�⠬ ��� ����."
   &DESCRIPTION   = "�㭪樨 ��� �।��⠢����� ���ଠ樨 �� �।�⠬ ��� ����."
   }

DEFINE TEMP-TABLE ttLoanType NO-UNDO 
   FIELD fProd AS CHARACTER 
   FIELD fPos  AS CHARACTER 
   FIELD fProv AS CHARACTER 
   FIELD fTerm AS DECIMAL  
   FIELD fSumm AS DECIMAL 
   FIELD fVal  AS INT64
   FIELD fCode AS CHARACTER  
INDEX prim fTerm fSumm.

/*----------------------------------------------------------------------------*/
/* ���� ����� � �����䨪��� � ���� misc �� �宦�����                     */
/*----------------------------------------------------------------------------*/
FUNCTION GetCodeByMisc RETURN CHAR
    (iClass AS CHAR,   /* ����� �����䨪��� */
     iCount AS INT64,    /* � ����� ���� misc ᬮ���� */
     iVal   AS CHAR):  /* ���祭�� */

   DEF VAR vAccType AS CHAR NO-UNDO.
   DEF BUFFER bcode FOR CODE.
   {profile BK601}
      /* �饬 ������ � �����䨪��� � ᮮ�. ���� misc �� �宦�����,
      ** �஬� "*" */
   FIND FIRST bcode WHERE
              bcode.class =  iClass
          AND CAN-DO(bcode.misc[iCount],iVal)
          AND bcode.misc[iCount] <> "*"
             NO-LOCK NO-ERROR.

      /* �᫨ �� ��諨, � �饬 "*" */
   IF NOT AVAIL bcode THEN
      FIND FIRST bcode WHERE
                 bcode.class        =  iClass
             AND bcode.misc[iCount] =  "*"
                 NO-LOCK NO-ERROR.

    /* �᫨ ��諨, � ��६ ��� �� �����䨪��� */
   IF AVAIL bcode THEN vAccType = bcode.code.
   {profile BK610}
   RETURN vAccType.
END FUNCTION.
/*----------------------------------------------------------------------------*/
/* ���� ��᫥����� ��ࠬ��� �� ������ ��������                              */
/*----------------------------------------------------------------------------*/
PROCEDURE GetLastDateParam:
   DEF INPUT  PARAM iContract  AS CHAR NO-UNDO.
   DEF INPUT  PARAM iContCode  AS CHAR NO-UNDO.
   DEF INPUT  PARAM iOper      AS INT64  NO-UNDO.    /* ��� ��ࠬ��� */
   DEF INPUT  PARAM iDate      AS DATE NO-UNDO.    /* ��� ���� */
   DEF OUTPUT PARAM oLstDate   AS DATE NO-UNDO.

   DEF BUFFER bloan-int FOR loan-int.

   {profile BK151}
   oLstDate = ?.

lint1:
   FOR EACH bloan-int WHERE
            bloan-int.contract  = iContract
        AND bloan-int.cont-code = iContCode
        AND bloan-int.id-k      = iOper
        AND bloan-int.mdate    <= iDate
            NO-LOCK
       BY bloan-int.mdate DESC:

       oLstDate = bloan-int.mdate.

       LEAVE lint1.
   END.

lint2:
   FOR EACH bloan-int WHERE
            bloan-int.contract  = iContract
        AND bloan-int.cont-code = iContCode
        AND bloan-int.id-d      = iOper
        AND bloan-int.mdate    <= iDate
            NO-LOCK
         BY bloan-int.mdate DESC:

      IF oLstDate =  ?               OR
         oLstDate =  bloan-int.mdate THEN
         oLstDate = bloan-int.mdate.

      LEAVE lint2.
   END.

   {profile BK160}
END.
/*----------------------------------------------------------------------------*/
/* ���� ��᫥���� ����樨                                                   */
/*----------------------------------------------------------------------------*/
PROCEDURE GetLastDateOper:
    DEF INPUT  PARAM iContract  AS CHAR NO-UNDO.
    DEF INPUT  PARAM iContCode  AS CHAR NO-UNDO.
    DEF INPUT  PARAM iOper      AS INT64  NO-UNDO.    /* ��� ����樨 */
    DEF INPUT  PARAM iDate      AS DATE NO-UNDO.    /* ��� ���� */
    DEF OUTPUT PARAM oLstDate   AS DATE NO-UNDO.

   DEF BUFFER bloan-int FOR loan-int.
   DEFINE BUFFER b_chowhe FOR chowhe.

   oLstDate = ?.
   {profile BK161}

   FIND FIRST b_chowhe WHERE
              b_chowhe.id-op =  iOper
              NO-LOCK NO-ERROR.
   IF AVAILABLE(b_chowhe) THEN DO:
      FIND LAST bloan-int WHERE
                bloan-int.contract  =  iContract
            AND bloan-int.cont-code =  iContCode
            AND bloan-int.id-k      =  b_chowhe.id-k
            AND bloan-int.id-d      =  b_chowhe.id-d
            AND bloan-int.mdate     <= iDate
                NO-LOCK NO-ERROR.
      IF AVAILABLE(bloan-int) THEN oLstDate = bloan-int.mdate.
   END.
   {profile BK170}
END.
/*----------------------------------------------------------------------------*/
/* �����饭�� ����� ����樨 �� ID ����樨                                */
/*----------------------------------------------------------------------------*/
PROCEDURE GetSummOper.
   DEF INPUT PARAM iContract  AS CHAR NO-UNDO.
   DEF INPUT PARAM iContCode  AS CHAR NO-UNDO.
   DEF INPUT PARAM iOper      AS INT64  NO-UNDO.   /* ��� ����樨 */
   DEF INPUT PARAM iDate      AS DATE NO-UNDO.   /* ��� ���� */
   DEF OUTPUT PARAM oAmtPrm   AS DEC  NO-UNDO.
   DEF OUTPUT PARAM oAmtDb    AS DEC  NO-UNDO.   /* ����窨 */
   DEF OUTPUT PARAM oAmtCr    AS DEC  NO-UNDO.   /* ����窨 */

   DEFINE BUFFER b_chowhe FOR chowhe.
   DEFINE BUFFER b_lint   FOR loan-int.
   {profile BK171}
   ASSIGN
      oAmtPrm = 0.00
      oAmtDb  = 0.00
      oAmtCr  = 0.00
   .

   FIND FIRST b_chowhe WHERE
              b_chowhe.id-op =  iOper
              NO-LOCK NO-ERROR.

   IF AVAIL b_chowhe THEN
      FOR EACH b_lint WHERE
               b_lint.contract  =  iContract     AND
               b_lint.cont-code =  iContCode     AND
               b_lint.id-d      =  b_chowhe.id-d AND
               b_lint.id-k      =  b_chowhe.id-k AND
               b_lint.mdate     <= iDate
               NO-LOCK:
         oAmtPrm = oAmtPrm + b_lint.amt-rub.
      END.

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p("GetSummOper","iContract = " + iContract
                                 + " iContCode = " + iContCode
                                 + " iOper = " + string(iOper)
                                 + " iDate = " +
                            (IF iDate <> ? THEN STRING(iDate) ELSE "    ")
                                 + " oAmtPrm = " + STRING(oAmtPrm)).

   &ENDIF
   {profile BK180}
END.
/*----------------------------------------------------------------------------*/
/* ��� ��ࢮ�� ��।���� �뭮� �� ������                                */
/*----------------------------------------------------------------------------*/
PROCEDURE GetPastDate:
   DEF INPUT PARAM iContract AS CHAR NO-UNDO.
   DEF INPUT PARAM iContCode AS CHAR NO-UNDO.
   DEF INPUT PARAM iDate     AS DATE NO-UNDO.    /* ��� ����               */
   DEF INPUT PARAM iDatePrev AS DATE NO-UNDO.    /* �।���� ��� ���㧪�  */
   DEF OUTPUT PARAM oPstDate AS DATE NO-UNDO.

   DEF BUFFER bloan-int FOR loan-int.
   DEF BUFFER bop       FOR op.

   oPstDate = ?.
   {profile BK181}
lint:
   FOR EACH bloan-int WHERE
            bloan-int.contract  =  iContract
        AND bloan-int.cont-code =  iContCode
        AND bloan-int.id-d      =  7
        AND (bloan-int.id-k     =  0 OR
             bloan-int.id-k     =  13)
        AND bloan-int.mdate     <= iDate
        AND bloan-int.mdate     >  iDatePrev
            NO-LOCK
         BY bloan-int.mdate:

       FIND FIRST bop WHERE
                  bop.op =  bloan-int.op
                  NO-LOCK NO-ERROR.
       IF AVAIL bop THEN DO:
          oPstDate = bop.op-date.
          LEAVE lint.
       END.
   END.
   {profile BK190}
END.

/*----------------------------------------------------------------------------*/
/* ���᫥��� �㬬� ����権 �� ᯨ�� ��ࠬ��஢                             */
/*----------------------------------------------------------------------------*/

{pfuncdef
   &DEFPROC="ListGetSummByIntVar"
   &DESCRIPTION="���᫥��� �㬬� ����権 �� ᯨ�� ��ࠬ��஢"
   &PARAMETERS="���� �������,���,��� ��ࠬ��஢"
   &RESULT=" "
   &SAMPLE="ListGetSummByIntVar(buffer loan,iDate,"�����")"}
FUNCTION ListGetSummByIntVar RETURNS DECIMAL
    (BUFFER vLoan FOR loan,  /* �।��� ������� */
     iDate     AS DATE,     /* ��� ���㧪� */
     iListOper AS CHAR):    /* ᯨ᮪ ��ࠬ��஢ �����䨪��� ����_������� */

   DEF VAR vAmtCur   AS DEC  NO-UNDO. /* �㬬� �� ����� ����樨 */
   DEF VAR vAmt      AS DEC  NO-UNDO. /* ���� �㬬� */
   DEF VAR vI        AS INT64  NO-UNDO. /* ���稪 */

   {profile BK241}
   ASSIGN vAmt      = 0.00.

   DO vI = 1 TO NUM-ENTRIES(iListOper):
       vAmtCur = 0.
       RUN GetSummByIntVar IN THIS-PROCEDURE (BUFFER vLoan,
                           iDate,
                           ENTRY(vI,iListOper),
                           OUTPUT vAmtCur).
      vAmt = vAmt + vAmtCur.
   END.
   {profile BK250}
   RETURN vAmt.
END FUNCTION.

/*----------------------------------------------------------------------------*/
/* ���᫥��� �㬬� �� ������ � ���⪠� �� ��ࠬ��ࠬ                     */
/*----------------------------------------------------------------------------*/
PROCEDURE GetSummByIntVar:
   DEF PARAM  BUFFER loan FOR loan.               /* �।��� �������        */
   DEF INPUT  PARAM  iDate AS DATE      NO-UNDO.  /* ��� ����              */
   DEF INPUT  PARAM  iCode AS CHARACTER NO-UNDO.  /* ��� - �� ����塞      */
   DEF OUTPUT PARAM  oAmt  AS DECIMAL   NO-UNDO.  /* �㬬� �� ������ � ��ࠬ��ࠬ */

   DEF VAR vI       AS INT64    NO-UNDO.
   DEF VAR vAmtCur  AS DECIMAL    NO-UNDO.
   DEF VAR vAmtDb   AS DECIMAL    NO-UNDO.
   DEF VAR vAmtCr   AS DECIMAL    NO-UNDO.
   DEF VAR vContract AS CHARACTER NO-UNDO.
   DEF VAR vContCode AS CHARACTER NO-UNDO.

   DEF BUFFER bcode FOR CODE.
   DEF BUFFER bloan FOR loan.

   {profile BK191}
   IF NOT AVAIL loan THEN RETURN ERROR "�訡�� ���᪠ �������.".

   IF NOT GetCodeBuff("����_�������",iCode, BUFFER bcode)
       THEN RETURN ERROR "� �����䨪��� ����_������� ��� ��ࠬ��� " + iCode.

   ASSIGN
      oAmt      = 0.00
      vContract = loan.contract
      vContCode = loan.cont-code
   .

   /* ᯨ᮪ ����権, ����� �㦭� ᫮���� */
   IF bcode.misc[1] > "" THEN
   DO vI = 1 TO NUM-ENTRIES(bcode.misc[1]):
      RUN GetSummUnv(vContract,
                     vContCode,
                     INT64(ENTRY(vI,bcode.misc[1])),
                     iDate,
                     "GetSummOper",
                     OUTPUT vAmtCur,
                     OUTPUT vAmtDb,
                     OUTPUT vAmtCr).
      oAmt = oAmt + vAmtCur.
   END.

  /* ᯨ᮪ ����権, ����� �㦭� ������ */
   IF bcode.misc[2] > "" THEN
   DO vI = 1 TO NUM-ENTRIES(bcode.misc[2]):
      RUN GetSummUnv(vContract,
                     vContCode,
                     INT64(ENTRY(vI,bcode.misc[2])),
                     iDate,
                     "GetSummOper",
                     OUTPUT vAmtCur,
                     OUTPUT vAmtDb,
                     OUTPUT vAmtCr).
      oAmt = oAmt - vAmtCur.
   END.

  /* ᯨ᮪ ��ࠬ��஢, ���⪨ �� ����� �㦭� ᫮���� */
   IF bcode.misc[3] > "" THEN
   DO vI = 1 TO NUM-ENTRIES(bcode.misc[3]):
      vAmtCur = 0.00.
      RUN GetSummUnv(vContract,
                     vContCode,
                     INT64(ENTRY(vI,bcode.misc[3])),
                     iDate,
                     "GetSummPar",
                     OUTPUT vAmtCur,
                     OUTPUT vAmtDb,
                     OUTPUT vAmtCr).
      oAmt = oAmt + vAmtCur.
   END.

  /* ᯨ᮪ ��ࠬ��஢, ���⪨ �� ����� �㦭� ������ */
   IF bcode.misc[4] > "" THEN
   DO vI = 1 TO NUM-ENTRIES(bcode.misc[4]):
      vAmtCur = 0.00.
      RUN GetSummUnv(vContract,
                     vContCode,
                     INT64(ENTRY(vI,bcode.misc[4])),
                     iDate,
                     "GetSummPar",
                     OUTPUT vAmtCur,
                     OUTPUT vAmtDb,
                     OUTPUT vAmtCr).
      oAmt = oAmt - vAmtCur.
   END.

  /* �㬬��㥬 ��業�� �� Interest */
   IF bcode.misc[5] =  "��" THEN
   DO:
      DO vI = 1 TO EXTENT(pick-var):
         vAmtCur = GetLoanProcent(BUFFER loan, vI).
         oAmt = oAmt + vAmtCur.
         /* �� �ᥬ �࠭蠬 */
         FOR EACH bloan WHERE bloan.contract  =  loan.contract
                          AND bloan.cont-code BEGINS ENTRY(1,loan.cont-code," ") + " "
                          AND NUM-ENTRIES(bloan.cont-code," ") >  1
         NO-LOCK:            
            vAmtCur = GetLoanProcent(BUFFER bloan, vI).
            oAmt = oAmt + vAmtCur.
         END.
      END.
      /* ���⠥� ����, ��⥭�� � Interest */
      vAmtCur = ListGetSummByIntVar(BUFFER loan,
                                   iDate,
                                   "���,���").
      oAmt = oAmt - vAmtCur.
   END.
   {profile BK200}
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* ���᫥��� ���� �� ������ � ���⪠� �� ��ࠬ��ࠬ                      */
/*----------------------------------------------------------------------------*/
PROCEDURE GetDateByIntVar:
   DEF INPUT PARAM  iContract AS CHAR NO-UNDO.
   DEF INPUT PARAM  iContCode AS CHAR NO-UNDO.
   DEF INPUT PARAM  iDate     AS DATE NO-UNDO.
   DEF INPUT PARAM  iCode     AS CHAR NO-UNDO.   /* ��� - �� ����塞       */
   DEF OUTPUT PARAM oDate     AS DATE NO-UNDO.

   DEF VAR vI       AS INT64    NO-UNDO.
   DEF VAR vDateF   AS DATE   NO-UNDO.           /* ��� ��� ⥪�饩 ����樨 */
   DEF VAR vDateM   AS DATE   NO-UNDO.           /* ���ᨬ��쭠� ���         */

   DEF BUFFER bcode FOR CODE.

   {profile BK201}
   IF NOT GetCodeBuff("����_�������",iCode, BUFFER bcode) THEN
      RETURN ERROR "� �����䨪��� ����_������� ��� ��ࠬ��� " + iCode.

   ASSIGN
      vDateM = ?.

   /* ᯨ᮪ ����権, ����� �㦭� ᫮���� */
   DO vI = 1 TO NUM-ENTRIES(bcode.misc[1]):
      RUN GetLastDateUnv(iContract,
                         iContCode,
                         INT64(ENTRY(vI,bcode.misc[1])),
                         iDate,
                         "GetLastDateOper",
                          OUTPUT vDateF).

      IF vDateF <> ?     AND
         vDateM <> ?     AND
         vDateM < vDateF THEN
         vDateM = vDateF.
      ELSE IF vDateM = ? THEN
         vDateM = vDateF.
   END.

  /* ᯨ᮪ ��ࠬ��஢, ����� �㦭� ᫮���� */
   DO vI = 1 TO NUM-ENTRIES(bcode.misc[3]):
      RUN GetLastDateUnv(iContract,
                         iContCode,
                         INT64(ENTRY(vI,bcode.misc[3])),
                         iDate,
                         "GetLastDateParam",
                         OUTPUT vDateF).

      IF vDateF <> ?     AND
         vDateM <> ?     AND
         vDateM < vDateF THEN
         vDateM = vDateF.
       ELSE IF vDateM = ? THEN
         vDateM = vDateF.
   END.

   oDate = vDateM.
   {profile BK210}
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* ����� ���������� �� ��� ���ᯥ祭��                                     */
/*----------------------------------------------------------------------------*/
PROCEDURE PayByOB:
    DEF INPUT PARAM  iContract AS CHAR NO-UNDO.
    DEF INPUT PARAM  iContCode AS CHAR NO-UNDO.
    DEF INPUT PARAM  iDate     AS DATE NO-UNDO.
    DEF INPUT PARAM  iDatePrev AS DATE NO-UNDO.            /* ����窠         */
    DEF OUTPUT PARAM oDateOD   AS DATE NO-UNDO.

    DEF VAR vXattrOb AS CHAR NO-UNDO.
    DEF BUFFER bloan-int FOR loan-int.
    DEF BUFFER bop FOR op.

   {profile BK211}

   oDateOD = ?.

    /* �� �ᥬ ������ �᭮����� ����� */
bl:
   FOR EACH   bloan-int WHERE
              bloan-int.contract  =  iContract
        AND   bloan-int.cont-code =  iContCode
        AND ((bloan-int.id-d     =  1
        AND   bloan-int.id-k     =  2)
          OR (bloan-int.id-d     =  5
        AND   bloan-int.id-k     =  7)) 
        AND   bloan-int.mdate     <= iDate
            NO-LOCK:

        /* ��������㥬 ���㬥�� ������ */
      vXattrOb  = GetXattrValueEx("op",
                                  STRING(bloan-int.op),
                                  "���⎡��",
                                  "").

      IF vXattrOb = "��" THEN DO:
         FIND FIRST bop WHERE
                    bop.op = bloan-int.op
                    NO-LOCK NO-ERROR.
         IF AVAIL bop THEN
            ASSIGN oDateOD = bop.op-date.
         LEAVE bl.
      END.
   END.
   {profile BK220}
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* ��।����, �� �� ᯮ� �� ��।������� ���� � �����頥� ���� ��� 䨪�樨*/
/*----------------------------------------------------------------------------*/
PROCEDURE GetDisput:
   DEF INPUT PARAM iContract  AS CHAR NO-UNDO.
   DEF INPUT PARAM iContCode  AS CHAR NO-UNDO.
   DEF INPUT PARAM iDate      AS DATE NO-UNDO.  /* ��� ���㧪�              */
   DEF INPUT PARAM iDatePrev  AS DATE NO-UNDO.  /* �।���� ��� ���㧪�   */
   DEF OUTPUT PARAM oDateDisp AS DATE NO-UNDO.

   DEF BUFFER bterm-obl FOR term-obl.
   {profile BK221}
    /* �饬 ��᫥���� ᯮ� */
   FIND LAST bterm-obl WHERE
             bterm-obl.contract  = iContract
         AND bterm-obl.cont-code = iContCode
         AND bterm-obl.idnt      = 7
         AND bterm-obl.end-date <= iDate
             NO-LOCK NO-ERROR.

    /* �᫨ ��諨 ᯮ�, �஢��塞 �� �� ������ ����� ��᫥���� ���� ���㧪� */
   oDateDisp = IF AVAIL bterm-obl AND bterm-obl.end-date > iDatePrev
                  THEN bterm-obl.end-date
                  ELSE ?.
   {profile BK230}
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* ��।���� ��᫥���� ���� �뭮� �� ������ ��/��業⮢                 */
/*----------------------------------------------------------------------------*/
PROCEDURE GetLastDueDate:
   DEF INPUT PARAM  iContract  AS CHAR NO-UNDO.
   DEF INPUT PARAM  iContCode  AS CHAR NO-UNDO.
   DEF INPUT PARAM  iDate      AS DATE NO-UNDO. /* ��� ���㧪� */
   DEF OUTPUT PARAM oDate      AS DATE NO-UNDO. /* ��᫥���� ��� �뭮� �� ������ */

   DEF VAR vListOper AS CHAR NO-UNDO.
   DEF VAR vI        AS INT64  NO-UNDO.
   DEF VAR vDateF    AS DATE NO-UNDO.
   DEF VAR vDateM    AS DATE NO-UNDO.

   ASSIGN
      vDateM = ?
      vListOper = "��������,��������,��㯏�,��㯏��".

   {profile BK231}
   DO vI = 1 TO NUM-ENTRIES(vListOper):

      RUN GetDateByIntVar(iContract,
                          iContCode,
                          iDate,
                          ENTRY(vI,vListOper),
                          OUTPUT vDateF).
      IF vDateF <> ? AND
         vDateM <> ? AND
         vDateM < vDateF
         THEN vDateM = vDAteF.
      ELSE IF vDateM = ? THEN vDateM = vDateF.
   END.

   oDate = vDateM.
   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p("GetLastDueDate","iContract = " + iContract       +
                                  " iContCode = " + iContCode       +
                                      " iDate = " + STRING(iDate)   +
                                     " vDateM = " + STRING(vDateM)).
   &ENDIF
   {profile BK240}
END.
/*----------------------------------------------------------------------------*/
/* ��।���� ���� ������������� ������祭��� ����窨 ��/��業⮢          */
/*----------------------------------------------------------------------------*/
PROCEDURE GetFirstDueDate:
   DEF INPUT PARAM  iContract  AS CHAR NO-UNDO. /* �����祭�� ������� */
   DEF INPUT PARAM  iContCode  AS CHAR NO-UNDO. /* ����� ������� */
   DEF INPUT PARAM  iDate      AS DATE NO-UNDO. /* ��� ���㧪� */
   DEF OUTPUT PARAM oDate      AS DATE NO-UNDO. /* ��� �뭮� �� ������ */

   DEF VAR vListOper  AS CHAR NO-UNDO.          /* ���᮪ ����権 �뭮� �� ������ */
   DEF VAR vI         AS INT64  NO-UNDO.          /* ���稪 */
   DEF VAR vJ         AS INT64  NO-UNDO.          /* ���稪 */
   DEF VAR vDateF     AS DATE NO-UNDO.          /* �஬������ १����� */
   DEF VAR vDateM     AS DATE NO-UNDO.          /* �஬������ १����� */
   DEF VAR vAmnt      AS DEC  NO-UNDO.          /* ���줮 */

   DEF BUFFER bcode     FOR code.
   DEF BUFFER bloan-int FOR loan-int.

   ASSIGN
      vDateM    = ?
      vListOper = "��������,��������,����몏�,����몏�"
   .

   {profile BK231}
   DO vJ = 1 TO NUM-ENTRIES(vListOper):

      IF NOT GetCodeBuff("����_�������", ENTRY(vJ, vListOper), BUFFER bcode) THEN
         RETURN ERROR "� �����䨪��� ����_������� ��� ��ࠬ��� " + ENTRY(vJ, vListOper).

         /* �� ������ ��� ���������� ��।����� ���⮪,
         ** ��।��塞 ᯨ᮪ ��ࠬ��஢, �� ����� �㦭� ���� ���� */
      DO vI = 1 TO NUM-ENTRIES(bcode.misc[3]):
         ASSIGN
            vAmnt  = 0
            vDateF = ?
         .
            /* �� �������� � �࠭蠬 */
         FOR EACH bloan-int WHERE
                 (bloan-int.contract  =  iContract
            AND   bloan-int.cont-code =  iContCode
            AND   bloan-int.mdate     <= iDate)
/* ������� ���� ����
              OR (bloan-int.contract  =  iContract
            AND   bloan-int.cont-code BEGINS iContCode + " "
            AND   bloan-int.mdate     <= iDate)
����� 㤠������� �ࠣ���� ���� ���� */
         NO-LOCK BY bloan-int.mdate:
            IF    bloan-int.id-k =  INT64(ENTRY(vI, bcode.misc[3]))
               OR bloan-int.id-d =  INT64(ENTRY(vI, bcode.misc[3])) THEN
            DO:
               IF bloan-int.id-k =  INT64(ENTRY(vI, bcode.misc[3])) THEN
                  vAmnt  = vAmnt - bloan-int.amt-rub.
               ELSE
                  vAmnt  = vAmnt + bloan-int.amt-rub.
               IF vDateF =  ? THEN
                  vDateF = bloan-int.mdate.
               IF vAmnt =  0 THEN
                  vDateF = ?.
            END.
         END.
         IF vDateF <> ? THEN
            IF vDateM =  ? THEN
               vDateM = vDateF.
            ELSE IF vDateM > vDateF THEN
               vDateM = vDateF.
/* ��⠢�� ���� ���� */
         ASSIGN
            vAmnt  = 0
            vDateF = ?
         .
         FOR EACH bloan-int WHERE
                 /* (bloan-int.contract  =  iContract
            AND   bloan-int.cont-code =  iContCode
            AND   bloan-int.mdate     <= iDate)
              OR */ (bloan-int.contract  =  iContract
            AND   bloan-int.cont-code BEGINS iContCode + " "
            AND   bloan-int.mdate     <= iDate)
         NO-LOCK BY bloan-int.mdate:
            IF    bloan-int.id-k =  INT64(ENTRY(vI, bcode.misc[3]))
               OR bloan-int.id-d =  INT64(ENTRY(vI, bcode.misc[3])) THEN
            DO:
               IF bloan-int.id-k =  INT64(ENTRY(vI, bcode.misc[3])) THEN
                  vAmnt  = vAmnt - bloan-int.amt-rub.
               ELSE
                  vAmnt  = vAmnt + bloan-int.amt-rub.
               IF vDateF =  ? THEN
                  vDateF = bloan-int.mdate.
               IF vAmnt =  0 THEN
                  vDateF = ?.
            END.
         END.
         IF vDateF <> ? THEN
            IF vDateM =  ? THEN
               vDateM = vDateF.
            ELSE IF vDateM > vDateF THEN
               vDateM = vDateF.
/* ����� ��⠢�� ���� ���� */
      END.
   END.
   oDate = vDateM.

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p("GetFirstDueDate", "iContract = " + iContract      +
                                     "iContCode = " + iContCode      +
                                     "iDate     = " + STRING(iDate)  +
                                     "vDateM    = " + STRING(vDateM)).
   &ENDIF
   {profile BK240}
END.
/*----------------------------------------------------------------------------*/
/* 4. ��� ���                                                               */
/*----------------------------------------------------------------------------*/
FUNCTION BKIAccType RETURNS CHAR
   (BUFFER loan FOR loan): /* �।��� ������� */

   DEF VAR vAccType AS CHAR NO-UNDO.
   DEF VAR vI AS INT64 NO-UNDO.

   DEF BUFFER bcode FOR CODE.
   {profile BK251}
   IF NOT AVAIL loan THEN RETURN "ERROR".

   vAccType = GetXattrValueEx("loan",
                              loan.contract + "," + loan.cont-code,
                              "����_������",
                              "").
   /* �᫨ ���. ४����� �� �����, � ����塞 ��� */
   IF vAccType = "" THEN DO:
      /* ��� ������ 䨪�஢���� ��� � �����䨪��� */
      IF loan.cust-cat = "�" THEN vAccType = "15".

      /* ��� �ਤ��᪨� � 䨧��᪨� ��� - ����塞 */
      ELSE DO:
/* ��⠢�� ���� ���� */
         DEF VAR vLnType  AS CHAR NO-UNDO.
         DEF BUFFER bloan FOR loan.

         vLnType = loan.cont-type.
         IF (vLnType EQ "��祭��")
         THEN DO:
            FIND LAST bloan
                WHERE (bloan.contract   EQ loan.contract)
                  AND (bloan.cont-code  BEGINS loan.cont-code)
                  AND (NUM-ENTRIES(bloan.cont-code, " ") EQ 2)
                NO-LOCK NO-ERROR.
            IF (AVAIL bloan)
            THEN vLnType = bloan.cont-type.
            ELSE vLnType = GetXattrValue("loan", loan.contract + "," + loan.cont-code, "����������").
         END.
         IF (vLnType EQ "") THEN RETURN (IF (loan.cust-cat = "�") THEN "9" ELSE (IF (loan.cust-cat = "�") THEN "10" ELSE "99")).
/* ����� ��⠢�� ���� ���� */
         IF      loan.cust-cat = "�" THEN vI = 1.
         ELSE IF loan.cust-cat = "�" THEN vI = 2.
         ELSE RETURN "".

          vAccType = GetCodeByMisc("����_������",
                                   vI,
/* ������ ���� ����
                                   loan.cont-type).
*/                                 vLnType).
/* ����� ������ ���� ���� */
      END.
   END.
   {profile BK260}
/* ������ ���� ����
   RETURN vAccType.
*/ RETURN IF (vAccType = "") THEN (IF (loan.cust-cat = "�") THEN "9" ELSE (IF (loan.cust-cat = "�") THEN "10" ELSE "99")) ELSE vAccType.
/* ����� ������ ���� ���� */
END FUNCTION.
/*----------------------------------------------------------------------------*/
/* 5. �⭮襭�� � ����                                                       */
/*----------------------------------------------------------------------------*/
FUNCTION BKIAccRelation RETURNS CHAR
   (BUFFER loan     FOR loan,           /* �।��� ������� */
    BUFFER term-obl FOR term-obl,       /* ������� ���ᯥ祭�� */
    iCustSurr       AS CHAR, 
    iCorp           AS LOGICAL ):       /* ���� �� �����*/

   DEF VAR vAccRel     AS CHAR NO-UNDO.
   DEF VAR vFlPricipal AS LOG  NO-UNDO.
   DEF VAR vVersion    AS DEC  NO-UNDO.

   &IF DEFINED(IS-DEBUG) &THEN
   IF AVAIL term-obl THEN
      RUN dbgprint.p("BKIAccRelation","loan.Contract = " + loan.Contract
                                  + " loan.Cont-Code = " + loan.Cont-Code
                                  + " ������� ���ᯥ祭�� ")).
   ELSE IF AVAIL loan THEN
      RUN dbgprint.p("BKIAccRelation","loan.Contract = " + loan.Contract
                                  + " loan.Cont-Code = " + loan.Cont-Code
                                        + " cust-cat = " + loan.cust-cat)).
   ELSE RUN dbgprint.p("BKIAccRelation","loan.Contract = " + loan.Contract
                                    + " loan.Cont-Code = " + loan.Cont-Code
                                      + " ������� �� ������ ")).
   &ENDIF

   {profile BK261}
   IF NOT AVAIL loan THEN RETURN ?.

   ASSIGN
      vFlPricipal = FGetSetting("����","�ਭ樯�������","��") =  "��"
      vVersion    = DEC(GetAttrValue("",0,"NBKIVersion")) NO-ERROR.

   /* �᫨ �ਭ樯��� ��६ � ������஢ ��࠭⨩, �뤠���� ������ */
   IF     NOT vFlPricipal
      AND CAN-DO(GetXclassAllChildsEx("loan-guarantee"),loan.class-code)
      AND NOT AVAIL term-obl 
      AND vVersion >= 3.00 THEN
      RETURN "6".
   /* �᫨ ��।��� ������� ���ᯥ祭�� � �ਭ樯�� � ���ᯥ祭�� */
   ELSE IF AVAIL term-obl THEN 
   DO:
      /*��।��塞, �ਭ樯�� ��� �����⥫� */
      IF iCustSurr =  term-obl.symbol + "," + STRING(term-obl.fop) THEN 
         RETURN "5".
      ELSE
         RETURN "6".
   END.

   IF loan.cust-cat = "�" OR loan.cust-cat = "�" OR iCorp THEN vAccRel = "9".
   ELSE vAccRel = "1".

   IF FGetSetting("����","������騪","") =  "��"
      AND NOT AVAIL term-obl 
      AND iCustSurr <> loan.cust-cat + "," + STRING(loan.cust-id)
   THEN vAccRel = "4".

   {profile BK270}
   RETURN vAccRel.
END FUNCTION.
/*----------------------------------------------------------------------------*/
/* 7. ��� ��᫥���� �믫���                                                  */
/*----------------------------------------------------------------------------*/
FUNCTION BKIDateOFLstPay RETURNS DATE
   (iContract AS CHAR,
    iContCode AS CHAR,            /* �।��� ������� */
    iDate AS DATE):               /* ��� ���㧪� */

   DEF VAR vDateM AS DATE NO-UNDO. /* ��� ��᫥���� ���㧪� */
   {profile BK271}

   ASSIGN
      vDateM = ?
      .

   RUN GetDateByIntVar(iContract,
                       iContCode,
                       iDate,
                       "����莡�",
                       OUTPUT vDateM).
   &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p("BKIDateOFLstPay","iContract = " + iContract
                                       + " iContCode = " + iContCode
                                       + " vDateM = " + STRING(vDateM)).

   &ENDIF
   {profile BK280}
   RETURN vDateM.

END FUNCTION.
/*----------------------------------------------------------------------------*/
/* 13. ����窠                                                              */
/*----------------------------------------------------------------------------*/
FUNCTION BKIPastDue RETURNS DECIMAL
    (BUFFER loan     FOR loan,                   /* �।��� �������         */
            iDate    AS DATE,                    /* ��� ���㧪�             */
            iDatePay AS DATE):                   /* ��� ��᫥���� �믫���    */

   DEF VAR mAmt AS DECIMAL NO-UNDO.

   {profile BK281}
   IF NOT AVAIL loan THEN RETURN ?.
   mAmt = ListGetSummByIntVar(BUFFER loan,
                              iDate,
                              "��������,��������,���,���,����몏�,����몏�").
   {profile BK290}
   RETURN mAmt.
END FUNCTION.
/*----------------------------------------------------------------------------*/
/* 8. ����ﭨ� ���, 9. ��� ����ﭨ� ���                                */
/*----------------------------------------------------------------------------*/
PROCEDURE BKIAccRating:
   DEF PARAM BUFFER loan FOR loan.                 /* �।��� ������� */
   DEF INPUT PARAM iDate        AS DATE NO-UNDO.   /* ��� ����       */
   DEF INPUT PARAM iDatePrev    AS DATE NO-UNDO.   /* ��� �।��饩 ���㧪� */
   DEF INPUT PARAM iDatePay     AS DATE NO-UNDO.   /* ��� ��᫥���� �믫��� */
   DEF OUTPUT PARAM oAccRat     AS CHAR NO-UNDO.   /* ���ﭨ� ��� */
   DEF OUTPUT PARAM oDateAccRat AS DATE NO-UNDO.   /* ��� ���ﭨ� ��� */

   DEF VAR vAccRat   AS CHAR NO-UNDO.
   DEF VAR vAmtOst   AS DEC  NO-UNDO.
   DEF VAR vAmtCur   AS DEC  NO-UNDO.
   DEF VAR vOD       AS LOG  NO-UNDO.
   DEF VAR vListOper AS CHAR NO-UNDO.
   DEF VAR vDatePay  AS DATE NO-UNDO.
   DEF VAR vI        AS INT64  NO-UNDO.
   DEF VAR vDateOD   AS DATE NO-UNDO.
   DEF VAR vDateDisp AS DATE NO-UNDO.
   DEF VAR vDateSost  AS DATE NO-UNDO.

   DEF BUFFER signs FOR signs.
   DEF BUFFER bloan FOR loan.

   {profile BK291}

   IF NOT AVAIL loan THEN RETURN.
   vAmtOst      = 0.00.

   /* �᫨ ��� �।��饩 ���㧪� �� ������ (��ࢠ� ���㧪�),
   ** � ��६ ���� ������ ������� */
   IF iDatePrev = ? THEN iDatePrev = loan.open-date.

   vAccRat = GetXattrValueEx("loan",
                              loan.contract + "," + loan.cont-code,
                              "����_����ﭨ�",
                              "").

   vDateSost = DATE(GetXattrValue("loan",
                                   loan.contract + "," + loan.cont-code,
                                   "����_���℠�")) NO-ERROR.

   IF vDateSost =  ? OR vDateSost > iDate THEN vAccRat = "".

   /* ��।��塞 ���� ᯮ� */
   RUN GetDisput(loan.contract,
                 loan.cont-code,
                 iDate,
                 iDatePrev,
                 OUTPUT vDateDisp).

   /* �᫨ ���. ४����� �� �����, � ����塞 ��� */
   IF vAccRat = "" THEN DO:

      /* ����塞 �㬬� ���������� */
      RUN GetSummByIntVar(BUFFER loan,
                          iDate,
                          "��������",
                          OUTPUT vAmtOst).

      /* �᫨ ������������� �㫥��� �� ��, � �饬 ���㬥��
      ** � ����⪮� "���⎡��" */
      IF vAmtOst = 0.00
         THEN RUN GetFirstDateUnv(loan.contract,
                                  loan.cont-code,
                                  iDatePay,
                                  ?,
                                  "PayByOB",
                                  OUTPUT vDateOD).

      /* ����祭 */
      IF ListGetSummByIntVar(BUFFER loan,
                             iDate,
                             "��������,��������,����몏�,����몏�") > 0.00
          THEN DO:
          oAccRat = "52".
          RUN GetFirstDueDate(loan.contract,
                              loan.cont-code,
                              iDate,
                              OUTPUT oDateAccRat).
      END.

      /* ��।�� �� ���㦨����� � ����� �࣠������ */
      ELSE IF CAN-FIND (FIRST signs WHERE signs.file-name  =  "loan"
                                      AND signs.surrogate  BEGINS "�।��,"
                                      AND signs.code       =  "����"
                                      AND signs.code-value =  loan.cont-code) THEN
      DO:
         FOR FIRST signs WHERE signs.file-name  =  "loan"
                           AND signs.surrogate  BEGINS "�।��,"
                           AND signs.code       =  "����"
                           AND signs.code-value =  loan.cont-code
             NO-LOCK,
             FIRST bloan WHERE bloan.contract  =  ENTRY(1,signs.surrogate)
                           AND bloan.cont-code =  ENTRY(2,signs.surrogate)
             NO-LOCK:
             ASSIGN
                oAccRat     = "14"  
                oDateAccRat = bloan.open-date.  
         END.   
      END.

      /* ���� ᯮ� */
      ELSE IF vDateDisp <> ?
              THEN ASSIGN vAccRat = "21"
                          oDateAccRat = vDateDisp.
      /* ������ */
      ELSE IF loan.close-date <= iDate
              THEN ASSIGN oAccRat = "13"
                          oDateAccRat = loan.close-date.
      /* ����祭 � ��� ���ᯥ祭�� */
      ELSE IF vAmtOst =  0.00 AND
              vDateOD <> ?
          THEN ASSIGN oAccRat = "12"
                      oDateAccRat = vDateOD.
      ELSE ASSIGN oAccRat = "00"
                  oDateAccRat = iDate.
   END.
   /* �᫨ ����� ���. ४�����, � ����塞 ⮫쪮 ���� */
   ELSE DO:
      oAccRat = vAccRat.

      CASE vAccRat:
         WHEN "12" THEN DO:
            RUN GetFirstDateUnv(loan.contract,
                                loan.cont-code,
                                iDatePay,
                                ?,
                                "PayByOB",
                                OUTPUT oDateAccRat).
         END.
         WHEN "13" THEN oDateAccRat = loan.close-date.
         WHEN "14" OR
         WHEN "70" OR
         WHEN "85" OR
         WHEN "90" OR
         WHEN "95" THEN
               oDateAccRat = DATE(GetXattrValue("loan",
                                                         loan.contract + ","
                                                         + loan.cont-code,
                                                         "����_���℠�")).
         WHEN "21" THEN oDateAccRat = vDateDisp.

         /* ��� ����窨 (�뭮�) */
         WHEN "52" THEN DO:
            RUN GetFirstDueDate(loan.contract,
                                loan.cont-code,
                                iDate,
                                OUTPUT oDateAccRat).
         END.
         /* ���� ��।��� �뭮� */
         WHEN "61" THEN RUN GetFirstDateUnv(loan.contract,
                                            loan.cont-code,
                                            iDate,
                                            iDatePrev,
                                            "GetPastDate",
                                            OUTPUT oDateAccRat).
         WHEN "00" THEN oDateAccRat = iDate.

      END CASE.
   END.
   {profile BK300}
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* 11. ����� �।��/ ��室��� �㬬� �।��                                  */
/*----------------------------------------------------------------------------*/
FUNCTION BKICredLimitAmt RETURNS DECIMAL
   (BUFFER loan FOR loan):   /* �।��� ������� */

   DEF VAR vAmt     AS DEC NO-UNDO.
   DEF VAR vAmt1    AS DEC NO-UNDO.

   DEF BUFFER bloan      FOR loan.

   {profile BK301}
   IF NOT AVAIL loan THEN RETURN ?.

   /*�஢��塞, �࠭� �� ��*/
   IF INDEX(loan.doc-ref," ") = 0 THEN
   DO:
      
      RUN MaxLim IN THIS-PROCEDURE (loan.contract,
                                    loan.cont-code,
                                    TODAY,
                                    OUTPUT vAmt).

   END.
   ELSE DO:
      /* �饬 �᭮���� ������� */
      FIND FIRST bloan WHERE
                 bloan.filial-id = loan.filial-id
             AND bloan.contract  = loan.contract
             AND bloan.doc-ref   = ENTRY(1,loan.doc-ref," ")
          NO-LOCK NO-ERROR.

      IF AVAIL bloan THEN
      DO:

         RUN MaxLim IN THIS-PROCEDURE (bloan.contract,
                                       bloan.cont-code,
                                       TODAY,
                                       OUTPUT vAmt).

      END.
      /* �᫨ �� ��諨 ⠪��� �������, � �� ����୮ �� �祭��,
      ** �����頥� ��� �㬬� */
      ELSE
      DO:
         
         RUN MaxLim IN THIS-PROCEDURE (loan.contract,
                                       loan.cont-code,
                                       TODAY,
                                       OUTPUT vAmt).
      END.
   END.
   /* ������塞 ��㬫����� ������������� (��� ��ᨩ) */
   FOR EACH bloan WHERE (bloan.contract  =  loan.contract 
                     AND bloan.cont-code =  ENTRY(1,loan.cont-code," "))
/* ������� ���� ����
                     OR (bloan.contract  =  loan.contract 
                     AND bloan.cont-code BEGINS ENTRY(1,loan.cont-code," ")
                     AND NUM-ENTRIES(bloan.cont-code, " ") >  1)
����� 㤠������� �ࠣ���� ���� ���� */
       NO-LOCK:
       vAmt1 = ListGetSummByIntVar(BUFFER bloan,
                                   bloan.open-date,
                                   "��㯏�,��㯏��").
       vAmt = vAmt + vAmt1.    
   END.
/* ��⠢�� ���� ���� */
   FOR EACH bloan WHERE /* (bloan.contract  =  loan.contract 
                     AND bloan.cont-code =  ENTRY(1,loan.cont-code," "))
                     OR */ (bloan.contract  =  loan.contract 
                     AND bloan.cont-code BEGINS ENTRY(1,loan.cont-code," ")
                     AND NUM-ENTRIES(bloan.cont-code, " ") >  1)
       NO-LOCK:
       vAmt1 = ListGetSummByIntVar(BUFFER bloan,
                                   bloan.open-date,
                                   "��㯏�,��㯏��").
       vAmt = vAmt + vAmt1.    
   END.
/* ����� ��⠢�� ���� ���� */
   {profile BK310}
   RETURN vAmt.
END FUNCTION.

PROCEDURE MaxLim PRIVATE:

   DEFINE INPUT  PARAM iContract  AS CHAR NO-UNDO.
   DEFINE INPUT  PARAM iCont_code AS CHAR NO-UNDO.
   DEFINE INPUT  PARAM iDate      AS DATE NO-UNDO.
   DEFINE OUTPUT PARAM oAmt       AS DEC  NO-UNDO.

   DEFINE BUFFER loan      FOR loan.
   DEFINE BUFFER loan-cond FOR loan-cond.

   DEF VAR vAmtCond AS DEC NO-UNDO.

   FOR EACH loan-cond WHERE
            loan-cond.contract  =  iContract
        AND loan-cond.cont-code =  iCont_code
        AND loan-cond.since     <= iDate
   NO-LOCK:
 
       RUN RE_PLAN_SUMM_BY_LOAN IN h_loan (iContract,
                                           iCont_code,
                                           loan-cond.since,
                                           OUTPUT vAmtCond).

       oAmt = MAX(vAmtCond, oAmt).


   END.

   IF oAmt =  0 THEN
   DO:
      FIND FIRST loan WHERE
                 loan.contract  =  iContract
             AND loan.cont-code =  iCont_code
      NO-LOCK NO-ERROR.

      IF AVAIL loan THEN
         RUN RE_PLAN_SUMM_BY_LOAN IN h_loan (loan.contract, 
                                             loan.cont-code,
                                             loan.open-date,
                                             OUTPUT oAmt).
   END.


END PROCEDURE.

/*----------------------------------------------------------------------------*/
/* 12. ������                                                                 */
/*----------------------------------------------------------------------------*/
FUNCTION BKICredBalance RETURN DECIMAL
   (BUFFER loan FOR loan,     /* �।��� ������� */
    iDate AS DATE,            /* ��� ���㧪� */
    iDatePay AS DATE):        /* ��� ��᫥���� �믫���
                               - १���� ࠡ��� �㭪樨 BKIDateOFLstPay (7) */

   DEF VAR vAmt      AS DEC  NO-UNDO. /* ���� �㬬� */

   {profile BK311}
   IF NOT AVAIL loan THEN RETURN ?.

   ASSIGN
      vAmt      = 0.00
      .

   IF iDatePay = ?
       THEN iDatePay = BKIDateOFLstPay(loan.contract,
                                       loan.cont-code,
                                       iDate).

   /* ���� �믫�祭��� �㬬�, ������ ��業�� � ���� */
   RUN GetSummByIntVar(BUFFER loan,
                       IF iDatePay = ? THEN iDate ELSE iDatePay,
                       "����莡�",
                       OUTPUT vAmt).

   &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p("BKICredBalance","loan.contract = " + loan.contract
                                       + " loan.cont-code = " + loan.cont-code
                                       + " iDatePay = " + STRING(iDatePay)
                                       + " vAmt = " + STRING(vAmt)).

   &ENDIF

   {profile BK320}
   RETURN vAmt.
END FUNCTION.
/*----------------------------------------------------------------------------*/
/* 25. ������ ������������                                                   */
/*----------------------------------------------------------------------------*/

FUNCTION BKIAmountOutst RETURNS DECIMAL
    (BUFFER loan FOR loan,
     iDate AS DATE,                /* ��� ���㧪� */
     iFlNext AS LOG):              /* ᫥���騩 ���⥦ ��� ���⮪ */

  DEF VAR vAmount     AS DECIMAL NO-UNDO.
  DEF VAR vDt1        AS DATE    NO-UNDO.
  DEF VAR vDt2        AS DATE    NO-UNDO.
  DEF VAR vFlMinOD    AS LOG     NO-UNDO.
  DEF VAR vAmt        AS DECIMAL NO-UNDO.

  DEF BUFFER term-obl  FOR term-obl.
  DEF BUFFER loan-cond FOR loan-cond.
  DEF BUFFER comm-rate FOR comm-rate.
  DEF BUFFER bloan     FOR loan.

  IF NOT AVAIL loan THEN RETURN ?.

  /* ��६ ⥪���� ������������� */
  vAmount = ListGetSummByIntVar(BUFFER loan, 
                                iDate,
                                "������,�����").
  /* �᫨ �����뢠�� ᫥���騩 ���⥦, � ���뢠�� %����� � ������ */
  IF iFlNext THEN
  DO:
     /* ��� ���஢ �஢��塞 %����� � ����� ����� ������ */
     vFlMinOD = CAN-DO(GetXclassAllChildsEx("l_all_over_agreement"),loan.class-code).
     IF NOT vFlMinOD THEN
        vFlMinOD = CAN-DO(GetXclassAllChildsEx("loan_trans_ov"),loan.class-code).

     /* �᫨ ��� �� ��筮�, �� %�����, � ��६ ����祭��� ������������� */
     IF   (vAmount =  0
        OR vAmount =  ?) 
        AND vFlMinOD THEN     
           vAmount = ListGetSummByIntVar(BUFFER loan,
                                         iDate,
                                         "��������,��������").
     ELSE IF vFlMinOD THEN
     DO:  
        FIND LAST comm-rate WHERE comm-rate.commission =  "%�����"
                              AND comm-rate.kau        =  loan.contract + "," + loan.cont-code
                              AND comm-rate.since      <= iDate
        NO-LOCK NO-ERROR.
        IF     AVAIL comm-rate
           AND comm-rate.rate-comm >  0 THEN
        DO:
           vAmt = ListGetSummByIntVar(BUFFER loan,
                                      iDate,
                                      "�����").
           IF vAmt >  0 THEN
              vAmount = ROUND(vAmt * comm-rate.rate-comm / 100,2).
        END.
     END.
  END.
  RETURN vAmount.

END FUNCTION.
/*----------------------------------------------------------------------------*/
/* 14. ������騩 ���⥦                                                       */
/*----------------------------------------------------------------------------*/
FUNCTION BKINextPayment RETURN DECIMAL
    (iContract AS CHAR,
     iContCode AS CHAR,              /* �।��� ������� */
     iDate AS DATE):                 /* ��� ���㧪� */

   DEFINE VARIABLE vAmt   AS DEC         NO-UNDO.
   DEFINE VARIABLE vAmt1  AS DEC         NO-UNDO.
   DEFINE VARIABLE vAmt2  AS DEC         NO-UNDO.
   DEFINE VARIABLE vDate1 AS DATE        NO-UNDO.
   DEFINE VARIABLE vDate2 AS DATE        NO-UNDO.

   DEFINE BUFFER bLoan    FOR loan.
   DEFINE BUFFER term-obl FOR term-obl.

   {profile BK321}

   FIND FIRST bloan WHERE 
              bloan.contract  =  iContract 
         AND  bloan.cont-code =  iContCode
   NO-LOCK NO-ERROR.

   IF FGetSetting("����","���������","") =  "��" THEN
   DO:

      /* �������� ��� � �㬬� ������ �᭮����� ����� */
      RUN GetFirstAmtUnv(iContract,
                         iContCode,
                         3,
                         iDate,
                         "BKINextPayment1",
                         OUTPUT vAmt1,
                         OUTPUT vDate1).

      /* �������� ��� � �㬬� ������ ��業�� */
      RUN GetFirstAmtUnv(iContract,
                         iContCode,
                         1,
                         iDate,
                         "BKINextPayment1",
                         OUTPUT vAmt2,
                         OUTPUT vDate2).

      FIND FIRST term-obl WHERE term-obl.contract  =  iContract
                            AND term-obl.cont-code =  iContCode
                            AND term-obl.idnt      =  3
                            AND term-obl.end-date  =  vDate1
      NO-LOCK NO-ERROR.
      IF AVAIL term-obl THEN 
         RUN summ-t.p (OUTPUT vAmt1,
                       bloan.Contract,
                       bloan.Cont-Code,
                       RECID(term-obl),
                       vDate1).
      ELSE
         vAmt1 = 0.
      vAmt2 = 0.
      FOR EACH term-obl WHERE term-obl.contract  =  iContract
                          AND term-obl.cont-code =  iContCode
                          AND term-obl.idnt      =  1
                          AND term-obl.dsc-beg-date =  vDate2
      NO-LOCK:
         RUN summ-t1.p (OUTPUT vAmt,
                        RECID(term-obl),
                        RECID(bloan)).
         vAmt2 = vAmt2 + vAmt.
      END.

      vAmt = 0.
      IF FGetSetting("����","TR14RAZD","") =  "��" THEN
      DO:
         IF vDate1 =  vDate2 THEN
            vAmt = vAmt1 + vAmt2.
         ELSE 
         DO:
            IF vDate1 < vDate2 AND vAmt1 <> 0 THEN 
               vAmt = vAmt1.
            ELSE 
               vAmt = vAmt2.
         END.
      END.
      ELSE
         vAmt = vAmt1 + vAmt2.
   END.
   ELSE
   /* ��।��塞 �㬬� � ��⮬ �࠭襩 */
   RUN GetFirstAmtUnv(iContract,
                      iContCode,
                      3,
                      iDate,
                      "BKINextPayment1",
                      OUTPUT vAmt,
                      OUTPUT vDate2).

   IF FGetSetting("����","TR14","") =  "���" THEN 
   DO:

      vAmt = BKIAmountOutst(BUFFER bLoan,
                            INPUT  vDate2,
                            TRUE).

   END.

   {profile BK330}
   RETURN vAmt.
END FUNCTION.
/*----------------------------------------------------------------------------*/
/* ������騩 ���⥦                                                           */
/*----------------------------------------------------------------------------*/
PROCEDURE BKINextPayment1:
   DEF INPUT  PARAM iContract AS CHARACTER NO-UNDO.
   DEF INPUT  PARAM iContCode AS CHARACTER NO-UNDO.
   DEF INPUT  PARAM iOper     AS INT64       NO-UNDO.
   DEF INPUT  PARAM iDate     AS DATE      NO-UNDO.
   DEF OUTPUT PARAM oDateTr   AS DATE      NO-UNDO.
   DEF OUTPUT PARAM oAmt      AS DECIMAL   NO-UNDO.

   DEF VAR vAmt AS DEC NO-UNDO.
   DEF BUFFER bterm-obl  FOR term-obl.
   DEF BUFFER bloan-cond FOR loan-cond.
   DEFINE VARIABLE vLgotPer AS INT64 NO-UNDO INIT 0.
   DEFINE VARIABLE vFirstPayDate AS DATE NO-UNDO .
   {profile BK331}

   IF FGetSetting("����","�������⥦","") =  "��" THEN DO:
      FIND FIRST  bloan-cond WHERE
                  bloan-cond.contract   = iContract
            AND bloan-cond.cont-code  = iContCode
         NO-LOCK NO-ERROR.
      IF AVAILABLE bloan-cond THEN DO:
         vLgotPer = INT64(GetXAttrValueEx ( "loan-cond",
         iContract + "," + iContCode + "," + STRING(bloan-cond.since),
         "�����⏥�","0")).
      END.
   END.

   IF vLgotPer > 0
   THEN DO:  /*  � ��⮬ �죮��� ��ਮ��� */
      /*�죮�� ��ਮ�� ��।��塞  �� �᭮�� ��䨪� ��  -  �� ⠬ ��� */
      FIND FIRST bterm-obl WHERE
               bterm-obl.contract  = iContract
           AND bterm-obl.cont-code = iContCode
           AND bterm-obl.idnt      = 3
           NO-LOCK NO-ERROR.
          vFirstPayDate = bterm-obl.end-date .  /* ��� ��ࢮ�� ���⥦� �� �������� */
      IF   vFirstPayDate >= iDate
      THEN DO:   /* �������� � �죮�� ��ਮ� */
         ASSIGN
            oAmt = 0.00
            oDateTr = ?.
         /* �㬬��㥬 �� ���᫥��� ������ � �죮⭮� ��ਮ�� - �� � �㤥� ���� ���⥦ */
         FOR EACH  bterm-obl WHERE
                   bterm-obl.contract  = iContract
               AND bterm-obl.cont-code = iContCode
               AND bterm-obl.idnt      = iOper        /* iOper = 3 or 1 */
               AND bterm-obl.end-date <= vFirstPayDate
               NO-LOCK :
                  oAmt    = oAmt + bterm-obl.amt-rub .
                  oDateTr = bterm-obl.end-date.
                  &IF DEFINED(IS-DEBUG) &THEN
                        RUN dbgprint.p("BKINextPayment1",
                                       "�죮�� ��ਮ� = " +  string (vLgotPer)
                                       + " iContract = " + iContract
                                       + " iContCode = " + iContCode
                                       + " term-obl.end-date = "
                                       + STRING(bterm-obl.end-date) + " term-obl.amt-rub = "
                                       + STRING(bterm-obl.amt-rub)).
                  &ENDIF
         END.
      END.
      ELSE DO: /* ����� �죮⭮�� ��ਮ�� � ࠡ�⠥� ��� ���筮 �� ��䨪� */
         FIND FIRST bterm-obl WHERE
                  bterm-obl.contract  = iContract
               AND bterm-obl.cont-code = iContCode
               AND bterm-obl.idnt      = iOper        /* iOper = 3 or 1 */
               AND bterm-obl.end-date > iDate
            NO-LOCK NO-ERROR.
         IF AVAIL bterm-obl
            THEN ASSIGN
                     oAmt    = bterm-obl.amt-rub
                     oDateTr = bterm-obl.end-date.
         ELSE ASSIGN
               oAmt = 0.00
               oDateTr = ?.

         &IF DEFINED(IS-DEBUG) &THEN
            IF AVAIL bterm-obl THEN
               RUN dbgprint.p("BKINextPayment1",
                              "iContract = " + iContract
                              + " iContCode = " + iContCode
                              + " term-obl.end-date = "
                              + STRING(bterm-obl.end-date) + " term-obl.amt-rub = "
                              + STRING(bterm-obl.amt-rub)).
            ELSE RUN dbgprint.p("BKINextPayment","iContract = " + iContract
                                                + " iContCode = " + iContCode
                                                + " - ��� ���⥦��").
         &ENDIF
      END.
   END.
   ELSE DO: /*  ��� ��� �죮��� ��ਮ��� */
      FIND FIRST bterm-obl WHERE
               bterm-obl.contract  = iContract
            AND bterm-obl.cont-code = iContCode
            AND bterm-obl.idnt      = iOper        /* iOper = 3 or 1 */
            AND bterm-obl.end-date > iDate
         NO-LOCK NO-ERROR.
      IF AVAIL bterm-obl
         THEN ASSIGN
                  oAmt    = bterm-obl.amt-rub
                  oDateTr = bterm-obl.end-date.
      ELSE ASSIGN
            oAmt = 0.00
            oDateTr = ?.

      &IF DEFINED(IS-DEBUG) &THEN
         IF AVAIL bterm-obl THEN
            RUN dbgprint.p("BKINextPayment1",
                           "iContract = " + iContract
                           + " iContCode = " + iContCode
                           + " term-obl.end-date = "
                           + STRING(bterm-obl.end-date) + " term-obl.amt-rub = "
                           + STRING(bterm-obl.amt-rub)).
         ELSE RUN dbgprint.p("BKINextPayment","iContract = " + iContract
                                             + " iContCode = " + iContCode
                                             + " - ��� ���⥦��").
      &ENDIF
   END.
   {profile BK340}
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* 15.  ����� �믫��                                                        */
/*----------------------------------------------------------------------------*/
FUNCTION BKICredPayFreq RETURNS CHAR
    (iContract AS CHAR,
     iContCode AS CHAR,              /* �।��� ������� */
     iDate AS DATE):                 /* ��� ���㧪� */

   DEF VAR vAcctType AS CHAR NO-UNDO.
   DEF BUFFER bloan-cond FOR loan-cond.
   DEF BUFFER bLoan      FOR loan.

   {profile BK341}
   /* �饬 �������饥 �᫮��� �� ���� ���� */
   RUN RE_L_COND(iContract,
                 iContCode,
                 iDate,
                 BUFFER bloan-cond).

   IF NOT AVAIL bloan-cond THEN DO:

      FIND FIRST bLoan WHERE bLoan.contract  =  iContract
                         AND bLoan.cont-code =  iContCode NO-LOCK NO-ERROR.

      IF AVAIL bLoan THEN
         RUN RE_L_COND_FRST IN h_Loan(iContract,
                                      iContCode,
                                      bLoan.open-date,
                                      BUFFER bloan-cond).
   END.

   IF AVAIL bloan-cond THEN
      vAcctType = GetCodeByMisc("����_�����",
                                1,
                                bloan-cond.cred-period).
   {profile BK350}
   RETURN vAcctType.
END FUNCTION.
/*----------------------------------------------------------------------------*/
/* 16. �����६������� ���⥦��                                               */
/*----------------------------------------------------------------------------*/
FUNCTION BKIMOP RETURNS CHAR
    (iContract AS CHAR,               /* �।��� ������� */
     iContCode AS CHAR,
     iDate AS DATE):                  /* ��� ���㧪� */

   DEF VAR vAcctMOP   AS CHAR    NO-UNDO. /* ���. ४����� �� ������� */
   DEF VAR vDueDate   AS DATE    NO-UNDO. /* ��� �뭮� �� ������ */
   DEF VAR vOpDate    AS DATE    NO-UNDO. /* ��� ����樨 ����襭�� �� � ��� ���ᯥ祭�� */
   DEF VAR vPeriod    AS INT64   NO-UNDO.
   DEF VAR vVersion   AS DECIMAL NO-UNDO.
   DEF VAR vNBKISvMes AS LOG     NO-UNDO. /* �� ����_��_��� */

   vVersion = DECIMAL(FGetSetting("����","�����","")) NO-ERROR.
   vNBKISvMes = FGetSetting("����_��_���","","���") =  "��".

   {profile BK351}
   vAcctMOP = GetXattrValueEx("loan",
                              iContract + "," + iContCode,
                              "����_�����६���",
                              "").

   /* �᫨ ���. ४����� �� �����, � ����塞 ��� */
   IF vAcctMOP = "" THEN DO:
      /* �஢��塞, �뫠 �� ����� � ��� �����祭�� */
      RUN GetFirstDateUnv(iContract,
                          iContCode,
                          iDate,
                          ?,
                          "PayByOB",
                          OUTPUT vOpDate).
      IF vOpDate <> ?  THEN vAcctMOP = "8".
      ELSE DO:
            /* ��।��塞 ���� ������祭���� �뭮� �� ������ */
          RUN GetFirstDueDate(iContract,
                              iContCode,
                              iDate,
                              OUTPUT vDueDate).
          vPeriod = iDate - vDueDate.
             /* �᫨ ������ ����ன��, � ��५�塞 ��ਮ� ��-��㣮�� */
          IF vNBKISvMes THEN
             IF vDueDate =  ? THEN
                vPeriod = 0.
             ELSE
                vPeriod =  iDate - MAX(vDueDate, FirstMonDate(iDate)) + 1.

          IF     NOT vNBKISvMes 
             AND vDueDate = ? THEN DO:
              /* ��।��塞 ���� ��᫥���� �믫��� - �᫨ ���, � ���� */
              IF BKIDateOFLstPay(iContract,
                                 iContCode,
                                 iDate) = ?
                 THEN vAcctMOP = "0".      /* ���� */
              ELSE vAcctMOP = "1".         /* ����� ��� ����祪 */
          END.
          ELSE IF vPeriod < 6
                  THEN vAcctMOP = IF vVersion >= 2.12 THEN "A" ELSE "7".     /* ॣ���� ���⥦� */
          ELSE IF vPeriod >= 6 AND
                  vPeriod < 30
                  THEN vAcctMOP = "A".     /* ����窠 �� 6 �� 29 ���� */
          ELSE IF vPeriod >= 30 AND
                  vPeriod < 60
                  THEN vAcctMOP = "2".     /* ����窠 �� 30 �� 59 ���� */
          ELSE IF vPeriod >= 60 AND
                  vPeriod < 90
                  THEN vAcctMOP = "3".     /* ����窠 �� 60 �� 89 ���� */
          ELSE IF vPeriod >= 90 AND
                  vPeriod < 120
                  THEN vAcctMOP = "4".     /* ����窠 �� 90 �� 119 ���� */
          ELSE IF vPeriod >= 120
                  THEN vAcctMOP = "5".     /* ����窠 ����� 120 ���� */

      END.

   END.
   {profile BK360}
   RETURN vAcctMOP.
END FUNCTION.
/*----------------------------------------------------------------------------*/
/* 18. ��� ������                                                             */
/*----------------------------------------------------------------------------*/
FUNCTION BKICollatCode RETURNS CHAR
    (BUFFER bterm-obl FOR term-obl,
     iDate AS DATE):                 /* ��� ���㧪� */

   DEF VAR vAcctCode AS CHAR NO-UNDO.
   DEF VAR vTypeOb   AS CHAR NO-UNDO.
   DEF VAR vTR18     AS LOG  NO-UNDO.
   DEF VAR vTR18PCl  AS LOG  NO-UNDO.
   DEF VAR vKndOb    AS CHAR NO-UNDO.

   DEF BUFFER term-obl FOR term-obl.
   DEF BUFFER loan     FOR loan.

   IF NOT AVAIL bterm-obl THEN RETURN "".

   /* ��������� ⮫쪮 �� ������� */
   vTR18 = FGetSetting("����","TR18","���") =  "��".

   /* ���ᯥ祭�� �ய�᪠��, �᫨ �����⥫� - �� ������ �� �������� */
   vTR18PCl = FGetSetting("����","TR18PCl","���") =  "��".

   FIND FIRST loan WHERE
              loan.contract  =  bterm-obl.contract
          AND loan.cont-code =  bterm-obl.cont-code
   NO-LOCK NO-ERROR.
   IF NOT AVAIL loan THEN RETURN "".

      FOR EACH term-obl WHERE term-obl.contract  =  bterm-obl.contract
                          and term-obl.cont-code =  bterm-obl.cont-code
                          and term-obl.idnt      =  5
          no-lock:
      /* ����஫� - ������ �� �������� ࠢ�� �����⥫� */
      IF     vTR18PCl
        AND (term-obl.symbol <> loan.cust-cat
        OR   term-obl.fop    <> loan.cust-id)  THEN 
         NEXT.
          vKndOb = GetXattrValueEx("term-obl",
                                   term-obl.contract + "," +
                                   term-obl.cont-code + "," +
                                   STRING(term-obl.idnt) + "," +
                                   STRING(term-obl.end-date) + "," +
                                   STRING(term-obl.nn),
                                   "��������",
                                   "").
      /* �᫨ ���� ��������� ⮫쪮 �� ������� */
      IF    vTR18 
        AND NOT CAN-DO("�।��,�।���",vKndOb) THEN 
             next.
          vTypeOb = GetXattrValueEx("term-obl",
                                    term-obl.contract + "," +
                                    term-obl.cont-code + "," +
                                    STRING(term-obl.idnt) + "," +
                                    STRING(term-obl.end-date) + "," +
                                    STRING(term-obl.nn),
                                    "�����",
                                    "").
      vAcctCode = IF vTypeOb =  "��࠭��" AND term-obl.symbol <> "�" THEN
                     GetCodeByMisc("����_���������",
                                   1,
                                   "������")
                  ELSE
                     GetCodeByMisc("����_���������",
                                    1,
                                    vTypeOb).
      LEAVE.
      end.
   RETURN vAcctCode.
END FUNCTION.
/*----------------------------------------------------------------------------*/
/* ��� �������⥫��� ������� ��� ᥣ���� CL                                 */
/*----------------------------------------------------------------------------*/
{pfuncdef 
   &DEFPROC="BKICollatCodeCL"
   &DESCRIPTION="��� �������⥫��� ������� ��� ᥣ���� CL"
   &PARAMETERS="����� term-obl,����"
   &RESULT="��� ������"
   &SAMPLE="BKICollatCodeCL(BUFFER term-obl,01/01/2015)"}
FUNCTION BKICollatCodeCL RETURNS CHAR
    (BUFFER vTerm-obl FOR term-obl,
     iDate AS DATE):                 /* ��� ���㧪� */

   DEF VAR vAcctCode AS CHAR NO-UNDO.
   DEF VAR vTypeOb   AS CHAR NO-UNDO.
   DEF VAR vTR18     AS LOG  NO-UNDO.
   DEF VAR vTR18PCl  AS LOG  NO-UNDO.
   DEF VAR vKndOb    AS CHAR NO-UNDO.

   DEF BUFFER term-obl FOR term-obl.
   DEF BUFFER loan     FOR loan.

   IF NOT AVAIL vTerm-obl THEN RETURN "".

   /* ��������� ⮫쪮 �� ������� */
   vTR18 = FGetSetting("����","TR18","���") =  "��".

   /* ���ᯥ祭�� �ய�᪠��, �᫨ �����⥫� - �� ������ �� �������� */
   vTR18PCl = FGetSetting("����","TR18PCl","���") =  "��".

   FIND FIRST loan WHERE
              loan.contract  =  vTerm-obl.contract
          AND loan.cont-code =  vTerm-obl.cont-code
   NO-LOCK NO-ERROR.
   IF NOT AVAIL loan THEN RETURN "".

   /* ����஫� - ������ �� �������� ࠢ�� �����⥫� */
   IF     vTR18PCl
     AND (vTerm-obl.symbol <> loan.cust-cat
     OR   vTerm-obl.fop    <> loan.cust-id)  THEN 
     RETURN "".

   vKndOb = GetXattrValueEx("term-obl",
                             vTerm-obl.contract + "," +
                             vTerm-obl.cont-code + "," +
                             STRING(vTerm-obl.idnt) + "," +
                             STRING(vTerm-obl.end-date) + "," +
                             STRING(vTerm-obl.nn),
                             "��������",
                             "").
   /* �᫨ ���� ��������� ⮫쪮 �� ������� */
   IF    vTR18 
     AND NOT CAN-DO("�।��,�।���",vKndOb) THEN 
     RETURN "".
      vTypeOb = GetXattrValueEx("term-obl",
                              vTerm-obl.contract + "," +
                              vTerm-obl.cont-code + "," +
                              STRING(vTerm-obl.idnt) + "," +
                              STRING(vTerm-obl.end-date) + "," +
                              STRING(vTerm-obl.nn),
                                "�����",
                                "").
   vAcctCode = IF vTypeOb =  "��࠭��" AND vTerm-obl.symbol <> "�" THEN
                  GetCodeByMisc("����_���������",
                                1,
                                "������")
               ELSE
                  GetCodeByMisc("����_���������",
                                   1,
                                 vTypeOb).

   RETURN vAcctCode.
END FUNCTION.


/*----------------------------------------------------------------------------*/
/* 20.  ��� 䨭��쭮�� ���⥦�                                               */
/*----------------------------------------------------------------------------*/
FUNCTION BKIDatePayDue RETURNS DATE
   (iContract AS CHAR,
    iContCode AS CHAR,              /* �।��� ������� */
    iDate AS DATE):                 /* ��� ���㧪� */

   DEF VAR vDate AS DATE NO-UNDO.
   DEF BUFFER bloan FOR loan.

   /* ��뢠�� ��楤���, ����� ���� �࠭� */
   RUN GetLastDateUnv(iContract,
                      iContCode,
                      3,
                      iDate,
                      "BKIDatePayDue1",
                      OUTPUT vDate).
   /* �᫨ �� ��।����� �� ��䨪� ��᫥���� ����,
   ** ��६ ��᫥���� ���� �� �������� */
   IF vDate = ? THEN DO:
      /* �饬 ������� */
      RUN RE_B_LOAN (iContract,iContCode,BUFFER bloan).
      IF AVAIL bloan THEN vDate = bloan.end-date.
   END.

   RETURN vDate.

END FUNCTION.
/*----------------------------------------------------------------------------*/
/* ��� 䨭��쭮�� ���⥦�                                                    */
/*----------------------------------------------------------------------------*/
PROCEDURE BKIDatePayDue1.
   DEF INPUT  PARAM iContract AS CHARACTER NO-UNDO.
   DEF INPUT  PARAM iContCode AS CHARACTER NO-UNDO.
   DEF INPUT  PARAM iOper     AS INT64   NO-UNDO.
   DEF INPUT  PARAM iDate     AS DATE      NO-UNDO.
   DEF OUTPUT PARAM oDateTr   AS DATE      NO-UNDO.

   DEF BUFFER bterm-obl FOR term-obl.

   /* ��室�� ��᫥���� ���㫥��� ���⥦ �� */
   btobl:
   FOR EACH bterm-obl WHERE
            bterm-obl.contract  =  iContract
        AND bterm-obl.cont-code =  iContCode
        AND bterm-obl.idnt      =  iOper       /* iOper = 3 */
   NO-LOCK
   BY bterm-obl.end-date DESC:

       IF bterm-obl.amt-rub > 0.00 THEN DO:
           oDateTr = bterm-obl.end-date.
           LEAVE btobl.
       END.

   END.

END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* 21.  ��� 䨭��쭮� ���㫥��� �믫��� ��業⮢                            */
/*----------------------------------------------------------------------------*/
FUNCTION BKIDateInterPayDue RETURNS DATE
    (iContract AS CHAR,
     iContCode AS CHAR,              /* �।��� ������� */
     iDate AS DATE):                 /* ��� ���㧪� */

   DEF VAR vDate AS DATE NO-UNDO.
   DEF BUFFER bloan FOR loan.

   /* ��뢠�� ��楤���, ����� ���� �࠭� */
   RUN GetLastDateUnv(iContract,
                      iContCode,
                      1,
                      iDate,
                      "BKIDateInterPayDue1",
                      OUTPUT vDate).
   /* �᫨ �� ��।����� �� ��䨪� ��᫥���� ����,
   ** ��६ ��᫥���� ���� �� �������� */
   IF vDate = ? THEN DO:
      /* �饬 ������� */
      RUN RE_B_LOAN (iContract,iContCode,BUFFER bloan).
      IF AVAIL bloan THEN vDate = bloan.end-date.
   END.

   RETURN vDate.

END FUNCTION.
/*----------------------------------------------------------------------------*/
/* ��� 䨭��쭮� ���㫥��� �믫��� ��業⮢                                 */
/*----------------------------------------------------------------------------*/
PROCEDURE BKIDateInterPayDue1.
   DEF INPUT  PARAM iContract AS CHARACTER NO-UNDO.
   DEF INPUT  PARAM iContCode AS CHARACTER NO-UNDO.
   DEF INPUT  PARAM iOper     AS INT64   NO-UNDO.
   DEF INPUT  PARAM iDate     AS DATE      NO-UNDO.
   DEF OUTPUT PARAM oDateTr   AS DATE      NO-UNDO.

   DEF BUFFER bterm-obl FOR term-obl.

   /* ��室�� ��᫥���� ���㫥��� ���⥦ ��業⮢ */
   btobl:
   FOR EACH bterm-obl WHERE
            bterm-obl.contract  =  iContract
        AND bterm-obl.cont-code =  iContCode
        AND bterm-obl.idnt      =  iOper     /* iOper = 1 */
   NO-LOCK
   BY bterm-obl.end-date DESC:

       IF bterm-obl.amt-rub > 0.00 THEN DO:
           oDateTr = bterm-obl.dsc-beg-date.
           LEAVE btobl.
       END.

   END.

END.
/*----------------------------------------------------------------------------*/
/* 22.  ����� �믫�� ��業⮢                                              */
/*----------------------------------------------------------------------------*/
FUNCTION BKIInterPayFreq RETURNS CHAR
    (iContract AS CHAR,
     iContCode AS CHAR,              /* �।��� ������� */
     iDate AS DATE):                 /* ��� ���㧪� */

   DEF VAR vAcctType AS CHAR NO-UNDO.

   DEF BUFFER bloan-cond FOR loan-cond.
   DEF BUFFER bLoan      FOR loan.

   RUN RE_L_COND(iContract,
                 iContCode,
                 iDate,
                 BUFFER bloan-cond).

   IF NOT AVAIL bloan-cond THEN DO:

      FIND FIRST bLoan WHERE bLoan.contract  =  iContract
                         AND bLoan.cont-code =  iContCode NO-LOCK NO-ERROR.

      IF AVAIL bLoan THEN
         RUN RE_L_COND_FRST IN h_Loan(iContract,
                                      iContCode,
                                      bLoan.open-date,
                                      BUFFER bloan-cond).
   END.

   IF NOT AVAIL bloan-cond THEN RETURN ?.

   vAcctType = GetCodeByMisc("����_�����",
                             1,
                             bloan-cond.int-period).
   RETURN vAcctType.

END FUNCTION.
/*----------------------------------------------------------------------------*/
/* ���� ��ࢮ� ���� � ��⮬ �祭��                                         */
/*----------------------------------------------------------------------------*/
PROCEDURE  GetFirstDateUnv.
    DEF INPUT  PARAM iContract AS CHARACTER NO-UNDO.
    DEF INPUT  PARAM iContCode AS CHARACTER NO-UNDO.
    DEF INPUT  PARAM iDate     AS DATE      NO-UNDO.
    DEF INPUT  PARAM iDatePrev AS DATE      NO-UNDO.
    DEF INPUT  PARAM iNameProc AS CHARACTER NO-UNDO.
    DEF OUTPUT PARAM oDateTr   AS DATE      NO-UNDO.

   DEF BUFFER bloan FOR loan.
   DEF VAR vCurDate AS DATE NO-UNDO.

   RUN VALUE(iNameProc)(iContract,
                        iContCode,
                        iDate,
                        iDatePrev,
                        OUTPUT oDateTr).

   /* �饬 ������� */
   RUN RE_B_LOAN (iContract,iContCode,BUFFER bloan).

   /* �᫨ �� �墠�뢠�騩 �������-�祭��, � �饬 �� ��� �࠭� */
   IF AVAIL bloan AND
            bloan.cont-type = "��祭��" THEN
   DO:
      FOR EACH bloan WHERE
               bloan.contract  = iContract
           AND bloan.cont-code BEGINS iContCode + " "
           AND bloan.cont-code <> iContCode
          NO-LOCK:
          RUN VALUE(iNameProc)(bloan.contract,
                               bloan.cont-code,
                               iDate,
                               iDatePrev,
                               OUTPUT vCurDate).
          IF vCurDate <> ? AND
             vCurDate < oDateTr
             THEN oDateTr = vCurDate.
      END.
   END.
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* ���� ��᫥���� ���� � ��⮬ �祭��                                      */
/*----------------------------------------------------------------------------*/
PROCEDURE  GetLastDateUnv.
    DEF INPUT  PARAM iContract AS CHARACTER NO-UNDO.
    DEF INPUT  PARAM iContCode AS CHARACTER NO-UNDO.
    DEF INPUT  PARAM iOper     AS INT64   NO-UNDO.
    DEF INPUT  PARAM iDate     AS DATE      NO-UNDO.
    DEF INPUT  PARAM iNameProc AS CHARACTER NO-UNDO.
    DEF OUTPUT PARAM oDateTr   AS DATE      NO-UNDO.

   DEF BUFFER bloan FOR loan.
   DEF VAR vCurDate AS DATE NO-UNDO.

   RUN VALUE(iNameProc)(iContract,
                        iContCode,
                        iOper,
                        iDate,
                        OUTPUT oDateTr).

   /* �饬 ������� */
   RUN RE_B_LOAN (iContract,iContCode,BUFFER bloan).

   /* �᫨ �� �墠�뢠�騩 �������-�祭��, � �饬 �� ��� �࠭� */
   IF AVAIL bloan AND
            bloan.cont-type = "��祭��" THEN
   DO:
      FOR EACH bloan WHERE
               bloan.contract  = iContract
           AND bloan.cont-code BEGINS iContCode + " "
           AND bloan.cont-code <> iContCode
          NO-LOCK:
          RUN VALUE(iNameProc)(bloan.contract,
                               bloan.cont-code,
                               iOper,
                               iDate,
                               OUTPUT vCurDate).
          IF     oDateTr =  ?
             OR (    vCurDate <> ? 
                 AND vCurDate > oDateTr)
             THEN oDateTr = vCurDate.
      END.
   END.
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* �����頥� �㬬� �� ��ࠬ���� ��� ����樨 � ��⮬ �祭��                */
/*----------------------------------------------------------------------------*/
PROCEDURE GetSummUnv:
   DEF INPUT  PARAM iContract AS CHARACTER NO-UNDO.
   DEF INPUT  PARAM iContCode AS CHARACTER NO-UNDO.
   DEF INPUT  PARAM iOper     AS INT64   NO-UNDO.
   DEF INPUT  PARAM iDate     AS DATE      NO-UNDO.
   DEF INPUT  PARAM iNameProc AS CHARACTER NO-UNDO.
   DEF OUTPUT PARAM oAmtRes   AS DECIMAL   NO-UNDO.
   DEF OUTPUT PARAM oAmtDb    AS DECIMAL   NO-UNDO.
   DEF OUTPUT PARAM oAmtCr    AS DECIMAL   NO-UNDO.

   DEF BUFFER bloan FOR loan.
   DEF VAR vAmtCur AS DECIMAL NO-UNDO.
   DEF VAR vAmtDbCur AS DECIMAL NO-UNDO.
   DEF VAR vAmtCrCur AS DECIMAL NO-UNDO.

   {profile BK501}
   RUN VALUE(iNameProc)(iContract,
                        iContCode,
                        iOper,
                        iDate,
                        OUTPUT oAmtRes,
                        OUTPUT oAmtDb,
                        OUTPUT oAmtCr).
   {profile BK502}
   /* �饬 ������� */
   FIND FIRST bLoan WHERE
              bLoan.contract  =  iContract
          AND bLoan.cont-code =  iContCode
              NO-LOCK NO-ERROR.
   {profile BK503}
   ASSIGN
      vAmtCur   = 0.00
      vAmtDbCur = 0.00
      vAmtCrCur = 0.00
   .

/*----------- �᫨ �� �墠�뢠�騩 �������-�祭��, � �饬 �� ��� �࠭� --*/
   IF AVAIL bloan                  AND
      bloan.cont-type =  "��祭��" THEN
      FOR EACH bloan WHERE
               bloan.contract  = iContract
           AND bloan.cont-code BEGINS iContCode + " "
           AND bloan.cont-code <> iContCode
               NO-LOCK:
         {profile BK504}
         RUN VALUE(iNameProc)(bloan.contract,
                              bloan.cont-code,
                              iOper,
                              iDate,
                              OUTPUT vAmtCur,
                              OUTPUT vAmtDbCur,
                              OUTPUT vAmtCrCur).
         ASSIGN
            oAmtRes = oAmtRes + vAmtCur
            oAmtDb  = oAmtDb  + vAmtDbCur
            oAmtCr  = oAmtCr  + vAmtCrCur
         .
         {profile BK505}
      END.

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p("GetSummUnv","iContract = " + iContract
                                    + " iContCode = " + iContCode
                                    + " iOper = " + string(iOper)
                                    + " iDate = " +
                               (IF iDate <> ? THEN STRING(iDate) ELSE "    ")
                                    + " oAmtRes = " + STRING(oAmtRes)).

      &ENDIF
   {profile BK510}
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* ���� ��ࢮ� �㬬� � ��⮬ �祭��                                        */
/*----------------------------------------------------------------------------*/
PROCEDURE  GetFirstAmtUnv.
    DEF INPUT  PARAM iContract AS CHARACTER NO-UNDO.
    DEF INPUT  PARAM iContCode AS CHARACTER NO-UNDO.
    DEF INPUT  PARAM iOper     AS INT64     NO-UNDO. /* 1 - %%, 3 - �� */
    DEF INPUT  PARAM iDate     AS DATE      NO-UNDO.
    DEF INPUT  PARAM iNameProc AS CHARACTER NO-UNDO.
    DEF OUTPUT PARAM oAmt      AS DECIMAL   NO-UNDO.
    DEF OUTPUT PARAM oDateTr   AS DATE      NO-UNDO.

    DEF BUFFER bloan FOR loan.

    DEF VAR vCurDate AS DATE    NO-UNDO.
    DEF VAR vCurAmt  AS DECIMAL NO-UNDO.

   /* �饬 ������� */
   RUN RE_B_LOAN (iContract,iContCode,BUFFER bloan).

   /* �᫨ �� �墠�뢠�騩 �������-�祭��, � �饬 �� ��� �࠭� */
    IF AVAIL bloan AND
             bloan.cont-type = "��祭��"
        AND CAN-DO(FGetSetting("����", IF (iOper = 1) THEN "��������࠭�" ELSE "��������࠭�", ""), bloan.class-code)   /* ������� �� �࠭�� */
    THEN DO:
        FOR EACH bloan
            WHERE  bloan.contract   = iContract
              AND  bloan.cont-code  BEGINS iContCode + " "
              AND  bloan.cont-code  <> iContCode
              AND  bloan.open-date  <= iDate  /* �࠭�, �� �� �뤠��� �� ���� ����, �� ���뢠�� */
              AND (bloan.close-date =  ?
                OR bloan.close-date >  iDate)
            NO-LOCK:

            RUN VALUE(iNameProc)(bloan.contract,
                                 bloan.cont-code,
                                 iOper,
                                 iDate,
                                 OUTPUT vCurDate,
                                 OUTPUT vCurAmt).
            /* ���⥦� �� ���� ���� �㬬��㥬 */
            IF vCurDate <> ? AND
               vCurDate = oDateTr
            THEN oAmt = oAmt + vCurAmt.

            IF (vCurDate <> ?) AND (vCurAmt NE 0) AND
               (oDateTr = ? OR vCurDate < oDateTr)
            THEN ASSIGN
                    oDateTr = vCurDate
                    oAmt    = vCurAmt.
        END.
    END.
    ELSE    /* ������� ��� �࠭襩 */
        RUN VALUE(iNameProc)(iContract,
                             iContCode,
                             iOper,
                             iDate,
                             OUTPUT oDateTr,
                             OUTPUT oAmt).

    IF oAmt =  ? THEN oAmt = 0.

END PROCEDURE.

/*----------------------------------------------------------------------------*/
/* �����饭�� ����� �� ��ࠬ����                                          */
/*----------------------------------------------------------------------------*/
PROCEDURE GetSummPar.
   DEF INPUT PARAM iContract  AS CHAR NO-UNDO.
   DEF INPUT PARAM iContCode  AS CHAR NO-UNDO.
   DEF INPUT PARAM iOper      AS INT64  NO-UNDO.   /* ��� ����樨 */
   DEF INPUT PARAM iDate      AS DATE NO-UNDO.   /* ��� ���� */
   DEF OUTPUT PARAM oAmtPrm   AS DEC  NO-UNDO.
   DEF OUTPUT PARAM oAmtDb    AS DEC  NO-UNDO.   /* ����窨 */
   DEF OUTPUT PARAM oAmtCr    AS DEC  NO-UNDO.   /* ����窨 */

   DEF BUFFER bloan FOR loan.
   DEF VAR vAmtCur   AS DECIMAL NO-UNDO.
   DEF VAR vAmt      AS DECIMAL NO-UNDO.
   DEF VAR vAmtDbCur AS DECIMAL NO-UNDO.
   DEF VAR vAmtCrCur AS DECIMAL NO-UNDO.

   RUN STNDRT_PARAM(iContract,
                    iContCode,
                    iOper,
                    iDate,
                    OUTPUT vAmtCur,
                    OUTPUT vAmtDbCur,
                    OUTPUT vAmtCrCur).

   FIND FIRST bLoan WHERE bloan.contract =  iContract AND bloan.cont-code =  iContCode NO-LOCK NO-ERROR.

   IF AVAIL(bloan) THEN
   RUN inter_current  (buffer bloan, iOper, OUTPUT vAmt).

   ASSIGN
      oAmtPrm =  vAmt + vAmtCur.

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p("GetSummPar","iContract = " + iContract
                                 + " iContCode = " + iContCode
                                 + " iOper = " + string(iOper)
                                 + " vAmt = " + string(vAmt)
                                 + " iDate = " +
                            (IF iDate <> ? THEN STRING(iDate) ELSE "    ")
                                 + " oAmtPrm = " + STRING(oAmtPrm)).
   &ENDIF

END.

{pfuncdef 
   &DEFPROC="GetBuyerInfo"
   &DESCRIPTION="����祭�� ������ � �८���⥫� �ࠢ �� ��㤥"
   &PARAMETERS="���������� ��������,������������� ��������,����"
   &RESULT="����� � �८���⥫� �ࠢ �� ��㤥"
   &SAMPLE="GetBuyerInfo('�।��','123',01/01/2015,OUTPUT oBuyerName,OUTPUT oBuyerID, ~
OUTPUT oBuyerINN,OUTPUT oBuyerSNILS)"}

PROCEDURE GetBuyerInfo:
   DEF INPUT  PARAM iContract   AS CHAR NO-UNDO.
   DEF INPUT  PARAM iContCode   AS CHAR NO-UNDO.
   DEF INPUT  PARAM iDate       AS DATE NO-UNDO.   /* ��� ���� */ 
   DEF OUTPUT PARAM oBuyerName  AS CHAR NO-UNDO.
   DEF OUTPUT PARAM oBuyerID    AS CHAR NO-UNDO.
   DEF OUTPUT PARAM oBuyerINN   AS CHAR NO-UNDO.
   DEF OUTPUT PARAM oBuyerSNILS AS CHAR NO-UNDO.
   
   DEF VAR vPBirth   AS CHAR  NO-UNDO.
   DEF VAR vDtIss    AS DATE  NO-UNDO.
   DEF VAR vCode     AS CHAR  NO-UNDO.
   DEF VAR vCharAdd  AS CHAR  NO-UNDO.
   DEF VAR vCorpName AS CHAR  NO-UNDO.
   DEF VAR vCust     AS CHAR  NO-UNDO.
   DEF VAR vCustCat  AS CHAR  NO-UNDO.
   DEF VAR vCustId   AS INT64 NO-UNDO. 
   DEF VAR vFlCes    AS LOG   NO-UNDO. 
   
   DEF BUFFER signs      FOR signs.
   DEF BUFFER loan       FOR loan.
   DEF BUFFER bloan      FOR loan.
   DEF BUFFER person     FOR person.
   DEF BUFFER cust-ident FOR cust-ident.
   DEF BUFFER cust-corp  FOR cust-corp.
   DEF BUFFER banks-code FOR banks-code.
   
   main:
   DO:       
       FIND FIRST loan WHERE loan.contract  =  iContract
                         AND loan.cont-code =  iContCode
       NO-LOCK NO-ERROR.
       IF NOT AVAIL loan THEN
          LEAVE MAIN.
       /* �����? */
       vFlCes = CAN-DO(GetXclassAllChildsEx("loan_ces"),loan.class-code).
       IF vFlCes THEN
       DO:
          ASSIGN
             oBuyerName  = FGetSetting("������","","") 
             oBuyerID    = FGetSetting("�������","","").
          FIND FIRST banks-code WHERE banks-code.bank-code      =  oBuyerID
                                  AND banks-code.bank-code-type =  '���-9' 
          NO-LOCK NO-ERROR.
          IF AVAIL banks-code THEN
          DO:
             FIND FIRST cust-ident WHERE cust-ident.cust-cat  =  "�" 
                                     AND cust-ident.cust-id   =  banks-code.bank-id        
                                     AND cust-ident.cust-code-type =  "���"
             NO-LOCK NO-ERROR.
             IF AVAIL cust-ident THEN
                oBuyerINN = cust-ident.cust-code.
          END.
       END.
       ELSE
       DO:
           vCust = GetXAttrValueEx("loan",iContract + "," + iContCode,"����ࠣ���","").
           IF NOT {assigned vCust} THEN
           DO:
              /* ��।��塞, �த��� �� ��易⥫��⢠ */
              FIND FIRST signs WHERE signs.file-name  =  "loan"
                                 AND signs.surrogate  BEGINS "�।��,"
                                 AND signs.code       =  "����"
                                 AND signs.code-value =  iContCode
              NO-LOCK NO-ERROR.
              IF AVAIL signs THEN
              DO:
                 FIND FIRST bloan WHERE bloan.contract  =  ENTRY(1,signs.surrogate)
                                    AND bloan.cont-code =  ENTRY(2,signs.surrogate)
                 NO-LOCK NO-ERROR.
                 IF AVAIL bloan THEN
                 DO:
                    vCode = GetXAttrValueEx("loan",
                                            bloan.contract + "," + bloan.cont-code,
                                            "���������",
                                            "").
                    IF NOT {assigned vCode} THEN
                       LEAVE main.
                    FIND FIRST loan WHERE loan.contract  =  "aijk"
                                      AND loan.cont-code =  vCode
                    NO-LOCK NO-ERROR.
                    IF NOT AVAIL loan THEN
                       LEAVE main.
                    ASSIGN
                       vCustCat = loan.cust-cat
                       vCustId  = loan.cust-id
                    NO-ERROR.
                 END.
              END.
           END.
           ELSE
              ASSIGN
                 vCustCat = ENTRY(1,vCust)
                 vCustId  = INT64(ENTRY(2,vCust))
              NO-ERROR.
          
           IF     {assigned vCustCat}
              AND vCustId <> 0 THEN
           DO:
              IF vCustCat =  "�" THEN
              DO:
                 FIND FIRST person WHERE person.person-id =  vCustId NO-LOCK NO-ERROR.
                 IF NOT AVAIL person THEN
                    LEAVE main.
                 ASSIGN
                    vDtIss      = DATE(GetXAttrValueEx("person",
                                                       string(person.person-id),
                                                       "Document4Date_vid",
                                                       ?))
                    vPBirth     = GetXAttrValue("person",
                                                 string(person.person-id),
                                                "BirthPlace")
                    oBuyerName  = subst("&1 &2 &3 &4",
                                        person.name-last,
                                        person.first-names,
                                        person.BirthDay,
                                        vPBirth)
                    oBuyerID    = subst("&1 &2 &3",person.Document,vDtIss,person.Issue)
                    oBuyerINN   = person.inn
                 no-error.
                 IF ERROR-STATUS:ERROR THEN
                    LEAVE main.
                 FIND LAST cust-ident WHERE  cust-ident.cust-cat   =  '�' 
                                        AND  cust-ident.cust-id    =  person.person-id
                                        AND  cust-ident.cust-code-type =  person.document-id 
                                        AND  cust-ident.class-code =  'p-cust-ident'
                                        AND (cust-ident.close-date =  ? 
                                         OR  cust-ident.close-date >  end-date)
                 NO-LOCK NO-ERROR.
                 IF AVAIL cust-ident THEN
                 DO:
                    ASSIGN
                       vCode       = GetXAttrValueEx("cust-ident",
                                                 cust-ident.cust-code-type + "," + 
                                                 STRING(cust-ident.cust-code) + "," +
                                                 STRING(cust-ident.cust-type-num),
                                                 "���ࠧ�",
                                                 "")
                       oBuyerID    = subst("&1 &2",oBuyerID,vCode).
                 END.
                 FIND LAST cust-ident WHERE  cust-ident.cust-cat   =  '�' 
                                        AND  cust-ident.cust-id    =  person.person-id
                                        AND  cust-ident.cust-code-type =  "�����" 
                                        AND  cust-ident.class-code =  'p-cust-ident'
                                        AND (cust-ident.close-date =  ? 
                                         OR  cust-ident.close-date >  end-date)
                 NO-LOCK NO-ERROR.
                 IF AVAIL cust-ident THEN
                    oBuyerSNILS = cust-ident.cust-code.
              END. 
              ELSE
              DO:
                 FIND FIRST cust-corp WHERE cust-corp.cust-id =  vCustId NO-LOCK NO-ERROR.
                 IF NOT AVAIL cust-corp THEN
                    NEXT.
                 ASSIGN
                    vCharAdd  = FGetSetting("����","�����������","")
                    vCorpName = vCharAdd + cust-corp.Name-Corp + vCharAdd.
                 IF FGetSetting("����","�����ࣔ","") =  "��" THEN
                    vCorpName = GetCodeName("����।�",
                                            GetCodeVal("����।�",cust-corp.cust-stat)) +
                                            " " + vCorpName. 
                 ASSIGN 
                    oBuyerName = subst("&1, &2", vCorpName, cust-corp.name-short)
                    oBuyerID   = GetXAttrValueEx("cust-corp",string(cust-corp.cust-id),"����","")
                    oBuyerINN  = cust-corp.inn.
              END. 
           END.
       END.
   END.
   
END PROCEDURE.

{pfuncdef 
   &DEFPROC="BKIInitLoanType"
   &DESCRIPTION="����㧪� �����䨪��� NBKILoanType ��� ࠡ��� BKIGetLoanType"
   &PARAMETERS="����㧨��/������"
   &RESULT=" "
   &SAMPLE="BKIInitLoanType(YES)"}

PROCEDURE BKIInitLoanType:
   DEFINE INPUT PARAMETER iInit AS LOGICAL NO-UNDO.
    
   DEFINE BUFFER bcode  FOR code. 
   IF iInit THEN 
      FOR EACH bcode WHERE bcode.class  = 'NBKILoanType'
                       AND bcode.parent = 'NBKILoanType' NO-LOCK:
         CREATE ttLoanType.
         ASSIGN
            ttLoanType.fProd = bcode.misc[1]
            ttLoanType.fPos  = bcode.misc[2]
            ttLoanType.fProv = bcode.misc[3]
            ttLoanType.fTerm = DECIMAL(bcode.misc[4])
            ttLoanType.fSumm = DECIMAL(bcode.misc[5])
            ttLoanType.fVal  = INT64(bcode.misc[6])
            ttLoanType.fCode = bcode.val
         .
      END.
   ELSE DO:
     {empty ttLoanType}
   END.
END PROCEDURE.
   
{pfuncdef 
   &DEFPROC="BKIGetLoanType"
   &DESCRIPTION="��।������ ⨯� ������� ��� ���"
   &PARAMETERS="BUFFER boan,����"
   &RESULT="⨯ ������� �� ��-� NBKILoanType"
   &SAMPLE="BKIGetLoanType(BUFFER vLoan, end-date)"}
FUNCTION BKIGetLoanType RETURNS INT64 
   (BUFFER vLoan     FOR loan,
    INPUT iDate     AS DATE):

   DEFINE VARIABLE vProd AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vSumm AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE vTerm AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE vPos  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vSVal AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vCode AS CHARACTER NO-UNDO INIT ?.
   DEFINE VARIABLE vND   AS INT64     NO-UNDO.
   DEFINE VARIABLE vNM   AS INT64     NO-UNDO.
   DEFINE VARIABLE vNY   AS INT64     NO-UNDO.
   DEFINE VARIABLE vI    AS INT64     NO-UNDO.
   DEFINE VARIABLE vProv AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vVal  AS DECIMAL   NO-UNDO.
          
   DEFINE BUFFER ttIndicate FOR ttIndicate.
   DEFINE BUFFER term-obl FOR term-obl.
   DEFINE BUFFER bcode FOR code. 

   IF AVAILABLE vLoan THEN 
   LOOP:
   DO: 
      ASSIGN 
         vProd = IF vLoan.class-code =  "precrd_loan" 
                 THEN GetXattrValueEx("loan",
                                       vLoan.contract + "," + 
                                       vLoan.cont-code,
                                       "����������",
                                       "")
                 ELSE vLoan.cont-type
         vPos  = LnInBagOnDate (vLoan.contract, vLoan.cont-code, iDate)
         vPos = IF {assigned vPos} THEN "�" ELSE "�"
      . 
      RUN DMY_In_Per IN h_date (vLoan.open-date,
                                vLoan.end-date,
                                OUTPUT vND, 
                                OUTPUT vNM, 
                                OUTPUT vNY). 
  /* ��㫠 �ਡ����⥫쭠� �⮡ ������ ����� 楫��� ������⢠ ��� ��� ��� */ 
      vTerm = vNY + vNM / 12 + vND / 360.
      
      FIND FIRST term-obl WHERE term-obl.contract  =  vLoan.contract
                            AND term-obl.cont-code =  vLoan.cont-code
                            AND term-obl.idnt      =  2
      NO-LOCK NO-ERROR.                            
      IF AVAILABLE term-obl THEN 
         vSumm = IF vLoan.currency =  "" 
                 THEN term-obl.amt-rub
                 ELSE CurToBase ("�������", 
                                 vLoan.currency, 
                                 iDate, 
                                 term-obl.amt-rub).
      ELSE vSumm = 0.
      
      ASSIGN 
         vVal = 0
         vProv = ""
      .
      FOR EACH term-obl WHERE term-obl.contract  =  vLoan.contract
                          AND term-obl.cont-code =  vLoan.cont-code
                          AND term-obl.idnt      =  5
      NO-LOCK:
         vSVal = GetXAttrValueEx("term-obl",
                                 GetSurrogateBuffer("term-obl",
                                                    BUFFER term-obl:HANDLE),
                                 "���",
                                 "").
         IF vSVal MATCHES "*�஡��*" THEN 
         DO vI = 1 TO NUM-ENTRIES(vSVal,"|"):
            IF ENTRY(1, ENTRY(vI,vSVal,"|"), "=") =  "�஡��" THEN 
              vVal = INT64(ENTRY(2, ENTRY(vI,vSVal,"|"), "=")) NO-ERROR. 
         END.                       
         vProv = vProv + "," + GetXAttrValueEx("term-obl",
                                               GetSurrogateBuffer("term-obl",
                                               BUFFER term-obl:HANDLE),
                                               "�����",
                                               "").
   
      END.
      vProv = TRIM(vProv,",").
      
      CASE vProd:
   
         WHEN "���" THEN
         DO:
            /* SORT-ACCESS code ����室�� ��� ���४⭮�� ���᪠ */
            FOR FIRST ttLoanType WHERE ttLoanType.fProd =  vProd  
                                   AND ttLoanType.fVal  >= vVal 
            NO-LOCK:
               vCode = ttLoanType.fCode.               
            END.
         END.
         OTHERWISE 
            LOOP1:
            DO:
               IF vProv <> "" 
               THEN 
                  DO vI = 1 TO NUM-ENTRIES(vProv):
                     FOR FIRST ttLoanType WHERE CAN-DO(ttLoanType.fProd,vProd)
                                            AND CAN-DO(ttLoanType.fPos ,vPos) 
                                            AND CAN-DO(ttLoanType.fProv,
                                                       ENTRY(vI,vProv))
                                            AND ttLoanType.fTerm >= vTerm
                                            AND ttLoanType.fSumm >= vSumm   
                     NO-LOCK:
                        vCode = ttLoanType.fCode.
                        LEAVE LOOP1.               
                     END.
                  END.
               ELSE
                  FOR FIRST ttLoanType WHERE CAN-DO(ttLoanType.fProd,vProd)
                                         AND CAN-DO(ttLoanType.fPos ,vPos) 
                                         AND ttLoanType.fProv =  vProv
                                         AND ttLoanType.fTerm >= vTerm
                                         AND ttLoanType.fSumm >= vSumm   
                  NO-LOCK:
                     vCode = ttLoanType.fCode.
                     LEAVE LOOP1.               
                  END.
            END.
      END CASE. 
   END.
   IF vCode =  ? THEN vCode = "999".
   IF NUM-ENTRIES(vCode,"_") >  1 THEN
      vCode = ENTRY(1,vCode,"_").
   RETURN INT64(vCode).
END FUNCTION.


{pfuncdef 
   &DEFPROC="BKIGetShipWayApp"
   &DESCRIPTION="��।������ c��ᮡ� ��ଫ���� ������ ��� ���"
   &PARAMETERS="BUFFER boan"
   &RESULT="��� c��ᮡ� ��ଫ���� ������"
   &SAMPLE="BKIGetShipWayApp(BUFFER vLoan)"}
FUNCTION BKIGetShipWayApp RETURNS CHARACTER 
   (BUFFER vLoan FOR loan):

   DEFINE VARIABLE vSpPol AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vDopOf AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vCode  AS CHARACTER NO-UNDO.
          
   DEFINE BUFFER cust-role FOR cust-role.

   IF AVAILABLE vLoan AND 
      vLoan.class-code =  "precrd_loan" THEN 
   LOOP:
   DO: 
      ASSIGN 
         vSpPol = GetXattrValueEx("loan",
                                  vLoan.contract + "," + 
                                  vLoan.cont-code,
                                  "���ᮡ�����",
                                  "")
         vDopOf = GetXattrValueEx("loan",
                                  vLoan.contract + "," + 
                                  vLoan.cont-code,
                                  "������",
                                  "")
      .

      IF {assigned vSpPol} AND
         {assigned vDopOf} THEN
      FOR FIRST cust-role NO-LOCK
          WHERE cust-role.Class-Code =  "ImaginRO"
            AND cust-role.RegNum     =  vDopOf : 
          vCode = GetXAttrValueEx("cust-role",
                                   GetSurrogateBuffer("cust-role", 
                                                      BUFFER cust-role:HANDLE),
                                  "����_���ᮡ�।",
                                  "").
      END.
   END.
   IF NOT {assigned vCode } THEN vCode = "1".
   RETURN vCode.
END FUNCTION.

{pfuncdef 
   &DEFPROC="BKIGetApprovalFlag"
   &DESCRIPTION="��।������ ������ �襭�� �� ����७�� ��� ��� ���"
   &PARAMETERS="BUFFER boan,����,�����"
   &RESULT="䫠� ������ �襭�� �� ����७�� ���"
   &SAMPLE="BKIGetApprovalFlag(BUFFER vLoan, end-date, TIME)"}
FUNCTION BKIGetApprovalFlag RETURNS CHARACTER 
   (BUFFER vLoan     FOR loan,
    INPUT iDatePrev  AS DATE,
    INPUT iTimePrev  AS INT64):

   DEFINE VARIABLE vCode    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vBegStat AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vFinStat AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vResult  AS CHARACTER NO-UNDO.

   DEFINE BUFFER term-obl FOR term-obl.
       
   IF AVAILABLE vLoan AND 
      vLoan.class-code =  "precrd_loan" THEN 
   DO: 
       LOOP:
       /* SORT-ACCESS term-obl */
       FOR EACH term-obl NO-LOCK 
          WHERE term-obl.contract  =  "��⮪��"
            AND ENTRY(1,term-obl.cont-code,"-") =  DelFilFromLoan(vLoan.cont-code) 
            AND term-obl.idnt      =  1000000 
          BY term-obl.end-date DESCENDING 
          BY term-obl.fop      DESCENDING :
 
          IF iDatePrev         =  ?
             OR
             term-obl.end-date >  iDatePrev
             OR
             term-obl.end-date =  iDatePrev AND
             term-obl.fop      >  iTimePrev THEN
          ASSIGN  
             vBegStat = term-obl.Cont-type
             vFinStat = term-obl.Symbol
             vCode    = vLoan.cont-cli + "," + 
                        vBegStat       + "_" +
                        vFinStat 
             vResult  = IF GetCode("����_����_����", vCode) =  "����७�"
                        THEN "Y"
                        ELSE ""
          .

          IF vResult =  "Y" THEN
          LEAVE LOOP.
       END.

       IF NOT {assigned vResult} THEN
       DO:
          FIND LAST term-obl OF vLoan 
              WHERE term-obl.idnt =  1000000 NO-LOCK NO-ERROR.

          IF AVAIL term-obl
             AND ( iDatePrev         =  ?
                   OR
                   term-obl.end-date >  iDatePrev 
                   OR
                   term-obl.end-date =  iDatePrev AND
                   term-obl.fop      >  iTimePrev
                 ) THEN
          ASSIGN  
             vBegStat = term-obl.Cont-type
             vFinStat = term-obl.Symbol
             vCode    = vLoan.cont-cli + "," + 
                        vBegStat       + "_" +
                        vFinStat 
             vResult  = IF GetCode("����_����_����", vCode) =  "����७�"
                        THEN "Y"
                        ELSE ""
          .
       END.
   END.
   RETURN vResult.
END FUNCTION.


{pfuncdef 
   &DEFPROC="BKIGetRejAmount"
   &DESCRIPTION="��।������ ��ࠬ��஢ �⪫������� ��� ��� ���"
   &PARAMETERS="BUFFER boan,����,�����"
   &RESULT="YES/NO"
   &SAMPLE="BKIGetRejAmount(BUFFER vLoan, end-date, TIME, 
                            OUTPUT vAmt, OUTPUT vAmCurr, OUTPUT vRejDate, OUTPUT vRejReason)"}
FUNCTION BKIGetRejAmount RETURNS LOGICAL 
   (BUFFER vLoan     FOR loan,
     INPUT iDatePrev  AS DATE,
     INPUT iTimePrev  AS INT64,
    OUTPUT oAmt       AS DECIMAL,
    OUTPUT oAmCurr    AS CHARACTER,
    OUTPUT oRejDate   AS DATE,
    OUTPUT oRejReason AS CHARACTER):

   DEFINE VARIABLE vContCli AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vBegStat AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vFinStat AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vResult  AS LOGICAL   NO-UNDO.

   DEFINE BUFFER  term-obl FOR term-obl.
   DEFINE BUFFER xterm-obl FOR term-obl.
   DEFINE BUFFER      code FOR code.
          
   IF AVAILABLE vLoan AND 
      vLoan.class-code =  "precrd_loan" THEN 
   DO: 
       loopcode:
       FOR EACH code NO-LOCK
          WHERE code.class =  "����_����_����" 
            AND code.val   =  "�⪠�" :

          vContCli = ENTRY(1,code.code).
          IF NUM-ENTRIES(code.code) >= 2 THEN
          vBegStat = ENTRY(1,ENTRY(2,code.code),"_").
          IF NUM-ENTRIES(code.code) >= 2 AND
             NUM-ENTRIES(ENTRY(2,code.code),"_") >= 2 THEN
          vFinStat = ENTRY(2,ENTRY(2,code.code),"_").

          IF vLoan.cont-cli =  vContCli THEN
          DO:
             FIND LAST term-obl OF vLoan 
                 WHERE term-obl.idnt      =  1000000 
                   AND term-obl.Cont-type =  vBegStat
                   AND term-obl.Symbol    =  vFinStat NO-LOCK NO-ERROR.
             IF AVAIL term-obl
                AND ( iDatePrev         =  ?
                      OR
                      term-obl.end-date >  iDatePrev 
                      OR
                      term-obl.end-date =  iDatePrev AND
                      term-obl.fop      >  iTimePrev
                    ) THEN
             DO:
                 FIND FIRST xterm-obl
                      WHERE xterm-obl.contract  =  vLoan.contract
                        AND xterm-obl.cont-code =  vLoan.cont-code
                        AND xterm-obl.end-date  =  vLoan.open-date 
                        AND xterm-obl.idnt      =  2 
                        AND xterm-obl.nn        =  1 NO-LOCK NO-ERROR.
                 
                 ASSIGN 
                    vResult    = YES
                    oAmt       = IF AVAIL xterm-obl THEN xterm-obl.amt-rub
                                                    ELSE 0
                    oAmCurr    = IF vLoan.currency =  "" THEN "810" 
                                                         ELSE vLoan.currency
                    oRejDate   = term-obl.end-date
                    oRejReason = GetXattrValueEx("loan",
                                                 vLoan.contract + "," + 
                                                 vLoan.cont-code,
                                                 "����_����",
                                                 "5")
                 .     
                 LEAVE loopcode.
             END.
          END.
       END.
   END.
   RETURN vResult.
END FUNCTION.


/* ��।������ �ਧ���� ��䮫� */
{pfuncdef 
   &DEFPROC="IsDefaultPayment"
   &DESCRIPTION="��।������ �ਧ���� ��䮫� "
   &PARAMETERS="BUFFER boan,����,�����.����,�-�� �ய�᪮�"
   &RESULT="YES/NO"
   &SAMPLE="IsDefaultPayment(BUFFER bLoan, end-date, 120, 2)"}
FUNCTION IsDefaultPayment RETURNS LOGICAL
   (BUFFER vLoan     FOR loan,
    INPUT iDate     AS DATE,
    INPUT iTermDays AS INT64,
    INPUT iCnt      AS INT64):
       
   DEFINE BUFFER term-obl FOR term-obl.
   
   DEFINE VARIABLE vRes   AS LOGICAL NO-UNDO INIT ?.
   DEFINE VARIABLE vSumma AS DECIMAL NO-UNDO.
   DEFINE VARIABLE vCnt   AS INT64   NO-UNDO.
   
   IF AVAILABLE vLoan THEN 
   LOOP:
   DO: 
      vRes = YES.
      vCnt = 0.
      FOR EACH term-obl WHERE
               term-obl.contract  =  vLoan.contract
           AND term-obl.cont-code =  vLoan.cont-code
           AND term-obl.idnt      =   3
           AND term-obl.end-date  >= iDate - iTermDays 
           AND term-obl.end-date  <= iDate
      NO-LOCK:
         RUN summ-t.p (OUTPUT vSumma,
                       vLoan.contract,
                       vLoan.cont-code,
                       RECID(term-obl),
                       iDate).
         IF vSumma >  0 THEN vCnt = vCnt + 1.
         IF vCnt >  iCnt THEN LEAVE LOOP.
      END.
      vCnt = 0.
      FOR EACH term-obl WHERE
               term-obl.contract  =  vLoan.contract
           AND term-obl.cont-code =  vLoan.cont-code
           AND term-obl.idnt      =  1
           AND term-obl.end-date  >= iDate - iTermDays
           AND term-obl.end-date  <= iDate
      NO-LOCK:
         RUN summ-t1.p (OUTPUT vSumma,RECID(term-obl),RECID(vLoan)).
         IF vSumma >  0 THEN vCnt = vCnt + 1.
         IF vCnt >  iCnt THEN LEAVE LOOP.
      END.
      vRes = NO.
   END.
   RETURN vRes.
END FUNCTION.

/* ������ ४����� �� ���� �� �����䨪��� ����_�������� 
   ��� ���ᠭ�� ᢮�� �㭪権 */
{pfuncdef 
   &DEFPROC="BKIGetXAttrByCode"
   &DESCRIPTION="������ ४����� �� ���� �� �����䨪��� ����_��������~
��� ���ᠭ�� ᢮�� �㭪権"
   &PARAMETERS="BUFFER boan,��� ��-�"
   &RESULT="���祭�� ४�����"
   &SAMPLE="BKIGetXAttrByCode(BUFFER vLoan,"1")"}
FUNCTION BKIGetXAttrByCode RETURNS CHARACTER  
   (BUFFER vLoan     FOR loan,
    INPUT  iCode     AS  CHARACTER):
   
   DEFINE VARIABLE vRes  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vCode AS CHARACTER NO-UNDO.
   
   CASE iCode:
      WHEN "" THEN
      DO: /* �����誠 ��� ��᫥���饩 ��ࠡ�⪨ */
         vRes = "".
      END.         
      OTHERWISE 
      DO: /* ���祭�� ४����� �������, �� ���� �����䨪��� ����_��������
             �롨ࠥ��� ��� ४����� */
         vCode = GetCode("����_��������",iCode).
         vRes = GetXAttrValueEx("loan",
                                vLoan.contract + "," + vLoan.cont-code,
                                vCode,
                                GetValue(BUFFER vLoan:HANDLE,vCode)
                               ).
      END.
   END CASE.
       
   RETURN vRes.
END FUNCTION.
 
{pfuncdef 
   &DEFPROC="GetLoanEPS"
   &DESCRIPTION="����祭�� ��� �� ��������"
   &PARAMETERS="���������� ��������,������������� ��������,����"
   &RESULT="���"
   &SAMPLE="GetLoanEPS('�।��','123',01/01/2015,OUTPUT oEps)"}
PROCEDURE GetLoanEps:
   DEF INPUT  PARAM iContract AS CHAR NO-UNDO.
   DEF INPUT  PARAM iContCode AS CHAR NO-UNDO.
   DEF INPUT  PARAM iDate     AS DATE NO-UNDO.
   DEF OUTPUT PARAM oEps      AS DEC  NO-UNDO.
   
   DEF VAR vAddPay   AS LOG  NO-UNDO.
   
   /* ᭠砫� ��⠥��� ������� ��� � �� �� ������ */
   oEps = DEC(GetXAttrValueEx("loan",iContract + "," + iContCode,"���","0")).
   IF oEps =  0 THEN
   DO:
      RUN pGetEpsLoan IN h_loan (iContract, 
                                 iContCode, 
                                 iDate,
                                 OUTPUT oEps,
                                 OUTPUT vAddPay).  
      oEps = oEps * 100.
   END.
END PROCEDURE.

{pfuncdef 
   &DEFPROC="GetFinalDate"
   &DESCRIPTION="����祭�� ���� 䠪.�ᯮ������ ��易⥫��� �� ��������"
   &PARAMETERS="BUFFER loan,����"
   &RESULT="��� 䠪��᪮�� �ᯮ������ ��易⥫���"
   &SAMPLE="GetLoanEPS(BUFFER Loan,01/01/2015,OUTPUT oFinDate)"}
PROCEDURE GetFinalDate:
   DEF PARAM  BUFFER loan FOR loan.               /* �।��� �������        */
   DEF INPUT  PARAM  iDate    AS DATE NO-UNDO.  /* ��� ����           */
   DEF OUTPUT PARAM  oFinDate AS DATE NO-UNDO.  /* ��� 䠪��᪮�� �ᯮ������ ��易⥫��� */

   DEF VAR vListCode AS CHAR  NO-UNDO 
       INIT "������,�����,��������,��������,����몏�,����몏�,���,�����".
   DEF VAR vCnt      AS INT64 NO-UNDO.
   DEF VAR vCnt1     AS INT64 NO-UNDO.

   DEF BUFFER code     FOR code.
   DEF BUFFER loan-int FOR loan-int.
   
   /* ��室�� ���� ��᫥����� ᯨᠭ�� �� ��ࠬ��ࠬ �� ����_������� */
   DO vCnt = 1 TO NUM-ENTRIES(vListCode):
      FOR EACH code WHERE code.class =  "����_�������"
                      AND code.code  =  ENTRY(vCnt,vListCode)
      NO-LOCK:
         DO vCnt1 = 1 TO NUM-ENTRIES(code.misc[3]):
            FIND LAST loan-int WHERE loan-int.contract  =  loan.contract
                                 AND loan-int.cont-code =  loan.cont-code
                                 AND loan-int.id-d      =  INT64(ENTRY(vCnt1,code.misc[3]))
            NO-LOCk NO-ERROR.
            IF AVAIL loan-int THEN
            DO:
               IF oFinDate =  ? THEN
                  oFinDate = loan-int.mdate.
               ELSE 
                  oFinDate = MAX(oFinDate,loan-int.mdate).
            END.
         END.
      END.
   END.
   
END PROCEDURE.
/******************************************************************************/
/* $LINTFILE='pp-bki.p' */
/* $LINTMODE='11,-9,6,3' */
/* $LINTENV ='common' */
/* $LINTVSS ='$/ws3-dpl/common/bq/' */
/* $LINTUSER='seei' */
/* $LINTDATE='07/04/2017 12:16:13.518+03:00' */
/*prosignOOq5JdgNSEbc7yj+ocs0sg*/