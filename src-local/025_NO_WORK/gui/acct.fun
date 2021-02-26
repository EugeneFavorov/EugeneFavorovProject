
/* +++ acct.fun was humbly modified by (c)blodd converter v.1.09 on 9/19/2016 7:55am +++ */

/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2000 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: ACCT.FUN
      Comment: ������, ᮤ�ঠ騩 �孮�����᪨� ����७���
               �㭪樨 ࠡ��� � ��楢묨 ��⠬�
   Parameters:
         Uses:
      Used by:
      Created: 10/03/00 Serge
     Modified: 13/03/00 SSV ���� buf-acct ����� �㭪権
     Modified: 17/05/02 Gunk �㭪�� AcctWhoCreated
     Modified: 21/01/03 SAP  ��� 9741 
     Modified: 25/02/2005 abko 0042965 GetBlockPosition - �����頥� �����஢�����
                                       �㬬� �� ���
     Modified: 25/02/2005 abko 0045485 IsRedSaldo - �����頥� ��᭮� ᠫ줮 �� ���
                                       � ���� �� ������ ��� ���������
     Modified: 21/09/2007 muta 0080056 ��� ��� ���������� �������஢����� �㬬
                                       � GetBlockPosition
     Modified: 21/09/2007 muta 0081914 �����஢���� �����஢�� �㬬� ��� ��।���⥩,
                                       㪠������ � ����� "�����㬬" �����䨪��� acct-status                                        

*/
Form "~n@(#) ACCT.FUN 1.0 Serge 10/03/00 Serge 10/03/00 comment"
with frame sccs-id stream-io width 250.

{lim-pos.i}
{intrface.get blkob}
{intrface.get cust}
{intrface.get db2l}
{intrface.get strng}
{intrface.get xclass}
{intrface.get tmess}

/* �஢�ઠ �� �������� ��楢��� ���. �����頥� yes, no, ? */
/* ��� �஢�ન �� ���� ������ */
FUNCTION CheckOpenAcct RETURN LOG
    (INPUT cAcct AS CHAR,
     INPUT cCurrency AS CHAR,
     INPUT dOpDate AS DATE):

  def buffer fun-acct for acct.

   {find-act.i
      &bact = fun-acct
      &acct = cAcct
   } 
   /* � �����⢥���� ����, ��� ��뢠���� ��� �����㬥�� (mn2cycle.i) ����� ��।������ �� 6-8 ����樨 ���
      �� �� �㤥� ࠡ���� � ���⨭�� � ��������� ���.*/
   return if not avail fun-acct then ?
          else if fun-acct.close-date eq ? then yes
          else fun-acct.close-date > dOpDate.
END.

/* �஢�ઠ �� ��������� ��楢��� ���. �����頥� yes, no, ? */
/* ��� �஢�ન �� ���� ������ */
FUNCTION CheckCloseAcct RETURN LOG
    (INPUT cAcct AS CHAR,
     INPUT cCurrency AS CHAR,
     INPUT dOpDate AS DATE):

  def buffer fun-acct for acct.

     {find-act.i
        &bact = fun-acct
        &acct = cAcct
        &curr = cCurrency
     } 
   return if not avail fun-acct then ?
          else if fun-acct.close-date = ? then no
          else fun-acct.close-date <= dOpDate.
END.
/* ��� ���㤭���, ����襣� ��� */
FUNCTION AcctWhoCreated RETURNS CHARACTER
   (INPUT in-acct as char,
    INPUT in-curr as char):
   
   DEFINE VARIABLE WhomCre AS CHARACTER  NO-UNDO.
   DEFINE BUFFER fun-acct FOR acct.
   /* ���砫� �� */
   WhomCre = GetXAttrValue("acct",in-acct + "," + in-curr,"���������").
   /* ��⮬ History */
   IF WhomCre EQ "" THEN
      FOR FIRST history WHERE
                history.FILE-NAME EQ "acct"
            AND history.field-ref EQ in-acct + "," + in-curr
            AND history.modify EQ "C" NO-LOCK:
          WhomCre = history.user-id.  
      END.
   /* �᫨ ��� �� ⮣�, �� ��㣮��, ᬮ�ਬ �⢥��⢥����� */
   IF WhomCre EQ "" THEN DO:
      {find-act.i
         &bact = fun-acct
         &acct = in-acct
         &curr = in-curr
      }
      IF AVAIL fun-acct THEN
         WhomCre = fun-acct.user-id.
   END.
   
   RETURN WhomCre.
END FUNCTION.

/* �����㬥�� ��� ��।������ ������᪨� ४����⮢ ����७���� ���
���⠭���� � 0044276
*/
PROCEDURE GetCustIdCli.

   DEFINE INPUT  PARAMETER iAcctSurr AS CHAR.
   DEFINE OUTPUT PARAMETER oCliCat   AS CHAR.
   DEFINE OUTPUT PARAMETER oCliID    AS INT64.

   ASSIGN
      oCliCat = GetXAttrValueEx("acct",
                                iAcctSurr,
                                "�����",
                                "")
      oCliID  = INT64(GetXAttrValueEx("acct",
                                        iAcctSurr,
                                        "IDCust",
                                        ?))
   .

END PROCEDURE.

/*************************************************************************************************/

/* ��।���� ��࠭� ������ ���
*/
PROCEDURE GetCountryIdCli.

   DEFINE INPUT  PARAMETER iAcct      AS CHAR.
   DEFINE INPUT  PARAMETER iCurr      AS CHAR.
   DEFINE OUTPUT PARAMETER oCountryID AS CHAR.
   DEFINE OUTPUT PARAMETER oCustCat   AS CHAR.

   DEFINE BUFFER tAcct FOR acct.
   
   {find-act.i
      &bact = tAcct
      &acct = iAcct
      &curr = iCurr
   }
   IF NOT AVAIL tAcct THEN
   DO:
      oCountryID = "".
      RETURN "1".
   END.
   oCustCat = tAcct.cust-cat.
   IF tAcct.cust-cat EQ "�" THEN
   DO:
      oCountryID = "".
      RETURN "2".
   END.
   oCountryID = GetXAttrValueEx("acct",
                                tAcct.Acct + "," + tAcct.Currency,
                                "��࠭�",
                                "").
   IF oCountryID EQ "" THEN
   DO:
      CASE tAcct.cust-cat:         /* ��ࠡ�⪠ �� ⨯� ������ */
         WHEN "�" THEN
         DO:                       /* �ਤ��᪨� ���          */
            FIND FIRST cust-corp WHERE cust-corp.cust-id EQ tAcct.cust-id NO-LOCK NO-ERROR.
            IF AVAIL cust-corp THEN
               oCountryID = IF cust-corp.country-id EQ ? THEN ""
                                                         ELSE cust-corp.country-id.
         END.
         WHEN "�" THEN
         DO:                       /* �����᪨� ���           */
            FIND FIRST person WHERE person.person-id EQ tAcct.cust-id NO-LOCK NO-ERROR.
            IF AVAIL person THEN
               oCountryID = IF person.country-id EQ ? THEN ""
                                                      ELSE person.country-id.
         END.
         WHEN "�" THEN
         DO:                      /* �।��� �࣠����樨      */
            FIND FIRST banks WHERE banks.bank-id EQ tAcct.cust-id NO-LOCK NO-ERROR.
            IF AVAIL banks THEN
               oCountryID = IF banks.country-id EQ ? THEN ""
                                                     ELSE banks.country-id.
         END.
      END.
   END.
   RETURN "0".

END PROCEDURE.

/*************************************************************************************************/

/* �����㬥�� ��� ��।������ ��᭮�� ᠫ줮 ��� �� �������� ���� �� ���� ����
���⠭���� � 0045485
*/

&IF DEFINED(NotDefShTempNew) = 0 &THEN

   &GLOB NotDefShTempNew YES

   {sh-temp.i "new"}

&ENDIF

PROCEDURE IsRedSaldo.

   DEFINE INPUT  PARAMETER iAcct      AS CHARACTER. /* */
   DEFINE INPUT  PARAMETER iCurr      AS CHARACTER.
   DEFINE INPUT  PARAMETER iBegDate   AS DATE.
   DEFINE INPUT  PARAMETER iEndDate   AS DATE.
   DEFINE INPUT  PARAMETER iStatus    AS CHARACTER.
   DEFINE INPUT  PARAMETER iOpRecid   AS RECID.
   DEFINE INPUT  PARAMETER iAndLimit  AS LOGICAL.
   DEFINE INPUT  PARAMETER iAndBlock  AS LOGICAL.
   DEFINE OUTPUT PARAMETER oSumSaldo  AS DECIMAL.
   DEFINE OUTPUT PARAMETER oSaldoDate AS DATE.

   DEFINE VARIABLE vTmpDate AS DATE    NO-UNDO.
   DEFINE VARIABLE vSumOpR  AS DECIMAL NO-UNDO.
   DEFINE VARIABLE vSumOpV  AS DECIMAL NO-UNDO.
   DEFINE VARIABLE vlim-pos AS DECIMAL INIT 0 NO-UNDO.
   DEFINE VARIABLE vmbl-pos AS DECIMAL INIT 0 NO-UNDO.
   
   DEFINE BUFFER top FOR op.
   DEFINE BUFFER top-entry FOR op-entry.
   DEFINE BUFFER dacct FOR acct.

   ASSIGN
      vSumOpR = 0
      vSumOpV = 0
   .

   {find-act.i
      &bact = dacct
      &fila = LAST
      &acct = iAcct
      &curr = iCurr
   }
   IF iOpRecid NE ? THEN /* �᫨ �㦭� ������ �஢���� �� ���㬥��� */
   DO:
      FIND FIRST top WHERE RECID(top) EQ iOpRecid NO-LOCK NO-ERROR.
      IF top.op-status GE iStatus THEN /* ⮫쪮 �᫨ ����� ���㬥�� �ਭ������� � ���� */
         FOR EACH top-entry OF top NO-LOCK:
            IF top-entry.acct-db EQ iAcct THEN
               ASSIGN
                  vSumOpR = vSumOpR - top-entry.amt-rub
                  vSumOpV = vSumOpV - top-entry.amt-cur
               .
            IF top-entry.acct-cr EQ iAcct THEN
               ASSIGN
                  vSumOpR = vSumOpR + top-entry.amt-rub
                  vSumOpV = vSumOpV + top-entry.amt-cur
               .
         END.
   END.
   IF iEndDate EQ ? THEN
   DO:
      FIND LAST op-date NO-LOCK NO-ERROR.
      iEndDate = op-date.op-date.
   END.

   RUN apos-sh.p(iacct,iCurr,iBegDate,iEndDate,iStatus).

   FOR EACH sh NO-LOCK,                                   /* �� �ᥬ ��⠬, �� ����� �뫨 */
       FIRST op-entry WHERE op-entry.acct-db   EQ iAcct   /* �������� �� ����              */
                        AND op-entry.op-date   EQ sh.since
                        AND op-entry.op-status GE iStatus NO-LOCK:
      vTmpDate = sh.since.

      IF iAndBlock THEN
         vmbl-pos = GetBlockPosition (iAcct,
                                      iCurr,
                                      "",
                                      vTmpDate).
      IF iAndLimit THEN
         vlim-pos = GetLimitPosition (BUFFER dacct, vTmpDate).
      vmbl-pos = vmbl-pos + vlim-pos.
      
      IF   (    iCurr EQ ""
            AND (   (sh.bal + vSumOpR > vmbl-pos AND dacct.side = "�")
                 OR (sh.bal + vSumOpR < vmbl-pos AND dacct.side = "�")))
         OR (   iCurr NE ""
            AND (   (sh.val + vSumOpV > vmbl-pos AND dacct.side = "�")
                 OR (sh.val + vSumOpV < vmbl-pos AND dacct.side = "�"))) THEN
      DO:
         ASSIGN
            oSumSaldo = IF iCurr EQ "" THEN sh-bal + vSumOpR ELSE sh-val + vSumOpV
            oSaldoDate = vTmpDate
         .
         LEAVE.
      END.
   END.

END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* ��� ��ࢮ�� �������� �� ����                                             */
/*----------------------------------------------------------------------------*/
FUNCTION getFirstMove RETURNS DATE 
   (INPUT iAcct AS CHAR,
    INPUT iCurr AS CHAR):
   
   DEFINE VARIABLE vRetVal AS DATE       NO-UNDO.
   DEFINE BUFFER op-entry FOR op-entry.
   
   FOR FIRST op-entry WHERE
             op-entry.acct-cr   EQ iAcct
         AND op-entry.op-status GE gOp-Status
      NO-LOCK:
      /* �।�⮢�� �������� */
      vRetVal = op-entry.op-date.         
   END.
   
   FOR FIRST op-entry WHERE
             op-entry.acct-db   EQ iAcct
         AND op-entry.op-status GE gOp-Status
      NO-LOCK:
      /* ����⮢�� �������� */
      vRetVal = IF vRetVal EQ ? 
                THEN op-entry.op-date        
                ELSE MIN (vRetVal, op-entry.op-date).
   END.
   RETURN vRetVal.
END FUNCTION.

/* �����頥� ���� �ਧ��� �������� (����稥 �஢����)
** �� ���� � ��⮬ �����. */
FUNCTION GetFirtsMoveDate RETURN LOG (
   INPUT  iAcct      AS CHAR,    /* ����� ���. */
   INPUT  iBegDate   AS DATE,    /* ��� ��砫� ���ࢠ��. */
   INPUT  iEndDate   AS DATE,    /* ��� ����砭�� ��������. */
   INPUT  iStatus    AS LOG      /* YES - ����� ����� ��릠 (��楯�). */
):
   DEF BUFFER op-entry FOR op-entry. /* ���������� ����. */

   /* ���� �।�⮢��� ��������. */
   IF iStatus
      THEN FIND FIRST op-entry WHERE 
               op-entry.acct-cr  EQ iAcct
         AND   (  iBegDate          EQ ?
            OR    op-entry.op-date  GT iBegDate)
         AND   op-entry.op-date  LE iEndDate
         AND   op-status         GE "�"
      NO-LOCK NO-ERROR.
      ELSE FIND FIRST op-entry WHERE 
               op-entry.acct-cr  EQ iAcct
         AND   (  iBegDate          EQ ?
            OR    op-entry.op-date  GT iBegDate)
         AND   op-entry.op-date  LE iEndDate
         AND   op-status         LT "�"
      NO-LOCK NO-ERROR.
                        /* ���� ����⮢��� ��������. */
   IF NOT AVAIL op-entry
      THEN IF iStatus
         THEN FIND FIRST op-entry WHERE 
                  op-entry.acct-db  EQ iAcct
            AND   (  iBegDate          EQ ?
               OR    op-entry.op-date  GT iBegDate)
            AND   op-entry.op-date  LE iEndDate
            AND   op-status         GE "�"
         NO-LOCK NO-ERROR.
         ELSE FIND FIRST op-entry WHERE 
                  op-entry.acct-db  EQ iAcct
            AND   (  iBegDate          EQ ?
               OR    op-entry.op-date  GT iBegDate)
            AND   op-entry.op-date  LE iEndDate
            AND   op-status         LT "�"
         NO-LOCK NO-ERROR.
   RETURN AVAIL op-entry.
END FUNCTION.


/* �஢�ઠ, ���� �� ��� ��⮬ �����. �����頥� yes, no */
FUNCTION IsAcctKassa RETURN LOG
    (INPUT cAcct AS CHAR,
     INPUT cCurrency AS CHAR):
   
   DEFINE BUFFER b_acct FOR acct.
   DEFINE VAR vKassContract AS CHARACTER NO-UNDO.
   vKassContract = FGetSetting ("�����犠�",?,"").
   
   {find-act.i
      &bact = b_acct
      &acct = cAcct
      &curr = cCurrency
   } 
   RETURN AVAIL b_acct AND CAN-DO(vKassContract,b_acct.contract).
END.

/* �����頥� ᢮��� ��� �� �����䨪��� �����
*/
FUNCTION GetSvodAcctCode RETURN CHARACTER(
   INPUT  iBalAcct   AS INT64,    /* ��� ���. */
   INPUT  iCurrency  AS CHARACTER, /* ����� ��� */
   INPUT  iProduct   AS CHARACTER, /* ��� �த��. */
   INPUT  iAcctType  AS CHARACTER, /* ���� ��� */
   INPUT  iBranchId  AS CHARACTER  /* ��� ���ࠧ������ */
   ):

   DEFINE VARIABLE vRetAcct AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vClass AS CHARACTER   NO-UNDO.
   
   DEFINE BUFFER   bcode FOR code.
   DEFINE BUFFER   bsigns FOR signs.

   ASSIGN
      vRetAcct = "NO"
      vClass = "�����".

   FOR EACH bcode 
      WHERE bcode.PARENT EQ vClass
      AND bcode.description[3] EQ iProduct
      NO-LOCK:
      IF GetXAttrValueEx("code",
                         bcode.class + "," + bcode.code,
                         "mAcct2nd",
                         "NO"
                         ) EQ STRING(iBalAcct)
         THEN
         IF GetXAttrValueEx("code",
                            bcode.class + "," + bcode.code,
                            "mCurrency",
                            "NO"
                            ) EQ iCurrency
            THEN
            IF CAN-DO(GetXAttrValueEx("code",
                                      bcode.class + "," + bcode.code,
                                      "maccttype",
                                      "NO"
                                       ),iAcctType)
               THEN
               IF GetXAttrValueEx("code",
                                  bcode.class + "," + bcode.code,
                                  "mBranchCode",
                                  "NO"
                                  ) EQ iBranchId
                  THEN
               DO:
                  vRetAcct = bcode.code.
                  LEAVE.
               END.
   END. /* FOR EACH: */

   RETURN vRetAcct.
   
END FUNCTION.

/* ���������� ����� � ᯨ�� � ���� �㭪樨, � �� ����� */
FUNCTION addItem RETURN CHARACTER (INPUT iList AS CHARACTER,
                                   INPUT iItem AS CHARACTER,
                                   INPUT iSep  AS CHARACTER):
   RETURN iList + ( IF NUM-ENTRIES(iList, iSep) = 0 THEN "" ELSE iSep) + iItem.
END FUNCTION.

/*
   ������ ���祭�� �ࠣ���� ��᪨ ���� �� �᭮����� ��������
   ��ப�, �������騥 ᨬ���� ���������� �� ᨬ��� �� 㬮�砭��.
   �ࠣ���� ��᪨ ���� - �� ���� ��᫥����⥫쭮��� ���������� ᨬ�����,
   ���� ᨬ��� � 㪠������ ��������� �� ��ப�-���筨��.
*/
FUNCTION EvaluateAcctMaskFragment RETURN CHARACTER (INPUT iFragment AS CHARACTER,
                                                    INPUT iValue    AS CHARACTER,
                                                    INPUT iDefault  AS CHARACTER):
   DEFINE VARIABLE vStart AS INT64 NO-UNDO INITIAL -1.
   DEFINE VARIABLE vEnd   AS INT64 NO-UNDO INITIAL -1.

   IF LENGTH(iFragment) > 0 THEN DO:
      IF TRIM(iFragment, SUBSTRING(iFragment, 1, 1)) = "" THEN
         ASSIGN
            vStart = 1
            vEnd   = LENGTH(iFragment)
         .
      ELSE
         RUN ParseIntRange IN h_strng (SUBSTRING(iFragment, 2),
                                       "[]-",
                                       OUTPUT vStart,
                                       OUTPUT vEnd).
   END.
   IF vStart = -1 OR vEnd = -1 THEN
      RETURN ?.
   RETURN GetValueRange(iValue, vStart, vEnd, iDefault).
END FUNCTION.

/*
   �������� ������ ���� ���� (�� ᮤ�ঠ��� ����������) �� �ࠣ�����,
   �����頥� ᯨ᮪ �ࠣ���⮢ � ������� ࠧ����⥫��.
   ��. �������਩ � EvaluateAcctMaskFragment.
*/
FUNCTION SplitSimpleAcctMask RETURN CHARACTER (INPUT iAcctMask AS CHARACTER,
                                               INPUT iSep      AS CHARACTER):
   DEFINE VARIABLE vI      AS INT64     NO-UNDO.
   DEFINE VARIABLE vPrev   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vCurr   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vResult AS CHARACTER NO-UNDO.

   IF NOT {assigned iAcctMask} THEN
      RETURN iAcctMask.
   DO vI = 1 TO LENGTH(iAcctMask):
      vPrev = vCurr.
      vCurr = SUBSTRING(iAcctMask, vI, 1).
      vResult = vResult + vPrev.
      IF vCurr <> vPrev THEN
         vResult = vResult + iSep.
   END.
   RETURN SUBSTRING(vResult, 1 + LENGTH(iSep)) + vCurr.
END FUNCTION.

/*
   �������� ���� ���� �� �ࠣ�����,
   �����頥� ᯨ᮪ �ࠣ���⮢ � ������� ࠧ����⥫��.
   ��. �������਩ � EvaluateAcctMaskFragment.
*/
FUNCTION SplitAcctMask RETURN CHARACTER (INPUT iAcctMask AS CHARACTER,
                                         INPUT iSep      AS CHARACTER):
   DEFINE VARIABLE vI      AS INT64     NO-UNDO INITIAL 1.
   DEFINE VARIABLE vJ      AS INT64     NO-UNDO INITIAL 1.
   DEFINE VARIABLE vS      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vResult AS CHARACTER NO-UNDO.

   IF NOT {assigned iAcctMask} THEN
      RETURN iAcctMask.
   DO WHILE 0 < vI AND vI < LENGTH(iAcctMask):
      vI = INDEX(iAcctMask, "[", vI).
      IF vI > 0 THEN DO:
         IF vI = vJ THEN
            RETURN ?.
         vS = SUBSTRING(iAcctMask, vJ, vI - vJ - 1).
         IF vS > "" THEN
            vResult = addItem(vResult, SplitSimpleAcctMask(vS, iSep), iSep).
         vJ = INDEX(iAcctMask, "]", vI) + 1.
         IF vJ = 1 THEN
            RETURN ?.
         vS = SUBSTRING(iAcctMask, vI - 1, 1 + vJ - vI).
         vResult = addItem(vResult, vS, iSep).
         vI = vJ + 1.
      END.
   END.
   vS = SUBSTRING(iAcctMask, vJ).
   RETURN IF vS = ""
          THEN vResult
          ELSE addItem(vResult, SplitSimpleAcctMask(vS, iSep), iSep).
END FUNCTION.

/*
   �믮���� ����⠭���� ���祭�� ��� ��� �宦����� ��������� ᨬ���� ��᪨
   ����. ��ࠬ��� iPositional ������ ����室������ ��᫥������� ����樨 ᨬ����
   � ��᪥ � �롮� ���祭�� ᮣ��᭮ �� (�� ����� ��᫠ ��� ᨬ�����, �
   ������ � 㪠��� ��������). �������騥 ����樨 ���������� �� ᨬ���
   �� 㬮�砭�� iDefault.
*/
FUNCTION EvaluateAcctMask RETURN CHARACTER (INPUT iAcctMask   AS CHARACTER,
                                            INPUT iChar       AS CHARACTER,
                                            INPUT iPositional AS LOGICAL,
                                            INPUT iValue      AS CHARACTER,
                                            INPUT iDefault    AS CHARACTER):
   DEFINE VARIABLE vFragments AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vI         AS INT64     NO-UNDO.
   DEFINE VARIABLE vJ         AS INT64     NO-UNDO INITIAL 1.
   DEFINE VARIABLE vC         AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vS         AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vResult    AS CHARACTER NO-UNDO.

   IF LENGTH(iChar) <> 1 THEN
      RETURN ?.
   vFragments = SplitAcctMask(iAcctMask, CHR(1)).
   IF NOT {assigned vFragments} THEN
      RETURN ?.
   DO vI = 1 TO NUM-ENTRIES(vFragments, CHR(1)):
      vS = ENTRY(vI, vFragments, CHR(1)).
      IF NOT {assigned vS} THEN
         RETURN ?.
      vC = SUBSTRING(vS, 1, 1).
      IF vC = iChar THEN DO:
         vS = EvaluateAcctMaskFragment(vS, SUBSTRING(iValue, vJ), iDefault).
      END.
      ELSE IF iPositional THEN
         vJ = vJ + LENGTH(EvaluateAcctMaskFragment(vS, "", "x")).
      vResult = vResult + vS.
   END.
   RETURN vResult.
END FUNCTION.

/*
   �뤥����� � ��᪥ ����� ��� ������� ����� ��⮢ ��।��񭭮�
   ��㯯� ᨬ�����. ��⠫�� ᨬ���� ���������� �� �������.
*/
FUNCTION EmphasizeMaskSymbol RETURN CHARACTER (INPUT iClassCode AS CHARACTER,
                                               INPUT iSymbol    AS CHARACTER,
                                               INPUT iCover     AS CHARACTER):
   DEFINE VARIABLE vSep    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vMask   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vGroups AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vChars  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vG      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vC      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vI      AS INT64     NO-UNDO.

   IF LENGTH(iSymbol) = 1 AND
      LENGTH(iCover)  = 1
   THEN DO:
      ASSIGN
         vSep  = IF iCover = CHR(2)
                 THEN CHR(3)
                 ELSE CHR(2)
         vMask = GetXAttrInit(iClassCode, "acct")
      .
      IF {assigned vMask} THEN DO:
         vGroups = SplitAcctMask(vMask, vSep).
         DO vI = 1 TO NUM-ENTRIES(vGroups, vSep):
            vG = ENTRY(vI, vGroups, vSep).
            IF {assigned vG} THEN DO:
               vC = SUBSTRING(vG, 1, 1).
               IF vC <> iSymbol THEN
                  vChars = addItem(vChars, vC, vSep).
            END.
         END.
         DO vI = 1 TO NUM-ENTRIES(vChars, vSep):
            vC = ENTRY(vI, vChars, vSep).
            vMask = EvaluateAcctMask(vMask, vC, NO, iCover, iCover).
         END.
         RETURN vMask.
      END.
   END.
   RETURN ?.
END FUNCTION.

/*
   �८�ࠧ�� ���� ����, ��⠢��� � ��� ⮫쪮 �ࠣ����� ��������� ⨯�,
   ������� ��� ��⠫쭮� �� ������� ᨬ���.
*/
FUNCTION FilterAcctMask RETURN CHARACTER (INPUT iAcctMask AS CHARACTER,
                                          INPUT iSymbol   AS CHARACTER,
                                          INPUT iCover    AS CHARACTER):
   DEFINE VARIABLE vSep       AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vFragments AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vI         AS INT64     NO-UNDO.
   DEFINE VARIABLE vS         AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vResult    AS CHARACTER NO-UNDO.

   vSep = IF iCover = CHR(2)
          THEN CHR(3)
          ELSE CHR(2).
   vFragments = SplitAcctMask(iAcctMask, vSep).
   DO vI = 1 TO NUM-ENTRIES(vFragments, vSep):
      vS = ENTRY(vI, vFragments, vSep).
      IF NOT (vS BEGINS iSymbol) THEN
         vS = EvaluateAcctMaskFragment(vS, "", iCover).
      vResult = vResult + vS.
   END.
   RETURN vResult.
END FUNCTION.

/*
   ��ਠ�� EvaluateAcctMask � ��ࠢ�������� �� �ࠢ��� ���.
   �������騥 ����樨 ���������� �� ᨬ���, 㪠����� � iFill.
*/
FUNCTION EvaluateAcctMaskR RETURN CHARACTER (INPUT iAcctMask   AS CHARACTER,
                                             INPUT iChar       AS CHARACTER,
                                             INPUT iPositional AS LOGICAL,
                                             INPUT iValue      AS CHARACTER,
                                             INPUT iFill       AS CHARACTER,
                                             INPUT iDefault    AS CHARACTER):
   DEFINE VARIABLE vTmpValue AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vTmpMask  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vLen      AS INT64     NO-UNDO.

   IF LENGTH(iFill) > 1 THEN
      RETURN ?.
   vTmpMask = FilterAcctMask(iAcctMask, iChar, CHR(3)).
   vTmpValue = EvaluateAcctMask(vTmpMask, iChar, iPositional, "", CHR(2)).
   vLen = LENGTH(REPLACE(vTmpValue, CHR(3), "")).
   RETURN EvaluateAcctMask(iAcctMask,
                           iChar,
                           iPositional,
                           GetValueRange(iValue,
                                         LENGTH(iValue) - vLen + 1,
                                         LENGTH(iValue),
                                         iFill),
                           iDefault).
END FUNCTION.

/* �஢�ઠ, �室�� �� ᨬ��� � ���� ��⮢ ������� ����� */
FUNCTION MaskSupportsSymbol RETURN LOGICAL (INPUT iClassCode AS CHARACTER,
                                            INPUT iSymbol    AS CHARACTER):
   DEFINE VARIABLE vMask AS CHARACTER NO-UNDO.

   vMask = EmphasizeMaskSymbol(iClassCode,
                               iSymbol,
                               IF iSymbol = CHR(2) THEN CHR(3) ELSE CHR(2)).
   RETURN IF vMask = ?
          THEN ?
          ELSE INDEX(vMask, iSymbol) > 0.
END FUNCTION.

/* ��।������ ᢥ����� � �������⢥ ������ ���� */
PROCEDURE GetBankrupcyInfo:
   DEFINE INPUT  PARAMETER iAcct       LIKE acct.acct     NO-UNDO.
   DEFINE INPUT  PARAMETER iCurrency   LIKE acct.currency NO-UNDO.
   DEFINE INPUT  PARAMETER iDate       AS   DATE          NO-UNDO.
   DEFINE OUTPUT PARAMETER oIsBankrupt AS   LOGICAL       NO-UNDO.
   DEFINE OUTPUT PARAMETER oStage      AS   CHARACTER     NO-UNDO.
   DEFINE OUTPUT PARAMETER oDocNum     AS   CHARACTER     NO-UNDO.
   DEFINE OUTPUT PARAMETER oEndDate    AS   DATE          NO-UNDO.

   DEFINE BUFFER acct FOR acct.

   {find-act.i &acct = iAcct
               &curr = iCurrency}
   IF AVAILABLE acct THEN DO:
      IF toLogical(GetXAttrValue("acct",
                                 Surrogate(BUFFER acct:HANDLE),
                                 "��������")) = YES
      THEN
         oIsBankrupt = YES.
      ELSE IF acct.side = "�" AND
              CAN-DO(FGetSetting("����������", "�����������", ""),
                     acct.contract)
      THEN
         RUN GetBankrupcyInfo IN h_cust (acct.cust-cat,
                                         acct.cust-id,
                                         iDate,
                                         OUTPUT oIsBankrupt,
                                         OUTPUT oStage,
                                         OUTPUT oDocNum,
                                         OUTPUT oEndDate).
   END.
   ELSE
      oIsBankrupt = ?.
END PROCEDURE.

/* ��ନ஢���� �।�०����� � ࠡ�� � ���⮬ ������-������ */
PROCEDURE NotifyBankruptAcct:
   DEFINE INPUT  PARAMETER iAcct        LIKE acct.acct     NO-UNDO.
   DEFINE INPUT  PARAMETER iCurrency    LIKE acct.currency NO-UNDO.
   DEFINE INPUT  PARAMETER iAcctCR      LIKE acct.acct     NO-UNDO.
   DEFINE INPUT  PARAMETER iDate        AS   DATE          NO-UNDO.
   DEFINE INPUT  PARAMETER iCanContinue AS   LOGICAL       NO-UNDO.

   DEFINE BUFFER acct    FOR acct.
   DEFINE BUFFER bacctcr FOR acct.
   DEFINE BUFFER code    FOR code.

   DEFINE VARIABLE vIsBankrupt AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE vStage      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vDocNum     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vEndDate    AS DATE      NO-UNDO.
   DEFINE VARIABLE vMessage    AS CHARACTER NO-UNDO.

   IF toLogical(FGetSetting("����������", "��������", "���")) <> YES THEN
      RETURN.
   {find-act.i &acct = iAcct
               &curr = iCurrency}
               
   IF AVAILABLE acct THEN DO:
      IF CAN-DO(FGetSetting("����������", "���������", ""),
                acct.contract)
      THEN
         vIsBankrupt = YES.
      ELSE DO:
/*=== �⪫�祭� �� ���᭥��� �।�����祭�� ===
         FIND FIRST code WHERE
            code.class  = "���47423" AND
            code.parent = "���47423" AND
            (ENTRY(1, code.code,"|") = bacctcr.number OR
             code.name               = bacctcr.number)
         NO-LOCK NO-ERROR.
         IF NOT AVAILABLE code THEN
*/
            RUN GetBankrupcyInfo IN THIS-PROCEDURE (acct.acct,
                                                    acct.currency,
                                                    iDate,
                                                    OUTPUT vIsBankrupt,
                                                    OUTPUT vStage,
                                                    OUTPUT vDocNum,
                                                    OUTPUT vEndDate).

      END.
   END.
   IF vIsBankrupt THEN DO:
      vMessage = SUBSTITUTE("���� &1~n������ - ������!", acct.number).
      IF {assigned vDocNum} THEN
         vMessage = SUBSTITUTE("&1~n  ����� ����: &2",
                               vMessage,
                               vDocNum).
      IF vEndDate <> ? THEN
         vMessage = SUBSTITUTE("&1~n��� �襭��: &2",
                               vMessage,
                               STRING(vEndDate, "99.99.99")).
      IF iCanContinue THEN DO:
         pick-value = "2".
         RUN Fill-SysMes IN h_tmess("",
                                    "acctr02",
                                     "3",
                                     "%s=" + vMessage).
         IF pick-value <> "1" THEN
            RETURN "CANCEL".
      END.
      ELSE DO:
         RUN Fill-SysMes IN h_tmess ("", "acctr03", "", "%s=" + vMessage).
         RETURN "CANCEL".
      END.
   END.
END PROCEDURE.
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='29/12/2015 18:37:59.422+04:00' */
/* $LINTUSER='krok' */
/* $LINTMODE='1' */
/* $LINTFILE='acct.fun' */
/*prosignymCn4wPMmdhlNCZgvUoGjQ*/
/* --- acct.fun was humbly modified by (c)blodd converter v.1.09 on 9/19/2016 7:55am --- */
