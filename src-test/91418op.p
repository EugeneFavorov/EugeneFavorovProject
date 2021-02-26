/* Глухов */
{globals.i}
{sh-defs.i}

{intrface.get tmess}
{intrface.get xclass}
{intrface.get trans}
{intrface.get pbase}

{tmprecid.def}



DEFINE BUFFER b-op FOR op.
DEFINE BUFFER b-op-entry FOR op-entry.


DEFINE VARIABLE mAcctCr   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAcctDb   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDetails  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mOsnov    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mSoob     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNom      AS INT64 NO-UNDO.
DEFINE VARIABLE mNewOpRid AS RECID NO-UNDO.
DEFINE VARIABLE mFilAcct  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mBik      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCurr     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDocType  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mSum      AS DECIMAL  NO-UNDO.
DEFINE VARIABLE mDocNum   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNeedBank AS CHARACTER NO-UNDO.
DEFINE VARIABLE mBankBik  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mBankName AS CHARACTER NO-UNDO.
DEFINE VARIABLE mBankCorr AS CHARACTER NO-UNDO.
DEFINE VARIABLE mStatus   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mBenACCt  AS CHARACTER NO-UNDO.
DEFINE VARIABLE in-op-date AS DATE NO-UNDO.
DEFINE VARIABLE most91418 AS DECIMAL NO-UNDO.
DEFINE VARIABLE most47802 AS DECIMAL NO-UNDO.
DEFINE VARIABLE mcontcode AS CHARACTER NO-UNDO.
DEFINE VARIABLE mname AS CHARACTER NO-UNDO.


{getdate.i}
in-op-date = end-date.

mSoob = "Списание приобретенных прав требований по Кредитному Договору &1 от &2 &3 номинальная стоимость приобретенных прав требования.".

FOR EACH tmprecid NO-LOCK,
   EACH loan WHERE RECID(loan) EQ tmprecid.id NO-LOCK:


   most91418 = 0.

   FOR EACH loan-acct OF loan WHERE
      CAN-DO("Кред91418",loan-acct.acct-type) 
      AND loan-acct.acct BEGINS "91418" NO-LOCK:
      mAcctCr = loan-acct.acct.
      RUN acct-pos IN h_base (loan-acct.acct,loan-acct.currency,TODAY,TODAY,?).
      most91418 = sh-bal.
   END.

   IF most91418 = 0 THEN NEXT.

   most47802 = 0.

   /* RUN dbgprint.p("1111",mcontcode). */

   FOR EACH loan-acct OF loan WHERE
      loan-acct.acct BEGINS "47802" 
      NO-LOCK:

      RUN acct-pos  IN h_base (loan-acct.acct,loan-acct.currency,TODAY,TODAY,?).

      /*  RUN dbgprint.p("1111",loan-acct.acct + ";" + STRING(sh-bal)). */
      most47802 =  most47802 + ABS(sh-bal).      

   END.
   IF most91418 - most47802 GT 0 THEN 
   DO:
      mname = "".
      FIND FIRST PERSON 
         WHERE PERSON.PERSON-ID = loan.cust-id
         NO-LOCK NO-ERROR.
      IF AVAILABLE PERSON THEN
         /* ФИО клиента */
         mname = PERSON.NAME-LAST + " " + PERSON.FIRST-NAMES.
   /*        RUN dbgprint.p("",mcontcode + ";" + mname + ";" + STRING(most47802) + ";" + STRING(most91418)). */

   ASSIGN
      mStatus   = CHR(251)
      mSum      =  most91418 - most47802
      mCurr     = ""
      mAcctDb   = "99999810400000000000     @0000"
      mDocNum   =  "14"
      mDetails  = SUBSTITUTE(mSoob,loan.doc-ref,
      GetXAttrValueEx ("loan",loan.contract + "," + loan.cont-code,"ДатаСогл",?),
      mname)
      mdoctype  = "ВБО"
      mNeedBank = "0"
      .
   RUN opcreate.

   END.



END.







PROCEDURE opcreate.

   RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"in-filial",shFilial).
   RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"acctdb",macctdb).
   RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"acctcr",macctcr).
   RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"__USERID",USERID('bisquit')).
   RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"in-sSum",mSum).
   RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"in-curr",mCurr).
   RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"in-doc-type",mDocType).
   RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"in-op-date",in-op-date).
   RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"in-op-status",mStatus).
   RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"in-op-details",mDetails).
   RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"in-acct-cat","o").
   RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"in-doc-num",mDocNum).
   RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"needbank",mNeedBank).
   RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"in-bank-code",mBankBik).
   RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"in-bank-name",mBankName).
   RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"in-corr-acct",mBankCorr).
   RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"in-ben-acct",mBenAcct).
RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"in-bank-code","").
RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"in-bank-name","").
RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"in-corr-acct","").
RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"in-ben-acct","").
RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"in-inn","").
RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"in-kpp","").
RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"in-kpp1","").
RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"in-name-ben","").
RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"in-bank-type","").
RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"in-doc-kind","").
RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"in-type","ВН").

   IF GetBaseOpDate() EQ ? THEN RUN InitBaseLibrary IN h_pbase (?,in-op-date,?).

   RUN SetSysconf IN h_Base ("АВТОМАТИЧЕСКАЯ ПОСТАНОВКА","ДА"). 

   RUN RunTransaction IN h_pbase ("op14") NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
      DO:
         RUN Fill-SysMes IN h_tmess ("", "", "0", "Ошибка создания документа~n" + ERROR-STATUS:GET-MESSAGE(1)).
         UNDO, RETRY.
      END.
END PROCEDURE.
