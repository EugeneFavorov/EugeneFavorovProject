



{globals.i}
{tmprecid.def}  
{intrface.get xclass}
{intrface.get xobj}     /* ������⥪� ��� ࠡ��� � ��ꥪ⠬�. */
{intrface.get dps}
{intrface.get dpspr}
{dpsproc.def}
{f_for_t.i} /* �㭪樨 ��� ࠡ��� � ���������ﬨ */
{sh-defs.i}             /* ��६���� ��� ����� ���⪠ �� ����. */


DEFINE TEMP-TABLE dps_param NO-UNDO
    FIELD   acct    LIKE    acct.acct
    FIELD   curr    LIKE    acct.currency
    FIELD   kau     LIKE    kau.kau
    FIELD   name    AS CHAR
    FIELD   blns    LIKE    acct-pos.balance
    FIELD   date1 AS DATE
    FIELD   date2 AS DATE
    INDEX   acct acct.



DEF VAR d1 AS DATE.
DEF VAR d2 AS DATE.
DEF VAR dat1 AS DATE NO-UNDO.
DEF VAR dd1 AS DATE NO-UNDO .
DEF VAR dd2 AS DATE NO-UNDO .
DEF VAR Is_Old AS LOGICAL NO-UNDO .
DEF VAR fl-prol-cond AS LOGICAL NO-UNDO .
DEF VAR mPeriod AS CHAR NO-UNDO .
DEF VAR mOstatok AS DECIMAL NO-UNDO.
def var result  as decimal no-undo.
def var result1 as decimal no-undo.
def var flag    as INT64 init -1.
def var l_acct    like loan-acct.acct no-undo. /* ��� ������ */
def var l_acct_type as char           no-undo.

&glob str-type "loan-dps-ts,loan-dps-tsk,loan-dps-ink"


DEFINE VARIABLE vDRVal        AS CHARACTER NO-UNDO.
DEFINE VARIABLE vSubj         AS CHARACTER NO-UNDO.
DEFINE VARIABLE vI            AS INT64   NO-UNDO.
DEFINE VARIABLE mName         AS CHARACTER NO-UNDO EXTENT 3.
DEFINE VARIABLE mCode         AS CHARACTER NO-UNDO.
def var cred-per as char format "x(17)".
def var int-per  as char format "x(17)".
DEFINE VARIABLE tels          AS CHARACTER EXTENT 6 FORMAT "x(17)"
  INIT ['�������筮 �� ',
        '�������⠫쭮 �� ',
        '������ �� 11,21,31/1',
        '�ந����쭮',
        '���㣮�',
        '���'].
def var m-comm       as   char                       no-undo.
def var s-comm       as   char                       no-undo.
def var a-inter      as   char                       no-undo.
def var pen-interest as   char                       no-undo.
DEF VAR mMin-ostStr  AS   CHAR                       NO-UNDO.
DEFINE VARIABLE sh-val-close  AS DECIMAL NO-UNDO.
DEFINE VARIABLE sh-val-na     AS DECIMAL NO-UNDO.
DEFINE VARIABLE sh-bal-close  AS DECIMAL NO-UNDO.
DEFINE VARIABLE sh-bal-na     AS DECIMAL NO-UNDO.


DEF BUFFER bloan     FOR loan.
DEF BUFFER b-acct    FOR acct.
      
FUNCTION GetCliName RETURN CHAR (
   in-cat AS CHAR,
   in-id AS CHAR
):
   IF    NOT {assigned in-cat}
      OR NOT {assigned in-id}
      THEN RETURN "".
   CASE in-cat:
   WHEN "�" THEN RETURN GetObjName("branch",in-id,NO).
   OTHERWISE DO:
      RUN GetCustName IN h_base(in-cat,in-id,?,
                                OUTPUT mName[1],
                                OUTPUT mName[2],
                                INPUT-OUTPUT mName[3]).
      RETURN TRIM(mName[1]) + " " + TRIM(mName[2]).
   END.
   END CASE.
END FUNCTION.


{setdest.i &col=170 } 

FOR EACH tmprecid,
   FIRST bloan WHERE
      RECID(bloan) EQ tmprecid.id
NO-LOCK:

/* ���� ��� ������ */
   IF bloan.class-code NE "dep_pers_trans" AND
      NOT CAN-DO(bloan.class-code,GetXclassAllChildsEx("dps_pers_trans_cap")) THEN
      DO:
         RUN GetBaseAcctRole (RECID(bloan),
                        gend-date,
                        OUTPUT l_acct_type). 
                      
      FIND LAST loan-acct OF bloan WHERE
         loan-acct.acct-type EQ l_acct_type /*BEGINS (if loan.end-date eq ? then "loan-dps-p"
                                                    else "loan-dps-t")*/ AND
         loan-acct.since LE gend-date
         NO-LOCK NO-ERROR.

      /* �� ��砩 ���室� ��筮�� ������ �� ����� �� ����ॡ������, ��
       ���죨 ������� �� ��� ��筮�� ������ */
      IF NOT AVAIL loan-acct AND bloan.end-date EQ ? THEN
          FIND LAST loan-acct OF bloan WHERE loan-acct.acct-type EQ "loan-dps-t"
       NO-LOCK NO-ERROR.

      END.
      ELSE 
      DO:
         IF NOT CAN-DO(bloan.class-code,GetXclassAllChildsEx("dps_pers_trans_cap")) THEN
         DO:
            FIND LAST loan-acct OF bloan WHERE
            loan-acct.acct-type EQ (IF bloan.end-date NE ? THEN "loan-dps-ts" ELSE "loan-dps-ps") AND
            loan-acct.since LE gend-date
            NO-LOCK NO-ERROR.
         END.
         ELSE
         DO:
            FIND LAST loan-acct OF bloan WHERE
            loan-acct.acct-type EQ "loan-dps-tsk" AND
            loan-acct.since LE gend-date
            NO-LOCK NO-ERROR.
         END.
      END.

      l_acct = IF AVAIL loan-acct THEN DelFilFromAcct(loan-acct.acct)
         ELSE "�� " + STRING (gend-date) + " ��� �� ��।����".
      FIND b-acct WHERE b-acct.acct EQ loan-acct.acct NO-LOCK NO-ERROR.
      PUT UNFORMATTED "����� N:"  bloan.cont-code FORMAT "x(20)" SKIP.
      PUT UNFORMATTED " ����⥪� ��⮢" SKIP.
      PUT UNFORMATTED 
         "��������������������������������������������������������������������������������Ŀ" SKIP
         "�             �������� ����              �       ����� �����      �����  ����  ���" SKIP
         "��������������������������������������������������������������������������������Ĵ" SKIP.
              
   
      FOR EACH loan-acct WHERE loan-acct.contract  EQ bloan.contract 
                           AND loan-acct.cont-code EQ bloan.cont-code,
         FIRST acct WHERE acct.acct     EQ loan-acct.acct 
                      AND acct.currency EQ loan-acct.currency,
         FIRST code WHERE code.code  EQ loan-acct.acct-type 
                      AND code.class EQ "loan-acct" AND code.parent EQ "loan-dps"
      NO-LOCK:
   
      PUT UNFORMATTED 
         "�" code.name FORMAT "x(40)"    
         "�" loan-acct.acct FORMAT "x(24)"
         "�" loan-acct.currency FORMAT "x(3)"
         "�" loan-acct.since        
         "�" acct.acct-cat
         "�" SKIP.
   
      END.
   
   
      PUT UNFORMATTED
         "����������������������������������������������������������������������������������" SKIP(2).

END.

{preview.i &col=170}
{intrface.del}

PROCEDURE Get_Par:
    DEFINE VAR in-kau       AS CHARACTER          NO-UNDO.
    DEFINE VAR in-k         AS CHARACTER EXTENT 2 NO-UNDO.
    DEFINE VAR l-acct       AS CHARACTER          NO-UNDO.

    DEFINE VAR vKind        AS CHARACTER          NO-UNDO.
    DEFINE VAR vTempl       AS INT64            NO-UNDO.
    DEFINE VAR vKindD       AS DATE               NO-UNDO.

    DEFINE VAR dat_start    AS DATE               NO-UNDO. /* ��砫� ��ਮ�� ���᫥��� ��業⮢ */
    DEFINE VAR cod_ost      AS CHARACTER          NO-UNDO. /* ��� ���⪠ */
    DEFINE VAR beg-dat      AS DATE               NO-UNDO.
    DEFINE VAR in-surrogate AS CHARACTER          NO-UNDO.
    DEFINE VAR i            AS INT64            NO-UNDO.

    DEFINE VAR str-kau      AS CHARACTER          NO-UNDO.
    DEFINE VAR comm         AS CHARACTER          NO-UNDO. /* ��� �᭮���� �����ᨨ */
    DEFINE VAR in-interest  AS CHARACTER          NO-UNDO. /* �奬� ���. ��業⮢ */
    DEFINE VAR dat-t        AS DATE               NO-UNDO.
    DEFINE VAR summ%        LIKE acct-pos.balance NO-UNDO.

    DEFINE VAR str-acct     AS CHARACTER          NO-UNDO.
    DEFINE VAR date-contr   AS DATE               NO-UNDO.
    DEFINE VAR end-dat      AS DATE               NO-UNDO.
    DEFINE VAR nn           AS INT64            NO-UNDO.
    DEFINE VAR strnn        AS CHARACTER          NO-UNDO.

    def buffer yop-templ for op-templ .
    {justasec}

    RUN get-beg-date-prol in h_dpspc(RECID(bloan),end-date,
    OUTPUT dd1, OUTPUT dd2).

    RUN loan_param.        /* ����� ���⪮� �� ������ */
    IF dd2 <> ? /* �।��������, �� ������ � ���������ﬨ
                �� �뢠�� �� ����ॡ������ */    
    THEN            
    RUN current_persent.    /* ����� ⥪��� ��業⮢ �� ���������� */
    /*------------ ��।������ ���� ���᫥��� ��業⮢ ------------*/

    FIND LAST loan-cond WHERE
              loan-cond.contract  EQ bloan.contract
          AND loan-cond.cont-code EQ bloan.cont-code
          AND loan-cond.since     LE end-date
    NO-LOCK NO-ERROR.

    IF AVAIL loan-cond THEN DO:
       /*����塞 ����� ॠ���� ���� ���᫥��� %%*/
       RUN DateOfCharge in h_dpspc (end-date, RECID(loan-cond), OUTPUT dat_start).
       IF dat_start NE ? THEN DO:
          /*�᫨ ���� �஡���� � ��室�묨 (� ��� ���� ����� �������) */
          IF NOT chk_date(recid(loan-cond), dat_start) THEN DO:
             REPEAT i = 0 TO 30:
                IF chk_date(RECID(loan-cond), dat_start + i ) THEN
                LEAVE.
                HIDE MESSAGE NO-PAUSE .
             END.
             dat_start = dat_start + i.
          END.
       END.
       IF dat_start NE ? THEN DO:
          CREATE dps_param.
          ASSIGN
             dps_param.acct = "???"
             dps_param.name = "��� ����. ��業⮢"
          .
          IF bloan.end-date NE ? THEN
             dps_param.date1 = MINIMUM (dat_start, bloan.end-date).
          ELSE
             dps_param.date1 = dat_start.
       END.
    END.

    /*---------------------------------------------------------*/
   
    /* Guva �஢��塞 ⨯ ������ */
    IF dd2 EQ ? THEN
    ASSIGN
       in-k[1] = '��₪��'
       in-k[2] = '�����'
    .
    ELSE
    ASSIGN
       in-k[1] = '��₪��'
       in-k[2] = '�����1'
    .
                       
    cod_ost = bloan.contract + ',' + bloan.cont-code + ',' + in-k[2].
    RUN  get-beg-date-all in h_dpspc (RECID(bloan),end-date,OUTPUT dat_start).
    
    IF     dat_start NE ? 
       AND (   bloan.loan-status LT "���"
            OR bloan.end-date    EQ ?
            OR mOstatok         NE 0)
       AND (    bloan.close-date EQ ?
            OR  bloan.close-date GT end-date)      
       THEN DO: /* ����塞 ⥪�騥 ��業�� */
       FIND LAST loan-acct OF bloan
          WHERE loan-acct.acct-type EQ (if ENTRY(3,cod_ost) EQ '�����' THEN 'loan-dps-p'
                                                                        ELSE 'loan-dps-t')
           AND loan-acct.since      LE dat_start
       NO-LOCK NO-ERROR. /* ��� */

       IF NOT AVAIL loan-acct THEN 
         FIND FIRST loan-acct OF loan
          WHERE loan-acct.acct-type EQ (if ENTRY(3,cod_ost) EQ '�����' THEN 'loan-dps-p'
                                                                        ELSE 'loan-dps-t')
           AND loan-acct.since      GT dat_start
       NO-LOCK NO-ERROR. /* ��� */
        IF NOT AVAIL loan-acct THEN
       RETURN.

       FIND acct WHERE acct.acct     EQ loan-acct.acct
                   AND acct.currency EQ loan-acct.currency
       NO-LOCK NO-ERROR.

       str-kau = bloan.contract +  ',' + bloan.cont-code + ',' + in-k[1].
       beg-date = dat_start.
       
       CREATE dps_param.
       ASSIGN
          dps_param.acct  = loan-acct.acct
          dps_param.curr  = loan-acct.currency
          d1 = beg-date
          d2 = end-date
       .

       RUN get_acct in h_dpspc(recid(bloan),
                                       beg-date,
                                       end-date,
                                       output str-acct).

       DO i = 1 TO NUM-ENTRIES(str-acct) BY 2:
          FIND FIRST acct
               WHERE acct.acct EQ ENTRY(i,str-acct)
          NO-LOCK NO-ERROR.
          IF NOT AVAIL acct THEN NEXT.
          IF i GT 1 THEN beg-date = IF beg-date LT DATE(ENTRY(i + 1,str-acct)) THEN DATE(ENTRY(i + 1,str-acct))
                                                                               ELSE beg-date.
          IF NUM-ENTRIES(str-acct) GE i + 3
          THEN date-contr = date(entry(i + 3,str-acct)).
          ELSE date-contr = end-date.

          RUN Get_Last_Inter in h_dpspc (RECID(bloan),beg-date,date-contr,OUTPUT in-interest).
          IF in-interest = ?  OR in-interest = '?' THEN DO:
             i = i + 1.
             NEXT.
          END.
          RUN Get_Last_Commi in h_dpspc (RECID(bloan),beg-date,date-contr,OUTPUT comm).
          IF comm = ?  OR comm = '?'  THEN DO :
             i = i + 1.
             NEXT.
          END.

          DO WHILE beg-date LT date-contr:
             result = 0.
             result1 = 0.
             {findsch.i &dir=first
                        &sch=in-interest
                        &since1=" GT beg-date AND interest-sch-line.since LE date-contr"}
             IF AVAIL interest-sch-line THEN dat-t = interest-sch-line.since.
                                        ELSE dat-t = date-contr.

             RELEASE interest-sch-line.
             {findsch.i &dir=last &sch=in-interest &since1 =" lt dat-t"}
             if avail interest-sch-line then
             RUN nachkin.p(recid(interest-sch-line),
                           comm,
                           recid(acct),
                           dat-t,
                           str-kau,
                           ?,
                           output result,
                           output result1,
                           input-output beg-date,
                           output flag)  .
             summ% = summ% + result .
             if flag ne 0 then return .
             beg-date = dat-t .
          END.
       END.
       IF AVAIL dps_param THEN
       ASSIGN
          dps_param.kau   = "No"
          dps_param.name  = "����騥 ��業��"
          dps_param.blns  = summ%
          dps_param.date1 = d1
          dps_param.date2 = d2
       .
    END.

end procedure.

PROCEDURE list-kau.
  /*DEF INPUT PARAMETER rec AS RECID.*/
  DEF INPUT PARAMETER l-acct AS CHAR.
  DEF INPUT PARAMETER in-kau AS CHAR.
  DEF OUTPUT PARAMETER list-kau AS CHAR NO-UNDO.
  DEF VAR tmp-list AS CHAR NO-UNDO.
  DEF BUFFER b-loan-acct FOR loan-acct.
  DEF BUFFER b-kau FOR kau.
  tmp-list = "".
  RUN  list-kau-dv(in-kau,l-acct,OUTPUT list-kau ).
  IF list-kau <> '' THEN RETURN .
  FOR EACH b-loan-acct OF bloan WHERE b-loan-acct.acct-type EQ l-acct
                     AND b-loan-acct.since >= dat1
                     AND b-loan-acct.since <= end-date   NO-LOCK:
      FIND b-kau WHERE b-kau.acct = b-loan-acct.acct AND
                       b-kau.currency = b-loan-acct.currency AND
                       b-kau.kau = in-kau NO-LOCK NO-ERROR.
      IF AVAILABLE b-kau THEN DO:
        IF NOT CAN-DO(STRING(RECID(b-kau)), tmp-list) THEN DO:
           {additem.i tmp-list STRING(RECID(b-kau))}
        END.
      END.
  END.
  list-kau = REPLACE(tmp-list,",",";").
END PROCEDURE.

/* ��楤�� ��� ᯥ���᪮� ��ࠡ�⪨ ����������  � ����祭�� ᯨ᪠ ��⮢ �� ���������� */
PROCEDURE list-kau-dv.
   DEF INPUT PARAM in-kau AS CHAR NO-UNDO .
   DEF INPUT PARAM l-acct AS CHAR NO-UNDO .
   DEF OUTPUT PARAMETER list-kau AS CHAR NO-UNDO.

   DEF VAR tmp-list AS CHAR NO-UNDO.

   DEF BUFFER b-loan-acct FOR loan-acct.
   DEF BUFFER b-kau FOR kau.

   IF NOT CAN-DO({&str-type},l-acct) THEN RETURN .
   FOR EACH b-loan-acct OF bloan WHERE b-loan-acct.acct-type EQ l-acct
                     AND b-loan-acct.since >= dat1
                     and b-loan-acct.since <= end-date NO-LOCK,
      EACH b-kau WHERE b-kau.acct = b-loan-acct.acct AND
                       b-kau.currency = b-loan-acct.currency AND
                       b-kau.kau BEGINS ENTRY(1,in-kau) + ',' + ENTRY(2,in-kau)   NO-LOCK :
      IF ENTRY(4,b-kau.kau) = ENTRY(3,in-kau) THEN
         DO :  
         {additem.i tmp-list STRING(RECID(b-kau))}

      END. 
   END.
   list-kau = REPLACE(tmp-list,",",";").
END PROCEDURE .
/* ��।������ ��������� ���⪮� �� ������ */
PROCEDURE LOAN_PARAM.
    DEFINE VARIABLE tmp_summ     AS DECIMAL   NO-UNDO. /* �६����� �㬬� ��� ���⮢ */
    DEFINE VARIABLE fin_summ     AS DECIMAL   NO-UNDO. /* ���⮪ �� ���������� */
    DEFINE VARIABLE vLstTypeChar AS CHARACTER NO-UNDO. /*���᮪ ஫�� ��⮢ ������� � �����������*/
    DEFINE VARIABLE vLstCodChar  AS CHARACTER NO-UNDO. /*���᮪ ����� ���⪠*/
    DEFINE VARIABLE vLstNameChar AS CHARACTER NO-UNDO. /*������������ ���⪮�*/
    DEFINE VARIABLE vIndLst      AS INT64   NO-UNDO. /*������ �� ᯨ�� ��⮢,���⪮�,������������*/
    DEFINE VARIABLE vStrKau      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vListKauChar AS CHARACTER NO-UNDO. /*���᮪ RecId ��� ��� �� ����*/
    vLstTypeChar = "loan-dps-p"   + "|" +
                   "loan-dps-p"   + "|" +
                   "loan-dps-t"   + "|" +
                   "loan-dps-t"   + "|" +
                   "loan-dps-int" + "|" +
                   "loan-dps-exc" + "|" +
                   "loan-dps-ts"  + "|" +
                   "loan-dps-ts"  + "|" +
                   "loan-dps-tsk" + "|" +
                   "loan-dps-tsk" + "|" +
                   "loan-dps-ink".
    vLstCodChar  = "��₪��"      + "|" +
                   "�����"       + "|" +
                   "��₪��"      + "|" +
                   "�����1"      + "|" +
                   "����"        + "|" +
                   "����"         + "|" +
                   "��₪���"     + "|" +
                   ",�����1"     + "|" +
                   "��₪���"     + "|" +
                   "�����1"      + "|" +
                   "����".
    vLstNameChar = "���⮪ ������ �� ����ॡ������"              + "|" +
                   "���᫥��� ��業��"                         + "|" +
                   "���⮪ ��筮�� ������"                      + "|" +
                   "���᫥��� ��業��"                         + "|" +
                   "�।���⥫쭮 ���᫥��� ��業��"          + "|" +
                   "����祭�� ���᫥��� ��業��"            + "|" +
                   "���⮪ �� ����������"                       + "|" +
                   "���᫥��� ��業��"                         + "|" +
                   "���⮪ �� ����������"                       + "|" +
                   "���᫥��� ��業��"                         + "|" +
                   "�।���⥫쭮 ���᫥��� ��業��".
    mOstatok = 0.
    DO vIndLst = 1 TO NUM-ENTRIES(vLstTypeChar,"|"):
       fin_summ = 0.
       FIND LAST loan-acct OF bloan
          WHERE loan-acct.acct-type EQ ENTRY(vIndLst,vLstTypeChar,"|")
            AND loan-acct.since     LE end-date
       NO-LOCK NO-ERROR.
       IF NOT AVAIL loan-acct THEN NEXT.
       vStrKau = bloan.contract  + "," +
                 bloan.cont-code + "," +
                 ENTRY(vIndLst,vLstCodChar,"|").
       RUN list-kau (loan-acct.acct-type, vStrKau, OUTPUT vListKauChar).
       RUN kau-pos.p(loan-acct.acct,
                     loan-acct.currency,
                     end-date,
                     end-date,
                     gop-status,
                     vStrKau).
       fin_summ = IF loan-acct.currency  EQ "" THEN ksh-bal
                                               ELSE ksh-val.

       IF fin_summ = 0 THEN DO :
          RUN get-kauost-trans(bloan.contract,
                               bloan.cont-code,
                               "*",
                               loan-acct.acct-type,
                               "",
                               "",
                               ENTRY(vIndLst,vLstCodChar,"|"),
                               end-date,
                               end-date,
                               gop-status,
                               OUTPUT fin_summ
                              ).          
       END.
       IF fin_summ <> 0 THEN DO :
         CREATE dps_param.
         ASSIGN
           dps_param.acct = loan-acct.acct
           dps_param.curr = loan-acct.currency
           dps_param.kau  = IF vListKauChar NE ""
                              THEN vListKauChar
                              ELSE (bloan.contract + "," + 
                                    bloan.cont-code + "," + 
                                    loan-acct.acct-type + "," + 
                                    ENTRY(vIndLst,vLstCodChar,"|"))
           dps_param.name = ENTRY(vIndLst,vLstNameChar,"|")
           dps_param.blns = fin_summ
         .
       END.
       IF    ENTRY(vIndLst,vLstCodChar,"|") EQ "��₪��"
          OR ENTRY(vIndLst,vLstCodChar,"|") EQ "��₪���" THEN
       DO:
          mOstatok = fin_summ.
       END.   
    END.
END.
/* /���⮪ �� ���������� */

/* ���᫥��� ��業�� �� ���������� */
PROCEDURE CURRENT_PERSENT.
   DEFINE VAR refin_summ AS DECIMAL    NO-UNDO. /* �६����� �㬬� ��� ���⮢ */
   DEFINE VAR fin_summ   AS DECIMAL    NO-UNDO. /* ���⮪ �� ���������� */
   DEFINE VAR all_summ   AS DECIMAL    NO-UNDO. /* ���⮪ �� ���������� */
   DEFINE VAR is_ok      AS LOGICAL    NO-UNDO. /* �ਧ��� �믮������ ����. */
   DEFINE VAR lnp_date   AS DATE       NO-UNDO. /* ��� ��ࢮ�� �।���⥫쭮��
                                                ** ���᫥��� ��業⮢ �� ����. */
   DEF VAR vFlNach        AS LOG       NO-UNDO. /* �ਧ���, �뫮 �� ���᫥��� */
   DEF VAR vTransOpenDate AS DATE      NO-UNDO. /* ��� ��ࢮ�� �������� �� ����. */
   DEF VAR vLnpD          AS DATE      NO-UNDO. /* ��� ������ ��ࢮ�� ����������
                                                ** (�᫨ �� �뫮 ���᫥���) ���
                                                ** ��� ��ࢮ�� �।���⥫쭮��
                                                ** ���᫥��� ��業⮢ �� ����. */
   DEF VAR vFlagErr       AS INT64   NO-UNDO. /* ���� �訡�� �� ���� ��業⮢ */
   
   IF        (bloan.loan-status GE "���"
         AND  mOstatok         EQ 0) 
      OR (    bloan.close-date NE ?
         AND  bloan.close-date LE end-date) THEN
   RETURN. 
      
   FOR EACH loan-trans OF bloan 
      NO-LOCK:

      /* ��।������ ��ࢮ�� �������� */
      RUN get_first_kau_date(loan-trans.contract,
                             loan-trans.cont-code,
                             loan-trans.trans-code,
                             end-date,
                             gop-status,
                             OUTPUT vFlNach,
                             OUTPUT vTransOpenDate).

      /* ���� ���� ��᫥������ ���᫥��� ��業⮢ */
      RUN get_beg_date_trans (loan-trans.contract,
                              loan-trans.cont-code,
                              loan-trans.trans-code,
                              end-date,
                              OUTPUT lnp_date).
      /* �᫨ ���᫥��� �㤥� � ���饬 - ���쬥� ���� ��砫� ���������� */
      IF lnp_date EQ ? THEN 
         lnp_date = IF dd1 LT loan-trans.open-date THEN loan-trans.open-date
                                                   ELSE dd1.
      
      IF    lnp_date GT vTransOpenDate
         OR vLnpD    EQ ? THEN 
         vLnpD = lnp_date.

      /* ������ �㬬� �।���⥫쭮 ���᫥���� ��業⮢ */
      RUN Calc_Interest_Full_Dovl IN h_dpspr (BUFFER bloan,
                                              lnp_date,
                                              IF end-date GT lnp_date THEN end-date 
                                                                      ELSE lnp_date,
                                              ?,
                                              NO,
                                              YES,
                                              loan-trans.trans-code,
                                              NO,                                                 
                                              OUTPUT fin_summ,
                                              OUTPUT refin_summ,
                                              OUTPUT vFlagErr).
      all_summ = all_summ + fin_summ.
   END.

   IF all_summ NE 0.00 THEN 
   DO:
      CREATE dps_param.
      ASSIGN
         dps_param.acct = ""
         dps_param.curr = ""
         dps_param.kau  = "No"
         dps_param.name = "����騥 ��業�� �� ����������"
         dps_param.blns =  all_summ
         dps_param.date1 = vLnpD 
         dps_param.date2 = IF end-date GT lnp_date  THEN end-date ELSE lnp_date
         .
   END.

END PROCEDURE.
procedure chek_dps.
    find first dps_param no-error.
    if not avail dps_param then do:
        message "�� ���� " + string(end-date,"99.99.9999") +
        " �� ��।���� �� ���� ���!"
        view-as alert-box buttons OK.
    end.
end procedure.
