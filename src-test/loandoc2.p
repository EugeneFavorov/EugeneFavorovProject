/*
               KSV Editor
    Copyright: (C) 2000-2005 Serguey Klimoff (bulklodd)
     Filename: LOANDOC1.P
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
      Created: 18.04.2008 12:15 fEAk    
     Modified: 18.04.2008 12:15 fEAk     <comment>
*/

{globals.i}
{norm.i NEW}
{tmprecid.def}
{prn-doc.def &with_proc=YES}
DEF VAR proxy_h AS HANDLE NO-UNDO.
RUN "l-prox.p" PERSISTENT SET proxy_h.

def input parameter in-branch-id like DataBlock.branch-id no-undo.

DEF VAR mZay AS CHAR NO-UNDO.
def VAR in-dataclass-id like DataClass.DataClass-Id no-undo.
def VAR InputFName as character no-undo.
def VAR in-beg-date like DataBlock.Beg-Date no-undo.
def VAR in-end-date like DataBlock.End-Date no-undo.    
DEF VAR mTemplateName AS CHARACTER NO-UNDO. /* ��� 蠡���� ����⮩ ����, 
                                               ���祭�� ���ண� ���� ��ࠡ��뢠���� */
DEF VAR mRID          AS RECID     NO-UNDO. /* recid ⥪�饣� 蠡���� */
DEF VAR mline           AS CHAR      NO-UNDO.
DEF VAR mIndL           AS INT64     NO-UNDO.
DEF VAR mIndR           AS INT64     NO-UNDO.
DEF VAR mtag            AS CHAR      NO-UNDO.
DEF VAR mf-crdr-list    AS CHAR      NO-UNDO.
DEF VAR mf-cv-list      AS CHAR      NO-UNDO.
DEF VAR mf-dover-list   AS CHAR      NO-UNDO.
DEF VAR mOther-tag-list AS CHAR      NO-UNDO.
DEF VAR vOtherTags      AS CHARACTER NO-UNDO.
DEf VAR iRet            AS LOG       NO-UNDO.

DEF VAR kk              AS INT64     NO-UNDO.
DEF VAR vProcName       AS CHAR      NO-UNDO.
DEF VAR mParams         AS CHAR      NO-UNDO.

DEF VAR vSupportedProc  AS CHARACTER   NO-UNDO.
DEF VAR vTmpStr         AS CHARACTER   NO-UNDO.

DEF SHARED VAR rid_loan  AS RECID.

DEF TEMP-TABLE ttRecids LIKE tmprecid.

DEF TEMP-TABLE ttTags NO-UNDO
   FIELD var-id    AS CHARACTER
   FIELD dataclass AS CHARACTER
INDEX idx AS UNIQUE PRIMARY var-id dataclass
.

ASSIGN 
   in-beg-date = gbeg-date
   in-end-date = gend-date
   in-branch-id = GetXAttrValue("_user",USERID('bisquit'),"�⤥�����")
   .

{norm-beg.i &title="'��������� ������' " &nofil=yes &is-branch = yes}

find first loan WHERE RECID(loan) = rid_loan NO-LOCK NO-ERROR.
 mZay = GetXattrValueEx("loan",
                         loan.contract + "," + loan.cont-code,
                            "��⠍�珥��",
                    	     ?).
IF mZay = ? OR mZay = "?" THEN 
DO:
 message "��� ���� ����室��� ��������� ���.४�����: ��� ��砫� ����᫥��� %% � 42301"
   VIEW-AS ALERT-BOX.
                    	             RETURN.
                    	             END.

FIND FIRST DataClass WHERE DataClass.DataClass-Id EQ "����" NO-LOCK NO-ERROR.
IF NOT AVAILABLE DataClass THEN DO:
   RUN Fill-SysMes ("", "", "0","�� ���� ���� �����").   
   RETURN.
END.
ELSE DO:
    {for_form.i 
       &DataClass = DataClass
       &End-Date  = gend-date}
       {additem.i mf-crdr-list formula.var-id}
       
    END.
END.

FIND FIRST DataClass WHERE DataClass.DataClass-Id EQ "��" NO-LOCK NO-ERROR.
IF NOT AVAILABLE DataClass THEN DO:
   RUN Fill-SysMes ("", "", "0","�� ���� ���� �����").   
   RETURN.
END.
ELSE DO:
    {for_form.i 
       &DataClass = DataClass
       &End-Date  = gend-date 
       &nodef     = {comment}
      }
       
       {additem.i mf-cv-list formula.var-id}
       
    END.
END.

FIND FIRST DataClass WHERE DataClass.DataClass-Id EQ "������" NO-LOCK NO-ERROR.
IF NOT AVAILABLE DataClass THEN DO:
   RUN Fill-SysMes ("", "", "0","�� ���� ���� �����").   
   RETURN.
END.
ELSE DO:
    {for_form.i 
       &DataClass = DataClass
       &End-Date  = gend-date 
       &nodef     = {comment}
      }
       
       {additem.i mf-dover-list formula.var-id}
    END.
END.

/* ���� �맢����� 蠡���� */
mRID = INT64(GetSysConf("user-proc-id")).
FIND FIRST user-proc WHERE RECID(user-proc) EQ mRID NO-LOCK NO-ERROR.

/* ��।������ 蠡���� ���� ��� ���������� */
mTemplateName = GetXAttrValueEx("user-proc",
                                STRING(user-proc.public-number),
                                "������",
                                "").
IF NOT {assigned mTemplateName} 
THEN 
mTemplateName = GetXAttrValueEx("user-proc",
                                STRING(user-proc.public-number),
                                "������",
                                "depodv"). 
                                
                                
/* �뤥��� ⥣� �� 蠡���� */                                
FOR EACH Reports 
   WHERE Reports.name EQ mTemplateName 
   NO-LOCK BY reports.line:
      
   mLine = reports.txt.
   mIndL = INDEX(mLine,"<#").
   IF mIndL > 0 THEN
   DO:
      mIndR = INDEX(mLine,"#>",mIndL).
      IF mIndR > 0 THEN
      DO:
         mtag = TRIM(SUBSTRING(mLine, mIndL + 2, mIndR - mIndL - 2)).
      END.
   END.
   
   /* ⥣� 蠡���� �� ����� ���� */
   IF CAN-DO(mf-crdr-list,mtag) THEN DO:
      FIND FIRST ttTags 
           WHERE ttTags.var-id    EQ mtag
             AND ttTags.DataClass EQ "����" NO-ERROR.                
      IF NOT AVAIL ttTags THEN DO:
         CREATE ttTags.
         ASSIGN 
            ttTags.var-id    = mtag              
            ttTags.DataClass = "����"
         .             
      END.
      NEXT.  
   END.
   /* ⥣� 蠡���� �� ����� �� */
   IF CAN-DO(mf-cv-list,mtag) THEN DO:
      FIND FIRST ttTags 
           WHERE ttTags.var-id    EQ mtag
             AND ttTags.DataClass EQ "��" NO-ERROR.                
      IF NOT AVAIL ttTags THEN DO:
         CREATE ttTags.
         ASSIGN 
            ttTags.var-id    = mtag              
            ttTags.DataClass = "��"
         .             
      END.
      NEXT.
   END.
   /* ⥣� 蠡���� �� ����� ������ */
   IF CAN-DO(mf-dover-list,mtag) THEN DO:
      FIND FIRST ttTags 
           WHERE ttTags.var-id    EQ mtag
             AND ttTags.DataClass EQ "������" NO-ERROR.                
      IF NOT AVAIL ttTags THEN DO:
         CREATE ttTags.
         ASSIGN 
            ttTags.var-id    = mtag              
            ttTags.DataClass = "������"
         .             
      END.
      NEXT.
   END.
   /* ��稥 ⥣� */
   ELSE DO:
      {additem.i mOther-tag-list mtag}
      NEXT.
   END.
END.
/* ��ࠬ���� ��� ���� ⥣�� */
ASSIGN
   RetString      = YES
   vSupportedProc = FGetSetting("������", ?, "dps_prn,cardprn,loanform,dog,bankinfo,userinfo,dpsprn_rshb,proxyprn")
   PrinterWidth   = 10000
   .

/* Functions & Procedures*/
/* �ॡ����� ��� ���� ⥣�� �� �� ����ᮢ ������ */
{crtagval.i}
/*-----------------------*/

/* ���⠥� ⥣� */
/* ��� ��� ⥣�� 蠡���� �� ����ᮢ ���� � �� */
FOR EACH ttTags NO-LOCK,
    FIRST DataClass 
    WHERE DataClass.DataClass-Id EQ ttTags.DataClass 
    NO-LOCK:
       find last formula of DataClass 
       where formula.var-id eq ttTags.var-id 
         and formula.since <= gend-date no-lock no-error.      
      ASSIGN
         mIndL = INDEX(formula.formula, "(") 
         mIndR = INDEX(formula.formula, ")")
      . 
      IF     mIndL NE 0 
         AND mIndR NE 0 
      THEN
         ASSIGN 
            vProcName = SUBSTR(formula.formula, 
                               1, 
                               mIndL - 1)
            mParams   = SUBSTR(formula.formula, 
                               mIndL + 1, 
                               mIndR - mIndL - 1)
         .      
      IF CAN-DO(vSupportedProc, vProcName) THEN
      DO:
         RUN Run-PrnProc(vProcName, gbeg-date, gend-date, mParams, OUTPUT vTmpStr).
         RUN Insert_TTName(formula.var-id, vTmpStr).
      END. 
END.
/* ⥣� �� �� ����ᮢ ������ */    
IF mOther-tag-list NE ? AND mOther-tag-list NE ""
THEN DO:
   DO kk = 1 TO NUM-ENTRIES(mOther-tag-list) :
      IF CAN-DO(vOtherTags,ENTRY(kk, mOther-tag-list)) THEN DO:
         IF ENTRY(kk, mOther-tag-list) BEGINS "�����_�ਪ�" THEN
            RUN Run-PrnProc("dps_prn", gbeg-date, gend-date, ENTRY(kk, mOther-tag-list), OUTPUT vTmpStr).
         /* ���᫥��� ��� ���� "��ࠬ��� <��� ��ࠬ���>" */
         ELSE IF ENTRY(kk, mOther-tag-list) BEGINS "��ࠬ���" THEN DO:
            run param.p (OUTPUT vTmpStr, 
                         gbeg-date, 
                         gend-date, 
                         ENTRY(2, ENTRY(kk, mOther-tag-list), "@")).     
            ASSIGN vTmpStr = printtext.
         END.
         ELSE DO:
            RUN VALUE(ENTRY(kk, mOther-tag-list)) (rid_loan,
                                                   gend-date,
                                                   OUTPUT vTmpStr).

            IF RETURN-VALUE EQ "Err" THEN 
               vTmpStr = ?.     
         END.
         RUN Insert_TTName(ENTRY(kk, mOther-tag-list),
                              IF RETURN-VALUE NE "Err" THEN vTmpStr 
                                                    ELSE ?).
      END.  
   END.
END.

{norm-end.i &nofil=yes}

RUN printvd.p(mTemplateName,
              INPUT TABLE ttnames).
/* $LINTUSER='KOZV' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/4.1d/1ut/src' */
/* $LINTDATE='25/09/2014 17:17:46.616+04:00' */
/* $LINTFILE='loandoc1.p' */
/*prosign6pUyXd6FYHjXPCph/utxAg*/