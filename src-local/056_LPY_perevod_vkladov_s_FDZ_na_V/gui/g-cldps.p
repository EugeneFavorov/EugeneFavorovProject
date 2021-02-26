{globals.i}

/* +++ g-cldps.p was humbly modified by (c)blodd converter v.1.10 on 2/16/2017 8:56am +++ */

/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2014 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: G-CLDPS.P
      Comment: ��㯯���� �����⨥ �������
   Parameters: ��� ��, RECID �࠭���樨
         Uses:
      Used by:
      Created: �����
     Modified: SAP 0011372 ����� �맮�� acctcldp.p - �맮� acctclo.p.
*/              


{globals.i }
{intrface.get tmess}
def input param in-op-date like op.op-date.
def input param oprid as recid.
def stream err .
def stream err1.
def stream err2 .
def var in-status as char no-undo .
DEF VAR t-type  AS CHAR                 NO-UNDO. /* ���� ������� ��� ��ࠡ�⪨ */
def var  in-currency like currency.currency no-undo .
DEF VAR cur-n         AS CHAR NO-UNDO.
def var fl-mess as logical init no no-undo .
def var fl as logical init no no-undo .
def var fl1 as logical no-undo .
def new shared var rights-user as logical initial no no-undo.
def new shared var rights-pos  as logical initial no no-undo.
DEF VAR vI                     AS INT64               NO-UNDO. /* ���稪 ⨯�� ������� */
DEF VAR mRowId                 AS ROWID               NO-UNDO.
DEF VAR mHPQuery               AS HANDLE              NO-UNDO.
DEF VAR vOldStatus   AS CHARACTER NO-UNDO. /* ����� ������ �� ������� */
DEF VAR vdpsclRetVal AS CHARACTER NO-UNDO. /* RETURN-VALUE dpscl.p */
DEF VAR vBefTranProc AS CHARACTER NO-UNDO. /* �믄��� �� �࠭���樨, �믄� �� 蠡���� */

find op-kind where recid(op-kind) eq oprid no-lock no-error .
if not avail op-kind then return .
find first op-template of op-kind where op-template.cr-class-code begins 'dep_person' no-lock no-error .
if not avail op-template then return .
DEF VAR c-type   AS CHARACTER INITIAL '*' NO-UNDO .
{dpsproc.def}
/*���� ��� �믮��塞 ��楤��� �� �믄��� �� �࠭���樨, �᫨ ��� ������*/
vBefTranProc = GetXAttrValueEx("op-kind",
                               op-kind.op-kind, 
                               "�믄���", "").
IF vBefTranProc NE "" THEN
DO:
   IF NOT SearchPFile(vBefTranProc) THEN 
   DO:
      RUN Fill-AlertSysMes IN h_tmess("","",1,SUBSTITUTE("�� ������� ��楤�� ��蠡������ ��ࠡ�⪨ &1 !", vBefTranProc)).

      RETURN.
   END.
   ELSE 
   DO:
      RUN VALUE(vBefTranProc + ".p")
               (in-op-date,
      OUTPUT vI).
      IF vI = -1 THEN RETURN.
   END.
END.

cur-n = FGetSetting("�����悠�", ?, "{&in-NC-Code}").

rights-pos  = getThisUserXAttrValue("��ᬮ�����")  eq "��".
rights-user = getThisUserXAttrValue("��ᬮ������") eq "��".

IF GetXAttrValueEx("op-kind",op-kind.op-kind,"cont-type",?) <> ? THEN
   c-type = GetXAttrValueEx("op-kind",op-kind.op-kind,"cont-type",?).
 /* ��।������ ⨯�� ������� ��� ��ࠡ�⪨ */
FOR EACH CODE WHERE CODE.class   EQ "cont-type" 
                AND CODE.PARENT  EQ "cont-type" 
    NO-LOCK:
       IF  CAN-DO(c-type, CODE.CODE)  THEN DO:      
        {additem.i t-type CODE.CODE}
       END.
END.
{setdest.i &stream="stream err"}
{setdest.i &stream="stream err1" &filename='_spool1.tmp'}
{setdest.i &stream="stream err2" &filename='_spool2.tmp'}
RUN SetSysConf IN h_base ("gLogMessage","��").
 
cl:
for each op-template of op-kind where op-template.cr-class-code begins 'dep_person' no-lock :
  assign
    in-status     = Get_Param('loan-status',recid(op-template))
    in-currency   = Get_Param('l_currency',recid(op-template))
  .
  if in-currency = cur-n then in-currency = '' .
 
   /*����� ࠧ �믮��塞 ��楤��� �� �믄� �� 蠡���� �࠭���樨, �᫨ ��� ������*/
   vBefTranProc = GetXAttrValueEx("op-template",
                                  op-kind.op-kind + "," + STRING(op-template.op-template), 
                                  "�믄�", "").
   IF vBefTranProc NE "" AND NOT SearchPFile(vBefTranProc) THEN 
   DO:
      RUN Fill-AlertSysMes IN h_tmess("","",1,SUBSTITUTE("�� ������� ��楤�� ��蠡������ ��ࠡ�⪨ &1 蠡���� &2 !", vBefTranProc, op-template.op-template)).

      NEXT cl.
   END. 
  RUN qrybrwld.p ("dep_person",THIS-PROCEDURE:HANDLE,OUTPUT mHPQuery) NO-ERROR.
   /* � ��६����� mHBrwQuery (bstty.def) �������� ��� ����㦥���� DS-���������� */
  IF NOT VALID-HANDLE (mHPQuery)
  THEN DO:
      RUN Fill-SysMes IN h_tmess ("", "", "-1", "�訡�� ����㧪� ��楤��� �ନ஢���� ����� ��� ����� ~"dep_person~"").
      UNDO cl, LEAVE cl.
  END.

  DO vi = 1 TO NUM-ENTRIES(t-type):                    
     
     RUN SetDSContext IN mHPQuery ("icontr~001close-date1~001close-date2~001cont-type" + ( IF in-currency NE ? THEN "~001currency"
                                                      ELSE "") + ( IF in-status NE ? THEN "~001status" ELSE ""),
                                  "dps~001?~001?~001" + ENTRY(vi, t-type)  + ( IF in-currency NE ? THEN ("~001" + in-currency)
                                                  ELSE "") + ( IF in-status NE ? THEN ("~001" + in-status) ELSE ""),
                                  "SortBy" ,
                                  "loan.cont-code" ) NO-ERROR.

     IF ERROR-STATUS:ERROR THEN 
        UNDO cl, LEAVE cl.
         
      /* ����⨥ ����� */
     RUN Open-Query IN mHPQuery.
      /* ��ࠡ�⪠ �訡�� */
     IF ERROR-STATUS:ERROR THEN 
        UNDO cl, LEAVE cl.
      
   cl1:
   DO WHILE TRUE  
      transaction 
      on error undo cl1 , NEXT cl1 
      on endkey undo cl1, leave cl1:
     IF RETRY THEN .   
  
     RUN Get_Query_Record IN mHPQuery ("NEXT",
                                       "loan",
                                       OUTPUT mRowId).    
      IF mRowId EQ ? THEN
            LEAVE cl1.
       
     FIND FIRST loan WHERE ROWID(loan) EQ mRowId
     and loan.filial-id eq shFilial
          NO-LOCK NO-ERROR.
     IF AVAIL loan THEN
     DO:
        IF vBefTranProc NE "" THEN  /*ᯥ���᪠� �஢�ઠ �������!*/
        DO:
           RUN VALUE(vBefTranProc + ".p")
                    (in-op-date,
                    RECID(loan),
                    OUTPUT vdpsclRetVal).
           IF vdpsclRetVal NE "" THEN NEXT cl1.
        END.
      vOldStatus = loan.loan-status.
      run dpscl.p (loan.cont-code,in-op-date).
      vdpsclRetVal = {&RETURN_VALUE}.
      /* �᫨ ����� �� ������ - �஡㥬 ������� ��� ��� */
      
      IF vdpsclRetVal = "YES" THEN 
      do:         
         run acctclo.p (loan.cont-code,in-op-date).
         /* �᫨ ��� ���� �� "�᭮����" ��⮢ ������ �� �� ������ - 
            ���뢠�� ���⭮ ����� */
         IF {&RETURN_VALUE} EQ "NO" THEN DO:
            ASSIGN
               loan.loan-status = vOldStatus
               loan.close-date  = ?
               .
            
            /* �뢮��� ���ଠ�� � �������⮬ ������ */
            if not fl-mess then 
            do:
               put stream err unformatted '�������� ���������� �������' SKIP .
               FL-MESS = not fl-mess .
            end.
            put stream err unformatted  '����� ' + loan.cont-code  skip /*vdpsclRetVal skip(1)*/ .
            IF GetSysConf("LogMessage") NE "" AND GetSysConf("LogMessage") NE ? THEN
               put stream err unformatted  GetSysConf("LogMessage") skip.
            put stream err UNFORMATTED SKIP(1).
         END.   
         /* ���� �뢮��� ���ଠ�� � �����⮬ ������ */
         ELSE
         DO:
            if not fl then do :
               put stream err1 unformatted '�������� �������� �������' SKIP(1) .
               fl = not fl .
            end.
            put stream err1 unformatted '����� ' + loan.cont-code skip(1) .
         END.
        
        /* �᫨ �� �����⨨ ��⮢ �뫨 �訡�� - �뢮��� �� */
        if GetSysConf("LogMessage") NE "" AND GetSysConf("LogMessage") NE ? then do :
          if not  fl1  then do :
            put stream err2 unformatted '�������� ���������� ������ �� �������' skip(1).
            fl1 = not fl1 .
          END.
          PUT stream err2  unformatted '����� ' + loan.cont-code skip GetSysConf("LogMessage") skip(1) .
          RUN DeleteOldDataProtocol IN h_base ("LogMessage").
        end.
      end.
      /* �᫨ ����� �� ������ - �뢮��� ���ଠ�� �� �⮬ */
      else do :
       if not fl-mess then do :
        put stream err unformatted '�������� ���������� �������' SKIP .
        FL-MESS = not fl-mess .
       END.
       
       put stream err unformatted  '����� ' + loan.cont-code  skip {&RETURN_VALUE} skip(1) .
      end.
     end.
   END. /*cl1  */
    
   RUN Close-Query IN mHPQuery (NO).
  END.
   
  RUN Close-Query IN mHPQuery (YES).
end.
RUN DeleteOldDataProtocol IN h_base ("gLogMessage").
if fl-mess then do :
  {preview.i &stream="stream err"}
end.
if fl then do :
  {preview.i &stream="stream err1" &filename='_spool1.tmp' &nodef="/*"}
end.
if fl1  then do :
  {preview.i &stream="stream err2" &filename='_spool2.tmp' &nodef="/*"}
end.
{intrface.del}
/* $LINTUSER='BIS' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='*' */
/* $LINTDATE='25/08/2014 14:23:35.978+04:00' */
/* $LINTFILE='g-cldps.p' */
/*prosignDG+fZj3qmMaB0h30M5VrNg*/
/* --- g-cldps.p was humbly modified by (c)blodd converter v.1.10 on 2/16/2017 8:56am --- */
