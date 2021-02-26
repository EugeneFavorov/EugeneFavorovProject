{globals.i}

/* +++ pp-pacct.p was humbly modified by (c)blodd converter v.1.09 on 9/19/2016 7:56am +++ */

/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2015 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: PP-PACCT.P
      Comment: ������⥪� �㭪権 ����� ��� ��⮢
   Parameters: ���
         Uses:
      Used by:
      Created: 10.02.2004 14:30 laav    
     Modified: 28.04.2004 12:56 KSV      (0029414) ��������� �㭪樨
                                         �������_��� � �������_���.
     Modified: 12.05.2004 20:08 KSV      (0030054) ��������� �㭪�� ����.
     Modified: 14.05.2004 15:07 KSV      (0029314) ��������� �㭪��
                                         ������_����.
     Modified: 21.03.2005 17:26 KSV      (0039833) ��������� �㭪樨
                                         �������/�������
     Modified: 31.05.2005 17:39 KSV      (0047392) �������� ���ᠭ�� ����୮�
                                         �㭪樨 �������
     Modified: 16.06.2005 18:52 KSV      (0039620) ��ࠢ���� ���� ��।������
                                         � ��楤��� acct-pos � �-���
                                         �������_���/�������_���.
     Modified: 17.06.2005 12:08 KSV      (0039620) � �-樨
                                         �������_���/�������_��� ���������
                                         ����������� ������ ����楯⮢������
                                         ���⪠.
     Modified: 04.08.2007 15:41 OZMI     (0075109)
     Modified: 24.10.2007 18:06 MUTA     0082119 ��������� �㭪�� "�������_���" 
     Modified: 08.04.2008 MUTA 0090931 ��ࠡ�⪠ �����㬥�� acct-qty.
     Modified: 07.07.2011 VPA 0150535
*/

&GLOBAL-DEFINE GET-ACCT DEFINE BUFFER acct FOR acct. ~
                        FIND FIRST acct WHERE ~
                           acct.acct     = AddFilToAcct(iAcct, shFilial)   AND ~
                           acct.currency = iCurr   NO-LOCK NO-ERROR. ~
                        IF NOT AVAILABLE acct THEN ~
                        DO: ~
                           is-ok = -1. ~
                           RUN Fill-SysMes IN h_tmess("","","-1", ~
                                                      "�� ������ ��� � ����஬ " + iAcct ~
                                                    + " � ����� ������ " + iCurr). ~
                           RETURN . ~
                        END.

{globals.i}
{sh-defs.i}
{intrface.get tmess}
{intrface.get pbase}
{intrface.get xclass}
{intrface.get acct}
{intrface.get cust}
{intrface.get rsrv}
{intrface.get blkob}
{intrface.get op}
{intrface.get refer}
{intrface.get trans}
{intrface.get db2l}

DEFINE VARIABLE h_pvok AS HANDLE      NO-UNDO.
RUN pp-pvok.p PERSISTENT SET h_pvok.

{pfuncdef
   &LIBDEF        = "YES"
   &NAME          = "PACCT"
   &LIBNAME       = "������⥪� �㭪権 ����� ��� ��楢�� � ��⮢ 2-�� ���浪�."
   &DESCRIPTION   = "����ন� �㭪樨� ����� ��� ��⮢: ४������ ���, ���⪨"
   }

/* ��।������ ������ ��� */
{pfuncdef
   &NAME          = "�����_������"
   &DESCRIPTION   = "��।���� ��� ���, ��⨢�� �� ��� ���ᨢ��"
   &PARAMETERS    = "����� �����,������ �����"
   &RESULT        = "'�' - ��� ��⨢���� ��� | '�' - ��� ���ᨢ����"
   &SAMPLE        = "�����_������(@acct,@currency) = '�' "
   }

   DEFINE INPUT  PARAMETER iAcct      AS CHAR NO-UNDO. /* ����� ���*/
   DEFINE INPUT  PARAMETER iCurr      AS CHAR NO-UNDO. /* ����� ���*/
   DEFINE OUTPUT PARAMETER oUt_Result AS CHAR NO-UNDO. /* ���. ���-�: � ���� � */
   DEFINE OUTPUT PARAMETER is-ok      AS INT64  NO-UNDO. 

   {pchkpar iAcct}

   IF iCurr <> "" THEN
   DO:
      {pchkpar iCurr}
   END.

   {&GET-ACCT}

   ASSIGN
      oUt_Result = acct.side
      is-ok = 0.
END PROCEDURE.

{pfuncdef
   &NAME          = "�������_���"
   &DESCRIPTION   = "�����頥� �㡫��� ��楯⮢���� ��� ����楯⮢���� ���⮪ �� ���� �� ����"
   &PARAMETERS    = "����� �����, ������ �����[,���� = ���� �������[,������ = ~~373]]"
   &RESULT        = "�������"
   &SAMPLE        = "�������_���('10201810000020010028','',DATE('01/01/2004')) - ~
����祭�� ��楯⮢������ ���⪠ �� 01/01/2004~~n~
�������_���('10201810000020010028','') - ����祭�� ��楯⮢������ ���⪠ �� ���� ���भ�~~n~
�������_���('10201810000020010028','',����(),'�') - ����祭�� ����楯⮢������ ���⪠ �� ~
���� ���भ�"
   }
   DEFINE INPUT  PARAMETER iAcct       AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iCurrency   AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iDate       AS DATE       NO-UNDO.
   DEFINE INPUT  PARAMETER iStatus     AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER oUt_Result  AS DECIMAL    NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok       AS INT64    NO-UNDO.
   DEFINE BUFFER bacct FOR acct. 
    
   FIND FIRST bacct WHERE bacct.acct EQ iAcct NO-LOCK NO-ERROR.
   IF NOT AVAIL bacct THEN DO:
      is-ok = -1. 
      RUN Fill-SysMes IN h_tmess("","","-1","�� ������ ��� � ����஬ " + iAcct 
                                          + " � ����� ������ " + iCurrency). 
      RETURN . 
   END.

   {pchkpar iAcct}

   IF iCurrency <> "" THEN
   DO:
      {pchkpar iCurrency}
   END.

   IF iDate = ? THEN iDate = GetBaseOpDate().
   IF iStatus = ? THEN iStatus = CHR(251).
   
   RUN acct-pos IN h_base (iAcct,iCurrency,iDate,iDate,iStatus).

   oUt_Result = sh-bal.
END PROCEDURE.

{pfuncdef
   &NAME          = "�������_���"
   &DESCRIPTION   = "�����頥� ������⢮ 業��� �㬠�, � ��⮬ �᫠ � ��楯⮢����� ��� ~
����楯⮢����� �஢����� �� ���� �� ����"
   &PARAMETERS    = "����� �����, ������ �����[,���� = ���� �������[,������ = ~~373]]"
   &RESULT        = "�������"
   &SAMPLE        = "�������_���('10201810000020010028','',DATE('01/01/2004')) - ����祭�� ~
������⢠ 業��� �㬠� � ��⮬ �᫠ � ��楯�஢����� �஢����� �� 01/01/2004~~n~
�������_���('10201810000020010028','') - ����祭�� ������⢠ 業��� �㬠� � ��⮬ �᫠ � ~
��楯�஢����� �஢����孠 ���� ���भ�~~n~
�������_���('10201810000020010028','',����(),'�') - ����祭�� ������⢠ 業��� �㬠� � ~
��⮬ �᫠ � ����楯�஢����� �஢�����  �� ���� ���भ�"
   }
   DEFINE INPUT  PARAMETER iAcct       AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iCurrency   AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iDate       AS DATE       NO-UNDO.
   DEFINE INPUT  PARAMETER iStatus     AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER oUt_Result  AS DECIMAL    NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok       AS INT64    NO-UNDO.
   DEFINE BUFFER bacct FOR acct. 
    
   FIND FIRST bacct WHERE bacct.acct EQ iAcct NO-LOCK NO-ERROR.
   IF NOT AVAIL bacct THEN DO:
      is-ok = -1. 
      RUN Fill-SysMes IN h_tmess("","","-1","�� ������ ��� � ����஬ " + iAcct 
                                          + " � ����� ������ " + iCurrency). 
      RETURN . 
   END.

   {pchkpar iAcct}

   IF iCurrency <> "" THEN
   DO:
      {pchkpar iCurrency}
   END.

   IF iDate = ? THEN iDate = GetBaseOpDate().
   IF iStatus = ? THEN iStatus = CHR(251).
   
   RUN acct-qty IN h_base (iAcct,iCurrency,iDate,iDate,iStatus).

   oUt_Result = sh-qty.
END PROCEDURE.

{pfuncdef
   &NAME          = "�����_��_���"
   &DESCRIPTION   = "����� �㬬�୮�� ���⪠ �� ���� ��⠬"
   &PARAMETERS    = "������[,�������[,����[,������]]]"
   &RESULT        = "���������� ��"
   &SAMPLE        = "�����_��_���('001', '�', ����(), CHR(251)) = 1234.56789"
}

   DEFINE INPUT  PARAMETER iSecCode    AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER iSide       AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER iDate       AS DATE        NO-UNDO.
   DEFINE INPUT  PARAMETER iStatus     AS CHARACTER   NO-UNDO.
   DEFINE OUTPUT PARAMETER oUt_Result  AS DECIMAL     NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok       AS INT64     NO-UNDO.

   DEFINE BUFFER acct FOR acct.

   {pchkpar iSecCode}

   IF NOT {assigned iSide} THEN
      iSide = "�".

   IF iDate EQ ? THEN
      iDate = GetBaseOpDate().

   IF NOT {assigned iStatus} THEN
      iStatus = CHR(251).

   FOR EACH acct WHERE acct.currency EQ iSecCode
                   AND acct.acct-cat EQ "d"
                   AND acct.side     EQ iSide
   NO-LOCK:

      RUN acct-qty IN h_base (acct.acct,
                              acct.currency,
                              iDate,
                              iDate,
                              iStatus).

      oUt_Result = oUt_Result + sh-qty.
   END.
END PROCEDURE.

{pfuncdef
   &NAME          = "�������_���"
   &DESCRIPTION   = "�����頥� ������ ��楯⮢���� ��� ����楯⮢���� ���⮪ �� ���� �� ����"
   &PARAMETERS    = "����� �����, ������ �����[,���� = ���� �������[,������ = ~~373]]"
   &RESULT        = "�������"
   &SAMPLE        = "�������_���('20203840300020000000','840',DATE('01/01/2004')) - ~
����祭�� ��楯⮢������ ���⪠ �� 01/01/2004~~n~
�������_���('20203840300020000000','840') - ����祭�� ��楯⮢������ ���⪠ �� ���� ���भ�~~n~
�������_���('20203840300020000000','840',����(),'�') - ����祭�� ����楯⮢������ ���⪠ �� ~
���� ���भ�"
   }
   DEFINE INPUT  PARAMETER iAcct       AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iCurrency   AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iDate       AS DATE       NO-UNDO.
   DEFINE INPUT  PARAMETER iStatus     AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER oUt_Result  AS DECIMAL    NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok       AS INT64    NO-UNDO.
   DEFINE BUFFER bacct FOR acct. 
    
   FIND FIRST bacct WHERE bacct.acct EQ iAcct NO-LOCK NO-ERROR.
   IF NOT AVAIL bacct THEN DO:
      is-ok = -1. 
      RUN Fill-SysMes IN h_tmess("","","-1","�� ������ ��� � ����஬ " + iAcct 
                                          + " � ����� ������ " + iCurrency). 
      RETURN . 
   END.

   {pchkpar iAcct}

   IF iCurrency <> "" THEN
   DO:
      {pchkpar iCurrency}
   END.

   IF iDate = ? THEN iDate = GetBaseOpDate().
   IF iStatus = ? THEN iStatus = CHR(251).
   
   RUN acct-pos IN h_base (iAcct,iCurrency,iDate,iDate,iStatus).

   oUt_Result = sh-val.
END PROCEDURE.

{pfuncdef
   &NAME          = "����"
   &DESCRIPTION   = "�஢���� ����� ��� �� ���४⭮���, �����뢠�� ���� ~
��楢��� ��� � �।���頥� ��� ����୮� �ᯮ�짮�����. ��� ��।������ ~
��⮤� ���� ���� �ᯮ������ ���� �����, ��।���� � ����⢥ ��ࠬ���, ~
���� �ᯮ������ ����� ⥪�饣� 蠡����, ����, �᫨ ⥪�騩 蠡��� �� ��।����, ~
�ᯮ������ ����� ACCTB"
   &PARAMETERS    = "����� �����[, ��� = ��� ���[, ����� �����]]"
   &RESULT        = "����� �����"
   &SAMPLE        = "����('10201011x00000000055') = '10201011600000000055' - ~
�����뢠�� ���� ��� ��� ��襣� ����� � १�ࢨ��� ���,~~n~
����('40702810x00000000001','044585109','acctbi') = '40702810100000000001' - ~
�����뢠�� ���� ��� ��� ��㣮�� �����"
   }
   DEFINE INPUT  PARAMETER iAcct       AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iBIK        AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iClass      AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER oUt_Result  AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok       AS INT64    NO-UNDO.

   DEFINE VARIABLE vClass  AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vTmplID AS INT64    NO-UNDO.
   DEFINE VARIABLE vProg   AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vKey    AS INT64    NO-UNDO.
   DEFINE VARIABLE vMask   AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vPos    AS INT64    NO-UNDO.
   DEFINE VARIABLE vOk     AS LOGICAL    NO-UNDO.
   DEFINE VARIABLE vTokidx AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vToklen AS CHARACTER  NO-UNDO.

   DEFINE BUFFER bTmpl FOR op-kind-tmpl.
   DEFINE BUFFER bAcct FOR acct.

      
   {pchkpar iAcct}

   IF iBIK = FGetSetting("�������","","") THEN iBIK = ?.

   /* ����� �� ����� - ��६ �� ⥪�饣� 蠡���� */
   IF iClass = ? THEN
   DO:
      /* Commented by KSV: ��।��塞 ����� ��� �१ 蠡��� �࠭���樨 */
      vTmplID = GetBaseTemplate().

      FIND FIRST bTmpl WHERE 
         bTmpl.tmpl-id = vTmplID NO-LOCK NO-ERROR.

      vClass = IF NOT AVAILABLE bTmpl 
               THEN "acctb" ELSE bTmpl.work-class-code.
   END.
   ELSE
      vClass = iClass.

   /* Commented by KSV: ��।��塞 �ணࠬ�� ���� ���� */
   vprog = GET-CLASS-METHOD(vClass, "U1").
   IF vprog = ? THEN vprog = "key-tst".
   vProg = vProg + ".p".

   RUN VALUE(vProg) (iAcct,iBIK,OUTPUT vKey).

   IF vKey = ? THEN
   DO:
      RUN Fill-SysMes IN h_tmess("","acct01","","%s=" + iAcct).
      is-ok = -1.
      RETURN .
   END.

   /* Commented by KSV: ��।��塞 ������ ���� � ����� ��� */
   vMask = GetXattrInit(vClass,"acct").
   /* �᫨ 蠡��� � ���� ���� [1-5]�������������, � �訡�� � ��।������� ����樨 */
   RUN GetAcctMask IN h_acct(vMask, OUTPUT vMask, OUTPUT vTokidx, OUTPUT vToklen).

   IF {assigned vMask} THEN
      vPos = INDEX(vMask,"�").

   IF vPos = 0 THEN vPos = 9.

   SUBSTR(iAcct,vPos,1) = STRING(vKey) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN
   DO:
      RUN Fill-SysMes IN h_tmess("","acct01","","%s=" + iAcct).
      is-ok = -1.
      RETURN .
   END.
      
   IF iBIK = ? THEN DO:

      /* Commented by KSV: ����ࢨ�㥬 ��� */
      RUN AcctKeep IN h_acct(iAcct,OUTPUT vOk).
      IF vOk <> YES THEN
      DO:
         RUN Fill-SysMes IN h_tmess("","acct02","","%s=" + iAcct).
         is-ok = -1.
         RETURN .
      END.
      
      /* Commented by KSV: �஢��塞 ����� ��� �� 㭨���쭮��� */
      {find-act.i
         &bact = bAcct
         &acct = iAcct
      }
      IF AVAIL bAcct THEN
      DO:
         RUN Fill-SysMes IN h_tmess("","acct03","","%s=" + iAcct).
         is-ok = -1.
         RETURN .
      END.

   END. /* iBIK <> ? */

   oUt_Result = iAcct.

END PROCEDURE.

{pfuncdef
   &NAME          = "������_����"
   &DESCRIPTION   = "�����頥� ���� ��� ��� ��楢��� ���"
   &PARAMETERS    = "����� �����,������ �����,[� ��������[,� ��� �]]"
   &RESULT        = "����� �����"
   &SAMPLE        = "������_����('10201810000170010004','','�') = '20202810400000031001'"
   }
   DEFINE INPUT  PARAMETER iAcct       AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iCurr       AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iRemainder  AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iPass       AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER oUt_Result  AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok       AS INT64    NO-UNDO.
   
   DEF VAR vIs     AS LOG  NO-UNDO .
   DEF VAR vResult AS CHAR NO-UNDO .
   DEF VAR vOst1   AS DEC  NO-UNDO.
   DEF VAR vOst2   AS DEC  NO-UNDO.

   {pchkpar iAcct}

   IF iCurr <> "" THEN
   DO:
      {pchkpar iCurr}
   END.

   iAcct = DelFilFromAcct(iAcct).
   {&GET-ACCT}

   IF iRemainder = "��" THEN 
   DO:
      RUN acct-pos IN h_base (acct.acct,
                              acct.currency,
                              GetBaseOpDate(),
                              GetBaseOpDate(),
                              CHR(251)).

      vOst1 = IF acct.currency EQ "" THEN sh-bal ELSE sh-val.
      IF vOst1 NE 0 
      THEN oUt_Result = acct.acct.

      IF {assigned acct.contr-acct} THEN DO:
         RUN acct-pos IN h_base (acct.contr-acct,
                                 acct.currency,
                                 GetBaseOpDate(),
                                 GetBaseOpDate(),
                                 CHR(251)).
         vOst2 = IF acct.currency EQ "" THEN sh-bal ELSE sh-val.
         IF vOst1 = 0 AND vOst2 = 0 THEN DO:
            /*��।����� ���, ����� ��� ���⪠ �㫥�� */
            oUt_Result = acct.acct.
            RUN GetAcctForAP IN THIS-PROCEDURE(acct.acct,
                                               acct.side,
                                               acct.contr-acct,
                                               iPass, 
                                               OUTPUT vIs, 
                                               OUTPUT vResult ) .
            IF vIs THEN
               oUt_Result = vResult.
         END.
         ELSE IF ABS(vOst2) > ABS(vOst1) THEN 
            oUt_Result = acct.contr-acct.
      END.
   END.
   ELSE
      oUt_Result = acct.contr-acct.

END PROCEDURE.

PROCEDURE GetAccTForAP PRIVATE :
  DEF INPUT  PARAM iAcc1   AS CHAR NO-UNDO .
  DEF INPUT  PARAM iSide   AS CHAR NO-UNDO .
  DEF INPUT  PARAM iAcc2   AS CHAR NO-UNDO .
  DEF INPUT  PARAM iPasAct AS CHAR NO-UNDO .
  DEF OUTPUT PARAM oIs     AS LOG  NO-UNDO .
  DEF OUTPUT PARAM oResAcc AS CHAR NO-UNDO .
  /* �஢�ઠ ��ࠬ��஢ */
  IF NOT (iPasAct = "�"  OR
          iPasAct = "A�" OR
          iPasAct = "A" )  OR
          iPasAct = ? OR
          iPasAct = "" THEN 
  DO:
      oIs = FALSE .
      RETURN .
  END.
  /* ��।������ ����� ��� ����� */
  IF Iside = iPasAct THEN
     ASSIGN
        oIs = TRUE
        oResAcc = iacc1 .
  ELSE
     ASSIGN
        oIs = TRUE
        oResAcc = iacc2.
END PROCEDURE. /* GetAccTForAP  */

{pfuncdef
   &NAME          = "������_�����"
   &DESCRIPTION   = "�����頥� ᯨ᮪ ����� �� ����� ���.~~n~
���筮 ������ ����� ��� ᮮ⢥����� ���� �����,~~n~
�� ⥮���᪨ �������� �����, ����� �������~~n~
��᪮�쪮 ��⮢ � ��������묨 �������, �� ࠧ�묨 ����⠬�.~~n~
�᫨ �� ������� �� ������ ��� � 㪠����� ������ - �����頥��� �訡��."
   &PARAMETERS    = "����� �����"
   &RESULT        = "������ �����,������ �����,..."
   &SAMPLE        = "������_�����('10201810000170010004') = ''~~n~
������_�����('42301276000020000003') = ',276,840'"
   }
   DEFINE INPUT  PARAMETER iAcct       AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER oUt_Result  AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok       AS INT64    NO-UNDO.

   {pchkpar iAcct}

   DEFINE BUFFER acct FOR acct.
   
   is-ok = 0.
   oUt_Result = "".
   FOR EACH acct WHERE acct.acct = iAcct NO-LOCK:
     is-ok = is-ok + 1.
     oUt_Result = RIGHT-TRIM(acct.currency + "," + oUt_Result, ",").
   END.
   IF is-ok = 0 THEN DO:
      RUN Fill-SysMes IN h_tmess("","","-1","�� ������� �� ������ ��� � ������ [" + 
                                            ( IF iAcct = ? THEN "" ELSE iAcct) + "]").
      is-ok = -1.
      RETURN .
   END.
END PROCEDURE.

/* VPA 0150535 ������� ��ࠬ��� iUsrlst */
&GLOBAL-DEFINE OBOROT_PARAMS  TREAT_PARAM: ~
                              DO: ~
                                 IF iDateFrom <> ?  THEN ~
                                    vParStr = vParStr + "," + STRING(iDateFrom). ~
                                 ELSE ~
                                    LEAVE TREAT_PARAM. ~
                                 IF iDateTo <> ?  THEN ~
                                    vParStr = vParStr + "," + STRING(iDateTo). ~
                                 ELSE ~
                                    LEAVE TREAT_PARAM. ~
                                 IF iCurrency <> ?  THEN ~
                                    vParStr = vParStr + "," + iCurrency. ~
                                 ELSE ~
                                    LEAVE TREAT_PARAM. ~
                                 IF iOpStatus <> ?  THEN ~
                                    vParStr = vParStr + "," + iOpStatus. ~
                                 ELSE ~
                                    LEAVE TREAT_PARAM. ~
                                 IF iCorrAcctMask <> ?  THEN ~
                                    vParStr = vParStr + "," + iCorrAcctMask. ~
                                 ELSE ~
                                    LEAVE TREAT_PARAM. ~
                                 IF iUsrlst<> ?  THEN ~
                                    vParStr = vParStr + "," + iUsrlst. ~
                                 ELSE ~
                                    LEAVE TREAT_PARAM. ~
                              END.

{pfuncdef
   &NAME          = "�������"
   &DESCRIPTION   = "�����頥� ����� �� ���� � �㡫��"
   &PARAMETERS    = "����� �����,��� �������[,���� ������ = ���� �������[,���� ��������� = ���� ~
�������[,������[,������ ���������� = ~~373[,����� ������������������ ����� = *[,����� ~
������������� = *]]]]]]"
   &RESULT        = "������ �� �����"
   &SAMPLE        = "�������('47402810200020000055', '�') = 10000 - ����⮢� �㡫��� ����� �� ~
���� �� ���थ�� ~~n~
�������('47402810200020000055', '�', DATE('01~/01~/' + YEAR(����())), ����(), '', ~
373, '30102810*') = 200000 - �।�⮢� �㡫��� ����� � ��砫� ���� � ����ᯮ����樨 � ~
��⠬� 30102810 ~~n~"
   }
   DEFINE INPUT  PARAMETER iAcct          AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iTurnType      AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iDateFrom      AS DATE       NO-UNDO.
   DEFINE INPUT  PARAMETER iDateTo        AS DATE       NO-UNDO.
   DEFINE INPUT  PARAMETER iCurrency      AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iOpStatus      AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iCorrAcctMask  AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iUsrlst        AS CHARACTER  NO-UNDO. /* VPA 0150535 */
   DEFINE OUTPUT PARAMETER oUt_Result     AS DECIMAL    NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok          AS INT64    NO-UNDO.

   DEFINE VARIABLE vParStr AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vOk     AS LOGICAL    NO-UNDO.

   iCorrAcctMask = REPLACE(iCorrAcctMask, ",", ";").
   /* VPA 0150535 */
   iUsrlst = REPLACE(iUsrlst, ",", ";"). 
   {find-act.i &acct=iAcct }
   IF AVAIL acct THEN iAcct=acct.acct.   /* VPA 0150535 ������� ��� �����䨫���쭮� ���� */
   /* ----------- */
   IF CAN-DO("�,�",iTurnType) <> YES THEN iTurnType = ?.

   {pchkpar iAcct iTurnType}

   vParStr = iAcct + "," + iTurnType.

   {&OBOROT_PARAMS}

   RUN oborot.p("�",vParStr,GetBaseOpDate(),OUTPUT vOk,OUTPUT oUt_Result).

   IF vOk <> YES THEN is-ok = -1.

END PROCEDURE.

{pfuncdef
   &NAME          = "�������"
   &DESCRIPTION   = "�����頥� ����� �� ���� � �����"
   &PARAMETERS    = "����� �����,��� �������[,���� ������ = ���� �������[,���� ��������� = ���� ~
�������[,������[,������ ���������� = ~~373[,����� ������������������ ����� = *[,����� ~
������������� = *]]]]]]"
   &RESULT        = "������ �� �����"
   &SAMPLE        = "�������('47402840500020000055', '�') = 10000 - ~
����⮢� ������ ����� �� ���� �� ���थ�� ~~n~
�������('47402840500020000055', '�', DATE('01~/01~/' + YEAR(����())), ����(), '840', ~
373, '30102840*') = 200000 - �।�⮢� ������ ����� � ��砫� ���� � ����ᯮ����樨 � ~
��⠬� 30102840 ~~n~"
   }
   DEFINE INPUT  PARAMETER iAcct          AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iTurnType      AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iDateFrom      AS DATE       NO-UNDO.
   DEFINE INPUT  PARAMETER iDateTo        AS DATE       NO-UNDO.
   DEFINE INPUT  PARAMETER iCurrency      AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iOpStatus      AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iCorrAcctMask  AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iUsrlst        AS CHARACTER  NO-UNDO.  /* VPA 0150535 */
   DEFINE OUTPUT PARAMETER oUt_Result     AS DECIMAL    NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok          AS INT64    NO-UNDO.

   DEFINE VARIABLE vParStr AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vOk     AS LOGICAL    NO-UNDO.

   iCorrAcctMask = REPLACE(iCorrAcctMask, ",", ";").
   /* VPA 0150535 */
   iUsrlst = REPLACE(iUsrlst, ",", ";"). 
   {find-act.i &acct=iAcct }
   IF AVAIL acct THEN iAcct=acct.acct.     /* VPA 0150535 ������� ��� �����䨫���쭮� ���� */
   /* ----------- */
   IF CAN-DO("�,�",iTurnType) <> YES THEN iTurnType = ?.

   {pchkpar iAcct iTurnType}

   vParStr = iAcct + "," + iTurnType.

   {&OBOROT_PARAMS}

   RUN oborot.p("�",vParStr,GetBaseOpDate(),OUTPUT vOk,OUTPUT oUt_Result).

   IF vOk <> YES THEN is-ok = -1.

END PROCEDURE.

{pfuncdef
   &NAME          = "������������_�����"
   &DESCRIPTION   = "�����頥� ������������ ��� �� ��� ������, ���� ������ � ���� 䨫����."
   &PARAMETERS    = "����� ����� [,������] [,������]"
   &RESULT        = "������������ �����"
   &SAMPLE        = "������������_�����('30122978100090000055','978')='��� ��᮫��'~~n~
������������_�����('10201810300020010029')='��⠢�� 䮭� ��� ���⠧��'"
}
   DEFINE INPUT  PARAMETER iAcct       AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iCurr       AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iFilial     AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER oUt_Result  AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok       AS INT64    NO-UNDO.

   DEFINE VARIABLE vName1        AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vName2        AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vCustINN      AS CHARACTER NO-UNDO.

   IF iFilial EQ ? THEN
      iFilial = ShFilial.
   IF iCurr EQ ? THEN
      {find-act.i
          &bacct  = acct
          &acct   = iAcct
          &filial = iFilial
      }
   ELSE
      {find-act.i 
          &bacct  = acct
          &acct   = iAcct
          &filial = iFilial
          &curr   = iCurr
      }
   IF AVAILABLE (acct) THEN DO:
      RUN GetCust IN h_base (BUFFER acct,
                             NO, NO,
                             OUTPUT vName1, 
                             OUTPUT vName2, 
                             OUTPUT vCustINN).
      oUt_Result = TRIM (vName1 + " " + vName2).
      is-OK = 0.
   END.
   ELSE DO:
      oUt_Result = "������".
      is-OK = 0.
   END.

END PROCEDURE.

{pfuncdef
   &NAME          = "���������"
   &DESCRIPTION   = "�����頥� ���祭�� �� �������� � �㦭�� �ଠ�:~~n~
1, � ���祭�� ����� ���: N <���������> �� <��/��/����>~~n~
2, � ���祭�� ����� ���: <��/��/����>~~n~
3, � ���祭�� ����� ���: <���������>~~n~
�� 㬮�砭�� 1."
   &PARAMETERS    = "����� �����,������[,������]"
   &RESULT        = "�������� �� �������� � ������ �������"
   &SAMPLE        = "���������('30122978100090000055','978',2)"
}
   DEFINE INPUT  PARAMETER iAcct       AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iCurr       AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iFormat     AS INT64    NO-UNDO.
   DEFINE OUTPUT PARAMETER oUt_Result  AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok       AS INT64    NO-UNDO.

   DEFINE VARIABLE vTmp AS CHARACTER   NO-UNDO.

   {find-act.i &acct=iAcct
               &curr=iCurr}

    IF AVAIL(acct) THEN
       vTmp = GetXattrValue("acct",Acct.acct + "," + acct.currency,"��������").

   IF vTmp NE "" THEN
      CASE iFormat:
         WHEN 1 THEN
            oUt_Result = "N " + ( IF NUM-ENTRIES(vTmp) EQ 2 THEN
                                    ENTRY(2,vTmp)
                                 ELSE "") + 
                         " �� " + ENTRY(1,vTmp).
         WHEN 2 THEN 
            oUt_Result = ENTRY(1,vTmp).
         WHEN 3 THEN 
            oUt_Result = IF NUM-ENTRIES(vTmp) EQ 2 THEN
                            ENTRY(2,vTmp)
                         ELSE "". 
         OTHERWISE
             oUt_Result = "N " + ( IF NUM-ENTRIES(vTmp) EQ 2 THEN
                                     ENTRY(2,vTmp)
                                  ELSE "") + 
                          " �� " + ENTRY(1,vTmp).
      END CASE.
   ELSE 
      oUt_Result = "".

   is-OK = 0.

END PROCEDURE.

{pfuncdef
   &NAME          = "����_��_����"
   &DESCRIPTION   = "��।���� ��� �� ��������� �����ᮢ��� ���� 2-�� ���浪�, ~
���ࠧ������� � ��⥣�ਨ ���"
   &PARAMETERS    = "���������� ���� 2-�� �������,�������������[,��������� �����]"
   &RESULT        = "����� ���"
   &SAMPLE        = "����_��_����("30102","002") = '30102-011-3-0002-0000000'"
   }

   DEFINE INPUT  PARAMETER iBalAcct    AS CHARACTER   NO-UNDO. /* ��� 2-�� ���浪� */
   DEFINE INPUT  PARAMETER iBranch     AS CHARACTER   NO-UNDO. /* ���ࠧ������� */
   DEFINE INPUT  PARAMETER iAcctType   AS CHARACTER   NO-UNDO. /* ��⥣��� ��� */
   DEFINE OUTPUT PARAMETER oUt_Result  AS CHARACTER   NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok       AS INT64     NO-UNDO.
   
   DEFINE BUFFER acct FOR acct.

   {pchkpar iBalAcct iBranch}

   FIND FIRST acct WHERE acct.bal-acct  EQ INT64(iBalAcct)
                     AND acct.branch-id EQ iBranch
                     AND acct.close-date EQ ?
                     AND acct.acct-cat  EQ IF NOT {assigned iAcctType} 
                                           THEN "b"
                                           ELSE iAcctType NO-LOCK NO-ERROR.
   IF AVAIL acct THEN
      ASSIGN
         oUt_Result = acct.acct
         is-ok      = 0
      .
END PROCEDURE.


{pfuncdef
   &NAME          = "����_��_������"
   &DESCRIPTION   = "��।���� ��� �� ��������� �����祭��, ������᪮�� ����, ~
����� � ���� 2-�� ���浪�"
   &PARAMETERS    = "����������,���������� ����,������,���������� ���� 2-�� �������"
   &RESULT        = "����� ���"
   &SAMPLE        = "����_��_������('��⊭�','40817840000010000091','840','47407') ~
= '47407840500001000202'"
   }

   DEFINE INPUT  PARAMETER iContract   AS CHARACTER   NO-UNDO. /* �����祭�� */
   DEFINE INPUT  PARAMETER iClAcct     AS CHARACTER   NO-UNDO. /* ������᪨� ��� */
   DEFINE INPUT  PARAMETER iCurr       AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER iBalAcct    AS CHARACTER   NO-UNDO. /* ��� 2-�� ���浪� */
   DEFINE OUTPUT PARAMETER oUt_Result  AS CHARACTER   NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok       AS INT64     NO-UNDO.
   
   {pchkpar iContract iClAcct iCurr iBalAcct }

   DEFINE BUFFER acct  FOR acct.
   DEFINE BUFFER cAcct FOR acct.

   oUt_Result = "".

   {find-act.i
      &bact = cAcct
      &acct = iClAcct}
      
   IF AVAIL cAcct THEN DO:
      FIND FIRST acct WHERE acct.contract EQ iContract
                        AND acct.cust-cat EQ cAcct.cust-cat
                        AND acct.cust-id  EQ cAcct.cust-id
                        AND acct.bal-acct EQ INT64 (iBalAcct)
                        AND acct.currency EQ iCurr
         NO-LOCK NO-ERROR.
      IF AVAIL acct THEN
         oUt_Result = acct.acct.
   END.

   is-ok = 0.

END PROCEDURE.


{pfuncdef
   &NAME          = "�窑���"
   &DESCRIPTION   = "�����頥� ���� ᢮����� ����� ���"
   &PARAMETERS    = "����_2_�������, ���_������, 7_���������_����_���_�������,
                     ���_����_������_������[, ���[, ���_����[,���������]]]"
   &RESULT        = "����_����_������_����� + ���"
   &SAMPLE        = "�窑���(�����,���,�������,��[,���[,���_����[,���������]]]) 
                     = ����(��)[+ ���]"
   }
   DEFINE INPUT  PARAMETER iBalAcct    AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iSecCod     AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iUnc        AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iForex      AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iStep       AS INT64    NO-UNDO.
   DEFINE INPUT  PARAMETER iInitCnt    AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iAcctCat    AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER oUt_Result  AS CHARACTER INIT "" NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok       AS INT64    NO-UNDO.

   DEFINE VARIABLE vCnt AS CHARACTER INIT "" NO-UNDO.

   DEFINE BUFFER acct FOR acct.
   
   {pchkpar iBalAcct iSecCod iUnc iForex}
   
   vCnt = fGetSetting( "���", "����窑���", "").
   is-ok = 0.
   IF vCnt NE "" THEN
   DO:
      oUt_Result = vCnt.
      RETURN.
   END.

   IF iInitCnt EQ ? THEN
       iInitCnt = "01".

   IF iStep EQ 0 OR iStep EQ ? THEN
       iStep = 1.

   IF iAcctCat EQ ? THEN
       iAcctCat = "f".

   FOR EACH acct WHERE acct.bal-acct             EQ INT64(iBalAcct)
                   AND acct.currency             EQ iSecCod
                   AND acct.acct-cat             EQ iAcctCat
                   AND SUBSTRING(acct.acct,10,7) EQ iUnc
                   AND SUBSTRING(acct.acct,19,2) EQ iForex
                   /* USE-INDEX acct-curr */ NO-LOCK:
       IF SUBSTRING(acct.acct,17,2) GT vCnt THEN
           vCnt = SUBSTRING(acct.acct,17,2).
   END.

   IF vCnt NE "" THEN
   DO:
       IF INT64(vCnt) + iStep LT 10 THEN 
           oUt_Result = "0".
       oUt_Result = oUt_Result + STRING(INT64(vCnt) + iStep).
   END.                                           

   ELSE
       oUt_Result = iInitCnt.
END PROCEDURE.

   /* ��� ��।������ ���祭�� ���.४����� �����ਡ���� */
   /* ===================================--=-=-=-=-= */
{pfuncdef
   &NAME          = "���_����������"
   &DESCRIPTION   = "��� ��।������ ���祭�� ���.४����� �����ਡ����"
   &PARAMETERS    = "����� �����"
   &RESULT        = "���祭�� �� ��� ��⠭����"
   &SAMPLE        = "���_����������(@acct)"
   }
   DEF INPUT  PARAM iAcct      AS CHAR NO-UNDO.   /* ����� ��� */
   DEF OUTPUT PARAM oUt_Result AS CHAR NO-UNDO.   /* �������   */
   DEF OUTPUT PARAM is-ok      AS INT64  NO-UNDO. 

   DEF VAR vResult   AS CHAR NO-UNDO.
   
   {pchkpar iAcct}

   RUN GetCodeSpodPU IN h_acct (iAcct,
                                GetBaseOpDate(),
                                OUTPUT vResult).
   ASSIGN
      oUt_Result = vResult
      is-ok      = 0
   .
END PROCEDURE.

{pfuncdef
   &NAME          = "�������"
   &DESCRIPTION   = "��।���� ����� 䨧��� �� ����, ������ �-�� ������ ~
� �⠭������ �࠭������.~~n~
��ࠬ��� ��� ������ ����� ����� ���祭��:~~n~
1  - �������~~n~
2  - ���, �����⢮~~n~
3  - ��� ��࠭�~~n~
4  - ����~~n~
5  - ⨯ ���㬥��~~n~
6  - ���(�����) ���㬥��~~n~
7  - ��� �뤠� (� �/�) + ��� �뤠� ���㬥��~~n~
8  - ��� �뤠� ���㬥��~~n~
9  - ��� �뤠� (� �/�)~~n~
10 - ��� �뤠� (��� �/�)~~n~
11 - ��� ���ࠧ�������~~n~
13 - ���"
   &PARAMETERS    = "��� ������,����[,������]"
   &RESULT        = "������"
   &SAMPLE        = "�������("11","40817810000020000025") = '520102'"
   }
   DEFINE INPUT  PARAMETER iType      AS INT64 NO-UNDO. /* ⨯ ���ଠ樨 */
   DEFINE INPUT  PARAMETER iAcct      AS CHAR    NO-UNDO. /* ��� */
   DEFINE INPUT  PARAMETER iCurrency  AS CHAR    NO-UNDO. /* ����� */
   DEFINE OUTPUT PARAMETER oUt_Result AS CHAR    NO-UNDO INIT ?.
   DEFINE OUTPUT PARAMETER is-ok      AS INT64 INIT -1.

   DEFINE VAR vResult AS CHAR NO-UNDO.
   DEFINE BUFFER xacct FOR acct.
   
   {pchkpar iType iAcct}

   IF iCurrency <> ? THEN DO:
      iCurrency = STRING(INT64(iCurrency),"999").
      {find-act.i
          &bact = xacct
          &acct = iAcct
          &curr = iCurrency
      }
   END.
   ELSE DO:
      {find-act.i
          &bact = xacct
          &acct = iAcct
      }
   END.
   IF AVAIL xacct THEN DO:
      IF xacct.cust-cat = "�"
      THEN RUN GetCustInfo2 IN h_cust (iType,
                                       xacct.acct,
                                       xacct.currency,
                                       OUTPUT vResult).
      ELSE vResult = ?.
      ASSIGN
         oUt_Result = vResult
         is-ok      = 0
      .
   END.
END PROCEDURE.

/* �஢�ઠ ��⮢ �� ��୮��� � ��뢠��� ��� ��⮢, �᫨ ��� �� ���� */
/* ===================================--=-=-=-=-= */
{pfuncdef
   &NAME          = "�������_�������"
   &DESCRIPTION   = "�஢�ઠ ��⮢ �� ��୮��� � ��뢠��� ��� ��⮢, �᫨ ��� �� ����"
   &PARAMETERS    = "����� �����1,����� �����2"
   &RESULT        = " "
   &SAMPLE        = "�������_�������('40817840000010000091','42309840000000930091')"
   }
   DEF INPUT  PARAM iAcct1     AS CHAR   NO-UNDO.   /* ����� ���1 */
   DEF INPUT  PARAM iAcct2     AS CHAR   NO-UNDO.   /* ����� ���2 */
   DEF OUTPUT PARAM oUt_Result AS CHAR   NO-UNDO.   /* �������   */
   DEF OUTPUT PARAM is-ok      AS INT64  NO-UNDO. 

   DEF BUFFER bAcct FOR acct.
   DEF BUFFER xAcct FOR acct.
   
   {pchkpar iAcct1}
   {pchkpar iAcct2}
   {find-act.i
         &bact = bAcct
         &acct = iAcct1
         &lockac = "EXCLUSIVE-LOCK"         
   }
   {find-act.i
         &bact   = xAcct
         &acct   = iAcct2
         &lockac = "EXCLUSIVE-LOCK"
   } 
   IF     {assigned bAcct.contr-acct} 
      AND bAcct.contr-acct NE xAcct.contr-acct THEN 
   DO:
      is-ok = -1.
      RUN Fill-SysMes IN h_tmess("","","-1","� ��� " + iAcct1 + " 㦥 ���� ��㣮� ���� ���." +
                                            " ���������� ᤥ���� ��� " + iAcct2 + " ����").
      RETURN. 
   END.
   IF     {assigned xAcct.contr-acct} 
      AND xAcct.contr-acct ne bAcct.contr-acct THEN 
   DO:
      is-ok = -1.
      RUN Fill-SysMes IN h_tmess("","","-1","� ��� " + iAcct2 + " 㦥 ���� ��㣮� ���� ���" +
                                            " ���������� ᤥ���� ��� " + iAcct1 + " ����").
      RETURN. 
   END.
   IF NOT {assigned xAcct.contr-acct} THEN 
   DO:
      ASSIGN
         xAcct.contr-acct = bAcct.acct
         bAcct.contr-acct = xAcct.acct
      NO-ERROR. 
      IF ERROR-STATUS:ERROR THEN 
      DO:
         is-ok = -1.
         RUN Fill-SysMes IN h_tmess("","","-1",ERROR-STATUS:GET-MESSAGE(1)). 
      END.   
      VALIDATE xAcct NO-ERROR.
      IF ERROR-STATUS:ERROR THEN 
      DO:
         is-ok = -1.
         RUN Fill-SysMes IN h_tmess("","","-1",ERROR-STATUS:GET-MESSAGE(1)). 
      END.      
      VALIDATE bAcct NO-ERROR.
      IF ERROR-STATUS:ERROR THEN 
      DO:
         is-ok = -1.
         RUN Fill-SysMes IN h_tmess("","","-1",ERROR-STATUS:GET-MESSAGE(1)). 
      END.
   END.
   
   ASSIGN
      oUt_Result = ""
      is-ok      = 0
   . 
END PROCEDURE.

/* �஢�ઠ ��⮢ �� ��୮��� � ��뢠��� ��� ��⮢, �᫨ ��� �� ���� */
/* ===================================--=-=-=-=-= */
{pfuncdef
   &NAME          = "����_����"
   &DESCRIPTION   = "������� ���, �᫨ 㪠���� ஫�, ⨯ ������� � ����� �������, ~
� �ਢ�뢠�� � ��������"
   &PARAMETERS    = "����� �����,������[,���� �����,��� ��������,����� ��������]"
   &RESULT        = "����� �����"
   &SAMPLE        = "����_����('40817810������������','','�।��','�।��','12131@002')"
   }
   DEF INPUT  PARAM iMask      AS CHAR  NO-UNDO.
   DEF INPUT  PARAM iCurrency  AS CHAR  NO-UNDO.
   DEF INPUT  PARAM iAcctRole  AS CHAR  NO-UNDO.
   DEF INPUT  PARAM iContract  AS CHAR  NO-UNDO.
   DEF INPUT  PARAM iContcode  AS CHAR  NO-UNDO.
   DEF OUTPUT PARAM oUt_Result AS CHAR  NO-UNDO.   /* �������   */
   DEF OUTPUT PARAM is-ok      AS INT64 NO-UNDO. 
   
   DEF VAR vPos0  AS INT64 NO-UNDO. /* ������ � 蠡���� */
   DEF VAR vPos1  AS INT64 NO-UNDO. /* ������ � �����  */ 
   DEF VAR vNumb  AS CHAR  NO-UNDO.
   DEF VAR vOtdel AS CHAR  NO-UNDO.

   DEFINE BUFFER bal-acct FOR bal-acct.
   DEFINE BUFFER loan FOR loan.
   DEFINE BUFFER acct FOR acct.
   DEFINE BUFFER loan-acct FOR loan-acct.

   FIND FIRST bal-acct WHERE
              bal-acct.bal-acct = INT64(SUBSTRING(iMask,1,5))
   NO-LOCK NO-ERROR.
   IF NOT AVAILABLE bal-acct THEN
   DO:
      is-ok = -1.
      RUN Fill-SysMes IN h_tmess("","","-1","�� ������ �����ᮢ� ��� ��ண� ���浪� " + 
                                            SUBSTRING(iMask,1,5)).
   END.
   
   IF {assigned iContract} THEN
   DO:
      FIND FIRST loan WHERE loan.contract  eq iContract
                        AND loan.cont-code EQ iContcode
      NO-LOCK NO-ERROR.
      IF NOT AVAIL loan THEN
      DO:          
         is-ok = -1.
         RUN Fill-SysMes IN h_tmess("","","-1","�� ������ ������� ⨯ " + 
                                               iContract + " ����� " + iContcode).
      END.
      IF INDEX(iMask, "g") > 0 THEN
      DO:
          ASSIGN
             vPos0 = LENGTH(iMask)
             vNumb = FILL("0", vPos0) + STRING(YEAR(loan.open-date))
             vPos1 = LENGTH(vNumb).
          DO WHILE TRUE:
             vPos0 = R-INDEX(iMask, "g" , vPos0).
             IF vPos0 <= 0 THEN LEAVE.
             ASSIGN
                SUBSTR(iMask, vPos0) = SUBSTR(vNumb, vPos1 , 1)
                vPos1 = vPos1 - 1
                vPos0 = vPos0 - 1.
          END.
      END. 
      vOtdel = loan.filial-id.
   END.
   IF vOtdel EQ "" THEN
      vOtdel = TRIM(GetUserBranchId(userid("bisquit"))).
   RELEASE acct NO-ERROR.
   /* ᮧ���� ��� */
   RUN Cm_acct_cr IN h_acct (
          "acct" + bal-acct.acct-cat,  /* iClass                  */
          bal-acct.bal-acct,           /* iBal                    */
          iCurrency,                   /* iCurr                   */
          '',                          /* iCustCat                */
          '',                          /* iCustID                 */
          gend-date,                   /* iOpenDate               */
          OUTPUT oUt_Result,           /* oAcct                   */
          BUFFER acct,                 /* BUFFER iacct FOR acct . */
          iMask,                       /* iAcctMask               */
          '',                          /* iDetails                */
          '',                          /* iKauId                  */
          '',                          /* iContract               */
          USERID ('bisquit'),          /* iUserId                 */
          vOtdel,                      /* iBranchId               */
          YES                          /* iCopyBalXattr           */
   ) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN 
   DO:
      is-ok = -1.
      RUN Fill-SysMes IN h_tmess("","","-1",ERROR-STATUS:GET-MESSAGE(1)). 
   END.
   RUN BalToAcct_Xattr IN h_acct(RECID(acct),"*",YES,YES).
   IF {&RETURN_VALUE} EQ "ERROR" THEN
   DO:
      is-ok = -1.
      RUN Fill-SysMes IN h_tmess("","","-1",
                                 "�訡�� �� ���樠������ ���.४����⮢ � ��� " +
                                 "2-�� ���浪� � �� �����䨪��� [��᪨��᫥�]").
   END.
   /* �᫨ ��।�� ������� � ஫� ���, � �ਢ�뢠�� ��� */
   IF     {assigned iAcctRole}
      AND {assigned iContcode} THEN
   DO:
      CREATE loan-acct.
      loan-acct.cont-code = loan.cont-code.
      {lacc.ini
          &loan-acct = loan-acct
          &contract  = loan.contract
          &acct      = acct.acct
          &currency  = acct.currency
          &acct-type = iAcctRole
          &c-since   = gend-date
      }
      RELEASE loan-acct.
   END.
   RELEASE acct.
END PROCEDURE.

{pfuncdef
   &NAME          = "�����_���"
   &DESCRIPTION   = "�����頥� �㬬� १�ࢠ �� ����"
   &PARAMETERS    = "����� �����, ������ �����[,���� = ���� �������]"
   &RESULT        = "�����"
   &SAMPLE        = "�����_���('10201810000020010028','',DATE('01/01/2012'))~ 
- ����祭�� �㬬� १�ࢠ �� 01/01/2012"
   }
   DEFINE INPUT  PARAMETER iAcct       AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iCurrency   AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iDate       AS DATE       NO-UNDO.
   DEFINE OUTPUT PARAMETER oUt_Result  AS DECIMAL    NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok       AS INT64    NO-UNDO.

   {pchkpar iAcct}

   IF iCurrency <> "" THEN
   DO:
      {pchkpar iCurrency}
   END.

   IF iDate = ? THEN iDate = GetBaseOpDate().
   oUt_Result  = DesReserveAmount(iAcct,
                                  iCurrency,
                                  iDate).
   IF oUt_Result = ? THEN oUt_Result = 0.

END PROCEDURE.

{pfuncdef
   &NAME          = "����_��"
   &DESCRIPTION   = "�����頥� ᯨ᮪ �����஢�� ���"
   &PARAMETERS    = "����� �����,������ �����"
   &RESULT        = "������ ����������"
   &SAMPLE        = "����_��('10201810000170010004,') = '������'"
   }
   DEFINE INPUT  PARAMETER iAcct       AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iCurrency   AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER oUt_Result  AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok       AS INT64    NO-UNDO.

   {pchkpar iAcct}

   oUt_Result = BlockAcct(iAcct + "," + iCurrency,DATETIME(gend-date,MTIME)).

END PROCEDURE.

{pfuncdef
   &NAME          = "����_�������"
      &DESCRIPTION   = "�����뢠�� ���⮪ �� ��� ������ �� ��㫥 ���줮 �� ~~n~
������� ���� + ���㬥��� ���᫥��� � '��릥' - ~~n~
���㬥��� ᯨᠭ�� - ���㬥��� � �2"
   &PARAMETERS    = "����� ���,����� ���,��� ��砫� ��ਮ��,��� ���� ��ਮ��, ~~n~
����� �� �� '�⠭���/AccessStatus', ����� ��� �� ᮢ������� � ~~n~
��᪮� AccessAcct ��⮢, ��楤�� ����� ���⪠(cli-pos), ~~n~
���뢠�� ���㬥��� � �2,��।����� ���⥦�"
   &RESULT        = "���⮪ �� ���"
   &SAMPLE        = "����_�������(@acct(10),@currency(10),���(),���(),~~n~
����ன��('�⠭���','AccessStatus','�'),'�','cli-pos',���,*)"
   }
   DEFINE INPUT  PARAMETER iAcct       AS CHARACTER   NO-UNDO. 
   DEFINE INPUT  PARAMETER iCurr       AS CHARACTER   NO-UNDO. 
   DEFINE INPUT  PARAMETER iDate       AS DATE        NO-UNDO.
   DEFINE INPUT  PARAMETER iDate2      AS DATE        NO-UNDO.
   DEFINE INPUT  PARAMETER iStat       AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER iStat2      AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER iProc       AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER iCrd        AS LOGICAL     NO-UNDO.
   DEFINE INPUT  PARAMETER iSeq        AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER iBlk        AS CHARACTER   NO-UNDO.
   DEFINE OUTPUT PARAMETER oUt_Result  AS DECIMAL     NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok       AS INTEGER     NO-UNDO.

   DEFINE VARIABLE         vBal        AS DECIMAL     NO-UNDO.
   DEFINE VARIABLE         vVal        AS DECIMAL     NO-UNDO.
   DEFINE VARIABLE         vBlk        AS LOGICAL     NO-UNDO.

   {pchkpar iAcct iCurr iDate iDate2 iStat iStat2 iProc iCrd iSeq iBlk}

   /* ����᪠�� ��楤��� ����� ���⪠ � ࠤ㥬�� */
   IF NOT {assigned iSeq} THEN iSeq = "*".
   IF NOT {assigned iBlk} THEN vBlk = no.
                          ELSE vBlk = CAN-DO("YES,Yes,yes,��,��,��",iBlk).

   RUN CalcAvailPos IN h_op(iAcct,iCurr,iDate,iDate2,iStat,iStat2,iProc,iCrd,iSeq,vBlk,
                            OUTPUT vBal,OUTPUT vVal).

   IF iCurr EQ ""
      THEN ASSIGN
              oUt_Result = vBal
              is-ok = 1
           .
      ELSE ASSIGN
              oUt_Result = vVal
              is-ok = 1
           .

END PROCEDURE.

/* �����⢫�� ����஢���� ���. ४����⮢ � ��� 2-�� ���浪�
   �� ��楢�� ���. �����⢫�� ���樠������ ���. ४����⮢,
   㪠������ � ������䨪��� ��᪨��᫥� �� ��楢�� ���.  */
/* ===================================--=-=-=-=-= */
{pfuncdef
   &NAME          = "��_�_�����"
   &DESCRIPTION   = "������� �� � ��� 2-�� ���浪� �� ��楢�� ���."
   &PARAMETERS    = "����� �����,������ �����"
   &RESULT        = "��_�_�����"
   &SAMPLE        = "��_�_�����('40817810200215441532','')"
   }
   DEF INPUT  PARAM iAcct      AS CHAR  NO-UNDO.
   DEF INPUT  PARAM iCurrency  AS CHAR  NO-UNDO.
   DEF OUTPUT PARAM oUt_Result AS CHAR  NO-UNDO.   /* �������   */
   DEF OUTPUT PARAM is-ok      AS INT64 NO-UNDO. 
   
   DEF BUFFER acct FOR acct.

   FIND FIRST acct WHERE
              acct.acct     EQ iAcct
          AND acct.currency EQ iCurrency
   NO-LOCK NO-ERROR.
   IF NOT AVAILABLE acct THEN
   DO:
      is-ok = -1.
      RUN Fill-SysMes IN h_tmess("","","-1","�� ������ ��� " + iAcct + " � ����� " + iCurrency).
   END.
         
   RUN BalToAcct_Xattr IN h_acct(RECID(acct),"*",YES,YES).
   IF {&RETURN_VALUE} EQ "ERROR" THEN
   DO:
      is-ok = -1.
      RUN Fill-SysMes IN h_tmess("","","-1",
                                 "�訡�� �� ���樠������ ���.४����⮢ � " +
                                 "��� 2-�� ���浪� � �� �����䨪��� [��᪨��᫥�]").
   END.
END PROCEDURE.

{pfuncdef
   &NAME          = "����_��_��"
   &DESCRIPTION   = "�����頥� ᯨ᮪ ��������� �����஢�� ��� ��� ~~n~
㪠������ ��।���� ���⥦�"
   &PARAMETERS    = "����� �����,������ �����,����������� �������"
   &RESULT        = "������ ����������"
   &SAMPLE        = "����_��_��('10201810000170010004','','6') = '������'"
   }
   DEFINE INPUT  PARAMETER iAcct       AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iCurrency   AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iSeq        AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER oUt_Result  AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok       AS INT64    NO-UNDO.

   {pchkpar iAcct iCurrency iSeq}

   IF NOT {assigned iSeq} THEN iSeq = "*".

   oUt_Result = BlckAcctOrdPay(iAcct + "," + iCurrency,DATETIME(gend-date,MTIME),iSeq).

END PROCEDURE.

{pfuncdef
   &NAME          = "��_�������"
   &DESCRIPTION   = "��।������ ᢥ����� � �������⢥ ������ ����"
   &PARAMETERS    = "����� ����,����� ����[,���=��� ���भ�]"
   &RESULT        = "���᮪ �� 4 ����⮢ � ࠧ����⥫�� CHR(1),~
 �� ����� ���ண� �஬� 1-�� ����� ���� �����. �������� ᯨ᪠:~
   - ���� �� ������ �����⮬ (��/���);~
   - �⠤�� �������⢠ ������ (code.code �� �����䨪��� ����_����������⢠);~
   - ����� ���� � �������⢥ (��ப� �ந����쭮�� �ଠ�);~
   - ��� �襭�� � �������⢥ ������ (� �ଠ� 99.99.99)."
   &SAMPLE        = "@TmpStr = ��_�������(@acct(10), @currency(10), @OpDate)"
   }
   DEFINE INPUT  PARAMETER iAcct     LIKE acct.acct     NO-UNDO.
   DEFINE INPUT  PARAMETER iCurrency LIKE acct.currency NO-UNDO.
   DEFINE INPUT  PARAMETER iDate     AS   DATE          NO-UNDO.
   DEFINE OUTPUT PARAMETER oResult   AS   CHARACTER     NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok     AS   INT64         NO-UNDO.

   DEFINE BUFFER acct FOR acct.

   DEFINE VARIABLE vIsBankrupt AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE vStage      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vDocNum     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vEndDate    AS DATE      NO-UNDO.
   DEFINE VARIABLE vS          AS CHARACTER NO-UNDO.

   {pchkpar iAcct iCurrency}

   {find-act.i &acct = iAcct
               &curr = iCurrency}
   IF AVAIL(acct) AND CAN-DO(FGetSetting("����������","���������",""),
                             acct.contract)
   THEN vIsBankrupt = YES.
   ELSE                                          
   RUN GetBankrupcyInfo IN h_acct (iAcct,
                                   iCurrency,
                                   iDate,
                                   OUTPUT vIsBankrupt,
                                   OUTPUT vStage,
                                   OUTPUT vDocNum,
                                   OUTPUT vEndDate).
   IF vIsBankrupt = ? THEN
      is-ok = -1.
   ELSE DO:
      vS = IF vIsBankrupt THEN "��" ELSE "���".
      {additem3.i oResult vS 1}
      vS = IF {assigned vStage} THEN vStage ELSE "".
      {additem3.i oResult vS 1}
      vS = IF {assigned vDocNum} THEN vDocNum ELSE "".
      {additem3.i oResult vS 1}
      vS = IF vEndDate = ? THEN "" ELSE STRING(vEndDate, "99.99.99").
      {additem3.i oResult vS 1}
   END.
END PROCEDURE.

{pfuncdef
   &NAME          = "�����_�����"
   &DESCRIPTION   = "�����頥� ����� ��� �� ���� � ��ࠬ��ࠬ ��  ~~n~
���� �࠭���樨"
   &PARAMETERS    = "��� �����"
   &RESULT        = "����� �����"
   &SAMPLE        = "�����_�����('������')"
   }

   DEFINE INPUT  PARAMETER iCode   AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iPref   AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iDate   AS DATE       NO-UNDO.
   DEFINE OUTPUT PARAMETER oResult AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok   AS INT64      NO-UNDO.

   DEFINE BUFFER DataBlock FOR DataBlock.
   DEFINE BUFFER DataAttr  FOR DataAttr.

   DEFINE VARIABLE vDataClass-Id AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vDateP446     AS DATE        NO-UNDO.
   DEFINE VARIABLE vDebug        AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE vValue        AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vCodeAttr     AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vValueLst     AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vValueLst2    AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vOldPars      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vOldParams    AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vCurOldParam  AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vParams       AS CHARACTER   NO-UNDO EXTENT 10.
   DEFINE VARIABLE vCounter      AS INT64       NO-UNDO.
   DEFINE VARIABLE vCustId       AS INT64       NO-UNDO.

   IF iDate EQ ? THEN
      iDate = gend-date.

   IF NOT {assigned iPref} THEN
      iPref = "".

   vDebug = GetAttrValue2("",0,"FADebug") NE "??_ERROR_!!".
   /* ��� �鸞 ⨯�� ���᪠ �⫨砥��� ������ �� � ��᫥ �446 */
   IF CAN-DO("�焐��,�焂�",iCode) THEN
   DO:
      vDateP446 = DATE(fGetSetting("��446�","���446�","01/01/2016")).
      IF vDebug THEN
         RUN Fill-SysMes IN h_tmess("", "", "",
         SUBSTITUTE("�⫠��� ���᪠ ���: ~n��� �ࠢ�筨��: &1~n��� ���᪠: &2~n��� 446�: &3",
                             iCode,STRING(iDate),STRING(vDateP446))).
      
      IF iDate <= vDateP446 THEN
      DO:
         ASSIGN
            vOldPars   = GetAttrValue2("",0,iPref + "OldPars")
            vOldParams = GetAttrValue2("",0,iPref + "OldParams")
            .
         DO vCounter = 1 TO NUM-ENTRIES(vOldParams):
            vCurOldParam = ENTRY(vCounter,vOldParams).
            IF vCurOldParam BEGINS "#" THEN
            DO:
               vCurOldParam = ENTRY(2,vCurOldParam,"#").
               vParams[vCounter] = GetAttrValue2("",
                                                 0,
                                                 iPref + vCurOldParam).
               IF vParams[vCounter] = "??_ERROR_!!" THEN
               DO:
                  RUN Fill-SysMes IN h_tmess("", "", "",
                  SUBSTITUTE("�� ��������� ��६����� ~"&1&2~" ����室���� ��� ~
                             ���᪠ ��� ~"&3~".",
                             iPref,vCurOldParam,iCode)).
                  is-ok = -1.
               END.
            END.
            ELSE
            DO:
               vParams[vCounter] = vCurOldParam.
            END.
         END.
         CASE vOldPars:
            WHEN "�����_�����_��" THEN
            DO:
               RUN PARSFUNC-�����_�����_�� IN h_pvok (vParams[1], 
                                                      vParams[2], 
                                                      vParams[3], 
                                                      vParams[4],
                                                      vParams[5],
                                                      vParams[6],
                                                      OUTPUT oResult, 
                                                      OUTPUT is-ok).
               IF vDebug THEN
                  RUN Fill-SysMes IN h_tmess("", "", "",
                  SUBSTITUTE("�⫠��� ���᪠ ���: ~n���� �� ������� �� �446: ~
                             &1~n��ࠬ����: &2~n�������� ���: &3",
                             vOldPars,vOldParams,oResult)).
            END.
            WHEN "������������" THEN
            DO:
               iCode = vParams[1].
            END.
         END CASE.
      END.
   END.

   IF NOT {assigned oResult} THEN
      /*
   CASE iCode:
      WHEN "�焂�"    THEN
      OTHERWISE
      */
      DO:
         /* ��室�� ��᫥���� ���ᠭ�� 㭨���ᠫ쭮�� �ࠢ�筨�� */
         FIND LAST DataBlock WHERE 
                   DataBlock.DataClass-Id EQ iCode
               AND DataBlock.beg-date     <= iDate
            NO-LOCK NO-ERROR.
         IF AVAIL DataBlock THEN
         DO:
            vDataClass-Id = DataBlock.DataClass-Id + "|" + STRING(DataBlock.Data-Id).
            /* ��� ������� ����� ��室�� ���祭�� � ���� �࠭���樨 */
            FOR EACH DataAttr WHERE 
                     DataAttr.DataClass-Id EQ vDataClass-Id
             AND NOT DataAttr.IsView
             NO-LOCK BY DataAttr.order:
               ASSIGN
                  vCodeAttr = iPref + REPLACE(GetMangledName(DataAttr.DataAttr-Id)," ","_")
                  vValue    = GetAttrValue2("",0,vCodeAttr)
                  .
               IF vValue = "??_ERROR_!!" THEN
               DO:
                  RUN Fill-SysMes IN h_tmess("", "", "",
                  SUBSTITUTE("�� ��������� ��६����� ~"&1~" ����室���� ��� ���᪠ ��� ~"&2~".",
                             vCodeAttr,iCode)).
                  is-ok = -1.
               END.
               IF     DataAttr.DataAttr-Id EQ "currency" 
                  AND vValue               EQ ""         THEN
                  vValue = "810".

               IF    DataAttr.DataAttr-Id EQ "cust-cat" THEN
               DO:
                  vCustId = INT64(GetAttrValue2("",0,iPref + "cust-id")) NO-ERROR.
                  IF ClientXAttrVal(vValue,vCustId,"�।��") BEGINS "�।" THEN
                     vValue = "�".
                  ELSE
                  DO:
                     IF vValue EQ "�" AND
                        CAN-DO(FGetSetting("�⠭���", "����ᔋ��", ""),
                               GetValueAttr("cust-corp", STRING(vCustId), "cust-stat"))
                     THEN
                        vValue = "�".
                  END.
               END.
               {additem.i vValueLst vValue}
                  /*
               IF DataAttr.DataAttr-Id EQ "currency" THEN
               DO:
                  {additem.i vValueLst2 vValue}
               END.
               ELSE
               DO:
                  {additem.i vValueLst2 "*"}
               END. */
            END.
            IF is-ok >= 0 THEN
            DO:
               oResult = GetRefVal(iCode,iDate,vValueLst).
               IF NOT {assigned oResult} THEN
                  oResult = GetRefVal(iCode,iDate,vValueLst2).
               IF vDebug THEN
                  RUN Fill-SysMes IN h_tmess("", "", "",
                  SUBSTITUTE("&1&2~n���祭�� ���ਥ�: &3~n�������� ���: &4",
                             "�⫠��� ���᪠ ���: ~n��� �ࠢ�筨��: ",
                             iCode,
                             vValueLst,
                             oResult)).
            END.
         END.
      END.
  /* END CASE. */

END PROCEDURE.
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='16/12/2015 06:42:01.259+04:00' */
/* $LINTUSER='elus' */
/* $LINTMODE='1' */
/* $LINTFILE='pp-pacct.p' */
/*prosignjI6579U5xvUj93bkqplxUg*/
/* --- pp-pacct.p was humbly modified by (c)blodd converter v.1.09 on 9/19/2016 7:56am --- */
