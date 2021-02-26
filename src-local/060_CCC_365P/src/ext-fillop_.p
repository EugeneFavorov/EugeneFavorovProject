/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2015 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: ext-fillop.p
      Comment: TT:0259807 ������. 365-� �।��⠢����� ��饩 �믨᪨ �� ��⠬ �� � ��
   Parameters:
         Uses:
      Used by:
      Created: 22/03/2014 15:15 KMBIS ���室��� ��� ���㧪� ������ �� ���譥� ��⥬�.
                                      ���㬥��� �� ����.
     Modified: 
*/
ROUTINE-LEVEL ON ERROR UNDO, THROW.
{globals.i}
{extexch.def} /* ����ন� ���ᠭ�� �६����� ⠡���� ttExtAcct */

DEF INPUT  PARAM iHRepOp   AS  HANDLE NO-UNDO.   /* �����⥫� �� ⠡���� ��� ������ �� ������ */
DEF INPUT  PARAM iBegDate  AS  DATE   NO-UNDO.   /* ��� ��砫� ���㧪�                         */ 
DEF INPUT  PARAM iEndDate  AS  DATE   NO-UNDO.   /* ��� ����砭�� ���㧪�                      */
DEF INPUT  PARAM iUpID     AS  INT64  NO-UNDO.   /* ID ���㦠����� ���                        */
DEF INPUT  PARAM iAllFil   AS  LOG    NO-UNDO.   /* �� �ᥬ 䨫����� ��� ⮫쪮 ⥪�騩          */ 
DEF INPUT  PARAM TABLE     FOR ttExtAcct.        /* ������ � ����묨 �� ����                   */
DEF OUTPUT PARAM oNumDocs  AS  INT64  NO-UNDO.   /* ������⢮ ���㬥�⮢                        */

/* �� ᬮ��� �� � �� ���� ��।�� ⠡����, � ��� �ᥣ�� �㤥� 1 ��� */

{intrface.get strng}

DO ON ERROR UNDO, RETURN ERROR
   ON STOP UNDO, RETURN ERROR:

IF NOT CONNECTED("bank")
  THEN CONNECT -pf VALUE("/home2/bis/quit41d/conf/bismfr.pf").

FOR FIRST ttExtAcct NO-LOCK:
   /*
   /* ������� ������ � ⠡��� ����権 */
   iHRepOp:BUFFER-CREATE().
   ASSIGN
      /* oNumDocs 㢥��稢��� � ����� ᮧ����� ���㬥�⮬ �� 1 */
      oNumDocs = oNumDocs + 1
   
      /* ��易⥫�� ��⥬�� ���� */
      iHRepOp:BUFFER-FIELD(GetMangledName("ID")):BUFFER-VALUE       = STRING(oNumDocs)
      iHRepOp:BUFFER-FIELD(GetMangledName("UpId")):BUFFER-VALUE     = STRING(iUpID)
   
      /* ���� ��� ���㧪�, ����� ᮮ⢥����� ��� 䠩�� */
      iHRepOp:BUFFER-FIELD(GetMangledName("��⠎���")):BUFFER-VALUE = "01.01.1900"
      iHRepOp:BUFFER-FIELD(GetMangledName("������")):BUFFER-VALUE   = "01"
      iHRepOp:BUFFER-FIELD(GetMangledName("������")):BUFFER-VALUE   = "123"
      iHRepOp:BUFFER-FIELD(GetMangledName("��⠄��")):BUFFER-VALUE  = "01.01.1900"
      iHRepOp:BUFFER-FIELD(GetMangledName("��������")):BUFFER-VALUE = "99999999999999999999"
      iHRepOp:BUFFER-FIELD(GetMangledName("������")):BUFFER-VALUE   = "������ - ���㧪� �� ���譥� ��⥬�"
      iHRepOp:BUFFER-FIELD(GetMangledName("�����")):BUFFER-VALUE    = "9999999999"
      iHRepOp:BUFFER-FIELD(GetMangledName("�����")):BUFFER-VALUE    = "999999999"
      iHRepOp:BUFFER-FIELD(GetMangledName("�����")):BUFFER-VALUE    = "888888888"
      iHRepOp:BUFFER-FIELD(GetMangledName("������")):BUFFER-VALUE   = "������ - ���㧪� �� ���譥� ��⥬�"
      iHRepOp:BUFFER-FIELD(GetMangledName("����珏")):BUFFER-VALUE  = "88888888888888888888"
      iHRepOp:BUFFER-FIELD(GetMangledName("�����")):BUFFER-VALUE    = "1.11"
      iHRepOp:BUFFER-FIELD(GetMangledName("�।��")):BUFFER-VALUE   = "2.22"
      iHRepOp:BUFFER-FIELD(GetMangledName("������")):BUFFER-VALUE   = SUBST("���⮢�� ���㧪� �� ���譥� ��⥬�. ���: &1",
                                                                            ttExtAcct.number)
      iHRepOp:BUFFER-FIELD(GetMangledName("�����")):BUFFER-VALUE    = "����� - ���㧪� �� ���譥� ��⥬�"
  .
  /* �����뢠�� ������ � �᢮������� �� */
  iHRepOp:BUFFER-RELEASE().
  */
  FOR EACH tobis365p
   WHERE tobis365p.postime >= iBegDate
     AND tobis365p.postime <  (iEndDate + 1)
     AND tobis365p.account EQ DelFilFromAcct( ttExtAcct.acct)
   NO-LOCK:  
     /* ������� ������ � ⠡��� ����権 */
     iHRepOp:BUFFER-CREATE().
    ASSIGN
        /* oNumDocs 㢥��稢��� � ����� ᮧ����� ���㬥�⮬ �� 1 */
        oNumDocs = oNumDocs + 1
     
        /* ��易⥫�� ��⥬�� ���� */
        iHRepOp:BUFFER-FIELD(GetMangledName("ID")):BUFFER-VALUE       = STRING(oNumDocs)
        iHRepOp:BUFFER-FIELD(GetMangledName("UpId")):BUFFER-VALUE     = STRING(iUpID)
     
        /* ���� ��� ���㧪�, ����� ᮮ⢥����� ��� 䠩�� */
        iHRepOp:BUFFER-FIELD(GetMangledName("��⠎���")):BUFFER-VALUE = tobis365p.data_oper
        iHRepOp:BUFFER-FIELD(GetMangledName("������")):BUFFER-VALUE   = tobis365p.vid_doc
        iHRepOp:BUFFER-FIELD(GetMangledName("������")):BUFFER-VALUE   = tobis365p.nom_doc
        iHRepOp:BUFFER-FIELD(GetMangledName("��⠄��")):BUFFER-VALUE  = tobis365p.data_doc
        iHRepOp:BUFFER-FIELD(GetMangledName("��������")):BUFFER-VALUE = tobis365p.nom_kor_sch /* ����� ���.��� ����� ���⥫�騪� */
        iHRepOp:BUFFER-FIELD(GetMangledName("������")):BUFFER-VALUE   = tobis365p.naim_bp     /* ������������ ����� ���⥫�騪� */
        iHRepOp:BUFFER-FIELD(GetMangledName("�����")):BUFFER-VALUE    = tobis365p.bik_bp      /* ��� ����� ���⥫�騪� */
        iHRepOp:BUFFER-FIELD(GetMangledName("�����")):BUFFER-VALUE    = tobis365p.inn_pp      /* ���/��� ���⥫�騪�/�����⥫� */
        iHRepOp:BUFFER-FIELD(GetMangledName("�����")):BUFFER-VALUE    = tobis365p.kpp_pp      /* ��� ���⥫�騪�/�����⥫� */
        iHRepOp:BUFFER-FIELD(GetMangledName("������")):BUFFER-VALUE   = tobis365p.naim_pp     /* ������������ ���⥫�騪�/�����⥫� */
        iHRepOp:BUFFER-FIELD(GetMangledName("����珏")):BUFFER-VALUE  = tobis365p.nom_sch_pp  /* ��� ���⥫�騪�/�����⥫� */
        iHRepOp:BUFFER-FIELD(GetMangledName("�����")):BUFFER-VALUE    = tobis365p.debet
        iHRepOp:BUFFER-FIELD(GetMangledName("�।��")):BUFFER-VALUE   = tobis365p.kredit
        iHRepOp:BUFFER-FIELD(GetMangledName("������")):BUFFER-VALUE   = tobis365p.nazn_pl
        iHRepOp:BUFFER-FIELD(GetMangledName("�����")):BUFFER-VALUE    = tobis365p.fio_pp      /* ��� ���⥫�騪�/�����⥫� */
    .
    /* �����뢠�� ������ � �᢮������� �� */
    iHRepOp:BUFFER-RELEASE().
  END.
END. /* FOR FIRST ttExtAcct NO-LOCK */
CATCH eAnyError AS Progress.Lang.Error:
  RETURN ERROR RETURN-VALUE + " " + eAnyError:GetMessage(1).
END CATCH.
END.
