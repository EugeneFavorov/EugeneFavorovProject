{globals.i}
{intrface.get tmess}

/* +++ ext-fillop.p was humbly modified by (c)blodd converter v.1.09 on 10/16/2015 7:11am +++ */

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

FOR FIRST ttExtAcct NO-LOCK:

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

END. /* FOR FIRST ttExtAcct NO-LOCK */

/* --- ext-fillop.p was humbly modified by (c)blodd converter v.1.09 on 10/16/2015 7:11am --- */
