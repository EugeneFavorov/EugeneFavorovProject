/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2010 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: strahbrw0.P
      Comment: �����⥫� ���客�� �६�� - ����� ��㧥�
   Parameters:
         Uses:
      Used by:
      Created: kam 
     Modified:    
*/
{globals.i}             /* �������� ��६���� ��ᨨ. */
{intrface.get tmess}    /* �����㬥��� ��ࠡ�⪨ ᮮ�饭��. */

DEFINE INPUT PARAMETER iLevel AS INT64 NO-UNDO.

RUN strahbrw.p("strahpol","strahpol","�����⥫� ���客�� �६��",iLevel).
