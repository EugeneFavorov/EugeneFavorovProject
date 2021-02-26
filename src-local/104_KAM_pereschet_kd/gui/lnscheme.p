/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2015 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: lnscheme.p
      Comment: �맮� �奬 ���᫥��� ��業⮢ � ���㫥 �&�.
   Parameters: ���
         Uses:
      Used by:
      Created: 18/03/2002
     Modified: ���� 08.10.2002
     Modified: ���� 11.11.2002 ��楤�� GetXattr �뭥ᥭ� � �⤥��� ������
                                (����� ���ॡ������� � ��㣨� �����)
     Modified:
*/
form "~n@(#) lnscheme.p 1.0 19/03/2002"
with frame sccs-id stream-io width 250.

{globals.i}    /* �������� ���७�� ��६���� ��ᨨ */
{svarloan.def} /* �������� ���७�� ��६���� ����� �ॡ�� ln-nach.i */
{t-otch.i }    /* �������: �஬������ � ���⮢ */

{intrface.get xclass} /* �����㬥��� ��� ࠡ��� � ����奬��. */
{intrface.get comm}   /* �����㬥��� ��� ࠡ��� � ������ﬨ. */
{intrface.get schem}  /* �����㬥��� ��� ࠡ��� � �奬��� ���᫥���. */
{intrface.get date}   /* �����㬥��� ��� ࠡ��� � ��⠬� */
{intrface.get loan}   /* �����㬥��� ��� ࠡ��� � ������ࠬ� */

{ln-nach.i}    /* �����㬥��� ��� �맮�� �奬 ���᫥��� ��業⮢ �� �।�⠬
                  �ॡ�� svarloan.def */
/*{empty LnShPrm}*/

/* �����᪨ ᫮���訥�� ��ࠬ���� (���� �������� �����) */
define input param ipContractChar as char no-undo. /* �����祭�� ������� */
define input param ipContCodeChar as char no-undo. /* ����� ������� */
define input param ipBegDate      as date no-undo. /* ��� ��砫� ���� */
define input param ipEndDate      as date no-undo. /* ��� ����砭�� ���� */
define input param dat-per        as date no-undo. /* ��� ���室� �� 39� */
define input param cod-par        as INT64  no-undo. /* ��� ��ࠬ��� */
define input param fl-type-ost    as INT64  no-undo. /* �ᥣ�� ��।����� 1 �� ��. */

define var vAcctTypeChar as char no-undo. /* ���� �᭮����� ��� */
define var vCountInt     as INT64 init 1 no-undo. /* ���稪 */
define var vPrmInt       as INT64  no-undo. /* ���浪��� ����� ��業� ��� ���� */
def var    vCalcString   as CHAR   no-undo.
def var    vInt          as INT64  no-undo . 

{&UTIME_BEG}
/* �������� ��������� �᫮��� � ���ࢠ�� ��� */
RUN GetCondDate (ipContractChar, /* �����祭�� ������� */
                 ipContCodeChar, /* ����� ������� */
                 ipBegDate,      /* ��砫� ���ࢠ�� */
                 ipEndDate).     /* ����砭�� ���ࢠ�� */

/* �������� �奬� ���᫥��� � ���ࢠ�� ��� */
RUN GetLnScheam (ipContractChar, /* �����祭�� ������� */
                 ipContCodeChar, /* ����� ������� */
                 ipBegDate,      /* ��砫� ���ࢠ�� */
                 ipEndDate).     /* ����砭�� ���ࢠ�� */


/* ����室��� ���� ��ॡ�� �� �ᥬ ��ࠬ��ࠬ - �����筮 ��࠭������
������⢮� ����⮢ � ᯨ᪥ ������� */

 IF cod-par = ?
 THEN DO: 

   vCalcString = GetXattrValueEx("loan",
                                 ipContractChar + "," + ipContCodeChar,
                                 "�������","").
   IF vCalcString = "" THEN
   DO:
      FIND FIRST loan WHERE loan.contract  EQ ipContractChar
                        AND loan.cont-code EQ ipContCodeChar
           NO-LOCK NO-ERROR.
      vCalcString = GetXattrInit(loan.class-code,"�������").
   END.

   IF vCalcString <> '' AND vCalcString <> ?
   THEN vInt = NUM-ENTRIES(vCalcString).
   ELSE vInt = {&par-dim}.
 END.  
 ELSE 
    ASSIGN
      vCountint = cod-par
      vInt = cod-par .  
    
/* ��� (s1.p) � ��ॡ�� �� ��ࠬ��ࠬ */
DO WHILE vCountInt le vInt:

/* ��� ᫮���� ��砥� �������ਧ�樨 - ��.-���. � ��.-���. 
   �㦭� �࣠�������� ࠧ����� �� ��⠬ ��������� ⨯� ����襭��.
   �।�������� ⠪�� �������� "���訢���" ��:������ = �� */


    /* �맮� �奬 ���᫥��� ��業⮢ �� ������ࠬ
    ** �� ��ନ஢����� �६����� ⠡��� */
    IF    dat-per LT ipBegDate 
       OR dat-per GE ipEndDate THEN
       RUN RunLnScheam (ipContractChar,  /* �����祭�� ������� */
                        ipContCodeChar,  /* ����� ������� */
                        ipBegDate,       /* ��砫� ���ࢠ�� ���᫥��� */
                        ipEndDate,       /* ����砭�� ���ࢠ�� ���᫥��� */
                        dat-per,         /* ��� ���室� �� 39� */
                        vCountInt,       /* ��� ��ࠬ��� */
                        fl-type-ost).    /* �ᥣ�� ��।����� 1 �� ��. */
    ELSE DO:
       RUN RunLnScheam (ipContractChar,  /* �����祭�� ������� */
                        ipContCodeChar,  /* ����� ������� */
                        ipBegDate,       /* ��砫� ���ࢠ�� ���᫥��� */
                        dat-per,     /* ����砭�� ���ࢠ�� ���᫥��� */
                        dat-per,         /* ��� ���室� �� 39� */
                        vCountInt,       /* ��� ��ࠬ��� */
                        fl-type-ost).    /* �ᥣ�� ��।����� 1 �� ��. */
       RUN RunLnScheam (ipContractChar,  /* �����祭�� ������� */
                        ipContCodeChar,  /* ����� ������� */
                        dat-per + 1,         /* ��砫� ���ࢠ�� ���᫥��� */
                        ipEndDate,       /* ����砭�� ���ࢠ�� ���᫥��� */
                        dat-per,         /* ��� ���室� �� 39� */
                        vCountInt,       /* ��� ��ࠬ��� */
                        fl-type-ost).    /* �ᥣ�� ��।����� 1 �� ��. */
    END.
    vCountInt = vCountInt + 1.
END.

{&UTIME_END}

/* ���⪠ �����. */
{intrface.del}
/*{empty LnShPrm}*/
return.
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='23/12/2015 13:48:23.934+04:00' */
/* $LINTUSER='guiv' */
/* $LINTMODE='1' */
/* $LINTFILE='lnscheme.p' */
/*prosignCtiWJo3XHn+MNQ3cB2ELDA*/