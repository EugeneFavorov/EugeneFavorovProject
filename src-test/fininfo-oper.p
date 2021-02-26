/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2015 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: FININFO-OPER.P
      Comment: �����ᮢ�� ���ଠ�� �� ��. ��栬. ������㥬� ����樨 �� ���� (224331)
   Parameters: iCustCat - ⨯ ������
               iCustId  - ��� ������
               ifrmName - ��������� ���
               iClassCode - ��� �����
         Uses:
      Used by:
      Created: 25.04.2014 14:16
*/

{globals.i}
{intrface.get xclass}   /* ������⥪� �����㬥�⮢ ����奬�. */
{intrface.get dynqr}    /* ������⥪� ��� ࠡ��� � �������᪨�� ����ᠬ�. */
{navigate.def}
{flt-file.i}

DEFINE INPUT  PARAMETER iCustCat    AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER iCustId     AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER ifrmname    AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER iClassCode  AS CHARACTER  NO-UNDO.

DEFINE VARIABLE num-line    AS INT64     NO-UNDO. /* ����室��� ��� navigate.cqr */
DEFINE VARIABLE mVidOper    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mPeriodOper AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAmountOper AS INT64     NO-UNDO.
DEFINE VARIABLE mSummaOper  AS DECIMAL   NO-UNDO.
DEFINE VARIABLE mCurrency   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mSymbol     AS CHARACTER  NO-UNDO.

/* ��� �६����� ⠡���� TmpObj �� 䨫���樨 �� ��� */
DEFINE VARIABLE mHTmpObj    AS HANDLE    NO-UNDO.

ASSIGN
   mSymbol = CHR(9).

FORM
   mVidOper
      VIEW-AS FILL-IN SIZE 17 BY 1
      FORMAT       "x(20)"
      COLUMN-LABEL "��� ����!����樨"
      HELP         "��� ���� ����樨"
   mPeriodOper
      VIEW-AS FILL-IN SIZE 16 BY 1
      FORMAT       "x(20)"
      COLUMN-LABEL "�����!����権"
      HELP         "����� ����権"
   mAmountOper
      VIEW-AS FILL-IN SIZE 7 BY 1
      FORMAT       ">>>>>>9"
      COLUMN-LABEL "���-��!����権"
      HELP         "������⢮ ����権"
   mSummaOper
      VIEW-AS FILL-IN SIZE 11 BY 1
      FORMAT       ">>>>>>>9.99"
      COLUMN-LABEL "�㬬�!����権,!� ���������"
      HELP         "�㬬� ����権"
   mCurrency
      VIEW-AS FILL-IN SIZE 3 BY 1
      FORMAT       "x(3)"
      COLUMN-LABEL "���!���"
      HELP         "��� ������"
   cust-ident.open-date
      COLUMN-LABEL "���!��砫�!����⢨�"
      HELP         "��� ��砫� ����⢨�"
      FORMAT "99/99/99" 
   cust-ident.close-date
      COLUMN-LABEL "���!����砭��!����⢨�"
      HELP         "��� ����砭�� ����⢨�"
      FORMAT "99/99/99" 
WITH FRAME browse1  TITLE  COLOR BRIGHT-WHITE ifrmname .

mHTmpObj   = WIDGET-HANDLE (GetFltValEx("UseTmpObjInQuery","*")) NO-ERROR.
RUN SetFltField IN THIS-PROCEDURE ("UseTmpObjInQuery","no").

MAIN-BLOCK:
DO 
   ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

{qrdef.i
   &buff-list        = "cust-ident"
   &Join-list        = "EACH"
   &fixed-where      = "'WHERE cust-ident.cust-id EQ ' + STRING(iCustId) + 
                        ' AND cust-ident.cust-cat EQ ""' + iCustCat + 
                      '"" AND cust-ident.class-code EQ ""' + iClassCode + '""' " 
}

{navigate.cqr
   &file       = "cust-ident"
   &files      = "cust-ident"
   &avfile     = "cust-ident"
   &maxfrm     = 1
   &bf1        = "mVidOper mPeriodOper mAmountOper mSummaOper mCurrency cust-ident.open-date cust-ident.close-date"
   &cf1        = "mVidOper mPeriodOper mAmountOper mSummaOper mCurrency cust-ident.open-date cust-ident.close-date"
   
   &edit       = "bis-tty.ef "
                     &class             = iClassCode
                     &before-run-method = "fininfo-cbc.bfe "
   &postfind   = "fininfo-oper.fnd "
                    
   &look      = "bis-tty.nav "
   
   &rat_upclass = iClassCode
}

END. /* MAIN-BLOCK: */

RUN SetFltField IN THIS-PROCEDURE ("UseTmpObjInQuery",mHTmpObj).

{intrface.del}          /* ���㧪� �����㬥����. */ 
/* $LINTUSER='BIS' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='15/01/2015 07:49:28.355+04:00' */
/* $LINTFILE='fininfo-oper.p' */
/*prosignfAHwsmzeEP5dgr8IGF/UMQ*/