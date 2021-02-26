/*
     Filename: OP-KAU.P
      Comment: ��㧥� �����ᮢ�� ���㬥�⮢ �� ����⥪�. (��뢠���� �� ��㧥� ���㬥�⮢ ����⥪�)
   Parameters:
         Uses:
      Used by:
      Created: 31.01.2009 Sami 
*/

{globals.i}
{flt-file.i}
{form.def}
{sh-defs.i}

DEFINE BUFFER lop FOR op.
DEFINE BUFFER lop-entry FOR op-entry.
DEFINE BUFFER nkau FOR kau.

DEFINE VARIABLE mBic      AS CHARACTER NO-UNDO.    /*��� �����*/
DEFINE VARIABLE mAcct     AS CHARACTER NO-UNDO.    /*��� �����⥫�*/
DEFINE VARIABLE mAcctStat AS CHARACTER NO-UNDO.    /*��� ���*/
DEFINE VARIABLE mDocDate  AS DATE      NO-UNDO.    /*��� �����஢����*/
DEFINE VARIABLE mSumma    AS DECIMAL   NO-UNDO.    /*�㬬�*/
DEFINE VARIABLE mSummaOst AS DECIMAL   NO-UNDO.    /*�㬬� ���⮪*/
DEFINE VARIABLE mSummaRub AS DECIMAL   NO-UNDO.    /*�㬬� ���. ���.*/
DEFINE VARIABLE mPerenos  AS CHARACTER NO-UNDO.    /*����⥪� ��७��*/
DEFINE VARIABLE mPos      AS DECIMAL   NO-UNDO.    /*���⮪ �� �����ᮢ�� ���*/
DEFINE VARIABLE mNumKau   AS CHARACTER NO-UNDO.    /*��� �࠭���� ��� �� ������ �஢����*/
DEFINE VARIABLE mTypeKart AS CHARACTER NO-UNDO.    /*��� ����⥪�*/ 

DEFINE SHARED VARIABLE vKart AS CHARACTER NO-UNDO.    /*���祭� ����⥪, ����砥��� �� ����᪥ ��㧥�*/

mTypeKart = vKart.

FORM
   op.order-pay          FORMAT "x(2)"               COLUMN-LABEL "��㯯�!��।����"             HELP "��㯯� ��।���� �ᯮ������ ���㬥��"
   op.ins-date           FORMAT "99/99/9999"         COLUMN-LABEL "���!����㯫����"               HELP "��� ����㯫���� � ����"
   mDocDate              FORMAT "99/99/9999"         COLUMN-LABEL "��������!���"                  HELP "�������� ��� ��楯�"
   mSumma                FORMAT "->>,>>>,>>>,>>9.99" COLUMN-LABEL "�㬬� ���㬥��"                HELP "�㬬� ���㬥�"
   op.doc-date           FORMAT "99/99/9999"         COLUMN-LABEL "���㬥�� ��"                    HELP "���㬥�� ��"
   op.doc-num            FORMAT "x(12)"              COLUMN-LABEL "N �����ᮢ���!���㬥��"        HELP "����� �����ᮢ��� ���㬥��"
   mSummaRub             FORMAT "->>,>>>,>>>,>>9.99" COLUMN-LABEL "� ��樮���쭮�!�����"          HELP "�㬬� � ���. ����� �� ��⭮�� ����� ��"
   mSummaOst             FORMAT "->>,>>>,>>>,>>9.99" COLUMN-LABEL "���⮪ ᯨᠭ��!�� ����⥪�"  HELP "���⮪ �� ����⥪�"
   op.doc-type           FORMAT "x(2)"               COLUMN-LABEL "���!����樨"                   HELP "��� ����樨"
   mBic                  FORMAT "x(9)"               COLUMN-LABEL "��� �����"                      HELP "��� �����"
   mAcct                 FORMAT "x(20)"              COLUMN-LABEL "��� ���⥫�騪�"               HELP "��� ���⥫�騪�"
   mPos                  FORMAT "->>,>>>,>>>,>>9.99" COLUMN-LABEL "���⮪ ��!�����ᮢ�� ���"    HELP "���⮪ �� ���, ����㯭� ��� ᯨᠭ��"
   mAcctStat             FORMAT "x(12)"              COLUMN-LABEL "����� ���"                   HELP "����� ���"
   mPerenos              FORMAT "x(13)"              COLUMN-LABEL "��७�� �����!����⥪���"      HELP "��㤠/�㤠 �� ��७�ᥭ ���㬥��"
   WITH FRAME browse1
   TITLE COLOR bright-white "[ " + "���������� ��������� �� ���������" + " ]" WIDTH 220.

{qrdef.i
   &buff-list        = "op"
   &need-buff-list   = "op"
   &Join-list        = "EACH"
   &SortBy           = "'BY op.order-pay BY op.doc-date BY op.doc-num'"
   &condition        = "YES"
   }

{navigate.cqr
   &autosigns        = "YES"
   &filt             = "YES"
   &file             = "op"
   &avfile           = "op"
   &files            = "op"
   &bf1              = "op.order-pay op.ins-date mDocDate mSumma op.doc-date op.doc-num mSummaRub mSummaOst op.doc-type mBic mAcct mPos mAcctStat mPerenos "
   &tmprecid         = "YES"
   &first-frm        = 1
   &maxfrm           = 1
   &CalcFld          = "mDocDate mSumma mSummaRub mSummaOst mBic mAcct mPos mAcctStat mPerenos "
   &CalcVar          = "opkau.clc "
   &postfind         = "opkau.clc "
   &oth1             = "opkau.f1 "
   &oth2             = "opkau.f2 "
   &oth6             = "flt-file.f6 "
   &help-label       = "'F1�F2 ᯨᠭ���F6 䨫��� '"
   }

{intrface.del}
