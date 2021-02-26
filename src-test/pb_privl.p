/**
����᪨� �ࠢ� �ਭ�������: ��� ���� ����
�᭮�����:
�� ������:     ��楤�� ��ᬮ�� �����䨪��� �ਢ��祭��
������:         18.07.2017 ���ᮢ �.�.
*/

DEF INPUT PARAM iClass  AS CHAR NO-UNDO. /* ��� �����䨪���. */
DEF INPUT PARAM iParent AS CHAR NO-UNDO. /* ����⥫� �����䨪���. */
DEF INPUT PARAM iTitle  AS CHAR NO-UNDO. /* ��������� �ࠢ�筨��. */

{globals.i}             /* �������� ��६���� ��ᨨ. */
{navigate.def}          /* ��६���� ��� navigate.cqr. */
/*
{flt-file.i NEW}
{brow-cod.i}            /* ��ࠬ���� ��楤��� ��ᬮ�� + pp-rights */

{intrface.get xclass}   /* ������⥪� �����㬥�⮢ ����奬�. */
{intrface.get cust}
{intrface.get count}
{intrface.get tmess}
*/
FORM
    code.name
        COLUMN-LABEL  "��� ��-���������"
        FORMAT "x(39)"
        VIEW-AS FILL-IN SIZE 39 BY 1
    code.code
        COLUMN-LABEL  "��� ��"
        FORMAT "x(12)"
    code.val
        COLUMN-LABEL  "�����"
    SPACE(0)
WITH FRAME browse1 TITLE COLOR bright-white "".

&GLOB oqry0 OPEN QUERY qry0             ~
    FOR EACH code                       ~
        WHERE (code.class   = iClass)   ~
          AND (code.parent  = iParent)  ~
        NO-LOCK BY code.name.

{navigate.cqr
   &qry           = "qry0 "
   &defquery      = "def query qry0 for code scrolling. "
   &maxoq         = 1
   &file          = code
   &files         = code
   &avfile        = code

   &access-class  = "'code'"
   &access-surr   = "code.class + ',' + code.code"

   &maxfrm        = "1"
   &first-frm     = "1"
   &bf1           = "code.name code.code code.val "

   &look          = "pb_privl.nav "
   &edit          = "pb_privl.cqr "
   &return        = "return.cqr
                       &rfld = code "
}
/*
FORM
    code.name
        FORMAT "x(100)"
        VIEW-AS FILL-IN SIZE 50 BY 1
        LABEL "���"
    code.code
        FORMAT "x(12)"
        LABEL "����� ��"
    code.val
        FORMAT "x(30)"
        LABEL "��த"
WITH FRAME edit.

   &ef            = "pb_privl.uf "

   &create        = "pb_privl.cr "
   &delete        = "pclass.del "
   &lookup        = "pb_privl.nau "

   &local-recid   = YES
   &local-rest    = YES
   &local-keep    = YES

*/
{intrface.del}          /* ���㧪� �����㬥����. */
