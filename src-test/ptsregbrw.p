/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2010 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: ptsregbrw.P
      Comment: ��������(ॣ������) ���㬥�⮢ - ��㧥�
   Parameters:
         Uses:
      Used by:
      Created: kam 
     Modified:    
*/

{globals.i}             /* �������� ��६���� ��ᨨ. */

{flt-file.i NEW}  

{intrface.get xclass}   /* ������⥪� �����㬥�⮢ ����奬�. */
{navigate.def}          /* ��६���� ��� navigate.cqr. */
{intrface.get count}
{intrface.get tmess}

DEF INPUT PARAM   iRec        AS    RECID    NO-UNDO.

DEFINE VARIABLE DateChange AS CHAR NO-UNDO.
DEFINE VARIABLE DateValue AS DATE NO-UNDO.
DEFINE VARIABLE NameEvent AS CHAR NO-UNDO.
DEFINE VARIABLE Branch_ AS CHAR NO-UNDO.
DEFINE VARIABLE User_Id AS CHAR NO-UNDO.
DEFINE VARIABLE User_FIO AS CHAR NO-UNDO.
DEFINE VARIABLE DocType AS CHAR NO-UNDO.
DEFINE VARIABLE Details AS CHAR NO-UNDO.
DEFINE VARIABLE Descriptions AS CHAR NO-UNDO.
DEFINE VARIABLE ContCode AS CHAR NO-UNDO.
DEFINE VARIABLE Branch_Cur_User AS CHAR NO-UNDO.
DEFINE VARIABLE Cur_User AS CHAR NO-UNDO.
DEFINE VARIABLE tmpChar AS CHAR NO-UNDO. 
DEFINE VARIABLE tmpInt AS INT NO-UNDO.
DEFINE VARIABLE tmpBool AS LOGICAL NO-UNDO.
DEFINE VARIABLE tmpEvent AS CHAR NO-UNDO.
DEFINE VARIABLE nullStr AS CHAR NO-UNDO INIT "".
DEFINE VARIABLE PrinalFIO AS CHAR NO-UNDO.

&GLOBAL-DEFINE eh        ptsregbrw.eh~032
&GLOBAL-DEFINE tmprecid  YES

FIND FIRST term-obl WHERE RECID(term-obl) = iRec NO-LOCK NO-ERROR.
IF NOT AVAIL term-obl THEN RETURN.

ContCode = term-obl.cont-code + ' ' + string(term-obl.nn).
Cur_User = USERID("bisquit").
FIND FIRST _user WHERE _user._userid = USERID("bisquit") NO-LOCK NO-ERROR.
IF AVAIL _user THEN 
    Branch_Cur_User = GetXattrValueEx("_user",STRING(_user._userid),"�⤥�����",""). 
    ELSE Branch_Cur_User = "".

{ptsregbrw.frm}
{ptsregbrw.qry}
{ptsregbrw.fun}

{navigate.cqr

   &file          = pl_indocsreg
   &files         = pl_indocsreg
   &avfile        = pl_indocsreg

   &edit          = "ptsregbrw.cqr "
   &create        = "ptsregbrw.cr "
   &update        = "ptsregbrw.upd "
   
   &befdel        = "ptsregbrw.del "
    
   &bf1           = "DateValue NameEvent Branch_ Details Descriptions DateChange User_Id"
   &ef            = "ptsregbrw.uf "
   
   &look          = "ptsregbrw.nav "
   &lookup        = "ptsregbrw.nau " 
   
   &postfind      = "ptsregbrw.fnd "

   &need-tmprecid = YES
   &local-recid   = YES
   &local-rest    = YES
   &local-keep    = YES
}



{intrface.del}          /* ���㧪� �����㬥����.  */ 





