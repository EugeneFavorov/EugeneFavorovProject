{globals.i}
{prn-doc.def &with_proc=YES}
{tmprecid.def}
{intrface.get cust}
{intrface.get tmess}
{sh-defs.i new}
{dpsproc.def}
{ksh-defs.i NEW}
{justasec}
{intrface.get refer}    /* ������⥪� �㦡� �ࠢ�筨���. */
{intrface.get date}

{pb_exf_exl.i}          /** ������⥪� ��� ���㧪� � XL */


/*
DEFINE INPUT PARAMETER iStr AS CHARACTER NO-UNDO.
*/

DEFINE VARIABLE iStr	     AS CHARACTER 			NO-UNDO. /* � �*/


DEFINE VARIABLE mName	     AS CHARACTER EXTENT 2 NO-UNDO.
DEFINE VARIABLE SummaStr     AS CHARACTER EXTENT 2 NO-UNDO.
DEFINE VARIABLE mInn	     AS CHARACTER 			NO-UNDO. /* � �*/
DEFINE VARIABLE mAdrReg      AS CHARACTER 			NO-UNDO. /*�����*/
DEFINE VARIABLE mAdrFact     AS CHARACTER 			NO-UNDO. /*�������*/
DEFINE VARIABLE mCustName    AS CHARACTER EXTENT 3 NO-UNDO.
DEFINE VARIABLE mValName     AS CHARACTER          NO-UNDO.
DEFINE VARIABLE mAddress1    AS CHARACTER          NO-UNDO.
DEFINE VARIABLE mAddress2    AS CHARACTER          NO-UNDO.
DEFINE VARIABLE vAmtStr      AS CHARACTER          NO-UNDO.
DEFINE VARIABLE vDecStr      AS CHARACTER          NO-UNDO.
DEFINE VARIABLE mError       AS CHARACTER          NO-UNDO.
DEFINE VARIABLE mSex         AS CHARACTER          NO-UNDO.
DEFINE VARIABLE mWord        AS LOGICAL            NO-UNDO.
DEFINE VARIABLE mText        AS CHARACTER          NO-UNDO.
DEFINE VARIABLE mDocV        AS CHARACTER          NO-UNDO.
DEFINE VARIABLE phone        AS CHARACTER          NO-UNDO INIT ''.
DEFINE VARIABLE mSignsVal    AS CHARACTER          NO-UNDO.
DEFINE VARIABLE mDatTMP      AS DATE               NO-UNDO.
DEFINE VARIABLE mList_Acct   AS CHARACTER          NO-UNDO.
DEFINE VARIABLE mCurrency    AS CHARACTER          NO-UNDO.
DEFINE VARIABLE shfilial_    AS CHARACTER          NO-UNDO.
DEFINE VARIABLE ost          AS decimal            NO-UNDO.
DEFINE VARIABLE mBegDate     AS date               NO-UNDO.   
DEFINE VARIABLE mEndDate     AS date               NO-UNDO.   
DEFINE VARIABLE n            AS int                NO-UNDO.   
DEFINE VARIABLE i            AS int                NO-UNDO.   
DEFINE VARIABLE j            AS int                NO-UNDO.   
DEFINE VARIABLE i1           AS int                NO-UNDO.   
DEFINE VARIABLE j2           AS int                NO-UNDO.   
DEFINE VARIABLE mSh-Db       AS dec                NO-UNDO.   
DEFINE VARIABLE mSh-Cr       AS dec                NO-UNDO.   
DEFINE VARIABLE mSh-vDb      AS dec                NO-UNDO.   
DEFINE VARIABLE mSh-vCr      AS dec                NO-UNDO.   
DEFINE VARIABLE mList_cont   AS CHARACTER          NO-UNDO.
DEFINE VARIABLE mBalance     AS dec                NO-UNDO.   
DEFINE VARIABLE mBalanceK1   AS dec                NO-UNDO.   
DEFINE VARIABLE mBalanceK2   AS dec                NO-UNDO.   
DEFINE VARIABLE StrBalanceK1 AS char               NO-UNDO.   
DEFINE VARIABLE StrBalanceK2 AS char               NO-UNDO.   
DEFINE VARIABLE namestat     AS char               NO-UNDO.   
DEFINE VARIABLE mtoday       AS date               NO-UNDO.   
DEFINE VARIABLE mSumma       AS dec                NO-UNDO.   
DEFINE VARIABLE iDate        AS date               NO-UNDO.   
DEFINE VARIABLE datebeg_     AS date               NO-UNDO.   
DEFINE VARIABLE vWork        AS LOGICAL            NO-UNDO.
DEFINE VARIABLE List-bal     AS CHARACTER          NO-UNDO.    /* ���᮪ �����ᮢ�� ��⮢ */
DEFINE VARIABLE List-bal-2   AS CHARACTER          NO-UNDO.    /* ���᮪ �����ᮢ�� ��⮢ */
DEFINE VARIABLE List-bal-3   AS CHARACTER          NO-UNDO.    /* ���᮪ �����ᮢ�� ��⮢ */
DEFINE VARIABLE List-bal-4   AS CHARACTER          NO-UNDO.    /* ���᮪ �����ᮢ�� ��⮢ */
DEFINE VARIABLE bal_         AS CHARACTER          NO-UNDO.
DEFINE VARIABLE file-name    AS CHARACTER          NO-UNDO.
DEFINE VARIABLE kold         AS int                NO-UNDO.    /* ᪮�쪮 ���� ��ॡ���� ��� ���᪠ ࠡ��� ���� */
DEFINE VARIABLE koldo        AS int                NO-UNDO.    /* ������⢮ ࠡ��� ���� */

DEFINE VARIABLE var-NameOrg   AS char               NO-UNDO.   
DEFINE VARIABLE var-NameShort AS char               NO-UNDO.   

DEFINE VARIABLE beg-dt1      AS date               NO-UNDO.   
DEFINE VARIABLE cFl   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cXL   AS CHARACTER NO-UNDO.


{getdates.i}


DEF BUFFER bacct for acct.

DEFINE TEMP-TABLE ttDoc
   FIELD branch       AS CHARACTER
   FIELD op-date      AS DATE
   FIELD rec-name  AS CHARACTER
   FIELD account      AS CHARACTER
   FIELD num-doc      AS CHARACTER
   FIELD currency     AS CHARACTER
   FIELD amt          AS decimal
   FIELD date-mess    AS DATE
   FIELD date-ans     AS CHARACTER
   FIELD op           AS CHARACTER
   FIELD type         AS CHARACTER
   INDEX ind1 branch op-date .


PAUSE 0.

kold = 45.
koldo = 15.



/*
���᫥��� �।�� �� ��� �� � �� (407*,406*, 40802*) � �㡫�� � ��⮢ ��१����⮢:
�� ��ᨩ᪮�� �����  � �.�. ����७��� ����樨 � ��⮢ �����⮢ ��१����⮢ ��� ���� ����)  � ��⮢ 40807*, 40820*, 426*.
*/

list-bal   = "406*,407*,40802*".
list-bal-2 = "40807*,40820*,426*".
/*
list-bal-3 = "30111*".
*/
list-bal-3 = "30111*,30302*".
list-bal-4 = "40702,40802".

for each op-entry where  op-entry.op-date  >= beg-date
                    and  op-entry.op-date  <= end-date
                    and  op-entry.op-status >= chr(251)
                    and  CAN-DO(list-bal, op-entry.acct-cr)
                    no-lock.
   find first op of op-entry.
   if  CAN-DO(list-bal-2, op.ben-acct) and  op-entry.currency = "" and CAN-DO(list-bal, op-entry.acct-cr) then do:
       RUN create_ttDoc(op.op,"����饭��").
   end.
   if  CAN-DO(list-bal-3, op.ben-acct) and  op-entry.currency = "" and CAN-DO(list-bal, op-entry.acct-cr) and substr(trim(op.details),2,2) EQ "VO"  then do:
       RUN create_ttDoc(op.op,"����饭��").
   end.
   if  CAN-DO(list-bal-2, op-entry.acct-db)  and  op-entry.currency = "" and  CAN-DO(list-bal, op-entry.acct-cr) then do:
         RUN create_ttDoc(op.op,"����饭��").
   end.
   if  CAN-DO(list-bal, op-entry.acct-cr) and op-entry.amt-cur <> 0
   then do:
       find first acct where acct.acct = op-entry.acct-cr no-lock no-error.
       if avail acct and acct.contract begins "�࠭" then do:
          RUN create_ttDoc(op.op,"�����������").
      end.
   end.
end.
/*
output to "ttDoc.txt".
for each ttDoc.
   export ttDoc.
end.
output close.
*/

FOR each branch WHERE                                                
   lookup("����",branch.parent-id,";") GT 0                          
   AND (branch.close-date EQ ? OR branch.close-date >= today)     
   AND branch.isbank EQ yes                                          
   NO-LOCK                                                           
   BY branch.order                                                   
   BY branch.branch-id 
   .

   cFl = "./qm.xml".
   
   OUTPUT TO VALUE(cFl).
   
   PUT UNFORMATTED XLHead("tmp", "CCCCCCNCCCC", "70,90,300,215,100,100,90,125,110,100,120").
   
   cXL = XLCellHead("������",0,0,0)
       + XLCellHead("��� ���᫥���",0,0,0)
       + XLCellHead("������������ ������",0,0,0)
       + XLCellHead("������� ����",0,0,0)
       + XLCellHead("����� 㢥�������� / ����饭��",0,0,0)
       + XLCellHead("����� ����㯫����",0,0,0)
       + XLCellHead("�㬬� ����㯫����",0,0,0)
       + XLCellHead("�ப �।��⠢����� ���᭮�뢠��� ���㬥�⮢",0,0,0)
       + XLCellHead("������ ���⠫�� (���)",0,0,0)
       + XLCellHead("�����������/����饭��",0,0,0)
       + XLCellHead("�����䨪��� ���㬥��",0,0,0)
       .
   
   PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
   
   FOR EACH ttDoc where ttDoc.branch = branch.branch-id NO-LOCK.
      cXL = XLCell(string(ttDoc.branch))                 
          + XLCell("`" + STRING(ttDoc.op-date     ))          
          + XLCell(STRING(ttDoc.rec-name ))          
          + XLCell(STRING(entry(1,ttDoc.account,"@") ))          
          + XLCell(STRING(ttDoc.num-doc ))            
          + XLCell(STRING(ttDoc.currency))                                   
          + XLNumCell(ttDoc.amt)                                      
          + XLCell("`" + STRING(ttDoc.date-mess ))                                   
          + XLCell("`" + STRING(ttDoc.date-ans  ))                                        
          + XLCell(STRING(ttDoc.type))                         
          + XLCell(STRING(ttDoc.op))                         
          .
      PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
   END.
   
   
   PUT UNFORMATTED XLEnd().
   OUTPUT CLOSE.
   
      RUN pb_mail.p ("v.ignatchenko,e.polovinkina,o.shakin,A.Korobova,M.Vasileva", "QM Report " + branch.branch-id , "QM Report " + branch.branch-id, cFl).

end.
return.
   
PROCEDURE create_ttDOc:
   DEF INPUT  PARAM pop      AS DEC   NO-UNDO.
   DEF INPUT  PARAM type     AS Char  NO-UNDO.

   DEF BUFFER bop for op.
   DEF BUFFER bop-entry for op-entry.



   DEFINE VARIABLE buf-str        AS char   NO-UNDO.   
   DEFINE VARIABLE mCust-Cat      AS CHARACTER          NO-UNDO.
   DEFINE VARIABLE ost          AS decimal            NO-UNDO.
   DEFINE VARIABLE SummaStr     AS CHARACTER EXTENT 2 NO-UNDO.
   DEFINE VARIABLE mSignsVal    AS CHARACTER          NO-UNDO.
   DEFINE VARIABLE i            AS int                NO-UNDO.   
   DEFINE VARIABLE j            AS int                NO-UNDO.   
   DEFINE VARIABLE iDate        AS date               NO-UNDO.   
   DEFINE VARIABLE datebeg_     AS date               NO-UNDO.   
   DEFINE VARIABLE kold         AS int                NO-UNDO.    /* ᪮�쪮 ���� ��ॡ���� ��� ���᪠ ࠡ��� ���� */
   DEFINE VARIABLE koldo        AS int                NO-UNDO.    /* ������⢮ ࠡ��� ���� */

   DEFINE VARIABLE var-sum        AS char   NO-UNDO.   
   DEFINE VARIABLE var-srok       AS char   NO-UNDO.   
   DEFINE VARIABLE var-stat       AS char  NO-UNDO.   
   DEFINE VARIABLE var-inn        AS char   NO-UNDO.   
   DEFINE VARIABLE var-date-ans  AS char   NO-UNDO.   


/*
����� �/� /��� ���᫥��� �� ����� ��� ��.��.����
����� ���
������������ ��-樨, ���
AddressUr - �ਤ��᪨� ����
(��᪮� 䨫���� ��� ���� ����) (� ����ᨬ��� �� 䨫����)
Var_01 �ਢ�� (㪠�뢠���� ��� ���᫥��� �� �/���
(����� �/�) � ���ண� �ந�室�� �ନ஢���� ���㬥��
�㬬� ��ࠬ�:
�㬬� �ய����:
� �ப (����ந�� ��⮬���᪨ 15 ࠡ. ���� � ����, ᫥���饩 �� ��⮩ ���᫥���) �� ��.��.����

*/
   find first bop where bop.op = pop no-lock no-error.
   find first bop-entry of bop no-error.
   find first acct where acct.acct = bop-entry.acct-cr no-lock no-error.
   datebeg_ = bop-entry.op-date.
   j = 0.
   kold = 45.
   koldo = 15.
   do i= 1 to kold:
      iDate = datebeg_ + i.
      vWork = NOT holidayru(iDate).
      if vWork then do:
         j = j + 1.
      end.
      if j >= koldo then do:
         leave.       
      end.
   end.
   var-srok      = string(iDate,"99.99.9999").
   ost           =  op-entry.amt-rub.
   var-sum       = string(ost).
   var-date-ans = GetXattrValueEx("op",STRING(Op.op),"��-��⠄���",?). 

   IF acct.cust-cat = "�" THEN
   DO:
 /* ����� ������ */      
         FIND FIRST cust-corp WHERE
         		   cust-corp.cust-id EQ acct.cust-id NO-LOCK NO-ERROR.
         IF NOT AVAIL cust-corp THEN RETURN.
     
         var-NameOrg = cust-corp.name-corp.
         var-NameShort = cust-corp.name-short.
   END. /*�ਤ��᪮� ���*/   
   IF acct.cust-cat = "�" THEN
   DO:
 /* ����� ������ */      
         FIND FIRST person WHERE
         		   person.person-id EQ acct.cust-id NO-LOCK NO-ERROR.
         IF NOT AVAIL person THEN RETURN.
         var-NameOrg = "�� " + person.name-last + " " + person.first-names.
         var-NameShort = var-NameOrg.
   END. /* �����᪮� ��� */   
   create ttDOc.
   assign
      ttDOc.branch       =  bop.filial-id
      ttDOc.op-date      =  bop.op-date
      ttDOc.rec-name     =  var-NameShort
      ttDOc.account      =  acct.acct
      ttDOc.num-doc      =  bop.doc-num
      ttDOc.currency     =  if bop-entry.currency = "" then "643" else bop-entry.currency
      ttDOc.amt          =  if bop-entry.currency = "" then bop-entry.amt-rub else bop-entry.amt-cur
      ttDOc.date-mess    =  date(var-srok)
      ttDOc.date-ans     =  if var-date-ans <> ? then  var-date-ans  else ""
      ttDOc.type         =  type
      ttDOc.op           =  string(bop.op)
   .
END PROCEDURE.




