/*

����� 02.
����㯨��  ��筥��� �ॡ������ � �����:
1.	�������� ��� ������, �� ����饣� �ਧ���� 3,  ���㬥�� ���ண� ������������,  �뢮� � ���� �� ��������� � �ਧ��� �ਭ��������� ᯨ�� 550-�
2.	��� ������ �������� ���� ����⮢��� ����� �� 3 ����� �� ���� �ନ஢���� ����. 
3.	� 蠡���� ��� �㬬 ������ �ଠ� �᫮, ���� ��ꥤ������ �祥�. - ⠪ � �뫮 ᤥ����.

*/


{globals.i}
{prn-doc.def &with_proc=YES}

{tmprecid.def}          /** �ᯮ��㥬 ���ଠ�� �� ��㧥� */
{intrface.get xclass}   /** �㭪樨 ��� ࠡ��� � ����奬�� */
{intrface.get strng}    /** �㭪樨 ��� ࠡ��� � ��ப��� */

{sh-defs.i}
{pb_exf_exl.i}          /** ������⥪� ��� ���㧪� � XL */

{justasec}
{intrface.get refer}    /* ������⥪� �㦡� �ࠢ�筨���. */
{intrface.get date}

DEF VAR mStrTable   AS CHAR   NO-UNDO.
DEF VAR str02       AS CHAR   NO-UNDO.
def var inn_pol     as char   NO-UNDO.
def var inn_pl      as char   NO-UNDO.
def var name_pl     as char   NO-UNDO.
def var name_       as char   NO-UNDO.
def var name_c      as char   NO-UNDO.
def var name_pred   as char   NO-UNDO.
def var name_ofm    as char   NO-UNDO.
def var name_ofm_c  as char   NO-UNDO.
DEF VAR vOpTimeB    AS INT    NO-UNDO.
def var inn_        as char   NO-UNDO.
def var inn_c       as char   NO-UNDO.
def var acct_       as char   NO-UNDO.
def var acct_c      as char   NO-UNDO.
def var t550_c      as char   NO-UNDO.
def var mOtsenka    as char   NO-UNDO.
def var city_       as char   NO-UNDO.
def var client_c    as char   NO-UNDO.
def var bank_       as char   NO-UNDO.
def var sort_       as char   NO-UNDO.
def var mmm_        as char   NO-UNDO.
def var int_mmm_    as int    NO-UNDO.
def var int_yyy_    as int    NO-UNDO.
def var int_ddd_    as int    NO-UNDO.

def var cli_AFM     as char   NO-UNDO init "".    
def var cli_550     as char   NO-UNDO init "".  
def var summa_db    as dec    NO-UNDO init 0.  



DEFINE VARIABLE cXL   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFl   AS CHARACTER NO-UNDO.
def var date1       as date   NO-UNDO.
   def var i             as INT64   no-undo .
   def var j             as INT64   no-undo .
   DEF VAR vWork         AS LOG     NO-UNDO.
   def var iDate         as date    no-undo.

                   
def temp-table acct_ no-undo
    field acct      as char
    field inn-pl    as char
    field name-pl   as char
    field OFM       as char
    index pl IS PRIMARY UNIQUE inn-pl
    .
def temp-table op_  no-undo
    field op          as int
    field inn         as char
    field inn_c       as char
    field name_ofm_c  as char
    field t550_c      as char
    field name        as char
    field city        as char
    field name_c      as char
    field bic_c       as char
    field client_c    as char
    field summa       as dec
    field sort        as char
    field cli_AFM     as char
    field cli_550     as char
    field summa_db    as dec
    field stop_       as char
    field stop_c      as char
    index pol sort inn
    .

DEFINE BUFFER bacct_ FOR acct_. 
   end-date = today .
   do i= 1 to 45:
      end-date = end-date - i.
      if not holidayru(end-date) then do:
         leave.
      end.
   end.

vOpTimeB = TIME.


for each op-entry where op-entry.op-date = end-date
                    and op-entry.currency = ""
                    and op-entry.acct-cat = "b"
   no-lock,
   first op of op-entry
   no-lock:
   inn_  = "".
   inn_c = "".
   name_ofm_c  = "".
   t550_c = "".
   acct_  = "".
   acct_c = "".
   mOtsenka = "".
   city_  = "".
   name_ = "".
   name_c = "".
   client_c = "".
   bank_ = "".
   sort_ = "".
   cli_AFM   = "".
   cli_550   = "".
   summa_db  = 0.

   if op-entry.acct-db begins "3" or op-entry.acct-cr begins "3" then do:
      /* ���譨� ���⥦� */
      /* � ���� �⡨����� ���譨� ���㬥���  � �㡫��,  � ������ �� ������ ��� �।��� 
         㪠��� ���  ��襣� ������ (�ਤ��᪮� ��� ��� ��) � ������������  �� ��������� 
         ��� ࠢ�� 1, 2, 4, 
      */

      if op-entry.acct-db begins "3" then do:
         acct_ = op-entry.acct-cr.
         acct_c = acct-db.
         sort_ = "2".
      end.
      else  do:
         acct_ = op-entry.acct-db.
         acct_c = acct-cr.
         sort_ = "1".
      end.
      if not acct_ begins "4" then next.
      find first acct where acct.acct = acct_ no-lock no-error.
      if not avail acct then next.
      if not (acct.cust-cat = "�" or acct.cust-cat = "�") then next.
      if acct.cust-cat = "�" then do:
         find first cust-corp  where cust-corp.cust-id = acct.cust-id no-lock no-error.
         if not avail cust-corp then next.
         name_ = cust-corp.name-short.
         inn_ = cust-corp.inn.
         name_ofm   = GetXattrValueEX("cust-corp", STRING(cust-corp.cust-id), "���������", "").
      end.
      if acct.cust-cat = "�" then do:
         find first person  where person.person-id = acct.cust-id no-lock no-error.
         if not avail person then next.
         name_pred  = GetXattrValueEX("person", STRING(person.person-id), "�।��", "").
         if  name_pred <> "�।��" then next.
         name_ofm   = GetXattrValueEX("person", STRING(person.person-id), "���������", "").
         inn_ = person.inn.
         name_ = person.name-last + " " + person.first-names.
      end.
      cli_AFM = name_ofm.
      find first LegCBROtk_mart where LegCBROtk_mart.inn = inn_ no-lock no-error.
      if avail LegCBROtk_mart then do:
         cli_550 = "550-�".
      end.

      run acct-pos in h_base (acct.acct,acct.currency,end-date - 90,end-date,?).
      summa_db = sh-db.

      if name_ofm <> "3" then do:
         /* � ᫥����� �����:
            1. ����ࠣ��⮬ ���� ��.��� ��� ��, ����� �����  ��� ���� �����-���� ����� ��� 
               ⥪�騩 ��� � ��襬 �����, � � ���ண�  �� ��������� =3, 
               ��� �� ᮤ�ন��� � ᯨ᪥ ��  550-�;
            2. ����ࠣ��⮬ ���� �૨� ��� ��, ����� �� ���� ��訬 �����⮬, 
               �� ᮤ�ন��� � ᯨ᪥ 550-�. 
         */


/*

�㦭� ���᪠�� ������ �� ��� ( �� op.inn )

*/
         inn_c  = op.inn.
         if inn_c = "" or inn_c = ?  or inn_c = "0"  or inn_c = "000000000000" then next.

         find first LegCBROtk_mart where LegCBROtk_mart.inn = inn_c no-lock no-error.
         if avail LegCBROtk_mart then do:
            t550_c = "550-�".
         end.
         find cust-corp where cust-corp.inn = op.inn no-lock no-error.
         if avail cust-corp then do:
            client_c = "��".
            name_ofm_c   = GetXattrValueEX("cust-corp", STRING(cust-corp.cust-id), "���������", "").
            inn_c  = cust-corp.inn.
            name_c = cust-corp.name-short.
         end.
         else do:
            find person where person.inn = op.inn no-lock no-error.
            if avail person then do:
               client_c = "��".
               name_pred  = GetXattrValueEX("person", STRING(person.person-id), "�।��", "").
               if  name_pred <> "�।��" then next.
               name_ofm_c   = GetXattrValueEX("person", STRING(person.person-id), "���������", "").
               inn_c = person.inn.
               name_c = person.name-last + " " + person.first-names.
            end.
            else do:
               client_c = "���".
               name_c = op.name-ben.
            end.
         end.

         if name_ofm_c = "3" or t550_c = "550-�"  then do:
            find first branch where branch.branch-id = op.branch-id no-lock no-error.
            if avail branch then do:
               city_ = TRIM(ENTRY(2,branch.address)).
            end.
            find first op-bank where op-entry.op = op-bank.op no-lock no-error.
            if avail op-bank then bank_ = op-bank.bank-code.
            create op_.
            assign
               op_.op          = op.op  
               op_.inn         = inn_
               op_.inn_c       = inn_c
               op_.name_ofm_c  = name_ofm_c
               op_.t550_c      = t550_c
               op_.name        = name_
               op_.city        = city_
               op_.name_c      = name_c
               op_.bic_c       = bank_
               op_.client_c    = client_c
               op_.summa       = op-entry.amt-rub
               op_.sort        = sort_
               op_.cli_AFM     = cli_AFM  
               op_.cli_550     = cli_550  
               op_.summa_db    = summa_db 
            .
         end.
      end.
   end.
   else do:
      /* ����७��� ���⥦� */
      /* � ⠪�� ����७��� ���⥦� � ��⮢ ��� �� ��� �����⮢ (� ������������  �� ��������� 
         ��� ࠢ�� 1, 2, 4), � ᫥����� �����:
         1. ����ࠣ��⮬ ���� ��.��� ��� ��, ����� �����  ��� ���� �����-���� ����� ��� 
            ⥪�騩 ��� � ��襬 �����, � � ���ண�  �� ��������� =3, 
            ��� �� ᮤ�ন��� � ᯨ᪥ ��  550-�;
         2. ����ࠣ��⮬ ���� �૨� ��� ��, ����� �� ���� ��訬 �����⮬, 
            �� ᮤ�ন��� � ᯨ᪥ 550-�. 
       */
      /* ᭠砫� ��⠥� �����⮬ ���� �� ������*/
      acct_  = op-entry.acct-db.
      acct_c = op-entry.acct-cr.
      sort_ = "3".

      if not (acct_ begins "4" and acct_c begins "4") then next.
      find first acct where acct.acct = acct_ no-lock no-error.
      if not avail acct then next.
      if not (acct.cust-cat = "�" or acct.cust-cat = "�") then next.
      inn_pl = "".
      if acct.cust-cat = "�" then do:
         find first cust-corp  where cust-corp.cust-id = acct.cust-id no-lock no-error.
         if not avail cust-corp then next.
         inn_ = cust-corp.inn.
         name_ = cust-corp.name-short.
         name_ofm   = GetXattrValueEX("cust-corp", STRING(cust-corp.cust-id), "���������", "").
      end.
      if acct.cust-cat = "�" then do:
         find first person  where person.person-id = acct.cust-id no-lock no-error.
         if not avail person then next.
         name_pred  = GetXattrValueEX("person", STRING(person.person-id), "�।��", "").
         if  name_pred <> "�।��" then next.
         name_ofm   = GetXattrValueEX("person", STRING(person.person-id), "���������", "").
         inn_ = person.inn.
         name_ = person.name-last + " " + person.first-names.
      end.
      cli_AFM = name_ofm.
      find first LegCBROtk_mart where LegCBROtk_mart.inn = inn_ no-lock no-error.
      if avail LegCBROtk_mart then do:
         cli_550 = "550-�".
      end.
      run acct-pos in h_base (acct.acct,acct.currency,end-date - 90,end-date,?).
      summa_db = sh-db.
      if name_ofm <> "3" then do:
         client_c = "��".
         find first acct where acct.acct = acct_c no-lock no-error.
         if not avail acct then next.
         if not (acct.cust-cat = "�" or acct.cust-cat = "�") then next.
         inn_pl = "".
         if acct.cust-cat = "�" then do:
            find first cust-corp  where cust-corp.cust-id = acct.cust-id no-lock no-error.
            if not avail cust-corp then next.
            inn_c  = cust-corp.inn.
            name_c = cust-corp.name-short.
            name_ofm_c   = GetXattrValueEX("cust-corp", STRING(cust-corp.cust-id), "���������", "").
            name_c = cust-corp.name-short.
            find first LegCBROtk_mart where LegCBROtk_mart.inn = inn_c no-lock no-error.
            if avail LegCBROtk_mart then do:
               t550_c = "550-�".
            end.
         end.
         if acct.cust-cat = "�" then do:
            find first person  where person.person-id = acct.cust-id no-lock no-error.
            if not avail person then next.
            name_pred  = GetXattrValueEX("person", STRING(person.person-id), "�।��", "").
            if  name_pred <> "�।��" then next.
            name_ofm_c   = GetXattrValueEX("person", STRING(person.person-id), "���������", "").
            inn_c = person.inn.
            name_c = person.name-last + " " + person.first-names.
            find first LegCBROtk_mart where LegCBROtk_mart.inn = inn_c no-lock no-error.
            if avail LegCBROtk_mart then do:
               t550_c = "550-�".
            end.
         end.
         if inn_c = inn_ then next.
         if name_ofm_c = "3" or t550_c = "550-�"  then do:
            find first branch where branch.branch-id = op.branch-id no-lock no-error.
            if avail branch then do:
               city_ = TRIM(ENTRY(2,branch.address)).
            end.
            create op_.
            assign
               op_.op          = op.op  
               op_.inn         = inn_
               op_.inn_c       = inn_c
               op_.name_ofm_c  = name_ofm_c
               op_.t550_c      = t550_c
               op_.name        = name_
               op_.city        = city_
               op_.name_c      = name_c
               op_.bic_c       = ""
               op_.client_c    = client_c
               op_.summa       = op-entry.amt-rub
               op_.sort        = sort_
               op_.cli_AFM     = cli_AFM  
               op_.cli_550     = cli_550  
               op_.summa_db    = summa_db 
            .
         end.
      end.

      /* ⥯��� ��⠥� �����⮬ ���� �� �।��� � �஢��塞 ��� � �� ᠬ�� */
      if client_c = "��" then next.
      acct_  = op-entry.acct-cr.
      acct_c = op-entry.acct-db.
      sort_ = "4".

      if not (acct_ begins "4" and acct_c begins "4") then next.
      find first acct where acct.acct = acct_ no-lock no-error.
      if not avail acct then next.
      if not (acct.cust-cat = "�" or acct.cust-cat = "�") then next.
      run acct-pos in h_base (acct.acct,acct.currency,end-date - 90,end-date,?).
      summa_db = sh-db.
      inn_pl = "".
      if acct.cust-cat = "�" then do:
         find first cust-corp  where cust-corp.cust-id = acct.cust-id no-lock no-error.
         if not avail cust-corp then next.
         inn_ = cust-corp.inn.
         name_ = cust-corp.name-short.
         name_ofm   = GetXattrValueEX("cust-corp", STRING(cust-corp.cust-id), "���������", "").
      end.
      if acct.cust-cat = "�" then do:
         find first person  where person.person-id = acct.cust-id no-lock no-error.
         if not avail person then next.
         name_pred  = GetXattrValueEX("person", STRING(person.person-id), "�।��", "").
         if  name_pred <> "�।��" then next.
         name_ofm   = GetXattrValueEX("person", STRING(person.person-id), "���������", "").
         inn_ = person.inn.
         name_ = person.name-last + " " + person.first-names.
      end.
      cli_AFM = name_ofm.
      find first LegCBROtk_mart where LegCBROtk_mart.inn = inn_ no-lock no-error.
      if avail LegCBROtk_mart then do:
         cli_550 = "550-�".
      end.
      run acct-pos in h_base (acct.acct,acct.currency,today - 90,today,?).
      summa_db = sh-db.
      if name_ofm <> "3" then do:
         client_c = "��".
         find first acct where acct.acct = acct_c no-lock no-error.
         if not avail acct then next.
         if not (acct.cust-cat = "�" or acct.cust-cat = "�") then next.
         inn_pl = "".
         if acct.cust-cat = "�" then do:
            find first cust-corp  where cust-corp.cust-id = acct.cust-id no-lock no-error.
            if not avail cust-corp then next.
            inn_c  = cust-corp.inn.
            name_c = cust-corp.name-short.
            name_ofm_c   = GetXattrValueEX("cust-corp", STRING(cust-corp.cust-id), "���������", "").
            name_c = cust-corp.name-short.
            find first LegCBROtk_mart where LegCBROtk_mart.inn = inn_c no-lock no-error.
               if avail LegCBROtk_mart then do:
                  t550_c = "550-�".
               end.
         end.
         if acct.cust-cat = "�" then do:
            find first person  where person.person-id = acct.cust-id no-lock no-error.
            if not avail person then next.
            name_pred  = GetXattrValueEX("person", STRING(person.person-id), "�।��", "").
            if  name_pred <> "�।��" then next.
            name_ofm_c   = GetXattrValueEX("person", STRING(person.person-id), "���������", "").
            inn_c = person.inn.
            name_c = person.name-last + " " + person.first-names.
            find first LegCBROtk_mart where LegCBROtk_mart.inn = inn_c no-lock no-error.
               if avail LegCBROtk_mart then do:
                  t550_c = "550-�".
               end.
         end.
         if inn_c = inn_ then next.
         if name_ofm_c = "3" or t550_c = "550-�"  then do:
            find first branch where branch.branch-id = op.branch-id no-lock no-error.
            if avail branch then do:
               city_ = TRIM(ENTRY(2,branch.address)).
            end.
            create op_.
            assign
               op_.op          = op.op  
               op_.inn         = inn_
               op_.inn_c       = inn_c
               op_.name_ofm_c  = name_ofm_c
               op_.t550_c      = t550_c
               op_.name        = name_
               op_.city        = city_
               op_.name_c      = name_c
               op_.bic_c       = ""
               op_.client_c    = client_c
               op_.summa       = op-entry.amt-rub
               op_.sort        = sort_
               op_.cli_AFM     = cli_AFM  
               op_.cli_550     = cli_550  
               op_.summa_db    = summa_db 
            .
         end.
      end.
   end.
end.


for each op_.
   FIND FIRST code WHERE
              code.class EQ "fm-rec"
          AND code.code  EQ op_.inn_c
   NO-LOCK NO-ERROR.
   IF AVAIL code THEN DO:
      delete op_.
   END.
end.


output to "stop.err".

for each op_.
   FIND FIRST code WHERE
              code.class   EQ "StopList"
          AND code.misc[5] EQ op_.inn
   NO-LOCK NO-ERROR.
   IF AVAIL code THEN DO:
      op_.stop_ = "��".
   END.
   FIND FIRST code WHERE
              code.class   EQ "StopList"
          AND code.misc[5] EQ op_.inn_c
   NO-LOCK NO-ERROR.
   IF AVAIL code THEN DO:
      op_.stop_c = "��".
   END.
end.




output close.

/*����室���  � ������� ࠧ���� ����  � �� ������� � �� ����ࠣ����  �������� �������: ����稥 � �⮯���� �����.*/

cFl = "./fm.xml".

OUTPUT TO VALUE(cFl).

PUT UNFORMATTED XLHead("tmp", "CCCCCNCCCCCCCCNC", "130,90,90,116,110,118,160,130,90,80,72,52,116,110,118,112").
/* 
PUT UNFORMATTED XLRow(0) XLCellHat("���� �� ᮬ��⥫�� ���⥦�� �����⮢ �� " + STRING(end-date, "99.99.9999"), 13) XLRowEnd().
*/

cXL = XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("���� �� ᮬ��⥫�� ���⥦�� �����⮢ �� " + STRING(end-date, "99.99.9999"),0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    .
PUT UNFORMATTED XLRow(0) cXL XLRowEnd().




cXL = XLCellHead("������ ��� ���� ����~n��� �ਧ���� 3",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("����ࠣ���",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    .
PUT UNFORMATTED XLRow(0) cXL XLRowEnd().



cXL = XLCellHead("������������",0,0,0)
    + XLCellHead("���",0,0,0)
    + XLCellHead("������ ��",0,0,0)
    + XLCellHead("550-�",0,0,0)
    + XLCellHead("�室�� � �⮯-����",0,0,0)
    + XLCellHead("����� c " + string(end-date - 90,"99/99/9999"),0,0,0)
    + XLCellHead("��த ���ࠧ�������, ��� ���㦨������ ��� ������",0,0,0)
    + XLCellHead("������������",0,0,0)
    + XLCellHead("���",0,0,0)
    + XLCellHead("��� ����� ",0,0,0)
    + XLCellHead("����� �ਧ��� 3",0,0,0)
    + XLCellHead("��� ������",0,0,0)
    + XLCellHead("�室�� � ᯨ᮪ ��  550-�",0,0,0)
    + XLCellHead("�室�� � �⮯-����",0,0,0)
    + XLCellHead("�㬬� ����樨~n�㡫�",0,0,0)
    + XLCellHead("�����䨪��� ���㬥��",0,0,0)
    .

PUT UNFORMATTED XLRow(0) cXL XLRowEnd().


cXL = XLCellHead("1",0,0,0)
    + XLCellHead("2",0,0,0)
    + XLCellHead("3",0,0,0)
    + XLCellHead("4",0,0,0)
    + XLCellHead("5",0,0,0)
    + XLCellHead("6",0,0,0)
    + XLCellHead("7",0,0,0)
    + XLCellHead("8",0,0,0)
    + XLCellHead("9",0,0,0)
    + XLCellHead("10",0,0,0)
    + XLCellHead("11",0,0,0)
    + XLCellHead("12",0,0,0)
    + XLCellHead("13",0,0,0)
    + XLCellHead("14",0,0,0)
    + XLCellHead("15",0,0,0)
    + XLCellHead("16",0,0,0)
    .
PUT UNFORMATTED XLRow(0) cXL XLRowEnd().

PUT UNFORMATTED XLRow(0) XLEmptyCells(14) XLRowEnd().

cXL = XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("��室�騥 ���譨�",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    .

PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
PUT UNFORMATTED XLRow(0) XLEmptyCells(14) XLRowEnd().
FOR EACH op_ where op_.sort = "1".
    NO-LOCK.
   cXL = XLCell(string(op_.name))

       + XLCell(STRING(op_.inn))

       + XLCell(STRING(op_.cli_AFM))
       + XLCell(STRING(op_.cli_550))  
       + XLCell(STRING(op_.stop_))
       + XLNumCell(op_.summa_db)
       + XLCell(STRING(op_.city))
       + XLCell(STRING(op_.name_c))

       + XLCell(STRING(op_.inn_c))

       + XLCell(STRING(op_.bic_c))
       + XLCell(STRING(op_.name_ofm_c))
       + XLCell(STRING(op_.client_c))
       + XLCell(STRING(op_.t550_c))
       + XLCell(STRING(op_.stop_c))
       + XLNumCell(op_.summa)
       + XLCell(STRING(op_.op))
       .
   PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
END.
/*
PUT UNFORMATTED XLRow(0) XLCellHead("�室�騥 ���譨�",0,0,13) XLRowEnd().
*/
cXL = XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("�室�騥 ���譨�",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    .

PUT UNFORMATTED XLRow(0) cXL XLRowEnd().


PUT UNFORMATTED XLRow(0) XLEmptyCells(14) XLRowEnd().
FOR EACH op_ where op_.sort = "2".
    NO-LOCK.
   cXL = XLCell(string(op_.name))
       + XLCell(STRING(op_.inn))
       + XLCell(STRING(op_.cli_AFM))
       + XLCell(STRING(op_.cli_550))  
       + XLCell(STRING(op_.stop_))
       + XLNumCell(op_.summa_db)
       + XLCell(STRING(op_.city))
       + XLCell(STRING(op_.name_c))
       + XLCell(STRING(op_.inn_c))
       + XLCell(STRING(op_.bic_c))
       + XLCell(STRING(op_.name_ofm_c))
       + XLCell(STRING(op_.client_c))
       + XLCell(STRING(op_.t550_c))
       + XLCell(STRING(op_.stop_c))
       + XLNumCell(op_.summa)
       + XLCell(STRING(op_.op))
       .
   PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
END.
/*
PUT UNFORMATTED XLRow(0) XLCellHead("��室�騥 ����७���",0,0,13) XLRowEnd().
*/

cXL = XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("��室�騥 ����७���",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    .
PUT UNFORMATTED XLRow(0) cXL XLRowEnd().


PUT UNFORMATTED XLRow(0) XLEmptyCells(14) XLRowEnd().
FOR EACH op_ where op_.sort = "3".
    NO-LOCK.
   cXL = XLCell(string(op_.name))
       + XLCell(STRING(op_.inn))
       + XLCell(STRING(op_.cli_AFM))
       + XLCell(STRING(op_.cli_550))  
       + XLCell(STRING(op_.stop_))
       + XLNumCell(op_.summa_db)
       + XLCell(STRING(op_.city))
       + XLCell(STRING(op_.name_c))
       + XLCell(STRING(op_.inn_c))
       + XLCell(STRING(op_.bic_c))
       + XLCell(STRING(op_.name_ofm_c))
       + XLCell(STRING(op_.client_c))
       + XLCell(STRING(op_.t550_c))
       + XLCell(STRING(op_.stop_c))
       + XLNumCell(op_.summa)
       + XLCell(STRING(op_.op))
       .
   PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
END.
/*
PUT UNFORMATTED XLRow(0) XLCellHead("�室�騥 ����७���",0,0,13) XLRowEnd().
*/
cXL = XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("�室�騥 ����७���",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    + XLCellHead("",0,0,0)
    .
PUT UNFORMATTED XLRow(0) cXL XLRowEnd().


PUT UNFORMATTED XLRow(0) XLEmptyCells(14) XLRowEnd().
FOR EACH op_ where op_.sort = "4".
    NO-LOCK.
   cXL = XLCell(string(op_.name))
       + XLCell(STRING(op_.inn))
       + XLCell(STRING(op_.cli_AFM))
       + XLCell(STRING(op_.cli_550))  
       + XLCell(STRING(op_.stop_))
       + XLNumCell(op_.summa_db)
       + XLCell(STRING(op_.city))
       + XLCell(STRING(op_.name_c))
       + XLCell(STRING(op_.inn_c))
       + XLCell(STRING(op_.bic_c))
       + XLCell(STRING(op_.name_ofm_c))
       + XLCell(STRING(op_.client_c))
       + XLCell(STRING(op_.t550_c))
       + XLCell(STRING(op_.stop_c))
       + XLNumCell(op_.summa)
       + XLCell(STRING(op_.op))
       .
   PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
END.


PUT UNFORMATTED XLEnd().
OUTPUT CLOSE.

    RUN mail-add.p ("FM-Report").
/*
    RUN pb_mail.p ("v.ignatchenko", "FM Report", "FM Report", cFl).

*/

    RUN pb_mail.p (RETURN-VALUE, "FM Report", "FM Report", cFl).

