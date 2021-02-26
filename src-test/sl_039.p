/*

7.	�㭪樮����� �ॡ������ [���஡��� ���⠭���� ����� (�� 蠣��, � ���ᠭ��� �����⬠ ࠡ��� ��⥬�, �ॡ������ � ��� ����, ����䥩�� � ��.)]
� �裡 � ���㯫����� � 01.03.2018�. ������樨 ����� ���ᨨ 181- �  � ���浪� �।�⠢����� १����⠬� � ��१����⠬� 㯮�����祭�� ������ ���⢥ত���� ���㬥�⮢ � ���ଠ樨 �� �����⢫���� ������� ����権, � ������ �ଠ� ��� � ���⭮�� �� ������ ������, ���浪� � �ப�� �� �।�⠢�����, ����室��� � ��� ��᪢�� �믮����� ᫥���騥 ��ࠡ�⪨:
7.1.	�������  ����, ����� �������� ����� ����樨 �����⮢, ��������� ��� ��।������ ���ਨ, � ������ � ᮤ�ঠ��� ����樨 ������ ���� 㪠��� ��� ���� ����樨 {VO}
7.2.	����� ���� �믮����� � ������ - ��楤�� ���� - �롮� ���� - �ଠ� ���� Excel
7.3.	�� ����᪥ ���� ������ ���� �஠������஢��� ��� ������:
- ��᪨ ���: 406*, 407*, 40802*.
- �����祭�� ���: �����, �����, �࠭�1.
 � ��砥 ������ �㦭�� ����権 ��ࠧ��� �� � ����.
7.4.	��ଠ ����: 
        ����஫� �㬬 200 000 �㡫��                                                 							

��� 䨫����/��	������������ ������ ��/��	��� ������	��� ���⥦�	����� ���⥦�	�㬬� � ����� ��ॢ���	���������� � ���.�����	���ࠢ����� ���⥦�:    1 - ���᫥���,2 - ���ᠭ��	�����祭�� ���⥦�
0500/0300/0000	��� ����誠	40702-840-0-0000-0027356	01.03.18�.	���஢�� ��� �㪢����� ������祭�� ������ (840/USD)	1000.00		� ��砥 �᫨ ��� ������ 㪠��� �� ��. ��� ���ࠢ����� ���⥦� - 2, �� ��. ���- 1	����ঠ��� ����樨 � ����� {VO}

*/

{globals.i}
{tmprecid.def}          /** �ᯮ��㥬 ���ଠ�� �� ��㧥� */

{sh-defs.i}
{pb_exf_exl.i}          /** ������⥪� ��� ���㧪� � XL */


DEFINE VARIABLE List-bal     AS CHARACTER NO-UNDO.    /* ���᮪ �����ᮢ�� ��⮢ */
DEFINE VARIABLE List-cont    AS CHARACTER NO-UNDO.    /* ���᮪ �����祭�� ��⮢ */
DEFINE VARIABLE cXL          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFl          AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAmt         AS decimal   NO-UNDO.


def temp-table op_  no-undo
    field op          as int
    field filial-id   as char
    field name-ben    as char
    field acct        as char
    field op-date     as date
    field curr        as char
    field amt-cur     as dec
    field amt-rub     as dec
    field direct      as char
    field details     as char
    index date op-date filial-id acct amt-rub
    .

list-bal  = "406*,407*,40802*".
list-cont = "�����,�����,�࠭�1".
{getdates.i}

FOR EACH tmprecid NO-LOCK, 
   FIRST cust-corp     WHERE RECID(cust-corp) EQ tmprecid.id NO-LOCK :

   for each acct where  acct.cust-cat EQ '�'
                   and  acct.cust-id  EQ cust-corp.cust-id
                   and  acct.acct-cat EQ 'b'
                   and  CAN-DO(list-bal,string(acct.bal-acct))
                   NO-LOCK:
      if not CAN-DO(list-cont,acct.contract) then next.

      for each op-entry where  op-entry.acct-cr    = acct.acct
                          and  op-entry.op-date   >= beg-date
                          and  op-entry.op-date   <= end-date
                          and  op-entry.op-status >= chr(251)
                          no-lock.
         FIND FIRST op OF op-entry NO-LOCK NO-ERROR.
         if substr(op.details,2,2) <> "VO" then next.

         create op_.
         assign
            op_.op          = op-entry.op  
            op_.filial-id   = op-entry.filial-id
            op_.name-ben    = cust-corp.name-short
            op_.acct        = acct.number
            op_.op-date     = op-entry.op-date
            op_.curr        = if acct.curr = "" then "810" else acct.curr
            op_.amt-cur     = if acct.curr = "" then op-entry.amt-rub else op-entry.amt-cur 
            op_.amt-rub     = op-entry.amt-rub
            op_.direct      = "���᫥���"
            op_.details     = op.details
         .
      end.
      for each op-entry where  op-entry.acct-db    = acct.acct
                          and  op-entry.op-date   >= beg-date
                          and  op-entry.op-date   <= end-date
                          and  op-entry.op-status >= chr(251)
                          no-lock.
         FIND FIRST op OF op-entry NO-LOCK NO-ERROR.
         if substr(op.details,2,2) <> "VO" then next.

         create op_.
         assign
            op_.op          = op-entry.op  
            op_.filial-id   = op-entry.filial-id
            op_.name-ben    = cust-corp.name-short
            op_.acct        = acct.number
            op_.op-date     = op-entry.op-date
            op_.curr        = if acct.curr = "" then "810" else acct.curr
            op_.amt-cur     = if acct.curr = "" then op-entry.amt-rub else op-entry.amt-cur 
            op_.amt-rub     = op-entry.amt-rub
            op_.direct      = "���ᠭ��"
            op_.details     = op.details
         .
      end.
   end.
end.

output to "op.txt".
for each op_.
   export op_.
end.
output close.



cFl = "./sl_039.xml".
output to VALUE(cFl).


/*
��� 䨫����/��	������������ ������ ��/��	��� ������	��� ���⥦�	����� ���⥦�	�㬬� � ����� ��ॢ���	���������� � ���.�����	
���ࠢ����� ���⥦�:    1 - ���᫥���,2 - ���ᠭ��	�����祭�� ���⥦�
0500/0300/0000	��� ����誠	40702-840-0-0000-0027356	01.03.18�.	���஢�� ��� �㪢����� ������祭�� ������ (840/USD)	1000.00		� ��砥 �᫨ ��� ������ 㪠��� �� ��. ��� ���ࠢ����� ���⥦� -2, �� ��. ���- 1	����ঠ��� ����樨 � ����� {VO}
*/               

PUT UNFORMATTED XLHead("tmp", "CCCCCNNCCC", "90,200,150,70,70,110,110,100,80,500").
cXL = XLCellHead("��� 䨫����/��",0,0,0)
    + XLCellHead("������������ ������ ��/��",0,0,0)
    + XLCellHead("��� ������",0,0,0)
    + XLCellHead("��� ���⥦�",0,0,0)
    + XLCellHead("����� ���⥦�",0,0,0)
    + XLCellHead("�㬬� � ����� ��ॢ���",0,0,0)
    + XLCellHead("���������� � ���.�����",0,0,0)
    + XLCellHead("���ࠢ����� ���⥦�",0,0,0)
    + XLCellHead("�� ���㬥��",0,0,0)
    + XLCellHead("�����祭�� ���⥦�",0,0,0)
    .
PUT UNFORMATTED XLRow(0) cXL XLRowEnd().

FOR EACH op_ where op_.amt-rub >= mAmt
   NO-LOCK.
   cXL = XLCell(string(op_.filial-id ))
       + XLCell(STRING(op_.name-ben  ))
       + XLCell(STRING(op_.acct      ))
       + XLCell(STRING(op_.op-date   ))  
       + XLCell(STRING(op_.curr      ))
       + XLNumCell    (op_.amt-cur   )
       + XLNumCell    (op_.amt-rub   )
       + XLCell(STRING(op_.direct    ))
       + XLCell(STRING(op_.op        ))
       + XLCellWrap(STRING(op_.details   ))
       .
   PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
END.


PUT UNFORMATTED XLEnd().
OUTPUT CLOSE.

/*
    RUN mail-add.p ("FM-Report").
    RUN pb_mail.p ("v.ignatchenko", "FM Report", "FM Report", cFl).
    RUN pb_mail.p (RETURN-VALUE, "FM Report", "FM Report", cFl).
*/

  
    RUN sndbispc ("file=" + cFl + ";class=bq").



