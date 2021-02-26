
/* kam */
/* ᭨���� �����஢�� ���������, �᫨ ���� */

{globals.i}
{sh-defs.i}

Define Input Parameter acctblock As CHAR. /* ����� ��� � ���ண� ᭨���� �����஢�� */
Define Output Parameter mSum As DECIMAL.

mSum = 0.

FOR EACH  BlockObject WHERE
          BlockObject.class-code   EQ 'BlockAcct'
      AND BlockObject.FILE-NAME    EQ 'acct'
      AND BlockObject.block-type   EQ '���������'
      AND BlockObject.surrogate    EQ acctblock + ','
      AND BlockObject.end-datetime EQ ?
:

ASSIGN
    BlockObject.end-datetime = BlockObject.beg-datetime. 

END.


