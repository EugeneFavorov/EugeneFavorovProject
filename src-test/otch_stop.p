/* ��������� �� ������樨 */
/* kam */



{globals.i}
{setdest.i}

def var i as int no-undo.

i = 1.

FOR each code WHERE
   code.class EQ 'StopList'
   AND code.misc[1] EQ '�'
   AND code.parent EQ 'StopList'
   NO-LOCK:
       
    FIND FIRST loan where loan.contract = '�।��'
    and loan.cust-cat = '�'
    and loan.filial-id <> '0400'
    and loan.close-date = ?
    and loan.cust-id = int64(misc[2]) NO-LOcK no-error.
    IF AVAIL loan then do:
        put unformatted string(i) ';' trim(loan.cont-code) ';' code.misc[3] ';' code.misc[7] skip.
        i = i + 1.
    end.
END.

{preview.i}       

  
  