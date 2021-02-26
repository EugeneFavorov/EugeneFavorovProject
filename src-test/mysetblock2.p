/*   
     Filename: setblock.p
     Created:  kam
     Comment:  Устанавливаем блокировку БлокВозвр
*/


DEF INPUT  PARAM in-rid  AS RECID NO-UNDO.

/*
DEF INPUT  PARAM in-rid1 AS RECID NO-UNDO.
DEF OUTPUT PARAM fl      AS INT64   NO-UNDO INIT 0.
  */
DEF VAR mSum AS DECIMAL NO-UNDO.
DEF VAR acctblock AS CHAR NO-UNDO. 
/*
message "1" view-as alert-box.
  */


FIND op WHERE RECID(op)  = in-rid NO-LOCK NO-ERROR .
IF NOT AVAIL op THEN 
DO :
    RETURN .
END.
find first op-entry of op no-lock no-error.


mSum = - op-entry.amt-rub.
acctblock = op-entry.acct-db.
/*
message string(op-entry.amt-rub) view-as alert-box.
  */
FOR EACH  BlockObject WHERE
    BlockObject.class-code   EQ 'BlockAcct'
    AND BlockObject.FILE-NAME    EQ 'acct'
    AND BlockObject.block-type   EQ 'БлокВозвр'
    AND BlockObject.surrogate    EQ acctblock + ','
    AND BlockObject.end-datetime EQ BlockObject.beg-datetime
    AND BlockObject.val[3] < 0
    :

    mSum = BlockObject.val[3] - mSum.
    IF mSum < 0 THEN do:
            BlockObject.val[3] = mSum.
            BlockObject.end-datetime = ?.
            mSum = 0.
            /* return. */
    end.
    else do:
            BlockObject.val[3] =  0.
            mSum = - mSum.
    end.
END.


/*
mSum = op-entry.amt-rub.
acctblock = op-entry.acct-db.
/*
message string(op-entry.amt-rub) view-as alert-box.
  */
FOR EACH  BlockObject WHERE
    BlockObject.class-code   EQ 'BlockAcct'
    AND BlockObject.FILE-NAME    EQ 'acct'
    AND BlockObject.block-type   EQ 'БлокВозвр'
    AND BlockObject.surrogate    EQ acctblock + ','
    AND BlockObject.end-datetime EQ BlockObject.beg-datetime 
    :

    mSum = ABS(BlockObject.val[3]) - mSum.
    IF mSum > 0 THEN
        ASSIGN
            BlockObject.val[3] =  - mSum
            BlockObject.end-datetime = ?.
END.
*/
