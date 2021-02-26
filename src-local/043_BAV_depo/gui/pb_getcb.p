{globals.i}
{intrface.get tmess}

/* +++ pb_getcb.p was humbly modified by (c)blodd converter v.1.09 on 12/16/2016 7:07am +++ */

/* Выбор ценной бумаги по F1 в PROMPT */

DEFINE INPUT PARAMETER level AS CHARACTER NO-UNDO.

{pick-val.i}
pick-value = "".
DO TRANSACTION:
    RUN browseld.p ("sec-code", "instr-cat", "sec-code", ?, 5).
END.

/* --- pb_getcb.p was humbly modified by (c)blodd converter v.1.09 on 12/16/2016 7:07am --- */
