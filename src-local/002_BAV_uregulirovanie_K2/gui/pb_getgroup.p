{globals.i}
{intrface.get tmess}

/* +++ pb_getgroup.p was humbly modified by (c)blodd converter v.1.09 on 10/13/2016 2:22pm +++ */

/* Выбор группы по F1 в PROMPT */

DEFINE INPUT PARAMETER level AS CHARACTER NO-UNDO.

{pick-val.i}
pick-value = ?.
RUN browseld.p("acct-group", "class", "acct-group", "", 4).

/* --- pb_getgroup.p was humbly modified by (c)blodd converter v.1.09 on 10/13/2016 2:22pm --- */
