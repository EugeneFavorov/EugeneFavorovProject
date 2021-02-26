{globals.i}
{intrface.get tmess}

/* +++ pb_getcust.p was humbly modified by (c)blodd converter v.1.09 on 12/16/2016 7:07am +++ */

/* Выбор организации из классификатора ОргРЕПО по F1 в PROMPT */

DEFINE INPUT PARAMETER level AS CHARACTER NO-UNDO.

{pick-val.i}
pick-value = "".
RUN codelay.p ("ОргРЕПО", "ОргРЕПО", "Организация по сделке РЕПО", 5).

/* --- pb_getcust.p was humbly modified by (c)blodd converter v.1.09 on 12/16/2016 7:07am --- */
