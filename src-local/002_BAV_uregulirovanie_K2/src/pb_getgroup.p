/* Выбор группы по F1 в PROMPT */

DEFINE INPUT PARAMETER level AS CHARACTER NO-UNDO.

{pick-val.i}
pick-value = ?.
RUN browseld.p("acct-group", "class", "acct-group", "", 4).
