/* Выбор ценной бумаги по F1 в PROMPT */

DEFINE INPUT PARAMETER level AS CHARACTER NO-UNDO.

{pick-val.i}
pick-value = "".
DO TRANSACTION:
    RUN browseld.p ("sec-code", "instr-cat", "sec-code", ?, 5).
END.
