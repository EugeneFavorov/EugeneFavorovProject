/* �롮� 業��� �㬠�� �� F1 � PROMPT */

DEFINE INPUT PARAMETER level AS CHARACTER NO-UNDO.

{pick-val.i}
pick-value = "".
DO TRANSACTION:
    RUN browseld.p ("sec-code", "instr-cat", "sec-code", ?, 5).
END.
