/* Выбор организации из классификатора ОргРЕПО по F1 в PROMPT */

DEFINE INPUT PARAMETER level AS CHARACTER NO-UNDO.

{pick-val.i}
pick-value = "".
RUN codelay.p ("ОргРЕПО", "ОргРЕПО", "Организация по сделке РЕПО", 5).
