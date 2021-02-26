{globals.i}

DEFINE INPUT PARAMETER level AS INT64 NO-UNDO.
DEFINE VARIABLE vClientCode AS INTEGER NO-UNDO.

RUN browseld.p ("person", "", "", ?, 3).
  
vClientCode = INTEGER(pick-value).
  
FIND FIRST person WHERE person.person-id EQ vClientCode NO-LOCK NO-ERROR.
  
pick-value = person.name-last + ' ' + person.first-name.