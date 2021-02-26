RUN DeleteOldDataProtocol IN h_base ("op-contract-date").
RUN DeleteOldDataProtocol IN h_base ("op-id").  


DO vPLI-I = 1 TO PLI-maxlibcount:
   RUN UnLoadByProcedure(vPLI-libhandle[vPLI-I]).
   IF VALID-HANDLE(vPLI-libhandle[vPLI-I]) THEN DELETE PROCEDURE vPLI-libhandle[vPLI-I].
END. 
