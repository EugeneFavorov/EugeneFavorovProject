
/* +++ plibdel.i was humbly modified by (c)blodd converter v.1.09 on 7/13/2015 6:51am +++ */

RUN DeleteOldDataProtocol IN h_base ("op-contract-date").
RUN DeleteOldDataProtocol IN h_base ("op-id").  


DO vPLI-I = 1 TO PLI-maxlibcount:
   RUN UnLoadByProcedure(vPLI-libhandle[vPLI-I]).
   IF VALID-HANDLE(vPLI-libhandle[vPLI-I]) THEN DELETE PROCEDURE vPLI-libhandle[vPLI-I].
END. 

/* --- plibdel.i was humbly modified by (c)blodd converter v.1.09 on 7/13/2015 6:51am --- */
