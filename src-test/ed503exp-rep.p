/*              
                ­ª®¢áª ï ¨­â¥£à¨à®¢ ­­ ï á¨áâ¥¬  ˆ‘ª¢¨â
    Copyright: (C) 1992-2014 ‡€Ž " ­ª®¢áª¨¥ ¨­ä®à¬ æ¨®­­ë¥ á¨áâ¥¬ë"
     Filename: ED503EXP-REP.P
      Comment: ‚ë¢®¤ ¯à®â®ª®«  ª®­âà®«ï á®®¡é¥­¨© SWIFT
   Parameters: iHFiles - åí­¤« ¢à¥¬¥­­®© â ¡«¨æë
         Uses:
      Used BY:
      Created: 21.11.2014 VASOV
     Modified: 
*/

{globals.i}
{intrface.get xclass}

DEFINE INPUT PARAMETER iHFiles AS HANDLE NO-UNDO.

DEFINE STREAM mSRep.


{setdest.i &stream = " STREAM mSRep " }

PUT STREAM mSRep UNFORMATTED
   "Ž’ŽŠŽ‹ ŠŽ’Ž‹Ÿ ‘ŽŽ™…ˆ‰ SWIFT „‹Ÿ Ž’€‚Šˆ ‚ ‘Ž‘’€‚… ED503"
   SKIP(1).

PUT STREAM mSRep UNFORMATTED
   "Ž’Ž€› „‹Ÿ ”ŽŒˆŽ‚€ˆŸ ED503:" SKIP(1).

RUN PutFileList IN THIS-PROCEDURE(SUBSTITUTE("FOR EACH &1 WHERE &1.__ID GT 0" + 
                                            " AND &1.BadErrors EQ '' " + 
                                             "NO-LOCK BY &1.bank-code-rec", 
                                              iHFiles:NAME)).

PUT STREAM mSRep UNFORMATTED
   "Ž’€ŠŽ‚€›:" SKIP(1).

RUN PutFileList IN THIS-PROCEDURE(SUBSTITUTE("FOR EACH &1 WHERE &1.__ID GT 0" +
                                            " AND &1.BadErrors GT '' " + 
                                             "NO-LOCK BY &1.bank-code-rec", 
                                              iHFiles:NAME)).

{preview.i &stream = " STREAM mSRep "}

PROCEDURE PutFileList:
   DEFINE INPUT PARAMETER iWhere AS CHARACTER NO-UNDO.

   DEFINE VARIABLE vHBuffer AS HANDLE    NO-UNDO.
   DEFINE VARIABLE vHQuery  AS HANDLE    NO-UNDO.
   DEFINE VARIABLE vI       AS INT64     NO-UNDO.
   DEFINE VARIABLE vErrCode AS CHARACTER NO-UNDO.

   DEFINE BUFFER xattr FOR xattr.

   CREATE BUFFER vHBuffer FOR TABLE iHFiles.
   CREATE QUERY vHQuery.
   vHQuery:ADD-BUFFER(vHBuffer).

   vHQuery:QUERY-PREPARE(iWhere).
   vHQuery:QUERY-OPEN().
   IF vHQuery:GET-FIRST() THEN DO:
      PUT STREAM mSRep UNFORMATTED
      "”€‰‹                 BIC SWIFT   ’ˆ …”……‘          ‘“ŒŒ€" SKIP
      "------------------------------------------------------------" SKIP
      .
      DO WHILE NOT vHQuery:QUERY-OFF-END :
         PUT STREAM mSRep UNFORMATTED
            STRING(vHBuffer::FName,         "X(20)") " "
            STRING(vHBuffer::bank-code-rec, "X(11)") " "
            STRING(vHBuffer::DType,   "X(3)" ) " "
            STRING(vHBuffer::Ref,     "X(12)") " "
            STRING(vHBuffer::amt-rub, ">>>>>>9.99") " "
            SKIP.
         DO vI = 1 TO NUM-ENTRIES(vHBuffer::ErrorList) :
            vErrCode = ENTRY(vI, vHBuffer::ErrorList).
            RUN GetXAttr IN h_xclass ("ErrorESID", vErrCode, BUFFER xattr).
            PUT STREAM mSRep UNFORMATTED
               STRING(vErrCode,"X(8)") " "
               STRING(xattr.Initial,"X(8)") " "
               xattr.Name SKIP
            .
         END.
         vHQuery:GET-NEXT().
      END.
      PUT STREAM mSRep UNFORMATTED SKIP(1).
   END.
   ELSE
      PUT STREAM mSRep UNFORMATTED
      "Ž’‘“’‘’‚“ž’" SKIP(1).
   vHQuery:QUERY-CLOSE().

   DELETE OBJECT vHQuery  NO-ERROR.
   DELETE OBJECT vHBuffer NO-ERROR.

END PROCEDURE.


/******************************************************************************/
/* $LINTUSER='VASOV' */
/* $LINTENV ='dpl' */
/* $LINTVSS ='$/ws3-dpl/bq' */
/* $LINTDATE='08/12/2014 12:07:15.859+04:00' */
/* $LINTFILE='ed503exp-rep.p' */
/*prosignupF2lLAC+LsjuYqzDC5U3A*/