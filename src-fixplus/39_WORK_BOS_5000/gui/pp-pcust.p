{globals.i}

/* +++ pp-pcust.p was humbly modified by (c)blodd converter v.1.09 on 8/10/2016 10:03am +++ */

/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2015 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: pp-pcust.p
      Comment: ������⥪� ������� �㭪権 ��� ࠡ��� � �����⠬� � ����� �࠭������
   Parameters: ���
         Uses:
      Used by:
      Created: 05.10.2007 16:01 ariz    
     Modified: 05.10.2007 16:01 ariz     <comment>
*/

{globals.i}             /* �������� ��६���� ��ᨨ. */
{intrface.get cust}     /* ������⥪� ��� ࠡ��� � �����⠬�. */
{intrface.get xclass}   /* ������⥪� �����㬥�⮢ ����奬�. */
{intrface.get tmess}    /* �����㬥��� ��ࠡ�⪨ ᮮ�饭��. */
{intrface.get instrum} 

{pfuncdef
   &LIBDEF        = "YES"
   &NAME          = "PCUST"
   &LIBNAME       = "������⥪� ������� �㭪権 ��� ࠡ��� � �����⠬�"
   &DESCRIPTION   = "������⥪� ������� �㭪権 ��� ࠡ��� � �����⠬� � ����� �࠭������"
   }

{g-pfunc.def}

{pfuncdef
   &NAME          = "������_�����"
   &DESCRIPTION   = "�஢����, ���� �� ��ꥪ� - �����⮬ �����."
   &PARAMETERS    = "��� ��ꥪ�,��� ��ꥪ�"
   &RESULT        = "�� - ������, ��� - �� ������"
   &SAMPLE        = "������_�����('�',123) = ��"
   }
   DEFINE INPUT  PARAMETER iCustCat   AS CHAR    NO-UNDO.
   DEFINE INPUT  PARAMETER iCustId    AS INT64     NO-UNDO.
   DEFINE OUTPUT PARAMETER out_Result AS CHAR    NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok      AS INT64 NO-UNDO.

   {pchkpar iCustCat iCustId}

   out_Result = STRING(IsSubjClient(iCustCat,iCustId),"��/���").

   RETURN out_Result.
END PROCEDURE.

{pfuncdef
   &NAME          = "�����_�������"
   &DESCRIPTION   = "�����頥� ���� ������."
   &PARAMETERS    = "��� ��ꥪ�,��� ��ꥪ�"
   &RESULT        = "���ካ����"
   &SAMPLE        = "�����_�������('�',123)"
   }
   DEFINE INPUT  PARAMETER iCustCat   AS CHAR    NO-UNDO.
   DEFINE INPUT  PARAMETER iCustId    AS INT64     NO-UNDO.
   DEFINE OUTPUT PARAMETER out_Result AS CHAR    NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok      AS INT64 NO-UNDO.
   
   DEFINE VARIABLE vAddr AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vINN  AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vKPP  AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vType AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vCode AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vAcct AS CHARACTER   NO-UNDO.
   
   {pchkpar iCustCat iCustId}
   GetCliName(iCustCat,
              STRING(iCustId),
              OUTPUT vAddr,
              OUTPUT vINN,
              OUTPUT vKPP,
              INPUT-OUTPUT vType,
              OUTPUT vCode,
              OUTPUT vAcct).
   out_Result = SUBSTRING(vAddr,1,INDEX(vAddr,",,") - 1) + ","
              + GetCodeName ("���������",
                             GetXattrValueEx ("cust-corp", 
                                              STRING(iCustId), 
                                              "���������", 
                                              ""
                                              )
                           ) + "," 
              + SUBSTRING(vAddr,INDEX(vAddr,",,") + 2,LENGTH(vAddr)).
   RETURN out_Result.
END PROCEDURE.

{pfuncdef
   &NAME          = "���_�������"
   &DESCRIPTION   = "�����頥� ��� ������."
   &PARAMETERS    = "��� ��ꥪ�,��� ��ꥪ�"
   &RESULT        = "009012893712300"
   &SAMPLE        = "���_�������('�',123)"
   }
   DEFINE INPUT  PARAMETER iCustCat   AS CHAR  NO-UNDO.
   DEFINE INPUT  PARAMETER iCustId    AS INT   NO-UNDO.
   DEFINE OUTPUT PARAMETER out_Result AS CHAR  NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok      AS INT64 NO-UNDO.
   
   DEFINE VARIABLE vAddr AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vINN  AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vKPP  AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vType AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vCode AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vAcct AS CHARACTER   NO-UNDO.

   {pchkpar iCustCat iCustId}
   GetCliName(iCustCat,
              STRING(iCustId),
              OUTPUT vAddr,
              OUTPUT vINN,
              OUTPUT vKPP,
              INPUT-OUTPUT vType,
              OUTPUT vCode,
              OUTPUT vAcct).
   out_Result = vINN.

   RETURN out_Result.
END PROCEDURE.

{pfuncdef
   &NAME          = "���_�������"
   &DESCRIPTION   = "�����頥� ��� ������."
   &PARAMETERS    = "��� ��ꥪ�,��� ��ꥪ�"
   &RESULT        = "092384123"
   &SAMPLE        = "���_�������('�',123)"
   }
   DEFINE INPUT  PARAMETER iCustCat   AS CHAR  NO-UNDO.
   DEFINE INPUT  PARAMETER iCustId    AS INT   NO-UNDO.
   DEFINE OUTPUT PARAMETER out_Result AS CHAR  NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok      AS INT64 NO-UNDO.
   
   DEFINE VARIABLE vAddr AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vINN  AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vKPP  AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vType AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vCode AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vAcct AS CHARACTER   NO-UNDO.

   {pchkpar iCustCat iCustId}
   GetCliName(iCustCat,
              STRING(iCustId),
              OUTPUT vAddr,
              OUTPUT vINN,
              OUTPUT vKPP,
              INPUT-OUTPUT vType,
              OUTPUT vCode,
              OUTPUT vAcct).
   out_Result = vKPP.

   RETURN out_Result.
END PROCEDURE.

{pfuncdef
   &NAME          = "���_��������������_�������"
   &DESCRIPTION   = "�����頥� ⨯ �����䨪��� ������"
   &PARAMETERS    = "��� ��ꥪ�,��� ��ꥪ�"
   &RESULT        = "REGN"
   &SAMPLE        = "���_��������������_�������('�',123)"
   }
   DEFINE INPUT  PARAMETER iCustCat   AS CHAR  NO-UNDO.
   DEFINE INPUT  PARAMETER iCustId    AS INT   NO-UNDO.
   DEFINE OUTPUT PARAMETER out_Result AS CHAR  NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok      AS INT64 NO-UNDO.
   
   DEFINE VARIABLE vAddr AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vINN  AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vKPP  AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vType AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vCode AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vAcct AS CHARACTER   NO-UNDO.

   {pchkpar iCustCat iCustId}
   GetCliName(iCustCat,
              STRING(iCustId),
              OUTPUT vAddr,
              OUTPUT vINN,
              OUTPUT vKPP,
              INPUT-OUTPUT vType,
              OUTPUT vCode,
              OUTPUT vAcct).
   out_Result = IF {assigned vType} THEN vType
                                    ELSE "".
   RETURN out_Result.
END PROCEDURE.

{pfuncdef
   &NAME          = "���_��������������_�������"
   &DESCRIPTION   = "�����頥� ��� �����䨪��� ������"
   &PARAMETERS    = "��� ��ꥪ�,��� ��ꥪ�"
   &RESULT        = "000000001"
   &SAMPLE        = "���_��������������_�������('�',123)"
   }
   DEFINE INPUT  PARAMETER iCustCat   AS CHAR  NO-UNDO.
   DEFINE INPUT  PARAMETER iCustId    AS INT   NO-UNDO.
   DEFINE OUTPUT PARAMETER out_Result AS CHAR  NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok      AS INT64 NO-UNDO.
   
   DEFINE VARIABLE vAddr AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vINN  AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vKPP  AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vType AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vCode AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vAcct AS CHARACTER   NO-UNDO.

   {pchkpar iCustCat iCustId}
   GetCliName(iCustCat,
              STRING(iCustId),
              OUTPUT vAddr,
              OUTPUT vINN,
              OUTPUT vKPP,
              INPUT-OUTPUT vType,
              OUTPUT vCode,
              OUTPUT vAcct).
   out_Result = IF {assigned vCode} THEN vCode
                                    ELSE "".
   RETURN out_Result.
END PROCEDURE.

{pfuncdef
   &NAME          = "�������_�������"
   &DESCRIPTION   = "�����頥� ������ ������"
   &PARAMETERS    = "��� ��ꥪ�,��� ��ꥪ�"
   &RESULT        = "30102810000000161456"
   &SAMPLE        = "�������_�������('�',123)"
   }
   DEFINE INPUT  PARAMETER iCustCat   AS CHAR  NO-UNDO.
   DEFINE INPUT  PARAMETER iCustId    AS INT   NO-UNDO.
   DEFINE OUTPUT PARAMETER out_Result AS CHAR  NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok      AS INT64 NO-UNDO.
   
   DEFINE VARIABLE vAddr AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vINN  AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vKPP  AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vType AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vCode AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vAcct AS CHARACTER   NO-UNDO.

   {pchkpar iCustCat iCustId}
   GetCliName(iCustCat,
              STRING(iCustId),
              OUTPUT vAddr,
              OUTPUT vINN,
              OUTPUT vKPP,
              INPUT-OUTPUT vType,
              OUTPUT vCode,
              OUTPUT vAcct).
   out_Result = IF {assigned vAcct} THEN vAcct
                                    ELSE "".
   RETURN out_Result.
END PROCEDURE.


{pfuncdef
   &NAME          = "�������_������������_�������"
   &DESCRIPTION   = "�����頥� ��⪮� ������������ ������"
   &PARAMETERS    = "��� ��ꥪ�,��� ��ꥪ�"
   &RESULT        = "������ ���� ��������"
   &SAMPLE        = "�������_������������_�������('�',123)"
   }
   DEFINE INPUT  PARAMETER iCustCat   AS CHAR  NO-UNDO.
   DEFINE INPUT  PARAMETER iCustId    AS INT   NO-UNDO.
   DEFINE OUTPUT PARAMETER out_Result AS CHAR  NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok      AS INT64 NO-UNDO.
   
   DEFINE VARIABLE vName AS CHARACTER   NO-UNDO.

   RUN GetCustNameShort IN h_base(iCustCat, iCustId, OUTPUT vName).
   
   out_Result = IF {assigned vName} THEN vName
                                    ELSE "".
   RETURN out_Result.
END PROCEDURE.

{pfuncdef
   &NAME          = "���_������_����"
   &DESCRIPTION   = "�����頥� ��⥣��� ������ �� ����"
   &PARAMETERS    = "���[,���᮪ ���樠権 <��⥣���:��᪠ ��>]"
   &RESULT        = "�"
   &SAMPLE        = "���_������_����('40817810000020000009'[,'�:30109,30110;�:40802;�:40817,40820;�:40702'])"
   }

   DEFINE INPUT  PARAMETER iAcct      AS CHAR  NO-UNDO.
   DEFINE INPUT  PARAMETER iParam     AS CHAR   NO-UNDO.
   DEFINE OUTPUT PARAMETER oUt_Result AS CHAR  NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok      AS INT64 NO-UNDO.

   DEFINE BUFFER   bAcct FOR acct.
   {pchkpar iAcct}

   {find-act.i
      &bact   = bAcct
      &acct   = iAcct}
      
   IF AVAIL bAcct THEN
      oUt_Result = GetCatOnAcct (STRING(bAcct.bal-acct),iParam,bAcct.cust-cat).

   RETURN oUt_Result.
END PROCEDURE.

{pfuncdef
   &NAME          = "����������"
   &DESCRIPTION   = "��室�� � 㪠������ ���भ� � ⥪�饬 䨫���� (�᫨ <iPoisk> = ������)~~n~
��� �� �ᥬ 䨫����� (�᫨ <iPoisk> = ����) ���㬥��� ��ॢ���� ��� � �஢����� 20202* - 409*,~~n~ 
����� ᤥ��� 㪠����� ������.  �᫨ � �㭪�� ��।��� ��ࠬ��� <iCustId>, � ��� ���᪠ ~~n~
��ॢ���� �㤥� �ᯮ�짮���� �� ��� 㤮�⮢�७�� ��筮��. �᫨ � �㭪�� ��।��� ��ࠬ���� ~~n~
<iCli>,<iNum>,<iName>, � �ᯮ��㥬 ⮫쪮 �� ���祭�� ��� ���᪠. ���⢥��⢨� ४����⮢: ~~n~
�� document-id ���㬥��- ��� ���㬥�� (��ࠬ��� iCli), �� ���� ���㬥�� -  ��� � ����� ~~n~
���㬥�� (��ࠬ��� iNum), �� ��� ���㬥�� -  ��� ������ (��ࠬ��� iName)."
   &PARAMETERS    = "����� ������,��� ���㬥��,����� ���㬥�� (��� � �����),��� ������,~~n~
����� ��ॢ���,����� ���ᨬ��쭮� �㬬� ��ॢ���,���ᨬ��쭠� �㬬� ��ॢ���,~~n~
�᪠�� �� �ᥬ 䨫����� ����� (���祭�� ����) ��� ⮫쪮 �� ⥪�饬� (������),��� �/�" 
   &RESULT      = "��������� �㬬� ��ॢ��� ������ � ����� �� ��ࠬ��� iVal, �.�. �� ���祭��~~n~ 
�� ��ࠬ��� <iMaxSumm> � ����� ��ࠬ��� iMaxVal ���⠥� ��������� �㬬� ��ॢ����."
   &SAMPLE      = "����������(?,'��ᯮ��','40 04 425374','���ᨬ�� ���� ����������','','',~~n~
10000,������,����()) - ���� ��ॢ����  �� ��������� 㤮�⮢�७�� ��筮��, ����. �㬬� 10 000 ~~n~  
�㡫��, �����頥� १���� � �㡫��.~~n~
����������(1299,'','','','840','840',100,������,����()) - ���� ��ॢ���� 㪠������� 䨧��᪮��~~n~ 
��� �� �ᥬ ��� 㤮�⮢�७�� ��筮��, ����. �㬬� ��ॢ��� 100  �����஢, �����頥�~~n~
१���� � �������."
   }

   DEFINE INPUT  PARAMETER iCustId     AS INT64       NO-UNDO.
   DEFINE INPUT  PARAMETER iCli        AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER iNum        AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER iName       AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER iVal        AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER iMaxVal     AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER iMaxSumm    AS DECIMAL     NO-UNDO.
   DEFINE INPUT  PARAMETER iPoisk      AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER iDate       AS DATE        NO-UNDO.
   DEFINE OUTPUT PARAMETER out_Result  AS DECIMAL     NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok       AS INTEGER     NO-UNDO.

   DEFINE VARIABLE         vSum        AS DECIMAL     NO-UNDO.
   DEFINE VARIABLE         vFIOCl      AS CHARACTER EXTENT 3 NO-UNDO.
   DEFINE VARIABLE         vTmpStr     AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE         vTmpStr1    AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE         vDokIdOk    AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE         vDokOk      AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE         vFIOOk      AS LOGICAL     NO-UNDO.
 
   DEFINE BUFFER xsigns     FOR signs.
   DEFINE BUFFER ysigns     FOR signs.
   DEFINE BUFFER zsigns     FOR signs.
   DEFINE BUFFER cust-ident FOR cust-ident.
   DEFINE BUFFER op         FOR op.
   DEFINE BUFFER op-entry   FOR op-entry.

   vSum = 0.

   loop_op :
   FOR EACH op NO-LOCK
      WHERE op.op-date   EQ iDate
        AND ( op.filial-id EQ shFilial OR
              iPoisk NE "������" ),
      FIRST op-entry OF op NO-LOCK 
      WHERE op-entry.acct-db  BEGINS "20202"
        AND (op-entry.acct-cr  BEGINS "40912" 
        OR op-entry.acct-cr  BEGINS "40913")
	:

      IF iCustId NE ? AND 
         iCustId NE 0 THEN
      FOR EACH cust-ident NO-LOCK
         WHERE ( cust-ident.close-date EQ ? OR 
                 cust-ident.close-date GE iDate )         
           AND cust-ident.class-code EQ 'p-cust-ident'                               
           AND cust-ident.cust-cat   EQ '�'                                            
           AND cust-ident.cust-id    EQ iCustId :

         RUN GetCustName IN h_base (cust-ident.cust-cat,
                                    cust-ident.cust-id,
                                    ?,
                                    OUTPUT       vFIOCl[1],
                                    OUTPUT       vFIOCl[2],
                                    INPUT-OUTPUT vFIOCl[3]).
         ASSIGN
            vTmpStr  = TRIM(cust-ident.cust-code-type)
            vDokIdOk = NO
         .
         loop1:
         FOR EACH xsigns NO-LOCK
            WHERE xsigns.file-name   EQ "op"
              AND xsigns.surrogate   EQ STRING(op.op)
              AND xsigns.code        EQ "document-id" :
            vTmpStr1 = TRIM(IF {assigned xsigns.code-value}
                            THEN xsigns.code-value
                            ELSE xsigns.xattr-value).
            IF vTmpStr1 EQ vTmpStr THEN
            DO:
               vDokIdOk = YES.
               LEAVE loop1.
            END. 
         END.

         ASSIGN
            vTmpStr = TRIM(REPLACE(cust-ident.cust-code," ",""))
            vDokOk  = NO
         .
         loop2:
         FOR EACH ysigns NO-LOCK
            WHERE ysigns.file-name   EQ "op"
              AND ysigns.surrogate   EQ STRING(op.op)
              AND ysigns.code        EQ "����" :
            vTmpStr1 = TRIM(REPLACE(IF {assigned ysigns.code-value}
                                    THEN ysigns.code-value
                                    ELSE ysigns.xattr-value," ","")).
            IF vTmpStr1 BEGINS vTmpStr THEN
            DO:
               vDokOk = YES.
               LEAVE loop2.
            END.
         END.

         ASSIGN
            vTmpStr = TRIM(CAPS(vFIOCl[1] + " " + vFIOCl[2]))
            vFIOOk  = NO
         .
         loop3:
         FOR EACH zsigns NO-LOCK
            WHERE zsigns.file-name   EQ "op"
              AND zsigns.surrogate   EQ STRING(op.op)
              AND zsigns.code        EQ "���" :
            vTmpStr1 = TRIM(CAPS(IF {assigned zsigns.code-value}
                                 THEN zsigns.code-value
                                 ELSE zsigns.xattr-value)).
            IF vTmpStr1 EQ vTmpStr THEN
            DO:
               vFIOOk = YES.
               LEAVE loop3.
            END.
         END.

         IF vDokIdOk AND 
            vDokOk   AND
            vFIOOk   THEN
         DO:
            vSum = vSum + op-entry.amt-rub.
            NEXT loop_op.
         END.
      END.

      ELSE
      DO:
         ASSIGN
            vTmpStr  = TRIM(iCli)
            vDokIdOk = NO
         .
         IF {assigned vTmpStr} THEN
         loop4:
         FOR EACH xsigns NO-LOCK
            WHERE xsigns.file-name   EQ "op"
              AND xsigns.surrogate   EQ STRING(op.op)
              AND xsigns.code        EQ "document-id" :
            vTmpStr1 = TRIM(IF {assigned xsigns.code-value}
                            THEN xsigns.code-value
                            ELSE xsigns.xattr-value).
            IF vTmpStr1 EQ vTmpStr THEN
            DO:
               vDokIdOk = YES.
               LEAVE loop4.
            END. 
         END.

         ASSIGN
            vTmpStr = TRIM(REPLACE(iNum," ",""))
            vDokOk  = NO
         .
         IF {assigned vTmpStr} THEN
         loop5:
         FOR EACH ysigns NO-LOCK
            WHERE ysigns.file-name   EQ "op"
              AND ysigns.surrogate   EQ STRING(op.op)
              AND ysigns.code        EQ "����" :
            vTmpStr1 = TRIM(REPLACE(IF {assigned ysigns.code-value}
                                    THEN ysigns.code-value
                                    ELSE ysigns.xattr-value," ","")).
            IF vTmpStr1 BEGINS vTmpStr THEN
            DO:
               vDokOk = YES.
               LEAVE loop5.
            END.
         END.

         ASSIGN
            vTmpStr = TRIM(CAPS(iName))
            vFIOOk  = NO
         .
         IF {assigned vTmpStr} THEN
         loop6:
         FOR EACH zsigns NO-LOCK
            WHERE zsigns.file-name   EQ "op"
              AND zsigns.surrogate   EQ STRING(op.op)
              AND zsigns.code        EQ "���" :
            vTmpStr1 = TRIM(CAPS(IF {assigned zsigns.code-value}
                                 THEN zsigns.code-value
                                 ELSE zsigns.xattr-value)).
            IF vTmpStr1 EQ vTmpStr THEN
            DO:
               vFIOOk = YES.
               LEAVE loop6.
            END.
         END.
         ELSE vFIOOk = YES.

         IF vDokIdOk AND 
            vDokOk   AND
            vFIOOk   THEN
         vSum = vSum + op-entry.amt-rub.
      END.

   END.
                                    
   ASSIGN
      vSum = iMaxSumm - CurToCurWork("�������","",iMaxVal,iDate,vSum)
      vSum = CurToCurWork("�������",iMaxVal,iVal,iDate,vSum) 
   NO-ERROR.                                                                

   ASSIGN
      out_Result = vSum
      is-ok      = 1
   .
END PROCEDURE.

/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='02/11/2015 16:35:12.518+04:00' */
/* $LINTUSER='koms' */
/* $LINTMODE='1' */
/* $LINTFILE='pp-pcust.p' */
/*prosignbzWT61sg74lGQfvsvaocsg*/
/* --- pp-pcust.p was humbly modified by (c)blodd converter v.1.09 on 8/10/2016 10:03am --- */
