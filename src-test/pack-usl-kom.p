/*
               ��� "���� ����"
     Filename: pack-usl-kom.p
      Comment: ��楤�� ���� �ॢ�襭�� �����ᨨ �� ����⠬ ���
   Parameters: 
      Used by: extrpars.fun
      Created: 21.10.2014 KAU   ��७�� �� extrpars.fun ��� �ᯮ�짮����� ⥬�-⠡�
     Modified: 
*/

{globals.i}
{pp-corr.p}

DEF INPUT PARAMETER vAcct   AS CHAR. /* �室�騩 ��ࠬ��� ��� ������ */
DEF INPUT PARAMETER vDate   AS DATE. /* �室�騩 ��ࠬ��� ��� ���भ� */
DEF INPUT PARAMETER vSumKS  AS DEC.  /* �室�騩 ��ࠬ��� �㬬� ���ᮢ�� ����樨 */
DEF INPUT-OUTPUT PARAMETER vKom AS CHAR.

DEF VAR vDateN      AS DATE NO-UNDO. /* ��� ��砫� ����� */
DEF VAR vCust-cat   AS CHAR NO-UNDO.
DEF VAR vCust-id    AS DEC  NO-UNDO.
DEF VAR vCust-OP    AS DATE NO-UNDO. /* ��� ��������� ������ */
DEF VAR vProd       AS CHAR NO-UNDO. /* �������� ����� ��� ������ */
DEF VAR vDateProd   AS DATE NO-UNDO. /* ��� ��砫� ����⢨� ����� ��� */
DEF VAR vDateX      AS DATE NO-UNDO.
DEF VAR vKDoc       AS DEC  NO-UNDO. /* ������⢮ ���㬥�⮢ �� ����� */  
DEF VAR vKDocT      AS DEC  NO-UNDO. /* ������⢮ ���㬥�⮢ �� ���थ�� */
DEF VAR vSumVn      AS DEC  NO-UNDO. /* �㬬� ���ᥭ�� �।�� �� ��� �� ����� */
DEF VAR vSumIz      AS DEC  NO-UNDO. /* �㬬� ������ �।�� �१ ����� �� ����� */
DEF VAR cNull       AS CHAR NO-UNDO. cNull = '�㫥���1'.



/*
DEF TEMP-TABLE tt-packUsl  NO-UNDO
    FIELD      namePack    AS CHAR
    FIELD      kolDoc      AS DEC
    FIELD      summVn      AS DEC
    FIELD      summIz      AS DEC
    FIELD      kolSpr      AS DEC
.
CREATE tt-packUsl.
ASSIGN tt-packUsl.namePack = "���⮢�+"
       tt-packUsl.kolDoc = 10  tt-packUsl.summVn = 0       tt-packUsl.summIz = 100000
.
CREATE tt-packUsl.
ASSIGN tt-packUsl.namePack = "������+"
       tt-packUsl.kolDoc = 20  tt-packUsl.summVn = 30000   tt-packUsl.summIz = 50000
.
CREATE tt-packUsl.
ASSIGN tt-packUsl.namePack = "��⨬����+"
       tt-packUsl.kolDoc = 40  tt-packUsl.summVn = 50000   tt-packUsl.summIz = 200000
.
CREATE tt-packUsl.
ASSIGN tt-packUsl.namePack = "�६��+"
       tt-packUsl.kolDoc = 80  tt-packUsl.summVn = 70000   tt-packUsl.summIz = 250000
.



/*==================== ��ன ���� ������, �� ���� ����� �� �����࠭���� ====================*/
IF vAcct EQ "40702810800000127648     @0000" THEN RETURN.   /* ��� "��ࠢ����� �������� "��ᯥ��" */
/*================================================================================================*/



vDateN = DATE( '01' + SUBSTRING(STRING(vDate),3) ).



vKDocT = 0.



IF vKom EQ 'K08TAR' OR vKom EQ 'K34TAR' OR vKom EQ 'K01TAR' OR vKom EQ 'K19TAR' OR vKom EQ '�������16' OR vKom EQ '�������᫥16' OR vKom EQ '���㬄�16' OR vKom EQ '���㬏�᫥16'
THEN RUN SetSysConf IN h_base ("�������������", STRING(vKDocT)).



FIND FIRST acct WHERE acct.acct BEGINS vAcct NO-LOCK NO-ERROR.
IF AVAIL acct THEN DO:
    vCust-cat = acct.cust-cat.
    vCust-id  = acct.cust-id.
END.
ELSE MESSAGE "��� �� ������ " vAcct VIEW-AS ALERT-BOX.



IF vCust-cat EQ '�' THEN DO: FIND FIRST cust-corp WHERE cust-corp.cust-id EQ vCust-id NO-LOCK NO-ERROR.
                             vCust-op = cust-corp.date-in.
                             FIND LAST tmpsigns WHERE tmpsigns.file-name EQ 'cust-corp'
                                                  AND tmpsigns.surrogate EQ STRING(vCust-id)
                                                  AND tmpsigns.code      EQ '�����'
                                                  AND tmpsigns.since     <  vDateN
                                  NO-LOCK NO-ERROR.
                                  IF AVAIL TMPSIGNS THEN DO: vProd     = tmpsigns.xattr-value.
                                                             vDateProd = tmpsigns.since.
                                                         END.
                                                    ELSE DO: FIND LAST tmpsigns WHERE tmpsigns.file-name EQ 'cust-corp'
                                                                                  AND tmpsigns.surrogate EQ STRING(vCust-id)
                                                                                  AND tmpsigns.code      EQ '�����'
                                                                                  AND tmpsigns.since     EQ vCust-op NO-LOCK NO-ERROR.
                                                             IF AVAIL tmpsigns THEN DO: vProd     = tmpsigns.xattr-value.
                                                                                        vDateProd = tmpsigns.since.
                                                                                    END.    
                                                                               ELSE DO: FIND FIRST tmpsigns WHERE tmpsigns.file-name EQ 'cust-corp'
                                                                                                              AND tmpsigns.surrogate EQ STRING(vCust-id)
                                                                                                              AND tmpsigns.code      EQ '����멏���' NO-LOCK NO-ERROR.
                                                                                        IF AVAIL tmpsigns THEN DO: vProd     = tmpsigns.xattr-value.
                                                                                                                   vDateProd = tmpsigns.since.
                                                                                                               END.      
                                                                                    END.
                                                         END.
                         END.



IF vCust-cat EQ '�' THEN DO: FIND FIRST person WHERE person.person-id EQ vCust-id NO-LOCK NO-ERROR.
                             vCust-op = person.date-in.
                             FIND LAST tmpsigns WHERE tmpsigns.file-name EQ 'person'
                                                  AND tmpsigns.surrogate EQ STRING(vCust-id)
                                                  AND tmpsigns.code      EQ '�����'
                                                  AND tmpsigns.since     <  vDateN
                                  NO-LOCK NO-ERROR.
                                  IF AVAIL TMPSIGNS THEN DO: vProd     = tmpsigns.xattr-value.
                                                             vDateProd = tmpsigns.since.
                                                         END.
                                                    ELSE DO: FIND LAST tmpsigns WHERE tmpsigns.file-name EQ 'person'
                                                                                  AND tmpsigns.surrogate EQ STRING(vCust-id)
                                                                                  AND tmpsigns.code      EQ '�����'
                                                                                  AND tmpsigns.since     EQ vCust-op NO-LOCK NO-ERROR.
                                                             IF AVAIL tmpsigns THEN DO: vProd     = tmpsigns.xattr-value.
                                                                                        vDateProd = tmpsigns.since.
                                                                                    END.    
                                                                               ELSE DO: FIND FIRST tmpsigns WHERE tmpsigns.file-name EQ 'person'
                                                                                                              AND tmpsigns.surrogate EQ STRING(vCust-id)
                                                                                                              AND tmpsigns.code      EQ '����멏���' NO-LOCK NO-ERROR.
                                                                                        IF AVAIL tmpsigns THEN DO: vProd     = tmpsigns.xattr-value.
                                                                                                                   vDateProd = tmpsigns.since.
                                                                                                               END.      
                                                                                    END.
                                                         END.
                         END.



/*============================================= action-date ===============================================*/
IF vKom EQ "VAcct" OR vKom EQ "VAcctB" OR vKom EQ "K27TAR"
THEN 
   DO:
      IF can-do("405*,406*,407*,40807*", vAcct)
         AND acct.currency = ""  
         AND GetXAttrValue("acct", vAcct + "," + acct.currency, "action-date") <> ""
         AND (
             (
                (INT(MONTH(DATE(GetXAttrValue("acct", vAcct + "," + acct.currency, "action-date")))) <> 11)
                AND
                (INT(MONTH(DATE(GetXAttrValue("acct", vAcct + "," + acct.currency, "action-date")))) <> 12)
                AND
                (INT(MONTH(DATE(TODAY))) < INT(MONTH(ADD-INTERVAL(DATE(GetXAttrValue("acct", vAcct + "," + acct.currency, "action-date")), +3, "months"))))
                AND
                (INT(YEAR(DATE(TODAY))) = INT(YEAR(DATE(GetXAttrValue("acct", vAcct + "," + acct.currency, "action-date")))))
             )
             OR
             (
                (
                   (INT(MONTH(DATE(GetXAttrValue("acct", vAcct + "," + acct.currency, "action-date")))) = 11)
                   OR
                   (INT(MONTH(DATE(GetXAttrValue("acct", vAcct + "," + acct.currency, "action-date")))) = 12)
                ) 
                AND
                (INT(MONTH(DATE(TODAY))) < INT(MONTH(ADD-INTERVAL(DATE(GetXAttrValue("acct", vAcct + "," + acct.currency, "action-date")), +3, "months"))))
                AND
                (INT(YEAR(DATE(TODAY))) = INT(YEAR(DATE(GetXAttrValue("acct", vAcct + "," + acct.currency, "action-date")))) + 1)
             )
             )
      THEN
         DO: 
            vKom = cNull.
            RETURN.
         END.
   END.
/*================================================= end ===================================================*/



/*========================================= ����� ��� ���⮢�+ ========================================*/
IF vKom EQ "VAcct" OR vKom EQ "VAcctB" OR vKom EQ "K27TAR"
THEN 
   DO:
      IF vProd NE "" 
      THEN
         DO:
            vDateX = date_correct(month(tmpsigns.since), 3, 1, year(tmpsigns.since)).

            IF tmpsigns.xattr-value = "���⮢�+"
               AND vDate < vDateX
               AND tmpsigns.since <= date("31/12/2016")
            THEN 
               DO:
                  vKom = cNull.
                  RETURN.
               END.
         END.
   END.
/*=================================================== end =================================================*/
*/



/*======================================== ����� ���� ����-�ࠩ� =========================================*/
FIND FIRST acct WHERE acct.acct BEGINS vAcct NO-LOCK NO-ERROR.
IF AVAIL acct THEN DO: vCust-cat = acct.cust-cat.
                       vCust-id  = acct.cust-id.
                   END.
              ELSE MESSAGE "��� �� ������ " vAcct VIEW-AS ALERT-BOX.

IF vCust-cat EQ '�' THEN DO: FIND LAST tmpsigns WHERE tmpsigns.file-name EQ 'cust-corp'
                                                  AND tmpsigns.surrogate EQ STRING(vCust-id)
                                                  AND tmpsigns.code      EQ '����멏���'
                                  NO-LOCK NO-ERROR.
                                  IF AVAIL TMPSIGNS THEN DO: vProd     = tmpsigns.xattr-value.
                                                             vDateProd = tmpsigns.since.
                                                         END.
                                                    ELSE RETURN.
                         END.

IF vCust-cat EQ '�' THEN DO: FIND LAST tmpsigns WHERE tmpsigns.file-name EQ 'person'
                                                  AND tmpsigns.surrogate EQ STRING(vCust-id)
                                                  AND tmpsigns.code      EQ '����멏���'
                                  NO-LOCK NO-ERROR.
                                  IF AVAIL TMPSIGNS THEN DO: vProd     = tmpsigns.xattr-value.
                                                             vDateProd = tmpsigns.since.
                                                         END.
                                                    ELSE RETURN.
                         END.

IF vProd EQ "����-�ࠩ�" AND (vKom EQ "�������16"    OR 
                              vKom EQ "�������᫥16" OR 
                              vKom EQ "VAcct"        OR 
                              vKom EQ "VAcctB"       OR 
                              vKom EQ "K27TAR")
THEN
     DO:
         IF today <= vDateProd + 90
         THEN DO:
                  vKom = cNull.
                  RETURN.
              END.

     END.
/*==================================================== end ==================================================*/



/*
FIND FIRST tt-packUsl WHERE tt-packUsl.namePack EQ vProd NO-ERROR.
IF NOT AVAIL tt-packUsl THEN DO:
   RETURN.
END.



/* �஢��塞 � ������ ��� ��砫� ����� �� ᮢ������ � ��⮩ ������ ��� + ������ ����ᠫ ������ � �⮬ �����, ⮣�� ��� �०���. */
IF     vDateProd NE vCust-op
   AND vDateProd >= vDateN
THEN DO:
   RETURN.
END.    



/*============================================ 1 ����⨥ ��� =============================================*/
IF vKom EQ 'OtkSch' 
THEN DO:
    vKom = cNull.
    RETURN.
END.    



/*============================================ 2 �������筮� ������� ��� ==================================*/
IF vKom EQ 'VAcct' OR vKom EQ 'VAcctB'
THEN DO:
    IF acct.currency = '' THEN DO: 
        vKom = cNull.
        RETURN.
    END.
    ELSE
        RETURN.
END.



/*============================================ 3 �������筮� ���㦨����� ������ ============================*/
IF vKom EQ 'K27TAR'
THEN DO:
    IF acct.currency = '' THEN DO: 
        vKom = cNull.
        RETURN.
    END.
    ELSE
        RETURN.
END.



/*============================================ 4 ����᫥��� ���. ��-� � ��� ============================*/
IF vKom EQ 'K08TAR' OR vKom EQ 'K34TAR' OR vKom EQ 'K01TAR' OR vKom EQ 'K19TAR'
THEN DO:

    /*===================== fev =====================*/
    IF vAcct BEGINS "40821" OR vAcct BEGINS "40701"
    THEN
    DO:
    FIND FIRST acct WHERE acct.acct BEGINS vAcct
                      AND can-do('���揀,���ப', acct.contract)
                    NO-LOCK NO-ERROR.
    IF AVAIL acct THEN RETURN. 
    END.
    /*===================== end =====================*/

    vKDoc = 0.
    FOR EACH acct WHERE acct.cust-cat = vCust-cat
                    AND acct.cust-id = vCust-id
                    AND acct.contract = '�����'
    NO-LOCK:
        FOR EACH op-entry WHERE op-entry.acct-db EQ acct.acct
                            AND can-do('�,��,���,���', op-entry.op-status)
                            AND op-entry.op-date >= vDateN 
                            AND op-entry.op-date <= vDate
                            AND can-do('30102*,30301*,30223*1', op-entry.acct-cr)
        NO-LOCK:
        FIND op WHERE op.op EQ op-entry.op NO-LOCK NO-ERROR.
        IF AVAIL op 
             AND op.doc-type BEGINS '01' 
             AND NOT op.ben-acct BEGINS '40101'
             AND NOT op.ben-acct BEGINS '40102'
             AND NOT op.ben-acct BEGINS '40201'
             AND NOT op.ben-acct BEGINS '40204'
             AND NOT op.ben-acct BEGINS '40402'
        THEN DO:
            FIND FIRST op-bank WHERE op-bank.op EQ op.op 
                                 AND op-bank.bank-code-type EQ '���-9'

                              /* AND op-bank.bank-code NE '045209783'
                                 AND op-bank.bank-code NE '047106641' */

                                 AND op-bank.bank-code NE '044525129'
                                 AND op-bank.bank-code NE '047106641'
                                 AND op-bank.bank-code NE '045209884'
            NO-LOCK NO-ERROR.
            IF NOT AVAIL op-bank THEN NEXT.
        END.
        ELSE NEXT.
            vKDoc = vKDoc + 1.
            IF op-entry.op-date EQ vDate THEN vKDocT = vKDocT + 1.
        END.

    END.

    IF vKDoc - vKDocT <= tt-packUsl.kolDoc
    THEN DO:
        IF vKDoc > tt-packUsl.kolDoc
        THEN DO:
            vKDocT = vKDoc - tt-packUsl.kolDoc.
        END.
        ELSE DO:
            vKDocT = 0.
            vKom = cNull.
        END.
        RUN SetSysConf IN h_base ("�������������",STRING(vKDocT)).
        RETURN.
    END.
    ELSE DO:
        MESSAGE "� ��� " vAcct SKIP
                "�ॢ�襭� ���ᨬ��쭮� ������⢮ ���㬥�⮢ �� ����" SKIP
                STRING(vKDoc) " > " STRING(tt-packUsl.kolDoc) VIEW-AS ALERT-BOX.
        RETURN.
    END.

END.



/*============================================ 5 ����᫥��� ���. ��-� � ��� ��� � ��� ===================*/
IF vKom EQ '�������16' OR vKom EQ '�������᫥16' OR vKom EQ '���㬄�16' OR vKom EQ '���㬏�᫥16'
THEN DO:

    vKDoc = 0.
    FOR EACH acct WHERE acct.cust-cat = vCust-cat
                    AND acct.cust-id = vCust-id
                    AND acct.contract = '�����'
    NO-LOCK:
        FOR EACH op-entry WHERE op-entry.acct-db EQ acct.acct
                            AND can-do('�,��,���,���,���,���,���', op-entry.op-status)
                            AND op-entry.op-date >= vDateN 
                            AND op-entry.op-date <= vDate
                            AND can-do('30102*,30301*,30223*1', op-entry.acct-cr)
        NO-LOCK:
        FIND op WHERE op.op EQ op-entry.op NO-LOCK NO-ERROR.
        IF AVAIL op 
             AND op.doc-type BEGINS '01' 
             AND can-do('!40101*,!40102*,!40201*,!40204*,!40402*,*', op.ben-acct)
        THEN DO:
            FIND FIRST op-bank WHERE op-bank.op EQ op.op 
                                 AND op-bank.bank-code-type EQ '���-9'
                                 AND can-do('!044525129,!047106641,!045209884,!"",*', op-bank.bank-code)                        
            NO-LOCK NO-ERROR.
            IF NOT AVAIL op-bank THEN NEXT.
        END.
        ELSE NEXT.
            vKDoc = vKDoc + 1.
            IF op-entry.op-date EQ vDate THEN vKDocT = vKDocT + 1.
        END.

    END.

    vKDoc = vKDoc + 1.

    /* message string(vKDoc) view-as alert-box. */

    IF vKDoc - vKDocT <= tt-packUsl.kolDoc
    THEN 
       DO:
          IF vKDoc > tt-packUsl.kolDoc
             THEN 
                DO:
                   vKDocT = vKDoc - tt-packUsl.kolDoc.
                   MESSAGE "� ��� " vAcct SKIP
                           "�ॢ�襭� ���ᨬ��쭮� ������⢮ ���㬥�⮢ �� ����" SKIP
                           STRING(vKDoc) " > " STRING(tt-packUsl.kolDoc) VIEW-AS ALERT-BOX.

                END.
             ELSE 
                DO:
                   vKDocT = 0.
                   vKom = cNull.
                   RUN SetSysConf IN h_base ("fflag", 0).
                END.
          RUN SetSysConf IN h_base ("�������������", STRING(vKDocT)).
          RETURN.
       END.
    ELSE 
       DO:
          MESSAGE "� ��� " vAcct SKIP
                  "�ॢ�襭� ���ᨬ��쭮� ������⢮ ���㬥�⮢ �� ����" SKIP
                  STRING(vKDoc) " > " STRING(tt-packUsl.kolDoc) VIEW-AS ALERT-BOX.
          RETURN.
       END.
END.



/*============================================ 6 �ਥ�, ������ � ���᫥��� �� ����� ��� ���. ���. ��-� ===*/
IF (vKom BEGINS 'PrZachUr') AND (vProd <> "���⮢�+")
THEN DO:

    /*===================== fev =====================*/
    IF vAcct BEGINS "40821" OR vAcct BEGINS "40701"
    THEN
    DO:
    FIND FIRST acct WHERE acct.acct BEGINS vAcct
                      AND can-do('���揀,���ப', acct.contract)
                    NO-LOCK NO-ERROR.
    IF AVAIL acct THEN RETURN. 
    END.
    /*===================== end =====================*/

    vSumVn = 0.
    FOR EACH acct WHERE acct.cust-cat = vCust-cat
            AND acct.cust-id = vCust-id
            AND acct.contract = '�����'
    NO-LOCK:
        FOR EACH op-entry WHERE op-entry.acct-cr EQ acct.acct
                AND op-entry.acct-db BEGINS '20202'
                AND op-entry.op-date >= vDateN 
                AND op-entry.op-date <= vDate
        NO-LOCK:
            MESSAGE op-entry.op-date " " op.doc-num VIEW-AS ALERT-BOX.
            vSumVn = vSumVn + op-entry.amt-rub.
        END.
    END.
    
    DEF VAR vOverVn AS DEC INIT 0.00 NO-UNDO.

    IF tt-packUsl.summVn >= vSumVn + vSumKS THEN DO:    /* �᫨ �㬬� ���ᥭ�� �� ������ �� ���௠�� */
        vKom = cNull.
        MESSAGE "����� ��� ��� " tt-packUsl.namePack " �� ���௠�." SKIP
                "������� 0 �㡫��." VIEW-AS ALERT-BOX.
    END.
    ELSE
        IF tt-packUsl.summVn <= vSumVn THEN           /* �᫨ �㬬� ���ᥭ�� �� ������ ���௠�� */
            MESSAGE "����� ��� ��� " tt-packUsl.namePack " ���௠�." SKIP
                    "������� � ᮮ⢥��⢨� � ��䠬� �����." VIEW-AS ALERT-BOX.
        ELSE DO:                                      /* �᫨ �㬬� ���ᥭ�� �� ������ ���௠�� ���筮 */
            vOverVn = vSumKS + vSumVn - tt-packUsl.summVn.
            MESSAGE "����� ���ᥭ�� ��� ��� " tt-packUsl.namePack " ���௠�." SKIP
                    "��� " vOverVn " ��. �㤥� ᯨᠭ� �������." VIEW-AS ALERT-BOX.
        END.

    RUN SetSysConf IN h_base ("��ᢥ�寠�",STRING(vOverVn)).
    RETURN.
    
END.



/*============================================ 7 �뤠� 祪.������ 50 ���⮢ ================================*/
IF vKom BEGINS '�����50'
THEN DO:
    MESSAGE '�뤠������ �� ������� 祪���� ������(50 �.)'
            SKIP(1)
            ' �� �६� ����⢨� �����?'
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
        UPDATE chs AS LOGICAL.
    IF chs THEN
        RETURN.
    ELSE DO:
        vKom = cNull.
        RETURN.
    END.
    /*vKom = cNull.
    MESSAGE '������� ��� ��� ' + vAcct + ' ����祭� � ��� ' + vProd VIEW-AS ALERT-BOX.
    RETURN.*/
END.



/*============================================ 8 �뤠� �㡫���⮢ �믨᮪ ==================================*/
IF (vKom EQ 'DublOD' OR vKom EQ 'DublZD') AND (vProd EQ '��⨬����+' OR vProd EQ '�६��+')
THEN DO:
        vKom = cNull.
        MESSAGE '������� ��� ��� ' + vAcct + ' ����祭� � ��� ' + vProd VIEW-AS ALERT-BOX.
        RETURN.
END.



/*============================================ 9 ��㣠 SMS =================================================*/
IF vKom EQ 'SMS' AND (vProd EQ '�६��+' OR vProd EQ '��⨬����+')
THEN DO:
    vKom = cNull.
    RETURN.
END.
*/