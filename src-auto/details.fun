/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2001 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: details.fun
      Comment:

  �᫨ �� �訫� ������ �㭪樮���쭮��� �����, � ������ �  ���� �⮣�
  䠩�� ��楤��� � ������ "���⮩������" � �������ਥ� ��। ���. ��������
  � �, � ��㣮� � ���� ᥡ� �� ���஢�,  �� �� ������ �ப������஢���,
  �� �㭪�� ������ � ��� �� ��뢠��.
                                                                Peter I. Bach.
   Parameters:
         Uses:
      Used by:
      Created: ...
     Modified: 03.12.2001 12:03 SEMA     �� ��� 0003724 ��������� �㭪�� ������� - �����頥� ������������ ���
     Modified: 05.12.2001 18:00 SEMA
     Modified: 05.12.2001 19:29 SEMA
     Modified: 20.02.2002 18:21 SEMA     �� ��� 0004870 ���ࠢ���� �㭪樨 ������ � ��⠎�
     Modified: 13.03.2002 11:42 SEMA     �� ��� 0006430 ���ꥬ 䨪� - ᮧ���� �㭪�� ������
     Modified: 13.03.2002 mitr           �� ��� 0006472 ���ꥬ 䨪� - �訡�� � ������, ���᪠, ��⠎�, ����
     Modified: 10.06.2002 18:30 SEMA     �� ��� 0007971 �����஥�� �㭪樨 ������, ��������, GetDetails, NDoc,
                                         Client, NClDoc, NClDocType, NDeposit, DocDate, ������2, DocCourse, ������,
                                         ������, ������ � 楫�� 㬥��襭�� ����
     Modified: 28.06.2002 12:49 SEMA     �� ��� 0007971 �ᮢ��襭�⢮����� ����
     Modified: 30.10.2002 12:21 SEMA     �� ��� 0011574 �㭪樮��� �.������ �࠭ � �᭮���� ������⥪� base-pp
     Modified: 11.11.2002 11:05 SEMA     �� ��� 0011116 � �⤥��� 䠩� �뭥ᥭ� ��楤��� ��� ࠡ��� � ⠡��楩
                                         frm-fields, �㭪樨 ������ � NDeposit ��७�ᥭ� � 䠩� parsacct.def
     Modified: 11.11.2002 15:23 SEMA     �� ��� 0011932 ���ꥬ ��� 0011116
     Modified: 28/04/2004 abko           �� 26319 �㭪�� "���, ����� �뤠� ���㬥�� 䨧���"
     Modified: 17.09.2004 16:00 laav     ��� 35056. ��ࠡ�⠭� �㭪樮���쭮��� ����� ������(). ��������� �����������
                                         ࠡ��� ����� ��� �뢮�� ��㧠 㪠������� �����䨪��� �� �࠭. ��� �⮣�
                                         ����室��� ��࠭��� ���祭�� ���� �����䨪��� 㪠���� � ����⢥ �⢥�⮣�
                                         �室���� ��ࠬ���.
     Modified: 23.05.2005 18:00 fedm     �� ��� 40596 �㭪�� ������ ��७�ᥭ�
                                         �� DETAILS.FUN � parsacct.def
     Modified: 23.01.2006 15:02 koav
     Modified: 30.01.2006 18:15 ZIAL     (37171) ����ୠ� �㭪�� ����᪠()
                                         ������㯭� ��� ���� �㬬� 蠡�����
                                         �࠭���権
     Modified: 31.01.2006 18:15 ZIAL     (37171) ����ୠ� �㭪�� ����᪠()
                                         ������㯭� ��� ���� �㬬� 蠡�����
                                         �࠭���権
     Modified: 10.05.2006 15:11 DEMO     ���  0050064. �������� ��楤��   �������
     Modified: 19.05.2006 17:25 MUTA     ���  0050064. �������� ��楤��   �������
     Modified: 02/11/2007 kraw (0083151) �����ࠫ�� ���ᮢ� ᨬ����.
     Modified: 21/07/2010 kraw (0090456) GetDetails2, DocDate2, NDoc2
     Modified: 22/12/2010 ches (0136028)  v svaznom   GetSysConf ("in-op-date")   = gend-date uchest pri pod'eme

*/

{intrface.get loan}
{intrface.get i254}
{intrface.get tmcod}
{intrface.get ovl}

PROCEDURE ����������:
    DEF OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* �ᯥ譮 �� �믮����� ? */
    DEFINE VARIABLE vProxyNumb   AS CHAR NO-UNDO.
   /* --- */
      is-ok = FALSE.
      IF NOT Pars-ValidParam(0) THEN RETURN.
      vProxyNumb = GetSysConf("ProxyPickVal"). 
      FIND FIRST loan WHERE loan.cont-code EQ vProxyNumb
                      AND   loan.contract  EQ "proxy"
                      NO-LOCK NO-ERROR.
         IF AVAIL loan THEN
            vProxyNumb = loan.doc-num.
               
  RUN Pars-SetCHARResult(vProxyNumb).

   /* --- */
   is-ok = TRUE.
 END PROCEDURE.

/*  **************************************************************************** */
   PROCEDURE ����७�����:
       DEF OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* �ᯥ譮 �� �믮����� ? */
       DEFINE VARIABLE vDov   AS CHAR   NO-UNDO.
      /* --- */
      is-ok = FALSE.
      IF NOT Pars-ValidParam(0) THEN RETURN.
      RUN GetLoan.
      IF NOT AVAIL loan THEN
        vDov = "NO".


      DEF BUFFER xloan FOR loan.

      FIND FIRST xloan WHERE xloan.contract  EQ loan.contract
                         AND xloan.cont-code EQ loan.cont-code NO-LOCK NO-ERROR.

      FIND FIRST loan WHERE loan.cust-id     EQ xloan.cust-id
                      AND loan.cust-cat    EQ xloan.cust-cat
                      AND loan.contract    EQ "proxy"
                      NO-LOCK NO-ERROR.
        IF  AVAIL loan THEN
            vDov = "YES".
            ELSE vDov = "NO".

      RUN Pars-SetCHARResult(vDov).
   /* --- */
       is-ok = TRUE.
END PROCEDURE.
/*  **************************************************************************** */

PROCEDURE ��⠄����:
    DEF OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* �ᯥ譮 �� �믮����� ? */
    DEFINE VARIABLE vData   AS CHAR   NO-UNDO.
    DEF VAR vProxyVal AS CHAR NO-UNDO.
   /* --- */
      is-ok = FALSE.
      IF NOT Pars-ValidParam(0) THEN RETURN.
/*      RUN GetLoan.
      IF NOT AVAIL loan THEN
        vData = "".
      DEF BUFFER xloan FOR loan.

      vProxy-code = GetSysConf("_Proxy-code_").
      
      FIND FIRST xloan WHERE xloan.contract  EQ loan.contract
                         AND xloan.cont-code EQ loan.cont-code NO-LOCK NO-ERROR.

      FIND FIRST loan WHERE loan.cust-id   EQ xloan.cust-id
                      AND loan.cust-cat    EQ xloan.cust-cat
                      AND loan.contract    EQ "proxy"
                      AND loan.cont-code   EQ vProxy-code
                      NO-LOCK NO-ERROR.
          IF  AVAIL loan THEN
            vData = STRING(loan.open-date, "99/99/9999").
            ELSE vData = "".
  */
        vProxyVal = GetSysConf("ProxyPickVal").
        FIND FIRST loan WHERE loan.cont-code EQ vProxyVal
                      AND   loan.contract  EQ "proxy"
                      NO-LOCK NO-ERROR.
         IF AVAIL loan THEN
            vData = STRING(loan.open-date, "99/99/9999").

  RUN Pars-SetCHARResult(vData).
   /* --- */
   is-ok = TRUE.
 END PROCEDURE.

/*  ****************************************************************************
  �� ������: �����頥� �������� ����� 業���� � �࠭������ ��, ���
              ��⥬� ��� ��� ����饭�� � ���� details
  ���⠪�� : ������()
*/

PROCEDURE ������:
  DEF OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* �ᯥ譮 �� �믮����� ? */
/* --- */
  is-ok = FALSE.
  IF NOT Pars-ValidParam(0) THEN RETURN.
/* --- */
  if chpar1 eq "" or chpar1 eq ? then do:
    message "�� ����� �������� ����� 業���� !".
    pause.
    return.
  end.
  RUN Pars-SetCHARResult (chpar1).
/* --- */
  is-ok = TRUE.
END PROCEDURE.
/*   ***************************************************************************
  �� ������: �����頥� ������������ 業���� � �࠭������ ��, ���
              ��⥬� ��� ��� ����饭�� � ���� details
  ���⠪�� : ��������()
*/

PROCEDURE ��������:
DEF OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* �ᯥ譮 �� �믮����� ? */

   DEF VAR i-tmpst    AS INT64  NO-UNDO INIT 1 .
   DEF VAR tmp-chpar1 AS CHAR NO-UNDO.

   is-ok = FALSE.

   MAIN:
   DO ON ERROR  UNDO, LEAVE
      ON ENDKEY UNDO, LEAVE:

      IF NOT Pars-ValidParam(0) THEN
         UNDO MAIN, LEAVE MAIN.

      IF NOT {assigned chpar1} THEN
      DO:
         RUN Fill-SysMes IN h_tmess ("", "", "0", "�� ����� �������� ����� 業����.").
         UNDO MAIN, LEAVE MAIN.
      END.
      IF NUM-ENTRIES(chpar1) GT 1 THEN
      DO:
         DO WHILE i-tmpst LE NUM-ENTRIES(chpar1):
            FIND FIRST loan        WHERE
                       loan.contract  EQ chpar2
                   AND loan.doc-ref   EQ DelFilFromLoan(ENTRY(i-tmpst,chpar1))
                   AND loan.filial-id EQ shFilial
            NO-LOCK.
            FIND FIRST asset OF loan NO-LOCK.
            ASSIGN
               tmp-chpar1 = tmp-chpar1 + ";" + asset.name
               i-tmpst    = i-tmpst + 1
            .
         END.
         tmp-chpar1 = SUBSTRING(tmp-chpar1, 2, LENGTH(tmp-chpar1)). /* ��� ����� ���� 1 ᨬ��� */

         /* � ������������ ����� ���� ����窠, �.�. ����� �� �१���, ������塞 �� �ਭ㤨⥫쭮 */
         IF tmp-chpar1 BEGINS """" THEN
            RUN Pars-SetCHARResult (""" " + tmp-chpar1 + " ").
         ELSE
         IF tmp-chpar1 BEGINS "'" THEN
            RUN Pars-SetCHARResult ("' " + tmp-chpar1 + " ").
         ELSE
            RUN Pars-SetCHARResult (tmp-chpar1 + " ").
      END.
      ELSE DO:
         FIND FIRST loan        WHERE
                    loan.contract  EQ chpar2
                AND loan.doc-ref   EQ DelFilFromLoan(chpar1)
                AND loan.filial-id EQ shFilial
         NO-LOCK.
         FIND FIRST asset OF loan NO-LOCK.

         /* � ������������ ����� ���� ����窠, �.�. ����� �� �१���, ������塞 �� �ਭ㤨⥫쭮 */
         IF asset.name BEGINS """" THEN
            RUN Pars-SetCHARResult (""" " + asset.name + " ").
         ELSE
         IF asset.name BEGINS "'" THEN
            RUN Pars-SetCHARResult ("' " + asset.name + " ").
         ELSE
            RUN Pars-SetCHARResult (asset.name + " ").
      END.

   END.

   is-ok = TRUE.
END PROCEDURE.

/* GetDetails: ������ ���祭��, ��࠭����� �� �३�� *
   �ᯮ�짮����: GetDetails (�����_蠡����(id-optempl),���_�३�� (frame-name), ���_��६����� (frame-field))
             ��� GetDetails (���_�३�� (frame-name), ���_��६����� (frame-field)) ����� 蠡���� �� ���뢠����
             ��� GetDetails (���_��६����� (frame-field)) - ���� ����� �३�� �� ���뢠����

*/
procedure GetDetails:
    DEF OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* �ᯥ譮 �� �믮����� ? */

/* --- */
    IF NOT (Pars-ValidParam(1) OR
            Pars-ValidParam(2) OR
            Pars-ValidParam(3))
    THEN RETURN.
/* --- */

    IF pn < 2 THEN
    find first wop where recid(wop) eq rid no-lock no-error.

    def var param-id-optempl as INT64 no-undo.
    def var param-field-name as char no-undo.
    def var param-frame-name as char no-undo.
    DEFINE VARIABLE vResult AS CHARACTER  NO-UNDO.
    ASSIGN
        param-id-optempl = IF pn < 2 THEN
           (IF AVAIL wop THEN wop.op-templ ELSE ? ) ELSE Pars-GetInt ( 0 )
        param-frame-name = IF pn < 1 THEN "" ELSE Pars-GetString ( pn - 1 )
        param-field-name = Pars-GetString ( pn )
        .

    RUN internal-parser-getdetails-form-ttable (param-id-optempl,param-frame-name, param-field-name, OUTPUT vResult).
    IF vResult EQ ? THEN
       RUN ElseTryGetFieldFraMeVal(param-field-name, OUTPUT vResult).
    RUN Pars-SetCHARResult (IF vResult EQ ? THEN "?" ELSE vResult).

/* --- */
    is-ok = TRUE.
end procedure.

procedure GetDetails2:
   DEFINE OUTPUT PARAMETER is-ok AS LOGICAL NO-UNDO. /* �ᯥ譮 �� �믮����� ? */

   DEFINE VARIABLE vResult AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vHandle AS HANDLE     NO-UNDO.

/* --- */
   IF NOT (Pars-ValidParam(1) OR
           Pars-ValidParam(2) OR
           Pars-ValidParam(3))
   THEN RETURN.
/* --- */

   FIND FIRST wop WHERE RECID(wop) EQ rid NO-LOCK NO-ERROR.

   IF AVAILABLE wop THEN
       /* ���� ���㬥��. */
      FIND FIRST op WHERE
           RECID(op) EQ wop.op-recid NO-LOCK NO-ERROR.

   vHandle = BUFFER wop:HANDLE:BUFFER-FIELD (Pars-GetString ( pn )) NO-ERROR.

   IF NOT VALID-HANDLE(vHandle) THEN
      vHandle = BUFFER op:HANDLE:BUFFER-FIELD (Pars-GetString ( pn )) NO-ERROR.

   IF VALID-HANDLE(vHandle) THEN
      vResult = vHandle:BUFFER-VALUE.

/*   IF AVAILABLE op THEN
      vResult = STRING(wop:HANDLE).*/

   RUN Pars-SetCHARResult (IF vResult EQ ? THEN "?" ELSE vResult).

/* --- */
   is-ok = TRUE.
end procedure.

/* NDoc: �����頥� ����� ���㬥��
   �ᯮ�짮����: NDoc (�����_蠡����(id-optempl),���_�३�� (frame-name))
             ��� NDoc (���_�३�� (frame-name)) - ����� 蠡���� �� ���뢠����
             ��� NDoc () - ���� ����� �३�� �� ���뢠����
*/
procedure NDoc:
    DEF OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* �ᯥ譮 �� �믮����� ? */

/* --- */
    IF NOT (Pars-ValidParam(0) OR
            Pars-ValidParam(1) OR
            Pars-ValidParam(2))
    THEN RETURN.
/* --- */
    IF pn < 1 THEN
    find first wop where recid(wop) eq rid no-lock no-error.
    def var param-id-optempl as INT64 no-undo.
    def var param-frame-name as char no-undo.
    DEFINE VARIABLE v-str AS CHARACTER  NO-UNDO.

    param-id-optempl = IF pn < 1 THEN
       (IF AVAIL wop THEN wop.op-templ ELSE ? ) ELSE Pars-GetInt ( pn - 1 ).
    param-frame-name = if pn = -1 THEN "" ELSE Pars-GetString ( pn ).

    RUN internal-parser-getdetails-form-ttable (param-id-optempl,param-frame-name, "doc-num", OUTPUT v-str).
    if v-str eq ? then RUN internal-parser-getdetails-form-ttable (param-id-optempl,param-frame-name, "op-doc-num", OUTPUT v-str).
    RUN Pars-SetCHARResult (IF v-str EQ ? THEN "?" ELSE v-str).

/* --- */
    is-ok = TRUE.
end procedure.

procedure NDoc2:
   DEF OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* �ᯥ譮 �� �믮����� ? */

   DEFINE VARIABLE v-str AS CHARACTER  NO-UNDO.

/* --- */
   IF NOT (Pars-ValidParam(0) OR
           Pars-ValidParam(1) OR
           Pars-ValidParam(2))
   THEN RETURN.
/* --- */

   FIND FIRST wop WHERE RECID(wop) EQ rid NO-LOCK NO-ERROR.

   IF AVAILABLE wop THEN
       /* ���� ���㬥��. */
      FIND FIRST op WHERE
           RECID(op) EQ wop.op-recid NO-LOCK NO-ERROR.

   IF AVAILABLE op THEN
      v-str = STRING(op.doc-num).

    RUN Pars-SetCHARResult (IF v-str EQ ? THEN "?" ELSE v-str).

/* --- */
    is-ok = TRUE.
end procedure.

PROCEDURE NParamDeposit:
   DEF OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* �ᯥ譮 �� �믮����� ? */
   DEFINE VARIABLE vIdChar AS CHARACTER NO-UNDO. /*����� ��ࠬ���*/
   DEFINE VARIABLE vContract AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE vContCode AS CHARACTER NO-UNDO. 

   IF NOT Pars-ValidParam(1) THEN RETURN.
   RUN GetLoan.
   IF NOT AVAIL loan THEN DO:
      vContCode = Get_Loan_Trans().
      IF vContCode EQ ? THEN vContCode = Get_Loan().
      vContract = Get_Loan_contr().
      FIND FIRST loan WHERE loan.contract  EQ vContract
                        AND loan.cont-code EQ vContCode NO-LOCK NO-ERROR.
   END.
   IF AVAIL loan THEN
      CASE Pars-GetString ( 0 ):
         WHEN "1" THEN RUN Pars-SetCHARResult(STRING(loan.open-date,"99/99/9999")).
         WHEN "2" THEN RUN Pars-SetCHARResult(STRING(loan.end-date,"99/99/9999")).
         WHEN "3" THEN RUN Pars-SetCHARResult(STRING(loan.close-date,"99/99/9999")).
         WHEN "4" THEN RUN Pars-SetCHARResult(loan.cont-type).
         OTHERWISE RUN Pars-SetCHARResult("?").
      END CASE.
   ELSE
      RUN Pars-SetResult (0).

   is-ok = TRUE.
END PROCEDURE.

/* Client: �����頥� ���� ������ ��������� ��  �३��
   �ᯮ�짮����: Client (�����_蠡����(id-optempl),���_�३�� (frame-name))
             ��� Client (���_�३�� (frame-name)) - ����� 蠡���� �� ���뢠����
             ��� Client () - ���� ����� �३�� �� ���뢠����
*/

procedure Client:
    DEF OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* �ᯥ譮 �� �믮����� ? */
    DEFINE VARIABLE v-str AS CHARACTER  NO-UNDO.
/* --- */
    IF NOT (Pars-ValidParam(0) OR
            Pars-ValidParam(1) OR
            Pars-ValidParam(2))
    THEN RETURN.
/* --- */
    IF pn < 1 THEN
    find first wop where recid(wop) eq rid no-lock no-error.
    def var param-id-optempl as INT64 no-undo.
    def var param-frame-name as char no-undo.

    param-id-optempl = IF pn < 1 THEN
       (IF AVAIL wop THEN wop.op-templ ELSE ? ) ELSE Pars-GetInt ( pn - 1 ).
    param-frame-name = if pn = -1 THEN "" ELSE Pars-GetString ( pn ).

    RUN GetLoan.
    IF AVAIL loan AND loan.cust-cat EQ "�" THEN DO:
        find first person where person-id = loan.cust-id no-lock no-error.
        if available person then v-str = person.name-last + " " + person.first-names.
    end.

    if v-str eq ?
    then RUN internal-parser-getdetails-form-ttable (param-id-optempl,param-frame-name, "op-doc-num", OUTPUT v-str).

    RUN Pars-SetCHARResult (IF v-str EQ ? THEN "?" ELSE v-str).

/* --- */
    is-ok = TRUE.
end procedure.

/* NClDoc: �����頥� ����� ������᪮�� ���㬥��
   �ᯮ�짮����: NClDoc (�����_蠡����(id-optempl),���_�३�� (frame-name))
             ��� NClDoc (���_�३�� (frame-name)) - ����� 蠡���� �� ���뢠����
             ��� NClDoc () - ���� ����� �३�� �� ���뢠����
*/

procedure NClDoc:
    DEF OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* �ᯥ譮 �� �믮����� ? */

/* --- */
    IF NOT (Pars-ValidParam(0) OR
            Pars-ValidParam(1) OR
            Pars-ValidParam(2))
    THEN RETURN.
/* --- */
    IF pn < 1 THEN
    find first wop where recid(wop) eq rid no-lock no-error.
    def var param-id-optempl as INT64 no-undo.
    def var param-frame-name as char no-undo.
     DEFINE VARIABLE v-str AS CHARACTER  NO-UNDO.

    param-id-optempl = IF pn < 1 THEN
       (IF AVAIL wop THEN wop.op-templ ELSE ? ) ELSE Pars-GetInt ( pn - 1 ).
    param-frame-name = if pn = -1 THEN "" ELSE Pars-GetString ( pn ).
    RUN GetLoan.
    if AVAIL loan AND loan.cust-cat = "�" then
    do:
        find first person where person-id = loan.cust-id no-lock no-error.
        if available person then v-str = person.document.
    end.

    if v-str eq ?
    then RUN internal-parser-getdetails-form-ttable (param-id-optempl,param-frame-name, "op-doc-num", OUTPUT v-str).

    RUN Pars-SetCHARResult (IF v-str EQ ? THEN "?" ELSE v-str).

/* --- */
    is-ok = TRUE.
end procedure.

/* NClDocType: �����頥� ⨯ ���㬥��
   �ᯮ�짮����: NClDocType (�����_蠡����(id-optempl),���_�३�� (frame-name))
             ��� NClDocType (���_�३�� (frame-name)) - ����� 蠡���� �� ���뢠����
             ��� NClDocType () - ���� ����� �३�� �� ���뢠����
*/

procedure NClDocType:
    DEF OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* �ᯥ譮 �� �믮����� ? */

/* --- */
    IF NOT (Pars-ValidParam(0) OR
            Pars-ValidParam(1) OR
            Pars-ValidParam(2))
    THEN RETURN.
/* --- */
    IF pn < 1 THEN
    find first wop where recid(wop) eq rid no-lock no-error.
    def var param-id-optempl as INT64 no-undo.
    def var param-frame-name as char no-undo.
     DEFINE VARIABLE v-str AS CHARACTER  NO-UNDO.

    param-id-optempl = IF pn < 1 THEN
       (IF AVAIL wop THEN wop.op-templ ELSE ? ) ELSE Pars-GetInt ( pn - 1 ).
    param-frame-name = if pn = -1 THEN "" ELSE Pars-GetString ( pn ).

    RUN GetLoan.
    if AVAIL loan AND loan.cust-cat = "�" then
    do:
       find first person where person-id = loan.cust-id no-lock no-error.
       if available person then v-str = person.document-id.
    end.

    if v-str eq ?
    then RUN internal-parser-getdetails-form-ttable (param-id-optempl,param-frame-name, "op-doc-num", OUTPUT v-str).

    RUN Pars-SetCHARResult (IF v-str EQ ? THEN "?" ELSE v-str).

/* --- */
    is-ok = TRUE.
end procedure.

/* NClDocIss: �����頥� "�뤠�" ��� ���㬥�� 䨧��᪮�� ���
   �ᯮ�짮����: NClDocIss(�����_蠡����(id-optempl),���_�३�� (frame-name))
             ��� NClDocIss(���_�३�� (frame-name)) - ����� 蠡���� �� ���뢠����
             ��� NClDocIss() - ���� ����� �३�� �� ���뢠����
*/

PROCEDURE NClDocIss:
   DEF OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* �ᯥ譮 �� �믮����� ? */

   DEF VAR clss-name AS CHAR.

/* --- */
    IF NOT (Pars-ValidParam(0) OR
            Pars-ValidParam(1) OR
            Pars-ValidParam(2))
    THEN RETURN.
/* --- */
    IF pn < 1 THEN
       FIND FIRST wop WHERE RECID(wop) EQ rid NO-LOCK NO-ERROR.
    DEF VAR v-id-optempl AS INT64  NO-UNDO.
    DEF VAR v-frame-name AS CHAR NO-UNDO.
    DEF VAR v-str        AS CHAR NO-UNDO.

    v-id-optempl = IF pn < 1 THEN
       (IF AVAIL wop THEN wop.op-templ ELSE ? ) ELSE Pars-GetInt(pn - 1).
    v-frame-name = if pn = -1 THEN "" ELSE Pars-GetString(pn).
    RUN GetLoan.
    IF     AVAIL loan
       AND loan.cust-cat EQ "�" THEN
    DO:
       clss-name = 'person'.
        FIND FIRST person WHERE person-id = loan.cust-id NO-LOCK NO-ERROR.
        IF AVAIL person THEN
        DO:
           v-str = person.issue.
           IF INDEX(CAPS(v-str),"�����") EQ 0 THEN
              v-str = "�뤠� " + v-str.
        END.
    END.

    RUN Pars-SetCHARResult (IF v-str EQ ? THEN "?" ELSE v-str).

/* --- */
    is-ok = TRUE.
END PROCEDURE.

/* DocDate: �����頥� ���� ���㬥�� (��� �믨᪨ ���⭮-��������� ���㬥��)
   ��ࠬ����:�����_蠡���� � ��ଠ�_�뢮��_����, none ��� ��ଠ�_�뢮��_����
   �ਬ��:  DocDate (�����_蠡����(id-optempl),��ଠ�_�뢮��_���� )
            ��� DocDate() ��� DocDate("99/99/9999")
*/
PROCEDURE DocDate:
   DEFINE OUTPUT PARAMETER is-ok AS LOGICAL NO-UNDO. /* �ᯥ譮 �� �믮����� ? */

   DEFINE VARIABLE param-id-optempl AS INT64   NO-UNDO.
   DEFINE VARIABLE param-format     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE mChResult        AS CHARACTER NO-UNDO.
   DEFINE VARIABLE v-str            AS CHARACTER NO-UNDO.

   IF NOT (   Pars-ValidParam(0)
           OR Pars-ValidParam(1)
           OR Pars-ValidParam(2)
          )
   THEN RETURN.

   FIND FIRST wop WHERE
        RECID(wop) EQ rid NO-LOCK NO-ERROR.

   param-id-optempl = IF pn < 1
                      THEN (IF AVAIL wop
                            THEN wop.op-templ
                            ELSE ?)
                      ELSE Pars-GetInt(pn - 1).
   param-format = IF pn = -1
                  THEN "99/99/9999"
                  ELSE Pars-GetString(pn).

   IF NOT {assigned param-format} THEN
      param-format = "99/99/9999".

   RUN internal-parser-getdetails-form-ttable (param-id-optempl,
                                               "",
                                               "doc-date",
                                               OUTPUT v-str).
   IF v-str EQ ? THEN
      RUN internal-parser-getdetails-form-ttable (param-id-optempl,
                                                  "",
                                                  "op-doc-date",
                                                  OUTPUT v-str).

   IF v-str EQ ? THEN
      RUN ElseTryGetFieldFraMeVal("doc-date", OUTPUT v-str).

  IF AVAIL wop THEN
      /* ���� ���㬥��. */
     FIND FIRST op WHERE
          RECID(op) EQ wop.op-recid NO-LOCK NO-ERROR.

  IF     NOT {assigned v-str}
     AND AVAIL op THEN
     v-str = STRING(op.doc-date).

   RUN Pars-SetCHARResult (IF v-str EQ ?
                           THEN "?"
                           ELSE STRING(DATE(v-str), param-format)).

    is-ok = TRUE.
END PROCEDURE.

PROCEDURE DocDate2:
   DEFINE OUTPUT PARAMETER is-ok AS LOGICAL NO-UNDO. /* �ᯥ譮 �� �믮����� ? */

   DEFINE VARIABLE param-id-optempl AS INT64   NO-UNDO.
   DEFINE VARIABLE param-format     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE mChResult        AS CHARACTER NO-UNDO.
   DEFINE VARIABLE v-str            AS CHARACTER NO-UNDO.

   IF NOT (   Pars-ValidParam(0)
           OR Pars-ValidParam(1)
           OR Pars-ValidParam(2)
          )
   THEN RETURN.

   FIND FIRST wop WHERE
        RECID(wop) EQ rid NO-LOCK NO-ERROR.

   param-format = IF pn = -1
                  THEN "99/99/9999"
                  ELSE Pars-GetString(pn).

   IF NOT {assigned param-format} THEN
      param-format = "99/99/9999".

  IF AVAIL wop THEN
      /* ���� ���㬥��. */
     FIND FIRST op WHERE
          RECID(op) EQ wop.op-recid NO-LOCK NO-ERROR.

  IF AVAILABLE op THEN
     v-str = STRING(op.doc-date).

   RUN Pars-SetCHARResult (IF v-str EQ ?
                           THEN "?"
                           ELSE STRING(DATE(v-str), param-format)).

    is-ok = TRUE.
END PROCEDURE.
/* ����⠈��, DocDate, ������2, DocCourse */

/*******************************************************************************
   ������2 - �����頥� ������������ ������
   �ᯮ�짮����: ������2 (���_�३�� (frame-name))
             ��� ������2 () - ���� ����� �३�� �� ���뢠����
             ��� ������2(�����_������� �� ���ண� ����� ������)
*/
PROCEDURE ������2:

   DEFINE OUTPUT PARAMETER is-ok AS LOGICAL NO-UNDO. /* �ᯥ譮 �� �믮����� ? */

   DEFINE VARIABLE param-id-optempl AS INT64   NO-UNDO.
   DEFINE VARIABLE param-frame-name AS CHARACTER NO-UNDO.
   DEFINE VARIABLE mChResult        AS CHARACTER NO-UNDO.

   IF NOT (Pars-ValidParam(0) OR Pars-ValidParam(1)) THEN
      RETURN.

   FIND FIRST wop WHERE RECID(wop) EQ rid NO-LOCK NO-ERROR.

   IF pn EQ -1 THEN
   /* ��ࠬ���� �� ��।��� */
      ASSIGN /* ��६ �� ⥪��. 蠡���� */
         param-frame-name = ""
         param-id-optempl = IF AVAILABLE wop THEN wop.op-templ ELSE ?.
   ELSE DO:
      param-frame-name = Pars-GetString(0).
      /* ��।��塞, �� ��।���: ����� 蠡���� ��� ��� �३�� */
      param-id-optempl = INT64(param-frame-name) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN
      /* ��।��� ��� �३�� */
         ASSIGN
            param-id-optempl   = ?
            ERROR-STATUS:ERROR = NO.
      ELSE
         param-frame-name = "".
   END.

   RUN internal-parser-getdetails-form-ttable (
          param-id-optempl,
          param-frame-name,
          "name-ben",
          OUTPUT mChResult
       ).

   RUN Pars-SetCHARResult(mChResult).
   is-ok = TRUE.

END PROCEDURE.


/* DocCourse - �����頥� ��ப� "���� ��� �� _���_ _����_ ��."
   �ᯮ�짮����: DocCourse (�����_蠡����(id-optempl),���_�३�� (frame-name))
             ��� DocCourse (���_�३�� (frame-name)) - ����� 蠡���� �� ���뢠����
             ��� DocCourse () - ���� ����� �३�� �� ���뢠����
*/
procedure DocCourse:
  DEF OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* �ᯥ譮 �� �믮����� ? */
/* --- */
  is-ok = FALSE.
  IF NOT (Pars-ValidParam(0) OR
          Pars-ValidParam(1) OR
          Pars-ValidParam(2))
  THEN RETURN.
  /*{&type-er} = "".
  IF pn <> 0 THEN DO:
    {&type-er} = {&EGMBadParamCount}.
    RETURN.
  END. ELSE pn = pn - 1.*/
/* --- */
  IF pn < 1 THEN
    find first wop where recid(wop) eq rid no-lock no-error.
  def var param-id-optempl as INT64 no-undo.
  def var param-frame-name as char no-undo.
  param-id-optempl = IF pn < 1 THEN
     (IF AVAIL wop THEN wop.op-templ ELSE ? ) ELSE Pars-GetInt ( pn - 1 ).
  param-frame-name = if pn = -1 THEN "" ELSE Pars-GetString ( pn ).

  /*param-id-optempl = IF pn < 0 THEN wop.op-templ ELSE Pars-GetInt ( pn ).
  if pn = -1
  then param-frame-name = "".
  else param-frame-name = if mvar[pj - pn] eq "" then string(result_l[pj - pn]) else TRIM(mvar[pj - pn], """").
  else param-frame-name = if Pars-GetStr eq "" then string(Pars-GetString ( pn )) else TRIM(Pars-GetString ( pn ), """").
                                                           */

  DEFINE VARIABLE vS AS CHARACTER  NO-UNDO.
  def var l-date    as date no-undo.
  def var l-course  as dec  no-undo.
  def var l-amt-cur as dec  no-undo.
  def var l-amt-rub as dec  no-undo.

  RUN internal-parser-getdetails-form-ttable (param-id-optempl,param-frame-name, "doc-date", OUTPUT vS).
  l-date = date(vS).
  if l-date eq ?
  then DO:
      RUN internal-parser-getdetails-form-ttable (param-id-optempl,param-frame-name, "op-doc-date", OUTPUT vS).
      l-date = date(vS).
  END.
  RUN internal-parser-getdetails-form-ttable (param-id-optempl,param-frame-name, "doc-course", OUTPUT vS).
  l-course = dec(vS).
  if l-course eq ? then do:
      RUN internal-parser-getdetails-form-ttable (param-id-optempl,param-frame-name, "amt-rub", OUTPUT vS).
      l-amt-rub = dec(vS).
      if l-amt-rub eq ?
      then DO:
          RUN internal-parser-getdetails-form-ttable (param-id-optempl,param-frame-name, "op-amt-rub", OUTPUT vS).
          l-amt-rub = dec(vS).
      END.
      RUN internal-parser-getdetails-form-ttable (param-id-optempl,param-frame-name, "amt-cur", OUTPUT vS).
      l-amt-cur = dec(vS).
      if l-amt-cur eq ?
      then DO:
          RUN internal-parser-getdetails-form-ttable (param-id-optempl,param-frame-name, "op-amt-cur", OUTPUT vS).
          l-amt-cur = dec(vS).
      END.
      l-course = l-amt-rub / l-amt-cur.
  end.

  RUN Pars-SetCHARResult ( "���� �� " +
                           (if l-date eq ? then "?" else string(l-date, "99/99/9999")) + ": " +
                           (if l-course eq ? then "?" else trim(string(l-course, "zzz,zzz,zzz,zzz,zzz,zzz,zz9.999999")))
                         ).
/* --- */
  is-ok = TRUE.
/* *** */
end procedure.


/* ������ - �����頥� ��� ������ �� ���
   ��ࠬ����: ��� ������
   �ਬ��: ������("840") - ������: USD
           ������("040") - ������: ATS
*/
procedure ������.
  DEF OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* �ᯥ譮 �� �믮����� ? */

  /* --- */
  is-ok = FALSE.
  {&type-er} = "".
  IF pn <> 1 THEN DO:
    {&type-er} = {&EGMBadParamCount}.
    RETURN.
  END. ELSE pn = pn - 1.
/* --- */
  def var curr-iso as char no-undo.
  curr-iso = Pars-GetString ( 0 ).
  {curr-iso.i &curr=curr-iso}

  RUN Pars-SetCHARResult ( curr-iso ).

/* --- */
  is-ok = TRUE.
end procedure.


/*-----------------------------------------------------------------------------
    * �� ������: �����頥� ���祭�� �⠢�� १�ࢨ஢���� �� �������� �� ����
    * ���⠪�� : ���䐥�()
    * ����     : amam  01/04/05
    * �ਬ��    : ���䐥�() = 3.5
  -----------------------------------------------------------------------------*/
PROCEDURE ���䐥�:

   DEF OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* �ᯥ譮 �� �믮����� ? */

   DEF VAR ph AS HANDLE NO-UNDO.
   DEF VAR vRate AS DEC NO-UNDO.

   is-ok = FALSE.

   IF NOT Pars-ValidParam(0) THEN RETURN.

   /* ��।������ �ࠢ��쭮�� �ਬ������ �㭪樨,
   ** �ଠ� private-data */
   RUN LOAN_VALID_HANDLE(input-output ph).

   IF not valid-handle(ph) THEN RETURN.

   FIND FIRST loan WHERE
       loan.contract  = ENTRY(1, ph:private-data) and
       loan.cont-code = ENTRY(2, ph:private-data)
   NO-LOCK NO-ERROR.
   IF NOT AVAIL loan THEN RETURN.

   vRate = LnRsrvRate(loan.contract, loan.cont-code, in-op-date).

   ASSIGN
       mvar[pj - pn]     = STRING(vRate)
       result_l[pj - pn] = vRate
       is-ok             = TRUE
       .

   RETURN.

END PROCEDURE.


/*
    * �� ������: �����頥� ���� ������� �������.
                  �᫨ ��।�� 1 ��ࠬ��� "������" � �����頥��� ��� ������饣� �������
    * ���⠪�� : ��⠎�(["������"])
    * ����     : Om  16/10/00
    * �ਬ��    : ��⠎�(), ��⠎�("������")
*/
PROCEDURE ��⠎�:

    DEF OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* �ᯥ譮 �� �믮����� ? */
    def var ph as handle no-undo.

    is-ok = FALSE.

    IF NOT (Pars-ValidParam(0) OR Pars-ValidParam(1)) THEN RETURN.

    /* ��।������ �ࠢ��쭮�� �ਬ������ �㭪樨,
    ** �ଠ� private-data */
    run LOAN_VALID_HANDLE (input-output ph).

    if not valid-handle (ph)
    then return.

    IF pn EQ 0 AND TRIM(Pars-GetString(0), """'") EQ "������" THEN DO: /* �᫨ 1 ��ࠬ��� � �� ࠢ�� "������" */
        FIND FIRST loan WHERE
            loan.contract  EQ ENTRY(1, ph:private-data) AND
            loan.cont-code EQ SUBSTR(ENTRY(2, ph:private-data), 1, R-INDEX(ENTRY(2, ph:private-data), " ") - 1)
            NO-LOCK NO-ERROR.
    END.
    ELSE DO:
        find first loan where
            loan.contract  eq entry(1, ph:private-data) and
            loan.cont-code eq entry(2, ph:private-data)
            no-lock no-error.
    END.

    if not avail loan then return.

    RUN Pars-SetCHARResult (string(loan.open-date, "99/99/9999")).

    is-ok = TRUE.

END PROCEDURE.

/*
    * �� ������: �����頥� ������������ ������.
    * ���⠪�� : ����
    * ����     : Om  16/10/00
    * �ਬ��    : ����
*/

PROCEDURE ����:

    DEF OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* �ᯥ譮 �� �믮����� ? */
    DEF VAR ph    AS HANDLE    NO-UNDO.
    DEF VAR name1 AS CHARACTER NO-UNDO.
    DEF VAR name2 AS CHARACTER NO-UNDO.
    DEF VAR Inn   AS CHARACTER NO-UNDO.
    is-ok = FALSE.

    IF NOT Pars-ValidParam(0) THEN RETURN.
    /* ��।������ �ࠢ��쭮�� �ਬ������ �㭪樨,
    ** �ଠ� private-data */
    run LOAN_VALID_HANDLE (input-output ph).
    if not valid-handle (ph)
    then return.

    find first loan where
        loan.contract  eq entry(1, ph:private-data) and
        loan.cont-code eq entry(2, ph:private-data)
    no-lock no-error.
    if not avail loan then return.

    RUN GetCustName IN h_base (loan.cust-cat,
                               loan.cust-id,
                               "",
                               OUTPUT name1,
                               OUTPUT name2,
                               INPUT-OUTPUT Inn).
   IF name1 + name2 EQ "" 
      AND loan.cust-cat EQ "�" THEN
   DO:
      FIND FIRST person WHERE 
         person.person-id EQ loan.cust-id 
      NO-LOCK NO-ERROR.
      IF AVAIL(person) THEN 
         ASSIGN
            name1 = person.name-last
            name2 = person.first-names.
   END.
   IF name1 + name2 EQ "" 
      AND loan.cust-cat EQ "�" THEN
   DO:
      FIND FIRST cust-corp WHERE 
         cust-corp.cust-id EQ loan.cust-id 
      NO-LOCK NO-ERROR.
      IF AVAIL(cust-corp) THEN 
         ASSIGN
            name1 = cust-corp.cust-stat
            name2 = cust-corp.name-corp.
   END.

    IF name1 + name2 NE ""
    THEN DO:
      RUN  Pars-SetCHARResult(  name1 + " "
                              + name2 +
                                IF name2 NE "" THEN
                                   (IF    SUBSTRING (name2,LENGTH(name2)) EQ '"'
                                       OR SUBSTRING (name2,LENGTH(name2)) EQ "'"
                                    THEN " "
                                    ELSE "")
                                ELSE "").

      ASSIGN
         result_l[pj - pn] = 0
         is-ok = TRUE
      .
    END.
    RETURN.
END PROCEDURE.

/*
    * �� ������: �����頥� ���� (beg-date/end-date) ��ਮ�� (OP_flt.p).
    * ���⠪�� : ��⠏ ("C/��")
    * ����     : Om 11/03/2001
    * �ਬ��    : ��⠏ ("C")
*/
PROCEDURE ��⠏.

    DEF OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* �ᯥ譮 �� �믮����� ? */

    define var vSideDate as char no-undo. /* ��।���� ���� */

    is-ok = FALSE.

    IF NOT Pars-ValidParam(1)
        THEN RETURN.

    vSideDate = if mvar[pj - pn] eq ""
                        then string (result_l[pj - pn])
                        else trim(mvar[pj - pn], """'").

    ASSIGN
        mvar [pj - pn] = string (if vSideDate eq "�"
                                    then beg-date
                                    else end-date, "99/99/9999")
    .

    is-ok = TRUE.

END PROCEDURE.

/*
**  �� ������: �����頥� �������� ���� ���㬥��.
**  ���⠪�� : ���� ()
**  ����     : Om 15/11/2001
**  �ਬ��    : ���� ()
*/
PROCEDURE ����.

   DEFINE OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* �ᯥ譮 �� �믮����� ? */

   RUN DateDoc ( "�", rid, output is-ok).

   RETURN.

END PROCEDURE.

/*
**  �� ������: �����頥� ����� ����� �⭮�⥫쭮 �������� ���� ���㬥��.
**  ���⠪�� : ���⠊� ()
**  ����     : SAP 28/01/2004
**  �ਬ��    : ���� ()
*/
PROCEDURE ���⠊�.
    DEF OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* �ᯥ譮 �� �믮����� ? */
    DEF VAR mTmp-date AS DATE NO-UNDO.

    IF NOT Pars-ValidParam(0) THEN RETURN.

    /* ���� ᮧ��������� ���㬥�� �१ �६����� ⠡���� */
    find first wop where
        recid(wop) eq rid
    no-lock no-error.

    /* ���� ᮧ������ �஢���� */
    find first op-entry where
        recid(op-entry) eq rid
    no-lock no-error.
    if avail op-entry
    /* ���� ᮧ��������� ���㬥�� */
    then find first op of op-entry
    no-lock no-error.
    /* ���� ���� �� ���㬥�� �᫨ �� �������,
    ** �᫨ ��� ��� �� �६����� ⠡��� wop,
    ** ���� "" */
    ASSIGN
        mvar [pj - pn] = string (if avail op
                                    then op.contract-date
                                    else if avail wop
                                        then wop.con-date
                                        else ?, "99/99/9999")
        mvar [pj - pn] = if mvar [pj - pn] eq ?
                         then ""
                         else mvar [pj - pn].

    mTmp-date = DATE(mvar [pj - pn]).
    mvar [pj - pn] = STRING(DATE(IF MONTH(mTmp-date) = 12
                             THEN 1
                             ELSE MONTH(mTmp-date) + 1,
                            1,
                            IF MONTH(mTmp-date) = 12 THEN
                               YEAR(mTmp-date) + 1
                            ELSE YEAR(mTmp-date)) - 1, "99/99/9999" ) .
    is-ok = TRUE.
    RETURN.

END PROCEDURE.


/*
    * �� ������: �����頥� �������� ���.
    * ���⠪�� : ������� ( ��� [, ����� ])
    * ����     : Sema, 07/09/01
    * �ਬ��    : ������� ( ��(1) )
*/
PROCEDURE �������.

    DEF OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* �ᯥ譮 �� �믮����� ? */

    /* --- */
    is-ok = FALSE.
    {&type-er} = "".
    IF NOT (Pars-ValidParam(1) OR Pars-ValidParam(2)) THEN RETURN.

  /* --- */

    DEFINE VARIABLE vAcct AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE vCurrency AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE vName AS CHARACTER EXTENT 2 NO-UNDO.

    vAcct = Pars-GetString (0).

    IF pn EQ 0 THEN DO:
        {find-act.i
           &bact = acct
           &acct = vAcct
        }
    END.
    ELSE DO:
        vCurrency = Pars-GetStringFormatted (1, "999").

        IF vCurrency EQ FGetSetting("�����悠�",?,"{&in-NC-Code}") THEN
        vCurrency = "".
        {find-act.i
           &bact = acct
           &acct = vAcct
           &curr = vCurrency
        }
    END.
    IF NOT AVAIL acct THEN DO:
        MESSAGE "���" """" + vAcct + (IF vCurrency NE ? THEN "/" + vCurrency ELSE "") + """" "��।���� � ����⢥ ��ࠬ��� � �㭪�� ������� �� ������!"
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        RETURN.
    END.


       RUN getcust0 in h_base (buffer acct,
                                no, yes,
                                output vName[1], output vName[2]).

        RUN Pars-SetCHARResult ( IF TRIM(TRIM(vName[1]) + " " + TRIM(vName[2])) NE "" THEN
                                    " " + TRIM(TRIM(vName[1]) + " " + TRIM(vName[2])) + " "
                                 ELSE
                                    ""
                               ).



    is-ok = TRUE.

END PROCEDURE.


/*
** �� ������: �����頥� ���ଠ�� �� 䨧����, ��।�������� � ���.
               �᫨ ����� ��� ��।��� ����⠭�� "������",
               � ���筨��� ���ଠ樨 �㤥� ������, � �� ���.
** ���⠪�� : ������ (���_���ଠ樨,���[,�����])
** ����     : Sema 28/02/02
** �ਬ��    : ������ (1, ��(1)), ������ (1, ��(1), ���(1))
*/
PROCEDURE ������.

   DEF OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* �ᯥ譮 �� �믮����� ? */
   DEFINE VARIABLE vAcct     AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vCurrency AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vResult   AS CHARACTER  NO-UNDO INIT ?.
   DEFINE VARIABLE vCustCat  AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vCustId   AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE mCustId   AS INT64      NO-UNDO.

   DEF BUFFER person FOR person.

   IF NOT (Pars-ValidParam(2) OR Pars-ValidParam(3)) THEN RETURN.
   ASSIGN
      vAcct     = Pars-GetString ( 1 )
      vCurrency = IF pn EQ 2 THEN Pars-GetStringFormatted ( 2, "999" ) ELSE ?
   .   
                        /* ��� ����祭�� ���ଠ樨 �� ������.
                        ** �ᯮ������ � ����㭠���� ���⥦��. */
   IF vAcct EQ "������" THEN
   CASE GetSysConf("tmp-cust-cat"):
      WHEN "�" THEN
      DO:
         mCustId = INT64 (GetSysConf ("tmp-person-id")) NO-ERROR.
         FIND FIRST person WHERE
            person.person-id  EQ mCustId
         NO-LOCK NO-ERROR.

         IF AVAIL person THEN
            RUN GetCustInfo IN h_cust (Pars-GetInt (0), "�", mCustId, OUTPUT vResult) NO-ERROR.
         ELSE
            RUN Fill-AlertSysMes IN h_tmess (
               "", "", "0",
               "�� ������ ������ �� � �����䨪��஬ " + QUOTER (GetSysConf ("tmp-person-id")) + "."
            ).
      END.
      OTHERWISE
         RUN Fill-AlertSysMes IN h_tmess (
            "", "", "0",
            "��।�� ������� ⨯ ������ " + QUOTER (GetSysConf ("tmp-cust-cat")) + "."
         ).
   END CASE.

   ELSE DO:
      IF vCurrency NE ? THEN DO:
         {find-act.i
             &bact = acct
             &acct = vAcct
             &curr = vCurrency
         }
      END.
      ELSE DO:
         {find-act.i
             &bact = acct
             &acct = vAcct
         }
      END.
      IF AVAIL acct THEN
         IF acct.cust-cat EQ "�" THEN
            RUN GetCustInfo2 IN h_cust (Pars-GetInt ( 0 ),
                                        acct.acct,
                                        acct.currency,
                                        OUTPUT vResult).
         ELSE
            vResult = ?.
      ELSE
         MESSAGE "���" """" + vAcct +  """" "��।���� � ����⢥ ��ࠬ��� � �㭪�� ������ �� ������!"
         VIEW-AS ALERT-BOX ERROR BUTTONS OK.
   END.
   RUN Pars-SetCHARResult (vResult).

   is-ok = TRUE.

END PROCEDURE.

/*
    * �� ������: �����頥� ������� �� ��������
    * ���⠪�� : ������("<��� �����ᨨ>")
    * ����     : ����  29/10/2002
    * �ਬ��    : ������("%�।")
*/

PROCEDURE ������.

    DEFINE OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* �ᯥ譮 �� �믮����� ? */

    DEF VAR vCommName AS CHAR   NO-UNDO.
    DEF VAR h_templ   AS HANDLE NO-UNDO.

    is-ok = NO.

    IF NOT Pars-ValidParam(1)
    THEN RETURN.

    vCommName = Pars-GetString(0).

    /* ��।������ �ࠢ��쭮�� �ਬ������ �㭪樨, �ଠ� private-data */
    RUN LOAN_VALID_HANDLE (INPUT-OUTPUT h_templ).

    IF NOT VALID-HANDLE(h_templ)
    THEN RETURN .

    /*���� �������*/
    RUN RE_B_LOAN IN h_loan (ENTRY(1,h_templ:PRIVATE-DATA),
                   ENTRY(2,h_templ:PRIVATE-DATA),
                   BUFFER loan).
    IF NOT AVAIL loan
    THEN RETURN.

    /*�᫮���*/
    RUN RE_L_COND IN h_loan  (loan.contract,
                   loan.cont-code,
                   in-op-date,
                   BUFFER loan-cond).
    IF NOT AVAIL loan-cond
    THEN RETURN.

    FIND LAST comm-rate WHERE
              comm-rate.commi    EQ vCommName
          AND comm-rate.kau      EQ loan-cond.contract + "," + loan-cond.cont-code
          AND comm-rate.currency EQ loan.currency
          AND comm-rate.acct     EQ "0"
          AND comm-rate.since    LE in-op-date  NO-LOCK NO-ERROR.

    IF AVAIL comm-rate
    THEN RUN Pars-SetResult (comm-rate.rate-comm).

    is-ok = TRUE.

END PROCEDURE.

/*
    * �� ������: �����頥� ������� �� �������� ᮣ��襭��, �᫨ ��।�� �࠭�
    * ���⠪�� : ����������("<��� �����ᨨ>")
    * ����     : ����  29/10/2002
    * �ਬ��    : ����������("%�।")
*/

PROCEDURE ����������.

    DEFINE OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* �ᯥ譮 �� �믮����� ? */

    DEF VAR vCommName AS CHAR   NO-UNDO.
    DEF VAR h_templ   AS HANDLE NO-UNDO.

    DEF BUFFER xloan FOR loan. /* ���������� �����. */

    is-ok = NO.

    IF NOT Pars-ValidParam(1)
    THEN RETURN.

    vCommName = Pars-GetString(0).

    /* ��।������ �ࠢ��쭮�� �ਬ������ �㭪樨, �ଠ� private-data */
    RUN LOAN_VALID_HANDLE (INPUT-OUTPUT h_templ).

    IF NOT VALID-HANDLE(h_templ) THEN
       RETURN.

    /*���� �������*/
    RUN RE_B_LOAN IN h_loan (ENTRY(1,h_templ:PRIVATE-DATA),
                   ENTRY(2,h_templ:PRIVATE-DATA),
                   BUFFER xloan).
    IF NOT AVAIL xloan THEN
       RETURN.

    FIND FIRST loan WHERE loan.contract  EQ xloan.contract
                      AND loan.cont-code EQ ENTRY(1,xloan.cont-code," ")
    NO-LOCK NO-ERROR.

    IF NOT AVAIL loan THEN
       RETURN.

    /*�᫮���*/
    RUN RE_L_COND IN h_loan  (loan.contract,
                              loan.cont-code,
                              in-op-date,
                              BUFFER loan-cond).
    IF NOT AVAIL loan-cond
    THEN RETURN.

    FIND LAST comm-rate WHERE
              comm-rate.commi    EQ vCommName
          AND comm-rate.kau      EQ loan-cond.contract + "," + loan-cond.cont-code
          AND comm-rate.currency EQ loan.currency
          AND comm-rate.acct     EQ "0"
          AND comm-rate.since    LE in-op-date  NO-LOCK NO-ERROR.

    IF AVAIL comm-rate
    THEN RUN Pars-SetResult (comm-rate.rate-comm).

    is-ok = TRUE.

END PROCEDURE.

/*
    * �� ������: �����頥� ����  �������୮�� ���, ᫥���饣� �� ��⮩
    * �஭������᪨  ��᫥���� ����樨 � �����,  �ਭ������騬
    * �����஬� �����,  ��।������ �㭪樨  � ����⢥  ��ࠬ���.
    * ���祭� ����� ����権 ��������� �१ ";".
    * ���⠪�� : ��ᫍ��("<���祭�_�����_����権>"[,"<�ଠ�_�뢮��_����>"])
    * ����     : ����  29/10/2002
    * ������஢��: ��ॣ� 24/10/2003 (pesv)
    * �ਬ���   : ��ᫍ��("10;15","99/99/9999"), ��ᫍ��("10","99/99/9999"), ��ᫍ��("10;15"), ��ᫍ��("10")
*/
PROCEDURE ��ᫍ��.

    DEFINE OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* �ᯥ譮 �� �믮����� ? */

    DEF VAR d-date       AS DATE   NO-UNDO.
    DEF VAR ListParam    AS CHAR   NO-UNDO.
    DEF VAR h_templ      AS HANDLE NO-UNDO.
    DEF VAR param-format AS CHAR   NO-UNDO.

    is-ok = NO.

    IF NOT (Pars-ValidParam(1) OR
            Pars-ValidParam(2))
    THEN RETURN.

    param-format = IF pn = 0
                   THEN "99/99/9999"
                   ELSE Pars-GetString(1).

    ListParam    = Pars-GetString(0).

    /* ��।������ �ࠢ��쭮�� �ਬ������ �㭪樨, �ଠ� private-data */
    RUN LOAN_VALID_HANDLE (INPUT-OUTPUT h_templ).

    IF NOT VALID-HANDLE(h_templ)
    THEN RETURN .

    /*���� �������*/
    RUN RE_B_LOAN IN h_loan (ENTRY(1,h_templ:PRIVATE-DATA),
                   ENTRY(2,h_templ:PRIVATE-DATA),
                   BUFFER loan).
    IF NOT AVAIL loan
    THEN RETURN.

    FOR EACH loan-int OF loan
       WHERE loan-int.mdate < in-op-date
    NO-LOCK
    BY loan-int.mdate DESC
    :
       FIND FIRST chowhe WHERE
              chowhe.id-d EQ loan-int.id-d
          AND chowhe.id-k EQ loan-int.id-k
       NO-LOCK.
       IF LOOKUP(STRING(chowhe.id-op), ListParam, ";") <> 0 THEN
          LEAVE.
    END.

    d-date  = IF AVAILABLE loan-int
              THEN (loan-int.mdate + 1)
              ELSE loan.open-date.

    RUN Pars-SetCharResult (STRING(d-date,param-format)).

    is-ok = TRUE.

END PROCEDURE.

/*
    * �� ������: �����頥� ����� � ��।����� ���  (����� �����,
                  �������� �����, ���)
    * ���⠪�� : ��⠈��("<���>","��፮�"|"��፠�"|"���"|"��፠��")
    * ����     : ����  05/11/2002
    * �ਬ��    : ��⠈��(����(),"��፮�") - ����� ����� �������� ����
                  ��⠈��(����(),"��፠�") - ����. ����� �������� ����
                  ��⠈��(����(),"���")    - ��� �������� ����
*/

PROCEDURE ��⠈��:

    DEFINE OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* �ᯥ譮 �� �믮����� ? */

    DEF VAR vDate   AS DATE NO-UNDO.
    DEF VAR vType   AS CHAR NO-UNDO.
    DEF VAR vMonths AS CHAR NO-UNDO
    INIT "ﭢ���,䥢ࠫ�,����,��५�,���,���,���,������,ᥭ����,������,�����,�������".
    DEF VAR vMonthsR AS CHAR NO-UNDO
    INIT "ﭢ���,䥢ࠫ�,����,��५�,���,���,���,������,ᥭ����,������,�����,�������".

    is-ok = NO.

    IF NOT Pars-ValidParam(2)
    THEN RETURN.

    ASSIGN
      vDate = DATE(Pars-GetString(0))
      vType = Pars-GetString(1)
    NO-ERROR.

    IF ERROR-STATUS:ERROR
    THEN RETURN.

    CASE vType:
      WHEN "��፮�" THEN RUN Pars-SetCharResult (STRING(MONTH(vDate),"99")).
      WHEN "��፠�" THEN RUN Pars-SetCharResult (ENTRY(MONTH(vDate),vMonths)).
      WHEN "��፠��" THEN RUN Pars-SetCharResult (ENTRY(MONTH(vDate),vMonthsR)).
      WHEN "���"    THEN RUN Pars-SetCharResult (STRING(YEAR(vDate))).
      OTHERWISE RETURN.
    END CASE.

    is-ok = TRUE.

END.

/* ��� ��������� ���祭�� ���� in-field-name � ⥪�襬 �३��, �ᯮ�짮���� ��᫥
��楤��� internal-parser-getdetails-form-ttable, �����  ������� ���祭�� �᫨
�� ���� 㦥 "�஡�����" */

PROCEDURE ElseTryGetFieldFraMeVal :
   DEFINE INPUT  PARAMETER in-field-name AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER opResult AS CHARACTER  NO-UNDO.

   DEF VAR v-wh AS WIDGET-HANDLE NO-UNDO.
   opResult = "".
   v-wh = SELF:HANDLE NO-ERROR.
   IF NOT VALID-HANDLE(v-wh) THEN RETURN.
   IF v-wh:TYPE NE "FRAME" THEN
   DO:
      v-wh = v-wh:FRAME NO-ERROR.
      IF ERROR-STATUS:ERROR THEN
         RETURN.
   END.
   v-wh = v-wh:FIRST-CHILD NO-ERROR.
   v-wh = v-wh:FIRST-CHILD NO-ERROR.
   DO WHILE VALID-HANDLE(v-wh):
      IF v-wh:NAME EQ in-field-name THEN
      DO:
         opResult = v-wh:SCREEN-VALUE.
         LEAVE.
      END.
      v-wh = v-wh:NEXT-SIBLING.
   END.

END PROCEDURE.

/*
    * �� ������: �����뢠�� ��㧥� � ��ᯮ�⠬� ᤥ���, ���㠫��� ��� ������
                  ����樨 � �����頥� ��࠭�� �� Ctrl-Enter ����� ��ᯮ��
    * ���⠪�� : ��ᯮ�⑤����()
    * �ਬ��    : ��ᯮ�⑤����()
*/

PROCEDURE ��ᯮ�⑤����:

   DEFINE OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* �ᯥ譮 �� �믮����� ? */

   DEFINE BUFFER op FOR op.
   DEFINE BUFFER op-entry FOR op-entry.

   DEFINE VARIABLE vCat   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vIdLst AS CHARACTER NO-UNDO.

   DEFINE VARIABLE vDb  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vCr  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vCur AS CHARACTER NO-UNDO.

   IF NOT Pars-ValidParam(0) THEN
      RETURN.

   FOR FIRST wop      WHERE RECID(wop)   EQ rid   NO-LOCK
   :
      vDb = wop.acct-db.
      vCr = wop.acct-cr.
      vCur = wop.currency.
   END.

   IF NOT AVAILABLE wop THEN DO:
      FOR FIRST op       WHERE RECID(op)   EQ rid   NO-LOCK,
          FIRST op-entry WHERE op-entry.op EQ op.op NO-LOCK
      :
         vDb = op-entry.acct-db.
         vCr = op-entry.acct-cr.
         vCur = op-entry.currency.
      END.
   END.

   /* ���������� �� ������ �� ������ */
   FOR FIRST acct WHERE acct.acct     EQ vDb
                    AND acct.currency EQ vCur
                    AND CAN-DO("�,�", acct.cust-cat)
       NO-LOCK:

      /* �� ����� 䨫��஢��� �����⮢ ࠧ��� ��⥣�਩ */
      IF vCat EQ "" THEN
         vCat = acct.cust-cat.
      ELSE IF vCat NE acct.cust-cat THEN DO:
         LEAVE.
      END.

      IF vCat EQ "�" AND
         CAN-FIND(FIRST cust-corp WHERE cust-corp.cust-id EQ acct.cust-id
                                    AND (cust-corp.country-id EQ "RUS" OR
                                         NOT {assigned cust-corp.country-id}))
      THEN
         vIdLst = vIdLst + "," + STRING(acct.cust-id).

      IF vCat EQ "�" AND
         CAN-FIND(FIRST person WHERE person.person-id   EQ acct.cust-id
                                 AND (person.country-id EQ "RUS" OR
                                      NOT {assigned person.country-id}))
      THEN
         vIdLst = vIdLst + "," + STRING(acct.cust-id).
   END.

   /* ���������� �� ������ �� �।��� */
   FOR FIRST acct WHERE acct.acct     EQ vCr
                    AND acct.currency EQ vCur
                    AND CAN-DO("�,�", acct.cust-cat)
       NO-LOCK:

      /* �� ����� 䨫��஢��� �����⮢ ࠧ��� ��⥣�਩ */
      IF vCat EQ "" THEN
         vCat = acct.cust-cat.
      ELSE IF vCat NE acct.cust-cat THEN DO:
         LEAVE.
      END.

      IF vCat EQ "�" AND
         CAN-FIND(FIRST cust-corp WHERE cust-corp.cust-id EQ acct.cust-id
                                    AND (cust-corp.country-id EQ "RUS" OR
                                         NOT {assigned cust-corp.country-id}))
      THEN
         vIdLst = vIdLst + "," + STRING(acct.cust-id).

      IF vCat EQ "�" AND
         CAN-FIND(FIRST person WHERE person.person-id   EQ acct.cust-id
                                 AND (person.country-id EQ "RUS" OR
                                      NOT {assigned person.country-id}))
      THEN
         vIdLst = vIdLst + "," + STRING(acct.cust-id).
   END.

   vIdLst = TRIM(vIdLst, ",").

   pick-value = ?.

   RUN browseld("PS",
                "contract~001cust-cat~001cust-id",
                "������,������~001" + vCat + "~001" + vIdLst,
                "contract",
                2
   ).

   IF KEYFUNCTION(LASTKEY) NE "END-ERROR" THEN DO:
      /* � ��ࢮ� ENTRY �࠭���� ⨯ ��ᯮ��, �� ��஬ - ����� */
      RUN Pars-SetCharResult(ENTRY(2, pick-value)).
   END.

   is-ok = TRUE.
END.

/*
   �� ������: �����頥� ���祭�� �� �������� � �㦭�� �ଠ�:
                  "1", � ���祭�� ����� ���: "N <���������> �� <��/��/����>".
                  "2", � ���祭�� ����� ���: <��/��/����>
                  "3", � ���祭�� ����� ���: <���������>
                  �� 㬮�砭�� "1".
   ���⠪�� : ��������(���,�����[,�ଠ�])
*/
PROCEDURE ��������:
    DEFINE OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* �ᯥ譮 �� �믮����� ? */

    DEFINE VARIABLE vResult AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE vAcct   AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE vCurr   AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE vTmp    AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE vFormat AS INT64     NO-UNDO.

    IF NOT (pn EQ 2 OR pn EQ 3) THEN
      RETURN.
    ASSIGN
      pn      = pn - 1
      vAcct   = Pars-GetString(0)
      vCurr   = Pars-GetString(1)
      vCurr   = IF vCurr EQ "{&in-NC-Code}"
                THEN ""
                ELSE vCurr
      vFormat = IF pn EQ 2
                THEN Pars-GetInt(2)
                ELSE 1
    .

   {find-act.i &acct=vAcct
               &curr=vCurr}

    IF AVAIL(acct) THEN
       vTmp = GetXattrValue("acct",Acct.acct + "," + acct.currency,"��������").

    IF vTmp NE "" THEN
       CASE vFormat:
          WHEN 1 THEN
             vResult = "N " + (IF NUM-ENTRIES(vTmp) EQ 2 THEN
                                 ENTRY(2,vTmp)
                               ELSE "") +
                       " �� " + ENTRY(1,vTmp).
          WHEN 2 THEN
             vResult = ENTRY(1,vTmp).
          WHEN 3 THEN
             vResult = IF NUM-ENTRIES(vTmp) EQ 2 THEN
                          ENTRY(2,vTmp)
                       ELSE "".
          OTHERWISE
              vResult = "N " + (IF NUM-ENTRIES(vTmp) EQ 2 THEN
                                  ENTRY(2,vTmp)
                                ELSE "") +
                        " �� " + ENTRY(1,vTmp).
       END CASE.
    ELSE
       vResult = "".

    RUN Pars-SetCHARResult(vResult).

    is-ok = TRUE.

END PROCEDURE.

/*
  �� ������: �����頥� ������������ ���ᮢ�� ᨬ����� �� ���㬥��
  ���⠪�� : ������()
*/
PROCEDURE ������:
  DEF OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* �ᯥ譮 �� �믮����� ? */

  DEFINE VARIABLE i       AS INT64     NO-UNDO.
  DEFINE VARIABLE vResult AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE vKSList AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE vKS     AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE vStrTMP AS CHARACTER   NO-UNDO.

  IF {assigned naimks} THEN
     DO i = 1 TO NUM-ENTRIES(naimks):
        vKS = ENTRY(i,naimks).
        /* �᫨ 㦥 ��ࠡ��뢠�� ��, � �ய�᪠�� */
        IF LOOKUP(vKS,vKSList) EQ 0 THEN DO:
           vStrTMP = getTCodeFld("name", "��ᑨ�����", vKS, gend-date).

           IF vStrTMP NE ? AND vStrTMP NE "" THEN
              vResult = vResult + vStrTMP + "~n".
           /* ������塞 ��� ��ࠡ�⠭�� ��*/
           {additem.i vKSList vKS}
        END.
     END.
  ELSE
     RETURN.

  RUN Pars-SetCHARResult(vResult).

  is-ok  = TRUE.

END PROCEDURE.

/* �� ������: ������ ���祭�� ���� � �ଠ� ��/��/����                */
/*             �� ����室����� ᤢ����� ���� �� �ॡ㥬�� ���-�� ���भ�� �����*/
/*     ��ଠ�: date(��ப�[,���-�� ����]) */
/*  ���⠪��: date("")            - ⥪��� ���                          */
/*             date("������")      - ��� ���भ�                          */
/*             date("��珥ਮ��")  - ��砫� ��ਮ�� ��� op_flt && nach2flt */
/*             date("�����ਮ��")  - �����  ��ਮ�� ��� op_flt && nach2flt */
/*             date("���珥ਮ��") - ��砫� ������쭮�� ��ਮ��            */
/*             date("������ਮ��") - �����  ������쭮�� ��ਮ��            */
/*             date("��⠑�ப�")  - �८�ࠧ������ ��ப� � ����          */

PROCEDURE DATE:
   DEFINE OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* �ᯥ譮 �� �믮����� ? */

   DEFINE VARIABLE vDateStr  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vTmpDate  AS DATE      NO-UNDO.
   DEFINE VARIABLE vQntDay   AS INT64   NO-UNDO.
   /* ������ �室��� ��ࠬ��஢. */
   IF NOT (   Pars-ValidParam(1)
           OR Pars-ValidParam(2)) THEN DO:
      RETURN.
   END.

   ASSIGN
      vDateStr = TRIM(Pars-GetString(0),"""")
      vQntDay  = Pars-GetInt(1) WHEN pn EQ 1
   .
   CASE vDateStr:
      WHEN ""            THEN
         vTmpDate = TODAY.
      WHEN "������"      THEN DO:
        IF in-op-date = ? THEN
           in-op-date  = gend-date.
         vTmpDate = in-op-date.
      END.
      WHEN "��珥ਮ��"  THEN
         vTmpDate = beg-date.
      WHEN "�����ਮ��"  THEN
         vTmpDate = end-date.
      WHEN "���珥ਮ��" THEN
         vTmpDate = gbeg-date.
      WHEN "������ਮ��" THEN
         vTmpDate = gend-date.
      OTHERWISE DO:
         vTmpDate = DATE(vDateStr) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN
         DO:
            MESSAGE "� �㭪樨 <date> ������ ����ୠ� ���"
               VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            RETURN.
         END.
      END.
   END CASE.

   IF vQntDay NE 0 THEN
      vTmpDate = AfterOpDays(vTmpDate,-1 * vQntDay).

   ASSIGN
      result_l[pj - pn] = INT64(vTmpDate)
      mvar[pj - pn]     = STRING(vTmpDate, "99/99/9999")
   .
   is-ok = YES.
END PROCEDURE.


/* �� ������: �����頥� ��砫� ���� ��� ��।����� ����                  */
/*     ��ଠ�: ��烮�(���)                                                */
/*  ���⠪��: ��烮�(����())     - ⥪��� ���                          */
PROCEDURE ��烮�:
   DEFINE OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* �ᯥ譮 �� �믮����� ? */

   DEF VAR vBegDate  AS DATE NO-UNDO.
   DEF VAR vDate     AS DATE NO-UNDO.

   MAIN:
   DO ON ERROR  UNDO, LEAVE
      ON ENDKEY UNDO, LEAVE:
      /* ������ �室��� ��ࠬ��஢. */
      IF NOT (   Pars-ValidParam(0)
              OR Pars-ValidParam(1)) THEN
         LEAVE MAIN.

      IF pn EQ -1 THEN
         vBegDate = in-op-date.
      ELSE
         vBegDate = DATE(Pars-GetString(pn)).

      vDate = FirstYearDate (vBegDate).
      RUN Pars-SetCHARResult (STRING(vDate)).
      is-ok = YES.
   END.
END PROCEDURE.


/* �� ������: �����頥� ��砫� �����, ����⠫� ��� ���㣮���            */
/*             �⭮�⥫쭮 ���� ����-���                                  */
/*     ��ଠ�: ��珥ਮ��(⨯[,���])                                      */
/*  ���⠪��: ��珥ਮ��("�"[,���])  - ��砫� �����                     */
/*             ��珥ਮ��("�"[,���])  - ��砫� ����⠫�                   */
/*             ��珥ਮ��("�"[,���])  - ��砫� ���㣮���                  */
/*             ��珥ਮ��("�"[,���])  - ��砫� ����                       */
PROCEDURE ��珥ਮ��:
   DEFINE OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* �ᯥ譮 �� �믮����� ? */

   DEF VAR vDate     AS DATE NO-UNDO.
   DEF VAR vPeriod   AS CHAR NO-UNDO.

   MAIN:
   DO ON ERROR  UNDO, LEAVE
      ON ENDKEY UNDO, LEAVE:
      /* ������ �室��� ��ࠬ��஢. */
      IF NOT (   Pars-ValidParam(1)
              OR Pars-ValidParam(2)) THEN
         LEAVE MAIN.

      vPeriod = Pars-GetString(0).
      IF pn GT 0 THEN
         vDate = DATE(Pars-GetString(1)).
      ELSE
         vDate = in-op-date.
      IF vDate = ? THEN LEAVE MAIN.

      CASE vPeriod:
         WHEN "�" OR
         WHEN "M" THEN vDate = FirstMonDate(vDate).
         WHEN "K" OR
         WHEN "�" THEN vDate = kvart_beg(vDate).
         WHEN "�" THEN vDate = FirstHalfYearDate(vDate).
         WHEN "�" THEN vDate = FirstYearDate(vDate).
         OTHERWISE LEAVE MAIN.
      END CASE.

      RUN Pars-SetCHARResult (STRING(vDate)).
      is-ok = YES.
   END.
END PROCEDURE.

/* �� ������: �����頥� ��� 䨫����
**     ��ଠ�: ������()
**  ���⠪��: ������()  - �����頥� ��� 䨫����
*/
PROCEDURE ������:
   DEFINE OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* �ᯥ譮 �� �믮����� ? */

   IF Pars-ValidParam(0) THEN
   DO:
      RUN Pars-SetCHARResult (ShFilial).
      is-ok = YES.
   END.
END PROCEDURE.


/* �� ������: �����頥� ��� 䨫����
**     ��ଠ�: ������()
**  ���⠪��: ������()  - �����頥� ��� 䨫����
*/
PROCEDURE ������_��������:
   DEFINE OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* �ᯥ譮 �� �믮����� ? */

   DEF VAR vLoan   AS HANDLE NO-UNDO.
   DEF VAR VInStr  AS CHAR   NO-UNDO.
   DEF VAR VDate   AS DATE   NO-UNDO.
   DEF VAR VDef    AS CHAR   NO-UNDO.
   DEF VAR VResult AS CHAR   NO-UNDO.

   IF NOT (Pars-ValidParam(1) OR
           Pars-ValidParam(2) OR
           Pars-ValidParam(3)) THEN
      RETURN.
   ASSIGN
      VInStr = Pars-GetString(0)
      VDef   = Pars-GetString(2) WHEN pn GE 2
   .
   IF pn GE 1 AND Pars-GetString(1) EQ "��" THEN
      VDate = wop.con-date.
   ELSE
      VDate = in-op-date.
   RUN LOAN_VALID_HANDLE (INPUT-OUTPUT vLoan).
   IF NOT VALID-HANDLE (vLoan) THEN
      RETURN.
   RUN bag-data.p (ENTRY(1, vLoan:PRIVATE-DATA),
                   ENTRY(2, vLoan:PRIVATE-DATA),
                   VInStr,
                   VDate,
                   VDef,
                   OUTPUT VResult).
   RUN Pars-SetCHARResult (VResult).
   is-ok = YES.

END PROCEDURE.

/*
    * �� ������: �����頥� ��� � ����� ��.
    * ���⠪�� : ������_��
    * ����     : Ozmi  8/06/2011
    * �ਬ��    : ������_��
*/
PROCEDURE ������_��:

   DEFINE OUTPUT PARAMETER is-ok AS LOGICAL     NO-UNDO. /* �ᯥ譮 �� �믮����� ? */

   DEFINE VARIABLE ph      AS HANDLE        NO-UNDO.
   DEFINE VARIABLE mResult AS CHARACTER     NO-UNDO.

   DEFINE BUFFER sec-code FOR sec-code.

   is-ok = FALSE.

   IF NOT Pars-ValidParam(0) THEN RETURN.

   /* ��।������ �ࠢ��쭮�� �ਬ������ �㭪樨, �ଠ� private-data */
   RUN LOAN_VALID_HANDLE (INPUT-OUTPUT ph).

   IF NOT VALID-HANDLE(ph) THEN RETURN.

   FIND FIRST loan WHERE loan.contract  EQ ENTRY(1, ph:PRIVATE-DATA)
                     AND loan.cont-code EQ ENTRY(2, ph:PRIVATE-DATA) NO-LOCK NO-ERROR.
   IF NOT AVAIL loan THEN RETURN.

   FIND FIRST sec-code WHERE sec-code.sec-code EQ loan.sec-code NO-LOCK NO-ERROR.
   IF NOT AVAIL sec-code THEN RETURN.

   mResult = sec-code.series + " " + STRING(sec-code.form-nbr).

   RUN Pars-SetCHARResult(mResult).

   is-ok = TRUE.

END PROCEDURE.

/*
    * �� ������: �롮� ����७����
    * ���⠪�� : �����멂�������
    * ����     : kostik  01/07/2011
    * �ਬ��    : �����멂�������
*/
PROCEDURE ����������:
   DEFINE OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* �ᯥ譮 �� �믮����� ? */
   DEFINE VAR vacct AS CHARACTER NO-UNDO.
   DEFINE VAR vRetVal AS CHARACTER INIT "" NO-UNDO.
   DEFINE BUFFER bfacct FOR acct.
   DEFINE BUFFER loan FOR loan.

IF NOT Pars-ValidParam(0) THEN RETURN.

{find-act.i &acct=wop.acct-db &bact=bfacct}
IF AVAIL bfacct AND CAN-FIND(FIRST loan WHERE loan.class-code EQ "proxy-base"
                                          AND loan.cust-cat EQ bfacct.cust-cat 
                                          AND loan.cust-id  EQ bfacct.cust-id)
THEN vacct = bfacct.acct.
ELSE DO:
   {find-act.i &acct=wop.acct-cr &bact=bfacct}
   IF AVAIL bfacct AND CAN-FIND(FIRST loan WHERE loan.class-code EQ "proxy-base"
                                             AND loan.cust-cat EQ bfacct.cust-cat 
                                             AND loan.cust-id  EQ bfacct.cust-id)
   THEN vAcct = bfacct.acct.
END.

is-ok = YES.

IF not {assigned vAcct} THEN 
RETURN.

IF AVAIL bfacct THEN DO TRANSACTION:
   RUN browseld.p("proxy-base",
                  "cust-cat" + CHR(1) + "cust-id"       + CHR(1) + "drower-id",
                  bfacct.cust-cat  + CHR(1) + STRING(bfacct.cust-id) + CHR(1) +  STRING(bfacct.cust-id),
                  "cust-cat" + CHR(1) + "cust-id"       + CHR(1) + "drower-id",
                  3).
   IF LASTKEY EQ 10 AND pick-value NE "" THEN DO:

   FIND FIRST wop WHERE RECID(wop) EQ rid NO-LOCK NO-ERROR.

   IF AVAILABLE wop THEN
       /* ���� ���㬥��. */

       FIND FIRST op WHERE
           RECID(op) EQ wop.op-recid NO-LOCK NO-ERROR.

      
      FIND FIRST loan WHERE loan.contract EQ ENTRY(1,pick-value)
                        AND loan.cont-code EQ ENTRY(2,pick-value)
      NO-LOCK NO-ERROR.

      vRetVal = IF AVAIL loan THEN (loan.doc-num + " �� " + STRING(loan.open-date,"99.99.9999") + " �.") ELSE "".
      RUN SetSysConf IN h_base ("������",loan.contract + "," + loan.cont-code).
   END.
   RUN Pars-SetCHARResult(vRetVal).
END.
END PROCEDURE.

/*
    * �� ������: �����頥� ������� ����� ����७����
    * ���⠪�� : ����������()
    * ����     : ���  15/05/2014
    * �ਬ��    : ����������()
*/
PROCEDURE ����������:
   
DEFINE OUTPUT PARAM is-ok AS LOGICAL            NO-UNDO. /* �ᯥ譮 �� �믮����� ? */
DEFINE VARIABLE vDover    AS CHARACTER          NO-UNDO.
DEFINE VARIABLE vAgent    AS CHARACTER          NO-UNDO.
DEFINE VARIABLE vAcct     AS CHARACTER          NO-UNDO.
DEFINE VARIABLE vAgentID  AS INT64              NO-UNDO.
DEFINE VARIABLE vAgentCat AS CHARACTER          NO-UNDO.
DEFINE VARIABLE vClName   AS CHARACTER EXTENT 3 NO-UNDO.
DEFINE VARIABLE vRetVal   AS CHARACTER INIT ""  NO-UNDO.

DEFINE BUFFER acct FOR acct.
DEFINE BUFFER loan FOR loan.

IF NOT Pars-ValidParam(0) THEN RETURN.

ASSIGN
   vDover = GetSysConf("������")
   is-ok = YES.

IF {assigned vDover} AND NUM-ENTRIES(vDover) EQ 2 THEN
DO:
   FIND FIRST loan WHERE loan.contract  EQ ENTRY(1,vDover) AND 
                         loan.cont-code EQ ENTRY(2,vDover)
   NO-LOCK NO-ERROR.
END.
ELSE
DO:
   {find-act.i &acct=wop.acct-db &bact=acct}
   IF AVAIL acct AND CAN-FIND(FIRST loan WHERE loan.class-code EQ "proxy-base"
                                             AND loan.cust-cat EQ acct.cust-cat 
                                             AND loan.cust-id  EQ acct.cust-id)
   THEN vAcct = acct.acct.
   ELSE DO:
      {find-act.i &acct=wop.acct-cr &bact=acct}
      IF AVAIL acct AND CAN-FIND(FIRST loan WHERE loan.class-code EQ "proxy-base"
                                                AND loan.cust-cat   EQ acct.cust-cat 
                                                AND loan.cust-id    EQ acct.cust-id)
      THEN vAcct = acct.acct.
   END.
   IF AVAIL(acct) AND {assigned vAcct} THEN
   RUN browseld.p("proxy-base",
                  "cust-cat"     + CHR(1) + "cust-id"            + CHR(1) + "drower-id",
                  acct.cust-cat  + CHR(1) + STRING(acct.cust-id) + CHR(1) +  STRING(acct.cust-id),
                  "cust-cat"     + CHR(1) + "cust-id"            + CHR(1) + "drower-id",
                  3).
   IF LASTKEY EQ 10 AND pick-value NE "" THEN
   DO:
      FIND FIRST loan WHERE loan.contract  EQ ENTRY(1,pick-value) AND 
                            loan.cont-code EQ ENTRY(2,pick-value)
      NO-LOCK NO-ERROR.
   END.
END.
/**/
IF AVAIL(loan) THEN
DO:

   vAgentID  = INT64(GetXAttrValueEx("loan",
                                  loan.contract + "," + loan.cont-code,
                                  "agent-id",
                                  "0")) NO-ERROR.

   FIND FIRST signs WHERE signs.file-name  EQ "loan" AND 
                          signs.code       EQ "agent-cat" AND
                          signs.surrogate  EQ  loan.contract + "," + loan.cont-code NO-LOCK NO-ERROR.

   IF AVAIL(signs) THEN vAgentCat = signs.code-value + signs.xattr-value.

   IF NOT ERROR-STATUS:ERROR AND 
      vAgentID NE 0 THEN
   DO:
      RUN GetCustName IN h_base (vAgentCat, vAgentID, "",
                                 OUTPUT       vClName[1],
                                 OUTPUT       vClName[2],
                                 INPUT-OUTPUT vClName[3]).
   
      vAgent = vClName[1] + " " + vClName[2].
   END.

   vRetVal = vAgent.
   /*RUN SetSysConf IN h_base ("������",?).*/
END.

RUN Pars-SetCHARResult(vRetVal).

END PROCEDURE.
/* $LINTUSER='BIS' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='30/01/2015 09:19:12.805+04:00' */
/* $LINTFILE='details.fun' */
/*prosignJ9ZkNd+8Cn+bqCri5TY+HA*/