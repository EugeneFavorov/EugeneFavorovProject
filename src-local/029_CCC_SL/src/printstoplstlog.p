/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2016 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: printstoplstlog.p
      Comment: ���� "�஢�ઠ ���㬥�⮢ �� �⮯ ���⠬"
   Parameters: iOp oNoCont
         Uses:
      Used by:
      Created: 29.08.2013 
     Modified: 29.08.2013 
*/

{globals.i}     
{intrface.get xclass}
{wordwrap.def}
{intrface.get tmess}
{stoplist.fun}

DEFINE INPUT  PARAMETER iOp     AS INT64 NO-UNDO.
DEFINE OUTPUT PARAMETER oNoCont AS LOG   NO-UNDO.

DEFINE VARIABLE vSLType         AS CHAR          NO-UNDO.
DEFINE VARIABLE vNum            AS INT64         NO-UNDO.
DEFINE VARIABLE vI              AS INT64         NO-UNDO.
DEFINE VARIABLE vKritval        AS CHAR EXTENT 5 NO-UNDO.
DEFINE VARIABLE vSL             AS CHAR EXTENT 5 NO-UNDO.
DEFINE VARIABLE vRez            AS LOG           NO-UNDO.
DEFINE VARIABLE mFlag           AS LOG           NO-UNDO.

mFlag = yes.
IF GetSysConf ("IMPEXPSL") NE "YES" THEN DO:
   FIND FIRST op WHERE op.op EQ iOp NO-LOCK NO-ERROR.
   IF AVAIL op THEN
      vSLType = GetXattrValueEx("op-kind",op.op-kind,"������������_���","").
   ELSE
      vSLType = "".
   IF vSLType EQ "���" THEN
      RETURN.
   IF vSLType EQ "��" THEN 
      mFlag = no.
END.


vSLType = IF GetSysConf("IMPEXPSL") EQ "YES" THEN FGetSetting("�⮯-�����","��ᯈ��_���","")
                                             ELSE FGetSetting("�⮯-�����","������������_���","").

IF mFlag AND ENTRY(1,vSLType, ";") EQ "���" THEN
   RETURN.

vSLType = ENTRY(2,vSLType, ";") NO-ERROR.   
vSLType =  IF  {assigned  TRIM(vSLType) } THEN  vSLType ELSE "*".
{empty tt-view-sl}
RUN ChkStopListOp(iOp, vSLType, OUTPUT vRez).
FIND FIRST tt-view-sl NO-ERROR.
IF AVAIL tt-view-sl THEN
DO:
   /* �᫨ �뢮� �� �࠭ �⪫�祭 */
   IF GetProcSettingByCode("��_�뢮�����࠭") EQ "���" THEN
   /* �뢮� ᮮ�饭�� (� ��⮪��) */
   RUN Fill-SysMes("","core3301","","").
   ELSE  DO: /* ���� ����� ���짮��⥫� */
       RUN Fill-SysMes("","core59","","").
       IF (pick-value = "yes") THEN 
       DO:
          oNoCont = NO.
          RETURN.       
       END.
       ELSE
       DO:
          oNoCont = YES.
          {setdest.i &stream = "stream rep" &filename = "'chkstoplist.tmp'"  &cols = 170 &nodef   = "/*"}
          /*�뢮� ����*/
          FIND FIRST tt-view-sl NO-ERROR.
          IF AVAIL tt-view-sl THEN
          DO:
             /*�뢮� ����*/
          
             PUT STREAM rep UNFORMATTED " ���㬥��� � 䨣�࠭⠬� �ࠢ�筨�� '�⮯-�����'"  SKIP .
             PUT STREAM rep UNFORMATTED SKIP(1).
             PUT STREAM rep UNFORMATTED "�������������������������������������������������������������������������������������������������������������������������������������������Ŀ" SKIP.
             PUT STREAM rep UNFORMATTED "�� �/� � ��� ���㬥�� � � ���㬥��  �   ���਩               �    ������������ �����     �  � �/� �� �          �ਬ�砭��          �" SKIP.
             PUT STREAM rep UNFORMATTED "�      �                �              �                          �                              � �⮯-���⠳                              �" SKIP.
          
            FOR EACH tt-view-sl NO-LOCK 
                             BREAK BY tt-view-sl.fld3 
                                   BY tt-view-sl.fld2 
                                   BY tt-view-sl.fld1 
                                   BY tt-view-sl.fld7:
            IF       FIRST-OF(tt-view-sl.fld3)
               AND   FIRST-OF(tt-view-sl.fld2)
               AND   FIRST-OF(tt-view-sl.fld1)
               AND   FIRST-OF(tt-view-sl.fld7)
            THEN
            DO:
                ASSIGN
                   vNum        = vNum + 1
                   vKritval[1] =  tt-view-sl.fld4
                   vSL[1]      =  tt-view-sl.fld14.
          
          
          
                {wordwrap.i &s=vKritval  &n=5 &l=30 }
                {wordwrap.i &s=vSL    &n=5 &l=30 }
                PUT STREAM rep UNFORMATTED "�������������������������������������������������������������������������������������������������������������������������������������������Ĵ" SKIP.          
                PUT STREAM rep UNFORMATTED "�" string(vNum)      FORMAT "X(6)"  +     /*�����*/
                                "�" tt-view-sl.fld2     FORMAT "X(16)"  +     /*��� ���㬥��*/
                                "�" tt-view-sl.fld1     FORMAT "X(14)"  +     /*����� ���㬥��*/
                                "�" tt-view-sl.fld3     FORMAT "X(26)"  +     /*���਩*/
                                "�" vKritval[1]         FORMAT "X(30)"  +     /*���祭�� �����*/
                                "�" tt-view-sl.fld6     FORMAT "X(11)"  +     /*����� �⮯ ���*/
                                "�" vSL[1]              FORMAT "X(30)"  +     /*��� �⮯ ����*/
                                "�"
                SKIP. 
          
                DO vI = 2 TO 5:
                   IF   vKritval[vI] NE ""
                      OR vSL[vI]  NE ""
                   THEN
                      PUT STREAM rep UNFORMATTED "�" " "       FORMAT "X(6)"   + 
                                      "�" " "       FORMAT "X(16)"  + 
                                      "�" " "       FORMAT "X(14)"  + 
                                      "�" " "       FORMAT "X(26)"  + 
                                      "�" IF vKritval[vI]  NE "" THEN vKritval[vI] ELSE " " FORMAT "X(30)"  + 
                                      "�" " "       FORMAT "X(11)"  + 
                                      "�" IF vSL[vI]       NE "" THEN vSL[vI]      ELSE " " FORMAT "X(30)"  + 
                                      "�"
                      SKIP.
                END.
               END.
               
             END.
             PUT STREAM rep UNFORMATTED "���������������������������������������������������������������������������������������������������������������������������������������������" SKIP.
          END.
          {preview.i  &stream = "stream rep" &filename = "'chkstoplist.tmp'"} 
       END.
   END.
END.
/* $LINTFILE='printstoplstlog.p' */
/* $LINTMODE='1,4,6,3' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='shoi' */
/* $LINTDATE='22/06/2016 18:38:25.615+03:00' */
/*prosignwGKnShFqTtGEbbMGLlrFCA*/