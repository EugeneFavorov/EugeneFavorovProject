/*
               ������᪠� ��⥣�஢����� ��⥬� QBIS
    Copyright: (C) 1992-2015 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: brw446psym.p
      Comment: ��㧥� �����䨪��� "��446-����"
   Parameters: ���
      Created: 17.09.2015 soav
     Modified: 16.10.2015 krok
     Modified: <date> <who>
*/

&GLOBAL-DEFINE PClassDRAutoAssignCodeCode YES
&GLOBAL-DEFINE startup                    sign_ank.st~032
&GLOBAL-DEFINE mylookup                   sign_ank.nau
&GLOBAL-DEFINE PClassDRFormTrgFile        sign_ank.trg
&GLOBAL-DEFINE sort-oqry0                 BY code.val


{intrface.get xclass} 

/*
{nextnum.i}
&GLOBAL-DEFINE DRVarCode{&num}     code.misc[1]
&GLOBAL-DEFINE DRVarType{&num}     CHARACTER
&GLOBAL-DEFINE DRVarFormat{&num}   x(4)
&GLOBAL-DEFINE DRVarEFormat{&num}  x(4)
&GLOBAL-DEFINE DRVarBFormat{&num}  x(4)
&GLOBAL-DEFINE DRVarViewAs{&num}   FILL-IN SIZE 4 BY 1
&GLOBAL-DEFINE DRVarBViewAs{&num}  FILL-IN SIZE 8 BY 1
&GLOBAL-DEFINE DRVarLabel{&num}    ���!���ࠧ.
&GLOBAL-DEFINE DRVarELabel{&num}   ��� ���ࠧ�������
&GLOBAL-DEFINE DRVarColLabel{&num} ���
&GLOBAL-DEFINE DRVarHelp{&num}     ���

&GLOBAL-DEFINE DRVarViewAs{&num}   EDITOR INNER-CHARS 32 INNER-LINES 4
&GLOBAL-DEFINE DRVarBViewAs{&num}  FILL-IN SIZE 25 BY 1
*/

{nextnum.i}
&GLOBAL-DEFINE DRVarCode{&num}     code.val
&GLOBAL-DEFINE DRVarType{&num}     CHARACTER
&GLOBAL-DEFINE DRVarFormat{&num}   x(4)
&GLOBAL-DEFINE DRVarEFormat{&num}  x(4)
&GLOBAL-DEFINE DRVarBFormat{&num}  x(4)
&GLOBAL-DEFINE DRVarViewAs{&num}   FILL-IN SIZE 4 BY 1
&GLOBAL-DEFINE DRVarBViewAs{&num}  FILL-IN SIZE 8 BY 1
&GLOBAL-DEFINE DRVarLabel{&num}    ��� ���ࠧ�������
&GLOBAL-DEFINE DRVarELabel{&num}   ��� ���ࠧ�������
&GLOBAL-DEFINE DRVarColLabel{&num} ���
&GLOBAL-DEFINE DRVarHelp{&num}     ���

{nextnum.i}
&GLOBAL-DEFINE DRVarCode{&num}     code.misc[1]
&GLOBAL-DEFINE DRVarType{&num}     CHARACTER
&GLOBAL-DEFINE DRVarFormat{&num}   99/99/9999
&GLOBAL-DEFINE DRVarEFormat{&num}  99/99/9999
&GLOBAL-DEFINE DRVarBFormat{&num}  99/99/9999
&GLOBAL-DEFINE DRVarViewAs{&num}   FILL-IN SIZE 10 BY 1
&GLOBAL-DEFINE DRVarBViewAs{&num}  FILL-IN SIZE 12 BY 1
&GLOBAL-DEFINE DRVarLabel{&num}    ���!��砫�
&GLOBAL-DEFINE DRVarELabel{&num}   ��� ��砫�
&GLOBAL-DEFINE DRVarColLabel{&num} ��� ��砫�
&GLOBAL-DEFINE DRVarHelp{&num}     ��� ��砫�

{nextnum.i}
&GLOBAL-DEFINE DRVarCode{&num}     code.misc[2]
&GLOBAL-DEFINE DRVarType{&num}     CHARACTER
&GLOBAL-DEFINE DRVarFormat{&num}   99/99/9999
&GLOBAL-DEFINE DRVarEFormat{&num}  99/99/9999
&GLOBAL-DEFINE DRVarBFormat{&num}  99/99/9999
&GLOBAL-DEFINE DRVarViewAs{&num}   FILL-IN SIZE 10 BY 1
&GLOBAL-DEFINE DRVarBViewAs{&num}  FILL-IN SIZE 12 BY 1
&GLOBAL-DEFINE DRVarLabel{&num}    ��� ����砭��
&GLOBAL-DEFINE DRVarELabel{&num}   ��� ����砭��
&GLOBAL-DEFINE DRVarColLabel{&num} ��� ����砭��
&GLOBAL-DEFINE DRVarHelp{&num}     ��� ����砭��


{nextnum.i}
&GLOBAL-DEFINE DRVarCode{&num}     code.misc[3]
&GLOBAL-DEFINE DRVarType{&num}     CHARACTER
&GLOBAL-DEFINE DRVarFormat{&num}   x(300)
&GLOBAL-DEFINE DRVarEFormat{&num}  x(300)
&GLOBAL-DEFINE DRVarBFormat{&num}  x(300)
&GLOBAL-DEFINE DRVarViewAs{&num}   FILL-IN SIZE 23 BY 1
&GLOBAL-DEFINE DRVarBViewAs{&num}  FILL-IN SIZE 23 BY 1
&GLOBAL-DEFINE DRVarLabel{&num}    ���������
&GLOBAL-DEFINE DRVarELabel{&num}   ���������
&GLOBAL-DEFINE DRVarColLabel{&num} ���������
&GLOBAL-DEFINE DRVarHelp{&num}     ���������

{nextnum.i}
&GLOBAL-DEFINE DRVarCode{&num}     code.name
&GLOBAL-DEFINE DRVarType{&num}     CHARACTER
&GLOBAL-DEFINE DRVarFormat{&num}   x(70))
&GLOBAL-DEFINE DRVarEFormat{&num}  x(70)
&GLOBAL-DEFINE DRVarBFormat{&num}  x(70)
&GLOBAL-DEFINE DRVarViewAs{&num}   FILL-IN SIZE 20 BY 1
&GLOBAL-DEFINE DRVarBViewAs{&num}  FILL-IN SIZE 27 BY 1
&GLOBAL-DEFINE DRVarLabel{&num}    ����㤭��                
&GLOBAL-DEFINE DRVarELabel{&num}   ����㤭��            
&GLOBAL-DEFINE DRVarColLabel{&num} ����㤭��            
&GLOBAL-DEFINE DRVarHelp{&num}     ����㤭��           


DEFINE VARIABLE mSBalAcct   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mFrameIndex AS INT64     NO-UNDO.

{pclassdr.p}

RETURN.
/*
DEFINE VARIABLE ms       AS CHARACTER          NO-UNDO.
  
    ms  = GetCodeMisc("�����ᠭ�_���", "0000", 1).
 MESSAGE ms VIEW-AS ALERT-BOX .
*/


FINALLY:
   {intrface.del}
END FINALLY.
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='22/12/2015 18:35:57.715+04:00' */
/* $LINTUSER='krok' */
/* $LINTMODE='1' */
/* $LINTFILE='brw446psym.p' */
/*prosignNR8pPp1hn/55UI/OPSusqw*/