/* ������ ��� ���� �⮣�� ��� �� �⤥�����                     */
/* �ਭ����� 2 ��ࠬ��� city-id � kko ��� ���᪠ �⮣�� �� �⤥�����*/

/* ����塞 ��६����*/
ASSIGN
    mSum-b-all       = 0.00 /* ���� �㬬� ������*/
    mSum-o-all       = 0.00 /* ���� �㬬� ���������*/
    mSum-val-b-all   = 0.00 /* ���� �㬬� ����� ������*/
    mSum-val-o-all   = 0.00 /* ���� �㬬� ����� ���������*/

    mSum-e-b-all     = 0.00 /* ���� �㬬� ���஭�� ������*/
    mSum-e-o-all     = 0.00 /* ���� �㬬� ���஭�� ���������*/
    mSum-e-val-b-all = 0.00 /* ���� �㬬� ���஭�� ����� ������*/
    mSum-e-val-o-all = 0.00 /* ���� �㬬� ���஭�� ����� ���������*/

    mKas-b-all       = 0.00 /* ���� �㬬� ���ᮢ� ������*/
    mKas-o-all       = 0.00 /* ���� �㬬� ���ᮢ� ���������*/
    mKas-val-b-all   = 0.00 /* ���� �㬬� ���ᮢ� ����� ������*/
    mKas-val-o-all   = 0.00 /* ���� �㬬� ���ᮢ� ����� ���������*/

    mDd-b-all       = 0.00 /* ���� �㬬� ���㬥��� ��� ������*/
    mDd-o-all       = 0.00 /* ���� �㬬� ���㬥��� ��� ���������*/
    mDd-val-b-all   = 0.00 /* ���� �㬬� ���㬥��� ��� ����� ������*/
    mDd-val-o-all   = 0.00 /* ���� �㬬� ���㬥��� ��� ����� ���������*/ 

    mAg-p-b-all      = 0.00 /* ���� �㬬� �����᪨� �㬠�� ������*/
    mAg-p-o-all      = 0.00 /* ���� �㬬� �����᪨� �㬠�� ���������*/
    mAg-p-val-b-all  = 0.00 /* ���� �㬬� �����᪨� �㬠�� ����� ������*/
    mAg-p-val-o-all  = 0.00 /* ���� �㬬� �����᪨� �㬠�� ����� ���������*/

    mOt-p-b-all      = 0.00 /* ���� �㬬� ���� �㬠�� ������*/
    mOt-p-o-all      = 0.00 /* ���� �㬬� ���� �㬠�� ���������*/
    mOt-p-val-b-all  = 0.00 /* ���� �㬬� ���� �㬠�� ����� ������*/
    mOt-p-val-o-all  = 0.00 /* ���� �㬬� ���� �㬠�� ����� ���������*/

    mHoz-p-b-all     = 0.00 /* ���� �㬬� 宧.���. �㬠�� ������*/
    mHoz-p-o-all     = 0.00 /* ���� �㬬� 宧.���. �㬠�� ���������*/
    mHoz-p-val-b-all = 0.00 /* ���� �㬬� 宧.���. �㬠�� ����� ������*/
    mHoz-p-val-o-all = 0.00 /* ���� �㬬� 宧.���. �㬠�� ����� ���������*/

    mAg-e-b-all      = 0.00 /* ���� �㬬� �����᪨� ���஭�� ������*/
    mAg-e-o-all      = 0.00 /* ���� �㬬� �����᪨� ���஭�� ���������*/
    mAg-e-val-b-all  = 0.00 /* ���� �㬬� �����᪨� ���஭�� ����� ������*/
    mAg-e-val-o-all  = 0.00 /* ���� �㬬� �����᪨� ���஭�� ����� ���������*/

    mOt-e-b-all      = 0.00 /* ���� �㬬� ���� ���஭�� ������*/
    mOt-e-o-all      = 0.00 /* ���� �㬬� ���� ���஭�� ���������*/
    mOt-e-val-b-all  = 0.00 /* ���� �㬬� ���� ���஭�� ����� ������*/
    mOt-e-val-o-all  = 0.00 /* ���� �㬬� ���� ���஭�� ����� ���������*/

    mHoz-e-b-all     = 0.00 /* ���� �㬬� 宧.���. ���஭�� ������*/
    mHoz-e-o-all     = 0.00 /* ���� �㬬� 宧.���. ���஭�� ���������*/
    mHoz-e-val-b-all = 0.00 /* ���� �㬬� 宧.���. ���஭�� ����� ������*/
    mHoz-e-val-o-all = 0.00 /* ���� �㬬� 宧.���. ���஭�� ����� ���������*/

    mBuh-b-all       = 0.00 /* ���� �㬬� �㣠���᪨� ������*/
    mBuh-o-all       = 0.00 /* ���� �㬬� �㣠���᪨� ���������*/
    mBuh-val-b-all   = 0.00 /* ���� �㬬� �㣠���᪨� ����� ������*/
    mBuh-val-o-all   = 0.00 /* ���� �㬬� �㣠���᪨� ����� ���������*/

    mBuh-aut-b-all   = 0.00 /* ���� �㬬� ��⮬���᪨� �࠭���樨 ������*/
    mBuh-aut-o-all   = 0.00 /* ���� �㬬� ��⮬���᪨� �࠭���樨 ���������*/
    mBuh-auv-b-all   = 0.00 /* ���� �㬬� ��⮬���᪨� �࠭���樨 ����� ������*/
    mBuh-auv-o-all   = 0.00 /* ���� �㬬� ��⮬���᪨� �࠭���樨 ����� ���������*/
    .

/*�����뢠�� �⮣�*/

/* ��騥 �⮣� */
FOR EACH tt-day-itog 
    WHERE tt-day-itog.acct-cat EQ "b"
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mSum-b-all = mSum-b-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog
    WHERE tt-day-itog.acct-cat EQ "o"
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mSum-o-all = mSum-o-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog 
    WHERE tt-day-itog.acct-cat EQ "b"
    AND   tt-day-itog.currency EQ 'VAL'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mSum-val-b-all = mSum-val-b-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog
    WHERE tt-day-itog.acct-cat EQ "o"
    AND   tt-day-itog.currency EQ 'VAL'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mSum-val-o-all = mSum-val-o-all + tt-day-itog.itog.
END.
/**/


/* ��騥 �⮣� ���஭�� */
FOR EACH tt-day-itog 
    WHERE tt-day-itog.acct-cat EQ "b"
    AND   tt-day-itog.save-type EQ 'e'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mSum-e-b-all = mSum-e-b-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog
    WHERE tt-day-itog.acct-cat EQ "o"
    AND   tt-day-itog.save-type EQ 'e'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mSum-e-o-all = mSum-e-o-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog 
    WHERE tt-day-itog.acct-cat EQ "b"
    AND   tt-day-itog.save-type EQ 'e'
    AND   tt-day-itog.currency EQ 'VAL'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mSum-e-val-b-all = mSum-e-val-b-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog
    WHERE tt-day-itog.acct-cat EQ "o"
    AND   tt-day-itog.save-type EQ 'e'
    AND   tt-day-itog.currency EQ 'VAL'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mSum-e-val-o-all = mSum-e-val-o-all + tt-day-itog.itog.
END.
/**/


/* ���ᮢ� ���㬥��� */
FOR EACH tt-day-itog 
    WHERE tt-day-itog.acct-cat EQ "b"
    AND   tt-day-itog.razdel EQ 'k'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mKas-b-all = mKas-b-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog
    WHERE tt-day-itog.acct-cat EQ "o"
    AND   tt-day-itog.razdel EQ 'k'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mKas-o-all = mKas-o-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog 
    WHERE tt-day-itog.acct-cat EQ "b"
    AND   tt-day-itog.razdel EQ 'k'
    AND   tt-day-itog.currency EQ 'VAL'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mKas-val-b-all = mKas-val-b-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog
    WHERE tt-day-itog.acct-cat EQ "o"
    AND   tt-day-itog.razdel EQ 'k'
    AND   tt-day-itog.currency EQ 'VAL'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mKas-val-o-all = mKas-val-o-all + tt-day-itog.itog.
END.
/**/


/* ���㬥��� ��� */
FOR EACH tt-day-itog 
    WHERE tt-day-itog.acct-cat EQ "b"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'p'
    AND   tt-day-itog.shiv EQ '�����'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mDd-b-all = mDd-b-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog
    WHERE tt-day-itog.acct-cat EQ "o"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'p'
    AND   tt-day-itog.shiv EQ '�����'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mDd-o-all = mDd-o-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog 
    WHERE tt-day-itog.acct-cat EQ "b"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'p'
    AND   tt-day-itog.shiv EQ '�����'
    AND   tt-day-itog.currency EQ 'VAL'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mDd-val-b-all = mDd-val-b-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog
    WHERE tt-day-itog.acct-cat EQ "o"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'p'
    AND   tt-day-itog.shiv EQ '�����'
    AND   tt-day-itog.currency EQ 'VAL'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mDd-val-o-all = mDd-val-o-all + tt-day-itog.itog.
END.
/**/


/* �����᪨� �㬠�� */
FOR EACH tt-day-itog 
    WHERE tt-day-itog.acct-cat EQ "b"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'p'
    AND   tt-day-itog.shiv EQ '�����'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mAg-p-b-all = mAg-p-b-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog
    WHERE tt-day-itog.acct-cat EQ "o"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'p'
    AND   tt-day-itog.shiv EQ '�����'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mAg-p-o-all = mAg-p-o-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog 
    WHERE tt-day-itog.acct-cat EQ "b"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'p'
    AND   tt-day-itog.shiv EQ '�����'
    AND   tt-day-itog.currency EQ 'VAL'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mAg-p-val-b-all = mAg-p-val-b-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog
    WHERE tt-day-itog.acct-cat EQ "o"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'p'
    AND   tt-day-itog.shiv EQ '�����'
    AND   tt-day-itog.currency EQ 'VAL'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mAg-p-val-o-all = mAg-p-val-o-all + tt-day-itog.itog.
END.
/**/


/*�������� �㬬� �㬠��*/
FOR EACH tt-day-itog 
    WHERE tt-day-itog.acct-cat EQ "b"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'p'
    AND   tt-day-itog.shiv EQ '�������'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mOt-p-b-all = mOt-p-b-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog
    WHERE tt-day-itog.acct-cat EQ "o"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'p'
    AND   tt-day-itog.shiv EQ '�������'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mOt-p-o-all = mOt-p-o-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog 
    WHERE tt-day-itog.acct-cat EQ "b"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'p'
    AND   tt-day-itog.shiv EQ '�������'
    AND   tt-day-itog.currency EQ 'VAL'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mOt-p-val-b-all = mOt-p-val-b-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog
    WHERE tt-day-itog.acct-cat EQ "o"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'p'
    AND   tt-day-itog.shiv EQ '�������'
    AND   tt-day-itog.currency EQ 'VAL'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mOt-p-val-o-all = mOt-p-val-o-all + tt-day-itog.itog.
END.
/**/


/*宧���㬥��� �㬠��*/
FOR EACH tt-day-itog 
    WHERE tt-day-itog.acct-cat EQ "b"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'p'
    AND   tt-day-itog.shiv EQ '������'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mHoz-p-b-all = mHoz-p-b-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog
    WHERE tt-day-itog.acct-cat EQ "o"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'p'
    AND   tt-day-itog.shiv EQ '������'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mHoz-p-o-all = mHoz-p-o-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog 
    WHERE tt-day-itog.acct-cat EQ "b"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'p'
    AND   tt-day-itog.shiv EQ '������'
    AND   tt-day-itog.currency EQ 'VAL'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mHoz-p-val-b-all = mHoz-p-val-b-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog
    WHERE tt-day-itog.acct-cat EQ "o"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'p'
    AND   tt-day-itog.shiv EQ '������'
    AND   tt-day-itog.currency EQ 'VAL'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mHoz-p-val-o-all = mHoz-p-val-o-all + tt-day-itog.itog.
END.
/**/


/*�����᪨� ���஭��*/
FOR EACH tt-day-itog 
    WHERE tt-day-itog.acct-cat EQ "b"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'e'
    AND   tt-day-itog.shiv EQ '�����'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mAg-e-b-all = mAg-e-b-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog
    WHERE tt-day-itog.acct-cat EQ "o"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'e'
    AND   tt-day-itog.shiv EQ '�����'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mAg-e-o-all = mAg-e-o-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog 
    WHERE tt-day-itog.acct-cat EQ "b"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'e'
    AND   tt-day-itog.shiv EQ '�����'
    AND   tt-day-itog.currency EQ 'VAL'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mAg-e-val-b-all = mAg-e-val-b-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog
    WHERE tt-day-itog.acct-cat EQ "o"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'e'
    AND   tt-day-itog.shiv EQ '�����'
    AND   tt-day-itog.currency EQ 'VAL'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mAg-e-val-o-all = mAg-e-val-o-all + tt-day-itog.itog.
END.
/**/


/*�������� ���஭��*/
FOR EACH tt-day-itog 
    WHERE tt-day-itog.acct-cat EQ "b"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'e'
    AND   tt-day-itog.shiv EQ '�������'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mOt-e-b-all = mOt-e-b-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog
    WHERE tt-day-itog.acct-cat EQ "o"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'e'
    AND   tt-day-itog.shiv EQ '�������'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mOt-e-o-all = mOt-e-o-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog 
    WHERE tt-day-itog.acct-cat EQ "b"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'e'
    AND   tt-day-itog.shiv EQ '�������'
    AND   tt-day-itog.currency EQ 'VAL'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mOt-e-val-b-all = mOt-e-val-b-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog
    WHERE tt-day-itog.acct-cat EQ "o"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'e'
    AND   tt-day-itog.shiv EQ '�������'
    AND   tt-day-itog.currency EQ 'VAL'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mOt-e-val-o-all = mOt-e-val-o-all + tt-day-itog.itog.
END.
/**/


/*宧��� ���஭��*/
FOR EACH tt-day-itog 
    WHERE tt-day-itog.acct-cat EQ "b"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'e'
    AND   tt-day-itog.shiv EQ '������'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mHoz-e-b-all = mHoz-e-b-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog
    WHERE tt-day-itog.acct-cat EQ "o"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'e'
    AND   tt-day-itog.shiv EQ '������'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mHoz-e-o-all = mHoz-e-o-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog 
    WHERE tt-day-itog.acct-cat EQ "b"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'e'
    AND   tt-day-itog.shiv EQ '������'
    AND   tt-day-itog.currency EQ 'VAL'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mHoz-e-val-b-all = mHoz-e-val-b-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog
    WHERE tt-day-itog.acct-cat EQ "o"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'e'
    AND   tt-day-itog.shiv EQ '������'
    AND   tt-day-itog.currency EQ 'VAL'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mHoz-e-val-o-all = mHoz-e-val-o-all + tt-day-itog.itog.
END.
/**/


/* ��壠���᪨� ���㬥��� */
FOR EACH tt-day-itog 
    WHERE tt-day-itog.acct-cat EQ "b"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'e'
    AND   tt-day-itog.shiv EQ '�����'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mBuh-b-all = mBuh-b-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog
    WHERE tt-day-itog.acct-cat EQ "o"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'e'
    AND   tt-day-itog.shiv EQ '�����'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mBuh-o-all = mBuh-o-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog 
    WHERE tt-day-itog.acct-cat EQ "b"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'e'
    AND   tt-day-itog.shiv EQ '�����'
    AND   tt-day-itog.currency EQ 'VAL'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mBuh-val-b-all = mBuh-val-b-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog
    WHERE tt-day-itog.acct-cat EQ "o"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'e'
    AND   tt-day-itog.shiv EQ '�����'
    AND   tt-day-itog.currency EQ 'VAL'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mBuh-val-o-all = mBuh-val-o-all + tt-day-itog.itog.
END.
/**/


/* ��⮬���᪨� �࠭���樨 */
/*
FOR EACH tt-day-itog 
    WHERE tt-day-itog.acct-cat EQ "b"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'e'
    AND   tt-day-itog.shiv EQ '�����'
    AND   tt-day-itog.auto-t
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mBuh-aut-b-all = mBuh-aut-b-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog
    WHERE tt-day-itog.acct-cat EQ "o"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'e'
    AND   tt-day-itog.shiv EQ '�����'
    AND   tt-day-itog.auto-t
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mBuh-aut-o-all = mBuh-aut-o-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog 
    WHERE tt-day-itog.acct-cat EQ "b"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'e'
    AND   tt-day-itog.shiv EQ '�����'
    AND   tt-day-itog.auto-t
    AND   tt-day-itog.currency EQ 'VAL'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
   :
    mBuh-auv-b-all = mBuh-auv-b-all + tt-day-itog.itog.
END.

FOR EACH tt-day-itog
    WHERE tt-day-itog.acct-cat EQ "o"
    AND   tt-day-itog.razdel EQ 'b'
    AND   tt-day-itog.save-type EQ 'e'
    AND   tt-day-itog.shiv EQ '�����'
    AND   tt-day-itog.auto-t
    AND   tt-day-itog.currency EQ 'VAL'
  &IF DEFINED(kko) &THEN
    AND   tt-day-itog.kko = {&kko}
  &ENDIF
    :
    mBuh-auv-o-all = mBuh-auv-o-all + tt-day-itog.itog.
END.*/
/**/