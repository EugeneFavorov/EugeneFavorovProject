{globals.i}
{intrface.get tmess}

/* +++ pb_poststatus.p was humbly modified by (c)blodd converter v.1.11 on 7/27/2017 7:11am +++ */

/**
����᪨� �ࠢ� �ਭ�������: ��� ���� ����
�᭮�����:      �� �����.511
�� ������:     ����᪠�� ��楤��� ���� ��ࠡ�⪨ �� ᬥ�� �����
��� ࠡ�⠥�:   
��ࠬ����:      
���� ����᪠:  chst-opa.p
������:         06.04.2017 ���ᮢ �.�.
*/

{intrface.get xclass}

DEFINE PARAMETER BUFFER iop     FOR op.

DEFINE VARIABLE cProc       AS CHARACTER    NO-UNDO.    /* ��楤�� ���� ��ࠡ�⪨ */
DEFINE BUFFER   grCode      FOR code. 

cProc = GetCodeDesc("�����", iop.op-status, 2, "").

FOR FIRST code
    WHERE (code.class   EQ "��楤��늮���")
      AND (code.code    EQ cProc)
    NO-LOCK:

    IF {assigned code.val}
    THEN DO:    /* ��楤�� */
        RUN VALUE(code.val + ".p") (iop.op, code.description[1]).
    END.
    ELSE DO:    /* ��㯯� ��楤�� */
        FOR EACH grCode
            WHERE (grCode.class     EQ "��楤��늮���")
              AND (grCode.parent    EQ cProc)
              AND (grCode.val       NE "")
            NO-LOCK:

            RUN VALUE(grCode.val + ".p") (iop.op, grCode.description[1]).
        END.
    END.      
END.
{intrface.del}

/* --- pb_poststatus.p was humbly modified by (c)blodd converter v.1.11 on 7/27/2017 7:11am --- */
