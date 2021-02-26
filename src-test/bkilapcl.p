/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2006 ТОО "Банковские информационные системы"
     Filename: bkilapcl.p
      Comment: Типы кредитных договоров в НБКИ
   Parameters:
         Uses:
      Used by:
      Created: 24/04/06 ZIAL (0060494) БКИ. Разработка классификаторов для модуля 
               Кредиты и Депозиты
     Modified: 26/04/06 ZIAL (0060494) БКИ. Разработка классификаторов для модуля 
               Кредиты и Депозиты
     Modified: 
*/

&GLOBAL-DEFINE form-brw1 bkilapcl.br1
&GLOBAL-DEFINE bf11 "code.code code.name code.misc[1] code.misc[2]"
&GLOBAL-DEFINE form-edit bkilapcl.edf
&GLOBAL-DEFINE postfind1 pclassdu.fnd
&GLOBAL-DEFINE lookup1 bkilapcl.nau

{pclass.p bkilapcl.uf bkilapcl.nav}
