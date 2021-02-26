/* 
               Банковская интегрированная система БИСквит 
    Copyright: (C) 1992-2017 АО "Банковские информационные системы" 
     Filename: POSTJKH.P
      Comment: Программа просмотра отчетных данных 
               по классу PostJKU (Поставщики ЖКУ), 
   Parameters: НЕТ
      Created: 28/04/16 VASOV
     Modified: 
*/  

DEFINE BUFFER sDataBlock FOR DataBlock.
DEFINE BUFFER sDataLine  FOR DataLine.


{sv-cqr#.i 
    &prg        = "fraudu"
    &return     = "ret-surr.cqr "
    &ProgressFile = "'DataLine'"
}

RETURN "".

