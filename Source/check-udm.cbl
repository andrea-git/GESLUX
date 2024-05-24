       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      check-udm.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.            

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.            

       WORKING-STORAGE SECTION.
       77  statusPgm               signed-short.
       77  como-udm                pic x(9).   

       77  como-int                pic x(4).   
       77  como-dec                pic x(3).   

       77  totCharUdm              pic 9(4).
       77  charUdm                 pic 9(4). 
       77  charSep                 pic 9(2) value 0.
                                          
       77  nComma                  pic 99.
       77  nDot                    pic 99.

       LINKAGE SECTION.                     
       77  udm-old                 pic x(9).
       77  udm-new                 pic x(9).

      ******************************************************************
       PROCEDURE DIVISION USING udm-old udm-new.
       MAIN.
           move 0 to statusPgm.
           move spaces to udm-new.              
           move udm-old to como-udm.
           call "C$JUSTIFY" using como-udm, "L".
           inspect como-udm replacing leading x"30" by x"20".
           call "C$JUSTIFY" using como-udm, "L".

           move spaces to como-dec.
           move 0 to totCharUdm nComma nDot.

           inspect como-udm 
                   replacing trailing spaces by low-value
           inspect como-udm tallying totCharUdm 
                   for characters before low-value       
           inspect como-udm 
                   replacing trailing low-value by spaces
                                 
           perform CONTROLLO-UDM.
           if statusPgm = 0
              perform CONVERSIONE-UDM
           end-if.

           goback statusPgm.
                     
      ***---
       CONTROLLO-UDM.                             
           perform varying charUdm from 1 by 1 
                     until charUdm > totCharUdm
              if not ( como-udm(charUdm:1) = "1" or "2" or "3" or "4" or
                                             "5" or "6" or "7" or "8" or
                                             "9" or "." or "," )
                 move -1 to statusPgm
                 exit perform
              end-if
              if como-udm(charUdm:1) = ","
                 add 1 to nComma
                 move charUdm to charSep
              end-if
              if como-udm(charUdm:1) = "."
                 add 1 to nDot          
                 move charUdm to charSep
              end-if
           end-perform.
           if ( nComma > 1 or nDot > 1 ) or
              ( nComma = 1 and nDot = 1 )
              move -1 to statusPgm
           end-if.

      ***---
       CONVERSIONE-UDM.                   
           if charSep = 0 
              add 1 to totCharUdm giving charSep |Lo simulo 
           end-if.
                                       
           if charSep = 1
              move "0" to como-int     
              subtract 1 from charSep
           else                      
              subtract 1 from charSep
              move como-udm(1:charSep) to como-int
              if not (como-int = "0" or "00" or "000")
                 call "C$JUSTIFY" using como-int, "L"
                 inspect como-int replacing leading x"30" by x"20"
                 call "C$JUSTIFY" using como-int, "L"
              end-if
           end-if.
           inspect como-int replacing trailing spaces by low-value.

           add 2 to charSep.
           move como-udm(charSep:) to como-dec.
           inspect como-dec replacing trailing x"20" by x"30".
                                                     
           string como-int delimited low-value
                  ","      delimited size
                  como-dec delimited size
             into udm-new
           end-string.           
