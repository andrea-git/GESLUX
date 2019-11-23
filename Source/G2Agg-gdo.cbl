       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      g2agg-gdo.
       REMARKS. Aggiorna massivamente i gruppi GDO GESLUX in G2
                e il valore del codice-CS sia nei clienti GESLUX che in G2
       SPECIAL-NAMES. decimal-point is comma.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.     
      *    Files SSI             
           copy "clienti.sl".
           copy "tgrupgdo.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.     
      *    Files SSI               
           copy "clienti.fd".
           copy "tgrupgdo.fd".

       WORKING-STORAGE SECTION.
           COPY "link-G2agg.def".

      * Status Files SSI           
       77  status-clienti      pic x(2).
       77  status-tgrupgdo     pic x(2).
                              
       77 n-gruppi             pic 9(4)  value 0.
       77 num                  pic 999.
       77 lettera              pic x.
       77 n-cli                pic 9(10) value 0.

       78  titolo            value "Aggiornamento gruppi GDO".

      ******************************************************************
       PROCEDURE DIVISION.
           open i-o tgrupgdo.                
           move "A" to lettera.
           move low-value to gdo-rec.
           start tgrupgdo key >= gdo-chiave
           perform until 1 = 2
              read tgrupgdo next at end exit perform end-read
              add 1 to n-gruppi          
              if gdo-codice-G2 = spaces    
                 if num = 100
                    move 0 to num
                    perform VALORIZZA-LETTERA
                 end-if
                 move lettera  to gdo-codice-G2(1:1)
                 move num(2:2) to gdo-codice-G2(2:2)
                 rewrite gdo-rec           
                 add 1 to num
              end-if
           end-perform.
           close      tgrupgdo.
           open input tgrupgdo.

           open input clienti. 
           move 0 to n-gruppi.
           move low-value to gdo-rec.
           start tgrupgdo key >= gdo-chiave
           perform until 1 = 2
              read tgrupgdo next at end exit perform end-read
              if gdo-codice-G2 = spaces
                 add 1 to n-gruppi                                       
                 initialize G2Agg-linkage 
                 set G2Agg-gdo   to true
                 move gdo-codice to G2Agg-codice-gdo
                 set G2Agg-insert to true
                 call   "G2Agg" using G2Agg-linkage
                 cancel "G2Agg"   
              end-if
           end-perform.                                     
           move low-value to cli-rec.
           set cli-tipo-C to true.
           start clienti key >= cli-chiave
           perform until 1 = 2
              read clienti next at end exit perform end-read
              if cli-tipo-F exit perform end-if
              if cli-gdo not = spaces
                 add 1 to n-cli
                 initialize G2Agg-linkage 
                 set G2Agg-cli   to true
                 move cli-codice to G2Agg-codice
                 set G2Agg-update to true
                 call   "G2Agg" using G2Agg-linkage
                 cancel "G2Agg"   
              end-if
           end-perform.                                     
           close tgrupgdo.
           close clienti.

           display message "Elaborazione terminata"
                    x"0d0a"n-gruppi " Gruppi GDO"
                    x"0d0a"n-cli    " clienti"
                      title titolo
           goback.

      ***---
       VALORIZZA-LETTERA.
           evaluate lettera            
           when "A" move "B" to lettera
           when "B" move "C" to lettera
           when "C" move "D" to lettera
           when "D" move "E" to lettera
           when "E" move "F" to lettera
           when "F" move "G" to lettera
           when "G" move "H" to lettera
           when "H" move "I" to lettera
           when "I" move "J" to lettera
           when "J" move "K" to lettera
           when "K" move "L" to lettera
           when "L" move "M" to lettera
           when "M" move "N" to lettera
           when "N" move "O" to lettera
           when "O" move "P" to lettera
           when "P" move "Q" to lettera
           when "Q" move "R" to lettera
           when "R" move "S" to lettera
           when "S" move "T" to lettera
           when "T" move "U" to lettera
           when "U" move "V" to lettera
           when "V" move "W" to lettera
           when "W" move "X" to lettera
           when "X" move "Y" to lettera
           when "Y" move "Z" to lettera
           when "Z" move "a" to lettera            
           when "a" move "B" to lettera
           when "b" move "c" to lettera
           when "C" move "d" to lettera
           when "d" move "e" to lettera
           when "e" move "f" to lettera
           when "f" move "g" to lettera
           when "g" move "h" to lettera
           when "H" move "i" to lettera
           when "i" move "j" to lettera
           when "j" move "k" to lettera
           when "k" move "l" to lettera
           when "l" move "m" to lettera
           when "m" move "n" to lettera
           when "n" move "o" to lettera
           when "o" move "p" to lettera
           when "p" move "q" to lettera
           when "q" move "r" to lettera
           when "r" move "s" to lettera
           when "s" move "t" to lettera
           when "t" move "u" to lettera
           when "u" move "v" to lettera
           when "v" move "w" to lettera
           when "w" move "x" to lettera
           when "x" move "y" to lettera
           when "y" move "z" to lettera
           when "z" move " " to lettera
           end-evaluate.
