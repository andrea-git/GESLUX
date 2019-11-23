       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      esiord.
       AUTHOR.                          Andrea.
       REMARKS. Batch che imposta lo stato degli esiti dal 2012 su
                tutti gli ordini con la stessa chiave , ma col
                campo di riferimento non valorizzato (tor-esito-consegna)
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.                          
           copy "tordini.sl".
           copy "eordini.sl".

      ******************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tordini.fd".
           copy "eordini.fd".


      ******************************************************************
       WORKING-STORAGE SECTION.
           COPY "acucobol.def".
           copy "comune.def".
                                                 
       78  titolo    value "GESLUX - Impostazione esiti ordini".
       77  status-tordini       pic xx.
       77  status-eordini       pic xx.                

      ******************************************************************
       PROCEDURE DIVISION.
       DECLARATIVES.

      ***---
       TORDINI-ERR SECTION.
           use after error procedure on tordini.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tordini
           when "39"
                set errori to true
                display message "File [TORDINI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TORDINI] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message "File [TORDINI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.  

      ***---
       EORDINI-ERR SECTION.
           use after error procedure on eordini.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-eordini
           when "39"
                set errori to true
                display message "File [EORDINI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[EORDINI] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message "File [EORDINI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.  

       END DECLARATIVES.

       MAIN-PRG.
           perform INIT.
           perform OPEN-FILES.
           if tutto-ok
              perform ELABORAZIONE
              perform CLOSE-FILES
           end-if.
           perform EXIT-PGM.

      ***---
       INIT.
           set RecLocked to false.
           set tutto-ok  to true.

      ***---
       OPEN-FILES.
           open i-o   tordini 
           open input eordini.

      ***---
       ELABORAZIONE.
           move low-value to eor-rec.
           move 2012 to eor-anno.
           start eordini key >= eor-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read eordini next at end exit perform end-read
                    move eor-tor-chiave to tor-chiave                
                    read tordini no lock
                         invalid continue
                     not invalid                                
                         move eor-esito to tor-esito-consegna
                         rewrite tor-rec invalid continue end-rewrite
                    end-read
                 end-perform
           end-start.                        


      ***---
       CLOSE-FILES.
           close tordini eordini.

      ***---
       EXIT-PGM.
           display message "Elaborazione terminata!"
                     title titolo.
           goback.
