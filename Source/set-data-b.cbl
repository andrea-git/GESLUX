       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      set-data-b.
       AUTHOR.                          Andrea.
       REMARKS.
           Serve per "lavorare" sulla data bolla effettiva.

      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tordini.sl".

      *****************************************************************

       DATA DIVISION.
       FILE SECTION.
           copy "tordini.fd".

      *****************************************************************

       WORKING-STORAGE SECTION.
      * COPY

      * COSTANTI
       78  titolo value "Data Bolla Effettiva".

      * FILE-STATUS
       77  status-tordini            pic xx.

      * VARIABILI

      * FLAGS
       01  controlli                 pic xx.
         88 tutto-ok                 value "OK".
         88 errori                   value "ER".

      *****************************************************************

       PROCEDURE DIVISION.

       DECLARATIVES.
      ***---
       TORDINI-ERR SECTION.
           use after error procedure on tordini.
           set tutto-ok  to true.
           evaluate status-tordini
           when "35"
                set errori to true
                display message "File [TORDINI] not found!"
                          title titolo
                           icon 3
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
           end-evaluate.

       END DECLARATIVES.

      ***---
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
           set tutto-ok to true.

      ***---
       OPEN-FILES.
           open i-o tordini.

      ***---
       ELABORAZIONE.
           move low-value to tor-rec.
           move 2009  to tor-anno-bolla.
           move 37109 to tor-num-bolla.

           start tordini key >= k-bolla
                 invalid continue
           end-start.
           perform until 1 = 2 
              read tordini next at end exit perform end-read
              if tor-num-bolla < 37109 or
                 tor-num-bolla > 37383 or
                 tor-anno-bolla not = 2009
                 exit perform
              end-if
              if tor-data-bolla = 20090930
                 move 20091015       to tor-data-bolla-effettiva
              else
                 move tor-data-bolla to tor-data-bolla-effettiva
              end-if
              rewrite tor-rec invalid continue end-rewrite
           end-perform.

           move low-value to tor-rec.
           move 2009   to tor-anno-bolla.
           move 907145 to tor-num-bolla.

           start tordini key >= k-bolla
                 invalid continue
           end-start.
           perform until 1 = 2 
              read tordini next at end exit perform end-read
              if tor-num-bolla < 907145 or
                 tor-num-bolla > 907721 or
                 tor-anno-bolla not = 2009
                 exit perform
              end-if
              move tor-data-bolla to tor-data-bolla-effettiva
              rewrite tor-rec invalid continue end-rewrite
           end-perform.
           display message "Operazione terminata!"
                     title titolo.

      ***---                   
       CLOSE-FILES.
           close tordini.

      ***---
       EXIT-PGM.
           goback.
