       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      azzera-fe.
       AUTHOR.                          Andrea.
       REMARKS. AZZERA FIDO EXTRA DI TUTTI I CLIENTI.
  
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "clienti.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "clienti.fd".

       WORKING-STORAGE SECTION.
       77  como-data       pic 9(8).    
       77  status-clienti  pic xx.

       77  filler          pic 9.
         88 RecLocked      value 1, false 0.

       PROCEDURE DIVISION.

       DECLARATIVES.
      ***---
       CLIENTI-ERR SECTION.
           use after error procedure on clienti.
           set RecLocked to false.
           evaluate status-clienti              
           when "99" set RecLocked to true
           end-evaluate.
       END DECLARATIVES.

      ***---
       MAIN.
           accept como-data from century-date.
           open i-o clienti.

           set cli-tipo-C to true.
           move low-value to cli-rec.
           start clienti key >= cli-chiave.
           perform until 1 = 2
              read clienti next no lock at end exit perform end-read
              if cli-tipo-F exit perform end-if
              if como-data > cli-data-fido-extra
                 move 0 to cli-fido-extra
                 rewrite cli-rec
              end-if
           end-perform.

           close clienti.

           goback.
