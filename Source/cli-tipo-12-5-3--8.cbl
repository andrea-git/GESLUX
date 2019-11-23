       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      cli-tipo-12-5-3--8.
       AUTHOR.                          Andrea.
       REMARKS. Imposta la tipologia clienti da 12/5/3 a 8
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "clienti.sl".

      ******************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "clienti.fd".

       WORKING-STORAGE SECTION.
      * COPY
           copy "link-geslock.def".

      * COSTANTI
       78  titolo value "Cambio tipologia 12/5/3 - 8".

      * FILE STATUS
       77  status-clienti       pic xx.
       77  n-elab-12            pic 9(5).
       77  n-elab-5             pic 9(5).
       77  n-elab-3             pic 9(5).

      * FLAGS
       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".
       77  filler                pic 9.
           88 RecLocked          value 1 false 0. 

      ******************************************************************
       LINKAGE SECTION.
      ******************************************************************
       PROCEDURE DIVISION.

       DECLARATIVES.
       clienti-ERR SECTION.
           use after error procedure on clienti.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-clienti
           when "39"
                set errori to true
                display message "File [clienti] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[clienti] Indexed file corrupt!"
                          title titolo
                           icon 3  
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [clienti] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
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
           open i-o   clienti. 

      ***---
       ELABORAZIONE.              
           move low-value to cli-rec.
           set cli-tipo-C to true.
           start clienti key >= cli-chiave
                 invalid continue
           end-start.
           perform until 1 = 2
              read clienti next at end exit perform end-read
              if cli-tipo-F
                 exit perform
              end-if
              if cli-tipo = "12"
                 move "8 " to cli-tipo
                 rewrite cli-rec
                 add 1 to n-elab-12
              end-if
              if cli-tipo = "5 "
                 move "8 " to cli-tipo
                 rewrite cli-rec
                 add 1 to n-elab-5
              end-if
              if cli-tipo = "3 "
                 move "8 " to cli-tipo
                 rewrite cli-rec
                 add 1 to n-elab-3
              end-if
           end-perform.

      ***---
       CLOSE-FILES.
           close clienti.

      ***---
       EXIT-PGM. 
           display message "CAMBIO TIPOLOGIA:"    
                    x"0d0a""da 12 a 8: " n-elab-12
                    x"0d0a""da 5 a 8: " n-elab-5
                    x"0d0a""da 3 a 8: " n-elab-3
                     title titolo
                      icon 2.

           goback.
