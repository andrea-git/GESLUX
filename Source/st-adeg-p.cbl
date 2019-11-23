       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      st-adeg-p.
       AUTHOR.                          Andrea.

      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "lineseq.sl".
           copy "tmarche.sl".
           copy "statsett.sl".
           copy "ttipocli.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "lineseq.fd".
           copy "tmarche.fd".
           copy "statsett.fd".
           copy "ttipocli.fd".

       WORKING-STORAGE SECTION.
      * COPY
       copy "Acugui.def".
       copy "link-geslock.def".
       copy "common-excel.def".

      * COSTANTI
       78  titolo value "Stampa Adeguamento".

      * FILE STATUS
       77  status-lineseq        pic xx.
       77  status-tmarche        pic xx.
       77  status-ttipocli       pic xx.
       77  status-statsett       pic xx.
       77  wstampa               pic x(256).

      * VARIABILI
       77  adeg-corr             pic ---.---.--9,99.
       77  adeg-prog             pic ---.---.--9,99.

       77  como-data             pic 9(8).
       77  como-ora              pic 9(8).
       77  user-codi             pic x(10).

      * FLAGS
       01  controlli             pic xx.
         88 tutto-ok             value "OK".
         88 errori               value "ER".

       01  filler                pic 9.
         88 prima-volta          value 1, false 0.

       01  filler                pic 9.
         88 record-ok            value 1, false 0.

       01  filler                pic 9.
         88 trovato              value 1, false 0.

       01  filler                pic 9.
         88 RecLocked            value 1, false 0.

       LINKAGE SECTION.
       copy "link-st-adeg.def".

      ******************************************************************
       PROCEDURE DIVISION using st-adeg-linkage.

       DECLARATIVES.
       LINESEQ-ERR SECTION.
           use after error procedure on lineseq.
           set tutto-ok  to true.
           evaluate status-lineseq
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File lineseq [LINESEQ] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [LINESEQ] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[LINESEQ] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           when "93"
                initialize geslock-messaggio
                string   "File già in uso!"
                  x"0d0a""Impossibile procedere!" delimited size
                      into geslock-messaggio
                end-string
                move 1 to geslock-v-riprova
                move 0 to geslock-v-ignora
                move 1 to geslock-v-termina
                move   "File CSV"   to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova
                     open output lineseq
                when termina
                     set errori to true
                     display message "Operazione interrotta!"
                               title titolo
                                icon 2
                end-evaluate
           end-evaluate

      ***--- 
       TMARCHE-ERR SECTION.
           use after error procedure on tmarche.
           set tutto-ok  to true.
           evaluate status-tmarche
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File marche [TMARCHE] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [TMARCHE] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[TMARCHE] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           when "93"
           when "99"
                set RecLocked to true
                set errori    to true
           end-evaluate.

      ***--- 
       TTIPOCLI-ERR SECTION.
           use after error procedure on ttipocli.
           set tutto-ok  to true.
           evaluate status-ttipocli
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File tipol. clienti [TTIPOCLI] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [TTIPOCLI] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[TTIPOCLI] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           when "93"
           when "99"
                set RecLocked to true
                set errori    to true
           end-evaluate.  

      ***--- 
       STATSETT-ERR SECTION.
           use after error procedure on statsett.
           set tutto-ok  to true.
           evaluate status-statsett
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File statistiche sett [STATSETT] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [STATSETT] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[STATSETT] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           when "93"
           when "99"
                set RecLocked to true
                set errori    to true
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
      *-
           move link-sta-user to user-codi.
           accept como-data from century-date.
           accept como-ora  from time.
           initialize wstampa.
           set tutto-ok          to true.
           set prima-volta       to true.
           set trovato           to false.
           accept  wstampa       from environment "PATH-ST".
           inspect wstampa       replacing trailing spaces by low-value.
           inspect user-codi     replacing trailing spaces by low-value
           string  wstampa       delimited by low-value
                   "st-adeg"     delimited by size
                   "_"           delimited by size
                   user-codi     delimited by low-value
                   ".csv"        delimited by size
                   into wstampa
           end-string.

      ***---
       OPEN-FILES.
           open output lineseq.
           if tutto-ok
              open input tmarche
                         statsett
                         ttipocli
              if errori goback end-if
           end-if.

      ***---
       ELABORAZIONE.
           move link-sta-tipo    to sts-tipocli.
           move link-sta-mese-da to sts-mese.
           move link-sta-marca   to sts-marca.

           start statsett key is >= sts-chiave
                 invalid set errori to true
           end-start.
           if tutto-ok
              perform until 1 = 2
                 read statsett next at end exit perform end-read
                 if link-sta-tipo not = sts-tipocli and 
                    link-sta-tipo not = spaces
                    exit perform
                 end-if
                       
                 set record-ok to true
                 if sts-mese < link-sta-mese-da or
                    sts-mese > link-sta-mese-a
                    set record-ok to false
                 end-if

                 if link-sta-marca not = sts-marca and
                    link-sta-marca not = 0
                    set record-ok to false
                 end-if

                 if record-ok
                    perform SCRIVI-RIGA
                 end-if

              end-perform
           end-if.

           if not trovato 
              display message "Nessun adeguamento nei limiti richiesti"
                        title titolo
                         icon 2
           else
              perform CALL-EXCEL 
           end-if.

      ***---
       SCRIVI-RIGA.
           if prima-volta
              perform ACCETTA-SEPARATORE
              set prima-volta to false
              initialize line-riga
              string "Mese"                       delimited size
                     separatore                   delimited size
                     "Tipol."                     delimited size                                     
                     separatore                   delimited size
                     "Descrizione"                delimited size
                     separatore                   delimited size
                     "Marca"                      delimited size
                     separatore                   delimited size
                     "Descrizione"                delimited size
                     separatore                   delimited size
                     "Adeguamento anno corrente"  delimited size
                     separatore                   delimited size
                     "Adeguamento anno passato"   delimited size
                     into line-riga
              end-string
              write line-riga
              write line-riga from spaces
              set   trovato   to   true
           end-if.

           move sts-tipocli to tcl-codice.
           read ttipocli    no lock invalid continue end-read.

           move sts-marca to mar-codice.
           read tmarche   no lock invalid continue end-read.

           move sts-adeguam-corr to adeg-corr.
           move sts-adeguam-prog to adeg-prog.

           initialize line-riga.
           string sts-mese            delimited size
                  separatore          delimited size
                  sts-tipocli         delimited size
                  separatore          delimited size
                  tcl-descrizione     delimited size
                  separatore          delimited size
                  sts-marca           delimited size
                  separatore          delimited size
                  mar-descrizione     delimited size
                  separatore          delimited size
                  adeg-corr           delimited size
                  separatore          delimited size
                  adeg-prog           delimited size
                  into line-riga
           end-string.
           write line-riga.

      ***---
       CLOSE-FILES.
           close lineseq 
                 tmarche
                 ttipocli
                 statsett.

      ***---
       EXIT-PGM.
           goback.

      ***---
       PARAGRAFO-COPY.
       copy "common-excel.cpy".
