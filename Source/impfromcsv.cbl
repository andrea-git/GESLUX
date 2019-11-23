       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      impfromcsv.
       AUTHOR.                          Andrea.
       REMARKS. Batch che scrive importa files VISION 4 partendo da
                file sequenziali in formato CSV
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "lineseq.sl". 
           copy "prog.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "lineseq.fd".
           copy "prog.fd".

       WORKING-STORAGE SECTION.
           copy "link-geslock.def".
           copy "comune.def".

       78  titolo                           value "Importazione".

       77  status-lineseq                   pic xx.
       77  stat-prog                        pic xx.

       77  wstampa                          pic x(256) value spaces.
       77  path-csv                         pic x(256) value spaces.
       77  nome-file                        pic x(50).
       77  separatore                       pic x.

       LINKAGE SECTION.
       77  tipo-import                      pic x.
           88 imp-menu                      value "M".

      ******************************************************************
       PROCEDURE DIVISION USING tipo-import.

       DECLARATIVES.
       LINESEQ-ERR SECTION.
           use after error procedure on lineseq.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-lineseq
           when "35"
                display message "Impossibile procedere."
                         x"0d0a""File CSV inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "93"
                initialize geslock-messaggio
                string   "File CSV già in uso."
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
                     open input lineseq
                when termina
                     set errori to true
                     display message "Operazione interrotta!"
                               title titolo
                                icon 2
                end-evaluate
           end-evaluate.

      ***---
       PROG-ERR SECTION.
           use after error procedure on prog.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate stat-prog
           when "35"
                display message "Impossibile procedere."
                         x"0d0a""File Menu [PROG] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "93"
                initialize geslock-messaggio
                string   "File MENU già in uso."
                  x"0d0a""Impossibile procedere!" delimited size
                      into geslock-messaggio
                end-string
                move 1 to geslock-v-riprova
                move 0 to geslock-v-ignora
                move 1 to geslock-v-termina
                move   "prog"       to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova
                     open i-o prog allowing readers
                when termina
                     set errori to true
                     display message "Operazione interrotta!"
                               title titolo
                                icon 2
                end-evaluate
           end-evaluate.

       END DECLARATIVES.

      ***---
       MAIN-PRG.
           perform INIT.
           if tutto-ok
              perform OPEN-FILES
              if tutto-ok
                 perform ELABORAZIONE
                 perform CLOSE-FILES
              end-if
           end-if.
           perform EXIT-PGM.

      ***---
       INIT.
           set RecLocked to false.
           set tutto-ok  to true.
           set trovato   to false.
           initialize path-csv wstampa.
           accept path-csv from environment "PATH_CSV"
                  on  exception set errori to true
              not on exception 
                  if path-csv = spaces set errori to true end-if
           end-accept.
           if errori
              display message "Impossbile procedere!"
                       x"0d0a""VARIABILE PATH_CSV NON TROVATA!"
                        title tit-err
                         icon 3
           else   

              accept separatore from environment "SEPARATORE"
                     on exception move ";" to separatore
                 not on exception
                     if separatore = space
                        move ";" to separatore
                     end-if
              end-accept

              evaluate true
              when imp-menu move "menu.csv" to nome-file
              end-evaluate
              inspect nome-file replacing trailing spaces by low-value
              inspect path-csv  replacing trailing spaces by low-value
              string  path-csv  delimited low-value
                      nome-file delimited low-value
                      into wstampa
              end-string

           end-if.

      ***---
       OPEN-FILES.
           open input lineseq.
           if tutto-ok
              evaluate true
              when imp-menu open i-o prog allowing readers
              end-evaluate
              if errori
                 close lineseq
              end-if
           end-if.
      
      ***---
       ELABORAZIONE.
           evaluate true
           when imp-menu perform PULISCI-MENU
                         perform IMPORTA-MENU
           end-evaluate.
           if trovato
              display message "Elaborazione effettuata!"
                        title titolo
           end-if.

      ***---
       PULISCI-MENU.
           move low-value to prog-r.
           start prog key is >= prog-key
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read   prog next at end exit perform end-read
                    delete prog record  invalid continue end-delete
                 end-perform
           end-start.

      ***---
       IMPORTA-MENU.
           perform until 1 = 2
              initialize line-riga
              read lineseq next at end exit perform end-read
              if line-riga = spaces    exit perform end-if
              unstring line-riga delimited by separatore
                  into prog-menu-level
                       prog-id
                       prog-s-desc
                       prog-type
                       prog-type-2
                       prog-l-desc
              end-unstring
              write prog-r invalid continue end-write
              set trovato  to true
           end-perform.

      ***---
       CLOSE-FILES.
           close lineseq.
           evaluate true
           when imp-menu close prog
           end-evaluate.

      ***---
       EXIT-PGM.
           goback.
