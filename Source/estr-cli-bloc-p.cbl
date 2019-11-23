       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      estr-cli-bloc-p.
       AUTHOR.                          Luciano.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "clienti.sl".
           copy "lineseq.sl".


      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "clienti.fd".
           copy "lineseq.fd".

       WORKING-STORAGE SECTION.
           copy "acucobol.def".
           copy "acugui.def".
           copy "common-excel.def".
           copy "link-geslock.def".
           
       78  titolo value  "Estrazione clienti bloccati".

       77  status-clienti    pic xx.
       77  status-lineseq    pic xx.

       77  wstampa           pic x(256).
                                            
       77  como-data             pic 9(8).
       77  como-ora              pic 9(8).
       77  como-riga             pic x(200).

      * FLAGS
       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".
       77  filler                pic 9.
           88 RecLocked          value 1, false 0.
       77  filler                pic 9.
           88 prima-volta        value 1, false 0.

       77  codice-ed   pic z(5).
       77  como-nota   pic x(30).
       77  cont-ed  pic z(5).

       LINKAGE SECTION.
       77  Form-Handle USAGE IS HANDLE OF WINDOW.
       77  user-codi   pic x(10).

      ******************************************************************
       PROCEDURE DIVISION USING Form-Handle user-codi.

       DECLARATIVES.
      ***---
       CLIENTI-ERR SECTION.
           use after error procedure on clienti.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-clienti
           when "35"
                move "File [CLIENTI] inesistente" to como-riga
                   inspect como-riga 
                                replacing trailing space by low-value
                   display message box como-riga
                          title titolo
                set errori to true
           when "39"
                move "File [CLIENTI] mismatch size!" to como-riga
                   inspect como-riga 
                                replacing trailing space by low-value
                   display message box como-riga
                          title titolo
                set errori to true
           when "98"
                move "[CLIENTI] Indexed file corrupt!" to como-riga
                   inspect como-riga 
                                replacing trailing space by low-value
                   display message box como-riga
                          title titolo
                set errori to true
           when "93"
           when "99" 
                set RecLocked to true
           end-evaluate.

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
                string   "Chiudere file Excel!"
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
           end-evaluate.
       END DECLARATIVES.

      ***---
       MAIN-PRG.
           perform INIT.
           perform OPEN-FILES
           if tutto-ok
              perform ELABORAZIONE
              perform CLOSE-FILES
           end-if.
           perform EXIT-PGM.

      ***---
       INIT.
           move zero   to cont

           set tutto-ok      to true.
           set prima-volta   to true.

           accept como-data   from century-date
           accept como-ora   from time
           perform ACCETTA-SEPARATORE

           accept como-ora  from time.
           accept como-data from century-date.
           accept wstampa from environment "PATH_ST"
           inspect wstampa replacing trailing space by low-value
           string wstampa           delimited by low-value
                  "Clienti_bloccati_"  delimited by size
                  como-data            delimited by size
                  "_"                  delimited by size
                  como-ora             delimited by size
                  ".csv"               delimited by size
                  into wstampa
           inspect wstampa replacing trailing low-value by space.

      ***---
       OPEN-FILES.
           open input clienti. 

      ***---
       ELABORAZIONE.
           set cli-tipo-c to true
           move low-value to cli-codice
           start clienti key not < cli-chiave
              invalid 
                 continue
              not invalid
                 perform until 1 = 2
                    read clienti next 
                       at end 
                          exit perform 
                    end-read
                    if not cli-tipo-c
                       exit perform
                    end-if

                    add 1 to cont
                    move cont   to cont-ed
                    call "C$JUSTIFY" using cont-ed, "L"
                    display cont-ed 
                                upon form-handle at column 44 line 2
                    if cli-bloccato
                       perform TRATTA-CLIENTE
                       if errori
                          exit perform
                       end-if
                    end-if
                 end-perform
           end-start.

           if prima-volta
              display message box "Nessun cliente bloccato"
                       title titolo
           else
              close LINESEQ
              perform CALL-EXCEL
           end-if.

      ***---
       TRATTA-CLIENTE.
           if prima-volta
              set prima-volta  to false
              open output lineseq
              if errori
                 display message box "impossibile creare il file CSV"
                         title = titolo
                         icon 2
                 exit paragraph
              else
                 perform INTESTA-CSV
              end-if
           end-if.


           initialize line-riga
           move cli-codice   to codice-ed

           inspect cli-ragsoc-1 replacing trailing space by low-value
           inspect cli-ragsoc-2 replacing trailing space by low-value
           inspect cli-note replacing trailing space by low-value

           evaluate true
           when cli-prob-pag
                move "PROBLEMATICHE PAGAMENTO"       to como-nota
           when cli-nuovo-ragsoc
                move "NUOVA RAGIONE SOCIALE"         to como-nota
           when cli-no-angraf
           when other
                move "ANAGRAFICA ERRATA - MANCANTE"  to como-nota
           end-evaluate


           string codice-ed    delimited size
                  separatore   delimited size
                  cli-ragsoc-1 delimited low-value
                  " "          delimited size  
                  cli-ragsoc-2 delimited low-value
                  separatore   delimited size
                  como-nota    delimited size
                  separatore   delimited size
      *    aggiungo gli apici per bypassare gli eventuali a capo
                  x"22"        delimited size
                  cli-note     delimited low-value
                  x"22"        delimited size
                  into line-riga
           write line-riga.

      ***--
       CLOSE-FILES.
           close clienti.

      ***---
       EXIT-PGM.
           goback.

      ***---
       INTESTA-CSV.
           initialize line-riga.
           string "CLIENTE"         delimited size
                  separatore        delimited size
                  "RAGIONE SOCIALE" delimited size
                  separatore        delimited size
                  "CAUSALE"         delimited size
                  separatore        delimited size
                  "NOTE"            delimited size
                  into line-riga
           write line-riga.

      ***---
       PARAGRAFO-COPY.
       copy "common-excel.cpy".
