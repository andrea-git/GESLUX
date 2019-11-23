       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      statgio-p.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "lineseq.sl".
           copy "statsett.sl".
           copy "tmarche.sl".
           copy "ttipocli.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "lineseq.fd". 
           copy "statsett.fd".
           copy "tmarche.fd".
           copy "ttipocli.fd".

       WORKING-STORAGE SECTION.
       copy "link-geslock.def".
       copy "statsett.def".

      * COSTANTI
       78  titolo           value "DIREZIONALE: Statistica giornaliera".

       01  filler               pic 9.
           88 RicalcoloNotturno value 1, false 0. 

       01 statraff-tipo         pic  x.
           88 statraff-mensile  value is "M". 
           88 statraff-cumulato value is "C". 

       77  statraff-mese-a  pic 99.

       LINKAGE SECTION.
       77  link-path        pic x(256).
       77  link-mese        pic 99.

      ******************************************************************
       PROCEDURE DIVISION USING link-path link-mese.

       DECLARATIVES.
      
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
           end-evaluate.  
      
      ***---
       TTIPOCLI-ERR SECTION.
           use after error procedure on ttipocli.
           set tutto-ok  to true.
           evaluate status-ttipocli
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File tipologie clienti [TTIPOCLI] inesistente"
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
           end-evaluate.  

       LINESEQ-ERR SECTION.
           use after error procedure on lineseq.
           set tutto-ok  to true.
           evaluate status-lineseq
           when "39"
                display message "File Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "Indexed file corrupt!"
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
                move   "Lineseq"    to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova
                     perform OPEN-OUTPUT-LINESEQ
                when termina
                     set errori to true
                     display message "Operazione interrotta!"
                               title titolo
                                icon 2
                end-evaluate
           end-evaluate.

       STATSETT-ERR SECTION.
           use after error procedure on statsett.
           set tutto-ok  to true.
           evaluate status-statsett
           when "35"
                display message "File [STATSETT] not found!"
                           title titolo
                            icon 3
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
                initialize geslock-messaggio
                string   "File già in uso!"
                  x"0d0a""Impossibile procedere!" delimited size
                      into geslock-messaggio
                end-string
                move 1 to geslock-v-riprova
                move 0 to geslock-v-ignora
                move 1 to geslock-v-termina
                move   "statsett"   to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova
                     open input statsett
                when termina
                     set errori to true
                     display message "Operazione interrotta!"
                               title titolo
                                icon 2
                end-evaluate
           end-evaluate 
       END DECLARATIVES.

      ***--- 
       MAIN-PRG.
           set RicalcoloNotturno to false.
           perform INIT.
           perform OPEN-FILES.
           if tutto-ok
              perform ELABORAZIONE
              perform CLOSE-FILES
           end-if.

           perform EXIT-PGM.

      ***---
       ELABORAZIONE.
           set statraff-mensile to true.
           move  low-value to sts-rec.
           move  link-mese to sts-mese.
           start statsett key is >= sts-chiave
                 invalid set errori to true
           end-start.

           if tutto-ok

              perform until 1 = 2
                 read statsett next no lock
                      at end   perform SCRIVI-TOTALI-MESE
                               perform SCRIVI-TOTALI-PERIODO
                               perform SCRIVI-TOTALI-GENERALI
                               exit perform
                 end-read

                 if sts-mese = link-mese

                    evaluate true

                    when sts-tipocli not = SaveTipo
                         if SaveTipo not = spaces
                            perform SCRIVI-TOTALI-MESE
                            perform SCRIVI-TOTALI-PERIODO
                            move sts-tipocli to SaveTipo
                            perform SALTO-PAGINA
                            perform SCRIVI-INTESTAZIONE
                         else
                            move sts-tipocli to SaveTipo
                         end-if
                         move sts-mese    to SaveMese

                    when SaveMese not = sts-mese
                         if SaveMese not = 0
                            perform SCRIVI-TOTALI-MESE
                          end-if
                         move sts-mese    to SaveMese

                    end-evaluate

                    if prima-volta
                       perform SCRIVI-INTESTAZIONE
                       set prima-volta to false
                    end-if

                    perform VALORIZZA-OCCURS

                    if sts-fat-corr      not = 0 or
                       sts-csm-corr      not = 0 or
                       sts-kg-corr       not = 0 or
                       sts-adeguam-corr  not = 0 or
                       sts-fat-past      not = 0 or
                       sts-csm-past      not = 0 or
                       sts-kg-past       not = 0 or
                       sts-adeguam-past  not = 0
                       perform SCRIVI-RIGA
                    end-if
                    
                 end-if

              end-perform

           end-if.

           if not trovato
              display message "Nessun dato per il mese richiesto!"
                        title titolo
                         icon 2
           end-if.

      ***---
       SCRIVI-INTESTAZIONE.
           initialize tit-data.
           string como-data(7:2) delimited size
                  "/"            delimited size
                  como-data(5:2) delimited size
                  "/"            delimited size
                  como-data(3:2) delimited size
                  into tit-data
           end-string.
           move "(Dati mensili)"  to tit-tipo-stampa.
           perform SCRIVI-INTESTAZIONE-COMMON.

      ***---
       EXIT-PGM.
           move wstampa to link-path.
           goback.

      ***---
       PARAGRAFO-COPY.
           copy "statsett.cpy".
