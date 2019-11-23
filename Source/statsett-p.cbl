       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      statsett-p.
       AUTHOR.                          Andrea.
       REMARKS. Effettua la stampa del file statsett per 
                l'intervallo di mesi indicato in accettazione.
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

       01  filler               pic 9.
           88 RicalcoloNotturno value 1, false 0. 

      * COSTANTI
       78  titolo 
           value "Stampa statistiche settoriali con raffronto mese".

       LINKAGE SECTION.
       copy "link-statraff.def".

      ******************************************************************
       PROCEDURE DIVISION using statraff-linkage.

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
                display message "Impossibile procedere."
                  x"0d0a""File statistiche [STATSETT] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                set errori to true
                display message "File [STATSETT] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[STATSETT] Indexed file corrupt!"
                          title titolo
                           icon 3
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
           end-evaluate.
      
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
                set errori to true
                display message "File [TMARCHE] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TMARCHE] Indexed file corrupt!"
                          title titolo
                           icon 3
           end-evaluate.
      
       TTIPOCLI-ERR SECTION.
           use after error procedure on ttipocli.
           set tutto-ok  to true.
           evaluate status-ttipocli
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File tipocli [TTIPOCLI] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                set errori to true
                display message "File [TTIPOCLI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TTIPOCLI] Indexed file corrupt!"
                          title titolo
                           icon 3
           end-evaluate.

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
           move low-value        to sts-rec.
           move statraff-mese-da to sts-mese.
           start statsett key is >= sts-chiave
                 invalid set errori to true
           end-start.

           if tutto-ok

              perform until 1 = 2
                 read statsett next no lock
                      at end   perform SCRIVI-TOTALI-MESE
                               if statraff-mensile
                                  perform SCRIVI-TOTALI-PERIODO
                               end-if
                               perform SCRIVI-TOTALI-GENERALI
                               exit perform
                 end-read

                 if sts-mese >= statraff-mese-da and
                    sts-mese <= statraff-mese-a

                    evaluate true

                    when sts-tipocli not = SaveTipo
                         if SaveTipo not = spaces
                            perform SCRIVI-TOTALI-MESE
                            if statraff-mensile
                               perform SCRIVI-TOTALI-PERIODO
                            end-if               
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

                    if statraff-mensile
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
                    else
                       if sts-fat-prog           not = 0 or
                          sts-csm-prog           not = 0 or
                          sts-kg-prog            not = 0 or
                          sts-adeguam-prog       not = 0 or
                          sts-fat-prog-past      not = 0 or
                          sts-csm-prog-past      not = 0 or
                          sts-kg-prog-past       not = 0 or
                          sts-adeguam-prog-past  not = 0
                          perform SCRIVI-RIGA-2
                       end-if
                    end-if

                 end-if

              end-perform

           end-if.

      ***---
       SCRIVI-RIGA-2.
           move sts-fat-prog          to sts-fat-corr.
           move sts-csm-prog          to sts-csm-corr.
           move sts-kg-prog           to sts-kg-corr.
           move sts-adeguam-prog      to sts-adeguam-corr.
           move sts-fat-prog-past     to sts-fat-past.
           move sts-csm-prog-past     to sts-csm-past.
           move sts-kg-prog-past      to sts-kg-past.
           move sts-adeguam-prog-past to sts-adeguam-past.
           perform SCRIVI-RIGA.

      *****     move    sts-marca       to mar-codice.
      *****     read tmarche no lock invalid continue end-read.
      *****     move    mar-descrizione to r-descr.
      *****
      *****     compute como-valore = 
      *****           ( sts-fat-prog - sts-csm-prog ) / sts-kg-prog.
      *****     move como-valore to r-mrg-kg-1.
      *****
      *****     compute como-valore = sts-kg-prog / 100.
      *****     move    como-valore  to r-qli-1.
      *****     add     como-valore  to tot-mese-qli-1.
      *****     add     como-valore  to tot-periodo-qli-1.
      *****     add     como-valore  to tot-qli-1.
      *****
      *****     move    sts-fat-prog to r-fatt-1.       
      *****     add     sts-fat-prog to tot-periodo-fatt-1.
      *****     add     sts-fat-prog to tot-mese-fatt-1.
      *****     add     sts-fat-prog to tot-fatt-1.
      *****
      *****     compute como-valore = 
      *****           ( sts-fat-prog - sts-csm-prog ) + sts-adeguam-prog.
      *****     move    como-valore   to r-resa-1.
      *****     add     como-valore   to tot-mese-resa-1.
      *****     add     como-valore   to tot-periodo-resa-1.
      *****     add     como-valore   to tot-resa-1.
      *****
      ****************************************************************
      *****
      *****     compute como-valore = 
      *****     ( sts-fat-prog-past - sts-csm-prog-past ) / sts-kg-prog-past
      *****     move como-valore to r-mrg-kg-2.
      *****                                                        
      *****     compute como-valore = sts-kg-prog-past / 100.
      *****     move    como-valore to r-qli-2.
      *****     add     como-valore to tot-mese-qli-2.
      *****     add     como-valore to tot-periodo-qli-2.
      *****     add     como-valore to tot-qli-2.
      *****
      *****     move    sts-fat-prog-past  to r-fatt-2.
      *****     add     sts-fat-prog  to tot-mese-fatt-2.
      *****     add     sts-fat-prog  to tot-periodo-fatt-2.
      *****     add     sts-fat-prog  to tot-fatt-2.
      *****
      *****     compute como-valore = 
      *****           ( sts-fat-prog-past - sts-csm-prog-past ) + 
      *****             sts-adeguam-prog-past.
      *****     move    como-valore   to r-resa-2.
      *****     add     como-valore   to tot-mese-resa-2.
      *****     add     como-valore   to tot-periodo-resa-2.
      *****     add     como-valore   to tot-resa-2.
      *****
      ******************************************************************
      *****
      *****     compute como-valore = 
      *****     ((sts-fat-prog      - sts-csm-prog)      / sts-kg-prog) - 
      *****     ((sts-fat-prog-past - sts-csm-prog-past) / sts-kg-prog-past)
      *****     move como-valore to r-mrg-kg-3.
      *****
      *****     compute como-valore = ( sts-kg-corr / 100) - 
      *****                           ( sts-kg-past / 100).
      *****     move    como-valore to r-qli-3.
      *****     add     como-valore to tot-mese-qli-3.
      *****     add     como-valore to tot-periodo-qli-3.
      *****     add     como-valore to tot-qli-3.
      *****
      *****     compute como-valore = sts-fat-prog - sts-fat-prog-past.
      *****     move    como-valore  to r-fatt-3.
      *****     add     sts-fat-prog to tot-mese-fatt-3.
      *****     add     sts-fat-prog to tot-periodo-fatt-3.
      *****     add     sts-fat-prog to tot-fatt-3.
      *****
      *****     compute como-valore =
      *****         ( ( sts-fat-prog      - sts-csm-prog      ) +
      *****                                 sts-adeguam-prog  ) -
      *****         ( ( sts-fat-prog-past - sts-csm-prog-past ) +
      *****                                 sts-adeguam-prog-past ).
      *****     move    como-valore   to r-resa-3.
      *****     add     como-valore   to tot-mese-resa-3.
      *****     add     como-valore   to tot-periodo-resa-3.
      *****     add     como-valore   to tot-resa-3.
      *****
      *****     initialize line-riga.
      *****     move riga to line-riga.
      *****     perform STAMPA-RIGA.
      *****     set trovato       to true.

      ***---
       SCRIVI-INTESTAZIONE.
           initialize tit-data.
           string statraff-data(7:2) delimited size
                  "/"                delimited size
                  statraff-data(5:2) delimited size
                  "/"                delimited size
                  statraff-data(3:2) delimited size
                  into tit-data
           end-string.
           if statraff-mensile move "(Dati mensili)"  to tit-tipo-stampa
           else                move "(Dati cumulati)" to tit-tipo-stampa
           end-if.
           perform SCRIVI-INTESTAZIONE-COMMON.
  
      ***---
       EXIT-PGM.
           move wstampa to statraff-path.    
           goback.

      ***---
       PARAGRAFO-COPY.
           copy "statsett.cpy".
