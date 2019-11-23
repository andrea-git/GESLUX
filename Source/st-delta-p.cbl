       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      st-delta-p.
       AUTHOR.                          Andrea.
       REMARKS.  Serve per avere (prima di aver consolidato) una proiezione
           di quelli che saranno i guadagni per quella marca del mese.
           La stampa verrà richiamata solamente in caso venga richiesto 
           il mese successivo a quello di consolidamento.
           Più sotto i commenti alle formule
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           |FITTIZIO
           copy "tmp-tendenza.sl".

           copy "tendelta.sl".
           copy "rmovmag.sl".
           copy "lineseq.sl".
           copy "statraff.sl".
           copy "tmarche.sl".
           copy "progmagric.sl".
           copy "tmp-delta-marca.sl".
           copy "articoli.sl".
           copy "ttipocli.sl".
           copy "tparamge.sl".
           copy "tcontat.sl".
           copy "statsett.sl".
           COPY "statsett.sl"
                REPLACING ==statsett== BY ==statsett2==,
                          ==STATUS-statsett== BY ==STATUS-statsett2==.
                                
      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           |FITTIZIO
           copy "tmp-tendenza.fd".
                              
           copy "tendelta.fd".
           copy "rmovmag.fd".
           copy "lineseq.fd". 
           copy "statraff.fd".
           copy "tmarche.fd".
           copy "progmagric.fd".
           copy "tmp-delta-marca.fd".
           copy "articoli.fd".
           copy "ttipocli.fd".
           copy "tparamge.fd".
           copy "tcontat.fd".
           copy "statsett.fd".
           COPY "statsett.fd"
                REPLACING ==statsett== BY ==statsett2==,
                          ==STATUS-statsett== BY ==STATUS-statsett2==.

       WORKING-STORAGE SECTION.
      * FILE STATUS AND VARIABLES
       77  status-tmp-tendenza        pic xx.
       77  path-tmp-tendenza          pic x(256).

       77  status-tendelta            pic xx.
       77  status-lineseq             pic xx.
       77  status-statsett2           pic xx.
       77  status-statsett            pic xx.
       77  status-statraff            pic xx.
       77  status-tmarche             pic xx.
       77  status-progmagric          pic xx.
       77  status-tmp-delta-marca     pic xx.
       77  status-articoli            pic xx.
       77  status-ttipocli            pic xx.
       77  status-tparamge            pic xx.
       77  status-tcontat             pic xx.
       77  status-rmovmag             pic xx.

       77  wstampa                    pic x(256).
       77  path-tmp-delta-marca       pic x(256).

       77  tt-resa                    pic s9(12)v99.
       77  tt-delta                   pic s9(12)v99.
       77  tt-scostamento             pic s9(12)v99.

       77  tg-resa                    pic s9(12)v99.
       77  tg-delta                   pic s9(12)v99.
       77  tg-scostamento             pic s9(12)v99.

      * VARIABILI
       77  como-data                  pic 9(8).
       77  como-ora                   pic 9(8).
       77  como-vendite               pic s9(12)v999.
       77  como-acquisti              pic s9(12)v999.
       77  finali                     pic s9(12)v99.
       77  margine                    pic s9(12)v99.
       77  resa-euro                  pic s9(12)v99.
       77  delta                      pic s9(12)v99.
       77  scostamento                pic s9(12)v99.
       77  incidenza                  pic s9(3)v99.
       77  SaveTipo                   pic xx.    
       77  num-righe                  pic 9(3) value 0.
       77  num-page                   pic 9(3) value 0.
       77  margine-pos                pic 9(12)v99.

      * RIGHE PER LA STAMPA
       77  sav-riga                   pic x(900).

       77  divisorio-1                pic x(92) value all "=".
       77  divisorio-2                pic x(92) value all "^".

       01  riga-page.
         05 filler                    pic x(1).
         05 filler                    pic x(15) value "Ricalcolo del: ".
         05 st-data-rical             pic x(10).
         05 filler                    pic x(27) 
                                      value" Ultima Fatturazione del: ".
         05 st-data-fatt              pic x(10).
         05 filler                    pic x(21).
         05 filler                    pic x(4) value "Pag.".
         05 filler                    pic x.
         05 tit-page                  pic zz9.

       01  intestazione-1.
         05 filler                    pic x.
         05 filler                    pic x(7) value "CLIENTI".
         05 filler                    pic x.
         05 int-descrizione           pic x(50).

       01  intestazione-2.
         05 filler                    pic x.
         05 filler                    pic x(6)  value "MARCHE".
         05 filler                    pic x(34).
         05 filler                    pic x(10) value "Resa/Euro".
         05 filler                    pic x(9).
         05 filler                    pic x(11) value "Tendenza(*)".
         05 filler                    pic x(10).
         05 filler                    pic x(11) value "Scostamento".

       01  r-riga.
         05 filler                    pic x.
         05 r-mar-descrizione         pic x(30).
         05 filler                    pic x(4)            value spaces.
         05 r-marg-statsett           pic ----.---.--9,99 value spaces.
         05 filler                    pic x(6)            value spaces.
         05 r-delta                   pic ----.---.--9,99 value spaces.
         05 filler                    pic x(6)            value spaces.
         05 r-scostamento             pic ----.---.--9,99 value spaces.

       01  r-totali.
         05 filler                    pic x.
         05 tot-tipo                  pic x(30).
         05 filler                    pic x(4)            value spaces.
         05 tot-resa                  pic ----.---.--9,99 value spaces.
         05 filler                    pic x(6)            value spaces.
         05 tot-delta                 pic ----.---.--9,99 value spaces.
         05 filler                    pic x(6)            value spaces.
         05 tot-scostamento           pic ----.---.--9,99 value spaces.

      * FLAGS
       01  filler                     pic 9.
         88 trovato                   value 1, false 0.

       01  filler                     pic 9.
         88 trovato-contab            value 1, false 0.

       01  controlli                  pic xx.
         88 tutto-ok                  value "OK".
         88 errori                    value "ER".

      * COSTANTI
       78  titolo                     value "Stampa delta margine".
       78  max-righe                  value 62.

       LINKAGE SECTION.
       77  link-mese                  pic 99.
       77  link-data                  pic 9(8).

      ******************************************************************
       PROCEDURE DIVISION USING link-mese.

       DECLARATIVES.
      ***---
       LINESEQ-ERR SECTION.
           use after error procedure on lineseq.
           set tutto-ok  to true.
           evaluate status-lineseq
           when "35"
      *****          display message "Impossibile procedere."
      *****            x"0d0a""File lineseq [LINESEQ] inesistente"
      *****                    title titolo
      *****                     icon 2
                set errori to true
           when "39"
      *****          display message "File [LINESEQ] Mismatch size!"
      *****                    title titolo
      *****                     icon 3
                set errori to true
           when "98"
      *****          display message "[LINESEQ] Indexed file corrupt!"
      *****                    title titolo
      *****                     icon 3
                set errori to true
           when "93"
                set errori to true
      *****          initialize geslock-messaggio
      *****          string   "File già in uso!"
      *****            x"0d0a""Impossibile procedere!" delimited size
      *****                into geslock-messaggio
      *****          end-string
      *****          move 1 to geslock-v-riprova
      *****          move 0 to geslock-v-ignora
      *****          move 1 to geslock-v-termina
      *****          move   "Lineseq"    to geslock-nome-file
      *****          call   "geslock" using geslock-linkage
      *****          cancel "geslock"
      *****          evaluate true
      *****          when riprova
      *****               perform OPEN-OUTPUT-LINESEQ
      *****          when termina
      *****               set errori to true
      *****               display message "Operazione interrotta!"
      *****                         title titolo
      *****                          icon 2
      *****          end-evaluate
           end-evaluate.

      ***---
       STATSETT-ERR SECTION.
           use after error procedure on statsett.
           set tutto-ok  to true.
           evaluate status-statsett
           when "35"
      *****          display message "Impossibile procedere."
      *****            x"0d0a""File statistiche [STATSETT] inesistente"
      *****                    title titolo
      *****                     icon 2
                set errori to true
           when "39"
                set errori to true
      *****          display message "File [STATSETT] mismatch size!"
      *****                    title titolo
      *****                     icon 3
           when "98"
                set errori to true
      *****          display message "[STATSETT] Indexed file corrupt!"
      *****                    title titolo
      *****                     icon 3
           when "93"
      *****          initialize geslock-messaggio
      *****          string   "File già in uso!"
      *****            x"0d0a""Impossibile procedere!" delimited size
      *****                into geslock-messaggio
      *****          end-string
      *****          move 1 to geslock-v-riprova
      *****          move 0 to geslock-v-ignora
      *****          move 1 to geslock-v-termina
      *****          move   "statsett"   to geslock-nome-file
      *****          call   "geslock" using geslock-linkage
      *****          cancel "geslock"
      *****          evaluate true
      *****          when riprova
      *****               open input statsett
      *****          when termina
                     set errori to true
      *****               display message "Operazione interrotta!"
      *****                         title titolo
      *****                          icon 2
      *****          end-evaluate
           end-evaluate.
      
      ***---
       STATRAFF-ERR SECTION.
           use after error procedure on statraff.
           set tutto-ok  to true.
           evaluate status-statraff
           when "35"
      *****          display message "Impossibile procedere."
      *****            x"0d0a""File marche [STATRAFF] inesistente"
      *****                    title titolo
      *****                     icon 2
                set errori to true
           when "39"
                set errori to true
      *****          display message "File [STATRAFF] mismatch size!"
      *****                    title titolo
      *****                     icon 3
           when "98"
                set errori to true
      *****          display message "[STATRAFF] Indexed file corrupt!"
      *****                    title titolo
      *****                     icon 3
           end-evaluate.
      
      ***---
       TMARCHE-ERR SECTION.
           use after error procedure on tmarche.
           set tutto-ok  to true.
           evaluate status-tmarche
           when "35"
      *****          display message "Impossibile procedere."
      *****            x"0d0a""File marche [TMARCHE] inesistente"
      *****                    title titolo
      *****                     icon 2
                set errori to true
           when "39"
                set errori to true
      *****          display message "File [TMARCHE] mismatch size!"
      *****                    title titolo
      *****                     icon 3
           when "98"
                set errori to true
      *****          display message "[TMARCHE] Indexed file corrupt!"
      *****                    title titolo
      *****                     icon 3
           end-evaluate.
      
      ***---
       ARTICOLI-ERR SECTION.
           use after error procedure on articoli.
           set tutto-ok  to true.
           evaluate status-articoli
           when "35"
      *****          display message "Impossibile procedere."
      *****            x"0d0a""File  [ARTICOLI] inesistente"
      *****                    title titolo
      *****                     icon 2
                set errori to true
           when "39"
                set errori to true
      *****          display message "File [ARTICOLI] mismatch size!"
      *****                    title titolo
      *****                     icon 3
           when "98"
                set errori to true
      *****          display message "[ARTICOLI] Indexed file corrupt!"
      *****                    title titolo
      *****                     icon 3
           end-evaluate.
      
      ***---
       TTIPOCLI-ERR SECTION.
           use after error procedure on ttipocli.
           set tutto-ok  to true.
           evaluate status-ttipocli
           when "35"
      *****          display message "Impossibile procedere."
      *****            x"0d0a""File  [TTIPOCLI] inesistente"
      *****                    title titolo
      *****                     icon 2
                set errori to true
           when "39"
                set errori to true
      *****          display message "File [TTIPOCLI] mismatch size!"
      *****                    title titolo
      *****                     icon 3
           when "98"
                set errori to true
      *****          display message "[TTIPOCLI] Indexed file corrupt!"
      *****                    title titolo
      *****                     icon 3
           end-evaluate.
      
      ***---
       TMP-DELTA-MARCA-ERR SECTION.
           use after error procedure on tmp-delta-marca.
           set tutto-ok  to true.
           evaluate status-tmp-delta-marca
           when "35"
      *****          display message "Impossibile procedere."
      *****            x"0d0a""File [TMP-DELTA-MARCA] inesistente"
      *****                    title titolo
      *****                     icon 2
                set errori to true
           when "39"
                set errori to true
      ****          display message "File [TMP-DELTA-MARCA] mismatch size!"
      ****                    title titolo
      ****                     icon 3
           when "98"
                set errori to true
      *****          display message "[TMP-DELTA-MARCA]Indexed file corrupt!"
      *****                    title titolo
      *****                     icon 3
           when "93"
      *****          initialize geslock-messaggio
      *****          string   "File già in uso!"
      *****            x"0d0a""Impossibile procedere!" delimited size
      *****                into geslock-messaggio
      *****          end-string
      *****          move 1 to geslock-v-riprova
      *****          move 0 to geslock-v-ignora
      *****          move 1 to geslock-v-termina
      *****          move   "TMP-DELTA-MARCA"  to geslock-nome-file
      *****          call   "geslock" using geslock-linkage
      *****          cancel "geslock"
      *****          evaluate true
      *****          when riprova
      *****               open output tmp-delta-marca
      *****          when termina
                     set errori to true
      *****               display message "Operazione interrotta!"
      *****                         title titolo
      *****                          icon 2
      *****          end-evaluate
           end-evaluate.
      
      ***---
       PROGMAGRIC-ERR SECTION.
           use after error procedure on progmagric.
           set tutto-ok  to true.
           evaluate status-progmagric
           when "35"
      *****          display message "Impossibile procedere."
      *****            x"0d0a""File tipocli [PROGMAGRIC] inesistente"
      *****                    title titolo
      *****                     icon 2
                set errori to true
           when "39"
                set errori to true
      *****          display message "File [PROGMAGRIC] mismatch size!"
      *****                    title titolo
      *****                     icon 3
           when "98"
                set errori to true
      *****          display message "[PROGMAGRIC] Indexed file corrupt!"
      *****                    title titolo
      *****                     icon 3
           end-evaluate.   
      
      ***---
       RMOVMAG-ERR SECTION.
           use after error procedure on rmovmag.
           set tutto-ok  to true.
           evaluate status-rmovmag
           when "35"
      *****          display message "Impossibile procedere."
      *****            x"0d0a""File [RMOVMAG] inesistente"
      *****                    title titolo
      *****                     icon 2
                set errori to true
           when "39"
                set errori to true
      *****          display message "File [RMOVMAG] mismatch size!"
      *****                    title titolo
      *****                     icon 3
           when "98"
                set errori to true
      *****          display message "[RMOVMAG] Indexed file corrupt!"
      *****                    title titolo
      *****                     icon 3
           end-evaluate.

       END DECLARATIVES.

      ***--- 
       MAIN-PRG.
           perform INIT.
           perform OPEN-FILES.
           if tutto-ok
              perform CONTROLLA-PRESENZA-CONTAB
              if trovato-contab
                 perform ELABORAZIONE
              else
                 |FITTIZIO
                 close tmp-tendenza
                 delete file tmp-tendenza
              end-if
              perform CLOSE-FILES
           end-if.

           perform EXIT-PGM.

      ***---
       INIT.
           accept como-data from century-date.
           accept como-ora  from time.
           initialize wstampa path-tmp-delta-marca.
           set tutto-ok       to true.
           accept  wstampa    from environment "PATH-ST".
           inspect wstampa    replacing trailing spaces by low-value.
           string  wstampa    delimited by low-value
                   "st-delta" delimited by size
                   "_"        delimited by size
                   como-data  delimited by size
                   ".txt"     delimited by size
                   into wstampa
           end-string.
           accept  path-tmp-delta-marca from environment "PATH-ST".
           inspect path-tmp-delta-marca 
                   replacing trailing spaces by low-value
           string  path-tmp-delta-marca delimited by low-value
                   "tmp-delta"    delimited by size
                   "_"            delimited by size
                   como-data      delimited by size
                   "_"            delimited by size
                   como-ora       delimited by size
                   into path-tmp-delta-marca
           end-string.

      ***---
       OPEN-FILES.
           perform OPEN-OUTPUT-LINESEQ.
           if tutto-ok
              perform OPEN-TMP-DELTA-MARCA-LOCK
              if tutto-ok
                 open input  statsett statsett2 statraff 
                             tparamge tcontat rmovmag
                 open output tendelta
                 if tutto-ok
                    open input articoli ttipocli
                               tmarche  progmagric
                    if errori
                       close statsett statraff
                       close lineseq
                       delete file lineseq
                       close tmp-delta-marca
                       delete file tmp-delta-marca
                    end-if
                 else
                    close lineseq
                    delete file lineseq
                    close tmp-delta-marca
                    delete file tmp-delta-marca
                 end-if
              else
                 close lineseq
                 delete file lineseq
              end-if
           end-if.

      ***---
       OPEN-OUTPUT-LINESEQ.
           open output lineseq.
                              
      ***---
       OPEN-TMP-DELTA-MARCA-LOCK.
           open output tmp-delta-marca.
           if tutto-ok
              close tmp-delta-marca
              open i-o tmp-delta-marca allowing readers
           end-if.

      ***---
       ELABORAZIONE.
           move 0 to con-ult-stampa-fatt. 
           move spaces to tge-chiave.
           read tparamge no lock invalid continue end-read.
           move tge-anno to con-anno.
           read tcontat  no lock invalid continue end-read.
           move con-ult-stampa-fatt(7:2) to st-data-fatt(1:2).
           move "/"                      to st-data-fatt(3:1).
           move con-ult-stampa-fatt(5:2) to st-data-fatt(4:2).
           move "/"                      to st-data-fatt(6:1).
           move con-ult-stampa-fatt(1:4) to st-data-fatt(7:4).
           move link-data(7:2)           to st-data-rical(1:2).
           move "/"                      to st-data-rical(3:1).
           move link-data(5:2)           to st-data-rical(4:2).
           move "/"                      to st-data-rical(6:1).
           move link-data(1:4)           to st-data-rical(7:4).

           perform MARGINE-RICALCOLATO.
           perform MARGINE-CONSOLIDATO.

           |CALCOLO COLONNE
           move spaces    to SaveTipo.
           move low-value to sts-chiave of statsett.
           move link-mese to sts-mese   of statsett.
           start statsett key >= k-ord
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read statsett next 
                         at end
                         perform TOTALI-TIPO
                         perform TOTALI-GENERALI
                         exit perform 
                    end-read

                    if sts-mese of statsett not = link-mese 
                       perform TOTALI-TIPO
                       perform TOTALI-GENERALI
                       exit perform 
                    end-if

                    if sts-tipocli of statsett not = SaveTipo
                       if SaveTipo not = spaces
                          perform TOTALI-TIPO
                          perform SALTO-PAGINA
                       end-if
                       perform INTESTAZIONE-TIPO
                    end-if

                    perform CALCOLO-DELTA
    
                    if margine     not = 0 or
                       delta       not = 0
                       move mar-descrizione    to r-mar-descrizione

                       compute resa-euro = sts-fat-corr of statsett -
                                           sts-csm-corr of statsett

                       compute scostamento = delta - resa-euro

                       move resa-euro          to r-marg-statsett
                       move delta              to r-delta
                       move scostamento        to r-scostamento

                       move r-riga to line-riga
                       perform STAMPA-RIGA

                       |Serve per conservare i valori
                       move SaveTipo    to ten-tipocli
                       move mar-codice  to ten-marca
                       move resa-euro   to ten-resa
                       move delta       to ten-tendenza
                       move scostamento to ten-scostamento
                       write ten-rec invalid continue end-write

                       add resa-euro   to tt-resa
                       add delta       to tt-delta
                       add scostamento to tt-scostamento
                    end-if

                 end-perform
           end-start.

      ***---
       SALTO-PAGINA.
           write line-riga from space after page.
           write line-riga from x"09" after 1.
           add  1 to num-page.
           move num-page  to tit-page.
           move riga-page to line-riga.
           write line-riga.
           add  1 to num-righe.
           move 0 to num-righe.

      ***---
       STAMPA-RIGA.
           initialize sav-riga
           move line-riga to sav-riga
           if num-righe > max-righe| - 5
              perform SALTO-PAGINA
           end-if.
           move sav-riga to line-riga
           write line-riga
           add 1 to num-righe.

      ***---
       INTESTAZIONE-TIPO.
           if SaveTipo = spaces
              add 1 to num-page
              move num-page  to tit-page
              move riga-page to line-riga
              write line-riga
              move 1 to num-righe
           end-if.
           move sts-tipocli of statsett to SaveTipo.
           move sts-tipocli of statsett to tcl-codice
           read ttipocli no lock invalid continue end-read.
           move divisorio-1 to line-riga.
           write line-riga.
           move tcl-descrizione to int-descrizione.
           move intestazione-1  to line-riga.
           write line-riga.
           move divisorio-2     to line-riga.
           write line-riga.
           move intestazione-2  to line-riga.
           write line-riga.
           move divisorio-2     to line-riga.
           write line-riga.

           add 5 to num-righe.


      ***---
       TOTALI-TIPO.
           if num-righe > max-righe - 3
              perform SALTO-PAGINA
           end-if.

           write line-riga from divisorio-1.
           move "T O T A L I   T I P O" to tot-tipo.
           move tt-resa        to tot-resa.
           move tt-delta       to tot-delta.
           move tt-scostamento to tot-scostamento
           move r-totali       to line-riga.
           write line-riga.
           write line-riga   from divisorio-1.

           add 3 to num-righe.

           add tt-resa        to tg-resa.
           add tt-delta       to tg-delta.
           add tt-scostamento to tg-scostamento.

           move 0 to tt-resa.
           move 0 to tt-delta.
           move 0 to tt-scostamento.

      ***---
       TOTALI-GENERALI.
           if num-righe > max-righe - 4
              perform SALTO-PAGINA
           end-if.

           write line-riga from spaces.
           write line-riga from divisorio-1.
           move "T O T A L I   G E N E R A L I" to tot-tipo.
           move tg-resa        to tot-resa.
           move tg-delta       to tot-delta.
           move tg-scostamento to tot-scostamento.
           move r-totali       to line-riga.
           write line-riga.
           write line-riga   from divisorio-1.

      ***---
       CLOSE-FILES.
           close lineseq
                 statsett
                 statsett2
                 statraff
                 tmarche
                 articoli
                 ttipocli
                 progmagric
                 tmp-delta-marca
                 tcontat
                 tparamge
                 rmovmag
                 tendelta.

           delete file tmp-delta-marca.
  
      ***---
       EXIT-PGM.
           goback.

      ***---
       PARAGRAFO-COPY.
           copy "delta.cpy".
           copy "controlla-presenza-contab.cpy".
