       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      listini-forn-p.
       AUTHOR.                          Luciano.
       REMARKS.                         
           Importazione Listini Fornitori. 

      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "articoli.sl".
           copy "tlistini.sl".
           copy "rlistini.sl".
           copy "impforn.sl".
           copy "lineseq.sl". 
           copy "timposte.sl".
           copy "tpiombo.sl".
           copy "param.sl".
           copy "clienti.sl".

       SELECT rep-listini
           ASSIGN       TO  path-rep-listini
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE  IS SEQUENTIAL
           FILE STATUS  IS STATUS-rep-listini.


      *****************************************************************

       DATA DIVISION.
       FILE SECTION.
           copy "articoli.fd".
           copy "tlistini.fd".
           copy "rlistini.fd".
           copy "impforn.fd".
           copy "lineseq.fd".
           copy "timposte.fd".
           copy "tpiombo.fd".
           copy "param.fd".
           copy "clienti.fd".

       FD  rep-listini.
       01 rlst-rec         PIC  x(100).


      *****************************************************************

       WORKING-STORAGE SECTION.
      * COPY
           copy "acugui.def".
           copy "link-geslock.def".
           copy "common-excel.def".
           copy "prz-finito-forn.def".
           copy "imposte-fornitore.def".
           copy "trova-parametro.def".

       77  prg-peso-utf     PIC  9(5)v9(3).
       77  prg-peso-non-utf PIC  9(5)v9(3).

       77  user-codi        pic x(10).

      * COSTANTI
       78  titolo value "Importazione Listini Fornitori".

       01        pic 9.
           88 rec-locked value 1 false zero.

      * FILE-STATUS
       77  status-articoli           pic xx.
       77  status-tlistini           pic xx.
       77  status-rlistini           pic xx.
       77  status-impforn            pic xx.
       77  status-lineseq            pic xx.
       77  status-rep-listini        pic xx.
       77  status-timposte           pic xx.
       77  status-tpiombo            pic xx.
       77  status-param              pic xx.
       77  status-clienti            pic xx.
       77  wstampa                   pic x(256).
       77  path-rep-listini          pic x(256).

      * VARIABILI
       01  riga-errore.
           05 filler                 pic x(10) value "Record n° ".
           05 re-num-rec             pic z(6).
           05 filler                 pic x(2) value ": ".
           05 r-err                  pic x(80).

       01  riga-listino.
           10 rl-articolo       PIC  9(6).
           10 rl-descrizione    pic  x(50).
           10 rl-art-forn       PIC  x(20).
           10 rl-prz-acq        PIC  x(50).
           10 rl-sconto-1       PIC  x(50).
           10 rl-sconto-2       PIC  x(50).
           10 rl-sconto-3       PIC  x(50).
           10 rl-sconto-4       PIC  x(50).
           10 rl-sconto-5       PIC  x(50).
           10 rl-costi-agg      PIC  x(50).
           10 rl-perce-agg      PIC  x(50).
           10 rl-perce-pb       PIC  x(50).
           10 rl-netto          PIC  x(50).
           10 rl-disponibilita  PIC  9(9). 
           10 rl-tipo-imposte   PIC  x(50).
           10 rl-lead-time      PIC  x(50).
           10 rl-pfa            pic  x.

       77  como-rl-prz-acq      PIC  9(9)v9(4).
       77  como-rl-netto        PIC  9(9)v9(4).
      *
       77  como-data                 pic 9(8).
       77  como-ora                  pic 9(8).
       77  rec-ko                    pic 9(5) value 0.
       77  rec-ok                    pic 9(5) value 0.
       77  num-rec                   pic 9(5) value 0.
       77  num-rec-ed                pic zz.zzz.
       77  counter                   pic 9(10).
       77  counter2                  pic 9(10).
       77  counter-edit              pic z(10).

      * FLAGS
       01  controlli                 pic xx.
         88 tutto-ok                 value "OK".
         88 errori                   value "ER".

       01  filler                    pic 9.
         88 record-ok                value 1, false 0.

       01  filler                    pic 9.
         88 primo-errore             value 1, false 0.

       77  calcolo-piombo               pic 9.
           88  nuovo-calcolo-piombo     value 1, false 0.

       01  tipo-errore               pic 9.
           88 no-art                 value 1.
           88 no-prz                 value 2.
           88 no-disponibilita       value 3. 
           88 no-tipo-imp            value 4.
           88 no-piombo              value 5.
           88 lead-time-sbagliato    value 6.
           88 art-non-val            value 7.

       01                            pic 9.
           88 art-geslux             value 1 false zero.

       77  como-rl-lead-time         PIC  9(3).
       77  risultato                 pic 9(3).
       77  resto                     pic 9(3).

      *****************************************************************

       LINKAGE SECTION.
           copy "link-imp-listini-forn-p.def".

       PROCEDURE DIVISION USING  imp-listini-forn-p-linkage.

       DECLARATIVES.
      ***---
       ARTICOLI-ERR SECTION.
           use after error procedure on articoli.
           set tutto-ok  to true.
           evaluate status-articoli
           when "35"
                set errori to true
                display message "File [ARTICOLI] not found!"
                          title titolo
                           icon 3
           when "39"
                set errori to true
                display message "File [ARTICOLI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[ARTICOLI] Indexed file corrupt!"
                          title titolo
                           icon 3
           end-evaluate.

      ***---
       TLISTINI-ERR SECTION.
           use after error procedure on tlistini.
           set tutto-ok  to true.
           set rec-locked to false.
           evaluate status-tlistini
           when "35"
                set errori to true
                display message "File [TLISTINI] not found!"
                          title titolo
                           icon 3
           when "39"
                set errori to true
                display message "File [TLISTINI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TLISTINI] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "93"
           when "99"
                SET rec-locked  to true
           end-evaluate.

      ***---
       RLISTINI-ERR SECTION.
           use after error procedure on rlistini.
           set tutto-ok  to true.
           evaluate status-rlistini
           when "35"
                set errori to true
                display message "File [RLISTINI] not found!"
                          title titolo
                           icon 3
           when "39"
                set errori to true
                display message "File [RLISTINI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[RLISTINI] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "93"
           when "99"
                initialize geslock-messaggio
                string   "File già in uso!"
                  x"0d0a""Impossibile procedere!" delimited size
                      into geslock-messaggio
                end-string
                move 1 to geslock-v-riprova
                move 0 to geslock-v-ignora
                move 1 to geslock-v-termina
                move   "rlistini"    to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova
                     open i-o rlistini allowing readers
                when termina
                     set errori to true
                     display message "Operazione interrotta!"
                               title titolo
                                icon 2
                end-evaluate
           end-evaluate.

      ***---
       LINESEQ-ERR SECTION.
           use after error procedure on lineseq.
           set tutto-ok  to true.
           evaluate status-lineseq
           when "35"
                set errori to true
                display message "File [LINESEQ] not found!"
                          title titolo
                           icon 3
           when "39"
                set errori to true
                display message "File [LINESEQ] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[LINESEQ] Indexed file corrupt!"
                          title titolo
                           icon 3
           end-evaluate.

      ***---
       IMPFORN-ERR SECTION.
           use after error procedure on impforn.
           set tutto-ok  to true.
           evaluate status-impforn
           when "35"
                set errori to true
                display message "File [IMPFORN] not found!"
                          title titolo
                           icon 3
           when "39"
                set errori to true
                display message "File [IMPFORN] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[IMPFORN] Indexed file corrupt!"
                          title titolo
                           icon 3
           end-evaluate.

      ***---
       TIMPOSTE-ERR SECTION.
           use after error procedure on TIMPOSTE.
           set tutto-ok  to true.
           evaluate status-TIMPOSTE
           when "35"
                set errori to true
                display message "File [TIMPOSTE] not found!"
                          title titolo
                           icon 3
           when "39"
                set errori to true
                display message "File [TIMPOSTE] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TIMPOSTE] Indexed file corrupt!"
                          title titolo
                           icon 3
           end-evaluate.
      ***---
       TPIOMBO-ERR SECTION.
           use after error procedure on TPIOMBO.
           set tutto-ok  to true.
           evaluate status-TPIOMBO
           when "35"
                set errori to true
                display message "File [TPIOMBO] not found!"
                          title titolo
                           icon 3
           when "39"
                set errori to true
                display message "File [TPIOMBO] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TPIOMBO] Indexed file corrupt!"
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
           move ilf-user to user-codi.
           perform ACCETTA-SEPARATORE.
           set rec-locked to false
           set primo-errore  to true.
           accept como-data  from century-date.
           accept como-ora   from time.
           move 0 to counter counter2.
           set tutto-ok to true.

           move ilf-path  to wstampa.

           initialize path-rep-listini.
           accept  path-rep-listini from environment "PATH_ST".
           inspect path-rep-listini replacing trailing 
                                    spaces by low-value.
           string  path-rep-listini delimited low-value
                   "REP_LISTINI"    delimited size
                   "_"              delimited size
                   como-data        delimited size
                   "_"              delimited size
                   como-ora         delimited size
                   ".txt"           delimited size
                   into path-rep-listini
           end-string.

      ***---
       OPEN-FILES.
           open i-o tlistini.
           if tutto-ok
              open i-o rlistini
              if tutto-ok
                 open input articoli 
                 if tutto-ok
                    open input impforn param clienti
                    if tutto-ok
                        open input lineseq 
                        if tutto-ok
      *                    tolgo l'intestazione
                           initialize line-riga
                           read LINESEQ next
                              at end
                                 continue
                           end-read
                        else
                           close TLISTINI
                           close rlistini
                           close impforn
                           close articoli
                           display message box 
                                      "Flusso listini inesistente"
                        end-if
                    else
                       close TLISTINI
                       close rlistini
                       close articoli
                    end-if
                 else
                    close TLISTINI
                    close rlistini
                 end-if
              else
                 close tlistini
              end-if
           end-if.

           if tutto-ok
              open input timposte
              open input tpiombo
           end-if.

           if errori 
              goback 
           end-if.

      ***---
       ELABORAZIONE.
           set tutto-ok   to true
           accept como-data from century-date
           accept como-ora  from time

      *    controllo che il listino esista già altimenti lo creo
           if ilf-crea-listino
              perform CREA-LISTINO
           end-if

      *    leggo il listino con lock
       
           perform until 1 = 2
              move ilf-lis-chiave  to tlis-chiave
              set Rec-Locked       to false
              read tlistini with lock
                   invalid continue
              end-read

              if rec-locked
                 initialize geslock-messaggio
                 string   "File già in uso!"
                   x"0d0a""Impossibile procedere!" delimited size
                       into geslock-messaggio
                 end-string
                 move 1 to geslock-v-riprova
                 move 0 to geslock-v-ignora
                 move 1 to geslock-v-termina
                 move   "tlistini"    to geslock-nome-file
                 call   "geslock" using geslock-linkage
                 cancel "geslock"
                 evaluate true
                 when riprova
                      continue
                 when termina
                      set errori to true
                      display message "Operazione interrotta!"
                              title titolo
                              icon 2
                     exit perform
                 end-evaluate
              else
                 exit perform
              end-if
           end-perform.

           if tutto-ok
              perform CANCELLA-RIGHE
              perform IMPORTAZIONE
           end-if.

      ***---
       CANCELLA-RIGHE.
           move tlis-chiave  to rlis-codice.
           move low-value    to rlis-articolo.
           start rlistini key not < rlis-chiave
              invalid
                 continue
              not invalid
                 perform until 1 = 2
                    read rlistini next no lock
                       at end
                          exit perform
                    end-read
                    if tlis-chiave not = rlis-codice
                       exit perform
                    end-if
                    delete rlistini record
                       invalid
                          continue
                    end-delete
                 end-perform
           end-start.

      ***---
       CREA-LISTINO.
           move high-value    to tlis-codice
           start tlistini key >= tlis-chiave
                 invalid move zero   to tlis-codice
            not invalid
                 read tlistini previous no lock
                   at end move 0 to tlis-codice
                 end-read
           end-start.
           add 1 to tlis-codice
           move ilf-fornitore   to tlis-fornitore
           move ilf-destino     to tlis-destino
           move ilf-ini-val     to tlis-ini-val  
           move ilf-fine-val    to tlis-fine-val 
           move ilf-note        to tlis-descrizione

           set tlis-trasp-escluso  to true
           
           move como-data to tlis-data-creazione
                             tlis-data-ultima-modifica
           move como-ora  to tlis-ora-creazione
                             tlis-ora-ultima-modifica
           move user-codi to tlis-utente-creazione
                             tlis-utente-ultima-modifica

           move tlis-chiave  to ilf-lis-chiave.

           write tlis-rec invalid continue end-write.

      ***---
       IMPORTAZIONE.
           move como-data to tlis-data-ultima-modifica
           move como-ora  to tlis-ora-ultima-modifica
           move user-codi to tlis-utente-ultima-modifica

           move 0 to num-rec.
           perform until 1 = 2 

              add 1 to counter
              add 1 to counter2
              if counter2 = 10
                 move counter to counter-edit
                 display counter-edit
                    upon ilf-Handle at column 14 line 08
                 move 0 to counter2
              end-if

              set record-ok to true
              initialize tipo-errore

              initialize line-riga
              read lineseq next 
                 at end 
                    exit perform 
              end-read
              if line-riga = spaces    
                 exit perform 
              end-if
              unstring line-riga delimited by separatore
                       into rl-articolo
                            rl-descrizione
                            rl-art-forn
                            rl-prz-acq
                            rl-sconto-1
                            rl-sconto-2
                            rl-sconto-3
                            rl-sconto-4
                            rl-sconto-5 
                            rl-netto
                            rl-costi-agg
                            rl-perce-agg
                            rl-perce-pb
                            rl-disponibilita
                            rl-lead-time
                            rl-tipo-imposte
                            rl-pfa       
              end-unstring
              perform TRATTA-RECORD
           end-perform.

           write tlis-rec
              invalid
                 continue
           end-write.


           if rec-ko > 0
              write rlst-rec from spaces
              move all "-" to rlst-rec
              write rlst-rec 
              write rlst-rec from spaces
              move rec-ko to num-rec-ed
              string "Totale righe scartate: " delimited size
                     num-rec-ed                delimited size
                     into rlst-rec
              end-string
              write rlst-rec  after 2
              close REP-LISTINI
           end-if.

           if rec-ko > 0
              display message "Operazione terminata con errori!"
                      x"0d0a""====================="
                      x"0d0a""RIEPILOGO:"
                      x"0d0a"
                      x"0d0a""Totale righe listino " num-rec, " di cui:"
                      x"0d0a"" - " rec-ok, " importate"
                      x"0d0a"" - " rec-ko, " errate"
                      x"0d0a""Sarà visualizzato report riepilogativo..."
                      title titolo
                       icon 2
           else
              display message "Operazione conclusa con sucesso!"
                      x"0d0a""====================="
                      x"0d0a""RIEPILOGO:"
                      x"0d0a"
                      x"0d0a""Totale righe listino " num-rec, 
                      title titolo
           end-if.


      ***---
       CLOSE-FILES.
           close articoli 
                 lineseq 
                 tlistini 
                 rlistini
                 impforn
                 timposte
                 tpiombo
                 param
                 clienti.
           if rec-ko > 0 
              call   "spooler-a" using "A", path-rep-listini, "O"
              cancel "spooler-a"
              delete file rep-listini
           end-if.
              

      ***---
       EXIT-PGM.
           display "                                                "
                 upon ilf-handle at column 14 line 08
           goback.

      ***---
       TRATTA-RECORD.
           set tutto-ok   to true
           add 1 to num-rec.

           if rl-articolo = zero or rl-articolo >= 90000
              set art-geslux to false
           else
              set art-geslux to true
           end-if
              
           if rl-descrizione = space and rl-articolo = zero
              set errori        to true
              add 1 to rec-ko
              set art-non-val   to true
              perform SCRIVI-ERRORI

           end-if

           if tutto-ok
              perform VALIDA-ARTICOLO
           end-if
      *****     if tutto-ok
      *****        move rl-prz-acq   to como-rl-prz-acq convert
      *****        move rl-netto     to como-rl-netto   convert
      *****        if como-rl-prz-acq = 0 and como-rl-netto = 0
      *****           set errori  to true
      *****           add 1 to rec-ko
      *****           set no-prz  to true
      *****           perform SCRIVI-ERRORI
      *****        end-if
      *****     end-if.

           if tutto-ok
              if rl-disponibilita = zero
                 set errori  to true
                 add 1 to rec-ko
                 set no-disponibilita  to true
                 perform SCRIVI-ERRORI
              end-if
           end-if

           if tutto-ok
              move rl-tipo-imposte to imf-codice convert
              read impforn no lock
                 invalid
                    set errori  to true
                    add 1 to rec-ko
                    set no-tipo-imp  to true
                    perform SCRIVI-ERRORI
              end-read
           end-if.

           if tutto-ok
              move rl-lead-time to como-rl-lead-time convert
              divide como-rl-lead-time by 20 giving risultato
                          remainder resto

              if risultato = zero or resto not = zero
                 set errori  to true
                 add 1 to rec-ko
                 set lead-time-sbagliato  to true
                 perform SCRIVI-ERRORI
              end-if
           end-if

           if tutto-ok
              initialize rlis-rec  replacing alphanumeric by space  
                                   numeric by zero
              add 1 to rec-ok
              move tlis-chiave           to rlis-codice
              move tlis-chiave-ricerca   to rlis-chiave-ricerca
              move rl-articolo           to rlis-articolo    


              if rl-descrizione = space and art-geslux
                 move art-descrizione    to rlis-des-libera
              else
                 move rl-descrizione     to rlis-des-libera
              end-if

              move rl-art-forn           to rlis-art-forn    
              move rl-prz-acq            to rlis-prz-acq   convert
              move rl-sconto-1           to rlis-sconto-1  convert
              move rl-sconto-2           to rlis-sconto-2  convert
              move rl-sconto-3           to rlis-sconto-3  convert
              move rl-sconto-4           to rlis-sconto-4  convert
              move rl-sconto-5           to rlis-sconto-5  convert
              move rl-costi-agg          to rlis-costi-agg convert
              move rl-perce-agg          to rlis-perce-agg convert
              move rl-disponibilita      to rlis-disponibilita
              move rl-perce-pb           to rlis-perce-pb  convert
              move rl-netto              to rlis-netto     convert
              move rl-lead-time          to rlis-lead-time convert

              move rl-tipo-imposte    to rlis-tipo-tratt-imposte convert
              move 0 to como-trasporto
              move rl-pfa to rlis-pfa

              move costi-agg             to rlis-costi-agg-tot

      *       devo riforzare i valori perchè me li azzera il calcola prz
              move rl-sconto-1           to rlis-sconto-1  convert
              move rl-sconto-2           to rlis-sconto-2  convert   
              move rl-sconto-3           to rlis-sconto-3  convert
              move rl-sconto-4           to rlis-sconto-4  convert
              move rl-sconto-5           to rlis-sconto-5  convert
      *    
              move tlis-dati-comuni   to rlis-dati-comuni
              write rlis-rec invalid rewrite rlis-rec end-write
           end-if.

      ***---
       SCRIVI-ERRORI.
           if primo-errore
              set primo-errore  to false
              open output rep-listini

              move "Importazione Listini: riepilogo errori"  to rlst-rec
              call "c$justify" using rlst-rec, "C"
              write rlst-rec
              move all "-"   to rlst-rec
              write rlst-rec
           end-if

           move num-rec   to re-num-rec

           evaluate true
           when no-art
                move "articolo inesistente" to r-err
           when no-prz
                move "Prezzo non valorizzato" to r-err
           when no-disponibilita
                move "Disponibilità non valorizzata" to r-err
           when no-piombo
                move "Addizionale Piombo errata"  to r-err
           when no-tipo-imp
                move "Tipologia Imposte errata"  to r-err
           when lead-time-sbagliato
                move "Lead Time non multiplo di 20"  to r-err
           when art-non-val
                move "Codice articolo e descrizione non valorizzati"
                                                     to r-err
           end-evaluate

           write rlst-rec from riga-errore.

      ***---
       ATTRIBUISCI-CODICE.
           move tlis-chiave  to rlis-codice.
           move high-value   to rlis-articolo.
           start RLISTINI key not > rlis-chiave
              invalid
                 move 90000  to rl-articolo
              not invalid
                 read RLISTINI previous no lock
                    at end
                       move 90000  to rl-articolo
                    not at end
                       if rlis-articolo < 90000
                          move 90000  to rl-articolo
                       end-if
                 end-read
           end-start.
           add 1 to rl-articolo.

      ***---
       VALIDA-ARTICOLO.
           if art-geslux
              move rl-articolo  to art-codice
              read articoli
                 invalid
                    set errori  to true
                    add 1 to rec-ko
                    set no-art  to true
                    perform SCRIVI-ERRORI
              end-read
           else
      *    controllo tramite la descrizione di non aver già importato 
      *    l'articolo, se è così riutilizzo il codice
              move rl-descrizione  to rlis-des-libera
              move tlis-chiave     to rlis-codice
              start rlistini key not < rlis-k-descr of rlistini
                 invalid
                    perform DECIDI-CODICE
                 not invalid
                    read rlistini next no lock
                       at end
                          perform DECIDI-CODICE
                       not at end
                          if rl-descrizione not = rlis-des-libera or 
                             tlis-chiave    not = rlis-codice
                             perform DECIDI-CODICE
                          else
                             move rlis-articolo   to rl-articolo
                          end-if
                    end-read
              end-start
           end-if.

      ***---
       DECIDI-CODICE.
           if rl-articolo = zero
              perform ATTRIBUISCI-CODICE
           else
      *    leggo il listino per vedere se il codice è ancora disponibile
              move tlis-chiave  to rlis-codice
              move rl-articolo  to rlis-articolo
              read RLISTINI no lock
                   invalid continue
               not invalid perform ATTRIBUISCI-CODICE
              end-read
           end-if.

      ***---
           copy "common-excel.cpy".
           copy "imposte-fornitore.cpy".
           copy "addizionale-piombo-fornitore.cpy". 
           copy "trova-parametro.cpy".

      ***---
       CALCOLA-TRASPORTO. |DUMMY
