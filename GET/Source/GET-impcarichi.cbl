       PROGRAM-ID. get-impcarichi .
       AUTHOR.     Luciano.

       SPECIAL-NAMES. 
           DECIMAL-POINT IS COMMA.

       FILE-CONTROL.
           copy "tsetinvio.sl".
           copy "teva.sl".
           copy "reva.sl".
           copy "tordforn.sl".
           copy "rordforn.sl".
           copy "sordforn.sl".
           copy "paramget.sl".
           copy "progmag.sl".
           copy "articoli.sl".
           copy "tpiombo.sl".
           copy "tmarche.sl".
           copy "timposte.sl".
           copy "clienti.sl".
           copy "timbalqta.sl".
           copy "fileseq.sl".
           copy "param.sl".
           copy "lineseq.sl".
       select lineseq1
           assign       to  wstampa
           organization is line sequential
           access mode  is sequential
           file status  is status-lineseq.


       FILE SECTION.
           copy "tsetinvio.fd".          
           copy "teva.fd".
           copy "reva.fd".
           copy "tordforn.fd".
           copy "rordforn.fd".
           copy "sordforn.fd".
           copy "paramget.fd".
           copy "progmag.fd".
           copy "articoli.fd".
           copy "tpiombo.fd".
           copy "tmarche.fd".
           copy "timposte.fd".
           copy "clienti.fd".
           copy "timbalqta.fd".
           copy "param.fd".
           copy "fileseq.fd".
       FD  lineseq.
       01 line-riga        PIC  x(32000).
       FD  lineseq1.
       01 line-riga        PIC  x(32000).

       WORKING-STORAGE SECTION.
           COPY "comune.def".
           copy "imp-ws.def".

           COPY "AGGIORNA-STATO-ORDF.DEF".
           COPY "LINK-ST-BOZZE.DEF".
           COPY "LINK-WPROGMAG.DEF".
           COPY "LINK-NAMBAR.DEF".
           COPY "mail.DEF".
           copy "link-settaPDF.def".
           COPY "SELPRINT.LKS".
           copy "wait-3-secs.def".
           COPY "IMPOSTE.DEF".
           copy "link-readutente.def".
           copy "trova-parametro.def".

       01  imp-car-rec.
           05 C2COMM_COD       pic x(15).
           05 C2MAG_COD        pic x(2).
           05 C2FORN_COD       pic x(12).
           05 C2NUM_DOC        pic x(15).
           05 C2DATA_DOC       pic x(10).
           05 C2DATA_CAR       pic x(10).
           05 C2ARTICOLO       pic x(17).
           05 C2QTA            pic 9(11)v9(3).
           05 C2LOTTO          pic 9(12)v9(3).|x(15).
           05 C2UDM            pic x(3).
           05 C2ORD_RIF        pic x(20).
           05 C2DATA_RIF       pic x(10).
           05 C2CAUSA          pic x(2).
           05 C2STATO_M        pic x(2).
           05 C2UDF1           pic x(25).
           05 C2UDF2           pic x(25).
           05 C2UDF3           pic x(25).
           05 C2UDF4           pic x(25).
           05 C2UDF5           pic x(25).
           05 C2UDN1           pic 9(13).
           05 C2UDN2           pic 9(13).
           05 C2UDN3           pic 9(13).
           05 C2UDN4           pic 9(13).
           05 C2UDN5           pic 9(13).

       77  old-C2FORN_COD        pic x(12).

       01  old-tof-chiave.
           10 old-tof-anno         PIC  9(4).
           10 old-tof-numero       PIC  9(8).


      * FILE STATUS
       77  status-teva             pic xx.
       77  status-tsetinvio        pic xx.
       77  status-reva             pic xx.
       77  status-tordforn         pic xx.
       77  status-rordforn         pic xx.
       77  status-sordforn         pic xx.
       77  status-paramget         pic xx.
       77  status-progmag          pic xx.
       77  status-articoli         pic xx.
       77  status-tpiombo          pic xx.
       77  status-tmarche          pic xx.
       77  status-timposte         pic xx.
       77  status-clienti          pic xx.
       77  status-timbalqta        pic xx.
       77  status-lineseq          pic xx.
       77  status-param            pic xx.
      
      * SWITCHES

       01  filler                   pic 9.
           88 no-ordine             value 1 , false zero.

       01  filler                   pic 9.
           88 si-msg                value 1 , false zero.

      * OTHER DATA             
       77  como-data                pic 9(8)   value zero.
       77  como-ora                 pic 9(8)   value zero.

       77  titolo                   pic x(256).

       01  EXTEND-STAT.
           03 PRI-ERR               pic XX.
           03 SEC-ERR               pic X(10).

       77  TEXT-MESSAGE             pic X.

       77  wstampa                 pic x(256).
       77  path-er-flusso          pic x(256).

       77  record-counter pic 9(5).
       77  rec-counter-ed pic z(4)9.

       78  barra          value "\".

       77  user-codi      pic x(10).

       01 tab-ordf.
           05 tab-chiave
                      OCCURS 1000 TIMES
                      INDEXED  idx-ordf.
               10 tab-anno         PIC  9(4).
               10 tab-numero       PIC  9(8).

       01  save-rof-chiave.
           10 save-rof-chiave-testa.
              15 save-rof-anno         PIC  9(4).
              15 save-rof-numero       PIC  9(8).
           10 save-rof-riga         PIC  9(5).


       77  como-prz-unit PIC  9(9)v9999.

       77  teva-from      pic 9(8).
       77  teva-to        pic 9(8).
      
       77  mail           pic x(200).
      
      
       77  path-pdf          pic x(256).
                                            
       77  tentativi             pic 99.
      
       77  DestFile               pic x(256).
       77  NomeFile               pic x(256).
      
       77  cont           pic 9(5).

       01  FILE-INFO.
           02  FILE-SIZE    PIC X(8) COMP-X.
           02  FILE-DATE    PIC 9(8) COMP-X.
           02  FILE-TIME    PIC 9(8) COMP-X.
      
       77  old-SIZE         PIC X(8) COMP-X.


       77  status-code       pic 9.

       01                    pic 9.
           88 TIME-OUT-EXIT  value 1 false zero.

       77  MINUTI-PARTENZA   pic 99.
       77  MINUTI-arrivo     pic 99.

       77  como-nome-file         pic x(256).

       77 orig-imp-cons    PIC  s9(12)v9999.
       77 orig-coubat      PIC  s9(12)v9999.
       77 orig-add-pb      PIC  s9(12)v9999.

       77  rec-non-elab      pic 9(5).
       77  rec-elab          pic 9(5).

       LINKAGE SECTION.
           copy "link-imp.def".
      *-----------------------------------------------------------------*
       PROCEDURE DIVISION USING imp-linkage.

       DECLARATIVES.
       LINESEQ-ERR SECTION.
           use after error procedure on lineseq.
           set tutto-ok  to true.
           evaluate status-lineseq
           when "35"
                move "Impossibile procedere. File [LINESEQ] inesistente"
                          to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERRORE
           when "39"
                move "File [LINESEQ] Mismatch size!" to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERRORE
           when "98"
                move "[LINESEQ] Indexed file corrupt!" to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERRORE
           when "93"
                move "File già in uso! Impossibile procedere! Operazione 
      -              " interrotta!" 
                                                       to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERRORE
           end-evaluate.

       TEVA-ERR SECTION.
           use error  procedure on teva.
           set errori to true.

           evaluate status-teva
           when "35"
                move 
            "Impossibile procedere. File [TEVA] inesistente"
                          to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERRORE
           when "39"
                move "File [TEVA] Mismatch size!"  to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERRORE
           when "98"
                move "[TEVA] Indexed file corrupt!" 
                                                     to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERRORE
           when "99"
                initialize como-messaggio
                move record-counter   to rec-counter-ed
                string "record "               delimited by size
                       rec-counter-ed          delimited by size
                       ": Evasione Bloccata da altro utente!" 
                                               delimited by size
                       into como-messaggio
                set RecLocked      to true
                perform SCRIVI-ERRORE
           when other
                initialize como-messaggio
                call "C$RERR" using EXTEND-STAT, TEXT-MESSAGE
                string "Errore "               delimited size
                       EXTEND-STAT             delimited size
                       " sul file TEVA."   delimited size
                       " Contattare l'amministratore del Sistema."
                                               delimited size
                       into como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERRORE
           end-evaluate.


       REVA-ERR SECTION.
           use error  procedure on reva.
           set errori to true.

           evaluate status-reva
           when "35"
                move 
            "Impossibile procedere. File [REVA] inesistente"
                          to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERRORE
           when "39"
                move "File [REVA] Mismatch size!"  to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERRORE
           when "98"
                move "[REVA] Indexed file corrupt!" 
                                                     to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERRORE
           when "99"
                initialize como-messaggio
                move record-counter   to rec-counter-ed
                string "record "               delimited by size
                       rec-counter-ed          delimited by size
                       ": Riga Evasione Bloccata da altro utente!" 
                                               delimited by size
                       into como-messaggio
                set RecLocked      to true
                perform SCRIVI-ERRORE
           when other
                initialize como-messaggio
                call "C$RERR" using EXTEND-STAT, TEXT-MESSAGE
                string "Errore "               delimited size
                       EXTEND-STAT             delimited size
                       " sul file REVA."   delimited size
                       " Contattare l'amministratore del Sistema."
                                               delimited size
                       into como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERRORE
           end-evaluate.

       TORDFORN-ERR SECTION.
           use error  procedure on tordforn.
           set errori to true.

           evaluate status-tordforn
           when "35" 
                move 
            "Impossibile procedere. File [TORDFORN] inesistente"
                          to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERRORE
           when "99"
                set RecLocked      to true
           when "9D"
           when other
                initialize como-messaggio
                call "C$RERR" using EXTEND-STAT, TEXT-MESSAGE
                string "Errore "               delimited size
                       EXTEND-STAT             delimited size
                       " sul file TORDFORN."   delimited size
                       " Contattare l'amministratore del Sistema."
                                               delimited size
                       into como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERRORE
           end-evaluate.

       RORDFORN-ERR SECTION.
           use error  procedure on rordforn.
           set errori to true.

           evaluate status-rordforn
           when "35" 
                move 
            "Impossibile procedere. File [RORDFORN] inesistente"
                          to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERRORE
           when "99"
                set RecLocked      to true
           when "9D"
           when other
                initialize como-messaggio
                call "C$RERR" using EXTEND-STAT, TEXT-MESSAGE
                string "Errore "               delimited size
                       EXTEND-STAT             delimited size
                       " sul file RORDFORN."   delimited size
                       " Contattare l'amministratore del Sistema."
                                               delimited size
                       into como-messaggio
                perform SCRIVI-ERRORE
           end-evaluate.

       SORDFORN-ERR SECTION.
           use error  procedure on sordforn.
           set errori to true.

           evaluate status-sordforn
           when "35" 
                move 
            "Impossibile procedere. File [RORDFORN] inesistente"
                          to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERRORE
           when other
                initialize como-messaggio
                call "C$RERR" using EXTEND-STAT, TEXT-MESSAGE
                string "Errore "               delimited size
                       EXTEND-STAT             delimited size
                       " sul file SORDFORN."   delimited size
                       " Contattare l'amministratore del Sistema."
                                               delimited size
                       into como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERRORE
           end-evaluate.

      ***---
       PARAMget-ERR SECTION.
           use after error procedure on paramget.
           set tutto-ok  to true.
           evaluate status-paramget
           when "35"
                move 
            "Impossibile procedere. File vettori [PARAMget] inesistente"
                                                       to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERRORE
           when "39"
                move "File [PARAMget] Mismatch size!"  to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERRORE
           when other
                initialize como-messaggio
                call "C$RERR" using EXTEND-STAT, TEXT-MESSAGE
                string "Errore "               delimited size
                       EXTEND-STAT             delimited size
                       " sul file PROGMAG."    delimited size
                       " Contattare l'amministratore del Sistema."
                                               delimited size
                       into como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERRORE
           end-evaluate.

       PROGMAG-ERR SECTION.
           use error  procedure on progmag.
           set errori to true.

           evaluate status-progmag
           when "35"
                move 
            "Impossibile procedere. File [PROGMAG] inesistente"
                                                       to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERRORE
           when "9D"
           when other
                initialize como-messaggio
                call "C$RERR" using EXTEND-STAT, TEXT-MESSAGE
                string "Errore "               delimited size
                       EXTEND-STAT             delimited size
                       " sul file PROGMAG."   delimited size
                       " Contattare l'amministratore del Sistema."
                                               delimited size
                       into como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERRORE

           end-evaluate.

       ARTICOLI-ERR SECTION.
           use error  procedure on articoli.
           set errori to true.

           evaluate status-articoli
           when "35"
                move 
            "Impossibile procedere. File [ARTICOLI] inesistente"
                                                       to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERRORE
           when "9D"
           when other
                initialize como-messaggio
                call "C$RERR" using EXTEND-STAT, TEXT-MESSAGE
                string "Errore "               delimited size
                       EXTEND-STAT             delimited size
                       " sul file ARTICOLI."   delimited size
                       " Contattare l'amministratore del Sistema."
                                               delimited size
                       into como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERRORE
           end-evaluate.

       TPIOMBO-ERR SECTION.
           use error  procedure on tpiombo.
           set errori to true.

           evaluate status-tpiombo
           when "35"
                move 
            "Impossibile procedere. File [TPIOMBO] inesistente"
                                                       to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERRORE
           when "9D"
           when other
                initialize como-messaggio
                call "C$RERR" using EXTEND-STAT, TEXT-MESSAGE
                string "Errore "               delimited size
                       EXTEND-STAT             delimited size
                       " sul file TPIOMBO."   delimited size
                       " Contattare l'amministratore del Sistema."
                                               delimited size
                       into como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERRORE
           end-evaluate.

       TMARCHE-ERR SECTION.
           use error  procedure on tmarche.
           set errori to true.

           evaluate status-tmarche
           when "35"
                move 
            "Impossibile procedere. File [TMARCHE] inesistente"
                                                       to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERRORE
           when "9D"
           when other
                initialize como-messaggio
                call "C$RERR" using EXTEND-STAT, TEXT-MESSAGE
                string "Errore "               delimited size
                       EXTEND-STAT             delimited size
                       " sul file TMARCHE."   delimited size
                       " Contattare l'amministratore del Sistema."
                                               delimited size
                       into como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERRORE
           end-evaluate.

       TIMPOSTE-ERR SECTION.
           use error  procedure on timposte.
           set errori to true.

           evaluate status-timposte
           when "35"
                move 
            "Impossibile procedere. File [TIMPOSTE] inesistente"
                                                       to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERRORE
           when "9D"
           when other
                initialize como-messaggio
                call "C$RERR" using EXTEND-STAT, TEXT-MESSAGE
                string "Errore "               delimited size
                       EXTEND-STAT             delimited size
                       " sul file TIMPOSTE."   delimited size
                       " Contattare l'amministratore del Sistema."
                                               delimited size
                       into como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERRORE
           end-evaluate.

       CLIENTI-ERR SECTION.
           use error  procedure on clienti.
           set errori to true.

           evaluate status-clienti
           when "35"
                move 
            "Impossibile procedere. File [CLIENTI] inesistente"
                                                       to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERRORE
           when "9D"
           when other
                initialize como-messaggio
                call "C$RERR" using EXTEND-STAT, TEXT-MESSAGE
                string "Errore "               delimited size
                       EXTEND-STAT             delimited size
                       " sul file CLIENTI."    delimited size
                       " Contattare l'amministratore del Sistema."
                                               delimited size
                       into como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERRORE
           end-evaluate.

       TIMBALQTA-ERR SECTION.
           use error  procedure on timbalqta.
           set errori to true.

           evaluate status-timbalqta
           when "35"
                move 
            "Impossibile procedere. File [TIMBALQTA] inesistente"
                                                       to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERRORE
           when "9D"
           when other
                initialize como-messaggio
                call "C$RERR" using EXTEND-STAT, TEXT-MESSAGE
                string "Errore "               delimited size
                       EXTEND-STAT             delimited size
                       " sul file TIMBALQTA."  delimited size
                       " Contattare l'amministratore del Sistema."
                                               delimited size
                       into como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERRORE
           end-evaluate.

       END DECLARATIVES.


      ***---
       MAIN-PARAGRAPH.
           perform INIT.


           if tutto-ok 
              perform OPEN-FILES 
           end-if.

           if tutto-ok
              perform ELABORAZIONE
              perform CLOSE-FILES
           end-if.
           perform EXIT-PGM.

      ***---
       INIT.
           accept ru-user from environment "USER_CODI".
           call   "readutente" using ru-linkage.
           cancel "readutente".
           perform INITIALIZE-FLAG,
           set imp-carichi   to true.

           move zero      to idx
                             rec-non-elab
                             rec-elab

           move imp-user  to user-codi
           move low-value to old-C2FORN_COD.

           move  "GESLUX - Importazioni Carichi magazzino" to titolo

           set tutto-ok            to true.

           move zero      to imp-status.
           set RecLocked  to false.
           
           accept como-ora from time.
           accept como-data from century-date.

           move imp-path-log to path-er-flusso

           perform OPEN-PARAMget.

      ***---
       OPEN-FILES.
           perform OPEN-TEVA.
           perform OPEN-REVA.
           perform OPEN-TORDFORN
           perform OPEN-RORDFORN
           perform OPEN-SORDFORN
           open input progmag
           open input articoli
           open input tpiombo
           open input tmarche
           open input timposte
           open input clienti
           open input timbalqta
           open input param.
           if tutto-ok
              perform OPEN-INPUT-lineseq
           end-if.

      ***---
       OPEN-TEVA.
           set RecLocked to false.
           open i-o teva.
           
           if RecLocked
              set errori to true
              move -5 to imp-status
           end-if.

      ***---
       OPEN-REVA.
           set RecLocked to false.
           open i-o reva.
           
           if RecLocked
              set errori to true
              move -5 to imp-status
           end-if.

      ***---
       OPEN-TORDFORN.
           set RecLocked to false.
           open i-o tordforn.
           
           if RecLocked
              set errori to true
              move -5 to imp-status
           end-if.

      ***---
       OPEN-RORDFORN.
           set RecLocked to false.
           open i-o rordforn.
           
           if RecLocked
              set errori to true
              move -5 to imp-status
           end-if.

      ***---
       OPEN-SORDFORN.
           set RecLocked to false.
           open i-o sordforn.
           
           if RecLocked
              set errori to true
              move -5 to imp-status
           end-if.

      ***---
       OPEN-PARAMget.
           open input paramget.

           move space  to get-codice
           read paramget no lock 
              invalid 
                 set errori to true 
           end-read.

           if get-path-impcar   = space or
              get-file-carichi-impcar = space
              set errori to true
              set errore-bloccante to true
              move "Dati telematici dei carichi non valorizzati"
                    to como-messaggio
              perform SCRIVI-ERRORE
           end-if
           
           if tutto-ok
              perform PREPARA-PERCORSI
           end-if.

      ***---
       PREPARA-PERCORSI.
           inspect get-path-imp  replacing trailing space by low-value
           initialize wstampa
           string get-path-imp              delimited by low-value
                  barra                     delimited by size
                  get-file-carichi-impcar   delimited by size
                  into wstampa.

      ***---
       OPEN-INPUT-lineseq.
           set tutto-ok to true.
           open input lineseq.

           if tutto-ok
              initialize line-riga of lineseq
              read lineseq next
                 at end
                    set errori                    to true
                    move "Flusso di import vuoto!" to como-messaggio
                    perform SCRIVI-ERRORE
      *           not at end
      *              perform MOVE-TO-STRUTTURA
              end-read
              close lineseq
              open input lineseq
           end-if.

      ***---
       ELABORAZIONE.      
           accept imp-data from century-date.
           start timposte key <= imp-chiave
                 invalid continue
             not invalid
                 read timposte previous
           end-start.     
      *    primo ciclo di lettura per vedere quante evasioni devo creare
      *    per avere numerazioni consecutive
           perform CONTA-TEVA.

           if link-num-record not = zero
              perform VALORIZZA-NUMERO
              if tutto-ok
                 move zero   to record-counter
                 perform until 1 = 2
                    initialize line-riga of lineseq
                    read lineseq next 
                       at end 
                          exit perform 
                    end-read
                    if line-riga of lineseq(1:1)  = x"1A" 
                       exit perform
                    end-if
                    add 1 to record-counter
                    set tutto-ok   to true
                    perform MOVE-TO-STRUTTURA
                    if tutto-ok
                       perform VALORIZZA-RECORD
                    end-if
                 end-perform
                 close lineseq
                  
                 perform varying idx-ordf from 1 by 1 
                           until idx-ordf > idx
                    move tab-chiave(idx-ordf) to tof-chiave
                    perform AGGIORNA-STATO-ORDF
                 end-perform
              
              
                 perform INVIA-DATI
              
              end-if
           end-if.
           perform MSG-TEVA-ELABORATE.

      ***---
       MSG-TEVA-ELABORATE.
           initialize como-messaggio.
           move record-counter  to rec-counter-ed
           string "Letti         " delimited by size
                  rec-counter-ed   delimited by size 
                  " record"        delimited by size
                  into como-messaggio
           perform SCRIVI-MESSAGGIO.

           if rec-non-elab not = zero
              initialize como-messaggio
              move rec-non-elab  to rec-counter-ed
              string "Non elaborati " delimited by size
                     rec-counter-ed   delimited by size 
                     " record"        delimited by size
                     into como-messaggio
              perform SCRIVI-MESSAGGIO
           end-if
           
           if rec-elab not = zero
              initialize como-messaggio
              move rec-elab           to rec-counter-ed
              string "Elaborati     " delimited by size
                     rec-counter-ed   delimited by size 
                     " record"        delimited by size
                     into como-messaggio
              perform SCRIVI-MESSAGGIO
           
              initialize como-messaggio
              string "Generate le evasioni dalla numero " 
                                      delimited by size
                     teva-from        delimited by size 
                     " alle numero "  delimited by size
                     teva-to          delimited by size
                     into como-messaggio
              perform SCRIVI-MESSAGGIO
           end-if.

      ***---
       CONTA-TEVA.
           set si-msg  to true
           move zero   to link-num-record

           move zero   to record-counter
           perform until 1 = 2
              initialize line-riga of lineseq
              read lineseq next 
                 at end 
                    exit perform 
              end-read
              if line-riga of lineseq(1:1) = x"1A" 
                 exit perform
              end-if
              add 1 to record-counter
              set tutto-ok   to true
              perform MOVE-TO-STRUTTURA
              if tutto-ok
                 if C2FORN_COD not = old-C2FORN_COD
                    add 1 to link-num-record
                    move C2FORN_COD   to old-C2FORN_COD
                 end-if
              end-if
           end-perform.
           close lineseq.
           open input lineseq.
           move low-value to old-C2FORN_COD.
           set si-msg     to false.

      ***---
       VALORIZZA-RECORD.
      *     if C2CAUSA = "03"
      *        exit paragraph
      *     end-if.

           if C2FORN_COD not = old-C2FORN_COD
              perform VAL-TEVA
              move C2FORN_COD to old-C2FORN_COD
           end-if.

           add 1 to reva-riga

           move C2ARTICOLO(1:6) to reva-articolo convert
           move C2ARTICOLO(8:3) to reva-imballo
           move get-mag-get     to reva-codmag
           move C2LOTTO         to reva-peso

           move reva-chiave-progmag   to prg-chiave

           read progmag
              invalid
                 perform CREA-PROGMAG
           end-read.

           set no-ordine  to true
           if C2ORD_RIF not = space
              perform CERCA-ORDINE
           else
              perform CERCA-IN-VECCHI-ORDINI
           end-if.

           if no-ordine
              initialize tof-rec replacing numeric data by zeroes
                                           alphanumeric data by spaces
              initialize rof-rec replacing numeric data by zeroes
                                           alphanumeric data by spaces
           else
              set trovato to false
              set idx-ordf to 1
              search tab-chiave
              when tab-chiave(idx-ordf) = tof-chiave
                   set trovato to true
              end-search
              if not trovato
                 add 1 to idx
                 move tof-chiave to tab-chiave(idx)
              end-if
           end-if.

           move C2QTA  to reva-qta

           if not no-ordine
              perform AGGIORNA-RIGA-ORDINE
           end-if

           if no-ordine 
              move zero   to reva-prz-unit
                             reva-netto
                             reva-imp-cons
                             reva-coubat
                             reva-add-pb
                             reva-anno-ordf
                             reva-numero-ordf
                             reva-riga-ordf
           else
              perform VAL-IMPOSTE
              move rof-chiave   to reva-chiave-ordf
           end-if.

           set reva-attivo   to true

           move teva-dati-comuni   to reva-dati-comuni.

           write reva-rec
              invalid
                 continue
           end-write.

           perform AGGIORNA-PROGMAG.

           add 1 to rec-elab.

      ***---
       VAL-IMPOSTE.
           move prg-cod-articolo   to art-codice.
           read articoli
              invalid
                 continue
           end-read

           compute como-prz-unit =   rof-imponib-merce +
                                     rof-imp-consumo   +
                                     rof-imp-cou-cobat +
                                     rof-add-piombo

           move como-prz-unit   to como-prz-unitario

           if reva-chiave-progmag not = rof-prg-chiave
              perform RICALCOLO-IMPOSTE
           end-if


           add 0,0005 to como-prz-unit
           add 0,005  to como-prz-unit
           move como-prz-unit to reva-prz-unit
           
           add 0,0005 to rof-imp-consumo
           add 0,005  to rof-imp-consumo
           move rof-imp-consumo to reva-imp-cons
           
           add 0,0005 to rof-imp-cou-cobat
           add 0,005  to rof-imp-cou-cobat
           move rof-imp-cou-cobat to reva-coubat

           add 0,0005 to rof-add-piombo
           add 0,005  to rof-add-piombo
           move rof-add-piombo to reva-add-pb
           
           compute reva-netto = reva-prz-unit -
                                reva-imp-cons -
                                reva-coubat   -
                                reva-add-pb.

      ***---
       RICALCOLO-IMPOSTE.
           move prg-chiave to prg-chiave.
           read progmag no lock
              invalid 
                 move 0 to prg-peso-utf
           end-read.
           set TrattamentoGDO to true.
           accept como-data-ordine from century-date.
           move art-marca-prodotto to tpb-marca.
           move como-data-ordine   to tpb-data.
           move 0 to como-prm-cliente como-prm-destino.

           if como-prz-unitario not = 0
              perform CALCOLA-IMPOSTE
              perform ADDIZIONALE-PIOMBO
              |Se erano a zero significa che per qualche motivo 
              |(listino o manuale) non vanno impostate perciò
              |le lascio a zero 
              if orig-imp-cons = 0
                 move 0 to imposta-consumo
              end-if
              if orig-coubat = 0
                 move 0 to imposta-cou imposta-cobat
              end-if
              if orig-add-pb = 0
                 move 0 to add-piombo
              end-if
              move imposta-consumo to rof-imp-consumo
              add imposta-cou      to imposta-cobat giving imposta-cou
              move imposta-cou     to rof-imp-cou-cobat
              move add-piombo      to rof-add-piombo
              if como-prz-unitario < ( imposta-cou     + 
                                       imposta-consumo + 
                                       add-piombo )
                 compute como-prz-unitario = 
                         imposta-cou       +
                         imposta-consumo   +
                         add-piombo
                 move 0 to como-imposta
              else
                 compute como-imposta = como-prz-unitario - 
                                        imposta-cou       -
                                        imposta-consumo   -
                                        add-piombo
              end-if
              move como-imposta to rof-imponib-merce
           else
              move 0 to imposta-consumo rof-imp-consumo
                        imposta-cou     rof-imp-cou-cobat
                        add-piombo      rof-add-piombo rof-imponib-merce
           end-if.

      ***---
       AGGIORNA-RIGA-ORDINE.
           set tutto-ok  to true.

           perform 20 times
              set RecLocked to false
              read tordforn lock key tof-chiave 
                 invalid 
                    continue 
              end-read
              if not RecLocked
                 exit perform
              end-if
              call "c$sleep" using 1
           end-perform

           if RecLocked
              initialize como-messaggio
              string "Ord. fornitore anno: " delimited size
                     tof-anno                delimited size
                     " n. "                  delimited size
                     tof-numero              delimited size
                     " "                     delimited size
                     "Risulta bloccato su un altro terminale. "
                                             delimited size
                     "Sarà impossibile associarlo all'evasione: "
                                             delimited size
                     teva-anno               delimited size
                     " n. "                  delimited size
                     teva-numero             delimited size
                     " riga "                delimited size
                     reva-riga               delimited size
                     into como-messaggio
                perform SCRIVI-ERRORE
           end-if.


           set tutto-ok  to true.

           perform 20 times
              set RecLocked to false
              read rordforn lock key rof-chiave 
                 invalid 
                    continue 
              end-read
              if not RecLocked
                 exit perform
              end-if
              call "c$sleep" using 1
           end-perform

           if RecLocked
              initialize como-messaggio
              string "Ord. fornitore anno: " delimited size
                     tof-anno                delimited size
                     " n. "                  delimited size
                     tof-numero              delimited size
                     " riga "                delimited size
                     rof-riga                delimited size
                     " "                     delimited size
                     "Risulta bloccato su un altro terminale. "
                                             delimited size
                     "Sarà impossibile associarlo all'evasione: "
                                             delimited size
                     teva-anno               delimited size
                     " n. "                  delimited size
                     teva-numero             delimited size
                     " riga "                delimited size
                     reva-riga               delimited size
                     into como-messaggio
                perform SCRIVI-ERRORE
                move -1 to imp-status
           end-if.

           if not RecLocked
              perform STORNA-ORDINATO
              add  reva-qta                 to rof-qta-evasa

              move reva-chiave-progmag  to prg-chiave
              read progmag no lock 
                 invalid 
                    continue 
              end-read
              move reva-imballo             to rof-imb-ordinato

              move como-data to rof-data-ultima-modifica
              move como-ora  to rof-ora-ultima-modifica
              move user-codi to rof-utente-ultima-modifica
              rewrite rof-rec 
                 invalid 
                    continue 
              end-rewrite
              unlock rordforn all records
              unlock tordforn all records

              move rof-chiave to sof-chiave
              delete sordforn record 
                 invalid 
                    continue 
              end-delete
           end-if.

      ***---
       STORNA-ORDINATO.
           |Adesso devo stornare l'ordinato
           set link-update            to true.
           move tof-causale           to link-causale
           move rof-prg-chiave        to link-key.
           move tof-mese-rif          to link-mese-rif.
           move tof-chiave            to link-chiave-origine.

           move user-codi             to link-user.
      *    Lo tolgo dal vecchio progressivo
           move reva-qta              to link-valore.
           set link-update-um         to true.
           set link-update-peso       to false.
           set link-update-valore     to false.

           |Se è scoperto (non ho ancora scritto)
           if rof-qta-ord > rof-qta-evasa
              if reva-qta > rof-qta-ord - rof-qta-evasa
                 compute link-valore = rof-qta-ord - rof-qta-evasa
              else
                 move reva-qta        to link-valore
              end-if
           end-if.

           if link-valore > 0
              move "0000000000000000" to link-array
              move -1 to multiplyer(3)
              call   "wprogmag" using link-wprogmag
              cancel "wprogmag"
           end-if.


      ***---
       AGGIORNA-PROGMAG.
           if no-ordine
              move get-causale-impcar to link-causale
           else           
              move tof-causale        to link-causale
           end-if
           set link-update            to true.
           set link-update-um         to true.
           set link-update-peso       to false.
           set link-update-valore     to false.

           move tof-mese-rif          to link-mese-rif.
           move tof-chiave            to link-chiave-origine.
      *    Dopodiché aggiungo direttamente la qta alla giacenza
           move reva-chiave-progmag   to link-key.
           move reva-qta              to link-valore.
           move "1000000000000010"    to link-array.
           call   "wprogmag"        using link-wprogmag.
           cancel "wprogmag".


      ***---
       CERCA-ORDINE.
           move C2DATA_RIF(1:4) to tof-anno   convert
           move C2ORD_RIF       to tof-numero convert

           read tordforn no lock
              invalid
                 continue
              not invalid
                 perform CERCA-IN-ORDINE
           end-read.
           if no-ordine
              continue
           end-if.

      *     if not no-ordine
      *        set trovato to false
      *        set idx-ordf to 1
      *        search tab-chiave
      *        when tab-chiave(idx-ordf) = tof-chiave
      *             set trovato to true
      *        end-search
      *        if not trovato
      *           add 1 to idx
      *           move tof-chiave to tab-chiave(idx)
      *        end-if
      *     end-if.

      ***---
       CERCA-IN-ORDINE.
      *    cerco per prima cosa il match completo del progressivo 
      *    nell'ordine passato da get, intanto cerco anche se nello
      *    stesso ordine ho la compatibilità di una riga con l'articolo.
      *    La qta ancora da evadere deve essere >= della qta da aggiornare
           initialize save-rof-chiave
           move tof-chiave   to rof-chiave-testa
           move low-value    to rof-riga
           start rordforn key not < rof-chiave
              invalid
                 continue
              not invalid
                 perform until 1 = 2
                    read rordforn next no lock
                       at end
                          exit perform
                    end-read
                    if tof-chiave not = rof-chiave-testa
                       exit perform
                    end-if
                    if rof-prg-chiave = prg-chiave
                       if (rof-qta-ord - rof-qta-evasa) >= C2QTA
                          set no-ordine  to false
                          exit perform
                       end-if
                    end-if
                    if rof-cod-articolo = prg-cod-articolo
                       if (rof-qta-ord - rof-qta-evasa) >= C2QTA
                          if save-rof-chiave = zero
                             move rof-chiave   to save-rof-chiave
                          end-if
                       end-if
                    end-if
                 end-perform
           end-start.

           if save-rof-chiave not = zero
              move save-rof-chiave to rof-chiave
              set no-ordine  to false
           end-if.

      *    se non ho ancora l'ordine faccio la ricerca a partire dagli 
      *    ordini più vecchi
           if no-ordine
              perform CERCA-IN-VECCHI-ORDINI
           end-if.

      ***---
       CERCA-IN-VECCHI-ORDINI.
           move reva-articolo   to rof-cod-articolo
           move reva-codmag     to rof-cod-magazzino
           move low-value       to rof-chiave

           start rordforn key not < rof-k-art-mag
              invalid
                 continue
              not invalid
                 perform until 1 = 2
                    read RORDFORN next no lock
                       at end
                          exit perform
                    end-read
                    if reva-articolo not = rof-cod-articolo or
                       reva-codmag   not = rof-cod-magazzino
                       exit perform
                    end-if

                    if (rof-qta-ord - rof-qta-evasa) >= C2QTA
                       perform VERIFICA-FORNITORE-STATO
                       if tutto-ok
                          set no-ordine  to false                       
                          exit perform
                       end-if
                    end-if
                 end-perform
           end-start.

      ***---
       VERIFICA-FORNITORE-STATO.
           set errori              to true
           move rof-chiave-testa   to tof-chiave
           read TORDFORN no lock
              invalid
                 set tof-inserito  to true
                 initialize tof-cod-forn
           end-read

           if tof-cod-forn = C2FORN_COD
              evaluate true
              when tof-inviato
              when tof-in-lavorazione
                   set tutto-ok to true
              end-evaluate
           end-if.

      ***---
       CREA-PROGMAG.
           initialize link-wprogmag replacing numeric data by zeroes
                                         alphanumeric data by spaces.

           set link-batch       to true.
           move user-codi   to link-user.
           move prg-cod-articolo   to link-articolo
                                      art-codice.
           read articoli
              invalid
                 continue
           end-read
           move prg-cod-magazzino  to link-magazzino.
           move prg-tipo-imballo   to link-imballo
           move prg-peso           to link-non-utf.

           move art-peso-utf       to link-utf.

           add link-utf to link-non-utf giving link-peso.

           call   "wprogmag" using link-wprogmag.
           cancel "wprogmag".

      ***---
       VAL-TEVA.
           initialize teva-dati replacing numeric data by zeroes
                                    alphanumeric data by spaces.

           add 1 to teva-numero
           move link-anno    to teva-anno
           accept teva-data  from century-date
           move "F"          to teva-tipo-F
           move C2FORN_COD   to teva-cod-forn convert

           move get-mag-get  to teva-mag
           set  teva-aperta  to true

           move como-data    to teva-data-creazione
           move como-ora     to teva-ora-creazione
           move user-codi to teva-utente-creazione

           write teva-rec 
              invalid 
                 continue 
           end-write

           move teva-chiave  to reva-chiave-testa.
           move zero         to reva-riga.
       
      ***---
       VALORIZZA-NUMERO.
           accept esercizio-x from environment "ESERCIZIO".
           move   esercizio-x to esercizio.

           move esercizio     to link-anno.

           set  link-evasione-imp  to true.
           set  link-crea          to true.

           call   "nambar" using link-nambar.
           cancel "nambar".
           
           if link-status-nambar = -1 
              set errori           to true
              set errore-bloccante to true
              initialize como-messaggio
              string "Errore nell'assegnazione del numero d'evasione."
                                                        delimited size
                      " Impossibile procedere."
                                         delimited size
                       into como-messaggio
                perform SCRIVI-ERRORE
           else                       
              move link-numero to teva-numero
              add 1 to teva-numero giving teva-from
              add link-num-record  to teva-numero giving teva-to
           end-if.

      ***---
       MOVE-TO-STRUTTURA.
           initialize imp-car-rec replacing numeric data by zeroes
                                       alphanumeric data by spaces.

           move line-riga of lineseq to imp-car-rec.
           perform VALIDA-RECORD.

      ***---
       CLOSE-FILES.
           close teva
                 reva
                 tordforn
                 rordforn
                 paramget
                 progmag
                 articoli
                 tpiombo
                 tmarche
                 timposte
                 clienti
                 timbalqta
                 param.


      * Non considera gli stati di inserito, accettato e chiuso.
      ***---
       AGGIORNA-STATO-ORDF.
           set tutto-ok  to true.
           set RecLocked to false.

           set tutto-ok to true.

           perform 20 times
              read tordforn lock key tof-chiave 
                 invalid 
                    continue 
              end-read
              if not RecLocked
                 exit perform
              end-if
              call "c$sleep" using 1
           end-perform

           if RecLocked
              initialize como-messaggio
              string "Ord. fornitore anno: " delimited size
                     tof-anno                delimited size
                     " n. "                  delimited size
                     tof-numero              delimited size
                     " Risulta bloccato su un altro terminale. "
                                             delimited size
                     "Sarà impossibile aggiornarne lo stato."
                                             delimited size
                     into como-messaggio
              perform SCRIVI-ERRORE
           end-if.

           if not RecLocked
              move low-value  to rof-chiave
              move tof-chiave to rof-chiave-testa
              start rordforn key >= rof-chiave
                    invalid continue
                not invalid
                    set evaso-tutto to true
                    move 0 to tot-pz-ord
                    move 0 to tot-pz-arr
                    perform until 1 = 2
                       read rordforn next at end exit perform end-read
                       if rof-chiave-testa not = tof-chiave
                          exit perform
                       end-if
                       move 0 to rof-qta-evasa
                       move low-value  to reva-rec
                       move rof-chiave to reva-chiave-ordf
                       start reva key >= reva-chiave-ordf
                             invalid continue
                         not invalid
                             perform until 1 = 2
                                read reva next no lock
                                     at end exit perform
                                end-read
                                if reva-chiave-ordf not = rof-chiave
                                   exit perform
                                end-if
                                add reva-qta to rof-qta-evasa
                             end-perform
                       end-start
                       rewrite rof-rec
                       if rof-qta-ord > rof-qta-evasa
                          set evaso-tutto to false
                       end-if
                       add rof-qta-ord   to tot-pz-ord
                       add rof-qta-evasa to tot-pz-arr
                    end-perform
                    move tot-pz-ord to tof-pz-tot
                    move tot-pz-arr to tof-pz-arrivati

                    |STATO ORDINE
                    if tof-inserito or tof-chiusura-man |or tof-accettato
                       continue
                    else
                       if tof-pz-arrivati not = 0
                          if evaso-tutto
                             if tof-da-confermare-no
                                set tof-chiuso        to true
                                set tof-chiusura-auto to true
                              accept tof-data-chiusura from century-date
                                accept tof-ora-chiusura  from time
                                move user-codi to tof-utente-chiusura
                             end-if
                          else
                             set tof-in-lavorazione to true
                          end-if
                       else
                          set tof-inviato to true
                       end-if

                       |STATO EVASIONE
                       if tof-pz-arrivati = 0
                          set tof-inevaso to true
                       else
                          if not evaso-tutto
                             set tof-evas-parz to true
                          else
                             set tof-evas-tot to true
                          end-if
                       end-if

                       rewrite tof-rec invalid continue end-rewrite
                    end-if

              end-start         
              unlock tordforn all records
           end-if.

      ***---
       INVIA-DATI.
           if get-mail-impcar = space and
              get-mail-cc-impcar = space
              move "Impossibile inviare la stampa, in quanto non sono pr
      -            "esenti indirizzi a cui spedirla"
                             to como-messaggio
              perform SCRIVI-MESSAGGIO
              exit paragraph
           end-if

           perform CREA-PDF
           evaluate true
           when ru-SO-XP
                accept selprint-stampante
                        from environment "STAMPANTE_IMPCARICHI_PDF_XP"
           when ru-SO-VISTA
                accept selprint-stampante
                        from environment "STAMPANTE_IMPCARICHI_PDF_V"
           when ru-SO-7
                accept selprint-stampante
                        from environment "STAMPANTE_IMPCARICHI_PDF_7"
           end-evaluate.

           if selprint-stampante not = space
              move selprint-linkage to st-bozze-selprint-linkage
              move esercizio to stobz-da-teva-anno
                                stobz-a-teva-anno
              move teva-from to stobz-da-teva-numero
              move teva-to   to stobz-a-teva-numero
              call   "st-bozze"  using st-bozze-linkage
              cancel "st-bozze"
           end-if

           perform ASPETTA-PDF.

           move "Bozze d'evasione get"  to LinkSubject
           perform PREPARA-CORPO.
           perform PREPARA-INDIRIZZO.
           perform PREPARA-INDIRIZZO-CC.
      *     perform PREPARA-FROM.
           move path-pdf   to LinkAttach.

           set errori to true.
           move 0 to tentativi.
           move "GET-impcarichi" to NomeProgramma.
           perform 5 times
              add 1 to tentativi
              perform SEND-MAIL

      *        call "C$DELETE" using FileDest
              open input lineseq1
              read  lineseq1 next
              if line-riga of lineseq1 = "True"
                 set tutto-ok to true
                 close lineseq1
                 exit perform
              end-if
              close lineseq1
           end-perform.

           if errori
              move "Errore durante l'invio della stampa via mail"
                       to como-messaggio
              perform SCRIVI-MESSAGGIO
           end-if.

      ***---
       CREA-PDF.
           accept como-ora  from time.
           accept como-data from century-date.
           accept DestFile from environment "PATH_ST_CLIENT".

           inspect DestFile replacing trailing 
                                     spaces by low-value.

           string "Bozze_evasione_get"   delimited size  
                  "_("                   delimited size
                  como-data(7:2)         delimited by size
                  "-"                    delimited size
                  como-data(5:2)         delimited by size
                  "-"                    delimited size
                  como-data(1:4)         delimited by size
                  "_-_"                  delimited size
                  como-ora(1:2)          delimited size
                  "."                    delimited by size
                  como-ora(3:2)          delimited size
                  ")"                delimited size
                  into NomeFile
           end-string.
                  
           set settaPDF-setta   to true

           move NomeFile  to settaPDF-nome-file
           move DestFile  to settaPDF-percorso
           call   "settaPDF" using settaPDF-linkage
           cancel "settaPDF"

           inspect NomeFile 
                   replacing trailing spaces by low-value.
           string NomeFile   delimited low-value
                  ".pdf"     delimited size
                  into NomeFile

           inspect DestFile 
                   replacing trailing spaces by low-value.

           if not settaPDF-OK       
              display message "Archiviazione PDF fallita!"
                        title titolo
                         icon 2
           end-if.

           inspect NomeFile 
                   replacing trailing spaces by low-value.
           inspect DestFile 
                   replacing trailing spaces by low-value.

           initialize path-pdf
           string DestFile   delimited by low-value
                  NomeFile   delimited by low-value
                  into path-pdf.


      ***---
       PREPARA-CORPO.
           initialize linkBody
           string "In allegato le bozze generate in data " 
                                      delimited by size
                  como-data(7:2)      delimited by size
                  "/"                 delimited by size
                  como-data(5:2)      delimited by size
                  "/"                 delimited by size
                  como-data(1:4)      delimited by size
                  " alle "            delimited by size
                  como-ora(1:2)       delimited by size
                  ":"                 delimited by size
                  como-ora(3:2)       delimited by size
                  into LinkBody.

      ***---
       PREPARA-INDIRIZZO.
           move get-mail-impcar to mail
           initialize LinkAddress
           inspect mail replacing trailing space by low-value
           initialize cont 
           inspect mail tallying cont for characters before low-value
           if mail(cont:1) = ";"
              move low-value to mail(cont:1)
           end-if
           inspect mail replacing trailing low-value by space
           move mail   to LinkAddress.


      ***---
       PREPARA-INDIRIZZO-cc.
           move get-mail-cc-impcar to mail
           initialize LinkAddressCC
           inspect mail replacing trailing space by low-value
           initialize cont 
           inspect mail tallying cont for characters before low-value
           if mail(cont:1) = ";"
              move low-value to mail(cont:1)
           end-if
           inspect mail replacing trailing low-value by space
           move mail   to LinkAddressCC.


      ***---
       ASPETTA-PDF.
           move path-pdf to como-nome-file.
           move 0 to cont.
           inspect como-nome-file 
                   tallying cont for characters before ")".
           move ".pdf " to como-nome-file(cont + 1: 5).

           set trovato to false.
           perform 60 times
              CALL "C$FILEINFO" USING path-pdf,
                                      file-info, 
                               GIVING status-code
              if status-code = 0
                 set trovato to true
                 exit perform
              else
                 CALL "C$FILEINFO" USING como-nome-file,
                                         file-info, 
                                  GIVING status-code
                 if status-code = 0
                    move como-nome-file to path-pdf
                    set trovato to true
                    exit perform
                 end-if
              end-if
              call "c$sleep" using 1
           end-perform.

           if trovato
              move 0  to old-size
      *       aspetto finchè non esiste fisicamente il file
              move 99 to minuti-partenza
              perform until 1 = 2
                 CALL "C$FILEINFO" USING path-pdf,
                                         file-info, 
                                  GIVING status-code
              
                 if status-code = 0
                    if FILE-SIZE not = 0
                       if FILE-SIZE = old-size
                          exit perform
                       else
                          move FILE-SIZE to old-size
                          call "c$sleep" using 1
                       end-if
                    end-if
                 else
                    perform TIME-OUT
                    if time-out-exit
                       exit perform
                    end-if
                 end-if
              
              end-perform
           end-if.

           set settaPDF-resetta   to true.
           call   "settaPDF" using settaPDF-linkage
           cancel "settaPDF".


      ***---
       TIME-OUT.
      *    tengo 2 minuti di time-out
           set time-out-exit to false.
           accept como-time from time.

           if minuti-partenza = 99
              move como-time(3:2)  to minuti-partenza
           end-if.

           move como-time(3:2)  to minuti-arrivo.

           if minuti-arrivo < minuti-partenza
              add 60 to minuti-arrivo
           end-if.
           subtract minuti-partenza from minuti-arrivo.

           if minuti-arrivo >= 3
              set time-out-exit to true
           else
              call "c$sleep" using 1
           end-if.

      ***---
       VALIDA-RECORD.
           set tutto-ok         to true
           move record-counter  to rec-counter-ed

           if C2CAUSA = "03"
              set errori  to true
              if si-msg
                 initialize como-messaggio
                 string "record n "      delimited by size
                         rec-counter-ed  delimited by size
                         " non elaborato in quanto è un reso"
                                         delimited by size
                         into como-messaggio
                 perform SCRIVI-ERRORE
              end-if
           end-if

           if tutto-ok
              move C2FORN_COD   to cli-codice convert
              set cli-tipo-f    to true
              read clienti no lock
                 invalid
                    set errori  to true
                    if si-msg
                       initialize como-messaggio
                       string "record n "      delimited by size
                              rec-counter-ed   delimited by size
                              " non elaborato: Fornitore inesistente"
                                               delimited by size
                              into como-messaggio
                       perform SCRIVI-ERRORE
                    end-if
              end-read

              move C2ARTICOLO(1:6) to art-codice convert
              read articoli no lock
                 invalid
                    set errori  to true
                    if si-msg
                       initialize como-messaggio
                       string "record n "      delimited by size
                              rec-counter-ed   delimited by size
                              " non elaborato: Articolo inesistente"
                                               delimited by size
                              into como-messaggio
                       perform SCRIVI-ERRORE
                    end-if
              end-read


              move C2ARTICOLO(8:3) to imq-codice
              read timbalqta no lock
                 invalid
                    set errori  to true
                    if si-msg
                       initialize como-messaggio
                       string "record n "      delimited by size
                              rec-counter-ed   delimited by size
                              " non elaborato: Tipo imballo inesistente"
                                               delimited by size
                              into como-messaggio
                       perform SCRIVI-ERRORE
                    end-if
              end-read

           
              move C2LOTTO to reva-peso
              if reva-peso = zero
                 set errori  to true
                 if si-msg
                    initialize como-messaggio
                    string "record n "      delimited by size
                           rec-counter-ed   delimited by size
                           " non elaborato: Peso non valorizzato"
                                            delimited by size
                           into como-messaggio
                    perform SCRIVI-ERRORE
                 end-if
              end-if

              if C2QTA = zero
                 set errori  to true
                 if si-msg
                    initialize como-messaggio
                    string "record n "      delimited by size
                           rec-counter-ed   delimited by size
                           " non elaborato: Quantità non valorizzato"
                                            delimited by size
                           into como-messaggio
                    perform SCRIVI-ERRORE
                 end-if
              end-if
           end-if.

           if errori and si-msg
              add 1 to rec-non-elab
           end-if.

      **---
       PARAGRAFO-COPY.
           copy "mail.cpy".
           copy "imposte.cpy".
           copy "addizionale-piombo.cpy".
           COPY "imp-procedure.cpy".
           copy "trova-parametro.cpy".
