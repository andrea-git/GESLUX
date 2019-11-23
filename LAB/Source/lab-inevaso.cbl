       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      lab-inevaso.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.
           copy "tagli.sl".
           copy "articoli.sl". 
           copy "tgrupgdo.sl".
           copy "tmarche.sl".
           copy "tmp-inevaso.sl".
           copy "tmp-marca.sl".
           copy "tmp-tot-marca.sl".
           copy "mtordini.sl".
           copy "mrordini.sl".
           copy "tpromo.sl".
           copy "clienti.sl".   
           copy "lineseq.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tagli.fd".
           copy "articoli.fd".
           copy "tgrupgdo.fd".
           copy "tmarche.fd".
           copy "tmp-inevaso.fd".
           copy "tmp-marca.fd".
           copy "tmp-tot-marca.fd".
           copy "mtordini.fd".
           copy "mrordini.fd".
           copy "tpromo.fd".
           copy "clienti.fd".  
           copy "lineseq.fd".

       WORKING-STORAGE SECTION.
      * COPY
           copy "link-geslock.def".
           copy "spooler.def".
           copy "fonts.def".
           copy "selprint.lks".
           copy "pie-di-pagina.def".
           copy "link-settaPDF.def".
           copy "wait-3-secs.def".
           copy "link-readutente.def".    

       01  r-inizio.
         05 filler                 pic x(2)  value " [".
         05 r-data.
            10 r-gg                pic xx.
            10 filler              pic x     value "/".
            10 r-mm                pic xx.
            10 filler              pic x     value "/".
            10 r-aa                pic xx.
         05 filler                 pic x(5)  value "] - [".
         05 r-ora.
            10 r-hh                pic xx.
            10 filler              pic x     value X"22".
            10 r-min               pic xx.
            10 filler              pic x     value "'".
            10 r-sec               pic xx.
         05 filler                 pic x(2)  value "] ".
                                          
       77  como-riga             pic x(400).
       77  riga-stampa           pic x(400).                                
       77  counter               pic 9(9) value 0.
       77  counter2              pic 9(9) value 0.
       77  counter-edit          pic zzz.zzz.zz9.

       77  status-tagli          pic xx.
       77  status-articoli       pic xx.
       77  status-tgrupgdo       pic xx.
       77  status-tmarche        pic xx.
       77  status-tmp-inevaso    pic xx.
       77  status-tmp-marca      pic xx.
       77  status-tmp-tot-marca  pic xx.
       77  status-mtordini       pic xx.
       77  status-mrordini       pic xx.
       77  status-tpromo         pic xx.
       77  status-clienti        pic xx.
       77  status-lineseq        pic xx.

       77  wstampa               pic x(256).

       77  path-tmp-inevaso      pic x(256).
       77  path-tmp              pic x(256).
       77  path-tmp-tot-marca    pic x(256).

      * COSTANTI
       78  titolo                value "GESLUX - Inevaso giornaliero".
       78  78-PiePagina          value 1.
       78  78-MaxRows            value 44.

      * RIGHE PER LA STAMPA
       01  r-titolo              pic x(40).

       01  r-data-sta            pic x(10).

       01  r-intesta-s2.
           05 filler             pic x(7)  value "Cliente".
           05 filler             pic x(8)  value "Articolo".
           05 filler             pic x(11) value "Descrizione".
           05 filler             pic x(8)  value "Quantità".
           05 filler             pic x(6)  value "Prezzo".
           05 filler             pic x(6)  value "Totale".

       01  r-intesta.
           05 filler             pic x(8)  value "Articolo".
           05 filler             pic x(11) value "Descrizione".
           05 filler             pic x(8)  value "Quantità".
           05 filler             pic x(6)  value spaces.
           05 filler             pic x(6)  value "Totale".

       01  r-riga.
           05 r-gdo-intestazione pic x(35).
           05 r-art              pic z(6).
           05 r-art-descrizione  pic x(50).
           05 r-qta              pic zzz.zz9.
           05 r-prz              pic x(14).
           05 r-tot              pic x(14).

       01  r-totale.
           05 filler             pic x(19) value "Totale giornaliero:".
           05 r-tot-finale       pic x(16).

       01  r-riga-marca.
           05 r-marca            pic x(35).
           05 r-valore           pic x(16).

       01  r-riga-dettaglio.
           05 r-anno             pic 9(4).
           05 r-numero           pic z(8).
           05 r-ragsoc           pic x(40).
           05 r-qta-d            pic zzz.zz9.
           05 r-promo            pic x(30).

      * FLAGS
       77  controlli             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".
       77  filler                pic 9.
           88 RecLocked          value 1 false 0.
       77  filler                pic 9.
           88 stampa-totale      value 1 false 0.
       77  filler                pic 9.
           88 trovato            value 1 false 0.
       77  filler                pic 9.
           88 trovato-scorta     value 1 false 0.
       77  filler                pic 9.
           88 record-ok          value 1 false 0.
       77  TipoScorta            pic 9.
           88 scorta-2-0         value 2.
           88 scorta-5-9         value 5.
           88 scorta-tutti       value 0.

       77  filler                pic 9.
           88 PrimaVolta         value 1 false 0.

      * VARIABILI
       77  totale                pic 9(12)v99.
       77  valore-z              pic zzz.zzz.zz9,99.
       77  valore-x              pic x(14).
       77  como-qta              pic 9(8).

       77  num-righe             pic 9(4).

       77  messaggio             pic x(150) value spaces.
       77  font-size-dply        pic z(5).      
       77  WFONT-STATUS          pic s9(5)  value zero.

       77  Verdana20BI           handle of font.
       77  Verdana16B            handle of font.
       77  Verdana10B            handle of font.
       77  Verdana8              handle of font.
       77  Verdana7I             handle of font.
       77  Verdana8BI            handle of font.
       77  passo                 pic 99v99.
       77  save-riga             pic 9(7)v99.
       77  save-altezza-pagina   pic 9(7)v99.
       77  como-data             pic 9(8).
       77  como-ora              pic 9(8).

       77  DestFile               pic x(256).
       77  NomeFile               pic x(256).
       01                    pic 9.
           88 TIME-OUT-EXIT  value 1 false zero.

       77  MINUTI-PARTENZA   pic 99.
       77  MINUTI-arrivo     pic 99.
       77  como-nome-file         pic x(256).
       01  FILE-INFO.
           02  FILE-SIZE    PIC X(8) COMP-X.
           02  FILE-DATE    PIC 9(8) COMP-X.
           02  FILE-TIME    PIC 9(8) COMP-X.

       77  old-SIZE         PIC X(8) COMP-X.

       77  status-code       pic 9.
       77  cont pic 9(4).


       LINKAGE SECTION.  
       77  RichiamoSchedulatoFlag  pic 9.
           88 RichiamoSchedulato   value 1 false 0.
           
           copy "link-lab-inevaso.def".
           copy "link-batch.def".

      ******************************************************************
       PROCEDURE DIVISION using RichiamoSchedulatoFlag,
                                lab-inevaso-linkage,
                                batch-linkage.

       DECLARATIVES. 
      ***---
       MTORDINI-ERR SECTION.
           use after error procedure on mtordini.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-mtordini
           when "39"
                set errori to true
                if RichiamoSchedulato
                   move "File [MTORDINI] mismatch size!" to como-riga
                   perform SETTA-RIGA-STAMPA
                   move -1 to batch-status
                else
                   display message "File [MTORDINI] mismatch size!"
                             title titolo
                              icon 3
                end-if
           when "98"
                set errori to true
                if RichiamoSchedulato
                   move "File [MTORDINI] Indexed file corrupt!"
                     to como-riga
                   perform SETTA-RIGA-STAMPA
                   move -1 to batch-status
                else
                   display message 
                   "File [MTORDINI] Indexed file corrupt!"
                             title titolo
                              icon 3
                end-if
           when "35"  
                set errori to true
                if RichiamoSchedulato
                   move "File [MTORDINI] inesistente!"to como-riga
                   perform SETTA-RIGA-STAMPA
                   move -1 to batch-status
                else
                   display message "File [MTORDINI] inesistente!"
                             title titolo
                              icon 3
                end-if
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       MRORDINI-ERR SECTION.
           use after error procedure on mrordini.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-mrordini
           when "39"
                set errori to true
                if RichiamoSchedulato
                   move "File [MRORDINI] mismatch size!" to como-riga
                   perform SETTA-RIGA-STAMPA
                   move -1 to batch-status
                else
                   display message "File [MRORDINI] mismatch size!"
                             title titolo
                              icon 3
                end-if
           when "98"
                set errori to true
                if RichiamoSchedulato
                   move "File [MRORDINI] Indexed file corrupt!"
                     to como-riga
                   perform SETTA-RIGA-STAMPA
                   move -1 to batch-status
                else
                   display message 
                   "File [MRORDINI] Indexed file corrupt!"
                             title titolo
                              icon 3
                end-if
           when "35"  
                set errori to true
                if RichiamoSchedulato
                   move "File [MRORDINI] inesistente!"to como-riga
                   perform SETTA-RIGA-STAMPA
                   move -1 to batch-status
                else
                   display message "File [MRORDINI] inesistente!"
                             title titolo
                              icon 3
                end-if
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       TAGLI-ERR SECTION.
           use after error procedure on tagli.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tagli
           when "39"
                set errori to true
                if RichiamoSchedulato
                   move "File [TAGLI] mismatch size!" to como-riga
                   perform SETTA-RIGA-STAMPA
                   move -1 to batch-status
                else
                   display message "File [TAGLI] mismatch size!"
                             title titolo
                              icon 3
                end-if
           when "98"
                set errori to true
                if RichiamoSchedulato
                   move "File [TAGLI] Indexed file corrupt!"to como-riga
                   perform SETTA-RIGA-STAMPA
                   move -1 to batch-status
                else
                   display message "File [TAGLI] Indexed file corrupt!"
                             title titolo
                              icon 3
                end-if
           when "35"  
                set errori to true
                if RichiamoSchedulato
                   move "File [TAGLI] inesistente!"to como-riga
                   perform SETTA-RIGA-STAMPA
                   move -1 to batch-status
                else
                   display message "File [TAGLI] inesistente!"
                             title titolo
                              icon 3
                end-if
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       TGRUPGDO-ERR SECTION.
           use after error procedure on tgrupgdo.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tgrupgdo
           when "39"
                set errori to true
                if RichiamoSchedulato
                   move "File [TGRUPGDO] mismatch size!" to como-riga
                   perform SETTA-RIGA-STAMPA
                   move -1 to batch-status
                else
                   display message "File [TGRUPGDO] mismatch size!"
                             title titolo
                              icon 3
                end-if
           when "98"
                set errori to true
                if RichiamoSchedulato
                   move "File [TGRUPGDO] file corrupt!" to como-riga
                   perform SETTA-RIGA-STAMPA
                   move -1 to batch-status
                else
                   display message "File [TGRUPGDO] file corrupt!"
                             title titolo
                              icon 3
                end-if
           when "35"  
                set errori to true
                if RichiamoSchedulato
                   move "File [TGRUPGDO] inesistente!"to como-riga
                   perform SETTA-RIGA-STAMPA
                   move -1 to batch-status
                else
                   display message "File [TGRUPGDO] inesistente!"
                             title titolo
                              icon 3
                end-if
           when "93"
           when "99" set RecLocked to true
           end-evaluate. 

      ***---
       ARTICOLI-ERR SECTION.
           use after error procedure on articoli.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-articoli
           when "39"
                set errori to true
                if RichiamoSchedulato
                   move "File [ARTICOLI] mismatch size!" to como-riga
                   perform SETTA-RIGA-STAMPA
                   move -1 to batch-status
                else
                   display message "File [ARTICOLI] mismatch size!"
                             title titolo
                              icon 3
                end-if
           when "98"
                set errori to true
                if RichiamoSchedulato
                   move "File [ARTICOLI] file corrupt!"to como-riga
                   perform SETTA-RIGA-STAMPA
                   move -1 to batch-status
                else
                   display message "File [ARTICOLI] file corrupt!"
                             title titolo
                              icon 3
                end-if
           when "35"  
                set errori to true
                if RichiamoSchedulato
                   move "File [ARTICOLI] inesistente!"to como-riga
                   perform SETTA-RIGA-STAMPA
                   move -1 to batch-status
                else
                   display message "File [ARTICOLI] inesistente!"
                             title titolo
                              icon 3
                end-if
           when "93"
           when "99" set RecLocked to true
           end-evaluate. 

      ***---
       TMARCHE-ERR SECTION.
           use after error procedure on tmarche.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tmarche
           when "39"
                set errori to true
                if RichiamoSchedulato
                   move "File [TMARCHE] mismatch size!" to como-riga
                   perform SETTA-RIGA-STAMPA
                   move -1 to batch-status
                else
                   display message "File [TMARCHE] mismatch size!"
                             title titolo
                              icon 3
                end-if
           when "98"
                set errori to true
                if RichiamoSchedulato
                   move "File [TMARCHE] file corrupt!"to como-riga
                   perform SETTA-RIGA-STAMPA
                   move -1 to batch-status
                else
                   display message "File [TMARCHE] file corrupt!"
                             title titolo
                              icon 3
                end-if
           when "35"  
                set errori to true
                if RichiamoSchedulato
                   move "File [TMARCHE] inesistente!"to como-riga
                   perform SETTA-RIGA-STAMPA
                   move -1 to batch-status
                else
                   display message "File [TMARCHE] inesistente!"
                             title titolo
                              icon 3
                end-if
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       TMP-INEVASO-ERR SECTION.
           use after error procedure on tmp-inevaso.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tmp-inevaso
           when "39"
                set errori to true
                if RichiamoSchedulato
                   move "File [TMP-INEVASO] mismatch size!" to como-riga
                   perform SETTA-RIGA-STAMPA
                   move -1 to batch-status
                else
                   display message "File [TMP-INEVASO] mismatch size!"
                             title titolo
                              icon 3
                end-if
           when "98"
                set errori to true
                if RichiamoSchedulato
                   move "File [TMP-INEVASO] file corrupt!"to como-riga
                   perform SETTA-RIGA-STAMPA
                   move -1 to batch-status
                else
                   display message "File [TMP-INEVASO] file corrupt!"
                             title titolo
                              icon 3
                end-if
           when "35"  
                set errori to true
                if RichiamoSchedulato
                   move "File [TMP-INEVASO] inesistente!" to como-riga
                   perform SETTA-RIGA-STAMPA
                   move -1 to batch-status
                else
                   display message "File [TMP-INEVASO] inesistente!"
                             title titolo
                              icon 3
                end-if
           when "93"
                if RichiamoSchedulato
                   move "File [TMP-INEVASO] già in uso!" to como-riga
                   perform SETTA-RIGA-STAMPA
                   move -1 to batch-status
                   set errori to true
                else
                   initialize geslock-messaggio
                   string   "File già in uso!"
                     x"0d0a""Impossibile procedere!" delimited size
                           into geslock-messaggio
                   end-string
                   move 1 to geslock-v-riprova    
                   move 0 to geslock-v-ignora
                   move 1 to geslock-v-termina
                   move   "File TMP INEVASO"  to geslock-nome-file
                   call   "geslock" using geslock-linkage
                   cancel "geslock"
                   evaluate true
                   when riprova
                        open output tmp-inevaso
                   when termina
                        set errori to true
                        display message "Operazione interrotta!"
                                  title titolo
                                   icon 2
                   end-evaluate
                end-if
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       TMP-MARCA-ERR SECTION.
           use after error procedure on tmp-marca.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tmp-marca
           when "39"
                set errori to true
                if RichiamoSchedulato
                   move "File [TMP-MARCA] mismatch size!" to como-riga
                   perform SETTA-RIGA-STAMPA
                   move -1 to batch-status
                else
                   display message "File [TMP-MARCA] mismatch size!"
                             title titolo
                              icon 3
                end-if
           when "98"
                set errori to true
                if RichiamoSchedulato
                   move "File [TMP-MARCA] file corrupt!"to como-riga
                   perform SETTA-RIGA-STAMPA
                   move -1 to batch-status
                else
                   display message "File [TMP-MARCA] file corrupt!"
                             title titolo
                              icon 3
                end-if
           when "35"  
                set errori to true
                if RichiamoSchedulato
                   move "File [TMP-MARCA] inesistente!" to como-riga
                   perform SETTA-RIGA-STAMPA
                   move -1 to batch-status
                else
                   display message "File [TMP-MARCA] inesistente!"
                             title titolo
                              icon 3
                end-if
           when "93"
                if RichiamoSchedulato
                   move "File [TMP-MARCA] già in uso!" to como-riga
                   perform SETTA-RIGA-STAMPA
                   move -1 to batch-status
                   set errori to true
                else
                   initialize geslock-messaggio
                   string   "File già in uso!"
                     x"0d0a""Impossibile procedere!" delimited size
                           into geslock-messaggio
                   end-string
                   move 1 to geslock-v-riprova    
                   move 0 to geslock-v-ignora
                   move 1 to geslock-v-termina
                   move   "File TMP-MARCA"  to geslock-nome-file
                   call   "geslock" using geslock-linkage
                   cancel "geslock"
                   evaluate true
                   when riprova
                        open output tmp-inevaso
                   when termina
                        set errori to true
                        display message "Operazione interrotta!"
                                  title titolo
                                   icon 2
                   end-evaluate
                end-if
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       TMP-TOT-MARCA-ERR SECTION.
           use after error procedure on tmp-tot-marca.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tmp-tot-marca
           when "39"
                set errori to true
                if RichiamoSchedulato
                   move "File [TMP-TOT-MARCA] mismatch size!" 
                     to como-riga
                   perform SETTA-RIGA-STAMPA
                   move -1 to batch-status
                else
                   display message "File [TMP-TOT-MARCA] mismatch size!"
                             title titolo
                              icon 3
                end-if
           when "98"
                set errori to true
                if RichiamoSchedulato
                   move "File [TMP-TOT-MARCA] file corrupt!"to como-riga
                   perform SETTA-RIGA-STAMPA
                   move -1 to batch-status
                else
                   display message "File [TMP-TOT-MARCA] file corrupt!"
                             title titolo
                              icon 3
                end-if
           when "35"  
                set errori to true
                if RichiamoSchedulato
                   move "File [TMP-TOT-MARCA] inesistente!" to como-riga
                   perform SETTA-RIGA-STAMPA
                   move -1 to batch-status
                else
                   display message "File [TMP-TOT-MARCA] inesistente!"
                             title titolo
                              icon 3
                end-if
           when "93"
                if RichiamoSchedulato
                   move "File [TMP-TOT-MARCA] già in uso!" to como-riga
                   perform SETTA-RIGA-STAMPA
                   move -1 to batch-status
                   set errori to true
                else
                   initialize geslock-messaggio
                   string   "File già in uso!"
                     x"0d0a""Impossibile procedere!" delimited size
                           into geslock-messaggio
                   end-string
                   move 1 to geslock-v-riprova    
                   move 0 to geslock-v-ignora
                   move 1 to geslock-v-termina
                   move   "File TMP-TOT-MARCA"  to geslock-nome-file
                   call   "geslock" using geslock-linkage
                   cancel "geslock"
                   evaluate true
                   when riprova
                        open output tmp-inevaso
                   when termina
                        set errori to true
                        display message "Operazione interrotta!"
                                  title titolo
                                   icon 2
                   end-evaluate
                end-if
           when "99" set RecLocked to true
           end-evaluate.

       END DECLARATIVES.

      ***---
       MAIN-PRG.             
           if RichiamoSchedulato
              move batch-log to wstampa
              open extend lineseq
           end-if.
           accept  como-data from century-date.
           accept  como-ora  from time.
           set PrimaVolta to true.
           set tutto-ok   to true.
           initialize path-tmp-tot-marca.
           accept  path-tmp-tot-marca from environment "PATH_ST".
           inspect path-tmp-tot-marca replacing trailing 
                                      spaces by low-value.
           string  path-tmp-tot-marca delimited low-value
                   "TMP-TOT-MARCA"    delimited size
                   "_"                delimited size
                   como-data          delimited size
                   "_"                delimited size
                   como-ora           delimited size
                   ".tmp"             delimited size
                   into path-tmp-tot-marca
           end-string.
           open output tmp-tot-marca.
           if tutto-ok
              close       tmp-tot-marca
              open i-o    tmp-tot-marca| allowing readers.
           end-if.

           if tutto-ok
              perform INIT
              perform OPEN-FILES
           end-if.
           if tutto-ok
              set stampa-totale to false

              set scorta-2-0 to true

              if RichiamoSchedulato
                 move "ELABORAZIONE SCORTA 0-2" to como-riga
                 perform SETTA-RIGA-STAMPA
              end-if
              
              perform ELABORAZIONE
              perform CLOSE-FILES

              perform INIT
              perform OPEN-FILES
              set scorta-5-9 to true
      *****        set PrimaVolta to false
      
              if RichiamoSchedulato
                 move "ELABORAZIONE SCORTA 5-9" to como-riga
                 perform SETTA-RIGA-STAMPA
              end-if

              perform ELABORAZIONE
              perform CLOSE-FILES

              if trovato-scorta
                 set stampa-totale to true
              end-if

              perform INIT
              perform OPEN-FILES
              set scorta-tutti to true
      *****        set PrimaVolta   to false
      
              if RichiamoSchedulato
                 move "ELABORAZIONE SCORTA TUTTI" to como-riga
                 perform SETTA-RIGA-STAMPA
              end-if

              perform ELABORAZIONE

              if trovato-scorta
                 set stampa-totale to true
              end-if

              if stampa-totale          

                 if RichiamoSchedulato
                    move "STAMPA TOTALE" to como-riga
                    perform SETTA-RIGA-STAMPA
                 end-if

                 perform STAMPA-TOTALE-NOT2
              end-if

              perform CLOSE-FILES

              close       tmp-tot-marca
              delete file tmp-tot-marca

              if lin-pdf and settaPDF-OK
                                         
                 if RichiamoSchedulato
                    move "CHIUSURA PDF" to como-riga
                    perform SETTA-RIGA-STAMPA
                 end-if

                 set spl-chiusura to true
                 call   "spooler" using spooler-link

                 if RichiamoSchedulato
                    move "CHIUSURA PDF OK" to como-riga
                    perform SETTA-RIGA-STAMPA
                 end-if

              end-if

           end-if.
           perform EXIT-PGM.

      ***---
       INIT.
           accept ru-user from environment "USER_CODI".
           call   "readutente" using ru-linkage.
           cancel "readutente".

           move 0   to num-righe.
           move 0,5 to passo.
           set environment "PRINTER" to "-P SPOOLER".
           set tutto-ok   to true.
           set RecLocked  to false.
           set trovato    to false.
      *****     set PrimaVolta to true.
           initialize path-tmp-inevaso.
           accept  path-tmp-inevaso   from environment "PATH_ST".
           inspect path-tmp-inevaso   replacing trailing 
                                      spaces by low-value.
           string  path-tmp-inevaso   delimited low-value
                   "TMP-INEVASO"      delimited size
                   "_"                delimited size
                   como-data          delimited size
                   "_"                delimited size
                   como-ora           delimited size
                   ".tmp"             delimited size
                   into path-tmp-inevaso
           end-string.

           accept  path-tmp from environment "PATH_ST".
           inspect path-tmp replacing trailing spaces by low-value.
           string  path-tmp    delimited low-value
                   "TMP-MARCA" delimited size
                   "_"         delimited size
                   como-data   delimited size
                   "_"         delimited size
                   como-ora    delimited size
                   ".tmp"      delimited size
                   into path-tmp
           end-string.   

      ***---
       SETTA-RIGA-STAMPA.
           initialize riga-stampa.
           perform SETTA-INIZIO-RIGA.
           string r-inizio  delimited size
                  como-riga delimited size
                  into riga-stampa
           end-string.
           if RichiamoSchedulato
              write line-riga of lineseq from riga-stampa
           end-if.   

      ***---
       SETTA-INIZIO-RIGA.
           accept como-ora  from time.
           accept como-data from century-date.
                                        
           move como-data(3:2) to r-aa.
           move como-data(5:2) to r-mm.
           move como-data(7:2) to r-gg.

           move como-ora(1:2)  to r-hh.
           move como-ora(3:2)  to r-min.
           move como-ora(5:2)  to r-sec.

      ***---
       OPEN-FILES.    
           if RichiamoSchedulato
              move "APERTURA FILES" to como-riga
              perform SETTA-RIGA-STAMPA
           end-if.

           open output tmp-inevaso.
           if tutto-ok
              close    tmp-inevaso
              open i-o tmp-inevaso |allowing readers
              open output tmp-marca
              if tutto-ok
                 close    tmp-marca
                 open i-o tmp-marca |allowing readers
                 open input tagli tgrupgdo tmarche articoli mtordini
                            mrordini tpromo clienti
              else
                 unlock      tmp-inevaso all records
                 close       tmp-inevaso
                 delete file tmp-inevaso
              end-if
           end-if.  

      ***---
       ELABORAZIONE.
           set trovato-scorta to false.
           move low-value     to tag-rec.
           move lin-data     to tag-data.

           start tagli key >= tag-chiave
                 invalid set errori to true
           end-start.

           if tutto-ok
              perform until 1 = 2
                 read tagli next at end exit perform end-read

                 if tag-data not = lin-data
                    exit perform
                 end-if

                 perform CONTATORE-VIDEO

                 set trovato to true
                 initialize tin-rec 
                            gdo-rec
                            art-rec 
                            tma-rec 
                            tmpm-rec replacing numeric data by zeroes
                                          alphanumeric data by spaces
                 move tag-articolo to art-codice tin-articolo 
                 read articoli no lock invalid continue end-read

                 set record-ok to false

                 if scorta-2-0
                    if art-scorta = 2 or art-scorta = 0
                       set record-ok to true
                    end-if
                 end-if

                 if scorta-5-9 
                    if art-scorta = 5 or art-scorta = 9
                       set record-ok to true
                    end-if
                 end-if

                 if scorta-tutti 
                    if art-scorta not = 2 and
                       art-scorta not = 5 and
                       art-scorta not = 0 and
                       art-scorta not = 9
                       set record-ok to true
                    end-if
                 end-if

                 if record-ok
                    set trovato-scorta   to true
                    move art-descrizione to tin-art-descrizione

                    if scorta-2-0
                       move tag-gdo to gdo-codice
                       read tgrupgdo no lock invalid continue end-read
                       move gdo-intestazione to tin-gdo-intestazione
                    else
                       move spaces to tin-gdo-intestazione
                    end-if

                    read tmp-inevaso invalid continue end-read

                    if tin-prz < tag-prz
                       move tag-prz  to tin-prz
                    end-if

                    add  tag-qta  to tin-qta
                    compute totale = tag-prz * tag-qta
                    add totale to tin-tot

                    write tin-rec invalid rewrite tin-rec end-write

                    move art-marca-prodotto to tma-marca mar-codice
                    read tmarche   no lock invalid continue end-read
                    read tmp-marca no lock invalid continue end-read
                    add totale to tma-vag
                    write tma-rec invalid rewrite tma-rec end-write

                    if scorta-5-9 or scorta-tutti
                       move art-marca-prodotto to mar-codice
                       move mar-descrizione    to tmpm-marca
                       read tmp-tot-marca no lock
                            invalid continue
                       end-read
                       add totale to tmpm-totale
                       write tmpm-rec invalid rewrite tmpm-rec end-write
                    end-if

                 end-if

             end-perform

           end-if.  

           if not trovato
              if not lin-pdf
                 display message "Nessun taglio trovato"
                         title titolo
                         icon 2
              end-if
           else
              if trovato-scorta
                 perform STAMPA
              end-if
           end-if.    

      ***---
       STAMPA.
           initialize spooler-link.
           evaluate true
           when lin-stampa
                if PrimaVolta
                   call   "selprint" using selprint-linkage
                   cancel "selprint"
                end-if
           when lin-anteprima
                accept selprint-stampante 
                      from environment "STAMPANTE_ANTEPRIMA"
                move 1 to selprint-num-copie
           when lin-pdf
                if primavolta
                   perform CREA-PDF
                   if settaPDF-OK
                      accept selprint-stampante 
                              from environment "STAMPANTE_TAGLI_PDF"
                   else
                      move spaces to selprint-stampante
                   end-if
                end-if
           end-evaluate.

           if selprint-stampante not = space
              move 1 to selprint-num-copie
              if lin-pdf
                 if primavolta
                    perform APERTURA-STAMPA
                 else
                   set spl-salto-pagina to true
                   call "spooler" using spooler-link
                 end-if                 
              else
                 perform APERTURA-STAMPA
              end-if
           else
              set spl-sta-annu to true
              set errori to true
           end-if.

           if tutto-ok
              perform CONTA-RIGHE
              move 1 to pagina
              evaluate true
              when num-righe < 78-MaxRows
                   move 1 to tot-pag
              when num-righe = 78-MaxRows
                   move 2 to tot-pag
              when other
                   divide num-righe by 78-MaxRows 
                   giving tot-pag   remainder resto
                   if resto > 0 add 1 to tot-pag end-if
              end-evaluate
              move low-value to tin-rec
              start tmp-inevaso key >= tin-chiave
                    invalid continue
                not invalid
                    move 0 to totale
                    string lin-data(7:2) delimited size
                           "/"           delimited size
                           lin-data(5:2) delimited size
                           "/"           delimited size
                           lin-data(1:4) delimited size
                           into r-data-sta
                    end-string
                    perform SCRIVI-INTESTAZIONE
                    perform until 1 = 2
                       read tmp-inevaso next 
                            at end exit perform 
                       end-read   

                       perform CONTATORE-VIDEO

                       if num-righe >= 78-MaxRows            
                          perform SALTO-PAGINA               
                       end-if

                       move Verdana8             to spl-hfont

                       move tin-gdo-intestazione to r-gdo-intestazione
                       move tin-articolo         to r-art
                       move tin-art-descrizione  to r-art-descrizione
                       move tin-qta              to r-qta

                       if scorta-2-0
                          perform FORMAT-PREZZO
                       else
                          move spaces to r-prz
                       end-if

                       perform FORMAT-TOTALE

                       add tin-tot to totale

                       move r-riga  to spl-riga-stampa
                       if scorta-2-0
                          move 70      to spl-tipo-colonna
                       else
                          move 70,1    to spl-tipo-colonna
                       end-if
                       set spl-nero to true
                       perform SCRIVI

                       |le righe di dettaglio
                       move lin-data  to tag-data
                       start tagli key = tag-data
                             invalid continue
                         not invalid
                             perform until 1 = 2
                                read tagli next
                                     at end exit perform
                                end-read
                                if tag-data not = lin-data
                                   exit perform
                                end-if

                                perform CONTATORE-VIDEO

                                if tag-articolo = tin-articolo
                                   perform SCRIVI-DETTAGLIO
                                end-if
                             end-perform
                       end-start
                       
                       add 1 to num-righe

                 end-perform

                 if num-righe > 78-MaxRows - 2
                    perform SALTO-PAGINA
                 end-if

                 perform SCRIVI-TOTALE
                 add 2 to num-righe

                 move low-value  to tma-rec
                 start tmp-marca key >= tma-chiave
                       invalid   continue
                   not invalid
                       move 0 to totale

                       if num-righe >= 78-MaxRows - 6
                          perform SALTO-PAGINA-MARCA
                       else
                          move "** INEVASO MARCA **" to spl-riga-stampa
                          perform SCRIVI-INTESTAZIONE-MARCA
                       end-if

                       perform until 1 = 2
                          read tmp-marca next
                               at end exit perform
                          end-read      

                          perform CONTATORE-VIDEO

                          if num-righe >= 78-MaxRows
                             perform SALTO-PAGINA-MARCA
                          end-if

                          perform STAMPA-FRAME-MARCA

                          initialize mar-rec
                          move Verdana8   to spl-hfont
                          move tma-marca  to mar-codice
                          read tmarche no lock invalid continue end-read
                          move mar-descrizione to r-marca
                          perform FORMAT-VALORE-MARCA

                          add tma-vag to totale

                          move r-riga-marca to spl-riga-stampa
                          move 71           to spl-tipo-colonna
                          set spl-nero      to true
                          perform SCRIVI

                          perform STAMPA-LINEA-MARCA

                          add 1 to num-righe

                       end-perform
                 end-start

                 if num-righe > 78-MaxRows - 2
                    perform SALTO-PAGINA-MARCA
                 end-if

                 perform SCRIVI-TOTALE-MARCA

                 perform PIE-DI-PAGINA

                 if not lin-pdf
                    set spl-chiusura to true
                    call   "spooler" using spooler-link
                 end-if
           end-start.

      ***---
       SCRIVI-DETTAGLIO.                        
           move tag-mro-chiave-testa to mto-chiave.
           read mtordini no lock
                invalid continue
            not invalid                         
                       
                if num-righe >= 78-MaxRows
                   perform SALTO-PAGINA
                end-if

                move tag-mro-chiave to mro-chiave
                read mrordini no lock 
                     invalid move 0 to mro-qta mro-qta-e
                end-read
                compute como-qta = mro-qta - mro-qta-e
                |05/06/2015 usare direttamente la quantità dei tagli
                move tag-qta to como-qta
                move como-qta to r-qta-d
                if mto-gdo not = spaces
                   move mto-gdo to gdo-codice
                   read tgrupgdo no lock
                        invalid move spaces to gdo-intestazione
                   end-read
                   move gdo-intestazione to r-ragsoc
                else
                   set cli-tipo-C   to true
                   move mto-cod-cli to cli-codice
                   read clienti no lock
                        invalid move spaces to cli-ragsoc-1
                   end-read
                   move cli-ragsoc-1 to r-ragsoc
                end-if                          

                move mto-anno   to r-anno
                move mto-numero to r-numero
                move mro-promo  to tpr-codice
                read tpromo no lock
                     invalid move spaces to tpr-descrizione
                end-read
                move tpr-descrizione to r-promo
                                      
                set spl-blu    to true
                move Verdana7I to spl-hfont
                move r-riga-dettaglio  to spl-riga-stampa
                move 70,2              to spl-tipo-colonna
                perform SCRIVI         
                add 1 to num-righe

           end-read.       

      ***---
       CONTATORE-VIDEO.
           if RichiamoSchedulato
              add 1 to counter counter2
      
              if counter2 = 100
                 move counter to counter-edit
                 display counter-edit 
                         upon batch-win-handle
                         line 25,00
                       column 38,00
                 move 0 to counter2
              end-if
           end-if.                       

      ***---
       APERTURA-STAMPA.
           if RichiamoSchedulato
              move "APERTURA STAMPA" to como-riga
              perform SETTA-RIGA-STAMPA
           end-if.
           set PrimaVolta to false
           move selprint-num-copie to SPL-NUM-COPIE
           move selprint-stampante to SPL-NOME-STAMPANTE

           move titolo to spl-nome-job
           set spl-apertura to true
           set spl-vertical to true
           set WFDEVICE-WIN-PRINTER    to true
           call "spooler" using spooler-link
           if spl-sta-annu
              set errori to true
              if RichiamoSchedulato
                 move "APERTURA STAMPA ERR" to como-riga
                 move -1 to batch-status
              end-if
           else                       
              if RichiamoSchedulato    
                 move "APERTURA STAMPA OK" to como-riga                 
              end-if
              move spl-altezza to save-altezza-pagina
              perform CARICA-FONT
           end-if.              
           if RichiamoSchedulato
              perform SETTA-RIGA-STAMPA
           end-if.              

      ***---
       STAMPA-TOTALE-NOT2.             
           move 1 to pagina.
           move selprint-num-copie to SPL-NUM-COPIE.
           move selprint-stampante to SPL-NOME-STAMPANTE.
                                         
           move titolo to spl-nome-job.
           set spl-apertura to true.
           set spl-vertical to true.
           set WFDEVICE-WIN-PRINTER    to true.
           call "spooler" using spooler-link.

           move low-value  to tmpm-rec.
           start tmp-tot-marca key >= tmpm-chiave
                 invalid   continue
             not invalid
                 move 0 to totale
                        
                 move 58                      to spl-tipo-colonna
                 set spl-rosso                to true
                 move Verdana16B              to spl-hfont
                 move 0,0                     to save-riga
                 move "TUTTO TRANNE ETO"      to spl-riga-stampa
                 perform SCRIVI

                 move 0,0                          to save-riga
                 move "** TOTALE INEVASO MARCA **" to spl-riga-stampa
                 perform SCRIVI-INTESTAZIONE-MARCA

                 perform until 1 = 2
                    read tmp-tot-marca next
                         at end exit perform
                    end-read

                    perform STAMPA-FRAME-MARCA

                    initialize mar-rec
                    move Verdana8    to spl-hfont
                    move tmpm-marca  to r-marca
                    move tmpm-totale to tma-vag
                    perform FORMAT-VALORE-MARCA

                    add tmpm-totale   to totale

                    move r-riga-marca to spl-riga-stampa
                    move 71           to spl-tipo-colonna
                    set spl-nero      to true
                    perform SCRIVI

                    perform STAMPA-LINEA-MARCA

                    add 1 to num-righe

                 end-perform
           end-start.

           perform SCRIVI-TOTALE-MARCA.

           move 1 to tot-pag.
           perform PIE-DI-PAGINA.

           if not lin-pdf
              set spl-chiusura to true
              call   "spooler" using spooler-link
           end-if.

      ***---
       FORMAT-PREZZO.
           move tin-prz        to valore-z.
           move valore-z       to valore-x.
           call "C$JUSTIFY" using valore-x, "R"
           inspect valore-x replacing leading x"30" by x"20".
           call "C$JUSTIFY" using valore-x, "L".
           inspect valore-x replacing trailing spaces by low-value.
           initialize r-prz.
           string "€ "      delimited size
                  valore-x  delimited low-value
                  into r-prz
           end-string.

      ***---
       FORMAT-TOTALE.
           move tin-tot        to valore-z.
           move valore-z       to valore-x.
           call "C$JUSTIFY" using valore-x, "R"
           inspect valore-x replacing leading x"30" by x"20".
           call "C$JUSTIFY" using valore-x, "L".
           inspect valore-x replacing trailing spaces by low-value.
           initialize r-tot.
           string "€ "      delimited size
                  valore-x  delimited low-value
                  into r-tot
           end-string.

      ***---
       FORMAT-TOTALE-FINALE.
           move totale         to valore-z.
           move valore-z       to valore-x.
           call "C$JUSTIFY" using valore-x, "R"
           inspect valore-x replacing leading x"30" by x"20".
           call "C$JUSTIFY" using valore-x, "L".
           inspect valore-x replacing trailing spaces by low-value.
           initialize r-tot-finale.
           string "€ "      delimited size
                  valore-x  delimited low-value
                  into r-tot-finale
           end-string.

      ***---
       FORMAT-VALORE-MARCA.
           move tma-vag        to valore-z.
           move valore-z       to valore-x.
           call "C$JUSTIFY" using valore-x, "R"
           inspect valore-x replacing leading x"30" by x"20".
           call "C$JUSTIFY" using valore-x, "L".
           inspect valore-x replacing trailing spaces by low-value.
           initialize r-valore.
           string "€ "      delimited size
                  valore-x  delimited low-value
                  into r-valore
           end-string.

      ***---
       FORMAT-TOTALE-FINALE-MARCA.
           move totale         to valore-z.
           move valore-z       to valore-x.
           call "C$JUSTIFY" using valore-x, "R"
           inspect valore-x replacing leading x"30" by x"20".
           call "C$JUSTIFY" using valore-x, "L".
           inspect valore-x replacing trailing spaces by low-value.
           initialize r-valore
           string "€ "      delimited size
                  valore-x  delimited low-value
                  into r-valore
           end-string.

      ***---
       SCRIVI-TOTALE.
           add 0,7 to save-riga.
           perform STAMPA-LINEA.
           perform STAMPA-FRAME-TOTALE.
           subtract 0,2  from save-riga.
           set spl-rosso   to true.
           move Verdana10B to spl-hfont.
           perform FORMAT-TOTALE-FINALE.
           move 70,5       to spl-tipo-colonna.
           move r-totale   to spl-riga-stampa.
           perform SCRIVI.

      ***---
       STAMPA-FRAME-TOTALE.
           move 2     to spl-pen-with.
           move 13,98 to spl-colonna.
           move 19,32 to spl-colonna-fine.

           add  0,27  to save-riga giving spl-riga.
           add  0,82  to save-riga giving spl-riga-fine.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-rosso         to true.
           call "spooler"         using spooler-link.

           move 2     to spl-pen-with.
           move 14,02 to spl-colonna.
           move 19,30 to spl-colonna-fine.

           add  0,3   to save-riga giving spl-riga.
           add  0,79  to save-riga giving spl-riga-fine.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-giallo        to true.
           set  spl-brush-solid   to true.
           call "spooler"         using spooler-link.

      ***---
       SCRIVI-TOTALE-MARCA.
           add 0,4 to save-riga.
           perform STAMPA-FRAME-TOTALE-MARCA.
           subtract 0,2 from save-riga.
           set spl-rosso          to true.
           move Verdana10B        to spl-hfont.
           perform FORMAT-TOTALE-FINALE-MARCA.
           move "TOTALE:"         to r-marca.
           move 71                to spl-tipo-colonna.
           move r-riga-marca      to spl-riga-stampa.
           perform SCRIVI.

      ***---
       SCRIVI-INTESTAZIONE.
           move 0        to num-righe.
                        
           move 58                      to spl-tipo-colonna.
           set spl-rosso                to true.
           move Verdana16B              to spl-hfont.
           move 0,0                     to save-riga.
           evaluate true 
           when scorta-2-0
                move "LISTA PRODOTTI NON GESTITI" to spl-riga-stampa
           when scorta-5-9
                move "LISTA PRODOTTI SCORTA 5/9"  to spl-riga-stampa
           when scorta-tutti
                move "LISTA PRODOTTI A PICKING"   to spl-riga-stampa
           end-evaluate.
           perform SCRIVI.

           move 0,6              to save-riga.
           move "** INEVASO GIORNALIERO **" to spl-riga-stampa.
           move Verdana16B       to spl-hfont.
           move 58               to spl-tipo-colonna.
           set spl-blu           to true.
           perform SCRIVI.

           move 1,85  to spl-riga.
           move 2,6   to spl-riga-fine.
           perform STAMPA-FRAME-INIZIALE.

           move 1,35               to save-riga.
           move r-data-sta         to spl-riga-stampa.
           move Verdana16B         to spl-hfont.
           move 58                 to spl-tipo-colonna.
           set  spl-rosso          to true.
           perform SCRIVI.
                      
           move 2,5                to save-riga.
           set spl-blu to true.
           move Verdana8BI         to spl-hfont.

           if scorta-2-0
              move r-intesta-s2       to spl-riga-stampa
              move 69                 to spl-tipo-colonna
           else
              move r-intesta          to spl-riga-stampa
              move 69,5               to spl-tipo-colonna
           end-if.
           perform SCRIVI.

           move 3,5 to save-riga.
           perform STAMPA-LINEA.
           move 3,3                to save-riga.

      ***---
       SCRIVI-INTESTAZIONE-MARCA.
           add 0,6                    to save-riga.
           move Verdana20BI           to spl-hfont.
           move 58                    to spl-tipo-colonna.
           set spl-blu                to true.
           perform SCRIVI.

           add 1,55  to save-riga giving spl-riga.
           add 0,75  to spl-riga  giving spl-riga-fine.
           perform STAMPA-FRAME-INIZIALE.

           move "DATA DI RIFERIMENTO" to r-titolo.
           set spl-rosso              to true.
           add 0,45                   to save-riga.
           move r-titolo              to spl-riga-stampa.
           move Verdana10B            to spl-hfont.
           move 58                    to spl-tipo-colonna.
           perform SCRIVI.

           add 0,1                 to save-riga.
           set spl-rosso           to true.
           move r-data-sta         to spl-riga-stampa.
           move Verdana16B         to spl-hfont.
           move 58                 to spl-tipo-colonna.
           perform SCRIVI.

           add 0,7 to save-riga.
           add   6 to num-righe.

      ***---
       STAMPA-LINEA-MARCA.
           add  0,4                to save-riga.
           move 2                  to spl-pen-with.
           move 7,5                to spl-colonna.
           move 14,2               to spl-colonna-fine.
           move save-riga          to spl-riga spl-riga-fine.
           set  spl-oggetto        to true.
           set  spl-linea          to true.
           set  spl-pen-solid      to true.
           call "spooler"       using spooler-link.
           subtract 0,4 from save-riga.

      ***---
       STAMPA-FRAME-MARCA.
           move 2     to spl-pen-with.
           move 11    to spl-colonna.
           move 14,20 to spl-colonna-fine.

           add passo  to save-riga giving spl-riga.
           add  0,37  to  spl-riga giving spl-riga-fine.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-giallo        to true.
           set  spl-brush-solid   to true.
           call "spooler"         using spooler-link.

      ***---
       STAMPA-FRAME-TOTALE-MARCA.
           move 2     to spl-pen-with.
           move 10,0  to spl-colonna.
           move 14,22 to spl-colonna-fine.

           add 0,28 to save-riga  giving spl-riga.
           add 0,50 to  spl-riga  giving spl-riga-fine.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-blu           to true.
           set  spl-brush-solid   to true.
           call "spooler"         using spooler-link.

           move 2     to spl-pen-with.
           move 10,02 to spl-colonna.
           move 14,20 to spl-colonna-fine.

           add 0,29 to save-riga giving spl-riga.
           add 0,46 to  spl-riga giving spl-riga-fine.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-giallo        to true.
           set  spl-brush-solid   to true.
           call "spooler"         using spooler-link.
            
      ***---
       STAMPA-LINEA.
           move 10                 to spl-pen-with.
           move 1,1                to spl-colonna.
           move 19,3               to spl-colonna-fine.
           move save-riga          to spl-riga spl-riga-fine.
           set  spl-oggetto        to true.
           set  spl-linea          to true.
           set  spl-pen-solid      to true.
           set  spl-rosso          to true.
           call "spooler"       using spooler-link.

      ***---
       SCRIVI.
           add  passo         to save-riga.
           move save-riga     to spl-riga.
           set  spl-stringa   to true.
           call "spooler"  using spooler-link.

      ***---
       SALTO-PAGINA-MARCA.
           perform PIE-DI-PAGINA.
           add 1 to pagina.
           set spl-salto-pagina to true.
           call "spooler" using spooler-link.
           move 0 to num-righe.
           move "** INEVASO MARCA **" to spl-riga-stampa.
           perform SCRIVI-INTESTAZIONE-MARCA.

      ***---
       SALTO-PAGINA.           
           perform PIE-DI-PAGINA.
           add 1 to pagina.
           set spl-salto-pagina to true.
           call "spooler" using spooler-link.
           perform SCRIVI-INTESTAZIONE.
       
      ***---
       STAMPA-FRAME-INIZIALE.
           |DATA
           move 4     to spl-pen-with.
           move 7,7   to spl-colonna.
           move 12,35 to spl-colonna-fine.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-null    to true.
           call "spooler"         using spooler-link.

      ***---
       CONTA-RIGHE.
           |Il totale finale
           move 2 to num-righe.
           move low-value to tin-chiave.
           start tmp-inevaso key >= tin-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tmp-inevaso next at end exit perform end-read
                    add 1 to num-righe                                                       

                    |Le righe di dettaglio
                    move lin-data to tag-data
                    start tagli key = tag-data
                          invalid continue
                      not invalid
                          perform until 1 = 2
                             read tagli next 
                                  at end exit perform 
                             end-read
                             if tag-data not = lin-data
                                exit perform 
                             end-if
                             if tag-articolo = tin-articolo
                                add 1 to num-righe
                             end-if
                          end-perform
                    end-start
                   
                 end-perform
           end-start.

           |Il totale finale, l'intestazione e lo spazio prima
           add 7 to num-righe.
           move low-value to tma-chiave.
           start tmp-marca key >= tma-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tmp-marca next at end exit perform end-read
                    add 1 to num-righe
                 end-perform
           end-start.  

      ***---
       CARICA-FONT.
      * Verdana 20BI
           initialize wfont-data Verdana20BI.
           move 20 to wfont-size.
           move "Verdana"            to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to true.
           set  wfont-italic         to true.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, Verdana20BI, wfont-data
                        giving WFONT-STATUS.
      
      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if WFONT-STATUS not = 1
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if. 

      * Verdana 16B
           initialize wfont-data Verdana16B.
           move 16 to wfont-size.
           move "Verdana"            to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to true.
           set  wfont-italic         to false.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, Verdana16B, wfont-data
                        giving WFONT-STATUS.
      
      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if WFONT-STATUS not = 1
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if. 

      * Verdana 10B
           initialize wfont-data Verdana10B.
           move 10 to wfont-size.
           move "Verdana"            to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to true.
           set  wfont-italic         to false.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, Verdana10B, wfont-data
                        giving WFONT-STATUS.
      
      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if WFONT-STATUS not = 1
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if. 

      * Verdana 8B
           initialize wfont-data Verdana8.            
           move 8 to wfont-size.
           move "Verdana"            to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to false.
           set  wfont-italic         to false.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, Verdana8, wfont-data
                        giving WFONT-STATUS.
      
      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if WFONT-STATUS not = 1
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if.                        

      * Verdana 7I
           initialize wfont-data Verdana7I.            
           move 7 to wfont-size.
           move "Verdana"            to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to false.
           set  wfont-italic         to true.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, Verdana7I, wfont-data
                        giving WFONT-STATUS.
      
      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if WFONT-STATUS not = 1
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if. 

      * Verdana 8BI
           initialize wfont-data Verdana8BI.
           move 8 to wfont-size.
           move "Verdana"            to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to true.
           set  wfont-italic         to true.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, Verdana8BI, wfont-data
                        giving WFONT-STATUS.
      
      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if WFONT-STATUS not = 1
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if. 

      ***---
       MESSAGGIO-ERR-FONT.
      * ISACCO (MESSAGGIO DI ERRORE ED USCITA SE NON TROVA UN FONT)
           initialize messaggio.

           inspect WFONT-NAME replacing trailing SPACE by LOW-VALUE.
           move WFONT-SIZE    to FONT-SIZE-DPLY.

           string  "Font: "         delimited size
                   WFONT-NAME       delimited LOW-VALUE
                   X"0D0A"          delimited size
                   "Dimensione: ",  delimited size 
                   FONT-SIZE-DPLY,  delimited size
                   X"0D0A"          delimited size
                   "Non installato. La stampa verrà abortita!"
                                    delimited size
              into messaggio

           inspect messaggio replacing trailing SPACE by LOW-VALUE.

           if RichiamoSchedulato
              move messaggio to como-riga
              perform SETTA-RIGA-STAMPA
           else
              display message messaggio
           end-if.

      ***---
       CLOSE-FILES.
           close tagli tgrupgdo tmarche articoli 
                 mtordini mrordini tpromo clienti.

           unlock tmp-marca all records.
           close  tmp-marca        
           delete file tmp-marca.

           unlock tmp-inevaso all records.
           close  tmp-inevaso.
           delete file tmp-inevaso.
                                         
           if RichiamoSchedulato
              move "CHIUSURA FILES" to como-riga
              perform SETTA-RIGA-STAMPA
           end-if.

      ***---
       EXIT-PGM.
           set environment "PRINTER" to "-P SPOOLER-DIRECT".

           destroy Verdana20BI.
           destroy Verdana16B.
           destroy Verdana10B.
           destroy Verdana8.
           destroy Verdana7I.
           destroy Verdana8BI.
           destroy font-pie-pagina.

           cancel "spooler".
           initialize spooler-link. 

           if lin-pdf and settaPDF-OK
              perform ASPETTA-PDF
           end-if

           if RichiamoSchedulato
              close lineseq
           end-if.

           goback.

      ***---
       CREA-PDF.
           accept DestFile from environment "PATH_ST_CLIENT".
      
      *****     inspect DestFile replacing trailing 
      *****                               spaces by low-value.
      
           string "Riepilogo_tagli"   delimited size  
                  "_("                delimited size
                  como-data(7:2)      delimited by size
                  "-"                 delimited size
                  como-data(5:2)      delimited by size
                  "-"                 delimited size
                  como-data(1:4)      delimited by size
                  "_-_"               delimited size
                  como-ora(1:2)       delimited size
                  "."                 delimited by size
                  como-ora(3:2)       delimited size
                  ")"                 delimited size
                  into NomeFile
           end-string.
                  
           set settaPDF-setta   to true
      
           move NomeFile  to settaPDF-nome-file
           move DestFile  to settaPDF-percorso
           call   "settaPDF2" using settaPDF-linkage
           cancel "settaPDF2"
      
           inspect NomeFile 
                   replacing trailing spaces by low-value.
           string NomeFile   delimited low-value
                  ".pdf"     delimited size
                  into NomeFile.
      
      *****     inspect DestFile 
      *****             replacing trailing spaces by low-value.
      *****
      *****     if not settaPDF-OK       
      *****        display message "Archiviazione PDF fallita!"
      *****                  title titolo
      *****                   icon 2
      *****     end-if.
      *****
      *****     inspect NomeFile 
      *****             replacing trailing spaces by low-value.
      *****     inspect DestFile 
      *****             replacing trailing spaces by low-value.
      *****
      *****     initialize lin-path-pdf
      *****           
      *****     string DestFile   delimited by low-value
      *****            NomeFile   delimited by low-value
      *****            into lin-path-pdf
      *****     inspect lin-path-pdf replacing trailing low-value by space


      ***---
       ASPETTA-PDF.
      *****     move lin-path-pdf to como-nome-file.
      *****     move 0 to cont.
      *****     inspect como-nome-file 
      *****             tallying cont for characters before ")".
      *****     move ".pdf " to como-nome-file(cont + 1: 5).
      *****
      *****     set trovato to false.
      *****     perform 60 times
      *****        CALL "C$FILEINFO" USING lin-path-pdf,
      *****                                file-info, 
      *****                         GIVING status-code
      *****        if status-code = 0
      *****           set trovato to true
      *****           exit perform
      *****        else
      *****           CALL "C$FILEINFO" USING como-nome-file,
      *****                                   file-info, 
      *****                            GIVING status-code
      *****           if status-code = 0
      *****              move como-nome-file to lin-path-pdf
      *****              set trovato to true
      *****              exit perform
      *****           end-if
      *****        end-if
      *****        call "c$sleep" using 1
      *****     end-perform.
      *****
      *****     if trovato
      *****        move 0  to old-size
      ******       aspetto finchè non esiste fisicamente il file
      *****        move 99 to minuti-partenza
      *****        perform until 1 = 2
      *****           CALL "C$FILEINFO" USING lin-path-pdf,
      *****                                   file-info, 
      *****                            GIVING status-code
      *****        
      *****           if status-code = 0
      *****              if FILE-SIZE not = 0
      *****                 if FILE-SIZE = old-size
      *****                    exit perform
      *****                 else
      *****                    move FILE-SIZE to old-size
      *****                    call "c$sleep" using 1
      *****                 end-if
      *****              end-if
      *****           else
      *****              perform TIME-OUT
      *****              if time-out-exit
      *****                 exit perform
      *****              end-if
      *****           end-if
      *****        
      *****        end-perform
      *****     end-if.      
           set settaPDF-resetta   to true.
           call   "settaPDF2" using settaPDF-linkage.
           cancel "settaPDF2".   

           if settaPDF-OK
              move settaPDF-nome-file to lin-path-pdf
           else
              if RichiamoSchedulato
                 move -1 to batch-status
              end-if
           end-if.

      *****
      ********---
      ***** TIME-OUT.
      ******    tengo 2 minuti di time-out
      *****     set time-out-exit to false.
      *****     accept como-time from time.
      *****
      *****     if minuti-partenza = 99
      *****        move como-time(3:2)  to minuti-partenza
      *****     end-if.
      *****
      *****     move como-time(3:2)  to minuti-arrivo.
      *****
      *****     if minuti-arrivo < minuti-partenza
      *****        add 60 to minuti-arrivo
      *****     end-if.
      *****     subtract minuti-partenza from minuti-arrivo.
      *****
      *****     if minuti-arrivo >= 3
      *****        set time-out-exit to true
      *****     else
      *****        call "c$sleep" using 1
      *****     end-if.


      ***---
       PARAGRAFO-COPY.
           copy "pie-di-pagina.cpy".
