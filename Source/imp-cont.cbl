       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      imp-cont.
       AUTHOR.                          Andrea.
       REMARKS. Importazione contestazioni con gestione delle eccezioni.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "lineseq.sl". 
           copy "tordini.sl".
           copy "tnotacr.sl".
           copy "contestazioni.sl".
           copy "tcontat.sl".
           copy "note-cont.sl".
           copy "clienti.sl".
           copy "destini.sl".
           
       SELECT rep-errori
           ASSIGN       TO path-rep-errori
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE  IS SEQUENTIAL
           FILE STATUS  IS STATUS-rep-errori.

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "lineseq.fd". 
           copy "tordini.fd".
           copy "tnotacr.fd".
           copy "contestazioni.fd".
           copy "tcontat.fd".
           copy "note-cont.fd".
           copy "clienti.fd".
           copy "destini.fd".

       FD  rep-errori.
       01 rep-riga        PIC  x(80).

       WORKING-STORAGE SECTION.

      * COSTANTI
       78  titolo value "Importazione contestazioni".

      * FILE STATUS
       77  status-lineseq        pic xx.
       77  status-contestazioni  pic xx.
       77  status-tordini        pic xx.
       77  status-tnotacr        pic xx.
       77  status-tcontat        pic xx.
       77  status-note-cont      pic xx.
       77  status-rep-errori     pic xx.
       77  status-clienti        pic xx.
       77  status-destini        pic xx.

       77  wstampa               pic x(256).
       77  path-rep-errori       pic x(256).

      * FLAGS
       77  controlli                  pic xx.
           88 valori-alfanumerici     value "VI".
           88 chiave-non-completa     value "CN".
           88 nessuna-tipologia       value "NT".
           88 tipologia-doppia        value "TD".
           88 cliente-non-valido      value "CL".
           88 destino-non-valido      value "DE".
           88 fattura-e-nota          value "FN".
           88 fattura-non-trovata     value "FT".
           88 nota-non-trovata        value "NC".
           88 nota-colleg-non-trovata value "CO".
           88 nessun-errore           value "OK".
 
       77  controllo             pic xx.
           88 tutto-ok           value "OK".
           88 errori             value "ER".

       77  filler                pic 9.
           88 RecLocked          value 1 false 0.

       77  filler                pic 9.
           88 PrimaVolta         value 1 false 0.

       77  filler                pic 9.
           88 ha-relazioni       value 1 false 0.

       77  filler                pic 9.
           88 anno-alphanumeric  value 1, false 0.

       77  filler                pic 9.
           88 numero-alphanumeric  value 1, false 0.

       77  filler                pic 9.
           88 num-fatt-alphanumeric  value 1, false 0.

       77  filler                pic 9.
           88 num-nota-alphanumeric  value 1, false 0.

       77  filler                pic 9.
           88 importo-alphanumeric  value 1, false 0.

       77  filler                pic 9.
           88 nota-colleg-alphanumeric  value 1, false 0.

       77  filler                pic 9.
           88 cliente-alphanumeric  value 1, false 0.

       77  filler                pic 9.
           88 destino-alphanumeric  value 1, false 0.


       77  filler                pic 9.
           88 anno-zero          value 1, false 0.

       77  filler                pic 9.
           88 numero-zero        value 1, false 0.

       77  filler                pic 9.
           88 num-fatt-zero      value 1, false 0.

       77  filler                pic 9.
           88 data-fatt-zero     value 1, false 0.

       77  filler                pic 9.
           88 num-nota-zero      value 1, false 0.

       77  filler                pic 9.
           88 data-nota-zero     value 1, false 0.

       77  filler                pic 9.
           88 cliente-zero       value 1, false 0.

       77  filler                pic 9.
           88 destino-zero       value 1, false 0.

      * VARIABILI
       77  motivo               pic x(50).
       77  num-rec              pic 9(6) value 0.
       77  rec-ok               pic 9(6) value 0.
       77  riga-ed              pic zzz.zz9.
       77  riga-tot-ed          pic zzz.zz9.

       77  idx                  pic 99.
       77  como-data            pic 9(8).
       77  como-ora             pic 9(8).

       77  como-importo         pic 9(11).
       77  como-num-fatt        pic 9(8).
       77  como-data-fatt       pic 9(8).
       77  como-num-nota        pic 9(8).
       77  como-data-nota       pic 9(8).

       01  r-riga.
         05 r-anno              pic x(4).
         05 r-numero            pic x(8).
         05 r-data              pic 9(8).
         05 r-num-fatt          pic x(8).
         05 r-data-fatt         pic 9(8).
         05 r-num-nota          pic x(8).
         05 r-data-nota         pic 9(8).
         05 r-prezzo            pic x.
         05 r-merce             pic x.
         05 r-reso              pic x.
         05 r-importo           pic x(11).
         05 r-bolla             pic x(20).
         05 r-num-nd            pic x(20).
         05 r-appunti           pic x(500).
         05 r-nota-colleg       pic x(8).
         05 r-cliente           pic x(5).
         05 r-destino           pic x(5).
         
       77  r-anno-z             pic z(4).
       77  r-numero-z           pic z(8).
       77  r-num-fatt-z         pic z(8).
       77  r-num-nota-z         pic z(8).
       77  r-importo-z          pic z(11).
       77  r-nota-colleg-z      pic z(8).
       77  r-cliente-z          pic z(5).
       77  r-destino-z          pic z(5).

      ******************************************************************
       PROCEDURE DIVISION.

       DECLARATIVES.
       TCONTAT-ERR SECTION.
           use after error procedure on tcontat.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tcontat
           when "39"
                set errori to true
                display message "File [TCONTAT] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TCONTAT] Indexed file corrupt!"
                          title titolo
                           icon 3  
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [TCONTAT] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99"
                display message box "Impossibile procedere."
                  x"0d0a""File [TCONTAT] già in uso"
                        title = titolo
                        icon 2
                set errori to true
           end-evaluate.

       CONTESTAZIONI-ERR SECTION.
           use after error procedure on contestazioni.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-contestazioni
           when "39"
                set errori to true
                display message "File [CONTESTAZIONI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[CONTESTAZIONI] Indexed file corrupt!"
                          title titolo
                           icon 3  
           when "35"
                display message box "Impossibile procedere."
                  x"0d0a""File [CONTESTAZIONI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99"
                display message box "Impossibile procedere."
                  x"0d0a""File [CONTESTAZIONI] già in uso"
                        title = titolo
                        icon 2
                set errori to true
           end-evaluate.

       NOTE-CONT-ERR SECTION.
           use after error procedure on note-cont.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-note-cont
           when "39"
                set errori to true
                display message "File [NOTE-CONT] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[NOTE-CONT] Indexed file corrupt!"
                          title titolo
                           icon 3  
           when "35"
                display message box "Impossibile procedere."
                  x"0d0a""File [NOTE-CONT] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99"
                display message box "Impossibile procedere."
                  x"0d0a""File [NOTE-CONT] già in uso"
                        title = titolo
                        icon 2
                set errori to true
           end-evaluate.

       LINESEQ-ERR SECTION.
           use after error procedure on lineseq.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-lineseq
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
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File delle testate [LINESEQ] inesistente"
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
           set PrimaVolta    to true.
           set nessun-errore to true.
           accept como-data  from century-date.
           accept como-ora   from time.

           set tutto-ok to true.
           initialize wstampa.
           accept  wstampa from environment "PATH_ST".
           inspect wstampa replacing trailing spaces by low-value.
           string  wstampa             delimited low-value
                   "contestazioni.csv" delimited size
                   into wstampa
           end-string.
           initialize path-rep-errori.
           accept  path-rep-errori from environment "PATH_ST".
           inspect path-rep-errori replacing trailing 
                                   spaces by low-value.
           string  path-rep-errori     delimited low-value
                   "errori-imp-cont_"  delimited size
                   como-data           delimited size
                   "_"                 delimited size
                   como-ora            delimited size
                   ".txt"              delimited size
                   into path-rep-errori
           end-string.

      ***---
       OPEN-FILES.
           open input lineseq.
           if tutto-ok
              open i-o contestazioni allowing readers
              if tutto-ok
                 open i-o tcontat allowing readers
                 if tutto-ok
                    open i-o note-cont allowing readers
                    if tutto-ok
                       open input  tordini tnotacr clienti destini
                       open output rep-errori
                    else
                       close lineseq contestazioni tcontat
                    end-if
                 else
                    close lineseq contestazioni
                 end-if
              else
                 close lineseq
              end-if
           end-if.

      ***---
       ELABORAZIONE.
           initialize line-riga.
           perform until 1 = 2
              initialize line-riga
              read lineseq next at end exit perform end-read
              if line-riga  = spaces   exit perform end-if
              add 1 to num-rec
              unstring line-riga delimited by ";"
                       into r-anno
                            r-numero
                            r-data
                            r-num-fatt
                            r-data-fatt
                            r-num-nota
                            r-data-nota
                            r-prezzo
                            r-merce
                            r-reso
                            r-importo
                            r-bolla
                            r-num-nd
                            r-appunti
                            r-nota-colleg
                            r-cliente
                            r-destino
              end-unstring
              initialize cnt-rec replacing numeric data by zeroes
                                      alphanumeric data by spaces
              perform CONTROLLI
              if errori
                 perform SCRIVI-REPORT-ERRORI
              else

                 move r-anno-z         to cnt-anno
                 move r-numero-z       to cnt-numero
                 move r-data           to cnt-data
                 if r-prezzo not = spaces
                    set cnt-prezzo to true
                 end-if
                 if r-merce not = spaces
                    set cnt-merce to true
                 end-if
                 if r-reso not = spaces
                    set cnt-reso to true
                 end-if
                 move r-importo-z to como-importo
                 divide como-importo by 100 giving cnt-importo

                 move r-num-nd to cnt-numero-nota-debito
                 if cnt-numero-nota-debito not = spaces
                    set cnt-si-nota  to true
                    move cnt-importo to cnt-importo-nota
                 end-if
                 move r-appunti  to cnt-appunti 
                 move r-bolla    to cnt-bolla
                 if ha-relazioni
                    initialize noco-dati 
                               replacing numeric data by zeroes
                                    alphanumeric data by spaces
                    move tno-anno   to cnt-nota-anno-1 noco-anno
                    move tno-numero to cnt-nota-1      noco-numero
                    read note-cont
                         invalid 
                         accept noco-data-creazione from century-date
                         accept noco-ora-creazione  from time
                         move "BOSS" to noco-utente-creazione
                         move 1 to idx
                     not invalid
                         perform varying idx from 1 by 1
                                   until idx > 5
                            if noco-anno-cont(idx)   not = 0 and
                               noco-numero-cont(idx) not = 0
                               exit perform
                            end-if
                         end-perform
                    end-read
                    if idx <= 5
                       move cnt-anno   to noco-anno-cont(idx)
                       move cnt-numero to noco-numero-cont(idx)
                       write noco-rec invalid rewrite noco-rec end-write
                    end-if
                 end-if

                 set cnt-aperta  to true

                 accept cnt-data-creazione from century-date
                 accept cnt-ora-creazione  from time
                 move "BOSS"     to cnt-utente-creazione
                 write cnt-rec invalid rewrite cnt-rec end-write
                 move cnt-anno to con-anno
                 read tcontat invalid continue end-read
                 if con-num-cont < cnt-numero
                    move cnt-numero to con-num-cont
                    rewrite con-rec
                 end-if
                 add 1 to rec-ok
              end-if

           end-perform.
           if nessun-errore
              move num-rec to riga-ed
              display message "Nessun errore riscontrato!"
                       x"0d0a""Importate " riga-ed, " righe."
                        title titolo
           else
              write rep-riga from spaces
              move num-rec to riga-ed
              initialize rep-riga
              move rec-ok  to riga-ed
              move num-rec to riga-tot-ed
              initialize rep-riga
              string "IMPORTATE "                  delimited size
                     riga-ed                       delimited size
                     " CONTESTAZIONI CORRETTE SU " delimited size
                     riga-tot-ed                   delimited size
                     " TOTALI"                     delimited size
                     into rep-riga
              end-string
              write rep-riga
              write rep-riga from spaces

              close rep-errori
              display message "Importazione terminata con anomalie!"
                       x"0d0a""Verrà visualizzato il file degli errori!"
                        title titolo  
                         icon 2
              call   "spooler-a" using "A", path-rep-errori, "V"
              cancel "spooler-a"
              delete file rep-errori
           end-if.

      ***---
       CONTROLLI.
           call "C$JUSTIFY" using r-anno,        "R".
           call "C$JUSTIFY" using r-numero,      "R".
           call "C$JUSTIFY" using r-num-fatt,    "R".
           call "C$JUSTIFY" using r-num-nota,    "R".
           call "C$JUSTIFY" using r-importo,     "R".    
           call "C$JUSTIFY" using r-nota-colleg, "R".
           call "C$JUSTIFY" using r-cliente,     "R".
           call "C$JUSTIFY" using r-destino,     "R".

           inspect r-anno        replacing leading x"20" by x"30".
           inspect r-numero      replacing leading x"20" by x"30".
           inspect r-num-fatt    replacing leading x"20" by x"30".
           inspect r-num-nota    replacing leading x"20" by x"30".
           inspect r-importo     replacing leading x"20" by x"30".
           inspect r-nota-colleg replacing leading x"20" by x"30".
           inspect r-cliente     replacing leading x"20" by x"30".
           inspect r-destino     replacing leading x"20" by x"30".

           move r-anno        to r-anno-z.
           move r-numero      to r-numero-z.
           move r-num-fatt    to r-num-fatt-z.
           move r-num-nota    to r-num-nota-z.
           move r-importo     to r-importo-z.
           move r-nota-colleg to r-nota-colleg-z.
           move r-cliente     to r-cliente-z.
           move r-destino     to r-destino-z.

           set tutto-ok          to true.
           perform CONTROLLO-VALORI-NUMERICI.
           if tutto-ok
              perform CONTROLLO-TIPOLOGIA
           end-if.
           if tutto-ok
              perform CONTROLLA-DOCUMENTI
           end-if.

      ***---
       CONTROLLO-VALORI-NUMERICI.
           set anno-alphanumeric        to false.
           set numero-alphanumeric      to false.
           set num-fatt-alphanumeric    to false.
           set num-nota-alphanumeric    to false.
           set importo-alphanumeric     to false.
           set nota-colleg-alphanumeric to false.
           set cliente-alphanumeric     to false.
           set destino-alphanumeric     to false.

           if r-anno        is not numeric or
              r-numero      is not numeric or
              r-num-fatt    is not numeric or
              r-num-nota    is not numeric or
              r-importo     is not numeric or
              r-nota-colleg is not numeric
              set errori              to true
              set valori-alfanumerici to true
           end-if.
           |CONTROLLO CLIENTE E DESTINO SOLAMENTE SE MI SERVONO OSSIA
           |SE NON HO VALORIZZATO FATTURA E NOTA CREDITO DU PARTENZA
           if tutto-ok
              move r-num-fatt-z  to como-num-fatt
              move r-data-fatt   to como-data-fatt
              move r-num-nota-z  to como-num-nota
              move r-data-nota   to como-data-nota
              if como-num-fatt  = 0 or
                 como-data-fatt = 0
                 if como-num-nota  = 0 or
                    como-data-nota = 0
                    if r-cliente     is not numeric or
                       r-destino     is not numeric
                       set valori-alfanumerici to true
                       set errori              to true
                    end-if
                 end-if
              end-if

           end-if.
           if tutto-ok
              move r-anno-z   to cnt-anno
              move r-numero-z to cnt-numero
              if cnt-anno   = 0 or
                 cnt-numero = 0
                 set chiave-non-completa to true
                 set errori              to true
              end-if
           end-if.
           if valori-alfanumerici
              if r-anno        is not numeric
                 set anno-alphanumeric     to true
              end-if
              if r-numero      is not numeric
                 set numero-alphanumeric   to true
              end-if
              if r-num-fatt    is not numeric
                 set num-fatt-alphanumeric to true
              end-if
              if r-num-nota    is not numeric
                 set num-nota-alphanumeric to true
              end-if
              if r-importo     is not numeric
                 set importo-alphanumeric  to true
              end-if
              if r-nota-colleg is not numeric
                 set nota-colleg-alphanumeric to true
              end-if
              if r-cliente     is not numeric
                 set cliente-alphanumeric  to true
              end-if
              if r-destino     is not numeric
                 set destino-alphanumeric  to true
              end-if
           end-if.

      ***---
       CONTROLLO-TIPOLOGIA.
           if r-merce  = spaces and
              r-prezzo = spaces and
              r-reso   = spaces
              set nessuna-tipologia to true
              set errori            to true
           end-if.

           if r-prezzo not = spaces and
              r-reso   not = spaces
              set tipologia-doppia to true
              set errori           to true
           end-if.

           if r-prezzo not = spaces and
              r-merce  not = spaces
              set tipologia-doppia to true
              set errori           to true
           end-if.   

           if r-merce not = spaces and
              r-reso  not = spaces
              set tipologia-doppia to true
              set errori           to true
           end-if.

      ***---
       CONTROLLA-DOCUMENTI.
           move r-num-fatt-z     to tor-num-fattura.
           move r-data-fatt(1:4) to tor-anno-fattura.
           move r-num-nota-z     to tno-num-fattura.
           move r-data-nota(1:4) to tno-anno-fattura.
           if tor-num-fattura not = 0 and tno-num-fattura not = 0
              set fattura-e-nota  to true
              set errori          to true
           end-if.

           if tor-num-fattura = 0 and tno-num-fattura = 0
              move r-cliente-z     to cli-codice cnt-cod-cli
              set  cli-tipo-C      to true
              read clienti no lock
                   invalid
                   set errori             to true
                   set cliente-non-valido to true
               not invalid
                   move cli-codice  to des-codice
                   move r-destino-z to des-prog
                   if des-prog not = 0
                      read destini no lock
                           invalid
                           set errori             to true
                           set destino-non-valido to true
                       not invalid
                           move des-prog to cnt-prg-destino
                      end-read
                   end-if
              end-read
           end-if.

           if tutto-ok
              if tor-num-fattura not = 0
                 read tordini no lock key k-fattura
                      invalid
                      set errori              to true
                      set fattura-non-trovata to true
                  not invalid
                      move tor-cod-cli      to cnt-cod-cli
                      move tor-prg-destino  to cnt-prg-destino
                      move tor-anno-fattura to cnt-anno-fatt
                      move tor-num-fattura  to cnt-num-fatt
                      move tor-data-fattura to cnt-data-fatt
                 end-read      
              end-if
              if tno-num-fattura not = 0
                 read tnotacr no lock key k-fattura
                      invalid
                      set errori           to true
                      set nota-non-trovata to true
                  not invalid
                      move tno-cod-cli      to cnt-cod-cli
                      move tno-prg-destino  to cnt-prg-destino
                      move tno-anno-fattura to cnt-anno-nota
                      move tno-num-fattura  to cnt-num-nota
                      move tno-data-fattura to cnt-data-nota
                 end-read      
              end-if
           end-if.
           set ha-relazioni to false.
           if tutto-ok
              move r-nota-colleg-z to tno-num-fattura
              if tno-num-fattura not = 0
                 move r-anno-z   to tno-anno-fattura
                 read tnotacr no lock key k-fattura
                      invalid
                      set errori                  to true
                      set nota-colleg-non-trovata to true
                  not invalid
                      set ha-relazioni to true
                 end-read
              end-if
           end-if.

      ***---
       SCRIVI-REPORT-ERRORI.
           if PrimaVolta
              set PrimaVolta to false
              initialize rep-riga
              string "** ERRORI RISCONTRATI "    delimited size 
                     "IMPORT CONTESTAZIONI AL: " delimited size
                     como-data(7:2)              delimited size
                     "/"                         delimited size
                     como-data(5:2)              delimited size
                     "/"                         delimited size
                     como-data(1:4)              delimited size
                     " ALLE: "                   delimited size
                     como-ora(1:2)               delimited size
                     ":"                         delimited size
                     como-ora(3:2)               delimited size
                     " **"                       delimited size
                     into rep-riga
              end-string
              write rep-riga
              write rep-riga from spaces
           end-if.
           
           move num-rec to riga-ed.

           initialize rep-riga.
           evaluate true
           when valori-alfanumerici
                if anno-alphanumeric
                   move ": FORMATO ANNO NON VALIDO"   to motivo
                   perform SCRIVI-RIGA-ERRORE
                end-if
                if numero-alphanumeric
                   move ": FORMATO NUMERO NON VALIDO"   to motivo
                   perform SCRIVI-RIGA-ERRORE
                end-if
                if num-fatt-alphanumeric
                   move ": FORMATO N. FATTURA NON VALIDO"   to motivo
                   perform SCRIVI-RIGA-ERRORE
                end-if
                if num-nota-alphanumeric
                   move ": FORMATO N. NOTA NON VALIDO"   to motivo
                   perform SCRIVI-RIGA-ERRORE
                end-if
                if importo-alphanumeric
                   move ": FORMATO IMPORTO NON VALIDO"   to motivo
                   perform SCRIVI-RIGA-ERRORE
                end-if
                if nota-colleg-alphanumeric
                   move ": FORMATO NOTA COLLEG. NON VALIDO"   to motivo
                   perform SCRIVI-RIGA-ERRORE
                end-if
                if cliente-alphanumeric
                   move ": FORMATO CLIENTE NON VALIDO"   to motivo
                   perform SCRIVI-RIGA-ERRORE
                end-if
                if destino-alphanumeric
                   move ": FORMATO DESTINO NON VALIDO"   to motivo
                   perform SCRIVI-RIGA-ERRORE
                end-if

           when chiave-non-completa
                move ": CHIAVE INCOMPLETA"   to motivo
                perform SCRIVI-RIGA-ERRORE

           when nessuna-tipologia
                move ": NESSUNA TIPOLOGIA"   to motivo
                perform SCRIVI-RIGA-ERRORE

           when tipologia-doppia
                move ": TIPOLOGIA DOPPIA"    to motivo
                perform SCRIVI-RIGA-ERRORE

           when cliente-non-valido
                move ": CLIENTE NON VALIDO"  to motivo
                perform SCRIVI-RIGA-ERRORE

           when destino-non-valido
                move ": DESTINO NON VALIDO"  to motivo
                perform SCRIVI-RIGA-ERRORE

           when fattura-non-trovata
                move ": FATTURA NON TROVATA" to motivo
                perform SCRIVI-RIGA-ERRORE

           when nota-non-trovata
                move ": NOTA CREDITO NON TROVATA" to motivo
                perform SCRIVI-RIGA-ERRORE

           when nota-colleg-non-trovata
                move ": NOTA CREDITO COLLEGATA NON TROVATA" to motivo
                perform SCRIVI-RIGA-ERRORE

           end-evaluate.

      ***---
       SCRIVI-RIGA-ERRORE.
           string "RIGA "  delimited size
                  riga-ed  delimited size
                  motivo   delimited size
                  into rep-riga
           end-string.
           write rep-riga.

      ***---
       CLOSE-FILES.
           close contestazioni tordini 
                 tnotacr lineseq note-cont
                 clienti destini.

      ***---
       EXIT-PGM.
           goback.
