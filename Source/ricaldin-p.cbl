       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      ricaldin-p.
       AUTHOR.                          Andrea.
       REMARKS. Ricalcolo dati dinamici da:
                - note credito non fatturate
                - ordini non fatturati ma bolla emessa
                - movimenti di magazzino successivi al consolidamento

                - ordinato su ordini fornitori inviati e in lavorazione
                  in aumento per ordinata - evasa (aggiornamento stato e 
                  pezzi evasi)
                - giacenza su bozze aperte in aumento

                - aumento impegnato da ordini master (maggiore tra ord e eva) 
                  non chiusi (aggiornamento stato, pezzi e prezzi)

                da lanciare manualmente previo azzeramento dei 
                valori dinamici su progmag.
                La data di consolidamento viene passata in linkage.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.           
           copy "tsetinvio.sl".
           copy "mtordini.sl".
           copy "mrordini.sl".
           copy "tmovmag.sl".
           copy "rmovmag.sl".
           copy "tordini.sl".
           copy "rordini.sl".
           copy "tnotacr.sl".
           copy "rnotacr.sl".
           copy "progmag.sl".
           copy "tordforn.sl".
           copy "rordforn.sl".
           copy "teva.sl".
           copy "reva.sl".
           copy "clienti.sl".
           copy "destini.sl".
           copy "tpromo.sl".
           copy "tagli.sl".
           copy "ttipocli.sl".
           copy "lineseq.sl".
           copy "tparamge.sl".
           copy "tscorte.sl".
           copy "articoli.sl".
           copy "tcaumag.sl".
      *****     copy "evaclides.sl".
           copy "timposte.sl".
           copy "tmarche.sl".
           copy "param.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.          
           copy "tsetinvio.fd".
           copy "mtordini.fd".
           copy "mrordini.fd".
           copy "tmovmag.fd".
           copy "rmovmag.fd".
           copy "tordini.fd".
           copy "rordini.fd".
           copy "tnotacr.fd".
           copy "rnotacr.fd".
           copy "progmag.fd". 
           copy "tordforn.fd".
           copy "rordforn.fd".
           copy "teva.fd".
           copy "reva.fd".
           copy "clienti.fd".
           copy "destini.fd".
           copy "tpromo.fd". 
           copy "tagli.fd".
           copy "ttipocli.fd".
           copy "lineseq.fd".
           copy "tparamge.fd".
           copy "tscorte.fd".
           copy "articoli.fd".
           copy "tcaumag.fd".
      *****     copy "evaclides.fd".
           copy "timposte.fd".
           copy "tmarche.fd".
           copy "param.fd".

       WORKING-STORAGE SECTION.
           copy "link-geslock.def".
           copy "link-wprogmag.def".
           copy "aggiorna-stato-ordf.def".
           copy "aggiorna-stato-master.def".
           copy "versione-evasione.def".
           copy "trova-parametro.def".

       78  user-codi             value "BATCH".
       78  titolo                value "Ricalcolo valori dinamici".
       78  78-clear              value 
           "                                                          ".

       77  scelta                pic 9(8).
       77  status-tmovmag        pic xx.
       77  status-rmovmag        pic xx.
       77  status-progmag        pic xx.
       77  status-mtordini       pic xx.
       77  status-mrordini       pic xx.
       77  status-tordini        pic xx.
       77  status-rordini        pic xx.
       77  status-tnotacr        pic xx.
       77  status-rnotacr        pic xx.
       77  status-tordforn       pic xx.
       77  status-rordforn       pic xx.
       77  status-teva           pic xx.
       77  status-reva           pic xx.
       77  status-clienti        pic xx.
       77  status-destini        pic xx.
       77  status-tpromo         pic xx.
       77  status-tagli          pic xx.
       77  status-ttipocli       pic xx.
       77  status-tparamge       pic xx.
       77  status-tscorte        pic xx.
       77  status-articoli       pic xx.
       77  status-tcaumag        pic xx.
       77  status-timposte       pic xx.
       77  status-tmarche        pic xx.
      ***** 77  status-evaclides      pic xx.
       77  status-param          pic xx.
       77  status-lineseq        pic xx.
       77  status-tsetinvio      pic xx.
       77  wstampa               pic x(256).

       77  counter               pic 9(10).
       77  counter2              pic 9(10).
       77  counter-edit          pic z(10).

       77  start-secondi         pic 9(18).
       77  end-secondi           pic 9(18).
       77  tot-secondi           pic 9(18).
       77  hh                    pic 99.
       77  mm                    pic 99.
       77  ss                    pic 99.

       77  mese-oggi             pic 99.
       77  mese-consegna         pic 99.

       77  como-ora              pic 9(8).
       77  como-ora-edit         pic x(9).
       77  tot-call              pic 9(15).
       77  tot-call-edit         pic zzz.zzz.zzz.zzz.zz9.

       77  como-data             pic 9(8).
       77  old-mese              pic 99.

       77  como-data-1           pic 9(8).
       77  como-data-2           pic 9(8).
       77  resto                 pic 9(3).
       77  diff-giorni           pic 9(4).

       01  como-rec              pic x(5000).

       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".
           
       77  CosaElaborare         pic x.
           88  ElaboraGiacenza   value "G".
           88  ElaboraImpegnato  value "I".
      
       77  filler                pic 9.
           88 RecLocked          value 1, false 0.
       77  filler                pic 9.
           88 trovato            value 1, false 0.
       77  filler                pic 9.
           88 prima-volta        value 1, false 0.
       01  GdoInUsoFlag          pic x.
           88 GdoInUso           value "S". 
           88 GdoNonInUso        value " ".

       LINKAGE SECTION.
       77  user-link             pic x(10).
       77  link-data             pic 9(8).
       77  link-handle           handle of window.

      ******************************************************************
       PROCEDURE DIVISION using user-link link-data link-handle.

       DECLARATIVES.

      ***---
       MTORDINI-ERR SECTION.
           use after error procedure on mtordini.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-mtordini
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File delle testate [MTORDINI] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [MTORDINI] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[MTORDINI] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.  

      ***---
       MRORDINI-ERR SECTION.
           use after error procedure on mrordini.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-mrordini
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File delle testate [MRORDINI] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [MRORDINI] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[MRORDINI] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       TMOVMAG-ERR SECTION.
           use after error procedure on tmovmag.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tmovmag 
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File delle testate [TMOVMAG] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [TMOVMAG] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[TMOVMAG] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       RMOVMAG-ERR SECTION.
           use after error procedure on rmovmag.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-rmovmag 
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File delle righe [RMOVMAG] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [RMOVMAG] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[RMOVMAG] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       TORDINI-ERR SECTION.
           use after error procedure on tordini.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tordini
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File delle testate [TORDINI] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [TORDINI] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[TORDINI] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       RORDINI-ERR SECTION.
           use after error procedure on rordini.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-rordini
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File delle righe [RORDINI] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [RORDINI] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[RORDINI] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       TNOTACR-ERR SECTION.
           use after error procedure on tnotacr.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tnotacr
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File delle testate [TNOTACR] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [TNOTACR] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[TNOTACR] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       RNOTACR-ERR SECTION.
           use after error procedure on rnotacr.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-rnotacr
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File delle righe [RNOTACR] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [RNOTACR] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[RNOTACR] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       TORDFORN-ERR SECTION.
           use after error procedure on tordforn.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tordforn
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File delle testate [TORDFORN] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [TORDFORN] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[TORDFORN] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       RORDFORN-ERR SECTION.
           use after error procedure on rordforn.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-rordforn
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [RORDFORN] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [RORDFORN] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[RORDFORN] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       PROGMAG-ERR SECTION.
           use after error procedure on progmag.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-progmag 
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File dei progressivi [RMOVMAG] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [RMOVMAG] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[RMOVMAG] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       TEVA-ERR SECTION.
           use after error procedure on teva.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-teva
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File delle testate [TEVA] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [TEVA] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[TEVA] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       REVA-ERR SECTION.
           use after error procedure on reva.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-reva
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [REVA] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [REVA] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[REVA] Indexed file corrupt!"
                          title titolo
                           icon 3
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
           accept versione-evasione from environment "VERSIONE_EVASIONE"
           move 0 to counter counter2.
           set tutto-ok    to true.
           set prima-volta to true.

LUBEXX*****     compute link-data = function integer-of-date(link-data).
LUBEXX*****     add 1   to link-data.
LUBEXX*****     compute link-data = function date-of-integer(link-data).

      ***---
       OPEN-FILES.
           |Quando si consolida il magazzino NESSUNO deve essere
           |dentro al file dei movimenti (tranne che in visua) e mi
           |sembra il minimo dato che devo fare operazioni in massa

LUBEXX     |I files devono essere aperti in input. Corriamo il rischio
LUBEXX     |di elaborare intanto che ci lavorano, ma non importa
           open input clienti destini tpromo ttipocli tmarche timposte
                      tparamge tscorte articoli tcaumag |evaclides
                      param.
           if tutto-ok
              perform OPEN-INPUT-TMOVMAG-LOCK
              if tutto-ok
                 perform OPEN-INPUT-RMOVMAG-LOCK
                 if tutto-ok
                    perform OPEN-INPUT-TORDINI-LOCK
                    if tutto-ok
                       perform OPEN-INPUT-RORDINI-LOCK
                       if tutto-ok
                          perform OPEN-INPUT-TNOTACR-LOCK
                          if tutto-ok
                             perform OPEN-INPUT-RNOTACR-LOCK
                             if tutto-ok
                                perform OPEN-INPUT-TEVA-LOCK
                                if tutto-ok
                                   perform OPEN-INPUT-REVA-LOCK
                                   if tutto-ok
                                      perform OPEN-I-O-PROGMAG-LOCK
                                      if tutto-ok
                                         perform OPEN-I-O-TORDFORN
                                         if tutto-ok
                                            perform OPEN-I-O-RORDFORN 
                                            if tutto-ok
                                               perform OPEN-I-O-MTORDINI
                                               perform OPEN-I-O-MRORDINI
                                               if errori
                                                  close tmovmag rmovmag
                                                        tordini rordini 
                                                        progmag tnotacr 
                                                        reva teva
                                                        rnotacr tordforn
                                                        rordforn
                                               end-if      
                                            else
                                               close tmovmag rmovmag
                                                     tordini rordini 
                                                     progmag tnotacr 
                                                     reva teva
                                                     rnotacr tordforn
                                            end-if
                                         else
                                            close tmovmag rmovmag teva
                                                  tordini rordini 
                                                  progmag tnotacr 
                                                  rnotacr reva
                                         end-if
                                      else
                                         close tmovmag rmovmag
                                               tordini rordini teva
                                               tnotacr rnotacr reva
                                      end-if
                                   else
                                      close tmovmag rmovmag
                                            tordini rordini teva
                                            tnotacr rnotacr
                                   end-if
                                else
                                   close tmovmag rmovmag
                                         tordini rordini 
                                         tnotacr rnotacr
                                end-if
                             else
                                close tmovmag rmovmag tordini
                             end-if
                          else
                             close tmovmag rmovmag
                          end-if
                       else
                          close tmovmag
                       end-if
                    end-if
                 end-if
              end-if
           end-if.

      ***---
       OPEN-INPUT-TMOVMAG-LOCK.
           move "tmovmag" to geslock-nome-file.
           initialize geslock-messaggio.
           string   "Il file delle testate dei movimenti" 
             x"0d0a""risulta in uso su altro terminale."
             x"0d0a""Questo comporta l'impossibilità a"
             x"0d0a""procedere con il ricalcolo." delimited size
                 into geslock-messaggio
           end-string.

           set tutto-ok  to true.
           set RecLocked to false.
LUBEXX     open input tmovmag.
LUBEXX*****     open i-o tmovmag allowing readers.
           if RecLocked
              set errori to true
              move 1     to geslock-v-termina
              move 1     to geslock-v-riprova
              move 0     to geslock-v-ignora
              call   "geslock" using geslock-linkage
              cancel "geslock"

              evaluate true
              when riprova perform OPEN-INPUT-TMOVMAG-LOCK
              when other   display message box "Operazione interrotta!"
                                   title titolo
                                   icon 2
              end-evaluate
           end-if.

      ***---
       OPEN-INPUT-RMOVMAG-LOCK.
           move "rmovmag" to geslock-nome-file.
           initialize geslock-messaggio.
           string   "Il file delle righe dei movimenti" 
             x"0d0a""risulta in uso su altro terminale."
             x"0d0a""Questo comporta l'impossibilità a"
             x"0d0a""procedere con il ricalcolo." delimited size
                 into geslock-messaggio
           end-string.

           set tutto-ok  to true.
           set RecLocked to false.
LUBEXX     open input rmovmag.
LUBEXX*****     open i-o rmovmag allowing readers.
           if RecLocked
              set errori to true
              move 1     to geslock-v-termina
              move 1     to geslock-v-riprova
              move 0     to geslock-v-ignora
              call   "geslock" using geslock-linkage
              cancel "geslock"

              evaluate true
              when riprova perform OPEN-INPUT-RMOVMAG-LOCK
              when other   display message box "Operazione interrotta!"
                                   title titolo
                                   icon 2
              end-evaluate
           end-if.

      ***---
       OPEN-INPUT-TORDINI-LOCK.
           move "tordini" to geslock-nome-file.
           initialize geslock-messaggio.
           string   "Il file delle testate degli ordini" 
             x"0d0a""risulta in uso su altro terminale."
             x"0d0a""Questo comporta l'impossibilità a"
             x"0d0a""procedere con il ricalcolo." delimited size
                 into geslock-messaggio
           end-string.

           set tutto-ok  to true.
           set RecLocked to false.
LUBEXX     open input tordini.
LUBEXX*****     open i-o tordini allowing readers.
           if RecLocked
              set errori to true
              move 1     to geslock-v-termina
              move 1     to geslock-v-riprova
              move 0     to geslock-v-ignora
              call   "geslock" using geslock-linkage
              cancel "geslock"

              evaluate true
              when riprova perform OPEN-INPUT-TORDINI-LOCK
              when other   display message box "Operazione interrotta!"
                                   title titolo
                                   icon 2
              end-evaluate
           end-if.

      ***---
       OPEN-INPUT-RORDINI-LOCK.
           move "rordini" to geslock-nome-file.
           initialize geslock-messaggio.
           string   "Il file delle righe degli ordini" 
             x"0d0a""risulta in uso su altro terminale."
             x"0d0a""Questo comporta l'impossibilità a"
             x"0d0a""procedere con il ricalcolo." delimited size
                 into geslock-messaggio
           end-string.

           set tutto-ok  to true.
           set RecLocked to false.
LUBEXX     open input rordini.
LUBEXX*****     open i-o rordini allowing readers.
           if RecLocked
              set errori to true
              move 1     to geslock-v-termina
              move 1     to geslock-v-riprova
              move 0     to geslock-v-ignora
              call   "geslock" using geslock-linkage
              cancel "geslock"

              evaluate true
              when riprova perform OPEN-INPUT-RORDINI-LOCK
              when other   display message box "Operazione interrotta!"
                                   title titolo
                                   icon 2
              end-evaluate
           end-if.

      ***---
       OPEN-INPUT-TNOTACR-LOCK.
           move "tnotacr" to geslock-nome-file.
           initialize geslock-messaggio.
           string   "Il file delle testate note credito" 
             x"0d0a""risulta in uso su altro terminale."
             x"0d0a""Questo comporta l'impossibilità a"
             x"0d0a""procedere con il ricalcolo." delimited size
                 into geslock-messaggio
           end-string.

           set tutto-ok  to true.
           set RecLocked to false.
LUBEXX     open input tnotacr.
LUBEXX*****     open i-o tordini allowing readers.
           if RecLocked
              set errori to true
              move 1     to geslock-v-termina
              move 1     to geslock-v-riprova
              move 0     to geslock-v-ignora
              call   "geslock" using geslock-linkage
              cancel "geslock"

              evaluate true
              when riprova perform OPEN-INPUT-TNOTACR-LOCK
              when other   display message box "Operazione interrotta!"
                                   title titolo
                                   icon 2
              end-evaluate
           end-if.

      ***---
       OPEN-INPUT-RNOTACR-LOCK.
           move "rnotacr" to geslock-nome-file.
           initialize geslock-messaggio.
           string   "Il file delle righe note credito" 
             x"0d0a""risulta in uso su altro terminale."
             x"0d0a""Questo comporta l'impossibilità a"
             x"0d0a""procedere con il ricalcolo." delimited size
                 into geslock-messaggio
           end-string.

           set tutto-ok  to true.
           set RecLocked to false.
LUBEXX     open input rnotacr.
LUBEXX*****     open i-o rordini allowing readers.
           if RecLocked
              set errori to true
              move 1     to geslock-v-termina
              move 1     to geslock-v-riprova
              move 0     to geslock-v-ignora
              call   "geslock" using geslock-linkage
              cancel "geslock"
              
              evaluate true
              when riprova perform OPEN-INPUT-RNOTACR-LOCK
              when other   display message box "Operazione interrotta!"
                                   title titolo
                                   icon 2
              end-evaluate
           end-if.

      ***---
       OPEN-I-O-PROGMAG-LOCK.
           |Lo apro in lock per verificare che attualmente non ci
           |sia dentro nessuno. Il controllo dell'apertura con
           |lock passa poi direttamente al pgm. "wprogmag".
           move "progmag" to geslock-nome-file.
           initialize geslock-messaggio.
           string   "Il file dei progressivi di magazzino" 
             x"0d0a""risulta in uso su altro terminale."
             x"0d0a""Questo comporta l'impossibilità a"
             x"0d0a""procedere con il ricalcolo." delimited size
                 into geslock-messaggio
           end-string.

           set tutto-ok  to true.
           set RecLocked to false.
           open i-o progmag allowing readers.
           if RecLocked
              set errori to true
              move 1     to geslock-v-termina
              move 1     to geslock-v-riprova
              move 0     to geslock-v-ignora
              call   "geslock" using geslock-linkage
              cancel "geslock"

              evaluate true
              when riprova perform OPEN-I-O-PROGMAG-LOCK
              when other   display message "Operazione interrotta!"
                                     title titolo
                                      icon 2
              end-evaluate
           end-if.

      ***---
       OPEN-INPUT-TEVA-LOCK.
           move "teva" to geslock-nome-file.
           initialize geslock-messaggio.
           string   "Il file [TEVA]" 
             x"0d0a""risulta in uso su altro terminale."
             x"0d0a""Questo comporta l'impossibilità a"
             x"0d0a""procedere con il ricalcolo." delimited size
                 into geslock-messaggio
           end-string.

           set tutto-ok  to true.
           set RecLocked to false.
LUBEXX     open input teva.
LUBEXX*****     open i-o teva allowing readers.
           if RecLocked
              set errori to true
              move 1     to geslock-v-termina
              move 1     to geslock-v-riprova
              move 0     to geslock-v-ignora
              call   "geslock" using geslock-linkage
              cancel "geslock"

              evaluate true
              when riprova perform OPEN-INPUT-TEVA-LOCK
              when other   display message box "Operazione interrotta!"
                                   title titolo
                                   icon 2
              end-evaluate
           end-if.

      ***---
       OPEN-INPUT-REVA-LOCK.
           move "reva" to geslock-nome-file.
           initialize geslock-messaggio.
           string   "Il file [REVA]" 
             x"0d0a""risulta in uso su altro terminale."
             x"0d0a""Questo comporta l'impossibilità a"
             x"0d0a""procedere con il ricalcolo." delimited size
                 into geslock-messaggio
           end-string.

           set tutto-ok  to true.
           set RecLocked to false.
LUBEXX     open input reva.
LUBEXX*****     open i-o reva allowing readers.
           if RecLocked
              set errori to true
              move 1     to geslock-v-termina
              move 1     to geslock-v-riprova
              move 0     to geslock-v-ignora
              call   "geslock" using geslock-linkage
              cancel "geslock"

              evaluate true
              when riprova perform OPEN-INPUT-REVA-LOCK
              when other   display message box "Operazione interrotta!"
                                   title titolo
                                   icon 2
              end-evaluate
           end-if.

      ***---
       OPEN-I-O-MTORDINI.
           |Lo apro in i-o perchè devo modificare ma ad ogni scrittura
           |devo verificare che non sia presente il lock
           move "mtordini" to geslock-nome-file.
           initialize geslock-messaggio.
           string   "Il file [MTORDINI]" 
             x"0d0a""risulta in uso su altro terminale."
             x"0d0a""Questo comporta l'impossibilità a"
             x"0d0a""procedere con il ricalcolo." delimited size
                 into geslock-messaggio
           end-string.

           set tutto-ok  to true.
           set RecLocked to false.
           open i-o mtordini.
           if RecLocked
              set errori to true
              move 1     to geslock-v-termina
              move 1     to geslock-v-riprova
              move 0     to geslock-v-ignora
              call   "geslock" using geslock-linkage
              cancel "geslock"

              evaluate true
              when riprova perform OPEN-I-O-MTORDINI
              when other   display message "Operazione interrotta!"
                                     title titolo
                                      icon 2
              end-evaluate
           end-if.

      ***---
       OPEN-I-O-MRORDINI.
           |Lo apro in i-o perchè devo modificare ma ad ogni scrittura
           |devo verificare che non sia presente il lock
           move "mtordini" to geslock-nome-file.
           initialize geslock-messaggio.
           string   "Il file [MRORDINI]" 
             x"0d0a""risulta in uso su altro terminale."
             x"0d0a""Questo comporta l'impossibilità a"
             x"0d0a""procedere con il ricalcolo." delimited size
                 into geslock-messaggio
           end-string.

           set tutto-ok  to true.
           set RecLocked to false.
           open i-o mrordini.
           if RecLocked
              set errori to true
              move 1     to geslock-v-termina
              move 1     to geslock-v-riprova
              move 0     to geslock-v-ignora
              call   "geslock" using geslock-linkage
              cancel "geslock"

              evaluate true
              when riprova perform OPEN-I-O-MRORDINI
              when other   display message "Operazione interrotta!"
                                     title titolo
                                      icon 2
              end-evaluate
           end-if.

      ***---
       OPEN-I-O-TORDFORN.
           |Lo apro in i-o perchè devo modificare ma ad ogni scrittura
           |devo verificare che non sia presente il lock
           move "tordforn" to geslock-nome-file.
           initialize geslock-messaggio.
           string   "Il file [TORDFORN]" 
             x"0d0a""risulta in uso su altro terminale."
             x"0d0a""Questo comporta l'impossibilità a"
             x"0d0a""procedere con il ricalcolo." delimited size
                 into geslock-messaggio
           end-string.

           set tutto-ok  to true.
           set RecLocked to false.
           open i-o tordforn.
           if RecLocked
              set errori to true
              move 1     to geslock-v-termina
              move 1     to geslock-v-riprova
              move 0     to geslock-v-ignora
              call   "geslock" using geslock-linkage
              cancel "geslock"

              evaluate true
              when riprova perform OPEN-I-O-TORDFORN
              when other   display message "Operazione interrotta!"
                                     title titolo
                                      icon 2
              end-evaluate
           end-if.

      ***---
       OPEN-I-O-RORDFORN.
           |Lo apro in i-o perchè devo modificare ma ad ogni scrittura
           |devo verificare che non sia presente il lock
           move "rordforn" to geslock-nome-file.
           initialize geslock-messaggio.
           string   "Il file [RORDFORN]" 
             x"0d0a""risulta in uso su altro terminale."
             x"0d0a""Questo comporta l'impossibilità a"
             x"0d0a""procedere con il ricalcolo." delimited size
                 into geslock-messaggio
           end-string.

           set tutto-ok  to true.
           set RecLocked to false.
           open i-o rordforn.
           if RecLocked
              set errori to true
              move 1     to geslock-v-termina
              move 1     to geslock-v-riprova
              move 0     to geslock-v-ignora
              call   "geslock" using geslock-linkage
              cancel "geslock"

              evaluate true
              when riprova perform OPEN-I-O-RORDFORN
              when other   display message "Operazione interrotta!"
                                     title titolo
                                      icon 2
              end-evaluate
           end-if.

      ***---
       ELABORAZIONE.
           perform AZZERA-PROGMAG. 
           perform ELABORA-ORDINI-MASTER.
           perform ELABORA-NOTE-CREDITO-NON-FATTURATI.           
           perform ELABORA-INEVASI-E-BOLLA-EMESSA-NON-FATTURATI.  
           perform ELABORA-MOVIMENTI-DI-MAGAZZINO. 
           perform ELABORA-ORDINATO-ORDF.
           perform ELABORA-ORDINATO-EVASIONI-F.
    
      ***---
       AZZERA-PROGMAG.
      *****     perform ACCEPT-FORMAT-TIME.
      *****     display "[RICALDIN-P]: Azzeram. PROGMAG at: ", como-ora-edit 
      *****        upon syserr.
           |RIPULISCO LA SCREEN DAL CONTATORE
           display 78-clear
              upon link-handle at column 01
                                    line 03.
           ||||||||
           move low-value to prg-rec.
           start progmag key is >= prg-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2

                    add 1 to counter
                    add 1 to counter2
                    if counter2 = 100
                       if counter = 100
                          display "RICALDIN ON PROGMAG AZZERA"
                             upon link-handle at column 18,00
                                                   line  3,00
                       end-if
                       move counter to counter-edit
                       display counter-edit
                          upon link-handle at column 34,00
                                                line  3,00
                       move 0 to counter2
                    end-if

      *****              accept  como-ora from time
      *****              display "READ INI " como-ora upon syserr

                    read progmag next at end exit perform end-read 
      *****              accept  como-ora from time
      *****              display "READ FIN " como-ora upon syserr

                    |Azzeramento dei valori dinamici:
                    |GIACENZA - IMPEGNATO - ORDINATO
                    |L'ordinato sarà poi ricalcolato dalla nuova
                    |funzione richiamata immediatamente dopo
                    move 0 to prg-giacenza
                    move 0 to prg-impegnato
                    move 0 to prg-imp-master
                    move 0 to prg-imp-TRAD
                    move 0 to prg-imp-GDO
                    move 0 to prg-ordinato-1
                    move 0 to prg-ordinato-2
                    move 0 to prg-ordinato-3
                    move 0 to prg-ordinato-4
                    move 0 to prg-ordinato-5
                    move 0 to prg-ordinato-6
                    move 0 to prg-giacenza-bloc
                    |Metto in giacenza dinamica quella del periodo
                    move prg-giacenza-udm to prg-giacenza
                    |Aggiornamento del record

      *****              accept  como-ora from time
      *****              display "REWRITE INI " como-ora upon syserr

                    rewrite prg-rec invalid continue end-rewrite

      *****              accept  como-ora from time
      *****              display "REWRITE FIN " como-ora upon syserr

                 end-perform
           end-start.

      ***---
       ELABORA-ORDINI-MASTER.
           move 0 to counter counter2.
           |RIPULISCO LA SCREEN DAL CONTATORE
           display 78-clear
              upon link-handle at column 01
                                    line 03.

           move low-value to mto-rec.
           set mto-registrato to true.
           start mtordini key >= k-mto-stato
                 invalid continue
             not invalid
                 perform until 1 = 2

                    add 1 to counter
                    add 1 to counter2
                    if counter2 = 100
                       if counter = 100
                          display "RICALDIN ON MTORDINI"
                             upon link-handle at column 18,00
                                                   line  3,00
                       end-if
                       move counter to counter-edit
                       display counter-edit
                          upon link-handle at column 34,00
                                                line  3,00
                       move 0 to counter2
                    end-if

                    read mtordini next at end exit perform end-read
                    move mto-rec to como-rec
                    if mto-chiuso
                       exit perform
                    end-if
                    perform LOOP-RIGHE-MRORDINI
      *****              set ricalcolo to true
      *****              perform AGGIORNA-STATO-MASTER
      *****              move como-rec to mto-rec
      *****              start mtordini key > k-mto-stato
      *****                    invalid  exit perform
      *****              end-start

                 end-perform
           end-start.

      ***---
       LOOP-RIGHE-MRORDINI.
           move mto-chiave to mro-chiave-testa.
           move low-value  to mro-riga.
           start mrordini  key >= mro-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read mrordini next at end exit perform end-read
                    if mro-chiave-testa not = mto-chiave
                       exit perform
                    end-if
                    if mro-chiuso
                       continue
                    else
                       perform AGGIORNA-IMPEGNATO-MASTER
                    end-if
                 end-perform
           end-start.

      ***----
       AGGIORNA-IMPEGNATO-MASTER.
           set  cli-tipo-C  to true.
           move mto-cod-cli to cli-codice.
           read clienti     no lock.

           |Ad aumentare l'impegnato sulle qta evase 
           |ci penseranno poi le evasioni
           if mro-qta > mro-qta-e
              compute link-valore = mro-qta - mro-qta-e
           else
              move 0       to link-valore
           end-if.
           move link-valore to link-impegnato.
           perform DIREZIONA-IMPEGNATO.

           move "BATCH"               to link-user.
           move "0100000000000000"    to link-array.
           move mto-causale           to link-causale.
           move mro-prg-chiave        to link-key.
           set  link-update           to true.
           set  link-open-with-lock   to true.
           set  link-update-um        to true.
           set  link-update-peso      to false.
           set  link-update-valore    to false.
           set  link-chiamata-batch   to true.
           if prima-volta
              set prima-volta to false
              close progmag |wprogmag aprirà con lock il file!!!
           end-if.
           call "wprogmag" using link-wprogmag.
           if link-wprogmag-status = -1
              move -2 to link-wprogmag-status
              set errori to true
           end-if.

      *****     perform ACCEPT-FORMAT-TIME.
      *****     display "[RICALDIN-P]:  Fine Azzeram. at: ", como-ora-edit 
      *****        upon syserr.

      ***---
       ELABORA-NOTE-CREDITO-NON-FATTURATI.
           move 0 to tot-call.
      *****     perform ACCEPT-FORMAT-TIME.
      *****     display "[RICALDIN-P]: Ciclo TORDINI at: ", como-ora-edit 
      *****       upon syserr.

           move 0 to counter counter2.
           |RIPULISCO LA SCREEN DAL CONTATORE
           display 78-clear
              upon link-handle at column 01
                                    line 03.
           ||||||||
           move 0 to tno-anno-fattura.
           move 0 to tno-num-fattura.

           start tnotacr key is >= k-fattura
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tnotacr next at end exit perform end-read
                 
      *****              perform ACCEPT-FORMAT-TIME
      *****              display "[ORDINI]: Letto ", tor-chiave, 
      *****                      " at: ", como-ora-edit
      *****                 upon syserr
                 
                    add 1 to counter
                    add 1 to counter2
                    if counter2 = 100
                       if counter = 100
                          display "RICALDIN ON TNOTACR"
                             upon link-handle at column 18,00
                                                   line  3,00
                       end-if
                       move counter to counter-edit
                       display counter-edit
                          upon link-handle at column 34,00
                                                line  3,00
                       move 0 to counter2
                    end-if
                 
                    if tno-data-fattura not = 0 or
                       tno-num-fattura  not = 0
                       exit perform
                    end-if
                 
                    move tno-causale to tca-codice
                    read tcaumag no lock invalid continue end-read
                    if tca-nota-credito-si and tca-tipo-nota-reso
                       perform LOOP-RIGHE-RNOTACR
                    end-if

                    if errori exit perform end-if
                 end-perform
           end-start.

           move tot-call to tot-call-edit.
      *****     perform ACCEPT-FORMAT-TIME.
      *****     display "[RICALDIN-P]: Fine Ciclo TORDINI at: " como-ora-edit
      *****             " totale chiamate a WPROGMAG: " tot-call-edit
      *****        upon syserr.

      ***---
       LOOP-RIGHE-RNOTACR.
      *****     perform ACCEPT-FORMAT-TIME.
           set trovato     to false.
           move tno-anno   to rno-anno.
           move tno-numero to rno-numero.
           move low-values to rno-num-riga.
           start rnotacr key is >= rno-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rnotacr  next at end exit perform end-read
                    if tno-anno   not = rno-anno or
                       tno-numero not = rno-numero
                       exit perform
                    end-if
                    move user-link             to link-user
                    move rno-prg-chiave        to link-key
                    move "1000000000000010"    to link-array
                    move rno-qta               to link-valore
                    move tno-causale           to link-causale
                    move rno-prg-chiave        to link-key
                    set link-update            to true
                    set link-open-with-lock    to true
                    set link-update-um         to true
                    set link-update-peso       to false
                    set link-update-valore     to false
                    if prima-volta
                       set prima-volta to false
                       close progmag |wprogmag aprirà con lock il file!!!
                    end-if
                    call "wprogmag" using link-wprogmag
                    add 1 to tot-call
                    set trovato     to true
                    if link-wprogmag-status = -1
                       set errori to true
                       exit perform
                    end-if
                 end-perform
           end-start.

      ***---
       ELABORA-INEVASI-E-BOLLA-EMESSA-NON-FATTURATI.
           move 0 to tot-call.
      *****     perform ACCEPT-FORMAT-TIME.
      *****     display "[RICALDIN-P]: Ciclo TORDINI at: ", como-ora-edit 
      *****       upon syserr.

           move 0 to counter counter2.
           |RIPULISCO LA SCREEN DAL CONTATORE
           display 78-clear
              upon link-handle at column 01
                                    line 03.
           ||||||||
           move 0 to tor-anno-fattura.
           move 0 to tor-num-fattura.

           start tordini key is >= k-fattura
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tordini next at end exit perform end-read
                 
      *****              perform ACCEPT-FORMAT-TIME
      *****              display "[ORDINI]: Letto ", tor-chiave, 
      *****                      " at: ", como-ora-edit
      *****                 upon syserr
                 
                    add 1 to counter
                    add 1 to counter2
                    if counter2 = 100
                       if counter = 100
                          display "RICALDIN ON TORDINI"
                             upon link-handle at column 18,00
                                                   line  3,00
                       end-if
                       move counter to counter-edit
                       display counter-edit
                          upon link-handle at column 34,00
                                                line  3,00
                       move 0 to counter2
                    end-if
                 
                    if tor-anno-fattura not = 0 or
                       tor-num-fattura  not = 0
                       exit perform
                    end-if
                 
                    if tor-data-fattura = 0
                       if tor-num-bolla  not = 0 and
                          tor-data-bolla not = 0
                          set ElaboraGiacenza  to true
                       else
                          set ElaboraImpegnato to true
                       end-if
                 
                       perform LOOP-RIGHE-RORDINI
                    end-if

                    if errori exit perform end-if
                 end-perform
           end-start.
                 
           move tot-call to tot-call-edit.
      *****     perform ACCEPT-FORMAT-TIME.
      *****     display "[RICALDIN-P]: Fine Ciclo TORDINI at: " como-ora-edit
      *****             " totale chiamate a WPROGMAG: " tot-call-edit
      *****        upon syserr.
              
      ***---  
       LOOP-RIGHE-RORDINI.
           set trovato     to false.
           move tor-anno   to ror-anno.
           move tor-numero to ror-num-ordine.
           move low-values to ror-num-riga.
           start rordini key is >= ror-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rordini  next at end exit perform end-read
                    if tor-anno   not = ror-anno    or
                       tor-numero not = ror-num-ordine
                       exit perform
                    end-if
                    move 0                     to link-impegnato
                    move user-link             to link-user
                    move ror-prg-chiave        to link-key
                    evaluate true
                    when ElaboraGiacenza
                         |AGISCO SULLA GIACENZA (NORMALE E BLOCCATA)
                         move "1000000000000010"  to link-array
                    when ElaboraImpegnato
                         |AGISCO SULL'IMPEGNATO
                         move "0100000000000000"  to link-array
                    end-evaluate
                    move ror-qta               to link-valore
                    move tor-causale           to link-causale
                    move ror-prg-chiave        to link-key
                    set link-update            to true
                    set link-open-with-lock    to true
                    set link-update-um         to true
                    set link-update-peso       to false
                    set link-update-valore     to false
                    if prima-volta
                       set prima-volta to false
                       close progmag |wprogmag aprirà con lock il file!!!
                    end-if
                    call "wprogmag" using link-wprogmag
                    add 1 to tot-call
                    set trovato     to true
                    if link-wprogmag-status = -1
                       set errori to true
                       exit perform
                    end-if
                 end-perform
           end-start.
                 
      ***---     
       ELABORA-MOVIMENTI-DI-MAGAZZINO.
      *****     perform ACCEPT-FORMAT-TIME.
      *****     display "[RICALDIN-P]: Elaborazione righe "
      *****             "MOVMAG con WPROGMAG at: ", como-ora-edit 
      *****        upon syserr.
           move 0 to counter counter2.
           |RIPULISCO LA SCREEN DAL CONTATORE
           display 78-clear
              upon link-handle at column 01
                                    line 03.
           ||||||||
           set tutto-ok   to true.
           set trovato    to false.
           move low-value to tmo-rec.
           |Non importa anche se diventa 20051032. L'importante è che
           |la start successiva si posizioni sul giorno superiore al 31
           add 1 to link-data.
           move link-data  to tmo-data-movim.
           start tmovmag key is > k-data
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tmovmag  next at end exit perform end-read
   
                    add 1 to counter
                    add 1 to counter2
                    if counter2 = 100
                       if counter = 100
                          display "RICALDIN ON TMOVMAG"
                             upon link-handle at column 18,00
                                                   line  3,00
                       end-if
                       move counter to counter-edit
                       display counter-edit
                          upon link-handle at column 34,00
                                                line  3,00
                       move 0 to counter2
                    end-if
   
                    if tmo-data-movim <= link-data
                       exit perform
                    end-if
                    perform LOOP-RIGHE-MAGAZZINO
                 end-perform
           end-start.
   
      ***---
       LOOP-RIGHE-MAGAZZINO.
           move tmo-anno   to rmo-anno.
           move tmo-numero to rmo-movim.
           move low-value  to rmo-riga.
           start rmovmag key is >= rmo-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rmovmag next at end exit perform end-read
                    if rmo-anno   not =  tmo-anno or
                       rmo-movim  not =  tmo-numero              
                       exit perform
                    end-if
                    move user-link           to link-user
                    move rmo-chiave-progmag  to link-key
                    |AGISCO SULLA GIACENZA (NORMALE E BLOCCATA)
                    move "1000000000000010"  to link-array
                    move rmo-qta             to link-valore
                    move rmo-causale         to link-causale
                    move rmo-chiave-progmag  to link-key
                    set  link-update         to true
                    set  link-open-with-lock to true
                    set  link-update-um      to true
                    set  link-update-peso    to false
                    set  link-update-valore  to false
                    if prima-volta
                       set prima-volta to false
                       close progmag |wprogmag aprirà con lock il file!!!
                    end-if
                    call "wprogmag" using link-wprogmag
                    set trovato     to true
                    if link-wprogmag-status = -1
                       set errori to true
                       exit perform
                    end-if
                 end-perform
           end-start.

      ***---
       ELABORA-ORDINATO-ORDF.
      *****     perform ACCEPT-FORMAT-TIME.
      *****     display "[RICALDIN-P]: Elaborazione righe "
      *****             "MOVMAG con WPROGMAG at: ", como-ora-edit 
      *****        upon syserr.
           move 0 to counter counter2.
           |RIPULISCO LA SCREEN DAL CONTATORE
           display 78-clear
              upon link-handle at column 01
                                    line 03.
           ||||||||
           set tutto-ok   to true.
           set trovato    to false.
           move low-value to tof-rec.
           set tof-inserito to true.
           start tordforn key is > tof-k-stato
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tordforn next at end exit perform end-read
                    move tof-rec to como-rec
                 
                    add 1 to counter
                    add 1 to counter2
                    if counter2 = 100
                       if counter = 100
                          display "RICALDIN ON TORDFORN"
                             upon link-handle at column 18,00
                                                   line  3,00
                       end-if
                       move counter to counter-edit
                       display counter-edit
                          upon link-handle at column 34,00
                                                line  3,00
                       move 0 to counter2
                    end-if
                 
                    |Gli ordini chiusi manualmente e quelli inseriti
                    |non vanno proprio considerati
                    if tof-chiusura-man or tof-inserito
                       continue
                    else
                       perform READ-TORDFORN-LOCK
                       if errori
                          exit perform
                       end-if

                       move tof-mese-rif to old-mese
                       accept como-data from century-date
                       if como-data >= tof-data-consegna
                          move 1 to tof-mese-rif
                       else
                          if tof-urgente
                             move 1 to tof-mese-rif
                          else
                             move tof-data-consegna(5:2)to mese-consegna
                             move como-data(5:2)        to mese-oggi
                             |Data di consegna nell'anno successivo: il mese
                             |è di valore minore, ma la data INTERA no
                             if mese-oggi > mese-consegna
                                add 12 to mese-consegna
                             end-if
                             compute tof-mese-rif = 
                                   ( mese-consegna - mese-oggi ) + 1
                          end-if
                          
      *****                    compute como-data-1 =
      *****                            function INTEGER-OF-DATE(como-data)
      *****                    compute como-data-2 =
      *****                       function INTEGER-OF-DATE(tof-data-consegna)
      *****                    compute diff-giorni =
      *****                            como-data-2 - como-data-1
      *****                    if diff-giorni <= 30
      *****                       move 1 to tof-mese-rif
      *****                    else
      *****                       move 0 to resto
      *****                       divide diff-giorni by 30 
      *****                              giving tof-mese-rif
      *****                            remainder resto
      *****                       if resto not = 0
      *****                          add 1 to tof-mese-rif
      *****                       end-if
      *****                    end-if
                       end-if
                       rewrite tof-rec invalid continue end-rewrite
                       unlock tordforn all records
                       perform LOOP-RIGHE-RORDFORN
                       perform AGGIORNA-STATO-ORDF
                       move como-rec to tof-rec
                       start tordforn key > tof-k-stato
                             invalid  exit perform
                       end-start
                    end-if
                 end-perform
           end-start.

      ***---
       LOOP-RIGHE-RORDFORN.
           move tof-anno   to rof-anno.
           move tof-numero to rof-numero.
           move low-value  to rof-riga.
           start rordforn key is >= rof-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rordforn next at end exit perform end-read
                    if rof-chiave-testa not = tof-chiave
                       exit perform
                    end-if
                    |1. AGISCO SULL'ORDINATO col mese attuale
                    |(l'ordinato è già stato azzerato in precedenza)
                    move "0010000000000000"  to link-array
                    move tof-mese-rif        to link-mese-rif
                    move tof-chiave          to link-chiave-origine
                    move tof-causale         to link-causale
                    move rof-prg-chiave      to link-key
                    move rof-qta-ord         to link-valore
                    set  link-update         to true
                    set  link-open-with-lock to true
                    set  link-update-um      to true
                    set  link-update-peso    to false
                    set  link-update-valore  to false
                    set  link-chiamata-batch to true
                    if prima-volta
                       set prima-volta to false
                       close progmag |wprogmag aprirà con lock il file!!!
                    end-if
                    call "wprogmag" using link-wprogmag
                    set trovato     to true
                    if link-wprogmag-status = -1
                       set errori to true
                       exit perform
                    end-if
                    perform READ-RORDFORN-LOCK
                    if tutto-ok
                       move 0 to tof-pz-arrivati
                       set tof-inevaso to true
      
                       |2. Controllo se ci evasioni relativi
                       |alla riga con qta da evadere ed aggiorno
                       |la qta evasa come somme delle evasioni
                       move 0 to rof-qta-evasa
                       move rof-chiave to reva-chiave-ordf
                       start reva key >= reva-chiave-ordf
                             invalid continue
                         not invalid
                             perform until 1 = 2
                                read reva next 
                                     at end exit perform 
                                end-read
                                if reva-chiave-ordf not = rof-chiave
                                   exit perform
                                end-if
                                add reva-qta to rof-qta-evasa
                             end-perform
                       end-start

                       if rof-qta-evasa > 0
                          if rof-qta-evasa < rof-qta-ord
                             move rof-qta-evasa to link-valore
                          else
                             move rof-qta-ord to link-valore
                          end-if
                          |Storno l'ordinato col mese attuale
                          |per la quantità evasa
                          move "0000000000000000"  to link-array
                          move -1                  to multiplyer(3)
                          move tof-mese-rif        to link-mese-rif
                          move tof-chiave         to link-chiave-origine
                          move tof-causale         to link-causale
                          move rof-prg-chiave      to link-key
                          set  link-update         to true
                          set  link-open-with-lock to true
                          set  link-update-um      to true
                          set  link-update-peso    to false
                          set  link-update-valore  to false
                          set  link-chiamata-batch to true
                          if prima-volta
                             set prima-volta to false
                             close progmag |wprogmag aprirà con lock il file!!!
                          end-if
                          call "wprogmag" using link-wprogmag
                          set trovato     to true

                          if link-wprogmag-status = -1
                             set errori to true
                             exit perform
                          end-if
                          rewrite rof-rec invalid continue end-rewrite
                          unlock rordforn all records
                       end-if
                    end-if
                 end-perform
           end-start.

      ***---
       READ-TORDFORN-LOCK.
           set tutto-ok  to true.
           set RecLocked to false.
           initialize geslock-linkage.
           move "tordforn" to geslock-nome-file.

           set tutto-ok to true.
           read tordforn lock key tof-k-stato invalid continue end-read.

           perform until 1 = 2
              if not RecLocked
                 exit perform
              end-if
              set RecLocked to false
              move 1 to geslock-v-riprova
              move 0 to geslock-v-ignora
              move 0 to geslock-v-termina
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova 
                   read tordforn lock key tof-k-stato 
                        invalid continue 
                   end-read
              when other continue
              end-evaluate
           end-perform.

      ***---
       READ-RORDFORN-LOCK.
           set tutto-ok  to true.
           set RecLocked to false.
           initialize geslock-linkage.
           move "rordforn" to geslock-nome-file.

           set tutto-ok to true.
           read rordforn lock key rof-chiave invalid continue end-read.

           perform until 1 = 2
              if not RecLocked
                 exit perform
              end-if
              set RecLocked to false
              move 1 to geslock-v-riprova
              move 0 to geslock-v-ignora
              move 0 to geslock-v-termina
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova 
                   read rordforn lock key rof-chiave
                        invalid continue 
                   end-read
              when other continue
              end-evaluate
           end-perform.

      ***---
       ELABORA-ORDINATO-EVASIONI-F.
      *****     perform ACCEPT-FORMAT-TIME.
      *****     display "[RICALDIN-P]: Elaborazione righe "
      *****             "MOVMAG con WPROGMAG at: ", como-ora-edit 
      *****        upon syserr.
           move 0 to counter counter2.
           |RIPULISCO LA SCREEN DAL CONTATORE
           display 78-clear
              upon link-handle at column 01
                                    line 03.
           ||||||||
           set tutto-ok   to true.
           set trovato    to false.
           move low-value to teva-rec.
           set teva-aperta to true.
           start teva key is > teva-stato
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read teva next at end exit perform end-read

                    add 1 to counter
                    add 1 to counter2
                    if counter2 = 100
                       if counter = 100
                          display "RICALDIN ON TORDFORN"
                             upon link-handle at column 18,00
                                                   line  3,00
                       end-if
                       move counter to counter-edit
                       display counter-edit
                          upon link-handle at column 34,00
                                                line  3,00
                       move 0 to counter2
                    end-if

                    if teva-chiusa
                       exit perform
                    end-if
                    perform LOOP-RIGHE-REVA
                 end-perform
           end-start.

      ***---
       LOOP-RIGHE-REVA.
           move teva-anno   to reva-anno.
           move teva-numero to reva-numero.
           move low-value   to reva-riga.
           start reva key is >= reva-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read reva next at end exit perform end-read
                    if reva-chiave-testa not = teva-chiave
                       exit perform
                    end-if                
                    move reva-chiave-testa-ordf to tof-chiave
                    read tordforn no lock invalid continue end-read
                    |1. AGISCO SULLA GIACENZA (NORMALE E BLOCCATA)
                    move "1000000000000010"  to link-array
                    move reva-qta            to link-valore
                    move tof-causale         to link-causale
                    move reva-chiave-progmag to link-key
                    set  link-update         to true
                    set  link-open-with-lock to true
                    set  link-update-um      to true
                    set  link-update-peso    to false
                    set  link-update-valore  to false
                    set  link-chiamata-batch to true
                    if prima-volta
                       set prima-volta to false
                       close progmag |wprogmag aprirà con lock il file!!!
                    end-if
                    call "wprogmag" using link-wprogmag
                    set trovato     to true
                    if link-wprogmag-status = -1
                       set errori to true
                       exit perform
                    end-if
                 end-perform
           end-start.

      ***--
       CLOSE-FILES.
           close tmovmag  rmovmag ttipocli tmarche timposte
                 tordini  rordini tparamge
                 tnotacr  rnotacr
                 tordforn rordforn tscorte articoli
                 teva     reva tcaumag |evaclides
                 mtordini mrordini
                 clienti destini tpromo param.

      ********---
      ***** ACCEPT-FORMAT-TIME.
      *****     accept como-ora from time.
      *****     initialize como-ora-edit.
      *****     string como-ora(1:2) delimited size
      *****            ":"           delimited size
      *****            como-ora(3:2) delimited size
      *****            "'"           delimited size
      *****            como-ora(5:2) delimited size
      *****            x"22"         delimited size
      *****            into como-ora-edit
      *****     end-string.

      ***---
       EXIT-PGM.
      *****     perform ACCEPT-FORMAT-TIME.
      *****     display "[RICALDIN-P]: GoBack at: ", como-ora-edit
      *****        upon syserr.
      *****     move como-ora(1:2) to hh.
      *****     move como-ora(3:2) to mm.
      *****     move como-ora(5:2) to ss.
      *****     compute end-secondi = ( hh * 3600 ) + ( mm * 60   ) + ss.
      *****     compute tot-secondi = start-secondi - end-secondi
      *****     if tot-secondi < 60
      *****        move tot-secondi to ss
      *****        display "[RICALDIN-P]: 5555ìramma eseguito in: ",
      *****                ss, " secondi"
      *****           upon syserr
      *****     else
      *****        divide tot-secondi by 60 giving mm remainder ss
      *****        display "[RICALDIN-P]: Programma eseguito in: ",
      *****                mm, " minuti e ", ss, " secondi"
      *****           upon syserr
      *****     end-if.
           goback.

      ***---
       PARAGRAFO-COPY.
           copy "aggiorna-stato-ordf.cpy".
      *****     copy "aggiornaX-stato-master.cpy".
           copy "direziona-impegnato-common.cpy".
           copy "trova-parametro.cpy".
