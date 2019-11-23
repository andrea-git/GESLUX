       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      agfatt-p.
       AUTHOR.                          Andrea.
       REMARKS. Assegnazione stesso numero di lotto, numero di fattura
                progressivo (da tabella TCONTAT) e data digitata (da
                linkage) ai documenti aventi il flag "fatt-si-prenotata" 
                impostato a true e gli altri dati di fatturazione
                non valorizzati.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tordini.sl". 
           copy "tnotacr.sl". 
           copy "tcontat.sl".
           copy "tcaumag.sl".
           copy "rordini.sl".
           copy "tpromo.sl".
           copy "clienti.sl".
           copy "destini.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tordini.fd".
           copy "tnotacr.fd".
           copy "tcontat.fd".
           copy "tcaumag.fd".
           copy "rordini.fd".
           copy "tpromo.fd".
           copy "clienti.fd".
           copy "destini.fd".

       WORKING-STORAGE SECTION.
      * COPY
           copy "link-geslock.def".

      * COSTANTI
       78  titolo                value "Assegnazione n. di fattura".

      * FILE STATUS
       77  status-tordini        pic xx.
       77  status-rordini        pic xx.
       77  status-tnotacr        pic xx.
       77  status-tcontat        pic xx.
       77  status-tcaumag        pic xx.
       77  status-tpromo         pic xx.
       77  status-clienti        pic xx.
       77  status-destini        pic xx.

      * FLAGS
       77  FlagRecordOk          pic 9.
           88  RecordOk          value 1, false 0.
       77  FlagTrovato           pic 9.
           88  trovato           value 1, false 0.
       77  FlagTrovatoLotto      pic 9.
           88  trovato-lotto     value 1, false 0.
       77  FlagTrovataFatt       pic 9.
           88  trovato-fatt      value 1, false 0.
       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".
       77  filler                pic 9.
           88 RecLocked          value 1 false 0.
LUBEXX 77  filler                pic 9.
LUBEXX     88 ScrittoComunque    value 1 false 0.

      * VARIABILI       
       77  wstampa               pic x(256).
       77  como-data             pic 9(8).
       77  como-ora              pic 9(8).
       77  DocCounter            pic 9(8).
       77  save-anno-fattura     pic 9(4).
       77  save-data-fattura     pic 9(8).
       77  save-num-fattura      pic 9(8).
       77  save-num-prenot       pic 9(8).
       77  save-fatt-prenotata   pic x.
       77  save-chiave           pic 9(12).

       77  tor-numero-edit       pic z(8).
       77  counter               pic 9(10).
       77  counter2              pic 9(10).
       77  counter-edit          pic z(10).

       LINKAGE SECTION.
       copy "link-agfatt-p.def".

      ******************************************************************
       PROCEDURE DIVISION using agfatt-p-linkage.

       DECLARATIVES.
       TORDINI SECTION.
           use after error procedure on tordini.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tordini
           when "39"
                set errori to true
                display message "File [TORDINI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TORDINI] Indexed file corrupt!"
                          title titolo
                           icon 3  
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File delle testate [TORDINI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

       TNOTACR SECTION.
           use after error procedure on tnotacr.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tnotacr
           when "39"
                set errori to true
                display message "File [TNOTACR] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TNOTACR] Indexed file corrupt!"
                          title titolo
                           icon 3 
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File delle testate [TNOTACR] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

       TCONTAT SECTION.
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
                  x"0d0a""File dei contatori [TCONTAT] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

       TCAUMAG-ERR SECTION.
           use after error procedure on tcaumag.
           set tutto-ok  to true.
           evaluate status-tcaumag
           when "39"
                set errori to true
                display message "File [TCAUMAG] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TCAUMAG] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File causali [TCAUMAG] inesistente"
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
           perform OPEN-INPUT-FILES.
           perform TROVA-LOTTO.
           perform CLOSE-FILES.

           display "                                                   "
              upon link-handle at column 01 line 03.

           if tutto-ok
              perform OPEN-FILES
              if tutto-ok
                 if LinkNote
                    perform PRE-ELABORAZIONE-TNOTACR
                    if tutto-ok
                       perform ELABORAZIONE-TNOTACR
                    end-if
                 else                               
                    perform PRE-ELABORAZIONE-TORDINI
                    if tutto-ok
                       perform ELABORAZIONE-TORDINI
                    end-if
                 end-if
              end-if
              perform EXIT-PGM
           else
              goback
           end-if.

      ***---
       INIT.
LUBEXX     set ScrittoComunque to false.
           move 0 to tot-fat, postel-fat tot-doc.
           set tutto-ok  to true.

      ***---
       OPEN-INPUT-FILES.
           open input tordini.
           open input tnotacr.
           open input tcontat.
           open input tpromo.
           open input clienti.
           open input destini.

      ***--
       CLOSE-FILES.
           close tordini tnotacr tcontat tpromo clienti destini.

      ***---
       OPEN-FILES.
           open input tcaumag.
           if tutto-ok
              perform OPEN-IO-TCONTAT
              if tutto-ok
                 if LinkNote perform OPEN-IO-TNOTACR
                 else        perform OPEN-IO-TORDINI
                 end-if
                 if errori 
                    close tcontat 
                    close tcaumag
                 end-if
              else
                 close tcaumag
              end-if
           end-if.

           if errori goback end-if.

      ***---
       OPEN-IO-TORDINI. 
           move "tordini" to geslock-nome-file.
           string   "Il file delle testate degli ordini" 
             x"0d0a""risulta in uso su altro terminale."
             x"0d0a""Questo comporta l'impossibilità ad"
             x"0d0a""aggiornare le bolle come fatturate." delimited size
                 into geslock-messaggio
           end-string.

           set tutto-ok  to true.
           open i-o tordini.
           if RecLocked
              move 1 to geslock-v-termina
              move 1 to geslock-v-riprova
              move 0 to geslock-v-ignora
              call   "geslock" using geslock-linkage
              cancel "geslock"
              
              evaluate true
              when riprova perform OPEN-IO-TORDINI
              when other   display message box "Operazione interrotta!"
                                   title titolo
                                   icon 2
              end-evaluate
           end-if.                 

      ***---
       OPEN-IO-TNOTACR.                        
           move "tnotacr" to geslock-nome-file.
           string   "Il file delle testate N.C. / F.M." 
             x"0d0a""risulta in uso su altro terminale."
             x"0d0a""Questo comporta l'impossibilità ad"
             x"0d0a""aggiornare le bolle come fatturate." delimited size
                 into geslock-messaggio
           end-string.

           set tutto-ok  to true.
           open i-o tnotacr.
           if RecLocked
              move 1 to geslock-v-termina
              move 1 to geslock-v-riprova
              move 0 to geslock-v-ignora
              call   "geslock" using geslock-linkage
              cancel "geslock"
              
              evaluate true
              when riprova perform OPEN-IO-TNOTACR
              when other   display message box "Operazione interrotta!"
                                   title titolo
                                   icon 2
              end-evaluate
           end-if.
        
      ***---
       OPEN-IO-TCONTAT.                        
           move "tcontat" to geslock-nome-file.
           string     "La tabella dei contatori risulta"
             x"0D0A", "in uso su altro terminale."
             x"0D0A", "Questo comporta che essa non"
             x"0D0A", "potrà essere aggiornata." delimited size
                 into geslock-messaggio
           end-string.

           set tutto-ok  to true.
           open i-o tcontat allowing readers.
           if RecLocked
              move 1 to geslock-v-termina
              move 1 to geslock-v-riprova
              move 0 to geslock-v-ignora
              call   "geslock" using geslock-linkage
              cancel "geslock"
              
              evaluate true
              when riprova perform OPEN-IO-TCONTAT
              when other   display message box "Operazione interrotta!"
                                   title titolo
                                   icon 2
              end-evaluate
           end-if.
      
      ***---
      *Questa fase effettua un controllo per vedere se
      *sono già stati assegnati in precedenza lo stesso
      *numero di lotto/fattura sia n TORDINI che in TNOTACR
      * infatti  il contatore è il medesimo
       TROVA-LOTTO.
LUBEXX     move 0 to counter, counter2.
           set trovato-lotto to false.
           move LinkAnno to con-anno.

           read tcontat  no lock invalid continue end-read.

           add 1 to con-ult-num-pren. 
           set tutto-ok to true.
           move low-value to tor-rec.
           |CERCO IL NUMERO DI LOTTO ALL'INTERNO DEL FILE TORDINI
           move con-ult-num-pren      to tor-num-prenot.
           move con-anno              to tor-anno-fattura.

LUBEXX*****Utilizzo della chiave appropriata per questo pgm
LUBEXX     start tordini key is >= k-agfatt
LUBEXX*****           start tordini key is >= k4 |KEY FATTURA
                 invalid continue
             not invalid
                 perform until 1 = 2

LUBEXX              add 1 to counter
LUBEXX              add 1 to counter2
LUBEXX              if counter2 = 200
LUBEXX                 if counter = 200
LUBEXX                    display "CHECK LOTTO FATT" 
LUBEXX                       upon link-handle at column 03
LUBEXX                                             line 03
LUBEXX                 end-if
LUBEXX                 move counter to counter-edit
LUBEXX                 display counter-edit
LUBEXX                    upon link-handle at column 18,00
LUBEXX                                          line 03,00
LUBEXX                 move 0 to counter2
LUBEXX              end-if

                    read tordini next no lock at end exit perform 
                    end-read
                    evaluate true
                    when tor-num-prenot   = con-ult-num-pren and
                         tor-anno-fattura = con-anno
                         set trovato-lotto to true
                         exit perform
                    when tor-num-prenot > con-ult-num-pren
                         exit perform
                    end-evaluate
                 end-perform
           end-start.

           if not trovato-lotto 
              set tutto-ok to true
              move low-value to tno-rec
              |CERCO IL NUMERO DI LOTTO ALL'INTERNO DEL FILE TNOTACR
              move con-ult-num-pren      to tno-num-prenot
              move con-anno              to tno-anno-fattura
LUBEXX        move 0 to counter, counter2

LUBEXX*****Utilizzo della chiave appropriata per questo pgm
LUBEXX        start tnotacr key is >= k-agfatt
LUBEXX*****           start tnotacr key is >= k4 |KEY FATTURA
                    invalid continue
                not invalid
                    perform until 1 = 2

LUBEXX                 add 1 to counter
LUBEXX                 add 1 to counter2
LUBEXX                 if counter2 = 200
LUBEXX                    if counter = 200
LUBEXX                       display "CHECK LOTTO N.C." 
LUBEXX                          upon link-handle at column 03
LUBEXX                                                line 03
LUBEXX                    end-if
LUBEXX                    move counter to counter-edit
LUBEXX                    display counter-edit
LUBEXX                       upon link-handle at column 18,00
LUBEXX                                             line 03,00
LUBEXX                    move 0 to counter2
LUBEXX                 end-if

                       read tnotacr next no lock at end exit perform 
                       end-read
                       evaluate true
                       when tno-num-prenot   = con-ult-num-pren and
                            tno-anno-fattura = con-anno
                            set trovato-lotto to true
                            exit perform
                       when tno-num-prenot > con-ult-num-pren
                            exit perform
                       end-evaluate
                    end-perform
              end-start
           end-if.       

           if trovato-lotto perform MSG-ERRORE-LOTTO end-if.

      ***---
       PRE-ELABORAZIONE-TORDINI.
           |RIPULISCO LA SCREEN DAL CONTATORE
           display "                      "
              upon link-handle at column 15
                                    line 03.
           ||||||||
           move LinkAnno to con-anno.
           read tcontat  no lock invalid continue end-read.
                                      
           add  1 to con-num-fatt.
           move 0 to DocCounter.
           set trovato-fatt  to false.
           move low-value    to tor-rec.
           set tor-fatt-si-prenotata to true
LUBEXX*****Utilizzo della chiave appropriata per questo pgm
LUBEXX     start tordini key is >= k-agfatt
LUBEXX*****           start tordini key is >= k4 |KEY FATTURA
                 invalid set errori to true
           end-start.

           if tutto-ok
              perform until 1 = 2
                 read tordini next with no lock
                      at end  exit perform
                 end-read

                 add 1 to counter
                 add 1 to counter2
                 if counter2 = 200
                    if counter = 200
                       display "PRE" 
                          upon link-handle at column 15
                                                line 03
                    end-if
                    move counter to counter-edit
                    display counter-edit
                       upon link-handle at column 18,00
                                             line 03,00
                    move 0 to counter2
                 end-if

                 if tor-anno-fattura = 0 and
                    tor-data-fattura = 0 and
                    tor-num-prenot   = 0
                    if tor-fatt-si-prenotata
                       |CONTO QUANTE SONO I DOCUMENTI DA AGGIORNARE
                       |POI RIPARTO COL CONTEGGIO E CONTROLLO CHE
                       |PER OGNUNA DI ESSE NON SIA GIA' PRESENTE
                       |QUELLA FATTURA ALL'INTERNO DEL FILE
                       if LinkMerce
                          if tor-ordine 
                             if tor-data-bolla <= LinkData
                                add 1 to DocCounter
                             end-if
                          end-if
                       else
                          if tor-fattura-manuale
                             add 1 to DocCounter
                          end-if
                       end-if
                    end-if
                 else
                    exit perform
                 end-if

              end-perform                 

              if DocCounter = 0 set errori to true end-if

              move 0 to counter counter2
              perform DocCounter times

                 add 1 to counter
                 add 1 to counter2
                 if counter2 = 200          
                    move counter to counter-edit
                    display counter-edit
                       upon link-handle at column 18,00
                                             line 03,00
                    move 0 to counter2
                 end-if

                 move con-anno     to tor-anno-fattura
                 move con-num-fatt to tor-num-fattura
                 read tordini no lock key is k-fattura
                      invalid continue
                  not invalid
                      set trovato-fatt to true
                      exit perform
                 end-read
                 add 1 to con-num-fatt
              end-perform

           end-if.
           
           if errori
              display message box "Nessun documento presente avente"
                                  " il criterio selezionato"
                      title = titolo
                      icon 2
           end-if.

           move con-num-fatt to tor-numero-edit.
           if trovato-fatt  perform MSG-ERRORE-FATT end-if.

      ***---
       ELABORAZIONE-TORDINI.
           move 0 to counter counter2.
           |RIPULISCO LA SCREEN DAL CONTATORE
           display "                      "
              upon link-handle at column 15
                                    line 03.
           ||||||||
           move LinkAnno to con-anno.

           read tcontat  no lock invalid continue end-read.

           add 1 to con-ult-num-pren.
           set tutto-ok   to true.
           set trovato    to false.
           move low-value to tor-rec.
           set tor-fatt-si-prenotata to true.

LUBEXX*****Utilizzo della chiave appropriata per questo pgm
LUBEXX     start tordini key is >= k-agfatt
LUBEXX*****           start tordini key is >= k4 |KEY FATTURA
                 invalid set errori to true
           end-start.

           if tutto-ok
              perform until 1 = 2
                 read tordini next with no lock
                      at end exit perform
                 end-read

                 add 1 to counter
                 add 1 to counter2
                 if counter2 = 200
                    if counter = 200
                       display "EFF" 
                          upon link-handle at column 15
                                                line 03
                    end-if       
                    move counter to counter-edit
                    display counter-edit
                       upon link-handle at column 18,00
                                             line 03,00
                    move 0 to counter2
                 end-if

                 if tor-ordine
                    if LinkMerce   set RecordOk to true
                    else           set RecordOk to false
                    end-if
                 else
                    if LinkManuali set RecordOk to true
                    else           set RecordOk to false
                    end-if
                 end-if        

                 if RecordOk   and
                    LinkMerce  and
                    tor-ordine and
                    tor-data-bolla > LinkData
                    set RecordOk to false
                 end-if

                 if tor-anno-fattura = 0 and
                    tor-data-fattura = 0 and
                    tor-num-prenot   = 0
                    if tor-fatt-si-prenotata                       
                       if RecordOk
                          set trovato to true
                          perform READ-RECORD-TORDINI-LOCK
                          if termina
                             exit perform
                          end-if 
                          if not RecLocked
                             perform ORDINE-IN-FATTURA
                             perform RIPOSIZIONA-CURSORE-TORDINI
                          end-if
                       end-if
                    end-if
                 else
                    exit perform
                 end-if

              end-perform

           end-if.

           if tot-fat not = 0
              move LinkData to con-ult-stampa-fatt

              accept con-data-ultima-modifica from century-date
              accept con-ora-ultima-modifica  from time
              move link-user   to con-utente-ultima-modifica

              rewrite con-rec invalid continue end-rewrite
              move con-num-fatt        to LastNumber
LUBEXX     else
LUBEXX        |A TRUE nel caso in cui ho solo un documento da elaborare
LUBEXX        |ma che non rientra nell'emissione fattura (es. ADET)
LUBEXX        |Se entra qui vuol dire che non ha aumentato il contatore
LUBEXX        |delle fattura, ma ha cmq assegnato il lotto
LUBEXX        if ScrittoComunque
LUBEXX           move LinkData to con-ult-stampa-fatt
LUBEXX           accept con-data-ultima-modifica from century-date
LUBEXX           accept con-ora-ultima-modifica  from time
LUBEXX           move link-user   to con-utente-ultima-modifica
LUBEXX           rewrite con-rec invalid continue end-rewrite
LUBEXX        end-if
           end-if.

      ***---
       PRE-ELABORAZIONE-TNOTACR.
           |RIPULISCO LA SCREEN DAL CONTATORE
           display "                      "
              upon link-handle at column 15
                                    line 03.
           ||||||||
           move LinkAnno to con-anno.
           read tcontat  no lock invalid continue end-read.
                                      
           add  1 to con-ult-num-nc-fisc.
           move 0 to DocCounter.
           set trovato-fatt  to false.
           move low-value to tno-rec 
           set tno-fatt-si-prenotata to true.

LUBEXX*****Utilizzo della chiave appropriata per questo pgm
LUBEXX     start tnotacr key is >= k-agfatt
LUBEXX*****           start tordini key is >= k4 |KEY FATTURA
                 invalid set errori to true
           end-start.

           if tutto-ok
              perform until 1 = 2
                 read tnotacr next with no lock
                      at end  exit perform
                 end-read 

                 add 1 to counter
                 add 1 to counter2
                 if counter2 = 200
                    if counter = 200
                       display "PRE" 
                          upon link-handle at column 15
                                                line 03
                    end-if                 
                    move counter to counter-edit
                    display counter-edit
                       upon link-handle at column 18,00
                                             line 03,00
                    move 0 to counter2
                 end-if
           
                 if tno-anno-fattura = 0 and
                    tno-data-fattura = 0 and
                    tno-num-prenot   = 0
                    if tno-fatt-si-prenotata
                       |CONTO QUANTE SONO I DOCUMENTI DA AGGIORNARE
                       |POI RIPARTO COL CONTEGGIO E CONTROLLO CHE
                       |PER OGNUNA DI ESSE NON SIA GIA' PRESENTE
                       |QUELLA FATTURA ALL'INTERNO DEL FILE
                       add 1 to DocCounter
                    end-if
                 else
                    exit perform
                 end-if

              end-perform                 

              if DocCounter = 0 set errori to true end-if

              perform DocCounter times

                 add 1 to counter
                 add 1 to counter2
                 if counter2 = 200          
                    move counter to counter-edit
                    display counter-edit
                       upon link-handle at column 18,00
                                             line 03,00
                    move 0 to counter2
                 end-if

                 move con-anno            to tno-anno-fattura
                 move con-ult-num-nc-fisc to tno-num-fattura
                 read tnotacr no lock key is k-fattura
                      invalid continue
                  not invalid
                      set trovato-fatt to true
                      exit perform
                 end-read
                 add 1 to con-num-fatt
              end-perform

           end-if.
           
           if errori
              display message box "Nessun documento presente avente"
                                  " il criterio selezionato"
                      title = titolo
                      icon 2
           end-if.
                                                   
           move con-ult-num-nc-fisc to tor-numero-edit.
           if trovato-fatt  perform MSG-ERRORE-FATT  end-if.

      ***---
       READ-RECORD-TORDINI-LOCK.
           set RecLocked to false.
           set tutto-ok to true.
           read tordini with lock key is k-agfatt
                invalid continue 
           end-read.
           if RecLocked
              initialize geslock-linkage
              move tor-num-bolla to tor-numero-edit
              string "La bolla n. " tor-numero-edit
              x"0d0a""risulta in uso su altro terminale."
              x"0d0a""Questo comporta l'impossibilità ad"
              x"0d0a""aggiornarla come fatturata." delimited size
                into geslock-messaggio
              end-string
              move "tordini" to geslock-nome-file
              move 1 to geslock-v-riprova
              move 1 to geslock-v-ignora
              move 1 to geslock-v-termina
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova perform READ-RECORD-TORDINI-LOCK
              when ignora  set errori to true
                           read tordini no lock key is k-agfatt
                                invalid continue
                           end-read
              when termina set errori to true
                           display message box "Operazione interrotta!"
                                   title titolo
                                   icon 2
              end-evaluate
           end-if.

      ***---
       RIPOSIZIONA-CURSORE-TORDINI.
           move save-anno-fattura   to tor-anno-fattura.
           move save-data-fattura   to tor-data-fattura.
           move save-num-fattura    to tor-num-fattura.
           move save-num-prenot     to tor-num-prenot.
           move save-fatt-prenotata to tor-fatt-prenotata.
           move save-chiave         to tor-chiave.

LUBEXX     start tordini key is >= k-agfatt 
                 invalid continue 
           end-start.

      ***---
       ORDINE-IN-FATTURA.
           move tor-anno-fattura   to save-anno-fattura.
           move tor-data-fattura   to save-data-fattura.
           move tor-num-fattura    to save-num-fattura.
           move tor-num-prenot     to save-num-prenot.
           move tor-fatt-prenotata to save-fatt-prenotata.
           move tor-chiave         to save-chiave.

LUBEXX     set ScrittoComunque     to true.
LUBEXX     add 1 to tot-doc.
           move con-anno           to tor-anno-fattura.
           move LinkData           to tor-data-fattura.
           move con-ult-num-pren   to tor-num-prenot.

           |Come da richiesta del Sig. Trivella in data 25/02 in quanto
           |le vendite dirette NON devono avere il numero di fattura
           |assegnato per non interferire con l'assegnazione del numero
           |di fattura che è progressivo
           move tor-causale to tca-codice.
           read tcaumag no lock invalid continue end-read.
           if tca-si-emissione
              add 1 to tot-fat
              add 1 to con-num-fatt
              move con-num-fatt to tor-num-fattura
              if tor-invio-postel 
                 add 1 to postel-fat 
              end-if
              if tor-invio-edi  and tca-causale-edi not = spaces
                 add 1 to edi-fat 
              end-if
           end-if.

           accept como-data from century-date.
           accept como-ora  from time.
           move como-data   to con-data-ultima-modifica.
           move como-ora    to con-ora-ultima-modifica.
           move link-user   to con-utente-ultima-modifica.

           rewrite tor-rec invalid continue end-rewrite
           unlock tordini all records.

           if FirstNumber = 0 and
LUBEXX        tca-si-emissione
              move con-num-fatt to FirstNumber
           end-if.

      ***---
       ELABORAZIONE-TNOTACR.
           move 0 to counter counter2.
           |RIPULISCO LA SCREEN DAL CONTATORE
           display "                      "
              upon link-handle at column 15
                                    line 03.
           ||||||||
           move LinkAnno to con-anno.

           read tcontat  no lock invalid continue end-read.

           add 1 to con-ult-num-pren.
           set tutto-ok   to true.
           set trovato    to false.
           move low-value to tno-rec.
           set tno-fatt-si-prenotata to true.

LUBEXX*****Utilizzo della chiave appropriata per questo pgm
LUBEXX     start tnotacr key is >= k-agfatt
                 invalid set errori to true
           end-start.

           if tutto-ok
              perform until 1 = 2
                 read tnotacr next with no lock
                      at end exit perform
                 end-read

                 add 1 to counter
                 add 1 to counter2
                 if counter2 = 200
                    if counter = 200
                       display "EFF" 
                          upon link-handle at column 15
                                                line 03
                    end-if       
                    move counter to counter-edit
                    display counter-edit
                       upon link-handle at column 18,00
                                             line 03,00
                    move 0 to counter2
                 end-if

                 if tno-anno-fattura = 0 and
                    tno-data-fattura = 0 and
                    tno-num-prenot   = 0
                    if tno-fatt-si-prenotata
                       set trovato to true
                       perform READ-RECORD-TNOTACR-LOCK
                       if termina
                          exit perform
                       end-if 
                       if not RecLocked
                          perform NC-IN-FATTURA
                          perform RIPOSIZIONA-CURSORE-TNOTACR
                       end-if
                    end-if
                 else
                    exit perform
                 end-if

              end-perform

           end-if.

           if tot-fat not = 0
              move LinkData to con-ult-stampa-nc

              accept con-data-ultima-modifica from century-date
              accept con-ora-ultima-modifica  from time
              move link-user   to con-utente-ultima-modifica

              rewrite con-rec invalid continue end-rewrite
              move con-ult-num-nc-fisc to LastNumber
              
LUBEXX     else
LUBEXX        |A TRUE nel caso in cui ho solo un documento da elaborare
LUBEXX        |ma che non rientra nell'emissione fattura (es. ADET)
LUBEXX        |Se entra qui vuol dire che non ha aumentato il contatore
LUBEXX        |delle fattura, ma ha cmq assegnato il lotto
LUBEXX        if ScrittoComunque
LUBEXX           move LinkData to con-ult-stampa-nc
LUBEXX           accept con-data-ultima-modifica from century-date
LUBEXX           accept con-ora-ultima-modifica  from time
LUBEXX           move link-user   to con-utente-ultima-modifica
LUBEXX           rewrite con-rec invalid continue end-rewrite
LUBEXX        end-if
           end-if.

      ***---
       READ-RECORD-TNOTACR-LOCK.
           set RecLocked to false.
           set tutto-ok to true.
           read tnotacr with lock key is k-agfatt
                invalid continue 
           end-read.

           if RecLocked
              initialize geslock-linkage
              move tno-numero to tor-numero-edit
              string "Il documento n. " tor-numero-edit
              x"0d0a""risulta in uso su altro terminale."
              x"0d0a""Questo comporta l'impossibilità ad"
              x"0d0a""aggiornarla come fatturata." delimited size
                into geslock-messaggio
              end-string
              move "tnotacr" to geslock-nome-file
              move 1 to geslock-v-riprova
              move 1 to geslock-v-ignora
              move 1 to geslock-v-termina
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova perform READ-RECORD-TNOTACR-LOCK
              when ignora  set errori to true
                           read tnotacr no lock key is k-agfatt
                                invalid continue
                           end-read
              when termina set errori to true
                           display message box "Operazione interrotta!"
                                   title titolo
                                   icon 2
              end-evaluate
           end-if.

      ***---
       RIPOSIZIONA-CURSORE-TNOTACR.
           move save-anno-fattura   to tno-anno-fattura.
           move save-data-fattura   to tno-data-fattura.
           move save-num-fattura    to tno-num-fattura.
           move save-num-prenot     to tno-num-prenot.
           move save-fatt-prenotata to tno-fatt-prenotata.
           move save-chiave         to tno-chiave.

LUBEXX     start tnotacr key is >= k-agfatt 
                 invalid continue 
           end-start.

      ***---
       NC-IN-FATTURA.
           move tno-anno-fattura   to save-anno-fattura.
           move tno-data-fattura   to save-data-fattura.
           move tno-num-fattura    to save-num-fattura.
           move tno-num-prenot     to save-num-prenot.
           move tno-fatt-prenotata to save-fatt-prenotata.
           move tno-chiave         to save-chiave.

LUBEXX     set ScrittoComunque     to true.
LUBEXX     add 1 to tot-doc.
           move con-anno           to tno-anno-fattura.
           move LinkData           to tno-data-fattura.
           move con-ult-num-pren   to tno-num-prenot.

           |Come da richiesta del Sig. Trivella in data 25/02 in quanto
           |le vendite dirette NON devono avere il numero di fattura
           |assegnato per non interferire con l'assegnazione del numero
           |di fattura che è progressivo
           move tno-causale to tca-codice.
           read tcaumag no lock invalid continue end-read.
           if tca-si-emissione
              add 1 to tot-fat
              add 1 to con-ult-num-nc-fisc
              move con-ult-num-nc-fisc to tno-num-fattura
              if tno-invio-postel 
                 add 1 to postel-fat 
              end-if
              if tno-invio-edi and tca-causale-edi not = spaces
                 add 1 to edi-fat 
              end-if
           end-if.

           accept como-data from century-date.
           accept como-ora  from time.
           move como-data   to con-data-ultima-modifica.
           move como-ora    to con-ora-ultima-modifica.
           move link-user   to con-utente-ultima-modifica.

           rewrite tno-rec invalid continue end-rewrite.
           unlock tnotacr all records.
           
           if FirstNumber = 0 and
LUBEXX        tca-si-emissione
              move con-ult-num-nc-fisc to FirstNumber 
           end-if.

      ***---
       MSG-ERRORE-LOTTO.
           set errori to true.
           move con-ult-num-pren to tor-numero-edit.
           initialize geslock-linkage
           string "GRAVE ERRORE!!"
           x"0d0a""---------------"
           x"0d0a""Lotto ", tor-numero-edit, " già assegnato!"
           x"0d0a""Prendere nota e contattare assistenza!" 
                  delimited size
                  into geslock-messaggio
           end-string.

           move 0 to geslock-v-riprova.
           move 0 to geslock-v-ignora.
           move 1 to geslock-v-termina.
           call   "geslock" using geslock-linkage.
           cancel "geslock".           

      ***---
       MSG-ERRORE-FATT.
           set errori to true.
           initialize geslock-linkage
           string "GRAVE ERRORE!!"
           x"0d0a""---------------"
           x"0d0a""Numero fattura ", tor-numero-edit, " già assegnato!"
           x"0d0a""Prendere nota e contattare assistenza!" 
                  delimited size
                  into geslock-messaggio
           end-string.

           move 0 to geslock-v-riprova.
           move 0 to geslock-v-ignora.
           move 1 to geslock-v-termina.
           call   "geslock" using geslock-linkage.
           cancel "geslock".

      ***---
       EXIT-PGM.
           close tcontat.

           if LinkNote unlock tnotacr all records
                       close  tnotacr
           else        unlock tordini all records
                       close  tordini
           end-if.

           unlock tcontat  all records.

           goback.

      ***---
       PARAGRAFO-COPY.
