       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      sost-batt.
       AUTHOR.                          Andrea.
       REMARKS. Sostituzione articoli. Vengono sostituiti gli articoli
                per la sola quantità inevasa. In caso essa sia 0 la 
                riga viene cancellata, altrimenti la qta ordinata sarà 
                portata alla stregua di quella evasa
                01072020:
                provo ad evadere prima di tutto l'articolo inserito
                poi se non riesco con la catena reimposto quello inserito
                L'unica differenza rispetto alla sotituzione articoli
                è il parametro che viene preso da gparam e non dal cliente
                Inotlre non invia mai mail ed è sempre automatico
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "promoeva.sl".
           copy "articoli.sl". 
           copy "progmag.sl".
           copy "clienti.sl".
           copy "destini.sl".
           copy "mtordini.sl".
           copy "mrordini.sl". 
      *****     copy "timballi.sl".
      *****     copy "timbalqta.sl".
           copy "battsost.sl".
           copy "tcaumag.sl".
           copy "ttipocli.sl".
           copy "timposte.sl".
           copy "tmarche.sl".
           copy "tpiombo.sl".
           copy "tmagaz.sl".   
           copy "listini.sl".
           copy "param.sl".
           copy "tcontat.sl".    

       SELECT logfile
           ASSIGN       TO path-log
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE  IS SEQUENTIAL
           FILE STATUS  IS STATUS-logfile.


      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "promoeva.fd".
           copy "articoli.fd". 
           copy "progmag.fd".
           copy "clienti.fd".
           copy "destini.fd".
           copy "mtordini.fd".
           copy "mrordini.fd".
      *****     copy "timbalqta.fd".
      *****     copy "timballi.fd".
           copy "battsost.fd".
           copy "tcaumag.fd".
           copy "ttipocli.fd".
           copy "timposte.fd".
           copy "tmarche.fd".
           copy "tpiombo.fd".
           copy "tmagaz.fd".
           copy "listini.fd".
           copy "param.fd".
           copy "tcontat.fd".    

       FD  logfile.
       01 riga-log        PIC  x(900).

       WORKING-STORAGE SECTION.
           copy "link-geslock.def".
           copy "link-wprogmag.def".
           copy "link-find-progr.def".
           copy "imposte.def".
           copy "trova-parametro.def". 
           
       01  filler           pic 9.
           88 RichiamoSchedulato    value 1, false 0.

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

       77  como-riga        pic x(200).
       77  status-logfile   pic xx.
       77  path-log         pic x(256).
       77  status-promoeva      pic xx.
       77  status-articoli      pic xx.
       77  status-progmag       pic xx.
       77  status-clienti       pic xx.
       77  status-destini       pic xx.
       77  status-mtordini      pic xx.
       77  status-mrordini      pic xx.
      ***** 77  status-timballi   pic xx.
      ***** 77  status-timbalqta  pic xx.
       77  status-battsost        pic xx.
       77  status-tcaumag       pic xx.
       77  status-ttipocli      pic xx.
       77  status-timposte      pic xx.
       77  status-tmarche       pic xx.
       77  status-tpiombo       pic xx.
       77  status-tmagaz        pic xx.
       77  status-listini       pic xx.
       77  status-tcontat       pic xx.
       77  status-param         pic xx.

       78  titolo value "Sostituzione automatica articoli".

       77  scelta             pic 9.
       77  ultimo-disponibile pic 9(6).
       77  righe-trattate     pic 9(5).
       77  como-articolo      pic 9(6).
       77  master-articolo    pic 9(6).
       77  idx                pic 9(5).
       77  idx-orig           pic s9(5).
       77  idx-dest           pic 9(5).
       77  resto              pic 9(3).
       77  imballi            pic 9(8).
       77  giacenza           pic s9(8).
       77  impegnato          pic s9(8).
       77  disponibilita      pic s9(8).
       77  giac-utile         pic s9(8).
       77  qta                pic s9(8).
      **** 77  giac-assoluta      pic 9(8).
       77  save-giacenza      pic s9(8).
       77  tot-righe          pic 9(5).
       77  save-qta           pic 9(8).
       77  qta-reale          pic 9(8).
       77  QtaImballiOrdine   pic 9(4).
       77  DesImballiOrdine   pic x(30).

       01  sost-prg-chiave.
         05 sost-cod-articolo      pic 9(6).
         05 sost-prg-cod-magazzino pic x(3).
         05 sost-tipo-imballo      pic x(3).
         05 sost-prg-peso          pic 9(5)v999.

       01  init-prg-chiave.
         05 init-cod-articolo      pic 9(6).
         05 init-prg-cod-magazzino pic x(3).
         05 init-tipo-imballo      pic x(3).
         05 init-prg-peso          pic 9(5)v999.
       77  init-qta                pic 9(8).
       77  init-qta-e              pic 9(8).

       01 save-chiave.
         05 save-testa.
           10 save-anno      pic 9(4).
           10 save-numero    pic 9(8).
         05 save-riga        pic 9(8).

       01  filler            pic 9.
           88 riga-omaggio   value 1, false 0.

       01  controlli         pic xx.
           88 tutto-ok       value "OK".
           88 errori         value "ER".

       01  filler            pic 9 value 0.
           88 RecLocked      value 1 false 0.

       01  filler            pic 9.
           88 record-ok      value 1 false 0.

       01  filler            pic 9.
           88 trovato        value 1 false 0.

       01  filler            pic 9.
           88 trovato-mag    value 1 false 0.

       01  filler            pic 9.
           88 no-prg-listino value 0.
           88 si-prg-listino value 1.

       01 GdoInUsoFlag       pic x.
           88 GdoInUso       value "S". 
           88 GdoNonInUso    value " ". 
       01  filler            pic 9.
           88 primo-giro-ultimo value 1 false zero.

       77  como-data         pic 9(8).
       77  como-ora          pic 9(8).
       77  sost-articoli     pic x.     
       77  batch-notturno    pic x value space.
      ***** 77  como-scorta       pic 9(5).

       LINKAGE SECTION.
       copy "link-sost-art.def".

      ******************************************************************
       PROCEDURE DIVISION using sost-art-linkage.

       DECLARATIVES. 
       MTORDINI-ERR SECTION.
           use after error procedure on mtordini.
           set tutto-ok  to true.
           evaluate status-mtordini
           when "35"
                set errori to true
                if RichiamoSchedulato
                   move "File [MTORDINI] not found!" to como-riga
                   perform SCRIVI-RIGA-LOG
                else
                   display message "File [MTORDINI] not found!"
                             title titolo
                              icon 3
                end-if
           when "39"
                set errori to true
                if RichiamoSchedulato
                   move "File [MTORDINI] mismatch size!" to como-riga
                   perform SCRIVI-RIGA-LOG
                else
                   display message "File [MTORDINI] mismatch size!"
                             title titolo
                              icon 3
                end-if
           when "98"
                set errori to true
                if RichiamoSchedulato
                   move "[MTORDINI] Indexed file corrupt!" to como-riga
                   perform SCRIVI-RIGA-LOG
                else
                   display message "[MTORDINI] Indexed file corrupt!"
                             title titolo
                              icon 3
                end-if
           when "93"
           when "99"
                set RecLocked to true
           end-evaluate.

      ***---
       MRORDINI-ERR SECTION.
           use after error procedure on mrordini.
           set tutto-ok  to true.
           evaluate status-mrordini   
           when "35"
                set errori to true
                if RichiamoSchedulato
                   move "File [MRORDINI] not found!" to como-riga
                   perform SCRIVI-RIGA-LOG
                else
                   display message "File [MRORDINI] not found!"
                             title titolo
                              icon 3
                end-if
           when "39"
                set errori to true
                if RichiamoSchedulato
                   move "File [MRORDINI] mismatch size!" to como-riga
                   perform SCRIVI-RIGA-LOG
                else
                   display message "File [MRORDINI] mismatch size!"
                             title titolo
                              icon 3
                end-if
           when "98"
                set errori to true
                if RichiamoSchedulato
                   move "[MRORDINI] Indexed file corrupt!" to como-riga
                   perform SCRIVI-RIGA-LOG
                else
                   display message "[MRORDINI] Indexed file corrupt!"
                             title titolo
                              icon 3
                end-if
           when "93"
           when "99"
                set RecLocked to true
           end-evaluate.

      ***---
       PROGMAG-ERR SECTION.
           use after error procedure on progmag.
           set tutto-ok  to true.
           evaluate status-progmag
           when "35"
                set errori to true
                if RichiamoSchedulato
                   move "File [PROGMAG] not found!" to como-riga
                   perform SCRIVI-RIGA-LOG
                else
                   display message "File [PROGMAG] not found!"
                             title titolo
                              icon 3
                end-if
           when "39"
                set errori to true
                if RichiamoSchedulato
                   move "File [PROGMAG] mismatch size!" to como-riga
                   perform SCRIVI-RIGA-LOG
                else
                   display message "File [PROGMAG] mismatch size!"
                             title titolo
                              icon 3
                end-if
           when "98"
                set errori to true
                if RichiamoSchedulato
                   move "[PROGMAG] Indexed file corrupt!" to como-riga
                   perform SCRIVI-RIGA-LOG
                else
                   display message "[PROGMAG] Indexed file corrupt!"
                             title titolo
                              icon 3
                end-if
           when "93"
           when "99"
                set RecLocked to true
           end-evaluate.

      ***---
       ARTICOLI-ERR SECTION.
           use after error procedure on articoli.
           set tutto-ok  to true.
           evaluate status-articoli 
           when "35"
                set errori to true
                if RichiamoSchedulato
                   move "File [ARTICOLI] not found!" to como-riga
                   perform SCRIVI-RIGA-LOG
                else
                   display message "File [ARTICOLI] not found!"
                             title titolo
                              icon 3
                end-if
           when "39"
                set errori to true
                if RichiamoSchedulato
                   move "File [ARTICOLI] mismatch size!" to como-riga
                   perform SCRIVI-RIGA-LOG
                else
                   display message "File [ARTICOLI] mismatch size!"
                             title titolo
                              icon 3
                end-if
           when "98"
                set errori to true
                if RichiamoSchedulato
                   move "[ARTICOLI] Indexed file corrupt!" to como-riga
                   perform SCRIVI-RIGA-LOG
                else
                   display message "[ARTICOLI] Indexed file corrupt!"
                             title titolo
                              icon 3
                end-if
           when "93"
           when "99"
                set RecLocked to true
           end-evaluate.

      ***---
       CLIENTI-ERR SECTION.
           use after error procedure on clienti.
           set tutto-ok  to true.
           evaluate status-clienti
           when "35"                  
                set errori to true
                if RichiamoSchedulato
                   move "File [CLIENTI] not found!" to como-riga
                   perform SCRIVI-RIGA-LOG
                else
                   display message "File [CLIENTI] not found!"
                             title titolo
                              icon 3
                end-if
           when "39"
                set errori to true
                if RichiamoSchedulato
                   move "File [cLIENTI] mismatch size!" to como-riga
                   perform SCRIVI-RIGA-LOG
                else
                   display message "File [CLIENTI] mismatch size!"
                             title titolo
                              icon 3
                end-if
           when "98"
                set errori to true
                if RichiamoSchedulato
                   move "[CLIENTI] Indexed file corrupt!" to como-riga
                   perform SCRIVI-RIGA-LOG
                else
                   display message "[CLIENTI] Indexed file corrupt!"
                             title titolo
                              icon 3
                end-if
           when "93"
           when "99"
                set RecLocked to true
           end-evaluate.

      ***---
       DESTINI-ERR SECTION.
           use after error procedure on destini.
           set tutto-ok  to true.
           evaluate status-destini    
           when "35"
                set errori to true
                if RichiamoSchedulato
                   move "File [DESTINI] not found!" to como-riga
                   perform SCRIVI-RIGA-LOG
                else
                   display message "File [DESTINI] not found!"
                             title titolo
                              icon 3
                end-if
           when "39"
                set errori to true
                if RichiamoSchedulato
                   move "File [DESTINI] mismatch size!" to como-riga
                   perform SCRIVI-RIGA-LOG
                else
                   display message "File [DESTINI] mismatch size!"
                             title titolo
                              icon 3
                end-if
           when "98"
                set errori to true
                if RichiamoSchedulato
                   move "[DESTINI] Indexed file corrupt!" to como-riga
                   perform SCRIVI-RIGA-LOG
                else
                   display message "[DESTINI] Indexed file corrupt!"
                             title titolo
                              icon 3
                end-if
           when "93"
           when "99"
                set RecLocked to true
           end-evaluate.

      ***---
       battsost-ERR SECTION.
           use after error procedure on battsost.
           set tutto-ok  to true.
           evaluate status-battsost  
           when "35"
                set errori to true
                if RichiamoSchedulato
                   move "File [battsost] not found!" to como-riga
                   perform SCRIVI-RIGA-LOG
                else
                   display message "File [battsost] not found!"
                             title titolo
                              icon 3
                end-if
           when "39"
                set errori to true
                if RichiamoSchedulato
                   move "File [battsost] mismatch size!" to como-riga
                   perform SCRIVI-RIGA-LOG
                else
                   display message "File [battsost] mismatch size!"
                             title titolo
                              icon 3
                end-if
           when "98"
                set errori to true
                if RichiamoSchedulato
                   move "[battsost] Indexed file corrupt!" to como-riga
                   perform SCRIVI-RIGA-LOG
                else
                   display message "[battsost] Indexed file corrupt!"
                             title titolo
                              icon 3
                end-if
           when "93"
           when "99"
                set RecLocked to true
           end-evaluate.          

      ***---
       TMAGAZ-ERR SECTION.
           use after error procedure on tmagaz.
           set tutto-ok  to true.
           evaluate status-tmagaz  
           when "35"
                set errori to true
                if RichiamoSchedulato
                   move "File [TMAGAZ] not found!" to como-riga
                   perform SCRIVI-RIGA-LOG
                else
                   display message "File [TMAGAZ] not found!"
                             title titolo
                              icon 3
                end-if
           when "39"
                set errori to true
                if RichiamoSchedulato
                   move "File [TMAGAZ] mismatch size!" to como-riga
                   perform SCRIVI-RIGA-LOG
                else
                   display message "File [TMAGAZ] mismatch size!"
                             title titolo
                              icon 3
                end-if
           when "98"
                set errori to true
                if RichiamoSchedulato
                   move "[TMAGAZ] Indexed file corrupt!" to como-riga
                   perform SCRIVI-RIGA-LOG
                else
                   display message "[TMAGAZ] Indexed file corrupt!"
                             title titolo
                              icon 3
                end-if
           when "93"
           when "99"
                set RecLocked to true
           end-evaluate.

      ***---
       PROMOEVA-ERR SECTION.
           use after error procedure on promoeva.
           set tutto-ok  to true.
           evaluate status-promoeva   
           when "35"
                set errori to true
                if RichiamoSchedulato
                   move "File [PROMOEVA] not found!" to como-riga
                   perform SCRIVI-RIGA-LOG
                else
                   display message "File [PROMOEVA] not found!"
                             title titolo
                              icon 3
                end-if
           when "39"
                set errori to true
                if RichiamoSchedulato
                   move "File [PROMOEVA] mismatch size!" to como-riga
                   perform SCRIVI-RIGA-LOG
                else
                   display message "File [PROMOEVA] mismatch size!"
                             title titolo
                              icon 3
                end-if
           when "98"
                set errori to true
                if RichiamoSchedulato
                   move "[PROMOEVA] Indexed file corrupt!" to como-riga
                   perform SCRIVI-RIGA-LOG
                else
                   display message "[PROMOEVA] Indexed file corrupt!"
                             title titolo
                              icon 3
                end-if
           when "93"
           when "99"
                set RecLocked to true
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
           |Settata da scheduler, serve per non dare messaggi a video
           accept batch-notturno from environment "BATCH_NOTTURNO".                                               
           set tutto-ok to true.
           accept como-data from century-date
           accept como-ora  from time.
           if sost-art-batch and batch-notturno = "S"
              set RichiamoSchedulato to true
           end-if.

      ***---
       OPEN-FILES.             
           if RichiamoSchedulato
              move sost-art-log to path-log
              open extend logfile
           end-if.
           open input mtordini clienti battsost tcaumag timposte tcontat
                      destini progmag ttipocli |timbalqta  timballi 
                      tpiombo tmarche tmagaz listini promoeva param.
           open i-o mrordini articoli.
           if RecLocked
              set errori to true
           end-if.             

      ***---
       ELABORAZIONE.
           set tutto-ok  to true.
           set RecLocked to false.
           initialize geslock-linkage.
           move "mtordini" to geslock-nome-file.
           move sost-art-chiave to mto-chiave. 

           read mtordini no lock key mto-chiave.
           move mto-cod-cli     to como-prm-cliente.
           move mto-prg-destino to como-prm-destino.
           perform TROVA-PARAMETRO.
           if prm-sost-batt-no
              exit paragraph
           end-if.
                             
           set tutto-ok   to true.
           read mtordini lock key mto-chiave invalid continue end-read.
      
           perform until 1 = 2
              if not RecLocked
                 exit perform
              end-if
              initialize geslock-messaggio
              string "L'ordine master anno: " mto-anno " n. " mto-numero
              x"0d0a""Risulta bloccato su un altro terminale."
              x"0d0a""Sarà impossibile aggiornarne lo stato."
              delimited size
                 into geslock-messaggio
              end-string
              set RecLocked to false
              if RichiamoSchedulato
                 set ignora to true
                 exit perform
              else
                 move 1 to geslock-v-riprova
                 move 1 to geslock-v-ignora
                 move 0 to geslock-v-termina
                 call   "geslock" using geslock-linkage
                 cancel "geslock"
                 evaluate true
                 when riprova 
                      read mtordini lock key mto-chiave
                           invalid continue 
                      end-read
                 when ignora exit perform
                 end-evaluate
              end-if
           end-perform.
      
           if not RecLocked
              set  cli-tipo-C  to true
              move mto-cod-cli to cli-codice
              read clienti no lock
                   invalid continue
               not invalid
                   move cli-tipo to tcl-codice
                   read ttipocli no lock invalid continue end-read
              end-read

              move mto-causale to tca-codice
              read tcaumag no lock
                   invalid display message mto-chiave
               not invalid
                   perform RICERCA-PREVENTIVA-SOSTITUZIONE
                   if trovato       

                      if RichiamoSchedulato
                         move "SOSTITUZIONE TROVATA" to como-riga
                         perform SCRIVI-RIGA-LOG
                      end-if

                      perform LOOP-RIGHE-SOST
                   end-if
                   unlock mtordini all records
              end-read
           end-if.

      ***---
       RICERCA-PREVENTIVA-SOSTITUZIONE.
           if RichiamoSchedulato
              move "RICERCA PREVENTIVA SOSTITUZIONE" to como-riga
              perform SCRIVI-RIGA-LOG
           end-if.
           set trovato to false.
           move low-value  to mro-chiave.
           move mto-chiave to mro-chiave-testa.
           start mrordini key >= mro-chiave
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
                       if mro-qta > mro-qta-e and not mro-si-blister
                          if mto-prg-destino = 0
                             move cli-tipo-art to des-tipo-art
                          else
                             move mto-cod-cli     to des-codice
                             move mto-prg-destino to des-prog
                             read destini no lock 
                                  invalid continue 
                             end-read
                          end-if
                          move mro-cod-articolo to art-codice
                          read articoli no lock 
                               invalid continue 
                          end-read
                          set record-ok to true      
                          if record-ok
                             move mro-cod-articolo to como-articolo
                             perform TROVA-PRINCIPALE
                             
                             if record-ok
                                perform TROVA-ULTIMO-DISPONIBILE
                                if ultimo-disponibile = 0
                                   set record-ok to false
                                end-if
                             end-if

                             if record-ok   
                                set trovato to true
                                exit perform
                             end-if
                          end-if
                       end-if
                    end-if
                 end-perform
           end-start.

      ***---
       LOOP-RIGHE-SOST.       
           if RichiamoSchedulato
              move "INIZIO SOSTITUZIONE..." to como-riga
              perform SCRIVI-RIGA-LOG
           end-if

           initialize fp-linkage.
           move mto-chiave to fp-chiave.
           call   "find-progr" using fp-linkage.
           cancel "find-progr".

           if fp-tot-righe = 0
              move 1 to fp-tot-righe
           end-if.
           move fp-tot-righe to tot-righe.
           move 0 to righe-trattate.

           move low-value  to mro-chiave.
           move mto-chiave to mro-chiave-testa.
           start mrordini key >= mro-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read mrordini next at end exit perform end-read
                    if mro-chiave-testa not = mto-chiave
                       exit perform
                    end-if
                    if righe-trattate = tot-righe
                       exit perform
                    end-if   
                    add 1 to righe-trattate
                    if mro-chiuso                

                       if RichiamoSchedulato
                          initialize como-riga
                          string "RIGA #" delimited size
                                 mro-riga delimited size
                                 " - ARTICOLO: "  delimited size
                                 mro-cod-articolo delimited size
                                 " - CHIUSA"      delimited size
                            into como-riga
                          end-string
                          perform SCRIVI-RIGA-LOG
                       end-if

                       continue

                    else
                       if mro-cod-iva = "E15" or mro-si-omaggio or
                          mro-prz-unitario = 0
                          set riga-omaggio to true
                       else
                          set riga-omaggio to false
                       end-if
                       move mro-chiave to save-chiave
                       if mro-qta > mro-qta-e and not mro-si-blister
                          if mto-prg-destino = 0
                             move cli-tipo-art to des-tipo-art
                          else
                             move mto-cod-cli     to des-codice
                             move mto-prg-destino to des-prog
                             read destini no lock 
                                  invalid continue 
                             end-read
                          end-if
                          move mro-cod-articolo to art-codice
                          read articoli no lock 
                               invalid continue 
                          end-read                          
                          set record-ok to true
      *****                 evaluate true  also true
      *****                 when des-tipo-art-diretti    also art-si-diretti
      *****                      set record-ok  to true
      *****                 when des-tipo-art-gruppi     also art-si-gruppi
      *****                      set record-ok  to true
      *****                 when des-tipo-art-specialist 
      *****                      also art-si-specialist
      *****                      set record-ok  to true
      *****                 when des-tipo-art-eg         also art-si-eg
      *****                      set record-ok  to true
      *****                 when des-tipo-art-gda        also art-si-gda
      *****                      set record-ok  to true
      *****                 when des-tipo-art-gds        also art-si-gds
      *****                      set record-ok  to true
      *****                 when des-tipo-art-estero     also art-si-estero
      *****                      set record-ok  to true
      *****                 end-evaluate
                          if record-ok
                             move mro-cod-articolo to bts-codice
                             move 0                to bts-princ
                             start battsost key >= bts-chiave
                                   invalid set record-ok to false
                               not invalid
                                   read battsost next
                                   if bts-codice not = mro-cod-articolo
                                      set record-ok to false
                                   end-if
                             end-start
                             if record-ok
                                if bts-princ not = 0
                                   move bts-princ to bts-codice
                                   move 0         to bts-princ
                                   read battsost no lock 
                                end-if

                                perform TROVA-ULTIMO-DISPONIBILE
                                if ultimo-disponibile = 0
                                   set record-ok to false
                                end-if
                             end-if
   
                             if record-ok
                                move 0              to save-qta
                                move mro-qta        to init-qta
                                move mro-qta-e      to init-qta-e
                                compute qta = mro-qta - mro-qta-e
                                if mro-qta-e not = 0
                                   move mro-qta-e to  mro-qta
                                   compute mro-num-colli =
                                           mro-qta / mro-qta-imballi
                                   rewrite mro-rec
                                end-if
                                move mro-prg-chiave  to init-prg-chiave
                                move mro-des-imballo to DesImballiOrdine
                                move mro-qta-imballi to QtaImballiOrdine

                                perform ESAURIMENTO-SCORTA                             
                                move low-value to mro-riga
                                start mrordini key >= mro-chiave
                                      invalid continue
                                  not invalid
                                      perform until 1 = 2
                                         read mrordini next
                                              at end  exit perform
                                         end-read
                                         if mro-chiave = save-chiave
                                            move mro-qta  to qta-reale
                                            move init-qta to mro-qta
                                            perform STORNA-IMPEGNATO
                                            if save-qta = 0
                                               if mro-qta-e not = 0
                                                  move qta-reale
                                                    to mro-qta
                                                  perform 
                                                    AGGIUNGI-IMPEGNATO
                                               else
                                                  delete mrordini record
                                               end-if
                                            else
                                               compute mro-qta =
                                                       mro-qta-e + 
                                                       save-qta
                                               compute mro-num-colli =
                                                       mro-qta / 
                                                       mro-qta-imballi
                                               rewrite mro-rec
                                              perform AGGIUNGI-IMPEGNATO
                                            end-if
                                            if RichiamoSchedulato
                                               move 
                                               "SOSTITUZIONE EFFETTUATA"
                                                 to como-riga
                                               perform SCRIVI-RIGA-LOG
                                            end-if
                                            exit perform
                                         end-if
                                      end-perform
                                end-start
                             end-if
                          end-if
                       else          

                          if RichiamoSchedulato
                             initialize como-riga
                             string "RIGA #"         delimited size
                                    mro-riga         delimited size
                                    " - ARTICOLO: "  delimited size
                                    mro-cod-articolo delimited size
                                    " - EVASA"       delimited size
                               into como-riga
                             end-string
                             perform SCRIVI-RIGA-LOG
                          end-if

                       end-if
                    end-if
                 end-perform
           end-start.

      ***--- 
       ESAURIMENTO-SCORTA.
           move mro-cod-articolo to master-articolo.
           set primo-giro-ultimo to true.
           perform varying idx from 0 by 1 
                     until idx > 1000 
              if qta <= 0
                 exit perform
              end-if
              |01072020: L'ultimo dev'essere quello inserito
              if not primo-giro-ultimo
                 move master-articolo 
                   to ultimo-disponibile como-articolo
              else
                 |01072020: Provo prima il codice inserito nel master
                 if idx = 0
                    move mro-cod-articolo to como-articolo
                 else
                    if idx = 1
                       move bts-codice to como-articolo
                    else
                       if bts-collegato(idx - 1) = 0
                          exit perform
                       end-if
                       move bts-collegato(idx - 1) to como-articolo
                    end-if
                 end-if
              end-if         
              |01072020: L'ultimo dev'essere quello inserito
              move como-articolo to art-codice
              if ultimo-disponibile not = master-articolo
                 if primo-giro-ultimo
                    if como-articolo = ultimo-disponibile
                       set primo-giro-ultimo to false
                       move master-articolo to ultimo-disponibile
                    end-if
                 end-if
              end-if

              read articoli no lock
              if art-bloccato
                 continue
              else
                 initialize prg-chiave replacing numeric data by zeroes
                                            alphanumeric data by spaces
                 initialize sost-prg-chiave 
                                        replacing numeric data by zeros
                                            alphanumeric data by spaces
                 move tca-cod-magaz to prg-cod-magazzino
                 move como-articolo to prg-cod-articolo
                 move 0 to giacenza
                 move 0 to giac-utile
                 move 0 to impegnato
                 move 0 to disponibilita
                 move 0 to save-giacenza
                 set trovato-mag to false
                 perform TROVA-LISTINO
                 if no-prg-listino
                    start progmag key >= prg-chiave
                          invalid continue
                      not invalid
                          perform until 1 = 2
                             read progmag next 
                                  at end exit perform 
                             end-read
                             if prg-cod-articolo  not = como-articolo or
                                prg-cod-magazzino not = tca-cod-magaz
                                exit perform
                             end-if
                             if prg-attivo
                                if sost-cod-articolo = 0
                                   move prg-chiave to sost-prg-chiave   
                                end-if
                                set trovato-mag to true
      *****                          |assegno il progressivo con maggior
      *****                          |giacenza a valore assoluto
      *****                          move prg-giacenza to giac-assoluta
      *****                          if giac-assoluta > save-giacenza
      *****                             move giac-assoluta to save-giacenza
      *****                             move prg-chiave    to sost-prg-chiave
      *****                          end-if
                                |assegno il progressivo con > giacenza
                                move prg-giacenza to save-giacenza
                                if prg-giacenza > save-giacenza
                                   move prg-giacenza to save-giacenza
                                   move prg-chiave   to sost-prg-chiave
                                end-if
                             end-if
                          end-perform
                    end-start
                 end-if
                 if trovato-mag
      *****              |RICHIESTA DI WALTER (19/02/10): UTILIZZARE I VALORI DEL PADRE
      *****              initialize prg-chiave 
      *****                         replacing numeric data by zeroes
      *****                              alphanumeric data by spaces
      *****              move como-articolo to prg-cod-articolo
      *****              read progmag no lock invalid continue end-read
      *****              move prg-giacenza  to giacenza
      *****              move prg-impegnato to impegnato
                    
                    |RICHIESTA DI WALTER (22/06/10): UTILIZZARE I VALORI 
                    |IN BASE AL PARAMETRO NEL MAGAZZINO
                    move 0 to giacenza impegnato
                    move 0 to giac-utile
                    initialize prg-chiave 
                               replacing numeric data by zeroes
                                    alphanumeric data by spaces
                    move como-articolo to prg-cod-articolo
                    start progmag key >= prg-chiave 
                          invalid continue 
                    end-start
                    perform until 1 = 2
                       read progmag next at end exit perform end-read
                       if prg-cod-articolo not = como-articolo
                          exit perform
                       end-if
                       move prg-cod-magazzino to mag-codice
                       read tmagaz no lock 
                            |RECORD PADRE
                            invalid continue
                        not invalid
                            if mag-sostituzione-si
                               if giac-utile = 0
                                  move como-articolo to pev-articolo
                                  move 0 to pev-prog
                                  start promoeva key >= pev-chiave
                                        invalid continue
                                    not invalid
                                        perform until 1 = 2
                                           read promoeva next at end
                                                exit perform
                                           end-read
                                           if pev-articolo not = 
                                              como-articolo
                                              exit perform
                                           end-if
                                           add pev-giac-utile 
                                               to giac-utile
                                        end-perform
                                  end-start
                               end-if
                               add prg-giacenza  to giacenza
                               add prg-impegnato to impegnato
                            end-if
                       end-read
                    end-perform

                    if prg-giacenza < QtaImballiOrdine
                       move 0 to prg-giacenza
                    end-if

                    if sost-cod-articolo = mro-prg-cod-articolo
                       compute impegnato = 
                               impegnato - init-qta + init-qta-e
                    end-if
                    compute disponibilita = 
                            giacenza - impegnato - giac-utile
                    |Se sono sull'ultimo assegno la qta restante
                    if como-articolo = ultimo-disponibile
                       move qta to disponibilita giacenza
                    end-if
                    if disponibilita > 0 and giacenza > 0
                       move sost-prg-chiave to prg-chiave
                       read progmag   no lock invalid continue end-read
      *****              move prg-tipo-imballo to imq-codice
      *****              read timbalqta no lock invalid continue end-read
                       |Se la disponibilita copre PER INTERO
                       if disponibilita >= qta
                          move qta to disponibilita
                       end-if
                       move 0 to imballi
                       if disponibilita >= QtaImballiOrdine
                          move 0 to resto
                          divide disponibilita by QtaImballiOrdine
                                 giving imballi
                              remainder resto
                          if resto not = 0
                             add 1 to imballi
                          end-if
                       else
                          if como-articolo = ultimo-disponibile
                             move QtaImballiOrdine to disponibilita
                             move 1                to imballi
                          end-if
                       end-if
                       compute disponibilita =
                               QtaImballiOrdine * imballi
                       if disponibilita > 0
                          compute qta = qta - disponibilita
                          if como-articolo not = init-cod-articolo
                             perform APPLICA-SOSTITUZIONE
                          else
                             move disponibilita to save-qta
                          end-if
                       end-if
                    end-if
                 end-if
              end-if
           end-perform.
           
      ***---
       TROVA-LISTINO.
           set no-prg-listino to true.
      *****     if cli-gdo = space
           if tcl-gdo-no
              exit paragraph
           end-if.

           move cli-gdo          to lst-gdo
           move mto-data-ordine  to lst-data
           move como-articolo    to lst-articolo
           start listini key <= lst-k-articolo
                 invalid continue
              not invalid
                 read listini previous
                 if lst-gdo      = cli-gdo          and
                    lst-data    <= mto-data-ordine  and
                    lst-articolo = como-articolo

                    if lst-prg-cod-articolo = space
                       move 0 to lst-prg-cod-articolo
                    end-if

                    if lst-prg-cod-articolo not = 0
                       set si-prg-listino   to true
                    end-if
                       
                 end-if
           end-start.

           if si-prg-listino
              move lst-prg-chiave  to prg-chiave
              read progmag no lock
                   invalid set no-prg-listino to true
              end-read

              if not prg-attivo
                 set no-prg-listino to true
              end-if

              if tca-cod-magaz not = prg-cod-magazzino
                 set no-prg-listino to true
              end-if
           end-if.

           if si-prg-listino
              move lst-prg-chiave  to sost-prg-chiave
              set  trovato-mag     to true
           end-if.

      ***---
       TROVA-PRINCIPALE.
           set record-ok to true.
           move como-articolo to bts-codice.
           move 0 to bts-princ.
           start battsost  key >= bts-chiave
                 invalid set record-ok to false
             not invalid
                 read battsost next
                 if bts-codice not = como-articolo
                    set record-ok to false
                 end-if
           end-start
           if record-ok
              if bts-princ not = 0
                 move bts-princ to bts-codice
                 move 0         to bts-princ
                 read battsost no lock 
              end-if
           end-if.

      ***---
       TROVA-ULTIMO-DISPONIBILE.
           move 0 to ultimo-disponibile.
           perform varying idx from 1 by 1 
                     until idx > 1000
              if idx = 1
                 move bts-codice to como-articolo
              else
                 if bts-collegato(idx - 1) = 0
                    exit perform
                 end-if
                 move bts-collegato(idx - 1) to como-articolo
              end-if
              move como-articolo to art-codice
              read articoli no lock
              if art-bloccato
                 continue
              else
                 initialize prg-chiave replacing numeric data by zeroes
                                            alphanumeric data by spaces
                 move tca-cod-magaz to prg-cod-magazzino
                 move como-articolo to prg-cod-articolo
                 start progmag key >= prg-chiave
                       invalid continue
                   not invalid
                       perform until 1 = 2
                          read progmag next at end exit perform end-read
                          if prg-cod-articolo  not = como-articolo or
                             prg-cod-magazzino not = tca-cod-magaz
                             exit perform
                          end-if
                          if prg-attivo
                             move como-articolo to ultimo-disponibile
                             exit perform
                          end-if
                       end-perform
                 end-start
              end-if
           end-perform.

      ***---
       APPLICA-SOSTITUZIONE.
           initialize fp-linkage.
           move mto-chiave to fp-chiave.
           call   "find-progr" using fp-linkage.
           cancel "find-progr".

           move fp-riga  to mro-riga.
           move fp-progr to mro-progr.
      *****     move sost-cod-articolo  to mro-cod-articolo.
           move sost-prg-chiave      to mro-prg-chiave.
           move mro-prg-cod-articolo to mro-cod-articolo.
           move disponibilita        to mro-qta.
           move 0                    to mro-qta-e.
           move spaces to mro-utente-ultima-modifica.
           move 0      to mro-data-ultima-modifica.
           move 0      to mro-ora-ultima-modifica.
           move prg-peso-utf     to mro-peso-utf.
           move prg-peso-non-utf to mro-peso-non-utf.
      *****     move prg-tipo-imballo to imq-codice.

           if riga-omaggio
              set mro-si-omaggio to true
              move 0 to mro-imponib-merce
                        mro-prz-unitario
                        mro-imp-cou-cobat
                        mro-imp-consumo
                        mro-add-piombo
           else
              set mro-no-omaggio to true
              move cli-tipo to tcl-codice 
              read ttipocli no lock invalid continue end-read
              move sost-art-anno to con-anno
              read tcontat no lock 
              evaluate tcl-serie-bolle
              when 1 move con-ult-stampa-bolle-gdo to imp-data
              when 2 move con-ult-stampa-bolle-mv  to imp-data
              when 3 move con-ult-stampa-bolle-at  to imp-data
              end-evaluate

              start timposte key <= imp-chiave
                    invalid continue
                not invalid
                    read timposte previous
              end-start

              if ttipocli-gdo
                 set TrattamentoGDO to true
              else
                 move art-marca-prodotto to mar-codice
                 read tmarche no lock
              end-if

              perform CALCOLA-IMPOSTE

              move imposta-consumo to mro-imp-consumo
              move imposta-cou     to mro-imp-cou-cobat

              move 0 to add-piombo
              if tcl-si-piombo and art-si-cobat
                 move mto-cod-cli        to como-prm-cliente
                 move mto-prg-destino    to como-prm-destino
                 move mro-prz-unitario   to como-prz-unitario
                 move imposta-cobat      to mro-imp-cou-cobat
                 move art-marca-prodotto to tpb-marca
                 move mto-data-ordine    to como-data-ordine tpb-data
                 perform ADDIZIONALE-PIOMBO
              end-if
                        
              move add-piombo      to mro-add-piombo
              evaluate true
              when ttipocli-gdo
                   compute mro-imponib-merce = 
                           mro-prz-unitario  - 
                           mro-imp-cou-cobat - 
                           mro-imp-consumo   -
                           mro-add-piombo
              when other
                   if mro-prz-unitario >= 9999999,99
                      move 9999999,99 to mro-imponib-merce
                   else
                      compute mro-imponib-merce = 
                              mro-prz-unitario  
                   end-if
              end-evaluate
           end-if.

      *****     read timbalqta no lock invalid continue end-read.
      *****     move imq-tipo  to imb-codice.
      *****     read timballi  no lock invalid continue end-read.
           move DesImballiOrdine to mro-des-imballo.
           move QtaImballiOrdine to mro-qta-imballi.
           compute mro-num-colli = mro-qta / mro-qta-imballi.

           accept mro-data-creazione from century-date.
           accept mro-ora-creazione  from time.
           move   sost-art-user to mro-utente-creazione.
           move 0 to mro-data-ultima-modifica  mro-ora-ultima-modifica.
           move spaces to mro-utente-ultima-modifica.
           write mro-rec invalid rewrite mro-rec end-write.
                                           
           perform AGGIUNGI-IMPEGNATO.     

      ***---
       STORNA-IMPEGNATO.
           |Storno la quantità dall'impegnato
           initialize link-wprogmag.
           set link-update      to true.
           move init-prg-chiave to link-key.
           move mto-causale     to link-causale.
           move init-qta        to link-valore.
           compute link-impegnato = init-qta - init-qta-e.
           perform VALORIZZA-ARRAY-CAUSALI.
           if tca-si-stampa
              move -1                to multiplyer(2)
           else             
              move -1                to multiplyer(1)
              move -1                to multiplyer(15)
           end-if.
           move sost-art-user to link-user.
           call   "wprogmag" using link-wprogmag.
           cancel "wprogmag".

      ***---
       AGGIUNGI-IMPEGNATO.
           initialize link-wprogmag.
           set link-update      to true.
           move mro-prg-chiave  to link-key.
           move mto-causale     to link-causale.
           move mro-qta         to link-valore.
           compute link-impegnato = mro-qta - mro-qta-e.

           perform VALORIZZA-ARRAY-CAUSALI.
           move sost-art-user   to link-user.
           call   "wprogmag" using link-wprogmag. 
           cancel "wprogmag".

      ***---
       VALORIZZA-ARRAY-CAUSALI.
           set link-update-um      to true.
           set link-update-peso    to false.
           set link-update-valore  to false.
           move "0000000000000000" to link-array.
           move link-causale       to tca-codice.
           read tcaumag no lock invalid continue end-read.
           |In caso sia permessa la stampa della bolla
           |agisco sull'impegnato, altrimenti l'ordine
           |è da considerarsi già bollettato ed agisco
           |direttamente sulla giacenza e non sull'impegnato
           if tca-si-stampa 
              move 1 to multiplyer(2)
              perform DIREZIONA-IMPEGNATO
           else
              move 1 to multiplyer(1)
              move 1 to multiplyer(15)
           end-if.

      ***---
       CLOSE-FILES.
           close progmag articoli clienti destini ttipocli tcontat
                 mtordini mrordini battsost tcaumag tpiombo |timbalqta timballi
                 timposte tmarche tmagaz listini promoeva param.
           if RichiamoSchedulato
              close logfile
           end-if.

      ***---
       EXIT-PGM.
           goback.                    

      ***---
       SCRIVI-RIGA-LOG.             
           initialize riga-log.
           perform SETTA-INIZIO-RIGA.
           string r-inizio  delimited size
                  como-riga delimited size
                  into riga-log
           end-string.
           write riga-log.

      ***---
       SETTA-INIZIO-RIGA.
           accept como-ora  from time.
           accept como-data from century-date.

           move como-data(3:2) to r-aa.
           move como-data(5:2) to r-mm.
           move como-data(7:2) to r-gg.

           move como-ora(1:2) to r-hh.
           move como-ora(3:2) to r-min.
           move como-ora(5:2) to r-sec.

           
      ***---
       PARAGRAFO-COPY.
           copy "direziona-impegnato-common.cpy".
           copy "imposte.cpy".
           copy "addizionale-piombo.cpy".
           copy "trova-parametro.cpy".
