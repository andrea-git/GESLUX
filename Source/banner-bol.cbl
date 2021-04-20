       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      banner-bol.
       AUTHOR.                          Luciano.
       REMARKS. programma banner di bollettazione.  Batch 
                schedulato sul server. 
                Il programma scorre tutti gli ordini master.
                per ogni singolo ordine viene richiamato l'aggiornamento
                specifico.
                Nel caso ci siano le condizioni scrive il messaggio nella
                tabella "tipoavv" per gli utenti abilitati.
                Nel log di SYSER metto il log generale. 
                nel log generale viene riportato ora di inzio singola
                importazione ed esito importazione.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tsetinvio.sl".
           copy "lineseq.sl".
           copy "mtordini.sl".
           copy "mrordini.sl".
           copy "progmag.sl".
           copy "ttipoavv.sl".
           copy "user.sl".
           copy "useravv.sl".
           copy "clienti.sl".
           copy "ttipocli.sl".

       select lineseq1
           assign       to  wstampa
           organization is line sequential
           access mode  is sequential
           file status  is status-lineseq.

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.         
           copy "tsetinvio.fd".
           copy "lineseq.fd".
           copy "mtordini.fd".
           copy "mrordini.fd".
           copy "progmag.fd".
           copy "ttipoavv.fd".
           copy "user.fd".
           copy "useravv.fd".
           copy "clienti.fd".
           copy "ttipocli.fd".

       FD  lineseq1.
       01 line-riga        PIC  x(900).

       WORKING-STORAGE SECTION.
           copy "acucobol.def".
           copy "mail.def".
           copy "link-geslock.def".

       77  como-user    pic x(10).      
       77  GdoInUsoFlag pic x.
        88 GdoInuso     value "S".
        88 GdoNonInuso  value " ".

       77  save-articolo pic 9(6).
       77  como-qta      pic s9(8).

       77  giacenza-LBX     pic s9(8) value 0.
       77  impegnato-LBX    pic s9(8) value 0.
       77  imp-MASTER-LBX   pic s9(8) value 0.
       77  imp-TRAD-LBX     pic s9(8) value 0.
       77  imp-GDO-LBX      pic s9(8) value 0.

       01              pic 9.
           88 batch value 1 false zero.

       01  filler         pic 9.
           88 messaggio   value 1 false zero.

       01  filler         pic 9.
           88 messaggio-T value 1 false zero.

       01  filler         pic 9.
           88 messaggio-G value 1 false zero.

       77 peso-ed          PIC  zz9,999.
       77 codice-ed        PIC  z(5).

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

       77  status-mtordini      pic xx.
       77  status-tsetinvio      pic xx.
       77  status-mrordini      pic xx.
       77  status-progmag    pic xx.
       77  status-lineseq       pic xx.

       77  status-ttipoavv   pic xx.
       77  status-user       pic xx.
       77  status-useravv    pic xx.
       77  status-clienti    pic xx.
       77  status-ttipocli   pic xx.


       77  RENAME-STATUS            pic 9(9)            comp-4.

       77  wstampa               pic x(256).
                                            
       77  FileDest              pic x(256).

       77  como-data             pic 9(8).
       77  como-ora              pic 9(8).
       77  como-riga             pic x(100).
       77  riga-stampa           pic x(100).

       77  tentativi             pic 99.

      * FLAGS
       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".
       77  filler                pic 9.
           88 RecLocked          value 1, false 0.
       77  filler                pic 9.
           88 trovato            value 1, false 0.

       77  stato                 pic 9.
           88 nessun-errore      value 1.
           88 ok-recupero        value 2.
           88 errore-ko          value 3.

       77  calling-program      pic x(20).

       78  titolo value "GESLUX - Banner Bollettazione".

       77  cont     pic 9(5).
       77  cont-ed  pic z(5).

       linkage section.
       77  Form-Handle USAGE IS HANDLE OF WINDOW.
       77  user-codi   pic x(10).

      ******************************************************************
       PROCEDURE DIVISION using Form-Handle, user-codi.

       DECLARATIVES.
      ***---
       MTORDINI-ERR SECTION.
           use after error procedure on mtordini.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-mtordini
           when "35"
                move "File [MTORDINI] inesistente" to como-riga

                if batch
                   perform SETTA-RIGA-STAMPA
                else
                   perform MESSAGE-BOX
                end-if
                set errori to true
           when "39"
                move "File [MTORDINI] mismatch size!" to como-riga
                if batch
                   perform SETTA-RIGA-STAMPA
                else
                   perform MESSAGE-BOX
                end-if

                set errori to true
           when "98"
                move "[MTORDINI] Indexed file corrupt!" to como-riga
                if batch
                   perform SETTA-RIGA-STAMPA
                else
                   perform MESSAGE-BOX
                end-if

                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

       MRORDINI-ERR SECTION.
           use after error procedure on mrordini.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-mrordini
           when "35"
                move "File [MRORDINI] inesistente" to como-riga

                if batch
                   perform SETTA-RIGA-STAMPA
                else
                   perform MESSAGE-BOX
                end-if
                set errori to true
           when "39"
                move "File [MRORDINI] mismatch size!" to como-riga
                if batch
                   perform SETTA-RIGA-STAMPA
                else
                   perform MESSAGE-BOX
                end-if

                set errori to true
           when "98"
                move "[MRORDINI] Indexed file corrupt!" to como-riga
                if batch
                   perform SETTA-RIGA-STAMPA
                else
                   perform MESSAGE-BOX
                end-if

                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

       PROGMAG-ERR SECTION.
           use after error procedure on PROGMAG.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-PROGMAG
           when "35"
                move "File [PROGMAG] inesistente" to como-riga

                if batch
                   perform SETTA-RIGA-STAMPA
                else
                   perform MESSAGE-BOX
                end-if
                set errori to true
           when "39"
                move "File [PROGMAG] mismatch size!" to como-riga
                if batch
                   perform SETTA-RIGA-STAMPA
                else
                   perform MESSAGE-BOX
                end-if

                set errori to true
           when "98"
                move "[PROGMAG] Indexed file corrupt!" to como-riga
                if batch
                   perform SETTA-RIGA-STAMPA
                else
                   perform MESSAGE-BOX
                end-if

                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

       USER-ERR SECTION.
           use after error procedure on USER.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-USER
           when "35"
                move "File [USER] inesistente" to como-riga

                if batch
                   perform SETTA-RIGA-STAMPA
                else
                   perform MESSAGE-BOX
                end-if
                set errori to true
           when "39"
                move "File [USER] mismatch size!" to como-riga
                if batch
                   perform SETTA-RIGA-STAMPA
                else
                   perform MESSAGE-BOX
                end-if

                set errori to true
           when "98"
                move "[USER] Indexed file corrupt!" to como-riga
                if batch
                   perform SETTA-RIGA-STAMPA
                else
                   perform MESSAGE-BOX
                end-if

                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

       USERAVV-ERR SECTION.
           use after error procedure on USERAVV.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-USERAVV
           when "35"
                move "File [USERAVV] inesistente" to como-riga

                if batch
                   perform SETTA-RIGA-STAMPA
                else
                   perform MESSAGE-BOX
                end-if
                set errori to true
           when "39"
                move "File [USERAVV] mismatch size!" to como-riga
                if batch
                   perform SETTA-RIGA-STAMPA
                else
                   perform MESSAGE-BOX
                end-if

                set errori to true
           when "98"
                move "[USERAVV] Indexed file corrupt!" to como-riga
                if batch
                   perform SETTA-RIGA-STAMPA
                else
                   perform MESSAGE-BOX
                end-if

                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

       TTIPOAVV-ERR SECTION.
           use after error procedure on TTIPOAVV.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-TTIPOAVV
           when "35"
                move "File [TTIPOAVV] inesistente" to como-riga

                if batch
                   perform SETTA-RIGA-STAMPA
                else
                   perform MESSAGE-BOX
                end-if
                set errori to true
           when "39"
                move "File [TTIPOAVV] mismatch size!" to como-riga
                if batch
                   perform SETTA-RIGA-STAMPA
                else
                   perform MESSAGE-BOX
                end-if

                set errori to true
           when "98"
                move "[TTIPOAVV] Indexed file corrupt!" to como-riga
                if batch
                   perform SETTA-RIGA-STAMPA
                else
                   perform MESSAGE-BOX
                end-if

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
           else
              set errore-ko to true
           end-if.
           if batch
              perform INVIO-MAIL-LOG
           end-if
           perform EXIT-PGM.

      ***---
       INIT.
           accept GdoInUsoFlag from environment "GDO_IN_USO".
           call "C$CALLEDBY"  using calling-program.
           if calling-program = space        or 
                              = "ricaldin-p" or
                              = "ricaldin-bat"
              set batch to true
              move "BATCH" to como-user
           else
              set batch to false
              move user-codi to como-user
           end-if.

           set messaggio    to false.
           set messaggio-T  to false.
           set messaggio-G  to false.

           move "INIZIO PROGRAMMA" to como-riga.
           perform SETTA-RIGA-STAMPA.
           set tutto-ok      to true.
           set nessun-errore to true.

      ***---
       OPEN-FILES.
           move "APERTURA FILES" to como-riga.
           perform SETTA-RIGA-STAMPA.

           |Lanciando di notte non devo farte 
           |particolari controlli sul lock
           if tutto-ok
              open input mtordini 
              open input mrordini
              open input ttipoavv
              open input user
              open input clienti
              open input ttipocli
              open i-o   progmag
              open i-o   useravv
           end-if.
           if errori
              move "APERTURA FILES NON RIUSCITA" to como-riga
              if batch
                 perform SETTA-RIGA-STAMPA
              else
                 perform MESSAGE-BOX
              end-if

           else
              move "APERTURA FILES RIUSCITA" to como-riga
              perform SETTA-RIGA-STAMPA
           end-if.

      ***---
       ELABORAZIONE.
           perform AZZERA-ORD-MASTER.

           move "SCANSIONE DEGLI ORDINI MASTER" to como-riga.
           perform SETTA-RIGA-STAMPA.

           move low-value to mto-stato-ordine
                             mto-chiave.

           start mtordini key >= k-mto-stato
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read mtordini next at end exit perform end-read

                    if mto-chiuso exit perform end-if
                    perform TRATTA-ORDINE
                 end-perform
                 perform VALUTA-PROGRESSIVI
           end-start.

           move "FINE SCANSIONE ORDINI MASTER" to como-riga
           if batch
              perform SETTA-RIGA-STAMPA
      *****     else
      *****        perform MESSAGE-BOX
           end-if.

           if GdoInUso
              if messaggio-T
                 perform VAL-MESSAGGI-T
              else
                 perform DEL-MESSAGGI-T
              end-if
              if messaggio-g
                 perform VAL-MESSAGGI-G
              else
                 perform DEL-MESSAGGI-G
              end-if
           else
              if messaggio perform VAL-MESSAGGI-T
              else         perform DEL-MESSAGGI-T
              end-if
           end-if.

      ***---
       TRATTA-ORDINE.
           if not batch
              perform AGGIORNA-VIDEO
           end-if
           set tutto-ok   to true.

           move mto-chiave   to mro-chiave-testa.
           move low-value    to mro-riga

           start mrordini key >= mro-chiave
                 invalid  continue
             not invalid
                 perform until 1 = 2
                    read mrordini next no lock 
                         at end exit perform
                    end-read
                    if mto-chiave not = mro-chiave-testa
                       exit perform
                    end-if

                    |L'impegnato MASTER è un DI CUI perciò
                    |deve rimanere valorizzato come dall'inizio
                    |eliminando solamente quanto gia' uscito
                    compute como-qta = mro-qta - mro-qta-b
                    if como-qta > 0
                       perform AGGIORNA-PRG
                    end-if
                 end-perform
           end-start.

      ***---
       AGGIORNA-PRG.
           move mro-prg-chiave to prg-chiave.
           perform READ-PROGMAG-LOCK.

           if tutto-ok
              accept prg-data-ultima-modifica from century-date
              accept prg-ora-ultima-modifica  from time
              move como-user to prg-utente-ultima-modifica

              perform SOMMA-QTA

              rewrite prg-rec invalid continue end-rewrite

           end-if

      *    valorizzo il padre
           move spaces to prg-cod-magazzino.
           move spaces to prg-tipo-imballo.
           move 0      to prg-peso.

           perform READ-PROGMAG-LOCK.

           if tutto-ok
              accept prg-data-ultima-modifica from century-date
              accept prg-ora-ultima-modifica  from time
              move como-user to prg-utente-ultima-modifica
              perform SOMMA-QTA

              rewrite prg-rec invalid continue end-rewrite
           end-if.

      ***---
       SOMMA-QTA.
           if GdoInUso
              set cli-tipo-C   to true
              move mto-cod-cli to cli-codice
              read clienti  no lock invalid continue end-read
              move cli-tipo    to tcl-codice
              read ttipocli no lock invalid continue end-read
              if tcl-imp-GDO add como-qta to prg-imp-GDO
              else           add como-qta to prg-imp-TRAD
              end-if
           else
              add como-qta to prg-imp-MASTER
           end-if.

      ***---
       VALUTA-PROGRESSIVI.
           |La somma dei figli LBX
           move 0 to save-articolo.
           move low-value to prg-rec.
           move "LBX"     to prg-cod-magazzino.
           start progmag key >= key01
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read progmag next at end  exit perform end-read
                    if prg-cod-magazzino not = "LBX"
                       exit perform
                    end-if
                    if GdoInUso
                       if messaggio-T and
                          messaggio-G
                          exit perform
                       end-if
                    else
                       if messaggio
                          exit perform
                       end-if
                    end-if

                    if save-articolo = 0
                       move prg-cod-articolo to save-articolo
                    end-if

                    if prg-cod-articolo not = save-articolo
                       move prg-cod-articolo to save-articolo
                       if GdoInUso
                          if giacenza-LBX > 0
                             if not messaggio-T
                                if giacenza-LBX > imp-GDO-LBX and
                                   imp-GDO-LBX  > 0
                                   set messaggio-T  to true
                                end-if
                             end-if
                             if not messaggio-G
                                if giacenza-LBX > imp-TRAD-LBX and
                                   imp-TRAD-LBX > 0
                                   set messaggio-G  to true
                                end-if
                             end-if
                          end-if
                       else
                          if giacenza-LBX > 0
                             if ( giacenza-LBX  -
                                  impegnato-LBX +
                                  imp-MASTER-LBX ) > 0 and
                                  imp-master-LBX   > 0
                                set messaggio  to true
                             end-if
                          end-if
                       end-if
                       move 0 to giacenza-LBX
                                 impegnato-LBX
                                 imp-MASTER-LBX
                                 imp-TRAD-LBX
                                 imp-GDO-LBX
                    end-if

                    compute giacenza-LBX   = 
                            giacenza-LBX   + prg-giacenza
                    compute impegnato-LBX  = 
                            impegnato-LBX  + prg-impegnato
                    compute imp-MASTER-LBX = 
                            imp-MASTER-LBX + prg-imp-MASTER
                    compute imp-TRAD-LBX   = 
                            imp-TRAD-LBX   + prg-imp-TRAD
                    compute imp-GDO-LBX    = 
                            imp-GDO-LBX    + prg-imp-GDO

                 end-perform
           end-start.

      ***--
       CLOSE-FILES.
           close mtordini 
                 mrordini
                 progmag
                 ttipoavv
                 user
                 useravv
                 clienti
                 ttipocli.

      ***---
       SETTA-RIGA-STAMPA.
           if batch
              initialize riga-stampa
              perform SETTA-INIZIO-RIGA
              string r-inizio  delimited size
                     como-riga delimited size
                     into riga-stampa
              end-string
              display riga-stampa upon syserr
           end-if.

      ***---
       EXIT-PGM.
           move "TERMINE PROGRAMMA" to como-riga.
           perform SETTA-RIGA-STAMPA.
           
           goback.

      ***---
       PARAGRAFO-COPY.
           copy "mail.cpy".
           copy "setta-inizio-riga.cpy".

      ***---
       INVIO-MAIL-LOG.

           move "BANNER BOLLETTAZIONE" to LinkSubject.

           initialize linkBody.

           move "In allegato dettaglio funzionamento programma" 
                  to LinkBody     

           accept LinkAddress from environment "BANNER_BOL_ADDRESSES".
           accept LinkAttach  from environment "BANNER_BOL_LOG".

           set errori to true.
           move 0 to tentativi.
           move "banner-bol" to NomeProgramma.
           perform 10 times
              add 1 to tentativi
              perform SEND-MAIL
              
              call "C$DELETE" using FileDest
              open input lineseq1
              read  lineseq1 next
              if line-riga of lineseq1 = "True"
                 set tutto-ok to true
                 close lineseq1
                 exit perform
              end-if
              close lineseq1

              initialize como-riga
              string r-inizio              delimited size
                     "TENTATIVO N. "       delimited size
                     tentativi             delimited size
                     ": "                  delimited size
                     line-riga of lineseq1 delimited size
                     into como-riga
              end-string
              perform SETTA-RIGA-STAMPA

           end-perform
               
           initialize como-riga.
           if tutto-ok
              string r-inizio               delimited size
                     "INVIO MAIL RIUSCITO!" delimited size
                     into como-riga
              end-string
           else

              string r-inizio                   delimited size
                     "INVIO MAIL NON RIUSCITO!" delimited size
                     into como-riga
      
              end-string
           end-if.
           if batch
              perform SETTA-RIGA-STAMPA
           else
              perform MESSAGE-BOX
           end-if.

           delete file lineseq.

      ***--
       AZZERA-ORD-MASTER.
           move "AZZERAMENTO ORDINATO MASTER"
                                   to como-riga.
           perform SETTA-RIGA-STAMPA.

           accept como-data from century-date
           accept como-ora   from time
           move low-value    to prg-chiave.

           start progmag key >= prg-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read progmag next no lock 
                         at end exit perform
                    end-read
                    perform READ-PROGMAG-LOCK

                    if tutto-ok
                       move 0         to prg-imp-GDO
                       move 0         to prg-imp-TRAD
                       move 0         to prg-imp-MASTER

                       move como-user to prg-utente-ultima-modifica

                       move como-data to prg-data-ultima-modifica
                       move como-ora  to prg-ora-ultima-modifica

                       rewrite prg-rec invalid continue end-rewrite
                    end-if
                 end-perform
           end-start.
           move "FINE AZZERAMENTO ORDINATO MASTER" to como-riga
           perform SETTA-RIGA-STAMPA.


      ***---
       READ-PROGMAG-LOCK.
           set RecLocked to false.
           set trovato to true.

           perform until 1 = 2
              set RecLocked to false
              read progmag lock invalid set trovato to false end-read

              evaluate status-progmag
              when "99"  set RecLocked to true
              when other exit perform
              end-evaluate

              if batch
                 initialize como-riga
                 string "Record "      delimited size
                        "progressivi " delimited size
                        prg-chiave     delimited size 
                        "già in uso!"  delimited size
                        into como-riga
                 end-string

                 perform SETTA-RIGA-STAMPA
                 set errori  to true
      *    rileggo per poter procedere
                 read progmag no lock invalid continue end-read
                 exit perform
              else
                 move prg-cod-articolo to codice-ed
                 move prg-peso         to peso-ed
                 move "progmag"        to geslock-nome-file
                 initialize geslock-messaggio
                 string "Articolo:       ", codice-ed
                 x"0d0a""Magazzino:  ",     prg-cod-magazzino
                 x"0d0a""Imballo:       ",  prg-tipo-imballo
                 x"0d0a""Peso:           ", peso-ed 
                 x"0d0a""Record già in uso su altro terminale." 
                                                        delimited size
                        into geslock-messaggio
                 end-string
                 set errori to true
                 move 1     to geslock-v-riprova
                 move 0     to geslock-v-termina
                 move 1     to geslock-v-ignora
                 call   "geslock" using geslock-linkage
                 cancel "geslock"

                 |Non valuto il risultato in uscita
                 |perchè solo "riprova" è possibila

                 evaluate true
                 when riprova 
                      continue
                 when other
                      set errori to true
                      exit perform
                 end-evaluate
              end-if
           end-perform.           

      ***---
       MESSAGE-BOX.
           inspect como-riga replacing trailing space by low-value
           display message como-riga
                     title titolo.

      ***---
       AGGIORNA-VIDEO.
           add 1     to cont.
           move cont to cont-ed.
           call "C$JUSTIFY" using cont-ed, "L"
           display cont-ed  upon form-handle at column 44 line 2.

      ***---
       VAL-MESSAGGI-T.
           move "VALORIZZAZIONE MESSAGGI UTENTI" to como-riga.
           perform SETTA-RIGA-STAMPA.

           move low-value to tav-codice.
           start ttipoavv key >= tav-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read ttipoavv next at end exit perform end-read

                    if tav-banner-trad
                       perform VAL-MESSAGGIO-T
                    end-if
                 end-perform
           end-start.

           move "FINE VALORIZZAZIONE MESSAGGI UTENTI" to como-riga
           perform SETTA-RIGA-STAMPA.

      ***---
       VAL-MESSAGGI-G.
           move "VALORIZZAZIONE MESSAGGI UTENTI" to como-riga.
           perform SETTA-RIGA-STAMPA.

           move low-value to tav-codice.
           start ttipoavv key >= tav-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read ttipoavv next at end exit perform end-read

                    if tav-banner-gdo
                       perform VAL-MESSAGGIO-G
                    end-if
                 end-perform
           end-start.

           move "FINE VALORIZZAZIONE MESSAGGI UTENTI" to como-riga
           perform SETTA-RIGA-STAMPA.

      ***---
       VAL-MESSAGGIO-T.
           move low-value to user-cod.
           start user key >= user-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read user next at end exit perform end-read
                    if user-si-banner-t
                       move tav-codice to uav-tipologia
                       move user-cod   to uav-utente
                       move como-user  to uav-utente-creazione
                                          uav-utente-ultima-modifica
                       accept uav-data-creazione       from century-date
                       accept uav-ora-creazione        from time
                       accept uav-data-ultima-modifica from century-date
                       accept uav-ora-ultima-modifica  from time
                       write uav-rec invalid continue end-write
                    end-if
                 end-perform
           end-start.

      ***---
       VAL-MESSAGGIO-G.
           move low-value to user-cod.
           start user key >= user-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read user next at end exit perform end-read
                    if user-si-banner-g
                       move tav-codice to uav-tipologia
                       move user-cod   to uav-utente
                       move como-user  to uav-utente-creazione
                                          uav-utente-ultima-modifica
                       accept uav-data-creazione       from century-date
                       accept uav-ora-creazione        from time
                       accept uav-data-ultima-modifica from century-date
                       accept uav-ora-ultima-modifica  from time
                       write uav-rec invalid continue end-write
                    end-if
                 end-perform
           end-start.

      ***---
       DEL-MESSAGGI-T.
           move "CANCELLAZIONE MESSAGGI UTENTI"   to como-riga.
           perform SETTA-RIGA-STAMPA.

           move low-value to tav-codice.
           start ttipoavv key >= tav-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read ttipoavv next at end exit perform end-read

                    if tav-banner-trad
                       perform DEL-MESSAGGIO
                    end-if
                 end-perform
           end-start.

           move "FINE CANCELLAZIONE MESSAGGI UTENTI" to como-riga
           perform SETTA-RIGA-STAMPA.

      ***---
       DEL-MESSAGGI-G.
           move "CANCELLAZIONE MESSAGGI UTENTI"   to como-riga.
           perform SETTA-RIGA-STAMPA.

           move low-value to tav-codice.
           start ttipoavv key >= tav-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read ttipoavv next at end exit perform end-read

                    if tav-banner-gdo
                       perform DEL-MESSAGGIO
                    end-if
                 end-perform
           end-start.

           move "FINE CANCELLAZIONE MESSAGGI UTENTI" to como-riga
           perform SETTA-RIGA-STAMPA.

      ***---  
       DEL-MESSAGGIO.
           move tav-codice   to uav-tipologia.
           move low-value    to uav-utente.
           start USERAVV key not < uav-k-tipologia
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read USERAVV next no lock 
                         at end exit perform
                    end-read
                    if tav-codice not = uav-tipologia
                       exit perform
                    end-if
                    delete USERAVV record invalid continue end-delete
                 end-perform
           end-start.


