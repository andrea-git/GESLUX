       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      invio-sol.
       AUTHOR.                          Andrea.
       REMARKS.Stampa il sollecito e scrive/riscrive lo stato.
      * Se richiesta di ritornare lo stato (isol-stato) non mette a video
      * alcun messaggio per farlo gestire dal chiamante con la colorazione.
      * Se invece chiedo l'invio (isol-invio) mette il messaggio di errore,
      * utile solo in caso di errore non previsto (dovrebbe essere bloccato
      * dallo stato nella prima fase).
      * Utilizza la stessa routine per il controllo preventivo, dopodiché
      * passa o meno all'invio effettivo.
      * 
      * INPUT:
      *
      * chiave evasione da controllare
      * utente per l'aggiornamento dell'ultimo effettuato
      * operazione da eseguire
      *
      * OUTPUT:
      *
      *isol-status:
      *-1 invio non eseguito (invio)
      *   invio non eseguibile (stato)
      * 0 invio effettuato (invio)
      *   invio eseguibile (stato) 
      * 1 invio già eseguito (stato)

      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "clienti.sl".
           copy "tordini.sl".
           copy "stato-invio.sl".
           copy "tvettori.sl".
           copy "tsetinvio.sl".
           copy "usr-tel.sl".
           COPY "lineseq-mail.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.   
           copy "clienti.fd".
           copy "tordini.fd".
           copy "stato-invio.fd".
           copy "tvettori.fd". 
           copy "tsetinvio.fd".
           copy "usr-tel.fd".
           COPY "lineseq-mail.fd".

       WORKING-STORAGE SECTION.
           copy "mail.def".
           copy "link-geslock.def".
           copy "link-stbolle.def".
           copy "splcrt2graf.lks".
           copy "standard.def".
           copy "fonts.def".

      *    COSTANTI
       78  titolo value "Invio mail solleciti".

      *    FILE STATUS
       77  status-tordini      pic xx.
       77  status-stato-invio  pic xx.
       77  status-tvettori     pic xx.
       77  status-tsetinvio    pic xx.  
       77  status-usr-tel      pic xx.
       77  status-clienti      pic xx.
                                        
       77  status-lineseq-mail pic xx.
       77  path-lineseq-mail   pic x(256).
       77  tentativi           pic 9.
       77  bolla-x             pic x(8).
       77  form1-handle        handle of window.
       77  CountChar           pic 999.
       77  como-cc             pic x(1000).

      *    FLAGS
       01 file-info.
           05 file-size        PIC  X(8)
                      USAGE IS COMP-X.
           05 file-date        PIC  9(8)
                      USAGE IS COMP-X.
           05 file-time        PIC  9(8)
                      USAGE IS COMP-X.

       LINKAGE SECTION.
       copy "link-invio-sol.def".
      ******************************************************************
       PROCEDURE DIVISION USING isol-linkage.

       DECLARATIVES.
       copy "mail-decl.cpy".

      ***---
       TORDINI-ERR SECTION.
           use after error procedure on tordini.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tordini
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File delle scadenze [TORDINI] inesistente"
                          title titolo
                           icon 3
                set errori to true
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
           when "93"
           when "99" set RecLocked to true
           when "23"
           when "02" continue
           when other display message "ERROR ", status-tordini
           end-evaluate.

      ***---
       STATO-INVIO-ERR SECTION.
           use after error procedure on stato-invio.
           set tutto-ok  to true.
           evaluate status-stato-invio
           when "35"
                display message "Impossibile procedere."
               x"0d0a""Tabella stati [STATO-INVIO] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                set errori to true
                display message "File [STATO-INVIO] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[STATO-INVIO] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "99"
                set RecLocked to true     
                initialize geslock-messaggio
                string   "File [STATO-INVIO]!"
                  x"0d0a""Impossibile procedere!" delimited size
                      into geslock-messaggio
                end-string
                move 1 to geslock-v-riprova
                move 0 to geslock-v-ignora
                move 1 to geslock-v-termina
                move   "stato-invio"   to geslock-nome-file
                call   "geslock"    using geslock-linkage
                cancel "geslock"
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
           set tutto-ok     to true.
           set RecLocked    to false.

      ***---
       OPEN-FILES.
           open input tordini tvettori usr-tel clienti.
           open i-o   stato-invio.

      ***---
       ELABORAZIONE.
           move 0 to isol-status.
           set tutto-ok to true.
           move isol-chiave to tor-chiave.
           read tordini no lock
                invalid 
                if isol-invio
                   display message "EVASIONE INESISTENTE"
                             title titolo
                              icon 3
                end-if
                move -1 to isol-status
            not invalid
                if tor-anno-bolla = 0
                   if isol-invio
                      display message "Bolla non caricata"
                                title titolo
                                 icon 3
                   end-if
                   move -1 to isol-status
                else
                   if tor-vettore = 0
                      if isol-invio
                         display message 
                                "Nessun vettore per questa evasione"
                                  title titolo
                                   icon 3
                      end-if              
                      move -1 to isol-status
                   else
                      move tor-vettore to vet-codice
                      read tvettori no lock
                           invalid
                           if isol-invio
                              display message "Vettore non valido"
                                        title titolo
                                         icon 3
                           end-if
                           move -1 to isol-status
                       not invalid
                           if vet-mail-solleciti = spaces
                              if isol-invio
                                 display message 
                                "Mail solleciti vettore non valorizzata"
                                           title titolo
                                            icon 3
                              end-if
                              move -1 to isol-status
                           else
                              if isol-invio
                                 perform INVIO-SOLLECITO
                              else                                
                                 move isol-chiave to sto-chiave-ev
                                 read stato-invio lock
                                      invalid move 0 to isol-status
                                  not invalid move 1 to isol-status
                                 end-read
                              end-if
                           end-if
                      end-read                           
                   end-if
                end-if
           end-read.

      ***---
       INVIO-SOLLECITO.
           initialize sto-rec replacing numeric data by zeroes 
                                   alphanumeric data by spaces.
           move isol-chiave to sto-chiave-ev.
           set RecLocked to false.
           perform until 1 = 2
              read stato-invio lock
                   invalid              
                   perform PDF-BOLLA
                   if tutto-ok
                      move tor-anno-bolla to sto-anno-bolla
                      move tor-num-bolla  to sto-num-bolla
                      set sto-invio-si to true
                      accept sto-data from century-date
                      accept sto-ora  from time
                      move isol-user   to sto-user
                      write sto-rec end-write
                      move 0 to isol-status
                   end-if
                   exit perform
               not invalid
                   perform PDF-BOLLA
                   if tutto-ok
                      set sto-invio-si to true
                      accept sto-data from century-date
                      accept sto-ora  from time
                      move isol-user   to sto-user
                      rewrite sto-rec        
                      move 0 to isol-status
                   end-if
                   exit perform
              end-read
              if termina
                 move -1 to isol-status
                 exit perform
              end-if  
           end-perform.
           unlock stato-invio all records.

      ***---
       PDF-BOLLA.
           display message "Inviare il sollecito al vettore?"
                     title titolo
                      type 2
                    giving scelta
           if scelta = 2 
              move -1 to isol-status
              set errori to true
              exit paragraph 
           end-if.
           set InvioXX to true.
           perform ACCESSOXX.
           initialize stbolle-linkage.
           set stbolle-ristampa to true.
           move tor-anno        to stbolle-anno.
           move tor-numero      to stb-numero-da stb-numero-a.
           move tor-num-bolla   to stbolle-data.
           set  stbolle-k-bolla to true.
           set  stbolle-singola to true.

           move spaces to stbolle-path.
           call   "stbolle-p" using stbolle-linkage.
           cancel "stbolle-p".

           initialize file-info.
          
           call "C$FILEINFO" using stbolle-path, file-info.

           if file-size = 0
              set errori to true
              display message box "PDF non creato"
                           x"0d0A" stbolle-path
                       title titolo 
              move -1 to isol-status
           else                                  
              move "invio-sol" to NomeProgramma  
              set cli-tipo-C to true             
              move tor-cod-cli to cli-codice
              read clienti no lock
                                                              
              move tor-num-bolla   to bolla-x
              inspect bolla-x replacing leading x"30" by x"20"
              call "C$JUSTIFY" using bolla-x, "L"
                                                                     
              accept  LinkSubject from environment "INVIO_SOL_SUBJECT"
              inspect LinkSubject replacing trailing spaces by low-value
              string  LinkSubject  delimited low-value
                      ": "         delimited size
                      "NR. BOLLA " delimited size
                      bolla-x      delimited space
                      " - "        delimited size
                      cli-ragsoc-1
                  into LinkSubject
              end-string

              inspect LinkSubject replacing trailing low-value by spaces
              initialize LinkBody
              string "La presente per sollecitare con la massima "
                     "urgenza la consegna della bolla in allegato."
                     x"0d0a"x"0d0a"
                     "Attendiamo urgente riscontro, "
                     "non occorre prenotare!"                      
                     x"0d0a""Nr. bolla: " bolla-x
                     x"0d0a"x"0d0a"
                     "Grazie!" into LinkBody
              end-string
                                      
      *****        accept LinkBody    from environment "INVIO_SOL_BODY"
              move isol-user          to usr-codice
              read usr-tel
                   invalid move "info@lubex.it" to LinkAddressFrom
               not invalid move usr-ind-from    to LinkAddressFrom 
                                                   LinkAddressCC
              end-read
              accept como-cc from environment "INVIO_SOL_CC"
              if como-cc not = spaces
                 inspect LinkAddressCC 
                         replacing trailing spaces by low-value
                 move 0 to CountChar
                 inspect LinkAddressCC tallying CountChar 
                         for characters before low-value
                 inspect LinkAddressCC 
                         replacing trailing spaces by low-value
                 add 1 to CountChar
                 move ";" to LinkAddressCC(CountChar:1)
                 inspect LinkAddressCC 
                         replacing trailing low-value by spaces
                 move como-cc to LinkAddressCC(CountChar + 1:)
              end-if

              move vet-mail-solleciti to LinkAddress
              move stbolle-path       to LinkAttach
           
              move 5 to tentativi-mail
              perform CICLO-SEND-MAIL             

              if mail-ko
                 display message box "Invio non riuscito."
                              x"0d0A" line-riga-mail
                          title titolo
              else
                 display message box "Invio riuscito."
                          title titolo
              end-if
              
           end-if.

           call "C$DELETE" using stbolle-path.
           perform DESTROYXX.

      ***---
       AFTER-SEND-MAIL.

      ***---
       CLOSE-FILES.
           close tordini stato-invio tvettori usr-tel clienti.

      ***---
       EXIT-PGM.
           goback.

      ***---
       PARAGRAFO-COPY.
           copy "mail.cpy".
           copy "accessoxx.cpy".
