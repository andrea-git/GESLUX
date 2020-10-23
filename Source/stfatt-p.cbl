       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      stfatt-p.
       AUTHOR.                          Andrea.
       REMARKS. Ricordarsi che le modifiche "importanti" devono essere
                riportate anche sul pgm. "tmp-stfatt-p".
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tparamge.sl".
           copy "tordini.sl". 
           copy "rordini.sl". 
           copy "tnotacr.sl".
           copy "rnotacr.sl".
           copy "clienti.sl".
           copy "destini.sl".
           copy "recapiti.sl".
           copy "tvettori.sl".
           copy "articoli.sl".
           copy "tcodpag.sl".
           copy "tivaese.sl".
           copy "tcaumag.sl".
           copy "CLI.sl".  
           copy "tsetinvio.sl".
           copy "lineseq.sl". 
           COPY "lineseq.sl"
                REPLACING ==lineseq== BY ==lineseq1==,
                          ==STATUS-lineseq== BY ==STATUS-lineseq1==.   

       SELECT logfile
           ASSIGN       TO  path-logfile
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE  IS SEQUENTIAL
           FILE STATUS  IS STATUS-logfile.

       SELECT csvInput
           ASSIGN       TO path-csvInput
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE  IS SEQUENTIAL
           FILE STATUS  IS STATUS-csvInput.


      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tparamge.fd".
           copy "tordini.fd".
           copy "rordini.fd".
           copy "tnotacr.fd".
           copy "rnotacr.fd".
           copy "clienti.fd".
           copy "destini.fd". 
           copy "recapiti.fd".
           copy "tvettori.fd".
           copy "articoli.fd".
           copy "tcodpag.fd".
           copy "tivaese.fd".
           copy "tcaumag.fd".
           copy "CLI.fd".     
           copy "tsetinvio.fd".
           copy "lineseq.fd".
           COPY "lineseq.fd"
                REPLACING ==lineseq== BY ==lineseq1==,
                          ==STATUS-lineseq== BY ==STATUS-lineseq1==.
       FD  logfile.
       01 log-riga        PIC  x(900). 

       FD  csvInput.
       01 csvInput-riga        PIC  x(900).

       WORKING-STORAGE SECTION. 
           copy "acugui.def".
           copy "spooler.def".
           copy "fonts.def".
           copy "varsca".
           copy "selprint.lks".
           copy "stfatt.def".     
           copy "link-settaPDF.def".
           copy "mail.def".   

       01  r-inizio.
         05 filler              pic x(2)  value " [".
         05 r-data-i.
            10 r-gg             pic xx.
            10 filler           pic x     value "/".
            10 r-mm             pic xx.
            10 filler           pic x     value "/".
            10 r-aa             pic xx.
         05 filler              pic x(5)  value "] - [".
         05 r-ora.
            10 r-hh             pic xx.
            10 filler           pic x     value X"22".
            10 r-min            pic xx.
            10 filler           pic x     value "'".
            10 r-sec            pic xx.
         05 filler           pic x(2)     value "] ".

       77  como-numero      pic x(8).
       77  tentativi        pic 9.
       77  wstampa          pic x(256).  
       77  path-csvInput    pic x(200).
       77  status-csvInput  pic xx.
       77  status-lineseq   pic xx.       
       77  status-lineseq1  pic xx.
       77  status-logfile   pic xx.
       77  status-tsetinvio pic xx.
       77  como-riga        pic x(200).
       77  path-logfile     pic x(200).
       77  path-log         pic x(200).
       77  calling-pgm      pic x(15).
       77  path-bmp         pic x(200).
       77  path-bmp-fatture pic x(200).
                                      
       01  filler                pic 9 value 0.
           88 RichiamoSchedulato       value 1, false 0.   

       LINKAGE SECTION.
       copy "link-stfatt.def".

      ******************************************************************
       PROCEDURE DIVISION using stfatt-linkage.

       DECLARATIVES.
       TPARAMGE-ERR SECTION.
           use after error procedure on tparamge.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tparamge
           when "35"
                if not RichiamoSchedulato
                   display message box        "Impossibile procedere."
                     x"0d0a""File [TPARAMGE] inesistente"
                           title = titolo
                           icon 2          
                end-if
                set errori to true
           when "39"
                if not RichiamoSchedulato
                   display message "File [TPARAMGE] Mismatch size!"
                             title titolo
                              icon 3         
                end-if
                set errori to true
           when "98"  
                if not RichiamoSchedulato
                   display message "[TPARAMGE] Indexed file corrupt!"
                             title titolo
                             icon 3
                end-if
                set errori to true
           end-evaluate.  

      ***---
       TORDINI SECTION.
           use after error procedure on tordini.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tordini
           when "35"  
                if not RichiamoSchedulato
                   display message "Impossibile procedere."
                     x"0d0a""File delle testate [TORDINI] inesistente"
                             title titolo
                              icon 2
                end-if
                set errori to true
           when "39"  
                if not RichiamoSchedulato
                   display message "File [TORDINI] Mismatch size!"
                          title titolo
                           icon 3
                end-if
                set errori to true
           when "98"  
                if not RichiamoSchedulato
                   display message "[TORDINI] Indexed file corrupt!"
                          title titolo
                           icon 3
                end-if
                set errori to true
           when "93"
           when "99" set RecLocked to true
                     set errori    to true
           end-evaluate.  

      ***---
       TNOTACR SECTION.
           use after error procedure on tnotacr.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tnotacr 
           when "35"  
                if not RichiamoSchedulato
                   display message box        "Impossibile procedere."
                  x"0d0a""File delle testate [TNOTACR] inesistente"
                          title titolo
                           icon 2
                end-if
                set errori to true
           when "39"   
                if not RichiamoSchedulato
                   display message "File [TNOTACR] Mismatch size!"
                          title titolo
                           icon 3
                end-if
                set errori to true
           when "98"  
                if not RichiamoSchedulato
                   display message "[TNOTACR] Indexed file corrupt!"
                          title titolo
                           icon 3
                end-if
                set errori to true
           when "93"
           when "99" set RecLocked to true
                     set errori    to true
           end-evaluate.  

      ***---
       RNOTACR SECTION.
           use after error procedure on rnotacr.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-rnotacr
           when "35"
                if not RichiamoSchedulato
                   display message box        "Impossibile procedere."
                  x"0d0a""File delle righe [RNOTACR] inesistente"
                          title titolo
                           icon 2
                end-if
                set errori to true
           when "39"  
                if not RichiamoSchedulato
                   
                display message "File [RNOTACR] Mismatch size!"
                          title titolo
                           icon 3
                end-if
                set errori to true
           when "98"  
                if not RichiamoSchedulato
                   
                display message "[RNOTACR] Indexed file corrupt!"
                          title titolo
                           icon 3
                end-if
                set errori to true
           when "93"
           when "99" set RecLocked to true
                     set errori    to true
           end-evaluate.  

      ***---
       RORDINI SECTION.
           use after error procedure on rordini.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-rordini
           when "35"   
                if not RichiamoSchedulato
                display message box        "Impossibile procedere."
                  x"0d0a""File delle righe [RORDINI] inesistente"
                          title titolo
                           icon 2  
                end-if
                set errori to true
           when "39"    
                if not RichiamoSchedulato
                display message "File [RORDINI] Mismatch size!"
                          title titolo
                           icon 3 
                end-if
                set errori to true
           when "98"      
                if not RichiamoSchedulato
                display message "[RORDINI] Indexed file corrupt!"
                          title titolo
                           icon 3 
                end-if
                set errori to true
           when "93"
           when "99" set RecLocked to true
                     set errori    to true
           end-evaluate.  

      ***---
       CLIENTI-ERR SECTION.
           use after error procedure on clienti.
           set tutto-ok  to true.
           evaluate status-clienti
           when "35"      
                if not RichiamoSchedulato
                display message "Impossibile procedere."
                  x"0d0a""File clienti [CLIENTI] inesistente"
                          title titolo
                           icon 2  
                end-if
                set errori to true
           when "39"       
                if not RichiamoSchedulato
                display message "File [CLIENTI] Mismatch size!"
                          title titolo
                           icon 3  
                end-if
                set errori to true
           when "98"         
                if not RichiamoSchedulato
                display message "[CLIENTI] Indexed file corrupt!"
                          title titolo
                           icon 3   
                end-if
                set errori to true
           end-evaluate.  

      ***---
       CLI-ERR SECTION.
           use after error procedure on CLI.
           set tutto-ok  to true.
           evaluate status-cli
           when "35"             
                if not RichiamoSchedulato
                display message "Impossibile procedere."
                  x"0d0a""File clienti [CLI] inesistente"
                          title titolo
                           icon 2  
                end-if
                set errori to true
           when "39"             
                if not RichiamoSchedulato
                display message "File [CLI] Mismatch size!"
                          title titolo
                           icon 3  
                end-if
                set errori to true
           when "98"             
                if not RichiamoSchedulato
                display message "[CLI] Indexed file corrupt!"
                          title titolo
                           icon 3   
                end-if
                set errori to true
           end-evaluate.

      ***---
       DESTINI-ERR SECTION.
           use after error procedure on destini.
           set tutto-ok  to true.
           evaluate status-destini
           when "35"             
                if not RichiamoSchedulato
                display message "Impossibile procedere."
                  x"0d0a""File destini [DESTINI] inesistente"
                          title titolo
                           icon 2  
                end-if
                set errori to true
           when "39"             
                if not RichiamoSchedulato
                display message "File [DESTINI] Mismatch size!"
                          title titolo
                           icon 3  
                end-if
                set errori to true
           when "98"              
                if not RichiamoSchedulato
                display message "[DESTINI] Indexed file corrupt!"
                          title titolo
                           icon 3    
                end-if
                set errori to true
           end-evaluate.

      ***---
       RECAPITI-ERR SECTION.
           use after error procedure on recapiti.
           set tutto-ok  to true.
           evaluate status-recapiti
           when "35"               
                if not RichiamoSchedulato
                display message "Impossibile procedere."
                  x"0d0a""File recapiti [RECAPITI] inesistente"
                          title titolo
                           icon 2  
                end-if
                set errori to true
           when "39"               
                if not RichiamoSchedulato
                display message "File [RECAPITI] Mismatch size!"
                          title titolo
                           icon 3 
                end-if
                set errori to true
           when "98"                
                if not RichiamoSchedulato
                display message "[RECAPITI] Indexed file corrupt!"
                          title titolo
                           icon 3  
                end-if
                set errori to true
           end-evaluate.

      ***---
       TVETTORI-ERR SECTION.
           use after error procedure on tvettori.
           set tutto-ok  to true.
           evaluate status-tvettori
           when "35"              
                if not RichiamoSchedulato
                display message "Impossibile procedere."
                  x"0d0a""File vettori [TVETTORI] inesistente"
                          title titolo
                           icon 2 
                end-if
                set errori to true
           when "39"              
                if not RichiamoSchedulato
                display message "File [TVETTORI] Mismatch size!"
                          title titolo
                           icon 3   
                end-if
                set errori to true
           when "98"               
                if not RichiamoSchedulato
                display message "[TVETTORI] Indexed file corrupt!"
                          title titolo
                           icon 3   
                end-if
                set errori to true
           end-evaluate.

      ***---
       ARTICOLI-ERR SECTION.
           use after error procedure on articoli.
           set tutto-ok  to true.
           evaluate status-articoli
           when "35"               
                if not RichiamoSchedulato
                display message "Impossibile procedere."
                  x"0d0a""File articoli [ARTICOLI] inesistente"
                          title titolo
                           icon 2  
                end-if
                set errori to true
           when "39"               
                if not RichiamoSchedulato
                display message "File [ARTICOLI] Mismatch size!"
                          title titolo
                           icon 3 
                end-if
                set errori to true
           when "98"               
                if not RichiamoSchedulato
                display message "[ARTICOLI] Indexed file corrupt!"
                          title titolo
                           icon 3  
                end-if
                set errori to true
           end-evaluate.

      ***---
       TCODPAG-ERR SECTION.
           use after error procedure on tcodpag.
           set tutto-ok  to true.
           evaluate status-tcodpag
           when "35"               
                if not RichiamoSchedulato
                display message "Impossibile procedere."
                 x"0d0a""Tabella codici pagamento [TCODPAG] inesistente"
                          title titolo
                           icon 2 
                end-if
                set errori to true
           when "39"               
                if not RichiamoSchedulato
                display message "File [TCODPAG] Mismatch size!"
                          title titolo
                           icon 3  
                end-if
                set errori to true
           when "98"               
                if not RichiamoSchedulato
                display message "[TCODPAG] Indexed file corrupt!"
                          title titolo
                           icon 3  
                end-if
                set errori to true
           end-evaluate.

      ***---
       TIVAESE-ERR SECTION.
           use after error procedure on tivaese.
           set tutto-ok  to true.
           evaluate status-tivaese
           when "35"                
                if not RichiamoSchedulato
                display message "Impossibile procedere."
                 x"0d0a""Tabella codici iva [TIVAESE] inesistente"
                          title titolo
                           icon 2  
                end-if
                set errori to true
           when "39"                 
                if not RichiamoSchedulato
                display message "File [TIVAESE] Mismatch size!"
                          title titolo
                           icon 3   
                end-if
                set errori to true
           when "98"                 
                if not RichiamoSchedulato
                display message "[TIVAESE] Indexed file corrupt!"
                          title titolo
                           icon 3   
                end-if
                set errori to true
           end-evaluate.

      ***---
       TCAUMAG-ERR SECTION.
           use after error procedure on tcaumag.
           set tutto-ok  to true.
           evaluate status-tcaumag
           when "35"                 
                if not RichiamoSchedulato
                display message "Impossibile procedere."
                x"0d0a""Tabella causali magazzino [TCAUMAG] inesistente"
                          title titolo
                           icon 2   
                end-if
                set errori to true
           when "39"                  
                if not RichiamoSchedulato
                display message "File [TCAUMAG] Mismatch size!"
                          title titolo
                           icon 3   
                end-if
                set errori to true
           when "98"                   
                if not RichiamoSchedulato
                display message "[TCAUMAG] Indexed file corrupt!"
                          title titolo
                           icon 3
                end-if
                set errori to true
           end-evaluate.

       END DECLARATIVES.

      ***--- 
       MAIN-PRG.
           perform INIT.
           call "C$CALLEDBY" using calling-pgm.
           perform OPEN-FILES.
           if tutto-ok 
              perform ELABORAZIONE
              perform CLOSE-FILES
           else
              if RichiamoSchedulato
                 move "ERRORE IN APERTURA FILES" to como-riga
                 perform SCRIVI-RIGA-LOG
                 move -1 to stfatt-status
              end-if
           end-if.
           perform EXIT-PGM.

      ***---
       INIT.
           if SoloPdfBatch
              move 0 to stfatt-status
              set SoloPDF to true
              set RichiamoSchedulato to true
           end-if.
           set RigheFinali to false.
           set tutto-ok    to true.

           set environment "PRINTER" to "-P SPOOLER".
           set PrimaVolta to true.
           set NewPage    to true.
           move 0 to NumPagina.
           accept visualizza-totali from environment "VISUALIZZA_TOTALI"

           accept path-bmp-fatture from environment "PATH_BMP_FATTURE"
           inspect path-bmp-fatture 
                   replacing trailing spaces by low-value
           initialize path-bmp   
           string path-bmp-fatture delimited low-value
                  BitmapDiSfondo   delimited size
              into path-bmp 
           end-string   
           call "W$BITMAP" using WBITMAP-LOAD, path-bmp,
                          giving BitmapSfondoHandle.
                                                                      
           initialize path-bmp   
           string path-bmp-fatture delimited low-value
                  BitmapDiSfondoNc delimited size
              into path-bmp 
           end-string                                  
           call "W$BITMAP" using WBITMAP-LOAD, path-bmp
                          giving BitmapSfondoNcHandle.


      ***---
       OPEN-FILES.
           if RichiamoSchedulato
              accept  path-log from environment "SCHEDULER_PATH_LOG"
              perform OPEN-LOGFILE   
              move "INIZIO PROGRAMMA" to como-riga
              perform SCRIVI-RIGA-LOG
              move "APERTURA FILES" to como-riga
              perform SCRIVI-RIGA-LOG
           end-if.
           open input clienti   destini  tvettori CLI
                      articoli  rordini  recapiti
                      tcodpag   tivaese  tordini
                      tnotacr   rnotacr  tcaumag tparamge.
      
      ***---
       ELABORAZIONE.
           move spaces to tge-chiave.
           read tparamge no lock invalid continue end-read.
           if LinkElab = 1
              evaluate true
              when Tutti
                   perform ELABORA-FATTURE
      *            se ho interrrotto la stampa non devo andare
      *            a fare il ciclo sulle note di credito
                   if not spl-sta-annu 
                      perform ELABORA-NOTA-CREDITO 
                   end-if
              when Fatture
                   if calling-pgm = "stdoccsv"
                      move stfatt-path-csv to path-csvInput
                      open input csvInput
                      perform until 1 = 2
                         read csvInput next 
                              at end exit perform 
                          not at end
                              unstring csvInput-riga delimited by ";"
                                  into tor-anno-fattura
                                       tor-num-fattura
                              end-unstring      
                              move tor-anno-fattura to LinkAnno
                              move tor-num-fattura  to num-da num-a
                              perform ELABORA-FATTURE
                         end-read
                      end-perform
                      close csvInput
                   else
                      perform ELABORA-FATTURE
                   end-if
              when NoteCredito
                   if calling-pgm = "stdoccsv"
                      move stfatt-path-csv to path-csvInput
                      open input csvInput
                      perform until 1 = 2
                         read csvInput next 
                              at end exit perform 
                          not at end
                              unstring csvInput-riga delimited by ";"
                                  into tno-anno-fattura
                                       tno-num-fattura
                              end-unstring      
                              move tno-anno-fattura to LinkAnno
                              move tno-num-fattura  to num-da num-a
                              perform ELABORA-NOTA-CREDITO
                         end-read
                      end-perform
                      close csvInput
                   else
                      perform ELABORA-NOTA-CREDITO
                   end-if
              end-evaluate
           else
              perform ELABORA-FATTURE
      *       se ho interrrotto la stampa non devo andare
      *       a fare il ciclo sulle note di credito
              if not spl-sta-annu 
                 perform ELABORA-NOTA-CREDITO 
              end-if
           end-if.

           if spl-sta-annu   
              move 1 to stfatt-status
              if RichiamoSchedulato     
                 move "PROCEDURA INTERROTTA DALL'UTENTE" to como-riga
                 perform SCRIVI-RIGA-LOG
              else
                 display message "Procedura interrotta dall'utente"
                           title titolo
                            icon 2
              end-if
           else
              if trovato
                 if SoloPDF
                    if settaPDF-OK
                       perform CHIUDI-STAMPA
                       perform ASPETTA-PDF
                       if settaPDF-OK           
                          perform INVIO-MAIL
                       else               
                          initialize como-riga
                          string "PDF NON CREATO: " delimited size
                                 settaPDF-nome-file delimited size
                            into como-riga
                          end-string
                          perform SCRIVI-RIGA-LOG
                          move 1 to stfatt-status
                       end-if
                    end-if                
                 else
                    perform CHIUDI-STAMPA
                 end-if
              else              
                if RichiamoSchedulato
                   if calling-pgm = "pdfdoc-p"
                      move 2 to stfatt-status
                   else
                      move 1 to stfatt-status
                   end-if
                   move 
             "NESSUN DOCUMENTO PRESENTE AVENTE IL CRITERIO SELEZIONATO" 
                     to como-riga
                   perform SCRIVI-RIGA-LOG
                else
                   display message box 
                           "Nessun documento presente "
                           "avente il criterio selezionato"
                           title = titolo
                           icon 2
                 end-if
              end-if
           end-if.

      ***---
       ELABORA-FATTURE.
           if RichiamoSchedulato
              move "ELABORAZIONE FATTURE" to como-riga
              perform SCRIVI-RIGA-LOG
           end-if.
           perform varying NumStampa from 1 by 1 
                     until NumStampa > num-copie
              set tutto-ok   to true
              move low-value to tor-rec
              move LinkAnno  to tor-anno-fattura
              if LinkElab = 1
                 move num-da to tor-num-fattura
                 start tordini key is >= k-fattura 
                       invalid set errori to true
                 end-start
              else
LUBEXX*****      Dato che nella quasi totalita dei casi stampo l'ultimo
LUBEXX*****      lotto (o quello del giorno prima, ma difficilmente 
LUBEXX*****      quello del mese scorso) parto dalla FINE del file
                 move lotto  to tor-num-prenot
LUBEXX           move high-value to tor-data-fattura
LUBEXX                              tor-num-fattura
LUBEXX           set tor-fatt-si-prenotata to true
LUBEXX           start tordini key is <= k4
LUBEXX                 invalid set errori to true
LUBEXX           end-start
LUBEXX           set trovato-lotto to false
LUBEXX           perform until 1 = 2
LUBEXX*****         lo scorro a ritroso...
LUBEXX              read tordini previous at end exit perform end-read
LUBEXX*****         ...finchè non mi imbatto nel lotto interessato...
LUBEXX              if not trovato-lotto
LUBEXX                 evaluate true
LUBEXX*****            ...ed alzo il flag
LUBEXX                 when tor-num-prenot = lotto
LUBEXX                      set trovato-lotto to true
LUBEXX*****            Siccome i numeri di lotto sono sequenziali per 
LUBEXX*****            gruppi di fattura, una volta sorpassato significa
LUBEXX*****            che quel lotto non esiste ed esco dal ciclo
LUBEXX                 when tor-num-prenot < lotto
LUBEXX                      exit perform
LUBEXX                 end-evaluate
LUBEXX              else
LUBEXX*****            Se il numero di lotto è diverso da quello ricercato
LUBEXX*****            ma esiste all'interno del file (lo dice il flag
LUBEXX*****            alzato in precedenza) significa che sono posizionato
LUBEXX*****            sul record subito precedente per cui esco e sono
LUBEXX*****            pronto a riofare una start per maggiore e riprendere 
LUBEXX*****            col ciclo principale di read next
LUBEXX                 if tor-num-prenot not = lotto
LUBEXX                    exit perform
LUBEXX                 end-if
LUBEXX              end-if
LUBEXX           end-perform
LUBEXX           if trovato-lotto
                    start tordini key is > k4
                          invalid set errori to true
                    end-start
LUBEXX           else
LUBEXX              set errori to true
LUBEXX           end-if                    
              end-if

              if tutto-ok
                 set  trovato to false
                 perform until 1 = 2
                    set record-ok  to false
                    read tordini next at end exit perform end-read
                    if tor-anno-fattura not = LinkAnno
                       exit perform 
                    end-if
                    if tor-ordine  set elaborazione-fatture to true
                    else           set elaborazione-manuali to true 
                    end-if
                    if LinkElab = 1
                       if tor-num-fattura >  num-a exit perform end-if
                       if tor-num-fattura >= num-da
                          if tor-num-prenot   not = 0 and
                             tor-data-fattura not = 0 and
                             tor-anno-fattura not = 0
                             set record-ok to true
                          end-if
                       end-if
                    else
                       if tor-num-prenot  = lotto
                          if tor-num-prenot   not = 0 and
                             tor-data-fattura not = 0 and
                             tor-anno-fattura not = 0
                             set record-ok to true
                          end-if
                       end-if
                    end-if
                    
                    |Scarto le causali con le quali 
                    |NON devo emetter fattura
                    move tor-causale to tca-codice
                    read tcaumag no lock invalid continue end-read
                    if tca-no-emissione
                       set record-ok to false
                    end-if
                    
                    if record-ok
                       evaluate true
                       when NoPostel 
                            if tor-invio-postel 
                               set record-ok to false
                            end-if
                       when agenti
                            if tor-cod-agente = 0 
                               set record-ok to false
                            end-if
                       when SoloPDF
                            if calling-pgm not = "pdfdoc-p"
                               if tca-causale-edi not = "XX"
                                  set record-ok to false
                               end-if
                            end-if
                       end-evaluate     

                       if record-ok
                          if LinkCliente not = 0 and
                             LinkCliente not = tor-cod-cli
                             set record-ok to false
                          end-if
                       end-if

                       if record-ok
                          if LinkGDO not = spaces
                             set cli-tipo-C to true
                             move tor-cod-cli to cli-codice 
                             read clienti no lock
                                  invalid set record-ok to false
                              not invalid
                                  if cli-gdo not = LinkGDO
                                     set record-ok to false
                                  end-if
                             end-read
                          end-if
                       end-if

                       if record-ok
                          set trovato      to true
                          set prima-pagina to true
                          perform LOOP-RIGHE-ORDINE
                          if spl-sta-annu 
                             exit perform 
                          end-if
                       end-if
                    end-if
                 end-perform
              end-if
           end-perform.

      ***---
       ELABORA-NOTA-CREDITO.
           if RichiamoSchedulato
              move "ELABORAZIONE NOTE CREDITO" to como-riga
              perform SCRIVI-RIGA-LOG
           end-if.
           set elaborazione-note-credito to true.
           perform varying NumStampa from 1 by 1 
                     until NumStampa > num-copie
              set tutto-ok   to true
              move low-value to tno-rec
              move LinkAnno  to tno-anno-fattura
              if LinkElab = 1
                 move num-da to tno-num-fattura
                 start tnotacr key is >= k-fattura 
                       invalid set errori to true
                 end-start
              else      
LUBEXX*****      Dato che nella quasi totalita dei casi stampo l'ultimo
LUBEXX*****      lotto (o quello del giorno prima, ma difficilmente 
LUBEXX*****      quello del mese scorso) parto dalla FINE del file
                 move lotto  to tno-num-prenot
LUBEXX           move high-value to tno-data-fattura
LUBEXX                              tno-num-fattura
LUBEXX           set tno-fatt-si-prenotata to true
LUBEXX           start tnotacr key is <= k4
LUBEXX                 invalid set errori to true
LUBEXX           end-start
LUBEXX           set trovato-lotto to false
LUBEXX           perform until 1 = 2
LUBEXX*****         lo scorro a ritroso...
LUBEXX              read tnotacr previous at end exit perform end-read
LUBEXX*****         ...finchè non mi imbatto nel lotto interessato...
LUBEXX              if not trovato-lotto
LUBEXX                 evaluate true
LUBEXX*****            ...ed alzo il flag
LUBEXX                 when tno-num-prenot = lotto
LUBEXX                      set trovato-lotto to true
LUBEXX*****            Siccome i numeri di lotto sono sequenziali per 
LUBEXX*****            gruppi di fattura, una volta sorpassato significa
LUBEXX*****            che quel lotto non esiste ed esco dal ciclo
LUBEXX                 when tno-num-prenot < lotto
LUBEXX                      exit perform
LUBEXX                 end-evaluate
LUBEXX              else
LUBEXX*****            Se il numero di lotto è diverso da quello ricercato
LUBEXX*****            ma esiste all'interno del file (lo dice il flag
LUBEXX*****            alzato in precedenza) significa che sono posizionato
LUBEXX*****            sul record subito precedente per cui esco e sono
LUBEXX*****            pronto a riofare una start per maggiore e riprendere 
LUBEXX*****            col ciclo principale di read next
LUBEXX                 if tno-num-prenot not = lotto
LUBEXX                    exit perform
LUBEXX                 end-if
LUBEXX              end-if
LUBEXX           end-perform
LUBEXX           if trovato-lotto
                    start tnotacr key is > k4
                          invalid set errori to true
                    end-start
LUBEXX           else
LUBEXX              set errori to true
LUBEXX           end-if                    
              end-if

              if tutto-ok
                 |set  trovato to false
                 perform until 1 = 2
                    set record-ok  to false
                    read tnotacr next 
                         at end exit perform 
                    end-read
                    if LinkAnno  not = tno-anno-fattura   
                       exit perform 
                    end-if
                    if LinkElab = 1
                       if tno-num-fattura >  num-a exit perform end-if
                       if tno-num-fattura >= num-da
                          if tno-num-prenot   not = 0 and
                             tno-data-fattura not = 0 and
                             tno-anno-fattura not = 0
                             set record-ok to true
                          end-if
                       end-if
                    else
                       if tno-num-prenot  = lotto
                          if tno-num-prenot   not = 0 and
                             tno-data-fattura not = 0 and
                             tno-anno-fattura not = 0
                             set record-ok to true
                          end-if
                       end-if
                    end-if         
                    
                    |Scarto le causali con le quali 
                    |NON devo emetter fattura
                    move tno-causale to tca-codice
                    read tcaumag no lock invalid continue end-read
                    if tca-no-emissione
                       set record-ok to false
                    end-if    

                    if record-ok  
                       evaluate true
                       when NoPostel 
                            if tno-invio-postel 
                               set record-ok to false
                            end-if
                       when agenti
                            if tno-cod-agente = 0 
                               set record-ok to false
                            end-if  
                       when SoloPDF
                            if calling-pgm not = "pdfdoc-p"
                               if tca-causale-edi not = "XX"
                                  set record-ok to false
                               end-if
                            end-if
                       end-evaluate
                    end-if
                                 
                    if record-ok
                       if LinkCliente not = 0 and
                          LinkCliente not = tno-cod-cli
                          set record-ok to false
                       end-if
                    end-if    

                    if record-ok
                       set  trovato to true
                       set prima-pagina  to true
                       perform LOOP-RIGHE-NOTA-CREDITO
                       if spl-sta-annu 
                          exit perform 
                       end-if
                    end-if

                 end-perform
              end-if
           end-perform.

      ***---
       LOOP-RIGHE-ORDINE.
           initialize spooler-link.
           if calling-pgm = "stdoccsv"
              move "GESLUX - Stampa elenco fatture da csv" 
                to spl-nome-job
           else
              move "GESLUX - Stampa Fatture" to spl-nome-job
           end-if.
           if PrimaVolta    
              if SoloPDF
                 perform CREA-PDF
                 if settaPDF-OK
                    accept selprint-stampante 
                           from environment "EDI_FATT_STAMPANTE_DIRETTA"
                 else            
                    initialize como-riga
                    string "ACCESSO AL FILE INI IMPOSSIBILE: " 
                           delimited size
                           settaPDF-nome-file delimited size
                      into como-riga
                    end-string
                    perform SCRIVI-RIGA-LOG
                    move spaces to selprint-stampante
                 end-if
              else
                 call   "selprint" using selprint-linkage
                 cancel "selprint"
              end-if

              if selprint-stampante not = space
                 move selprint-num-copie to SPL-NUM-COPIE
                 move selprint-stampante to SPL-NOME-STAMPANTE
      *    NUOVA SELEZIONE
                 set spl-apertura to true
                 set spl-vertical to true
                 set WFDEVICE-WIN-PRINTER    to true
                 call "spooler" using spooler-link
                 set PrimaVolta   to false
                 if spl-sta-annu 
                    if RichiamoSchedulato
                       move "ERRORE APERTURA STAMPANTE" to como-riga
                       perform SCRIVI-RIGA-LOG
                       move -1 to stfatt-status
                    end-if
                    exit paragraph 
                 else
                    perform CARICA-FONT
                 end-if
      *    NUOVA SELEZIONE
              else
                 set spl-sta-annu to true
                 exit paragraph
              end-if
      *    NUOVA SELEZIONE
           end-if.   

           perform STAMPA-INTESTAZIONE.
                                   
           move low-value to ror-rec.
           move tor-anno     to ror-anno.
           move tor-numero   to ror-num-ordine.
           start rordini key is >= ror-chiave invalid continue end-start
           move 0 to WrittenRows.
           move 0 to importo-netto tot-imponibile tot-consumo   
                     tot-cou       tot-iva        tot-fattura
                     RowsToDo      imponibile-merce 
                     tot-cobat tot-piombo tot-solo-cou.
           
           perform INIT-TABELLA-IVA.

           perform CONTA-RIGHE-ORDINE.
                 
           |non ho mai l'intestazione perciò l'inizio
           |è fisso e il carattere dev'essere settato!!
           if elaborazione-fatture                   
              move CourierNew9 to spl-hfont
              move 12,3        to spl-riga
              move 0,4         to spl-passo
           end-if.

           perform until 1 = 2
              read rordini next at end exit perform end-read
              if ror-anno       not = tor-anno  or
                 ror-num-ordine not = tor-numero
                 exit perform
              end-if                
              perform STAMPA-RIGHE
           end-perform.

           perform STAMPA-TOTALI.
           perform STAMPA-DICHIARAZIONE.
           perform STAMPA-PIE-PAGINA.
           perform STAMPA-SCADENZE.

      ***---
       LOOP-RIGHE-NOTA-CREDITO.
           if spl-nome-job = spaces
              if calling-pgm = "stdoccsv"
                 move "GESLUX - Stampa elenco NC da csv" to spl-nome-job
              else                       
                 move "GESLUX - Stampa NC" to spl-nome-job
              end-if
           end-if.
           if PrimaVolta
              if SoloPDF       
                 perform CREA-PDF
                 if settaPDF-OK
                    accept selprint-stampante 
                           from environment "EDI_FATT_STAMPANTE_DIRETTA"
                 else
                    move spaces to selprint-stampante
                 end-if
              else
                 call   "selprint" using selprint-linkage
                 cancel "selprint"
              end-if

              if selprint-stampante not = space
                 move selprint-num-copie to SPL-NUM-COPIE
                 move selprint-stampante to SPL-NOME-STAMPANTE

                 set spl-apertura to true
                 set spl-vertical to true
                 set WFDEVICE-WIN-PRINTER    to true
                 call "spooler" using spooler-link
                 set PrimaVolta   to false
                 if spl-sta-annu 
                    if RichiamoSchedulato
                       move "ERRORE APERTURA STAMPANTE" to como-riga
                       perform SCRIVI-RIGA-LOG
                       move -1 to stfatt-status
                    end-if
                    exit paragraph 
                 else
                    perform CARICA-FONT
                 end-if
              else
                 set spl-sta-annu to true
                 exit paragraph
              end-if
           end-if.
           perform MOVE-TESTA-NOTA-TO-ORDINE.

           move 0 to WrittenRows.
           perform STAMPA-INTESTAZIONE.
           move low-value    to rno-rec.
           move tno-anno     to rno-anno.
           move tno-numero   to rno-numero.
           start rnotacr key is >= rno-chiave invalid continue end-start
           move 0 to importo-netto tot-imponibile tot-consumo   
                     tot-cou       tot-iva        tot-fattura
                     RowsToDo      imponibile-merce
                     tot-cobat tot-piombo tot-solo-cou.

           perform INIT-TABELLA-IVA.

           perform CONTA-RIGHE-NOTA-CREDITO.
                                   
      *****     move CourierNew9 to spl-hfont.
      *****     move 12,3        to spl-riga
      *****     move 0,4         to spl-passo

           perform until 1 = 2
              read rnotacr next at end exit perform end-read
              if rno-anno    not = tno-anno  or
                 rno-numero  not = tno-numero
                 exit perform
              end-if
              perform MOVE-RIGHE-NOTE-TO-ORDINE

              perform STAMPA-RIGHE
           end-perform.

           perform STAMPA-TOTALI.
           perform STAMPA-DICHIARAZIONE.
           perform STAMPA-PIE-PAGINA.
           perform STAMPA-SCADENZE.

      ***---
       CREA-PDF.
           if calling-pgm = "pdfdoc-p" 
              move stfatt-path-doc to DestFile
              move num-da          to como-numero
              inspect como-numero 
                      replacing leading x"30" by x"20"
              call "C$JUSTIFY" using como-numero, "L"
              inspect como-numero 
                      replacing trailing spaces by low-value
              move como-numero to NomeFile
           else

              accept DestFile from environment "EDI_FATT_PATH_PDF"
      *    tolgo l'eventuale barra finale
      *****     inspect DestFile replacing trailing spaces by low-value.
      *****     initialize cont.
      *****     inspect DestFile tallying cont
      *****             for characters before low-value.
      *****     if DestFile(cont:1) = "\" 
      *****        move low-value  to DestFile(cont:1)
      *****     end-if.
      *****     inspect DestFile replacing trailing low-value by spaces.
      *****
              accept como-data from century-date
              accept como-ora  from time 

              if fatture
                 move "ADDEBITI_GENERICI_DAL_" to TipoDoc
              else
                 move "ACCREDITI_GENERICI_DAL" to TipoDoc
              end-if
              inspect TipoDoc replacing trailing spaces by low-value
              string "LUBEX_" delimited size
                     TipoDoc            delimited low-value
                     "_"                delimited size
                     num-da             delimited size
                     "_AL_"             delimited size
                     num-a              delimited size
                     "__"               delimited size
                     como-data          delimited size
                     "_"                delimited size
                     como-ora           delimited size
                     into NomeFile
              end-string
           end-if.

           set settaPDF-setta to true.

           move NomeFile  to settaPDF-nome-file.
           move DestFile  to settaPDF-percorso.
           call   "settaPDF2" using settaPDF-linkage.
           cancel "settaPDF2".

      ***---
       ASPETTA-PDF.
           set settaPDF-resetta   to true.
           call   "settaPDF2" using settaPDF-linkage
           cancel "settaPDF2". 

      ***---
       INVIO-MAIL.
           if calling-pgm = "pdfdoc-p"
              exit paragraph
           end-if.
           if settaPDF-nome-file = spaces
              if RichiamoSchedulato
                 move 1 to stfatt-status
                 move "Invio non riuscito: PDF non trovato" to como-riga
                 perform SCRIVI-RIGA-LOG
              else
                 display message "Invio non riuscito: PDF non trovato"
                           title titolo
                            icon 2
              end-if
              move 9 to link-tipo-stampa
           else                    

              move "INVIO DOCUMENTI LUBEX"  to LinkSubject
              move "In allegato documenti." to LinkBody
              move settaPDF-nome-file       to LinkAttach
              move "info@lubex.it"          to LinkAddressFrom                  
              accept LinkAddress   
                     from environment "EDI_FATT_ADDRESS"                 
              accept LinkAddressCC 
                     from environment "EDI_FATT_ADDRESS_CC"
           
              set errori to true 
              move 0 to tentativi
              move "stfatt-p" to NomeProgramma
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

              end-perform 

              if errori         
                 if RichiamoSchedulato
                    move 1 to stfatt-status
                    move "INVIO NON RIUSCITO" to como-riga
                    perform SCRIVI-RIGA-LOG
                 else
                    display message box "Invio non riuscito."
                                 x"0d0A"line-riga of lineseq1
                             title titolo
                              icon 2       
                 end-if
                 move 9 to link-tipo-stampa
              else     
                 if RichiamoSchedulato        
                    move "INVIO NON RIUSCITO" to como-riga
                    perform SCRIVI-RIGA-LOG
                 else
                    display message "Invio riuscito."
                              title titolo
                 end-if
              end-if
           end-if.
 
      ***---
       CONTA-RIGHE-ORDINE.
           set no-cou    to true.
           set no-cobat  to true.
           set no-piombo to true.
           move 0 to righe-finali.
           perform until 1 = 2
              read rordini next at end exit perform end-read
              if ror-anno       not = tor-anno  or
                 ror-num-ordine not = tor-numero
                 exit perform
              end-if                
              add 1 to RowsToDo
LUBEXX        if ror-qta-omaggi not = 0
LUBEXX           add 1 to RowsToDo
LUBEXX        end-if
              if visualizza-totali = "S"
                 if tor-causale = "FTMA"
                    move 1 to ror-qta
                 end-if
                 move ror-cod-articolo to art-codice
                 read articoli no lock invalid continue end-read
                 if ror-add-piombo > 0
                    set si-piombo to true
                    compute tot-piombo = 
                            tot-piombo +
                            ror-add-piombo  * 
                          ( ror-qta - ror-qta-omaggi )
                 end-if
                 if ror-imp-cou-cobat > 0
                    if art-si-cobat
                       set si-cobat to true
                       compute tot-cobat = 
                               tot-cobat +
                               ror-imp-cou-cobat * 
                             ( ror-qta - ror-qta-omaggi )
                    else
                       set si-cou   to true
                       compute tot-solo-cou = 
                               tot-solo-cou +
                               ror-imp-cou-cobat * 
                             ( ror-qta - ror-qta-omaggi )
                    end-if
                 end-if
              end-if
           end-perform.

           if visualizza-totali = "S"
              if si-piombo add 1 to righe-finali end-if
              if si-cou    add 1 to righe-finali end-if
              if si-cobat  add 1 to righe-finali end-if
              add righe-finali to RowsToDo
              |La riga di spazio di divisione articoli - totali
              if righe-finali > 0 add 1 to RowsToDo end-if
           end-if.

           move low-value to ror-rec.
           move tor-anno     to ror-anno.
           move tor-numero   to ror-num-ordine.
           start rordini key is >= ror-chiave 
                 invalid continue 
           end-start.

      ***---
       CONTA-RIGHE-NOTA-CREDITO.
           perform until 1 = 2
              read rnotacr next at end exit perform end-read
              if rno-anno    not = tno-anno  or
                 rno-numero  not = tno-numero
                 exit perform
              end-if
              add 1 to RowsToDo
              if visualizza-totali = "S"
                 move tno-causale to tca-codice
                 read tcaumag no lock invalid continue end-read
                 if tca-tipo-nota-abbuono
                    move 1 to rno-qta
                 end-if
                 move rno-cod-articolo to art-codice
                 read articoli no lock invalid continue end-read
                 if rno-add-piombo > 0
                    set si-piombo to true
                    compute tot-piombo = 
                            tot-piombo +
                            rno-add-piombo * rno-qta
                 end-if
                 if rno-imp-cou-cobat > 0
                    if art-si-cobat
                       set si-cobat to true
                       compute tot-cobat = 
                               tot-cobat +
                               rno-imp-cou-cobat * rno-qta
                    else
                       set si-cou   to true
                       compute tot-solo-cou = 
                               tot-solo-cou +
                               rno-imp-cou-cobat * rno-qta
                    end-if
                 end-if
              end-if
           end-perform.

           if visualizza-totali = "S"
              if si-piombo add 1 to righe-finali end-if
              if si-cou    add 1 to righe-finali end-if
              if si-cobat  add 1 to righe-finali end-if
              add righe-finali to RowsToDo
           end-if.

           move low-value to rno-rec.
           move tno-anno     to rno-anno.
           move tno-numero   to rno-numero.
           start rnotacr key is >= rno-chiave invalid continue 
           end-start.

      ***---
       CHIUDI-STAMPA.
           set spl-chiusura to true.
           call   "spooler" using spooler-link.
           cancel "spooler".

      ***---
       CLOSE-FILES.
           close tordini  clienti destini CLI
                 tvettori rordini recapiti
                 articoli tcodpag tivaese tcaumag tparamge.

      ***---
       OPEN-LOGFILE.
           accept como-ora  from time.
           accept como-data from century-date
           inspect path-log replacing trailing spaces by low-value.
           initialize path-logfile.
           string path-log         delimited low-value
                  "LOG_ST_FATT_"   delimited size
                  como-data        delimited size
                  "_"              delimited size
                  como-ora         delimited size
                  ".log"           delimited size
                  into path-logfile
           end-string.
           open output logfile.

      ***---
       EXIT-PGM.
           set environment "PRINTER" to "-P SPOOLER-DIRECT".

           destroy Arial8B.
           destroy CourierNew7.
           destroy CourierNew9.
           destroy CourierNew11.
           call "W$BITMAP" using wbitmap-destroy, BitmapSfondoHandle.
           call "W$BITMAP" using wbitmap-destroy, BitmapSfondoNcHandle.

           cancel "spooler".
           initialize spooler-link.

           if RichiamoSchedulato
              move "FINE PROGRAMMA" to como-riga
              perform SCRIVI-RIGA-LOG
              close logfile
              move path-logfile to stfatt-path-log |Per ora non usato
           end-if.

           goback.

      ***---
       PARAGRAFO-COPY.
           copy "stfatt.cpy".
           copy "mail.cpy".

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
       SCRIVI-RIGA-LOG.
           if path-logfile = spaces exit paragraph end-if.
           inspect como-riga replacing trailing spaces by low-value.
           perform SETTA-INIZIO-RIGA.
           initialize log-riga.
           string r-inizio  delimited size
                  como-riga delimited low-value
             into log-riga
           end-string.
           write log-riga. 
