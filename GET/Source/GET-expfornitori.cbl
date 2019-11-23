       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      get-expfornitori.
       AUTHOR.                          Luciano.
       REMARKS. EXPORT dei fornitori
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "lineseq.sl".
           copy "fileseq.sl".
           copy "clienti.sl".
           copy "tlistini.sl".
           copy "paramget.sl".

      ******************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "lineseq.fd".
           copy "fileseq.fd".
           copy "clienti.fd".
           copy "tlistini.fd".
           copy "paramget.fd".

       WORKING-STORAGE SECTION.
       copy "exp-ws.def".

       78  titolo                value "Export Fornitori".
       77  status-lineseq        pic xx.
       77  status-clienti        pic xx.
       77  status-tlistini       pic xx.
       77  status-paramget       pic xx.
       77  wstampa               pic x(256).

       01  exp-cli-rec.
           05 exp-cli-codice       PIC  9(5).
           05 exp-cli-ragsoc-1     PIC  x(40).
           05 exp-cli-ragsoc-2     PIC  x(40).
           05 exp-cli-indirizzo    PIC  x(40).
           05 exp-cli-cap          PIC  x(5).
           05 exp-cli-localita     PIC  x(35).
           05 exp-cli-prov         PIC  x(2).
           05 exp-cli-nazione      PIC  x(3).
           05 exp-cli-codfis       PIC  x(16).
           05 exp-cli-piva         PIC  x(11).
           05 exp-cli-tel-1        PIC  x(15).
           05 exp-cli-tel-2        PIC  x(15).
           05 exp-cli-fax          PIC  x(15).
           05 exp-cli-email        PIC  x(100).
           05 exp-cli-web          PIC  x(100).
           05 exp-cli-referente    PIC  x(30).
           05 exp-cli-vettore      PIC  x(3).
           05 exp-cli-tipo-persona PIC  x.


       LINKAGE SECTION.
           copy "link-exp.def".

      ******************************************************************
       PROCEDURE DIVISION using exp-linkage.

       DECLARATIVES.
       LINESEQ-ERR SECTION.
           use after error procedure on lineseq.
           set tutto-ok  to true.
           evaluate status-lineseq
           when "35"
                move "Impossibile procedere. File [LINESEQ] inesistente"
                          to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           when "39"
                move "File [LINESEQ] Mismatch size!" to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           when "98"
                move "[LINESEQ] Indexed file corrupt!"  
                                                     to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           when "93"
                move "File già in uso! Impossibile procedere! Operazione 
      -              " interrotta!"                  to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           end-evaluate.

      ***---
       CLIENTI-ERR SECTION.
           use after error procedure on clienti.
           set tutto-ok  to true.
           evaluate status-clienti
           when "35"
                move 
            "Impossibile procedere. File vettori [CLIENTI] inesistente"
                          to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           when "39"
                move "File [CLIENTI] Mismatch size!" to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           when "98"
                move "[CLIENTI] Indexed file corrupt!" to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           end-evaluate.

      ***---
       TLISTINI-ERR SECTION.
           use after error procedure on tlistini.
           set tutto-ok  to true.
           evaluate status-tlistini
           when "35"
                move 
           "Impossibile procedere. File vettori [TLISTINI] inesistente"
                       to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           when "39"
                move "File [TLISTINI] Mismatch size!"   
                                                     to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           when "98"
                move "[TLISTINI] Indexed file corrupt!" 
                                                     to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
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
                perform SCRIVI-ERORRE
           when "39"
                move "File [PARAMget] Mismatch size!"   
                                                       to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           when "98"
                move "[PARAMget] Indexed file corrupt!" 
                                                       to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
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
           perform INITIALIZE-FLAG
           set exp-fornitori    to true.

           set tutto-ok         to true.

           open input paramget.
           move space  to get-codice
           read paramget
              invalid
                 continue
           end-read
           close paramget
           inspect get-path-exp replacing trailing space by low-value

           string get-path-exp        delimited by low-value
                  "\"                 delimited by size
                  get-file-fornitori  delimited by size
                  into wstampa.

      ***---
       OPEN-FILES.
           open output lineseq.
           if tutto-ok
              open input clienti
              open input tlistini
           end-if.

      ***---
       ELABORAZIONE.
           set cli-tipo-f to true
           move low-value to cli-codice.
           start clienti key is >= cli-chiave
              invalid  
                 set errori to true
           end-start.
           if tutto-ok
              perform until 1 = 2
                 read clienti next 
                    at end       
                       exit perform 
                 end-read
                 if not cli-tipo-f
                    exit perform
                 end-if
                 perform VALIDA-FORN
                 if tutto-ok
                    perform GENERA-FILE
                    add 1 to num-rec-exp
                 else
                    add 1 to num-rec-no-exp
                 end-if
              end-perform
           end-if.

           perform SCRIVI-RIEPILOGO.

      ***---
       SCRIVI-RIEPILOGO.       
           initialize como-messaggio
           move num-rec-exp  to num-rec-ed
           call "C$justify" using num-rec-ed, "L"
           inspect num-rec-ed replacing trailing space by low-value
           string "Esportati "     delimited by size
                   num-rec-ed      delimited by low-value
                   " record."      delimited by size
                   into como-messaggio
           perform SCRIVI-MESSAGGIO

           if num-rec-no-exp not = zero
              move num-rec-no-exp  to num-rec-ed
              initialize como-messaggio
              call "C$justify" using num-rec-ed, "L"
              inspect num-rec-ed replacing trailing space by low-value
              string "Non esportati " delimited by size
                     num-rec-ed      delimited by low-value
                     " record in quanto privi di listino." 
                                     delimited by size
                     into como-messaggio
              perform SCRIVI-MESSAGGIO
           end-if.

      ***---
       VALIDA-FORN.
           set errori  to true
           move cli-codice   to tlis-fornitore
           move low-value    to tlis-destino
                                tlis-ini-val
                                tlis-fine-val.
           start tlistini key not < tlis-chiave-ricerca
              invalid
                 continue
              not invalid
                 perform until 1 = 2
                    read tlistini next
                       at end
                          exit perform
                    end-read
                    if cli-codice not = tlis-fornitore
                       exit perform
                    else
                       set tutto-ok   to true
                       exit perform
                    end-if
                 end-perform
           end-start.


      ***---
       GENERA-FILE.
           move cli-codice            to exp-cli-codice         
           move cli-ragsoc-1          to exp-cli-ragsoc-1       
           move cli-ragsoc-2          to exp-cli-ragsoc-2       
           move cli-indirizzo         to exp-cli-indirizzo      
           move cli-cap               to exp-cli-cap            
           move cli-localita          to exp-cli-localita       
           move cli-prov              to exp-cli-prov           
           move cli-nazione           to exp-cli-nazione        
           move cli-codfis            to exp-cli-codfis         
           move cli-piva              to exp-cli-piva           
           move cli-tel-1             to exp-cli-tel-1          
           move cli-tel-2             to exp-cli-tel-2          
           move cli-fax               to exp-cli-fax            
           move cli-email             to exp-cli-email          
           move cli-web               to exp-cli-web            
           move cli-referente         to exp-cli-referente      
           if cli-vettore = zero
              move space              to exp-cli-vettore
           else
              move "L"                to exp-cli-vettore(1:1)
              move cli-vettore(4:2)   to exp-cli-vettore(2:2)
           end-if
           move cli-tipo-persona      to exp-cli-tipo-persona   

           move exp-cli-rec  to line-riga
           write line-riga.

      ***---
       CLOSE-FILES.
           close clienti 
                 tlistini
                 lineseq.
  
      ***---
           COPY "exp-procedure.cpy".
