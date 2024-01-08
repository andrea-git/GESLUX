       program-id.                      macrobatch.
       author.                          Andrea.
       remarks. 
           Creare un programma che esegue come una sorta di macro al "buio",
           ossia SENZA iterazione video le seguenti operazioni:
           - IMPORT EDI ORDINI
           - GENERAZIONE EDI (*)
           - GENERAZIONE EVASIONI (Evasione normale) (**); 
             avendo la necessità di entrare in evasione, occorre che NON 
             verifichi la presenza di utenti all'interno delle procedure ad 
             oggi controllate, ma SOLO in evasione. 
             Una volta entrato genera comunque un 
             blocco per gli altri utenti
           06/11/2020 Va eseguito separatamente
      *     - ESPORTAZIONE ORDINI SHI
      *     04/11/2020
      *     - ricalcolo impegnato (che utilizza però la linkage dei batch)

           (*) = per le tipologie contrassegnate da un nuovo flag dedicato
           (**) = per i magazzini contrassegnati da un nuovo flag dedicato

           I punti 2 e 3 devono inviare via mail un riepilogo del risultato 
           e mantenere le stampe che attualmente vengono generate con 
           la medesima logica.          

           21/11/2023
           Se chiamato da macrobatch2, esegue solamente:
           - CHIUSURA SERVIZIO
           - CALL-EVACLI      
           - APERTURA SERVIZIO
       special-names. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "log-macrobatch.sl".  
           copy "tsetinvio.sl". 
           copy "macrobatch.sl".
           copy "lockfile.sl".
           copy "lineseq-mail.sl".
           copy "lock-div.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION. 
           copy "log-macrobatch.fd".
           copy "tsetinvio.fd".
           copy "lineseq-mail.fd".   
           copy "macrobatch.fd".
           copy "lockfile.fd".    
           copy "lock-div.fd".

       WORKING-STORAGE SECTION.  
       77  status-log-macrobatch   pic xx.
       77  status-tsetinvio        pic xx.     
       77  status-lineseq-mail     pic xx.
       77  status-lineseq1         pic xx.
       77  status-macrobatch       pic xx.
       77  status-lockfile         pic xx.
       77  status-lock-div         pic xx.
       77  path-lineseq-mail       pic x(256).

       77  wstampa                 pic x(256).
       77  path-log-macrobatch     pic x(256) value spaces.
       77  debugger-test           pic x.
       77  cod-err                 pic x(4).
       
       77  user-cod                pic x(10).   
       77  como-data               pic 9(8).
       77  como-ora                pic 9(8).  

       77  ws-narg                 pic 9(3) comp-1 value 0.

       copy "log-macrobatch.def".      
       copy "mail.def".
       copy "common-linkage.def".
       copy "comune.def".
      * copy "link-batch.def".
                          
       PROCEDURE DIVISION.
       DECLARATIVES.
       copy "mail-decl.cpy".

      ***---
       LOCKFILE-ERR SECTION.
           use after error procedure on lockfile.
           continue.

      ***---
       LOCK-DIV-ERR SECTION.
           use after error procedure on lock-div.
           call "C$RERR" using cod-err. 
       END DECLARATIVES.

       MAIN.            
           call "C$NARG" using ws-narg.
     
           accept  path-log-macrobatch 
                   from environment "PATH_MACROBATCH_LOG".
           inspect path-log-macrobatch replacing 
                   trailing spaces by low-value.
           accept  como-data    from century-date.
           accept  como-ora     from time.
           string  path-log-macrobatch delimited low-value
                   "LOG-MACROBATCH"    delimited size
                   "_"                 delimited size
                   como-data           delimited size
                   "_"                 delimited size
                   como-ora            delimited size
                   ".log"              delimited size
              into path-log-macrobatch
           end-string.          

           open output log-macrobatch.
           open i-o    macrobatch lockfile.             

           move high-value to mb-id.
           start macrobatch key <= mb-id
                 invalid move 0 to mb-id
             not invalid
                 read macrobatch previous
           end-read.
           add 1 to mb-id.
           initialize mb-dati replacing numeric data by zeroes
                                   alphanumeric data by spaces.
           move path-log-macrobatch to mb-path-log.
           write mb-rec.

           call   "set-ini-log" using r-output.
           cancel "set-ini-log".
           initialize lm-riga.
           string r-output                        delimited size
                  "INIZIO PROCEDURA, INVIO MAIL " delimited size
             into lm-riga
           end-string.
           write lm-riga.       
      
           perform INVIO-MAIL-INI.

           open input lock-div.
           if status-lock-div = "00"
              close       lock-div
              open output lock-div  
              if status-lock-div not = "00"
                 call   "set-ini-log" using r-output
                 cancel "set-ini-log"
                 initialize lm-riga
                 string r-output   delimited size
                        "ERRORE: " delimited size
                        cod-err    delimited size
                        " SU FILE LOCK-DIV. ELABORAZIONE INTERROTTA"
                   into lm-riga
                 end-string
                 write lm-riga
              end-if
              close lock-div
           end-if.

           initialize lck-rec replacing numeric data by zeroes 
                                   alphanumeric data by spaces. 
           move "MACROBATCH" to lck-nome-pgm.
           read lockfile lock.
           if status-lockfile = "99" 
              call   "set-ini-log" using r-output
              cancel "set-ini-log"
              initialize lm-riga
              string r-output               delimited size
                     "PROCESSO GIA' IN USO" delimited size
                into lm-riga
              end-string
              write lm-riga
           else
              accept lck-ora-creazione  from time
              accept lck-data-creazione from century-date
              move "MACROBATCH" to lck-utente-creazione
              write lck-rec invalid rewrite lck-rec end-write
              read lockfile lock
              if ws-narg = 1
                 call   "set-ini-log" using r-output
                 cancel "set-ini-log"
                 initialize lm-riga
                 string r-output             delimited size
                        "CHIUSURA SERVIZIO " delimited size
                   into lm-riga
                 end-string
                 write lm-riga
                 call "C$SYSTEM" using "E:\GESLUX\acu-stop.bat"

                 call "C$SLEEP" using 5
              end-if
              perform ESEGUI-PROGRAMMI
           end-if.              

           if ws-narg = 1                
              call   "set-ini-log" using r-output
              cancel "set-ini-log"
              initialize lm-riga
              string r-output             delimited size
                     "APERTURA SERVIZIO " delimited size
                into lm-riga
              end-string
              write lm-riga
              call "C$SYSTEM" using "E:\GESLUX\acu-start.bat"

              call "C$SLEEP" using 5
           end-if.
                                       
           call   "set-ini-log" using r-output.
           cancel "set-ini-log".
           initialize lm-riga.
           string r-output          delimited size
                  "FINE PROCEDURA " delimited size
             into lm-riga
           end-string.
           write lm-riga.  

           close  log-macrobatch.
           close  macrobatch.
           unlock lockfile all records.
           close  lockfile.

           set environment "BATCH_NOTTURNO" to " ". |per spooler

           goback.

      ***---
       ESEGUI-PROGRAMMI.   
           close log-macrobatch.
           if ws-narg = 0
              perform CALL-EDI-IMPORD
              read macrobatch no lock
              if mb-edi-impord-stato-ok
                 perform CALL-EDI-SELORDINI
              end-if                   
              read macrobatch no lock
              if mb-edi-selordini-stato-ok
                 perform CALL-EVACLI
              end-if
           else                                  
              perform CALL-EVACLI                                                 
           end-if
                 
                                    
      *****     read macrobatch no lock.
      *****     if mb-evacli-stato-ok
      *****        perform CALL-SHI-EXP
      *****     end-if.
      *****                              
      *****     read macrobatch no lock.
      *****     if mb-shi-exp-stato-ok
      *****        set environment "MACROBATCH" to "S"
      *****        perform CALL-RICALDIN              
      *****        set environment "MACROBATCH" to " "
      *****     end-if.          
           
           open extend log-macrobatch.

           move spaces to user-cod.
           set environment "USER_CODI" to user-cod.  
          
           perform INVIO-MAIL-FINE.    

      ***---
       INVIO-MAIL-FINE.                                                   
           accept LinkAddress   from environment "MACROBATCH_ADDRESS".
           accept LinkAddressCC from environment "MACROBATCH_ADDRESS_CC"
           accept LinkSubject   from environment "MACROBATCH_SUBJECT".
           move path-log-macrobatch to LinkAttach.
      
           read macrobatch no lock.
                                            
           initialize LinkBody.
           if ws-narg = 0
              if mb-edi-selordini-tot-ordini = 0
                 string "RIEPILOGO FUNZIONAMENTO: " x"0d0a"
                        x"0d0a"   
                        "NESSUN ORDINE EDI GENERATO" x"0d0a"
                        delimited size
                   into LinkBody
                 end-string
              else                              
                 string "RIEPILOGO FUNZIONAMENTO: " x"0d0a"
                        x"0d0a"   
                        "GENERAZIONE ORDINI EDI" x"0d0a"
                        x"0d0a"
                        "DAL NUMERO: "    mb-edi-selordini-primo-numero
                         " - AL NUMERO: " mb-edi-selordini-ultimo-numero 
                        x"0d0a"
                        "TOTALE ORDINI EDI: "mb-edi-selordini-tot-ordini
                        x"0d0a"
                   into LinkBody
                 end-string
              end-if
              inspect LinkBody replacing trailing spaces by low-value
           end-if.
           if mb-evacli-tot-mag = 0
              string LinkBody delimited low-value
                     x"0d0a""GENERAZIONE EVASIONI AUTOMATICHE" x"0d0a"
                     x"0d0a"
                     "NESSUNA EVASIONE GENERATA" delimited size
                into LinkBody
              end-string
           else
              string LinkBody delimited low-value
                     x"0d0a""GENERAZIONE EVASIONI AUTOMATICHE" x"0d0a"
                     x"0d0a"  delimited size
                into LinkBody
              end-string
              perform until 1 = 2
                 add 1 to idx
                 if mb-evacli-mag-codice(idx) = spaces
                    exit perform
                 end-if                     
                 inspect LinkBody replacing trailing 
                                  spaces by low-value
                 string LinkBody              delimited low-value
                        "MAGAZZINO: " x"0d0a" delimited size
                        mb-evacli-mag-codice(idx)
                        x"0d0a"
                        "DAL NUMERO: "    mb-evacli-primo-numero(idx)
                         " - AL NUMERO: " mb-evacli-ultimo-numero(idx)
                        x"0d0a"
                        "TOTALE ORDINI EDI: " mb-evacli-tot-ordini(idx)
                        x"0d0a"  delimited size
                   into LinkBody
                 end-string
                 inspect LinkBody replacing trailing 
                                  spaces by low-value
                 string LinkBody              delimited low-value
                        "GENERATO LOG: " path-log-macrobatch
                        x"0d0a"  delimited size
                   into LinkBody
                 end-string

                 
              end-perform
           end-if.     
           inspect LinkBody replacing trailing spaces by low-value.
      *     if batch-status = 0
      *        string LinkBody delimited low-value
      *               x"0d0a""RICALCOLO IMPEGNATO" x"0d0a"
      *               x"0d0a"
      *               "ESEGUITO CON SUCCESSO. DETTAGLI LOG:" 
      *               x"0d0a"
      *               batch-log delimited size
      *          into LinkBody
      *        end-string
      *     else
      *        string LinkBody delimited low-value
      *               x"0d0a""RICALCOLO IMPEGNATO" x"0d0a"
      *               x"0d0a"
      *               "ESEGUITO CON ERRORI. DETTAGLI LOG:" 
      *               x"0d0a"
      *               batch-log
      *               delimited size
      *          into LinkBody
      *        end-string
      *     end-if.                

           accept como-data from century-date
           accept como-ora  from time.
           inspect LinkBody replacing trailing spaces by low-value.
           string  LinkBody             delimited low-value                        
                   x"0d0a"              delimited size
                   "MAIL GENERATA IL: " delimited size
                   como-data(7:2)       delimited size
                   "/"                  delimited size
                   como-data(5:2)       delimited size
                   "/"                  delimited size
                   como-data(1:4)       delimited size
                   " - ALLE: "          delimited size
                   como-ora(1:2)        delimited size
                   ":"                  delimited size
                   como-ora(3:2)        delimited size
              into LinkBody
           end-string.
                                     
           accept debugger-test from environment "DEBUGGER_TEST".
           if debugger-test not = "S"
              move 5 to tentativi-mail
              move "macrobatch" to NomeProgramma
              perform CICLO-SEND-MAIL
           end-if.
      
           if mail-ko
              call   "set-ini-log" using r-output
              cancel "set-ini-log"
              initialize lm-riga
              string r-output      delimited size
                     "ERRORE DURANTE L'INVIO DELLA MAIL RIEPOLOGATIVA"
                                   delimited size
                into lm-riga
              end-string
              write lm-riga
           else            
              call   "set-ini-log" using r-output
              cancel "set-ini-log"
              initialize lm-riga
              string r-output      delimited size
                     "INVIO DELLA MAIL RIEPOLOGATIVA AVVENUTO"
                                   delimited size
                into lm-riga
              end-string
              write lm-riga
           end-if.    

      ***---
       INVIO-MAIL-INI.        
           move "andrea.ae@live.it" to LinkAddress.
           accept LinkAddressCC from environment "MACROBATCH_ADDRESS_CC"
           accept LinkSubject   from environment "MACROBATCH_SUBJECT".
           accept LinkAttach    from environment "DUMMY_ATTACH".
                                            
           initialize LinkBody.

           move "Inizio elaborazione macrobatch" to LinkBody.
           inspect LinkBody replacing trailing spaces by low-value.

                                     
           accept debugger-test from environment "DEBUGGER_TEST".
           if debugger-test not = "S"
              move 5 to tentativi-mail
              move "macrobatch" to NomeProgramma
              perform CICLO-SEND-MAIL
           end-if.
      
           if mail-ko
              call   "set-ini-log" using r-output
              cancel "set-ini-log"
              initialize lm-riga
              string r-output      delimited size
                     "ERRORE DURANTE L'INVIO DELLA MAIL INIZIALE"
                                   delimited size
                into lm-riga
              end-string
              write lm-riga
           else            
              call   "set-ini-log" using r-output
              cancel "set-ini-log"
              initialize lm-riga
              string r-output      delimited size
                     "INVIO DELLA MAIL INIZIALE AVVENUTO"
                                   delimited size
                into lm-riga
              end-string
              write lm-riga
           end-if.
        
      ***---
       AFTER-SEND-MAIL.  
           call   "set-ini-log" using r-output.
           cancel "set-ini-log".
           initialize lm-riga.
           string r-output                    delimited size
                  "INVIO MAIL. TENTATIVO N. " delimited size
                  tentativo-mail              delimited size
             into lm-riga
           end-string.
           write lm-riga.

      ***---
       PREPARA-CALL.
           move "macrobatch"    to LK-BL-PROG-ID.
           move "BOSS" to user-cod.
           accept LK-BL-DATA from century-date.
           accept LK-BL-ORA  from time.
           move "MACROBATCH" to USER-CODI.
           move 1            to LIVELLO-ABIL.
           set environment "USER_CODI" to user-cod.
           set environment "BATCH_NOTTURNO" to "S". |per spooler

      ***---
       CALL-EDI-IMPORD.                      
           perform PREPARA-CALL.
           call   "edi-impord" using LK-BLOCKPGM, 
                                     USER-CODI, 
                                     LIVELLO-ABIL,
                                     mb-id
           cancel "edi-impord".
                                  
      ***---
       CALL-EDI-SELORDINI.
           perform PREPARA-CALL.
           call   "edi-selordini" using LK-BLOCKPGM, 
                                        USER-CODI, 
                                        LIVELLO-ABIL,
                                        mb-id
           cancel "edi-selordini".

      ***---
       CALL-EVACLI.         
           perform PREPARA-CALL.
           call   "evacli" using LK-BLOCKPGM, 
                                 USER-CODI, 
                                 LIVELLO-ABIL,
                                 mb-id
           cancel "evacli".

      ****---
      * CALL-SHI-EXP.
      *     perform PREPARA-CALL.
      *     call   "shi-exp" using LK-BLOCKPGM, 
      *                            USER-CODI, 
      *                            LIVELLO-ABIL,
      *                            mb-id
      *     cancel "shi-exp".

      ***---
      * CALL-RICALDIN.
      *     call   "ricaldin-bat" using batch-linkage.
      *     cancel "ricaldin-bat".                 

      ***---
       PARAGRAFO-COPY.
           copy "mail.cpy".
