       program-id.                      macrobatch.
       author.                          Andrea.
       remarks. 
           Creare un programma che esegue come una sorta di macro al "buio",
           ossia SENZA iterazione video le seguenti operazioni:
           - IMPORT EDI ORDINI
           - GENERAZIONE EDI (*)
           - GENERAZIONE EVASIONI (Evasione normale) (**); 
             avendo la necessit� di entrare in evasione, occorre che NON 
             verifichi la presenza di utenti all'interno delle procedure ad 
             oggi controllate, ma SOLO in evasione. 
             Una volta entrato genera comunque un 
             blocco per gli altri utenti
           - ESPORTAZIONE ORDINI SHI

           (*) = per le tipologie contrassegnate da un nuovo flag dedicato
           (**) = per i magazzini contrassegnati da un nuovo flag dedicato

           I punti 2 e 3 devono inviare via mail un riepilogo del risultato 
           e mantenere le stampe che attualmente vengono generate con 
           la medesima logica.
       special-names. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "log-macrobatch.sl".  
           copy "tsetinvio.sl".
           copy "lineseq.sl".   
           copy "macrobatch.sl".
           copy "lockfile.sl".
       select lineseq1
           assign       to  wstampa
           organization is line sequential
           access mode  is sequential
           file status  is status-lineseq.

      *****************************************************************
       DATA DIVISION.
       FILE SECTION. 
           copy "log-macrobatch.fd".
           copy "tsetinvio.fd".
           copy "lineseq.fd".   
           copy "macrobatch.fd".
           copy "lockfile.fd".  

       FD  lineseq1.
       01 line-riga        PIC  x(32000).

       WORKING-STORAGE SECTION.  
       77  status-log-macrobatch   pic xx.
       77  status-tsetinvio        pic xx.     
       77  status-lineseq          pic xx.
       77  status-lineseq1         pic xx.
       77  status-macrobatch       pic xx.
       77  status-lockfile         pic xx.

       77  wstampa                 pic x(256).
       77  path-log-macrobatch     pic x(256) value spaces.
       
       77  user-cod                pic x(10).   
       77  como-data               pic 9(8).
       77  como-ora                pic 9(8).  

       77  tentativi               pic 99.

       copy "log-macrobatch.def".      
       copy "mail.def".
       copy "common-linkage.def".
       copy "comune.def".
                          
       PROCEDURE DIVISION.
       DECLARATIVES.

      ***---
       LOCKFILE-ERR SECTION.
           use after error procedure on lockfile.
           continue.
       END DECLARATIVES.

       MAIN.                      
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
           string r-output            delimited size
                  "INIZIO PROCEDURA " delimited size
             into lm-riga
           end-string.
           write lm-riga.
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
              perform ESEGUI-PROGRAMMI
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

           goback.

      ***---
       ESEGUI-PROGRAMMI.   
           close       log-macrobatch.

           perform CALL-EDI-IMPORD.    

           read macrobatch no lock.

           if mb-edi-impord-stato-ok
              perform CALL-EDI-SELORDINI
           end-if.                       
                                    
           read macrobatch no lock.
           if mb-edi-selordini-stato-ok
              perform CALL-EVACLI
           end-if.                   
                                    
           read macrobatch no lock.
           if mb-evacli-stato-ok
              perform CALL-SHI-EXP
           end-if.          
           
           open extend log-macrobatch.

           move spaces to user-cod.
           set environment "USER_CODI" to user-cod.  
          
           perform INVIO-MAIL.    

      ***---
       INVIO-MAIL.                                                   
           accept LinkAddress   from environment "MACROBATCH_ADDRESS".
           accept LinkAddressCC from environment "MACROBATCH_ADDRESS_CC"
           accept LinkSubject   from environment "MACROBATCH_SUBJECT".
           move path-log-macrobatch to LinkAttach.
      
           read macrobatch no lock.
                                            
           initialize LinkBody.
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
                     "TOTALE ORDINI EDI: " mb-edi-selordini-tot-ordini
                     x"0d0a"
                into LinkBody
              end-string
           end-if.
           inspect LinkBody replacing trailing spaces by low-value.
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
              end-perform
           end-if.                                   

           accept como-data from time.
           accept como-ora  from century-date.
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

           set errori to true.
           move 0 to tentativi.
           move "macrobatch" to NomeProgramma.
           perform 5 times
              add 1 to tentativi    
      
              call   "set-ini-log" using r-output
              cancel "set-ini-log"
              initialize lm-riga
              string r-output                    delimited size
                     "INVIO MAIL. TENTATIVO N. " delimited size
                     tentativi                   delimited size
                into lm-riga
              end-string
              write lm-riga
      
              perform SEND-MAIL
              open input lineseq1
              read  lineseq1 next
              if line-riga of lineseq1 = "True"
                 set tutto-ok to true
                 close lineseq1
                 exit perform
              end-if
              close lineseq1
           end-perform.
      
           if errori             
              call   "set-ini-log" using r-output
              cancel "set-ini-log"
              initialize lm-riga
              string r-output      delimited size
                     "ERRORE DURANTE L'INVIO DELLA MAIL RIEPOLOGATIVA"
                                   delimited size
                into lm-riga
              end-string
              write lm-riga
           end-if.        

      ***---
       PREPARA-CALL.
           move "macrobatch"    to LK-BL-PROG-ID.
           move "BOSS" to user-cod.
           accept LK-BL-DATA from century-date.
           accept LK-BL-ORA  from time.
           move "MACROBATCH" to USER-CODI.
           move 1            to LIVELLO-ABIL.
           set environment "USER_CODI" to user-cod.

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

      ***---
       CALL-SHI-EXP.
           perform PREPARA-CALL.
           call   "shi-exp" using LK-BLOCKPGM, 
                                  USER-CODI, 
                                  LIVELLO-ABIL,
                                  mb-id
           cancel "shi-exp".

      ***---
       PARAGRAFO-COPY.
           copy "mail.cpy".