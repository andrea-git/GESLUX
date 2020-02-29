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
       FD  lineseq1.
       01 line-riga        PIC  x(32000).

       WORKING-STORAGE SECTION.  
       77  status-log-macrobatch   pic xx.
       77  status-tsetinvio        pic xx.     
       77  status-lineseq          pic xx.
       77  status-lineseq1         pic xx.

       77  wstampa                 pic x(256).
       77  path-log-macrobatch     pic x(256) value spaces.
       
       77  user-cod                pic x(10).   
       77  como-data               pic 9(8).
       77  como-ora                pic 9(8).   
       77  lk-mb-logfile           pic x(256).
       01  elaborazione-edi.                
         05 primo-numero-edi       pic x(8).
         05 ultimo-numero-edi      pic x(8).
         05 tot-ordini-edi         pic x(8).    

       77  tentativi             pic 99.

       copy "log-macrobatch.def".      
       copy "mail.def".
       copy "common-linkage.def".
       copy "comune.def".
                          
       PROCEDURE DIVISION.
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
           move path-log-macrobatch to lk-mb-logfile.

           open output log-macrobatch.
           call   "set-ini-log" using r-output.
           cancel "set-ini-log".
           initialize lm-riga.
           string r-output            delimited size
                  "INIZIO PROCEDURA " delimited size
             into lm-riga
           end-string.
           write lm-riga.
           close       log-macrobatch.

           perform CALL-EDI-IMPORD.    

           if lk-mb-logfile not = "KO"
              perform CALL-EDI-SELORDINI
           end-if.                     

           if lk-mb-logfile not = "KO"
              perform CALL-SHI-EXP
           end-if.          
           
           open extend log-macrobatch.

           move spaces to user-cod.
           set environment "USER_CODI" to user-cod.  
          
           perform INVIO-MAIL.  
                                       
           call   "set-ini-log" using r-output.
           cancel "set-ini-log".
           initialize lm-riga.
           string r-output          delimited size
                  "FINE PROCEDURA " delimited size
             into lm-riga
           end-string.
           write lm-riga.         

           close       log-macrobatch.

           goback.

      ***---
       INVIO-MAIL.                                                   
           accept LinkAddress   from environment "MACROBATCH_ADDRESS".
           accept LinkAddressCC from environment "MACROBATCH_ADDRESS_CC"
           accept LinkSubject   from environment "MACROBATCH_SUBJECT".
           move path-log-macrobatch to LinkAttach.
           accept primo-numero-edi  
                  from environment "PRIMO_NUMERO_EDI".
           accept ultimo-numero-edi 
                  from environment "ULTIMO_NUMERO_EDI".
           accept tot-ordini-edi    
                  from environment "TOT_ORDINI_EDI".

           initialize LinkBody.
           string "RIEPILOGO FUNZIONAMENTO: " x"0d0a"
                  x"0d0a"   
                  "GENERAZIONE ORDINI EDI" x"0d0a"
                  x"0d0a"
                  "DAL NUMERO: "    primo-numero-edi
                   " - AL NUMERO: " ultimo-numero-edi x"0d0a"
                  "TOTALE ORDINI EDI: " tot-ordini-edi
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

           set environment "PRIMO_NUMERO_EDI"  to " ".
           set environment "ULTIMO_NUMERO_EDI" to " ".
           set environment "TOT_ORDINI_EDI"    to " ".
                          

      ***---
       PREPARA-CALL.
           move "desktop"    to LK-BL-PROG-ID.
           move "BOSS" to user-cod..
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
                                     lk-mb-logfile
           cancel "edi-impord".
                                  
      ***---
       CALL-EDI-SELORDINI.
           perform PREPARA-CALL.
           call   "edi-selordini" using LK-BLOCKPGM, 
                                        USER-CODI, 
                                        LIVELLO-ABIL,
                                        lk-mb-logfile
           cancel "edi-selordini".

      ***---
       CALL-SHI-EXP.
           perform PREPARA-CALL.
           call   "shi-exp" using LK-BLOCKPGM, 
                                  USER-CODI, 
                                  LIVELLO-ABIL,
                                  lk-mb-logfile
           cancel "shi-exp".

      ***---
       PARAGRAFO-COPY.
           copy "mail.cpy".
