       program-id.                      call-shi-exp.
       author.                          Andrea.
       remarks. 
           Creare un programma che esegue come una sorta di macro al "buio",
           ossia SENZA iterazione video le seguenti operazioni:
           - ESPORTAZIONE ORDINI SHI
       special-names. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "log-macrobatch.sl".  
           copy "macrobatch.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION. 
           copy "log-macrobatch.fd".
           copy "macrobatch.fd".

       WORKING-STORAGE SECTION.  
       77  status-log-macrobatch   pic xx.
       77  status-macrobatch       pic xx.

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
       copy "link-batch.def".
                          
       PROCEDURE DIVISION.

       MAIN-PRG.
           accept  path-log-macrobatch 
                   from environment "PATH_MACROBATCH_LOG".
           inspect path-log-macrobatch replacing 
                   trailing spaces by low-value.
           accept  como-data    from century-date.
           accept  como-ora     from time.
           string  path-log-macrobatch delimited low-value
                   "LOG-SHI-EXP"       delimited size
                   "_"                 delimited size
                   como-data           delimited size
                   "_"                 delimited size
                   como-ora            delimited size
                   ".log"              delimited size
              into path-log-macrobatch
           end-string.

           open output log-macrobatch.
           open i-o    macrobatch.

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
           perform ESEGUI-PROGRAMMI
                                       
           call   "set-ini-log" using r-output.
           cancel "set-ini-log".
           initialize lm-riga.
           string r-output          delimited size
                  "FINE PROCEDURA " delimited size
             into lm-riga
           end-string.
           write lm-riga. 

           goback.

      ***---
       ESEGUI-PROGRAMMI.   
           close       log-macrobatch.
           perform CALL-SHI-EXP.
           
           open extend log-macrobatch.

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
       CALL-SHI-EXP.
           perform PREPARA-CALL.
           call   "shi-exp" using LK-BLOCKPGM, 
                                  USER-CODI, 
                                  LIVELLO-ABIL,
                                  mb-id
           cancel "shi-exp".
