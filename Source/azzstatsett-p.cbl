       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      azzstatsett-p.
       AUTHOR.                          Andrea.
       REMARKS. Questo batch verrà lanciato solo a FINE ANNO dai
                dipendenti Lubex per far "slittare" i dati valorizzando
                i campi dell'anno precedente.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "statsett.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "statsett.fd".

       WORKING-STORAGE SECTION.
      * COPY
       copy "link-geslock.def".

      * COSTANTI
       78  titolo value "Batch Azzeramento".

      * FILE-STATUS
       77  status-statsett       pic xx.

      * FLAGS
       01  controlli             pic xx.
         88 tutto-ok             value "OK".
         88 errori               value "ER".

      * VARIABILI
       77  counter               pic 9(10).
       77  counter2              pic 9(10).
       77  counter-edit          pic z(10).

       LINKAGE SECTION.
       77  link-handle           handle of window.

       PROCEDURE DIVISION USING link-handle.

       DECLARATIVES.
       STATSETT-ERR SECTION.
           use after error procedure on statsett.
           set tutto-ok  to true.
           evaluate status-statsett
           when "35"
                set errori to true
                display message "File [STATSETT] not found!"
                          title titolo
                           icon 3
           when "39"
                set errori to true
                display message "File [STATSETT] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[STATSETT] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "93"
           when "99"
                initialize geslock-messaggio
                string   "File già in uso!"
                  x"0d0a""Impossibile procedere!" delimited size
                      into geslock-messaggio
                end-string
                move 1 to geslock-v-riprova
                move 0 to geslock-v-ignora
                move 1 to geslock-v-termina
                move   "statsett"   to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova
                     open i-o statsett allowing readers
                when termina
                     set errori to true
                     display message "Operazione interrotta!"
                               title titolo
                                icon 2
                end-evaluate
           end-evaluate.
       END DECLARATIVES.

      ***---
       MAIN-PRG.
           perform INIT.
           perform OPEN-FILE.
           if tutto-ok
              perform ELABORAZIONE
              perform CLOSE-FILE
           end-if.
           perform EXIT-PGM.

      ***---
       INIT.
           set tutto-ok to true.

      ***---
       OPEN-FILE.
           open i-o statsett allowing readers.

      ***---
       ELABORAZIONE.
           move low-value  to sts-rec.
           start statsett  key is >= sts-chiave
                 invalid set errori to true
           end-start.

           if tutto-ok
              perform until 1 = 2
                 read statsett next at end exit perform end-read
                 

                 add 1 to counter
                 add 1 to counter2
                 if counter2 = 100
                    move counter to counter-edit
                    display counter-edit
                       upon link-handle at column 35
                                             line 18
                    move 0 to counter2
                 end-if

                 |Correzione. Nel caso per il cambio anno vengano
                 |sporcati i dati corr con la nuova fatturazione li
                 |ripristino. Se non sono stati sporcati invece
                 |sono gli stessi (cumulati e correnti a Gennaio 
                 |sono la stessa cosa) per cui non influisce
                 if sts-mese = 1
                    move sts-kg-prog      to sts-kg-past
                    move sts-fat-prog     to sts-fat-past
                    move sts-qta-prog     to sts-qta-past
                    move sts-csm-prog     to sts-csm-past
                    move sts-adeguam-prog to sts-adeguam-past
                 else   
                    move sts-kg-corr      to sts-kg-past
                    move sts-fat-corr     to sts-fat-past
                    move sts-qta-corr     to sts-qta-past
                    move sts-csm-corr     to sts-csm-past
                    move sts-adeguam-corr to sts-adeguam-past
                 end-if
      
                 
                 move sts-kg-prog      to sts-kg-prog-past
                 move sts-fat-prog     to sts-fat-prog-past
                 move sts-qta-prog     to sts-qta-prog-past
                 move sts-csm-prog     to sts-csm-prog-past
                 move sts-adeguam-prog to sts-adeguam-prog-past

      *****           if sts-mese not = 1
                    move 0             to sts-kg-corr     
                    move 0             to sts-fat-corr    
                    move 0             to sts-qta-corr    
                    move 0             to sts-csm-corr    
                    move 0             to sts-adeguam-corr
      *****           end-if

                 move 0                to sts-kg-prog     
                 move 0                to sts-fat-prog    
                 move 0                to sts-qta-prog    
                 move 0                to sts-csm-prog    
                 move 0                to sts-adeguam-prog

                 rewrite sts-rec  invalid continue end-rewrite
              end-perform
           end-if.

      ***---
       CLOSE-FILE.
           close statsett.

      ***---
       EXIT-PGM.
           goback.
