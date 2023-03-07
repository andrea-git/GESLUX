       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      allstat.
       AUTHOR.                          Andrea.
       REMARKS. Allineamento statistiche. Partendo dal valore prog, 
                ricalcolo il valore corr dei mesi.
                corr 1 = prog 1         
                corr 2 = prog 2 - prog 1
                corr 3 = prog 3 - prog 2
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.          
           copy "statsett.sl".    
        
       SELECT statsett2
           ASSIGN       TO  "statsett"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-statsett
           RECORD KEY   IS sts2-chiave
           ALTERNATE RECORD KEY IS k-ord2 = sts2-mese,
                                            sts2-tipocli, 
                                            sts2-marca
                        WITH DUPLICATES .


      *****************************************************************
       DATA DIVISION.
       FILE SECTION.          
           copy "statsett.fd".

       FD  statsett2.
       01 sts2-rec.
           05 sts2-chiave.
               10 sts2-tipocli      PIC  xx.
               10 sts2-mese         PIC  99.
               10 sts2-marca        PIC  9(4).
           05 sts2-dati.
               10 sts2-kg-corr      PIC  s9(15)v999.
               10 sts2-qta-corr     PIC  s9(9).
               10 sts2-fat-corr     PIC  s9(12)v99.
               10 sts2-csm-corr     PIC  s9(12)v99.
               10 sts2-adeguam-corr PIC  s9(12)v99.
               10 sts2-kg-past      PIC  s9(15)v999.
               10 sts2-qta-past     PIC  s9(9).
               10 sts2-fat-past     PIC  s9(12)v99.
               10 sts2-csm-past     PIC  s9(12)v99.
               10 sts2-adeguam-past PIC  s9(12)v99.
               10 sts2-kg-prog      PIC  s9(15)v999.
               10 sts2-qta-prog     PIC  s9(9).
               10 sts2-fat-prog     PIC  s9(12)v99.
               10 sts2-csm-prog     PIC  s9(12)v99.
               10 sts2-adeguam-prog PIC  s9(12)v99.
               10 sts2-kg-prog-past PIC  s9(15)v999.
               10 sts2-qta-prog-past            PIC  s9(9).
               10 sts2-fat-prog-past            PIC  s9(12)v99.
               10 sts2-csm-prog-past            PIC  s9(12)v99.
               10 sts2-adeguam-prog-past        PIC  s9(12)v99.
               10 sts2-comuni.
                   15 sts2-data-creazione           PIC  9(8).
                   15 sts2-ora-creazione            PIC  9(8).
                   15 sts2-utente-creazione         PIC  x(10).
                   15 sts2-data-ultima-modifica     PIC  9(8).
                   15 sts2-ora-ultima-modifica      PIC  9(8).
                   15 sts2-utente-ultima-modifica   PIC  x(10).
               10 sts2-vuoti.
                   15 sts2-num-vuoto-1  PIC  9(15).
                   15 sts2-num-vuoto-2  PIC  9(15).
                   15 sts2-num-vuoto-3  PIC  9(15).
                   15 sts2-alfa-vuoto-1 PIC  x(20).
                   15 sts2-alfa-vuoto-2 PIC  x(20).
                   15 sts2-alfa-vuoto-3 PIC  x(20).

       WORKING-STORAGE SECTION.
       77  status-statsett      pic xx.
       78  titolo             value "GESLUX - allineamento statistiche". 
       

       01  filler            pic 9 value 0.
           88 RecLocked      value 1 false 0.

       01  filler            pic 9.
           88 record-ok      value 1 false 0.

       01  filler            pic 9.
           88 trovato        value 1 false 0.

       01  controlli         pic xx.
           88 tutto-ok       value "OK".
           88 errori         value "ER".

       LINKAGE SECTION.

      ******************************************************************
       PROCEDURE DIVISION.
       DECLARATIVES.
      ***---
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
           set tutto-ok to true.

      ***---
       OPEN-FILES.             
           open i-o   statsett.
           open input statsett2.

      ***---
       ELABORAZIONE.
           move low-value to sts-rec.
           start statsett key >= k-ord
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read statsett next at end exit perform end-read
                    evaluate sts-mese 
                    when 1
                         move sts-kg-prog      to sts-kg-corr
                         move sts-qta-prog     to sts-qta-corr    
                         move sts-fat-prog     to sts-fat-corr    
                         move sts-csm-prog     to sts-csm-corr    
                         move sts-adeguam-prog to sts-adeguam-corr
                         rewrite sts-rec
                    when other
                         move sts-chiave to sts2-chiave
                         subtract 1 from sts-mese giving sts2-mese
                         read statsett2
                              invalid continue
                          not invalid
                              compute sts-kg-corr  =
                                      sts-kg-prog - 
                                      sts2-kg-prog
                              compute sts-qta-corr =            
                                      sts-qta-prog - 
                                      sts2-qta-prog
                              compute sts-fat-corr =         
                                      sts-fat-prog - 
                                      sts2-fat-prog
                              compute sts-csm-corr =         
                                      sts-csm-prog - 
                                      sts2-csm-prog
                              compute sts-adeguam-corr =
                                      sts-adeguam-prog - 
                                      sts2-adeguam-prog
                              rewrite sts-rec
                         end-read
                    end-evaluate
                end-perform
           end-start.

      ***---
       CLOSE-FILES.
           close statsett statsett2.

      ***---
       EXIT-PGM.
           display message "ELABORAZIONE TERMINATA".
           goback.                    
