       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      EDI-descoop.
       AUTHOR.                          Andrea.
       REMARKS.                         
           Importazione destini clienti COOP.

      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "clienti.sl".
           copy "destini.sl".
           copy "lineseq.sl".
           copy "EDI-clides.sl".

      *****************************************************************

       DATA DIVISION.
       FILE SECTION.
           copy "clienti.fd".
           copy "destini.fd".
           copy "lineseq.fd".
           copy "EDI-clides.fd".

      *****************************************************************

       WORKING-STORAGE SECTION.
      * COPY
           copy "acugui.def".
           copy "common-excel.def".
      * COSTANTI
       78  titolo 
                 value "Importazione destini clienti COOP".

       01        pic 9.
           88 rec-locked value 1 false zero.

      * FILE-STATUS
       77  status-destini            pic xx.
       77  status-clienti            pic xx.
       77  status-lineseq            pic xx.
       77  status-edi-clides         pic xx.
       77  wstampa                   pic x(256).

      * VARIABILI                            

       01  riga-destini.
           10 r-prog                 PIC x(5).
           10 r2                     PIC x(100).
           10 r3                     PIC x(100).
           10 r4                     pic x(100).
           10 r-cod-lubri            pic x(100).
      *                                               
       77  num-rec                   pic 9(5) value 0.                                    
       77  num-rec-ok                pic 9(5) value 0.                                    
       77  num-rec-ko                pic 9(5) value 0.
       77  num-rec-ed                pic zz.zzz.
       77  counter                   pic 9(10).
       77  counter2                  pic 9(10).
       77  counter-edit              pic z(10).
       77  user-codi                 pic x(10).

      * FLAGS
       01  controlli                 pic xx.
         88 tutto-ok                 value "OK".
         88 errori                   value "ER".      

      *****************************************************************

       LINKAGE SECTION.
           copy "link-imp-files.def".

       PROCEDURE DIVISION USING  imp-files-linkage.

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
           perform ACCETTA-SEPARATORE.
           move 0 to counter counter2.
           set tutto-ok to true.

           move ifs-path  to wstampa.
           move ifs-user  to user-codi.

      ***---
       OPEN-FILES.
           open i-o edi-clides.
           if tutto-ok
              open input lineseq destini clienti
              if tutto-ok
      *    tolgo l'intestazione
                 initialize line-riga
                 read LINESEQ next
              end-if
           end-if.

           if errori 
              goback 
           end-if.

      ***---
       ELABORAZIONE.
           set tutto-ok   to true
           move 0 to num-rec.
           set cli-tipo-C to true.
           move 4252 to cli-codice.
           read clienti no lock. 

           initialize ecd-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.
           move cli-codice      to ecd-cli-codice.
           move "00307850735"   to ecd-id-edi.
           move "ZZ"            to ecd-q-id-edi.
           move "8013826000002" to ecd-codforn.
           move "14"            to ecd-q-codforn.
           move cli-piva        to ecd-piva.
           move cli-ragsoc-1    to ecd-ragsoc-c.
           move cli-indirizzo   to ecd-indirizzo-c.
           move cli-localita    to ecd-citta-c.
           move cli-prov        to ecd-prov-c.
           move cli-cap         to ecd-cap-c.
           move "0000004252"    to ecd-cod-dest.
           move "91"            to ecd-q-cod-dest.
           write ecd-rec invalid rewrite ecd-rec end-write.

           perform until 1 = 2 
              perform DISPLAY-UPON-SCREEN

              initialize line-riga
              read lineseq next at end exit perform end-read
              if line-riga = spaces    exit perform end-if
              unstring line-riga delimited by separatore
                       into r-prog
                            r2 r3 r4
                            r-cod-lubri
              end-unstring
              perform TRATTA-RECORD

           end-perform.

      ***---
       CLOSE-FILES.
           close destini
                 edi-clides
                 lineseq 
                 clienti.

      ***---
       EXIT-PGM.
           display message "ELABORAZIONE TERMINATA"
                    x"0d0a""NUM REC. " num-rec
                    x"0d0a""CORRETTI " num-rec-ok
                    x"0d0a""ERRATI   " num-rec-ko

                     title titolo
                      icon 2
           goback.

      ***---
       TRATTA-RECORD.
           set tutto-ok   to true
           add 1 to num-rec.
           
           initialize des-rec ecd-rec replacing numeric data by zeroes
                                           alphanumeric data by spaces.
           move 4252    to des-codice. 
           call "C$JUSTIFY" using r-prog, "R".
           inspect r-prog replacing leading x"20" by x"30".
           move r-prog  to des-prog.
           read destini no lock 
                invalid add 1 to num-rec-ko
            not invalid add 1 to num-rec-ok
                move "0000004252"    to ecd-cod-dest
                move "91"            to ecd-q-cod-dest
                move des-codice    to ecd-cli-codice
                move des-prog      to ecd-prg-destino
                move r-cod-lubri   to ecd-cod-consegna
                move "92"          to ecd-q-cod-consegna
                move des-ragsoc-1  to ecd-ragsoc-d
                move des-indirizzo to ecd-indirizzo-d
                move des-localita  to ecd-citta-d
                move des-prov      to ecd-prov-d
                move des-cap       to ecd-cap-d
                write ecd-rec invalid rewrite ecd-rec end-write
           end-read.

      ***---
           copy "common-excel.cpy".
           copy "imp-files-procedure.cpy".

