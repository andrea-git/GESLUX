       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      EDI-imp-codici.
       AUTHOR.                          Andrea.
       REMARKS.

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
                 value "Importazione codici destini clienti".

       01        pic 9.
           88 rec-locked value 1 false zero.

      * FILE-STATUS
       77  status-destini            pic xx.
       77  status-clienti            pic xx.
       77  status-lineseq            pic xx.
       77  status-edi-clides         pic xx.
       77  wstampa                   pic x(256).

      * VARIABILI                            
       78  separatore value ";".

       01  riga-codici.
         05 r-cliente                pic x(5).
         05 r-ecd-cod-dest           pic x(17).
         05 r-ecd-q-cod-dest         pic x(3).
         05 r4                       pic x.
         05 r5                       pic x.
         05 r6                       pic x.
         05 r-destino                pic x(5).
         05 r-ecd-cod-consegna       pic x(17).
         05 r-ecd-q-cod-consegna     pic x(3).
         05 r10                      pic x.
         05 r11                      pic x.
         05 r12                      pic x.
         05 r13                      pic x.
         05 r-ecd-id-edi             pic x(35).
         05 r-ecd-q-id-edi           pic x(4). 
         05 r-ecd-codforn            pic x(17).
         05 r-ecd-q-codforn          pic x(3). 

      *                                               
       77  num-rec                   pic 9(5) value 0.                                    
       77  num-rec-ok                pic 9(5) value 0.                                    
       77  num-rec-cod               pic 9(5) value 0.

      * FLAGS
       01  controlli                 pic xx.
         88 tutto-ok                 value "OK".
         88 errori                   value "ER".      

      *****************************************************************

       LINKAGE SECTION.

       PROCEDURE DIVISION.

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

           move "codici-edi.csv" to wstampa.

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
           move 0 to num-rec.

           perform until 1 = 2 
              initialize line-riga
              read lineseq next at end exit perform end-read
              if line-riga = spaces    exit perform end-if
              unstring line-riga delimited by separatore
                       into r-cliente
                            r-ecd-cod-dest
                            r-ecd-q-cod-dest 
                            r4
                            r5
                            r6
                            r-destino
                            r-ecd-cod-consegna
                            r-ecd-q-cod-consegna
                            r10
                            r11
                            r12
                            r13          
                            r-ecd-id-edi   
                            r-ecd-q-id-edi 
                            r-ecd-codforn  
                            r-ecd-q-codforn

              end-unstring
              if r-ecd-cod-dest     not = spaces and 
                 r-ecd-cod-consegna not = spaces
                 perform TRATTA-RECORD
              end-if

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
                    x"0d0a""CON CODICE " num-rec-cod

                     title titolo
                      icon 2
           goback.

      ***---
       TRATTA-RECORD.
           set tutto-ok   to true
           add 1 to num-rec.
                                
           call "C$JUSTIFY" using r-cliente, "R"
           inspect r-cliente replacing leading x"20" by x"30"
           move r-cliente to des-codice.

           set cli-tipo-C to true.
           move r-cliente to cli-codice.
           read clienti no lock 
                invalid continue
            not invalid
                call "C$JUSTIFY" using r-destino, "R"
                inspect r-destino replacing leading x"20" by x"30"
                move r-destino to des-prog
                read destini no lock
                     invalid continue
                 not invalid
                     add 1 to num-rec-ok

                     move des-codice to ecd-cli-codice
                     move 0          to ecd-prg-destino
                     initialize ecd-dati 
                                replacing numeric data by zeroes
                                     alphanumeric data by spaces
                     
                     move r-ecd-id-edi     to ecd-id-edi     
                     move r-ecd-q-id-edi   to ecd-q-id-edi 
                     move r-ecd-codforn    to ecd-codforn   
                     move r-ecd-q-codforn  to ecd-q-codforn
                     move cli-piva         to ecd-piva
                     move cli-ragsoc-1     to ecd-ragsoc-c
                     move cli-indirizzo    to ecd-indirizzo-c
                     move cli-localita     to ecd-citta-c
                     move cli-prov         to ecd-prov-c
                     move cli-cap          to ecd-cap-c
                     move r-ecd-cod-dest   to ecd-cod-dest  
                     move r-ecd-q-cod-dest to ecd-q-cod-dest
                     write ecd-rec invalid rewrite ecd-rec end-write
                     
                     initialize ecd-rec replacing numeric data by zeroes
                                             alphanumeric data by spaces
                     move des-codice      to ecd-cli-codice   
                     move des-prog        to ecd-prg-destino  
                     move r-ecd-id-edi    to ecd-id-edi     
                     move r-ecd-q-id-edi  to ecd-q-id-edi 
                     move r-ecd-codforn   to ecd-codforn   
                     move r-ecd-q-codforn to ecd-q-codforn
                     move cli-piva        to ecd-piva
                     move cli-ragsoc-1    to ecd-ragsoc-c
                     move cli-indirizzo   to ecd-indirizzo-c
                     move cli-localita    to ecd-citta-c
                     move cli-prov        to ecd-prov-c
                     move cli-cap         to ecd-cap-c
                     move r-ecd-cod-dest  to ecd-cod-dest  
                     move r-ecd-q-cod-dest     to ecd-q-cod-dest
                     move r-ecd-cod-consegna   to ecd-cod-consegna
                     move r-ecd-q-cod-consegna to ecd-q-cod-consegna
                     move des-ragsoc-1    to ecd-ragsoc-d
                     move des-indirizzo   to ecd-indirizzo-d
                     move des-localita    to ecd-citta-d
                     move des-prov        to ecd-prov-d
                     move des-cap         to ecd-cap-d
                     write ecd-rec invalid rewrite ecd-rec end-write
                     if r-ecd-cod-dest not = spaces and
                        r-ecd-cod-consegna not = spaces
                        add 1 to num-rec-cod
                     end-if
                end-read
           end-read.               
