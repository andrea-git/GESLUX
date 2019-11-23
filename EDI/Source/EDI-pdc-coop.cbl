       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      EDI-pdc-coop.
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
           10 r-cod-az               pic x(100).
           10 r-ragsoc               pic x(100).
           10 r-indi                 pic x(100).
           10 r-cap                  pic x(100).
           10 r-citta                pic x(100).
           10 r-prov                 pic x(100).
           10 r-azi                  pic x(100).
           10 r-codcli               pic x(100).
           10 r-des                  pic x(100).
           10 r-ragsoc-azi           pic x(100).
           10 r-gln                  pic x(100).
           10 r-id-scambio           pic x(100).
           10 r-emit-fatt            pic x(100).
           10 r-dlv-point            pic x(100).
           10 r-emitt-ord            pic x(100).
           10 r-dest-fatt            pic x(100).
           10 r-insegna              pic x(100).

       77  como-cli pic x(5).
       77  como-des pic x(5).
      *                                               
       77  num-rec                   pic 9(5) value 0.                                    
       77  num-rec-add               pic 9(5) value 0.                                    
       77  num-rec-rw                pic 9(5) value 0.                                    
       77  num-rec-ko                pic 9(5) value 0.
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
           end-if.

           if errori 
              goback 
           end-if.

      ***---
       ELABORAZIONE.
           perform until 1 = 2 
              perform DISPLAY-UPON-SCREEN

              initialize line-riga
              read lineseq next at end exit perform end-read
              if line-riga = spaces    exit perform end-if
              unstring line-riga delimited by separatore
                       into r-cod-az        
                            r-ragsoc        
                            r-indi          
                            r-cap           
                            r-citta         
                            r-prov          
                            r-azi           
                            r-codcli        
                            r-des           
                            r-ragsoc-azi    
                            r-gln           
                            r-id-scambio    
                            r-emit-fatt     
                            r-dlv-point     
                            r-emitt-ord     
                            r-dest-fatt     
                            r-insegna       
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
                    x"0d0a""NUM REC. "   num-rec    
                    x"0d0a""AGGIUNTI "   num-rec-add
                    x"0d0a""AGGIORNATI " num-rec-rw
                    x"0d0a""ERRATI   "   num-rec-ko

                     title titolo
                      icon 2
           goback.

      ***---
       TRATTA-RECORD.
           set tutto-ok   to true
           add 1 to num-rec.
           
           initialize ecd-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.
           set cli-tipo-C to true.

           move r-codcli to como-cli.
           call "C$JUSTIFY" using como-cli, "R"
           inspect como-cli replacing leading x"20" by x"30".
           
           move como-cli to cli-codice.

           read clienti no lock 
                invalid continue 
            not invalid    
                move r-des to como-des
                call "C$JUSTIFY" using como-des, "R"
                inspect como-des replacing leading x"20" by x"30"
                move cli-codice to des-codice
                move como-des   to des-prog
                read destini no lock
                     invalid continue
                 not invalid
                     move des-codice to ecd-cli-codice
                     move 0          to ecd-prg-destino
                     read edi-clides no lock
                          invalid continue
                      not invalid
                          move des-codice to ecd-cli-codice
                          move des-prog   to ecd-prg-destino
                          read edi-clides no lock
                               invalid
                               initialize ecd-cliente ecd-destino
                                      replacing numeric data by zeroes         
                                           alphanumeric data by spaces
                               move r-gln         to ecd-cod-consegna
                               move 92            to ecd-q-cod-consegna
                               move des-ragsoc-1  to ecd-ragsoc-d
                               move des-indirizzo to ecd-indirizzo-d
                               move des-localita  to ecd-citta-d
                               move des-prov      to ecd-prov-d
                               move des-cap       to ecd-cap-d
                               write ecd-rec end-write
                               add 1 to num-rec-add
                           not invalid
                               move r-gln to ecd-cod-consegna
                               rewrite ecd-rec        
                               add 1 to num-rec-rw
                          end-read
                     end-read
                end-read
                
           end-read.

      ***---
           copy "common-excel.cpy".
           copy "imp-files-procedure.cpy".

