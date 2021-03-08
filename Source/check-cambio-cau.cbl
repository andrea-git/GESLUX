       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      check-cambio-cau.
       AUTHOR.                          Andrea.
       REMARKS. Verifica (e manda mail) che non ci siano ordini non
                fatturati con la causale cambiata
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tsetinvio.sl".
           copy "tordini.sl". 
           copy "lineseq.sl".
           COPY "lineseq.sl"
                REPLACING ==lineseq== BY ==lineseq1==,
                          ==STATUS-lineseq== BY ==STATUS-lineseq1==.

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.           
           copy "tsetinvio.fd".
           copy "tordini.fd". 
           copy "lineseq.fd".
           COPY "lineseq.fd"
                REPLACING ==lineseq== BY ==lineseq1==,
                          ==STATUS-lineseq== BY ==STATUS-lineseq1==.

       WORKING-STORAGE SECTION.

      * COSTANTI
       78  titolo                value "Contollo causale".

      * FILE STATUS
       77  status-tordini        pic xx.
       77  status-lineseq        pic xx.
       77  status-lineseq1       pic xx.
       77  status-tsetinvio      pic xx.

       77  wstampa               pic x(256).
                                          
       77  como-data             pic 9(8).
       77  como-ora              pic 9(8).
       77  separatore            pic x.

       01  filler                pic xx.
         88 tutto-ok             value "OK".
         88 errori               value "ER".

       COPY  "MAIL.DEF".

      ******************************************************************
       PROCEDURE DIVISION.

      ***---
       MAIN-PRG.
           perform INIT-PGM.
           perform OPEN-FILES.
           perform ELABORAZIONE.
           perform CLOSE-FILES.
           perform EXIT-PGM.

      ***---
       INIT-PGM.     
           accept separatore from environment "SEPARATORE".             
           set tutto-ok to true.
           accept como-data from century-date.
           accept como-ora  from time.
           accept  wstampa from environment "PATH_ST".
           inspect wstampa replacing trailing spaces by low-value.
           string  wstampa              delimited low-value
                   "report_cambio_cau_" delimited size
                   como-data            delimited size
                   "_"                  delimited size
                   como-ora             delimited size
                   ".csv"               delimited size
              into wstampa
           end-string.
           open output lineseq.
           string "Anno"        delimited size
                  separatore    delimited size
                  "N.Evasione"  delimited size
                  separatore    delimited size
                  "Originale"   delimited size
                  separatore    delimited size
                  "Attuale"     delimited size
                  separatore    delimited size
             into line-riga of lineseq
           end-string.
           write line-riga of lineseq.
                   

      ***---
       OPEN-FILES.
           open input tordini.

      ***---
       ELABORAZIONE.
           move low-value to tor-rec.
           start tordini key >= k-fattura
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tordini next at end exit perform end-read
                    if tor-anno-fattura not = 0 or
                       tor-num-fattura  not = 0
                       exit perform
                    end-if
                    if tor-causale      not = tor-causale-orig and
                       tor-causale-orig not = spaces
                       set errori to true
                       initialize line-riga of lineseq
                       string tor-anno          delimited size
                              separatore        delimited size
                              tor-numero        delimited size
                              separatore        delimited size
                              tor-causale-orig  delimited size
                              separatore        delimited size
                              tor-causale       delimited size
                              separatore        delimited size
                         into line-riga of lineseq
                       end-string
                       write line-riga of lineseq
                       set errori to true
                    end-if
                 end-perform
           end-start.

      ***---
       CLOSE-FILES.
           close tordini lineseq.

           if tutto-ok
              delete file lineseq
           else
              perform INVIO-MAIL
           end-if.

      ***---
       INVIO-MAIL.    
           move "ERRORI CAMBIO CAUSALE"              to LinkSubject.
           move "IN ALLEGATO ELENCO EVASIONI ERRATE" to LinkBody.
           accept LinkAddress 
                  from environment "CHECK_CAMBIO_CAU_ADDRESS".

           move wstampa to LinkAttach                                
                               
           move "check-cambio-cau" to NomeProgramma.
           perform 10 times
              perform SEND-MAIL
              open input lineseq1
              read lineseq1 next
              if line-riga of lineseq1 = "True"
                 close lineseq1
                 exit perform 
              end-if
              close lineseq1
           end-perform.

      ***---
       EXIT-PGM.
           goback.

      ***---
       PARAGRAFO-COPY.
           copy "mail.cpy". 
