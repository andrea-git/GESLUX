       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      check-prg-edi.
       AUTHOR.                          Andrea.
       REMARKS. Verifica (e manda mail) che non ci progressivi sbagliati
                articolo <> da articolo progressivo in master e edi
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tsetinvio.sl".
           copy "mtordini.sl". 
           copy "EDI-mtordini.sl". 
           copy "lineseq.sl".
           COPY "lineseq-mail.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.           
           copy "tsetinvio.fd".
           copy "mtordini.fd". 
           copy "EDI-mtordini.fd". 
           copy "lineseq.fd".
           COPY "lineseq-mail.fd".

       WORKING-STORAGE SECTION.

      * COSTANTI
       78  titolo                value "Contollo prog".

      * FILE STATUS
       77  status-mtordini       pic xx.
       77  status-EDI-mtordini   pic xx.
       77  status-lineseq        pic xx.
       77  status-lineseq1       pic xx.
       77  status-tsetinvio      pic xx.  
       77  status-lineseq-mail   pic xx.
       77  path-lineseq-mail     pic x(256).
       
       01 el-tor-chiave          occurs 99999.
          05 el-tor-anno         PIC  9(4).
          05 el-tor-numero       PIC  9(8).

       77  wstampa               pic x(256).
                                          
       77  como-data             pic 9(8).
       77  como-ora              pic 9(8).
       77  separatore            pic x.
       77  idx                   pic 9(5) value 0.
       77  tot-idx               pic 9(5) value 0.

       01  filler                pic xx.
         88 tutto-ok             value "OK".
         88 errori               value "ER".

       COPY  "MAIL.DEF".

      ******************************************************************
       PROCEDURE DIVISION.
       DECLARATIVES.
       copy "mail-decl.cpy".
       END DECLARATIVES.

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
           string  wstampa            delimited low-value
                   "report_err_prog_" delimited size
                   como-data          delimited size
                   "_"                delimited size
                   como-ora           delimited size
                   ".csv"             delimited size
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
           open i-o tordini.

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
                       add 1 to tot-idx
                       move tor-chiave to el-tor-chiave(tot-idx)
                    end-if
                 end-perform
           end-start.

           perform varying idx from 1 by 1 
                     until idx > tot-idx
              move el-tor-chiave(idx) to tor-chiave
              read tordini no lock
                   invalid continue
               not invalid 
                   move tor-causale-orig to tor-causale
                   rewrite tor-rec
              end-read
           end-perform.

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
           move 5 to tentativi-mail
           perform CICLO-SEND-MAIL.

      ***---
       AFTER-SEND-MAIL.

      ***---
       EXIT-PGM.
           goback.

      ***---
       PARAGRAFO-COPY.
           copy "mail.cpy". 
