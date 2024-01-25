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
           copy "mrordini.sl". 
           copy "mtordini.sl". 
           copy "EDI-mrordini.sl".
           copy "EDI-mtordini.sl". 
           copy "lineseq.sl".
           COPY "lineseq-mail.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.           
           copy "tsetinvio.fd".
           copy "mrordini.fd". 
           copy "mtordini.fd".     
           copy "EDI-mrordini.fd". 
           copy "EDI-mtordini.fd". 
           copy "lineseq.fd".
           COPY "lineseq-mail.fd".

       WORKING-STORAGE SECTION.

      * COSTANTI
       78  titolo                value "Contollo prog".

      * FILE STATUS                     
       77  status-mrordini       pic xx.
       77  status-mtordini       pic xx.
       77  status-EDI-mrordini   pic xx.
       77  status-EDI-mtordini   pic xx.
       77  status-lineseq        pic xx.
       77  status-lineseq1       pic xx.
       77  status-tsetinvio      pic xx.  
       77  status-lineseq-mail   pic xx.
       77  path-lineseq-mail     pic x(256).   

       77  wstampa               pic x(256).
                                          
       77  como-data             pic 9(8).
       77  como-ora              pic 9(8).
       77  separatore            pic x.           

       01  filler                pic xx.
         88 tutto-ok             value "OK".
         88 errori               value "ER".                

       77  ws-narg               pic 9(3) comp-1 value 0.
       77  anno                  pic 9(4) value 0.
       77  num-from              pic 9(8) value 0.
       77  num-to                pic 9(8) value 0.
       77  statusPgm             signed-short.

       COPY  "MAIL.DEF".

      ******************************************************************
       LINKAGE SECTION.
       77  link-anno-from                pic 9(4).
       77  link-num-from                 pic 9(8).
       77  link-num-to                   pic 9(8).

       PROCEDURE DIVISION USING link-anno-from 
                                link-num-from 
                                link-num-to.
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
           call "C$NARG" using ws-narg.
           if ws-narg > 0
              move link-anno-from to anno    
              move link-num-from  to num-from
              move link-num-to    to num-to
           end-if.

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
           string "Anno"         delimited size
                  separatore     delimited size
                  "N.Master"     delimited size
                  separatore     delimited size
                  "Stato"        delimited size
                  separatore     delimited size
                  "N.Master EDI" delimited size
                  separatore     delimited size
                  "Stato"        delimited size
                  separatore     delimited size
                  "Articolo"     delimited size
                  separatore     delimited size
                  "Progressivo"  delimited size
                  separatore     delimited size
                  "Magazzino"    delimited size
                  separatore     delimited size
                  "Imballo"      delimited size
                  separatore     delimited size
                  "Peso"         delimited size
                  separatore     delimited size
             into line-riga of lineseq
           end-string.
           write line-riga of lineseq.
                   

      ***---
       OPEN-FILES.
           open i-o mtordini EDI-mtordini mrordini EDI-mrordini.

      ***---
       ELABORAZIONE.                   
           if anno = 0
              move low-value to mro-rec
              move 2024      to mro-anno
              start mrordini key >= mro-chiave
                    invalid continue
                not invalid
                    perform until 1 = 2
                       read mrordini next at end exit perform end-read
                       if num-to > 0
                          if mro-anno   > anno or
                             mro-numero > num-to
                             exit perform
                          end-if
                       end-if
                       if mro-cod-articolo not = mro-prg-cod-articolo   
                          move mro-anno   to mto-anno
                          move mro-numero to mto-numero
                          read mtordini no lock invalid continue 
                          end-read  
                          initialize line-riga of lineseq
                          string mro-anno              delimited size
                                 separatore            delimited size
                                 mro-numero            delimited size
                                 separatore            delimited size
                                 mto-stato-ordine      delimited size
                                 separatore            delimited size
                                 ||
                                 separatore            delimited size
                                 ||                              
                                 separatore            delimited size
                                 mro-cod-articolo      delimited size
                                 separatore            delimited size
                                 mro-prg-cod-articolo  delimited size
                                 separatore            delimited size
                                 mro-prg-cod-magazzino delimited size    
                                 separatore            delimited size
                                 mro-prg-tipo-imballo  delimited size
                                 separatore            delimited size
                                 mro-prg-peso          delimited size
                                 separatore            delimited size
                            into line-riga of lineseq
                          end-string
                          write line-riga of lineseq
                          set errori to true
                       end-if
                    end-perform
              end-start
           end-if.

           move low-value to emro-rec. 
           if anno > 0
              move anno     to emro-anno
              move num-from to emro-numero
           else                      
              move 2024      to emro-anno
           end-if.                   

           start EDI-mrordini key >= emro-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read EDI-mrordini next at end exit perform end-read
                    if num-to > 0
                       if emro-anno   > anno or
                          emro-numero > num-to
                          exit perform
                       end-if
                    end-if
                    if emro-cod-articolo not = emro-prg-cod-articolo and
                       not emro-si-blister
                       move emro-anno   to emto-anno
                       move emro-numero to emto-numero
                       read edi-mtordini no lock 
                            invalid continue 
                       end-read  
                       initialize line-riga of lineseq
                       string emro-anno              delimited size
                              separatore             delimited size
                              ||
                              separatore             delimited size
                              ||
                              separatore             delimited size
                              emro-numero            delimited size
                              separatore             delimited size
                              emto-stato             delimited size                              
                              separatore             delimited size
                              emro-cod-articolo      delimited size
                              separatore             delimited size
                              emro-prg-cod-articolo  delimited size
                              separatore             delimited size
                              emro-prg-cod-magazzino delimited size    
                              separatore             delimited size
                              emro-prg-tipo-imballo  delimited size
                              separatore             delimited size
                              emro-prg-peso          delimited size
                              separatore             delimited size
                         into line-riga of lineseq
                       end-string
                       write line-riga of lineseq
                       set errori to true
                    end-if
                 end-perform
           end-start.

      ***---
       CLOSE-FILES.
           close EDI-mrordini mrordini mtordini EDI-mtordini lineseq.

           if tutto-ok
              delete file lineseq
           else
              perform INVIO-MAIL
           end-if.

      ***---
       INVIO-MAIL.    
           move "ERRORI PROGRESSIVI"  to LinkSubject.
           move "In allegato"         to LinkBody.
           move "andrea.ae@live.it"   to LinkAddress.

           move wstampa to LinkAttach                                
                               
           move "check-prg-edi" to NomeProgramma.
           move 5 to tentativi-mail.
      *     perform CICLO-SEND-MAIL.

      ***---
       AFTER-SEND-MAIL.

      ***---
       EXIT-PGM.
           move 0 to statusPgm.
           if errori
              move -1 to statusPgm
           end-if.
           goback statusPgm.

      ***---
       PARAGRAFO-COPY.
           copy "mail.cpy". 
