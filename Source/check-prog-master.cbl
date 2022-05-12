       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      check-prog-master.
       AUTHOR.                          Andrea.
       REMARKS. Controllo in fase di salvataggio che i progressivi 
                del master siano corretti.
                Controlla che in uscita non ci siano errori e notifica
                l'eventuale anomalia tramite mail segnalando se il 
                master in ingresso fosse corretto indicando un 
                malfunzionamento nel programma stesso.
                RICHIAMARE SEGUITO DA CANCEL SON IN USCITA!!!!
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tsetinvio.sl".
           copy "mrordini.sl".
           copy "lineseq.sl". 
           COPY "lineseq-mail.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tsetinvio.fd".
           copy "mrordini.fd".
           copy "lineseq.fd".
           COPY "lineseq-mail.fd".

       WORKING-STORAGE SECTION.
           copy "mail.def".

       78  titolo               value "Controllo progressivi su master".

       77  status-mrordini       pic xx.
       77  status-tsetinvio      pic xx.
       77  status-lineseq        pic xx.
       77  wstampa               pic x(256).   

       77  status-lineseq-mail   pic xx.
       77  path-lineseq-mail     pic x(256).
                                            
       77  FileDest              pic x(256).
       77  FileOrig              pic x(256).   
       77  tentativi             pic 99.

       77  save-progr            pic 9(5).

       77  como-ora              pic 9(8).
       77  como-data             pic 9(8).

       77  como-riga             pic x(160).

       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".
      
       77  filler                pic 9.
           88 RecLocked          value 1, false 0.
       77  filler                pic 9.
           88 errore-ingresso    value 1, false 0.
       77  filler                pic 9.
           88 errore-uscita      value 1, false 0.
       77  filler                pic 9.
           88 ingresso-tested    value 1, false 0.

       LINKAGE SECTION.
       01  link-cpm-chiave.
         05 link-cpm-anno        pic 9(4).
         05 link-cpm-numero      pic 9(8).
       01  link-cpm-operazione   pic x.
           88 link-cpm-ingresso  value "I".
           88 link-cpm-uscita    value "U".

      ******************************************************************
       PROCEDURE DIVISION USING link-cpm-chiave link-cpm-operazione.

       DECLARATIVES.
       copy "mail-decl.cpy".

      ***---
       MRORDINI-ERR SECTION.
           use after error procedure on mrordini.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-mrordini
           when "35"
                display message "File [MRORDINI] inesistente!"
                          title titolo
                           icon 2
           when "39"
                display message "File [MRORDINI] mismatch size!"
                          title titolo
                           icon 2
           when "41" |LREADY OPEN IN USCITA DATO CHE NON CE' LA CANCEL
                close      mrordini
                open input mrordini
           when "98"
                display message "File [MRORDINI] Indexed file corrupt!"
                          title titolo
                           icon 2
           when "93"
           when "99" set RecLocked to true
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
           move 0 to save-progr.
           if link-cpm-ingresso
              set errore-ingresso to false
              set errore-uscita   to false
              set ingresso-tested to true
           end-if.

      ***---
       OPEN-FILES.
           open input mrordini.

      ***---
       ELABORAZIONE.
           move low-value   to mro-rec.
           move link-cpm-chiave to mro-chiave.
           start mrordini key >= mro-k-progr
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read mrordini next at end exit perform end-read
                    if mro-chiave-testa not = link-cpm-chiave
                       exit perform 
                    end-if
                    if save-progr = 0
                       continue
                    else
                       if mro-progr = save-progr
                          if link-cpm-ingresso
                             set errore-ingresso to true
                          end-if
                          if link-cpm-uscita
                             set errore-uscita to true
                             perform INVIO-MAIL
                             exit perform
                          end-if
                       end-if
                    end-if 
                    move mro-progr to save-progr
                 end-perform
           end-start.

      ***--
       CLOSE-FILES.
           close mrordini.

      ***---
       EXIT-PGM.
           goback.

      ***---
       INVIO-MAIL.
           initialize LinkBody.
           string "ERRORE PROGRESSIVI MASTER - ANNO: " delimited size
                  link-cpm-anno                        delimited size
                  " - NUMERO: "                        delimited size
                  link-cpm-numero                      delimited size
                  into LinkSubject
           end-string.
      
           if not ingresso-tested
              move "FASE DI CREAZIONE!!!" to LinkBody
           else
              if errore-ingresso
                 move "IN INGRESSO IL MASTER ERA SBAGLIATO!!!"  
                   to LinkBody
              else
                 move "IN INGRESSO IL MASTER ERA CORRETTO!!!" 
                   to LinkBody
              end-if
           end-if
      
           accept LinkAddress from environment "CPM_ADDRESSES".
      
           set errori to true.
           move 0 to tentativi.
                               
           move "check-prog-master" to NomeProgramma.
           perform 5 times
              add 1 to tentativi
              perform SEND-MAIL
              
              open input lineseq-mail
              read  lineseq-mail next
              if line-riga-mail = "True"
                 set tutto-ok to true
                 close lineseq-mail
                 exit perform
              end-if
              close lineseq-mail
           end-perform
               
           initialize como-riga.
           if errori
              display message "INVIO MAIL NON RIUSCITO"
                       x"0d0a"line-riga-mail
                        title titolo
                         icon 2
           end-if.
      
           delete file lineseq.

      ***---
       PARAGRAFO-COPY.
           copy "mail.cpy".
