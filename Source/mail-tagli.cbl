       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      mail-tagli.
       AUTHOR.                          Luciano.
       REMARKS. programma di invio mail dei tagli. Batch 
                schedulato sul server. Il programma stampa i tagli per 
                il giorno precedente a quello di esecuzione. Nel log di 
                SYSERR metto il log di funzionamento.
      ******************************************************************
                                                                              
       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tsetinvio.sl".
           copy "lineseq.sl".
           copy "tagli.sl".
           copy "mrordini.sl".
           copy "lineseq-mail.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tsetinvio.fd".
           copy "lineseq.fd".
           copy "tagli.fd".
           copy "mrordini.fd".
           copy "lineseq-mail.fd".

       WORKING-STORAGE SECTION.
           copy "mail.def".
           copy "link-lab-inevaso.def".

       78  titolo value  "Gestione Tagli".
       01  r-inizio              pic x(25).

       77  status-tagli            pic xx.
       77  status-lineseq          pic xx.
       77  status-tsetinvio        pic xx.
       77  status-mrordini         pic xx.
       77  status-lineseq-mail     pic xx.
       77  path-lineseq-mail       pic x(256).

       77  wstampa               pic x(256).
                                            
      * 77  FileDest              pic x(256).
      ** 77  FileOrig              pic x(256).
       77  como-data             pic 9(8).
       77  como-ora              pic 9(8).  
       77  como-riga             pic x(400).
       77  riga-stampa           pic x(400).
       77  nargs                 pic 99 comp-1 value 0.
       77  data-oggi             pic 9(8).

      * FLAGS
       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".
       77  filler                pic 9.
           88 RecLocked          value 1, false 0.
       77  filler                pic 9.
           88 si-tagli           value 1, false 0.
                  

      ***** 77  calling-program          pic x(20).

      ***** 01                    pic 9.
      *****     88 on-line  value 1 false zero.
       01  RichiamoSchedulatoFlag pic 9 value 0.
           88 RichiamoSchedulato       value 1, false 0.
       01  filler                pic 9 value 0.
           88 invia-mail         value 1, false 0. 

       77  cont           pic 9(5).
       77  cont2          pic 9(4).
       77  cont3          pic 9(4).
       77  como-stringa   pic x(3000).
       77  como-stringa-2 pic x(3000).
       77  sostituto      pic x(50).
       77  da-sostituire  pic x(2).

       77  inevasa         pic s9(10)v99.

       77  stato                 pic 9.
           88 nessun-errore      value 1.
           88 errore-warning     value 2.
           88 errore-error       value 3.


       LINKAGE SECTION.
           copy "link-batch.def".

      ******************************************************************
       PROCEDURE DIVISION USING batch-linkage.

       DECLARATIVES. 
       copy "mail-decl.cpy".
      ***---
       TAGLI-ERR SECTION.
           use after error procedure on tagli.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tagli
           when "35"
                move "File [tagli] inesistente" to como-riga
      *****          if on-line
      *****             inspect como-riga 
      *****                          replacing trailing space by low-value
      *****             display message box como-riga
      *****                    title titolo
      *****          else
                   perform SETTA-RIGA-STAMPA
      *****          end-if
                set errori to true
                set errore-error to true
           when "39"
                move "File [tagli] mismatch size!" to como-riga
      *****          if on-line
      *****             inspect como-riga 
      *****                          replacing trailing space by low-value
      *****             display message box como-riga
      *****                    title titolo
      *****          else
                   perform SETTA-RIGA-STAMPA
      *****          end-if
                set errori to true      
                set errore-error to true
           when "98"
                move "[tagli] Indexed file corrupt!" to como-riga
      *****          if on-line
      *****             inspect como-riga 
      *****                          replacing trailing space by low-value
      *****             display message box como-riga
      *****                    title titolo
      *****          else
                   perform SETTA-RIGA-STAMPA
      *****          end-if
                set errori to true      
                set errore-error to true
           when "93"
           when "99" 
                set RecLocked to true
           end-evaluate.

      ***---
       LINESEQ-ERR SECTION.
           use after error procedure on lineseq.
           evaluate status-lineseq
           when "42" continue
           end-evaluate.


       END DECLARATIVES.

      ***---
       MAIN-PRG.
           perform INIT.
           perform OPEN-FILES
           if tutto-ok
              perform ELABORAZIONE
              perform CLOSE-FILES
           end-if.
           perform EXIT-PGM.

      ***---
       INIT.
           set nessun-errore to true.
      *****     call "C$CALLEDBY"  using calling-program.
      *****     set on-line  to false
           set tutto-ok   to true.
           set invia-mail to false.
           move 0    to cont.                

           CALL "C$NARG" USING NARGS.
           if nargs not = 0
              set RichiamoSchedulato to true
           else
              set RichiamoSchedulato to false
           end-if.
           if RichiamoSchedulato
              move  0 to batch-status
              initialize wstampa
              accept como-data from century-date
              accept como-ora  from time
              accept  wstampa from environment "SCHEDULER_PATH_LOG"
              inspect wstampa replacing trailing spaces by low-value
              string  wstampa       delimited low-value
                      "MAIL_TAGLI_" delimited size
                      como-data     delimited size
                      "_"           delimited size
                      como-ora      delimited size
                      ".log"        delimited size
                      into wstampa
              end-string
              set RichiamoSchedulato to true
              move wstampa to batch-log
              open output lineseq
           end-if.

           move "INIZIO PROGRAMMA" to como-riga.
           perform SETTA-RIGA-STAMPA.

      ***---
       OPEN-FILES.
           move "APERTURA FILES" to como-riga.
           perform SETTA-RIGA-STAMPA.
           open input tagli mrordini. 

           if errori
              set errore-error to true
              move "APERTURA FILES NON RIUSCITA" to como-riga
      *****        if on-line
      *****           inspect como-riga replacing trailing space by low-value
      *****           display message como-riga
      *****                     title titolo
      *****        else
                 perform SETTA-RIGA-STAMPA
      *****        end-if
           else
              move "APERTURA FILES RIUSCITA" to como-riga
      *****        if not on-line
                 perform SETTA-RIGA-STAMPA
      *****        end-if
           end-if.

      ***---
       ELABORAZIONE.
           accept data-oggi  from century-date

      *     move 20100506  to data-oggi

           compute data-oggi = FUNCTION integer-of-date (data-oggi)
           subtract 1 from data-oggi
           compute data-oggi = FUNCTION date-of-integer (data-oggi)
                                            
           perform CANCELLAZIONE-TAGLI.

           initialize como-riga.
           string "CONTROLLO PRESENZA TAGLI DEL: " delimited size
                  data-oggi(7:2)                   delimited size
                  "/"                              delimited size
                  data-oggi(5:2)                   delimited size
                  "/"                              delimited size
                  data-oggi(1:4)                   delimited size
                  into como-riga
           end-string.
           perform SETTA-RIGA-STAMPA.

           set si-tagli   to false
           move data-oggi to tag-data
           move low-value to tag-gdo tag-articolo tag-prog
      
           start tagli key >= tag-data
                 invalid continue
             not invalid
                 read tagli next no lock
                      at end continue
                 end-read
                 if tag-data = data-oggi
                    set si-tagli to true
                 end-if
           end-start.
      
           if si-tagli
              perform ELABORA-2
              if nessun-errore
                 set invia-mail to true
              end-if
           else
              move "NESSUN TAGLIO PRESENTE"  to como-riga
              perform SETTA-RIGA-STAMPA
           end-if.

      ***---
       ELABORA-2. 
           move "CREAZIONE DEL PDF DEI TAGLI" to como-riga.
           perform SETTA-RIGA-STAMPA.

           move data-oggi to lin-data
           set lin-pdf    to true
           if RichiamoSchedulato
              call   "lab-inevaso" using RichiamoSchedulatoFlag
                                         lab-inevaso-linkage
                                         batch-linkage
           else
              call   "lab-inevaso" using RichiamoSchedulatoFlag
                                         lab-inevaso-linkage
           end-if.
           cancel "lab-inevaso".

           if RichiamoSchedulato
              if batch-log = -1 
                 set errore-error to true 
              end-if
           end-if.

           if errore-error
              move "CREAZIONE PDF NON RIUSCITA" to como-riga
           else
              move "CREAZIONE PDF TERMINATA"    to como-riga
           end-if.
           perform SETTA-RIGA-STAMPA. 
 
      ***---
       CANCELLAZIONE-TAGLI.
           close    tagli.
           open i-o tagli.
           initialize como-riga.
           string "CANCELLAZIONE TAGLI DEL: " delimited size
                  data-oggi(7:2)              delimited size
                  "/"                         delimited size
                  data-oggi(5:2)              delimited size
                  "/"                         delimited size
                  data-oggi(1:4)              delimited size
                  into como-riga
           end-string.
           perform SETTA-RIGA-STAMPA.

           move data-oggi to tag-data.
      
           start tagli key >= tag-data
                 invalid set errori to true
             not invalid
                 perform until 1 = 2
                    read tagli next at end exit perform end-read
                    if tag-data not = data-oggi
                       exit perform
                    end-if  

                    move tag-mro-chiave to mro-chiave
                    read mrordini no lock
                         invalid perform ELIMINA-TAGLIO
                     not invalid
                         if mro-chiuso
                            compute inevasa = mro-qta - mro-qta-e
                            if inevasa < tag-qta
                               perform ELIMINA-TAGLIO
                            end-if
                         else
                            perform ELIMINA-TAGLIO
                         end-if
                    end-read

                 end-perform
           end-start.
           
           initialize como-riga.
           string "CANCELLATI " delimited size
                  cont          delimited size
                  " TAGLI"      delimited size
                  into como-riga
           end-string.
           perform SETTA-RIGA-STAMPA.
           close      tagli.
           open input tagli.
       

      ***---
       ELIMINA-TAGLIO.
           add 1 to cont.
           delete tagli record invalid continue end-delete.

      ***---
       SETTA-RIGA-STAMPA.
      *****     if on-line exit paragraph end-if.

           initialize riga-stampa.
           perform SETTA-INIZIO-RIGA.
           string r-inizio  delimited size
                  como-riga delimited size
                  into riga-stampa
           end-string.
           if RichiamoSchedulato
              write line-riga of lineseq from riga-stampa
           else
              display riga-stampa upon syserr
           end-if.

      ***---
       INVIO-MAIL.
           move "$1"   to da-sostituire
           initialize sostituto
           string data-oggi(7:2)   delimited by size
                  "/"              delimited by size
                  data-oggi(5:2)   delimited by size
                  "/"              delimited by size
                  data-oggi(1:4)   delimited by size
                  into sostituto

           accept LinkAddress from environment "MAIL_TAGLI_ADDRESSES"
           accept LinkAddressCC 
                              from environment "MAIL_TAGLI_ADDRESSES_CC"
           accept LinkSubject from environment "MAIL_TAGLI_SUBJECT"
           if LinkSubject = space
              move "Riepilogo gestione tagli in data $1"   
                    to LinkSubject
           end-if
           move LinkSubject  to como-stringa
           perform SOSTITUZIONE.
           move como-stringa to LinkSubject

           accept LinkBody from environment "MAIL_TAGLI_BODY".

           if LinkBody = space
              move "In allegato il riepilogo gestione tagli in data $1"   
                    to LinkBody
           end-if
           move LinkBody  to como-stringa
           perform SOSTITUZIONE.
           move como-stringa to LinkBody

           move lin-path-pdf  to LinkAttach

           move 5 to tentativi-mail.
           move "mail-tagli" to NomeProgramma.
           perform CICLO-SEND-MAIL.

           delete file lineseq-mail.

      ***---
       AFTER-SEND-MAIL.
           initialize como-riga
           string "TENTATIVO N. "  delimited size
                  tentativo-mail   delimited size
                  " "              delimited size
                  "STATUS: "       delimited size
                  StatusInvioMail  delimited size
                  " - "            delimited size
                  line-riga-mail   delimited size
             into como-riga
           end-string
           perform SETTA-RIGA-STAMPA.

      ***---
       SOSTITUZIONE.
           inspect sostituto replacing trailing space by low-value
           initialize cont
           inspect como-stringa tallying cont for all da-sostituire
           if cont not = zero
              perform cont times
                 initialize cont2
                 inspect como-stringa tallying cont2 
                                     for characters before da-sostituire
                 add 3 to cont2 giving cont3
                 initialize como-stringa-2
                 if cont2 = zero
                    string sostituto              delimited by low-value
                           como-stringa(cont3:)   delimited by size
                           into como-stringa-2
                 else
                    string como-stringa(1:cont2)  delimited by size
                           sostituto              delimited by low-value
                           como-stringa(cont3:)   delimited by size
                           into como-stringa-2
                 end-if
                 move como-stringa-2  to como-stringa
              end-perform
           end-if.

      ***--
       CLOSE-FILES.
           close tagli mrordini. 

      ***---
       EXIT-PGM.
           move "TERMINE PROGRAMMA" to como-riga.
           perform SETTA-RIGA-STAMPA.

           if invia-mail        
              move "INVIO MAIL IN CORSO..." to como-riga
              perform SETTA-RIGA-STAMPA
              if wstampa not = spaces
                 close lineseq
              end-if

              perform INVIO-MAIL

              if RichiamoSchedulato
                 move batch-log to wstampa
                 open extend lineseq
              end-if
               
              initialize como-riga
              if mail-ok
                 move "INVIO MAIL RIUSCITO!"   to como-riga
              else              
                 set errore-warning to true
                 string "INVIO MAIL NON RIUSCITO! " delimited size
                        line-riga-mail              delimited size
                        into como-riga
                 end-string
              end-if
      *****     if on-line
      *****        inspect como-riga replacing trailing space by low-value
      *****        display message box como-riga
      *****               title titolo
      *****
      *****     else
              perform SETTA-RIGA-STAMPA
      *****     end-if                 
           end-if.       
           if RichiamoSchedulato
              evaluate true
              when nessun-errore  move  0 to batch-status
              when errore-warning move  1 to batch-status
              when errore-error   move -1 to batch-status
              end-evaluate
              close lineseq     
              display "                                             "
                 upon batch-win-handle
                   line 25,00
                 column 35,00
           end-if.

           close lineseq.

           goback.

      ***---
       PARAGRAFO-COPY.
           copy "mail.cpy".
           copy "setta-inizio-riga.cpy".
