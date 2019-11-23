       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      aliva-p.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "articoli.sl". 
           copy "mtordini.sl".
           copy "mrordini.sl".
           copy "tordini.sl".
           copy "rordini.sl".                   

      ******************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "articoli.fd". 
           copy "mtordini.fd".
           copy "mrordini.fd".
           copy "tordini.fd".
           copy "rordini.fd". 


      ******************************************************************
       WORKING-STORAGE SECTION.
           COPY "acucobol.def".
           copy "link-geslock.def".
           copy "comune.def".

       77  counter             pic 9(10).
       77  counter2            pic 9(10).
       77  counter-edit        pic z(10).

       78  titolo    value "GESLUX - Cambio Aliquota IVA".
       78  78-clear            value 
           "                                                          ".

       77  status-articoli      pic xx.
       77  status-mtordini      pic xx.
       77  status-mrordini      pic xx.
       77  status-tordini       pic xx.
       77  status-rordini       pic xx.

       LINKAGE SECTION.
           copy "link-aliva.def".

      ******************************************************************
       PROCEDURE DIVISION USING aliva-linkage.

       DECLARATIVES.
       ARTICOLI-ERR SECTION.
           use after error procedure on articoli.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-articoli
           when "39"
                set errori to true
                display message "File [ARTICOLI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[ARTICOLI] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message "File [ARTICOLI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.  

      ***---
       TORDINI-ERR SECTION.
           use after error procedure on tordini.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tordini
           when "39"
                set errori to true
                display message "File [TORDINI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TORDINI] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message "File [TORDINI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.  

      ***---
       RORDINI-ERR SECTION.
           use after error procedure on rordini.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-rordini
           when "39"
                set errori to true
                display message "File [RORDINI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[RORDINI] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message "File [RORDINI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.                

      ***---
       MTORDINI-ERR SECTION.
           use after error procedure on mtordini.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-mtordini
           when "39"
                set errori to true
                display message "File [MTORDINI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[MTORDINI] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message "File [MTORDINI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.  
                                       
      ***---
       MRORDINI-ERR SECTION.
           use after error procedure on mrordini.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-mrordini
           when "39"
                set errori to true
                display message "File [MRORDINI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[MRORDINI] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message "File [MRORDINI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.               

       END DECLARATIVES.

       MAIN-PRG.
           perform INIT.
           perform OPEN-FILES.
           if tutto-ok

              evaluate ali-chiamata
              when 1 perform ELABORAZIONE-1
              when 2 perform ELABORAZIONE-2
              when 3 perform ELABORAZIONE-3
              end-evaluate

              perform CLOSE-FILES
           end-if.
           perform EXIT-PGM.

      ***---
       INIT.
           set RecLocked to false.
           set tutto-ok  to true.

      ***---
       OPEN-FILES.
           open input mtordini tordini
           if tutto-ok
              perform OPEN-IO-ARTICOLI
              if tutto-ok
                 perform OPEN-IO-MRORDINI
                 if tutto-ok
                    perform OPEN-IO-RORDINI
                    if errori
                       close mtordini tordini mrordini articoli
                    end-if
                 else
                    close mtordini tordini articoli
                 end-if
              else
                 close mtordini tordini
              end-if
           else
              goback
           end-if.

      ***---
       OPEN-IO-ARTICOLI.
           string   "Il file degli articoli" 
             x"0d0a""risulta in uso su altro terminale."
                 delimited size
                 into geslock-messaggio
           end-string.
           move "articoli" to geslock-nome-file.

           set tutto-ok   to true.
           perform until 1 = 2
              set RecLocked to false
              open i-o articoli
              if not RecLocked
                 exit perform
              end-if

              move 1 to geslock-v-riprova
              move 0 to geslock-v-ignora
              move 1 to geslock-v-termina
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova continue
              when termina 
                   set errori to true
                   exit perform
              end-evaluate
           end-perform.

      ***---
       OPEN-IO-MRORDINI.
           string   "Il file delle righe master" 
             x"0d0a""risulta in uso su altro terminale."
                 delimited size
                 into geslock-messaggio
           end-string.
           move "mrordini" to geslock-nome-file.

           set tutto-ok   to true.
           perform until 1 = 2
              set RecLocked to false
              open i-o mrordini
              if not RecLocked
                 exit perform
              end-if

              move 1 to geslock-v-riprova
              move 0 to geslock-v-ignora
              move 1 to geslock-v-termina
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova continue
              when termina 
                   set errori to true
                   exit perform
              end-evaluate
           end-perform.

      ***---
       OPEN-IO-RORDINI.
           string   "Il file delle righe evasione" 
             x"0d0a""risulta in uso su altro terminale."
                 delimited size
                 into geslock-messaggio
           end-string.
           move "rordini" to geslock-nome-file.

           set tutto-ok   to true.
           perform until 1 = 2
              set RecLocked to false
              open i-o rordini
              if not RecLocked
                 exit perform
              end-if

              move 1 to geslock-v-riprova
              move 0 to geslock-v-ignora
              move 1 to geslock-v-termina
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova continue
              when termina 
                   set errori to true
                   exit perform
              end-evaluate
           end-perform. 
      
      ***---
       ELABORAZIONE-1.
           move 0 to ali-articoli.
           move low-value to art-rec.
           start articoli key >= art-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read articoli next at end exit perform end-read
                    if art-codice-iva = ali-iva-old
                       move ali-iva-new to art-codice-iva
                       rewrite art-rec
                       add 1 to ali-articoli
                    end-if
                 end-perform
           end-start.
      
      ***---
       ELABORAZIONE-2.
           move 0 to ali-master.
           move low-value      to mto-rec.
           set  mto-registrato to true.
           start mtordini key >= k-mto-stato
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read mtordini next at end exit perform end-read
                    if mto-chiuso or mto-sped-tot
                       exit perform
                    end-if
                    perform LOOP-RIGHE-MASTER
                 end-perform
           end-start.

      ***---
       LOOP-RIGHE-MASTER.
           move low-value  to mro-rec.
           move mto-anno   to mro-anno.
           move mto-numero to mro-numero.
           start mrordini key >= mro-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read mrordini next at end exit perform end-read
                    if mro-chiave-testa not = mto-chiave
                       exit perform
                    end-if
                    perform CONTATORE-SCREEN
                    if mro-chiuso
                       continue
                    else
                       if mro-qta > mro-qta-e
                          if mro-cod-iva = ali-iva-old
                             move ali-iva-new to mro-cod-iva
                             rewrite mro-rec
                             add 1 to ali-master  
                          end-if
                       end-if
                    end-if
                 end-perform
           end-start.
      
      ***---
       ELABORAZIONE-3.
           move 0 to ali-evasioni.
           move 0 to tor-anno-fattura.
           move 0 to tor-num-fattura.
           start tordini key >= k-fattura
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tordini next at end exit perform end-read
                    if tor-anno-fattura not = 0 or
                       tor-num-fattura  not = 0
                       exit perform
                    end-if
                    perform LOOP-RIGHE-EVASIONI
                 end-perform
           end-start.

      ***---
       LOOP-RIGHE-EVASIONI.
           move low-value  to ror-rec.
           move tor-anno   to ror-anno.
           move tor-numero to ror-num-ordine.
           start rordini key >= ror-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rordini next at end exit perform end-read
                    if ror-anno       not = tor-anno or
                       ror-num-ordine not = tor-numero
                       exit perform
                    end-if
                    perform CONTATORE-SCREEN
                    if ror-cod-iva = ali-iva-old
                       move ali-iva-new to ror-cod-iva
                       rewrite ror-rec
                       add 1 to ali-evasioni
                    end-if
                 end-perform
           end-start.

      ***---
       CONTATORE-SCREEN.
           add 1 to counter
           add 1 to counter2
           if counter2 = 10
              move counter to counter-edit
              display counter-edit
                 upon ali-handle at column 35,00
                                      line 30,00
              move 0 to counter2
           end-if.

      ***---
       RESET-CONTATORE.
           move 0 to counter.
           move 0 to counter2.
           move counter to counter-edit
           display counter-edit
             upon ali-handle at column 35,00
                                  line 30,00.


      ***---
       CLOSE-FILES.
           close articoli mtordini mrordini tordini rordini.

      ***---
       EXIT-PGM.
           goback.
