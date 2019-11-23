       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      car-catart-p.
       AUTHOR.                          Luciano.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "articoli.sl". 
           COPY "articoli.sl"
                REPLACING ==articoli== BY ==articoli1==,
                          ==STATUS-articoli== BY ==STATUS-articoli1==.
           copy "catart.sl".
           copy "catart.sl"
                REPLACING ==catart== BY ==catart1==,
                          ==STATUS-catart== BY ==STATUS-catart1==.

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           COPY "articoli.fd".
           COPY "articoli.fd"
                REPLACING ==articoli== BY ==articoli1==,
                          ==STATUS-articoli== BY ==STATUS-articoli1==.
           copy "catart.fd".
           copy "catart.fd"
                REPLACING ==catart== BY ==catart1==,
                          ==STATUS-catart== BY ==STATUS-catart1==.

       WORKING-STORAGE SECTION.
           copy "link-geslock.def".
           copy "comune.def".

       77  status-articoli   pic xx.  
       77  status-articoli1  pic xx.  
       77  status-catart     pic xx.
       77  status-catart1    pic xx.


       78  titolo    value "Creazione catena articoli".

       77  como-data         pic 9(8).
       77  como-ora          pic 9(8).

      ******************************************************************
       PROCEDURE DIVISION.

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
                display message box        "Impossibile procedere."
                  x"0d0a""File delle testate [ARTICOLI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.  
                           
       ARTICOLI1-ERR SECTION.
           use after error procedure on articoli1.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-articoli1
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
                display message box        "Impossibile procedere."
                  x"0d0a""File delle testate [ARTICOLI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.  
                           
       CATART-ERR SECTION.
           use after error procedure on catart.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-catart
           when "39"
                set errori to true
                display message "File [CATART] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[CATART] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File delle righe [CATART] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.  

       CATART1-ERR SECTION.
           use after error procedure on catart1.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-catart1
           when "39"
                set errori to true
                display message "File [CATART] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[CATART] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File delle righe [CATART] inesistente"
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
              perform ELABORAZIONE
              perform CLOSE-FILES
           end-if.
           perform EXIT-PGM.

      ***---
       INIT.
           accept como-data  from century-date.
           accept como-ora   from time.

           set RecLocked to false.
           set tutto-ok  to true.

      ***---
       OPEN-FILES.
           perform OPEN-IO-CATART.
           if tutto-ok
              open input articoli
              open input articoli1
              if errori
                 close catart
                 goback
              end-if
           else
              goback
           end-if.

      ***---
       OPEN-IO-CATART.
           string   "Il file delle catene degli articoli" 
             x"0d0a""risulta in uso su altro terminale."
             x"0d0a""Questo comporta l'impossibilità a"
             x"0d0a""ricreare le catene." delimited size
                 into geslock-messaggio
           end-string.  

           perform until 1 = 2
              set RecLocked to false
              open i-o catart
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
              when termina set errori to true
                           exit perform
                           display message box "Operazione interrotta!"
                                   title titolo
                                   icon 2
              end-evaluate
           end-perform.
           |Nessuna OPEN OUTPUT x' da problemi di lock
           if tutto-ok
              move low-value to cat-rec of catart
              start catart key >= cat-chiave of catart
                    invalid continue
                not invalid
                    perform until 1 = 2
                       read   catart next at end exit perform end-read
                       delete catart record
                    end-perform
              end-start
           end-if.
           open i-o catart1.

      ***---
       ELABORAZIONE.
           move low-value to art-codice of articoli
           start articoli key not < art-codice of articoli
              invalid
                 continue
              not invalid
                 perform until 1 = 2
                    read articoli next no lock
                       at end
                          exit perform
                    end-read

                    if art-collegato of articoli not = zero
                       perform ELABORA-RECORD
                    end-if
                 end-perform
           end-start.
                          
      ***---
       ELABORA-RECORD.
           set tutto-ok   to true
      *    per prima cosa guardo di non aver già inserito l'articolo 
      *    come secondario, in tal caso non valorizzo la sua catena
           move art-codice of articoli   to cat-codice of catart
           move low-value to cat-princ of catart
           start catart key not < cat-chiave of catart
              invalid
                 continue
              not invalid
                 read catart next no lock
                    at end
                       continue
                    not at end
                       if art-codice of articoli = cat-codice of catart
      *    vul dire che l'articolo fa già parte di una catena quindi non
      *    vado a valorizzare la sua catena
                          set errori  to true
                       end-if
                 end-read
           end-start.

      *    Luciano 02/09/2010
      *    controllo che l'articolo non sia un elemento interno di una 
      *    catena non ancora valorizzata. 
      *    esempio: articoli collegati in questo ordine:
      *    1163 - 1150 - 3389, devo fare in modo che quando gestisco 
      *    l'articolo 1150 non inizio a creare la catena
           if tutto-ok
              move art-codice of articoli to art-collegato of articoli1
              start articoli1 key not < art-collegato of articoli1
                 invalid 
                    continue
                 not invalid
                    perform until 1 = 2
                       read ARTICOLI1 next 
                          at end
                             exit perform
                       end-read
                       if art-collegato of articoli1 not = 
                                   art-codice of articoli
                          exit perform
                       else
      *    vuol dire che sono collegato ad un altro articolo e quindi scarto
      *    l'articolo
                          set errori  to true
                          exit perform
                       end-if

                    end-perform
              end-start
           end-if
      *    Luciano 02/09/2010 fine


           if tutto-ok
              perform VAL-CATENA
           end-if.

      ***---
       VAL-CATENA.
           initialize cat-rec of catart
                             REPLACING NUMERIC       DATA BY ZEROS
                             ALPHANUMERIC  DATA BY SPACES
                             ALPHABETIC    DATA BY SPACES.

           move art-codice of articoli   to cat-codice of catart
           move zero                     to cat-princ of catart

           move zero   to cat-num-el-catena of catart.

           move art-collegato of articoli   to art-codice of articoli1
           perform until 1 = 2
              read articoli1 no lock
                 invalid
                    continue
              end-read

              add 1 to cat-num-el-catena of catart

              move art-codice of articoli1  
                 to cat-collegato of catart(cat-num-el-catena of catart)

              perform VAL-CATENA-SEC

              if art-collegato of articoli1 = zero
                 exit perform
              end-if

              move art-collegato of articoli1 to art-codice of articoli1
           end-perform.

           move "BOSS"          to cat-utente-creazione       of catart
                                   cat-utente-ultima-modifica of catart
           move como-data       to cat-data-creazione         of catart
                                   cat-data-ultima-modifica   of catart
           move como-ora        to cat-ora-ultima-modifica    of catart
                                   cat-ora-creazione          of catart

           write cat-rec of catart
              invalid
                 continue
           end-write.

      ***---
       VAL-CATENA-SEC.
           initialize cat-rec of catart1
                             REPLACING NUMERIC       DATA BY ZEROS
                             ALPHANUMERIC  DATA BY SPACES
                             ALPHABETIC    DATA BY SPACES.
           move art-codice of articoli1  to cat-codice of catart1.
           move art-codice of articoli   to cat-princ  of catart1.

           move "BOSS"    to cat-utente-creazione       of catart1
                             cat-utente-ultima-modifica of catart1
           move como-data to cat-data-creazione         of catart1
                             cat-data-ultima-modifica   of catart1
           move como-ora  to cat-ora-ultima-modifica    of catart1
                             cat-ora-creazione          of catart1

           write cat-rec of catart1
              invalid
                 continue
           end-write.

      ***---
       CLOSE-FILES.
           close articoli articoli1 catart catart1.
           unlock catart  all records.
           unlock catart1 all records.

      ***---
       EXIT-PGM.
           goback.
