       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      accorr-p.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tordini.sl". 
           copy "rordini.sl".
      *****     copy "btordini.sl".
      *****     copy "brordini.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tordini.fd".
           copy "rordini.fd".
      *****     copy "btordini.fd".
      *****     copy "brordini.fd".

       WORKING-STORAGE SECTION.
           COPY "acucobol.def".
           copy "link-geslock.def".
           copy "comune.def".

       78  titolo    value "Accorpamento Corrispettivi".
       78  78-clear              value 
           "                                                          ".

       77  tor-numero-edit  pic z(8).
       77  riga-edit        pic zz.zz9.
       77  status-tordini   pic xx.  
       77  status-rordini   pic xx.
      ***** 77  status-btordini  pic xx.  
      ***** 77  status-brordini  pic xx.
       01  PrimaChiave.
           05 primo-anno    pic 9(4).
           05 primo-numero  pic 9(8).
       77  prima-riga       pic 9(10).

       LINKAGE SECTION.
       copy "link-accorr-p.def".

      ******************************************************************
       PROCEDURE DIVISION using accorr-linkage.

       DECLARATIVES.
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
                display message box        "Impossibile procedere."
                  x"0d0a""File delle testate [TORDINI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.  
                           
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
                display message box        "Impossibile procedere."
                  x"0d0a""File delle righe [RORDINI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.  

      ********---
      ***** BTORDINI-ERR SECTION.
      *****     use after error procedure on btordini.
      *****     set RecLocked to false.
      *****     set tutto-ok  to true.
      *****     evaluate status-btordini 
      *****     when "39"
      *****          set errori to true
      *****          display message "File [BTORDINI] mismatch size!"
      *****                    title titolo
      *****                     icon 3
      *****     when "98"
      *****          set errori to true
      *****          display message "[BTORDINI] Indexed file corrupt!"
      *****                    title titolo
      *****                     icon 3
      *****     when "35"
      *****          display message box        "Impossibile procedere."
      *****            x"0d0a""File delle testate [BTORDINI] inesistente"
      *****                  title = titolo
      *****                  icon 2
      *****          set errori to true
      *****     when "93"
      *****     when "99" set RecLocked to true
      *****     end-evaluate.  
      ***** 
      ********---              
      ***** BRORDINI-ERR SECTION.
      *****     use after error procedure on brordini.
      *****     set RecLocked to false.
      *****     set tutto-ok  to true.
      *****     evaluate status-brordini
      *****     when "39"
      *****          set errori to true
      *****          display message "File [BRORDINI] mismatch size!"
      *****                    title titolo
      *****                     icon 3
      *****     when "98"
      *****          set errori to true
      *****          display message "[BRORDINI] Indexed file corrupt!"
      *****                    title titolo
      *****                     icon 3
      *****     when "35"
      *****          display message box        "Impossibile procedere."
      *****            x"0d0a""File delle righe [BRORDINI] inesistente"
      *****                  title = titolo
      *****                  icon 2
      *****          set errori to true
      *****     when "93"
      *****     when "99" set RecLocked to true
      *****     end-evaluate.  
       END DECLARATIVES.

       MAIN-PRG.
           perform INIT.
           perform OPEN-FILES.
           if tutto-ok
              perform ELABORAZIONE
              perform EXIT-PGM
           end-if.

      ***---
       INIT.
           move 0        to link-status.
           set RecLocked to false.
           set tutto-ok  to true.
           if PrimaChiave = "000000000000"
              move link-anno   to primo-anno
              move link-numero to primo-numero
              move 0           to prima-riga
           end-if.

      ***---
       OPEN-FILES.
           perform OPEN-IO-TORDINI.
           if tutto-ok     
      *****        open i-o btordini
              perform OPEN-IO-RORDINI
              if errori
                 close tordini |btordini
                 goback    
      *****        else
      *****           open i-o brordini
              end-if
           else
              goback
           end-if.

      ***---
       OPEN-IO-TORDINI.
           string   "Il file delle testate degli ordini" 
             x"0d0a""risulta in uso su altro terminale."
             x"0d0a""Questo comporta l'impossibilità ad"
             x"0d0a""aggiornare i corrispettivi." delimited size
                 into geslock-messaggio
           end-string.

           set tutto-ok   to true.
           move "TORDINI" to geslock-nome-file.
           open i-o tordini.
           if RecLocked                  
              move 1 to geslock-v-riprova
              move 0 to geslock-v-ignora
              move 1 to geslock-v-termina
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova perform OPEN-IO-TORDINI
              when termina set errori to true
                           display message box "Operazione interrotta!"
                                   title titolo
                                   icon 2
                           move -1 to link-status
              end-evaluate
           end-if.

      ***---
       OPEN-IO-RORDINI.
           string   "Il file delle righe degli ordini" 
             x"0d0a""risulta in uso su altro terminale."
             x"0d0a""Questo comporta l'impossibilità ad"
             x"0d0a""aggiornare i corrispettivi." delimited size
                 into geslock-messaggio
           end-string.

           set tutto-ok   to true.
           move "RORDINI" to geslock-nome-file.
           open i-o rordini.
           if RecLocked
              move 1 to geslock-v-riprova
              move 0 to geslock-v-ignora
              move 1 to geslock-v-termina
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova perform OPEN-IO-RORDINI
              when termina set errori to true
                           display message box "Operazione interrotta!"
                                   title titolo
                                   icon 2
                           move -1 to link-status
              end-evaluate
           end-if.
      
      ***---
       ELABORAZIONE.
           move link-anno   to tor-anno.
           move link-numero to tor-numero.
           read tordini no lock 
                invalid continue
            not invalid
                perform READ-TORDINI-LOCK
                if tutto-ok            
PATCH *****             move tor-rec to btor-rec
PATCH *****             read btordini lock invalid continue end-read
                   perform ELABORA-RECORD
                end-if
           end-read.

      ***---
       ELABORA-RECORD.
           if tutto-ok
              move low-value  to ror-rec
              move tor-chiave to ror-chiave
              start rordini key is >= ror-chiave
                    invalid continue
              end-start
              perform until 1 = 2

                 read rordini next no lock at end exit perform end-read
                 if errori
                    exit perform
                 end-if
                 if tor-anno   not = ror-anno      or
                    tor-numero not = ror-num-ordine
                    exit perform
                 end-if
                 perform READ-RORDINI-LOCK
                 if errori exit perform end-if
                 delete rordini record invalid continue end-delete

PATCH *****           move ror-rec to bror-rec
PATCH *****           delete brordini record invalid continue end-delete

                 add 1 to prima-riga
                 move primo-anno   to ror-anno
                 move primo-numero to ror-num-ordine
                 move prima-riga   to ror-num-riga
                 write ror-rec invalid rewrite ror-rec end-write

PATCH *****           move ror-rec to bror-rec
PATCH *****           write bror-rec invalid rewrite bror-rec end-write

              end-perform

              if tutto-ok
                 if tor-anno   = primo-anno   and
                    tor-numero = primo-numero
                    move link-data  to tor-data-passaggio-ordine
                    rewrite tor-rec 
                            invalid move 0          to link-numero
                        not invalid move tor-numero to link-numero
                    end-rewrite
PATCH *****              move tor-rec to btor-rec
PATCH *****              rewrite btor-rec invalid continue end-rewrite
                 else
                    delete  tordini record invalid continue end-delete
PATCH *****              move tor-rec to btor-rec
PATCH *****              delete  btordini record invalid continue end-delete
                 end-if
              end-if
           end-if.

      ****---
       READ-TORDINI-LOCK.
           set RecLocked to false.
           initialize geslock-linkage.
           move tor-numero to tor-numero-edit.
           string   "Il corrispettivo " tor-numero-edit
             x"0d0a""risulta in uso su altro terminale."
             x"0d0a""Questo comporta l'impossibilità ad"
             x"0d0a""essere aggiornato come chiuso." delimited size
                 into geslock-messaggio
           end-string. 
      
           set tutto-ok to true.
           read tordini with lock invalid set errori to true end-read.
           if RecLocked
              move 1 to geslock-v-riprova
              move 1 to geslock-v-ignora
              move 1 to geslock-v-termina
              move "tordini" to geslock-nome-file
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova perform READ-TORDINI-LOCK
              when ignora  set errori to true
                           read tordini no lock
                                invalid continue
                           end-read
                           if tor-anno   = primo-anno  and
                              tor-numero = primo-numero
                              move "000000000000" to PrimaChiave
                           end-if
              when termina display message box "Operazione interrotta!"
                                   title titolo
                                   icon 2
                           move -1 to link-status
                           set errori to true
              end-evaluate
           end-if.
           
      ****---
       READ-RORDINI-LOCK.
           set RecLocked to false.
           initialize geslock-linkage.
           move tor-numero   to tor-numero-edit.
           move ror-num-riga to riga-edit
           string   "La riga " riga-edit, 
                    " del corrispettivo " tor-numero-edit
             x"0d0a""risulta in uso su altro terminale."
             x"0d0a""Questo comporta l'impossibilità ad"
             x"0d0a""essere aggiornato come chiuso." 
             x"0d0a""Si consiglia di liberare la riga." delimited size
                 into geslock-messaggio
           end-string. 
      
           set tutto-ok to true.
           read rordini with lock invalid set errori to true end-read.
           if RecLocked
              move 1 to geslock-v-riprova
              move 1 to geslock-v-ignora
              move 1 to geslock-v-termina
              move "tordini" to geslock-nome-file
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova perform READ-RORDINI-LOCK
              when ignora  set errori to true
                           read rordini no lock
                                invalid continue
                           end-read
              when termina display message box "Operazione interrotta!"
                                   title titolo
                                   icon 2
                           move -1 to link-status
                           set errori to true
              end-evaluate
           end-if.

      ***---
       EXIT-PGM.
           move primo-numero to link-numero.
           unlock tordini  all records.
           unlock rordini  all records.
      *****     unlock btordini all records.
      *****     unlock brordini all records.
           close tordini rordini. |btordini brordini.
           goback.
