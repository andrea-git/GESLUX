       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      mag-ames-eva.
       AUTHOR.                          Andrea.
       REMARKS. Passa tutti i movim. magazzini con causale AMES ed allinea
                le quantità ed il prezzo finale con i valori dell'evasioni
                collegate
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.         
           copy "tordini.sl".
           copy "rordini.sl".
           copy "tmovmag.sl". 
           copy "rmovmag.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.    
           copy "tordini.fd".
           copy "rordini.fd".
           copy "tmovmag.fd". 
           copy "rmovmag.fd".

       WORKING-STORAGE SECTION.
           COPY "acucobol.def".
           copy "link-geslock.def".
           copy "comune.def".

       78  titolo    value "Allineamento movimenti AMES".
       78  78-clear              value 
           "                                                          ".

       77  rmo-numero-edit  pic z(8).  
       77  status-tordini   pic xx.
       77  status-rordini   pic xx.
       77  status-tmovmag   pic xx.  
       77  status-rmovmag   pic xx.    
                                             
       77 n-rec             pic 9(5) value 0.
       77 n-err             pic 9(5) value 0.
       77 n-elab            pic 9(5) value 0.
       77 nr-ok             pic 9(5) value 0.
       77 nr-ko             pic 9(5) value 0.

      ******************************************************************
       PROCEDURE DIVISION.

       DECLARATIVES.       
       TORDINI-ERR SECTION.
           use after error procedure on tordini.
           set RecLocked to false.
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
                  x"0d0a""File delle righe [TORDINI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

       RORDINI-ERR SECTION.
           use after error procedure on rordini.
           set RecLocked to false.
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

       TMOVMAG-ERR SECTION.
           use after error procedure on tmovmag.
           set RecLocked to false.
           evaluate status-tmovmag
           when "39"
                set errori to true
                display message "File [TMOVMAG] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TMOVMAG] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File delle testate [TMOVMAG] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.  
                           
       RMOVMAG-ERR SECTION.
           use after error procedure on rmovmag.
           set RecLocked to false.
           evaluate status-rmovmag
           when "39"
                set errori to true
                display message "File [RMOVMAG] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[RMOVMAG] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File delle righe [RMOVMAg] inesistente"
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
           perform ELABORAZIONE.
           perform EXIT-PGM.

      ***---
       INIT.                            
           set RecLocked to false.

      ***---
       OPEN-FILES.
           open input rordini tordini tmovmag.
           open i-o rmovmag.
      
      ***---
       ELABORAZIONE.
           move 2022      to tmo-anno.
           move "AMES"    to tmo-causale.
           move low-value to tmo-numero.
           start tmovmag key >= key01
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tmovmag next at end exit perform end-read
                    if tmo-anno    not = 2022  or
                       tmo-causale not = "AMES"
                       exit perform
                    end-if
                    add 1 to n-rec
                    move tmo-tor-chiave to tor-chiave
                    read tordini no lock
                         invalid
                         display message 
                      "Evasione n. " tor-anno " - " tor-numero
               x"0d0a""legata al movimento: " tmo-anno " - " tmo-numero
               x"0d0a""NON TROVATA!"
                                 title titolo
                                  icon 3      
                         add 1 to n-err
                     not invalid 
                         move tor-anno   to ror-anno
                         move tor-numero to ror-num-ordine
                         move low-value  to ror-num-riga
                         start rordini key >= ror-chiave
                               invalid 
                         display message 
                      "Evasione n. " tor-anno " - " tor-numero
               x"0d0a""Non sono presenti righe"
                                 title titolo
                                  icon 3      
                               add 1 to n-err
                           not invalid   
                               add 1 to n-elab
                               perform until 1 = 2
                                  read rordini next 
                                    at end exit perform 
                                  end-read
                                  if ror-anno       not = tor-anno or
                                     ror-num-ordine not = tor-numero
                                     exit perform
                                  end-if
                                  move tmo-anno     to rmo-anno
                                  move tmo-numero   to rmo-movim
                                  move ror-num-riga to rmo-riga
                                  read rmovmag no lock
                                       invalid
                                       display message 
                                     "Riga evasione n. " ror-anno 
                                                   " - " ror-num-ordine
                                                   " - " ror-num-riga
                              x"0d0a""Non trovata sul movimento: "
                                        rmo-anno " - " rmo-movim
                                        " - " rmo-riga
                                                title titolo
                                                 icon 3
                                       add 1 to nr-ko
                                   not invalid
                                       perform READ-RMOVMAG-LOCK
                                       if errori
                                          add 1 to nr-ko
                                       else
                                          add 1 to nr-ok
                                          move ror-qta 
                                            to rmo-qta
                                          move ror-prz-unitario 
                                            to rmo-listino
                                          move ror-imponib-merce
                                            to rmo-netto
                                          rewrite rmo-rec
                                          unlock rmovmag all records
                                       end-if
                                   end-read
                               end-perform
                         end-start
                    end-read
                 end-perform
           end-start.

      ****---
       READ-RMOVMAG-LOCK.
           set RecLocked to false.
           initialize geslock-linkage.
           move rmo-movim to rmo-numero-edit.
           string   "Il movimento " rmo-numero-edit
             x"0d0a""risulta in uso su altro terminale."
             x"0d0a""Questo comporta l'impossibilità ad"
             x"0d0a""essere aggiornato" delimited size
                 into geslock-messaggio
           end-string. 

           perform until 1 = 2
              set RecLocked to false
              read tordini with lock 
              if not RecLocked
                 exit perform
              end-if
              move 1 to geslock-v-riprova
              move 1 to geslock-v-ignora
              move 1 to geslock-v-termina
              move "rmovmag" to geslock-nome-file
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova continue
              when ignora  exit perform
              when termina display message box "Operazione interrotta!"
                                   title titolo
                                   icon 2
                           exit perform
              end-evaluate
           end-perform.

      ***---
       EXIT-PGM.
           display message "Elaborazioen terminata: "  
                    x"0d0a""Movimenti trattati: " n-rec  
                    x"0d0a""Movimenti elaborati: " n-elab
                    x"0d0a""Movimenti errati: " n-err
                    x"0d0a""Righe aggiornate: " nr-ok
                    x"0d0a""Righe non aggiornate: " nr-ko
           close tordini rordini tmovmag rmovmag.
           goback.
