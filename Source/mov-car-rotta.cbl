       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      mov-car-rotta.
       AUTHOR.                          Luciano.
       REMARKS. Creazione del movimento di carico della merce rotta.
                Parto leggendo il movimento di scarico e lo duplico
                con l'apposita causale sul magazzino fittizio.

      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tcaumag.sl".
           copy "tparamge.sl".
           copy "progmag.sl".
           copy "tmovmag.sl".
           copy "rmovmag.sl".
           copy "rmovmag.sl"
                REPLACING ==rmovmag== BY ==rmovmag1==,
                          ==status-rmovmag== BY ==status-rmovmag1==.


      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tcaumag.fd".
           copy "tparamge.fd".
           copy "progmag.fd".
           copy "tmovmag.fd".
           copy "rmovmag.fd".
           copy "rmovmag.fd"
                REPLACING ==rmovmag== BY ==rmovmag1==,
                          ==status-rmovmag== BY ==status-rmovmag1==.

       WORKING-STORAGE SECTION.
      * COPY
           copy "link-geslock.def".
           COPY "LINK-WPROGMAG.DEF".

      * COSTANTI
       78  titolo              value "MOVIMENTO DI CARICO MERCE ROTTA".

      *FILE-STATUS
       77  status-tcaumag    pic xx.
       77  status-tparamge   pic xx.
       77  status-progmag    pic xx.
       77  status-tmovmag    pic xx.
       77  status-rmovmag    pic xx.
       77  status-rmovmag1   pic xx.


      * FLAGS
       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".
       77  filler                pic 9.
           88 RecLocked          value 1, false 0.

       LINKAGE SECTION.
           copy "link-mov-car-rotta.def".
           copy "common-linkage.def".

      ******************************************************************
       PROCEDURE DIVISION using mov-car-rotta-linkage
                                LK-BLOCKPGM
                                USER-CODI
                                LIVELLO-ABIL.
       DECLARATIVES.

      ***---  
       TCAUMAG-ERR SECTION.
           use after error procedure on tcaumag.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tcaumag
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""Tabella Causali [TCAUMAG] inesistente"
                          title titolo
                           icon 2                              
                set errori to true
           when "39"
                set errori to true
                display message "File [TCAUMAG] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TCAUMAG] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---  
       TPARAMGE-ERR SECTION.
           use after error procedure on tparamge.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tparamge
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""Tabella Causali [TPARAMGE] inesistente"
                          title titolo
                           icon 2                              
                set errori to true
           when "39"
                set errori to true
                display message "File [TPARAMGE] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TPARAMGE] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---  
       PROGMAG-ERR SECTION.
           use after error procedure on progmag.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-progmag
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""Tabella [PROGMAG] inesistente"
                          title titolo
                           icon 2                              
                set errori to true
           when "39"
                set errori to true
                display message "File [PROGMAG] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[PROGMAG] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       TMOVMAG-ERR SECTION.
           use after error procedure on tmovmag.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tmovmag
           when "35"
                display message "Impossibile procedere."
               x"0d0a""Tabella [TMOVMAG] inesistente"
                          title titolo
                           icon 2
                set errori to true
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
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       RMOVMAG-ERR SECTION.
           use after error procedure on rmovmag.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-rmovmag
           when "35"
                display message "Impossibile procedere."
               x"0d0a""Tabella [RMOVMAG] inesistente"
                          title titolo
                           icon 2
                set errori to true
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
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

       END DECLARATIVES.

      ***---
       MAIN-PRG.
           perform INIT.
           perform OPEN-FILES.
           if tutto-ok
              perform LEGGI-PARAMETRI
              if tutto-ok
                 perform ELABORAZIONE
              end-if
              perform CLOSE-FILES
           end-if.
           perform EXIT-PGM.

      ***---
       INIT.
           set tutto-ok to true.

      ***---
       OPEN-FILES.
           open input tcaumag
           open input tparamge
           open input progmag
           open input rmovmag
           open i-o tmovmag
                    rmovmag1.

      ***---
       LEGGI-PARAMETRI.
           move space  to tge-codice
           read tparamge
              invalid
                 continue
           end-read.

           move tge-causale-rotta-c   to tca-codice
           read tcaumag
              invalid
                 continue
           end-read.


      ***---
       READ-RECORD-LOCK.
           set RecLocked to false.
           initialize geslock-linkage.
           move "tmovmag" to geslock-nome-file.

           set tutto-ok to true.
           read tmovmag with lock 
              invalid 
                 continue 
           end-read.

           if RecLocked
              move 1 to geslock-v-riprova
              move 0 to geslock-v-ignora
              move 1 to geslock-v-termina
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova 
                   perform READ-RECORD-LOCK
              when ignora  
              when termina 
                   set errori to true
                   unlock tmovmag all records
              end-evaluate
           end-if.


      ***---
       ELABORAZIONE.
           move mcr-tmo-chiave  to tmo-chiave


           perform READ-RECORD-LOCK

           if errori
              display message box 
                          "Impossibile creare il movimento di carico"
                      icon 3
                      title titolo
              exit paragraph
           end-if.

           move mcr-tmo-anno    to rmo-anno of rmovmag
           move mcr-tmo-numero  to rmo-movim of rmovmag
           move low-value       to rmo-riga of rmovmag

           start rmovmag key not < rmo-chiave of rmovmag
              invalid
                 continue
              not invalid
                 perform until 1 = 2
                    read rmovmag next no lock
                       at end
                          exit perform
                    end-read
                    if mcr-tmo-anno  not = rmo-anno of rmovmag or
                       mcr-tmo-numero not = rmo-movim of rmovmag
                       exit perform
                    end-if
                    perform TRASFERISCI-RIGA
                 end-perform
           end-start.


           add 1 to tmo-numero
           move tge-causale-rotta-c   to tmo-causale
           move mcr-peso-tot          to tmo-peso-utf

           write tmo-rec
              invalid
                 continue
           end-write.

           unlock tmovmag all record.


      ***---
       TRASFERISCI-RIGA.
           move rmo-rec of rmovmag to rmo-rec of rmovmag1

           add 1 to rmo-movim of rmovmag1
           move tge-causale-rotta-c   to rmo-causale  of rmovmag1

           move tca-cod-magaz         to rmo-codmag of rmovmag1

           write RMO-REC of rmovmag1.


           perform VERIFICA-PRG.

           if not ( tca-no-movim-giac and
                    tca-no-movim-imp  and
                    tca-no-movim-ord  and
                    tca-no-giac-bloc )
              perform AGGIORNA-MAG
           end-if.

      ***---
       VERIFICA-PRG.
           move rmo-chiave-progmag of rmovmag1 to prg-chiave
           read progmag no lock
              invalid
                 perform CREA-PRG
           end-read.

      ***---
       CREA-PRG.
           move rmo-chiave-progmag of rmovmag to prg-chiave
           read progmag no lock
              invalid
                 continue
           end-read

           initialize link-wprogmag replacing numeric data by zeroes
                                         alphanumeric data by spaces.

           set link-batch       to true.
           move user-codi   to link-user.

           move rmo-chiave-progmag of rmovmag1 to link-key

           move prg-peso-utf       to link-utf.
           move prg-peso-non-utf   to link-non-utf.

           add link-utf to link-non-utf giving link-peso.

           call   "wprogmag" using link-wprogmag.
           cancel "wprogmag".

           move rmo-chiave-progmag of rmovmag1 to prg-chiave
           read progmag no lock
              invalid
                 continue
           end-read.

      ***---
       AGGIORNA-MAG.
           initialize link-wprogmag
           perform VALORIZZA-ARRAY-CAUSALI
           set  link-update   to true
           move rmo-chiave-progmag of rmovmag1 to link-key
           move tca-codice   to link-causale
           move rmo-qta of rmovmag1        to link-valore
           compute link-valore-costo =
                   rmo-netto of rmovmag1          +
                   rmo-coubat of rmovmag1         +
                   rmo-imp-cons of rmovmag1 
           if link-valore-costo = 0 or
LUBEXX        rmo-qta of rmovmag1            = 0
              move 0 to multiplyer(16)
           end-if
           if link-valore = 0
              move 0 to multiplyer(1)
              move 0 to multiplyer(15)
           end-if
           move user-codi       to link-user of link-wprogmag
           call   "wprogmag" using link-wprogmag
           cancel "wprogmag".

      ***---
       VALORIZZA-ARRAY-CAUSALI.
           set link-update-um      to true.
           set link-update-peso    to false.
           set link-update-valore  to false.
           move "0000000000000000" to link-array.
           move 1 to multiplyer(1).
           move 1 to multiplyer(15).
           move 1 to multiplyer(16).



      ***--
       CLOSE-FILES.
           unlock tmovmag all records.
           close  tcaumag
                  tparamge
                  progmag
                  tmovmag
                  rmovmag
                  rmovmag1.
      
      ***---
       EXIT-PGM.
           goback.
