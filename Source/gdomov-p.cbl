       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      gdomov-p.
       AUTHOR.                          Andrea.
       REMARKS. Rettifica il gruppo GDO sui movimenti con quello attuale
                presente sull'anagrafica cliente
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tmovmag.sl".
           copy "clienti.sl".
LUBEXX     copy "tgrupgdo.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tmovmag.fd".
           copy "clienti.fd".
LUBEXX     copy "tgrupgdo.fd".

       WORKING-STORAGE SECTION.
           copy "link-geslock.def".
       78  titolo                value 
           "Rettifica gruppo GDO su movimenti di magazzino".

       77  status-tmovmag        pic xx.
       77  status-clienti        pic xx.
LUBEXX 77  status-tgrupgdo       pic xx.
       
       77  tmo-numero-edit       pic zz.zzz.zz9.
       77  counter               pic 9(10).
       77  counter2              pic 9(10).
       77  counter-edit          pic z(10).
       
       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".
       77  filler                pic 9.
           88 RecLocked          value 1, false 0.
       77  filler                pic 9.
           88 trovato            value 1, false 0.

       LINKAGE SECTION.
       77  link-esercizio        pic 9(4).
       77  link-tot-mov          pic 9(10).
       77  link-handle           handle of window.

      ******************************************************************
       PROCEDURE DIVISION using link-esercizio, 
                                link-tot-mov, 
                                link-handle.

       DECLARATIVES.
       TMOVMAG-ERR SECTION.
           use after error procedure on tmovmag.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tmovmag 
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File delle testate [TMOVMAG] inesistente"
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
       CLIENTI-ERR SECTION.             
           use after error procedure on clienti.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-clienti
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""Anagrafica Clienti [CLIENTI] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                set errori to true
                display message "File [CLIENTI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[CLIENTI] Indexed file corrupt!"
                          title titolo
                           icon 3 
           when "93"              
           when "99" set RecLocked to true
           end-evaluate.

LUBEXX***---
       TGRUPGDO-ERR SECTION.             
           use after error procedure on tgrupgdo.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tgrupgdo
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""Gruppi GDO [TGRUPGDO] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                set errori to true
                display message "File [TGRUPGDO] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TGRUPGDO] Indexed file corrupt!"
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
              perform ELABORAZIONE
              perform CLOSE-FILES
           end-if.
           perform EXIT-PGM.

      ***---
       INIT.
           move 0 to link-tot-mov.
           move 0 to counter counter2.
           set tutto-ok    to true.

      ***---
       OPEN-FILES.
           perform OPEN-IO-TMOVMAG
           if tutto-ok  
              open input clienti tgrupgdo
              if errori
                 close tmovmag
              end-if
           end-if.

           if errori goback end-if.

      ***---
       OPEN-IO-TMOVMAG.
           move "tmovmag" to geslock-nome-file.
           initialize geslock-messaggio.
           string   "Il file delle testate dei movimenti" 
             x"0d0a""risulta in uso su altro terminale."
             x"0d0a""Questo comporta l'impossibilità a"
             x"0d0a""procedere con il consolidamento." delimited size
                 into geslock-messaggio
           end-string.

           set tutto-ok  to true.
           set RecLocked to false.
           open i-o tmovmag.
           if RecLocked
              set errori to true
              move 1     to geslock-v-termina
              move 1     to geslock-v-riprova
              move 1     to geslock-v-ignora
              call   "geslock" using geslock-linkage
              cancel "geslock"
              
              evaluate true
              when riprova perform OPEN-IO-TMOVMAG
              when other   display message box "Operazione interrotta!"
                                   title titolo
                                   icon 2
              end-evaluate
           end-if.

      ***---
       ELABORAZIONE.
           move link-esercizio to tmo-anno.
           move low-value      to tmo-numero.
           start tmovmag key is >= tmo-chiave
                 invalid set errori to true
           end-start.

           if tutto-ok
              perform until 1 = 2
                 read tmovmag next no lock at end exit perform end-read

                 if tmo-anno not = link-esercizio exit perform end-if

                 add 1 to counter
                 add 1 to counter2
                 if counter2 = 100
                    if counter = 100
                       display "TMOVMAG" 
                          upon link-handle at column 28
                                                line 03
                    end-if
                    move counter to counter-edit
                    display counter-edit
                       upon link-handle at column 35
                                             line 03
                    move 0 to counter2
                 end-if

                 move tmo-tipo       to cli-tipo-CF
                 move tmo-cod-clifor to cli-codice
                 read clienti no lock
                      invalid continue
                  not invalid
                      move cli-gdo to gdo-codice
                      read tgrupgdo no lock 
                           invalid move spaces to gdo-capogruppo 
                      end-read
                      if gdo-capogruppo not = tmo-gdo
                         perform READ-TMOVMAG-LOCK-UPDATE
                         if errori exit perform end-if
                      end-if
                 end-read
              end-perform
           end-if.

           if link-tot-mov = 0
              display message "Nessun movimento da rettificare!"
                        title titolo
                         icon 2
           end-if.

      ***---
       READ-TMOVMAG-LOCK-UPDATE.
           move "tmovmag" to geslock-nome-file.
           initialize geslock-messaggio.

           set tutto-ok   to true.
           set RecLocked  to false.
           read tmovmag lock invalid continue end-read.
           if RecLocked
              move tmo-numero to tmo-numero-edit
              call "C$JUSTIFY" using tmo-numero-edit, "L"
              string   "Il movimento numero: " tmo-numero-edit
                x"0d0a""risulta in uso su altro terminale."
                x"0d0a""Questo comporta l'impossibilità ad"
                x"0d0a""aggiornare il suddetto record." delimited size
                    into geslock-messaggio
              end-string
              set errori to true
              move 1     to geslock-v-termina
              move 1     to geslock-v-riprova
              move 1     to geslock-v-ignora
              call   "geslock" using geslock-linkage
              cancel "geslock"
              
              evaluate true
              when riprova perform READ-TMOVMAG-LOCK-UPDATE
              when ignora  read tmovmag no lock
                           set tutto-ok to true
              when termina display message box "Operazione interrotta!"
                                   title titolo
                                   icon 2
                           set errori to true
              end-evaluate
           else                                     
              move gdo-capogruppo to tmo-gdo
              rewrite tmo-rec invalid continue end-rewrite
              add 1 to link-tot-mov
              unlock tmovmag all records
           end-if.

      ***--
       CLOSE-FILES.
           close tmovmag clienti tgrupgdo.

      ***---
       EXIT-PGM.
           goback.
