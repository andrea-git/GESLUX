       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      stintereg-p.
       AUTHOR.                          Filippo.
      ******************************************************************
       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "anautf.sl".
           copy "lineseq.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "anautf.fd".
           copy "lineseq.fd".

       WORKING-STORAGE SECTION.
           copy "acucobol.def".
           copy "link-geslock.def".
           copy "comune.def".
       77  status-anautf         pic x(2).
       77  status-lineseq        pic x(2).
       77  wstampa               pic x(256).
       77  sw-anautf             pic 9 value 0.

       77  como-data             pic 9(8).
       77  como-ora              pic 9(8).

       78  titolo value "Stampa intestazione registri".

       01  st-intesta1.
           05 filler        pic x(05) value spaces.
           05 st-ragsoc     pic x(30).
           05 filler        pic x(79) value spaces.
           05 filler        pic x(05) value "Anno:".
           05 st-anno       pic z(04).

       01  st-intesta2.
           05 filler        pic x(05) value spaces.
           05 st-indirizzo  pic x(30).
           05 filler        pic x(18) value spaces.
           05 filler        pic x(09) value "REGISTRO ".
           05 st-desc1      pic x(40).
           05 filler        pic x(12) value spaces.
           05 filler        pic x(12) value "Registro n. ".

       01  st-intesta3.
           05 filler        pic x(05) value spaces.
           05 st-cap        pic x(05).
           05 filler        pic x     value space.
           05 st-citta      pic x(40).
           05 filler        pic x(11) value spaces.
           05 st-desc2      pic x(40).

       01  st-intesta4.
           05 filler        pic x(05) value spaces.
           05 st-piva       pic 9(11).
           05 filler        pic x(98) value space.
           05 filler        pic x(10) value "Foglio n. ".
           05 st-pag        pic z(05).


       01  st-line-riga     pic x(86) value all "-".

       77  i                pic 9(5) value 0.      
       78  max-righe        value 66.
       77  num-righe        pic 99 value 0.
       77  diff-righe       pic 99 value 0.
       77  n-vuote          pic 99 value 0.

       LINKAGE SECTION.
       copy "link-stintereg.def".

      ******************************************************************
       PROCEDURE DIVISION using stintereg-linkage.

       DECLARATIVES.
       ANAUTF-ERR SECTION.
           use after error procedure on anautf.
           set tutto-ok  to true.
           evaluate status-anautf
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File anagrafica utf [ANAUTF] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [ANAUTF] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[ANAUTF] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           when "93"
           when "99"
                set RecLocked to true
                set errori    to true
           end-evaluate. 

       LINESEQ-ERR SECTION.
           use after error procedure on lineseq.
           set tutto-ok  to true.
           evaluate status-lineseq
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File lineseq [LINESEQ] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [LINESEQ] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[LINESEQ] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           when "93"
                initialize geslock-messaggio
                string   "File già in uso!"
                  x"0d0a""Impossibile procedere!" delimited size
                      into geslock-messaggio
                end-string
                move 1 to geslock-v-riprova
                move 0 to geslock-v-ignora
                move 1 to geslock-v-termina
                move   "File TXT"   to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova
                     open output lineseq
                when termina
                     set errori to true
                     display message "Operazione interrotta!"
                               title titolo
                                icon 2
                end-evaluate
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
           set tutto-ok to true.
           set trovato to false.
           accept  wstampa      from environment "PATH-ST".
           accept  como-data         from century-date.
           accept  como-ora          from time.
           inspect wstampa      replacing trailing 
                                     spaces by low-value.
           string wstampa       delimited by low-value
                  "stintereg"      delimited by size
                  "_"                delimited by size
                  como-data          delimited by size
                  "_"                delimited by size
                  como-ora           delimited by size
                  ".txt"             delimited by size
                  into wstampa
           end-string.

       OPEN-FILES.
           perform OPEN-OUTPUT-LINESEQ.
           if tutto-ok
              open input anautf
              if errori
                 close       lineseq
                 delete file lineseq
              end-if
           end-if.

      ***---
       OPEN-OUTPUT-LINESEQ.
           open output lineseq.
      
      ***---
       ELABORAZIONE.
           |lettura anautf
           move 0 to num-righe 
           initialize utf-rec.
           move stintereg-anno     to utf-anno.
           move stintereg-num-reg  to utf-num-reg.

           read anautf no lock
              invalid
                 set errori to true
              not invalid
                 perform STAMPA-DATI
           end-read
           
           if not trovato
              display message box "Nessun documento presente avente"
                                  " il criterio selezionato"
                      title = titolo
                      icon 2
           end-if.

      ***---
       STAMPA-DATI.            
           set trovato to true.

           perform SALTO-PAGINA.
           perform SALTO-PAGINA.

           perform PRIMA-PAG.
           perform PAGINE.

           perform SALTO-PAGINA.
           perform SALTO-PAGINA.


      ***---
       PRIMA-PAG.
           move 5 to n-vuote
           perform RIGHE-VUOTE

           initialize line-riga
           move "REGISTRO" to line-riga(60:)
           write line-riga

           write line-riga from spaces

           initialize line-riga
           move utf-desc1 to line-riga(45:)
           write line-riga

           write line-riga from spaces

           initialize line-riga
           move utf-desc2 to line-riga(45:)
           write line-riga

           write line-riga from spaces

           initialize line-riga
           move utf-modello to line-riga(60:)
           write line-riga

           move 5 to n-vuote
           perform RIGHE-VUOTE

           initialize line-riga
           move "Rilasciato alla ditta:        L U B E X   S.p.A."
                to line-riga(40:)
           write line-riga

           write line-riga from spaces

           initialize line-riga
           move "titolare della licenza U.T.I.F.  nr. __________"
                to line-riga(40:)
           write line-riga

           write line-riga from spaces

           initialize line-riga
           move
           "per l'esercizio del deposito per la vendita di oli minerali"
           to line-riga(40:)
           write line-riga

           write line-riga from spaces

           initialize line-riga
           move "sito in VIMODRONE Via G. Di Vittorio n. 13/15"
                to line-riga(40:)
           write line-riga

           move 5 to n-vuote
           perform RIGHE-VUOTE

           initialize line-riga
           move "U F F I C I O   T E C N I C O   di   F I N A N Z A"
                to line-riga(40:)
           write line-riga

           write line-riga from spaces

           initialize line-riga
           move "              di  M I L A N O" to line-riga(40:)
           write line-riga

           write line-riga from spaces
           write line-riga from spaces

           initialize line-riga
           move "Il presente registro nr. ________" to line-riga(40:)
           write line-riga

           write line-riga from spaces

           initialize line-riga
           move "si compone di nr. _________ fogli" to line-riga(40:)
           write line-riga

           write line-riga from spaces

           initialize line-riga
           move "numerati da _________ a _________" to line-riga(40:)
           write line-riga

           write line-riga from spaces

           initialize line-riga
           move "Esercizio finanziario ___________" to line-riga(40:)
           write line-riga

           write line-riga from spaces

           initialize line-riga
           move "Milano, li ______________________" to line-riga(40:)
           write line-riga

           move 45 to num-righe.

      ***---
       PAGINE.
           perform varying i from stintereg-pag-ini by 1
                   until i > (stintereg-pag-ini + stintereg-pag-st - 1)  

             perform SALTO-PAGINA

             write line-riga from spaces

             initialize st-intesta1
             move utf-ragsoc to st-ragsoc
             move utf-anno   to st-anno
             write line-riga from st-intesta1
 
             initialize st-intesta2
             move utf-indirizzo to st-indirizzo
             move utf-anno      to st-anno
             move utf-desc1     to st-desc1
             write line-riga from st-intesta2

             initialize st-intesta3
             move utf-cap       to st-cap
             move utf-anno      to st-anno
             string utf-citta " (" utf-provincia ")" delimited by "   "
                    into st-citta
             end-string
             move utf-desc2     to st-desc2
             write line-riga from st-intesta3

             initialize st-intesta4
             move utf-partita-iva to st-piva
             move i               to st-pag
             write line-riga from st-intesta4

             move 5 to num-righe

           end-perform.

      ***---
       SALTO-PAGINA.
           compute diff-righe = max-righe - num-righe.
           move diff-righe to n-vuote
           perform RIGHE-VUOTE  
           move 0 to num-righe.

      ***---
       RIGHE-VUOTE.
           perform n-vuote times
              write line-riga from spaces
           end-perform.

      ***---
       CLOSE-FILES.
           close anautf, lineseq.
           if not trovato
              delete file lineseq
              move spaces to wstampa
           end-if.

      ***---
       EXIT-PGM.
           move wstampa to stintereg-path.
           goback.
