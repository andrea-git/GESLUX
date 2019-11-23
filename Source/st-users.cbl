       PROGRAM-ID. st-users.

       SPECIAL-NAMES. decimal-point is comma.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "user.sl".
           copy "lineseq.sl".

       DATA DIVISION.
       FILE SECTION.
           copy "user.fd".
           copy "lineseq.fd".

       WORKING-STORAGE SECTION.
           copy "splcrt2graf.lks".
           copy "link-geslock.def".

      * FILE STATUS AND VARIABLES
       77  status-user              pic xx.
       77  status-lineseq           pic xx.

       77  wstampa                  pic x(256) value spaces.

      * COSTANTI
       78  titolo  value "Stampa utenti - password".

      * FLAGS
       01  filler                  pic 9.
           88 Prima-Volta          value 1, false 0.

       01  filler                  pic  9.
           88 RecLocked            value 1, false 0.

       01  filler                  pic  9.
           88 trovato              value 1, false 0.

       01  controlli               pic  xx.
           88 tutto-ok             value "OK".
           88 errori               value "ER".

       01  filler                  pic  9.
           88 bisestile            value 1, false 0.

      * VARIABILI

       77  como-anno               pic  9(4).

       01  data-validita.
           05 anno                 pic  9999.
           05 mese                 pic  99.
           05 giorno               pic  99.

       77  como-ora                pic 9(8).
       77  como-data               pic 9(8).
       77  data-oggi               pic 9(8).
       77  num-page                pic 9(3) value 0.
       77  num-righe               pic 9(3) value 0.
       77  num-max-righe           pic 9(3) value 0.
       77  num-max-righe-x         pic x(3) value spaces.

      * VARIABILI PER LA STAMPA
       01  intestazione-1.
           05 int-data             pic x(10).
           05 filler               pic x.
           05 int-ora              pic x(5).
           05 filler               pic x(29).
           05 filler               pic x(4)   value "Pag. ".
           05 int-page             pic zz9.

       01  intestazione-2          pic x(52).

       01  intestazione-3.
           05 filler               pic x(10) value "Utente".
           05 filler               pic x(5).
           05 filler               pic x(20) value "Password".
           05 filler               pic x(3).
           05 filler               pic x(20) value "Valida fino al".
                                  
       01  riga-utente.         
           05 r-user-cod           pic x(10).
           05 filler               pic x(5).
           05 r-password           pic x(20).
           05 filler               pic x(5).
           05 r-gg                 pic 9(2).
           05 filler               pic x value "/".
           05 r-mm                 pic 9(2).
           05 filler               pic x value "/".
           05 r-aa                 pic 9(4).

       PROCEDURE DIVISION.
       DECLARATIVES.
      ***---
       USER-ERR SECTION.
           use after error procedure on user.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-user
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File utenti [USER] inesistente"
                          title titolo
                           icon 3
                set errori to true
           when "39"
                display message "File [USER] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[USER] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.  

      ***---
       LINESEQ-ERR SECTION.
           use after error procedure on lineseq.
           set tutto-ok  to true.
           evaluate status-lineseq
           when "35"
                display message "Impossibile procedere."
               x"0d0a""File TXT [LINESEQ] inesistente"
                          title titolo
                           icon 2
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

      ***---
       MAIN.
           perform INIT.
           perform OPEN-FILES.
           perform ELABORAZIONE
           perform CLOSE-FILES

           perform STAMPA-FILE.

           perform EXIT-PGM.

      ***---
       INIT.
           move 999 to num-righe.
           set trovato     to false.
           set Prima-Volta to true.
           accept num-max-righe-x from environment "NUM-MAX-RIGHE".
           move   num-max-righe-x to num-max-righe with convert.

           accept  wstampa   from environment "PATH_ST".
           inspect wstampa   replacing trailing space by low-value.
           accept  como-data from century-date.
           accept  data-oggi from century-date.
           accept  como-ora  from time.

           string wstampa             delimited low-value
                  "user_pass"         delimited size
                  "_"                 delimited size
                  como-data           delimited size
                  "_"                 delimited size
                  como-ora            delimited size
                  ".txt"              delimited size
                  into wstampa     
           end-string.

           perform FORMATTA-DATA.
           perform FORMATTA-ORA.

      *     perform STAMPA-INTESTAZIONE.

      ***---
       FORMATTA-DATA.
           string como-data(7:2) delimited size
                  "/"            delimited size
                  como-data(5:2) delimited size
                  "/"            delimited size
                  como-data(1:4) delimited size
                  into int-data
           end-string.

      ***--
       FORMATTA-ORA.
           string como-ora(1:2)  delimited size
                  ":"            delimited size
                  como-ora(3:2)  delimited size
                  into int-ora
           end-string.

      ***---
       OPEN-FILES.
           open  input user.
           open output lineseq.

      ***---
       ELABORAZIONE.
           move low-value to user-chiave.

           start user key >= user-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read user next at end exit perform end-read
                    perform AGGIUNGI-6-MESI
                    perform SCRIVI-UTENTE
                 end-perform
           end-start.

      ***---
       SCRIVI-UTENTE.
           set trovato to true.
           if num-righe >= num-max-righe
              if not prima-volta
                 write line-riga from spaces after page
                 write line-riga from x"09"  after 1
              end-if
              perform STAMPA-INTESTAZIONE
           end-if.

           move user-cod   to r-user-cod.
           move user-pass  to r-password.

           move giorno     to r-gg.
           move mese       to r-mm.
           move anno       to r-aa.

           write line-riga from riga-utente after 2.

           add 2 to num-righe.

      ***---
       STAMPA-INTESTAZIONE.
           if prima-volta
              write line-riga from spaces
              set prima-volta   to false
           end-if.

           add  1              to num-page.
           move num-page       to int-page.
           move intestazione-1 to line-riga.
           write line-riga   from intestazione-1 after 1.

           move titolo to intestazione-2.
           call "c$justify" using intestazione-2, "C".
           move intestazione-2 to line-riga.

           write line-riga  after 2.
           write line-riga   from intestazione-3 after 2.

           move all "-"        to line-riga(1:52).
           write line-riga  after 1

           move 5   to num-righe.

      ***---
       AGGIUNGI-6-MESI.
           move user-pass-data to como-data.
           move como-data(1:4) to anno.
           move como-data(5:2) to mese.
           move como-data(7:2) to giorno.
           add  6 to mese.

           if mese > 12
              subtract 12 from mese
              add 1 to anno
           end-if.

           if mese = 2
              divide        anno  by 4 giving como-anno
              multiply como-anno  by 4 giving como-anno
              if como-anno = anno set bisestile to true
              else                set bisestile to false
              end-if
              evaluate true         
              when giorno >= 29
                   if bisestile move 29 to giorno
                   else         move 28 to giorno
                   end-if
              end-evaluate
           else
              if giorno = 31
                 evaluate mese
                 when  4
                 when  6
                 when  9
                 when 11  move 30 to giorno
                 end-evaluate
              end-if
           end-if.

      ***---
       STAMPA-FILE.
           if trovato
              move wstampa to splcrt2graf-percorso-stampa
              set  splcrt2graf-stampa    to true
              set  splcrt2graf-windows   to true
              set  splcrt2graf-verticale to true
              set  splcrt2graf-forza-crt to true
              set  splcrt2graf-10pt      to true
              call   "splcrt2graf"   using splcrt2graf-link
              cancel "splcrt2graf"
           end-if.

      ***---
       CLOSE-FILES.
           close user.
           close lineseq.

      ***---
       EXIT-PGM.
           delete file lineseq.
           goback.
