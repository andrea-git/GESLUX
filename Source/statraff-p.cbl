       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      statraff-p.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "lineseq.sl".
           copy "statraff.sl".
           copy "tmarche.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "lineseq.fd".
           copy "statraff.fd".
           copy "tmarche.fd".

       WORKING-STORAGE SECTION.
       copy "link-geslock.def".

      * COSTANTI
       78  titolo         value "Stampa statistiche con raffronto mese".

      * FILE STATUS
       77  status-lineseq             pic xx.
       77  status-statraff            pic xx.
       77  status-tmarche             pic xx.

      * VARIABILI
       77  como-data                  pic 9(8).
       77  como-ora                   pic 9(8).
       77  wstampa                    pic x(256).
       77  SaveMese                   pic 99 value 0.
       77  como-kg                    pic s9(12)v999.
       77  como-valore                pic s9(12)v99.
       77  tot-kg-1                   pic s9(12)v99.
       77  tot-indice-1               pic s9(12)v99.
       77  tot-vag-1                  pic s9(12)v99.
       77  tot-kg-2                   pic s9(12)v99.
       77  tot-indice-2               pic s9(12)v99.
       77  tot-vag-2                  pic s9(12)v99.
       77  tot-kg-3                   pic s9(12)v99.
       77  tot-indice-3               pic s9(12)v99.
       77  tot-vag-3                  pic s9(12)v99.

      * FLAGS
       01  controllo                  pic xx.
         88 errori                    value "ER".
         88 tutto-ok                  value "OK".

       01  filler                     pic 9.
         88 prima-volta               value 1, false 0.

      * RIGHE PER LA STAMPA
       01  riga-page.
         05 filler                    pic x(180).
         05 filler                    pic x(5) value "Pag. ".
         05 tit-page                  pic zzz9.

       01  riga-div-1.
         05 filler                    pic x(2).
         05 filler                    pic x(85)  value all "=".
         05 filler                    pic x      value "|".
         05 filler                    pic x(50)  value all "=".
         05 filler                    pic x      value "|".
         05 filler                    pic x(50)  value all "=".

       01  riga-div-2.
         05 filler                    pic x(2).
         05 filler                    pic x(85)  value all "-".
         05 filler                    pic x      value "+".
         05 filler                    pic x(50)  value all "-".
         05 filler                    pic x      value "+".
         05 filler                    pic x(50)  value all "-".

       01  titolo-1.
         05 filler                    pic x(2).
         05 filler                    pic x(27) 
            value "RAFFRONTO con ANNO PRECED.(".
         05 tipo-stampa               pic x(8).
         05 filler                    pic x   value ")".
         05 filler                    pic x(18).
         05 tit-mese-1                pic x(9).
         05 filler                    pic x(5) value "  -  ".
         05 tit-anno-1                pic 99.
         05 filler                    pic x(15).
         05 filler                    pic x value "|".
         05 filler                    pic x(17).
         05 tit-mese-2                pic x(9).
         05 filler                    pic x(5) value "  -  ".
         05 tit-anno-2                pic 99.
         05 filler                    pic x(17).
         05 filler                    pic x value "|".
         05 filler                    pic x(20).
         05 filler                    pic x(11) value "SCOSTAMENTI".

       01  titolo-2.
         05 filler                    pic x(2).
         05 filler                    pic x(4).
         05 filler                    pic x(15) value "GRUPPO PRODOTTI".
         05 filler                    pic x(28).
         05 filler                    pic x(3)  value "Kg.".
         05 filler                    pic x(8).
         05 filler                    pic x(9)  value "Indice Kg".
         05 filler                    pic x(9).
         05 filler                    pic x(9)  value "Val. Agg.".
         05 filler                    pic x(1)  value "|".
         05 filler                    pic x(12).
         05 filler                    pic x(3)  value "Kg.".
         05 filler                    pic x(8).
         05 filler                    pic x(9)  value "Indice Kg".
         05 filler                    pic x(9).
         05 filler                    pic x(9)  value "Val. Agg.".
         05 filler                    pic x(1)  value "|".
         05 filler                    pic x(12).
         05 filler                    pic x(3)  value "Kg.".
         05 filler                    pic x(8).
         05 filler                    pic x(9)  value "Indice Kg".
         05 filler                    pic x(9).
         05 filler                    pic x(9)  value "Val. Agg.".

       01  riga.
         05 filler                    pic x(2).
         05 r-marca                   pic z(4).
         05 filler                    pic x value "-".
         05 r-descr                   pic x(29).
         05 filler                    pic x.
         05 r-kg-1                    pic zz.zzz.zz9,999-.
         05 filler                    pic x(1).
         05 r-indice-1                pic zzz.zzz.zz9,99-.
         05 filler                    pic x(1).
         05 r-vag-1                   pic zz.zzz.zzz.zz9,99-.
         05 filler                    pic x(1) value "|".
         05 r-kg-2                    pic zz.zzz.zz9,999-.
         05 filler                    pic x(1).
         05 r-indice-2                pic zzz.zzz.zz9,99-.
         05 filler                    pic x(1).
         05 r-vag-2                   pic zz.zzz.zzz.zz9,99-.
         05 filler                    pic x(1) value "|".
         05 r-kg-3                    pic zz.zzz.zz9,999-.
         05 filler                    pic x(1).
         05 r-indice-3                pic zzz.zzz.zz9,99-.
         05 filler                    pic x(1).
         05 r-vag-3                   pic zz.zzz.zzz.zz9,99-.

       01  riga-totale.   
         05 filler                    pic x(2).
         05 filler                    pic x(6) value "----->".
         05 filler                    pic x.
         05 filler                    pic x(6) value "Totali".
         05 filler                    pic x(22).
         05 t-kg-1                    pic zz.zzz.zz9,999-.
         05 filler                    pic x(1).
         05 t-indice-1                pic zzz.zzz.zz9,99-.
         05 filler                    pic x(1).
         05 t-vag-1                   pic zz.zzz.zzz.zz9,99-.
         05 filler                    pic x(1) value "|".
         05 t-kg-2                    pic zz.zzz.zz9,999-.
         05 filler                    pic x(1).
         05 t-indice-2                pic zzz.zzz.zz9,99-.
         05 filler                    pic x(1).
         05 t-vag-2                   pic zz.zzz.zzz.zz9,99-.
         05 filler                    pic x(1) value "|".
         05 t-kg-3                    pic zz.zzz.zz9,999-.
         05 filler                    pic x(1).
         05 t-indice-3                pic zzz.zzz.zz9,99-.
         05 filler                    pic x(1).
         05 t-vag-3                   pic zz.zzz.zzz.zz9,99-.

       78  max-righe        value 42.
       77  diff-righe       pic 99 value 0.
       77  num-righe        pic 99 value 0.
       77  sav-riga         pic x(900).
       77  num-page         pic 9(4).

       LINKAGE SECTION.
       copy "link-statraff.def".

      ******************************************************************
       PROCEDURE DIVISION using statraff-linkage.

       DECLARATIVES.
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
                move   "Lineseq"    to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova
                     perform OPEN-OUTPUT-LINESEQ
                when termina
                     set errori to true
                     display message "Operazione interrotta!"
                               title titolo
                                icon 2
                end-evaluate
           end-evaluate.

       STATRAFF-ERR SECTION.
           use after error procedure on statraff.
           set tutto-ok  to true.
           evaluate status-statraff
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File statistiche [STATRAFF] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [STATRAFF] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[STATRAFF] Indexed file corrupt!"
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
                move   "statraff"   to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova
                     open input statraff
                when termina
                     set errori to true
                     display message "Operazione interrotta!"
                               title titolo
                                icon 2
                end-evaluate
           end-evaluate

      ***---
       TMARCHE-ERR SECTION.
           use after error procedure on tmarche.
           set tutto-ok  to true.
           evaluate status-tmarche
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File marche [TMARCHE] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [TMARCHE] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[TMARCHE] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
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
      *-
           accept como-data from century-date.
           accept como-ora  from time.
           initialize wstampa.
           set tutto-ok       to true.
           accept  wstampa    from environment "PATH-ST".
           inspect wstampa    replacing trailing spaces by low-value.
           string  wstampa    delimited by low-value
                   "statraff" delimited by size
                   "_"        delimited by size
                   como-data  delimited by size
                   "_"        delimited by size
                   como-ora   delimited by size
                   ".txt"     delimited by size
                   into wstampa
           end-string.
           set prima-volta to true.

      ***---
       OPEN-FILES.
           perform OPEN-OUTPUT-LINESEQ.
           if tutto-ok
              open input statraff tmarche
              if errori
                 close lineseq
                 delete file lineseq
              end-if
           end-if.

      ***---
       OPEN-OUTPUT-LINESEQ.
           open output lineseq.

      ***---
       ELABORAZIONE.
           move  low-value        to str-rec.
      *****     move  statraff-anno    to str-anno.
           move  statraff-mese-da to str-mese.
           start statraff key is >=  str-chiave
                 invalid set errori to true
           end-start.
           if tutto-ok

              perform until 1 = 2
                 read statraff next no lock 
                      at end perform SCRIVI-TOTALI
                             exit perform 
                 end-read

      *****           if str-anno > statraff-anno
      *****              perform SCRIVI-TOTALI
      *****              exit perform
      *****           end-if

                 if str-mese > statraff-mese-a 
                    perform SCRIVI-TOTALI
                    exit perform 
                 end-if

                 if SaveMese not = str-mese
                    if SaveMese not = 0
                       perform SCRIVI-TOTALI
                    end-if
                    perform SCRIVI-INTESTAZIONE
                    set prima-volta to false
                    move str-mese  to SaveMese
                 end-if
                 
                 move str-marca to r-marca mar-codice
                 read tmarche no lock 
                      invalid continue 
                  not invalid if statraff-mensile perform SCRIVI-RIGA-1
                              else                perform SCRIVI-RIGA-2
                              end-if
                 end-read

              end-perform

           end-if.

      ***---
       SCRIVI-RIGA-1.
           move    mar-descrizione to r-descr.

           move    str-kg-corr   to r-kg-1.
           add     str-kg-corr   to tot-kg-1.

           move    str-vag-corr  to r-vag-1.
           add     str-vag-corr  to tot-vag-1.

           compute como-valore = str-vag-corr / str-kg-corr.
           move    como-valore   to r-indice-1.
      *****     add     como-valore   to tot-indice-1.

           move    str-kg-past   to r-kg-2.
           add     str-kg-past   to tot-kg-2.

           move    str-vag-past  to r-vag-2.
           add     str-vag-past  to tot-vag-2.

           compute como-valore = str-vag-past / str-kg-past.
           move    como-valore   to r-indice-2.
           add     como-valore   to tot-indice-2.

           compute como-kg = str-kg-corr - str-kg-past.
           move    como-kg   to r-kg-3.
           add     como-kg   to tot-kg-3.

           compute como-valore = ( str-vag-corr / str-kg-corr ) -
                                 ( str-vag-past / str-kg-past ).
           move    como-valore   to r-indice-3.
           add     como-valore   to tot-indice-3.

           compute como-valore = str-vag-corr - str-vag-past.
           move    como-valore   to r-vag-3.
           add     como-valore   to tot-vag-3.

           initialize line-riga.
           move riga to line-riga
           perform STAMPA-RIGA.

      ***---
       SCRIVI-RIGA-2.
           move    mar-descrizione to r-descr.

           move    str-kg-prog   to r-kg-1.
           add     str-kg-prog   to tot-kg-1.

           move    str-vag-prog  to r-vag-1.
           add     str-vag-prog  to tot-vag-1.

           compute como-valore = str-vag-prog / str-kg-prog.
           move    como-valore   to r-indice-1.
      *****     add     como-valore   to tot-indice-1.

           move    str-kg-prog-past   to r-kg-2.
           add     str-kg-prog-past   to tot-kg-2.

           move    str-vag-prog-past  to r-vag-2.
           add     str-vag-prog-past  to tot-vag-2.

           compute como-valore = str-vag-prog-past / str-kg-prog-past.
           move    como-valore   to r-indice-2.
           add     como-valore   to tot-indice-2.

           compute como-kg = str-kg-prog - str-kg-prog-past.
           move    como-kg   to r-kg-3.
           add     como-kg   to tot-kg-3.

           compute como-valore = ( str-vag-prog      /
                                   str-kg-prog ) -
                                 ( str-vag-prog-past /
                                   str-kg-prog-past ).
           move    como-valore   to r-indice-3.
           add     como-valore   to tot-indice-3.

           compute como-valore = str-vag-prog - str-vag-prog-past.
           move    como-valore   to r-vag-3.
           add     como-valore   to tot-vag-3.

           initialize line-riga.
           move riga to line-riga
           perform STAMPA-RIGA.

      ***---
       SCRIVI-INTESTAZIONE.
           move 5 to diff-righe.
           perform PRONOSTICO.

           if num-righe > 0
              move spaces to line-riga
              perform STAMPA-RIGA
           end-if.

           if prima-volta
              set prima-volta to false
              move 1 to num-page
              move num-page  to tit-page
              move riga-page to line-riga
              write line-riga
              add 1 to num-righe
           end-if.

           initialize line-riga.
           move riga-div-1 to line-riga
           perform STAMPA-RIGA.

           evaluate str-mese
           when  1 move "GENNAIO"   to tit-mese-1 tit-mese-2
           when  2 move "FEBBRAIO"  to tit-mese-1 tit-mese-2
           when  3 move "MARZO"     to tit-mese-1 tit-mese-2
           when  4 move "APRILE"    to tit-mese-1 tit-mese-2
           when  5 move "MAGGIO"    to tit-mese-1 tit-mese-2
           when  6 move "GIUGNO"    to tit-mese-1 tit-mese-2
           when  7 move "LUGLIO"    to tit-mese-1 tit-mese-2
           when  8 move "AGOSTO"    to tit-mese-1 tit-mese-2
           when  9 move "SETTEMBRE" to tit-mese-1 tit-mese-2
           when 10 move "OTTOBRE"   to tit-mese-1 tit-mese-2
           when 11 move "NOVEMBRE"  to tit-mese-1 tit-mese-2
           when 12 move "DICEMBRE"  to tit-mese-1 tit-mese-2
           end-evaluate.
           if statraff-mensile move "MENSILE"  to tipo-stampa
           else                move "CUMULATO" to tipo-stampa
           end-if.
           move statraff-anno(3:2)  to tit-anno-1.
           subtract 1 from tit-anno-1 giving tit-anno-2.

           initialize line-riga.
           move titolo-1 to line-riga
           perform STAMPA-RIGA.

           initialize line-riga.
           move titolo-2 to line-riga
           perform STAMPA-RIGA.

           initialize line-riga.
           move riga-div-1 to line-riga
           perform STAMPA-RIGA.

      ***---
       SCRIVI-TOTALI.
           move 4 to diff-righe.
           perform PRONOSTICO.

           initialize line-riga.
           move riga-div-2 to line-riga
           perform STAMPA-RIGA.

           compute tot-indice-1 = tot-vag-1 / tot-kg-1.
           compute tot-indice-2 = tot-vag-2 / tot-kg-2.
           compute tot-indice-3 = tot-indice-1 - tot-indice-2.

           move tot-kg-1     to t-kg-1.
           move tot-indice-1 to t-indice-1.
           move tot-vag-1    to t-vag-1.
           move tot-kg-2     to t-kg-2.
           move tot-indice-2 to t-indice-2.
           move tot-vag-2    to t-vag-2.
           move tot-kg-3     to t-kg-3.
           move tot-indice-3 to t-indice-3.
           move tot-vag-3    to t-vag-3.

           initialize line-riga.
           move riga-totale to line-riga
           perform STAMPA-RIGA.

           initialize line-riga.
           move riga-div-2 to line-riga
           perform STAMPA-RIGA.

           move 0 to tot-kg-1
                     tot-indice-1
                     tot-vag-1
                     tot-kg-2
                     tot-indice-2
                     tot-vag-2
                     tot-kg-3
                     tot-indice-3
                     tot-vag-3.

      ***---
       SALTO-PAGINA.
           move 0 to num-righe.
           write line-riga from space after page.
           write line-riga from x"09" after 1.
           add 1 to num-page.
           move num-page  to tit-page.
           move riga-page to line-riga.
           write line-riga.
           add 1 to num-righe.

      ***---
       STAMPA-RIGA.
           initialize sav-riga.
           move line-riga to sav-riga.
           if num-righe > max-righe
              perform SALTO-PAGINA
           end-if.
           move sav-riga to line-riga.
           write line-riga.
           add 1 to num-righe. 

      ***---
       PRONOSTICO.
           if num-righe > max-righe - diff-righe
              perform SALTO-PAGINA
           end-if.

      ***---
       CLOSE-FILES.
           close lineseq statraff tmarche.
  
      ***---
       EXIT-PGM.
           move wstampa to statraff-path.
           goback.
