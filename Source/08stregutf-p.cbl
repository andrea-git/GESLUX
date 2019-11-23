       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      stregutf-p.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "anautf.sl". 
           copy "movutf.sl".
           copy "clienti.sl".
           copy "lineseq.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "anautf.fd". 
           copy "movutf.fd".
           copy "lineseq.fd".
           copy "clienti.fd".

       WORKING-STORAGE SECTION.
           copy "link-geslock.def".
           copy "comune.def".

       78  titolo                  value "Stampa registro fiscale".
       78  MaxRows                 value 64.
       77  diff-righe              pic 99 value 0.
       77  n-vuote                 pic 99 value 0.

       77  status-anautf           pic x(2).
       77  status-movutf           pic x(2).
       77  status-clienti          pic x(2).
       77  status-lineseq          pic x(2).
       77  wstampa                 pic x(256).
       
       77  iniziale                pic s9(12)v999.
       77  finale                  pic s9(12)v999.
       77  RowCounter              pic 9(3) value 0.
       77  save-data               pic 9(8) value 0.
       77  como-data               pic 9(8).
       77  como-ora                pic 9(8).
       77  num-reg-edit            pic z(8).        
       77  progressivo             pic 9(10).
       77  tot-bianchi-carico      pic s9(12)v999.
       77  tot-bianchi-scarico     pic s9(12)v999.
       77  tot-altri-carico        pic s9(12)v999.
       77  tot-altri-scarico       pic s9(12)v999.
       77  tot-giacenza-bianchi    pic s9(12)v999.
       77  tot-giacenza-altri      pic s9(12)v999.

       77  tot-mov-giorno-c-bianchi pic s9(12)v999.
       77  tot-mov-giorno-s-bianchi pic s9(12)v999.
       77  tot-mov-giorno-c-lubrif  pic s9(12)v999.
       77  tot-mov-giorno-s-lubrif  pic s9(12)v999.

       01  filler                   pic x.
         88 giacenza-positiva-altri value "+".
         88 giacenza-negativa-altri value "-".

       01  filler                     pic x.
         88 giacenza-positiva-bianchi value "+".
         88 giacenza-negativa-bianchi value "-".

       01  filler                  pic 9.
         88 trovato-giorno         value 1, false 0.

       01  filler                  pic 9.
         88 record-ok              value 1, false 0.

       01  filler                  pic 9.
         88 prima-volta            value 1, false 0.

       01  filler                  pic 9.
         88 primo-passaggio        value 1, false 0.

      * RIGHE PER LA STAMPA
       01  riga-div                pic x(130) value all"-".

       01  tit-movim-data.
         03 filler                 pic x(130).

       01  r-titolo-1.
         03 filler                 pic x(30).
         03 filler                 pic x(23)
                                   value "ENTE EMITTENTE (carico)".
         03 filler                 pic x(17).
         03 filler                 pic x(24)
                                   value "Oli lubrificanti BIANCHI".
         03 filler                 pic x(11).
         03 filler                 pic x(22)
                                   value "Oli lubrificanti ALTRI".

       01  r-titolo-2.
         03 filler                 pic x(5).
         03 filler                 pic x(3) value "Nr.".
         03 filler                 pic x(4).
         03 filler                 pic x(4) value "Data".
         03 filler                 pic x(4).
         03 filler                 pic x(8) value "N. Docum".
         03 filler                 pic x(2).
         03 filler                 pic x(23)
                                   value "DESTINATARIO  (scarico)".
         03 filler                 pic x(18).
         03 filler                 pic x(6) value "Carico".
         03 filler                 pic x(9).
         03 filler                 pic x(7) value "Scarico".
         03 filler                 pic x(12).
         03 filler                 pic x(6) value "Carico".
         03 filler                 pic x(9).
         03 filler                 pic x(7) value "Scarico".

       01  r-riga.
         03 r-prog                 pic z(8).
         03 filler                 pic x(2).
         03 r-data                 pic x(8).
         03 filler                 pic x(2).
         03 r-num-doc              pic z(8).
         03 filler                 pic x(2).
         03 r-ragsoc               pic x(32).
         03 filler                 pic x(2).
         03 r-c-bianchi            pic z.zzz.zz9,999-.
         03 filler                 pic x(2).
         03 r-s-bianchi            pic z.zzz.zz9,999-.
         03 filler                 pic x(4).
         03 r-c-lubrif             pic z.zzz.zz9,999-.
         03 filler                 pic x(2).
         03 r-s-lubrif             pic z.zzz.zz9,999-.

       LINKAGE SECTION.
       copy "link-stregutf.def".

      ******************************************************************
       PROCEDURE DIVISION using stregutf-linkage.

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

       MOVUTF-ERR SECTION.
           use after error procedure on movutf.
           set tutto-ok  to true.
           evaluate status-movutf
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File movimenti utf [MOVUTF] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [MOVUTF] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[MOVUTF] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           when "93"
           when "99"
                set RecLocked to true
                set errori    to true
           end-evaluate. 

       CLIENTI-ERR SECTION.
           use after error procedure on clienti.
           set tutto-ok  to true.
           evaluate status-clienti
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File clienti [CLIENTI] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [CLIENTI] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[CLIENTI] Indexed file corrupt!"
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
                display message "File [CLIENTI] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[CLIENTI] Indexed file corrupt!"
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

      ***---
       MAIN-PRG.
           perform INIT.
           perform OPEN-FILES.
           if tutto-ok
              if stregutf-provvisoria perform READ-ANAUTF-NO-LOCK
              else                    perform READ-ANAUTF-LOCK
              end-if
              if tutto-ok
                 move stregutf-data-da      to save-data
                 perform until save-data > stregutf-data-a
                    move utf-num-prog-riga-reg to progressivo
                    set prima-volta to true
                    perform ELABORAZIONE
                    if trovato-giorno
                       perform VALORIZZA-CAMPI-ANAUTF
                       if stregutf-definitiva 
                          perform AGGIORNA-ANAUTF
                       end-if   
                       if errori
                          exit perform
                       end-if
                    end-if
                    perform AGGIUNGI-GIORNO
                 end-perform
                 if not trovato
                    display message "Nessun movimento UTF da elaborare!"
                              title titolo
                               icon 2
                 else
                    |SCARRELLAMENTO FINALE: PRONTO PER UN'ALTRA STAMPA
                    move 4 to n-vuote
                    perform RIGHE-VUOTE
                 end-if
              end-if
              unlock anautf all records
              perform CLOSE-FILES
           end-if.
           perform EXIT-PGM.

      ***---
       AGGIUNGI-GIORNO.
           move save-data to como-data.
           compute como-data =
                   function INTEGER_OF_DATE(save-data).
           add 1 to como-data.
           compute save-data =
                   function DATE_OF_INTEGER(como-data).

      ***---
       INIT.
           set RecLocked       to false.
           set tutto-ok        to true.
           set trovato         to false.
           set prima-volta     to true.
           set primo-passaggio to true.

           initialize wstampa.
           accept     como-data  from century-date.
           accept     como-ora   from time.
           accept     wstampa    from environment "PATH_ST".
           inspect    wstampa    replacing trailing spaces by low-value.
           string     wstampa    delimited low-value
                      "stregutf" delimited size
                      "_"        delimited size
                      como-data  delimited size
                      "_"        delimited size
                      como-ora   delimited size
                      ".txt"     delimited size
                      into wstampa
           end-string.

      ***---
       OPEN-FILES.
           perform OPEN-OUTPUT-LINESEQ.
           if tutto-ok
              open i-o anautf
              if tutto-ok
                 open input movutf clienti
                 if errori
                    close       lineseq
                    delete file lineseq
                 end-if
              else
                 close       lineseq
                 delete file lineseq
              end-if
           end-if.

      ***---
       OPEN-OUTPUT-LINESEQ.
           open output lineseq.

      ***---
       READ-ANAUTF-NO-LOCK.
           set  tutto-ok  to true.
           set  RecLocked to false.
           move stregutf-data-da(1:4)     to utf-anno.
           move stregutf-num-reg          to utf-num-reg.
           read anautf no lock invalid continue end-read.

      ***---
       READ-ANAUTF-LOCK.
           set  tutto-ok  to true.
           set  RecLocked to false.
           move stregutf-data-da(1:4)     to utf-anno.
           move stregutf-num-reg          to utf-num-reg.
           read anautf lock invalid continue end-read.
           if RecLocked
              initialize geslock-linkage
              move utf-num-reg to num-reg-edit
              string   "Il registro numero: " num-reg-edit
                x"0d0a""risulta in uso su altro terminale."
                x"0d0a""Questo comporta l'impossibilità ad"
                x"0d0a""essere aggiornato." delimited size
                    into geslock-messaggio
              end-string
              move 1 to geslock-v-riprova
              move 0 to geslock-v-ignora
              move 1 to geslock-v-termina
              move "anautf"       to geslock-nome-file
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova perform READ-ANAUTF-LOCK
              when termina display message box "Operazione interrotta!"
                                   title titolo
                                   icon 2
                           set errori to true
              end-evaluate
           end-if.
      
      ***---
       ELABORAZIONE.
           move 0 to tot-bianchi-carico
                     tot-bianchi-scarico
                     tot-altri-carico
                     tot-altri-scarico
                     tot-giacenza-bianchi
                     tot-giacenza-altri.

           move low-value             to mov-rec.
           move stregutf-data-da(1:4) to mov-anno.
           move stregutf-num-reg      to mov-num-reg.
           move save-data             to mov-data.
           set  mov-entrata           to true.
           set  trovato-giorno        to false.
           start movutf key is >= k-data-bolla
                 invalid set errori to true
           end-start.
           if tutto-ok
              perform until 1 = 2
                 read movutf next at end exit perform end-read
                 if mov-anno not = stregutf-data-da(1:4)
                    exit perform
                 end-if
                 if mov-num-reg not = stregutf-num-reg
                    exit perform
                 end-if
                 if mov-data > save-data
                    exit perform
                 end-if
                 if mov-kg not = 0
                    if mov-col-A or mov-col-D
                       if prima-volta
                          move 0 to RowCounter
                          perform SCRIVI-INTESTAZIONE
                          set prima-volta to false
                       end-if
                       perform VALORIZZA-RIGA
                    end-if
                 end-if
              end-perform
           end-if.
           if trovato-giorno
              perform SCRIVI-TOTALI
           end-if.

      ***---
       SCRIVI-INTESTAZIONE.
           if prima-volta
              perform SCRIVI-REGISTRO
           end-if.
           move spaces to tit-movim-data.
           string "MOVIMENTAZIONE IN DATA: " delimited size
                  save-data(7:2)             delimited size
                  "/"                        delimited size
                  save-data(5:2)             delimited size
                  "/"                        delimited size
                  save-data(3:2)             delimited size
                  into tit-movim-data
           end-string.
           call "C$JUSTIFY" using tit-movim-data, "C".
           move tit-movim-data to line-riga.
           write line-riga.
           add 1 to RowCounter.

           move riga-div to line-riga.
           write line-riga.
           add 1 to RowCounter.

           move r-titolo-1 to line-riga.
           write line-riga.
           add 1 to RowCounter.

           move r-titolo-2 to line-riga.
           write line-riga.
           add 1 to RowCounter.

           move riga-div to line-riga.
           write line-riga.
           add 1 to RowCounter.

           perform SCRIVI-RIPORTO.

      ***---
       VALORIZZA-RIGA.
           add 1 to progressivo.
           if RowCounter >= MaxRows - 1
              perform SALTO-PAGINA
              perform SCRIVI-INTESTAZIONE
           end-if.
           move mov-tipo-CF    to cli-tipo-CF.
           move mov-cod-clifor to cli-codice.
           read clienti no lock
                invalid move "** NON TROVATO **" to cli-ragsoc-1
           end-read.
           initialize r-riga.
           string save-data(7:2) delimited size
                  "/"            delimited size
                  save-data(5:2) delimited size
                  "/"            delimited size
                  save-data(3:2) delimited size
                  into r-data
           end-string.
           move progressivo  to r-prog.
           move mov-num-doc  to r-num-doc.
           move cli-ragsoc-1 to r-ragsoc.
           evaluate true
           when mov-col-A
                if mov-entrata move mov-kg to r-c-bianchi
                               add  mov-kg to tot-bianchi-carico
                               add  mov-kg to tot-mov-giorno-c-bianchi
                else           move mov-kg to r-s-bianchi
                               add  mov-kg to tot-bianchi-scarico
                               add  mov-kg to tot-mov-giorno-s-bianchi
                end-if
           when mov-col-D
                if mov-entrata move mov-kg to r-c-lubrif
                               add  mov-kg to tot-altri-carico
                               add  mov-kg to tot-mov-giorno-c-lubrif
                else           move mov-kg to r-s-lubrif
                               add  mov-kg to tot-altri-scarico
                               add  mov-kg to tot-mov-giorno-s-lubrif
                end-if
           end-evaluate.
           move r-riga to line-riga.
           write line-riga.
           add 1 to RowCounter.
           set trovato        to true.
           set trovato-giorno to true.

      ***---
       SCRIVI-TOTALI.
           if RowCounter > MaxRows - 4
              perform SALTO-PAGINA
           end-if.
           initialize r-riga.
           move "TOTALI GIORNO + RIPORTO" to r-ragsoc.
           move tot-bianchi-carico        to r-c-bianchi.
           move tot-bianchi-scarico       to r-s-bianchi.
           move tot-altri-carico          to r-c-lubrif.
           move tot-altri-scarico         to r-s-lubrif.
           move r-riga                    to line-riga.
           write line-riga.

           add 1 to RowCounter.
           initialize r-riga.
           move "TOTALI PROGRESSIVI ANNUALI" to r-ragsoc.
           add tot-mov-giorno-c-bianchi to utf-prog-e-bianchi.
           add tot-mov-giorno-s-bianchi to utf-prog-u-bianchi.
           add tot-mov-giorno-c-lubrif  to utf-prog-e-lubrif.
           add tot-mov-giorno-s-lubrif  to utf-prog-u-lubrif.

           move utf-prog-e-bianchi to r-c-bianchi.
           move utf-prog-u-bianchi to r-s-bianchi.
           move utf-prog-e-lubrif  to r-c-lubrif.
           move utf-prog-u-lubrif  to r-s-lubrif.

           move r-riga to line-riga.
           write line-riga.

           add 1 to RowCounter.
           initialize r-riga.
           move "GIACENZA FINE GIORNATA" to r-ragsoc.

           if tot-bianchi-carico >= tot-bianchi-scarico
              compute tot-giacenza-bianchi =
                      tot-bianchi-carico - tot-bianchi-scarico
              move tot-giacenza-bianchi to r-c-bianchi
              set giacenza-positiva-bianchi to true
           else
              compute tot-giacenza-bianchi =
                      tot-bianchi-scarico - tot-bianchi-carico
              move tot-giacenza-bianchi to r-s-bianchi
              set giacenza-negativa-bianchi to true
           end-if.

           if tot-altri-carico >= tot-altri-scarico
              compute tot-giacenza-altri =
                      tot-altri-carico - tot-altri-scarico
              move tot-giacenza-altri to r-c-lubrif
              set giacenza-positiva-altri to true
           else
              compute tot-giacenza-altri =
                      tot-altri-scarico - tot-altri-carico
              move tot-giacenza-altri to r-s-lubrif
              set giacenza-negativa-bianchi to true
           end-if.

           move r-riga to line-riga.
           write line-riga.
           add 1 to RowCounter.

           perform varying RowCounter from RowCounter by 1
                     until RowCounter = MaxRows
              move riga-div to line-riga
              write line-riga
           end-perform.

           move 0 to tot-mov-giorno-c-bianchi.
           move 0 to tot-mov-giorno-s-bianchi.
           move 0 to tot-mov-giorno-c-lubrif.
           move 0 to tot-mov-giorno-s-lubrif.

      ***---
       SCRIVI-RIPORTO.
           initialize r-riga.
           if prima-volta
              move "RIPORTO GIACENZA PRECEDENTE" to r-ragsoc
              move utf-giac-bianchi              to r-c-bianchi
                                                    tot-bianchi-carico
              move utf-giac-lubrif               to r-c-lubrif
                                                    tot-altri-carico
           else
              move "RIPORTO"            to r-ragsoc
              move tot-bianchi-carico   to r-c-bianchi
              move tot-bianchi-scarico  to r-s-bianchi
              move tot-altri-carico     to r-c-lubrif
              move tot-altri-scarico    to r-s-lubrif
           end-if.
           move r-riga to line-riga.
           write line-riga.
           add 1 to RowCounter.

      ***---
       SCRIVI-REGISTRO.
           |Per compensare il margine fisico iniziale
           if not primo-passaggio
              move 6 to n-vuote
              perform RIGHE-VUOTE
           else
              set primo-passaggio to false
           end-if.
           write line-riga from spaces.

LUBEXX
030806*    Adriano - originale
030806*     initialize line-riga.
030806*     move utf-num-reg-ass to line-riga(114:).
030806*    Adriano - originale fine

030806*    Adriano - Inizio modifica
030806     initialize line-riga.
030806     move utf-num-reg-ass to line-riga(115:).
030806*    Adriano - fine modifica

           write line-riga.
           move 2 to n-vuote. 
           perform RIGHE-VUOTE.

030806*    Adriano - originale
030806*     move 6 to RowCounter.
030806*    Adriano - fine originale

030806*    Adriano - modifica
030806     move 8 to RowCounter.
030806*    Adriano - fine modifica
LUBEXX

      ***---
       SALTO-PAGINA.
           perform SCRIVI-RIPORTO.
           compute diff-righe = MaxRows - RowCounter.
           move diff-righe to n-vuote.
           perform RIGHE-VUOTE.
           move 0 to RowCounter.

           perform SCRIVI-REGISTRO.

      ***---
       RIGHE-VUOTE.
           perform n-vuote times
              write line-riga from spaces
           end-perform.
           add n-vuote to RowCounter.

      ***---
       AGGIORNA-ANAUTF.
           move stregutf-data-a      to utf-data-ult-stampa.
           rewrite utf-rec invalid continue end-rewrite.
           perform READ-ANAUTF-LOCK.

      ***---
       VALORIZZA-CAMPI-ANAUTF.
           if giacenza-positiva-bianchi
              move tot-giacenza-bianchi to utf-giac-bianchi
           else
              compute utf-giac-bianchi =
                    ( tot-giacenza-bianchi -
                    ( tot-giacenza-bianchi * 2 ) ) 
           end-if.

           if giacenza-positiva-altri
              move tot-giacenza-altri   to utf-giac-lubrif
           else
              compute utf-giac-lubrif =
                    ( tot-giacenza-altri -
                    ( tot-giacenza-altri * 2 ) ) 
           end-if.
           move progressivo          to utf-num-prog-riga-reg.

      ***---
       CLOSE-FILES.
           close lineseq anautf movutf clienti.
           if not trovato
              delete file lineseq
              move spaces to wstampa
           end-if.

      ***---
       EXIT-PGM.
           move wstampa to stregutf-path.
           goback.
