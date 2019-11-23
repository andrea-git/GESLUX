       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      check-prog-p.
       AUTHOR.                          Andrea.
       REMARKS. Controllo (da data a data) che i movimenti 
                creati abbiano i relativi progressivi presenti

      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tmovmag.sl".
           copy "rmovmag.sl".
           copy "progmag.sl".
           copy "lineseq.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tmovmag.fd".
           copy "rmovmag.fd".
           copy "progmag.fd".
           copy "lineseq.fd".

       WORKING-STORAGE SECTION.
      * COSTANTI
       78  titolo              value "CONTROLLO PROGRESSIVI".

      *FILE-STATUS
       77  status-tmovmag        pic xx.
       77  status-rmovmag        pic xx.
       77  status-progmag        pic xx.
       77  status-lineseq        pic xx.

       77  wstampa               pic x(256).

      * VARIABILI 
       77  counter               pic 9(10).
       77  counter2              pic 9(10).
       77  counter-edit          pic z(10).
       77  peso-utf              pic 9(3)v999.
       77  peso-non-utf          pic 9(3)v999.
       77  como-data             pic 9(8).
       77  como-ora              pic 9(8).

       01  r-intesta.
           05 filler             pic x(6) value "CODART".
           05 filler             pic x(2).
           05 filler             pic x(3) value "MAG".
           05 filler             pic x(2).
           05 filler             pic x(3) value "IMB".
           05 filler             pic x(2).
           05 filler             pic x(7) value "   PESO".
           05 filler             pic x(2).
           05 filler             pic x(7) value "    UTF".
           05 filler             pic x(2).
           05 filler             pic x(7) value "NON UTF".

       01  r-riga.
           05 r-art              pic z(6).
           05 filler             pic x(2).
           05 r-mag              pic x(3).
           05 filler             pic x(2).
           05 r-imb              pic x(3).
           05 filler             pic x(2).
           05 r-peso             pic zz9,999.
           05 filler             pic x(2).
           05 r-utf              pic zz9,999.
           05 filler             pic x(2).
           05 r-non-utf          pic zz9,999.

      * FLAGS
       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".
       77  filler                pic 9.
           88 trovato            value 1, false 0.

       LINKAGE SECTION.
       77  como-data-from        pic 9(8).
       77  como-data-to          pic 9(8).
       77  link-user             pic x(20).
       77  link-handle           handle of window.

      ******************************************************************
       PROCEDURE DIVISION using como-data-from 
                                como-data-to
                                link-user      
                                link-handle.
                                

       DECLARATIVES.
      ***---
       TMOVMAG-ERR SECTION.
           use after error procedure on tmovmag.
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
           end-evaluate.
      ***---
       RMOVMAG-ERR SECTION.
           use after error procedure on rmovmag.
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
           end-evaluate.

      ***---
       PROGMAG-ERR SECTION.
           use after error procedure on progmag.
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
           move 0 to counter counter2.
           set tutto-ok to true.
           set trovato  to false.

      ***---
       OPEN-FILES.
           open input tmovmag rmovmag progmag.
           accept wstampa   from environment "PATH_ST".
           accept como-data from century-date.
           accept como-ora  from time.
           inspect wstampa replacing trailing spaces by low-value.
           string  wstampa       delimited low-value
                   "CHECK-PROG_" delimited size
                   como-data     delimited size
                   "_"           delimited size
                   como-ora      delimited size
                   into wstampa
           end-string.
           open output lineseq.

      ***---
       ELABORAZIONE.
           move low-value      to tmo-rec.
           move como-data-from to tmo-data-movim.
           start tmovmag key >= k-data
                 invalid set errori to true
           end-start.
      
           if tutto-ok

              perform until 1 = 2
                 read tmovmag next at end exit perform end-read

                 if tmo-data-movim > como-data-to
                    exit perform
                 end-if

                 add 1 to counter
                 add 1 to counter2
                 if counter2 = 200
                    move counter to counter-edit
                    display counter-edit 
                       upon link-handle at column 22,00 line 09,00
                    move 0 to counter2
                 end-if

                 move low-value  to rmo-chiave
                 move tmo-chiave to rmo-chiave
                 start rmovmag key >= rmo-chiave
                       invalid continue
                   not invalid
                       perform until 1 = 2
                          read rmovmag next at end exit perform end-read
                          if rmo-anno  not = tmo-anno or
                             rmo-movim not = tmo-numero
                             exit perform
                          end-if
                          move rmo-chiave-progmag to prg-chiave
                          read progmag no lock
                               invalid
                               perform SCRIVI-REPORT
                          end-read
                       end-perform
                 end-start
              end-perform
           end-if.

           if not trovato
              display message "Trovati tutti i progressivi"
                        title titolo
                         icon 2
           else
              call   "spooler-a" using "S", wstampa, "V"
              cancel "spooler-a"
           end-if.

      ***---
       SCRIVI-REPORT.
           if not trovato
              set trovato to true
              move "** PROGRESSIVI DA CREARE **" to line-riga
              write line-riga
              write line-riga from spaces
              write line-riga from r-intesta
           end-if.

           divide rmo-peso-tot     by rmo-qta giving peso-non-utf.
           divide rmo-peso-tot-utf by rmo-qta giving peso-utf.
           move prg-cod-articolo   to r-art.
           move prg-cod-magazzino  to r-mag.
           move prg-tipo-imballo   to r-imb.
           move prg-peso           to r-peso.
           move peso-non-utf       to r-non-utf.
           move peso-utf           to r-utf.
           write line-riga from r-riga.

      ***--
       CLOSE-FILES.
           close  tmovmag
                  rmovmag
                  progmag
                  lineseq.
           if not trovato
              delete file lineseq
           end-if.
      
      ***---
       EXIT-PGM.
           display "                                                   "
              upon link-handle at column 22,00 line 10,00.
           goback.
