       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      check-cli-eva.
       AUTHOR.                          Andrea.
       REMARKS. Controllo che le evasioni abbiano lo stesso
                cliente presente sul master di riferimento

      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "mtordini.sl".
           copy "mrordini.sl".
           copy "tordini.sl".
           copy "rordini.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "mtordini.fd".
           copy "mrordini.fd".
           copy "tordini.fd".
           copy "rordini.fd".

       WORKING-STORAGE SECTION.
      * COSTANTI
       78  titolo              value "CONTROLLO CLIENTE EVASIONE".

      *FILE-STATUS
       77  status-mtordini       pic xx.
       77  status-mrordini       pic xx.
       77  status-tordini        pic xx.
       77  status-rordini        pic xx.

      * VARIABILI 
       77  counter               pic 9(10).
       77  counter2              pic 9(10).
       77  counter-edit          pic z(10).
       77  peso-utf              pic 9(5)v999.
       77  peso-non-utf          pic 9(5)v999.
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

      ******************************************************************
       PROCEDURE DIVISION.

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
           open input mtordini mrordini tordini rordini.
      *****     accept wstampa   from environment "PATH_ST".
      *****     accept como-data from century-date.
      *****     accept como-ora  from time.
      *****     inspect wstampa replacing trailing spaces by low-value.
      *****     string  wstampa       delimited low-value
      *****             "CHECK-PROG_" delimited size
      *****             como-data     delimited size
      *****             "_"           delimited size
      *****             como-ora      delimited size
      *****             into wstampa
      *****     end-string.
      *****     open output lineseq.

      ***---
       ELABORAZIONE.
           move low-value to ror-rec.
           move 2009      to ror-anno.
           start rordini key >= ror-chiave
                 invalid set errori to true
           end-start.
      
           if tutto-ok

              perform until 1 = 2
                 read rordini next at end exit perform end-read

      *****           add 1 to counter
      *****           add 1 to counter2
      *****           if counter2 = 200
      *****              move counter to counter-edit
      *****              display counter-edit 
      *****                 upon link-handle at column 22,00 line 09,00
      *****              move 0 to counter2
      *****           end-if

                 move ror-chiave-ordine-testa to mro-chiave-testa
                 move ror-progr-master        to mro-progr
                 read mrordini no lock key mro-k-progr
                      invalid continue
                  not invalid
                      move mro-chiave-testa to mto-chiave
                      read mtordini no lock
                           invalid continue
                       not invalid
                           move ror-anno       to tor-anno
                           move ror-num-ordine to tor-numero
                           read tordini no lock
                           if mto-cod-cli     not = tor-cod-cli  or
                              mto-prg-destino not = tor-prg-destino
                              if tor-causale  not = "OMOM"
                                 display message "EVASIONE " tor-numero
                                           title titolo
                                            icon 2
                              end-if
                           end-if
                     end-read
                 end-read
              end-perform
           end-if.

           display message "FINE"
                     title titolo.

      *****           move low-value  to rmo-chiave
      *****           move tmo-chiave to rmo-chiave
      *****           start rmovmag key >= rmo-chiave
      *****                 invalid continue
      *****             not invalid
      *****                 perform until 1 = 2
      *****                    read rmovmag next at end exit perform end-read
      *****                    if rmo-anno  not = tmo-anno or
      *****                       rmo-movim not = tmo-numero
      *****                       exit perform
      *****                    end-if
      *****                    move rmo-chiave-progmag to prg-chiave
      *****                    read progmag no lock
      *****                         invalid
      *****                         perform SCRIVI-REPORT
      *****                    end-read
      *****                 end-perform
      *****           end-start
      *****        end-perform
      *****     end-if.
      *****
      *****     if not trovato
      *****        display message "Trovati tutti i progressivi"
      *****                  title titolo
      *****                   icon 2
      *****     else
      *****        call   "spooler-a" using "S", wstampa, "V"
      *****        cancel "spooler-a"
      *****     end-if.

      ********---
      ***** SCRIVI-REPORT.
      *****     if not trovato
      *****        set trovato to true
      *****        move "** PROGRESSIVI DA CREARE **" to line-riga
      *****        write line-riga
      *****        write line-riga from spaces
      *****        write line-riga from r-intesta
      *****     end-if.
      *****
      *****     divide rmo-peso-tot     by rmo-qta giving peso-non-utf.
      *****     divide rmo-peso-tot-utf by rmo-qta giving peso-utf.
      *****     move prg-cod-articolo   to r-art.
      *****     move prg-cod-magazzino  to r-mag.
      *****     move prg-tipo-imballo   to r-imb.
      *****     move prg-peso           to r-peso.
      *****     move peso-non-utf       to r-non-utf.
      *****     move peso-utf           to r-utf.
      *****     write line-riga from r-riga.

      ***--
       CLOSE-FILES.
           close  mtordini
                  mrordini
                  tordini
                  rordini.
      *****     if not trovato
      *****        delete file lineseq
      *****     end-if.
      
      ***---
       EXIT-PGM.
      *****     display "                                                   "
      *****        upon link-handle at column 22,00 line 10,00.
           goback.
