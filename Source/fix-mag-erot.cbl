       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      fix-mag-erot.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "rmovmag.sl".
           copy "progmag.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.          
           copy "rmovmag.fd".
           copy "progmag.fd".

       WORKING-STORAGE SECTION.
           COPY "comune.def".
           copy "mail.def".

       78  titolo    value "Controllo evasioni senza giacenza".

       01  int-1.
         05 filler          pic x(8)  value "ARTICOLO".
         05 filler          pic x(2).
         05 filler          pic x(11) value "DESCRIZIONE".
         05 filler          pic x(23).
         05 filler          pic x(9)  value "IMPEGNATO".
         05 filler          pic x(6).
         05 filler          pic x(9)  value " GIACENZA".

       01  riga-stampa.
         05 filler          pic x(2).
         05 r-articolo      pic z(6).
         05 filler          pic x(2).
         05 r-descrizione   pic x(28).
         05 filler          pic x(5).
         05 r-impegnato     pic --.---.--9.
         05 filler          pic x(5).
         05 r-giacenza      pic --.---.--9.
      *****   05 filler          pic x(9)  value "IMPEGNATO".
      *****   05 filler          pic x(2).
      *****   05 filler          pic x(9)  value " GIACENZA".

       77  status-lineseq   pic xx.  
       77  status-tsetinvio pic xx.
       77  status-progmag   pic xx.
       77  status-articoli  pic xx.
       77  wstampa          pic x(256).

       77  impegnato        pic s9(8).
       77  giacenza         pic s9(8).
       77  save-articolo    pic 9(6).

       77  como-data        pic 9(8).
       77  como-ora         pic 9(8).
       77  tentativi        pic 9(5).

       LINKAGE SECTION.
       77  save-magazzino   pic x(3).
       77  link-user        pic x(10).

      ******************************************************************
       PROCEDURE DIVISION USING save-magazzino.

       MAIN-PRG.
           perform INIT.
           perform OPEN-FILES.
           perform ELABORAZIONE.
           perform CLOSE-FILES.
           perform EXIT-PGM.

      ***---
       INIT.
           set trovato     to false.
           initialize wstampa.
           accept wstampa   from environment "PATH_ST".
           accept como-data from century-date.
           accept como-ora  from time
           inspect wstampa  replacing trailing spaces by low-value.
           string  wstampa      delimited low-value
                   "tmp_genera" delimited size
                   "_"          delimited size
                   como-data    delimited size
                   "_"          delimited size
                   como-ora     delimited size
                   ".txt"       delimited size
                   into wstampa
           end-string.

      ***---
       OPEN-FILES.
           open  input progmag articoli.
           open output lineseq.

      ***---
       ELABORAZIONE.
           move 0 to giacenza impegnato save-articolo.
           move low-value      to prg-rec.
           move save-magazzino to prg-cod-magazzino.
           start progmag key >= key01
                 invalid continue
           end-start.
           perform until 1 = 2
              read progmag next at end exit perform end-read
              if prg-cod-magazzino not = save-magazzino
                 exit perform
              end-if
              if save-articolo = 0
                 move prg-cod-articolo to save-articolo
              end-if
              if save-articolo not = prg-cod-articolo
                 if impegnato > giacenza
                    if not trovato
                       set trovato to true
                       move int-1 to line-riga of lineseq
                       write line-riga of lineseq
                    end-if
                    move prg-cod-articolo to art-codice
                    read articoli no lock
                    move art-codice      to r-articolo
                    move art-descrizione to r-descrizione
                    move impegnato       to r-impegnato
                    move giacenza        to r-giacenza
                    move riga-stampa to line-riga of lineseq
                    write line-riga of lineseq

                    move 0 to impegnato giacenza
                 end-if
              end-if

              compute impegnato =
                      impegnato +
                  prg-impegnato - ( prg-imp-gdo + prg-imp-trad )
              compute giacenza  =
                      giacenza  +
                  prg-giacenza

           end-perform.


      ***---
       CLOSE-FILES.
           close progmag lineseq articoli.
           if not trovato
              delete file lineseq
           else
              initialize LinkBody
              accept como-ora  from time
              accept como-data from century-date
              string "DATA: "                  delimited size
                     como-data(7:2)            delimited size
                     "/"                       delimited size
                     como-data(5:2)            delimited size
                     "/"                       delimited size
                     como-data(1:4)            delimited size
                     " - ORA: "                delimited size
                     como-ora(1:2)             delimited size
                     ":"                       delimited size
                     como-ora(3:2)             delimited size
                     " - UTENTE: "             delimited size
                     link-user                 delimited low-value
                     ". In allegato dettagli " delimited size
                     "problemi riscontrati."   delimited size
                     into LinkBody
              end-string

              move wstampa to LinkAttach

              move "EVASIONE ARTICOLI SENZA GIACENZA" to LinkSubject
              move "wvetrugno@lubex.it;a.eventi@goodworks.it" 
                to LinkAddress

              set errori to true
              move 0 to tentativi 
              move "prog-after-evas" to NomeProgramma
              perform 5 times
                 add 1 to tentativi
                 perform SEND-MAIL
                 open input lineseq1
                 read  lineseq1 next
                 if line-riga of lineseq1 = "True"
                    set tutto-ok to true
                    close lineseq1
                    exit perform
                 end-if
                 close lineseq1
              end-perform

              if errori
                 display message box 
                       "Errore durante l'invio della mail riepologativa"
                          title titolo
                          icon 2
              end-if
           end-if.

      ***---
       EXIT-PGM.
           goback.

      ***---
       PARAGRAFO-COPY.
           copy "mail.cpy".
