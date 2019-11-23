       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      SHI-agg-ord-exp.
       AUTHOR.                          Luciano.
       REMARKS. Aggiornamento delle bozze ordini
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tordini.sl".
           copy "tmp-tordini-exp.sl".
           copy "fileseq.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tordini.fd".
           copy "tmp-tordini-exp.fd".
           copy "fileseq.fd".

       WORKING-STORAGE SECTION.
       copy "exp-ws.def".

       78  titolo                  value "Export Vettori".

       77  status-tordini          pic xx.
       77  status-tmp-tordini-exp  pic xx.
       77  path-tmp-tordini-exp    pic x(256).

       01  filler                  pic 9.
           88 prima-volta          value 1 false zero. 

       01  FlagLocked       PIC  9.
           88 RecLocked VALUE IS 1    WHEN SET TO FALSE  0. 

       77  tor-numero-ed     pic z(8).

       77  num-rec-righe     pic 9(10).
       77  num-rec-note      pic 9(10).
       77  num-rec-art       pic 9(10).

       77  como-utente       pic 9(10).

       77  como-data         pic 9(8).
       77  como-ora          pic 9(8).



       LINKAGE SECTION.
           copy "link-exp.def".
           copy "link-expordini.def".

      ******************************************************************
       PROCEDURE DIVISION using exp-linkage, expordini-linkage.

       DECLARATIVES.
      ***---
       tordini-ERR SECTION.
           use after error procedure on tordini.
           set tutto-ok  to true.
           evaluate status-tordini
           when "35"
                move 
             "Impossibile procedere. File vettori [tordini] inesistente"
                                                       to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           when "39"
                move "File [tordini] Mismatch size!"   to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           when "98"
                move "[tordini] Indexed file corrupt!" to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           when "93"
           when "99"
                set RecLocked to true
           end-evaluate.

      ***---
       TMP-TODINI-EXP-ERR SECTION.
           use after error procedure on TMP-TORDINI-EXP.
           set tutto-ok  to true.
           evaluate status-TMP-TORDINI-EXP
           when "35"
                move 
           "Impossibile procedere. File [TMP-TORDINI-EXP] inesistente"
                       to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           when "39"
                move "File [TMP-TORDINI-EXP] Mismatch size!"  
                                         to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           when "98"
                move "[TMP-TORDINI-EXP] Indexed file corrupt!"
                                                       to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
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
           move exp-ord-path to path-tmp-tordini-exp

           accept como-utente from environment "USER_IMPORT_ARTICOLI"
           perform INITIALIZE-FLAG
      *     move zero   to num-rec-righe
      *                    num-rec-note
      *                    num-rec-prodener
      *                    num-rec-ean
      *                    num-rec-art.

           set exp-ordini       to true.

           set prima-volta      to true
           set tutto-ok         to true.


      ***---
       OPEN-FILES.
           open i-o   tordini
           open input  tmp-tordini-exp.

      ***---
       ELABORAZIONE.
           move low-value          to tmp-tor-chiave
 
           start tmp-tordini-exp key not < tmp-tor-chiave
              invalid  
                 set errori to true
           end-start.
           if tutto-ok
              perform until 1 = 2
                 read tmp-tordini-exp next no lock
                    at end       
                       exit perform 
                 end-read
                    
                 perform AGGIORNA-TORDINE
              end-perform
           end-if.

           perform SCRIVI-RIEPILOGO.

      ***---
       SCRIVI-RIEPILOGO.       
           initialize como-messaggio
           move num-rec-exp  to num-rec-ed
           call "C$justify" using num-rec-ed, "L"
           inspect num-rec-ed   replacing trailing space by low-value
           string "Aggiornati " delimited by size
                   num-rec-ed   delimited by low-value
                   " ordini."   delimited by size
                   into como-messaggio
           perform SCRIVI-MESSAGGIO

           if num-rec-no-exp not = zero
              move num-rec-no-exp  to num-rec-ed
              initialize como-messaggio
              call "C$justify" using num-rec-ed, "L"
              inspect num-rec-ed replacing trailing space by low-value
              string "Non aggiornati " delimited size
                     num-rec-ed        delimited low-value
                     " ordini."        delimited size
                   into como-messaggio
              perform SCRIVI-MESSAGGIO
           end-if.

      ***---
       AGGIORNA-TORDINE.
           move tmp-tor-chiave to tor-chiave.
           perform READ-TORDINE-LOCK.
           if tutto-ok
              perform EXP-ORDINE
           end-if.

      ***---
       READ-TORDINE-LOCK.
           set RecLocked to false.

           set tutto-ok to true.
           read tordini with lock 
                invalid continue 
           end-read.

           if RecLocked
              initialize como-messaggio
              move tor-numero   to tor-numero-ed
              call "C$JUSTIFY" using tor-numero-ed, "L"
              inspect tor-numero-ed 
                                replacing trailing space by low-value
              string "Ordine "           delimited by size
                     tor-anno            delimited by size
                     "/"                 delimited by size
                     tor-numero-ed       delimited by low-value
                     " non aggiornato "  delimited by size
                     "in quanto già in uso su un altro terminale"
                                         delimited by size
                     into como-messaggio
              set errore-bloccante  to false
              perform SCRIVI-ERORRE
              read tordini no lock
              unlock tordini all records
              add 1 to num-rec-no-exp
           end-if.


      ***---
       EXP-ORDINE.
           add 1 to num-rec-exp
           set tor-da-inviare-no to true.

           move como-utente     to tor-utente-ultima-modifica
           accept tor-data-ultima-modifica  from century-date
           accept tor-ora-ultima-modifica   from time

           rewrite TOR-REC
               invalid
                 continue
           end-rewrite.



      ***---
       CLOSE-FILES.
           close tordini.
           close tmp-tordini-exp.
           delete file tmp-tordini-exp.


      ***---
           COPY "exp-procedure.cpy".

