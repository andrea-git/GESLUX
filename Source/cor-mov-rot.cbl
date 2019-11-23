       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      cor-mov-rot.
       AUTHOR.                          Luciano.
       REMARKS.
           Correzione movimenti magazino rot. 

      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *     copy "progmag.sl".
           copy "rmovmag.sl".
           copy "tmovmag.sl".
           copy "lineseq.sl".

      * SELECT rep-listini
      *     ASSIGN       TO  path-rep-listini
      *     ORGANIZATION IS LINE SEQUENTIAL
      *     ACCESS MODE  IS SEQUENTIAL
      *     FILE STATUS  IS STATUS-rep-listini.


      *****************************************************************

       DATA DIVISION.
       FILE SECTION.
      *     copy "progmag.fd".
           copy "rmovmag.fd".
           copy "tmovmag.fd".
           copy "lineseq.fd".

      * FD  rep-listini.
      * 01 rlst-rec         PIC  x(100).
      *

      *****************************************************************

       WORKING-STORAGE SECTION.
      * COPY
           copy "acugui.def".
           copy "common-excel.def".
           copy "link-geslock.def".
      * COSTANTI
       78  titolo 
                 value "Correzione movimenti rot".

       01        pic 9.
           88 reclocked value 1 false zero.

      * FILE-STATUS
      * 77  status-progmag          pic xx.
       77  status-rmovmag          pic xx.
       77  status-tmovmag          pic xx.
       77  status-lineseq          pic xx.
       77  status-rep-listini      pic xx.
       77  wstampa                 pic x(256).
       77  path-rep-listini        pic x(256).

      * VARIABILI
       01  riga-errore.
           05 filler               pic x(10) value "Record n° ".
           05 re-num-rec           pic z(6).
           05 filler               pic x(2) value ": ".
           05 r-err                pic x(80).

       01  riga-listino.
           10 rl-anno           PIC x(50).
           10 rl-numero         PIC x(50).
           10 rl-riga           pic x(50).

      *
       77  como-data                 pic 9(8).
       77  como-ora                  pic 9(8).
       77  rec-ko                    pic 9(5) value 0.
       77  rec-ok                    pic 9(5) value 0.
       77  num-rec                   pic 9(5) value 0.
       77  num-rec-ed                pic zz.zzz.
       77  counter                   pic 9(10).
       77  counter2                  pic 9(10).
       77  counter-edit              pic z(10).

      * FLAGS
       01  controlli                 pic xx.
         88 tutto-ok                 value "OK".
         88 errori                   value "ER".

       01  filler                    pic 9.
         88 record-ok                value 1, false 0.

       01  filler                    pic 9.
         88 primo-errore             value 1, false 0.

       01  tipo-errore               pic 9.
           88 no-cliente             value 1.
           88 cliente-lock           value 2.
      *     88 no-mag-std             value 3.
      *     88 mag-errato             value 4.

      *****************************************************************

       LINKAGE SECTION.
       77  user-codi   pic x(10).

       PROCEDURE DIVISION using user-codi.

       DECLARATIVES.

      ****---
      * progmag-ERR SECTION.
      *     use after error procedure on progmag.
      *     set tutto-ok  to true.
      *     evaluate status-progmag
      *     when "35"
      *          set errori to true
      *          display message "File [progmag] not found!"
      *                    title titolo
      *                     icon 3
      *     when "39"
      *          set errori to true
      *          display message "File [progmag] mismatch size!"
      *                    title titolo
      *                     icon 3
      *     when "98"
      *          set errori to true
      *          display message "[progmag] Indexed file corrupt!"
      *                    title titolo
      *                     icon 3
      *     when "93"
      *     when "99"
      *          SET reclocked  to true
      *     end-evaluate.

      ***---
       RMOVMAG-ERR SECTION.
           use after error procedure on rmovmag.
           set tutto-ok  to true.
           evaluate status-rmovmag
           when "35"
                set errori to true
                display message "File [rmovmag] not found!"
                          title titolo
                           icon 3
           when "39"
                set errori to true
                display message "File [rmovmag] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[rmovmag] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "93"
           when "99"
                SET reclocked  to true
           end-evaluate.

      ***---
       TMOVMAG-ERR SECTION.
           use after error procedure on tmovmag.
           set tutto-ok  to true.
           evaluate status-tmovmag
           when "35"
                set errori to true
                display message "File [tmovmag] not found!"
                          title titolo
                           icon 3
           when "39"
                set errori to true
                display message "File [tmovmag] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[tmovmag] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "93"
           when "99"
                SET reclocked  to true
           end-evaluate.


      ***---
       LINESEQ-ERR SECTION.
           use after error procedure on lineseq.
           set tutto-ok  to true.
           evaluate status-lineseq
           when "35"
                set errori to true
                display message "File [LINESEQ] not found!"
                          title titolo
                           icon 3
           when "39"
                set errori to true
                display message "File [LINESEQ] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[LINESEQ] Indexed file corrupt!"
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
           perform ACCETTA-SEPARATORE.
           set reclocked to false
           set primo-errore  to true.
           accept como-data  from century-date.
           accept como-ora   from time.
           move 0 to counter counter2.
           set tutto-ok to true.

           move "mov-errati.csv"  to wstampa.

      ***---
       OPEN-FILES.
      *     open i-o progmag.
           open i-o rmovmag.
           open i-o tmovmag.
           if tutto-ok
              open input lineseq 
              if tutto-ok
      *    tolgo l'intestazione
                 initialize line-riga
                 read LINESEQ next
                    at end
                       continue
                 end-read
              end-if
           end-if.

           if errori 
              goback 
           end-if.

      ***---
       ELABORAZIONE.
           set tutto-ok   to true
           accept como-data from century-date
           accept como-ora  from time

           move 0 to num-rec.
           perform until 1 = 2 

      *        perform DISPLAY-UPON-SCREEN

              set record-ok to true
              initialize tipo-errore

              initialize line-riga
              read lineseq next 
                 at end 
                    exit perform 
              end-read
              if line-riga = spaces    
                 exit perform 
              end-if
              initialize riga-listino
              unstring line-riga delimited by separatore
                       into rl-anno
                            rl-numero
                            rl-riga

              end-unstring
              perform TRATTA-RECORD

           end-perform.

           display message box "Correzione terminata."
                               x"0D0A"
                               "corrette " num-rec
                               " righe di movimenti".

      *     if rec-ko > 0
      *        write rlst-rec from spaces
      *        move all "-" to rlst-rec
      *        write rlst-rec 
      *        write rlst-rec from spaces
      *        move rec-ko to num-rec-ed
      *        string "Totale righe scartate: " delimited size
      *               num-rec-ed                delimited size
      *               into rlst-rec
      *        end-string
      *        write rlst-rec  after 2
      *        close REP-LISTINI
      *     end-if.
      *
      *     if rec-ko > 0
      *        display message "Operazione terminata con errori!"
      *                x"0d0a""====================="
      *                x"0d0a""RIEPILOGO:"
      *                x"0d0a"
      *               x"0d0a""Totale righe trattate " num-rec, " di cui:"
      *                x"0d0a"" - " rec-ok, " importate"
      *                x"0d0a"" - " rec-ko, " errate"
      *                x"0d0a""Sarà visualizzato report riepilogativo..."
      *                title titolo
      *                 icon 2
      *     else
      *        display message "Operazione conclusa con sucesso!"
      *                x"0d0a""====================="
      *                x"0d0a""RIEPILOGO:"
      *                x"0d0a"
      *                x"0d0a""Totale righe trattate " num-rec, 
      *                title titolo
      *     end-if.


      ***---
       CLOSE-FILES.
           close |progmag
                 rmovmag
                 tmovmag
                 lineseq.
      *     if rec-ko > 0 
      *        call   "spooler-a" using "A", path-rep-listini, "O"
      *        cancel "spooler-a"
      *        delete file rep-listini
      *     end-if.
              

      ***---
       EXIT-PGM.
           goback.

      ***---
       TRATTA-RECORD.
           set tutto-ok   to true



           move rl-anno   to rmo-anno convert
           move rl-numero to rmo-movim convert
           move rl-riga   to rmo-riga convert


           read rmovmag no lock
              invalid
                 continue
              not invalid
                 perform ELABORA-RIGA
           end-read.

      ***---
       ELABORA-RIGA.
           if rmo-peso-tot-utf = zero
              exit paragraph
           end-if.


      *    controllo che non sia in uso la testa
           move rmo-anno  to tmo-anno
           move rmo-movim to tmo-numero

           perform READ-RECORD-LOCK
           if tutto-ok
              perform SCAMBIA-PESI
           end-if.



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
       SCAMBIA-PESI.
           add 1 to num-rec
           move rmo-peso-tot-utf   to rmo-peso-tot
           move zero               to rmo-peso-tot-utf   

           rewrite rmo-rec
              invalid
                 continue
           end-rewrite

           move zero         to tmo-peso-utf
           move low-value    to rmo-riga
           start rmovmag key not < rmo-chiave
              invalid
                 continue
              not invalid
                 perform until 1 = 2
                    read rmovmag next no lock
                       at end 
                          exit perform
                    end-read
                    if rmo-anno    not = tmo-anno or
                       rmo-movim   not = tmo-numero
                       exit perform
                    end-if

                    add rmo-peso-tot-utf to tmo-peso-utf
                 end-perform
           end-start.

           rewrite tmo-rec
              invalid
                 continue
           end-rewrite

           unlock tmovmag  all records.            

      ****---
      * SCRIVI-ERRORI.
      *     if primo-errore
      *        set primo-errore  to false
      *        open output rep-listini
      *
      *        move "Importazione fido progmag: riepilogo errori"  
      *                                         to rlst-rec
      *        call "c$justify" using rlst-rec, "C"
      *        write rlst-rec
      *        move all "-"   to rlst-rec
      *        write rlst-rec
      *     end-if
      *
      *     move num-rec   to re-num-rec
      *
      *     evaluate true
      *     when no-cliente
      *          move "Cliente inesistente"                    to r-err
      *     when cliente-lock
      *          move "Cliente in uso su un altro termanile"   to r-err
      *     end-evaluate
      *
      *     write rlst-rec from riga-errore.
      *
      ****---
      * VALIDA-CLIENTE.
      **    controllo l'esistenza dell'articolo
      *     move rl-codice    to cli-codice
      *     set cli-tipo-C    to true
      *
      *     read progmag no lock
      *        invalid
      *           set errori        to true
      *           add 1             to rec-ko
      *           set no-cliente    to true
      *           perform SCRIVI-ERRORI
      *     end-read.
      *
      **    controllo che il cliente non sia lockato
      *     if tutto-ok
      *        set reclocked       to false
      *        set cli-tipo-C to true
      *        read progmag with lock
      *           invalid
      *              continue
      *        end-read
      *
      *        if reclocked
      *           set errori           to true
      *           add 1                to rec-ko
      *           set cliente-lock     to true
      *           perform SCRIVI-ERRORI
      *        end-if
      *     end-if.
      *
      ****---
           copy "common-excel.cpy".
      *     copy "imp-files-procedure.cpy".

