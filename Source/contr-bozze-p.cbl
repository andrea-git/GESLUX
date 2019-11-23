       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      contr-bozze-p.
       AUTHOR.                          Luciano.
       REMARKS.
           Controllo Bozza per corrispondenza su ordini fornitori. 

      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "reva.sl".
           copy "teva.sl".
           copy "rordforn.sl".

       SELECT rep-listini
           ASSIGN       TO  path-rep-listini
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE  IS SEQUENTIAL
           FILE STATUS  IS STATUS-rep-listini.


      *****************************************************************

       DATA DIVISION.
       FILE SECTION.
           copy "reva.fd".
           copy "teva.fd".
           copy "rordforn.fd".

       FD  rep-listini.
       01 rlst-rec         PIC  x(110).


      *****************************************************************

       WORKING-STORAGE SECTION.
      * COPY
           copy "acugui.def".
      * COSTANTI
       78  titolo value "Controllo Bozze d'evasione".

       01        pic 9.
           88 rec-locked value 1 false zero.

      * FILE-STATUS
       77  status-reva          pic xx.
       77  status-teva          pic xx.
       77  status-rordforn      pic xx.
       77  status-rep-listini   pic xx.
       77  path-rep-listini     pic x(256).


      * VARIABILI
       01  riga-errore.
           05 re-bozza-n           pic z(8).
           05 filler               pic x value "/".
           05 re-bozza-anno        pic 9(4).
           05 filler               pic x(2) value space.
           05 re-bozza-riga        pic z(5).
           05 filler               pic x(2) value space.
           05 re-bozza-articolo    pic z(8).
           05 filler               pic x(3) value space.
           05 re-mov-n             pic z(7)9.
           05 filler               pic x value "/".
           05 re-mov-anno          pic 9(4).
           05 filler               pic x(3) value space.
           05 re-ord-n             pic z(8).
           05 filler               pic x value "/".
           05 re-ord-anno          pic 9(4).
           05 filler               pic x(2) value space.
           05 re-ord-riga          pic z(5).
           05 filler               pic x(2) value space.
           05 re-ord-articolo      pic z(8).
           05 filler               pic x(3) value space.
           05 re-err               pic x(40).

       01  riga-int-errore.
           05 filler               pic x(8) value "Bozza N.".
           05 filler               pic x value space.
           05 filler               pic x(4) value space.
           05 filler               pic x(2) value space.
           05 filler               pic x(5) value " Riga".
           05 filler               pic x(2) value space.
           05 filler               pic x(8) value "Articolo".
           05 filler               pic x(3) value space.
           05 filler               pic x(8) value "Mov. N.".
           05 filler               pic x value space.
           05 filler               pic x(4) value space.
           05 filler               pic x(3) value space.
           05 filler               pic x(8) value "Ordine N".
           05 filler               pic x value space.
           05 filler               pic x(4) value space.
           05 filler               pic x(2) value space.
           05 filler               pic x(5) value " Riga".
           05 filler               pic x(2) value space.
           05 filler               pic x(8) value "Articolo".
           05 filler               pic x(3) value space.
           05 filler               pic x(20) value "Errore".

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

       01  tipo-errore          pic 9.
           88 no-ordine         value 1.  
           88 articolo-diverso  value 2.    


      *****************************************************************

       LINKAGE SECTION.
           copy "link-contr-bozze-p.def".

       PROCEDURE DIVISION USING  contr-bozze-p-linkage.

       DECLARATIVES.

      ***---
       REVA-ERR SECTION.
           use after error procedure on reva.
           set tutto-ok  to true.
           evaluate status-reva
           when "35"
                set errori to true
                display message "File [REVA] not found!"
                          title titolo
                           icon 3
           when "39"
                set errori to true
                display message "File [REVA] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[REVA] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "93"
           when "99"
                SET rec-locked  to true
           end-evaluate.

      ***---
       TEVA-ERR SECTION.
           use after error procedure on teva.
           set tutto-ok  to true.
           evaluate status-teva
           when "35"
                set errori to true
                display message "File [TEVA] not found!"
                          title titolo
                           icon 3
           when "39"
                set errori to true
                display message "File [TEVA] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TEVA] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "93"
           when "99"
                SET rec-locked  to true
           end-evaluate.

       RORDFORN-ERR SECTION.
           use after error procedure on rordforn.
           set tutto-ok  to true.
           evaluate status-rordforn
           when "35"
                set errori to true
                display message "File [RORDFORN] not found!"
                          title titolo
                           icon 3
           when "39"
                set errori to true
                display message "File [RORDFORN] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[RORDFORN] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "93"
           when "99"
                SET rec-locked  to true
           end-evaluate.

      ***---
       REP-LISTINI SECTION.
           use after error procedure on rep-listini.
           set tutto-ok  to true.
           evaluate status-rep-listini
           when "35"
                set errori to true
                display message "File [REP-LISTINI] not found!"
                          title titolo
                           icon 3
           when "39"
                set errori to true
                display message "File [REP-LISTINI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[REP-LISTINI] Indexed file corrupt!"
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
           set rec-locked to false
           set primo-errore  to true.
           accept como-data  from century-date.
           accept como-ora   from time.
           move 0 to counter counter2.
           set tutto-ok to true.

           move cbo-path  to path-rep-listini.

      ***---
       OPEN-FILES.
           open input reva.
           open input teva.
           open input rordforn.
           if errori 
              goback 
           end-if.

      ***---
       ELABORAZIONE.
           set tutto-ok   to true
           accept como-data from century-date
           accept como-ora  from time

           move 0 to num-rec.

           move low-value to reva-chiave
           start reva key not < reva-chiave
              invalid
                 continue
              not invalid
                 perform until 1 = 2 
                    read reva next
                       at end
                          exit perform
                    end-read
                    perform DISPLAY-UPON-SCREEN


                    perform TRATTA-RECORD
                 end-perform
           end-start.

           if rec-ko > 0
              write rlst-rec from spaces
              move all "-" to rlst-rec
              write rlst-rec 
              write rlst-rec from spaces
              move rec-ko to num-rec-ed
              string "Totale errori: "   delimited size
                     num-rec-ed          delimited size
                     into rlst-rec
              end-string
              write rlst-rec  after 2
              close REP-LISTINI
           end-if.

      ***---
       CLOSE-FILES.
           close reva
                 teva
                 rordforn.
              
      ***---
       EXIT-PGM.
           goback.

      ***---
       TRATTA-RECORD.
           move reva-chiave-ordf   to rof-chiave
           read rordforn
              invalid
                 set errori        to true
                 add 1             to rec-ko
                 set no-ordine  to true
                 perform SCRIVI-ERRORI
              not invalid
                 if reva-articolo not = rof-cod-articolo
                    set errori           to true
                    add 1                to rec-ko
                    set articolo-diverso to true
                    perform SCRIVI-ERRORI
                 end-if
           end-read.


      ***---
       SCRIVI-ERRORI.
           if primo-errore
              set primo-errore  to false
              open output rep-listini

              move "Controllo Bozze: riepilogo errori"  
                                               to rlst-rec
              call "c$justify" using rlst-rec, "C"
              write rlst-rec

              initialize rlst-rec
              write rlst-rec

              write rlst-rec from riga-int-errore 
              move all "-"   to rlst-rec
              write rlst-rec
           end-if

           move reva-anno          to re-bozza-anno     
           move reva-numero        to re-bozza-n
           move reva-riga          to re-bozza-riga   

           move reva-articolo      to re-bozza-articolo

           move reva-chiave-testa  to teva-chiave
           read teva no lock
              invalid
                 initialize teva-anno-movim
                            teva-num-movim
           end-read

           move teva-anno-movim    to re-mov-anno
           move teva-num-movim     to re-mov-n

           move reva-anno-ordf     to re-ord-anno
           move reva-numero-ordf   to re-ord-n   
           move reva-riga-ordf     to re-ord-riga
           move rof-cod-articolo   to re-ord-articolo

           evaluate true
           when no-ordine
                move "Ordine forn. Correlato inesistente"  to re-err
           when articolo-diverso
                move "Articoli non corrispondenti"   to re-err
           end-evaluate

           write rlst-rec from riga-errore.

      ***---
       DISPLAY-UPON-SCREEN.
           add 1 to counter
           add 1 to counter2
           if counter2 = 10
              move counter to counter-edit
              display counter-edit upon cbo-Handle at column 43 line 2
              move 0 to counter2
           end-if.
