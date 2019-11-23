       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      qta-vend.
       AUTHOR.                          Andrea.
       REMARKS. Motore per la lavorazione sul file QTA-VEND.
                Batch notturno della durata di 4 minuti max

      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tmovmag.sl".
           copy "rmovmag.sl".
           copy "qta-vend.sl".
           copy "tcaumag.sl".
           copy "tparamge.sl".
           copy "lineseq.sl".

      *****************************************************************

       DATA DIVISION.
       FILE SECTION.
           copy "tmovmag.fd".
           copy "rmovmag.fd".
           copy "qta-vend.fd".
           copy "tcaumag.fd".
           copy "tparamge.fd".
           copy "lineseq.fd".

      *****************************************************************

       WORKING-STORAGE SECTION.
      * COPY
       copy "link-geslock.def".
       copy "recupero-addizionale.def".

      * COSTANTI
       78  titolo value "Aggiornamento file ordini a fornitori".

      * FILE-STATUS
       77  status-tmovmag           pic xx.
       77  status-rmovmag           pic xx.
       77  status-qta-vend          pic xx.
       77  status-tcaumag           pic xx.
       77  status-tparamge          pic xx.
       77  status-lineseq           pic xx.
       77  wstampa                  pic x(256).      

       01  r-inizio.
         05 filler                 pic x(2)  value " [".
         05 r-data.
            10 r-gg                pic xx.
            10 filler              pic x     value "/".
            10 r-mm                pic xx.
            10 filler              pic x     value "/".
            10 r-aa                pic xx.
         05 filler                 pic x(5)  value "] - [".
         05 r-ora.
            10 r-hh                pic xx.
            10 filler              pic x     value X"22".
            10 r-min               pic xx.
            10 filler              pic x     value "'".
            10 r-sec               pic xx.
         05 filler                 pic x(2)  value "] ".

       77  como-riga             pic x(100).
       77  riga-stampa           pic x(100).

      * VARIABILI
       01  data-start.
           05 anno-start            pic 9(4).
           05 mese-start            pic 9(2).
           05 giorno-start          pic 9(2).
       77  como-qta                 pic 9(12).
       77  como-data                pic 9(8).
       77  como-ora                 pic 9(8).
       77  mese                     pic 9(2).
       77  anno                     pic 9(4).
       77  anno-1                   pic 9(4).
       77  anno-2                   pic 9(4).
       77  anno-prec                pic 9(15).
       77  anno-corr                pic 9(15).
       77  media                    pic s9(15)v99.
       77  nargs                    pic 99 comp-1 value 0.   

       77  counter               pic 9(9).
       77  counter2              pic 9(9).
       77  counter-edit          pic zzz.zzz.zz9.

      * FLAGS
       01  controlli                pic xx.
         88 errori                  value "ER".
         88 tutto-ok                value "OK".

       01  YearType                 pic x.
         88 Year-2                  value "2".
         88 Year-1                  value "1".
         88 Year-0                  value "0".

       01  filler                pic 9 value 0.
           88 RichiamoSchedulato       value 1, false 0.   


      *****************************************************************

       LINKAGE SECTION.
       copy "link-batch.def".

      *****************************************************************

       PROCEDURE DIVISION USING batch-linkage.

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
           set  tutto-ok       to true.      

           CALL "C$NARG" USING NARGS.
           if nargs not = 0
              set RichiamoSchedulato to true
           else
              set RichiamoSchedulato to false
           end-if.
           if RichiamoSchedulato
              move 0 to batch-status
              initialize wstampa
              accept como-data from century-date
              accept como-ora  from time
              accept  wstampa from environment "SCHEDULER_PATH_LOG"
              inspect wstampa replacing trailing spaces by low-value
              string  wstampa      delimited low-value
                      "QTA-VEND_"  delimited size
                      como-data    delimited size
                      "_"          delimited size
                      como-ora     delimited size
                      ".log"       delimited size
                      into wstampa
              end-string
              set RichiamoSchedulato to true
              move wstampa to batch-log
              open output lineseq
           end-if.

      ***---
       OPEN-FILES.
           open output qta-vend.
           close       qta-vend.
           open i-o    qta-vend.
           if tutto-ok
              open input tmovmag
                         rmovmag
                         tcaumag
                         tparamge 
           end-if.
           if RichiamoSchedulato and errori
              move -1 to batch-status
           end-if.

      ***---
       ELABORAZIONE.
           move "INIZIO PROGRAMMA" to como-riga.
           perform SETTA-RIGA-STAMPA.

           move 0 to counter counter2.
           move spaces to tge-chiave.
           read tparamge no lock invalid continue end-read.
           
           move tge-data-consolid-progmag(1:4) to anno.
           subtract 2 from anno giving anno-2.
           subtract 1 from anno giving anno-1.

           move anno-2 to anno-start.
           move 1      to mese-start.
           move 1      to giorno-start.

           move low-value  to tmo-rec.
           move data-start to tmo-data-movim.
           start tmovmag key is >= k-data
                 invalid set errori to true
           end-start.

           if tutto-ok           

              move "INIZIO ELABORAZIONE TMOVMAG" to como-riga
              perform SETTA-RIGA-STAMPA

              perform until 1 = 2

                 read tmovmag next at end exit perform end-read

                 if tmo-data-movim > tge-data-consolid-progmag
                    exit perform 
                 end-if

                 evaluate true
                 when tmo-data-movim(1:4) = anno-2
                      set Year-2 to true
                 when tmo-data-movim(1:4) = anno-1
                      set Year-1 to true
                 when tmo-data-movim(1:4) = anno
                      set Year-0 to true
                 end-evaluate

                 move tmo-data-movim(5:2) to mese

                 if tmo-cliente
                    move tmo-causale to tca-codice
                    read tcaumag no lock invalid continue end-read
                    if tca-cliente 
LUBEXX                 if tca-si-stat
                          if tca-movim-ven-pos      or
                             tca-movim-ven-neg      or
                             tca-movim-resi-for-pos or
                             tca-movim-resi-for-neg
LUBEXX                       if tca-cod-magaz = "LBX"
                                perform LOOP-RIGHE-ORDINE
LUBEXX                       end-if
                          end-if
LUBEXX                 end-if
                    end-if
                 end-if

              end-perform                       
              move "FINE ELABORAZIONE TMOVMAG" to como-riga
              perform SETTA-RIGA-STAMPA
                                  
              move "INIZIO ELABORAZIONE QTA-VEND" to como-riga
              perform SETTA-RIGA-STAMPA

              move 0 to counter counter2
              |Alla fine calcolo già la media
              move low-value to qta-rec
              start qta-vend key >= qta-chiave
                    invalid continue
                not invalid
                    perform until 1 = 2
                       read qta-vend next at end exit perform end-read
                       if RichiamoSchedulato
                          perform CONTATORE-VIDEO
                       end-if
                       move tge-data-consolid-progmag(5:2) to mese
                       evaluate mese
                       when  1
                            compute anno-prec =
                                    qta-anno-2(2)  + 
                                    qta-anno-2(3) + 
                                    qta-anno-2(4) + 
                                    qta-anno-2(5) + 
                                    qta-anno-2(6) + 
                                    qta-anno-2(7) + 
                                    qta-anno-2(8) + 
                                    qta-anno-2(9) + 
                                    qta-anno-2(10) + 
                                    qta-anno-2(11) + 
                                    qta-anno-2(12) + 
                                    qta-anno-1(1)
                            compute anno-corr =
                                    qta-anno-1(2)  + 
                                    qta-anno-1(3) + 
                                    qta-anno-1(4) + 
                                    qta-anno-1(5) + 
                                    qta-anno-1(6) + 
                                    qta-anno-1(7) + 
                                    qta-anno-1(8) + 
                                    qta-anno-1(9) + 
                                    qta-anno-1(10) + 
                                    qta-anno-1(11) + 
                                    qta-anno-1(12) + 
                                    qta-anno(1)
                       when  2
                            compute anno-prec =
                                    qta-anno-2(3)  + 
                                    qta-anno-2(4) + 
                                    qta-anno-2(5) + 
                                    qta-anno-2(6) + 
                                    qta-anno-2(7) + 
                                    qta-anno-2(8) + 
                                    qta-anno-2(9) + 
                                    qta-anno-2(10) + 
                                    qta-anno-2(11) + 
                                    qta-anno-2(12) + 
                                    qta-anno-1(1) + 
                                    qta-anno-1(2)
                            compute anno-corr =
                                    qta-anno-1(3)  + 
                                    qta-anno-1(4) + 
                                    qta-anno-1(5) + 
                                    qta-anno-1(6) + 
                                    qta-anno-1(7) + 
                                    qta-anno-1(8) + 
                                    qta-anno-1(9) + 
                                    qta-anno-1(10) + 
                                    qta-anno-1(11) + 
                                    qta-anno-1(12) + 
                                    qta-anno(1) + 
                                    qta-anno(2)
                       when  3
                            compute anno-prec =
                                    qta-anno-2(4)  + 
                                    qta-anno-2(5) + 
                                    qta-anno-2(6) + 
                                    qta-anno-2(7) + 
                                    qta-anno-2(8) + 
                                    qta-anno-2(9) + 
                                    qta-anno-2(10) + 
                                    qta-anno-2(11) + 
                                    qta-anno-2(12) + 
                                    qta-anno-1(1) + 
                                    qta-anno-1(2) + 
                                    qta-anno-1(3)
                            compute anno-corr =
                                    qta-anno-1(4)  + 
                                    qta-anno-1(5) + 
                                    qta-anno-1(6) + 
                                    qta-anno-1(7) + 
                                    qta-anno-1(8) + 
                                    qta-anno-1(9) + 
                                    qta-anno-1(10) + 
                                    qta-anno-1(11) + 
                                    qta-anno-1(12) + 
                                    qta-anno(1) + 
                                    qta-anno(2) + 
                                    qta-anno(3)
                       when  4
                            compute anno-prec =
                                    qta-anno-2(5)  + 
                                    qta-anno-2(6) + 
                                    qta-anno-2(7) + 
                                    qta-anno-2(8) + 
                                    qta-anno-2(9) + 
                                    qta-anno-2(10) + 
                                    qta-anno-2(11) + 
                                    qta-anno-2(12) + 
                                    qta-anno-1(1) + 
                                    qta-anno-1(2) + 
                                    qta-anno-1(3) + 
                                    qta-anno-1(4)
                            compute anno-corr =
                                    qta-anno-1(5)  + 
                                    qta-anno-1(6) + 
                                    qta-anno-1(7) + 
                                    qta-anno-1(8) + 
                                    qta-anno-1(9) + 
                                    qta-anno-1(10) + 
                                    qta-anno-1(11) + 
                                    qta-anno-1(12) + 
                                    qta-anno(1) + 
                                    qta-anno(2) + 
                                    qta-anno(3) + 
                                    qta-anno(4)
                       when  5
                            compute anno-prec =
                                    qta-anno-2(6)  + 
                                    qta-anno-2(7) + 
                                    qta-anno-2(8) + 
                                    qta-anno-2(9) + 
                                    qta-anno-2(10) + 
                                    qta-anno-2(11) + 
                                    qta-anno-2(12) + 
                                    qta-anno-1(1) + 
                                    qta-anno-1(2) + 
                                    qta-anno-1(3) + 
                                    qta-anno-1(4) + 
                                    qta-anno-1(5)
                            compute anno-corr =
                                    qta-anno-1(6)  + 
                                    qta-anno-1(7) + 
                                    qta-anno-1(8) + 
                                    qta-anno-1(9) + 
                                    qta-anno-1(10) + 
                                    qta-anno-1(11) + 
                                    qta-anno-1(12) + 
                                    qta-anno(1) + 
                                    qta-anno(2) + 
                                    qta-anno(3) + 
                                    qta-anno(4) + 
                                    qta-anno(5)
                       when  6
                            compute anno-prec =
                                    qta-anno-2(7)  + 
                                    qta-anno-2(8) + 
                                    qta-anno-2(9) + 
                                    qta-anno-2(10) + 
                                    qta-anno-2(11) + 
                                    qta-anno-2(12) + 
                                    qta-anno-1(1) + 
                                    qta-anno-1(2) + 
                                    qta-anno-1(3) + 
                                    qta-anno-1(4) + 
                                    qta-anno-1(5) + 
                                    qta-anno-1(6)
                            compute anno-corr =
                                    qta-anno-1(7)  + 
                                    qta-anno-1(8) + 
                                    qta-anno-1(9) + 
                                    qta-anno-1(10) + 
                                    qta-anno-1(11) + 
                                    qta-anno-1(12) + 
                                    qta-anno(1) + 
                                    qta-anno(2) + 
                                    qta-anno(3) + 
                                    qta-anno(4) + 
                                    qta-anno(5) + 
                                    qta-anno(6)
                       when  7
                            compute anno-prec =
                                    qta-anno-2(8)  + 
                                    qta-anno-2(9) + 
                                    qta-anno-2(10) + 
                                    qta-anno-2(11) + 
                                    qta-anno-2(12) + 
                                    qta-anno-1(1) + 
                                    qta-anno-1(2) + 
                                    qta-anno-1(3) + 
                                    qta-anno-1(4) + 
                                    qta-anno-1(5) + 
                                    qta-anno-1(6) + 
                                    qta-anno-1(7)
                            compute anno-corr =
                                    qta-anno-1(8)  + 
                                    qta-anno-1(9) + 
                                    qta-anno-1(10) + 
                                    qta-anno-1(11) + 
                                    qta-anno-1(12) + 
                                    qta-anno(1) + 
                                    qta-anno(2) + 
                                    qta-anno(3) + 
                                    qta-anno(4) + 
                                    qta-anno(5) + 
                                    qta-anno(6) + 
                                    qta-anno(7)
                       when  8
                            compute anno-prec =
                                    qta-anno-2(9)  + 
                                    qta-anno-2(10) + 
                                    qta-anno-2(11) + 
                                    qta-anno-2(12) + 
                                    qta-anno-1(1) + 
                                    qta-anno-1(2) + 
                                    qta-anno-1(3) + 
                                    qta-anno-1(4) + 
                                    qta-anno-1(5) + 
                                    qta-anno-1(6) + 
                                    qta-anno-1(7) + 
                                    qta-anno-1(8)
                            compute anno-corr =
                                    qta-anno-1(9)  + 
                                    qta-anno-1(10) + 
                                    qta-anno-1(11) + 
                                    qta-anno-1(12) + 
                                    qta-anno(1) + 
                                    qta-anno(2) + 
                                    qta-anno(3) + 
                                    qta-anno(4) + 
                                    qta-anno(5) + 
                                    qta-anno(6) + 
                                    qta-anno(7) + 
                                    qta-anno(8)
                       when  9
                            compute anno-prec =
                                    qta-anno-2(10) + 
                                    qta-anno-2(11) + 
                                    qta-anno-2(12) + 
                                    qta-anno-1(1) + 
                                    qta-anno-1(2) + 
                                    qta-anno-1(3) + 
                                    qta-anno-1(4) + 
                                    qta-anno-1(5) + 
                                    qta-anno-1(6) + 
                                    qta-anno-1(7) + 
                                    qta-anno-1(8) + 
                                    qta-anno-1(9)
                            compute anno-corr =
                                    qta-anno-1(10) + 
                                    qta-anno-1(11) + 
                                    qta-anno-1(12) + 
                                    qta-anno(1) + 
                                    qta-anno(2) + 
                                    qta-anno(3) + 
                                    qta-anno(4) + 
                                    qta-anno(5) + 
                                    qta-anno(6) + 
                                    qta-anno(7) + 
                                    qta-anno(8) + 
                                    qta-anno(9)
                       when 10
                            compute anno-prec =
                                    qta-anno-2(11) + 
                                    qta-anno-2(12) + 
                                    qta-anno-1(1) + 
                                    qta-anno-1(2) + 
                                    qta-anno-1(3) + 
                                    qta-anno-1(4) + 
                                    qta-anno-1(5) + 
                                    qta-anno-1(6) + 
                                    qta-anno-1(7) + 
                                    qta-anno-1(8) + 
                                    qta-anno-1(9) + 
                                    qta-anno-1(10)
                            compute anno-corr =
                                    qta-anno-1(11) + 
                                    qta-anno-1(12) + 
                                    qta-anno(1) + 
                                    qta-anno(2) + 
                                    qta-anno(3) + 
                                    qta-anno(4) + 
                                    qta-anno(5) + 
                                    qta-anno(6) + 
                                    qta-anno(7) + 
                                    qta-anno(8) + 
                                    qta-anno(9) + 
                                    qta-anno(10)
                       when 11
                            compute anno-prec =
                                    qta-anno-2(12) + 
                                    qta-anno-1(1) + 
                                    qta-anno-1(2) + 
                                    qta-anno-1(3) + 
                                    qta-anno-1(4) + 
                                    qta-anno-1(5) + 
                                    qta-anno-1(6) + 
                                    qta-anno-1(7) + 
                                    qta-anno-1(8) + 
                                    qta-anno-1(9) + 
                                    qta-anno-1(10) + 
                                    qta-anno-1(11)
                            compute anno-corr =
                                    qta-anno-1(12) + 
                                    qta-anno(1) + 
                                    qta-anno(2) + 
                                    qta-anno(3) + 
                                    qta-anno(4) + 
                                    qta-anno(5) + 
                                    qta-anno(6) + 
                                    qta-anno(7) + 
                                    qta-anno(8) + 
                                    qta-anno(9) + 
                                    qta-anno(10) + 
                                    qta-anno(11)
                       when 12
                            compute anno-prec =
                                    qta-anno-1(1) + 
                                    qta-anno-1(2) + 
                                    qta-anno-1(3) + 
                                    qta-anno-1(4) + 
                                    qta-anno-1(5) + 
                                    qta-anno-1(6) + 
                                    qta-anno-1(7) + 
                                    qta-anno-1(8) + 
                                    qta-anno-1(9) + 
                                    qta-anno-1(10) + 
                                    qta-anno-1(11) + 
                                    qta-anno-1(12)
                            compute anno-corr =
                                    qta-anno(1) + 
                                    qta-anno(2) + 
                                    qta-anno(3) + 
                                    qta-anno(4) + 
                                    qta-anno(5) + 
                                    qta-anno(6) + 
                                    qta-anno(7) + 
                                    qta-anno(8) + 
                                    qta-anno(9) + 
                                    qta-anno(10) + 
                                    qta-anno(11) + 
                                    qta-anno(12)
                       end-evaluate
                       compute media =
                            (( anno-prec / anno-corr ) - 1 ) * 100
                       compute media = media * -1
                       move media to qta-media
                       rewrite qta-rec
                    end-perform   
              end-start              
              move "FINE ELABORAZIONE QTA-VEND" to como-riga
              perform SETTA-RIGA-STAMPA
           end-if.

      ***---
       LOOP-RIGHE-ORDINE.
           set  tutto-ok   to true.
           move low-value  to rmo-rec.
           move tmo-chiave to rmo-chiave.
           start rmovmag key is >= rmo-chiave
                 invalid set errori to true
           end-start.
           if tutto-ok
              perform until 1 = 2
                 read rmovmag next at end exit perform end-read
                 if rmo-anno  not = tmo-anno or
                    rmo-movim not = tmo-numero
                    exit perform
                 end-if
                                         
                 if RichiamoSchedulato
                    perform CONTATORE-VIDEO
                 end-if

                 |Serve per non considerare le NCPZ 
                 |e le variazioni di valore
                 if rmo-qta not = 0
                    move rmo-qta      to como-qta

                    move rmo-codmag   to qta-mag
                    move rmo-articolo to qta-articolo
                    read qta-vend
                         invalid
                         initialize qta-dati
                                    replacing numeric data by zeroes
                                         alphanumeric data by spaces
                    end-read
                    evaluate true
                    when Year-2 add como-qta to qta-anno-2(mese)
                    when Year-1 add como-qta to qta-anno-1(mese)
                    when Year-0 add como-qta to qta-anno(mese)
                    end-evaluate
                    write qta-rec invalid rewrite qta-rec end-write
                 end-if


              end-perform
           end-if.

      ***---
       SETTA-RIGA-STAMPA.
           initialize riga-stampa.
           perform SETTA-INIZIO-RIGA.
           string r-inizio  delimited size
                  como-riga delimited size
                  into riga-stampa
           end-string.   
           if RichiamoSchedulato
              write line-riga of lineseq from riga-stampa
           else
              display riga-stampa upon syserr
           end-if.

      ***---
       SETTA-INIZIO-RIGA.
           accept como-ora  from time.
           accept como-data from century-date.

           move como-data(3:2) to r-aa.
           move como-data(5:2) to r-mm.
           move como-data(7:2) to r-gg.

           move como-ora(1:2) to r-hh.
           move como-ora(3:2) to r-min.
           move como-ora(5:2) to r-sec.

      ***---
       CLOSE-FILES.
           close tmovmag
                 rmovmag
                 tcaumag
                 tparamge
                 qta-vend.

      ***---
       CONTATORE-VIDEO.
           add 1 to counter counter2

           if counter2 = 1000
              move counter to counter-edit
              display counter-edit 
                      upon batch-win-handle
                      line 25,00
                    column 38,00
              move 0 to counter2
           end-if.

      ***---
       EXIT-PGM.
           if RichiamoSchedulato                 
              move "FINE PROGRAMMA" to como-riga
              perform SETTA-RIGA-STAMPA
              close lineseq          
              display "                                             "
                 upon batch-win-handle
                   line 25,00
                 column 35,00
           end-if.
           goback.
