      ***---
       RIEMPI-TMP.
           move 0 to counter counter2.
           move spaces to tge-chiave.
           read tparamge no lock invalid continue end-read.
           perform SECCA-TMP.
           set tutto-ok    to true.
           set trovato     to false.
           set prima-volta to true.
           initialize path-tmp-statsett.
           accept  path-tmp-statsett from environment "PATH-ST".
           inspect path-tmp-statsett
                   replacing trailing spaces by low-value.
           accept como-data from century-date.
           accept como-ora  from time.
           string path-tmp-statsett  delimited low-value
                  "tmp-statsett"     delimited size
                  "_"                delimited size
                  como-data          delimited size
                  "_"                delimited size
                  como-ora           delimited size
                  ".tmp"             delimited size
                  into path-tmp-statsett
           end-string.
           open output tmp-statsett.
           if status-tmp-statsett = "00"
              close       tmp-statsett
              open i-o    tmp-statsett
              set FileOpen to true
              perform CICLO-LETTURA-STATSETT
              perform RIEMPI-GRIGLIA
              perform CALCOLA-TOT-FATTURATO
           end-if.

      ***---
       CICLO-LETTURA-STATSETT.
           move 0 to SaveMese.
           move low-value to sts-chiave.
      *****     move tge-anno  to sts-anno.
           start statsett key is >= k-ord
                 invalid set errori to true
           end-start.

           if tutto-ok
              perform until 1 = 2
                 set record-ok to false
                 read statsett next
                      at end
                      if trovato
                         perform VALORIZZA-TOTALI
                      end-if
                      exit perform
                 end-read

                 add 1 to counter
                 add 1 to counter2
                 if counter2 = 100
                    move counter to counter-edit
                    display counter-edit
                       upon form2-handle at column 22
                                              line 03
                    move 0 to counter2
                 end-if

                 move sts-tipocli to tcl-codice
                 read ttipocli
                      invalid move spaces to tcl-descrizione
                 end-read
                 perform MOVE-DATI
              end-perform
           end-if.

      ***---
       MOVE-DATI.
           initialize tmp-sts-rec replacing numeric by zeroes
                                       alphanumeric by spaces.

           if SaveMese not = sts-mese
              if prima-volta
                 set prima-volta to false
              else
                 perform VALORIZZA-TOTALI
              end-if
              move sts-mese to SaveMese tmp-sts-mese
              write tmp-sts-rec invalid continue end-write
           end-if.

           move sts-mese        to tmp-sts-mese.
           move tcl-descrizione to tmp-sts-tipocli.
           read tmp-statsett
                invalid
                move 0 to tmp-sts-fat-corr
                move 0 to tmp-sts-fat-past
           end-read.
           add sts-fat-corr     to tmp-sts-fat-corr.
           add sts-fat-past     to tmp-sts-fat-past.
           compute tmp-sts-diff     =
                   tmp-sts-fat-corr - tmp-sts-fat-past.
           if tmp-sts-fat-past not  = 0
              |ALTRIMENTI LA DIVISIONE VA IN ERRORE. E' OVVIO CHE
              |SE NON HO FATTURATO L'ANNO PRECEDENTE AVRO' ANCHE
              |UNA DIFFERENZA TRA I DUE ANNO DELLA TOTALITA' (100)
              compute tmp-sts-perce-diff =
                      ( 100 * tmp-sts-diff ) / tmp-sts-fat-past
           else
              move 100 to tmp-sts-perce-diff
           end-if.
           write tmp-sts-rec invalid rewrite tmp-sts-rec end-write.
           set trovato to true.
           add sts-fat-corr        to TotFatCorr.
           add sts-fat-past        to TotFatPast.

      ***---
       VALORIZZA-TOTALI.
           move SaveMese to tmp-sts-mese.
           move spaces   to tmp-sts-tipocli.
           read tmp-statsett
                invalid continue
            not invalid
                move TotFatCorr   to tmp-sts-fat-corr
                move TotFatPast   to tmp-sts-fat-past
                compute tmp-sts-diff = TotFatCorr - TotFatPast
                if TotFatPast not = 0
                   compute tmp-sts-perce-diff =
                   ( 100 * tmp-sts-diff ) / TotFatPast
                else
                   move 100 to tmp-sts-perce-diff
                end-if
                rewrite tmp-sts-rec invalid continue end-rewrite
           end-read.
           initialize tmp-sts-rec replacing numeric by zeroes
                                       alphanumeric by spaces.

           move 0 to TotFatCorr TotFatPast.

      ***---
       RIEMPI-GRIGLIA.
           close tmp-statsett.
           open input tmp-statsett.
           set tutto-ok to true.
           modify  gd1, mass-update = 1.
           modify  gd1, reset-grid  = 1.
           perform INTESTAZIONE-GRID.
           move low-value to tmp-sts-rec.
           start tmp-statsett key is >= tmp-sts-chiave
                 invalid set errori to true
           end-start.
           if tutto-ok
              move 1 to riga
              perform until 1 = 2
                 read tmp-statsett next at end exit perform end-read
                 if   tmp-sts-tipocli =  spaces
                      perform RIEMPI-COLUMNS
      
                      set NoHasSons      to true
                      modify gd1(riga, 1),
                             hidden-data = hid-HasSons
      
                      set hid-close to true
                      modify gd1(riga, 2),
                             hidden-data = hid-OpenClose
      
                      set NoIsSon to true
                      modify gd1(riga, 3),
                             hidden-data = hid-IsSon
      
                      modify gd1, (riga, 1),
                             bitmap        = multi-bmp,
                             bitmap-number = 1,
                             bitmap-width  = 16
                 else
                    if SaveMese = tmp-sts-mese
                       set YesHasSons to true                           
                       modify gd1(riga, 1), 
                              hidden-data = hid-HasSons
                       move 0 to SaveArticolo
                       modify gd1, (riga, 1),
                              bitmap        = multi-bmp,
                              bitmap-number = 3,
                              bitmap-width  = 16
                    end-if
                 end-if
              end-perform
           end-if.
           modify  gd1, mass-update = 0.

      ***---
       VEDI-MASTER.
           inquire gd1, last-row in tot-righe.
           perform varying riga from riga by -1 
                     until riga < 2
              inquire gd1(riga, 3), hidden-data in hid-IsSon
              if NoIsSon exit perform end-if
           end-perform.
           move riga to como-riga.
           add 1 to riga.

           perform varying riga from riga by 1 
                     until riga > tot-righe 
              inquire gd1(riga, 3), hidden-data in hid-IsSon
              if NoIsSon exit perform end-if

              modify gd1, record-to-delete = riga
              subtract 1 from riga

           end-perform.

      ***---
       VEDI-SLAVES.                      
           move low-value to tmp-sts-chiave.
           move hid-mese  to tmp-sts-mese.
                                         
           start tmp-statsett key is >= tmp-sts-chiave
                 invalid set errori to true
           end-start.

           if tutto-ok
              perform until 1 = 2
                 read tmp-statsett next at end exit perform end-read
                 if   tmp-sts-mese not = hid-mese
                      exit perform
                 end-if
                 if tmp-sts-tipocli not = spaces
                    perform RIEMPI-COLUMNS

                    set NoHasSons      to true
                    modify gd1(riga, 1),
                           hidden-data = hid-HasSons

                    set hid-open to true
                    modify gd1(riga, 2),
                           hidden-data = hid-OpenClose

                    set YesIsSon to true
                    modify gd1(riga, 3),
                           hidden-data = hid-IsSon   

                    modify gd1, (riga, 2),
                           bitmap        = multi-bmp,
                           bitmap-number = 4,
                           bitmap-width  = 16
                 end-if
              end-perform
           end-if.

      ***---
       RIEMPI-COLUMNS.
           add 1 to riga.
           initialize rec-grid
                      replacing numeric data by zeroes
                           alphanumeric data by spaces.
           if tmp-sts-tipocli = spaces
              evaluate tmp-sts-mese
              when 01  move "GENNAIO"   to col-tipocli
              when 02  move "FEBBRAIO"  to col-tipocli
              when 03  move "MARZO"     to col-tipocli
              when 04  move "APRILE"    to col-tipocli
              when 05  move "MAGGIO"    to col-tipocli
              when 06  move "GIUGNO"    to col-tipocli
              when 07  move "LUGLIO"    to col-tipocli
              when 08  move "AGOSTO"    to col-tipocli
              when 09  move "SETTEMBRE" to col-tipocli
              when 10  move "OTTOBRE"   to col-tipocli
              when 11  move "NOVEMBRE"  to col-tipocli
              when 12  move "DICEMBRE"  to col-tipocli
              end-evaluate
           else
              move tmp-sts-tipocli    to col-tipocli
           end-if.
           move tmp-sts-mese       to SaveMese hid-mese.
           move tmp-sts-fat-corr   to col-fat-corr.
           move tmp-sts-fat-past   to col-fat-past.
           move tmp-sts-diff       to col-diff.
           move tmp-sts-perce-diff to col-perce-diff.

           modify gd1(riga),
                  insertion-index = riga
                  record-to-add   = rec-grid.

           modify gd1(riga, 4),
                  hidden-data = hid-mese.

      ***---
       CALCOLA-TOT-FATTURATO.
           move 0 to tot-fatt-2004 tot-fatt-2005.
           inquire gd1, last-row in tot-righe.
           perform varying riga from 2 by 1
                     until riga > tot-righe
              inquire gd1(riga, 4), cell-data in como-tot-fatt-2005
              inquire gd1(riga, 5), cell-data in como-tot-fatt-2004
              compute tot-fatt-2005 = tot-fatt-2005 + como-tot-fatt-2005
              compute tot-fatt-2004 = tot-fatt-2004 + como-tot-fatt-2004
           end-perform.
           compute tot-diff = tot-fatt-2005 - tot-fatt-2004.
           if tot-fatt-2004 not = 0
              |ALTRIMENTI LA DIVISIONE VA IN ERRORE. E' OVVIO CHE
              |SE NON HO FATTURATO L'ANNO PRECEDENTE AVRO' ANCHE
              |UNA DIFFERENZA TRA I DUE ANNO DELLA TOTALITA' (100)
              compute tot-perce =
                      ( 100 * tot-diff ) / tot-fatt-2004
           else
              move 100 to tot-perce
           end-if.
           move tot-fatt-2005   to tot-fatt-2005-ed.
           move tot-fatt-2004   to tot-fatt-2004-ed.
           move tot-diff        to tot-diff-ed.
           move tot-perce       to tot-perce-ed.
           modify gd-tot(1, 2), cell-data = tot-fatt-2005-ed.
           modify gd-tot(1, 3), cell-data = tot-fatt-2004-ed.
           modify gd-tot(1, 4), cell-data = tot-diff-ed.
           modify gd-tot(1, 5), cell-data = tot-perce-ed.
           modify gd-tot(1),     row-font = font-evidenzia-griglia.
           move 0 to riga.
           move 2 to event-data-2.
           perform SPOSTAMENTO.
