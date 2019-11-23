      ***---
       BEFORE-PROGRAM.
           move NotaChiave       to WK-LinkChiave.

           accept esercizio-g2 from environment "ESERCIZIO_G2".
           accept esercizio-x  from environment "ESERCIZIO".
           move   esercizio-x  to esercizio.

           set inserimento to true.
           move "GESLUX - Creazione Contestazioni" to titolo.

      ***---
       LEGGI-TESTATA.
           move tor-cod-cli  to cli-codice.
           set cli-tipo-C    to true.
           read clienti  invalid continue end-read.

           if tor-prg-destino not = 0
              move tor-cod-cli     to des-codice
              move tor-prg-destino to des-prog
              read destini invalid continue end-read
           end-if.

           move tor-vettore to vet-codice.
           read tvettori invalid continue end-read.
           
      ***---
       VALORIZZA-SCREEN.
           move tor-data-fattura to como-data.
           perform DATE-TO-SCREEN.

           if WK-LinkNumero not = 0 

              if LinkFatt move "Fattura n." to lab-docum-buf
              else        move "Nota cr n." to lab-docum-buf
              end-if

              move WK-LinkNumero to lab-num-buf
              move como-data to lab-data-buf

           else
              move data-oggi       to como-data
              perform DATE-TO-SCREEN
              move como-data       to lab-data-buf
           end-if.

           move esercizio       to lab-esercizio-buf.

           move tor-cod-cli     to codice-ed.
           move codice-ed       to ef-cli-buf.
           call "C$JUSTIFY"  using ef-cli-buf, "L".

           move tor-prg-destino to codice-ed.
           move codice-ed       to ef-des-buf.
           call "C$JUSTIFY"  using ef-des-buf, "L".

           move cli-ragsoc-1    to lab-cli-buf.
           move cli-indirizzo   to lab-ind-buf.
           move cli-localita    to lab-loca-buf.

           move des-ragsoc-1    to lab-des-buf.
           move des-indirizzo   to lab-ind-d-buf.
           move des-localita    to lab-loca-d-buf.

      ***---
       CURRENT-RECORD.
           move 0 to tno-cod-cli      tor-cod-cli
                     tno-prg-destino  tor-prg-destino
                     tno-data-fattura tor-data-fattura
                     tor-vettore.
           if LinkFatt
              read tordini
                   invalid |Valorizzo per lo meno la data di sistema...
                           accept como-data from century-date
                           perform DATE-TO-SCREEN
                           move como-data   to lab-data-buf
                           |... l'anno di esercizio, ...
                           move esercizio   to lab-esercizio-buf
                           |... e muovo a blank codice e destino.
                           move spaces to ef-cli-buf ef-des-buf
               not invalid perform LEGGI-TESTATA
                           perform VALORIZZA-SCREEN
              end-read
           else
              read tnotacr
                   invalid |Valorizzo per lo meno la data di sistema...
                           accept como-data from century-date
                           perform DATE-TO-SCREEN
                           move como-data   to lab-data-buf
                           |... l'anno di esercizio, ...
                           move esercizio   to lab-esercizio-buf
                           |... e muovo a blank codice e destino.
                           move spaces to ef-cli-buf ef-des-buf
               not invalid move tno-cod-cli      to tor-cod-cli
                           move tno-prg-destino  to tor-prg-destino
                           move tno-data-fattura to tor-data-fattura
                           perform LEGGI-TESTATA
                           perform VALORIZZA-SCREEN
              end-read
           end-if.
           perform FORM1-BUF-TO-FLD.
           perform VALORIZZA-OLD. 

      ***---
       VALORIZZA-OLD.
           move cnt-rec to old-cnt-rec.
           set old-cnt-aperta to true.

      ***---
       ABILITAZIONI.
           move BitmapNewDisabled     to BitmapNumNew.
           move BitmapDeleteDisabled  to BitmapNumDelete.
           move BitmapEditDisabled    to BitmapNumEdit.
           move BitmapPrintDisabled   to BitmapNumPrint.
           move BitmapPreviewDisabled to BitmapNumPreview.
           move BitmapSelectDisabled  to BitmapNumSelect.

           if livello-abil > 1 move BitmapSaveEnabled  to BitmapNumSave
                               move 1                  to e-salva
                               move 1                  to e-modifica mod
           else                move BitmapSaveDisabled to BitmapNumSave
                               move 0                  to e-salva
                               move 0                  to e-modifica mod
           end-if.
           modify tool-modifica,   value mod.
           move 0 to mod-rich mod-nota.

           move 0 to e-cancella 
                     e-anteprima 
                     e-stampa 
                     e-nuovo
                     e-seleziona
                     e-modifica.

           if e-cerca = 0 move BitmapZoomDisabled to BitmapNumZoom
           else           move BitmapZoomEnabled  to BitmapNumZoom
           end-if.

           perform DISPLAY-SCREEN.
                                                      
           modify tool-nuovo,     enabled = e-nuovo,
                                  bitmap-number = BitmapNumNew.
           modify tool-cancella,  enabled = e-cancella,
                                  bitmap-number = BitmapNumDelete.
           modify tool-modifica,  enabled = e-modifica,
                                  bitmap-number = BitmapNumEdit.
           modify tool-anteprima, enabled = e-anteprima,
                                  bitmap-number = BitmapNumPreview.
           modify tool-stampa,    enabled = e-stampa,
                                  bitmap-number = BitmapNumPrint.
           modify tool-seleziona, enabled = e-seleziona,
                                  bitmap-number = BitmapNumSelect.
           modify tool-salva,     enabled = e-salva,
                                  bitmap-number = BitmapNumSave.  
           modify tool-cerca,     enabled = e-cerca,
                                  bitmap-number = BitmapNumZoom .

      * FITTIZZI --> NON TOCCARE!!!
      ***---
       TORNA-IN-VISUA.
