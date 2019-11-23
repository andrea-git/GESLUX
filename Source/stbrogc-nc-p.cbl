       PROGRAM-ID.     stbrogc-fm-nc-p.
       AUTHOR.         Andrea.

       SPECIAL-NAMES. DECIMAL-POINT is COMMA.

       FILE-CONTROL.
           copy "tnotacr.sl".
           copy "rnotacr.sl".
           copy "clienti.sl".
           copy "articoli.sl".
           copy "destini.sl". 
           copy "tvettori.sl".
           copy "lineseq.sl".
           copy "agenti.sl".
           copy "tcodpag.sl".
           copy "tivaese.sl".
           copy "progmag.sl".
           copy "tcaumag.sl".

       FILE SECTION.
           copy "tnotacr.fd".
           copy "rnotacr.fd".
           copy "clienti.fd".
           copy "articoli.fd".
           copy "destini.fd". 
           copy "tvettori.fd".
           copy "lineseq.fd".
           copy "agenti.fd". 
           copy "tcodpag.fd".
           copy "tivaese.fd".
           copy "progmag.fd".
           copy "tcaumag.fd".

       WORKING-STORAGE SECTION.
           copy "comune.def".

       78  78-CrtDesArt       value 40.
       77  como-data          pic 9(8).
       77  como-ora           pic 9(8).
       77  como-imponibile    pic 9(9)v999  value 0.
       77  tot-imponibile     pic 9(9)v99   value 0.
       77  como-ivato         pic 9(9)v999  value 0.
       77  tot-ivato          pic 9(9)v99   value 0.
       77  como-valore        pic 9(9)v99   value 0.

      * STATUS DEI FILES
       77  status-tnotacr     pic xx.
       77  status-rnotacr     pic xx.
       77  status-clienti     pic xx.
       77  status-articoli    pic xx.
       77  status-destini     pic xx.
       77  status-tvettori    pic xx.
       77  status-lineseq     pic xx.
       77  status-agenti      pic xx.
       77  status-tcodpag     pic xx.
       77  status-tivaese     pic xx.
       77  status-progmag     pic xx.
       77  status-tcaumag     pic xx.

       77  wstampa            pic x(256).

      * FLAGS
       77  filler             pic 9.
         88 NoteGiaScritte    value 1, false 0.

       77  filler             pic 9.
         88 ExitPerform       value 1, false 0.

       77  filler             pic 9.
         88 no-dati           value 1, false 0.

       77  filler             pic x.
         88 primo-passaggio   value 1, false 0.

      * 77  controlli          pic xx.
      *   88 tutto-ok          value "OK".
      *   88 errori            value "ER".

       01  testata-1.
           05 t1-tipodoc      pic x(30).
           05 filler          pic x.
           05 t1-numero       pic z(6).
           05 filler          pic x(3).
           05 t1-ragsoc       pic x(80).
           05 filler          pic x(2).
           05 t1-cod-cli      pic z(6).

       01  testata-2.
           05 filler          pic x(40).
           05 t2-indirizzo    pic x(40).

       01  testata-3.
           05 filler          pic x(40).
           05 t3-cap          pic z(5).
           05 filler          pic x.
           05 t3-localita     pic x(30).
           05 filler          pic x(17).
           05 t3-cod-pag      pic x(3).
           05 filler          pic x.
           05 t3-des-pag      pic x(31).

       01  testata-4.
           05 filler          pic x(10) value "Consegna: ".
           05 t4-prg-destino  pic z(5).
           05 filler          pic x(1).
           05 t4-ragsoc       pic x(35).
           05 filler          pic x(1).
           05 t4-indirizzo    pic x(35).
           05 filler          pic x(1).
           05 t4-localita     pic x(35).

       01  testata-fissa.
           05 filler          pic x(4).
           05 titolo-codice   pic x(6)  value "Codice".
           05 filler          pic x(2).
           05 titolo-articolo pic x(78-CrtDesArt).
           05 filler          pic x(3).
           05 titolo-qta      pic x(9)  value "Quantita'".
           05 filler          pic x(7).
           05 filler          pic x(6)  value "Prezzo".
           05 filler          pic x(3).
           05 filler          pic x(7)  value "Imposta".
           05 filler          pic x(7).
           05 filler          pic x(3)  value "COU".
           05 filler          pic x(4).
           05 filler          pic x(6)  value "Piombo".
           05 filler          pic x(8).
           05 filler          pic x(6)  value "Valore".
           05 filler          pic x(1).
           05 filler          pic x(3)  value "IVA".
           05 filler          pic x(2).
           05 filler          pic x(3)  value "Cod".

       01  divisorio.
           05 filler          pic x(130) value all "-".

       01  riga1.
           05 r-riga          pic 99   blank zero.
           05 filler          pic x(2).
           05 r-articolo      pic z(6) blank zero.
           05 filler          pic x(2).
           05 r-des-articolo  pic x(78-CrtDesArt).
           05 filler          pic x.
           05 r-qta           pic zz.zzz.zz9 blank zero.
           05 filler          pic x(2).
           05 r-netto         pic z.zzz.zz9,99.
           05 filler          pic x(2).
           05 r-imposta       pic z.zz9,99.
           05 filler          pic x(2).
           05 r-cou           pic z.zz9,99.
           05 filler          pic x(2).
           05 r-piombo        pic z.zz9,99.
           05 filler          pic x(2).
           05 r-valore        pic z.zzz.zz9,99.
           05 filler          pic x(2).
           05 r-iva           pic 99   blank zero.
           05 filler          pic x(2).
           05 r-cod-iva       pic x(3).

       01  riga-note.
           05 filler          pic x(12).
           05 r-riga-note     pic x(50).

       01  riga-totali.
           05 filler          pic x(18) value "Totale Imponibile:".
           05 filler          pic x(2).
           05 tot-imp-edit    pic zzz.zzz.zz9,99.
           05 filler          pic x(15).
           05 filler          pic x(13) value "Totale Ivato:".
           05 filler          pic x(2).
           05 tot-iva-edit    pic zzz.zzz.zz9,99.
           05 filler          pic x(15).
           05 filler          pic x(14) value "Contropartita:".
           05 filler          pic x(2).
           05 tot-controp     pic x(8).

       01  divisorio-finale.
           05 filler          pic x(130) value all "*".
      
       01  intestazioni-note pic x(5000).
       01  intestazione-note pic x(50)   occurs 100 
                                         redefines intestazioni-note.
       77  num-righe-note                pic 9(2).
       77  cont-inizio                   pic 9(3).
       77  cont-per                      pic 9(3).
       77  cont-char                     pic 9(3).

       01  filler                        pic 9.
           88 exit-perform-int           value 1 false zero.

       78  max-righe        value 66.
       77  num-righe        pic 99 value 0.
       77  diff-righe       pic 99 value 0.
       77  n-vuote          pic 99 value 0.
       77  sav-riga         pic x(900).
       77  sw-corpo         pic 9  value 0.

       LINKAGE SECTION.
           copy "link-stbrogc.def".

       PROCEDURE DIVISION USING stbrogcp-limiti.
       DECLARATIVES.
      
      ***---
       TNOTACR-ERR SECTION.
           use after error procedure on tnotacr.
           set tutto-ok  to true.
           evaluate status-tnotacr
           when "35"
                display message "Impossibile procedere."
                x"0d0a""File testata nota credito [TNOTACR] inesistente"
                          title tit-err
                           icon 2
                set errori to true
           when "39"
                display message "File Mismatch size!"
                          title tit-err
                           icon 3
                set errori to true
           when "98"
                display message "Indexed file corrupt!"
                          title tit-err
                           icon 3
                set errori to true
           end-evaluate.
      
      ***---
       RNOTACR-ERR SECTION.
           use after error procedure on rnotacr.
           set tutto-ok  to true.
           evaluate status-rnotacr
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File riga nota credito [RNOTACR] inesistente"
                          title tit-err
                           icon 2
                set errori to true
           when "39"
                display message "File Mismatch size!"
                          title tit-err
                           icon 3
                set errori to true
           when "98"
                display message "Indexed file corrupt!"
                          title tit-err
                           icon 3
                set errori to true
           end-evaluate.

      ***---
       CLIENTI-ERR SECTION.
           use after error procedure on clienti.
           set tutto-ok  to true.
           evaluate status-clienti
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File clienti [CLIENTI] inesistente"
                          title tit-err
                           icon 2
                set errori to true
           when "39"
                display message "File Mismatch size!"
                          title tit-err
                           icon 3
                set errori to true
           when "98"
                display message "Indexed file corrupt!"
                          title tit-err
                           icon 3
                set errori to true
           end-evaluate.

      ***---
       ARTICOLI-ERR SECTION.
           use after error procedure on articoli.
           set tutto-ok  to true.
           evaluate status-articoli
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File articoli [ARTICOLI] inesistente"
                          title tit-err
                           icon 2
                set errori to true
           when "39"
                display message "File Mismatch size!"
                          title tit-err
                           icon 3
                set errori to true
           when "98"
                display message "Indexed file corrupt!"
                          title tit-err
                           icon 3
                set errori to true
           end-evaluate.

      ***---
       DESTINI-ERR SECTION.
           use after error procedure on destini.
           set tutto-ok  to true.
           evaluate status-destini
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File destini [DESTINI] inesistente"
                          title tit-err
                           icon 2
                set errori to true
           when "39"
                display message "File Mismatch size!"
                          title tit-err
                           icon 3
                set errori to true
           when "98"
                display message "Indexed file corrupt!"
                          title tit-err
                           icon 3
                set errori to true
           end-evaluate.

      ***---
       TVETTORI-ERR SECTION.
           use after error procedure on tvettori.
           set tutto-ok  to true.
           evaluate status-tvettori
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File vettori [TVETTORI] inesistente"
                          title tit-err
                           icon 2
                set errori to true
           when "39"
                display message "File Mismatch size!"
                          title tit-err
                           icon 3
                set errori to true
           when "98"
                display message "Indexed file corrupt!"
                          title tit-err
                           icon 3
                set errori to true
           end-evaluate.

      ***---
       LINESEQ-ERR SECTION.
           use after error procedure on lineseq.
           set tutto-ok  to true.
           evaluate status-lineseq
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File lineseq [LINESEQ] inesistente"
                          title tit-err
                           icon 2
                set errori to true
           when "39"
                display message "File Mismatch size!"
                          title tit-err
                           icon 3
                set errori to true
           when "98"
                display message "Indexed file corrupt!"
                          title tit-err
                           icon 3
                set errori to true
           end-evaluate.

      ***---
       AGENTI-ERR SECTION.
           use after error procedure on agenti.
           set tutto-ok  to true.
           evaluate status-agenti
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File agenti [AGENTI] inesistente"
                          title tit-err
                           icon 2
                set errori to true
           when "39"
                display message "File Mismatch size!"
                          title tit-err
                           icon 3
                set errori to true
           when "98"
                display message "Indexed file corrupt!"
                          title tit-err
                           icon 3
                set errori to true
           end-evaluate.

      ***---
       TCODPAG-ERR SECTION.
           use after error procedure on tcodpag.
           set tutto-ok  to true.
           evaluate status-tcodpag
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File codici pagamenti [TCODPAG] inesistente"
                          title tit-err
                           icon 2
                set errori to true
           when "39"
                display message "File Mismatch size!"
                          title tit-err
                           icon 3
                set errori to true
           when "98"
                display message "Indexed file corrupt!"
                          title tit-err
                           icon 3
                set errori to true
           end-evaluate.

      ***---
       TIVAESE-ERR SECTION.
           use after error procedure on tivaese.
           set tutto-ok  to true.
           evaluate status-tivaese
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""Tabella codici iva [TIVAESE] inesistente"
                          title tit-err
                           icon 2
                set errori to true
           when "39"
                display message "File Mismatch size!"
                          title tit-err
                           icon 3
                set errori to true
           when "98"
                display message "Indexed file corrupt!"
                          title tit-err
                           icon 3
                set errori to true
           end-evaluate.

      ***---
       PROGMAG-ERR SECTION.
           use after error procedure on progmag.
           set tutto-ok  to true.
           evaluate status-progmag
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File progressivi [PROGMAG] inesistente"
                          title tit-err
                           icon 2
                set errori to true
           when "39"
                display message "File Mismatch size!"
                          title tit-err
                           icon 3
                set errori to true
           when "98"
                display message "Indexed file corrupt!"
                          title tit-err
                           icon 3
                set errori to true
           end-evaluate.

      ***---
       TCAUMAG-ERR SECTION.
           use after error procedure on tcaumag.
           set tutto-ok  to true.
           evaluate status-tcaumag
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File causali magazzino [TCAUMAG] inesistente"
                          title tit-err
                           icon 2
                set errori to true
           when "39"
                display message "File Mismatch size!"
                          title tit-err
                           icon 3
                set errori to true
           when "98"
                display message "Indexed file corrupt!"
                          title tit-err
                           icon 3
                set errori to true
           end-evaluate.

       END DECLARATIVES.

       MAIN SECTION.
           perform INIT.
           perform APRI-FILES.

           if tutto-ok
              perform CICLO-TNOTACR

              if no-dati              
                 display message "Nessun Dato per i limiti impostati!"
                         title "Stampa notacr"
                         icon 2
                 initialize stbrog-path
              else
                 perform SALTO-PAGINA
              end-if

              perform CHIUDI-FILES
           end-if

           goback.
 
      ***---
       INIT.
           initialize wstampa.
           accept  wstampa         from environment "PATH-ST".
           inspect wstampa         replacing trailing space by low-value
           accept  como-data       from century-date.
           accept  como-ora        from time.
           set     NoteGiaScritte  to false.
           set     no-dati         to true.
           set     primo-passaggio to true.
           set     tutto-ok        to true.
           string wstampa          delimited by low-value
                  "stbrogc-fm-nc"  delimited by size
                  "_"              delimited by size
                  como-data        delimited by size
                  "_"              delimited by size
                  como-ora         delimited by size
                  ".txt"           delimited by size
                  into wstampa
           end-string.
           move wstampa to stbrog-path.

      ***---
       APRI-FILES.
           open input  tnotacr,
                       rnotacr,
                       clienti,
                       articoli,
                       tvettori,
                       destini
                       agenti
                       tcodpag
                       tivaese
                       progmag
                       tcaumag.
           open output lineseq.


      ***---
       CHIUDI-FILES.
           close       tnotacr,
                       rnotacr,
                       clienti,
                       articoli,
                       tvettori,
                       destini
                       agenti
                       tcodpag
                       tivaese
                       lineseq
                       progmag
                       tcaumag.

      ***---
       CICLO-TNOTACR.    
           set  tutto-ok      to true.                                                                        

           move stbrogc-da-anno to tno-anno.
           move stbrogc-da-num  to tno-numero.

           start tnotacr key >= tno-chiave
                 invalid continue
             not invalid perform SCORRI-TNOTACR
           end-start.

      ***---
       SCORRI-TNOTACR.
           set ExitPerform to false.
           perform until 1 = 2
              read tnotacr next 
                   at end  set ExitPerform to true 
              end-read

              if tno-anno > stbrogc-a-anno
                 set ExitPerform to true
              end-if

              if ExitPerform
                 exit perform
              end-if

              perform VALIDA-RECORD

              if tutto-ok
                 perform CICLO-STAMPA
              end-if
           end-perform.

      ***---
       VALIDA-RECORD.
           move 0 to como-imponibile
                      tot-imponibile
                     como-ivato
                      tot-ivato
                     como-valore.
           set tutto-ok to true.
           if  tno-numero < stbrogc-da-num or 
               tno-numero > stbrogc-a-num
               set errori to true
           else
LUBEXX*       |Scarto gli ordini già fatturati
LUBEXX        if tno-num-fattura = 0 or tno-data-fattura = 0
                 if no-dati set no-dati to false end-if
                 perform RELAZIONE-TNOTACR-CLIENTI
                 if tutto-ok
                    perform RELAZIONE-TNOTACR-DESTINI 
                    perform RELAZIONE-TNOTACR-AGENTI  
                    perform RELAZIONE-TNOTACR-TCODPAG
                    perform RELAZIONE-TNOTACR-CAUSALI
                 end-if
LUBEXX        else
LUBEXX           set errori to true
LUBEXX        end-if
           end-if.  

      ***---
      * RELAZIONI DELLA TESTATA notacr (Tnotacr)
      ***---
       RELAZIONE-TNOTACR-CLIENTI.
           initialize cli-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.
           set tutto-ok     to true.
           set cli-tipo-C   to true.
           move tno-cod-cli to cli-codice.
           read clienti key cli-chiave invalid continue end-read.

      ***---
       RELAZIONE-TNOTACR-DESTINI.
           initialize des-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.
           set tutto-ok to true.
           if tno-prg-destino       not = 0
              move tno-cod-cli      to des-codice
              move tno-prg-destino  to des-prog
              read destini  invalid continue end-read
           end-if.

      ***---
       RELAZIONE-TNOTACR-AGENTI.
           set tutto-ok to true.
           initialize age-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.
           if tno-cod-agente not = 0                              
              move tno-cod-agente to age-codice
              read agenti invalid continue end-read
           end-if.

      ***---
       RELAZIONE-TNOTACR-TCODPAG.
           set tutto-ok to true.
           initialize record-tblpa replacing numeric data by zeroes
                                        alphanumeric data by spaces.
           if tno-cod-pagamento not = spaces
              move "PA"              to tblpa-codice1
              move tno-cod-pagamento to tblpa-codice2
              move spaces to tblpa-descrizione1
              move spaces to tblpa-descrizione2
              read tcodpag no lock invalid continue end-read
           end-if. 
            
      ***---
       RELAZIONE-TNOTACR-CAUSALI.
           initialize tca-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.
           move tno-causale to tca-codice
           read tcaumag invalid continue end-read.

      ***---
      * RELAZIONI DELLE RIGHE DEGLI notacr (Rnotacr)
      ***---
       RELAZIONE-RNOTACR-ARTICOLI.
           move rno-cod-articolo to art-codice.
           
           read articoli key art-chiave
              invalid 
                 initialize art-rec
           end-read.

      ***---
       RELAZIONE-RNOTACR-PROGMAG.
           move spaces           to prg-cod-magazzino.
           move spaces           to prg-tipo-imballo.
           move 0                to prg-peso.
           move rno-cod-articolo to prg-cod-articolo.
           
           read progmag key prg-chiave 
                invalid move 0 to prg-costo-ultimo
           end-read.

      ***---
       RELAZIONE-RNOTACR-IVA.
           move 0           to tbliv-percentuale.
           move "IV"        to tbliv-codice1.
           move rno-cod-iva to tbliv-codice2.
           read tivaese invalid continue end-read.

      ***---
       CICLO-STAMPA.
           perform STAMPA-INTESTAZIONE.

           move stbrogc-da-anno to rno-anno.
           move tno-numero      to rno-numero.
           move low-values      to rno-num-riga.

           start rnotacr key >= rno-chiave
             invalid 
                continue
             not invalid
                perform until 1 = 2
                   read rnotacr next 
                        at end  
                           perform STAMPA-PIEDE
                           set NoteGiaScritte to false
                           exit perform 
                    end-read

                   if rno-anno   not = tno-anno   or
                      rno-numero not = tno-numero
                      perform STAMPA-PIEDE
                      set NoteGiaScritte to false
                      exit perform
                   end-if

                   perform RELAZIONE-RNOTACR-ARTICOLI
                   perform RELAZIONE-RNOTACR-PROGMAG
                   perform RELAZIONE-RNOTACR-IVA
                   perform STAMPA-CORPO
                end-perform
           end-start.

      ***---
       STAMPA-INTESTAZIONE.
           initialize t1-tipodoc     t1-numero t1-cod-cli   t1-ragsoc
                      t2-indirizzo   t3-cap    t3-localita  t3-cod-pag
                      t4-prg-destino t4-ragsoc t4-indirizzo t4-localita.

           move   tno-numero    to t1-numero.
           call "C$JUSTIFY"  using t1-numero, "L".
           move tca-descrizione to t1-tipodoc.
           move tno-cod-cli to t1-cod-cli.
           initialize t1-ragsoc.
           inspect cli-ragsoc-1 replacing trailing spaces by low-value.
           string  cli-ragsoc-1 delimited by low-value
                   " "          delimited by size
                   cli-ragsoc-2 delimited by size
                   into t1-ragsoc
           end-string.
           move tno-cod-cli    to t1-cod-cli.
           call "C$JUSTIFY" using t1-cod-cli, "L".
           move cli-ragsoc-1   to t1-ragsoc.
           initialize line-riga.
           move testata-1 to line-riga.
           perform STAMPA-RIGA.
                                
           move cli-indirizzo to t2-indirizzo.
           initialize line-riga.
           move testata-2 to line-riga.
           perform STAMPA-RIGA.

           move cli-cap            to t3-cap.
           move cli-localita       to t3-localita.
           move tno-cod-pagamento  to t3-cod-pag.
           move tblpa-descrizione1 to t3-des-pag.
           initialize line-riga.
           move testata-3 to line-riga.
           perform STAMPA-RIGA. 

           move tno-prg-destino to t4-prg-destino.
           move des-ragsoc-1    to t4-ragsoc.
           move des-indirizzo   to t4-indirizzo.
           move des-localita    to t4-localita.
           initialize line-riga.
           move testata-4 to line-riga.
           perform STAMPA-RIGA. 

           move spaces to line-riga.
           perform STAMPA-RIGA. 

           move tno-causale to tca-codice
           read tcaumag no lock invalid continue end-read.
           if tca-tipo-nota-abbuono
              move spaces             to titolo-codice
              move "Descrizione Voce" to titolo-articolo
              move spaces             to titolo-qta
           else
              move "Codice"    to titolo-codice
              move "Articolo"  to titolo-articolo
              move "Quantita'" to titolo-qta
           end-if.
           
           call "C$JUSTIFY" using titolo-articolo, "C".
           initialize line-riga.
           move testata-fissa to line-riga.
           perform STAMPA-RIGA.

           initialize line-riga.
           move divisorio to line-riga.
           perform STAMPA-RIGA.


      ***---
       STAMPA-CORPO.
           if not NoteGiaScritte 
              perform SCRIVI-NOTE
           end-if.
           initialize r-riga    r-articolo r-des-articolo r-qta r-netto
                      r-imposta r-cou      r-piombo r-valore    r-iva.
           move rno-num-riga      to r-riga.
           move rno-cod-articolo  to r-articolo.
           move art-descrizione   to r-des-articolo.
           move rno-qta           to r-qta.
           call "C$JUSTIFY"    using r-qta, "R".
           move rno-prz-unitario  to r-netto.
           call "C$JUSTIFY"    using r-netto, "R".
           move rno-imp-consumo   to r-imposta.
           call "C$JUSTIFY"    using r-imposta, "R".
           move rno-imp-cou-cobat to r-cou.
           call "C$JUSTIFY"    using r-cou, "R".
           move rno-add-piombo    to r-piombo.
           call "C$JUSTIFY"    using r-piombo, "R".

           move tno-causale to tca-codice.
           read tcaumag no lock invalid continue end-read.
           if tca-tipo-nota-abbuono
              move 1              to rno-qta
              move rno-des-libera to r-des-articolo
           end-if.

           compute como-valore =  (rno-prz-unitario  + rno-imp-consumo +
                                   rno-imp-cou-cobat + rno-add-piombo) * 
                                   rno-qta.
           move como-valore       to r-valore.
           call "C$JUSTIFY"    using r-valore, "R".
           move tbliv-percentuale to r-iva.
           move rno-cod-iva       to r-cod-iva.
           initialize line-riga.
           move riga1 to line-riga.
           move 1 to sw-corpo.
           perform STAMPA-RIGA.
           move 0 to sw-corpo.

           compute como-imponibile = como-imponibile + como-valore.

           compute como-ivato      = como-ivato +
                   ( como-valore + 
                 ( ( como-valore * tbliv-percentuale ) / 100) ).

      ***---
       STAMPA-PIEDE.
           move tno-contropartita to tot-controp.
           move 0 to tot-imponibile tot-ivato.
           compute tot-imponibile = como-imponibile + 0,005.
           compute tot-ivato      = como-ivato      + 0,005.
           move tot-imponibile to tot-imp-edit.
           move tot-ivato      to tot-iva-edit.
           call "C$JUSTIFY" using tot-imponibile, "R".
           call "C$JUSTIFY" using tot-ivato,      "R".
           move spaces to line-riga.
           perform STAMPA-RIGA.

           move spaces to line-riga.
           perform STAMPA-RIGA.

           initialize line-riga.
           move riga-totali to line-riga.
           perform STAMPA-RIGA.

           move spaces to line-riga.
           perform STAMPA-RIGA.

           initialize line-riga.
           move divisorio-finale to line-riga.
           perform STAMPA-RIGA.

           move spaces to line-riga.
           perform STAMPA-RIGA.

           move spaces to line-riga.
           perform STAMPA-RIGA.

      ***---
       SCRIVI-NOTE.
           set NoteGiaScritte to true.
           initialize intestazioni-note.

           move zero   to idx
           move 1      to cont-inizio
           set exit-perform-int to false

           perform 100 times
              if cont-inizio >= 500
                 exit perform
              end-if
              add 1 to idx                 
              move zero   to cont-char
      *    controllo di non andare oltre i 500 caratteri
              if cont-inizio > 451
                 compute cont-per = 500 - cont-inizio
                 set exit-perform-int to true
              else
                 move 50  to cont-per
              end-if

              inspect tno-note(cont-inizio:cont-per)
                      tallying cont-char for all x"0D"

              if cont-char = zero
                 |move 50  to cont-per
                 continue
              else
                 initialize cont-per
                 inspect tno-note(cont-inizio:50) tallying cont-per
                       for characters before x"0D"
              end-if
              if cont-per not = zero
                 move tno-note(cont-inizio:cont-per) 
                                         to intestazione-note(idx)
      *    se appena dopo i 50 caratteri premo invio devo ignorarlo
                 if cont-per = 50
                    add cont-per  to cont-inizio
                    if cont-inizio < 499
                             and tno-note(cont-inizio:1) = x"0D"
                       add 2 to cont-inizio
                    end-if
                    subtract cont-per from cont-inizio
                 end-if
              else
                 move space  to intestazione-note(idx)
              end-if
              if cont-char = zero
                 add 50   to cont-inizio
              else
                 compute cont-inizio = cont-inizio + cont-per + 2                                                         
              end-if
              if exit-perform-int
                 exit perform
              end-if
           end-perform.

           initialize num-righe-note
           perform varying idx from 1 by 1 until idx > 100
              if intestazione-note(idx) not = space
                 move idx to num-righe-note
              end-if
           end-perform.

           perform varying idx from 1 by 1 until idx > num-righe-note
              initialize riga-note line-riga
              move intestazione-note(idx) to r-riga-note
              initialize line-riga
              move riga-note to line-riga
              perform STAMPA-RIGA
           end-perform.

      ***---
       SALTO-PAGINA.
           compute diff-righe = max-righe - num-righe.
           move diff-righe to n-vuote
           perform RIGHE-VUOTE  
           move 0 to num-righe.

      ***---
       RIGHE-VUOTE.
           perform n-vuote times
              write line-riga from spaces
           end-perform
           add n-vuote to num-righe. 

      ***---
       STAMPA-RIGA.
           initialize sav-riga.
           move line-riga to sav-riga
           if num-righe > max-righe - 5
              perform SALTO-PAGINA
              if sw-corpo = 1
                 initialize line-riga
                 write line-riga from testata-fissa
                 initialize line-riga
                 write line-riga from divisorio
                 add 2 to num-righe
              end-if
           end-if
           move sav-riga to line-riga
           write line-riga
           add 1 to num-righe. 
