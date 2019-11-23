       program-id.     stbrogcp.
       author.         Andrea.

       special-names. DECIMAL-POINT is COMMA.

       file-control.
           copy "mtordini.sl".
           copy "tordini.sl".
           copy "rordini.sl".
           copy "clienti.sl".
           copy "articoli.sl".
           copy "destini.sl". 
           copy "tvettori.sl".
           copy "lineseq.sl".
           copy "agenti.sl".
           copy "tcodpag.sl".
           copy "tivaese.sl".
           copy "progmag.sl".
           copy "tmovmag.sl".
           copy "rmovmag.sl".
           copy "tcaumag.sl".
           copy "ttipocli.sl".
           copy "lisagente.sl".
           copy "assorcli.sl".
           copy "tparamge.sl".
           copy "tpiombo.sl".
           copy "listini.sl".
           copy "param.sl".

       file section.          
           copy "mtordini.fd".
           copy "tordini.fd".
           copy "rordini.fd".
           copy "clienti.fd".
           copy "articoli.fd".
           copy "destini.fd". 
           copy "tvettori.fd".
           copy "lineseq.fd".
           copy "agenti.fd". 
           copy "tcodpag.fd".
           copy "tivaese.fd".
           copy "progmag.fd".
           copy "tmovmag.fd".
           copy "rmovmag.fd".
           copy "tcaumag.fd".
           copy "ttipocli.fd".
           copy "lisagente.fd".
           copy "assorcli.fd".
           copy "tparamge.fd".
           copy "tpiombo.fd".
           copy "listini.fd".
           copy "param.fd".

       working-storage section.
           copy "imposte.def".
           copy "comune.def".
           copy "PRT-ESC.def".
           copy "recupera-prz-listino.def".
           copy "trova-parametro.def".

       78  titolo             value "Stampa Brogliaccio".
       77  como-lst-data      pic 9(8).
       77  como-data          pic 9(8).
       77  como-ora           pic 9(8).
       77  como-imponibile    pic 9(9)v999  value 0.
       77  tot-imponibile     pic 9(9)v99   value 0.
       77  como-ivato         pic 9(9)v999  value 0.
       77  tot-ivato          pic 9(9)v99   value 0.
       77  como-valore        pic 9(9)v99   value 0.
       77  como-arrot         pic 9(2)v999.
       77  differenza         pic 9(9)v999.
       77  totale             pic 9(6)v999.
       77  tassato            pic 9(6)v999.
       77  peso               pic 9(6)v999.
       77  tot-qli            pic 9(9)v99.
       77  tot-totali         pic 9(9)v99.
       77  imballi-ed         pic  zz.zzz.zz9.
       77  r-netto-ord        pic zzz.zz9,99 blank zero.
       77  counter            pic 9(10).
       77  counter2           pic 9(10).
       77  counter-edit       pic z(10).

      * STATUS DEI FILES
       77  status-mtordini    pic xx.
       77  status-tordini     pic xx.
       77  status-rordini     pic xx.
       77  status-clienti     pic xx.
       77  status-articoli    pic xx.
       77  status-destini     pic xx.
       77  status-tvettori    pic xx.
       77  status-lineseq     pic xx.
       77  status-agenti      pic xx.
       77  status-tcodpag     pic xx.
       77  status-progmag     pic xx.
       77  status-tmovmag     pic xx.
       77  status-rmovmag     pic xx.
       77  status-tcaumag     pic xx.
       77  status-tivaese     pic xx.
       77  status-ttipocli    pic xx.
       77  status-lisagente   pic xx.
       77  status-timballi    pic xx.
       77  status-assorcli    pic xx.
       77  status-tparamge    pic xx.
       77  status-tpiombo     pic xx.
       77  status-listini     pic xx.
       77  status-param       pic xx.

       77  wstampa            pic x(256).

      * FLAGS
LUBEXX 77  filler                pic 9.
LUBEXX     88  StampaPrenotate   value 1, false 0.

       77  filler                pic 9.
           88  trovato-assorcli  value 1, false 0.

       77  filler             pic 9.
         88 trovato-ultimo-prezzo value 1, false 0.

       77  filler             pic 9.
         88 NoteGiaScritte    value 1, false 0.

       77  filler             pic 9.
         88 no-dati           value 1, false 0.

       77  filler             pic x.
         88 primo-passaggio   value 1, false 0.

      * 77  controlli          pic xx.
      *   88 tutto-ok          value "OK".
      *   88 errori            value "ER".

       01  testata-1-ord.
      *     05 filler             pic x(10).
           05 t1-cau-descrizione pic x(39).
           05 filler             pic x.
           05 t1-numero-ord      pic z(8).
           05 filler             pic x(1).
           05 t1-data-ord        pic x(8).
           05 filler             pic x(2).
           05 filler             pic x(7)  value "Ord Cli".
           05 filler             pic x(1).
           05 t1-num-ord-cli     pic x(10).
           05 filler             pic x(5)  value "Bolla".
           05 filler             pic x(3).
           05 t1-bolla-ord       pic z(8).
           05 filler             pic x(2).
           05 t1-datbolla-ord    pic x(8).
           05 filler             pic x(2).
           05 filler             pic x(7)  value "Fattura".
           05 filler             pic x(2).
           05 t1-fattura-ord     pic z(8).
           05 filler             pic x(2).
           05 t1-datfattura-ord  pic x(8).
           05 filler             pic x(21) value "  Blocco 500 Kg UTF".
           05 t1-blocco-500      pic x(2).

       01  testata-2-ord.
           05 filler             pic x(10).
           05 t2-cod-cli-ord     pic z(5).
           05 filler             pic x(5).
           05 t2-ragsoc-ord      pic x(40).
           05 filler             pic x(17).
           05 filler             pic x(6)  value "Agente".
           05 filler             pic x(3).
           05 t2-cod-agente-ord  pic z(5).
           05 filler             pic x(1).
           05 t2-des-agente-ord  pic x(40).

       01  testata-3-ord.
           05 filler             pic x(20).
           05 t3-indirizzo-ord   pic x(40).
           05 filler             pic x(17).
           05 filler             pic x(6)  value "Pagam.".
           05 filler             pic x(3).
           05 t3-cod-pag-ord     pic x(3).
           05 filler             pic x(3).
           05 t3-des-pag-ord     pic x(40).

       01  testata-4-ord.
           05 filler             pic x(20).
           05 t4-cap-ord         pic x(5).
           05 filler             pic x(1).
           05 t4-localita-ord    pic x(40).
           05 filler             pic x(1).
           05 t4-prov-ord        pic x(2).
           05 filler             pic x(8).
           05 filler             pic x(9)  value "Corriere:".
           05 t4-cod-vettore-ord pic z(5).
           05 filler             pic x(1).
           05 t4-des-vettore-ord pic x(40).

       01  testata-5-ord.
           05 filler             pic x(10).
           05 filler             pic x(9)  value "Consegna:".
           05 filler             pic x(1).
           05 t5-cod-destino     pic z(5).
           05 filler             pic x(1).
           05 t5-destino-ord     pic x(40).
           05 filler             pic x(1).
           05 t5-indirizzo-ord   pic x(40).

       01  testata-6-ord.
           05 filler             pic x(20).
           05 t6-localita-ord    pic x(40).
           05 filler             pic x(7).
           05 t6-prov-ord        pic x(2).   

       01  testata-7-ord.
           05 filler             pic x(2).
           05 filler             pic x(17) value "Dati di consegna:".
           05 filler             pic x(1).
           05 t7-note-1          pic x(19).
           05 filler             pic x(2).
           05 t7-note-data       pic x(8).
           05 filler             pic x(2).
           05 t7-note-2          pic x(30).
           05 filler             pic x(2).
           05 t7-note-3          pic x(30).
           05 filler             pic x(2).
           05 t7-note-4          pic x(30).


       01  testata-fissa-ord.
           05 filler             pic x(2).
           05 filler             pic x(6)  value "Codice".
           05 filler             pic x(5).
           05 filler             pic x(4)  value "Peso".
           05 filler             pic x(1).
           05 filler             pic x(8)  value "Cod. GDO".
           05 filler             pic x(6).
           05 filler             pic x(9)  value "Nr. colli".
           05 filler             pic x(13).
           05 filler             pic x(8)  value "Articolo".
           05 filler             pic x(23).
           05 filler             pic x(5)  value "Pezzi".
           05 filler             pic x(6).
           05 filler             pic x(5)  value "NETTO".
           05 filler             pic x(3).
           05 filler             pic x(3)  value "IVA".
           05 filler             pic x(3).
           05 filler             pic x(9)  value "Pz.Agente".
           05 filler             pic x(3).
           05 filler             pic x(14) value "Ultima vendita".

       01  riga1-ord.
           05 r-riga-ord         pic 99.
           05 filler             pic x.
           05 r-articolo-ord     pic z(5).
           05 filler             pic x.
           05 r-peso-ord         pic zzz9,999 blank zero.
           05 filler             pic x.
           05 r-cod-art-cli-ord  pic x(11).
           05 filler             pic x.
           05 r-colli-ord        pic z(4).
           05 filler             pic x.
           05 r-imballo-ord      pic x(18).
           05 filler             pic x.
           05 r-des-articolo-ord pic x(30).
           05 filler             pic x(1).
           05 r-qta-ord          pic z(5).
           05 filler             pic x.
           05 r-netto-ord-x      pic x(10).
           05 filler             pic x(3).
           05 r-cod-iva          pic x(3).
           05 filler             pic x(2) value " (".
           05 r-pz-agente        pic zzz.zz9,99 blank zero.
           05 filler             pic x value ")".
           05 filler             pic x(2).                      
           05 r-data-ord         pic x(8).
           05 filler             pic x.
           05 r-ult-prz-ord      pic zzz.zz9,99 blank zero.
           05 filler             pic x.
           05 r-sottocosto-ord   pic x(2).
           05 filler             pic x.
           05 r-costo-ultimo-ord pic zzz.zz9,99 blank zero.

       01  riga-totali-ord.
           05 filler             pic x(22) value all"-".
           05 filler             pic x(2).
           05 filler             pic x(15) value "Totale peso Kg:".
           05 filler             pic x(2).
           05 tot-peso-ord       pic zzz.zz9,999 blank zero.
           05 filler             pic x(2).
           05 filler             pic x(11) value "Tassato Kg:".
           05 filler             pic x(2).
           05 tot-tassato-ord    pic zzz.zz9,999 blank zero.
           05 filler             pic x(1).
           05 filler             pic x(10) value "Differenza".
           05 filler             pic x(2).
           05 tot-diff-ord       pic zzz.zz9,99 blank zero.

       01  totalizzatore-finale-ord.
           05 filler             pic x(10).
           05 tot-data-ord       pic x(10).
           05 filler             pic x(3) value " - ".
           05 tot-ora-ord        pic x(5).
           05 filler             pic x(20).
           05 tot-descr-ord      pic x(35).
           05 filler             pic x(1).
           05 tot-qli-ed-ord     pic zzz.zzz.zz9,99.

       01  testata-1-fm.
           05 t1-tipodoc-fm      pic x(18).
           05 filler             pic x.
           05 t1-numero-fm       pic z(8).
           05 filler             pic x(2).
           05 t1-cod-cli-fm      pic z(6).
           05 filler             pic x(5).
           05 t1-ragsoc-fm       pic x(80).

       01  testata-2-fm.
           05 filler             pic x(40).
           05 t2-indirizzo-fm    pic x(40).

       01  testata-3-fm.
           05 filler             pic x(40).
           05 t3-cap-fm          pic z(5).
           05 filler             pic x.
           05 t3-localita-fm     pic x(35).
           05 filler             pic x(25).
           05 t3-cod-pag-fm      pic x(3).

       01  testata-4-fm.
           05 filler             pic x(10) value "Consegna: ".
           05 t4-prg-destino-fm  pic z(5).
           05 filler             pic x(1).
           05 t4-ragsoc-fm       pic x(35).
           05 filler             pic x(1).
           05 t4-indirizzo-fm    pic x(35).
           05 filler             pic x(1).
           05 t4-localita-fm     pic x(35).

       01  testata-fissa-fm.
           05 filler             pic x(4).
           05 titolo-codice-fm   pic x(6)  value "Codice".
           05 filler             pic x(2).
           05 titolo-articolo-fm pic x(40).
           05 filler             pic x(3).
           05 titolo-qta-fm      pic x(9)  value "Quantita'".
           05 filler             pic x(12).
           05 filler             pic x(6)  value "Prezzo".
           05 filler             pic x(7).
           05 filler             pic x(7)  value "Imposta".
           05 filler             pic x(7).
           05 filler             pic x(3)  value "COU".
           05 filler             pic x(4).
           05 filler             pic x(6)  value "Piombo".
           05 filler             pic x(8).
           05 filler             pic x(6)  value "Valore".
           05 filler             pic x(3).
           05 filler             pic x(3)  value "IVA".
           05 filler             pic x(2).
           05 filler             pic x(2)  value " %".

       01  riga1-fm.
           05 r-riga-fm          pic 99   blank zero.
           05 filler             pic x(2).
           05 r-articolo-fm      pic z(6) blank zero.
           05 filler             pic x(2).
           05 r-des-articolo-fm  pic x(40).
           05 filler             pic x.
           05 r-qta-fm           pic zz.zzz.zz9 blank zero.
           05 filler             pic x(7).
           05 r-netto-fm         pic z.zzz.zz9,99.
           05 filler             pic x(6).
           05 r-imposta-fm       pic z.zz9,99.
           05 filler             pic x(2).
           05 r-cou-fm           pic z.zz9,99.
           05 filler             pic x(2).
           05 r-piombo-fm        pic z.zz9,99.
           05 filler             pic x(2).
           05 r-valore-fm        pic z.zzz.zz9,99.
           05 filler             pic x(3).
           05 r-cod-iva-fm       pic x(3).
           05 filler             pic x(2).
           05 r-iva-fm           pic 99 blank zero.

       01  riga-note-fm.
           05 filler             pic x(12).
           05 r-riga-note-fm     pic x(50).

       01  riga-totali-fm.
           05 filler             pic x(18) value "Totale Imponibile:".
           05 filler             pic x(2).
           05 tot-imp-edit-fm    pic zzz.zzz.zz9,99.
           05 filler             pic x(15).
           05 filler             pic x(13) value "Totale Ivato:".
           05 filler             pic x(2).
           05 tot-iva-edit-fm    pic zzz.zzz.zz9,99.
           05 filler             pic x(15).
           05 filler             pic x(14) value "Contropartita:".
           05 filler             pic x(2).
           05 tot-controp-fm     pic x(8).

       01  divisorio.
           05 filler          pic x(155) value all "-".

       01  divisorio-finale.
           05 filler          pic x(155) value all "*". 

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
       77  como-lst-cod-art-cli pic x(15).        
       77  tot-imposte          pic 9(10)v999.

       LINKAGE SECTION.
           copy "link-stbrogc.def".

       PROCEDURE DIVISION USING stbrogcp-limiti.
       DECLARATIVES.
      
      ***---
       TORDINI-ERR SECTION.
           use after error procedure on tordini.
           set tutto-ok  to true.
           evaluate status-tordini
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File testata [TORDINI] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [TORDINI] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[TORDINI] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           end-evaluate.

      ***---
       RORDINI-ERR SECTION.
           use after error procedure on rordini.
           set tutto-ok  to true.
           evaluate status-rordini
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File righe [RORDINI] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [RORDINI] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[RORDINI] Indexed file corrupt!"
                          title titolo
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
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [CLIENTI] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[CLIENTI] Indexed file corrupt!"
                          title titolo
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
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [ARTICOLI] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[ARTICOLI] Indexed file corrupt!"
                          title titolo
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
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [DESTINI] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[DESTINI] Indexed file corrupt!"
                          title titolo
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
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [TVETTORI] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[TVETTORI] Indexed file corrupt!"
                          title titolo
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
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [LINESEQ] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[LINESEQ] Indexed file corrupt!"
                          title titolo
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
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [AGENTI] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[AGENTI] Indexed file corrupt!"
                          title titolo
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
                  x"0d0a""File codici pagamento [TCODPAG] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [TCODPAG] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[TCODPAG] Indexed file corrupt!"
                          title titolo
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
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [TIVAESE] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[TIVAESE] Indexed file corrupt!"
                          title titolo
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
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [PROGMAG] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[PROGMAG] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           end-evaluate.

      ***---
       TMOVMAG-ERR SECTION.
           use after error procedure on tmovmag.
           set tutto-ok  to true.
           evaluate status-tmovmag
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File testata magazzino [TMOVMAG] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [TMOVMAG] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[TMOVMAG] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           end-evaluate.

      ***---
       RMOVMAG-ERR SECTION.
           use after error procedure on rmovmag.
           set tutto-ok  to true.
           evaluate status-rmovmag
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File riga magazzino [RMOVMAG] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [RMOVMAG] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[RMOVMAG] Indexed file corrupt!"
                          title titolo
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
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [TCAUMAG] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[TCAUMAG] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           end-evaluate.

      ***---
       TTIPOCLI-ERR SECTION.
           use after error procedure on ttipocli.
           set tutto-ok  to true.
           evaluate status-ttipocli
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File tipologia clienti [TTIPOCLI] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [TTIPOCLI] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[TTIPOCLI] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           end-evaluate.

      ***---
       LISAGENTE-ERR SECTION.
           use after error procedure on lisagente.
           set tutto-ok  to true.
           evaluate status-lisagente
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File listini agenti [LISAGENTE] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [LISAGENTE] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[LISAGENTE] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           end-evaluate.

      ***---
       ASSORCLI-ERR SECTION.
           use after error procedure on assorcli.
           set tutto-ok  to true.
           evaluate status-assorcli
           when "35"
                display message "Impossibile procedere."
               x"0d0a""File assortimento clienti [ASSORCLI] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [ASSORCLI] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[ASSORCLI] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           end-evaluate.

      ***---
       TPARAMGE-ERR SECTION.
           use after error procedure on tparamge.
           set tutto-ok  to true.
           evaluate status-tparamge
           when "35"
                display message "Impossibile procedere."
               x"0d0a""File parametri [TPARAMGE] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [TPARAMGE] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[TPARAMGE] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           end-evaluate.

      ***---
       LISTINI-ERR SECTION.
           use after error procedure on listini.
           set tutto-ok  to true.
           evaluate status-listini
           when "35"
                display message "Impossibile procedere."
               x"0d0a""File [LISTINI] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [LISTINI] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true


           when "98"
                display message "[LISTINI] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           end-evaluate.

       END DECLARATIVES.

       MAIN SECTION.
           perform INIT.
           perform APRI-FILES.

           if tutto-ok
              move spaces to tge-chiave
              read tparamge no lock invalid continue end-read
              perform RESET-BUFFER-PRINTER
              |Pilotaggio del carattere
              move I-12    to line-riga
              write line-riga after 0

              perform CICLO-TORDINI

              if no-dati
                 display message "Nessun Dato per i limiti impostati!"
                           title titolo
                            icon 2
                 close  lineseq
                 delete file lineseq
              else
                 perform SALTO-PAGINA
                 perform RESET-BUFFER-PRINTER
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
           set     no-dati         to true.
           set     primo-passaggio to true.
           set     tutto-ok        to true.
           string wstampa          delimited by low-value
                  "stbrogcp"       delimited by size
                  "_"              delimited by size
                  como-data        delimited by size
                  "_"              delimited by size
                  como-ora         delimited by size
                  ".txt"           delimited by size
                  into wstampa
           end-string.
           move wstampa to stbrog-path.
           move 0 to tot-totali tot-qli.
LUBEXX     if stbrogc-da-anno = 9999 set StampaPrenotate to true
LUBEXX     else                      set StampaPrenotate to false
LUBEXX     end-if.

      ***---
       APRI-FILES.
           open input  mtordini,
                       tordini,
                       rordini,
                       clienti,
                       articoli,
                       tvettori,
                       destini,
                       agenti,
                       tcodpag,
                       progmag,
                       tmovmag,
                       rmovmag,
                       tcaumag,
                       tivaese,
                       ttipocli,
                       lisagente,
                       assorcli
                       tparamge
                       tpiombo
                       listini
                       param.
           open output lineseq.


      ***---
       CHIUDI-FILES.
           close       mtordini,
                       tordini,
                       rordini,
                       clienti,
                       articoli,
                       tvettori,
                       destini,
                       agenti,
                       tcodpag,
                       progmag,
                       tmovmag,
                       rmovmag,
                       tcaumag,
                       tivaese,
                       ttipocli,
                       lisagente,
                       assorcli
                       tparamge
                       listini
                       param.

           if not no-dati close lineseq end-if.

      ***---
       CICLO-TORDINI.    
           set  tutto-ok      to true.                                                                        

LUBEXX     if StampaPrenotate
LUBEXX        move 0 to tor-anno-fattura
LUBEXX        move 0 to tor-data-fattura
LUBEXX        move 0 to tor-num-fattura
LUBEXX        move 0 to tor-num-prenot
LUBEXX        set tor-fatt-si-prenotata to true
LUBEXX        start tordini key >= k4
LUBEXX              invalid continue
LUBEXX          not invalid perform SCORRI-TORDINI
LUBEXX        end-start
LUBEXX     else
              move stbrogc-da-anno to tor-anno
              move stbrogc-da-num  to tor-numero

              start tordini key >= tor-chiave
                    invalid continue
                not invalid perform SCORRI-TORDINI
              end-start
           end-if.

           if tot-totali not = 0
              perform SCRIVI-TOTALIZZATORI-FINALI
           end-if.

           display "                           " upon stbrogc-handle
                                            at column 30,50
                                            at line   08,00.

      ***---
       SCORRI-TORDINI.
           perform until 1 = 2
              read tordini next 
                   at end  exit perform
              end-read

              if tor-anno > stbrogc-a-anno
                 exit perform
              end-if

LUBEXX        if StampaPrenotate
LUBEXX           if tor-anno-fattura not = 0 or
LUBEXX              tor-num-fattura  not = 0 or
LUBEXX              tor-data-fattura not = 0 or
LUBEXX              tor-num-prenot   not = 0 or
LUBEXX              tor-fatt-no-prenotata
LUBEXX              exit perform
LUBEXX           end-if
LUBEXX        end-if

              add 1 to counter
              add 1 to counter2
              if counter2 = 50
                 move counter to counter-edit
                 display counter-edit
                    upon stbrogc-handle at column 30,50
                                             line 08,00
                 move 0 to counter2
              end-if

              perform VALIDA-ORDINE

              if tutto-ok
                 perform CICLO-STAMPA
              end-if
           end-perform.

      ***---
       VALIDA-ORDINE.
           move 0 to como-imponibile
                      tot-imponibile
                     como-ivato
                      tot-ivato
                     como-valore.
           set tutto-ok to true.
           if  tor-numero < stbrogc-da-num or 
               tor-numero > stbrogc-a-num
               set errori to true
           else
LUBEXX*       |Scarto gli ordini già fatturati
LUBEXX        if tor-num-fattura = 0 or tor-data-fattura = 0 or
LUBEXX           stbrogc-si-fattu
LUBEXX           if tutto-ok
LUBEXX              evaluate true
LUBEXX              when stbrogc-solo-fm 
LUBEXX                   if tor-ordine 
LUBEXX                      set errori to true 
LUBEXX                   end-if
LUBEXX              when stbrogc-no-fm
LUBEXX                   if tor-fattura-manuale 
LUBEXX                      set errori to true 
LUBEXX                   end-if
LUBEXX              end-evaluate
LUBEXX           end-if

LUBEXX           if tutto-ok
LUBEXX              set NoteGiaScritte to false
                    perform RELAZIONE-TORDINI-CLIENTI
                    if tutto-ok
                       perform RELAZIONE-TORDINI-DESTINI
                       perform RELAZIONE-TORDINI-AGENTI
                       perform RELAZIONE-TORDINI-TCODPAG
                       perform RELAZIONE-TORDINI-TVETTORI
                       perform RELAZIONE-TORDINI-CAUSALI
                    end-if
LUBEXX           end-if
                    
LUBEXX        else
LUBEXX           set errori to true
              end-if
           end-if.  

      ***---
      * RELAZIONI DELLA TESTATA ORDINI (TORDINI)
      ***---
       RELAZIONE-TORDINI-CLIENTI.
           initialize cli-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.
           set tutto-ok     to true.
           set cli-tipo-C   to true.
           move tor-cod-cli to cli-codice.
           read clienti key cli-chiave
                invalid continue
            not invalid
                initialize tcl-rec replacing numeric data by zeroes
                                        alphanumeric data by spaces
                move cli-tipo to tcl-codice
                read ttipocli no lock invalid continue end-read
LUBEXX          if not StampaPrenotate
                   if ClienteGDO
                      if tcl-brogliaccio-NORMALE 
                         set errori to true 
                      end-if
      *****                if cli-gdo     = spaces set errori to true end-if
                   else         
      *****                if cli-gdo not = spaces set errori to true end-if
                      if tcl-brogliaccio-GDO
                         set errori to true 
                      end-if
                   end-if
LUBEXX          end-if
           end-read.

      ***---
       RELAZIONE-TORDINI-DESTINI.
           initialize des-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.
           set tutto-ok to true.
           if tor-prg-destino       not = 0
              move tor-cod-cli      to des-codice
              move tor-prg-destino  to des-prog
              read destini invalid continue end-read
           end-if.


      ***---
       RELAZIONE-TORDINI-AGENTI.
           set tutto-ok to true.
           initialize age-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.
           if tor-cod-agente not = 0                              
              move tor-cod-agente to age-codice
              read agenti invalid continue end-read
           end-if.

      ***---
       RELAZIONE-TORDINI-TCODPAG.
           set tutto-ok to true.
           initialize record-tblpa replacing numeric data by zeroes
                                        alphanumeric data by spaces.
           if tor-cod-pagamento not = spaces
              move "PA"              to tblpa-codice1
              move tor-cod-pagamento to tblpa-codice2
              move spaces to tblpa-descrizione1
              move spaces to tblpa-descrizione2
              read tcodpag no lock invalid continue end-read
           end-if.

      ***---
       RELAZIONE-TORDINI-TVETTORI.
           set tutto-ok to true.
           initialize vet-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.
           if tor-vettore not = 0                              
              move tor-vettore  to vet-codice
              read tvettori key vet-chiave invalid continue end-read
           end-if. 
             
      ***---
       RELAZIONE-TORDINI-CAUSALI.
           initialize tca-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.
           move tor-causale to tca-codice
           read tcaumag invalid continue end-read.

      ***---
      * RELAZIONI DELLE RIGHE DEGLI ORDINI (RORDINI)
      ***---
       RELAZIONE-RORDINI-ARTICOLI.
           move ror-cod-articolo to art-codice.
           
           read articoli key art-chiave invalid initialize art-rec
           end-read.

      ***---
       RELAZIONE-RORDINI-PROGMAG.
           move spaces           to prg-cod-magazzino.
           move spaces           to prg-tipo-imballo.
           move 0                to prg-peso.
           move ror-cod-articolo to prg-cod-articolo.

           read progmag key prg-chiave
                invalid move 0 to prg-costo-ultimo
           end-read.

      ***---
       RELAZIONE-RORDINI-TMOVMAG.
           |TENTATIVO CON DESTINO VALORIZZATO
           set trovato-ultimo-prezzo to false.
           initialize r-data-ord r-ult-prz-ord.
           set  tmo-cliente     to true.
           move tor-cod-cli     to tmo-cod-clifor.
           move tor-prg-destino to tmo-destino.
           move high-value   to tmo-data-movim.
           start tmovmag key is <= k2
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tmovmag previous 
                         at end exit perform 
                    end-read
                    if tor-cod-cli not = tmo-cod-clifor
                       exit perform
                    end-if
LUBEXX              if tor-causale = tmo-causale
                       if tmo-anno-fattura  not = 0 and
                          tmo-data-fattura  not = 0
                          perform TROVA-RMOVMAG
                          if trovato-ultimo-prezzo 
                             exit perform 
                          end-if
                       end-if
LUBEXX              end-if                        
                 end-perform
           end-start.

           |TENTATIVO CON DESTINO GENERICO
           if not trovato-ultimo-prezzo
              initialize r-data-ord r-ult-prz-ord
              set  tmo-cliente  to true
              move tor-cod-cli  to tmo-cod-clifor
              move high-value   to tmo-destino
              move high-value   to tmo-data-movim
              start tmovmag key is <= k2
                    invalid continue
                not invalid
                    perform until 1 = 2
                       read tmovmag previous 
                            at end exit perform 
                       end-read
                       if tor-cod-cli not = tmo-cod-clifor
                          exit perform
                       end-if
LUBEXX                 if tor-causale = tmo-causale
                          if tmo-anno-fattura  not = 0 and
                             tmo-data-fattura  not = 0
                             perform TROVA-RMOVMAG
                             if trovato-ultimo-prezzo 
                                exit perform 
                             end-if
                          end-if
LUBEXX                 end-if                        
                    end-perform
              end-start
           end-if.

      ***---
       RELAZIONE-RORDINI-IVA.
           move 0           to tbliv-percentuale.
           move "IV"        to tbliv-codice1.
           move ror-cod-iva to tbliv-codice2.
           read tivaese invalid continue end-read.

      ***---
       TROVA-RMOVMAG.
           initialize rmo-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.
           move tmo-anno           to rmo-anno.
           move tmo-numero         to rmo-movim.
           move ror-cod-articolo   to rmo-articolo.
           read rmovmag key is k-articolo
                invalid set trovato-ultimo-prezzo to false
            not invalid 
LUBEXX          |Escludo i movimenti rettifiche di prezzo
LUBEXX          if rmo-qta not = 0
LUBEXX             set trovato-ultimo-prezzo to true
LUBEXX          end-if
           end-read.

      ***---
       CICLO-STAMPA.
           move 0 to totale differenza tassato.
           if tor-ordine perform STAMPA-INTESTAZIONE-ORD
           else          perform STAMPA-INTESTAZIONE-FM
           end-if.

           move tor-anno   to ror-anno.
           move tor-numero to ror-num-ordine.
           move low-values to ror-num-riga.

           start rordini key >= ror-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rordini next
                      at end
                         if tor-ordine perform STAMPA-PIEDE-ORD
                         else          perform STAMPA-PIEDE-FM
                         end-if
                         exit perform
                    end-read

                   if ror-anno        not = tor-anno or
                      ror-num-ordine  not = tor-numero

                      if tor-ordine perform STAMPA-PIEDE-ORD
                      else          perform STAMPA-PIEDE-FM
                      end-if

                      exit perform
                   end-if
                   
                   if tor-ordine perform RELAZIONE-RORDINI-TMOVMAG
                   else          perform RELAZIONE-RORDINI-IVA
                   end-if

                   perform RELAZIONE-RORDINI-ARTICOLI
                   perform RELAZIONE-RORDINI-PROGMAG

                   if tor-ordine perform STAMPA-CORPO-ORD
                   else          perform STAMPA-CORPO-FM
                   end-if

                   compute totale = totale       +
                                  ( ror-prg-peso * ror-qta )
                end-perform
           end-start.

      ***---
       STAMPA-INTESTAZIONE-ORD.
           if no-dati set no-dati to false end-if.
           initialize t1-cau-descrizione
                      t1-numero-ord
                      t1-data-ord
                      t1-bolla-ord 
                      t1-datbolla-ord
                      t1-fattura-ord
                      t1-datfattura-ord
                      t1-num-ord-cli
                      t2-cod-cli-ord
                      t2-ragsoc-ord
                      t2-cod-agente-ord
                      t2-des-agente-ord
                      t3-indirizzo-ord
                      t3-cod-pag-ord
                      t3-des-pag-ord
                      t4-cap-ord
                      t4-localita-ord
                      t4-prov-ord
                      t4-cod-vettore-ord
                      t4-des-vettore-ord
                      t5-cod-destino
                      t5-destino-ord
                      t5-indirizzo-ord
                      t6-localita-ord
                      t6-prov-ord
                      t7-note-1
                      t7-note-data
                      t7-note-2
                      t7-note-3
                      t7-note-4.

      *     Adriano - Stampa descrizione causale
           perform RELAZIONE-TORDINI-CAUSALI
           move tca-descrizione to t1-cau-descrizione
      *     Adriano - Fine

           move   tor-numero   to t1-numero-ord.
           call "C$JUSTIFY" using t1-numero-ord, "L".
           string tor-data-ordine(7:2) delimited by size
                  "/"                  delimited by size
                  tor-data-ordine(5:2) delimited by size
                  "/"                  delimited by size
                  tor-data-ordine(3:2) delimited by size
                  into t1-data-ord
           end-string.
           move tor-num-bolla  to t1-bolla-ord.
           call "C$JUSTIFY" using t1-bolla-ord, "L".
           string tor-data-bolla(7:2) delimited by size
                  "/"                 delimited by size
                  tor-data-bolla(5:2) delimited by size
                  "/"                 delimited by size
                  tor-data-bolla(3:2) delimited by size
                  into t1-datbolla-ord
           end-string.
           move tor-num-fattura to t1-fattura-ord.
           call "C$JUSTIFY"  using t1-fattura-ord, "L".
           string tor-data-fattura(7:2) delimited by size
                  "/"                   delimited by size
                  tor-data-fattura(5:2) delimited by size
                  "/"                   delimited by size
                  tor-data-fattura(3:2) delimited by size
                  into t1-datfattura-ord
           end-string.

           if tor-prg-destino = zero
              move cli-superamento-500  to des-superamento-500
           end-if
      *     if cli-superamento-500 = "S"
      *        move "SI" to t1-blocco-500
      *     else
              if des-superamento-500 = "S"
                 move "SI" to t1-blocco-500
              else
                 move "NO" to t1-blocco-500
              end-if
      *     end-if.
           initialize line-riga.

           move tor-num-ord-cli to t1-num-ord-cli.
           move testata-1-ord   to line-riga.
           perform STAMPA-RIGA-T. 

           move tor-cod-cli    to t2-cod-cli-ord.
           call "C$JUSTIFY" using t2-cod-cli-ord, "L".
           move cli-ragsoc-1   to t2-ragsoc-ord.
           move tor-cod-agente to t2-cod-agente-ord.
           move age-ragsoc-1   to t2-des-agente-ord.
           initialize line-riga.
           move testata-2-ord to line-riga.
           perform STAMPA-RIGA.

           move cli-indirizzo     to t3-indirizzo-ord.
           move tor-cod-pagamento to t3-cod-pag-ord. 
           initialize t3-des-pag-ord.
           inspect tblpa-descrizione1 replacing trailing 
                                         spaces by low-value
           string  tblpa-descrizione1 delimited by low-value
                   " "                delimited by size
                   tblpa-descrizione2 delimited by size
                   into t3-des-pag-ord
           end-string.
           initialize line-riga.
           move testata-3-ord to line-riga.
           perform STAMPA-RIGA.

           move cli-cap         to t4-cap-ord.
           move cli-localita    to t4-localita-ord.
           move cli-prov        to t4-prov-ord.
           move tor-vettore     to t4-cod-vettore-ord.
           move vet-descrizione to t4-des-vettore-ord.
           initialize line-riga.
           move testata-4-ord to line-riga.
           perform STAMPA-RIGA.           
           
           if des-prog not = 0
              move des-prog      to t5-cod-destino
           end-if
           move des-ragsoc-1  to t5-destino-ord.
           move des-indirizzo to t5-indirizzo-ord.
           initialize line-riga.
           move testata-5-ord to line-riga.
           perform STAMPA-RIGA.

           move des-localita to t6-localita-ord.
           move des-prov     to t6-prov-ord.
           initialize line-riga.
           move testata-6-ord to line-riga.
           perform STAMPA-RIGA.

           if tor-data-note1 not = 0
              string tor-data-note1(7:2) delimited by size
                     "/"                 delimited by size
                     tor-data-note1(5:2) delimited by size
                     "/"                 delimited by size
                     tor-data-note1(3:2) delimited by size
                     into t7-note-data
              end-string
           end-if.
           move tor-note1 to t7-note-1.
           move tor-note2 to t7-note-2.
           move tor-note3 to t7-note-3.
           move tor-note4 to t7-note-4.
           move testata-7-ord to line-riga.
           perform STAMPA-RIGA.

           move spaces to line-riga.
           perform STAMPA-RIGA.

           initialize line-riga.
           move testata-fissa-ord to line-riga.
           perform STAMPA-RIGA.

           initialize line-riga.
           move divisorio to line-riga.
           perform STAMPA-RIGA.

      ***---                                                            
       STAMPA-CORPO-ORD.
           initialize r-riga-ord 
                      r-articolo-ord
                      r-peso-ord
                      r-cod-art-cli-ord
                      r-colli-ord
                      r-imballo-ord
                      r-des-articolo-ord
                      r-qta-ord
                      r-netto-ord
                      r-netto-ord-x
                      r-pz-agente
                      r-cod-iva
                      r-data-ord
                      r-ult-prz-ord
                      r-sottocosto-ord
                      r-costo-ultimo-ord.
           move ror-num-riga     to r-riga-ord.
           move ror-cod-articolo to r-articolo-ord.
           move r-articolo-ord   to r-articolo-ord.
           move ror-prg-peso     to r-peso-ord.
      *****     if cli-gdo not = spaces
           if tcl-gdo-si or tcl-gdo-opz
              perform TROVA-CODICE-ARTICOLO-ON-LISTINI
           else
              move spaces to como-lst-cod-art-cli
           end-if.
           if como-lst-cod-art-cli = spaces
              perform TROVA-CODICE-ARTICOLO-ON-ASSORCLI
              move asc-cod-articolo-per-cliente to r-cod-art-cli-ord
           else
              move como-lst-cod-art-cli         to r-cod-art-cli-ord
           end-if.
           move ror-num-colli    to r-colli-ord.
LUBEXX     move ror-cod-iva      to r-cod-iva.
           call "C$JUSTIFY"   using r-colli-ord, "R".
BLISTR     if ror-si-blister
BLISTR        move ror-des-imballo to r-imballo-ord
           else
              inspect ror-des-imballo replacing trailing spaces 
                                             by low-value
              move ror-qta-imballi to imballi-ed
              call "C$JUSTIFY"  using imballi-ed, "L"
              initialize r-imballo-ord
              string  ror-des-imballo delimited by low-value
                      " da "          delimited by size
                      imballi-ed      delimited by spaces
                      " x "           delimited by size
                      art-udm-imballo delimited by size
                      into r-imballo-ord
              end-string
           end-if.
           move art-descrizione  to r-des-articolo-ord.
OMAGGI     compute como-valore = ror-qta - ror-qta-omaggi.
OMAGGI     move como-valore      to r-qta-ord.
           call "C$JUSTIFY"   using r-qta-ord, "R".
           if ror-si-omaggio
              move "OMAGGIO"      to r-netto-ord-x
              call "C$JUSTIFY" using r-netto-ord-x, "R"
           else             
              move 0 to tot-imposte
              if ttipocli-gdo
                 compute como-valore = 
                         ror-imponib-merce + 
                         ror-imp-consumo   +
                         ror-imp-cou-cobat +
                         ror-add-piombo
              else
                 move ror-imponib-merce to como-valore

                 compute tot-imposte = 
                         ror-imp-consumo   +
                         ror-imp-cou-cobat +
                         ror-add-piombo
              end-if
              move como-valore to r-netto-ord
              move r-netto-ord to r-netto-ord-x
           end-if.
      *****     call "C$JUSTIFY"   using r-netto-ord, "R".
           if art-prezzo-vendita > ror-prz-unitario
              compute differenza = 
              ( art-prezzo-vendita - ror-prz-unitario )
              compute ror-perce-sconto =
              ( differenza * art-prezzo-vendita ) / 100
      *****        move ror-perce-sconto to r-sconto-ord
           else
              move 0 to ror-perce-sconto
           end-if.
                         
           move tor-cod-cli     to como-prm-cliente.
           move tor-prg-destino to como-prm-destino.

      *    Adriano - Prezzo agente
           perform RELAZIONE-TORDINI-AGENTI.
           perform RECUPERA-PRZ-LISTINO.
           if prezzo-listino > ( ror-imp-consumo + ror-imp-cou-cobat )
              compute prezzo-listino  =
                      prezzo-listino  -
                      ror-imp-consumo -
                      ror-imp-cou-cobat
           else
              move 0 to prezzo-listino
           end-if.
           move prezzo-listino to r-pz-agente.

      *    Adriano - Fine
           if trovato-ultimo-prezzo
              string tmo-data-movim(7:2) delimited size
                     "/"                 delimited size
                     tmo-data-movim(5:2) delimited size
                     "/"                 delimited size
                     tmo-data-movim(3:2) delimited size
                     into r-data-ord
              end-string
              if ttipocli-gdo
                 compute rmo-netto = rmo-netto  +
                                     rmo-coubat +
                                     rmo-imp-cons
              end-if
              move rmo-netto to r-ult-prz-ord
           end-if.
           call "C$JUSTIFY"   using r-ult-prz-ord, "R".  
                              
           if tot-imposte not = 0
              if prg-costo-ultimo > tot-imposte
                 subtract tot-imposte from prg-costo-ultimo
                   giving prg-costo-ultimo
              else
                 move 0 to prg-costo-ultimo
              end-if
           end-if.

           if prg-costo-ultimo > ror-prz-unitario
              move "**"             to r-sottocosto-ord
              move prg-costo-ultimo to r-costo-ultimo-ord
              call "C$JUSTIFY"   using r-costo-ultimo-ord, "R"
           end-if.
           initialize line-riga.

           move riga1-ord to line-riga.
           move 1 to sw-corpo.
           perform STAMPA-RIGA.
           move 0 to sw-corpo.

OMAGGI     if ror-qta-omaggi not = 0
OMAGGI        move 0                to r-articolo-ord
OMAGGI        move 0                to r-peso-ord
OMAGGI        move spaces           to r-cod-art-cli-ord
OMAGGI        move 0                to r-colli-ord
OMAGGI        move spaces           to r-imballo-ord
OMAGGI        move ror-qta-omaggi   to r-qta-ord
OMAGGI        move "OMAGGIO"        to r-netto-ord-x
OMAGGI        call "C$JUSTIFY"   using r-netto-ord-x, "R"
OMAGGI        move 0                to r-pz-agente
OMAGGI        move 0                to r-ult-prz-ord
OMAGGI        move spaces           to r-sottocosto-ord
OMAGGI        move 0                to r-costo-ultimo-ord
OMAGGI        move riga1-ord        to line-riga
OMAGGI        move 1                to sw-corpo 
OMAGGI        move tge-cod-iva-omag to r-cod-iva
OMAGGI        move riga1-ord        to line-riga
OMAGGI        perform STAMPA-RIGA
OMAGGI        move 0                to sw-corpo
OMAGGI     end-if.

      ***---
       STAMPA-PIEDE-ORD.
           initialize tot-peso-ord tot-tassato-ord tot-diff-ord.
           move spaces to line-riga.
           perform STAMPA-RIGA.
           move totale to como-arrot tassato peso.
           if como-arrot not = 0
             if como-arrot > 50
                add 100 to tassato
                move 00 to tassato(5:2)
             else
                move 50 to tassato(5:2)
             end-if
           end-if.
           move 0 to tassato(7:3).
           move tassato        to tot-tassato-ord.
           move peso           to tot-peso-ord.
           subtract tassato  from peso giving differenza.
           move differenza     to tot-diff-ord.
           call "C$JUSTIFY" using tot-peso-ord,    "R".
           call "C$JUSTIFY" using tot-diff-ord,    "R".
           call "C$JUSTIFY" using tot-tassato-ord, "R".
           compute tot-totali = tot-totali + totale
           compute tot-qli    = tot-totali / 100
           initialize line-riga.
           move riga-totali-ord to line-riga.
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
       SCRIVI-TOTALIZZATORI-FINALI.
           initialize tot-data-ord.
           string como-data(7:2) delimited by size
                  "/"            delimited by size
                  como-data(5:2) delimited by size
                  "/"            delimited by size
                  como-data(1:4) delimited by size
                  into tot-data-ord
           end-string.

           initialize tot-ora-ord.
           string como-ora(1:2) delimited by size
                  ":"           delimited by size
                  como-ora(3:2) delimited by size
                  into tot-ora-ord
           end-string.

LUBEXX     if StampaPrenotate
LUBEXX        move "TOTALE vend prenotate q.li:" to tot-descr-ord
LUBEXX     else
              if ClienteGDO 
                 move "TOTALE vendite G.D.O. q.li:" to tot-descr-ord
              else
                 move "TOTALE vendite ALTRE q.li:"  to tot-descr-ord
LUBEXX        end-if
           end-if.

           move tot-qli    to tot-qli-ed-ord.

           move spaces to line-riga.
           perform STAMPA-RIGA.
           move spaces to line-riga.
           perform STAMPA-RIGA.
           move spaces to line-riga.
           perform STAMPA-RIGA.
           move spaces to line-riga.
           perform STAMPA-RIGA.

           initialize line-riga.
           move divisorio-finale to line-riga.
           perform STAMPA-RIGA-T.

           move spaces to line-riga.
           perform STAMPA-RIGA.

           initialize line-riga.
           move totalizzatore-finale-ord to line-riga.
           perform STAMPA-RIGA.

           move spaces to line-riga.
           perform STAMPA-RIGA.

           initialize line-riga.
           move divisorio-finale to line-riga.
           perform STAMPA-RIGA.

      ***---
       STAMPA-INTESTAZIONE-FM.
           if no-dati set no-dati to false end-if.
           initialize t1-tipodoc-fm
                      t1-numero-fm
                      t1-cod-cli-fm
                      t1-ragsoc-fm
                      t2-indirizzo-fm
                      t3-cap-fm
                      t3-localita-fm
                      t3-cod-pag-fm
                      t4-prg-destino-fm
                      t4-ragsoc-fm
                      t4-indirizzo-fm
                      t4-localita-fm.


           move   tor-numero   to t1-numero-fm.
           call "C$JUSTIFY" using t1-numero-fm, "L".
           move tca-descrizione to t1-tipodoc-fm.
           move tor-cod-cli to t1-cod-cli-fm.
           initialize t1-ragsoc-fm.
           inspect cli-ragsoc-1 replacing trailing spaces by low-value.
           string  cli-ragsoc-1 delimited by low-value
                   " "          delimited by size
                   cli-ragsoc-2 delimited by size
                   into t1-ragsoc-fm
           end-string.
           move tor-cod-cli    to t1-cod-cli-fm.
           call "C$JUSTIFY" using t1-cod-cli-fm, "L".
           move cli-ragsoc-1   to t1-ragsoc-fm.
           initialize line-riga.
           move testata-1-fm to line-riga.
           perform STAMPA-RIGA.

           move cli-indirizzo to t2-indirizzo-fm.
           initialize line-riga.
           move testata-2-fm to line-riga.
           perform STAMPA-RIGA.

           move cli-cap           to t3-cap-fm.
           move cli-localita      to t3-localita-fm.
           move tor-cod-pagamento to t3-cod-pag-fm.
           initialize line-riga.
           move testata-3-fm to line-riga.
           perform STAMPA-RIGA.

           move tor-prg-destino to t4-prg-destino-fm.
           move des-ragsoc-1    to t4-ragsoc-fm.
           move des-indirizzo   to t4-indirizzo-fm.
           move des-localita    to t4-localita-fm.
           initialize line-riga.
           move testata-4-fm to line-riga.
           perform STAMPA-RIGA.

           move spaces to line-riga.
           perform STAMPA-RIGA.

           evaluate tor-causale
           when "FTMA" 
                move spaces             to titolo-codice-fm
                move "Descrizione Voce" to titolo-articolo-fm
                move spaces             to titolo-qta-fm
           when "FTTR"
           when "FTND" 
                move "Codice"    to titolo-codice-fm
                move "Articolo"  to titolo-articolo-fm
                move "Quantita'" to titolo-qta-fm
           end-evaluate.
           
           call "C$JUSTIFY" using titolo-articolo-fm, "C".
           initialize line-riga.
           move testata-fissa-fm to line-riga.
           perform STAMPA-RIGA.

           initialize line-riga.
           move divisorio to line-riga.
           perform STAMPA-RIGA.

      ***---
       STAMPA-CORPO-FM.
           if not NoteGiaScritte perform SCRIVI-NOTE-FM end-if.
           initialize r-riga-fm
                      r-articolo-fm
                      r-des-articolo-fm
                      r-qta-fm
                      r-netto-fm
                      r-imposta-fm
                      r-cou-fm
                      r-valore-fm
                      r-iva-fm.
           move ror-num-riga      to r-riga-fm.
           move ror-cod-articolo  to r-articolo-fm.
           move art-descrizione   to r-des-articolo-fm.
           move ror-qta           to r-qta-fm.
           call "C$JUSTIFY"    using r-qta-fm, "R".
           move ror-prz-unitario  to r-netto-fm.
           call "C$JUSTIFY"    using r-netto-fm, "R".
           move ror-imp-consumo   to r-imposta-fm.
           call "C$JUSTIFY"    using r-imposta-fm, "R".
           move ror-imp-cou-cobat to r-cou-fm.
           call "C$JUSTIFY"    using r-cou-fm, "R".
           move ror-add-piombo    to r-piombo-fm.
           call "C$JUSTIFY"    using r-piombo-fm, "R".

           evaluate tor-causale
           when "FTMA"
                move 1              to ror-qta
                move ror-des-libera to r-des-articolo-fm
           end-evaluate.

           compute como-valore =
                 ( ror-prz-unitario  + 
                   ror-imp-consumo   +
OMAGGI             ror-imp-cou-cobat +
                   ror-add-piombo )  * ( ror-qta - ror-qta-omaggi ).
           move como-valore       to r-valore-fm.
           call "C$JUSTIFY"    using r-valore-fm, "R".
           move tbliv-percentuale to r-iva-fm.
LUBEXX     move ror-cod-iva       to r-cod-iva-fm.
           initialize line-riga.
           move riga1-fm to line-riga.
           perform STAMPA-RIGA.

           compute como-imponibile = como-imponibile + como-valore.

           compute como-ivato      = como-ivato +
                   ( como-valore + 
                 ( ( como-valore * tbliv-percentuale ) / 100) ).

      ***---
       STAMPA-PIEDE-FM.
           move tor-contropartita to tot-controp-fm.
           move 0 to tot-imponibile tot-ivato.
           compute tot-imponibile = como-imponibile + 0,005.
           compute tot-ivato      = como-ivato      + 0,005.
           move tot-imponibile to tot-imp-edit-fm.
           move tot-ivato      to tot-iva-edit-fm.
           call "C$JUSTIFY" using tot-imponibile, "R".
           call "C$JUSTIFY" using tot-ivato,      "R".

           move spaces to line-riga.
           perform STAMPA-RIGA.
           move spaces to line-riga.
           perform STAMPA-RIGA.
           
           initialize line-riga.
           move riga-totali-fm to line-riga.
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
       SCRIVI-NOTE-FM.
           set NoteGiaScritte to true.
           initialize intestazioni-note.

           move 0      to idx
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

              inspect tor-note(cont-inizio:cont-per) tallying cont-char
                       for all x"0D"
              if cont-char = 0
                 |move 50  to cont-per
                 continue
              else
                 initialize cont-per
                 inspect tor-note(cont-inizio:50) tallying cont-per
                       for characters before x"0D"
              end-if
              if cont-per not = zero
                 move tor-note(cont-inizio:cont-per) 
                                         to intestazione-note(idx)
      *    se appena dopo i 50 caratteri premo invio devo ignorarlo
                 if cont-per = 50
                    add cont-per  to cont-inizio
                    if cont-inizio < 499
                             and tor-note(cont-inizio:1) = x"0D"
                       add 2 to cont-inizio
                    end-if
                    subtract cont-per from cont-inizio
                 end-if
              else
                 move space  to intestazione-note(idx)
              end-if
              if cont-char = 0
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
              initialize riga-note-fm line-riga
              move intestazione-note(idx) to r-riga-note-fm
              initialize line-riga
              move riga-note-fm to line-riga
              perform STAMPA-RIGA
           end-perform.

      ***---
       SALTO-PAGINA.
           compute diff-righe = max-righe - num-righe.
           move diff-righe to n-vuote.
           perform RIGHE-VUOTE.
           move 0 to num-righe.

      ***---
       RIGHE-VUOTE.
           perform n-vuote times
              write line-riga from spaces
           end-perform
           add n-vuote to num-righe. 

      ***---
       STAMPA-RIGA.
           initialize sav-riga
           move line-riga to sav-riga
           if num-righe > max-righe - 5
              perform SALTO-PAGINA
              if sw-corpo = 1
                 initialize line-riga
                 write line-riga from testata-fissa-ord
                 initialize line-riga
                 write line-riga from divisorio
                 add 2 to num-righe
              end-if
           end-if
           move sav-riga to line-riga
           write line-riga
           add 1 to num-righe. 

      ***---
       STAMPA-RIGA-T.
           initialize sav-riga.
           move line-riga to sav-riga.
           if num-righe > max-righe - 10
              perform SALTO-PAGINA
           end-if.
           move sav-riga to line-riga.
           write line-riga.
           add 1 to num-righe.

      ***---
       RESET-BUFFER-PRINTER.
           |inizializza il buffer dei settaggi della stampante
           move ESC-ESC to line-riga.
           write line-riga after 0.

      ***---
       PARAGRAFO-COPY.
           copy "trova-su-assorcli.cpy".
           copy "recupera-prz-listino.cpy".
           copy "trova-su-listini.cpy".
           copy "trova-parametro.cpy".
