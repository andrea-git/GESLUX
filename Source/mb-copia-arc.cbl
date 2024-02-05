       IDENTIFICATION DIVISION.
       PROGRAM-ID.         mb-copia-arc.
       REMARKS. Lanciato ad ogni macrobatch.

       SPECIAL-NAMES. decimal-point is comma.

      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.          
           copy "mtordini.sl".
       SELECT mtordini-c
           ASSIGN       TO path-mtordini-c
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL
           FILE STATUS  IS STATUS-mtordini-c
           RECORD KEY   IS cmto-chiave
           ALTERNATE RECORD KEY IS cmto-k-ord-cli = cmto-anno, 
           cmto-num-ord-cli
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS cmto-k-data = cmto-data-ordine, 
           cmto-chiave
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS cmto-k-clides = cmto-cod-cli, 
           cmto-prg-destino, cmto-data-ordine
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS cmto-k-age = cmto-cod-agente, 
           cmto-data-ordine
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-cmto-stato-sel = cmto-stato-ordine, 
           cmto-cod-cli, cmto-data-ordine
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-cmto-stato = cmto-stato-ordine, 
           cmto-chiave
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS cmto-k-gdo = cmto-gdo, 
           cmto-data-ordine, cmto-chiave
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS cmto-k-bloc = cmto-stato-attivazione, 
           cmto-data-ordine, cmto-cod-cli, cmto-prg-destino
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-giang = cmto-data-note1, 
           cmto-chiave
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS cmto-k-promo = cmto-promo, 
           cmto-tipo-CF, cmto-cod-cli, cmto-prg-destino, cmto-chiave
           WITH DUPLICATES .

           copy "mrordini.sl".
       SELECT mrordini-c
           ASSIGN       TO path-mrordini-c
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL WITH ROLLBACK 
           FILE STATUS  IS STATUS-mrordini-c
           RECORD KEY   IS cmro-chiave
           ALTERNATE RECORD KEY IS cmro-k-promo = cmro-promo, 
           cmro-chiave
           ALTERNATE RECORD KEY IS cmro-k-articolo = cmro-cod-articolo, 
           cmro-chiave
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS cmro-k-progr = cmro-chiave-testa, 
           cmro-progr
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS cmro-k-tprev = cmro-promo, 
           cmro-prg-cod-articolo, cmro-chiave
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS cmro-k-ord-art = cmro-chiave-testa, 
           cmro-cod-articolo
           WITH DUPLICATES .

           copy "tordini.sl".
       SELECT tordini-c
           ASSIGN       TO path-tordini-c
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL
           FILE STATUS  IS STATUS-tordini-c
           RECORD KEY   IS ctor-chiave
           ALTERNATE RECORD KEY IS k-causale = ctor-causale, ctor-anno, 
           ctor-numero
           ALTERNATE RECORD KEY IS k1 = ctor-cod-cli, ctor-prg-destino, 
           ctor-anno, ctor-numero
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k2 = ctor-data-passaggio-ordine, 
           ctor-anno, ctor-numero
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-bolla = ctor-anno-bolla, 
           ctor-num-bolla
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k3 = ctor-anno-bolla, 
           ctor-data-bolla, 
           ctor-num-bolla, ctor-bolla-prenotata
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-fattura = ctor-anno-fattura, 
           ctor-num-fattura
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k4 = ctor-anno-fattura, 
           ctor-data-fattura, ctor-num-fattura, ctor-num-prenot, 
           ctor-fatt-prenotata
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-contab = ctor-agg-contab, 
           ctor-anno-fattura, ctor-num-fattura
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-tipo = ctor-tipo, ctor-chiave
           ALTERNATE RECORD KEY IS k-data = ctor-data-creazione, 
           ctor-numero
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-agfatt = ctor-anno-fattura, 
           ctor-data-fattura, ctor-num-fattura, ctor-num-prenot, 
           ctor-fatt-prenotata, ctor-chiave
           ALTERNATE RECORD KEY IS k-stbolle = ctor-anno-bolla, 
           ctor-data-bolla, ctor-num-bolla, ctor-bolla-prenotata, 
           ctor-chiave
           ALTERNATE RECORD KEY IS k-andamento-data = ctor-agg-contab, 
           ctor-data-fattura
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-andamento-cliente = ctor-cod-cli, 
           ctor-agg-contab, ctor-data-fattura
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-andamento-clides = ctor-cod-cli, 
           ctor-prg-destino, ctor-agg-contab, ctor-data-fattura
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-promo = ctor-stato, ctor-promo, 
           ctor-data-ordine, ctor-numero, ctor-cod-cli, ctor-prg-destino
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-or = ctor-cod-cli, 
           ctor-prg-destino, 
           ctor-num-ord-cli
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-ctor-inviare = ctor-da-inviare OF 
           tordini-c, ctor-chiave OF tordini-c
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-ctor-tipocli = ctor-tipocli OF 
           tordini-c, ctor-cod-cli OF tordini-c, 
           ctor-prg-destino OF tordini-c, 
           ctor-chiave OF tordini-c
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-ctor-gdo = ctor-gdo OF tordini-c, 
           ctor-cod-cli OF tordini-c, ctor-prg-destino OF tordini-c, 
           ctor-chiave OF tordini-c
           WITH DUPLICATES .

           copy "rordini.sl".
       SELECT rordini-c
           ASSIGN       TO path-rordini-c
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL WITH ROLLBACK 
           FILE STATUS  IS STATUS-rordini-c
           RECORD KEY   IS cror-chiave OF rordini-c
           ALTERNATE RECORD KEY IS cror-k-promo = cror-promo 
           OF rordini-c
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS cror-k-articolo = 
           cror-cod-articolo OF 
           rordini-c, cror-chiave OF rordini-c
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS cror-k-master = cror-chiave-ordine OF 
           rordini-c, cror-chiave OF rordini-c
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS cror-k-stbolle = 
           cror-anno OF rordini-c, 
           cror-num-ordine OF rordini-c, cror-chiave-ordine OF rordini-c
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS cror-k-ord-art = 
           cror-anno OF rordini-c, 
           cror-num-ordine OF rordini-c, cror-cod-articolo OF rordini-c
           WITH DUPLICATES .

           copy "tpromo.sl".
       SELECT tpromo-c
           ASSIGN       TO path-tpromo-c
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL WITH ROLLBACK 
           FILE STATUS  IS STATUS-tpromo-c
           RECORD KEY   IS ctpr-chiave OF tpromo-c
           ALTERNATE RECORD KEY IS ctpr-chiave-ricerca = 
           ctpr-chiave-ricerca OF tpromo-c
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS ctpr-chiave-gdo-fine = ctpr-gdo OF 
           tpromo-c, ctpr-fine-dpo OF tpromo-c, ctpr-ini-dpo OF tpromo-c
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS ctpr-chiave-fine = ctpr-fine-dpo OF 
           tpromo-c, ctpr-ini-dpo OF tpromo-c, ctpr-gdo OF tpromo-c
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS ctpr-chiave-ini = ctpr-ini-dpo OF 
           tpromo-c, ctpr-fine-dpo OF tpromo-c, ctpr-gdo OF tpromo-c
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS ctpr-chiave-volantino = 
           ctpr-ini-volantino OF tpromo-c, ctpr-fine-volantino 
           OF tpromo-c, 
           ctpr-gdo OF tpromo-c
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS ctpr-k-fine-vol = ctpr-fine-volantino 
           OF tpromo-c, ctpr-ini-volantino OF tpromo-c, ctpr-gdo 
           OF tpromo-c
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS ctpr-k-data-ins = ctpr-data-creazione 
           OF tpromo-c, ctpr-codice OF tpromo-c, ctpr-gdo OF tpromo-c
           WITH DUPLICATES .

           copy "rpromo.sl".
       SELECT rpromo-c
           ASSIGN       TO path-rpromo-c
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL WITH ROLLBACK 
           FILE STATUS  IS STATUS-rpromo-c
           RECORD KEY   IS crpr-chiave
           ALTERNATE RECORD KEY IS k-stampa = crpr-codice, 
           crpr-data-creazione, crpr-ora-creazione
           WITH DUPLICATES .
                                     
           copy "tagli.sl".
       SELECT tagli-c
           ASSIGN       TO path-tagli-c
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tagli-c
           RECORD KEY   IS ctag-chiave
           ALTERNATE RECORD KEY IS k2 = ctag-data
           WITH DUPLICATES .

           copy "EDI-mtordini.sl".
       SELECT EDI-mtordini-c
           ASSIGN       TO path-EDI-mtordini-c
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL
           FILE STATUS  IS STATUS-EDI-mtordini-c
           RECORD KEY   IS cemto-chiave
           ALTERNATE RECORD KEY IS cemto-k-ord-cli = cemto-anno, 
           cemto-num-ord-cli
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-cemto-stato = cemto-stato, 
           cemto-chiave
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-cemto-data = cemto-data-ordine, 
           cemto-chiave
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-cemto-clides = cemto-cod-cli, 
           cemto-prg-destino, cemto-chiave
           WITH DUPLICATES .       

           copy "EDI-mrordini.sl".
       SELECT EDI-mrordini-c
           ASSIGN       TO path-EDI-mrordini-c
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL
           FILE STATUS  IS STATUS-EDI-mrordini-c
           RECORD KEY   IS cemro-chiave
           ALTERNATE RECORD KEY IS cemro-k-articolo = 
           cemro-cod-articolo, 
           cemro-chiave
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS cemro-k-stato = cemro-stato, 
           cemro-chiave
           WITH DUPLICATES .

           copy "log-macrobatch.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.   
           copy "mtordini.fd".
       FD  mtordini-c.
       01 cmto-rec.
           05 cmto-chiave.
               10 cmto-anno         PIC  9(4).
               10 cmto-numero       PIC  9(8).
           05 cmto-dati.
               10 cmto-causale      PIC  x(4).
               10 cmto-tipo-CF      PIC  x.
               10 cmto-cod-cli      PIC  9(5).
               10 cmto-prg-destino  PIC  9(5).
               10 cmto-gdo          PIC  x(5).
               10 cmto-num-ord-cli  PIC  X(50).
               10 cmto-data-ordine  PIC  9(8).
               10 cmto-data-passaggio-ordine    PIC  9(8).
               10 cmto-cod-agente   PIC  9(5).
               10 cmto-cod-pagamento            PIC  x(3).
               10 cmto-cod-ese-iva  PIC  x(3).
               10 cmto-gest-plus    PIC  9(5).
               10 cmto-vettore      PIC  9(5).
               10 cmto-note1        PIC  X(19).
               10 cmto-data-note1   PIC  9(8).
               10 cmto-note2        PIC  X(30).
               10 cmto-note3        PIC  X(30).
               10 cmto-note4        PIC  X(30).
               10 cmto-note         PIC  X(500).
               10 cmto-pz-tot       PIC  9(8).
               10 cmto-pz-eva       PIC  9(8).
               10 cmto-ritira-in-lubex          PIC  9.
                   88 cmto-ritira-si VALUE IS 1. 
                   88 cmto-ritira-no VALUE IS 0. 
               10 cmto-promo        PIC  9.
                   88 cmto-si-promo VALUE IS 1. 
                   88 cmto-no-promo VALUE IS 0. 
               10 cmto-stato-attivazione        PIC  X(1).
                   88 cmto-attivo VALUE IS "A". 
                   88 cmto-bloccato VALUE IS "B". 
                   88 cmto-chiuso-man VALUE IS "C". 
               10 cmto-stato-ordine PIC  9.
                   88 cmto-registrato VALUE IS 1. 
                   88 cmto-in-lavorazione VALUE IS 2. 
                   88 cmto-sped-parz VALUE IS 3. 
                   88 cmto-sped-tot VALUE IS 4. 
      *****             88 cmto-fatt-parz VALUE IS 5. 
      *****             88 cmto-fatt-tot VALUE IS 6. 
      *
      *
                   88 cmto-chiuso VALUE IS 7. 
               10 cmto-dati-comuni.
                   15 cmto-data-creazione           PIC  9(8).
                   15 cmto-ora-creazione            PIC  9(8).
                   15 cmto-utente-creazione         PIC  X(10).
                   15 cmto-data-ultima-modifica     PIC  9(8).
                   15 cmto-ora-ultima-modifica      PIC  9(8).
                   15 cmto-utente-ultima-modifica   PIC  X(10).
               10 cmto-prenotazione-qta         PIC  9(1).
                   88 cmto-prenotazione-qta-si VALUE IS 1. 
                   88 cmto-prenotazione-qta-no VALUE IS 0. 
               10 cmto-causale-blocco           PIC  xx.
                   88 cmto-causale-blocco-prezzo VALUE IS "PR". 
                   88 cmto-causale-blocco-fido VALUE IS "FI". 
                   88 cmto-causale-blocco-manuale VALUE IS "MA". 
               10 cmto-saldi-banco  PIC  9(1).
                   88 cmto-saldi-banco-si VALUE IS 1. 
                   88 cmto-saldi-banco-no VALUE IS 0. 
               10 cmto-forn-reso    PIC  9(5).
               10 cmto-saldi-promo  PIC  9(1).
                   88 cmto-saldi-promo-si VALUE IS 1. 
                   88 cmto-saldi-promo-no VALUE IS 0. 
               10 cmto-immediato    PIC  9(1).
                   88 cmto-immediato-si VALUE IS 1. 
                   88 cmto-immediato-no VALUE IS 0. 
               10 cmto-vuoti.
      *(( XFD NAME = cmto-num-vuoto-3_1 ))
                   15 cmto-promo-fittizia           PIC  9(15).
                   15 cmto-ordine-EDI.
                       20 cmto-ordine-EDI-anno          PIC  9(4).
                       20 cmto-ordine-EDI-numero        PIC  9(8).
                   15 cmto-alfa-vuoto-1 PIC  X(5).
                   15 cmto-alfa-vuoto-2 PIC  X(20).
                   15 cmto-alfa-vuoto-3 PIC  X(20).
               10 cmto-ultima-evasione.
                   15 cmto-data-evasione            PIC  9(8).
                   15 cmto-ora-evasione PIC  9(8).
                   15 cmto-utente-evasione          PIC  x(15).
               10 cmto-note-bolla-1 PIC  x(500).
               10 cmto-note-bolla-2 PIC  x(500).
               10 cmto-urgente      PIC  9.
                   88 cmto-urgente-si VALUE IS 1. 
                   88 cmto-urgente-no VALUE IS 0. 
               10 cmto-note-libere  PIC  x(150).
               10 cmto-contrassegno PIC  x.
                   88 cmto-contrassegno-no VALUE IS "N" , " ". 
                   88 cmto-contrassegno-si VALUE IS "S". 
               10 FILLER           PIC  x(48).

           copy "mrordini.fd".
       FD  mrordini-c.
       01 cmro-rec.
           05 cmro-chiave.
               10 cmro-chiave-testa.
                   15 cmro-anno         PIC  9(4).
                   15 cmro-numero       PIC  9(8).
               10 cmro-riga         PIC  9(5).
           05 cmro-dati.
               10 cmro-cod-articolo PIC  9(6).
               10 cmro-qta          PIC  9(8).
               10 cmro-qta-e        PIC  9(8).
               10 cmro-qta-b        PIC  9(8).
               10 cmro-qta-f        PIC  9(8).
               10 cmro-qta-omaggi   PIC  9(8).
               10 cmro-prz-unitario PIC  9(9)v9(2).
               10 cmro-imp-consumo  PIC  9(4)v9(2).
               10 cmro-imp-cou-cobat            PIC  9(4)v9(2).
               10 cmro-add-piombo   PIC  9(4)v9(2).
               10 cmro-imponib-merce            PIC  9(9)v9(2).
               10 cmro-perce-sconto PIC  9(2)v9(2).
               10 cmro-omaggio      PIC  X(1).
                   88 cmro-si-omaggio VALUE IS "S". 
                   88 cmro-no-omaggio VALUE IS "N". 
               10 cmro-peso-utf     PIC  9(5)v9(3).
               10 cmro-peso-non-utf PIC  9(5)v9(3).
               10 cmro-num-colli    PIC  9(5).
               10 cmro-cod-imballo  PIC  X(3).
               10 cmro-des-imballo  PIC  X(50).
               10 cmro-qta-imballi  PIC  9(4).
               10 cmro-cod-art-cli  PIC  X(15).
               10 cmro-cod-iva      PIC  x(3).
               10 cmro-prz-commle   PIC  9(9)v9(2).
               10 cmro-prg-chiave.
                   15 cmro-prg-cod-articolo         PIC  9(6).
                   15 cmro-prg-cod-magazzino        PIC  X(3).
                   15 cmro-prg-tipo-imballo         PIC  X(3).
                   15 cmro-prg-peso     PIC  9(5)v9(3).
               10 cmro-dati-blister.
                   15 cmro-bli-codice   PIC  9(6).
                   15 cmro-bli-qta      PIC  9(8).
                   15 cmro-bli-perce    PIC  9(3)v99.
                   15 cmro-blister      PIC  9.
                       88 cmro-si-blister VALUE IS 1    WHEN SET TO 
           FALSE  0. 
               10 cmro-promo        PIC  9(15).
               10 cmro-flag-cancellato          PIC  9.
                   88 cmro-cancellato VALUE IS 1    WHEN SET TO FALSE  
           0. 
               10 cmro-prz-promo    PIC  9.
                   88 cmro-si-prz-promo VALUE IS 1. 
                   88 cmro-no-prz-promo VALUE IS 0. 
               10 cmro-progr        PIC  9(5).
               10 cmro-evadi-dal    PIC  9(8).
               10 cmro-dati-comuni.
                   15 cmro-data-creazione           PIC  9(8).
                   15 cmro-ora-creazione            PIC  9(8).
                   15 cmro-utente-creazione         PIC  X(10).
                   15 cmro-data-ultima-modifica     PIC  9(8).
                   15 cmro-ora-ultima-modifica      PIC  9(8).
                   15 cmro-utente-ultima-modifica   PIC  X(10).
               10 cmro-vuoti.
                   15 cmro-stato        PIC  9(2).
                       88 cmro-registrato VALUE IS 1. 
                       88 cmro-in-lavorazione VALUE IS 2. 
                       88 cmro-sped-parz VALUE IS 3. 
                       88 cmro-sped-tot VALUE IS 4. 
      *****             88 mto-fatt-parz VALUE IS 5. 
      *****             88 mto-fatt-tot VALUE IS 6. 
      *
                       88 cmro-chiuso VALUE IS 7. 
                   15 cmro-prz-manuale  PIC  9.
                       88 cmro-prz-manuale-si VALUE IS 1. 
                       88 cmro-prz-manuale-no VALUE IS 0. 
                   15 cmro-num-vuoto-1  PIC  9(15).
                   15 cmro-num-vuoto-2  PIC  9(18).
                   15 cmro-num-vuoto-3  PIC  9(18).
                   15 ex-cmro-giang     PIC  x.
                       88 ex-cmro-attesa VALUE IS " ". 
                       88 ex-cmro-tagliare-merce VALUE IS "M". 
                       88 ex-cmro-tenere-saldo VALUE IS "T". 
                       88 ex-cmro-sostituzione VALUE IS "S". 
                   15 cmro-bloccato-prezzo          PIC  9.
                       88 cmro-bloccato-prezzo-si VALUE IS 1. 
                       88 cmro-bloccato-prezzo-no VALUE IS 0. 
                   15 cmro-alfa-vuoto   PIC  X(498).

           copy "tordini.fd".
       FD  tordini-c.
       01 ctor-rec.
           05 ctor-chiave.
               10 ctor-anno         PIC  9(4).
               10 ctor-numero       PIC  9(8).
           05 ctor-dati.
               10 ctor-causale      PIC  x(4).
               10 ctor-cod-cli      PIC  9(5).
               10 ctor-prg-destino  PIC  9(5).
               10 ctor-num-ord-cli  PIC  X(50).
               10 ctor-data-ordine  PIC  9(8).
               10 ctor-data-passaggio-ordine    PIC  9(8).
               10 ctor-cod-agente   PIC  9(5).
               10 ctor-cod-pagamento            PIC  x(3).
               10 ctor-cod-ese-iva  PIC  x(3).
               10 ctor-spostam-ric-ago          PIC  X(1).
                   88 ctor-si-ric-ago VALUE IS "S". 
                   88 ctor-no-ric-ago VALUE IS "N". 
               10 ctor-spostam-ric-dic          PIC  X(1).
                   88 ctor-si-ric-dic VALUE IS "S". 
                   88 ctor-no-ric-dic VALUE IS "N". 
               10 ctor-vettore      PIC  9(5).
               10 ctor-note1        PIC  X(19).
               10 ctor-data-note1   PIC  9(8).
               10 ctor-note2        PIC  X(30).
               10 ctor-note3        PIC  X(30).
               10 ctor-note4        PIC  X(30).
               10 ctor-invio        PIC  x.
                   88 ctor-invio-manuale VALUE IS "M". 
                   88 ctor-invio-postel VALUE IS "P". 
                   88 ctor-invio-edi VALUE IS "E". 
               10 ctor-bolla.
                   15 ctor-anno-bolla   PIC  9(4).
                   15 ctor-num-bolla    PIC  9(8).
                   15 ctor-data-bolla   PIC  9(8).
                   15 ctor-bolla-prenotata          PIC  x.
                       88 ctor-bolla-si-prenotata VALUE IS "S". 
                       88 ctor-bolla-no-prenotata VALUE IS "N". 
               10 ctor-fattura.
                   15 ctor-anno-fattura PIC  9(4).
                   15 ctor-num-fattura  PIC  9(8).
                   15 ctor-data-fattura PIC  9(8).
                   15 ctor-num-prenot   PIC  9(8).
                   15 ctor-fatt-prenotata           PIC  x.
                       88 ctor-fatt-si-prenotata VALUE IS "S". 
                       88 ctor-fatt-no-prenotata VALUE IS "N". 
               10 ctor-mod-caricamento          PIC  X(1).
                   88 ctor-manuale VALUE IS "M". 
                   88 ctor-guidata VALUE IS "G". 
               10 ctor-agg-contab   PIC  x.
                   88 ctor-si-agg-contab VALUE IS "S". 
                   88 ctor-no-agg-contab VALUE IS "N". 
               10 ctor-tipo         PIC  x.
                   88 ctor-fattura-manuale VALUE IS "M". 
                   88 ctor-ordine VALUE IS "O". 
               10 ctor-note         PIC  X(500).
      *(( XFD NAME = ctor-contropartita_ ))
               10 ctor-contropartita            PIC  X(8).
               10 ctor-stato        PIC  X(1).
                   88 ctor-attivo VALUE IS "A". 
                   88 ctor-disattivo VALUE IS "D". 
                   88 ctor-bloccato VALUE IS "B". 
               10 ctor-dati-comuni.
                   15 ctor-data-creazione           PIC  9(8).
                   15 ctor-ora-creazione            PIC  9(8).
                   15 ctor-utente-creazione         PIC  X(10).
                   15 ctor-data-ultima-modifica     PIC  9(8).
                   15 ctor-ora-ultima-modifica      PIC  9(8).
                   15 ctor-utente-ultima-modifica   PIC  X(10).
               10 ctor-vuoti.
                   15 ctor-data-contab  PIC  9(8).
                   15 ctor-promo        PIC  9.
                       88 ctor-si-promo VALUE IS 1. 
                       88 ctor-no-promo VALUE IS 0. 
                   15 ctor-gest-plus    PIC  9(5).
                   15 ctor-ritira-in-lubex          PIC  9.
                       88 ctor-ritira-si VALUE IS 1. 
                       88 ctor-ritira-no VALUE IS 0. 
                   15 ctor-taglio       PIC  9(6).
                   15 ctor-flag-rec-prezzi          PIC  9.
                       88 ctor-rec-prezzi VALUE IS 1    WHEN SET TO 
           FALSE  0. 
                   15 ctor-ordine-testa.
                       20 ctor-anno-testa   PIC  9(4).
                       20 ctor-num-testa    PIC  9(8).
                   15 ctor-da-ordine    PIC  9.
                       88 ctor-da-ordine-no VALUE IS 0. 
                       88 ctor-da-ordine-si VALUE IS 1. 
                   15 ctor-forn-reso    PIC  9(5).
                   15 ctor-num-vuoto-3  PIC  9(5).
                   15 ctor-esito-consegna           PIC  X(10).
                   15 ctor-data-bolla-effettiva     PIC  9(8).
                   15 ctor-tipo-evasione            PIC  X(1).
                       88 ctor-ev-singola VALUE IS "S". 
                       88 ctor-ev-manuale VALUE IS "M". 
                       88 ctor-ev-immediata VALUE IS "I". 
                       88 ctor-ev-normale VALUE IS "N". 
                       88 ctor-ev-auto-trad VALUE IS "T". 
                       88 ctor-ev-auto-gdo VALUE IS "G". 
                   15 ctor-da-inviare   PIC  X(1).
                       88 ctor-da-inviare-si VALUE IS "S". 
                       88 ctor-da-inviare-no VALUE IS "N" " "    WHEN 
           SET TO FALSE  " ". 
                   15 ctor-ora-contab   PIC  9(8).
                   15 ctor-fattura-from.
                       20 ctor-fattura-from-data        PIC  9(8).
                       20 ctor-fattura-from-numero      PIC  x(8).
                   15 ctor-gdo          PIC  X(5).
                   15 ctor-tipocli      PIC  X(2).
                   15 ctor-note-bolla-1 PIC  X(500).
                   15 ctor-note-bolla-2 PIC  X(500).
                   15 ctor-causale-orig PIC  X(4).
                   15 ctor-contrassegno PIC  x.
                       88 ctor-contrassegno-no VALUE IS "N" , " ". 
                       88 ctor-contrassegno-si VALUE IS "S". 
                   15 ctor-epal         PIC  9(10).
                   15 ctor-bancali      PIC  9(10).
      *(( XFD NAME = ctor-gdo_1_1_2_1_2 ))
                   15 FILLER           PIC  X(1975).

           copy "rordini.fd".
       FD  rordini-c.
       01 cror-rec.
           05 cror-chiave.
               10 cror-anno         PIC  9(4).
               10 cror-num-ordine   PIC  9(8).
               10 cror-num-riga     PIC  9(5).
           05 cror-dati.
               10 cror-cod-articolo PIC  9(6).
               10 cror-des-libera   PIC  x(150).
               10 cror-qta          PIC  9(8).
               10 cror-prz-unitario PIC  9(9)v9(2).
               10 cror-imp-consumo  PIC  9(4)v9(2).
               10 cror-imp-cou-cobat            PIC  9(4)v9(2).
               10 cror-imponib-merce            PIC  9(9)v9(2).
               10 cror-perce-sconto PIC  9(2)v9(2).
               10 cror-omaggio      PIC  X(1).
                   88 cror-si-omaggio VALUE IS "S". 
                   88 cror-no-omaggio VALUE IS "N". 
               10 cror-peso-utf     PIC  9(5)v9(3).
               10 cror-peso-non-utf PIC  9(5)v9(3).
               10 cror-num-colli    PIC  9(5).
               10 cror-cod-imballo  PIC  X(3).
               10 cror-des-imballo  PIC  X(50).
               10 cror-qta-imballi  PIC  9(4).
               10 cror-cod-art-cli  PIC  X(15).
               10 cror-cod-iva      PIC  x(3).
               10 cror-prz-commle   PIC  9(9)v9(2).
               10 cror-prg-chiave.
                   15 cror-prg-cod-articolo         PIC  9(6).
                   15 cror-prg-cod-magazzino        PIC  X(3).
                   15 cror-prg-tipo-imballo         PIC  X(3).
                   15 cror-prg-peso     PIC  9(5)v9(3).
               10 cror-stato        PIC  X(1).
                   88 cror-attivo VALUE IS "A". 
                   88 cror-disattivo VALUE IS "D". 
                   88 cror-bloccato VALUE IS "B". 
               10 cror-dati-comuni.
                   15 cror-data-creazione           PIC  9(8).
                   15 cror-ora-creazione            PIC  9(8).
                   15 cror-utente-creazione         PIC  X(10).
                   15 cror-data-ultima-modifica     PIC  9(8).
                   15 cror-ora-ultima-modifica      PIC  9(8).
                   15 cror-utente-ultima-modifica   PIC  X(10).
               10 cror-vuoti.
                   15 cror-qta-omaggi   PIC  9(8).
                   15 cror-blister      PIC  9.
                       88 cror-si-blister VALUE IS 1    WHEN SET TO 
           FALSE  0. 
                   15 cror-promo        PIC  9(15).
                   15 cror-flag-cancellato          PIC  9.
                       88 cror-cancellato VALUE IS 1    WHEN SET TO 
           FALSE  0. 
                   15 cror-add-piombo   PIC  9(4)v9(2).
                   15 cror-riservato-split          PIC  9(8).
      *NON USATO
                   15 FILLER           PIC  9.
                   15 cror-prz-promo    PIC  9.
                       88 cror-si-prz-promo VALUE IS 1. 
                       88 cror-no-prz-promo VALUE IS 0. 
                   15 cror-chiave-ordine.
                       20 cror-chiave-ordine-testa.
                           25 cror-anno-master  PIC  9(4).
                           25 cror-numero-master            PIC  9(8).
      *(( XFD NAME = cror-progr-mster ))
                       20 cror-progr-master PIC  9(5).
                   15 cror-bli-codice   PIC  9(6).
                   15 cror-prz-manuale  PIC  9.
                       88 cror-prz-manuale-si VALUE IS 1. 
                       88 cror-prz-manuale-no VALUE IS 0. 
      *(( XFD NAME = cror-prz-manuale_1 ))
                   15 cror-evasa-SHI    PIC  9.
                       88 cror-evasa-SHI-si VALUE IS 1. 
                       88 cror-evasa-SHI-no VALUE IS 0. 
      *(( XFD NAME = cror-prz-manuale_1_ ))
                   15 cror-evasa-GET    PIC  9.
                       88 cror-evasa-GET-si VALUE IS 1. 
                       88 cror-evasa-GET-no VALUE IS 0. 
                   15 cror-bloccato-prezzo          PIC  9.
                       88 cror-bloccato-prezzo-si VALUE IS 1. 
                       88 cror-bloccato-prezzo-no VALUE IS 0. 
                   15 cror-alfa-vuoto   PIC  X(199).

           copy "tpromo.fd".
       FD  tpromo-c.
       01 ctpr-rec.
           05 ctpr-chiave.
               10 ctpr-codice       PIC  9(15).
           05 ctpr-dati.
               10 ctpr-chiave-ricerca.
                   15 ctpr-gdo          PIC  x(5).
                   15 ctpr-ini-dpo      PIC  9(8).
                   15 ctpr-fine-dpo     PIC  9(8).
               10 ctpr-tipo         PIC  x.
                   88 ctpr-nazionale VALUE IS "N". 
                   88 ctpr-locale VALUE IS "L". 
               10 ctpr-descrizione  PIC  x(50).
               10 ctpr-ini-volantino            PIC  9(8).
               10 ctpr-fine-volantino           PIC  9(8).
               10 ctpr-sett-uscita  PIC  9(3).
               10 ctpr-comuni.
                   15 ctpr-data-creazione           PIC  9(8).
                   15 ctpr-ora-creazione            PIC  9(8).
                   15 ctpr-utente-creazione         PIC  x(10).
                   15 ctpr-data-modifica            PIC  9(8).
                   15 ctpr-ora-modifica PIC  9(8).
                   15 ctpr-utente-modifica          PIC  x(10).
               10 ctpr-vuoti.
                   15 ctpr-dati-fittizia.
                       20 ctpr-fittizia     PIC  9(1).
                           88 ctpr-fittizia-si VALUE IS 1. 
                           88 ctpr-fittizia-no VALUE IS 0. 
                       20 ctpr-chiave-master.
                           25 ctpr-anno-master  PIC  9(4).
                           25 ctpr-numero-master            PIC  9(8).
                   15 ctpr-num-righe    PIC  9(3).
                   15 ctpr-num-vuoto-2  PIC  9(14).
                   15 ctpr-num-vuoto-3  PIC  9(15).
                   15 ctpr-note         PIC  x(35).
                   15 ctpr-alfa-vuoto   PIC  x(25).

           copy "rpromo.fd".
       FD  rpromo-c.
       01 crpr-rec.
           05 crpr-chiave.
               10 crpr-codice       PIC  9(15).
               10 crpr-articolo     PIC  9(6).
      *(( XFD NAME = crpr-gdo_1 ))
           05 crpr-dati.
               10 crpr-prz-ven      PIC  9(9)v99.
               10 crpr-prz-acq      PIC  9(9)v99.
               10 crpr-qta          PIC  9(9).
               10 crpr-comuni.
                   15 crpr-data-creazione           PIC  9(8).
                   15 crpr-ora-creazione            PIC  9(8).
                   15 crpr-utente-creazione         PIC  x(10).
                   15 crpr-data-modifica            PIC  9(8).
                   15 crpr-ora-modifica PIC  9(8).
                   15 crpr-utente-modifica          PIC  x(10).
               10 crpr-vuoti.
                   15 crpr-mro-chiave.
                       20 crpr-mro-chiave-testa.
                           25 crpr-mro-anno     PIC  9(4).
                           25 crpr-mro-numero   PIC  9(8).
                       20 crpr-mro-riga     PIC  9(5).
                   15 crpr-num-vuoto-2  PIC  9(13).
                   15 crpr-num-vuoto-3  PIC  9(15).
                   15 crpr-prenotazioni PIC  x(1).
                       88 crpr-prenotazioni-si VALUE IS "S". 
                       88 crpr-prenotazioni-no VALUE IS "N". 
                   15 crpr-alfa-vuoto-1 PIC  x(19).
                   15 crpr-alfa-vuoto-2 PIC  x(20).
                   15 crpr-alfa-vuoto-3 PIC  x(20).

           copy "tagli.fd".
       FD  tagli-c.
       01 ctag-rec.
           05 ctag-chiave.
               10 ctag-data         PIC  9(8).
               10 ctag-gdo          PIC  x(5).
               10 ctag-articolo     PIC  9(6).
               10 ctag-prog         PIC  9(3).
           05 ctag-dati.
               10 ctag-prz          PIC  s9(9)v99.
               10 ctag-qta          PIC  9(8).
               10 ctag-comuni.
                   15 ctag-data-creazione           PIC  9(8).
                   15 ctag-ora-creazione            PIC  9(8).
                   15 ctag-utente-creazione         PIC  x(10).
                   15 ctag-data-modifica            PIC  9(8).
                   15 ctag-ora-modifica PIC  9(8).
                   15 ctag-utente-modifica          PIC  x(10).
               10 ctag-vuoti.
                   15 ctag-mro-chiave.
                       20 ctag-mro-chiave-testa.
                           25 ctag-mro-anno     PIC  9(4).
                           25 ctag-mro-numero   PIC  9(8).
                       20 ctag-mro-riga     PIC  9(5).
                   15 ctag-num-vuoto-2  PIC  9(13).
                   15 ctag-num-vuoto-3  PIC  9(15).
                   15 ctag-cli-tipo     PIC  x(2).
                   15 ctag-alfa-vuoto-1 PIC  x(18).
                   15 ctag-alfa-vuoto-2 PIC  x(20).
                   15 ctag-alfa-vuoto-3 PIC  x(20).
                                   
           copy "EDI-mtordini.fd".
       FD  EDI-mtordini-c.
       01 cemto-rec.
           05 cemto-chiave.
               10 cemto-anno        PIC  9(4).
               10 cemto-numero      PIC  9(8).
      *
      *
      *
           05 cemto-dati.
               10 cemto-causale     PIC  x(4).
               10 cemto-cod-cli     PIC  9(5).
               10 cemto-prg-destino PIC  9(5).
               10 cemto-gdo         PIC  x(5).
               10 cemto-num-ord-cli PIC  X(50).
               10 cemto-data-ordine PIC  9(8).
               10 cemto-data-passaggio-ordine   PIC  9(8).
               10 cemto-cod-agente  PIC  9(5).
               10 cemto-cod-pagamento           PIC  x(3).
               10 cemto-cod-ese-iva PIC  x(3).
               10 cemto-vettore     PIC  9(5).
               10 cemto-note1       PIC  X(19).
               10 cemto-data-note1  PIC  9(8).
               10 cemto-note2       PIC  X(30).
               10 cemto-note3       PIC  X(30).
               10 cemto-note4       PIC  X(30).
               10 cemto-note        PIC  X(500).
               10 cemto-pz-tot      PIC  9(8).
               10 cemto-ritira-in-lubex         PIC  9.
                   88 cemto-ritira-si VALUE IS 1. 
                   88 cemto-ritira-no VALUE IS 0. 
               10 cemto-prenotazione-qta        PIC  9(1).
                   88 cemto-prenotazione-qta-si VALUE IS 1. 
                   88 cemto-prenotazione-qta-no VALUE IS 0. 
               10 cemto-saldi-banco PIC  9(1).
                   88 cemto-saldi-banco-si VALUE IS 1. 
                   88 cemto-saldi-banco-no VALUE IS 0. 
               10 cemto-saldi-promo PIC  9(1).
                   88 cemto-saldi-promo-si VALUE IS 1. 
                   88 cemto-saldi-promo-no VALUE IS 0. 
               10 cemto-stato       PIC  X(1).
                   88 cemto-attivo VALUE IS "A". 
                   88 cemto-bloccato VALUE IS "B". 
                   88 cemto-caricato VALUE IS "C". 
               10 cemto-errori.
                   15 cemto-cliente     PIC  9.
                       88 cemto-cliente-valido VALUE IS 0. 
                       88 cemto-clides-non-valido VALUE IS 1. 
                       88 cemto-cliente-non-valido VALUE IS 2. 
                       88 cemto-cliente-non-attivo VALUE IS 3. 
                   15 cemto-cliente-fido            PIC  9.
                       88 cemto-cliente-fido-ok VALUE IS 0. 
                       88 cemto-cliente-fuori-fido VALUE IS 1. 
                   15 cemto-destino     PIC  9.
                       88 cemto-destino-valido VALUE IS 0. 
                       88 cemto-destino-non-valido VALUE IS 1. 
                       88 cemto-destino-non-attivo VALUE IS 2. 
                   15 cemto-righe       PIC  9.
                       88 cemto-righe-presenti VALUE IS 0. 
                       88 cemto-righe-non-presenti VALUE IS 1. 
                   15 cemto-qta         PIC  9.
                       88 cemto-qta-ok VALUE IS 0. 
                       88 cemto-qta-ko VALUE IS 1. 
                   15 cemto-art         PIC  9.
                       88 cemto-art-ok VALUE IS 0. 
                       88 cemto-art-ko VALUE IS 1. 
                   15 cemto-prg         PIC  9.
                       88 cemto-prg-ok VALUE IS 0. 
                       88 cemto-prg-ko VALUE IS 1. 
                   15 cemto-prz         PIC  9.
                       88 cemto-prz-ok VALUE IS 0. 
                       88 cemto-prz-ko VALUE IS 1. 
                   15 cemto-esistente   PIC  9.
                       88 cemto-esistente-no VALUE IS 0. 
                       88 cemto-esistente-si VALUE IS 1. 
               10 cemto-ordine.
                   15 cemto-ordine-anno PIC  9(4).
                   15 cemto-ordine-numero           PIC  9(8).
               10 cemto-dati-comuni.
                   15 cemto-data-creazione          PIC  9(8).
                   15 cemto-ora-creazione           PIC  9(8).
                   15 cemto-utente-creazione        PIC  X(10).
                   15 cemto-data-ultima-modifica    PIC  9(8).
                   15 cemto-ora-ultima-modifica     PIC  9(8).
                   15 cemto-utente-ultima-modifica  PIC  X(10).
           05 cemto-dati-import.
               15 cemto-nome-file   PIC  x(100).
               15 cemto-riga-file   PIC  9(6).
               15 cemto-record-01T.
                   20 cemto-01T-filler  PIC  x(35).
                   20 cemto-02T-filler  PIC  x(35).
                   20 cemto-03T-filler  PIC  x(35).
                   20 cemto-01T04-BGM-DATADOC       PIC  x(8).
                   20 cemto-01T05-BGM-NUMDOC        PIC  x(35).
                   20 cemto-06T-filler  PIC  x(35).
                   20 cemto-07T-filler  PIC  x(35).
                   20 cemto-08T-filler  PIC  x(35).
                   20 cemto-09T-filler  PIC  x(35).
                   20 cemto-10T-filler  PIC  x(35).
                   20 cemto-01T11-DTM-DATACONS      PIC  x(8).
                   20 cemto-12T-filler  PIC  x(35).
                   20 cemto-13T-filler  PIC  x(35).
                   20 cemto-14T-filler  PIC  x(35).
                   20 cemto-15T-filler  PIC  x(35).
                   20 cemto-16T-filler  PIC  x(35).
                   20 cemto-17T-filler  PIC  x(35).
                   20 cemto-18T-filler  PIC  x(35).
                   20 cemto-19T-filler  PIC  x(35).
                   20 cemto-20T-filler  PIC  x(35).
                   20 cemto-01T21-NAB-CODBUYER      PIC  x(17).
                   20 cemto-01T22-NAB-QCODBUYER     PIC  x(35).
                   20 cemto-01T23-NAB-RAGSOCB       PIC  x(70).
                   20 cemto-01T24-NAB-INDIRB        PIC  x(35).
                   20 cemto-01T25-NAB-CITTAB        PIC  x(35).
                   20 cemto-01T26-NAB-PROVB         PIC  x(3).
                   20 cemto-01T27-NAB-CAPB          PIC  x(5).
      *"LBX" = prezzo e cliente LBX
      *"CLBX" = solo cliente LBX
      *"PLBX" = solo prezzo LBX
                   20 cemto-01T28-NAD-CODCONS       PIC  x(17).
                   20 cemto-29T-filler  PIC  x(35).
      *(( XFD NAME = cemto-01T23-NAB-RAG ))
                   20 cemto-01T30-NAD-RAGSOCD       PIC  x(70).
      *(( XFD NAME = cemto-01T24-NAB-IND ))
                   20 cemto-01T31-NAD-INDIRD        PIC  x(35).
      *(( XFD NAME = cemto-01T25-NAB-CIT ))
                   20 cemto-01T32-NAD-CITTAD        PIC  x(35).
      *(( XFD NAME = cemto-01T26-NAB-PRO ))
                   20 cemto-01T33-NAD-PROVD         PIC  x(3).
      *(( XFD NAME = cemto-01T27-NAB-CAP ))
                   20 cemto-01T34-NAD-CAPD          PIC  x(5).
                   20 cemto-01T35-FTX-NOTE          PIC  x(350).
                   20 cemto-36T-filler  PIC  x(35).
                   20 cemto-01T37-BGM-CODAZION      PIC  x(35).
                   20 cemto-38T-filler  PIC  x(35).
                   20 cemto-39T-filler  PIC  x(35).
                   20 cemto-40T-filler  PIC  x(35).
                   20 cemto-01T41-NAI-CODFATT       PIC  x(35).
                   20 cemto-42T-filler  PIC  x(35).
                   20 cemto-43T-filler  PIC  x(35).
                   20 cemto-44T-filler  PIC  x(35).
                   20 cemto-45T-filler  PIC  x(35).
                   20 cemto-46T-filler  PIC  x(35).
                   20 cemto-47T-filler  PIC  x(35).
                   20 cemto-48T-filler  PIC  x(35).
                   20 cemto-49T-filler  PIC  x(35).
                   20 cemto-50T-filler  PIC  x(35).
                   20 cemto-51T-filler  PIC  x(35).
                   20 cemto-52T-filler  PIC  x(35).
                   20 cemto-53T-filler  PIC  x(35).
                   20 cemto-54T-filler  PIC  x(35).
                   20 cemto-55T-filler  PIC  x(35).
                   20 cemto-56T-filler  PIC  x(35).
                   20 cemto-57T-filler  PIC  x(35).
                   20 cemto-58T-filler  PIC  x(35).
                   20 cemto-59T-filler  PIC  x(35).
      * Valore del campo originale dal file di import
                   20 cemto-01T60-inversione-imposte            PIC  x.
      * Valore del campo originale dal file di import
                   20 cemto-01T61-ev-immediata      PIC  x.
                   20 cemto-60T-filler  PIC  x(33).
                   20 cemto-61T-filler  PIC  x(35).
                   20 cemto-62T-filler  PIC  x(35).
                   20 cemto-63T-filler  PIC  x(35).
                   20 cemto-64T-filler  PIC  x(35).
                   20 cemto-65T-filler  PIC  x(35).
                   20 cemto-66T-filler  PIC  x(35).
                   20 cemto-67T-filler  PIC  x(35).
                   20 cemto-01T68-FTX-NOTE          PIC  x(350).
                   20 cemto-01T69-FTX-NOTE          PIC  x(350).
                   20 cemto-01T70-FTX-NOTE          PIC  x(350).
                   20 cemto-71T-filler  PIC  x(35).
                   20 cemto-72T-filler  PIC  x(35).
                   20 cemto-73T-filler  PIC  x(35).
                   20 cemto-74T-filler  PIC  x(35).
                   20 cemto-75T-filler  PIC  x(35).
                   20 cemto-76T-filler  PIC  x(35).
                   20 cemto-77T-filler  PIC  x(35).
                   20 cemto-78T-filler  PIC  x(35).
                   20 cemto-79T-filler  PIC  x(35).
                   20 cemto-80T-filler  PIC  x(35).
                   20 cemto-81T-filler  PIC  x(35).
                   20 cemto-82T-filler  PIC  x(35).
                   20 cemto-83T-filler  PIC  x(35).
                   20 cemto-84T-filler  PIC  x(35).
                   20 cemto-85T-filler  PIC  x(35).
                   20 cemto-86T-filler  PIC  x(35).
                   20 cemto-87T-filler  PIC  x(35).
                   20 cemto-88T-filler  PIC  x(35).
                   20 cemto-89T-filler  PIC  x(35).
                   20 cemto-90T-filler  PIC  x(35).
                   20 cemto-91T-filler  PIC  x(35).
                   20 cemto-92T-filler  PIC  x(35).
                   20 cemto-93T-filler  PIC  x(35).
                   20 cemto-94T-filler  PIC  x(35).
                   20 cemto-95T-filler  PIC  x(35).
                   20 cemto-96T-filler  PIC  x(35).
                   20 cemto-97T-filler  PIC  x(35).
                   20 cemto-98T-filler  PIC  x(35).
                   20 cemto-99T-filler  PIC  x(35).
                   20 cemto-100T-filler PIC  x(35).
                   20 cemto-101T-filler PIC  x(35).
                   20 cemto-102T-filler PIC  x(35).
                   20 cemto-103T-filler PIC  x(35).
                   20 cemto-104T-filler PIC  x(35).
                   20 cemto-105T-filler PIC  x(35).
                   20 cemto-106T-filler PIC  x(35).
                   20 cemto-107T-filler PIC  x(35).
                   20 cemto-108T-filler PIC  x(35).
                   20 cemto-109T-filler PIC  x(35).
                   20 cemto-110T-filler PIC  x(35).
                   20 cemto-111T-filler PIC  x(35).
                   20 cemto-112T-filler PIC  x(35).
                   20 cemto-113T-filler PIC  x(35).
                   20 cemto-114T-filler PIC  x(35).
                   20 cemto-115T-filler PIC  x(35).
                   20 cemto-116T-filler PIC  x(35).
                   20 cemto-117T-filler PIC  x(35).
                   20 cemto-118T-filler PIC  x(35).
                   20 cemto-119T-filler PIC  x(35).
                   20 cemto-120T-filler PIC  x(35).
                   20 cemto-121T-filler PIC  x(35).
                   20 cemto-122T-filler PIC  x(35).
                   20 cemto-123T-filler PIC  x(35).
                   20 cemto-124T-filler PIC  x(35).
                   20 cemto-125T-filler PIC  x(35).
                   20 cemto-126T-filler PIC  x(35).
                   20 cemto-127T-filler PIC  x(35).
                   20 cemto-128T-filler PIC  x(35).
                   20 cemto-129T-filler PIC  x(35).
                   20 cemto-130T-filler PIC  x(35).
                   20 cemto-131T-filler PIC  x(35).
                   20 cemto-132T-filler PIC  x(35).
                   20 cemto-133T-filler PIC  x(35).
                   20 cemto-134T-filler PIC  x(35).
                   20 cemto-135T-filler PIC  x(35).
                   20 cemto-136T-filler PIC  x(35).
                   20 cemto-137T-filler PIC  x(35).
                   20 cemto-138T-filler PIC  x(35).
                   20 cemto-139T-filler PIC  x(35).
                   20 cemto-140T-filler PIC  x(35).
                   20 cemto-141T-filler PIC  x(35).
                   20 cemto-142T-filler PIC  x(35).
                   20 cemto-143T-filler PIC  x(35).
                   20 cemto-144T-filler PIC  x(35).
                   20 cemto-145T-filler PIC  x(35).
                   20 cemto-146T-filler PIC  x(35).
                   20 cemto-147T-filler PIC  x(35).
                   20 cemto-148T-filler PIC  x(35).
                   20 cemto-149T-filler PIC  x(35).
                   20 cemto-150T-filler PIC  x(35).
                   20 cemto-151T-filler PIC  x(35).
                   20 cemto-152T-filler PIC  x(35).
                   20 cemto-153T-filler PIC  x(35).
                   20 cemto-154T-filler PIC  x(35).
                   20 cemto-155T-filler PIC  x(35).
                   20 cemto-156T-filler PIC  x(35).
                   20 cemto-157T-filler PIC  x(35).
                   20 cemto-158T-filler PIC  x(35).
                   20 cemto-159T-filler PIC  x(35).
                   20 cemto-160T-filler PIC  x(35).
                   20 cemto-161T-filler PIC  x(35).
                   20 cemto-162T-filler PIC  x(35).
                   20 cemto-01T163-TOD-CODCOST      PIC  x(3).
                   20 filler           PIC  x(501).
               15 cemto-bloc-forzato            PIC  9.
                   88 cemto-bloc-forzato-si VALUE IS 1. 
                   88 cemto-bloc-forzato-no VALUE IS 0. 
               15 cemto-Sum         PIC  9(12)v999.
               15 cemto-evadi-dal   PIC  9(8).
      *L'inversione viene impostata solo per clienti tradizionali, quindi conservo il valore nel campo 60 ma poi uso questo
               15 cemto-inversione-imposte      PIC  9.
                   88 cemto-inversione-imposte-si VALUE IS 1. 
                   88 cemto-inversione-imposte-no VALUE IS 0. 
      *L'inversione viene impostata solo per clienti tradizionali, quindi conservo il valore nel campo 60 ma poi uso questo
               15 cemto-ev-immediata            PIC  9.
                   88 cemto-ev-immediata-si VALUE IS 1. 
                   88 cemto-ev-immediata-no VALUE IS 0. 
      *L'inversione viene impostata solo per clienti tradizionali, quindi conservo il valore nel campo 60 ma poi uso questo
               15 cemto-contrassegno            PIC  x.
                   88 cemto-contrassegno-no VALUE IS "N" , " ". 
                   88 cemto-contrassegno-si VALUE IS "S". 
               15 FILLER           PIC  x(173).

           copy "EDI-mrordini.fd".
       FD  EDI-mrordini-c.
       01 cemro-rec.
           05 cemro-chiave.
               10 cemro-chiave-testa.
                   15 cemro-anno        PIC  9(4).
                   15 cemro-numero      PIC  9(8).
               10 cemro-riga        PIC  9(5).
           05 cemro-dati.
               10 cemro-cod-articolo            PIC  9(6).
               10 cemro-qta-EDI     PIC  9(8).
               10 cemro-qta-GESLUX  PIC  9(8).
               10 cemro-qta         PIC  9(8).
               10 cemro-prz-EDI     PIC  9(9)v9(2).
               10 cemro-prz-GESLUX  PIC  9(9)v9(2).
               10 cemro-prz         PIC  9(9)v9(2).
      *(( XFD NAME = cemro-prz_B ))
               10 cemro-bloccato-prezzo         PIC  9.
                   88 cemro-bloccato-prezzo-no VALUE IS 0. 
                   88 cemro-bloccato-prezzo-si VALUE IS 1. 
               10 cemro-peso-utf    PIC  9(5)v9(3).
               10 cemro-peso-non-utf            PIC  9(5)v9(3).
               10 cemro-num-colli   PIC  9(5).
               10 cemro-cod-imballo PIC  X(3).
               10 cemro-des-imballo PIC  X(50).
               10 cemro-qta-imballi PIC  9(4).
               10 cemro-cod-art-cli PIC  X(15).
               10 cemro-prz-commle  PIC  9(9)v9(2).
               10 cemro-prg-chiave.
                   15 cemro-prg-cod-articolo        PIC  9(6).
                   15 cemro-prg-cod-magazzino       PIC  X(3).
                   15 cemro-prg-tipo-imballo        PIC  X(3).
                   15 cemro-prg-peso    PIC  9(5)v9(3).
               10 cemro-prg-forzato.
                   15 cemro-prg-cod-articolo-f      PIC  9(6).
                   15 cemro-prg-cod-magazzino-f     PIC  X(3).
                   15 cemro-prg-tipo-imballo-f      PIC  X(3).
                   15 cemro-prg-peso-f  PIC  9(5)v9(3).
               10 cemro-dati-blister.
                   15 cemro-bli-codice  PIC  9(6).
                   15 cemro-bli-qta     PIC  9(8).
                   15 cemro-bli-perce   PIC  9(3)v99.
                   15 cemro-blister     PIC  9.
                       88 cemro-si-blister VALUE IS 1    WHEN SET TO 
           FALSE  0. 
               10 cemro-promo       PIC  9(15).
               10 cemro-prz-promo   PIC  9.
                   88 cemro-si-prz-promo VALUE IS 1. 
                   88 cemro-no-prz-promo VALUE IS 0. 
               10 cemro-evadi-dal   PIC  9(8).
               10 cemro-stato       PIC  X(1).
                   88 cemro-attivo VALUE IS "A". 
                   88 cemro-bloccato VALUE IS "B". 
                   88 cemro-caricato VALUE IS "C". 
               10 cemro-errori.
                   15 cemro-articolo    PIC  9.
                       88 cemro-articolo-valido VALUE IS 0. 
                       88 cemro-articolo-non-valido VALUE IS 1. 
                       88 cemro-articolo-non-attivo VALUE IS 2. 
      *(( XFD NAME = cemro-qta_1 ))
                   15 cemro-qtac        PIC  9.
                       88 cemro-qtac-ok VALUE IS 0. 
                       88 cemro-qtac-non-presente VALUE IS 1. 
                       88 cemro-qtac-adattata VALUE IS 2. 
      *(( XFD NAME = cemro-prz_1 ))
                   15 cemro-prezzo      PIC  9.
                       88 cemro-prezzo-valido VALUE IS 0. 
                       88 cemro-prezzo-non-valido VALUE IS 1. 
      *(( XFD NAME = cemro-prog ))
                   15 cemro-progressivo PIC  9.
                       88 cemro-progressivo-valido VALUE IS 0. 
                       88 cemro-progressivo-non-trovato VALUE IS 1. 
                       88 cemro-progressivo-non-attivo VALUE IS 2. 
                       88 cemro-progressivo-non-forzato VALUE IS 3. 
                       88 cemro-progressivo-non-valido VALUE IS 4. 
               10 cemro-ordine.
                   15 cemro-ordine-testa.
                       20 cemro-ordine-anno PIC  9(4).
                       20 cemro-ordine-numero           PIC  9(8).
                   15 cemro-ordine-riga PIC  9(5).
               10 cemro-dati-comuni.
                   15 cemro-data-creazione          PIC  9(8).
                   15 cemro-ora-creazione           PIC  9(8).
                   15 cemro-utente-creazione        PIC  X(10).
                   15 cemro-data-ultima-modifica    PIC  9(8).
                   15 cemro-ora-ultima-modifica     PIC  9(8).
                   15 cemro-utente-ultima-modifica  PIC  X(10).
           05 cemro-dati-import.
               10 cemro-nome-file   PIC  x(100).
               10 cemro-riga-file   PIC  9(6).
               10 cemro-record-02D.
                   15 cemro-01D-filler  PIC  x(35).
                   15 cemro-02D-filler  PIC  x(35).
                   15 cemro-03D-filler  PIC  x(35).
                   15 cemro-04D-filler  PIC  x(35).
                   15 cemro-05D-filler  PIC  x(35).
                   15 cemro-06D-filler  PIC  x(35).
                   15 cemro-07D-filler  PIC  x(35).
                   15 cemro-08D-filler  PIC  x(35).
                   15 cemro-09D-filler  PIC  x(35).
                   15 cemro-10D-filler  PIC  x(35).
                   15 cemro-11D-filler  PIC  x(35).
                   15 cemro-12D-filler  PIC  x(35).
                   15 cemro-02D13-LIN-CODFORTU      PIC  x(35).
      *(( XFD NAME = cemro-02D13-LIN-COD ))
                   15 cemro-02D14-LIN-CODDISTU      PIC  x(35).
                   15 cemro-02D15-LIN-DESART        PIC  x(100).
                   15 cemro-16D-filler  PIC  x(35).
                   15 cemro-02D17-QTAORD            PIC  x(20).
                   15 cemro-18D-filler  PIC  x(35).
                   15 cemro-02D19-LIN-PRZUNI        PIC  x(20).
                   15 cemro-19D-filler  PIC  x(35).
                   15 cemro-20D-filler  PIC  x(35).
                   15 cemro-21D-filler  PIC  x(35).
      *(( XFD NAME = cemro-21D-filler_1 ))
                   15 cemro-02D22-LIN-NRCUINTU      PIC  x(20).
      *(( XFD NAME = cemro-21D-filler_1_ ))
                   15 cemro-02D222-QTAPZ            PIC  x(20).
                   15 FILLER           PIC  x(180).

           copy "log-macrobatch.fd".

       WORKING-STORAGE SECTION.
           copy "acucobol.def".
                  
       77  status-mtordini       pic xx.
       77  status-mrordini       pic xx.
       77  status-tordini        pic xx.
       77  status-rordini        pic xx.
       77  status-tpromo         pic xx.
       77  status-rpromo         pic xx.
       77  status-tagli          pic xx.
       77  status-EDI-mtordini   pic xx.
       77  status-EDI-mrordini   pic xx.
       77  status-mtordini-c     pic xx.
       77  status-mrordini-c     pic xx.
       77  status-tordini-c      pic xx.
       77  status-rordini-c      pic xx.
       77  status-tpromo-c       pic xx.
       77  status-rpromo-c       pic xx.
       77  status-tagli-c        pic xx.
       77  status-EDI-mtordini-c pic xx.
       77  status-EDI-mrordini-c pic xx.
       77  status-log-macrobatch pic xx.
       
       77  path-mtordini-c     pic x(256).
       77  path-mrordini-c     pic x(256).
       77  path-tordini-c      pic x(256).
       77  path-rordini-c      pic x(256).
       77  path-tpromo-c       pic x(256).
       77  path-rpromo-c       pic x(256).
       77  path-tagli-c        pic x(256).
       77  path-EDI-mtordini-c pic x(256).
       77  path-EDI-mrordini-c pic x(256).
                
       77  nome-file           pic x(13).
       77  NomeFile            pic x(256).
       77  num-rec             pic 9(10).
       77  num-rec-z           pic z.zzz.zzz.zz9.
       77  num-rec-x           pic x(15).

       78  78-anno-start       value "2023".


       77  pathCopia           pic x(256).
       copy "log-macrobatch.def".      

       LINKAGE SECTION.
       77  link-path           pic x(256).

      ******************************************************************
       PROCEDURE DIVISION using link-path.

      ***---
       MAIN-PRG.
           perform INIT.
           perform OPEN-FILES.
           perform ELABORAZIONE.
           perform CLOSE-FILES.
           perform EXIT-PGM.

      ***---
       INIT.
           accept  pathCopia from environment "PATH_MACROBATCH_COPY".
           inspect pathCopia replacing trailing spaces by low-value.
           move link-path to path-log-macrobatch.

      ***---
       OPEN-FILES.                       
           open input mtordini mrordini tordini rordini tpromo   
                      rpromo tagli EDI-mtordini EDI-mrordini.
           open extend log-macrobatch.

      ***---
       ELABORAZIONE.            
           call   "set-ini-log" using r-output.
           cancel "set-ini-log".
           initialize lm-riga.
           string r-output                delimited size
                  "INIZIO COPIA ARCHIVI " delimited size
             into lm-riga
           end-string.
           write lm-riga.     

           move "mtordini" to nome-file.
           perform COPIA-FILE.
           move "mrordini" to nome-file.
           perform COPIA-FILE.
           move "tordini"  to nome-file.
           perform COPIA-FILE.
           move "rordini"  to nome-file.
           perform COPIA-FILE.
           move "tpromo"   to nome-file.
           perform COPIA-FILE.
           move "rpromo"   to nome-file.
           perform COPIA-FILE. 
           move "tagli"    to nome-file.
           perform COPIA-FILE. 
           move "EDI-mtordini" to nome-file.
           perform COPIA-FILE. 
           move "EDI-mrordini" to nome-file.
           perform COPIA-FILE.  

           call   "set-ini-log" using r-output.
           cancel "set-ini-log".
           initialize lm-riga.
           string r-output              delimited size
                  "FINE COPIA ARCHIVI " delimited size
             into lm-riga
           end-string.
           write lm-riga.     
           
      ***---
       COPIA-FILE.    
           inspect nome-file replacing trailing spaces by low-value.
           initialize nomeFile.
           string pathCopia delimited low-value
                  nome-file delimited low-value
             into nomeFile
           end-string.        
           inspect nome-file replacing trailing low-value by spaces.

           move 0 to num-rec.   

           call   "set-ini-log" using r-output.
           cancel "set-ini-log".
           initialize lm-riga.
           string r-output          delimited size
                  nome-file         delimited size
                  " - INIZIO COPIA" delimited size
             into lm-riga
           end-string.
           write lm-riga.     

           evaluate nome-file
           when "mtordini"                      
                move nomeFile to path-mtordini-c
                open output mtordini-c          
                move low-value to mto-rec
                move 78-anno-start to mto-anno
                start mtordini key >= mto-chiave
                      invalid continue
                  not invalid
                      perform until 1 = 2
                         read mtordini next at end exit perform end-read
                         move mto-rec to cmto-rec
                         write cmto-rec
                         add 1 to num-rec
                      end-perform
                end-start
                close mtordini-c
           when "mrordini" 
                move nomeFile to path-mrordini-c
                open output mrordini-c
                move low-value to mro-rec
                move 78-anno-start to mro-anno
                start mrordini key >= mro-chiave
                      invalid continue
                  not invalid
                      perform until 1 = 2
                         read mrordini next at end exit perform end-read
                         move mro-rec to cmro-rec
                         write cmro-rec
                         add 1 to num-rec
                      end-perform
                end-start
                close mrordini-c
           when "tordini"  
                move nomeFile to path-tordini-c
                open output tordini-c
                move low-value to tor-rec
                move 78-anno-start to tor-anno
                start tordini key >= tor-chiave
                      invalid continue
                  not invalid
                      perform until 1 = 2
                         read tordini next at end exit perform end-read
                         move tor-rec to ctor-rec
                         write ctor-rec
                         add 1 to num-rec
                      end-perform
                end-start
                close tordini-c
           when "rordini"        
                move nomeFile to path-rordini-c
                open output rordini-c
                move low-value to ror-rec
                move 78-anno-start to ror-anno
                start rordini key >= ror-chiave
                      invalid continue
                  not invalid
                      perform until 1 = 2
                         read rordini next at end exit perform end-read
                         move ror-rec to cror-rec
                         write cror-rec
                         add 1 to num-rec
                      end-perform
                end-start
                close rordini-c
           when "tpromo"   
                move nomeFile to path-tpromo-c
                open output tpromo-c
                move low-value to tpr-rec
                start tpromo key >= tpr-chiave
                      invalid continue
                  not invalid
                      perform until 1 = 2
                         read tpromo next at end exit perform end-read
                         move tpr-rec to ctpr-rec
                         write ctpr-rec
                         add 1 to num-rec
                      end-perform
                end-start
                close tpromo-c
           when "rpromo"   
                move nomeFile to path-rpromo-c
                open output rpromo-c
                move low-value to rpr-rec
                start rpromo key >= rpr-chiave
                      invalid continue
                  not invalid
                      perform until 1 = 2
                         read rpromo next at end exit perform end-read
                         move rpr-rec to crpr-rec
                         write crpr-rec
                         add 1 to num-rec
                      end-perform
                end-start
                close rpromo-c 
           when "tagli"    
                move nomeFile to path-tagli-c  
                open output tagli-c
                move low-value to tag-rec
                string 78-anno-start "0101" into tag-data

                start tagli key >= tag-chiave
                      invalid continue
                  not invalid
                      perform until 1 = 2
                         read tagli next at end exit perform end-read
                         move tag-rec to ctag-rec
                         write ctag-rec
                         add 1 to num-rec
                      end-perform
                end-start
                close tagli-c       
           when "EDI-mtordini"                      
                move nomeFile to path-EDI-mtordini-c
                open output EDI-mtordini-c          
                move low-value to emto-rec
                move 78-anno-start to emto-anno
                start EDI-mtordini key >= emto-chiave
                      invalid continue
                  not invalid
                      perform until 1 = 2
                         read EDI-mtordini next 
                           at end exit perform end-read
                         move emto-rec to cemto-rec
                         write cemto-rec
                         add 1 to num-rec
                      end-perform
                end-start
                close EDI-mtordini-c
           when "EDI-mrordini"                      
                move nomeFile to path-EDI-mrordini-c
                open output EDI-mrordini-c          
                move low-value to emro-rec
                move 78-anno-start to emro-anno
                start EDI-mrordini key >= emro-chiave
                      invalid continue
                  not invalid
                      perform until 1 = 2
                         read EDI-mrordini next 
                           at end exit perform end-read
                         move emro-rec to cemro-rec
                         write cemro-rec
                         add 1 to num-rec
                      end-perform
                end-start
                close EDI-mrordini-c
           end-evaluate.

           move num-rec   to num-rec-z
           move num-rec-z to num-rec-x.
           call "C$JUSTIFY" using num-rec-x, "L".
           inspect num-rec-x replacing trailing spaces by low-value.

           call   "set-ini-log" using r-output.
           cancel "set-ini-log".
           initialize lm-riga.
           string r-output      delimited size
                  nome-file     delimited size
                  " - COPIATI " delimited size
                  num-rec-x     delimited low-value
                  " RECORDS."   delimited size
             into lm-riga
           end-string.
           write lm-riga.     

      ***---
       CLOSE-FILES.                      
           close mtordini mrordini tordini rordini tpromo   
                 rpromo tagli log-macrobatch EDI-mtordini EDI-mrordini.

      ***---
       EXIT-PGM.
           goback.
