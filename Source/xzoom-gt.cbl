       program-id.     Zoom-GT.
       REMARKS. NUMERO MASSIMO DI CAMPI IN GRID 10.

      *************************
       working-storage section.
      *************************
       copy "acugui.def".
       copy "acucobol.def".
       copy "crtvars.def".

       78 78-walt              value 1.
       78 78-win-box           value 2.
       78 78-about             value 3.
       78 78-walt-file         value 4.
       78 78-walt-delim        value 5.
       78 78-quit              value 27.

           copy "xzoom-common.def".
           copy "xzoom3.def".
           copy "externals.def".

       77  param-size pic 9(5).
       77  nome-file  pic X(256).

       77  stato-zoom signed-long.

       77  idx pic 9(3).

       77  filler  pic 9 value 0.
         88 TuttiIRec value 1, false 0.

       77  filler  pic 9 value 0.
         88 destinif value 1, false 0.

       LINKAGE SECTION.
       77  como-file   pic x(20).
       77  como-record pic x(32000).

      ************************************************************************
       procedure division using como-file, como-record.
      ************************************************************************
       MAIN-LOGIC.

           set environment "XZOOM_HIDE_CLOCK"     to 1.

           set environment "XZOOM_WINDOW_DELAYED" to 1.

      * PER LO ZOOM DEGLI ARTICOLI PER NON VEDERE LA SCRITTA NULL NEI SOTTOGRUPPI
           set environment "XZOOM_NULL_TEXT_NUMERIC" to "#Blank#".
           set environment "XZOOM_NULL_TEXT_ALPHA"   to "#Blank#".

           call "C$PARAMSIZE" 
                using 2, 
                giving param-size.

           set environment "XZOOM_LAYOUT" to "Grid".


           evaluate como-file
           when "user-pgm"
                perform PREPARA-USER-PGM           |CERCA
           when "tregioni"       
                perform PREPARA-TREGIONI           |CERCA
           when "ABI"
                perform PREPARA-ABI            |CERCA
           when "agenti"
           when "agenti-alfa"
                perform PREPARA-AGENTI             |CERCA
           when "tmarche"
           when "tmarche-alfa"
                perform PREPARA-TMARCHE            |CERCA
           when "tprov"
                perform PREPARA-TPROV              |CERCA
           when "tnazioni"
                perform PREPARA-TNAZIONI           |CERCA
           when "tcodpag"
                perform PREPARA-TCODPAG            |CERCA
           when "tblco"
                perform PREPARA-TBLCO
           when "tivaese"
           when "tivaese-ese"
           when "tivaese-non"
                perform PREPARA-TIVAESE            |CERCA
           when "clienti"
           when "clienti-alfa"
                perform PREPARA-CLIENTI
           when "clienti-all"
           when "clienti-alfa-all" |da gclienti, gforn e gmovmag
      *    Il programma di gestione Clienti/Fornitori deve visualizzare
      *    tutti i record indipendentemente dallo status (così anche per
      *    i movimenti di magazzino)
                set TuttiIRec to true
                perform PREPARA-CLIENTI            |CERCA
           when "ttipocli"
                perform PREPARA-TTIPOCLI           |CERCA                                          
           when "tgrupgdo"
                perform PREPARA-TGRUPGDO           |CERCA
           when "tvettori"
           when "tvettori-alfa"
                perform PREPARA-TVETTORI           |CERCA
           when "destini"
           when "destini-alfa"
                perform PREPARA-VIDEO-DESTINI      |CERCA
           when "destini-citta"
                perform PREPARA-CITTA-DESTINI      |CERCA
           when "destinif"
           when "destinif-forn"
           when "destinif-alfa"
                perform PREPARA-VIDEO-DESTINIF     |CERCA
           when "clienti-des"
           when "clienti-des-alf"
                perform PREPARA-CLIENTI-DI-DESTINI |CERCA
           when "clienti-des-all"
           when "clienti-des-alf-all"
                set TuttiIRec to true
                perform PREPARA-CLIENTI-DI-DESTINI |CERCA
           when "clienti-desf"
           when "clienti-desf-alf"
                set destinif to true
                perform PREPARA-CLIENTI-DI-DESTINI |CERCA
           when "clienti-desf-all"
           when "clienti-desf-alf-all"
                set destinif  to true
                set TuttiIRec to true
                perform PREPARA-CLIENTI-DI-DESTINI |CERCA
           when "forn-des"
           when "forn-des-alf"
                perform PREPARA-FORN-DI-DESTINI |CERCA
           when "articoli-all"
           when "articoli-alfa-all"
                set TuttiIRec to true
                perform PREPARA-ARTICOLI
           when "articoli"
           when "articoli-alfa"
                perform PREPARA-ARTICOLI           |CERCA E/O SELEZIONA
           when "clienti-gdo"
                perform PREPARA-CLIENTI-GDO        |CERCA
           when "clienti-no-gdo"
                perform PREPARA-CLIENTI-NON-GDO        |CERCA
           when "clienti-gdo-all"
           when "clienti-gdo-alf-all"
                set TuttiIRec to true
                perform PREPARA-CLIENTI-DI-GDO     |CERCA
           when "assorcli"           
                perform PREPARA-VIDEO-ASSORCLI     |SELEZIONA GASSORCLI
           when "asc-clienti"
                perform PREPARA-CLIENTI-ASSORCLI   |SELEZIONA GASSORCLI
           when "asc-destini"
                perform PREPARA-DESTINI-ASSORCLI   |SELEZIONA GASSORCLI
           when "asc-articoli"
                perform PREPARA-ARTICOLI-ASSORCLI  |SELEZIONA GASSORCLI
           when "asc-art-gordc"
                perform PREPARA-ARTICOLI-ASSORCLI-GORDC |CERCA
           when "tmagaz"
                perform PREPARA-TMAGAZ             |CERCA
           when "timballi"
                perform PREPARA-TIMBALLI           |CERCA
           when "timbalqta"
                perform PREPARA-TIMBALQTA           |CERCA           
           when "progmag-sons" 
                perform PREPARA-PROGMAG-ONLY-SONS
           when "progmag" 
                perform PREPARA-VIDEO-PROGMAG      |SELEZIONA PROGMAG
           when "prg-artico-sons"
           when "prg-artico-sons-a"
                perform PREPARA-PROGMAG-ARTICOLO-ONLY-SONS
           when "prg-tmagaz"
                perform PREPARA-PROGMAG-TMAGAZ     |SELEZIONA PROGMAG
           when "prg-timbalqta"
                perform PREPARA-TIMBALQTA-PROGMAG   |SELEZIONA PROGMAG
           when "prg-peso"
                perform PREPARA-PESO-PROGMAG       |SELEZIONA PROGMAG
           when "tcontat"
                perform PREPARA-TCONTAT            |SELEZIONA TCONTAT
           when "tsetmerc"
                perform PREPARA-TSETMERC           |CERCA
           when "tcla1art"
                perform PREPARA-TCLA1ART           |CERCA
           when "tudm"
                perform PREPARA-TUDM               |CERCA
           when "tnomen"
                perform PREPARA-TNOMEN             |CERCA
           when "ttipodoc"                          
                perform PREPARA-TTIPODOC           |SELEZIONA TTIPODOC
           when "tmp-assorcli"
                perform PREPARA-TMP-ASSORCLI       |SELEZIONA TTIPODOC
           when "tcaumag"
                perform PREPARA-TCAUMAG            |SELEZIONA TCAUMAG
           when "tcaumag-si-mov"
                perform PREPARA-CAUSALI-SI-MOVIM
           when "tcaumag-si-mov-ordf"
                perform PREPARA-CAUSALI-SI-MOVIM-ORDF
           when "tmovmag-cau"
                perform PREPARA-CAUSALI-TMOVMAG    |CERCA
           when "tcaumag-forn"
                perform PREPARA-CAUSALI-FORNITORE        
           when "tcaumag-cli"
                perform PREPARA-CAUSALI-CLIENTE
           when "tcaumag-forn-mov-qta"
                perform PREPARA-CAUSALI-FORNITORE-MOV-QTA
           when "clienti-CF"
           when "clienti-alfa-CF"
                perform PREPARA-CLIENTI-CF         |CERCA
           when "clienti-CF-all"
           when "clienti-alfa-CF-all"
      *    I programmi di interrogazione Clienti/Fornitori deve
      *    visualizzare tutti i record indipendentemente dallo status
                set TuttiIRec to true
                perform PREPARA-CLIENTI-CF          |CERCA
           when "tordini"
                perform PREPARA-TORDINI            |CERCA
           when "teva"
                perform PREPARA-TEVA            |CERCA
           when "teva-a"
                perform PREPARA-TEVA-APERTE            |CERCA
           when "tordini-no-pren"
                perform PREPARA-TORDINI-NON-PRENOTATI
           when "tordini-si-pren"
                perform PREPARA-TORDINI-PRENOTATI
           when "tordini-bolle"
                perform PREPARA-TORDINI-BOLLE
           when "tor-fat-no-pren"
                perform PREPARA-TORDINI-FATTURE-NON-PRENOTA
           when "tor-fat-si-pren" 
                perform PREPARA-TORDINI-FATTURE-PRENOTA-NON-FAT
           when "tor-fat"
                perform PREPARA-TORDINI-FATTURE-SI-FAT         
           when "tor-fat-ord"
                perform PREPARA-TORDINI-FATTURE-SI-FAT-ORD
           when "tnotacr"
                perform PREPARA-TNOTACR            |CERCA 
           when "btnotacr"
                perform PREPARA-BTNOTACR            |CERCA
           when "tno-fat-no-pren"
                perform PREPARA-TNOTACR-FATTURE-NON-PRENOTA
           when "tno-fat-si-pren" 
                perform PREPARA-TNOTACR-FATTURE-PRENOTA-NON-FAT
           when "tno-fat"
                perform PREPARA-TNOTACR-FATTURE-SI-FAT
           when "articoli-marche"
           when "art-des-marche"
                perform PREPARA-ARTICOLI-MARCHE    |CERCA
           when "MAS"
                perform PREPARA-MASTRO
           when "tcaudisb"
                perform PREPARA-CAUSALI-DISTINTA
           when "zoom-distinteb"
                perform PREPARA-ZOOM-DISTINTEB
           when "prog"                  
                perform PREPARA-PROG    |SELEZIONA
           when "user"
                perform PREPARA-USER    |SELEZIONA
           when "user-diversi"
                perform PREPARA-USER-DIVERSI
           when "prog-l2"
                perform PREPARA-PROG-L2    |CERCA
           when "prog-l3"
                perform PREPARA-PROG-L3    |CERCA
           when "prog-l4"
                perform PREPARA-PROG-L4    |CERCA
           when "prog-l5"
                perform PREPARA-PROG-L5    |CERCA
           when "movutf"
                perform PREPARA-MOVUTF     |CERCA
           when "anautf"
                perform PREPARA-ANAUTF
           when "anautf-anno"
                perform PREPARA-ANAUTF-FILTER-ANNO |CERCA con filtro anno
           when "tmovtrat"
                perform PREPARA-TMOVTRAT
           when "lisagente"
                perform PREPARA-LISAGENTE
           when "lisagente-articolo"
                perform PREPARA-LISAGENTE-FILTER-ARTICOLO
           when "provvig"
                perform PREPARA-PROVVIG
           when "zoom-trasporti"
                perform PREPARA-TRASPORTI
           when "tarifvet"
                perform PREPARA-TARIFVET
           when "tmp-per-addebito"
           when "tmp-progmag-zoom"
           when "tmp-progmag-zoom-o"
           when "tmp-progmag-zoom-E"
                perform PREPARA-TMP-PROGMAG-ZOOM
           when "tmp-tordini-zoom"
                perform PREPARA-TMP-TORDINI-ZOOM
           when "tmp-assorcli-zoom"
                perform PREPARA-TMP-ASSORCLI-ZOOM
           when "zoom-fatture"
           when "zoom-fatture-b"
           when "zoom-note"
           when "zoom-note-fatt"
           when "zoom-note-clidest"
                perform PREPARA-ZOOM-TORDINI-FATTURE
           when "zoom-tordini"
           when "zoom-contestazioni"
                perform PREPARA-ZOOM-TORDINI
           when "zoom-tordini-b"
                perform PREPARA-ZOOM-TORDINI-BOLLE
           when "zoom-tordini+fat"
                perform PREPARA-ZOOM-TORDINI-DATI-FAT
           when "zoom-tnotacr"
                perform PREPARA-ZOOM-TNOTACR
           when "zoom-tor-bolle"
                perform PREPARA-ZOOM-TOR-BOLLE
           when "zoom-tor-fat"
                perform PREPARA-ZOOM-TOR-FAT
           when "zoom-tno-fat"
                perform PREPARA-ZOOM-TNO-FAT
           when "zoom-tor-postel"
                perform PREPARA-ZOOM-TOR-POSTEL
           when "zoom-tarifvet"
                perform PREPARA-ZOOM-TARIFVET
           when "zoom-tcaumag"
                perform PREPARA-ZOOM-TCAUMAG
           when "tagli"
                perform PREPARA-TAGLI
           when "tmp-destini"
                perform PREPARA-TMP-DESTINI
           when "zoom-fat-promo"
                perform PREPARA-FAT-PROMO
           when "zoom-tor-master"
           when "zoom-tor-master-b"
                perform PREPARA-ZOOM-TOR-MASTER
           when "zoom-tor-master-c"
                perform PREPARA-ZOOM-TOR-MASTER-C
           when "tmp-promo-prz"
           when "tmp-promo-prz2"
                perform PREPARA-TMP-PROMO-PRZ
           when "blister"
                perform PREPARA-BLISTER      
           when "tgiormag"
                perform PREPARA-TGIORMAG
           when "tpiombo"
                perform PREPARA-TPIOMBO
           when "tmp-fatturati"
                perform PREPARA-TMP-FATTURATI
           when "tescons"
                perform PREPARA-TESCONS           |CERCA
           when "tscorte"
           when "tscorte-alfa"
                perform PREPARA-TSCORTE            |CERCA
           when "tlistini"
                perform PREPARA-TLISTINI
           when "ttipoavv"
                perform PREPARA-TTIPOAVV           |CERCA
           when "tordforn"
           when "tordforn-stato"
                perform PREPARA-TORDFORN            |CERCA
           when other
                display message box "guarda che non è ancora stato fatto
      -                             "IL PARAGRAFO DI PREPARAZIONE PER QU
      -                             "ESTO FILE"
                exit program
           end-evaluate.


           call "XZOOM" using xzoom-linkage 
                                 COMO-RECORD(1:PARAM-SIZE)
                                 giving stato-zoom.

           cancel "XZOOM".          

           goback stato-zoom.

      ***---
       PREPARA-USER-PGM.
           initialize xzoom-linkage xzoom-ext-info(1).
           move  zero                   to xzoom-row.
           move  zero                   to xzoom-cln.
           move  16                     to xzoom-lw.
           move  60                     to xzoom-sw.
           move "UserPgm"               to xzoom-file-name(1).
           move  0                      to xzoom-file-key.
           move  1                      to xzoom-fields.
                                      
           move  15                     to xzoom-field-length(1).
           move  10                     to xzoom-field-offset(1).
                                      
           move  15                     to xzoom-field-column(1).
                                      
           move "Programma"             to xzoom-field-name(1).  
           set  xzoom-ft-alpha (1)      to true. 
                                      
           move  -1                     to xzoom-delimiter-offset.
           move  19                     to xzoom-delimiter-length.
           move "000"                   to xzoom-from-value.
           move "000"                   to xzoom-to-value.
                                      
           set xzoom-when-true(1)       to true.
           set xzoom-begin-with(1)      to true.
           set xzoom-ignore-case(1)     to false.
                                      
                                      
           move COMO-RECORD(1:10)       to xzoom-wild-value(1).
           move 10                      to xzoom-wild-value-length(1).
           move 10                      to xzoom-wild-length(1).
           move 0                       to xzoom-wild-offset(1).
                                      
           set xzoom-when-false(2)      to true.
           set xzoom-begin-with(2)      to true.
           set xzoom-ignore-case(2)     to false.
      *                               
           move ZERO                    to xzoom-wild-value(2).
           move 1                       to xzoom-wild-value-length(2).
           move 1                       to xzoom-wild-length(2).
           move 25                      to xzoom-wild-offset(2).  
                                    
      ***---
       PREPARA-TREGIONI.
           move zero to idx.
           initialize xzoom-linkage xzoom-ext-info(1).
           move  zero                    to xzoom-row.
           move  zero                    to xzoom-cln.
           move  16                      to xzoom-lw.
           move  90                      to xzoom-sw.
           move "tregioni"               to xzoom-file-name(1).
           move  0                       to xzoom-file-key.
           move  2                       to xzoom-fields.

      * CAMPO 1
           add 1 to idx
           move  3                       to xzoom-field-length(idx).
           move  0                       to xzoom-field-offset(idx).
           move  7                       to xzoom-field-column(idx).
           move "Codice"                 to xzoom-field-name(idx).  
           set xzoom-al-right(idx)       to true.
           set xzoom-field-unsigned(idx) to true.
           set xzoom-ft-display(idx)     to true.
           move 3                        to xzoom-field-digits(idx).
           move  0                       to xzoom-field-dec(idx).
           move "##0"                    to xzoom-field-fmt(idx).

      * CAMPO 2
           add 1 to idx
           move  30                      to xzoom-field-length(idx).
           move  3                       to xzoom-field-offset(idx).
           move  50                      to xzoom-field-column(idx).
           move "Descrizione"            to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 
                                       
           move  -1                     to xzoom-delimiter-offset.
           move  5                      to xzoom-delimiter-length.
           move "000"                   to xzoom-from-value.
           move "000"                   to xzoom-to-value.

      ***---
       PREPARA-ABI.
           move zero to idx.
           initialize xzoom-linkage xzoom-ext-info(1).
           move  zero                    to xzoom-row.
           move  zero                    to xzoom-cln.
           move  16                      to xzoom-lw.
           move  155                     to xzoom-sw.
           move "abi"                    to xzoom-file-name(1).
           move  0                       to xzoom-file-key.
           move  5                       to xzoom-fields.

      * CAMPO 1
           add   1 to idx
           move  5                       to xzoom-field-length(idx).
           move  0                       to xzoom-field-offset(idx).
           move  10                      to xzoom-field-column(idx).
           move "Codice ABI"             to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

      * CAMPO 2
           add 1 to idx
           move  5                       to xzoom-field-length(idx).
           move  5                       to xzoom-field-offset(idx).
           move  10                      to xzoom-field-column(idx).
           move "Codice CAB"             to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

      * CAMPO 3
           add 1 to idx
           move  30                      to xzoom-field-length(idx).
           move  10                      to xzoom-field-offset(idx).
           move  30                      to xzoom-field-column(idx).
           move "Banca"                  to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

      * CAMPO 4
           add 1 to idx
           move  30                      to xzoom-field-length(idx).
           move  40                      to xzoom-field-offset(idx).
           move  30                      to xzoom-field-column(idx).
           move "Sportello/Filiale"      to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 
                                       
      * CAMPO 5
           add 1 to idx
           move  30                      to xzoom-field-length(idx).
           move  135                     to xzoom-field-offset(idx).
           move  30                      to xzoom-field-column(idx).
           move "Città"                  to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

           move  -1                     to xzoom-delimiter-offset.
           move  5                      to xzoom-delimiter-length.
           move "000"                   to xzoom-from-value.
           move "000"                   to xzoom-to-value.

      ***---
       PREPARA-AGENTI.
           initialize xzoom-linkage xzoom-ext-info(1).

           evaluate como-file
           when "agenti"       move  0 to xzoom-file-key
           when "agenti-alfa"  move  1 to xzoom-file-key
           end-evaluate.

           move  zero to idx.
           move  zero                    to xzoom-row.
           move  zero                    to xzoom-cln.
           move  16                      to xzoom-lw.
           move  138                     to xzoom-sw.
           move "agenti"                 to xzoom-file-name(1).
           move  5                       to xzoom-fields.

      * CAMPO 1
           add 1 to idx
           move  5                       to xzoom-field-length(idx).
           move  0                       to xzoom-field-offset(idx).
           move  9                       to xzoom-field-column(idx).
           move "Codice"                 to xzoom-field-name(idx).  
           set xzoom-al-right(idx)       to true.
           set xzoom-field-unsigned(idx) to true.
           set xzoom-ft-display(idx)     to true.
           move 6                        to xzoom-field-digits(idx).
           move  0                       to xzoom-field-dec(idx).
           move "####0"                  to xzoom-field-fmt(idx).

      * CAMPO 2
           add 1 to idx
           move  40                      to xzoom-field-length(idx).
           move  5                       to xzoom-field-offset(idx).
           move  30                      to xzoom-field-column(idx).
           move "Ragione Sociale"        to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

      * CAMPO 3
           add 1 to idx
           move  40                      to xzoom-field-length(idx).
           move  85                      to xzoom-field-offset(idx).
           move  30                      to xzoom-field-column(idx).
           move "Indirizzo"              to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 
                                       
      * CAMPO 4
           add 1 to idx
           move  35                      to xzoom-field-length(idx).
           move  130                     to xzoom-field-offset(idx).
           move  30                      to xzoom-field-column(idx).
           move "Località"               to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

      * CAMPO 5
           add 1 to idx
           move  2                       to xzoom-field-length(idx).
           move  165                     to xzoom-field-offset(idx).
           move  4                       to xzoom-field-column(idx).
           move "Prov."                  to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

           move  -1                     to xzoom-delimiter-offset.
           move  5                      to xzoom-delimiter-length.
           move "000"                   to xzoom-from-value.
           move "000"                   to xzoom-to-value.

      ***---
       PREPARA-TMARCHE.
           initialize xzoom-linkage xzoom-ext-info(1).
      
           evaluate como-file
           when "tmarche"       move  0 to xzoom-file-key
           when "tmarche-alfa"  move  1 to xzoom-file-key
           end-evaluate.
      *
           move zero to idx.
           move  zero                    to xzoom-row.
           move  zero                    to xzoom-cln.
           move  16                      to xzoom-lw.
           move  119                     to xzoom-sw.
           move "tmarche"                to xzoom-file-name(1).
           move  6                       to xzoom-fields.
      
      * CAMPO 1
           add 1 to idx
           move  4                       to xzoom-field-length(idx).
           move  0                       to xzoom-field-offset(idx).
           move  9                       to xzoom-field-column(idx).
           move "Codice"                 to xzoom-field-name(idx).  
           set xzoom-al-right(idx)       to true.
           set xzoom-field-unsigned(idx) to true.
           set xzoom-ft-display(idx)     to true.
           move 4                        to xzoom-field-digits(idx).
           move  0                       to xzoom-field-dec(idx).
           move "###0"                   to xzoom-field-fmt(idx).
      
      * CAMPO 2
           add 1 to idx
           move  30                      to xzoom-field-length(idx).
           move  4                       to xzoom-field-offset(idx).
           move  30                      to xzoom-field-column(idx).
           move "Descrizione"            to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 
      
      * CAMPO 3
           add 1 to idx
           move  4                       to xzoom-field-length(idx).
           move  34                      to xzoom-field-offset(idx).
           move  12                      to xzoom-field-column(idx).
           move "Var. Acquisto (-)"      to xzoom-field-name(idx).  
           set xzoom-al-right(idx)       to true.
           set xzoom-field-unsigned(idx) to true.
           set xzoom-ft-display(idx)     to true.
           move 5                        to xzoom-field-digits(idx).
           move  2                       to xzoom-field-dec(idx).
           move "##,00"                  to xzoom-field-fmt(idx).
                                       
      * CAMPO 4
           add 1 to idx
           move  4                       to xzoom-field-length(idx).
           move  38                      to xzoom-field-offset(idx).
           move  12                      to xzoom-field-column(idx).
           move "Var. Acquisto (+)"      to xzoom-field-name(idx).  
           set xzoom-al-right(idx)       to true.
           set xzoom-field-unsigned(idx) to true.
           set xzoom-ft-display(idx)     to true.
           move 4                        to xzoom-field-digits(idx).
           move  2                       to xzoom-field-dec(idx).
           move "##,00"                  to xzoom-field-fmt(idx).
      
      * CAMPO 5
           add 1 to idx
           move  4                       to xzoom-field-length(idx).
           move  42                      to xzoom-field-offset(idx).
           move  12                      to xzoom-field-column(idx).
           move "Var. Vendita (-)"       to xzoom-field-name(idx).  
           set xzoom-al-right(idx)       to true.
           set xzoom-field-unsigned(idx) to true.
           set xzoom-ft-display(idx)     to true.
           move 4                        to xzoom-field-digits(idx).
           move  2                       to xzoom-field-dec(idx).
           move "##,00"                  to xzoom-field-fmt(idx).

      * CAMPO 6
           add 1 to idx
           move  4                       to xzoom-field-length(idx).
           move  46                      to xzoom-field-offset(idx).
           move  12                      to xzoom-field-column(idx).
           move "Var. Vendita (+)"       to xzoom-field-name(idx).  
           set xzoom-al-right(idx)       to true.
           set xzoom-field-unsigned(idx) to true.
           set xzoom-ft-display(idx)     to true.
           move 4                        to xzoom-field-digits(idx).
           move  2                       to xzoom-field-dec(idx).
           move "##,00"                  to xzoom-field-fmt(idx).
      
           move  -1                     to xzoom-delimiter-offset.
           move  5                      to xzoom-delimiter-length.
           move "000"                   to xzoom-from-value.
           move "000"                   to xzoom-to-value.


      ***---
       PREPARA-TPROV.
           initialize xzoom-linkage xzoom-ext-info(1).

           move zero to idx.
           move  zero                    to xzoom-row.
           move  zero                    to xzoom-cln.
           move  16                      to xzoom-lw.
           move  80                      to xzoom-sw.
           move "tprov"                  to xzoom-file-name(1).
           move  2                       to xzoom-fields.

      * CAMPO 1
           add 1 to idx
           move  2                       to xzoom-field-length(idx).
           move  0                       to xzoom-field-offset(idx).
           move  13                      to xzoom-field-column(idx).
           move "Codice"                 to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

      * CAMPO 2
           add 1 to idx
           move  30                      to xzoom-field-length(idx).
           move  2                       to xzoom-field-offset(idx).
           move  45                      to xzoom-field-column(idx).
           move "Descrizione"            to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

           move  -1                     to xzoom-delimiter-offset.
           move  5                      to xzoom-delimiter-length.
           move "000"                   to xzoom-from-value.
           move "000"                   to xzoom-to-value.


      ***---
       PREPARA-TNAZIONI.
           initialize xzoom-linkage xzoom-ext-info(1).

           move zero to idx.
           move  zero                    to xzoom-row.
           move  zero                    to xzoom-cln.
           move  16                      to xzoom-lw.
           move  80                      to xzoom-sw.
           move "tnazioni"               to xzoom-file-name(1).
           move  2                       to xzoom-fields.

      * CAMPO 1
           add 1 to idx
           move  3                       to xzoom-field-length(idx).
           move  0                       to xzoom-field-offset(idx).
           move  15                      to xzoom-field-column(idx).
           move "Codice"                 to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

      * CAMPO 2
           add 1 to idx
           move  30                      to xzoom-field-length(idx).
           move  3                       to xzoom-field-offset(idx).
           move  43                      to xzoom-field-column(idx).
           move "Descrizione"            to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

           move  -1                     to xzoom-delimiter-offset.
           move  5                      to xzoom-delimiter-length.
           move "000"                   to xzoom-from-value.
           move "000"                   to xzoom-to-value.

      ***---
       PREPARA-TCODPAG.
           initialize xzoom-linkage xzoom-ext-info(1).

           move zero to idx.
           move  zero                    to xzoom-row.
           move  zero                    to xzoom-cln.
           move  16                      to xzoom-lw.
           move  90                      to xzoom-sw.
           move "TBL"                    to xzoom-file-name(1).
           move  0                       to xzoom-file-key.

      * CAMPO 1
           add 1 to idx
           move  3                       to xzoom-field-length(idx).
           move  2                       to xzoom-field-offset(idx).
           move  6                       to xzoom-field-column(idx).
           move "Codice"                 to xzoom-field-name(idx).  
           set xzoom-al-right(idx)       to true.
           set xzoom-field-unsigned(idx) to true.
           set xzoom-ft-display(idx)     to true.
           set xzoom-ft-alpha(idx)      to true. 

      * CAMPO 2
           add 1 to idx
           move  30                      to xzoom-field-length(idx).
           move  22                      to xzoom-field-offset(idx).
           move  30                      to xzoom-field-column(idx).
           move "Descrizione"            to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

      * CAMPO 3
           add 1 to idx
           move  30                      to xzoom-field-length(idx).
           move  52                      to xzoom-field-offset(idx).
           move  30                      to xzoom-field-column(idx).
      *     move "Descrizione"            to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

           move  idx                       to xzoom-fields.

           move  -1                     to xzoom-delimiter-offset.
           move  5                      to xzoom-delimiter-length.
           move "000"                   to xzoom-from-value.
           move "000"                   to xzoom-to-value.

           set  xzoom-begin-with(1)   to true
           set  xzoom-when-true(1)    to true
           set  xzoom-ignore-case(1)  to false

           move "PA"                  to xzoom-wild-value(1).
           move 2                     to xzoom-wild-value-length(1).
           move 2                     to xzoom-wild-length(1).
           move 0                     to xzoom-wild-offset(1).

      ***---
       PREPARA-TBLCO.
           initialize xzoom-linkage xzoom-ext-info(1).

           move zero to idx.
           move  zero                    to xzoom-row.
           move  zero                    to xzoom-cln.
           move  16                      to xzoom-lw.
           move  90                      to xzoom-sw.
           move "TBL"                    to xzoom-file-name(1).
           move  0                       to xzoom-file-key.

      * CAMPO 1
           add 1 to idx
           move  3                       to xzoom-field-length(idx).
           move  2                       to xzoom-field-offset(idx).
           move  6                       to xzoom-field-column(idx).
           move "Codice"                 to xzoom-field-name(idx).  
           set xzoom-al-right(idx)       to true.
           set xzoom-field-unsigned(idx) to true.
           set xzoom-ft-display(idx)     to true.
           set xzoom-ft-alpha(idx)      to true. 

      * CAMPO 2
           add 1 to idx
           move  30                      to xzoom-field-length(idx).
           move  22                      to xzoom-field-offset(idx).
           move  30                      to xzoom-field-column(idx).
           move "Descrizione"            to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

      * CAMPO 3
           add 1 to idx
           move  30                      to xzoom-field-length(idx).
           move  52                      to xzoom-field-offset(idx).
           move  30                      to xzoom-field-column(idx).
      *     move "Descrizione"            to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

           move  idx                       to xzoom-fields.

           move  -1                     to xzoom-delimiter-offset.
           move  5                      to xzoom-delimiter-length.
           move "000"                   to xzoom-from-value.
           move "000"                   to xzoom-to-value.

           set  xzoom-begin-with(1)   to true
           set  xzoom-when-true(1)    to true
           set  xzoom-ignore-case(1)  to false

           move "CO"                  to xzoom-wild-value(1).
           move 2                     to xzoom-wild-value-length(1).
           move 2                     to xzoom-wild-length(1).
           move 0                     to xzoom-wild-offset(1).

      ***---
       PREPARA-TIVAESE.
           initialize xzoom-linkage xzoom-ext-info(1).

           move  0                       to xzoom-file-key.

           move  zero                    to idx.
           move  zero                    to xzoom-row.
           move  zero                    to xzoom-cln.
           move  16                      to xzoom-lw.
           move  112                     to xzoom-sw.
           move "TBL"                    to xzoom-file-name(1).

      * CAMPO 1
           add 1 to idx
           move  3                       to xzoom-field-length(idx).
           move  2                       to xzoom-field-offset(idx).
           move  6                       to xzoom-field-column(idx).
           move "Codice"                 to xzoom-field-name(idx).  
           set xzoom-al-right(idx)       to true.
           set xzoom-field-unsigned(idx) to true.
           set xzoom-ft-display(idx)     to true.
           set xzoom-ft-alpha(idx)       to true. 

      * CAMPO 2
           add 1 to idx
           move  30                      to xzoom-field-length(idx).
           move  22                      to xzoom-field-offset(idx).
           move  30                      to xzoom-field-column(idx).
           move "Descrizione"            to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

      * CAMPO 3
           add 1 to idx
           move  30                      to xzoom-field-length(idx).
           move  52                      to xzoom-field-offset(idx).
           move  30                      to xzoom-field-column(idx).
      *     move "Descrizione"            to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

      * CAMPO 4
           add 1 to idx
           move  5                       to xzoom-field-length(idx).
           move  82                      to xzoom-field-offset(idx).
           move   9                      to xzoom-field-column(idx).
           move "Percentuale"            to xzoom-field-name(idx).  
           set xzoom-al-right(idx)       to true.
           set xzoom-field-signed(idx)   to true.
           set xzoom-ft-display(idx)     to true.
           move 5                        to xzoom-field-digits(idx).
           move  2                       to xzoom-field-dec(idx).
           move "##0,00"                 to xzoom-field-fmt(idx).

      * CAMPO 5
           add 1 to idx
           move  1                       to xzoom-field-length(idx).
           move  96                      to xzoom-field-offset(idx).
           move  8                       to xzoom-field-column(idx).
           move "Esenzione"              to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 
           set  xzoom-al-center(idx)     to true.

           move  idx                     to xzoom-fields.

           move  -1                     to xzoom-delimiter-offset.
           move  5                      to xzoom-delimiter-length.
           move "000"                   to xzoom-from-value.
           move "000"                   to xzoom-to-value.

           set  xzoom-begin-with(1)   to true
           set  xzoom-when-true(1)    to true
           set  xzoom-ignore-case(1)  to false

           move "IV"                  to xzoom-wild-value(1).
           move 2                     to xzoom-wild-value-length(1).
           move 2                     to xzoom-wild-length(1).
           move 0                     to xzoom-wild-offset(1).

      *    solo per i gclienti faccio vedere le solo i tipi esenzione
           evaluate como-file
           when "tivaese-ese"
                set  xzoom-equal(2)        to true
                set  xzoom-when-true(2)    to true
                set  xzoom-ignore-case(2)  to false

                move "00000"               to xzoom-wild-value(2)
                move 5                     to xzoom-wild-value-length(2)
                move 5                     to xzoom-wild-length(2)
                move 82                    to xzoom-wild-offset(2)
           when "tivaese-non"
                set  xzoom-equal(2)        to true
                set  xzoom-when-false(2)   to true
                set  xzoom-ignore-case(2)  to false

                move "00000"               to xzoom-wild-value(2)
                move 5                     to xzoom-wild-value-length(2)
                move 5                     to xzoom-wild-length(2)
                move 82                    to xzoom-wild-offset(2)
           end-evaluate.

      ***---
       PREPARA-CLIENTI.
           initialize xzoom-linkage xzoom-ext-info(1).
      
           move  2   to xzoom-file-key.
      *
           move  0                       to idx.
           move  0                       to xzoom-row.
           move  0                       to xzoom-cln.
           move  16                      to xzoom-lw.
           move  158                     to xzoom-sw.
           move "clienti"                to xzoom-file-name(1).
           move  6                       to xzoom-fields.
      
      * CAMPO 1
           add 1 to idx
           move  5                       to xzoom-field-length(idx).
           move  1                       to xzoom-field-offset(idx).
           move  7                       to xzoom-field-column(idx).
           move "Codice"                 to xzoom-field-name(idx).  
           set xzoom-al-right(idx)       to true.
           set xzoom-field-unsigned(idx) to true.
           set xzoom-ft-display(idx)     to true.
           move 5                        to xzoom-field-digits(idx).
           move  0                       to xzoom-field-dec(idx).
           move "####0"                  to xzoom-field-fmt(idx).
      
      * CAMPO 2
           add 1 to idx
           move  40                      to xzoom-field-length(idx).
           move  6                       to xzoom-field-offset(idx).
           move  40                      to xzoom-field-column(idx).
           move "Ragione Sociale"        to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 
      
      * CAMPO 3
           add 1 to idx
           move  40                      to xzoom-field-length(idx).
           move  86                      to xzoom-field-offset(idx).
           move  30                      to xzoom-field-column(idx).
           move "Indirizzo"              to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 
                                       
      * CAMPO 4
           add 1 to idx
           move  5                       to xzoom-field-length(idx).
           move  126                     to xzoom-field-offset(idx).
           move  6                      to xzoom-field-column(idx).
           move "C.A.P."                 to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 
      
      * CAMPO 5
           add 1 to idx
           move  35                      to xzoom-field-length(idx).
           move  131                     to xzoom-field-offset(idx).
           move  30                      to xzoom-field-column(idx).
           move "Località"               to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

      * CAMPO 6
           add 1 to idx
           move  2                       to xzoom-field-length(idx).
           move  166                     to xzoom-field-offset(idx).
           move  7                      to xzoom-field-column(idx).
           move "Provincia"              to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 
      
      * FILTRA I RECORD SOLO SU CLIENTI
           set  xzoom-begin-with(1)      to true.
           set  xzoom-when-true(1)       to true.
           set  xzoom-ignore-case(1)     to false.
           
           evaluate como-file
           when "clienti-all"
           when "clienti-alfa-all"
                move como-record(1:1)         to xzoom-wild-value(1)
           when other
                move "C"                      to xzoom-wild-value(1)
           end-evaluate.

           move 1                        to xzoom-wild-value-length(1).
           move 1                        to xzoom-wild-length(1).
           move 0                        to xzoom-wild-offset(1).

      *    Il programma di gestione Clienti/Fornitori deve visualizzare
      *    tutti i record indipendentemente dallo status
           if not TuttiIRec
              |FILTRO SU RECORD ATTIVI
              move "A" to como-record(744:1)

              set xzoom-when-true(2)    to true
              set xzoom-begin-with(2)   to true
              set xzoom-ignore-case(2)  to true
                                 
              move como-record(744:1)   to xzoom-wild-value(2)
              move 1                    to xzoom-wild-value-length(2)
              move 1                    to xzoom-wild-length(2)
              move 743                  to xzoom-wild-offset(2)
           end-if

           move  -1                     to xzoom-delimiter-offset.
           move  5                      to xzoom-delimiter-length.
           move "000"                   to xzoom-from-value.
           move "000"                   to xzoom-to-value.

      ***---
      * QUESTO PRF. SEPARA I RECORD DEL FILE CLIENTI TRA
      * CLIENTI E FORNITORI A SECONDA DEL TIPO (TIPO-CF) SETTATO
       PREPARA-CLIENTI-CF.
           initialize xzoom-linkage xzoom-ext-info(1).

           move -1                       to xzoom-delimiter-offset.

           move 2 to xzoom-file-key.
      *
           move  zero to idx.
           move  zero                    to xzoom-row.
           move  zero                    to xzoom-cln.
           move  16                      to xzoom-lw.
           move  158                     to xzoom-sw.
           move "clienti"                to xzoom-file-name(1).
           move  6                       to xzoom-fields.
      
      * CAMPO 1
           add 1 to idx
           move  5                       to xzoom-field-length(idx).
           move  1                       to xzoom-field-offset(idx).
           move  7                       to xzoom-field-column(idx).
           move "Codice"                 to xzoom-field-name(idx).  
           set xzoom-al-right(idx)       to true.
           set xzoom-field-unsigned(idx) to true.
           set xzoom-ft-display(idx)     to true.
           move 5                        to xzoom-field-digits(idx).
           move  0                       to xzoom-field-dec(idx).
           move "####0"                  to xzoom-field-fmt(idx).

      * CAMPO 2
           add 1 to idx
           move  40                      to xzoom-field-length(idx).
           move  6                       to xzoom-field-offset(idx).
           move  40                      to xzoom-field-column(idx).
           move "Ragione Sociale"        to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 
      
      * CAMPO 3
           add 1 to idx
           move  40                      to xzoom-field-length(idx).
           move  86                      to xzoom-field-offset(idx).
           move  30                      to xzoom-field-column(idx).
           move "Indirizzo"              to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 
      
      * CAMPO 4
           add 1 to idx
           move   5                      to xzoom-field-length(idx).
           move  126                     to xzoom-field-offset(idx).
           move  6                       to xzoom-field-column(idx).
           move "C.A.P."                 to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 
      
      * CAMPO 5
           add 1 to idx
           move  35                      to xzoom-field-length(idx).
           move  131                     to xzoom-field-offset(idx).
           move  30                      to xzoom-field-column(idx).
           move "Località"               to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true.                   
      
      * CAMPO 6
           add 1 to idx
           move   2                      to xzoom-field-length(idx).
           move  166                     to xzoom-field-offset(idx).
           move  7                       to xzoom-field-column(idx).
           move "Provincia"              to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

           move como-record(1:1)         to xzoom-wild-value(1).
           move 1                        to xzoom-wild-value-length(1).
           move 1                        to xzoom-wild-length(1).
           move 0                        to xzoom-wild-offset(1).

           set  xzoom-begin-with(1)      to true.
           set  xzoom-when-true(1)       to true.
           set  xzoom-ignore-case(1)     to false.
                                                        
           if not TuttiIRec

              |FILTRO SU RECORD ATTIVI
              move "A" to como-record(744:1)

              set xzoom-when-true(2)    to true
              set xzoom-begin-with(2)   to true 
              set xzoom-ignore-case(2)  to true
                                 
              move como-record(744:1)   to xzoom-wild-value(2)
              move 1                    to xzoom-wild-value-length(2)
              move 1                    to xzoom-wild-length(2)
              move 743                  to xzoom-wild-offset(2)

           end-if.

      ***---
       PREPARA-TTIPOCLI.
           initialize xzoom-linkage xzoom-ext-info(1).

           move zero to idx.
           move  zero                    to xzoom-row.
           move  zero                    to xzoom-cln.
           move  16                      to xzoom-lw.
           move  109                     to xzoom-sw.
           move "ttipocli"               to xzoom-file-name(1).
           move  0                       to xzoom-file-key.
           move  6                       to xzoom-fields.

      * CAMPO 1
           add 1 to idx
           move  2                       to xzoom-field-length(idx).
           move  0                       to xzoom-field-offset(idx).
           move  10                      to xzoom-field-column(idx).
           move "Codice"                 to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

      * CAMPO 2
           add 1 to idx
           move  30                      to xzoom-field-length(idx).
           move  2                       to xzoom-field-offset(idx).
           move  39                      to xzoom-field-column(idx).
           move "Descrizione"            to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

      * CAMPO 3
           add 1 to idx
           move  1                       to xzoom-field-length(idx).
           move  32                      to xzoom-field-offset(idx).
           move  7                       to xzoom-field-column(idx).
           move "Agente"                 to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

      * CAMPO 4
           add 1 to idx
           move  1                       to xzoom-field-length(idx).
           move  33                      to xzoom-field-offset(idx).
           move  7                       to xzoom-field-column(idx).
           move "GDO"                    to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

      * CAMPO 5
           add 1 to idx
           move  1                       to xzoom-field-length(idx).
           move  34                      to xzoom-field-offset(idx).
           move  10                      to xzoom-field-column(idx).
           move "Tratt. Imposta"         to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

      * CAMPO 6
           add 1 to idx
           move  1                       to xzoom-field-length(idx).
           move  35                      to xzoom-field-offset(idx).
           move  10                      to xzoom-field-column(idx).
           move "Gestione Fido"          to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

           move  -1                     to xzoom-delimiter-offset.
           move  1                      to xzoom-delimiter-length.
           move "000"                   to xzoom-from-value.
           move "000"                   to xzoom-to-value.


      ***---
       PREPARA-TGRUPGDO.
           initialize xzoom-linkage xzoom-ext-info(1).

           move zero to idx.
           move  zero                    to xzoom-row.
           move  zero                    to xzoom-cln.
           move  16                      to xzoom-lw.
           move  80                      to xzoom-sw.
           move "tgrupgdo"               to xzoom-file-name(1).
           move  0                       to xzoom-file-key.
           move  2                       to xzoom-fields.

      * CAMPO 1
           add 1 to idx
           move  5                       to xzoom-field-length(idx).
           move  0                       to xzoom-field-offset(idx).
           move  13                      to xzoom-field-column(idx).
           move "Gruppo"                 to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

      * CAMPO 2
           add 1 to idx
           move  30                      to xzoom-field-length(idx).
           move  5                       to xzoom-field-offset(idx).
           move  45                      to xzoom-field-column(idx).
           move "Descrizione"            to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

           move  -1                     to xzoom-delimiter-offset.
           move  5                      to xzoom-delimiter-length.
           move "000"                   to xzoom-from-value.
           move "000"                   to xzoom-to-value.

      ***---
       PREPARA-TVETTORI.
           initialize xzoom-linkage xzoom-ext-info(1).

           evaluate como-file
           when "tvettori"        move 0 to xzoom-file-key
           when "tvettori-alfa"   move 1 to xzoom-file-key
           end-evaluate.

           move zero to idx.
           move  zero                    to xzoom-row.
           move  zero                    to xzoom-cln.
           move  16                      to xzoom-lw.
           move  95                      to xzoom-sw.
           move "tvettori"               to xzoom-file-name(1).
           move  3                       to xzoom-fields.

      * CAMPO 1
           add 1 to idx
           move  5                       to xzoom-field-length(idx).
           move  0                       to xzoom-field-offset(idx).
           set xzoom-al-right(idx)       to true.
           set xzoom-field-unsigned(idx) to true.
           set xzoom-ft-display(idx)     to true.
           move 5                        to xzoom-field-digits(idx).
           move 0                        to xzoom-field-dec(idx).
           move "####0"                  to xzoom-field-fmt(idx).
           move  10                      to xzoom-field-column(idx).
           move "Codice"                 to xzoom-field-name(idx).  

      * CAMPO 2
           add 1 to idx
           move  40                      to xzoom-field-length(idx).
           move  5                       to xzoom-field-offset(idx).
           move  30                      to xzoom-field-column(idx).
           move "Descrizione"            to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

      * CAMPO 3
           add 1 to idx
           move  40                      to xzoom-field-length(idx).
           move  45                      to xzoom-field-offset(idx).
           move  30                      to xzoom-field-column(idx).
           move "Indirizzo"              to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

           move  -1                     to xzoom-delimiter-offset.
           move  5                      to xzoom-delimiter-length.
           move "000"                   to xzoom-from-value.
           move "000"                   to xzoom-to-value.

      ***---
       PREPARA-VIDEO-DESTINI.
           initialize xzoom-linkage xzoom-ext-info(1).

           evaluate como-file
           when "destini"       move 0 to xzoom-file-key
           when "destini-alfa"  move 1 to xzoom-file-key
           end-evaluate.

           move  zero to idx.
           move  zero                    to xzoom-row.
           move  zero                    to xzoom-cln.
           move  16                      to xzoom-lw.
           move  130                     to xzoom-sw.
           move "destini"                to xzoom-file-name(1).
           move "clienti"                to xzoom-file-name(2).
           move  5                       to xzoom-fields.

      ** CAMPO 1
      *     add   1 to idx.
      *     move  1                       to xzoom-field-file(Idx).
      *     move  0                       to xzoom-field-rel(Idx).      
      *     move  0                       to xzoom-field-offset(idx).
      *     move  8                       to xzoom-field-column(idx).      
      *     move  5                       to xzoom-field-length(idx).
      *     move "Cliente"                to xzoom-field-name(idx).  
      *     set xzoom-al-right(idx)       to true.
      *     set xzoom-field-unsigned(idx) to true.
      *     set xzoom-ft-display(idx)     to true.
      *     move 5                        to xzoom-field-digits(idx).
      *     move 0                        to xzoom-field-dec(idx).
      *     move "####0"                  to xzoom-field-fmt(idx).
      *
      ** CAMPO 2
      *     add   1 to idx.
      *     move  2                       to xzoom-field-file(Idx).
      *     move  1                       to xzoom-field-rel(Idx).      
      *     move  40                      to xzoom-field-length(idx).
      *     move  6                       to xzoom-field-offset(idx).
      *     move  30                      to xzoom-field-column(idx).
      *     move "Ragione Sociale"        to xzoom-field-name(idx).  
      *     set xzoom-ft-alpha(idx)       to true.

      * CAMPO 3
           add 1 to idx.
           move  1                       to xzoom-field-file(Idx).
           move  0                       to xzoom-field-rel(Idx).      
           move  5                       to xzoom-field-length(idx).
           move  5                       to xzoom-field-offset(idx).
           move  6                       to xzoom-field-column(idx).
           move "Destino"                to xzoom-field-name(idx).  
           set xzoom-al-right(idx)       to true.
           set xzoom-field-unsigned(idx) to true.
           set xzoom-ft-display(idx)     to true.
           move 5                        to xzoom-field-digits(idx).
           move  0                       to xzoom-field-dec(idx).
           move "####0"                  to xzoom-field-fmt(idx).

      * CAMPO 4
           add   1 to idx.
           move  1                       to xzoom-field-file(Idx).
           move  0                       to xzoom-field-rel(Idx).      
           move  100                     to xzoom-field-length(idx).
           move  10                      to xzoom-field-offset(idx).
           move  30                      to xzoom-field-column(idx).
           move "Ragione Sociale"        to xzoom-field-name(idx).  
           set xzoom-ft-alpha(idx)       to true.

      * CAMPO 5
           add 1 to idx.
           move  1                       to xzoom-field-file(Idx).
           move  0                       to xzoom-field-rel(Idx).      
           move  40                      to xzoom-field-length(idx).
           move  210                     to xzoom-field-offset(idx).
           move  25                      to xzoom-field-column(idx).
           move "Indirizzo"              to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

      ** CAMPO 6
      *     add 1 to idx.
      *     move  1                       to xzoom-field-file(Idx).
      *     move  0                       to xzoom-field-rel(Idx).      
      *     move  5                       to xzoom-field-length(idx).
      *     move  130                     to xzoom-field-offset(idx).
      *     move  7                       to xzoom-field-column(idx).
      *     move "C.A.P."                 to xzoom-field-name(idx).  
      *     set  xzoom-ft-alpha(idx)      to true. 

      * CAMPO 7
           add 1 to idx.
           move  1                       to xzoom-field-file(Idx).
           move  0                       to xzoom-field-rel(Idx).
           move  35                      to xzoom-field-length(idx).
           move  255                     to xzoom-field-offset(idx).
           move  30                      to xzoom-field-column(idx).
           move "Località"               to xzoom-field-name(idx).
           set  xzoom-ft-alpha(idx)      to true.

      * CAMPO 8
           add 1 to idx.
           move  1                       to xzoom-field-file(Idx).
           move  0                       to xzoom-field-rel(Idx).
           move  2                       to xzoom-field-length(idx).
           move  290                     to xzoom-field-offset(idx).
           move  5                       to xzoom-field-column(idx).
           move "Prov."                  to xzoom-field-name(idx).
           set  xzoom-ft-alpha(idx)      to true.
      *
      **    File Reference Settings - Relazione destini > clienti
      *     move   1                to Idx.
      *     move   1                to xzoom-ref-m-file (Idx).
      *     move   2                to xzoom-ref-s-file (Idx).
      *     move   0                to xzoom-ref-s-key  (Idx).
      *     move   1                to xzoom-ref-fields (Idx).
      *     set xzoom-ref-join-outer(Idx) to true.
      *     
      **            Master File Settings
      *     move 5 to xzoom-ref-m-length(Idx, 1).
      *     move 0 to xzoom-ref-m-offset (Idx, 1).
      *     set xzoom-ref-m-unsigned (Idx, 1) to true.
      *     set xzoom-ref-m-alpha    (Idx, 1) to true.
      *     
      **            Slave File Settings
      *     move 5 to xzoom-ref-s-length(Idx, 1).
      *     move 1 to xzoom-ref-s-offset (Idx, 1).
      *     set xzoom-ref-s-unsigned (Idx, 1) to true.
      *     set xzoom-ref-s-alpha    (Idx, 1) to true.

           move  -1                      to xzoom-delimiter-offset.
           move  5                       to xzoom-delimiter-length.
           move "000"                    to xzoom-from-value.
           move "000"                    to xzoom-to-value. 

           move 0 to idx.

      *****     if not TuttiIRec
      *****        |FILTRO SU RECORD ATTIVI
      *****        add 1 to idx
      *****        move "A" to como-record(288:1)
      *****
      *****        set xzoom-when-true(idx)    to true
      *****        set xzoom-begin-with(idx)   to true
      *****        set xzoom-ignore-case(idx)  to true
      *****                           
      *****        move como-record(288:1)   to xzoom-wild-value(idx)
      *****        move 1                    to xzoom-wild-value-length(idx)
      *****        move 1                    to xzoom-wild-length(idx)
      *****        move 287                  to xzoom-wild-offset(idx)
      *****     end-if.

      ***---
       PREPARA-CITTA-DESTINI.
           initialize xzoom-linkage xzoom-ext-info(1).

           move 2 to xzoom-file-key.

           move  zero to idx.
           move  zero                    to xzoom-row.
           move  zero                    to xzoom-cln.
           move  16                      to xzoom-lw.
           move  137                     to xzoom-sw.
           move "destini"                to xzoom-file-name(1).
           move  6                       to xzoom-fields.


      * CAMPO 1
           add 1 to idx.
           move  1                       to xzoom-field-file(Idx).
           move  0                       to xzoom-field-rel(Idx).      
           move  5                       to xzoom-field-length(idx).
           move  5                       to xzoom-field-offset(idx).
           move  6                       to xzoom-field-column(idx).
           move "Destino"                to xzoom-field-name(idx).  
           set xzoom-al-right(idx)       to true.
           set xzoom-field-unsigned(idx) to true.
           set xzoom-ft-display(idx)     to true.
           move 5                        to xzoom-field-digits(idx).
           move  0                       to xzoom-field-dec(idx).
           move "####0"                  to xzoom-field-fmt(idx).

      * CAMPO 2
           add   1 to idx.
           move  1                       to xzoom-field-file(Idx).
           move  0                       to xzoom-field-rel(Idx).      
           move  100                     to xzoom-field-length(idx).
           move  10                      to xzoom-field-offset(idx).
           move  30                      to xzoom-field-column(idx).
           move "Ragione Sociale"        to xzoom-field-name(idx).  
           set xzoom-ft-alpha(idx)       to true.

      * CAMPO 3
           add 1 to idx.
           move  1                       to xzoom-field-file(Idx).
           move  0                       to xzoom-field-rel(Idx).      
           move  40                      to xzoom-field-length(idx).
           move  90                      to xzoom-field-offset(idx).
           move  210                     to xzoom-field-column(idx).
           move "Indirizzo"              to xzoom-field-name(idx).  

           set  xzoom-ft-alpha(idx)      to true. 

      * CAMPO 4
           add 1 to idx.
           move  1                       to xzoom-field-file(Idx).
           move  0                       to xzoom-field-rel(Idx).
           move  35                      to xzoom-field-length(idx).
           move  255                     to xzoom-field-offset(idx).
           move  30                      to xzoom-field-column(idx).
           move "Località"               to xzoom-field-name(idx).
           set  xzoom-ft-alpha(idx)      to true.

      * CAMPO 5
           add 1 to idx.
           move  1                       to xzoom-field-file(Idx).
           move  0                       to xzoom-field-rel(Idx).
           move  2                       to xzoom-field-length(idx).
           move  290                     to xzoom-field-offset(idx).
           move  5                       to xzoom-field-column(idx).
           move "Prov."                  to xzoom-field-name(idx).
           set  xzoom-ft-alpha(idx)      to true.

      * CAMPO 6
           add 1 to idx.
           move  1                       to xzoom-field-file(Idx).
           move  0                       to xzoom-field-rel(Idx).      
           move  5                       to xzoom-field-length(idx).
           move  250                     to xzoom-field-offset(idx).
           move  7                       to xzoom-field-column(idx).
           move "C.A.P."                 to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

           move  -1                      to xzoom-delimiter-offset.
           move  5                       to xzoom-delimiter-length.
           move "000"                    to xzoom-from-value.
           move "000"                    to xzoom-to-value.

           |FILTRO SUL CLIENTE
           if como-record(1:5) not = "00000"
              move como-record(1:5)        to xzoom-wild-value(1)
              move 05                      to xzoom-wild-value-length(1)
              move 05                      to xzoom-wild-length(1)
              move 0                       to xzoom-wild-offset(1)
              set xzoom-when-true(1)       to true
              set xzoom-begin-with(1)      to true
              set xzoom-ignore-case(1)     to false
           end-if. 

      *****     |FILTRO SU RECORD ATTIVI
      *****     move "A" to como-record(288:1).
      *****
      *****
      *****     set xzoom-when-true(2)    to true.
      *****     set xzoom-begin-with(2)   to true.
      *****     set xzoom-ignore-case(2)  to true.
      *****                           
      *****     move como-record(288:1)   to xzoom-wild-value(2).
      *****     move 1                    to xzoom-wild-value-length(2).
      *****     move 1                    to xzoom-wild-length(2).
      *****     move 287                  to xzoom-wild-offset(2).

      ***---
       PREPARA-VIDEO-DESTINIF.
           initialize xzoom-linkage xzoom-ext-info(1).

           evaluate como-file
           when "destinif"       
           when "destinif-forn"
                 move 0 to xzoom-file-key
           when "destinif-alfa"
                 move 1 to xzoom-file-key
           end-evaluate.

           move  zero to idx.
           move  zero                    to xzoom-row.
           move  zero                    to xzoom-cln.
           move  16                      to xzoom-lw.
           move  150                     to xzoom-sw.
           move "destinif"               to xzoom-file-name(1).
           move  7                       to xzoom-fields.

      * CAMPO 1
           add   1 to idx.
           move  1                       to xzoom-field-file(Idx).
           move  0                       to xzoom-field-rel(Idx).
           move  0                       to xzoom-field-offset(idx).
           move  10                      to xzoom-field-column(idx).
           move  5                       to xzoom-field-length(idx).
           move "Fornitore"              to xzoom-field-name(idx).
           set xzoom-al-right(idx)       to true.
           set xzoom-field-unsigned(idx) to true.
           set xzoom-ft-display(idx)     to true.
           move 5                        to xzoom-field-digits(idx).
           move 0                        to xzoom-field-dec(idx).
           move "####0"                  to xzoom-field-fmt(idx).

      * CAMPO 3
           add 1 to idx.
           move  1                       to xzoom-field-file(Idx).
           move  0                       to xzoom-field-rel(Idx).
           move  5                       to xzoom-field-length(idx).
           move  5                       to xzoom-field-offset(idx).
           move  6                       to xzoom-field-column(idx).
           move "Destino"                to xzoom-field-name(idx).
           set xzoom-al-right(idx)       to true.
           set xzoom-field-unsigned(idx) to true.
           set xzoom-ft-display(idx)     to true.
           move 5                        to xzoom-field-digits(idx).
           move  0                       to xzoom-field-dec(idx).
           move "####0"                  to xzoom-field-fmt(idx).

      * CAMPO 4
           add   1 to idx.
           move  1                       to xzoom-field-file(Idx).
           move  0                       to xzoom-field-rel(Idx).      
           move  40                      to xzoom-field-length(idx).
           move  10                      to xzoom-field-offset(idx).
           move  30                      to xzoom-field-column(idx).
           move "Ragione Sociale"        to xzoom-field-name(idx).  
           set xzoom-ft-alpha(idx)       to true.

      * CAMPO 5
           add 1 to idx.
           move  1                       to xzoom-field-file(Idx).
           move  0                       to xzoom-field-rel(Idx).      
           move  40                      to xzoom-field-length(idx).
           move  90                      to xzoom-field-offset(idx).
           move  25                      to xzoom-field-column(idx).
           move "Indirizzo"              to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

      * CAMPO 6
           add 1 to idx.
           move  1                       to xzoom-field-file(Idx).
           move  0                       to xzoom-field-rel(Idx).      
           move  5                       to xzoom-field-length(idx).
           move  130                     to xzoom-field-offset(idx).
           move  7                       to xzoom-field-column(idx).
           move "C.A.P."                 to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

      * CAMPO 7
           add 1 to idx.
           move  1                       to xzoom-field-file(Idx).
           move  0                       to xzoom-field-rel(Idx).      
           move  35                      to xzoom-field-length(idx).
           move  135                     to xzoom-field-offset(idx).
           move  30                      to xzoom-field-column(idx).
           move "Località"               to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

      * CAMPO 8
           add 1 to idx.
           move  1                       to xzoom-field-file(Idx).
           move  0                       to xzoom-field-rel(Idx).      
           move  2                       to xzoom-field-length(idx).
           move  170                     to xzoom-field-offset(idx).
           move  5                       to xzoom-field-column(idx).
           move "Prov."                  to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true.    

           evaluate como-file
           when "destinif-forn"
           |FILTRO SU RECORD ATTIVI
                move 1 to idx
                set xzoom-when-true(idx)    to true
                set xzoom-begin-with(idx)   to true
                set xzoom-ignore-case(idx)  to true
                                 
                move como-record(1:5)   to xzoom-wild-value(idx)
                move 5                  to xzoom-wild-value-length(idx)
                move 5                  to xzoom-wild-length(idx)
                move 0                  to xzoom-wild-offset(idx)
           end-evaluate.

           move  -1                      to xzoom-delimiter-offset.
           move  5                       to xzoom-delimiter-length.
           move "000"                    to xzoom-from-value.
           move "000"                    to xzoom-to-value.

      ***---
       PREPARA-ARTICOLI.
           initialize xzoom-linkage xzoom-ext-info(1).

           evaluate como-file
           when "articoli-marche"     move 1 to xzoom-file-key
           when "articoli"            move 1 to xzoom-file-key
           when "articoli-alfa"       move 1 to xzoom-file-key
           when "articoli-all"        move 1 to xzoom-file-key
           when "articoli-alfa-all"   move 1 to xzoom-file-key
           when "art-des-marche"      move 1 to xzoom-file-key
           when "articoli-alfa-all"   move 1 to xzoom-file-key
           when "art-des-marche-all"  move 1 to xzoom-file-key
           end-evaluate.

           move  zero to idx.
           move  zero                    to xzoom-row.
           move  zero                    to xzoom-cln.
           move  16                      to xzoom-lw.
           move  143                     to xzoom-sw.
           move "articoli"               to xzoom-file-name(1).
           move "timbalqta"              to xzoom-file-name(2).
           move "timballi"               to xzoom-file-name(3).
           move  7                       to xzoom-fields.

      * CAMPO 1
           add 1 to idx
           move  1                       to xzoom-field-file(Idx).
           move  0                       to xzoom-field-rel(Idx).      
           move  6                       to xzoom-field-length(idx).
           move  0                       to xzoom-field-offset(idx).
           move  9                       to xzoom-field-column(idx).
           move "Codice"                 to xzoom-field-name(idx).  
           set xzoom-al-right(idx)       to true.
           set xzoom-field-unsigned(idx) to true.
           set xzoom-ft-display(idx)     to true.
           move 6                        to xzoom-field-digits(idx).
           move  0                       to xzoom-field-dec(idx).
           move "#####0"                 to xzoom-field-fmt(idx).

      * CAMPO 2
           add 1 to idx.
           move  1                       to xzoom-field-file(Idx).
           move  0                       to xzoom-field-rel(Idx).      
           move  50                      to xzoom-field-length(idx).
           move  6                       to xzoom-field-offset(idx).
           move  50                      to xzoom-field-column(idx).
           move "Descrizione"            to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true.                

      * CAMPO 3
           add 1 to idx.
           move    1                     to xzoom-field-file(Idx).
           move    0                     to xzoom-field-rel(Idx).      
           move    3                     to xzoom-field-length(idx).
           move  104                     to xzoom-field-offset(idx).
           move    9                     to xzoom-field-column(idx).
           move "Imballo Std."           to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true.

      * CAMPO 4
           add   1 to idx.
           move  3                       to xzoom-field-file(Idx).
           move  2                       to xzoom-field-rel(Idx).      
           move  50                      to xzoom-field-length(idx).
           move  3                       to xzoom-field-offset(idx).
           move  20                      to xzoom-field-column(idx).
           move "Descrizione"            to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true.

      * CAMPO 5
           add   1                          to Idx.
           move  2                          to xzoom-field-file(Idx).
           move  1                          to xzoom-field-rel(Idx).      
           move  4                          to xzoom-field-length(idx).
           move  6                          to xzoom-field-offset(idx).
           move  4                          to xzoom-field-column(idx).
           move "Q.tà"                      to xzoom-field-name(idx).  
           move  4                          to xzoom-field-digits(Idx).
           move  0                          to xzoom-field-dec(Idx).
           move "###0"                      to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.
      
      * CAMPO 6
           add   1                          to Idx.
           move  1                          to xzoom-field-file(Idx).
           move  0                          to xzoom-field-rel(Idx).      
           move  7                          to xzoom-field-length(idx).
           move  97                         to xzoom-field-offset(idx).
           move  8                          to xzoom-field-column(idx).
           move "Peso"                      to xzoom-field-name(idx).  
           move 7                           to xzoom-field-digits(Idx).
           move 3                           to xzoom-field-dec(Idx).
           move "#.##0,000"                 to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.
      
      * CAMPO 7
           add   1                          to Idx.
           move  1                          to xzoom-field-file(Idx).
           move  0                          to xzoom-field-rel(Idx).      
           move  6                          to xzoom-field-length(idx).
           move  85                         to xzoom-field-offset(idx).
           move  8                          to xzoom-field-column(idx).
           move "di cui UTF"                to xzoom-field-name(idx).  
           move 6                           to xzoom-field-digits(Idx).
           move 3                           to xzoom-field-dec(Idx).
           move "##0,000"                   to xzoom-field-fmt(Idx).

           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      *    1
      *    File Reference Settings - Relazione articoli > timbalqta
           move   1                to Idx.
           move   1                to xzoom-ref-m-file (Idx).
           move   2                to xzoom-ref-s-file (Idx).
           move   0                to xzoom-ref-s-key  (Idx).
           move   1                to xzoom-ref-fields (Idx).
           set xzoom-ref-join-outer(Idx) to true.
           
      *            Master File Settings
           move   3  to xzoom-ref-m-length(Idx, 1).
           move 104  to xzoom-ref-m-offset (Idx, 1).
           set xzoom-ref-m-unsigned (Idx, 1) to true.
           
      *            Slave File Settings
           move 3 to xzoom-ref-s-length(Idx, 1).
           move 0 to xzoom-ref-s-offset (Idx, 1).
           set xzoom-ref-s-unsigned (Idx, 1) to true.

      *    RELAZIONE #2
      **    File Reference Settings - Relazione timbalqta > timballi
           add    1                to Idx.
           move   2                to xzoom-ref-m-file (Idx).
           move   3                to xzoom-ref-s-file (Idx).
           move   0                to xzoom-ref-s-key  (Idx).
           move   1                to xzoom-ref-fields (Idx).
           set xzoom-ref-join-outer(Idx) to true.
           
      *            Master File Settings
           move 3 to xzoom-ref-m-length(Idx, 1).
           move 3 to xzoom-ref-m-offset (Idx, 1).
           set xzoom-ref-m-unsigned (Idx, 1) to true.
           set xzoom-ref-m-alpha    (Idx, 1) to true.
           
      *            Slave File Settings
           move 3 to xzoom-ref-s-length(Idx, 1).
           move 0 to xzoom-ref-s-offset (Idx, 1).
           set xzoom-ref-s-unsigned (Idx, 1) to true.
           set xzoom-ref-s-alpha    (Idx, 1) to true.

           move 0 to idx.

           |FILTRO SU RECORD ATTIVI
           if not TuttiIRec
              add 1    to idx
              move "A" to como-record(779:1)

              set xzoom-when-true(idx)    to true
              set xzoom-begin-with(idx)   to true
              set xzoom-ignore-case(idx)  to true
                                 
              move como-record(779:1)   to xzoom-wild-value(idx)
              move 1                    to xzoom-wild-value-length(idx)
              move 1                    to xzoom-wild-length(idx)
              move 778                  to xzoom-wild-offset(idx)
           end-if.

           move  -1                      to xzoom-delimiter-offset.
           move  5                       to xzoom-delimiter-length.
           move "000"                    to xzoom-from-value.
           move "000"                    to xzoom-to-value.

      ***---
       PREPARA-CLIENTI-GDO.
           initialize xzoom-linkage xzoom-ext-info(1).

           move 2 to xzoom-file-key.
      *
           move  zero to idx.
           move  zero                    to xzoom-row.
           move  zero                    to xzoom-cln.
           move  16                      to xzoom-lw.
           move  158                     to xzoom-sw.
           move "clienti"                to xzoom-file-name(1).

      * CAMPO 1
           add 1 to idx.
           move  5                       to xzoom-field-length(idx).
           move  1                       to xzoom-field-offset(idx).
           move  7                       to xzoom-field-column(idx).
           move "Codice"                 to xzoom-field-name(idx).  
           set xzoom-al-right(idx)       to true.
           set xzoom-field-unsigned(idx) to true.
           set xzoom-ft-display(idx)     to true.
           move 5                        to xzoom-field-digits(idx).
           move  0                       to xzoom-field-dec(idx).
           move "####0"                  to xzoom-field-fmt(idx).
      
      * CAMPO 2
           add 1 to idx
           move  40                      to xzoom-field-length(idx).
           move  6                       to xzoom-field-offset(idx).
           move  40                      to xzoom-field-column(idx).
           move "Ragione Sociale"        to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 
      
      * CAMPO 3
           add 1 to idx
           move  40                      to xzoom-field-length(idx).
           move  86                      to xzoom-field-offset(idx).
           move  30                      to xzoom-field-column(idx).
           move "Indirizzo"              to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 
                                       
      * CAMPO 4
           add 1 to idx
           move  5                       to xzoom-field-length(idx).
           move  126                     to xzoom-field-offset(idx).
           move  6                       to xzoom-field-column(idx).
           move "C.A.P."                 to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 
      
      * CAMPO 5
           add 1 to idx
           move  35                      to xzoom-field-length(idx).
           move  131                     to xzoom-field-offset(idx).
           move  30                      to xzoom-field-column(idx).
           move "Località"               to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

      * CAMPO 6
           add 1 to idx
           move  2                       to xzoom-field-length(idx).
           move  166                     to xzoom-field-offset(idx).
           move  7                       to xzoom-field-column(idx).
           move "Provincia"              to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

           move  idx                     to xzoom-fields.

           move  -1                      to xzoom-delimiter-offset.
           move  5                       to xzoom-delimiter-length.
           move "000"                    to xzoom-from-value.
           move "000"                    to xzoom-to-value.

           move 1 to idx.
           set xzoom-when-true(idx)      to true.
           set xzoom-begin-with(idx)     to true.
           set xzoom-ignore-case(idx)    to false.
           
           |SOLO CLIENTI           
           move "C"                      to xzoom-wild-value(idx).                                
           move 1                        to xzoom-wild-value-length(idx)
           move 2                        to xzoom-wild-length(idx).
           move 0                        to xzoom-wild-offset(idx).

           add 1 to idx.
           set xzoom-when-false(idx)  to true.
           set xzoom-ignore-case(idx) to true.
           set xzoom-equal(idx)       to true.
                                 
           move spaces                to xzoom-wild-value(idx).
           move 5                     to xzoom-wild-value-length(idx).
           move 5                     to xzoom-wild-length(idx).
           move 305                   to xzoom-wild-offset(idx).

           |FILTRO SU RECORD ATTIVI
           if not TuttiIRec
              add 1 to idx
              move "A" to como-record(744:1)

              set xzoom-when-true(idx)    to true
              set xzoom-begin-with(idx)   to true
              set xzoom-ignore-case(idx)  to true
                                 
              move como-record(744:1)   to xzoom-wild-value(idx)
              move 1                    to xzoom-wild-value-length(idx)
              move 1                    to xzoom-wild-length(idx)
              move 743                  to xzoom-wild-offset(idx)
           end-if.

      ***---
       PREPARA-CLIENTI-NON-GDO.
           initialize xzoom-linkage xzoom-ext-info(1).

           move 2 to xzoom-file-key.
      *
           move  zero to idx.
           move  zero                    to xzoom-row.
           move  zero                    to xzoom-cln.
           move  16                      to xzoom-lw.
           move  158                     to xzoom-sw.
           move "clienti"                to xzoom-file-name(1).

      * CAMPO 1
           add 1 to idx.
           move  5                       to xzoom-field-length(idx).
           move  1                       to xzoom-field-offset(idx).
           move  7                       to xzoom-field-column(idx).
           move "Codice"                 to xzoom-field-name(idx).  
           set xzoom-al-right(idx)       to true.
           set xzoom-field-unsigned(idx) to true.
           set xzoom-ft-display(idx)     to true.
           move 5                        to xzoom-field-digits(idx).
           move  0                       to xzoom-field-dec(idx).
           move "####0"                  to xzoom-field-fmt(idx).
      
      * CAMPO 2
           add 1 to idx
           move  40                      to xzoom-field-length(idx).
           move  6                       to xzoom-field-offset(idx).
           move  40                      to xzoom-field-column(idx).
           move "Ragione Sociale"        to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 
      
      * CAMPO 3
           add 1 to idx
           move  40                      to xzoom-field-length(idx).
           move  86                      to xzoom-field-offset(idx).
           move  30                      to xzoom-field-column(idx).
           move "Indirizzo"              to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 
                                       
      * CAMPO 4
           add 1 to idx
           move  5                       to xzoom-field-length(idx).
           move  126                     to xzoom-field-offset(idx).
           move  6                       to xzoom-field-column(idx).
           move "C.A.P."                 to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 
      
      * CAMPO 5
           add 1 to idx
           move  35                      to xzoom-field-length(idx).
           move  131                     to xzoom-field-offset(idx).
           move  30                      to xzoom-field-column(idx).
           move "Località"               to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

      * CAMPO 6
           add 1 to idx
           move  2                       to xzoom-field-length(idx).
           move  166                     to xzoom-field-offset(idx).
           move  7                       to xzoom-field-column(idx).
           move "Provincia"              to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

           move  idx                     to xzoom-fields.

           move  -1                      to xzoom-delimiter-offset.
           move  5                       to xzoom-delimiter-length.
           move "000"                    to xzoom-from-value.
           move "000"                    to xzoom-to-value.

           move 1 to idx.
           set xzoom-when-true(idx)      to true.
           set xzoom-begin-with(idx)     to true.
           set xzoom-ignore-case(idx)    to false.
           
           |SOLO CLIENTI           
           move "C"                      to xzoom-wild-value(idx).                                
           move 1                        to xzoom-wild-value-length(idx)
           move 2                        to xzoom-wild-length(idx).
           move 0                        to xzoom-wild-offset(idx).

           add 1 to idx.
           set xzoom-when-true(idx)   to true.
           set xzoom-ignore-case(idx) to true.
           set xzoom-equal(idx)       to true.
                                 
           move spaces                to xzoom-wild-value(idx).
           move 5                     to xzoom-wild-value-length(idx).
           move 5                     to xzoom-wild-length(idx).
           move 305                   to xzoom-wild-offset(idx).

           |FILTRO SU RECORD ATTIVI
           if not TuttiIRec
              add 1 to idx
              move "A" to como-record(744:1)

              set xzoom-when-true(idx)    to true
              set xzoom-begin-with(idx)   to true
              set xzoom-ignore-case(idx)  to true
                                 
              move como-record(744:1)   to xzoom-wild-value(idx)
              move 1                    to xzoom-wild-value-length(idx)
              move 1                    to xzoom-wild-length(idx)
              move 743                  to xzoom-wild-offset(idx)
           end-if.


      ***---
       PREPARA-CLIENTI-DI-GDO.
           initialize xzoom-linkage xzoom-ext-info(1).

           move 2 to xzoom-file-key.
      *
           move  zero to idx.
           move  zero                    to xzoom-row.
           move  zero                    to xzoom-cln.
           move  16                      to xzoom-lw.
           move  158                     to xzoom-sw.
           move "clienti"                to xzoom-file-name(1).

      * CAMPO 1
           add 1 to idx.
           move  5                       to xzoom-field-length(idx).
           move  1                       to xzoom-field-offset(idx).
           move  7                       to xzoom-field-column(idx).
           move "Codice"                 to xzoom-field-name(idx).  
           set xzoom-al-right(idx)       to true.
           set xzoom-field-unsigned(idx) to true.
           set xzoom-ft-display(idx)     to true.
           move 5                        to xzoom-field-digits(idx).
           move  0                       to xzoom-field-dec(idx).
           move "####0"                  to xzoom-field-fmt(idx).
      
      * CAMPO 2
           add 1 to idx
           move  40                      to xzoom-field-length(idx).
           move  6                       to xzoom-field-offset(idx).
           move  40                      to xzoom-field-column(idx).
           move "Ragione Sociale"        to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 
      
      * CAMPO 3
           add 1 to idx
           move  40                      to xzoom-field-length(idx).
           move  86                      to xzoom-field-offset(idx).
           move  30                      to xzoom-field-column(idx).
           move "Indirizzo"              to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 
                                       
      * CAMPO 4
           add 1 to idx
           move  5                       to xzoom-field-length(idx).
           move  126                     to xzoom-field-offset(idx).

           move  6                       to xzoom-field-column(idx).
           move "C.A.P."                 to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 
      
      * CAMPO 5
           add 1 to idx
           move  35                      to xzoom-field-length(idx).
           move  131                     to xzoom-field-offset(idx).
           move  30                      to xzoom-field-column(idx).
           move "Località"               to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

      * CAMPO 6
           add 1 to idx
           move  2                       to xzoom-field-length(idx).
           move  166                     to xzoom-field-offset(idx).
           move  7                       to xzoom-field-column(idx).
           move "Provincia"              to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

           move  idx                     to xzoom-fields.

           move  -1                      to xzoom-delimiter-offset.
           move  5                       to xzoom-delimiter-length.
           move "000"                    to xzoom-from-value.
           move "000"                    to xzoom-to-value.

           move 1 to idx.
           set xzoom-when-true(idx)      to true.
           set xzoom-begin-with(idx)     to true.
           set xzoom-ignore-case(idx)    to false.
           
           |SOLO CLIENTI           
           move "C"                      to xzoom-wild-value(idx).                                
           move 1                        to xzoom-wild-value-length(idx)
           move 2                        to xzoom-wild-length(idx).
           move 0                        to xzoom-wild-offset(idx).

           if como-record(306:5) not = spaces
              add 1 to idx
              set xzoom-when-true(idx)   to true
              set xzoom-begin-with(idx)  to true
              set xzoom-ignore-case(idx) to false
                                 
              move como-record(306:5)    to xzoom-wild-value(idx)
              move 5                     to xzoom-wild-value-length(idx)
              move 5                     to xzoom-wild-length(idx)
              move 305                   to xzoom-wild-offset(idx)
           end-if.

           |FILTRO SU RECORD ATTIVI
           if not TuttiIRec
              add 1 to idx
              move "A" to como-record(744:1)

              set xzoom-when-true(idx)    to true
              set xzoom-begin-with(idx)   to true
              set xzoom-ignore-case(idx)  to true
                                 
              move como-record(744:1)   to xzoom-wild-value(idx)
              move 1                    to xzoom-wild-value-length(idx)
              move 1                    to xzoom-wild-length(idx)
              move 743                  to xzoom-wild-offset(idx)
           end-if.

      ***---                 
       PREPARA-CLIENTI-DI-DESTINI.
           initialize xzoom-linkage xzoom-ext-info(1).
      
           if destinif perform PREPARA-VIDEO-DESTINIF
           else        perform PREPARA-VIDEO-DESTINI
           end-if.

           add 1 to idx.
           set xzoom-when-true(idx)       to true.
           set xzoom-begin-with(idx)      to true.
           set xzoom-ignore-case(idx)     to false.
                                 
           move como-record(1:5)        to xzoom-wild-value(idx).                                
           move 5                       to xzoom-wild-value-length(idx).
           move 5                       to xzoom-wild-length(idx).
           move 0                       to xzoom-wild-offset(idx).

      ***---
       PREPARA-FORN-DI-DESTINI.
           initialize xzoom-linkage xzoom-ext-info(1).
      
           perform PREPARA-VIDEO-DESTINIF.

           move 1 to idx.

           set xzoom-when-true(idx)       to true.
           set xzoom-begin-with(idx)      to true.
           set xzoom-ignore-case(idx)     to false.
                                 
           move como-record(1:5)        to xzoom-wild-value(idx).                                
           move 5                       to xzoom-wild-value-length(idx).
           move 5                       to xzoom-wild-length(idx).
           move 0                       to xzoom-wild-offset(idx).

      ***---
       PREPARA-CLIENTI-ASSORCLI.
           perform PREPARA-VIDEO-ASSORCLI.
           move  2                      to xzoom-file-key.

           set xzoom-when-true(1)       to true.
           set xzoom-begin-with(1)      to true.
           set xzoom-ignore-case(1)     to false.
                                 
           move como-record(1:5)        to xzoom-wild-value(1).                                
           move 5                       to xzoom-wild-value-length(1).
           move 5                       to xzoom-wild-length(1).
           move 0                       to xzoom-wild-offset(1).

      ***---
       PREPARA-DESTINI-ASSORCLI.
           perform PREPARA-VIDEO-ASSORCLI.

           set xzoom-when-true(1)       to true.
           set xzoom-begin-with(1)      to true.
           set xzoom-ignore-case(1)     to false.
           move  3                      to xzoom-file-key.
                                 
           move como-record(1:8 )       to xzoom-wild-value(1).                                
           move 8                       to xzoom-wild-value-length(1).
           move 8                       to xzoom-wild-length(1).
           move 0                       to xzoom-wild-offset(1).

      ***---
       PREPARA-ARTICOLI-ASSORCLI.
           perform PREPARA-VIDEO-ASSORCLI.

           set xzoom-when-true(1)       to true.
           set xzoom-begin-with(1)      to true.
           set xzoom-ignore-case(1)     to false.
           move  4                      to xzoom-file-key.
                                 
           move como-record(1:15)       to xzoom-wild-value(1).                                
           move 15                      to xzoom-wild-value-length(1).
           move 15                      to xzoom-wild-length(1).
           move 0                       to xzoom-wild-offset(1).

      ***---
       PREPARA-ARTICOLI-ASSORCLI-GORDC.

           perform PREPARA-VIDEO-ASSORCLI.

           set xzoom-when-true(1)       to true.
           set xzoom-begin-with(1)      to true.
           set xzoom-ignore-case(1)     to false.
                                 
           move como-record(1:10)       to xzoom-wild-value(1).                                
           move 10                      to xzoom-wild-value-length(1).
           move 10                      to xzoom-wild-length(1).
           move 0                       to xzoom-wild-offset(1).

      ***---
       PREPARA-VIDEO-ASSORCLI.
           initialize xzoom-linkage xzoom-ext-info(1).

           move  zero                   to xzoom-row.
           move  zero                   to xzoom-cln.
           move  16                     to xzoom-lw.
           move  170                    to xzoom-sw.

           move "assorcli"              to xzoom-file-name(1).
           move "clienti"               to xzoom-file-name(2).       
           move "destini"               to xzoom-file-name(3).
           move "articoli"              to xzoom-file-name(4).
           move "tgrupgdo"              to xzoom-file-name(5).
           move  1                      to xzoom-file-key.
           move  8                      to xzoom-fields.

      *    Il file non ha il tipo record
           move  -1                         to xzoom-delimiter-offset.

      *    Definizione del campo 1
           move 1                           to Idx.
           move 1                           to xzoom-field-file(Idx).
           move 0                           to xzoom-field-rel(Idx).
           move 5                           to xzoom-field-length(Idx).
           move 0                           to xzoom-field-offset(Idx).
           move "Gruppo"                    to xzoom-field-name(Idx).  
           move 10                          to xzoom-field-column(Idx).
           set  xzoom-ft-alpha(idx)         to true. 

      *    Definizione del campo 2
           add  1                           to Idx.
           move 5                           to xzoom-field-file(Idx).
           move 4                           to xzoom-field-rel(Idx).
           move 30                          to xzoom-field-length(idx).
           move 5                           to xzoom-field-offset(idx).
           move 25                          to xzoom-field-column(idx).
           move "Intestazione"              to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)         to true. 

      *    Definizione del campo 3
           add  1                           to Idx.
           move 1                           to xzoom-field-file(Idx).
           move 0                           to xzoom-field-rel(Idx).
           move 5                           to xzoom-field-length(idx).
           move 5                           to xzoom-field-offset(idx).
           move "Cliente"                   to xzoom-field-name(idx).  
           move 5                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "####0"                     to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.
           move 6                           to xzoom-field-column(Idx).

      *    Definizione del campo 4
           add  1                           to Idx.
           move 2                           to xzoom-field-file(Idx).
           move 1                           to xzoom-field-rel(Idx).
           move 40                          to xzoom-field-length(idx).
           move 6                           to xzoom-field-offset(idx).
           move 25                          to xzoom-field-column(idx).
           move "Cognome/Rag. sociale"      to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)         to true. 

      *    Definizione del campo 5
           add  1                           to Idx.
           move 1                           to xzoom-field-file(Idx).
           move 0                           to xzoom-field-rel(Idx).
           move 5                           to xzoom-field-length(idx).
           move 10                          to xzoom-field-offset(idx).
           move "Destino"                   to xzoom-field-name(idx).  
           move 5                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "####0"                     to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.
           move 6                           to xzoom-field-column(Idx).

      *    Definizione del campo 6 (descrizione destino)
           add   1                          to Idx.
           move  3                          to xzoom-field-file(Idx).
           move  2                          to xzoom-field-rel(Idx).
           move  35                         to xzoom-field-length(idx).
           move  135                        to xzoom-field-offset(idx).
           move  25                         to xzoom-field-column(idx).
           move "Destinazione"              to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)         to true. 
            
      *    Definizione del campo 7
           add   1                          to Idx.
           move  1                          to xzoom-field-file(Idx).
           move  0                          to xzoom-field-rel(Idx).      
           move  6                          to xzoom-field-length(idx).
           move  15                         to xzoom-field-offset(idx).
           move  6                          to xzoom-field-column(idx).
           move "Articolo"                  to xzoom-field-name(idx).  
           move 6                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "#####0"                    to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.
      
      *    Definizione del campo 8
           add   1                          to Idx.
           move  4                          to xzoom-field-file(Idx).
           move  3                          to xzoom-field-rel(Idx).
           move  50                         to xzoom-field-length(idx).
           move  6                          to xzoom-field-offset(idx).
           move  25                         to xzoom-field-column(idx).
           move "Descrizione"               to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)         to true. 

      *    1
      *    File Reference Settings - Relazione assorcli > clienti
           move   1                to Idx.
           move   1                to xzoom-ref-m-file (Idx).
           move   2                to xzoom-ref-s-file (Idx).
           move   0                to xzoom-ref-s-key  (Idx).
           move   1                to xzoom-ref-fields (Idx).
           set xzoom-ref-join-outer(Idx) to true.
           
      *            Master File Settings
           move 5  to xzoom-ref-m-length(Idx, 1).
           move 5  to xzoom-ref-m-offset (Idx, 1).
           set xzoom-ref-m-unsigned (Idx, 1) to true.
           
      *            Slave File Settings
           move 5 to xzoom-ref-s-length(Idx, 1).
           move 1 to xzoom-ref-s-offset (Idx, 1).
           set xzoom-ref-s-unsigned (Idx, 1) to true.
      *

      *    2
      *    File Reference Settings - Relazione assorcli > destini
           add    1                to Idx.
           move   1                to xzoom-ref-m-file (Idx).
           move   3                to xzoom-ref-s-file (Idx).
           move   0                to xzoom-ref-s-key  (Idx).
           move   1                to xzoom-ref-fields (Idx).
           set xzoom-ref-join-outer(Idx) to true.
           
      *            Master File Settings
           move 10 to xzoom-ref-m-length(Idx, 1).
           move  5 to xzoom-ref-m-offset (Idx, 1).
           set xzoom-ref-m-unsigned (Idx, 1) to true.
           set xzoom-ref-m-alpha    (Idx, 1) to true.
           
      *            Slave File Settings
           move 10 to xzoom-ref-s-length(Idx, 1).
           move  0 to xzoom-ref-s-offset (Idx, 1).
           set xzoom-ref-s-unsigned (Idx, 1) to true.
           set xzoom-ref-s-alpha    (Idx, 1) to true.

      *    3
      *    File Reference Settings - Relazione assorcli > articoli
           add    1                to Idx.
           move   1                to xzoom-ref-m-file (Idx).
           move   4                to xzoom-ref-s-file (Idx).
           move   0                to xzoom-ref-s-key  (Idx).
           move   1                to xzoom-ref-fields (Idx).
           set xzoom-ref-join-outer(Idx) to true.
           
      *            Master File Settings
           move 6      to xzoom-ref-m-length(Idx, 1).
           move 15     to xzoom-ref-m-offset (Idx, 1).
           set xzoom-ref-m-unsigned (Idx, 1) to true.
           set xzoom-ref-m-alpha    (Idx, 1) to true.
           
      *            Slave File Settings
           move 6  to xzoom-ref-s-length(Idx, 1).
           move 0  to xzoom-ref-s-offset (Idx, 1).
           set xzoom-ref-s-unsigned (Idx, 1) to true.
           set xzoom-ref-s-alpha    (Idx, 1) to true.


      *    4
      *    File Reference Settings - Relazione assorcli > tgrupgdo
           add    1                to Idx.
           move   1                to xzoom-ref-m-file (Idx).
           move   5                to xzoom-ref-s-file (Idx).
           move   0                to xzoom-ref-s-key  (Idx).
           move   1                to xzoom-ref-fields (Idx).
           set xzoom-ref-join-outer(Idx) to true.
           
      *            Master File Settings
           move 5      to xzoom-ref-m-length(Idx, 1).
           move 0      to xzoom-ref-m-offset (Idx, 1).
           set xzoom-ref-m-unsigned (Idx, 1) to true.
           set xzoom-ref-m-alpha    (Idx, 1) to true.
           
      *            Slave File Settings
           move 5  to xzoom-ref-s-length(Idx, 1).
           move 0  to xzoom-ref-s-offset (Idx, 1).
           set xzoom-ref-s-unsigned (Idx, 1) to true.
           set xzoom-ref-s-alpha    (Idx, 1) to true.   

           |FILTRO SU RECORD ATTIVI
           move "A" to como-record(88:1).

           set xzoom-when-true(1)    to true.
           set xzoom-begin-with(1)   to true.
           set xzoom-ignore-case(1)  to true.
                                 
           move como-record(88:3)    to xzoom-wild-value(1).
           move 1                    to xzoom-wild-value-length(1).
           move 1                    to xzoom-wild-length(1).
           move 87                   to xzoom-wild-offset(1).

      ***---
       PREPARA-TMAGAZ.
           initialize xzoom-linkage xzoom-ext-info(1).

           move  zero to idx.
           move  zero                    to xzoom-row.
           move  zero                    to xzoom-cln.
           move  16                      to xzoom-lw.
           move  80                      to xzoom-sw.
           move "tmagaz"                 to xzoom-file-name(1).
           move  2                       to xzoom-fields.

      * CAMPO 1
           add 1 to idx
           move  3                       to xzoom-field-length(idx).
           move  0                       to xzoom-field-offset(idx).
           move  9                       to xzoom-field-column(idx).
           move "Codice"                 to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

      * CAMPO 2
           add 1 to idx
           move  50                      to xzoom-field-length(idx).
           move  3                       to xzoom-field-offset(idx).
           move  51                      to xzoom-field-column(idx).
           move "Descrizione"            to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

           move  -1                      to xzoom-delimiter-offset.
           move  5                       to xzoom-delimiter-length.
           move "000"                    to xzoom-from-value.
           move "000"                    to xzoom-to-value.


      ***---
       PREPARA-TIMBALLI.
           initialize xzoom-linkage xzoom-ext-info(1).

           move  zero to idx.
           move  zero                    to xzoom-row.
           move  zero                    to xzoom-cln.
           move  16                      to xzoom-lw.
           move  80                      to xzoom-sw.
           move "timballi"               to xzoom-file-name(1).
           move  2                       to xzoom-fields.

      * CAMPO 1
           add 1 to idx
           move  3                       to xzoom-field-length(idx).
           move  0                       to xzoom-field-offset(idx).
           move  9                       to xzoom-field-column(idx).
           move "Codice"                 to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

      * CAMPO 2
           add 1 to idx
           move  50                      to xzoom-field-length(idx).
           move  3                       to xzoom-field-offset(idx).
           move  51                      to xzoom-field-column(idx).
           move "Descrizione"            to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

           move  -1                     to xzoom-delimiter-offset.
           move  5                      to xzoom-delimiter-length.
           move "000"                   to xzoom-from-value.
           move "000"                   to xzoom-to-value.

      ***---
       PREPARA-TIMBALQTA.
           initialize xzoom-linkage xzoom-ext-info(1).

           move  zero to idx.
           move  zero                    to xzoom-row.
           move  zero                    to xzoom-cln.
           move  16                      to xzoom-lw.
           move  96                      to xzoom-sw.
           move "timbalqta"              to xzoom-file-name(1).
           move "timballi"               to xzoom-file-name(2).

           move zero to idx.
      * CAMPO 1
           add 1 to idx
           move  1                       to xzoom-field-file(Idx).
           move  0                       to xzoom-field-rel(Idx).
           move  3                       to xzoom-field-length(idx).
           move  0                       to xzoom-field-offset(idx).
           move  10                      to xzoom-field-column(idx).
           move "Codice"                 to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

      * CAMPO 2
           add 1 to idx
           move  1                       to xzoom-field-file(Idx).
           move  0                       to xzoom-field-rel(Idx).
           move  3                       to xzoom-field-length(idx).
           move  3                       to xzoom-field-offset(idx).
           move  10                      to xzoom-field-column(idx).
           move "Tipo"                   to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

      * CAMPO 3
           add 1 to idx
           move  2                       to xzoom-field-file(Idx).
           move  1                       to xzoom-field-rel(Idx).
           move  50                      to xzoom-field-length(idx).
           move  3                       to xzoom-field-offset(idx).
           move  45                      to xzoom-field-column(idx).
           move "Descrizione"            to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

      * CAMPO 3
           add 1 to idx
           move  1                       to xzoom-field-file(Idx).
           move  0                       to xzoom-field-rel(Idx).
           move  4                       to xzoom-field-length(idx).
           move  6                       to xzoom-field-offset(idx).
           move  5                       to xzoom-field-column(idx).
           move "Qta"                    to xzoom-field-name(idx).  
           set xzoom-al-right(Idx)       to true.
           set xzoom-field-unsigned(Idx) to true.
           set xzoom-ft-display(Idx)     to true.
           move 4                        to xzoom-field-digits(Idx).
           move   0                      to xzoom-field-dec(Idx).
           move "###0"                   to xzoom-field-fmt(Idx).

           move  idx                     to xzoom-fields.

           move  -1                     to xzoom-delimiter-offset.
           move  5                      to xzoom-delimiter-length.
           move "000"                   to xzoom-from-value.
           move "000"                   to xzoom-to-value.

      *    File Reference Settings - Relazione timbalqta > timballi
           move   1                to Idx.
           move   1                to xzoom-ref-m-file (Idx).
           move   2                to xzoom-ref-s-file (Idx).
           move   0                to xzoom-ref-s-key  (Idx).
           move   1                to xzoom-ref-fields (Idx).
           set xzoom-ref-join-outer(Idx) to true.
           
      *            Master File Settings
           move 3 to xzoom-ref-m-length(Idx, 1).
           move 3 to xzoom-ref-m-offset (Idx, 1).
           set xzoom-ref-m-unsigned (Idx, 1) to true.
           set xzoom-ref-m-alpha    (Idx, 1) to true.
           
      *            Slave File Settings
           move 3 to xzoom-ref-s-length(Idx, 1).
           move 0 to xzoom-ref-s-offset (Idx, 1).
           set xzoom-ref-s-unsigned (Idx, 1) to true.
           set xzoom-ref-s-alpha    (Idx, 1) to true.


      ***---
       PREPARA-VIDEO-PROGMAG.
           initialize xzoom-linkage xzoom-ext-info(1).

           move  zero                   to xzoom-row.
           move  zero                   to xzoom-cln.
           move  16                     to xzoom-lw.
           move  169                    to xzoom-sw.

           move "progmag"               to xzoom-file-name(1).
           move "timbalqta"             to xzoom-file-name(2).
           move "timballi"              to xzoom-file-name(3).
           move "articoli"              to xzoom-file-name(4).
           move "tmagaz"                to xzoom-file-name(5).
           move  0                      to xzoom-file-key.
           move  10                     to xzoom-fields.

      *    Il file non ha il tipo record
           move  -1                     to xzoom-delimiter-offset.

      *    Definizione del campo 1
           move  1                          to Idx.
           move  1                          to xzoom-field-file(Idx).
           move  0                          to xzoom-field-rel(Idx).
           move  6                          to xzoom-field-length(Idx).
           move  0                          to xzoom-field-offset(Idx).
           move "Articolo"                  to xzoom-field-name(Idx).  
           move 9                           to xzoom-field-column(Idx).
           move 6                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "#####0"                    to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      *    Definizione del campo 2
           add  1                           to Idx.
           move 4                           to xzoom-field-file(Idx).
           move 3                           to xzoom-field-rel(Idx).
           move 50                          to xzoom-field-length(idx).
           move 6                           to xzoom-field-offset(idx).
           move "Descrizione"               to xzoom-field-name(idx).  
           move 30                          to xzoom-field-column(Idx).
           set  xzoom-ft-alpha(idx)         to true. 

      *    Definizione del campo 3
           add  1                           to Idx.
           move 1                           to xzoom-field-file(Idx).
           move 0                           to xzoom-field-rel(Idx).
           move 3                           to xzoom-field-length(idx).
           move 6                           to xzoom-field-offset(idx).
           move 10                          to xzoom-field-column(idx).
           move "Magazzino"                 to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)         to true. 

      *    Definizione del campo 4
           add  1                           to Idx.
           move 5                           to xzoom-field-file(Idx).
           move 4                           to xzoom-field-rel(Idx).
           move 50                          to xzoom-field-length(idx).
           move 3                           to xzoom-field-offset(idx).
           move "Descrizione"               to xzoom-field-name(idx).  
           move 25                          to xzoom-field-column(Idx).
           set  xzoom-ft-alpha(idx)         to true. 

      *    Definizione del campo 5
           add   1                          to Idx.
           move  1                          to xzoom-field-file(Idx).
           move  0                          to xzoom-field-rel(Idx).
           move  3                          to xzoom-field-length(idx).
           move  9                          to xzoom-field-offset(idx).
           move  6                          to xzoom-field-column(idx).
           move "Imballo"                   to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)         to true. 
            
      *    Definizione del campo 6
           add   1                          to Idx.
           move  3                          to xzoom-field-file(Idx).
           move  2                          to xzoom-field-rel(Idx).      
           move  50                         to xzoom-field-length(idx).
           move  3                          to xzoom-field-offset(idx).
           move  20                         to xzoom-field-column(idx).
           move "Descrizione"               to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)         to true. 
      
      *    Definizione del campo 7
           add   1                          to Idx.
           move  1                          to xzoom-field-file(Idx).
           move  0                          to xzoom-field-rel(Idx).      
           move  6                          to xzoom-field-length(idx).
           move  12                         to xzoom-field-offset(idx).
           move  8                          to xzoom-field-column(idx).
           move "Peso"                      to xzoom-field-name(idx).  
           move 7                           to xzoom-field-digits(Idx).
           move 3                           to xzoom-field-dec(Idx).
           move "#.##0,000"                 to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.
      
      *    Definizione del campo 8
           add   1                          to Idx.
           move  1                          to xzoom-field-file(Idx).
           move  0                          to xzoom-field-rel(Idx).      
           move  6                          to xzoom-field-length(idx).
           move  18                         to xzoom-field-offset(idx).
           move  8                          to xzoom-field-column(idx).
           move "di cui UTF"                to xzoom-field-name(idx).  
           move 7                           to xzoom-field-digits(Idx).
           move 3                           to xzoom-field-dec(Idx).
           move "##0,000"                   to xzoom-field-fmt(Idx).

           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.
      
      *    Definizione del campo 9
           add   1                          to Idx.
           move  1                          to xzoom-field-file(Idx).
           move  0                          to xzoom-field-rel(Idx).      
           move  8                          to xzoom-field-length(idx).
           move  60                         to xzoom-field-offset(idx).
           move  7                          to xzoom-field-column(idx).
           move "Giacenza"                  to xzoom-field-name(idx).  
           move 8                           to xzoom-field-digits(Idx).
           move "##.###.##0"                to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-signed(Idx )     to true.
           set xzoom-ft-display(Idx)        to true.
      
      *    Definizione del campo 10
           add  1                           to Idx.
           move 1                           to xzoom-field-file(Idx).
           move 0                           to xzoom-field-rel(Idx).
           move 1                           to xzoom-field-length(idx).
           move 400                         to xzoom-field-offset(idx).
           move "Stato"                     to xzoom-field-name(idx).  
           move 4                           to xzoom-field-column(Idx).
           set xzoom-ft-alpha(idx)          to true. 
           set xzoom-al-center(Idx)         to true.

           move 0 to idx.

      *    RELAZIONE #1
      **    File Reference Settings - Relazione progmag > timbalqta
           add    1                to Idx.
           move   1                to xzoom-ref-m-file (Idx).
           move   2                to xzoom-ref-s-file (Idx).
           move   0                to xzoom-ref-s-key  (Idx).
           move   1                to xzoom-ref-fields (Idx).
           set xzoom-ref-join-outer(Idx) to true.
      *     
      *            Master File Settings
           move 3 to xzoom-ref-m-length(Idx, 1).
           move 9 to xzoom-ref-m-offset (Idx, 1).
           set xzoom-ref-m-unsigned (Idx, 1) to true.
           set xzoom-ref-m-alpha    (Idx, 1) to true.
           
      *            Slave File Settings
           move 3 to xzoom-ref-s-length(Idx, 1).
           move 0 to xzoom-ref-s-offset (Idx, 1).
           set xzoom-ref-s-unsigned (Idx, 1) to true.
           set xzoom-ref-s-alpha    (Idx, 1) to true.
                       
      *    RELAZIONE #2
      **    File Reference Settings - Relazione timbalqta > timballi
           add    1                to Idx.
           move   2                to xzoom-ref-m-file (Idx).
           move   3                to xzoom-ref-s-file (Idx).
           move   0                to xzoom-ref-s-key  (Idx).
           move   1                to xzoom-ref-fields (Idx).
           set xzoom-ref-join-outer(Idx) to true.
           
      *            Master File Settings
           move 3 to xzoom-ref-m-length(Idx, 1).
           move 3 to xzoom-ref-m-offset (Idx, 1).
           set xzoom-ref-m-unsigned (Idx, 1) to true.
           set xzoom-ref-m-alpha    (Idx, 1) to true.
           
      *            Slave File Settings
           move 3 to xzoom-ref-s-length(Idx, 1).
           move 0 to xzoom-ref-s-offset (Idx, 1).
           set xzoom-ref-s-unsigned (Idx, 1) to true.
           set xzoom-ref-s-alpha    (Idx, 1) to true.

      *    RELAZIONE #3
      *    File Reference Settings - Relazione progmag > articoli
           add    1                to Idx.
           move   1                to xzoom-ref-m-file (Idx).
           move   4                to xzoom-ref-s-file (Idx).
           move   0                to xzoom-ref-s-key  (Idx).
           move   1                to xzoom-ref-fields (Idx).
           set xzoom-ref-join-outer(Idx) to true.
           
      *            Master File Settings
           move 6  to xzoom-ref-m-length(Idx, 1).
           move 0  to xzoom-ref-m-offset (Idx, 1).
           set xzoom-ref-m-unsigned (Idx, 1) to true.
           set xzoom-ref-m-alpha    (Idx, 1) to true.
           
      *            Slave File Settings
           move 6  to xzoom-ref-s-length(Idx, 1).
           move 0  to xzoom-ref-s-offset (Idx, 1).
           set xzoom-ref-s-unsigned (Idx, 1) to true.
           set xzoom-ref-s-alpha    (Idx, 1) to true.
      *                
      *    RELAZIONE #4
      *    File Reference Settings - Relazione progmag > tmagaz
           add    1                to Idx.
           move   1                to xzoom-ref-m-file (Idx).
           move   5                to xzoom-ref-s-file (Idx).
           move   0                to xzoom-ref-s-key  (Idx).
           move   1                to xzoom-ref-fields (Idx).
           set xzoom-ref-join-outer(Idx) to true.
           
      *            Master File Settings
           move 3 to xzoom-ref-m-length(Idx, 1).
           move 6 to xzoom-ref-m-offset (Idx, 1).
           set xzoom-ref-m-unsigned (Idx, 1) to true.
           set xzoom-ref-m-alpha    (Idx, 1) to true.
           
      *            Slave File Settings
           move 3 to xzoom-ref-s-length(Idx, 1).
           move 0 to xzoom-ref-s-offset (Idx, 1).
           set xzoom-ref-s-unsigned (Idx, 1) to true.
           set xzoom-ref-s-alpha    (Idx, 1) to true.

      ***---
       PREPARA-PROGMAG-TMAGAZ.
           perform PREPARA-VIDEO-PROGMAG.

           set xzoom-when-true(1)       to true.
           set xzoom-begin-with(1)      to true.
           set xzoom-ignore-case(1)     to false.
                                 
           move como-record(7:3)        to xzoom-wild-value(1).                                
           move 3                       to xzoom-wild-value-length(1).
           move 3                       to xzoom-wild-length(1).
           move 6                       to xzoom-wild-offset(1).

           set xzoom-when-true(2)       to true.
           set xzoom-begin-with(2)      to true.
           set xzoom-ignore-case(2)     to false.
                                 
           move "A"                     to xzoom-wild-value(2).
           move 1                       to xzoom-wild-value-length(2).
           move 1                       to xzoom-wild-length(2).
           move 400                     to xzoom-wild-offset(2).

      ***---
       PREPARA-PROGMAG-ONLY-SONS.
           perform PREPARA-VIDEO-PROGMAG.
      * QUESTO FILTRO SCARTA IL RECORD PADRE...
           set xzoom-when-false(1)      to true.
           set xzoom-begin-with(1)      to true.
           set xzoom-ignore-case(1)     to false.
                                 
           move spaces                  to xzoom-wild-value(1).
           move 03                      to xzoom-wild-value-length(1).
           move 03                      to xzoom-wild-length(1).
           move 09                      to xzoom-wild-offset(1).

           |FILTRO SUL CODICE MAGAZZINO
           if como-record(7:3) not = spaces
              set xzoom-when-true(2)    to true
              set xzoom-begin-with(2)   to true
              set xzoom-ignore-case(2)  to false
                                 
              move como-record(7:3)     to xzoom-wild-value(2)
              move 3                    to xzoom-wild-value-length(2)
              move 3                    to xzoom-wild-length(2)
              move 6                    to xzoom-wild-offset(2)
           end-if.

      ***---
       PREPARA-PROGMAG-ARTICOLO-ONLY-SONS.
           perform PREPARA-VIDEO-PROGMAG.
                         
           set xzoom-when-true(1)       to true.
           set xzoom-begin-with(1)      to true.
           set xzoom-ignore-case(1)     to false.

           move como-record(1:6)        to xzoom-wild-value(1).                                
           move 6                       to xzoom-wild-value-length(1).
           move 6                       to xzoom-wild-length(1).
           move 0                       to xzoom-wild-offset(1).

      * QUESTO FILTRO SCARTA IL RECORD PADRE...
           set xzoom-when-false(2)      to true.
           set xzoom-begin-with(2)      to true.
           set xzoom-ignore-case(2)     to false.
                                 
           move spaces                  to xzoom-wild-value(2).
           move 3                       to xzoom-wild-value-length(2).
           move 3                       to xzoom-wild-length(2).
           move 9                       to xzoom-wild-offset(2).

           |FILTRO SUL CODICE MAGAZZINO
           if como-record(7:3) not = spaces
              set xzoom-when-true(3)    to true
              set xzoom-begin-with(3)   to true
              set xzoom-ignore-case(3)  to false
                                 
              move como-record(7:3)     to xzoom-wild-value(3)
              move 3                    to xzoom-wild-value-length(3)
              move 3                    to xzoom-wild-length(3)
              move 6                    to xzoom-wild-offset(3)
           end-if.
                                       
           |FILTRO SOLO SUGLI ATTIVI
           if como-file = "prg-artico-sons-a"
              set xzoom-when-true(4)    to true
              set xzoom-begin-with(4)   to true
              set xzoom-ignore-case(4)  to false
                                 
              move "A"                  to xzoom-wild-value(4)
              move 1                    to xzoom-wild-value-length(4)
              move 1                    to xzoom-wild-length(4)
              move 400                  to xzoom-wild-offset(4)
           end-if.

      ***---
       PREPARA-TIMBALQTA-PROGMAG.
           perform PREPARA-VIDEO-PROGMAG.

           set xzoom-when-true(1)       to true.
           set xzoom-begin-with(1)      to true.
           set xzoom-ignore-case(1)     to false.
                                 
           move como-record(1:09  )     to xzoom-wild-value(1).                                
           move 09                      to xzoom-wild-value-length(1).
           move 90                      to xzoom-wild-length(1).
           move 0                       to xzoom-wild-offset(1).

      ***---
       PREPARA-PESO-PROGMAG.
           perform PREPARA-VIDEO-PROGMAG.

           set xzoom-when-true(1)       to true.
           set xzoom-begin-with(1)      to true.
           set xzoom-ignore-case(1)     to false.
                                 
           move como-record(1:12 )      to xzoom-wild-value(1).                                
           move 12                      to xzoom-wild-value-length(1).
           move 12                      to xzoom-wild-length(1).
           move 0                       to xzoom-wild-offset(1).

      ***---
       PREPARA-TCONTAT.
           initialize xzoom-linkage xzoom-ext-info(1).

           move zero to idx.
           move  zero                    to xzoom-row.
           move  zero                    to xzoom-cln.
           move  16                      to xzoom-lw.
           move  85                      to xzoom-sw.
           move "tcontat"                to xzoom-file-name(1).
           move  5                       to xzoom-fields.

      * CAMPO 1
           add 1 to idx
           move  4                       to xzoom-field-length(idx).
           move  0                       to xzoom-field-offset(idx).
           move  8                       to xzoom-field-column(idx).
           move "Anno"                   to xzoom-field-name(idx).  
           move 4                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "###0"                      to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      * CAMPO 2
           add 1 to idx.
           move  8                       to xzoom-field-length(idx).
           move  4                       to xzoom-field-offset(idx).
           move  12                      to xzoom-field-column(idx).
           move "Ultimo n. ordine"       to xzoom-field-name(idx).  
           move 8                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "#######0"                  to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      * CAMPO 3
           add 1 to idx.
           move  8                        to xzoom-field-length(idx).
           move  12                       to xzoom-field-offset(idx).
           move  18                       to xzoom-field-column(idx).
           move "Ultimo movim. magazzino" to xzoom-field-name(idx). 
           move 8                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "#######0"                  to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      * CAMPO 4
           add 1 to idx.
           move  8                        to xzoom-field-length(idx).
           move  20                       to xzoom-field-offset(idx).
           move  12                       to xzoom-field-column(idx).
           move "Ultimo n. bolle"         to xzoom-field-name(idx). 
           move 8                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "#######0"                  to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      * CAMPO 5
           add 1 to idx.
           move  8                        to xzoom-field-length(idx).
           move  36                       to xzoom-field-offset(idx).
           move  12                       to xzoom-field-column(idx).
           move "Ultimo n. fatture"       to xzoom-field-name(idx). 
           move 8                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "#######0"                  to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

           move  -1                     to xzoom-delimiter-offset.
           move  5                      to xzoom-delimiter-length.
           move "000"                   to xzoom-from-value.
           move "000"                   to xzoom-to-value.


      ***---
       PREPARA-TSETMERC.
           initialize xzoom-linkage xzoom-ext-info(1).

           move zero to idx.
           move  zero                    to xzoom-row.
           move  zero                    to xzoom-cln.
           move  16                      to xzoom-lw.
           move  80                      to xzoom-sw.
           move "tsetmerc"               to xzoom-file-name(1).
           move  2                       to xzoom-fields.

      * CAMPO 1
           add 1 to idx
           move  4                       to xzoom-field-length(idx).
           move  0                       to xzoom-field-offset(idx).
           move  9                      to xzoom-field-column(idx).
           move "Codice"                 to xzoom-field-name(idx).  
           move 4                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "###0"                      to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      * CAMPO 2
           add 1 to idx
           move  30                      to xzoom-field-length(idx).
           move  4                       to xzoom-field-offset(idx).
           move  50                      to xzoom-field-column(idx).
           move "Descrizione"            to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

           move  -1                     to xzoom-delimiter-offset.
           move  5                      to xzoom-delimiter-length.
           move "000"                   to xzoom-from-value.
           move "000"                   to xzoom-to-value.

      ***---
       PREPARA-TCLA1ART.
           initialize xzoom-linkage xzoom-ext-info(1).

           move zero to idx.
           move  zero                    to xzoom-row.
           move  zero                    to xzoom-cln.
           move  16                      to xzoom-lw.
           move  80                      to xzoom-sw.
           move "tcla1art"               to xzoom-file-name(1).
           move  2                       to xzoom-fields.

      * CAMPO 1
           add 1 to idx
           move  4                       to xzoom-field-length(idx).
           move  0                       to xzoom-field-offset(idx).
           move  9                      to xzoom-field-column(idx).
           move "Codice"                 to xzoom-field-name(idx).  
           move 4                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "###0"                      to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      * CAMPO 2
           add 1 to idx
           move  30                      to xzoom-field-length(idx).
           move  4                       to xzoom-field-offset(idx).
           move  51                      to xzoom-field-column(idx).
           move "Descrizione"            to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

           move  -1                     to xzoom-delimiter-offset.
           move  5                      to xzoom-delimiter-length.
           move "000"                   to xzoom-from-value.
           move "000"                   to xzoom-to-value.


      ***---
       PREPARA-TUDM.
           initialize xzoom-linkage xzoom-ext-info(1).

           move zero to idx.
           move  zero                    to xzoom-row.
           move  zero                    to xzoom-cln.
           move  16                      to xzoom-lw.
           move  80                      to xzoom-sw.
           move "tudm"                   to xzoom-file-name(1).
           move  2                       to xzoom-fields.

      * CAMPO 1
           add 1 to idx
           move  2                       to xzoom-field-length(idx).
           move  0                       to xzoom-field-offset(idx).
           move  9                       to xzoom-field-column(idx).
           move "Codice"                 to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

      * CAMPO 2
           add 1 to idx
           move  30                      to xzoom-field-length(idx).
           move  2                       to xzoom-field-offset(idx).
           move  51                      to xzoom-field-column(idx).
           move "Descrizione"            to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

           move  -1                     to xzoom-delimiter-offset.
           move  5                      to xzoom-delimiter-length.
           move "000"                   to xzoom-from-value.
           move "000"                   to xzoom-to-value.


      ***---
       PREPARA-TNOMEN.
           initialize xzoom-linkage xzoom-ext-info(1).

           move zero to idx.
           move  zero                    to xzoom-row.
           move  zero                    to xzoom-cln.
           move  16                      to xzoom-lw.
           move  109                        to xzoom-sw.
           move "tnomen"                 to xzoom-file-name(1).

      * CAMPO 1
           add 1 to idx
           move  8                       to xzoom-field-length(idx).
           move  0                       to xzoom-field-offset(idx).
           move  10                      to xzoom-field-column(idx).
           move "Codice"                 to xzoom-field-name(idx).  
           move 8                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "#######0"                  to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      * CAMPO 2
           add 1 to idx
           move  50                      to xzoom-field-length(idx).
           move  8                       to xzoom-field-offset(idx).
           move  40                      to xzoom-field-column(idx).
           move "Descrizione"            to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

      * CAMPO 2
           add 1 to idx
           move  1                       to xzoom-field-length(idx).
           move  58                      to xzoom-field-offset(idx).
           move  8                       to xzoom-field-column(idx).
           move "Olio"                   to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

           add 1 to idx
           move  1                       to xzoom-field-length(idx).
           move  59                      to xzoom-field-offset(idx).
           move  8                      to xzoom-field-column(idx).
           move "Utf"                    to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

           add 1 to idx
           move  1                       to xzoom-field-length(idx).
           move  60                      to xzoom-field-offset(idx).
           move  8                       to xzoom-field-column(idx).
           move "Consumo"                to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

           add 1 to idx
           move  1                       to xzoom-field-length(idx).
           move  61                      to xzoom-field-offset(idx).
           move  8                       to xzoom-field-column(idx).
           move "Cobat"                  to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 


           move  idx                     to xzoom-fields.


           move  -1                     to xzoom-delimiter-offset.
           move  5                      to xzoom-delimiter-length.
           move "000"                   to xzoom-from-value.
           move "000"                   to xzoom-to-value.


      ***---
       PREPARA-TTIPODOC.
           initialize xzoom-linkage xzoom-ext-info(1).

           move zero to idx.
           move  zero                    to xzoom-row.
           move  zero                    to xzoom-cln.
           move  16                      to xzoom-lw.
           move  90                      to xzoom-sw.
           move "ttipodoc"               to xzoom-file-name(1).
           move  3                       to xzoom-fields.

      * CAMPO 1
           add 1 to idx
           move  2                       to xzoom-field-length(idx).
           move  0                       to xzoom-field-offset(idx).
           move  8                       to xzoom-field-column(idx).
           move "Codice"                 to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

      * CAMPO 2
           add 1 to idx
           move  30                      to xzoom-field-length(idx).
           move  2                       to xzoom-field-offset(idx).
           move  30                      to xzoom-field-column(idx).
           move "Descrizione"            to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

      * CAMPO 3
           add 1 to idx
           move  20                      to xzoom-field-length(idx).
           move  32                      to xzoom-field-offset(idx).
           move  30                      to xzoom-field-column(idx).
           move "Causale Trasporto"      to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

           move  -1                     to xzoom-delimiter-offset.
           move  5                      to xzoom-delimiter-length.
           move "000"                   to xzoom-from-value.
           move "000"                   to xzoom-to-value.

      ***---
       PREPARA-TMP-ASSORCLI.
           initialize xzoom-linkage xzoom-ext-info(1).

           move  0                      to xzoom-row.
           move  0                      to xzoom-cln.
           move  16                     to xzoom-lw.
           move  150                    to xzoom-sw.

           move ext-file                to xzoom-file-name(1).
           move "clienti"               to xzoom-file-name(2).       
           move "destini"               to xzoom-file-name(3).
           move "articoli"              to xzoom-file-name(4).
           move  0                      to xzoom-file-key.
           move  7                      to xzoom-fields.

      *    Il file non ha il tipo record
           move  -1                to xzoom-delimiter-offset.

      *    Definizione del campo 1
           move 1                           to Idx.
           move 1                           to xzoom-field-file(Idx).
           move 0                           to xzoom-field-rel(Idx).
           move 5                           to xzoom-field-length(Idx).
           move 0                           to xzoom-field-offset(Idx).
           move "Gruppo"                    to xzoom-field-name(Idx).  
           move 9                           to xzoom-field-column(Idx).
           set  xzoom-ft-alpha(idx)         to true. 

      *    Definizione del campo 2
           add  1                           to Idx.
           move 1                           to xzoom-field-file(Idx).
           move 0                           to xzoom-field-rel(Idx).
           move 5                           to xzoom-field-length(idx).
           move 5                           to xzoom-field-offset(idx).
           move "Cliente"                   to xzoom-field-name(idx).  
           move 5                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "####0"                     to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.
           move  6                          to xzoom-field-column(Idx).

      *    Definizione del campo 3
           add  1                           to Idx.
           move 2                           to xzoom-field-file(Idx).
           move 1                           to xzoom-field-rel(Idx).
           move 40                          to xzoom-field-length(idx).
           move 6                           to xzoom-field-offset(idx).
           move 30                          to xzoom-field-column(idx).
           move "Cognome/Rag. sociale"      to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)         to true. 

      *    Definizione del campo 4
           add  1                           to Idx.
           move 1                           to xzoom-field-file(Idx).
           move 0                           to xzoom-field-rel(Idx).
           move 5                           to xzoom-field-length(idx).
           move 10                          to xzoom-field-offset(idx).
           move "Destino"                   to xzoom-field-name(idx).  
           move 5                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "####0"                     to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.
           move  6                          to xzoom-field-column(Idx).

      *    Definizione del campo 5 (descrizione destino)
           add   1                          to Idx.
           move  3                          to xzoom-field-file(Idx).
           move  2                          to xzoom-field-rel(Idx).
           move  35                         to xzoom-field-length(idx).
           move  135                        to xzoom-field-offset(idx).
           move  25                         to xzoom-field-column(idx).
           move "Destinazione"              to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)         to true. 
            
      *    Definizione del campo 6
           add   1                          to Idx.
           move  1                          to xzoom-field-file(Idx).
           move  0                          to xzoom-field-rel(Idx).      
           move  6                          to xzoom-field-length(idx).
           move  15                         to xzoom-field-offset(idx).
           move  6                          to xzoom-field-column(idx).
           move "Articolo"                  to xzoom-field-name(idx).  
           move 6                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "#####0"                    to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.
      
      *    Definizione del campo 7
           add   1                          to Idx.
           move  4                          to xzoom-field-file(Idx).
           move  3                          to xzoom-field-rel(Idx).
           move  50                         to xzoom-field-length(idx).
           move  6                          to xzoom-field-offset(idx).
           move  30                         to xzoom-field-column(idx).
           move "Descrizione"               to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)         to true. 
      
      *    File Reference Settings - Relazione assorcli > clienti
           move   1                to Idx.
           move   1                to xzoom-ref-m-file (Idx).
           move   2                to xzoom-ref-s-file (Idx).
           move   0                to xzoom-ref-s-key  (Idx).
           move   1                to xzoom-ref-fields (Idx).
           set xzoom-ref-join-outer(Idx) to true.
           
      *            Master File Settings
           move 5  to xzoom-ref-m-length(Idx, 1).
           move 5  to xzoom-ref-m-offset (Idx, 1).
           set xzoom-ref-m-unsigned (Idx, 1) to true.
           set xzoom-ref-m-alpha    (Idx, 1) to true.

      *            Slave File Settings
           move 5 to xzoom-ref-s-length(Idx, 1).
           move 1 to xzoom-ref-s-offset (Idx, 1).
           set xzoom-ref-s-unsigned (Idx, 1) to true.
           set xzoom-ref-s-alpha    (Idx, 1) to true.

      *
      *    File Reference Settings - Relazione assorcli > destini
           add    1                to Idx.
           move   1                to xzoom-ref-m-file (Idx).
           move   3                to xzoom-ref-s-file (Idx).
           move   0                to xzoom-ref-s-key  (Idx).
           move   1                to xzoom-ref-fields (Idx).
           set xzoom-ref-join-outer(Idx) to true.
           
      *            Master File Settings
           move 10 to xzoom-ref-m-length(Idx, 1).
           move 5  to xzoom-ref-m-offset (Idx, 1).
           set xzoom-ref-m-unsigned (Idx, 1) to true.
           set xzoom-ref-m-alpha    (Idx, 1) to true.
           
      *            Slave File Settings
           move 10 to xzoom-ref-s-length(Idx, 1).
           move  0 to xzoom-ref-s-offset (Idx, 1).
           set xzoom-ref-s-unsigned (Idx, 1) to true.
           set xzoom-ref-s-alpha    (Idx, 1) to true.
      
      *    File Reference Settings - Relazione assorcli > articoli
           add    1                to Idx.
           move   1                to xzoom-ref-m-file (Idx).
           move   4                to xzoom-ref-s-file (Idx).
           move   0                to xzoom-ref-s-key  (Idx).
           move   1                to xzoom-ref-fields (Idx).
           set xzoom-ref-join-outer(Idx) to true.
           
      *            Master File Settings
           move 6      to xzoom-ref-m-length(Idx, 1).
           move 15     to xzoom-ref-m-offset (Idx, 1).
           set xzoom-ref-m-unsigned (Idx, 1) to true.
           set xzoom-ref-m-alpha    (Idx, 1) to true.
           
      *            Slave File Settings
           move 6  to xzoom-ref-s-length(Idx, 1).
           move 0  to xzoom-ref-s-offset (Idx, 1).
           set xzoom-ref-s-unsigned (Idx, 1) to true.
           set xzoom-ref-s-alpha    (Idx, 1) to true.

      ***---
       PREPARA-TCAUMAG.
           initialize xzoom-linkage xzoom-ext-info(1).

           move  0                      to idx.
           move  0                      to xzoom-row.
           move  0                      to xzoom-cln.
           move  16                     to xzoom-lw.
           move  145                    to xzoom-sw.
           move "tcaumag"               to xzoom-file-name(1).
           move "tmagaz"                to xzoom-file-name(2).

      * CAMPO 1
           add   1                      to idx.
           move  1                      to xzoom-field-file(Idx).
           move  0                      to xzoom-field-rel(Idx).      
           move  4                      to xzoom-field-length(idx).
           move  0                      to xzoom-field-offset(idx).
           move  8                      to xzoom-field-column(idx).
           move "Codice"                to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)     to true. 

      * CAMPO 2
           add   1                      to idx.
           move  1                      to xzoom-field-file(Idx).
           move  0                      to xzoom-field-rel(Idx).      
           move  40                     to xzoom-field-length(idx).
           move  4                      to xzoom-field-offset(idx).
           move  40                     to xzoom-field-column(idx).
           move "Descrizione"           to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)     to true. 

      * CAMPO 3
           add   1                      to idx.
           move  1                      to xzoom-field-file(Idx).
           move  0                      to xzoom-field-rel(Idx).      
           move  1                      to xzoom-field-length(idx).
           move  44                     to xzoom-field-offset(idx).
           move  12                     to xzoom-field-column(idx).
           move "Cliente/Fornitore"     to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)     to true. 

      * CAMPO 4                                                 
           add   1                      to idx.
           move  1                      to xzoom-field-file(Idx).
           move  0                      to xzoom-field-rel(Idx).
           move  3                      to xzoom-field-length(idx).
           move  88                     to xzoom-field-offset(idx).
           move  10                     to xzoom-field-column(idx).
           move "Magazzino"             to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)     to true. 

      * CAMPO 5                                                 
           add   1                      to idx.
           move  2                      to xzoom-field-file(Idx).
           move  1                      to xzoom-field-rel(Idx).
           move  50                     to xzoom-field-length(idx).
           move  3                      to xzoom-field-offset(idx).
           move  40                     to xzoom-field-column(idx).
           move "Descrizione Magazzino" to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)     to true.

           move  idx                    to xzoom-fields.

           move  -1                     to xzoom-delimiter-offset.
           move  5                      to xzoom-delimiter-length.
           move "000"                   to xzoom-from-value.

           move "000"                   to xzoom-to-value.
      
      *    File Reference Settings - Relazione tcaumag > tmagaz
           move   1                to Idx.
           move   1                to xzoom-ref-m-file (Idx).
           move   2                to xzoom-ref-s-file (Idx).
           move   0                to xzoom-ref-s-key  (Idx).
           move   1                to xzoom-ref-fields (Idx).
           set xzoom-ref-join-outer(Idx) to true.
           
      *            Master File Settings
           move 3      to xzoom-ref-m-length(Idx, 1).
           move 88     to xzoom-ref-m-offset (Idx, 1).
           set xzoom-ref-m-unsigned (Idx, 1) to true.
           set xzoom-ref-m-alpha    (Idx, 1) to true.
           
      *            Slave File Settings
           move 3  to xzoom-ref-s-length(Idx, 1).
           move 0  to xzoom-ref-s-offset (Idx, 1).
           set xzoom-ref-s-unsigned (Idx, 1) to true.
           set xzoom-ref-s-alpha    (Idx, 1) to true.

      ***---
       PREPARA-CAUSALI-SI-MOVIM.
           perform PREPARA-TCAUMAG.
           
           move 1 to idx.
           |FILTRO SU MOVIM = TRUE
           set xzoom-when-true(idx)     to true.
           set xzoom-begin-with(idx)    to true.
           set xzoom-ignore-case(idx)   to false.
      *                                
           move "S"                     to xzoom-wild-value(idx).
           move 1                       to xzoom-wild-value-length(idx).
           move 1                       to xzoom-wild-length(idx).
           move 87                      to xzoom-wild-offset(idx).

      ***---
       PREPARA-CAUSALI-SI-MOVIM-ORDF.
           perform PREPARA-CAUSALI-SI-MOVIM.
           
           add 1 to idx.
           |FILTRO SU ORDINI FORNITORI = TRUE
           set xzoom-when-true(idx)     to true.
           set xzoom-begin-with(idx)    to true.
           set xzoom-ignore-case(idx)   to false.
      *                                
           move "1"                     to xzoom-wild-value(idx).
           move 1                       to xzoom-wild-value-length(idx).
           move 1                       to xzoom-wild-length(idx).
           move 150                     to xzoom-wild-offset(idx).

      ***---
       PREPARA-CAUSALI-TMOVMAG.
           perform PREPARA-VIDEO-TMOVMAG.

           set xzoom-when-true(1)       to true.
           set xzoom-begin-with(1)      to true.
           set xzoom-ignore-case(1)     to false.
                                 
           move como-record(1:4)        to xzoom-wild-value(1).                                
           move 4                       to xzoom-wild-value-length(1).
           move 4                       to xzoom-wild-length(1).
           move 0                       to xzoom-wild-offset(1).
           
           set xzoom-when-true(2)       to true.
           set xzoom-begin-with(2)      to true.
           set xzoom-ignore-case(2)     to false.
                                 
           move como-record(21:4)       to xzoom-wild-value(2).                                
           move 4                       to xzoom-wild-value-length(2).
           move 4                       to xzoom-wild-length(2).
           move 20                      to xzoom-wild-offset(2).

      ***---
       PREPARA-CAUSALI-FORNITORE.
           perform PREPARA-TCAUMAG.
           
           move 1 to idx.

           |FILTRO SU TCA-TIPO = FORNITORE
           set xzoom-when-true(idx)     to true.
           set xzoom-begin-with(idx)    to true.
           set xzoom-ignore-case(idx)   to false.
      *                                
           move "F"                     to xzoom-wild-value(idx).
           move 1                       to xzoom-wild-value-length(idx).
           move 1                       to xzoom-wild-length(idx).
           move 44                      to xzoom-wild-offset(idx).

      ***---
       PREPARA-CAUSALI-CLIENTE.
           perform PREPARA-TCAUMAG.
           
           move 1 to idx.

           |FILTRO SU TCA-TIPO = FORNITORE
           set xzoom-when-true(idx)     to true.
           set xzoom-begin-with(idx)    to true.
           set xzoom-ignore-case(idx)   to false.
      *                                
           move "C"                     to xzoom-wild-value(idx).
           move 1                       to xzoom-wild-value-length(idx).
           move 1                       to xzoom-wild-length(idx).
           move 44                      to xzoom-wild-offset(idx).

      ***---
       PREPARA-CAUSALI-FORNITORE-MOV-QTA.
           perform PREPARA-CAUSALI-FORNITORE.

           add 1 to idx.
           |FILTRO SU TCA-SI-MOVIM --> causali che movimentano il magazzino
           set xzoom-when-true(idx)     to true.
           set xzoom-begin-with(idx)    to true.
           set xzoom-ignore-case(idx)   to false.
      *                                
           move "S"                     to xzoom-wild-value(idx).
           move 1                       to xzoom-wild-value-length(idx).
           move 1                       to xzoom-wild-length(idx).
           move 87                      to xzoom-wild-offset(idx).

           add 1 to idx.
           |FILTRO SU "tca-SI-movim-giac-periodo" o "tca-NO-movim-giac-periodo"
           |--> causali che generano una variazione di quantità
           set xzoom-when-false(idx)    to true.
           set xzoom-begin-with(idx)    to true.
           set xzoom-ignore-case(idx)   to false.
      *
           move spaces                  to xzoom-wild-value(idx).
           move 1                       to xzoom-wild-value-length(idx).
           move 1                       to xzoom-wild-length(idx).
           move 55                      to xzoom-wild-offset(idx).

      ***---
       PREPARA-VIDEO-TMOVMAG.
           initialize xzoom-linkage xzoom-ext-info(1).

           move  zero to idx.
           move  zero                    to xzoom-row.
           move  zero                    to xzoom-cln.
           move  18                      to xzoom-lw.
           move  118                     to xzoom-sw.
           move "tmovmag"                to xzoom-file-name(1).
           move "tcaumag"                to xzoom-file-name(2).
           move "clienti"                to xzoom-file-name(3).
           move  6                       to xzoom-fields.

      * CAMPO 1
           add 1 to idx
           move  4                       to xzoom-field-length(idx).
           move  0                       to xzoom-field-offset(idx).
           move  6                       to xzoom-field-column(idx).
           move "Anno"                   to xzoom-field-name(idx).  
           move 4                        to xzoom-field-digits(Idx).
           move 0                        to xzoom-field-dec(Idx).
           move "###0"                   to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)       to true.
           set xzoom-field-unsigned(Idx) to true.
           set xzoom-ft-display(Idx)     to true.

      * CAMPO 2
           add 1 to idx
           move  8                       to xzoom-field-length(idx).
           move  4                       to xzoom-field-offset(idx).
           move  8                       to xzoom-field-column(idx).
           move "Numero"                 to xzoom-field-name(idx).  
           move 8                        to xzoom-field-digits(Idx).
           move 0                        to xzoom-field-dec(Idx).
           move "#######0"               to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)       to true.
           set xzoom-field-unsigned(Idx) to true.
           set xzoom-ft-display(Idx)     to true.

      * CAMPO 3
           add 1 to idx
           move  4                       to xzoom-field-length(idx).
           move  20                      to xzoom-field-offset(idx).
           move  6                       to xzoom-field-column(idx).
           move "Causale"                to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

      * CAMPO 4
           add 1 to idx
           move  2                       to xzoom-field-file(idx).
           move  1                       to xzoom-field-rel(idx).
           move  40                      to xzoom-field-length(idx).
           move  4                       to xzoom-field-offset(idx).
           move  30                      to xzoom-field-column(idx).
           move "Descrizione"            to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

      * CAMPO 5
           add 1 to idx
           move  1                       to xzoom-field-file(idx).
           move  5                       to xzoom-field-length(idx).
           move  25                      to xzoom-field-offset(idx).
           move  8                       to xzoom-field-column(idx).
           move "Cliente"                to xzoom-field-name(idx).  
           move 5                        to xzoom-field-digits(Idx).
           move 0                        to xzoom-field-dec(Idx).
           move "####0"                  to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)       to true.
           set xzoom-field-unsigned(Idx) to true.
           set xzoom-ft-display(Idx)     to true.

      * CAMPO 6
           add 1 to idx
           move  3                       to xzoom-field-file(idx).
           move  2                       to xzoom-field-rel(idx).
           move  40                      to xzoom-field-length(idx).
           move  6                       to xzoom-field-offset(idx).
           move  30                      to xzoom-field-column(idx).
           move "Descrizione"            to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

           move  -1                     to xzoom-delimiter-offset.
           move  5                      to xzoom-delimiter-length.
           move "000"                   to xzoom-from-value.
           move "000"                   to xzoom-to-value.

      *    File Reference Settings - Relazione tmovmag > tcaumag
           move   1                to Idx.
           move   1                to xzoom-ref-m-file (Idx).
           move   2                to xzoom-ref-s-file (Idx).
           move   0                to xzoom-ref-s-key  (Idx).
           move   1                to xzoom-ref-fields (Idx).
           set xzoom-ref-join-outer(Idx) to true.
           
      *            Master File Settings
           move 4  to xzoom-ref-m-length(Idx, 1).
           move 20 to xzoom-ref-m-offset (Idx, 1).
           set xzoom-ref-m-unsigned (Idx, 1) to true.
           set xzoom-ref-m-alpha    (Idx, 1) to true.
           
      *            Slave File Settings
           move 4 to xzoom-ref-s-length(Idx, 1).
           move 0 to xzoom-ref-s-offset (Idx, 1).
           set xzoom-ref-s-unsigned (Idx, 1) to true.
           set xzoom-ref-s-alpha    (Idx, 1) to true.
      *
      *    File Reference Settings - Relazione tmovmag > clienti
           add    1                to Idx.
           move   1                to xzoom-ref-m-file (Idx).
           move   3                to xzoom-ref-s-file (Idx).
           move   0                to xzoom-ref-s-key  (Idx).
           move   1                to xzoom-ref-fields (Idx).
           set xzoom-ref-join-outer(Idx) to true.
           
      *            Master File Settings
           move 6  to xzoom-ref-m-length(Idx, 1).
           move 24 to xzoom-ref-m-offset (Idx, 1).
           set xzoom-ref-m-unsigned (Idx, 1) to true.
           set xzoom-ref-m-alpha    (Idx, 1) to true.
           
      *            Slave File Settings
           move 6    to xzoom-ref-s-length(Idx, 1).
           move 0    to xzoom-ref-s-offset (Idx, 1).
           set xzoom-ref-s-unsigned (Idx, 1) to true.
           set xzoom-ref-s-alpha    (Idx, 1) to true.

      ***---
       PREPARA-TORDINI.
           initialize xzoom-linkage xzoom-ext-info(1).

           move  zero to idx.
           move  zero                       to xzoom-row.
           move  zero                       to xzoom-cln.
           move  16                         to xzoom-lw.
           move  145                        to xzoom-sw.
           move "tordini"                   to xzoom-file-name(1).
           move "tcaumag"                   to xzoom-file-name(2).
           move "destini"                   to xzoom-file-name(3).
           move  8                          to xzoom-fields.

      * CAMPO 1
           add  1 to idx
           move  4                          to xzoom-field-length(idx).
           move  0                          to xzoom-field-offset(idx).
           move  7                          to xzoom-field-column(idx).
           move "Anno"                      to xzoom-field-name(idx).  
           move 4                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "###0"                      to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      * CAMPO 2
           add  1 to idx
           move  8                          to xzoom-field-length(idx).
           move  4                          to xzoom-field-offset(idx).
           move  7                          to xzoom-field-column(idx).
           move "Numero"                    to xzoom-field-name(idx).  
           move 8                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "#######0"                  to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      * CAMPO 3
           add  1 to Idx.
           move 8                           to xzoom-field-length(idx).
           move 36                          to xzoom-field-offset(idx).
           move "Data Ordine"               to xzoom-field-name(idx).
           set xzoom-field-unsigned(Idx)    to true.
           set xzoom-ft-display(Idx)        to true.
           move 10                          to xzoom-field-digits(Idx).
           move   0                         to xzoom-field-dec(Idx).
           move "AAAAMMGG-GG/MM/AAAA"       to xzoom-field-fmt(Idx).
           move 13                          to xzoom-field-column(Idx).

      * CAMPO 4
           add  1 to idx.
           move 5                           to xzoom-field-length(idx).
           move 8                           to xzoom-field-column(idx).
           move 16                          to xzoom-field-offset(idx).
           move "Cliente"                   to xzoom-field-name(idx).  
           move 5                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "####0"                     to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      * CAMPO 5
           add  1 to idx.
           move 5                           to xzoom-field-length(idx).
           move 8                           to xzoom-field-column(idx).
           move 21                          to xzoom-field-offset(idx).
           move "Destino"                   to xzoom-field-name(idx).  
           move 5                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "#####"                     to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      * CAMPO 6
           add  1  to idx.
           move 3                           to xzoom-field-file(idx).
           move 2                           to xzoom-field-rel(idx).
           move 35                          to xzoom-field-length(idx).
           move 30                          to xzoom-field-column(idx).
           move 135                         to xzoom-field-offset(idx).
           move "Destinazione"              to xzoom-field-name(idx).  
           set xzoom-ft-alpha(idx)          to true.

      * CAMPO 7
           add  1  to idx.
           move 4                           to xzoom-field-length(idx).
           move 6                           to xzoom-field-column(idx).
           move 12                          to xzoom-field-offset(idx).
           move "Causale"                   to xzoom-field-name(idx).  
           set xzoom-ft-alpha(idx)          to true.

      * CAMPO 8
           add  1  to idx.
           move 2                           to xzoom-field-file(idx).
           move 1                           to xzoom-field-rel(idx).
           move 40                          to xzoom-field-length(idx).
           move 30                          to xzoom-field-column(idx).
           move 4                           to xzoom-field-offset(idx).
           move "Descrizione"               to xzoom-field-name(idx).  
           set xzoom-ft-alpha(idx)          to true.

           move  -1                         to xzoom-delimiter-offset.
           move  5                          to xzoom-delimiter-length.
           move "000"                       to xzoom-from-value.
           move "000"                       to xzoom-to-value.

      *    File Reference Settings - Relazione tordini > tcaumag
           move   1                to Idx.
           move   1                to xzoom-ref-m-file (Idx).
           move   2                to xzoom-ref-s-file (Idx).
           move   0                to xzoom-ref-s-key  (Idx).
           move   1                to xzoom-ref-fields (Idx).
           set xzoom-ref-join-outer(Idx) to true.
           
      *            Master File Settings
           move  4  to xzoom-ref-m-length(Idx, 1).
           move 12  to xzoom-ref-m-offset (Idx, 1).
           set xzoom-ref-m-unsigned (Idx, 1) to true.
           set xzoom-ref-m-alpha    (Idx, 1) to true.
           
      *            Slave File Settings
           move 4   to xzoom-ref-s-length(Idx, 1).
           move 0   to xzoom-ref-s-offset (Idx, 1).
           set xzoom-ref-s-unsigned (Idx, 1) to true.
           set xzoom-ref-s-alpha    (Idx, 1) to true.


      *    File Reference Settings - Relazione tordini > destini
           add    1                to Idx.
           move   1                to xzoom-ref-m-file (Idx).
           move   3                to xzoom-ref-s-file (Idx).
           move   0                to xzoom-ref-s-key  (Idx).
           move   1                to xzoom-ref-fields (Idx).
           set xzoom-ref-join-outer(Idx) to true.
           
      *            Master File Settings
           move 10  to xzoom-ref-m-length(Idx, 1).
           move 16  to xzoom-ref-m-offset (Idx, 1).
           set xzoom-ref-m-unsigned (Idx, 1) to true.
           set xzoom-ref-m-alpha    (Idx, 1) to true.
           
      *            Slave File Settings
           move 10   to xzoom-ref-s-length(Idx, 1).
           move 0    to xzoom-ref-s-offset (Idx, 1).
           set xzoom-ref-s-unsigned (Idx, 1) to true.
           set xzoom-ref-s-alpha    (Idx, 1) to true. 

           |FILTRO SU RECORD ATTIVI
           move 1 to idx.
           move "A" to como-record(750:1).

           set xzoom-when-true(idx)    to true.
           set xzoom-begin-with(idx)   to true.
           set xzoom-ignore-case(idx)  to true.
                                 
           move como-record(750:1)   to xzoom-wild-value(idx).
           move 1                    to xzoom-wild-value-length(idx).
           move 1                    to xzoom-wild-length(idx).
           move 749                  to xzoom-wild-offset(idx).

      ***---
       PREPARA-TORDINI-NON-PRENOTATI.
           initialize xzoom-linkage xzoom-ext-info(1).

           perform PREPARA-TORDINI.

           add 1 to idx.
           |FILTRO SU BOLLE NON PRENOTATE
           set xzoom-when-true(idx)     to true.
           set xzoom-begin-with(idx)    to true.
           set xzoom-ignore-case(idx)   to false.
      *                                
           move "N"                     to xzoom-wild-value(idx).
           move 1                       to xzoom-wild-value-length(idx).
           move 1                       to xzoom-wild-length(idx).
           move 208                     to xzoom-wild-offset(idx).
      *
           add 1 to idx.                         
           set xzoom-when-true(idx)     to true.
           set xzoom-begin-with(idx)    to true.
           set xzoom-ignore-case(idx)   to true.
      *
           |FILTRO SU ANNO
           move como-record(1:4)        to xzoom-wild-value(idx).
           move 4                       to xzoom-wild-value-length(idx).
           move 4                       to xzoom-wild-length(idx).
           move 0                       to xzoom-wild-offset(idx).
      
      *****     add 1 to idx.                         
      *****     set xzoom-when-true(idx)     to true.
      *****     set xzoom-begin-with(idx)    to true.
      *****     set xzoom-ignore-case(idx)   to false.
      ******                                
      *****     move "S"                     to xzoom-wild-value(idx).
      *****     move 1                       to xzoom-wild-value-length(idx).
      *****     move 1                       to xzoom-wild-length(idx).
      *****     move 81                      to xzoom-wild-offset(idx).
      *****
      *****     move 1                       to xzoom-wild-rel(idx).   
      
           add 1 to idx.
           set xzoom-when-true(idx)     to true.
           set xzoom-equal(idx)         to true.
           set xzoom-ignore-case(idx)   to false.

      *    |TIPO RECORD ORDINE
           move "O"                     to xzoom-wild-value(idx).
           move 1                       to xzoom-wild-value-length(idx).
           move 1                       to xzoom-wild-length(idx).
           move 240                     to xzoom-wild-offset(idx).
      
      ***---
       PREPARA-TORDINI-PRENOTATI.
           initialize xzoom-linkage xzoom-ext-info(1).

           perform PREPARA-TORDINI.

           add 1 to idx.
           set xzoom-when-true(idx)     to true.
           set xzoom-begin-with(idx)    to true.
           set xzoom-ignore-case(idx)   to false.
      *
           move "S"                     to xzoom-wild-value(idx).
           move 1                       to xzoom-wild-value-length(idx).
           move 1                       to xzoom-wild-length(idx).
           move 208                     to xzoom-wild-offset(idx).

      *****     add 1 to idx.
      *****     set xzoom-when-true(idx)     to true.
      *****     set xzoom-begin-with(idx)    to true.
      *****     set xzoom-ignore-case(idx)   to false.
      ******
      *****     move "S"                     to xzoom-wild-value(idx).
      *****     move 1                       to xzoom-wild-value-length(idx).
      *****     move 1                       to xzoom-wild-length(idx).
      *****     move 81                      to xzoom-wild-offset(idx).
      *****
      *****     move 1                       to xzoom-wild-rel(idx).
      *
           add 1 to idx.
           set xzoom-when-true(idx)     to true.
           set xzoom-begin-with(idx)    to true.
           set xzoom-ignore-case(idx)   to true.
      *
           move "0000000000000000"      to xzoom-wild-value(idx).
           move 16                      to xzoom-wild-value-length(idx).
           move 16                      to xzoom-wild-length(idx).
           move 191                     to xzoom-wild-offset(idx).
      *
           add 1 to idx.
           set xzoom-when-true(idx)     to true.
           set xzoom-begin-with(idx)    to true.
           set xzoom-ignore-case(idx)   to true.
      *
           move como-record(1:4)        to xzoom-wild-value(idx).
           move 4                       to xzoom-wild-value-length(idx).
           move 4                       to xzoom-wild-length(idx).
           move 0                       to xzoom-wild-offset(idx).

      *     move 1                       to xzoom-wild-rel(idx).      
           
      ***---
       PREPARA-TORDINI-BOLLE.
           initialize xzoom-linkage, xzoom-ext-info(1).

           move  zero to idx.
           move  zero                       to xzoom-row.
           move  zero                       to xzoom-cln.
           move  16                         to xzoom-lw.
           move  100                        to xzoom-sw.
           move "tordini"                   to xzoom-file-name(1).
           move "clienti"                   to xzoom-file-name(2).
           move  5                          to xzoom-file-key.
           move  6                          to xzoom-fields.

      * CAMPO 1
           add 1 to idx.
           move 1                           to xzoom-field-file(Idx).
           move 0                           to xzoom-field-rel(Idx).
           move  4                          to xzoom-field-length(idx).
           move  188                        to xzoom-field-offset(idx).
           move  10                         to xzoom-field-column(idx).
           move "Anno"                      to xzoom-field-name(idx).
           move 4                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "###0"                      to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      * CAMPO 2
           add   1                          to idx.
           move 1                           to xzoom-field-file(Idx).
           move 0                           to xzoom-field-rel(Idx).
           move  8                          to xzoom-field-length(idx).
           move  192                        to xzoom-field-offset(idx).
           move  13                         to xzoom-field-column(idx).
           move "N. Bolla  "                to xzoom-field-name(idx).
           move 8                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "#######0"                  to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      * CAMPO 3
           add  1                           to Idx.
           move 1                           to xzoom-field-file(Idx).
           move 0                           to xzoom-field-rel(Idx).
           move 8                           to xzoom-field-length(idx).
           move 200                         to xzoom-field-offset(idx).
           move 20                          to xzoom-field-column(idx).
           move "Data Fatturazione"         to xzoom-field-name(idx).
           set xzoom-field-unsigned(Idx)    to true.
           set xzoom-ft-display(Idx)        to true.
           move 10                          to xzoom-field-digits(Idx).
           move   0                         to xzoom-field-dec(Idx).
           move "AAAAMMGG-GG/MM/AAAA"       to xzoom-field-fmt(Idx). 

      * CAMPO 4
           add   1                          to idx.
           move 1                           to xzoom-field-file(Idx).
           move 0                           to xzoom-field-rel(Idx).
           move  8                          to xzoom-field-length(idx).
           move  4                          to xzoom-field-offset(idx).
           move  13                         to xzoom-field-column(idx).
           move "N. Registrazione"          to xzoom-field-name(idx).
           move 8                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "#######0"                  to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      * CAMPO 5
           add   1                          to idx.
           move 1                           to xzoom-field-file(Idx).
           move 0                           to xzoom-field-rel(Idx).
           move  5                          to xzoom-field-length(idx).
           move  16                         to xzoom-field-offset(idx).
           move  8                          to xzoom-field-column(idx).
           move "Cliente"                   to xzoom-field-name(idx).
           move 5                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "####0"                     to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      * CAMPO 6
           add  1                           to Idx.
           move 2                           to xzoom-field-file(Idx).
           move 1                           to xzoom-field-rel(Idx).
           move 40                          to xzoom-field-length(idx).
           move 6                           to xzoom-field-offset(idx).
           move 30                          to xzoom-field-column(idx).
           move "Cognome/Rag. sociale"      to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)         to true. 

      *    File Reference Settings - Relazione tordini > clienti
           move   1                to Idx.
           move   1                to xzoom-ref-m-file (Idx).
           move   2                to xzoom-ref-s-file (Idx).
           move   0                to xzoom-ref-s-key  (Idx).
           move   1                to xzoom-ref-fields (Idx).
           set xzoom-ref-join-outer(Idx) to true.
           
      *            Master File Settings
           move 5  to xzoom-ref-m-length(Idx, 1).
           move 16 to xzoom-ref-m-offset (Idx, 1).
           set xzoom-ref-m-unsigned (Idx, 1) to true.
           set xzoom-ref-m-alpha    (Idx, 1) to true.

      *            Slave File Settings
           move 5 to xzoom-ref-s-length(Idx, 1).
           move 1 to xzoom-ref-s-offset (Idx, 1).
           set xzoom-ref-s-unsigned (Idx, 1) to true.
           set xzoom-ref-s-alpha    (Idx, 1) to true.

           move 1 to idx.
           set xzoom-when-true(idx)       to true.
           set xzoom-begin-with(idx)      to true.
           set xzoom-ignore-case(idx)     to true.

           move como-record(189:4)    to xzoom-wild-value(idx).
           move 4                     to xzoom-wild-value-length(idx).
           move 4                     to xzoom-wild-length(idx).
           move 188                   to xzoom-wild-offset(idx).
      *
           |DEVE AVERE IL NUMERO DELLA BOLLA VALORIZZATO!!!
           add 1 to idx.
      *
           set xzoom-when-false(idx)  to true.
           set xzoom-begin-with(idx)  to true.
           set xzoom-ignore-case(idx) to false.
      *
           move "00000000"            to xzoom-wild-value(idx).
           move 8                     to xzoom-wild-value-length(idx).
           move 8                     to xzoom-wild-length(idx).
           move 192                   to xzoom-wild-offset(idx). 

           |FILTRO SU RECORD ATTIVI
           add 1 to idx.
           move "A" to como-record(750:1).

           set xzoom-when-true(idx)   to true.
           set xzoom-begin-with(idx)  to true.
           set xzoom-ignore-case(idx) to true.
                                 
           move como-record(750:1)    to xzoom-wild-value(idx).
           move 1                     to xzoom-wild-value-length(idx).
           move 1                     to xzoom-wild-length(idx).
           move 749                   to xzoom-wild-offset(idx).
                                           
           move  -1                   to xzoom-delimiter-offset.
           move  5                    to xzoom-delimiter-length.
           move "000"                 to xzoom-from-value.
           move "000"                 to xzoom-to-value.

      ***---
       PREPARA-TORDINI-FATTURE-NON-PRENOTA.
           perform PREPARA-TORDINI.

           add 1 to idx.
           set xzoom-when-true(idx)       to true.
           set xzoom-begin-with(idx)      to true.
           set xzoom-ignore-case(idx)     to true.

           move como-record(1:4)        to xzoom-wild-value(idx).
           move 4                       to xzoom-wild-value-length(idx).
           move 4                       to xzoom-wild-length(idx).
           move 0                       to xzoom-wild-offset(idx).

           add 1 to idx.

           set xzoom-when-true(idx)     to true.
           set xzoom-begin-with(idx)    to true.
           set xzoom-ignore-case(idx)   to false.

           move "N"                     to xzoom-wild-value(idx).
           move 1                       to xzoom-wild-value-length(idx).
           move 1                       to xzoom-wild-length(idx).
           move 237                     to xzoom-wild-offset(idx).
      *
           |DEVE AVERE L'ANNO DELLA BOLLA VALORIZZATO!!!
           add 1 to idx.
      *
           set xzoom-when-false(idx)  to true.
           set xzoom-begin-with(idx)  to true.
           set xzoom-ignore-case(idx) to false.
      *
           move "0000"                to xzoom-wild-value(idx).
           move 4                     to xzoom-wild-value-length(idx).
           move 4                     to xzoom-wild-length(idx).
           move 188                   to xzoom-wild-offset(idx).

      ***---
       PREPARA-TORDINI-FATTURE-PRENOTA-NON-FAT.
           perform PREPARA-TORDINI.

           add 1 to idx.
           set xzoom-when-true(idx)     to true.
           set xzoom-begin-with(idx)    to true.
           set xzoom-ignore-case(idx)   to false.

           move "S"                     to xzoom-wild-value(idx).
           move 1                       to xzoom-wild-value-length(idx).
           move 1                       to xzoom-wild-length(idx).
           move 237                     to xzoom-wild-offset(idx).

           add 1                        to idx.
           set xzoom-when-true(idx)     to true.
           set xzoom-begin-with(idx)    to true.
           set xzoom-ignore-case(idx)   to false.

           move "0000000000000000"      to xzoom-wild-value(idx).
           move 16                      to xzoom-wild-value-length(idx).
           move 16                      to xzoom-wild-length(idx).
           move 209                     to xzoom-wild-offset(idx).
      *
           add 1                        to idx.
           set xzoom-when-true(idx)     to true.
           set xzoom-begin-with(idx)    to true.
           set xzoom-ignore-case(idx)   to true.
      *
           move como-record(1:4)        to xzoom-wild-value(idx).
           move 4                       to xzoom-wild-value-length(idx).
           move 4                       to xzoom-wild-length(idx).
           move 0                       to xzoom-wild-offset(idx).

      ***---
       PREPARA-TORDINI-FATTURE-SI-FAT.
           initialize xzoom-linkage, xzoom-ext-info(1).

           move  zero to idx.
           move  zero                       to xzoom-row.
           move  zero                       to xzoom-cln.
           move  16                         to xzoom-lw.
           move  88                         to xzoom-sw.
           move "tordini"                   to xzoom-file-name(1).
           move  5                          to xzoom-file-key.
           move  6                          to xzoom-fields.

      * CAMPO 1
           add   1 to idx.
           move  4                          to xzoom-field-length(idx).
           move  209                        to xzoom-field-offset(idx).
           move  10                         to xzoom-field-column(idx).
           move "Anno"                      to xzoom-field-name(idx).
           move 4                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "###0"                      to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      * CAMPO 2
           add  1  to idx.
           move 4                           to xzoom-field-length(idx).
           move 6                           to xzoom-field-column(idx).
           move 12                          to xzoom-field-offset(idx).
           move "Causale"                   to xzoom-field-name(idx).  
           set xzoom-ft-alpha(idx)          to true.

      * CAMPO 3
           add   1                          to idx.
           move  8                          to xzoom-field-length(idx).
           move  213                        to xzoom-field-offset(idx).
           move  13                         to xzoom-field-column(idx).
           move "N. Fattura"                to xzoom-field-name(idx).
           move 8                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "#######0"                  to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      * CAMPO 4
           add  1                           to Idx.
           move 8                           to xzoom-field-length(idx).
           move 221                         to xzoom-field-offset(idx).
           move 20                          to xzoom-field-column(idx).
           move "Data Fatturazione"         to xzoom-field-name(idx).
           set xzoom-field-unsigned(Idx)    to true.
           set xzoom-ft-display(Idx)        to true.
           move 10                          to xzoom-field-digits(Idx).
           move   0                         to xzoom-field-dec(Idx).
           move "AAAAMMGG-GG/MM/AAAA"       to xzoom-field-fmt(Idx).

      * CAMPO 5
           add   1                          to idx.
           move  8                          to xzoom-field-length(idx).
           move  229                        to xzoom-field-offset(idx).
           move  8                          to xzoom-field-column(idx).
           move "Lotto"                     to xzoom-field-name(idx).
           move 8                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "#######0"                  to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      * CAMPO 6
           add   1                          to idx.
           move  1                          to xzoom-field-length(idx).
           move  187                        to xzoom-field-offset(idx).
           move  8                          to xzoom-field-column(idx).
           move "Postel"                    to xzoom-field-name(idx).
           set xzoom-ft-alpha(idx)          to true.
           set xzoom-al-right(idx)          to true.

      *****
           set xzoom-when-true(1)           to true.
           set xzoom-begin-with(1)          to true.
           set xzoom-ignore-case(1)         to false.
      *****

      * FILTRO SU ANNO DI FATTURAZIONE
           move como-record(210:4)         to xzoom-wild-value(1).
           move 4                          to xzoom-wild-value-length(1)
           move 4                          to xzoom-wild-length(1).
           move 209                        to xzoom-wild-offset(1).

      *****
           set xzoom-when-true(2)          to true.
           set xzoom-begin-with(2)         to true.


           set xzoom-ignore-case(2)        to false.

      *****
      * FILTRO SU FATTURE SOLO PRENOTATE
           move "S"                        to xzoom-wild-value(2).
           move 1                          to xzoom-wild-value-length(2)
           move 1                          to xzoom-wild-length(2).
           move 237                        to xzoom-wild-offset(2).

           set xzoom-when-false(3)         to true.
           set xzoom-begin-with(3)         to true.
           set xzoom-ignore-case(3)        to false.
      *****
      * FILTRO SU FATTURE CHE SONO GIA' STATE FATTURATE
           move 0                          to xzoom-wild-value(3).
           move 1                          to xzoom-wild-value-length(3)
           move 1                          to xzoom-wild-length(3).
           move 209                        to xzoom-wild-offset(3).

           |FILTRO SU RECORD ATTIVI
           move 4 to idx.
           move "A" to como-record(750:1).

           set xzoom-when-true(idx)    to true.
           set xzoom-begin-with(idx)   to true.
           set xzoom-ignore-case(idx)  to true.
                                 
           move como-record(750:1)   to xzoom-wild-value(idx).
           move 1                    to xzoom-wild-value-length(idx).
           move 1                    to xzoom-wild-length(idx).
           move 749                  to xzoom-wild-offset(idx).

           move  -1                        to xzoom-delimiter-offset.
           move  5                         to xzoom-delimiter-length.
           move "000"                      to xzoom-from-value.
           move "000"                      to xzoom-to-value.

      ***---
       PREPARA-TORDINI-FATTURE-SI-FAT-ORD.
           perform PREPARA-TORDINI-FATTURE-SI-FAT.

           set xzoom-when-true(4)          to true.
           set xzoom-begin-with(4)         to true.
           set xzoom-ignore-case(4)        to false.
      *****
      * FILTRO SU FATTURE CHE SONO GIA' STATE FATTURATE
           move "O"                        to xzoom-wild-value(4).
           move 1                          to xzoom-wild-value-length(4)
           move 1                          to xzoom-wild-length(4).
           move 240                        to xzoom-wild-offset(4).

      ***---
       PREPARA-ARTICOLI-MARCHE.                                                                        
           perform PREPARA-ARTICOLI.
           
           |FILTRO SULLA MARCA
           if como-record(61:4) not = "0000"
              add 1 to idx
              move como-record(61:4)     to xzoom-wild-value(idx)
              move 04                    to xzoom-wild-value-length(idx)
              move 04                    to xzoom-wild-length(idx)
              move 60                    to xzoom-wild-offset(idx)
              set xzoom-when-true(idx)   to true
              set xzoom-begin-with(idx)  to true
              set xzoom-ignore-case(idx) to false
           end-if.

      ***---
       PREPARA-TNOTACR.
           initialize xzoom-linkage xzoom-ext-info(1).

           move  zero to idx.
           move  zero                       to xzoom-row.
           move  zero                       to xzoom-cln.
           move  16                         to xzoom-lw.
           move  145                        to xzoom-sw.
           move "tnotacr"                   to xzoom-file-name(1).
           move "tcaumag"                   to xzoom-file-name(2).
           move "destini"                   to xzoom-file-name(3).
           move  8                          to xzoom-fields.

      * CAMPO 1
           add   1                          to idx.
           move  4                          to xzoom-field-length(idx).
           move  0                          to xzoom-field-offset(idx).
           move  7                          to xzoom-field-column(idx).
           move "Anno"                      to xzoom-field-name(idx).  
           move 4                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "###0"                      to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      * CAMPO 2
           add   1                          to idx.
           move  8                          to xzoom-field-length(idx).
           move  4                          to xzoom-field-offset(idx).
           move  7                          to xzoom-field-column(idx).
           move "Numero"                    to xzoom-field-name(idx).  
           move 8                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "#######0"                  to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      * CAMPO 3
           add  1                           to Idx.
           move 8                           to xzoom-field-length(idx).
           move 26                          to xzoom-field-offset(idx).
           move "Data"                      to xzoom-field-name(idx).
           set xzoom-field-unsigned(Idx)    to true.
           set xzoom-ft-display(Idx)        to true.
           move 10                          to xzoom-field-digits(Idx).
           move   0                         to xzoom-field-dec(Idx).
           move "AAAAMMGG-GG/MM/AAAA (GGG)" to xzoom-field-fmt(Idx).
           move 13                          to xzoom-field-column(Idx).

      * CAMPO 4
           add  1                           to idx.
           move 5                           to xzoom-field-length(idx).
           move 8                           to xzoom-field-column(idx).
           move 16                          to xzoom-field-offset(idx).
           move "Cliente"                   to xzoom-field-name(idx).  
           move 5                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "####0"                     to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      * CAMPO 5
           add  1                           to idx.
           move 5                           to xzoom-field-length(idx).
           move 8                           to xzoom-field-column(idx).
           move 21                          to xzoom-field-offset(idx).
           move "Destino"                   to xzoom-field-name(idx).  
           move 5                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "#####"                     to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      * CAMPO 6
           add  1                           to idx.
           move 3                           to xzoom-field-file(idx).
           move 2                           to xzoom-field-rel(idx).
           move 35                          to xzoom-field-length(idx).
           move 30                          to xzoom-field-column(idx).
           move 135                         to xzoom-field-offset(idx).
           move "Destinazione"              to xzoom-field-name(idx).  
           set xzoom-ft-alpha(idx)          to true.

      * CAMPO 7
           add  1                           to idx.
           move 4                           to xzoom-field-length(idx).
           move 6                           to xzoom-field-column(idx).
           move 12                          to xzoom-field-offset(idx).
           move "Causale"                   to xzoom-field-name(idx).
           set xzoom-ft-alpha(idx)          to true.

      * CAMPO 8
           add  1                           to idx.
           move 2                           to xzoom-field-file(idx).
           move 1                           to xzoom-field-rel(idx).
           move 40                          to xzoom-field-length(idx).
           move 30                          to xzoom-field-column(idx).
           move 4                           to xzoom-field-offset(idx).
           move "Descrizione"               to xzoom-field-name(idx).  
           set xzoom-ft-alpha(idx)          to true.

           move  -1                         to xzoom-delimiter-offset.
           move  5                          to xzoom-delimiter-length.
           move "000"                       to xzoom-from-value.
           move "000"                       to xzoom-to-value.

      *    File Reference Settings - Relazione tordini > tcaumag
           move   1                         to Idx.
           move   1                         to xzoom-ref-m-file (Idx).
           move   2                         to xzoom-ref-s-file (Idx).
           move   0                         to xzoom-ref-s-key  (Idx).
           move   1                         to xzoom-ref-fields (Idx).
           set xzoom-ref-join-outer(Idx)    to true.

      *            Master File Settings
           move  4  to xzoom-ref-m-length(Idx, 1).
           move 12  to xzoom-ref-m-offset (Idx, 1).
           set xzoom-ref-m-unsigned (Idx, 1) to true.
           set xzoom-ref-m-alpha    (Idx, 1) to true.

      *            Slave File Settings
           move 4   to xzoom-ref-s-length(Idx, 1).
           move 0   to xzoom-ref-s-offset(Idx, 1).
           set xzoom-ref-s-unsigned (Idx, 1) to true.
           set xzoom-ref-s-alpha    (Idx, 1) to true.

      *    File Reference Settings - Relazione tordini > destini
           add    1                to Idx.
           move   1                to xzoom-ref-m-file (Idx).
           move   3                to xzoom-ref-s-file (Idx).
           move   0                to xzoom-ref-s-key  (Idx).
           move   1                to xzoom-ref-fields (Idx).
           set xzoom-ref-join-outer(Idx) to true.

      *            Master File Settings
           move 10  to xzoom-ref-m-length(Idx, 1).
           move 16  to xzoom-ref-m-offset (Idx, 1).
           set xzoom-ref-m-unsigned (Idx, 1) to true.
           set xzoom-ref-m-alpha    (Idx, 1) to true.

      *            Slave File Settings
           move 10   to xzoom-ref-s-length(Idx, 1).
           move 0    to xzoom-ref-s-offset (Idx, 1).
           set xzoom-ref-s-unsigned (Idx, 1) to true.
           set xzoom-ref-s-alpha    (Idx, 1) to true.

           |FILTRO SU RECORD ATTIVI
           move 1 to idx.
           move "A" to como-record(595:1).

           set xzoom-when-true(idx)    to true.
           set xzoom-begin-with(idx)   to true.
           set xzoom-ignore-case(idx)  to true.
                                 
           move como-record(595:1)   to xzoom-wild-value(idx).
           move 1                    to xzoom-wild-value-length(idx).
           move 1                    to xzoom-wild-length(idx).
           move 594                  to xzoom-wild-offset(idx).

      ***---
       PREPARA-TNOTACR-FATTURE-NON-PRENOTA.
           perform PREPARA-TNOTACR.

           add 1 to idx.
           set xzoom-when-true(idx)     to true.
           set xzoom-begin-with(idx)    to true.
           set xzoom-ignore-case(idx)   to true.
                       
           move como-record(1:4)        to xzoom-wild-value(idx).
           move 4                       to xzoom-wild-value-length(idx).
           move 4                       to xzoom-wild-length(idx).
           move 0                       to xzoom-wild-offset(idx).

           add 1 to idx.

           set xzoom-when-true(idx)     to true.
           set xzoom-begin-with(idx)    to true.
           set xzoom-ignore-case(idx)   to false.

           move "N"                     to xzoom-wild-value(idx).
           move 1                       to xzoom-wild-value-length(idx).
           move 1                       to xzoom-wild-length(idx).
           move 584                     to xzoom-wild-offset(idx).

      ******
      *****     |DEVE AVERE L'ANNO DELLA BOLLA VALORIZZATO!!!
      *****     add 1 to idx.
      *****     set xzoom-when-false(idx)    to true.
      *****     set xzoom-begin-with(idx)    to true.
      *****     set xzoom-ignore-case(idx)   to false.
      ******                       
      *****     move "0000"                  to xzoom-wild-value(idx).                                
      *****     move 4                       to xzoom-wild-value-length(idx).
      *****     move 4                       to xzoom-wild-length(idx).
      *****     move 188                     to xzoom-wild-offset(idx).

      ***---
       PREPARA-TNOTACR-FATTURE-PRENOTA-NON-FAT.
           perform PREPARA-TNOTACR.

           add 1 to idx.
           set xzoom-when-true(idx)     to true.
           set xzoom-begin-with(idx)    to true.
           set xzoom-ignore-case(idx)   to false.
                                      
           move "S"                     to xzoom-wild-value(idx).
           move 1                       to xzoom-wild-value-length(idx).
           move 1                       to xzoom-wild-length(idx).
           move 584                     to xzoom-wild-offset(idx).       

           add 1                        to idx.
           set xzoom-when-true(idx)     to true.
           set xzoom-begin-with(idx)    to true.
           set xzoom-ignore-case(idx)   to false.

           move "0000000000000000"      to xzoom-wild-value(idx).
           move 16                      to xzoom-wild-value-length(idx).
           move 16                      to xzoom-wild-length(idx).
           move 556                     to xzoom-wild-offset(idx).
      *
           add 1                        to idx.
           set xzoom-when-true(idx)     to true.
           set xzoom-begin-with(idx)    to true.
           set xzoom-ignore-case(idx)   to true.
      *
           move como-record(1:4)        to xzoom-wild-value(idx).
           move 4                       to xzoom-wild-value-length(idx).
           move 4                       to xzoom-wild-length(idx).
           move 0                       to xzoom-wild-offset(idx).

      ***---
       PREPARA-TNOTACR-FATTURE-SI-FAT.
           initialize xzoom-linkage, xzoom-ext-info(1).

           move  0                          to idx.
           move  0                          to xzoom-row.
           move  0                          to xzoom-cln.
           move  16                         to xzoom-lw.
           move  88                         to xzoom-sw.
           move "tnotacr"                   to xzoom-file-name(1).
           move  5                          to xzoom-file-key.
           move  6                          to xzoom-fields.

      * CAMPO 1
           add   1                          to idx
           move  4                          to xzoom-field-length(idx).
           move  556                        to xzoom-field-offset(idx).
           move  10                         to xzoom-field-column(idx).
           move "Anno"                      to xzoom-field-name(idx).
           move 4                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "###0"                      to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      * CAMPO 2
           add  1                           to idx.
           move 4                           to xzoom-field-length(idx).
           move 6                           to xzoom-field-column(idx).
           move 12                          to xzoom-field-offset(idx).
           move "Causale"                   to xzoom-field-name(idx).
           set xzoom-ft-alpha(idx)          to true.

      * CAMPO 3
           add   1                          to idx.
           move  8                          to xzoom-field-length(idx).
           move  560                        to xzoom-field-offset(idx).
           move  13                         to xzoom-field-column(idx).
           move "N. Nota Credito"           to xzoom-field-name(idx).  
           move 8                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "#######0"                  to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      * CAMPO 4
           add  1                           to Idx.
           move 8                           to xzoom-field-length(idx).
           move 568                         to xzoom-field-offset(idx).
           move 20                          to xzoom-field-column(idx).
           move "Data Fatturazione"         to xzoom-field-name(idx).
           set xzoom-field-unsigned(Idx)    to true.
           set xzoom-ft-display(Idx)        to true.
           move 10                          to xzoom-field-digits(Idx).
           move   0                         to xzoom-field-dec(Idx).
           move "AAAAMMGG-GG/MM/AAAA"       to xzoom-field-fmt(Idx).

      * CAMPO 5
           add   1                          to idx.
           move  8                          to xzoom-field-length(idx).
           move  576                        to xzoom-field-offset(idx).
           move  8                          to xzoom-field-column(idx).
           move "Lotto"                     to xzoom-field-name(idx).  
           move  8                          to xzoom-field-digits(Idx).
           move  0                          to xzoom-field-dec(Idx).
           move "#######0"                  to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      * CAMPO 6
           add   1                          to idx.
           move  1                          to xzoom-field-length(idx).
           move  555                        to xzoom-field-offset(idx).
           move  8                          to xzoom-field-column(idx).
           move "Postel"                    to xzoom-field-name(idx).  
           set xzoom-ft-alpha(idx)          to true.
           set xzoom-al-right(idx)          to true.

      *****
           set xzoom-when-true(1)           to true.
           set xzoom-begin-with(1)          to true.
           set xzoom-ignore-case(1)         to false.
      *****

      * FILTRO SU ANNO DI FATTURAZIONE
           move como-record(557:4)         to xzoom-wild-value(1).
           move 4                          to xzoom-wild-value-length(1)
           move 4                          to xzoom-wild-length(1).
           move 556                        to xzoom-wild-offset(1).

      *****
           set xzoom-when-true(2)          to true.
           set xzoom-begin-with(2)         to true.
           set xzoom-ignore-case(2)        to false.
      *****
      * FILTRO SU FATTURE SOLO PRENOTATE
           move "S"                        to xzoom-wild-value(2).
           move 1                          to xzoom-wild-value-length(2)
           move 1                          to xzoom-wild-length(2).
           move 584                        to xzoom-wild-offset(2).

           set xzoom-when-false(3)         to true.
           set xzoom-begin-with(3)         to true.
           set xzoom-ignore-case(3)        to false.
      *****
      * FILTRO SU FATTURE CHE SONO GIA' STATE FATTURATE
           move 0                          to xzoom-wild-value(3).
           move 1                          to xzoom-wild-value-length(3)
           move 1                          to xzoom-wild-length(3).
           move 555                        to xzoom-wild-offset(3).

           |FILTRO SU RECORD ATTIVI
           move 4 to idx.
           move "A" to como-record(595:1).

           set xzoom-when-true(idx)    to true.
           set xzoom-begin-with(idx)   to true.
           set xzoom-ignore-case(idx)  to true.
                                 
           move como-record(595:1)   to xzoom-wild-value(idx).
           move 1                    to xzoom-wild-value-length(idx).
           move 1                    to xzoom-wild-length(idx).
           move 594                  to xzoom-wild-offset(idx).       
                                           
           move  -1                        to xzoom-delimiter-offset.
           move  5                         to xzoom-delimiter-length.
           move "000"                      to xzoom-from-value.
           move "000"                      to xzoom-to-value.

      ***---
       PREPARA-MASTRO.
           initialize xzoom-linkage, xzoom-ext-info(1).

           move  0                          to idx.
           move  0                          to xzoom-row.
           move  0                          to xzoom-cln.
           move  16                         to xzoom-lw.
           move  150                        to xzoom-sw.
           move "MAS"                       to xzoom-file-name(1).
           move  0                          to xzoom-file-key.
           move  6                          to xzoom-fields.

      * CAMPO 1
           add   1                          to idx
           move  8                          to xzoom-field-length(idx).
           move  0                          to xzoom-field-offset(idx).
           move  9                          to xzoom-field-column(idx).
           move "Codice"                    to xzoom-field-name(idx).
           set xzoom-ft-alpha(Idx)          to true.

      * CAMPO 2
           add   1                          to idx.
           move  30                         to xzoom-field-length(idx).
           move   8                         to xzoom-field-offset(idx).
           move  30                         to xzoom-field-column(idx).
           move "Cod. Alternativo"          to xzoom-field-name(idx).
           set xzoom-ft-alpha(Idx)          to true.

      * CAMPO 3
           add   1                          to Idx.
           move 30                          to xzoom-field-length(idx).
           move 38                          to xzoom-field-offset(idx).
           move 30                          to xzoom-field-column(idx).
           move "Descrizione"               to xzoom-field-name(idx).
           set xzoom-ft-alpha(Idx)          to true.

      * CAMPO 4
           add   1                          to Idx.
           move 30                          to xzoom-field-length(idx).
           move 68                          to xzoom-field-offset(idx).
           move 30                          to xzoom-field-column(idx).
           move spaces                      to xzoom-field-name(idx).
           set xzoom-ft-alpha(Idx)          to true.

      * CAMPO 5
           add   1                          to idx.
           move  6                          to xzoom-field-length(idx).
           move  303                        to xzoom-field-offset(idx).
           move  7                          to xzoom-field-column(idx).
           move "Dare"                      to xzoom-field-name(idx).
           move 6                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "#####0"                    to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      * CAMPO 6
           add   1                          to idx.
           move  6                          to xzoom-field-length(idx).
           move  309                        to xzoom-field-offset(idx).
           move  7                          to xzoom-field-column(idx).
           move "Avere"                     to xzoom-field-name(idx).
           move 6                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "#####0"                    to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

           move  -1                         to xzoom-delimiter-offset.
           move  5                          to xzoom-delimiter-length.
           move "000"                       to xzoom-from-value.
           move "000"                       to xzoom-to-value.

      ***---
       PREPARA-CAUSALI-DISTINTA.
           initialize xzoom-linkage, xzoom-ext-info(1).

           move  0                          to idx.
           move  0                          to xzoom-row.
           move  0                          to xzoom-cln.
           move  16                         to xzoom-lw.
           move  175                        to xzoom-sw.
           move "tcaudisb"                  to xzoom-file-name(1).
           move "tcaumag"                   to xzoom-file-name(2).
           move  0                          to xzoom-file-key.

      * CAMPO 1
           add   1                          to idx
           move  4                          to xzoom-field-length(idx).
           move  0                          to xzoom-field-offset(idx).
           move  8                          to xzoom-field-column(idx).
           move "Codice"                    to xzoom-field-name(idx).
           set xzoom-ft-alpha(Idx)          to true.
                          
      * CAMPO 2
           add    1                         to idx.
           move  40                         to xzoom-field-length(idx).
           move   4                         to xzoom-field-offset(idx).
           move  40                         to xzoom-field-column(idx).
           move "Descrizione"               to xzoom-field-name(idx).
           set xzoom-ft-alpha(Idx)          to true.

      * CAMPO 3
           add    1                         to idx
           move   4                         to xzoom-field-length(idx).
           move  44                         to xzoom-field-offset(idx).
           move  12                         to xzoom-field-column(idx).
           move "Causale di carico"         to xzoom-field-name(idx).
           set xzoom-ft-alpha(Idx)          to true.

      * CAMPO 4
           add   1 to idx.
           move  2                       to xzoom-field-file(Idx).
           move  1                       to xzoom-field-rel(Idx).
           move  40                      to xzoom-field-length(idx).
           move  4                       to xzoom-field-offset(idx).
           move  30                      to xzoom-field-column(idx).
           move "Descrizione"            to xzoom-field-name(idx).
           set  xzoom-ft-alpha(idx)      to true.

      * CAMPO 5
           add    1                         to idx
           move   4                         to xzoom-field-length(idx).
           move  48                         to xzoom-field-offset(idx).
           move  12                         to xzoom-field-column(idx).
           move "Causale di Scarico"        to xzoom-field-name(idx).
           set xzoom-ft-alpha(Idx)          to true.

      * CAMPO 6
           add   1 to idx.
           move  2                       to xzoom-field-file(Idx).
           move  2                       to xzoom-field-rel(Idx).
           move  40                      to xzoom-field-length(idx).
           move  4                       to xzoom-field-offset(idx).
           move  30                      to xzoom-field-column(idx).
           move "Descrizione"            to xzoom-field-name(idx).
           set  xzoom-ft-alpha(idx)      to true.

           move  -1                         to xzoom-delimiter-offset.
           move  5                          to xzoom-delimiter-length.
           move "000"                       to xzoom-from-value.
           move "000"                       to xzoom-to-value.

           move  idx                        to xzoom-fields.

      *    File Reference Settings - Relazione tcaudisb > tcaumag (carico)
           move   1                to Idx.
           move   1                to xzoom-ref-m-file (Idx).
           move   2                to xzoom-ref-s-file (Idx).
           move   0                to xzoom-ref-s-key  (Idx).
           move   1                to xzoom-ref-fields (Idx).
           set xzoom-ref-join-outer(Idx) to true.

      *            Master File Settings
           move  4  to xzoom-ref-m-length(Idx, 1).
           move 44  to xzoom-ref-m-offset (Idx, 1).
           set xzoom-ref-m-unsigned (Idx, 1) to true.
           set xzoom-ref-m-alpha    (Idx, 1) to true.

      *            Slave File Settings
           move 4   to xzoom-ref-s-length(Idx, 1).
           move 0   to xzoom-ref-s-offset (Idx, 1).
           set xzoom-ref-s-unsigned (Idx, 1) to true.
           set xzoom-ref-s-alpha    (Idx, 1) to true.         

      *    File Reference Settings - Relazione tcaudisb > tcaumag (scarico)
           add    1                to Idx.
           move   1                to xzoom-ref-m-file (Idx).
           move   2                to xzoom-ref-s-file (Idx).
           move   0                to xzoom-ref-s-key  (Idx).
           move   1                to xzoom-ref-fields (Idx).
           set xzoom-ref-join-outer(Idx) to true.
      
      *            Master File Settings
           move  4  to xzoom-ref-m-length(Idx, 1).
           move 48  to xzoom-ref-m-offset (Idx, 1).
           set xzoom-ref-m-unsigned (Idx, 1) to true.
           set xzoom-ref-m-alpha    (Idx, 1) to true.
      
      *            Slave File Settings
           move 4   to xzoom-ref-s-length(Idx, 1).
           move 0   to xzoom-ref-s-offset (Idx, 1).
           set xzoom-ref-s-unsigned (Idx, 1) to true.
           set xzoom-ref-s-alpha    (Idx, 1) to true.

      ***---
       PREPARA-PROG.
           perform PREPARA-PROG-COM.

      ***---
       PREPARA-PROG-L2.
           perform PREPARA-PROG-COM.
           move como-record(1:2)        to xzoom-wild-value(1).
           move 02                      to xzoom-wild-value-length(1).
           move 02                      to xzoom-wild-length(1).
           move 0                       to xzoom-wild-offset(1).

           set xzoom-when-true(1)       to true.
           set xzoom-begin-with(1)      to true.
           set xzoom-ignore-case(1)     to false. 

           move zero                    to xzoom-wild-value(2).
           move 06                      to xzoom-wild-value-length(2).
           move 06                      to xzoom-wild-length(2).
           move 4                       to xzoom-wild-offset(2).

           set xzoom-when-true(2)       to true.
           set xzoom-begin-with(2)      to true.
           set xzoom-ignore-case(2)     to false. 

      ***---
       PREPARA-PROG-L3.
           perform PREPARA-PROG-COM.
           move como-record(1:4)        to xzoom-wild-value(1).
           move 04                      to xzoom-wild-value-length(1).
           move 04                      to xzoom-wild-length(1).
           move 0                       to xzoom-wild-offset(1).

           set xzoom-when-true(1)       to true.
           set xzoom-begin-with(1)      to true.
           set xzoom-ignore-case(1)     to false. 

           move zero                    to xzoom-wild-value(2).
           move 04                      to xzoom-wild-value-length(2).
           move 04                      to xzoom-wild-length(2).
           move 6                       to xzoom-wild-offset(2).

           set xzoom-when-true(2)       to true.
           set xzoom-begin-with(2)      to true.
           set xzoom-ignore-case(2)     to false. 

      ***---
       PREPARA-PROG-L4.
           perform PREPARA-PROG-COM.
           move como-record(1:6)        to xzoom-wild-value(1).
           move 06                      to xzoom-wild-value-length(1).
           move 06                      to xzoom-wild-length(1).
           move 0                       to xzoom-wild-offset(1).

           set xzoom-when-true(1)       to true.
           set xzoom-begin-with(1)      to true.
           set xzoom-ignore-case(1)     to false. 



           move zero                    to xzoom-wild-value(2).
           move 02                      to xzoom-wild-value-length(2).
           move 02                      to xzoom-wild-length(2).
           move 8                       to xzoom-wild-offset(2).

           set xzoom-when-true(2)       to true.
           set xzoom-begin-with(2)      to true.
           set xzoom-ignore-case(2)     to false. 

      ***---
       PREPARA-PROG-L5.
           perform PREPARA-PROG-COM.
           move como-record(1:8)        to xzoom-wild-value(1).
           move 08                      to xzoom-wild-value-length(1).
           move 08                      to xzoom-wild-length(1).
           move 0                       to xzoom-wild-offset(1).

           set xzoom-when-true(2)       to true.
           set xzoom-begin-with(2)      to true.
           set xzoom-ignore-case(2)     to false. 

      ***---
       PREPARA-PROG-COM.
           initialize xzoom-linkage xzoom-ext-info(1).
           move  zero                    to xzoom-row.
           move  zero                    to xzoom-cln.
           move  16                      to xzoom-lw.
           move  70                      to xzoom-sw.
           move "program"                to xzoom-file-name(1).
           move  0                       to xzoom-file-key.
                                      
           move zero to idx.

      *    livello 1
           add 1 to idx.
           move  2                          to xzoom-field-length(idx).
           move  0                          to xzoom-field-offset(idx).
                                      
           move  7                          to xzoom-field-column(idx).
                                      
           move "Livello"                   to xzoom-field-name(idx).  

           move 2                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "#0"                        to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.


      *    livello 2
           add 1 to idx.
           move  2                          to xzoom-field-length(idx).
           move  2                          to xzoom-field-offset(idx).
           move  3                          to xzoom-field-column(idx).
           move space                       to xzoom-field-name(idx).  

           move 2                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "##"                        to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      *    livello 3
           add 1 to idx.
           move  2                          to xzoom-field-length(idx).
           move  4                          to xzoom-field-offset(idx).
           move  3                          to xzoom-field-column(idx).
           move space                       to xzoom-field-name(idx).  

           move 2                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "##"                        to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.


      *    livello 4
           add 1 to idx.
           move  2                          to xzoom-field-length(idx).
           move  6                          to xzoom-field-offset(idx).
           move  3                          to xzoom-field-column(idx).
           move space                       to xzoom-field-name(idx).  

           move 2                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "##"                        to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.


      *    livello 5
           add 1 to idx.
           move  2                          to xzoom-field-length(idx).
           move  8                          to xzoom-field-offset(idx).
           move  3                          to xzoom-field-column(idx).
           move space                       to xzoom-field-name(idx).  

           move 2                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "##"                        to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.


      *    livello prog-id
           add 1 to idx.
           move  15                         to xzoom-field-length(idx).
           move  10                         to xzoom-field-offset(idx).
           move  10                         to xzoom-field-column(idx).
           move "Programma"                 to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)         to true. 
                                      
      *    livello descrizione
           add 1 to idx.
           move  30                         to xzoom-field-length(idx).
           move  25                         to xzoom-field-offset(idx).
           move  30                         to xzoom-field-column(idx).
           move "Descrizone"                to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)         to true. 



           move  idx                        to xzoom-fields.


           move  -1                         to xzoom-delimiter-offset.
           move  19                         to xzoom-delimiter-length.
           move "000"                       to xzoom-from-value.
           move "000"                       to xzoom-to-value.
                                      
      ***---
       PREPARA-USER.
           initialize xzoom-linkage xzoom-ext-info(1).
           move  zero                       to xzoom-row.
           move  zero                       to xzoom-cln.
           move  16                         to xzoom-lw.
           move  30                         to xzoom-sw.
           move "users"                     to xzoom-file-name(1).
           move  0                          to xzoom-file-key.
                                      
           move zero to idx.

      *    livello User-ID
           add 1 to idx.
           move  10                         to xzoom-field-length(idx).
           move   0                         to xzoom-field-offset(idx).
           move  10                         to xzoom-field-column(idx).
           move "User-ID"                   to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)         to true. 
                                      
      *    livello descrizione
           add 1 to idx.
           move  30                         to xzoom-field-length(idx).
           move  10                         to xzoom-field-offset(idx).
           move  30                         to xzoom-field-column(idx).
           move "Nome"                      to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)         to true. 

           move  idx                        to xzoom-fields.

           move  -1                         to xzoom-delimiter-offset.
           move  19                         to xzoom-delimiter-length.
           move "000"                       to xzoom-from-value.
           move "000"                       to xzoom-to-value.

      ***---
       PREPARA-USER-DIVERSI.
           perform PREPARA-USER.
           |Non devo visualizzare l'utente passato (quello in uso)
           set xzoom-when-false(1)   to true.
           set xzoom-begin-with(1)   to true.
           set xzoom-ignore-case(1)  to true.

           move como-record(1:10)    to xzoom-wild-value(1).
           move 10                   to xzoom-wild-value-length(1).
           move 10                   to xzoom-wild-length(1).
           move 0                    to xzoom-wild-offset(1).

      ***---
       PREPARA-MOVUTF.
           initialize xzoom-linkage xzoom-ext-info(1).
           move  zero                       to xzoom-row.
           move  zero                       to xzoom-cln.
           move  15                         to xzoom-lw.
           move  138                        to xzoom-sw.
           move "movutf"                    to xzoom-file-name(1).
           move "clienti"                   to xzoom-file-name(2).
           move  0                          to xzoom-file-key.

           move zero to idx.

      *    Anno d'esercizio
           add    1                         to idx.
           move   4                         to xzoom-field-length(idx).
           move   0                         to xzoom-field-offset(idx).
           move  10                         to xzoom-field-column(idx).
           move "Anno d'es."                to xzoom-field-name(idx).
           set  xzoom-ft-alpha(idx)         to true. 
           set  xzoom-al-right(Idx)         to true.

      *    Numero Registro
           add    1                         to idx.
           move   8                         to xzoom-field-length(idx).
           move   4                         to xzoom-field-offset(idx).
           move  10                         to xzoom-field-column(idx).
           move "N° Registro"               to xzoom-field-name(idx).
           move  8                          to xzoom-field-digits(Idx).
           move  0                          to xzoom-field-dec(Idx).
           move "######0"                   to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      *    Progressivo Registrazione
           add    1                         to idx.
           move  10                         to xzoom-field-length(idx).
           move  12                         to xzoom-field-offset(idx).
           move  13                         to xzoom-field-column(idx).
           move "Prg. Registrazione"        to xzoom-field-name(idx).
           move 10                          to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "########0"                 to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      *    Data Operazione
           add    1                         to idx.
           move   8                         to xzoom-field-length(idx).
           move  22                         to xzoom-field-offset(idx).
           move  13                         to xzoom-field-column(idx).
           move "Data operazione"           to xzoom-field-name(idx).
           set xzoom-field-unsigned(Idx)    to true.
           set xzoom-ft-display(Idx)        to true.
           move   8                         to xzoom-field-digits(Idx).
           move   0                         to xzoom-field-dec(Idx).
           move "AAAAMMGG-GG/MM/AAAA (GGG)" to xzoom-field-fmt(Idx).

      *    Tipo Operazione
           add    1                         to idx.
           move   1                         to xzoom-field-length(idx).
           move  30                         to xzoom-field-offset(idx).
           move  16                         to xzoom-field-column(idx).
           move "Operazione su Giacenza"    to xzoom-field-name(idx).
           set xzoom-ft-alpha(Idx)          to true.
           set xzoom-al-center(Idx)         to true.

      *    Tipo Colonna
           add    1                         to idx.
           move   1                         to xzoom-field-length(idx).
           move  31                         to xzoom-field-offset(idx).
           move   6                         to xzoom-field-column(idx).
           move "Colonna"                   to xzoom-field-name(idx).
           set xzoom-ft-alpha(Idx)          to true.
           set xzoom-al-center(Idx)         to true.

      *    Kg. Movimentati
           add   1                          to idx.
           move  12                         to xzoom-field-length(idx).
           move  32                         to xzoom-field-offset(idx).
           move  12                         to xzoom-field-column(idx).
           move "Kg. Movimentati"           to xzoom-field-name(idx).
           move 12                          to xzoom-field-digits(Idx).
           move 3                           to xzoom-field-dec(Idx).
           move "###.###.##0,000"           to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-signed(Idx )     to true.
           set xzoom-ft-display(Idx)        to true.

      *    Tipo CF
           add    1                         to idx.
           move   1                         to xzoom-field-length(idx).
           move  44                         to xzoom-field-offset(idx).
           move   3                         to xzoom-field-column(idx).
           move "C/F"                       to xzoom-field-name(idx).
           set xzoom-ft-alpha(Idx)          to true.
           set xzoom-al-center(Idx)         to true.

      *    Codice
           add    1                         to idx.
           move   5                         to xzoom-field-length(idx).
           move  45                         to xzoom-field-offset(idx).
           move   6                         to xzoom-field-column(idx).
           move "Codice"                    to xzoom-field-name(idx).
           move 5                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "#####0"                    to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      *    Numero di documento (bolla XAB)
           add    1                         to idx.
           move  10                         to xzoom-field-length(idx).
           move  50                         to xzoom-field-offset(idx).
           move  13                         to xzoom-field-column(idx).
           move "N° Documento (XAB)"        to xzoom-field-name(idx).
           move  10                         to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "#########0"                to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

           move  idx                        to xzoom-fields.

           move  -1                         to xzoom-delimiter-offset.
           move  19                         to xzoom-delimiter-length.
           move "000"                       to xzoom-from-value.
           move "000"                       to xzoom-to-value.

      ***---
       PREPARA-ANAUTF.
           initialize xzoom-linkage xzoom-ext-info(1).
           move  zero                       to xzoom-row.
           move  zero                       to xzoom-cln.
           move  15                         to xzoom-lw.
           move  131                        to xzoom-sw.
           move "anautf"                    to xzoom-file-name(1).
           move  0                          to xzoom-file-key.

           move zero to idx.

      *    Anno d'esercizio
           add    1                         to idx.
           move   4                         to xzoom-field-length(idx).
           move   0                         to xzoom-field-offset(idx).
           move  10                         to xzoom-field-column(idx).
           move "Anno d'es."                to xzoom-field-name(idx).
           set  xzoom-ft-alpha(idx)         to true. 
           set  xzoom-al-right(Idx)         to true.

      *    Numero Registro
           add    1                         to idx.
           move   8                         to xzoom-field-length(idx).
           move   4                         to xzoom-field-offset(idx).
           move   9                         to xzoom-field-column(idx).
           move "N° Registro"               to xzoom-field-name(idx).
           move  8                          to xzoom-field-digits(Idx).
           move  0                          to xzoom-field-dec(Idx).
           move "######0"                   to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      *    Descrizione 1
           add    1                         to idx.
           move  30                         to xzoom-field-length(idx).
           move  12                         to xzoom-field-offset(idx).
           move  35                         to xzoom-field-column(idx).
           move "Descrizione 1"             to xzoom-field-name(idx).
           set xzoom-ft-alpha(Idx)          to true.
           set xzoom-al-left(Idx)           to true.

      *    Descrizione 2
           add    1                         to idx.
           move  30                         to xzoom-field-length(idx).
           move  42                         to xzoom-field-offset(idx).
           move  35                         to xzoom-field-column(idx).
           move "Descrizione 2"             to xzoom-field-name(idx).
           set xzoom-ft-alpha(Idx)          to true.
           set xzoom-al-left(Idx)           to true.

      *    Tipo registro
           add    1                         to idx.
           move   1                         to xzoom-field-length(idx).
           move 533                         to xzoom-field-offset(idx).
           move  10                         to xzoom-field-column(idx).
           move "Tipo registro"             to xzoom-field-name(idx).
           set xzoom-ft-alpha(Idx)          to true.
           set xzoom-al-left(Idx)           to true.

           move  idx                        to xzoom-fields.

           move  -1                         to xzoom-delimiter-offset.
           move  19                         to xzoom-delimiter-length.
           move "000"                       to xzoom-from-value.
           move "000"                       to xzoom-to-value.

      ***---
       PREPARA-ANAUTF-FILTER-ANNO.
           perform PREPARA-ANAUTF.
           |FILTRO SU UTF-ANNO
           set xzoom-when-true(1)    to true.
           set xzoom-begin-with(1)   to true.
           set xzoom-ignore-case(1)  to false.
                                 
           move como-record(1:4)     to xzoom-wild-value(1).
           move 4                    to xzoom-wild-value-length(1).
           move 4                    to xzoom-wild-length(1).
           move 0                    to xzoom-wild-offset(1).

      ***---
       PREPARA-TMOVTRAT.
           initialize xzoom-linkage xzoom-ext-info(1).
           move  zero                       to xzoom-row.
           move  zero                       to xzoom-cln.
           move  15                         to xzoom-lw.
           move  95                         to xzoom-sw.
           move "tmovtrat"                  to xzoom-file-name(1).
           move  0                          to xzoom-file-key.

           move zero to idx.

      *    Anno
           add    1                         to idx.
           move   4                         to xzoom-field-length(idx).
           move   0                         to xzoom-field-offset(idx).
           move  10                         to xzoom-field-column(idx).
           move "Anno"                      to xzoom-field-name(idx).
           set  xzoom-ft-alpha(idx)         to true. 
           set  xzoom-al-right(Idx)         to true.

      *    UTF - ultimo movimento di mag. trattato
           add    1                         to idx.
           move   8                         to xzoom-field-length(idx).
           move   4                         to xzoom-field-offset(idx).
           move  20                         to xzoom-field-column(idx).
           move "UTF - Ult. mov. mag."      to xzoom-field-name(idx).
           move  8                          to xzoom-field-digits(Idx).
           move  0                          to xzoom-field-dec(Idx).
           move "#######0"                   to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      *    TRASPORTI - ultimo movimento di mag. trattato
           add    1                         to idx.
           move   8                         to xzoom-field-length(idx).
           move  12                         to xzoom-field-offset(idx).
           move  20                         to xzoom-field-column(idx).
           move "TRASPORTI - Ult. mov. mag." to xzoom-field-name(idx).
           move  8                          to xzoom-field-digits(Idx).
           move  0                          to xzoom-field-dec(Idx).
           move "#######0"                   to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      *    PROVVIGIONI - ultima fattura trattata
           add    1                         to idx.
           move   8                         to xzoom-field-length(idx).
           move  20                         to xzoom-field-offset(idx).
           move  20                         to xzoom-field-column(idx).
           move "PROVVIGIONI - Ult. fattura" to xzoom-field-name(idx).
           move  8                          to xzoom-field-digits(Idx).
           move  0                          to xzoom-field-dec(Idx).
           move "#######0"                  to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

           move  idx                        to xzoom-fields.

           move  -1                         to xzoom-delimiter-offset.
           move  19                         to xzoom-delimiter-length.
           move "000"                       to xzoom-from-value.
           move "000"                       to xzoom-to-value.

      ***---
       PREPARA-LISAGENTE.
           initialize xzoom-linkage xzoom-ext-info(1).
           move  zero                       to xzoom-row.
           move  zero                       to xzoom-cln.
           move  15                         to xzoom-lw.
           move  137                        to xzoom-sw.
           move "lisagente"                 to xzoom-file-name(1).
           move "articoli"                  to xzoom-file-name(2).
           move  0                          to xzoom-file-key.
      
           move 0 to idx.
      
      *    Codice
           add    1                         to idx.
           move   1                         to xzoom-field-file(Idx).
           move   0                         to xzoom-field-rel(Idx).
           move   4                         to xzoom-field-length(idx).
           move   0                         to xzoom-field-offset(idx).
           move  10                         to xzoom-field-column(idx).
           move "Codice"                    to xzoom-field-name(idx).
           move  4                          to xzoom-field-digits(Idx).
           move  0                          to xzoom-field-dec(Idx).
           move "###0"                      to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.
      
      *    Articolo
           add    1                         to idx.
           move   1                         to xzoom-field-file(Idx).
           move   0                         to xzoom-field-rel(Idx).
           move   6                         to xzoom-field-length(idx).
           move   4                         to xzoom-field-offset(idx).
           move  10                         to xzoom-field-column(idx).
           move "Articolo"                  to xzoom-field-name(idx).
           move  6                          to xzoom-field-digits(Idx).
           move  0                          to xzoom-field-dec(Idx).
           move "#####0"                    to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.
      
      *    Descrizione articolo
           add    1                         to idx.
           move   2                         to xzoom-field-file(Idx).
           move   1                         to xzoom-field-rel(Idx).
           move  50                         to xzoom-field-length(idx).
           move   6                         to xzoom-field-offset(idx).
           move  35                         to xzoom-field-column(idx).
           move "Descrizione articolo"      to xzoom-field-name(idx).
           set xzoom-ft-alpha(Idx)          to true.
      
      *    Descrizione listino
           add    1                         to idx.
           move   1                         to xzoom-field-file(Idx).
           move   0                         to xzoom-field-rel(Idx).
           move  50                         to xzoom-field-length(idx).
           move  10                         to xzoom-field-offset(idx).
           move  35                         to xzoom-field-column(idx).
           move "Descrizione listino"       to xzoom-field-name(idx).
           set xzoom-ft-alpha(Idx)          to true.
      
      *    Prezzo
           add   1                          to idx.
           move  1                          to xzoom-field-file(Idx).
           move  0                          to xzoom-field-rel(Idx).
           move  11                         to xzoom-field-length(idx).
           move  76                         to xzoom-field-offset(idx).
           move  12                         to xzoom-field-column(idx).
           move "Prezzo"                    to xzoom-field-name(idx).
           move 11                          to xzoom-field-digits(Idx).
           move 2                           to xzoom-field-dec(Idx).
           move "###.###.##0,00"            to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-signed(Idx )     to true.
           set xzoom-ft-display(Idx)        to true.
      
           move  idx                        to xzoom-fields.
      
      *    File Reference Settings - Relazione lisagente > articoli
           move   1                         to Idx.
           move   1                         to xzoom-ref-m-file (Idx).
           move   2                         to xzoom-ref-s-file (Idx).
           move   0                         to xzoom-ref-s-key  (Idx).
           move   1                         to xzoom-ref-fields (Idx).
           set xzoom-ref-join-outer(Idx)    to true.
           
      *            Master File Settings
           move 6  to xzoom-ref-m-length (Idx, 1).
           move 4  to xzoom-ref-m-offset (Idx, 1).
           set xzoom-ref-m-unsigned (Idx, 1) to true.
           set xzoom-ref-m-alpha    (Idx, 1) to true.
           
      *            Slave File Settings
           move 6 to xzoom-ref-s-length(Idx, 1).
           move 0 to xzoom-ref-s-offset (Idx, 1).
           set xzoom-ref-s-unsigned (Idx, 1) to true.
           set xzoom-ref-s-alpha    (Idx, 1) to true.
      
           move  -1                         to xzoom-delimiter-offset.
           move  19                         to xzoom-delimiter-length.
           move "000"                       to xzoom-from-value.
           move "000"                       to xzoom-to-value.

      ***---
       PREPARA-LISAGENTE-FILTER-ARTICOLO.
           perform PREPARA-LISAGENTE.
           |FILTRO SU ARTICOLO
           set xzoom-when-true(1)    to true.
           set xzoom-equal(1)        to true.
           set xzoom-ignore-case(1)  to false.
                                 
           move como-record(5:6)     to xzoom-wild-value(1).
           move 6                    to xzoom-wild-value-length(1).
           move 6                    to xzoom-wild-length(1).
           move 4                    to xzoom-wild-offset(1).

      ***---
       PREPARA-PROVVIG.
           initialize xzoom-linkage xzoom-ext-info(1).
           move  zero                       to xzoom-row.
           move  zero                       to xzoom-cln.
           move  15                         to xzoom-lw.
           move  163                        to xzoom-sw.
           move "provvig"                   to xzoom-file-name(1).
           move "agenti"                    to xzoom-file-name(2).
           move "articoli"                  to xzoom-file-name(3).
           move  0                          to xzoom-file-key.

           move zero to idx.

      *    Anno
           add    1                         to idx.
           move   4                         to xzoom-field-length(idx).
           move   0                         to xzoom-field-offset(idx).
           move   8                         to xzoom-field-column(idx).
           move "Anno"                      to xzoom-field-name(idx).
           set  xzoom-ft-alpha(idx)         to true. 
           set  xzoom-al-right(Idx)         to true.

      *    N. Fattura
           add    1                         to idx.
           move   8                         to xzoom-field-length(idx).
           move   4                         to xzoom-field-offset(idx).
           move  10                         to xzoom-field-column(idx).
           move "N. Fattura"                to xzoom-field-name(idx).
           move  8                          to xzoom-field-digits(Idx).
           move  0                          to xzoom-field-dec(Idx).
           move "#######0"                  to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      *    Riga
           add    1                         to idx.
           move   5                         to xzoom-field-length(idx).
           move  12                         to xzoom-field-offset(idx).
           move   6                         to xzoom-field-column(idx).
           move "Riga"                      to xzoom-field-name(idx).
           move  4                          to xzoom-field-digits(Idx).
           move  0                          to xzoom-field-dec(Idx).
           move "###0"                      to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true. 

      *    Data fattura
           add    1                         to idx.
           move   8                         to xzoom-field-length(idx).
           move  17                         to xzoom-field-offset(idx).
           move  13                         to xzoom-field-column(idx).
           move "Data Fattura"              to xzoom-field-name(idx).
           set xzoom-field-unsigned(Idx)    to true.
           set xzoom-ft-display(Idx)        to true.
           move   8                         to xzoom-field-digits(Idx).
           move   0                         to xzoom-field-dec(Idx).
           move "AAAAMMGG-GG/MM/AAAA (GGG)" to xzoom-field-fmt(Idx).

      *    Codice Agente
           add    1                         to idx.
           move   5                         to xzoom-field-length(idx).
           move  25                         to xzoom-field-offset(idx).
           move   7                         to xzoom-field-column(idx).
           move "Agente"                    to xzoom-field-name(idx).
           move  4                          to xzoom-field-digits(Idx).
           move  0                          to xzoom-field-dec(Idx).
           move "###0"                      to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true. 

      * Ragione Sociale Agente
           add   1 to idx.
           move  2                       to xzoom-field-file(Idx).
           move  1                       to xzoom-field-rel(Idx).      
           move  40                      to xzoom-field-length(idx).
           move  5                       to xzoom-field-offset(idx).
           move  25                      to xzoom-field-column(idx).
           move "Ragione Sociale"        to xzoom-field-name(idx).  
           set xzoom-ft-alpha(idx)       to true.

      *    Codice Articolo
           add    1                         to idx.
           move   6                         to xzoom-field-length(idx).
           move  35                         to xzoom-field-offset(idx).
           move   7                         to xzoom-field-column(idx).
           move "Articolo"                  to xzoom-field-name(idx).
           move  6                          to xzoom-field-digits(Idx).
           move  0                          to xzoom-field-dec(Idx).
           move "#####0"                    to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true. 

      *    Descrizione
           add   1 to idx.
           move  3                       to xzoom-field-file(Idx).
           move  2                       to xzoom-field-rel(Idx).      
           move  50                      to xzoom-field-length(idx).
           move  6                       to xzoom-field-offset(idx).
           move  25                      to xzoom-field-column(idx).
           move "Descrizione"            to xzoom-field-name(idx).  
           set xzoom-ft-alpha(idx)       to true.

      *    Valore Provvigione
           add    1                         to idx.
           move  11                         to xzoom-field-length(idx).
           move  92                         to xzoom-field-offset(idx).
           move  10                         to xzoom-field-column(idx).
           move "Valore provv."             to xzoom-field-name(idx).
           move 11                          to xzoom-field-digits(Idx).
           move 2                           to xzoom-field-dec(Idx).
           move "###.###.##0,00"            to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-signed(Idx )     to true.
           set xzoom-ft-display(Idx)        to true.    

      *    Data liquidazione
           add    1                         to idx.
           move   8                         to xzoom-field-length(idx).
           move  103                        to xzoom-field-offset(idx).
           move  13                         to xzoom-field-column(idx).
           move "Liquidata il"              to xzoom-field-name(idx).
           set xzoom-field-unsigned(Idx)    to true.
           set xzoom-ft-display(Idx)        to true.
           move   8                         to xzoom-field-digits(Idx).
           move   0                         to xzoom-field-dec(Idx).
           move "AAAAMMGG-GG/MM/AAAA (GGG)" to xzoom-field-fmt(Idx).

           move  idx                        to xzoom-fields.  
      *
      *    File Reference Settings - Relazione provvig > agenti
           move   1                to Idx.
           move   1                to xzoom-ref-m-file (Idx).
           move   2                to xzoom-ref-s-file (Idx).
           move   0                to xzoom-ref-s-key  (Idx).
           move   1                to xzoom-ref-fields (Idx).
           set xzoom-ref-join-outer(Idx) to true.
           
      *            Master File Settings
           move  5 to xzoom-ref-m-length(Idx, 1).
           move 25 to xzoom-ref-m-offset (Idx, 1).
           set xzoom-ref-m-unsigned (Idx, 1) to true.
           set xzoom-ref-m-alpha    (Idx, 1) to true.
           
      *            Slave File Settings
           move 5 to xzoom-ref-s-length(Idx, 1).
           move 0 to xzoom-ref-s-offset (Idx, 1).
           set xzoom-ref-s-unsigned (Idx, 1) to true.
           set xzoom-ref-s-alpha    (Idx, 1) to true.         
      ******
      *    File Reference Settings - Relazione provvig > articoli
           add    1                to Idx.
           move   1                to xzoom-ref-m-file (Idx).
           move   3                to xzoom-ref-s-file (Idx).
           move   0                to xzoom-ref-s-key  (Idx).
           move   1                to xzoom-ref-fields (Idx).
           set xzoom-ref-join-outer(Idx) to true.
           
      *            Master File Settings
           move  6 to xzoom-ref-m-length(Idx, 1).
           move 35 to xzoom-ref-m-offset (Idx, 1).
           set xzoom-ref-m-unsigned (Idx, 1) to true.
           set xzoom-ref-m-alpha    (Idx, 1) to true.
           
      *            Slave File Settings
           move 6 to xzoom-ref-s-length(Idx, 1).
           move 0 to xzoom-ref-s-offset (Idx, 1).
           set xzoom-ref-s-unsigned (Idx, 1) to true.
           set xzoom-ref-s-alpha    (Idx, 1) to true.

           move  -1                         to xzoom-delimiter-offset.
           move  19                         to xzoom-delimiter-length.
           move "000"                       to xzoom-from-value.
           move "000"                       to xzoom-to-value.

      ***---
       PREPARA-TRASPORTI.
           initialize xzoom-linkage xzoom-ext-info(1).

           move  zero to idx.
           move  zero                       to xzoom-row.
           move  zero                       to xzoom-cln.
           move  16                         to xzoom-lw.
           move  130                        to xzoom-sw.
           move  ext-file                   to xzoom-file-name(1).
           move  1                          to xzoom-file-key.

      * CAMPO 1
           add  1 to idx
           move  4                          to xzoom-field-length(idx).
           move  0                          to xzoom-field-offset(idx).
           move  6                          to xzoom-field-column(idx).
           move "Anno"                      to xzoom-field-name(idx).
           move 4                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "###0"                      to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.
      *
      * CAMPO 2
           add  1 to idx
           move  8                          to xzoom-field-length(idx).
           move  4                          to xzoom-field-offset(idx).
           move  9                          to xzoom-field-column(idx).
           move "N. Bolla"                  to xzoom-field-name(idx).
           move 8                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "#######0"                  to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.
      *
      * CAMPO 3
           add  1 to Idx.                                              
           move  9                          to xzoom-field-column(idx).
           move 10                          to xzoom-field-length(idx).
           move 12                          to xzoom-field-offset(idx).
           move "Progressivo"               to xzoom-field-name(idx).
           move 10                          to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "#########0"                to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.
      *
      * CAMPO 4
           add  1  to idx.
           move  5                          to xzoom-field-length(idx).
           move  6                          to xzoom-field-column(idx).
           move 22                          to xzoom-field-offset(idx).
           move "Vettore"                   to xzoom-field-name(idx).
           move 5                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "#####"                     to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.
      
      * CAMPO 5
           add  1 to idx.
           move 40                          to xzoom-field-length(idx).
           move 30                          to xzoom-field-column(idx).
           move 27                          to xzoom-field-offset(idx).
           move "Descrizione"               to xzoom-field-name(idx).  
           set xzoom-ft-alpha(idx)          to true.
      *
      * CAMPO 6
           add  1 to idx.
           move 5                           to xzoom-field-length(idx).
           move 6                           to xzoom-field-column(idx).
           move 67                          to xzoom-field-offset(idx).
           move "Cliente"                   to xzoom-field-name(idx).  
           move 5                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "#####"                     to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      * CAMPO 7
           add  1  to idx.
           move 40                          to xzoom-field-length(idx).
           move 30                          to xzoom-field-column(idx).
           move 72                          to xzoom-field-offset(idx).
           move "Ragione sociale"           to xzoom-field-name(idx).  
           set xzoom-ft-alpha(idx)          to true.

           move idx to xzoom-fields.

           move  -1                         to xzoom-delimiter-offset.
           move  5                          to xzoom-delimiter-length.
           move "000"                       to xzoom-from-value.
           move "000"                       to xzoom-to-value.

      ***---
       PREPARA-TARIFVET.
           initialize xzoom-linkage xzoom-ext-info(1).
           move  zero                       to xzoom-row.
           move  zero                       to xzoom-cln.
           move  15                         to xzoom-lw.
           move  110                        to xzoom-sw.
           move "tarifvet"                  to xzoom-file-name(1).
           move  0                          to xzoom-file-key.

           move zero to idx.

      *    Codice
           add    1                         to idx.
           move   5                         to xzoom-field-length(idx).
           move   0                         to xzoom-field-offset(idx).
           move   8                         to xzoom-field-column(idx).
           move "Codice"                    to xzoom-field-name(idx).
           move  5                          to xzoom-field-digits(Idx).
           move  0                          to xzoom-field-dec(Idx).
           move "####0"                     to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true. 

      *    Campo1
           add    1                         to idx.
           move   5                         to xzoom-field-length(idx).
           move   5                         to xzoom-field-offset(idx).
           move   8                         to xzoom-field-column(idx).
           move "Campo1"                    to xzoom-field-name(idx).
           set  xzoom-ft-alpha(idx)         to true. 
           set  xzoom-al-right(Idx)         to true.

      *    Campo2
           add    1                         to idx.
           move   5                         to xzoom-field-length(idx).
           move  10                         to xzoom-field-offset(idx).
           move  12                         to xzoom-field-column(idx).
           move "Campo2"                    to xzoom-field-name(idx).  
           move  5                          to xzoom-field-digits(Idx).
           move  0                          to xzoom-field-dec(Idx).
           move "####0"                     to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true. 

      *    Progressivo
           add    1                         to idx.
           move   2                         to xzoom-field-length(idx).
           move  15                         to xzoom-field-offset(idx).
           move   8                         to xzoom-field-column(idx).
           move "Prog."                     to xzoom-field-name(idx).
           move  8                          to xzoom-field-digits(Idx).
           move  0                          to xzoom-field-dec(Idx).
           move "#######0"                  to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true. 

      *    Da quintali
           add    1                         to idx.
           move  11                         to xzoom-field-length(idx).
           move  17                         to xzoom-field-offset(idx).
           move  15                         to xzoom-field-column(idx).
           move "Da q.li"                   to xzoom-field-name(idx).
           move 11                          to xzoom-field-digits(Idx).
           move 2                           to xzoom-field-dec(Idx).
           move "###.###.##0,00"            to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-signed(Idx )     to true.
           set xzoom-ft-display(Idx)        to true.    

      *    A quintali
           add    1                         to idx.
           move  11                         to xzoom-field-length(idx).
           move  28                         to xzoom-field-offset(idx).
           move  15                         to xzoom-field-column(idx).
           move "A q.li"                    to xzoom-field-name(idx).
           move 11                          to xzoom-field-digits(Idx).
           move 2                           to xzoom-field-dec(Idx).
           move "###.###.##0,00"            to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-signed(Idx )     to true.
           set xzoom-ft-display(Idx)        to true.    

      *    Euro
           add    1                         to idx.
           move  11                         to xzoom-field-length(idx).
           move  39                         to xzoom-field-offset(idx).
           move  15                         to xzoom-field-column(idx).
           move "Euro"                      to xzoom-field-name(idx).
           move 11                          to xzoom-field-digits(Idx).
           move 2                           to xzoom-field-dec(Idx).
           move "###.###.##0,00"            to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-signed(Idx )     to true.
           set xzoom-ft-display(Idx)        to true.    

           move  idx                        to xzoom-fields.

           move  -1                         to xzoom-delimiter-offset.
           move  19                         to xzoom-delimiter-length.
           move "000"                       to xzoom-from-value.
           move "000"                       to xzoom-to-value.

      ***---
       PREPARA-TMP-PROGMAG-ZOOM.
           initialize xzoom-linkage xzoom-ext-info(1).
           move  zero                       to xzoom-row.
           move  zero                       to xzoom-cln.
           move  ext-file                   to xzoom-file-name(1).
           move  0                          to xzoom-file-key.
                                                         
           move  15                         to xzoom-lw.

           if como-file = "tmp-per-addebito"
              move  147                     to xzoom-sw
           else
              move  158                     to xzoom-sw
           end-if.

           move zero to idx.

      *    Definizione del campo 1
           move  1                          to Idx.
           move  6                          to xzoom-field-length(Idx).
           move  0                          to xzoom-field-offset(Idx).
           move "Articolo"                  to xzoom-field-name(Idx).  
           move 8                           to xzoom-field-column(Idx).
           move 6                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "#####0"                    to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      *    Definizione del campo 2
           add  1                           to Idx.
           move 40                          to xzoom-field-length(idx).
           move 21                          to xzoom-field-offset(idx).
           move "Descrizione"               to xzoom-field-name(idx).  
           move 29                          to xzoom-field-column(Idx).
           set  xzoom-ft-alpha(idx)         to true. 

      *    Definizione del campo 3
           add  1                           to Idx.
           move 3                           to xzoom-field-length(idx).
           move 6                           to xzoom-field-offset(idx).
           move 10                          to xzoom-field-column(idx).
           move "Magazzino"                 to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)         to true. 

      *    Definizione del campo 4
           add  1                           to Idx.
           move 50                          to xzoom-field-length(idx).
           move 61                          to xzoom-field-offset(idx).
           move "Descrizione"               to xzoom-field-name(idx).  
           move 20                          to xzoom-field-column(Idx).
           set  xzoom-ft-alpha(idx)         to true. 

      *    Definizione del campo 5
           add   1                          to Idx.
           move  3                          to xzoom-field-length(idx).
           move  9                          to xzoom-field-offset(idx).
           move  6                          to xzoom-field-column(idx).
           move "Imballo"                   to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)         to true. 

      *    Definizione del campo 6
           add   1                          to Idx.
           move  50                         to xzoom-field-length(idx).
           move  111                        to xzoom-field-offset(idx).
           move  20                         to xzoom-field-column(idx).
           move "Descrizione"               to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)         to true. 

      *    Definizione del campo 7
           add   1                          to Idx.
           move  6                          to xzoom-field-length(idx).
           move  12                         to xzoom-field-offset(idx).
           move  8                          to xzoom-field-column(idx).
           move "Peso"                      to xzoom-field-name(idx).  
           move 7                           to xzoom-field-digits(Idx).
           move 3                           to xzoom-field-dec(Idx).
           move "#.##0,000"                 to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx)    to true.
           set xzoom-ft-display(Idx)        to true.

           if como-file not = "tmp-per-addebito"
      *       Definizione del campo 8
              add   1                        to Idx
              move  8                        to xzoom-field-length(idx)
              move  161                      to xzoom-field-offset(idx)
              move  9                        to xzoom-field-column(idx)
              move 8                         to xzoom-field-digits(Idx)
              move "##.###.##0"              to xzoom-field-fmt(Idx)
              set xzoom-al-right(Idx)        to true
              set xzoom-field-signed(Idx )   to true
              set xzoom-ft-display(Idx)      to true
              move "Giacenza"                to xzoom-field-name(idx)
              if como-file = "tmp-progmag-zoom-E"
                 move "Ordinata" to xzoom-field-name(idx)
              end-if

      *       Definizione del campo 9
              add   1                        to Idx
              move  8                        to xzoom-field-length(idx)
              move  169                      to xzoom-field-offset(idx)
              move  9                        to xzoom-field-column(idx)
              move "Impegnato"               to xzoom-field-name(idx)
              move 8                         to xzoom-field-digits(Idx)
              move "##.###.##0"              to xzoom-field-fmt(Idx)
              set xzoom-al-right(Idx)        to true
              set xzoom-field-signed(Idx )   to true
              set xzoom-ft-display(Idx)      to true

              if como-file = "tmp-progmag-zoom-E"
                 move "Evasa" to xzoom-field-name(idx)
              end-if

              |Solo per gestione ordini
              if como-file = "tmp-progmag-zoom-o"
                 move  170                     to xzoom-sw
      *          Definizione del campo 10
                 add   1                      to Idx
                 move  8                      to xzoom-field-length(idx)
                 move  177                    to xzoom-field-offset(idx)
                 move  9                      to xzoom-field-column(idx)
                 move "Ordinato Padre"        to xzoom-field-name(idx)
                 move 8                       to xzoom-field-digits(Idx)
                 move "##.###.##0"            to xzoom-field-fmt(Idx)
                 set xzoom-al-right(Idx)      to true
                 set xzoom-field-signed(Idx)  to true
                 set xzoom-ft-display(Idx)    to true
              end-if
           else
      *       Definizione del campo 8
              add   1                        to Idx
              move  8                        to xzoom-field-length(idx)
              move  185                      to xzoom-field-offset(idx)
              move  10                       to xzoom-field-column(idx)
              move "Prezzo Unit."            to xzoom-field-name(idx)
              move 8                         to xzoom-field-digits(Idx)
              move 2                         to xzoom-field-dec(Idx)
              move "###.##0,00"              to xzoom-field-fmt(Idx)
              set xzoom-al-right(Idx)        to true
              set xzoom-field-unsigned(Idx)  to true
              set xzoom-ft-display(Idx)      to true
           end-if.

           move  idx                        to xzoom-fields.

           move  -1                         to xzoom-delimiter-offset.
           move  19                         to xzoom-delimiter-length.
           move "000"                       to xzoom-from-value.
           move "000"                       to xzoom-to-value.

      ***---
       PREPARA-TMP-TORDINI-ZOOM.
           initialize xzoom-linkage xzoom-ext-info(1).
           move  zero                       to xzoom-row.
           move  zero                       to xzoom-cln.
           move  ext-file                   to xzoom-file-name(1).
           move  0                          to xzoom-file-key.
                                                        
           move  146                        to xzoom-sw. 
           move  8                          to xzoom-lw.

           move zero to idx.

      *    Definizione del campo 1
           move  1                          to Idx.
           move  4                          to xzoom-field-length(Idx).
           move  0                          to xzoom-field-offset(Idx).
           move "Anno"                      to xzoom-field-name(Idx).  
           move 7                           to xzoom-field-column(Idx).
           move 4                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "###0"                      to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      *    Definizione del campo 2
           add   1                          to Idx.
           move  8                          to xzoom-field-length(Idx).
           move  4                          to xzoom-field-offset(Idx).
           move "Evasione"                  to xzoom-field-name(Idx).  
           move 8                           to xzoom-field-column(Idx).
           move 8                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "#######0"                  to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      *    Definizione del campo 3
           add   1                          to Idx.
           move  10                         to xzoom-field-length(Idx).
           move  12                         to xzoom-field-offset(Idx).
           move "Data"                      to xzoom-field-name(Idx).  
           move 9                           to xzoom-field-column(Idx).
           set  xzoom-ft-alpha(idx)         to true. 

      *    Definizione del campo 4
           add   1                          to Idx.
           move  4                          to xzoom-field-length(Idx).
           move  22                         to xzoom-field-offset(Idx).
           move "Causale"                   to xzoom-field-name(Idx).  
           move 8                           to xzoom-field-column(Idx).
           set  xzoom-ft-alpha(idx)         to true. 
      
      *    Definizione del campo 5
           add   1                          to Idx.
           move  5                          to xzoom-field-length(Idx).
           move  26                         to xzoom-field-offset(Idx).
           move "Cliente"                   to xzoom-field-name(Idx).  
           move 6                           to xzoom-field-column(Idx).
           move 5                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "####0"                     to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.
      
      *    Definizione del campo 6
           add  1                           to Idx.
           move 40                          to xzoom-field-length(idx).
           move 31                          to xzoom-field-offset(idx).
           move "Ragione Sociale"           to xzoom-field-name(idx).  
           move 25                          to xzoom-field-column(Idx).
           set  xzoom-ft-alpha(idx)         to true. 
      
      *    Definizione del campo 7
           add   1                          to Idx.
           move  5                          to xzoom-field-length(idx).
           move  71                         to xzoom-field-offset(idx).
           move  7                          to xzoom-field-column(idx).
           move "Destino"                   to xzoom-field-name(idx).  
           move 5                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "####0"                     to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.
      
      *    Definizione del campo 8 
           add  1                           to Idx.
           move 35                          to xzoom-field-length(idx).
           move 76                          to xzoom-field-offset(idx).
           move "Località"                  to xzoom-field-name(idx).  
           move 25                          to xzoom-field-column(Idx).
           set  xzoom-ft-alpha(idx)         to true. 
      
      *    Definizione del campo 9
           add   1                          to Idx.
           move  4                          to xzoom-field-length(Idx).
           move  111                        to xzoom-field-offset(Idx).
           move "Anno"                      to xzoom-field-name(Idx).  
           move 7                           to xzoom-field-column(Idx).
           move 4                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "###0"                      to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.
      
      *    Definizione del campo 10
           add   1                          to Idx.
           move  8                          to xzoom-field-length(Idx).
           move 115                         to xzoom-field-offset(Idx).
           move "Bolla"                     to xzoom-field-name(Idx).  
           move 7                          to xzoom-field-column(Idx).
           move 8                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "#######0"                  to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

           move  idx                        to xzoom-fields.

           move  -1                         to xzoom-delimiter-offset.
           move  19                         to xzoom-delimiter-length.
           move "000"                       to xzoom-from-value.
           move "000"                       to xzoom-to-value.

      ***---
       PREPARA-ZOOM-TOR-MASTER.
           initialize xzoom-linkage xzoom-ext-info(1).
           move  zero                       to xzoom-row.
           move  zero                       to xzoom-cln.
           move  ext-file                   to xzoom-file-name(1).
           move  0                          to xzoom-file-key.
                                                        
           move  151                        to xzoom-sw. 
           move  8                          to xzoom-lw.

           move zero to idx.

      *    Definizione del campo 1
           move  1                          to Idx.
           move  4                          to xzoom-field-length(Idx).
           move  0                          to xzoom-field-offset(Idx).
           move "Anno"                      to xzoom-field-name(Idx).  
           move 8                           to xzoom-field-column(Idx).
           move 4                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "###0"                      to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      *    Definizione del campo 2
           add   1                          to Idx.
           move  8                          to xzoom-field-length(Idx).
           move  4                          to xzoom-field-offset(Idx).
           move "Evasione"                  to xzoom-field-name(Idx).  
           move 10                          to xzoom-field-column(Idx).
           move 8                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "#######0"                  to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      *    Definizione del campo 3
           add   1                          to Idx.
           move  10                         to xzoom-field-length(Idx).
           move  12                         to xzoom-field-offset(Idx).
           move "Data"                      to xzoom-field-name(Idx).  
           move 11                          to xzoom-field-column(Idx).
           set  xzoom-ft-alpha(idx)         to true. 
           set  xzoom-al-center(Idx)        to true.

      *    Definizione del campo 4
           add   1                          to Idx.
           move  10                         to xzoom-field-length(Idx).
           move  22                         to xzoom-field-offset(Idx).
           move "Data B"                    to xzoom-field-name(Idx).  
           move 11                          to xzoom-field-column(Idx).
           set  xzoom-ft-alpha(idx)         to true. 
           set  xzoom-al-center(Idx)        to true.
      
      *    Definizione del campo 5
           add   1                          to Idx.
           move  8                          to xzoom-field-length(Idx).
           move  32                         to xzoom-field-offset(Idx).
           move "Num. B"                    to xzoom-field-name(Idx).  
           move 7                           to xzoom-field-column(Idx).
           move 8                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "#######0"                  to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      *    Definizione del campo 6
           add   1                          to Idx.
           move  10                         to xzoom-field-length(Idx).
           move  40                         to xzoom-field-offset(Idx).
           move "Data F"                    to xzoom-field-name(Idx).  
           move 11                          to xzoom-field-column(Idx).
           set  xzoom-ft-alpha(idx)         to true. 
           set  xzoom-al-center(Idx)        to true.
      
      *    Definizione del campo 7
           add   1                          to Idx.
           move  8                          to xzoom-field-length(Idx).
           move  50                         to xzoom-field-offset(Idx).
           move "Num. F"                    to xzoom-field-name(Idx).  
           move 6                           to xzoom-field-column(Idx).
           move 8                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "#######0"                  to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.
      
      *    Definizione del campo 8 
           add  1                           to Idx.
           move 30                          to xzoom-field-length(idx).
           move 58                          to xzoom-field-offset(idx).
           move "Vettore"                   to xzoom-field-name(idx).  
           move 25                          to xzoom-field-column(Idx).
           set  xzoom-ft-alpha(idx)         to true. 

           if como-file = "zoom-tor-master-b"
      *       Definizione del campo 9
              add   1                       to Idx
              move  30                      to xzoom-field-length(Idx)
              move  88                      to xzoom-field-offset(Idx)
              move "Esito"                  to xzoom-field-name(Idx)
              move 20                       to xzoom-field-column(Idx)
              set  xzoom-ft-alpha(idx)      to true

      *       Definizione del campo 10
              add   1                       to Idx
              move  6                       to xzoom-field-length(Idx)
              move  118                     to xzoom-field-offset(Idx)
              move "Qtà"                    to xzoom-field-name(Idx)
              move 5                        to xzoom-field-column(Idx)
              move 6                        to xzoom-field-digits(Idx)
              move 0                        to xzoom-field-dec(Idx)
              move "###.###0"               to xzoom-field-fmt(Idx)
              set xzoom-al-right(Idx)          to true
              set xzoom-field-unsigned(Idx )   to true
              set xzoom-ft-display(Idx)        to true
           else
      *       Definizione del campo 9
              add   1                       to Idx
              move  30                      to xzoom-field-length(Idx)
              move  88                      to xzoom-field-offset(Idx)
              move "Esito"                  to xzoom-field-name(Idx)
              move 25                       to xzoom-field-column(Idx)
              set  xzoom-ft-alpha(idx)      to true
           end-if.

           move  idx                        to xzoom-fields.

           move  -1                         to xzoom-delimiter-offset.
           move  19                         to xzoom-delimiter-length.
           move "000"                       to xzoom-from-value.
           move "000"                       to xzoom-to-value.

      ***---
       PREPARA-ZOOM-TOR-MASTER-C.
           initialize xzoom-linkage xzoom-ext-info(1).
           move  zero                       to xzoom-row.
           move  zero                       to xzoom-cln.
           move  ext-file                   to xzoom-file-name(1).
           move  0                          to xzoom-file-key.
                                                        
           move  86                         to xzoom-sw. 
           move  8                          to xzoom-lw.

           move zero to idx.

      *    Definizione del campo 1
           move  1                          to Idx.
           move  4                          to xzoom-field-length(Idx).
           move  0                          to xzoom-field-offset(Idx).
           move "Anno"                      to xzoom-field-name(Idx).  
           move 8                           to xzoom-field-column(Idx).
           move 4                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "###0"                      to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      *    Definizione del campo 2
           add   1                          to Idx.
           move  8                          to xzoom-field-length(Idx).
           move  4                          to xzoom-field-offset(Idx).
           move "Evasione"                  to xzoom-field-name(Idx).  
           move 10                          to xzoom-field-column(Idx).
           move 8                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "#######0"                  to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      *    Definizione del campo 3
           add   1                          to Idx.
           move  10                         to xzoom-field-length(Idx).
           move  12                         to xzoom-field-offset(Idx).
           move "Data"                      to xzoom-field-name(Idx).  
           move 11                          to xzoom-field-column(Idx).
           set  xzoom-ft-alpha(idx)         to true. 
           set  xzoom-al-center(Idx)        to true.
      
      *    Definizione del campo 4
           add  1                           to Idx.
           move 30                          to xzoom-field-length(idx).
           move 58                          to xzoom-field-offset(idx).
           move "N. Ord. Cli."              to xzoom-field-name(idx).  
           move 15                          to xzoom-field-column(Idx).
           set  xzoom-ft-alpha(idx)         to true. 

      *    Definizione del campo 5
           add   1                       to Idx
           move  30                      to xzoom-field-length(Idx)
           move  88                      to xzoom-field-offset(Idx)
           move "Stato"                  to xzoom-field-name(Idx)
           move 20                       to xzoom-field-column(Idx)
           set  xzoom-ft-alpha(idx)      to true.

           move  idx                        to xzoom-fields.

           move  -1                         to xzoom-delimiter-offset.
           move  19                         to xzoom-delimiter-length.
           move "000"                       to xzoom-from-value.
           move "000"                       to xzoom-to-value.

      ***---
       PREPARA-TMP-ASSORCLI-ZOOM.
           initialize xzoom-linkage xzoom-ext-info(1).
           move  zero                       to xzoom-row.
           move  zero                       to xzoom-cln.
           move  15                         to xzoom-lw.
           move  170                        to xzoom-sw.
           move  ext-file                   to xzoom-file-name(1).
           move  1                          to xzoom-file-key.

           move zero to idx.

      *    Definizione del campo 1
           move 1                           to Idx.
           move 5                           to xzoom-field-length(Idx).
           move 0                           to xzoom-field-offset(Idx).
           move "Gruppo"                    to xzoom-field-name(Idx).  
           move 10                          to xzoom-field-column(Idx).
           set  xzoom-ft-alpha(idx)         to true. 

      *    Definizione del campo 2
           add  1                           to Idx.

           move 30                          to xzoom-field-length(idx).
           move 21                          to xzoom-field-offset(idx).
           move 25                          to xzoom-field-column(idx).
           move "Intestazione"              to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)         to true. 

      *    Definizione del campo 3
           add  1                           to Idx.
           move 5                           to xzoom-field-length(idx).
           move 5                           to xzoom-field-offset(idx).
           move "Cliente"                   to xzoom-field-name(idx).  
           move 5                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "####0"                     to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.
           move 6                           to xzoom-field-column(Idx).

      *    Definizione del campo 4
           add  1                           to Idx.
           move 40                          to xzoom-field-length(idx).
           move 51                          to xzoom-field-offset(idx).
           move 25                          to xzoom-field-column(idx).
           move "Cognome/Rag. sociale"      to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)         to true. 

      *    Definizione del campo 5
           add  1                           to Idx.
           move 5                           to xzoom-field-length(idx).
           move 10                          to xzoom-field-offset(idx).
           move "Destino"                   to xzoom-field-name(idx).  
           move 5                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "####0"                     to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.
           move 6                           to xzoom-field-column(Idx).

      *    Definizione del campo 6 (descrizione destino)
           add   1                          to Idx.
           move  35                         to xzoom-field-length(idx).
           move  91                         to xzoom-field-offset(idx).
           move  25                         to xzoom-field-column(idx).
           move "Destinazione"              to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)         to true. 
            
      *    Definizione del campo 7
           add   1                          to Idx.
           move  6                          to xzoom-field-length(idx).
           move  15                         to xzoom-field-offset(idx).
           move  6                          to xzoom-field-column(idx).
           move "Articolo"                  to xzoom-field-name(idx).  
           move 6                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "#####0"                    to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.
      
      *    Definizione del campo 8
           add   1                          to Idx.
           move  50                         to xzoom-field-length(idx).
           move  126                        to xzoom-field-offset(idx).
           move  25                         to xzoom-field-column(idx).
           move "Descrizione"               to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)         to true. 

           move  idx                        to xzoom-fields.

           move  -1                         to xzoom-delimiter-offset.
           move  19                         to xzoom-delimiter-length.
           move "000"                       to xzoom-from-value.
           move "000"                       to xzoom-to-value.

      ***---
       PREPARA-ZOOM-TORDINI-FATTURE.
           perform PREPARA-ZOOM-TORDINI.
           evaluate como-file
           when "zoom-fatture"
                move  8 to xzoom-fields
                move "N. Fattura"             to xzoom-field-name(2)
                move "Data Fattura"           to xzoom-field-name(3)
           when "zoom-fatture-b"
                move  8 to xzoom-fields
                move "N. Bolla"               to xzoom-field-name(2)
                move "Data Bolla"             to xzoom-field-name(3)
           when "zoom-note-fatt"
                move  8 to xzoom-fields
                move "N. Fattura"             to xzoom-field-name(2)
                move "Data Fattura"           to xzoom-field-name(3)
           when "zoom-note-clidest"
                move  150                     to xzoom-sw
                move  3                       to xzoom-file-key
                move  8                       to xzoom-fields
                move "Contestazione"          to xzoom-field-name(2)
                move "Data"                   to xzoom-field-name(3)
           when "zoom-note"
                move  170                     to xzoom-sw
      *         CAMPO 9
                add  1 to idx
                move  8                       to xzoom-field-length(idx)
                move  125                     to xzoom-field-offset(idx)
                move  9                       to xzoom-field-column(idx)
                move "N. Nota"                to xzoom-field-name(idx)
                move 8                        to xzoom-field-digits(Idx)
                move 0                        to xzoom-field-dec(Idx)
                move "#######0"               to xzoom-field-fmt(Idx)
                set xzoom-al-right(Idx)       to true
                set xzoom-field-unsigned(Idx) to true
                set xzoom-ft-display(Idx)     to true

      *         CAMPO 10
                add  1 to idx
                move  8                       to xzoom-field-length(idx)
                move  133                     to xzoom-field-offset(idx)
                move "Data Nota"              to xzoom-field-name(idx)
                set xzoom-field-unsigned(Idx) to true
                set xzoom-ft-display(Idx)     to true
                move 8                        to xzoom-field-digits(Idx)
                move   0                      to xzoom-field-dec(Idx)
                move "AAAAMMGG-GG/MM/AAAA"    to xzoom-field-fmt(Idx)
                move  9                       to xzoom-field-column(Idx)
                                                                     
                move  10                      to xzoom-fields   
                move  6                       to xzoom-field-column(5)   
                move  6                       to xzoom-field-column(7)
                move  9                       to xzoom-field-column(2)
                move "Evasione"               to xzoom-field-name(2)
                move "Data"                   to xzoom-field-name(3)

                move  3                       to xzoom-file-key

           end-evaluate.

      ***---
       PREPARA-ZOOM-TORDINI.
           perform PREPARA-VIDEO-ZOOM-TORDINI.
           move 8 to xzoom-fields.

      ***---
       PREPARA-ZOOM-TORDINI-BOLLE.
           perform PREPARA-ZOOM-TORDINI.
           move "N. Bolla"                 to xzoom-field-name(2).
           move "Data Bolla"               to xzoom-field-name(3).

      ***---
       PREPARA-ZOOM-TORDINI-DATI-FAT.
           perform PREPARA-VIDEO-ZOOM-TORDINI.
           move  170                        to xzoom-sw.
           move 10 to xzoom-fields.

      * CAMPO 9
           add  1 to idx
           move  8                          to xzoom-field-length(idx).
           move  125                        to xzoom-field-offset(idx).
           move  8                          to xzoom-field-column(idx).
           move "N. Fattura"                to xzoom-field-name(idx).
           move 9                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "#######0"                  to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      * CAMPO 10
           add  1 to Idx.
           move 8                           to xzoom-field-length(idx).
           move 133                         to xzoom-field-offset(idx).
           move "Data Fattura"              to xzoom-field-name(idx).
           set xzoom-field-unsigned(Idx)    to true.
           set xzoom-ft-display(Idx)        to true.
           move 8                           to xzoom-field-digits(Idx).
           move   0                         to xzoom-field-dec(Idx).
           move "AAAAMMGG-GG/MM/AAAA"       to xzoom-field-fmt(Idx).
           move  9                          to xzoom-field-column(Idx).

      ***---
       PREPARA-VIDEO-ZOOM-TORDINI.
           initialize xzoom-linkage xzoom-ext-info(1).

           move  zero to idx.
           move  zero                       to xzoom-row.
           move  zero                       to xzoom-cln.
           move  16                         to xzoom-lw.
           move  153                        to xzoom-sw.
           move  ext-file                   to xzoom-file-name(1).
           move  0                          to xzoom-file-key.
           move  1                          to xzoom-file-key.

      * CAMPO 1
           add  1 to idx
           move  4                          to xzoom-field-length(idx).
           move  0                          to xzoom-field-offset(idx).
           move  6                          to xzoom-field-column(idx).
           move "Anno"                      to xzoom-field-name(idx).
           move 4                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "###0"                      to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      * CAMPO 2
           add  1 to idx
           move  8                          to xzoom-field-length(idx).
           move  4                          to xzoom-field-offset(idx).
           move  11                         to xzoom-field-column(idx).
           move "N. Ordine"                 to xzoom-field-name(idx).
           move 8                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "#######0"                  to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      * CAMPO 3
           add  1 to Idx.
           move 8                           to xzoom-field-length(idx).
           move 12                          to xzoom-field-offset(idx).
           move "Data Ordine"               to xzoom-field-name(idx).
           set xzoom-field-unsigned(Idx)    to true.
           set xzoom-ft-display(Idx)        to true.
           move 10                          to xzoom-field-digits(Idx).
           move  0                          to xzoom-field-dec(Idx).
           move "AAAAMMGG-GG/MM/AAAA"       to xzoom-field-fmt(Idx).
           move 9                           to xzoom-field-column(Idx).
           set xzoom-al-center(Idx)         to true.

      * CAMPO 4
           add  1  to idx.
           move 20                          to xzoom-field-length(idx).
           move 15                          to xzoom-field-column(idx).
           move 20                          to xzoom-field-offset(idx).
           move "Causale"                   to xzoom-field-name(idx).
           set xzoom-ft-alpha(idx)          to true.

      * CAMPO 5
           add  1 to idx.
           move 5                           to xzoom-field-length(idx).
           move 7                           to xzoom-field-column(idx).
           move 40                          to xzoom-field-offset(idx).
           move "Cliente"                   to xzoom-field-name(idx).
           move 5                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "####0"                     to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      * CAMPO 6
           add  1  to idx.
           move 40                          to xzoom-field-length(idx).
           move 30                          to xzoom-field-column(idx).
           move 45                          to xzoom-field-offset(idx).
           move "Ragione sociale"           to xzoom-field-name(idx).  
           set xzoom-ft-alpha(idx)          to true.

      * CAMPO 7
           add  1 to idx.
           move 5                           to xzoom-field-length(idx).
           move 7                           to xzoom-field-column(idx).
           move 85                          to xzoom-field-offset(idx).
           move "Destino"                   to xzoom-field-name(idx).  
           move 5                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "#####"                     to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      * CAMPO 8
           add  1  to idx.
           move 35                          to xzoom-field-length(idx).
           move 29                          to xzoom-field-column(idx).
           move 90                          to xzoom-field-offset(idx).
           move "Destinazione"              to xzoom-field-name(idx).  
           set xzoom-ft-alpha(idx)          to true.

           move  -1                         to xzoom-delimiter-offset.
           move  5                          to xzoom-delimiter-length.
           move "000"                       to xzoom-from-value.
           move "000"                       to xzoom-to-value.

      ***---
       PREPARA-ZOOM-TNOTACR.
           initialize xzoom-linkage xzoom-ext-info(1).

           move  zero to idx.
           move  zero                       to xzoom-row.
           move  zero                       to xzoom-cln.
           move  16                         to xzoom-lw.
           move  145                        to xzoom-sw.
           move  ext-file                   to xzoom-file-name(1).
           move  8                          to xzoom-fields.

      * CAMPO 1
           add  1 to idx
           move  4                          to xzoom-field-length(idx).
           move  0                          to xzoom-field-offset(idx).
           move  7                          to xzoom-field-column(idx).
           move "Anno"                      to xzoom-field-name(idx).
           move 4                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "###0"                      to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      * CAMPO 2
           add  1 to idx
           move  8                          to xzoom-field-length(idx).
           move  4                          to xzoom-field-offset(idx).
           move  7                          to xzoom-field-column(idx).
           move "Numero"                    to xzoom-field-name(idx).
           move 8                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "#######0"                  to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      * CAMPO 3
           add  1 to Idx.
           move 8                           to xzoom-field-length(idx).
           move 12                          to xzoom-field-offset(idx).
           move "Data"                      to xzoom-field-name(idx).
           set xzoom-field-unsigned(Idx)    to true.
           set xzoom-ft-display(Idx)        to true.
           move 10                          to xzoom-field-digits(Idx).
           move   0                         to xzoom-field-dec(Idx).
           move "AAAAMMGG-GG/MM/AAAA"       to xzoom-field-fmt(Idx).
           move 13                          to xzoom-field-column(Idx).

      * CAMPO 4
           add  1  to idx.
           move 5                           to xzoom-field-length(idx).
           move 8                           to xzoom-field-column(idx).
           move 20                          to xzoom-field-offset(idx).
           move "Cliente"                   to xzoom-field-name(idx).
           move 5                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "####0"                     to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      * CAMPO 5
           add  1 to idx.
           move 5                           to xzoom-field-length(idx).
           move 8                           to xzoom-field-column(idx).
           move 25                          to xzoom-field-offset(idx).
           move "Destino"                   to xzoom-field-name(idx).  
           move 5                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "#####"                     to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      * CAMPO 6
           add  1  to idx.
           move 35                          to xzoom-field-length(idx).
           move 30                          to xzoom-field-column(idx).
           move 30                          to xzoom-field-offset(idx).
           move "Destinazione"              to xzoom-field-name(idx).  
           set xzoom-ft-alpha(idx)          to true.

      * CAMPO 7
           add  1 to idx.
           move 4                           to xzoom-field-length(idx).
           move 8                           to xzoom-field-column(idx).
           move 65                          to xzoom-field-offset(idx).
           move "Causale"                   to xzoom-field-name(idx).
           set xzoom-ft-alpha(idx)          to true.

      * CAMPO 8
           add  1  to idx.
           move 40                          to xzoom-field-length(idx).
           move 30                          to xzoom-field-column(idx).
           move 69                          to xzoom-field-offset(idx).
           move "Descrizione"               to xzoom-field-name(idx).  
           set xzoom-ft-alpha(idx)          to true.

           move  -1                         to xzoom-delimiter-offset.
           move  5                          to xzoom-delimiter-length.
           move "000"                       to xzoom-from-value.
           move "000"                       to xzoom-to-value.

      ***---
       PREPARA-ZOOM-TOR-BOLLE.
           initialize xzoom-linkage xzoom-ext-info(1).

           move  zero to idx.
           move  zero                       to xzoom-row.
           move  zero                       to xzoom-cln.



           move  16                         to xzoom-lw.
           move  114                        to xzoom-sw.
           move  ext-file                   to xzoom-file-name(1).
           move  7                          to xzoom-fields.

      * CAMPO 1
           add  1 to idx
           move  4                          to xzoom-field-length(idx).
           move  0                          to xzoom-field-offset(idx).
           move  7                          to xzoom-field-column(idx).
           move "Anno"                      to xzoom-field-name(idx).
           move 4                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "###0"                      to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      * CAMPO 2
           add  1 to idx
           move  8                          to xzoom-field-length(idx).
           move  4                          to xzoom-field-offset(idx).
           move  7                          to xzoom-field-column(idx).
           move "N. Bolla"                  to xzoom-field-name(idx).
           move 8                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "#######0"                  to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      * CAMPO 3
           add  1 to Idx.
           move 8                           to xzoom-field-length(idx).
           move 12                          to xzoom-field-offset(idx).
           move "Data Fatturazione"         to xzoom-field-name(idx).
           set xzoom-field-unsigned(Idx)    to true.
           set xzoom-ft-display(Idx)        to true.
           move 10                          to xzoom-field-digits(Idx).
           move   0                         to xzoom-field-dec(Idx).
           move "AAAAMMGG-GG/MM/AAAA"       to xzoom-field-fmt(Idx).
           move 13                          to xzoom-field-column(Idx).

      * CAMPO 4
           add  1 to idx
           move  8                          to xzoom-field-length(idx).
           move  20                         to xzoom-field-offset(idx).
           move  14                         to xzoom-field-column(idx).
           move "N. Registrazione"          to xzoom-field-name(idx).
           move 8                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "#######0"                  to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      * CAMPO 5
           add  1  to idx.
           move 5                           to xzoom-field-length(idx).
           move 8                           to xzoom-field-column(idx).
           move 28                          to xzoom-field-offset(idx).
           move "Cliente"                   to xzoom-field-name(idx).
           move 5                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "####0"                     to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      * CAMPO 6
           add  1  to idx.
           move 40                          to xzoom-field-length(idx).
           move 30                          to xzoom-field-column(idx).
           move 33                          to xzoom-field-offset(idx).
           move "Cognome/Rag. sociale"      to xzoom-field-name(idx).  
           set xzoom-ft-alpha(idx)          to true.

      * CAMPO 7
           add  1 to idx.
           move 4                           to xzoom-field-length(idx).
           move 8                           to xzoom-field-column(idx).
           move 73                          to xzoom-field-offset(idx).
           move "Causale"                   to xzoom-field-name(idx).
           set xzoom-ft-alpha(idx)          to true.

           move  -1                         to xzoom-delimiter-offset.
           move  5                          to xzoom-delimiter-length.
           move "000"                       to xzoom-from-value.
           move "000"                       to xzoom-to-value.

      ***---
       PREPARA-ZOOM-TOR-FAT.
           initialize xzoom-linkage xzoom-ext-info(1).

           move  zero to idx.
           move  zero                       to xzoom-row.
           move  zero                       to xzoom-cln.
           move  16                         to xzoom-lw.
           move  114                        to xzoom-sw.
           move  ext-file                   to xzoom-file-name(1).
           move  7                          to xzoom-fields.

      * CAMPO 1
           add  1 to idx
           move  4                          to xzoom-field-length(idx).
           move  0                          to xzoom-field-offset(idx).
           move  7                          to xzoom-field-column(idx).
           move "Anno"                      to xzoom-field-name(idx).
           move 4                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "###0"                      to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      * CAMPO 2
           add  1 to idx
           move  8                          to xzoom-field-length(idx).
           move  4                          to xzoom-field-offset(idx).
           move  7                          to xzoom-field-column(idx).
           move "N. Fattura"                to xzoom-field-name(idx).
           move 8                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "#######0"                  to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      * CAMPO 3
           add  1 to Idx.
           move 8                           to xzoom-field-length(idx).
           move 12                          to xzoom-field-offset(idx).
           move "Data Fatturazione"         to xzoom-field-name(idx).
           set xzoom-field-unsigned(Idx)    to true.
           set xzoom-ft-display(Idx)        to true.
           move 10                          to xzoom-field-digits(Idx).
           move   0                         to xzoom-field-dec(Idx).
           move "AAAAMMGG-GG/MM/AAAA"       to xzoom-field-fmt(Idx).
           move 13                          to xzoom-field-column(Idx).

      * CAMPO 4
           add  1 to idx
           move  8                          to xzoom-field-length(idx).
           move  20                         to xzoom-field-offset(idx).
           move  14                         to xzoom-field-column(idx).
           move "N. Registrazione"          to xzoom-field-name(idx).
           move 8                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "#######0"                  to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      * CAMPO 5
           add  1  to idx.
           move 5                           to xzoom-field-length(idx).
           move 8                           to xzoom-field-column(idx).
           move 28                          to xzoom-field-offset(idx).
           move "Cliente"                   to xzoom-field-name(idx).
           move 5                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "####0"                     to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      * CAMPO 6
           add  1  to idx.
           move 40                          to xzoom-field-length(idx).
           move 30                          to xzoom-field-column(idx).
           move 33                          to xzoom-field-offset(idx).
           move "Cognome/Rag. sociale"      to xzoom-field-name(idx).  
           set xzoom-ft-alpha(idx)          to true.

      * CAMPO 7
           add  1 to idx.
           move 4                           to xzoom-field-length(idx).
           move 8                           to xzoom-field-column(idx).
           move 73                          to xzoom-field-offset(idx).
           move "Causale"                   to xzoom-field-name(idx).
           set xzoom-ft-alpha(idx)          to true.

           move  -1                         to xzoom-delimiter-offset.
           move  5                          to xzoom-delimiter-length.
           move "000"                       to xzoom-from-value.
           move "000"                       to xzoom-to-value.

      ***---
       PREPARA-ZOOM-TNO-FAT.
           initialize xzoom-linkage xzoom-ext-info(1).

           move  zero to idx.
           move  zero                       to xzoom-row.
           move  zero                       to xzoom-cln.
           move  16                         to xzoom-lw.
           move  122                        to xzoom-sw.
           move  ext-file                   to xzoom-file-name(1).
           move  7                          to xzoom-fields.

      * CAMPO 1
           add  1 to idx
           move  4                          to xzoom-field-length(idx).
           move  0                          to xzoom-field-offset(idx).
           move  7                          to xzoom-field-column(idx).
           move "Anno"                      to xzoom-field-name(idx).
           move 4                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "###0"                      to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      * CAMPO 2
           add  1 to idx
           move  8                          to xzoom-field-length(idx).
           move  4                          to xzoom-field-offset(idx).
           move  12                         to xzoom-field-column(idx).
           move "N. Nota Credito"           to xzoom-field-name(idx).
           move 8                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "#######0"                  to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      * CAMPO 3
           add  1 to Idx.
           move 8                           to xzoom-field-length(idx).
           move 12                          to xzoom-field-offset(idx).
           move "Data Fatturazione"         to xzoom-field-name(idx).
           set xzoom-field-unsigned(Idx)    to true.
           set xzoom-ft-display(Idx)        to true.
           move 10                          to xzoom-field-digits(Idx).
           move   0                         to xzoom-field-dec(Idx).
           move "AAAAMMGG-GG/MM/AAAA"       to xzoom-field-fmt(Idx).
           move 13                          to xzoom-field-column(Idx).

      * CAMPO 4
           add  1 to idx
           move  8                          to xzoom-field-length(idx).
           move  20                         to xzoom-field-offset(idx).
           move  14                         to xzoom-field-column(idx).
           move "N. Registrazione"          to xzoom-field-name(idx).
           move 8                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "#######0"                  to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      * CAMPO 5
           add  1  to idx.
           move 5                           to xzoom-field-length(idx).
           move 8                           to xzoom-field-column(idx).
           move 28                          to xzoom-field-offset(idx).
           move "Cliente"                   to xzoom-field-name(idx).
           move 5                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "####0"                     to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      * CAMPO 6
           add  1  to idx.
           move 40                          to xzoom-field-length(idx).
           move 30                          to xzoom-field-column(idx).
           move 33                          to xzoom-field-offset(idx).
           move "Cognome/Rag. sociale"      to xzoom-field-name(idx).  
           set xzoom-ft-alpha(idx)          to true.

      * CAMPO 7
           add  1 to idx.
           move 4                           to xzoom-field-length(idx).
           move 8                           to xzoom-field-column(idx).
           move 73                          to xzoom-field-offset(idx).
           move "Causale"                   to xzoom-field-name(idx).
           set xzoom-ft-alpha(idx)          to true.

           move  -1                         to xzoom-delimiter-offset.
           move  5                          to xzoom-delimiter-length.
           move "000"                       to xzoom-from-value.
           move "000"                       to xzoom-to-value.

      ***---
       PREPARA-ZOOM-TOR-POSTEL.
           initialize xzoom-linkage xzoom-ext-info(1).

           move  zero to idx.
           move  zero                       to xzoom-row.
           move  zero                       to xzoom-cln.
           move  16                         to xzoom-lw.

           move  160                        to xzoom-sw.
           move  ext-file                   to xzoom-file-name(1).
           move  9                          to xzoom-fields.
           move  1                          to xzoom-file-key.

      * CAMPO 1
           add  1 to idx
           move  4                          to xzoom-field-length(idx).
           move  0                          to xzoom-field-offset(idx).
           move  7                          to xzoom-field-column(idx).
           move "Anno"                      to xzoom-field-name(idx).
           move 4                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "###0"                      to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      * CAMPO 2
           add  1 to idx
           move  8                          to xzoom-field-length(idx).
           move  4                          to xzoom-field-offset(idx).
           move  11                         to xzoom-field-column(idx).
           move "N. Fattura"                to xzoom-field-name(idx).
           move 8                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "#######0"                  to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      * CAMPO 3
           add  1 to Idx.
           move 8                           to xzoom-field-length(idx).
           move 14                          to xzoom-field-offset(idx).
           move "Data Fattura"              to xzoom-field-name(idx).
           set xzoom-field-unsigned(Idx)    to true.
           set xzoom-ft-display(Idx)        to true.
           move 10                          to xzoom-field-digits(Idx).
           move   0                         to xzoom-field-dec(Idx).
           move "AAAAMMGG-GG/MM/AAAA"       to xzoom-field-fmt(Idx).
           move 10                          to xzoom-field-column(Idx).
           set xzoom-al-center(Idx)         to true.

      * CAMPO 4
           add  1  to idx.
           move 20                          to xzoom-field-length(idx).
           move 13                          to xzoom-field-column(idx).
           move 20                          to xzoom-field-offset(idx).
           move "Causale"                   to xzoom-field-name(idx).
           set xzoom-ft-alpha(idx)          to true.


      * CAMPO 5
           add  1 to idx.
           move 5                           to xzoom-field-length(idx).
           move 8                           to xzoom-field-column(idx).
           move 40                          to xzoom-field-offset(idx).
           move "Cliente"                   to xzoom-field-name(idx).
           move 5                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "####0"                     to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      * CAMPO 6
           add  1  to idx.
           move 40                          to xzoom-field-length(idx).
           move 30                          to xzoom-field-column(idx).
           move 45                          to xzoom-field-offset(idx).
           move "Ragione sociale"           to xzoom-field-name(idx).  
           set xzoom-ft-alpha(idx)          to true.

      * CAMPO 7
           add  1 to idx.
           move 5                           to xzoom-field-length(idx).
           move 8                           to xzoom-field-column(idx).
           move 85                          to xzoom-field-offset(idx).
           move "Destino"                   to xzoom-field-name(idx).  
           move 5                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "#####"                     to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      * CAMPO 8
           add  1  to idx.
           move 35                          to xzoom-field-length(idx).
           move 28                          to xzoom-field-column(idx).
           move 90                          to xzoom-field-offset(idx).
           move "Destinazione"              to xzoom-field-name(idx).  
           set xzoom-ft-alpha(idx)          to true.

      * CAMPO 9
           add    1                         to idx.
           move   1                         to xzoom-field-length(idx).
           move   6                         to xzoom-field-column(idx).
           move 125                         to xzoom-field-offset(idx).
           move "Postel"                    to xzoom-field-name(idx).  
           set xzoom-ft-alpha(idx)          to true.

           move  -1                         to xzoom-delimiter-offset.
           move  5                          to xzoom-delimiter-length.
           move "000"                       to xzoom-from-value.
           move "000"                       to xzoom-to-value.

      ***---
       PREPARA-ZOOM-TARIFVET.
           initialize xzoom-linkage xzoom-ext-info(1).

           move  zero to idx.
           move  zero                       to xzoom-row.
           move  zero                       to xzoom-cln.
           move  16                         to xzoom-lw.
           move  154                        to xzoom-sw.
           move  ext-file                   to xzoom-file-name(1).
           move  6                          to xzoom-fields.

      * CAMPO 1
           add  1 to idx
           move  5                          to xzoom-field-length(idx).
           move  0                          to xzoom-field-offset(idx).
           move  7                          to xzoom-field-column(idx).
           move "Vettore"                   to xzoom-field-name(idx).
           move 5                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "####0"                      to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      * CAMPO 2
           add  1  to idx.
           move 40                          to xzoom-field-length(idx).
           move 30                          to xzoom-field-column(idx).
           move 15                          to xzoom-field-offset(idx).
           move "Descrizione"               to xzoom-field-name(idx).
           set xzoom-ft-alpha(idx)          to true.

      * CAMPO 3
           add  1  to idx.
           move  5                          to xzoom-field-length(idx).
           move 15                          to xzoom-field-column(idx).
           move  5                          to xzoom-field-offset(idx).
           move "Campo 1"                   to xzoom-field-name(idx).
           set xzoom-ft-alpha(idx)          to true.

      * CAMPO 4
           add  1 to idx.
           move 40                          to xzoom-field-length(idx).
           move 30                          to xzoom-field-column(idx).
           move 55                          to xzoom-field-offset(idx).
           move "Descrizione Campo 1"       to xzoom-field-name(idx).
           set xzoom-ft-alpha(idx)          to true.

      * CAMPO 5
           add  1 to idx.
           move 5                           to xzoom-field-length(idx).
           move 8                           to xzoom-field-column(idx).
           move 10                          to xzoom-field-offset(idx).
           move "Destino"                   to xzoom-field-name(idx).  
           move 5                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "#####"                     to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      * CAMPO 6
           add  1  to idx.
           move 35                          to xzoom-field-length(idx).
           move 25                          to xzoom-field-column(idx).
           move 95                          to xzoom-field-offset(idx).
           move "Destinazione"              to xzoom-field-name(idx).  
           set xzoom-ft-alpha(idx)          to true.

           move  -1                         to xzoom-delimiter-offset.
           move  5                          to xzoom-delimiter-length.
           move "000"                       to xzoom-from-value.
           move "000"                       to xzoom-to-value.

      ***---
       PREPARA-ZOOM-TCAUMAG.
           initialize xzoom-linkage xzoom-ext-info(1).

           move  zero to idx.
           move  zero                       to xzoom-row.
           move  zero                       to xzoom-cln.
           move  16                         to xzoom-lw.
           move  132                        to xzoom-sw.
           move  ext-file                   to xzoom-file-name(1).
           move  4                          to xzoom-fields.

      * CAMPO 1
           add  1 to idx
           move  4                          to xzoom-field-length(idx).
           move  0                          to xzoom-field-offset(idx).
           move 10                          to xzoom-field-column(idx).
           move "Codice"                    to xzoom-field-name(idx).
           set xzoom-ft-alpha(idx)          to true.

      * CAMPO 2
           add  1  to idx.
           move 40                          to xzoom-field-length(idx).
           move 40                          to xzoom-field-column(idx).
           move  4                          to xzoom-field-offset(idx).
           move "Descrizione"               to xzoom-field-name(idx).
           set xzoom-ft-alpha(idx)          to true.

      * CAMPO 3
           add  1  to idx.
           move  3                          to xzoom-field-length(idx).
           move 10                          to xzoom-field-column(idx).
           move 44                          to xzoom-field-offset(idx).
           move "Magazzino"                 to xzoom-field-name(idx).
           set xzoom-ft-alpha(idx)          to true.

      * CAMPO 4
           add  1 to idx.
           move 40                          to xzoom-field-length(idx).
           move 40                          to xzoom-field-column(idx).
           move 47                          to xzoom-field-offset(idx).
           move "Descrizione"               to xzoom-field-name(idx).
           set xzoom-ft-alpha(idx)          to true.

           move  -1                         to xzoom-delimiter-offset.
           move  5                          to xzoom-delimiter-length.
           move "000"                       to xzoom-from-value.
           move "000"                       to xzoom-to-value.

      ***---
       PREPARA-ZOOM-DISTINTEB.
           initialize xzoom-linkage xzoom-ext-info(1).

           move  zero to idx.
           move  zero                       to xzoom-row.
           move  zero                       to xzoom-cln.
           move  16                         to xzoom-lw.
           move  165                        to xzoom-sw.
           move  ext-file                   to xzoom-file-name(1).
           move  10                         to xzoom-fields.
           move  1                          to xzoom-file-key.

      * CAMPO 1
           add 1 to idx.
           move  5                       to xzoom-field-length(idx).
           move  0                       to xzoom-field-offset(idx).
           move  7                       to xzoom-field-column(idx).
           move "Codice"                 to xzoom-field-name(idx).
           move  5                       to xzoom-field-digits(idx).
           move  0                       to xzoom-field-dec(idx).
           move "####0"                  to xzoom-field-fmt(idx).
           set xzoom-al-right(idx)       to true.
           set xzoom-field-unsigned(idx) to true.
           set xzoom-ft-display(idx)     to true.

      * CAMPO 2
           add 1 to idx.
           move  6                       to xzoom-field-length(idx).
           move  5                       to xzoom-field-offset(idx).
           move  7                       to xzoom-field-column(idx).
           move "Articolo"               to xzoom-field-name(idx).
           move  6                       to xzoom-field-digits(idx).
           move  0                       to xzoom-field-dec(idx).
           move "#####0"                 to xzoom-field-fmt(idx).
           set xzoom-al-right(idx)       to true.
           set xzoom-field-unsigned(idx) to true.
           set xzoom-ft-display(idx)     to true.

      * CAMPO 3
           add  1  to idx.
           move 50                          to xzoom-field-length(idx).
           move 30                          to xzoom-field-column(idx).
           move 11                          to xzoom-field-offset(idx).
           move "Descrizione"               to xzoom-field-name(idx).
           set xzoom-ft-alpha(idx)          to true.

      * CAMPO 4
           add  1  to idx.
           move  3                          to xzoom-field-length(idx).
           move  5                          to xzoom-field-column(idx).
           move 61                          to xzoom-field-offset(idx).
           move "Mag."                      to xzoom-field-name(idx).
           set xzoom-ft-alpha(idx)          to true.

      * CAMPO 5
           add  1 to idx.
           move 50                          to xzoom-field-length(idx).
           move 25                          to xzoom-field-column(idx).
           move 64                          to xzoom-field-offset(idx).
           move "Descrizione"               to xzoom-field-name(idx).
           set xzoom-ft-alpha(idx)          to true.

      * CAMPO 6
           add  1 to idx.
           move  3                          to xzoom-field-length(idx).
           move  6                          to xzoom-field-column(idx).
           move 114                         to xzoom-field-offset(idx).
           move "Imballo"                   to xzoom-field-name(idx).
           set xzoom-ft-alpha(idx)          to true.

      * CAMPO 7
           add  1 to idx.
           move 50                          to xzoom-field-length(idx).
           move 20                          to xzoom-field-column(idx).
           move 117                         to xzoom-field-offset(idx).
           move "Descrizione"               to xzoom-field-name(idx).
           set xzoom-ft-alpha(idx)          to true.
      
      * CAMPO 8
           add   1                          to idx.
           move  6                          to xzoom-field-length(idx).
           move  167                        to xzoom-field-offset(idx).
           move  7                          to xzoom-field-column(idx).
           move "Peso"                      to xzoom-field-name(idx).  
           move 6                           to xzoom-field-digits(idx).
           move 3                           to xzoom-field-dec(idx).
           move "##0,000"                   to xzoom-field-fmt(idx).
           set xzoom-al-right(idx)          to true.
           set xzoom-field-unsigned(idx)    to true.
           set xzoom-ft-display(idx)        to true.
      
      * CAMPO 9
           add   1                          to idx.
           move  6                          to xzoom-field-length(idx).
           move  173                        to xzoom-field-offset(idx).
           move  7                          to xzoom-field-column(idx).
           move "di cui UTF"                to xzoom-field-name(idx).  
           move 6                           to xzoom-field-digits(idx).
           move 3                           to xzoom-field-dec(idx).
           move "##0,000"                   to xzoom-field-fmt(idx).
           set xzoom-al-right(idx)          to true.
           set xzoom-field-unsigned(idx)    to true.
           set xzoom-ft-display(idx)        to true.

      * CAMPO 10
           add   1                          to idx.
           move  14                         to xzoom-field-length(idx).
           move  179                        to xzoom-field-offset(idx).
           move  10                          to xzoom-field-column(idx).
           move "Costo MP"                  to xzoom-field-name(idx).  
           move 14                          to xzoom-field-digits(idx).
           move 2                           to xzoom-field-dec(idx).
           move "###.###.###.##0,00"        to xzoom-field-fmt(idx).
           set xzoom-al-right(idx)          to true.
           set xzoom-field-unsigned(idx)    to true.
           set xzoom-ft-display(idx)        to true.

           move  -1                         to xzoom-delimiter-offset.
           move  5                          to xzoom-delimiter-length.
           move "000"                       to xzoom-from-value.
           move "000"                       to xzoom-to-value.

      ***---
       PREPARA-TAGLI.
           initialize xzoom-linkage xzoom-ext-info(1).

           move  zero to idx.
           move  zero                       to xzoom-row.
           move  zero                       to xzoom-cln.
           move  16                         to xzoom-lw.
           move  143                        to xzoom-sw.
           move "tagli"                     to xzoom-file-name(1).
           move "tgrupgdo"                  to xzoom-file-name(2).
           move "articoli"                  to xzoom-file-name(3).
           move  7                          to xzoom-fields.

      * CAMPO 1
           move 1 to idx.
           move 8                           to xzoom-field-length(idx).
           move 0                           to xzoom-field-offset(idx).
           move "Data"                      to xzoom-field-name(idx).
           set xzoom-field-unsigned(Idx)    to true.
           set xzoom-ft-display(Idx)        to true.
           set xzoom-al-center(Idx)         to true.
           move 10                          to xzoom-field-digits(Idx).
           move   0                         to xzoom-field-dec(Idx).
           move "AAAAMMGG-GG/MM/AAAA"       to xzoom-field-fmt(Idx).
           move 9                           to xzoom-field-column(Idx).

      * CAMPO 2
           add  1 to idx.
           move 5                           to xzoom-field-length(idx).
           move 7                           to xzoom-field-column(idx).
           move 8                           to xzoom-field-offset(idx).
           move "GDO"                       to xzoom-field-name(idx).
           set xzoom-ft-alpha(Idx)          to true.

      * CAMPO 3
           add  1  to idx.
           move 2                           to xzoom-field-file(idx).
           move 1                           to xzoom-field-rel(idx).
           move 30                          to xzoom-field-length(idx).
           move 30                          to xzoom-field-column(idx).
           move 5                           to xzoom-field-offset(idx).
           move "Descrizione"               to xzoom-field-name(idx).  
           set xzoom-ft-alpha(idx)          to true.

      * CAMPO 4
           add   1                          to Idx.
           move  6                          to xzoom-field-length(idx).
           move  13                         to xzoom-field-offset(idx).
           move  6                          to xzoom-field-column(idx).
           move "Articolo"                  to xzoom-field-name(idx).  
           move 6                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "#####0"                    to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      * CAMPO 5
           add  1  to idx.
           move 3                           to xzoom-field-file(idx).
           move 2                           to xzoom-field-rel(idx).
           move 50                          to xzoom-field-length(idx).
           move 34                          to xzoom-field-column(idx).
           move 6                           to xzoom-field-offset(idx).
           move "Descrizione"               to xzoom-field-name(idx).  
           set xzoom-ft-alpha(idx)          to true.
      
      *    CAMPO 6
           add   1                          to idx.
           move  14                         to xzoom-field-length(idx).
           move  19                         to xzoom-field-offset(idx).
           move  11                         to xzoom-field-column(idx).
           move "Prezzo"                    to xzoom-field-name(idx).
           move 14                          to xzoom-field-digits(Idx).
           move 2                           to xzoom-field-dec(Idx).
           move "###.###.###.##0,00"        to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-signed(Idx )     to true.
           set xzoom-ft-display(Idx)        to true.

      *    CAMPO 7
           add   1                          to Idx.
           move  8                          to xzoom-field-length(idx).
           move  27                         to xzoom-field-offset(idx).
           move  10                         to xzoom-field-column(idx).
           move "Q.tà"                      to xzoom-field-name(idx).  
           move 8                           to xzoom-field-digits(Idx).
           move "##.###.##0"                to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-signed(Idx )     to true.
           set xzoom-ft-display(Idx)        to true.

           move  -1                         to xzoom-delimiter-offset.
           move  5                          to xzoom-delimiter-length.
           move "000"                       to xzoom-from-value.
           move "000"                       to xzoom-to-value.

      *    File Reference Settings - Relazione tagli > tgrupgdo
           move   1                to Idx.
           move   1                to xzoom-ref-m-file (Idx).
           move   2                to xzoom-ref-s-file (Idx).
           move   0                to xzoom-ref-s-key  (Idx).
           move   1                to xzoom-ref-fields (Idx).
           set xzoom-ref-join-outer(Idx) to true.
           
      *            Master File Settings
           move  5  to xzoom-ref-m-length(Idx, 1).
           move  8  to xzoom-ref-m-offset (Idx, 1).
           set xzoom-ref-m-unsigned (Idx, 1) to true.
           set xzoom-ref-m-alpha    (Idx, 1) to true.
           
      *            Slave File Settings
           move 5   to xzoom-ref-s-length(Idx, 1).
           move 0   to xzoom-ref-s-offset (Idx, 1).
           set xzoom-ref-s-unsigned (Idx, 1) to true.
           set xzoom-ref-s-alpha    (Idx, 1) to true.


      *    File Reference Settings - Relazione tagli > articoli
           add    1                to Idx.
           move   1                to xzoom-ref-m-file (Idx).
           move   3                to xzoom-ref-s-file (Idx).
           move   0                to xzoom-ref-s-key  (Idx).
           move   1                to xzoom-ref-fields (Idx).
           set xzoom-ref-join-outer(Idx) to true.
           
      *            Master File Settings
           move  6  to xzoom-ref-m-length(Idx, 1).
           move 13  to xzoom-ref-m-offset (Idx, 1).
           set xzoom-ref-m-unsigned (Idx, 1) to true.
           set xzoom-ref-m-alpha    (Idx, 1) to true.
           
      *            Slave File Settings

           move 6    to xzoom-ref-s-length(Idx, 1).
           move 0    to xzoom-ref-s-offset (Idx, 1).
           set xzoom-ref-s-unsigned (Idx, 1) to true.
           set xzoom-ref-s-alpha    (Idx, 1) to true.
           

      ***---
       PREPARA-TMP-DESTINI.
           initialize xzoom-linkage xzoom-ext-info(1).

           move  0                      to xzoom-row.
           move  0                      to xzoom-cln.
           move  16                     to xzoom-lw.
           move  167                    to xzoom-sw.

           move ext-file                to xzoom-file-name(1).
           move  3                      to xzoom-file-key.
           move  5                      to xzoom-fields.
           move  0                      to idx.

      *    Il file non ha il tipo record
           move  -1                to xzoom-delimiter-offset.

      *    Definizione del campo 1
           add  1                           to Idx.
           move 1                           to xzoom-field-file(Idx).
           move 40                          to xzoom-field-length(idx).
           move 10                          to xzoom-field-offset(idx).
           move 30                          to xzoom-field-column(idx).
           move "Cliente"                   to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)         to true. 

      *    Definizione del campo 2
           add  1                           to Idx.
           move 1                           to xzoom-field-file(Idx).
           move 5                           to xzoom-field-length(idx).
           move 5                           to xzoom-field-offset(idx).
           move "Destino"                   to xzoom-field-name(idx).  
           move 5                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "####0"                     to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.
           move 5                           to xzoom-field-column(Idx).

      *    Definizione del campo 3
           add  1                           to Idx.
           move 1                           to xzoom-field-file(Idx).
           move 40                          to xzoom-field-length(idx).
           move 50                          to xzoom-field-offset(idx).
           move 30                          to xzoom-field-column(idx).
           move "Ragione Sociale"           to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)         to true. 

      *    Definizione del campo 4
           add  1                           to Idx.
           move 1                           to xzoom-field-file(Idx).
           move 40                          to xzoom-field-length(idx).
           move 90                          to xzoom-field-offset(idx).
           move 30                          to xzoom-field-column(idx).
           move "Località"                  to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)         to true. 

      *    Definizione del campo 5
           add  1                           to Idx.
           move 1                           to xzoom-field-file(Idx).
           move 40                          to xzoom-field-length(idx).
           move 130                         to xzoom-field-offset(idx).
           move 30                          to xzoom-field-column(idx).
           move "Indirizzo"                 to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)         to true. 

      ***---
       PREPARA-FAT-PROMO.
           initialize xzoom-linkage xzoom-ext-info(1).

           move  zero to idx.
           move  zero                       to xzoom-row.
           move  zero                       to xzoom-cln.
           move  21                         to xzoom-lw.
           move  166                        to xzoom-sw.
           move  ext-file                   to xzoom-file-name(1).
           move  9                          to xzoom-fields.

      * CAMPO 1
           add  1 to idx
           move  4                          to xzoom-field-length(idx).
           move  0                          to xzoom-field-offset(idx).
           move  7                          to xzoom-field-column(idx).
           move "Anno"                      to xzoom-field-name(idx).  
           move 4                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "###0"                      to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      * CAMPO 2
           add  1 to idx
           move  8                          to xzoom-field-length(idx).
           move  4                          to xzoom-field-offset(idx).
           move  7                          to xzoom-field-column(idx).
           move "Fattura"                   to xzoom-field-name(idx).  
           move 8                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "#######0"                  to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      * CAMPO 3
           add  1 to Idx.
           move 8                           to xzoom-field-length(idx).
           move 12                          to xzoom-field-offset(idx).
           move "Data Fattura"              to xzoom-field-name(idx).
           set xzoom-field-unsigned(Idx)    to true.
           set xzoom-ft-display(Idx)        to true.
           set xzoom-al-center(Idx)         to true.
           move 10                          to xzoom-field-digits(Idx).
           move   0                         to xzoom-field-dec(Idx).
           move "AAAAMMGG-GG/MM/AAAA"       to xzoom-field-fmt(Idx).
           move 10                          to xzoom-field-column(Idx).

      * CAMPO 4
           add  1 to idx.
           move 5                           to xzoom-field-length(idx).
           move 6                           to xzoom-field-column(idx).
           move 20                          to xzoom-field-offset(idx).
           move "GDO"                       to xzoom-field-name(idx).  
           set xzoom-ft-alpha(idx)          to true.

      * CAMPO 5
           add  1 to idx.
           move 30                          to xzoom-field-length(idx).
           move 20                          to xzoom-field-column(idx).
           move 25                          to xzoom-field-offset(idx).
           move "Descrizione"               to xzoom-field-name(idx).  
           set xzoom-ft-alpha(idx)          to true.

      * CAMPO 6
           add  1 to idx.
           move 5                           to xzoom-field-length(idx).
           move 8                           to xzoom-field-column(idx).
           move 55                          to xzoom-field-offset(idx).
           move "Cliente"                   to xzoom-field-name(idx).  
           move 5                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "#####"                     to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      * CAMPO 7
           add  1 to idx.
           move 40                          to xzoom-field-length(idx).
           move 30                          to xzoom-field-column(idx).
           move 60                          to xzoom-field-offset(idx).
           move "Ragione Sociale"           to xzoom-field-name(idx).
           set xzoom-ft-alpha(Idx)          to true.

      * CAMPO 8
           add  1 to idx.
           move 5                           to xzoom-field-length(idx).
           move 8                           to xzoom-field-column(idx).
           move 100                         to xzoom-field-offset(idx).
           move "Destino"                   to xzoom-field-name(idx).  
           move 5                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "#####"                     to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      * CAMPO 9
           add  1 to idx.
           move 40                          to xzoom-field-length(idx).
           move 30                          to xzoom-field-column(idx).
           move 105                         to xzoom-field-offset(idx).
           move "Ragione Sociale"           to xzoom-field-name(idx).
           set xzoom-ft-alpha(Idx)          to true.

           move  -1                         to xzoom-delimiter-offset.
           move  5                          to xzoom-delimiter-length.
           move "000"                       to xzoom-from-value.
           move "000"                       to xzoom-to-value.

      ***---
       PREPARA-TMP-PROMO-PRZ.
           initialize xzoom-linkage xzoom-ext-info(1).

           move  0                      to xzoom-row.
           move  0                      to xzoom-cln.
           move  16                     to xzoom-lw.

           move ext-file                to xzoom-file-name(1).
           move "tgrupgdo"              to xzoom-file-name(2).       
           move  0                      to xzoom-file-key.

           if como-file = "tmp-promo-prz"
              move  115                 to xzoom-sw
              move  5                   to xzoom-fields

           else                                    
              move  100                 to xzoom-sw
              move  4                   to xzoom-fields
           end-if.


      *    Il file non ha il tipo record
           move  -1                to xzoom-delimiter-offset.

      *    Definizione del campo 1
           move 1                           to Idx.
           move 2                           to xzoom-field-file(Idx).
           move 1                           to xzoom-field-rel(Idx).
           move 30                          to xzoom-field-length(Idx).
           move 5                           to xzoom-field-offset(Idx).
           move "Gruppo GDO"                to xzoom-field-name(Idx).  
           move 25                          to xzoom-field-column(Idx).
           set  xzoom-ft-alpha(idx)         to true. 

      *    Definizione del campo 2
           add  1                           to Idx.
           move 1                           to xzoom-field-file(Idx).
           move 0                           to xzoom-field-rel(Idx).
           move 30                          to xzoom-field-length(idx).
           move 20                          to xzoom-field-offset(idx).
           move "Nome Volantino"            to xzoom-field-name(idx).  
           move 30                          to xzoom-field-column(Idx).
           set  xzoom-ft-alpha(idx)         to true.

      *    Definizione del campo 3
           add  1                           to Idx.
           move 8                           to xzoom-field-length(idx).
           move 50                          to xzoom-field-offset(idx).
           move "Ini DPO"                   to xzoom-field-name(idx).
           set xzoom-field-unsigned(Idx)    to true.
           set xzoom-ft-display(Idx)        to true.
           move 8                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "AAAAMMGG-GG/MM/AAAA"       to xzoom-field-fmt(Idx).
           move 10                          to xzoom-field-column(Idx).
           set xzoom-al-center(Idx)         to true.

      *    Definizione del campo 4
           add  1                           to Idx.
           move 8                           to xzoom-field-length(idx).
           move 58                          to xzoom-field-offset(idx).
           move "Fine DPO"                  to xzoom-field-name(idx).
           set xzoom-field-unsigned(Idx)    to true.
           set xzoom-ft-display(Idx)        to true.
           move 8                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "AAAAMMGG-GG/MM/AAAA"       to xzoom-field-fmt(Idx).
           move 10                          to xzoom-field-column(Idx).

           set xzoom-al-center(Idx)         to true.

      *    Definizione del campo 5 (descrizione destino)
           if como-file = "tmp-promo-prz"
              add   1                     to Idx
              move  1                     to xzoom-field-file(Idx)
              move  0                     to xzoom-field-rel(Idx)
              move  8                     to xzoom-field-length(idx)
              move  66                    to xzoom-field-offset(idx)
              move  10                    to xzoom-field-column(idx)
              move "Prezzo"               to xzoom-field-name(idx)
              move 8                      to xzoom-field-digits(Idx)
              move 2                      to xzoom-field-dec(Idx)
              move "###.##0,00"           to xzoom-field-fmt(Idx)
              set xzoom-al-right(Idx)     to true
              set xzoom-field-signed(Idx) to true
              set xzoom-ft-display(Idx)   to true
           end-if.
      
      *    File Reference Settings - Relazione tmp > tgrupgdo
           move   1                to Idx.
           move   1                to xzoom-ref-m-file (Idx).
           move   2                to xzoom-ref-s-file (Idx).
           move   0                to xzoom-ref-s-key  (Idx).
           move   1                to xzoom-ref-fields (Idx).
           set xzoom-ref-join-outer(Idx) to true.
           
      *            Master File Settings
           move  5  to xzoom-ref-m-length(Idx, 1).
           move 15  to xzoom-ref-m-offset (Idx, 1).
           set xzoom-ref-m-unsigned (Idx, 1) to true.
           set xzoom-ref-m-alpha    (Idx, 1) to true.

      *            Slave File Settings
           move 5 to xzoom-ref-s-length(Idx, 1).
           move 0 to xzoom-ref-s-offset (Idx, 1).
           set xzoom-ref-s-unsigned (Idx, 1) to true.
           set xzoom-ref-s-alpha    (Idx, 1) to true.
       
      ***---
       PREPARA-BLISTER.
           initialize xzoom-linkage xzoom-ext-info(1).

           move  0                      to xzoom-row.
           move  0                      to xzoom-cln.
           move  16                     to xzoom-lw.
           move  110                    to xzoom-sw.

           move "blister"               to xzoom-file-name(1).
           move "tmagaz"                to xzoom-file-name(2).       
           move  1                      to xzoom-file-key.
           move  5                      to xzoom-fields.

      *    Il file non ha il tipo record
           move  -1                to xzoom-delimiter-offset.

      *    Definizione del campo 1
           move 1                           to Idx.
           move 1                           to xzoom-field-file(Idx).
           move 0                           to xzoom-field-rel(Idx).
           move 6                           to xzoom-field-length(Idx).
           move 0                           to xzoom-field-offset(Idx).
           move "Codice"                    to xzoom-field-name(Idx).  
           move 8                           to xzoom-field-column(Idx).
           move 6                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "#####0"                    to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      *    Definizione del campo 2
           add  1                           to Idx.
           move 1                           to xzoom-field-file(Idx).
           move 0                           to xzoom-field-rel(Idx).
           move 50                          to xzoom-field-length(idx).
           move  6                          to xzoom-field-offset(idx).
           move "Descrizione"               to xzoom-field-name(idx).  
           move 30                          to xzoom-field-column(Idx).
           set  xzoom-ft-alpha(idx)         to true.

      *    Definizione del campo 3
           add  1                           to Idx.
           move 3                           to xzoom-field-length(idx).
           move 56                          to xzoom-field-offset(idx).
           move 5                           to xzoom-field-column(Idx).
           move "Mag."                      to xzoom-field-name(idx).
           set xzoom-ft-alpha(Idx)          to true.

      *    Definizione del campo 4
           add  1                           to Idx.
           move 2                           to xzoom-field-file(Idx).
           move 1                           to xzoom-field-rel(Idx).
           move 50                          to xzoom-field-length(Idx).
           move 3                           to xzoom-field-offset(Idx).
           move "Descrizione"               to xzoom-field-name(Idx).  
           move 30                          to xzoom-field-column(Idx).
           set xzoom-ft-alpha(Idx)          to true.

      *    Definizione del campo 5
           add   1                          to Idx.
           move  1                          to xzoom-field-file(Idx).
           move  0                          to xzoom-field-rel(Idx).
           move  8                          to xzoom-field-length(idx).
           move  59                         to xzoom-field-offset(idx).
           move  9                          to xzoom-field-column(idx).
           move "Prezzo"                    to xzoom-field-name(idx).  
           move 8                           to xzoom-field-digits(Idx).
           move 2                           to xzoom-field-dec(Idx).
           move "###.##0,00"                to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-signed(Idx )     to true.
           set xzoom-ft-display(Idx)        to true.
      
      *    File Reference Settings - Relazione blister > tmagaz
           move   1                to Idx.
           move   1                to xzoom-ref-m-file (Idx).
           move   2                to xzoom-ref-s-file (Idx).
           move   0                to xzoom-ref-s-key  (Idx).
           move   1                to xzoom-ref-fields (Idx).
           set xzoom-ref-join-outer(Idx) to true.
           
      *            Master File Settings
           move  3  to xzoom-ref-m-length(Idx, 1).
           move 56  to xzoom-ref-m-offset (Idx, 1).
           set xzoom-ref-m-unsigned (Idx, 1) to true.
           set xzoom-ref-m-alpha    (Idx, 1) to true.

      *            Slave File Settings
           move 3 to xzoom-ref-s-length(Idx, 1).
           move 0 to xzoom-ref-s-offset (Idx, 1).
           set xzoom-ref-s-unsigned (Idx, 1) to true.
           set xzoom-ref-s-alpha    (Idx, 1) to true.
       
      ***---
       PREPARA-TGIORMAG.
           initialize xzoom-linkage xzoom-ext-info(1).

           move  0                      to xzoom-row.
           move  0                      to xzoom-cln.
           move  16                     to xzoom-lw.
           move  94                     to xzoom-sw.

           move "tgiormag"              to xzoom-file-name(1).
           move "tmagaz"                to xzoom-file-name(2).       
           move  0                      to xzoom-file-key.
           move  5                      to xzoom-fields.

      *    Il file non ha il tipo record
           move  -1                to xzoom-delimiter-offset.

      *    Definizione del campo 1
           move 1                           to Idx.
           move 1                           to xzoom-field-file(Idx).
           move 0                           to xzoom-field-rel(Idx).
           move 3                           to xzoom-field-length(Idx).
           move 0                           to xzoom-field-offset(Idx).
           move "Magazzino"                 to xzoom-field-name(Idx).  
           move 11                          to xzoom-field-column(Idx).
           set  xzoom-ft-alpha(idx)         to true.

      *    Definizione del campo 2
           add  1                           to Idx.
           move 2                           to xzoom-field-file(Idx).
           move 1                           to xzoom-field-rel(Idx).
           move 50                          to xzoom-field-length(idx).
           move  3                          to xzoom-field-offset(idx).
           move "Descrizione"               to xzoom-field-name(idx).  
           move 35                          to xzoom-field-column(Idx).
           set  xzoom-ft-alpha(idx)         to true.

      *    Definizione del campo 3
           add  1                           to Idx.
           move 4                           to xzoom-field-length(idx).
           move 3                           to xzoom-field-offset(idx).
           move 8                           to xzoom-field-column(Idx).
           move "Anno"                      to xzoom-field-name(idx).
           move 4                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "###0"                      to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      *    Definizione del campo 4
           add  1                           to Idx.
           move 2                           to xzoom-field-length(Idx).
           move 7                           to xzoom-field-offset(Idx).
           move "Mese"                      to xzoom-field-name(Idx).  
           move 8                           to xzoom-field-column(Idx).
           move 4                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "#0"                        to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      *    Definizione del campo 5
           add   1                          to Idx.
           move  4                          to xzoom-field-length(idx).
           move  9                          to xzoom-field-offset(idx).
           move  8                          to xzoom-field-column(idx).
           move "Ultima Pag."               to xzoom-field-name(idx).
           move 4                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "###0"                      to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.
      
      *    File Reference Settings - Relazione tgiormag > tmagaz
           move   1                to Idx.
           move   1                to xzoom-ref-m-file (Idx).
           move   2                to xzoom-ref-s-file (Idx).
           move   0                to xzoom-ref-s-key  (Idx).
           move   1                to xzoom-ref-fields (Idx).
           set xzoom-ref-join-outer(Idx) to true.
           
      *            Master File Settings
           move  3  to xzoom-ref-m-length(Idx, 1).
           move  0  to xzoom-ref-m-offset (Idx, 1).
           set xzoom-ref-m-unsigned (Idx, 1) to true.
           set xzoom-ref-m-alpha    (Idx, 1) to true.

      *            Slave File Settings
           move 3 to xzoom-ref-s-length(Idx, 1).
           move 0 to xzoom-ref-s-offset (Idx, 1).
           set xzoom-ref-s-unsigned (Idx, 1) to true.
           set xzoom-ref-s-alpha    (Idx, 1) to true.


      ***---
       PREPARA-TPIOMBO.
           initialize xzoom-linkage xzoom-ext-info(1).

           move  zero                    to xzoom-file-key.
           move  zero                    to idx.
           move  zero                    to xzoom-row.
           move  zero                    to xzoom-cln.
           move  16                      to xzoom-lw.
           move  65                      to xzoom-sw.
           move "tpiombo"                to xzoom-file-name(1).
           move "tmarche"                to xzoom-file-name(2).
           move  5                       to xzoom-fields.
      
      * CAMPO 1
           add 1 to idx.
           move  1                       to xzoom-field-file(Idx).
           move  0                       to xzoom-field-rel(Idx).      
           move  4                       to xzoom-field-length(idx).
           move  0                       to xzoom-field-offset(idx).
           move  8                       to xzoom-field-column(idx).
           move "Marca"                  to xzoom-field-name(idx).  
           set xzoom-al-right(idx)       to true.
           set xzoom-field-unsigned(idx) to true.
           set xzoom-ft-display(idx)     to true.
           move 4                        to xzoom-field-digits(idx).
           move  0                       to xzoom-field-dec(idx).
           move "###0"                   to xzoom-field-fmt(idx).
      
      * CAMPO 2
           add 1 to idx
           move  2                       to xzoom-field-file(Idx).
           move  1                       to xzoom-field-rel(Idx).      
           move  30                      to xzoom-field-length(idx).
           move  4                       to xzoom-field-offset(idx).
           move  26                      to xzoom-field-column(idx).
           move "Descrizione"            to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

      * CAMPO 3
           add  1 to Idx.
           move 1                        to xzoom-field-file(Idx).
           move 0                        to xzoom-field-rel(Idx).      
           move 8                        to xzoom-field-length(idx).
           move 4                        to xzoom-field-offset(idx).
           move "Data Validità"          to xzoom-field-name(idx).
           set xzoom-field-unsigned(Idx) to true.
           set xzoom-ft-display(Idx)     to true.
           move 10                       to xzoom-field-digits(Idx).
           move   0                      to xzoom-field-dec(Idx).
           move "AAAAMMGG-GG/MM/AAAA"    to xzoom-field-fmt(Idx).
           move 10                       to xzoom-field-column(Idx).
      
      * CAMPO 4
           add   1 to idx
           move  6                       to xzoom-field-length(idx).
           move  12                      to xzoom-field-offset(idx).
           move  8                       to xzoom-field-column(idx).
           move "% Auto"                 to xzoom-field-name(idx).  
           set xzoom-al-right(idx)       to true.
           set xzoom-field-unsigned(idx) to true.
           set xzoom-ft-display(idx)     to true.
           move 7                        to xzoom-field-digits(idx).
           move 3                        to xzoom-field-dec(idx).
           move "##0,000"                to xzoom-field-fmt(idx).
                                       
      * CAMPO 5
           add   1 to idx
           move  6                       to xzoom-field-length(idx).
           move  18                      to xzoom-field-offset(idx).
           move  8                       to xzoom-field-column(idx).
           move "% Moto"                 to xzoom-field-name(idx).  
           set xzoom-al-right(idx)       to true.
           set xzoom-field-unsigned(idx) to true.
           set xzoom-ft-display(idx)     to true.
           move 7                        to xzoom-field-digits(idx).
           move 3                        to xzoom-field-dec(idx).
           move "##0,000"                to xzoom-field-fmt(idx).

           move  -1                         to xzoom-delimiter-offset.
           move  5                          to xzoom-delimiter-length.
           move "000"                       to xzoom-from-value.
           move "000"                       to xzoom-to-value.

      *    File Reference Settings - Relazione tpiombo > tmarche
           move   1                to Idx.
           move   1                to xzoom-ref-m-file (Idx).
           move   2                to xzoom-ref-s-file (Idx).
           move   0                to xzoom-ref-s-key  (Idx).
           move   1                to xzoom-ref-fields (Idx).
           set xzoom-ref-join-outer(Idx) to true.
           
      *            Master File Settings
           move  4  to xzoom-ref-m-length(Idx, 1).
           move  0  to xzoom-ref-m-offset (Idx, 1).
           set xzoom-ref-m-unsigned (Idx, 1) to true.
           set xzoom-ref-m-alpha    (Idx, 1) to true.
      
      *            Slave File Settings
           move 4  to xzoom-ref-s-length(Idx, 1).
           move 0  to xzoom-ref-s-offset (Idx, 1).
           set xzoom-ref-s-unsigned (Idx, 1) to true.
           set xzoom-ref-s-alpha    (Idx, 1) to true.

      ***---
       PREPARA-TMP-FATTURATI.
           initialize xzoom-linkage xzoom-ext-info(1).

           move  0                      to xzoom-row.
           move  0                      to xzoom-cln.
           move  16                     to xzoom-lw.

           move ext-file                to xzoom-file-name(1).       
           move  1                      to xzoom-file-key.

           move  106                    to xzoom-sw.
           move  8                      to xzoom-fields.

      *    Il file non ha il tipo record
           move  -1                to xzoom-delimiter-offset.

      *    Definizione del campo 1
           move 1 to Idx.
           move 1                           to xzoom-field-file(Idx).
           move 0                           to xzoom-field-rel(Idx).      
           move 8                           to xzoom-field-length(idx).
           move 5                           to xzoom-field-offset(idx).
           move "Data Fattura"              to xzoom-field-name(idx).
           set xzoom-field-unsigned(Idx)    to true.
           set xzoom-ft-display(Idx)        to true.
           move 10                          to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "AAAAMMGG-GG/MM/AAAA"       to xzoom-field-fmt(Idx).
           move 12                          to xzoom-field-column(Idx).
      
      *    Definizione del campo 2
           add  1                           to Idx. 
           move 1                           to xzoom-field-file(Idx).
           move 0                           to xzoom-field-rel(Idx).
           move 8                           to xzoom-field-length(idx).
           move 13                          to xzoom-field-offset(idx).
           move "Num. Fattura"              to xzoom-field-name(idx).  
           move 10                          to xzoom-field-column(Idx).
           move 8                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "########"                  to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-signed(Idx)      to true.
           set xzoom-ft-display(Idx)        to true.

      *    Definizione del campo 3
           add   1                          to Idx.
           move  1                          to xzoom-field-file(Idx).
           move  0                          to xzoom-field-rel(Idx).
           move  8                          to xzoom-field-length(idx).
           move  21                         to xzoom-field-offset(idx).
           move  10                         to xzoom-field-column(idx).
           move "Prezzo U."                 to xzoom-field-name(idx).
           move 8                           to xzoom-field-digits(Idx).
           move 2                           to xzoom-field-dec(Idx).
           move "###.##0,00"                to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-signed(Idx)      to true.
           set xzoom-ft-display(Idx)        to true.

      *    Definizione del campo 4
           add   1                          to Idx.
           move  1                          to xzoom-field-file(Idx).
           move  0                          to xzoom-field-rel(Idx).
           move  8                          to xzoom-field-length(idx).
           move  29                         to xzoom-field-offset(idx).
           move  10                         to xzoom-field-column(idx).
           move "I. Consumo"                to xzoom-field-name(idx).
           move 8                           to xzoom-field-digits(Idx).
           move 2                           to xzoom-field-dec(Idx).
           move "###.##0,00"                to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-signed(Idx)      to true.
           set xzoom-ft-display(Idx)        to true.

      *    Definizione del campo 5
           add   1                          to Idx.
           move  1                          to xzoom-field-file(Idx).
           move  0                          to xzoom-field-rel(Idx).
           move  8                          to xzoom-field-length(idx).
           move  37                         to xzoom-field-offset(idx).
           move  10                         to xzoom-field-column(idx).
           move "COU/COBAT"                 to xzoom-field-name(idx).
           move 8                           to xzoom-field-digits(Idx).
           move 2                           to xzoom-field-dec(Idx).
           move "###.##0,00"                to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-signed(Idx)      to true.
           set xzoom-ft-display(Idx)        to true.

      *    Definizione del campo 6
           add   1                          to Idx.
           move  1                          to xzoom-field-file(Idx).
           move  0                          to xzoom-field-rel(Idx).
           move  8                          to xzoom-field-length(idx).
           move  45                         to xzoom-field-offset(idx).
           move  10                         to xzoom-field-column(idx).
           move "Add. Pb"                   to xzoom-field-name(idx).
           move 8                           to xzoom-field-digits(Idx).
           move 2                           to xzoom-field-dec(Idx).
           move "###.##0,00"                to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-signed(Idx)      to true.
           set xzoom-ft-display(Idx)        to true.

      *    Definizione del campo 7
           add   1                          to Idx.
           move  1                          to xzoom-field-file(Idx).
           move  0                          to xzoom-field-rel(Idx).
           move  8                          to xzoom-field-length(idx).
           move  53                         to xzoom-field-offset(idx).
           move  10                         to xzoom-field-column(idx).
           move "I. Merce"                  to xzoom-field-name(idx).
           move 8                           to xzoom-field-digits(Idx).
           move 2                           to xzoom-field-dec(Idx).
           move "###.##0,00"                to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-signed(Idx)      to true.
           set xzoom-ft-display(Idx)        to true.

      *    Definizione del campo 8
           add   1                          to Idx.
           move  1                          to xzoom-field-file(Idx).
           move  0                          to xzoom-field-rel(Idx).
           move  3                          to xzoom-field-length(idx).
           move  61                         to xzoom-field-offset(idx).
           move  6                          to xzoom-field-column(idx).
           move "Cod. IVA"                  to xzoom-field-name(idx).
           move 3                           to xzoom-field-digits(Idx).
           set  xzoom-ft-alpha (Idx)        to true. 

      ***---
       PREPARA-BTNOTACR.
           initialize xzoom-linkage xzoom-ext-info(1).

           move  zero to idx.
           move  zero                       to xzoom-row.
           move  zero                       to xzoom-cln.
           move  16                         to xzoom-lw.
           move  145                        to xzoom-sw.
           move "btnotacr"                  to xzoom-file-name(1).
           move "tcaumag"                   to xzoom-file-name(2).
           move "destini"                   to xzoom-file-name(3).
           move  8                          to xzoom-fields.

      * CAMPO 1
           add   1                          to idx.
           move  4                          to xzoom-field-length(idx).
           move  0                          to xzoom-field-offset(idx).
           move  7                          to xzoom-field-column(idx).
           move "Anno"                      to xzoom-field-name(idx).  
           move 4                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "###0"                      to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      * CAMPO 2
           add   1                          to idx.
           move  8                          to xzoom-field-length(idx).
           move  4                          to xzoom-field-offset(idx).
           move  7                          to xzoom-field-column(idx).
           move "Numero"                    to xzoom-field-name(idx).  
           move 8                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "#######0"                  to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      * CAMPO 3
           add  1                           to Idx.
           move 8                           to xzoom-field-length(idx).
           move 26                          to xzoom-field-offset(idx).
           move "Data"                      to xzoom-field-name(idx).
           set xzoom-field-unsigned(Idx)    to true.
           set xzoom-ft-display(Idx)        to true.
           move 10                          to xzoom-field-digits(Idx).
           move   0                         to xzoom-field-dec(Idx).
           move "AAAAMMGG-GG/MM/AAAA (GGG)" to xzoom-field-fmt(Idx).
           move 13                          to xzoom-field-column(Idx).

      * CAMPO 4
           add  1                           to idx.
           move 5                           to xzoom-field-length(idx).
           move 8                           to xzoom-field-column(idx).
           move 16                          to xzoom-field-offset(idx).
           move "Cliente"                   to xzoom-field-name(idx).  
           move 5                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "####0"                     to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      * CAMPO 5
           add  1                           to idx.
           move 5                           to xzoom-field-length(idx).
           move 8                           to xzoom-field-column(idx).
           move 21                          to xzoom-field-offset(idx).
           move "Destino"                   to xzoom-field-name(idx).  
           move 5                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "#####"                     to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      * CAMPO 6
           add  1                           to idx.
           move 3                           to xzoom-field-file(idx).
           move 2                           to xzoom-field-rel(idx).
           move 35                          to xzoom-field-length(idx).
           move 30                          to xzoom-field-column(idx).
           move 135                         to xzoom-field-offset(idx).
           move "Destinazione"              to xzoom-field-name(idx).  
           set xzoom-ft-alpha(idx)          to true.

      * CAMPO 7
           add  1                           to idx.
           move 4                           to xzoom-field-length(idx).
           move 6                           to xzoom-field-column(idx).
           move 12                          to xzoom-field-offset(idx).
           move "Causale"                   to xzoom-field-name(idx).
           set xzoom-ft-alpha(idx)          to true.

      * CAMPO 8
           add  1                           to idx.
           move 2                           to xzoom-field-file(idx).
           move 1                           to xzoom-field-rel(idx).
           move 40                          to xzoom-field-length(idx).
           move 30                          to xzoom-field-column(idx).
           move 4                           to xzoom-field-offset(idx).
           move "Descrizione"               to xzoom-field-name(idx).  
           set xzoom-ft-alpha(idx)          to true.

           move  -1                         to xzoom-delimiter-offset.
           move  5                          to xzoom-delimiter-length.
           move "000"                       to xzoom-from-value.
           move "000"                       to xzoom-to-value.

      *    File Reference Settings - Relazione tordini > tcaumag
           move   1                         to Idx.
           move   1                         to xzoom-ref-m-file (Idx).
           move   2                         to xzoom-ref-s-file (Idx).
           move   0                         to xzoom-ref-s-key  (Idx).
           move   1                         to xzoom-ref-fields (Idx).
           set xzoom-ref-join-outer(Idx)    to true.

      *            Master File Settings
           move  4  to xzoom-ref-m-length(Idx, 1).
           move 12  to xzoom-ref-m-offset (Idx, 1).
           set xzoom-ref-m-unsigned (Idx, 1) to true.
           set xzoom-ref-m-alpha    (Idx, 1) to true.

      *            Slave File Settings
           move 4   to xzoom-ref-s-length(Idx, 1).
           move 0   to xzoom-ref-s-offset(Idx, 1).
           set xzoom-ref-s-unsigned (Idx, 1) to true.
           set xzoom-ref-s-alpha    (Idx, 1) to true.

      *    File Reference Settings - Relazione tordini > destini
           add    1                to Idx.
           move   1                to xzoom-ref-m-file (Idx).
           move   3                to xzoom-ref-s-file (Idx).
           move   0                to xzoom-ref-s-key  (Idx).
           move   1                to xzoom-ref-fields (Idx).
           set xzoom-ref-join-outer(Idx) to true.

      *            Master File Settings
           move 10  to xzoom-ref-m-length(Idx, 1).
           move 16  to xzoom-ref-m-offset (Idx, 1).
           set xzoom-ref-m-unsigned (Idx, 1) to true.
           set xzoom-ref-m-alpha    (Idx, 1) to true.

      *            Slave File Settings
           move 10   to xzoom-ref-s-length(Idx, 1).
           move 0    to xzoom-ref-s-offset (Idx, 1).
           set xzoom-ref-s-unsigned (Idx, 1) to true.
           set xzoom-ref-s-alpha    (Idx, 1) to true.

      ***---
       PREPARA-TESCONS.
           initialize xzoom-linkage xzoom-ext-info(1).

           move  zero to idx.
           move  zero                    to xzoom-row.
           move  zero                    to xzoom-cln.
           move  16                      to xzoom-lw.
           move  90                      to xzoom-sw.
           move "tescons"                to xzoom-file-name(1).
           move  2                       to xzoom-fields.

      * CAMPO 1
           add 1 to idx
           move  10                      to xzoom-field-length(idx).
           move  0                       to xzoom-field-offset(idx).
           move  12                      to xzoom-field-column(idx).
           move "Codice"                 to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

      * CAMPO 2
           add 1 to idx
           move  50                      to xzoom-field-length(idx).
           move  10                      to xzoom-field-offset(idx).
           move  51                      to xzoom-field-column(idx).
           move "Descrizione"            to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

           move  -1                     to xzoom-delimiter-offset.
           move  5                      to xzoom-delimiter-length.
           move "000"                   to xzoom-from-value.
           move "000"                   to xzoom-to-value.

      ***---
       PREPARA-TSCORTE.
           initialize xzoom-linkage xzoom-ext-info(1).
      
           evaluate como-file
           when "tscorte"       
                move  0 to xzoom-file-key
           when "tscorte-alfa"  
                move  1 to xzoom-file-key
           end-evaluate.
      *
           move zero to idx.
           move  zero                    to xzoom-row.
           move  zero                    to xzoom-cln.
           move  16                      to xzoom-lw.
           move  15                      to xzoom-sw.
           move "tscorte"                to xzoom-file-name(1).
      
      * CAMPO 1
           add 1 to idx
           move  2                       to xzoom-field-length(idx).
           move  0                       to xzoom-field-offset(idx).
           move  9                       to xzoom-field-column(idx).
           move "Codice"                 to xzoom-field-name(idx).  
           set xzoom-al-right(idx)       to true.
           set xzoom-field-unsigned(idx) to true.
           set xzoom-ft-display(idx)     to true.
           move 2                        to xzoom-field-digits(idx).
           move  0                       to xzoom-field-dec(idx).
           move "#0"                   to xzoom-field-fmt(idx).
      
      * CAMPO 2
           add 1 to idx
           move  30                      to xzoom-field-length(idx).
           move  2                       to xzoom-field-offset(idx).
           move  50                      to xzoom-field-column(idx).
           move "Descrizione"            to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 
      
           move  idx                    to xzoom-fields.
      
           move  -1                     to xzoom-delimiter-offset.
           move  5                      to xzoom-delimiter-length.
           move "000"                   to xzoom-from-value.
           move "000"                   to xzoom-to-value.

      ***---
       PREPARA-TLISTINI.
           initialize xzoom-linkage xzoom-ext-info(1).
      
           move  0 to xzoom-file-key
      *
           move zero to idx.
           move  zero                    to xzoom-row.
           move  zero                    to xzoom-cln.
           move  16                      to xzoom-lw.
           move  84                      to xzoom-sw.
           move "tlistini"               to xzoom-file-name(1).
      
      * CAMPO 1
           add 1 to idx
           move  15                      to xzoom-field-length(idx).
           move  0                       to xzoom-field-offset(idx).
           move  9                       to xzoom-field-column(idx).
           move "Codice"                 to xzoom-field-name(idx).  
           set xzoom-al-right(idx)       to true.
           set xzoom-field-unsigned(idx) to true.
           set xzoom-ft-display(idx)     to true.
           move 15                       to xzoom-field-digits(idx).
           move  0                       to xzoom-field-dec(idx).
           move "##############0"        to xzoom-field-fmt(idx).
      
      * CAMPO 2
           add 1 to idx
           move  50                      to xzoom-field-length(idx).
           move  36                      to xzoom-field-offset(idx).
           move  28                      to xzoom-field-column(idx).
           move "Nome"                   to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

      * CAMPO 3
           add 1 to idx
           move  5                       to xzoom-field-length(idx).
           move  15                      to xzoom-field-offset(idx).
           move  6                       to xzoom-field-column(idx).
           move "Fornitore"              to xzoom-field-name(idx).  
           set xzoom-al-right(idx)       to true.
           set xzoom-field-unsigned(idx) to true.
           set xzoom-ft-display(idx)     to true.
           move  5                       to xzoom-field-digits(idx).
           move  0                       to xzoom-field-dec(idx).
           move "####0"                  to xzoom-field-fmt(idx).

      * CAMPO 4
           add  1 to Idx.
           move 8                        to xzoom-field-length(idx).
           move 20                       to xzoom-field-offset(idx).
           move "Dal"                    to xzoom-field-name(idx).
           set xzoom-field-unsigned(Idx) to true.
           set xzoom-ft-display(Idx)     to true.
           move 10                       to xzoom-field-digits(Idx).
           move   0                      to xzoom-field-dec(Idx).
           move "AAAAMMGG-GG/MM/AAAA"    to xzoom-field-fmt(Idx).
           move 9                       to xzoom-field-column(Idx).

      * CAMPO 5
           add  1 to Idx.
           move 8                        to xzoom-field-length(idx).
           move 28                       to xzoom-field-offset(idx).
           move "Al"                     to xzoom-field-name(idx).
           set xzoom-field-unsigned(Idx) to true.
           set xzoom-ft-display(Idx)     to true.
           move 10                       to xzoom-field-digits(Idx).
           move   0                      to xzoom-field-dec(Idx).
           move "AAAAMMGG-GG/MM/AAAA"    to xzoom-field-fmt(Idx).
           move 9                       to xzoom-field-column(Idx).

           move  idx                    to xzoom-fields.
      
           move  -1                     to xzoom-delimiter-offset.
           move  5                      to xzoom-delimiter-length.
           move "000"                   to xzoom-from-value.
           move "000"                   to xzoom-to-value.

      ***---
       PREPARA-TTIPOAVV.
           initialize xzoom-linkage xzoom-ext-info(1).

           move zero to idx.
           move  zero                    to xzoom-row.
           move  zero                    to xzoom-cln.
           move  16                      to xzoom-lw.
           move  80                      to xzoom-sw.
           move "ttipoavv"               to xzoom-file-name(1).
           move  2                       to xzoom-fields.

      * CAMPO 1
           add 1 to idx
           move  2                       to xzoom-field-length(idx).
           move  0                       to xzoom-field-offset(idx).
           move  15                      to xzoom-field-column(idx).
           move "Codice"                 to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

      * CAMPO 2
           add 1 to idx
           move  100                     to xzoom-field-length(idx).
           move  2                       to xzoom-field-offset(idx).
           move  43                      to xzoom-field-column(idx).
           move "Messaggio"              to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

           move  -1                     to xzoom-delimiter-offset.
           move  5                      to xzoom-delimiter-length.
           move "000"                   to xzoom-from-value.
           move "000"                   to xzoom-to-value.


      ***---
       PREPARA-TORDFORN.
           initialize xzoom-linkage xzoom-ext-info(1).

           move  zero to idx.
           move  zero                       to xzoom-row.
           move  zero                       to xzoom-cln.
           move  16                         to xzoom-lw.
           move  100                        to xzoom-sw.
           move "tordforn"                  to xzoom-file-name(1).
           move "tcaumag"                   to xzoom-file-name(2).

      * CAMPO 1
           add  1 to idx
           move  1                          to xzoom-field-file(idx).
           move  4                          to xzoom-field-length(idx).
           move  0                          to xzoom-field-offset(idx).
           move  7                          to xzoom-field-column(idx).
           move "Anno"                      to xzoom-field-name(idx).  
           move 4                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "###0"                      to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      * CAMPO 2
           add  1 to idx
           move  1                          to xzoom-field-file(idx).
           move  8                          to xzoom-field-length(idx).
           move  4                          to xzoom-field-offset(idx).
           move  7                          to xzoom-field-column(idx).
           move "Numero"                    to xzoom-field-name(idx).  
           move 8                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "#######0"                  to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      * CAMPO 3
           add  1 to Idx.
           move  1                          to xzoom-field-file(idx).
           move 8                           to xzoom-field-length(idx).
           move 16                          to xzoom-field-offset(idx).
           move "Data Ordine"               to xzoom-field-name(idx).
           set xzoom-field-unsigned(Idx)    to true.
           set xzoom-ft-display(Idx)        to true.
           move 10                          to xzoom-field-digits(Idx).
           move   0                         to xzoom-field-dec(Idx).
           move "AAAAMMGG-GG/MM/AAAA"       to xzoom-field-fmt(Idx).
           move 13                          to xzoom-field-column(Idx).

      * CAMPO 4
           add  1 to idx.
           move  1                          to xzoom-field-file(idx).
           move 5                           to xzoom-field-length(idx).
           move 8                           to xzoom-field-column(idx).
           move 32                          to xzoom-field-offset(idx).
           move "Fornitore"                 to xzoom-field-name(idx).  
           move 5                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "####0"                     to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      * CAMPO 7
           add  1  to idx.
           move  1                          to xzoom-field-file(idx).
           move 4                           to xzoom-field-length(idx).
           move 6                           to xzoom-field-column(idx).
           move 12                          to xzoom-field-offset(idx).
           move "Causale"                   to xzoom-field-name(idx).  
           set xzoom-ft-alpha(idx)          to true.

      * CAMPO 8
           add  1  to idx.
           move 2                           to xzoom-field-file(idx).
           move 1                           to xzoom-field-rel(idx).
           move 40                          to xzoom-field-length(idx).
           move 30                          to xzoom-field-column(idx).
           move 4                           to xzoom-field-offset(idx).
           move "Descrizione"               to xzoom-field-name(idx).  
           set xzoom-ft-alpha(idx)          to true.

           move  idx                        to xzoom-fields.
           move  -1                         to xzoom-delimiter-offset.
           move  5                          to xzoom-delimiter-length.
           move "000"                       to xzoom-from-value.
           move "000"                       to xzoom-to-value.

      *    File Reference Settings - Relazione tordini > tcaumag
           move   1                to Idx.
           move   1                to xzoom-ref-m-file (Idx).
           move   2                to xzoom-ref-s-file (Idx).
           move   0                to xzoom-ref-s-key  (Idx).
           move   1                to xzoom-ref-fields (Idx).
           set xzoom-ref-join-outer(Idx) to true.
           
      *            Master File Settings
           move  4  to xzoom-ref-m-length(Idx, 1).
           move 12  to xzoom-ref-m-offset (Idx, 1).
           set xzoom-ref-m-unsigned (Idx, 1) to true.
           set xzoom-ref-m-alpha    (Idx, 1) to true.
           
      *            Slave File Settings
           move 4   to xzoom-ref-s-length(Idx, 1).
           move 0   to xzoom-ref-s-offset (Idx, 1).
           set xzoom-ref-s-unsigned (Idx, 1) to true.
           set xzoom-ref-s-alpha    (Idx, 1) to true.


           if como-file = "tordforn-stato"
           |FILTRO SU RECORD ATTIVI
              move 1 to idx
              set xzoom-when-true(idx)    to true
              set xzoom-begin-with(idx)   to true
              set xzoom-ignore-case(idx)  to true
                                 
              move como-record(207:1)   to xzoom-wild-value(idx)
              move 1                    to xzoom-wild-value-length(idx)
              move 1                    to xzoom-wild-length(idx)
              move 206                  to xzoom-wild-offset(idx)
           end-if.

      ***---
       PREPARA-TEVA.
           initialize xzoom-linkage xzoom-ext-info(1).

           move  zero to idx.
           move  zero                       to xzoom-row.
           move  zero                       to xzoom-cln.
           move  16                         to xzoom-lw.
           move  135                        to xzoom-sw.
           move "teva"                      to xzoom-file-name(1).
           move "clienti"                   to xzoom-file-name(2).
           move "tmagaz"                    to xzoom-file-name(3).
           move  8                          to xzoom-fields.

      * CAMPO 1
           add  1 to idx
           move  4                          to xzoom-field-length(idx).
           move  0                          to xzoom-field-offset(idx).
           move  7                          to xzoom-field-column(idx).
           move "Anno"                      to xzoom-field-name(idx).  
           move 4                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "###0"                      to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      * CAMPO 2
           add  1 to idx
           move  8                          to xzoom-field-length(idx).
           move  4                          to xzoom-field-offset(idx).
           move  8                          to xzoom-field-column(idx).
           move "Numero"                    to xzoom-field-name(idx).  
           move 8                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "#######0"                  to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      * CAMPO 3
           add  1 to Idx.
           move 8                           to xzoom-field-length(idx).
           move 12                          to xzoom-field-offset(idx).
           move "Data"                      to xzoom-field-name(idx).
           set xzoom-field-unsigned(Idx)    to true.
           set xzoom-ft-display(Idx)        to true.
           move 10                          to xzoom-field-digits(Idx).
           move   0                         to xzoom-field-dec(Idx).
           move "AAAAMMGG-GG/MM/AAAA"       to xzoom-field-fmt(Idx).
           move 9                           to xzoom-field-column(Idx).

      * CAMPO 4
           add  1 to idx.
           move 5                           to xzoom-field-length(idx).
           move 6                           to xzoom-field-column(idx).
           move 21                          to xzoom-field-offset(idx).
           move "Cliente"                   to xzoom-field-name(idx).
           move 5                           to xzoom-field-digits(Idx).
           move 0                           to xzoom-field-dec(Idx).
           move "####0"                     to xzoom-field-fmt(Idx).
           set xzoom-al-right(Idx)          to true.
           set xzoom-field-unsigned(Idx )   to true.
           set xzoom-ft-display(Idx)        to true.

      * CAMPO 5
           add   1 to idx.
           move  2                       to xzoom-field-file(Idx).
           move  1                       to xzoom-field-rel(Idx).
           move  40                      to xzoom-field-length(idx).
           move   6                      to xzoom-field-offset(idx).
           move  30                      to xzoom-field-column(idx).
           move "Ragione Sociale"        to xzoom-field-name(idx).
           set xzoom-ft-alpha(idx)       to true.

      * CAMPO 6
           add  1  to idx.
           move 3                           to xzoom-field-length(idx).
           move 5                           to xzoom-field-column(idx).
           move 26                          to xzoom-field-offset(idx).
           move "Magaz."                    to xzoom-field-name(idx).  
           set xzoom-ft-alpha(idx)          to true.

      * CAMPO 7
           add  1  to idx.
           move 2                           to xzoom-field-file(idx).
           move 2                           to xzoom-field-rel(idx).
           move 40                          to xzoom-field-length(idx).
           move 30                          to xzoom-field-column(idx).
           move 4                           to xzoom-field-offset(idx).
           move "Descrizione"               to xzoom-field-name(idx).  
           set xzoom-ft-alpha(idx)          to true.

      * CAMPO 8
           add  1  to idx.
           move 1                           to xzoom-field-file(idx).
           move 0                           to xzoom-field-rel(idx).
           move 1                           to xzoom-field-length(idx).
           move 6                           to xzoom-field-column(idx).
           move 41                          to xzoom-field-offset(idx).
           move "Stato"                     to xzoom-field-name(idx).  
           set xzoom-ft-alpha(idx)          to true.

           move  -1                         to xzoom-delimiter-offset.
           move  5                          to xzoom-delimiter-length.
           move "000"                       to xzoom-from-value.
           move "000"                       to xzoom-to-value.

      *    File Reference Settings - Relazione teva > clienti
           move   1                to Idx.
           move   1                to xzoom-ref-m-file (Idx).
           move   2                to xzoom-ref-s-file (Idx).
           move   0                to xzoom-ref-s-key  (Idx).
           move   2                to xzoom-ref-fields (Idx).
           set xzoom-ref-join-outer(Idx) to true.

      *            Master File Settings
           move  6  to xzoom-ref-m-length(Idx, 1).
           move 20  to xzoom-ref-m-offset (Idx, 1).
           set xzoom-ref-m-unsigned (Idx, 1) to true.
           set xzoom-ref-m-alpha    (Idx, 1) to true.

      *            Slave File Settings
           move 6   to xzoom-ref-s-length(Idx, 1).
           move 0   to xzoom-ref-s-offset (Idx, 1).
           set xzoom-ref-s-unsigned (Idx, 1) to true.
           set xzoom-ref-s-alpha    (Idx, 1) to true.


      *    File Reference Settings - Relazione teva > tmagaz
           add    1                to Idx.
           move   1                to xzoom-ref-m-file (Idx).
           move   3                to xzoom-ref-s-file (Idx).
           move   0                to xzoom-ref-s-key  (Idx).
           move   1                to xzoom-ref-fields (Idx).
           set xzoom-ref-join-outer(Idx) to true.
           
      *            Master File Settings
           move  3  to xzoom-ref-m-length(Idx, 1).
           move 26  to xzoom-ref-m-offset (Idx, 1).
           set xzoom-ref-m-unsigned (Idx, 1) to true.
           set xzoom-ref-m-alpha    (Idx, 1) to true.
           
      *            Slave File Settings
           move 3    to xzoom-ref-s-length(Idx, 1).
           move 0    to xzoom-ref-s-offset (Idx, 1).
           set xzoom-ref-s-unsigned (Idx, 1) to true.
           set xzoom-ref-s-alpha    (Idx, 1) to true. 

      ***---
       PREPARA-TEVA-APERTE.
           perform PREPARA-TEVA.
           move 1 to idx.
           set xzoom-when-true(idx)    to true.
           set xzoom-begin-with(idx)   to true.
           set xzoom-ignore-case(idx)  to true.
                                 
           move "A"                  to xzoom-wild-value(idx).
           move 1                    to xzoom-wild-value-length(idx).
           move 1                    to xzoom-wild-length(idx).
           move 41                   to xzoom-wild-offset(idx).
