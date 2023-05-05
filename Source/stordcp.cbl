       PROGRAM-ID.     stordcp.
       AUTHOR.         ANDREA.

       SPECIAL-NAMES. DECIMAL-POINT is COMMA.

       FILE-CONTROL.
           copy "tordini.sl".
           copy "rordini.sl".
           copy "clienti.sl".
           copy "articoli.sl".
           copy "tudm.sl".
           copy "destini.sl".
           copy "tvettori.sl".          
           copy "ttipocli.sl".
           copy "blister.sl".
           copy "tcaumag.sl".
           copy "tmp-riep-vet.sl".
           copy "tmp-riep-bli-mas.sl".
           copy "tmp-riep-bli-man.sl".
           copy "tparamge.sl".
           copy "tcodpag.sl".
           copy "tpromo.sl".
           copy "tivaese.sl".
           copy "mtordini.sl".
           copy "mrordini.sl".
           copy "tmp-ord-mast.sl".
           copy "agenti.sl".

      *file di sort per stampare le evasioni in stordcp
       SELECT tmp-ord-ev
           ASSIGN       TO DISC path-tmp-ord-ev
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-ord-ev
           RECORD KEY   IS tmp-oe-chiave.

       SELECT tmp-ord-ev2
           ASSIGN       TO DISC path-tmp-ord-ev2
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-ord-ev2
           RECORD KEY   IS tmp-oe2-chiave.

       FILE SECTION.
           copy "tordini.fd".
           copy "rordini.fd".
           copy "clienti.fd".
           copy "articoli.fd".
           copy "tudm.fd".
           copy "destini.fd".
           copy "tvettori.fd".
           copy "ttipocli.fd".
           copy "blister.fd".
           copy "tcaumag.fd".
           copy "tmp-riep-vet.fd".
           copy "tmp-riep-bli-mas.fd".
           copy "tmp-riep-bli-man.fd".
           copy "tparamge.fd".
           copy "tcodpag.fd".  
           copy "tpromo.fd".
           copy "tivaese.fd".
           copy "mtordini.fd".
           copy "mrordini.fd".    
           copy "tmp-ord-mast.fd".  
           copy "agenti.fd".

      *(( XFD FILE = tmp-ord-mast ))
       FD  tmp-ord-ev.
       01 tmp-oe-rec.
           05 tmp-oe-chiave.
              10 tmp-oe-chiave-e.
                 15 tmp-oe-anno-e            PIC  9(4).
                 15 tmp-oe-numero-e          PIC  9(8).
              10 tmp-oe-chiave-m.
                 15 tmp-oe-anno-m            PIC  9(4).
                 15 tmp-oe-numero-m          PIC  9(8).

      *(( XFD FILE = tmp-ord-mast ))
       FD  tmp-ord-ev2.
       01 tmp-oe2-rec.
           05 tmp-oe2-chiave.                           
              10 tmp-oe2-stato                pic 9.
                 88 tmp-oe2-urgente           value 1.
                 88 tmp-oe2-evaso             value 2.
                 88 tmp-oe2-non-evaso         value 3.
              10 tmp-oe2-chiave-e.
                 15 tmp-oe2-anno-e            PIC  9(4).
                 15 tmp-oe2-numero-e          PIC  9(8).

       WORKING-STORAGE SECTION.
      * COPY
           copy "fonts.def".
           copy "acugui.def".
           copy "comune.def".
           copy "spooler.def".
           copy "selprint.lks".

LUBEXX 78  MaxRowsPerPage                value    33.
       01  filler  pic 9 value 0. 
         88 fai-tutto    value 1, false 0.
       01  filler  pic 9 value 0. 
         88 OrdinamentoEvasioni    value 1, false 0.

      * HANDLES        
       77  CourierNew8                   handle of font.    
       77  CourierNew10                  handle of font.
       77  CourierNew10B                 handle of font.
       77  CourierNew16B                 handle of font.
       77  CourierNew18B                 handle of font.
       77  CourierNew12B                 handle of font.
       77  Font-Copia-TimesNewRoman30    handle of font.
                                             
       77  idx-art            pic 999  value 0.
       77  tot-art            pic 99   value 0.
       77  el-art             pic 9(6) occurs 10.
       77  el-qta             pic 9(6) occurs 10.
       77  IdxChar            pic 999  value 0.
       77  NumChar            pic 999  value 0.
       77  como-art           pic x(6).
       77  age-codice-x       pic x(5).
 
      * COMODI
       77  como-cap           pic x(5).
       77  como-loca          pic x(40).
       77  como-prov          pic x(2).

       77  messaggio          pic X(150) value spaces.
       77  WFONT-STATUS       pic S9(5)  value ZERO.
       77  font-size-dply     pic Z(5)   value ZERO.
       
       77  como-data          pic 9(8)   value ZERO.
       77  como-ora           pic 9(8)   value ZERO.     
       77  edit-numero-8      pic Z(8).
       77  tot-colli          pic 9(5)   value ZERO.
       77  edit-tot-colli     pic Z(5).
       77  imballi-ed         pic  zz.zzz.zz9.
       77  edit-tot-peso      pic ZZZ.ZZ9,999.
       77  tot-peso           pic 9(8)v9(3) value ZERO.
       77  tot-peso-utf       pic 9(8)v9(3) value ZERO.
       77  tot-peso-no-utf    pic 9(8)v9(3) value ZERO.
       77  peso-totale        pic 9(8)v9(3) value ZERO.
       77  pos-piede          pic 9(3)v99   value ZERO.
       77  max-righe-corpo    pic 9(3)v99   value ZERO.
       77  num-pagina         pic 9(3)      value ZERO.
       77  tor-data-note-i    pic X(10)     value all "/".
       77  tot-pagine         pic 9(3).
       77  resto              pic 9(3).
       01  save-chiave.
         05 save-anno         pic 9(4).
         05 save-numero       pic 9(8).   

       01  tab-iva           occurs 3 indexed by idx-iva.
         05 el-iva           pic x(3).
         05 el-aliq          pic 9(5)v99.
         05 el-imponib       pic 9(10)v99.
       77  totale                 pic 9(6)v999.

       77  como-iva              pic 9(9)v999.
       77  como-iva-2dec         pic 9(9)v99.

       77  save-spl-riga      pic 99v99.

       77  idx-tot-master        pic 9(5).
       01  el-master             occurs 50 indexed by idx-master.
         05 el-chiave.
           10 el-anno-m          pic 9(4).
           10 el-numero-m        pic 9(8).
         05 el-saldi-promo       pic x.
         05 el-saldi-banco       pic x.

       77  numero-x              pic x(8).
                   
       77  idx-note              pic 99.
       77  tot-note              pic 99.
       
       01  tab-note                occurs 4.
           05 el-nota              pic x(35).
           05 el-promo             pic 9(15).

      * STATUS DEI FILES
       77  status-tordini          pic xx.
       77  status-rordini          pic xx.
       77  status-clienti          pic xx.
       77  status-articoli         pic xx.
       77  status-tudm             pic xx.
       77  status-destini          pic xx.
       77  status-tvettori         pic xx.
       77  status-ttipocli         pic xx.
       77  status-blister          pic xx.
       77  status-tcaumag          pic xx.
       77  status-tmp-riep-vet     pic xx.
       77  status-tmp-riep-bli-mas pic xx.
       77  status-tmp-riep-bli-man pic xx.
       77  status-tcodpag          pic xx.
       77  status-tparamge         pic xx.
       77  status-tpromo           pic xx.
       77  status-tivaese          pic xx.
       77  status-mtordini         pic xx.
       77  status-mrordini         pic xx.    
       77  status-tmp-ord-mast     pic xx.
       77  status-tmp-ord-ev       pic xx.
       77  status-tmp-ord-ev2      pic xx.
       77  status-agenti           pic xx.

       77  path-tmp-riep-vet       pic x(256).
       77  path-tmp-ord-mast       pic x(256).
       77  path-tmp-riep-bli-mas   pic x(256).
       77  path-tmp-riep-bli-man   pic x(256).     
       77  path-tmp-ord-ev         pic x(256).
       77  path-tmp-ord-ev2        pic x(256).

       77  r-tot-iva  pic zzz.zz9,99.
       77  r-tot      pic zzz.zz9,99.
           
      * FLAGS
       77  filler             pic X.
           88 sollecito                 value "S", false "N".   

       77  filler             pic 9.  
           88 NotaNonPresente value 0.
           88 NotaPresente    value 1.

       77  filler             pic X.
           88 no-dati                   value "S", false "N".   
          
       77  filler             pic X.
           88 primo-passaggio           value "S", false "N".            
          
       77  filler             pic X.
           88 prima-volta               value "S", false "N".   

       77  tipo-foglio                  pic X.
           88 copia                     value "C".
           88 originale                 value "O".
           88 ufficio                   value "U".
      *****     88 bloccato                  value "B".
           
       77  filler             pic X.
           88 rottura                   value "S", false "N".

       77  filler             pic X.
           88 trovato-master-urgente    value "S", false "N".
           
       01  riga-agente.                      
           05 filler             pic x(70) value spaces.
           05 filler             pic x(8)  value "Agente: ".
           05 r-agente           pic x(36).

       01  riga-data-ora.
           05 data-6             pic X(8).
           05 filler             pic x(3)  value " - ".
           05 ora-4              pic X(5).  

       01  riga-master.
           05 filler          pic x  value "*".
           05 stato-master-evasi   pic x value "N".
              88 master-evasi      value "S".
              88 master-non-evasi  value "N".
           05 filler          pic xx value "* ".
           05 filler          pic x(13) value "Rif. Master: ".
           05 r-master        pic x(80).

      * RIGA CODICE CLIENTE ED ORDINE DEL... (PARTE DI INTESTAZIONE)
       01  riga-del.
           05 r-cod-cli       pic Z(5).
           05 filler          pic X(5)  value spaces.
           05 filler          pic X(2)  value "N.".
           05 r-num-ord-cli   pic X(12).
           05 filler          pic X(2)  value spaces.
           05 filler          pic X(4)  value "DEL".
           05 r-data.                   
              10 r-gg         pic 99.
              10 filler       pic X     value "/".
              10 r-mm         pic 99.
              10 filler       pic X     value "/".
              10 r-aa         pic 99.

      * RIGA DI STAMPA PER IL CORPO
       01  riga-corpo.
           05 r-cod-articolo  pic x(6).
           05 filler          pic X(1)  value spaces.
           05 r-num-colli     pic Z(5).
           05 filler          pic X(1)  value spaces.
           05 r-des-imballo   pic X(25).
           05 filler          pic X     value spaces.
           05 r-bloccato      pic x     value spaces.
           05 r-des-articolo  pic X(33).
           05 r-ast           pic x.
           05 filler          pic X(1)  value spaces.
           05 r-art-codfrn    pic x(15).
           05 filler          pic X(1)  value spaces.
      *     05 r-peso-utf      pic Z(3),Z(3).
      *     05 filler          pic X(2)  value spaces.
           05 r-peso          pic zz.zz9,999.
           05 filler          pic X(1)  value spaces.
           05 r-udm           pic X(2).

       01  riga-qta.
           05 r-qta           pic Z(5).                                 |6

      * RIGA DI STAMPA PER IL VETTORE
       01  riga-vettore.
           05 r-cod-vet       pic z(5).
           05 filler          pic X(2)  value spaces.
           05 r-descr-vet     pic x(40).
           05 filler          pic X(2)  value spaces.
           05 r-peso-vet      pic zz.zzz.zz9,999.

       01  riga-vettore-int.
           05 filler          pic x(47) value "Corriere".
           05 filler          pic X(2)  value spaces.
           05 filler          pic x(14) value "       TOT Kg.".

       01  riga-bli-mas.
           05 filler          pic X(2)   value space.
           05 r-cod-bli-mas   pic z(6).
           05 filler          pic X(2)  value space.
           05 r-descr-bli-mas pic x(45).
           05 filler          pic X(2)  value space.
           05 r-qta-bli-man   pic zz.zzz.zz9.

       01  riga-bli-mas-int.
           05                 pic x(8) value "BLISTER ".
           05 filler          pic X(64)  value space.
           05 filler          pic X(8)  value "Quantità".


       01  riga-bli-mas-el.
           05                      pic x(10) value space.
           05 r-cod-bli-mas-el     pic z(6).
           05 filler               pic X(2) value space.
           05 r-descr-bli-mas-el   pic x(50).
           05 filler               pic X(5) value space.
           05 r-qta-bli-man-el     pic zzz.zz9.

      * RIGA TRATTINI
       01  riga-fill          pic X(105) value all "-".

      * RIGA NUMERO DI PAGINA
       01  riga-num-pagina.
           05 filler          pic X(5) value "Pag. ".                   |7
           05 edit-num-pagina pic ZZ9.                                  |10
           05 filler          pic x value "/".
           05 edit-num-dicui  pic x(3).

       77  cont pic 9(3).
       77  CallingPgm        pic x(20).

       77  da-ed             pic z(8).
       77  a-ed              pic z(8).   

       LINKAGE SECTION.
           copy "link-stordcp.def".
           
       PROCEDURE DIVISION USING stordcp-limiti.

       DECLARATIVES.

      ***---
       TORDINI-ERR SECTION.
           use after error procedure on tordini.
           set tutto-ok  to true.
           evaluate status-tordini
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File della testata [TORDINI] inesistente"
                          title tit-err
                           icon 2
                set errori to true
           when "39"
                display message "File [TORDINI] Mismatch size!"
                          title tit-err
                           icon 3
                set errori to true
           when "98"
                display message "[TORDINI] Indexed file corrupt!"
                          title tit-err
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
                  x"0d0a""File delle righe [RORDINI] inesistente"
                          title tit-err
                           icon 2
                set errori to true
           when "39"
                display message "File [RORDINI] Mismatch size!"
                          title tit-err
                           icon 3
                set errori to true
           when "98"
                display message "[RORDINI] Indexed file corrupt!"
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
                display message "File [CLIENTI] Mismatch size!"
                          title tit-err
                           icon 3
                set errori to true
           when "98"
                display message "[CLIENTI] Indexed file corrupt!"
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
                display message "File [ARTICOLI] Mismatch size!"
                          title tit-err
                           icon 3
                set errori to true
           when "98"
                display message "[ARTICOLI] Indexed file corrupt!"
                          title tit-err
                           icon 3
                set errori to true
           end-evaluate.

      ***---
       TUDM-ERR SECTION.
           use after error procedure on tudm.
           set tutto-ok  to true.
           evaluate status-tudm
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File udm [TUDM] inesistente"
                          title tit-err
                           icon 2
                set errori to true
           when "39"
                display message "File [TUDM] Mismatch size!"
                          title tit-err
                           icon 3
                set errori to true
           when "98"
                display message "[TUDM] Indexed file corrupt!"
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
                display message "File [DESTINI] Mismatch size!"
                          title tit-err
                           icon 3
                set errori to true
           when "98"
                display message "[DESTINI] Indexed file corrupt!"
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
                display message "File [TVETTORI] Mismatch size!"
                          title tit-err
                           icon 3
                set errori to true
           when "98"
                display message "[TVETTORI] Indexed file corrupt!"
                          title tit-err
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
                  x"0d0a""File [TTIPOCLI] inesistente"
                          title tit-err
                           icon 2
                set errori to true
           when "39"
                display message "File [TTIPOCLI] Mismatch size!"
                          title tit-err
                           icon 3
                set errori to true
           when "98"
                display message "[TTIPOCLI] Indexed file corrupt!"
                          title tit-err
                           icon 3
                set errori to true
           end-evaluate.

      ***---
       BLISTER-ERR SECTION.
           use after error procedure on blister.
           set tutto-ok  to true.
           evaluate status-blister
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File [BLISTER] inesistente"
                          title tit-err
                           icon 2
                set errori to true
           when "39"
                display message "File [BLISTER] Mismatch size!"
                          title tit-err
                           icon 3
                set errori to true
           when "98"
                display message "[BLISTER] Indexed file corrupt!"
                          title tit-err
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
                  x"0d0a""File [TPARAMGE] inesistente"
                          title tit-err
                           icon 2
                set errori to true
           when "39"
                display message "File [TPARAMGE] Mismatch size!"
                          title tit-err
                           icon 3
                set errori to true
           when "98"
                display message "[TPARAMGE] Indexed file corrupt!"
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
                  x"0d0a""File [TCODPAG] inesistente"
                          title tit-err
                           icon 2
                set errori to true
           when "39"
                display message "File [TCODPAG] Mismatch size!"
                          title tit-err
                           icon 3
                set errori to true
           when "98"
                display message "[TCODPAG] Indexed file corrupt!"
                          title tit-err
                           icon 3
                set errori to true
           end-evaluate.

       END DECLARATIVES.
         
       MAIN SECTION.
      *     set tutto-ok             to true.
           perform INIT.
           perform APRI-FILES.    
      *     perform SETTA-FONT.

      *     if tutto-ok
              perform CICLO-TORDINI

              if no-dati              
                display message "Nessun Dato per i limiti impostati!"
                     title "Stampa Ordini"
                     icon MB-WARNING-ICON
              end-if

      *     end-if

           perform CHIUDI-FILES.
           perform DISTRUGGI-FONT.

           goback.
 
      ***---
       INIT.
           initialize  tot-peso, peso-totale, tot-peso-utf,
                       tot-peso-no-utf,
                       num-pagina.

           accept  como-data       from century-date.
           accept  como-ora        from time.
           set     no-dati         to false.
           set     primo-passaggio to true.
           set     originale       to true.
           set     prima-volta     to true.

           |Per i programmi che non passano dal richiamo delle date.
           if stordc-da-data = 0 and
              stordc-a-data  = 0
              move 99999999 to stordc-a-data
           end-if.
           if stordc-stato-ord = spaces
              move "T" to stordc-stato-ord
           end-if.

           call "C$CALLEDBY" using CallingPgm.

      ***---
       APRI-FILES.
           open input  tordini,
                       rordini,
                       clienti,
                       articoli,
                       tudm,
                       tvettori,
                       ttipocli,
                       blister,
                       destini
                       tcaumag
                       tcodpag
                       tparamge
                       tpromo
                       tivaese
                       mtordini
                       mrordini
                       agenti.

           accept  path-tmp-ord-mast from environment "PATH_ST"
           inspect path-tmp-ord-mast 
                                   replacing trailing space by low-value
           string path-tmp-ord-mast   delimited by low-value
                  "tmp-ord-mast_"     delimited by size
                  como-data              delimited by size
                  "_"                    delimited by size
                  como-ora               delimited by size
                  into path-tmp-ord-mast
           inspect path-tmp-ord-mast 
                                   replacing trailing low-value by space
           open output tmp-ord-mast.   
           close       tmp-ord-mast.
           open i-o    tmp-ord-mast.

           accept  path-tmp-ord-ev from environment "PATH_ST"
           inspect path-tmp-ord-ev 
                                   replacing trailing space by low-value
           string path-tmp-ord-ev   delimited by low-value
                  "tmp-ord-ev_"     delimited by size
                  como-data              delimited by size
                  "_"                    delimited by size
                  como-ora               delimited by size
                  into path-tmp-ord-ev
           inspect path-tmp-ord-ev replacing trailing low-value by space
           open output tmp-ord-ev.   
           close       tmp-ord-ev.
           open i-o    tmp-ord-ev.

           accept  path-tmp-ord-ev2 from environment "PATH_ST"
           inspect path-tmp-ord-ev2 
                                   replacing trailing space by low-value
           string path-tmp-ord-ev2 delimited by low-value
                  "tmp-ord-ev2_"   delimited by size
                  como-data              delimited by size
                  "_"                    delimited by size
                  como-ora               delimited by size
                  into path-tmp-ord-ev2
           inspect path-tmp-ord-ev2 
                   replacing trailing low-value by space
           open output tmp-ord-ev2.   
           close       tmp-ord-ev2.
           open i-o    tmp-ord-ev2.

      ***---
       CHIUDI-FILES.
           close       tordini,
                       rordini,
                       clienti,
                       articoli,
                       tudm,
                       tvettori,
                       ttipocli,
                       blister,
                       destini
                       tcaumag
                       tcodpag
                       tparamge
                       tpromo
                       tivaese
                       mtordini
                       mrordini
                       agenti.
                                  
           close       tmp-ord-ev.
           delete file tmp-ord-ev.
           close       tmp-ord-ev2.
           delete file tmp-ord-ev2.
           close       tmp-ord-mast.
           delete file tmp-ord-mast.

      ***---
       CICLO-TORDINI.
           move spaces to tge-chiave.
           read tparamge no lock.

      *    luciano
           if stordc-da-anno not = stordc-a-anno or
              stordc-da-num  not = stordc-a-num
              perform CICLO-RIEPILOGHI
           end-if

           if not spl-sta-annu
              set  tutto-ok      to true

tmp-oe        if CallingPgm = "evacli"
tmp-oe           move stordc-da-anno to tor-anno
tmp-oe           move stordc-da-num  to tor-numero
tmp-oe           set fai-tutto to false
tmp-oe           start tordini key >= tor-chiave
tmp-oe                 invalid set no-dati to true
tmp-oe             not invalid               
tmp-oe                 perform until 1 = 2
tmp-oe                    read tordini next 
tmp-oe                         at end exit perform 
tmp-oe                    end-read
tmp-oe                 
tmp-oe                    if tor-anno > stordc-a-anno
tmp-oe                       exit perform
tmp-oe                    end-if
tmp-oe                 
tmp-oe                    if tor-numero > stordc-a-num
tmp-oe                       exit perform
tmp-oe                    end-if
tmp-oe                 
tmp-oe                    perform VALIDA-ORDINE
tmp-oe                 
tmp-oe                    if tutto-ok
tmp-oe                       perform MASTER-EVASIONI
tmp-oe                    end-if
tmp-oe                 end-perform
tmp-oe           end-start                  
tmp-oe           move stordc-da-anno to tor-anno
tmp-oe           move stordc-da-num  to tor-numero  
tmp-oe           start tordini key >= tor-chiave
tmp-oe                 invalid set no-dati to true
tmp-oe             not invalid               
tmp-oe                 perform until 1 = 2
tmp-oe                    read tordini next 
tmp-oe                         at end exit perform 
tmp-oe                    end-read
tmp-oe                 
tmp-oe                    if tor-anno > stordc-a-anno
tmp-oe                       exit perform
tmp-oe                    end-if
tmp-oe                 
tmp-oe                    if tor-numero > stordc-a-num
tmp-oe                       exit perform
tmp-oe                    end-if
tmp-oe                 
tmp-oe                    perform VALIDA-ORDINE
tmp-oe                 
tmp-oe                    if tutto-ok
tmp-oe                       perform ORDINAMENTO-EVASIONI
tmp-oe                    end-if
tmp-oe                 end-perform
tmp-oe           end-start
tmp-oe           set fai-tutto to true
tmp-oe           move low-value to tmp-oe2-rec
tmp-oe           start tmp-ord-ev2 key >= tmp-oe2-chiave
tmp-oe                 invalid continue
tmp-oe             not invalid set OrdinamentoEvasioni to true
tmp-oe                         perform SCORRI-TORDINI
tmp-oe           end-start      
tmp-oe        else                                    
                 set fai-tutto to true
                 move stordc-da-anno to tor-anno
                 move stordc-da-num  to tor-numero
        
                 start tordini key >= tor-chiave
                       invalid set no-dati to true
                   not invalid perform SCORRI-TORDINI
                 end-start
tmp-oe        end-if
           end-if.

      ***---
      * Creo un file che contiene i master per quell'evasione
tmp-oe MASTER-EVASIONI.
           move tor-anno   to ror-anno.
           move tor-numero to ror-num-ordine.
           move 0          to ror-num-riga.
           start rordini key >= ror-chiave
                 invalid continue
             not invalid       
                 perform until 1 = 2
                    read rordini next 
                         at end exit perform 
                    end-read
                    if ror-anno        not = tor-anno  or
                       ror-num-ordine  not = tor-numero
                       exit perform
                    end-if                      
                    move tor-chiave              to tmp-oe-chiave-e
                    move ror-chiave-ordine-testa to tmp-oe-chiave-m
                   |RELAZIONE UNO A MOLTI, PER UN'EVASIONE PIU MASTER
                    read tmp-ord-ev key tmp-oe-chiave
                         invalid 
                         move ror-chiave-ordine-testa to mto-chiave
                         write tmp-oe-rec
                    end-read
                 end-perform 
           end-start.   
           
      ***---
tmp-oe ORDINAMENTO-EVASIONI.
           |scorro i master per determinare lo stato
           |dell'evasione e fare l'ordinamento
           move low-value  to tmp-oe-rec
           move tor-chiave to tmp-oe-chiave-e
           start tmp-ord-ev key >= tmp-oe-chiave
                 invalid continue
             not invalid 
                 set tmp-oe2-evaso to true
                 perform until 1 = 2
                    read tmp-ord-ev next 
                         at end exit perform 
                    end-read
                    if tmp-oe-chiave-e not = tor-chiave
                       exit perform
                    end-if
                    move tmp-oe-chiave-m to mto-chiave
                    read mtordini no lock
                         invalid 
                         set tmp-oe2-non-evaso to true
                     not invalid
                         if mto-urgente-si
                            set tmp-oe2-urgente to true
                            exit perform
                         else  
                            if tmp-oe2-evaso
                               move mto-chiave to mro-chiave
                               move 0          to mro-riga
                               start mrordini key >= mro-chiave
                                     invalid continue
                                 not invalid
                                     perform until 1 = 2
                                        read mrordini next 
                                          at end exit perform
                                        end-read
                                        if mro-chiave-testa not = 
                                           mto-chiave
                                           exit perform
                                        end-if
                                        if mro-qta > mro-qta-e
                                           set tmp-oe2-non-evaso 
                                            to true
                                           exit perform
                                        end-if
                                     end-perform
                               end-start
                            end-if
                         end-if      
                    end-read          
                 end-perform          
                 move tor-chiave to tmp-oe2-chiave-e
                 write tmp-oe2-rec end-write
           end-start.

      ***---
       SCORRI-TORDINI.
           perform until 1 = 2
tmp-oe        if not OrdinamentoEvasioni
                 read tordini next at end exit perform end-read  
tmp-oe        else
tmp-oe           read tmp-ord-ev2 next at end exit perform end-read
tmp-oe           move tmp-oe2-chiave-e to tor-chiave
tmp-oe           read tordini no lock
tmp-oe                invalid display message "J"
tmp-oe           end-read
tmp-oe        end-if

              if tor-anno > stordc-a-anno
                 exit perform
              end-if

              if tor-numero > stordc-a-num
                 exit perform
              end-if

              perform VALIDA-ORDINE

              if tutto-ok
LUBEXX           perform CONTA-RIGHE

LUBEXX           |La procedura dei solleciti stampa una copia
LUBEXX           |sola dell'ordine con la dicitura SOLLECITO
LUBEXX           set sollecito to false
LUBEXX           if CallingPgm = "gordcvar"
                    set sollecito to true

                    set originale to true                 
                    perform CICLO-STAMPA
                   
                    if spl-sta-annu
                       exit perform
                    end-if
LUBEXX           else
                    set originale to true                 
                    perform CICLO-STAMPA
                 
                    if spl-sta-annu
                       exit perform
                    end-if

      *****              |04/02/2014 Walter: Togliere la stampa controllo
      *****              |da ordini/bozze (manuale e dopo evasione)
      *****              if CallingPgm = "stordc"
      *****                 set copia     to true
      *****                 perform CICLO-STAMPA
      *****              end-if

                    set  cli-tipo-C  to true
                    move tor-cod-cli to cli-codice
                    read clienti no lock invalid continue end-read
                    if cli-nazione not = "ITA"
                       set ufficio   to true
                       perform CICLO-STAMPA
                    end-if

      *****              if tor-bloccato
      *****                 set bloccato   to true
      *****                 perform CICLO-STAMPA
      *****              end-if

LUBEXX           end-if

              end-if
           end-perform.           

           if not primo-passaggio
              perform CHIUDI-STAMPA
           end-if.

      ***---
       VALIDA-ORDINE.
           set tutto-ok to true.

           if tor-numero < stordc-da-num or 
              tor-numero > stordc-a-num
              set errori to true
           end-if.

      *    controllo aggiunto su richiesta del Sig. Trivella del 07/12/2004
           if tor-num-bolla not = 0 or tor-data-bolla not = 0
              if CallingPgm not = "solleciti"
                 set errori to true
              end-if
           end-if.

LUBEXX     if tor-fattura-manuale
LUBEXX        set errori to true
LUBEXX     end-if.

           evaluate true
           when stordc-evasioni
                if tor-da-ordine-no
                   set errori   to true
                end-if
                if stordc-tipocli not = spaces
                   set cli-tipo-C to true
                   move tor-cod-cli to cli-codice
                   read clienti no lock
                   if cli-tipo not = stordc-tipocli
                      set errori to true
                   end-if
                end-if

           when stordc-GDO
                if tor-da-ordine-si
                   set errori   to true
                end-if
           when stordc-tutti
                continue
           end-evaluate

      *         10 tor-data-ordine  PIC  9(8).
      *         10 tor-data-passaggio-ordine    PIC  9(8).
           if tor-data-creazione < stordc-da-data or 
              tor-data-creazione > stordc-a-data
              set errori   to true
           end-if

           if tutto-ok and fai-tutto
LUBEXX        initialize des-rec cli-rec vet-rec
LUBEXX                  replacing numeric data by zeroes
LUBEXX                       alphanumeric data by spaces

              perform RELAZIONE-TORDINI-CLIENTI
              perform RELAZIONE-TORDINI-DESTINI
              perform RELAZIONE-TORDINI-TVETTORI
              perform RELAZIONE-TORDINI-TTIPOCLI
           end-if.

      ***---
LUBEXX CONTA-RIGHE.
           close       tmp-ord-mast.
           open output tmp-ord-mast.
           close       tmp-ord-mast.
           open i-o    tmp-ord-mast.

LUBEXX     move stordc-da-anno to ror-anno.
LUBEXX     move tor-numero     to ror-num-ordine.
LUBEXX     move low-values     to ror-num-riga.
LUBEXX                   
           set master-evasi           to true.  
           set trovato-master-urgente to false.

           move 0 to tot-note.      
           perform varying idx-note from 1 by 1
                     until idx-note > 4
              move 0      to el-promo(idx-note)
              move spaces to el-nota(idx-note)
           end-perform.

           perform varying idx-art from 1 by 1 
                     until idx-art > 10
              move 0 to el-art(idx-art)
              move 0 to el-qta(idx-art)
           end-perform.            
           move 0 to idx-art.

LUBEXX     start rordini key >= ror-chiave
LUBEXX           invalid continue
LUBEXX       not invalid
LUBEXX           move 0 to tot-righe tot-pagine
LUBEXX           perform until 1 = 2
LUBEXX              read rordini next at end exit perform end-read
LUBEXX      
LUBEXX              if ror-anno        not = tor-anno  or
LUBEXX                 ror-num-ordine  not = tor-numero
LUBEXX                 exit perform
LUBEXX              end-if          

                    |Prima bastava trovare un non evaso per uscire dal 
                    |ciclo. Ora ne devo valutare uno per uno
                    |per avere l'ordinamento. Per il flag S di stampa
                    |non cambia nulla, ma uso lo stesso ciclo per 
                    |avere l'ordinamento
      *****              if master-evasi
                       move ror-chiave-ordine-testa 
                         to tmp-om-chiave-master
                       read tmp-ord-mast key tmp-om-chiave-master
                            invalid perform VALUTA-MASTER-RIFERIMENTO
                       end-read
      *****              end-if      

                    perform AGGIUNGI-IVA

                    if tot-note <= 4            
                       if ror-promo not = 0
                          move ror-promo to tpr-codice
                          read tpromo no lock
                               invalid continue
                           not invalid
                               if tpr-note not = spaces   
                                  set NotaNonPresente to true
                                  perform varying idx-note from 1 by 1
                                            until idx-note > 4
                                     if el-promo(idx-note) = ror-promo
                                        set NotaPresente to true
                                        exit perform
                                     end-if
                                  end-perform
                                  if NotaNonPresente
                                     add 1 to tot-note
                                     if tot-note <= 4
                                        move tpr-note   
                                          to el-nota(tot-note)
                                        move tpr-codice 
                                          to el-promo(tot-note)
                                     end-if
                                  end-if
                               end-if
                          end-read
                       end-if
                    end-if              
LUBEXX
LUBEXX              add 1 to tot-righe
LUBEXX           end-perform   

                 compute totale = el-imponib(1) + 
                                  el-imponib(2) + 
                                  el-imponib(3)
                 move totale to r-tot
                 perform CALCOLA-IVA
LUBEXX     end-start.                  
LUBEXX     if tot-righe > 0
LUBEXX        move 0 to resto
LUBEXX        divide tot-righe by MaxRowsPerPage
LUBEXX                  giving tot-pagine
LUBEXX               remainder resto
LUBEXX        if resto not = 0
LUBEXX           add 1 to tot-pagine
LUBEXX        end-if
LUBEXX     end-if.

      ***---
       VALUTA-MASTER-RIFERIMENTO.
           move ror-chiave-ordine-testa to mto-chiave
           read mtordini no lock
                invalid set master-non-evasi to true
            not invalid                
                initialize tmp-om-stato
                set tmp-om-stato-evaso to true
                move mto-chiave to mro-chiave
                move low-value  to mro-riga
                start mrordini key >= mro-chiave
                      invalid 
                      set master-non-evasi       to true
                      set tmp-om-stato-non-evaso to true
                  not invalid
                      perform until 1 = 2
                         read mrordini next 
                              at end exit perform 
                         end-read
                         if mro-chiave-testa not = mto-chiave
                            exit perform
                         end-if
                         if mro-qta > mro-qta-e 
                            set master-non-evasi to true 
                            set tmp-om-stato-non-evaso to true
                            if idx-art < 10
                               add 1 to idx-art
                               compute el-qta(idx-art) =
                                       mro-qta - mro-qta-e
                               move mro-cod-articolo to el-art(idx-art)
                            end-if
                         end-if
                      end-perform
                end-start              
               |se è urgente va prima di tutti
                if mto-urgente-si 
                   set trovato-master-urgente to true
                   set tmp-om-stato-urgente   to true
                end-if                       
                if mto-saldi-promo-si
                   move "P" to tmp-om-promo
                else                                  
                   move " " to tmp-om-promo
                end-if
                if mto-saldi-banco-si
                   move "B" to tmp-om-banco
                else
                   move " " to tmp-om-banco
                end-if 
                move mto-chiave to tmp-om-chiave-master
                write tmp-om-rec end-write
           end-read.

      ***---
       CICLO-STAMPA.
           move zero              to num-pagina
           perform STAMPA-INTESTAZIONE.

           if not spl-sta-annu
              move stordc-da-anno to ror-anno
              move tor-numero     to ror-num-ordine
              move low-values     to ror-num-riga
      
              start rordini key >= ror-chiave
                    invalid continue
                not invalid
                    perform until 1 = 2
                       read rordini next 
                       at end
                          set rottura    to true

                          perform STAMPA-PIEDE
                          initialize tot-peso,     peso-totale,
                                     tot-peso-utf, tot-peso-no-utf
                                     tot-colli

      *****                    if originale or copia
      *****                       perform SALTA-PAGINA
      *****                    end-if

                          set rottura to false
                          exit perform
                       end-read
            
                       if  ror-anno        not = tor-anno  or
                           ror-num-ordine  not = tor-numero

                           set rottura     to true

                           perform STAMPA-PIEDE
                           initialize tot-peso,     peso-totale,
                                      tot-peso-utf, tot-peso-no-utf
                                      tot-colli

      *****                     perform SALTA-PAGINA

                           set rottura to false
                           exit perform

                       else
                          perform STAMPA-CORPO 
                       end-if

                    end-perform
              end-start
           end-if.

      ***---
       STAMPA-INTESTAZIONE.
           if primo-passaggio
              perform APRI-STAMPA

              if spl-sta-annu                 
                 exit paragraph            
              end-if

              set primo-passaggio to false
           else
              perform SALTA-PAGINA
           end-if.

      * STAMPA DATI CLIENTE
           set  spl-stringa        to true.
           move CourierNew10       to spl-hfont.

      *****     if originale
              add 1                to num-pagina
      *****     end-if.

           move cli-ragsoc-1       to spl-riga-stampa
           move 1                  to spl-riga, spl-colonna.
           call "spooler"       using spooler-link.

           move cli-indirizzo      to spl-riga-stampa.
           add  0,5                to spl-riga.
           move 1                  to spl-colonna.
           call "spooler"       using spooler-link.

           inspect cli-localita replacing trailing spaces by low-value.

           initialize spl-riga-stampa.
           string  cli-cap         delimited size,
                   " "             delimited size,
                   cli-localita    delimited low-value,
LUBEXX             " ("            delimited size,
LUBEXX             cli-prov        delimited size
LUBEXX             ")"             delimited size
              into spl-riga-stampa.
           add  0,5                to spl-riga.
           move 1                  to spl-colonna.
           call "spooler"       using spooler-link.

      * STAMPA DATI DEL DESTINO (solo se presenti)
      *     if tor-prg-destino not = ZERO
           if des-ragsoc-1 not = space
              move des-ragsoc-1    to spl-riga-stampa
           else
              move cli-ragsoc-1    to spl-riga-stampa
           end-if
           move 1                  to spl-riga
           move 10                 to spl-colonna
           call "spooler"       using spooler-link
           
           if des-indirizzo not = space
              move des-indirizzo   to spl-riga-stampa
           else
              move cli-indirizzo   to spl-riga-stampa
           end-if
           add  0,5                to spl-riga
           move 10                 to spl-colonna
           call "spooler"       using spooler-link

           if des-cap not = spaces
              move des-cap to como-cap
           else
              move cli-cap to como-cap
           end-if

           if des-localita not = space
              move des-localita    to como-loca
           else
              move cli-localita    to como-loca
           end-if
           inspect como-loca replacing trailing spaces by low-value

           if des-prov not = space
              move des-prov        to como-prov
           else
              move cli-prov        to como-prov
           end-if         
     
           move tor-cod-agente to age-codice
           read agenti no lock
                invalid move spaces to age-ragsoc-1
           end-read.

           initialize spl-riga-stampa.
           string  como-cap        delimited size,
                   " "             delimited size,
                   como-loca       delimited low-value,
                   " ("            delimited size,
                   como-prov       delimited size
                   ")"             delimited size
              into spl-riga-stampa.

           add  0,5                to spl-riga.
           move 10                 to spl-colonna.
           call "spooler"       using spooler-link.

      *     end-if.
                                                                       
           inspect age-ragsoc-1 replacing trailing spaces by low-value.
           move age-codice to age-codice-x.
           inspect age-codice-x replacing leading x"30" by x"20".
           call "C$JUSTIFY" using age-codice-x, "L"
           inspect age-codice-x replacing trailing spaces by low-value.
           initialize r-agente.
           string age-codice-x delimited low-value
                  " "          delimited size
                  age-ragsoc-1 delimited low-value
             into r-agente
           end-string.

           move CourierNew8        to spl-hfont.
           move riga-agente        to spl-riga-stampa.
           move 3,1                to spl-riga.
           move 1                  to spl-colonna.
           call "spooler"       using spooler-link.

      * DATA ED ORA
      *    RESTITUISCE LA DATA IN FORMATO gg/mm/aa
           move all "/"            to data-6.
           move como-data(7:2)     to data-6(1:2).
           move como-data(5:2)     to data-6(4:2).
           move como-data(3:4)     to data-6(7:2).

      *    RESTITUISCE L'ORA IN FORMATO HH:MM
           move all ":"            to ora-4.
           move como-ora(1:2)      to ora-4(1:2).
           move como-ora(3:2)      to ora-4(4:2).
                                                
           move CourierNew10       to spl-hfont.
           move riga-data-ora      to spl-riga-stampa.
           move 3                  to spl-riga.
           move 1                  to spl-colonna.
           call "spooler"       using spooler-link.
           
      * TIPOLOGIA CLIENTE                          
           call "C$TOUPPER"  using tcl-descrizione, value 30.
           move tcl-descrizione to spl-riga-stampa.
      *****     initialize spl-riga-stampa.
      *****     |Sulle GDO NIENTE
      *****     evaluate true
      *****     when tcl-evasione-ESTERO
      *****          move "ESTERO"       to spl-riga-stampa
      *****     when tcl-evasione-TRAD
      *****          move "TRADIZIONALE" to spl-riga-stampa
      *****     end-evaluate
           move CourierNew16B      to spl-hfont.

           move 2,8                to spl-riga.
           move 8,5                to spl-colonna.
           call "spooler"       using spooler-link.

           if trovato-master-urgente
              move "* URGENTE *" to spl-riga-stampa
              move 2,7           to spl-riga
              move 16,3          to spl-colonna  
              move CourierNew16B to spl-hfont
              call "spooler"       using spooler-link
           end-if.

      * SIGLA VETTORE E NUMERO ORDINE
           initialize spl-riga-stampa.

           move tor-numero           to edit-numero-8.
           move tor-causale          to tca-codice.
           read tcaumag no lock.

           string  vet-sigla       delimited low-value
                   " "             delimited size
                   edit-numero-8   delimited size
                   "    "          delimited size
                   tca-cod-magaz   delimited size
              into spl-riga-stampa.  

           move CourierNew16B      to spl-hfont.

           move 3,8                to spl-riga.
           move 13                 to spl-colonna.
           call "spooler"       using spooler-link.


      * CODICE CLIENTE, N. ORDINE DEL
           move CourierNew10         to spl-hfont.

      * RIFERIMENTO MASTER
           if prima-volta
              perform RIGA-MASTER-RIFERIMENTO
              set prima-volta to true
           end-if.

           move riga-master          to spl-riga-stampa.
           move 3,5                  to spl-riga.
           move 1                    to spl-colonna.
           call "spooler"         using spooler-link.
           add 1 to spl-riga.

           move tor-cod-cli          to r-cod-cli.
           move tor-data-ordine(7:2) to r-gg.
           move tor-data-ordine(5:2) to r-mm.
           move tor-data-ordine(3:2) to r-aa.
           move tor-num-ord-cli      to r-num-ord-cli.

           move riga-del             to spl-riga-stampa.
           move 4                    to spl-riga.
           move 1                    to spl-colonna.
           call "spooler"         using spooler-link.

           add 0,4  to spl-riga.      
           if idx-art not = 0
              if idx-art > 10
                 move "TAGLI: PIU' DI 10 ARTICOLI" to spl-riga-stampa
              else
                 move idx-art to tot-art
                 move "TAGLI: " to spl-riga-stampa
                 move 8 to IdxChar
                 perform varying idx-art from 1 by 1 
                           until idx-art > tot-art
                    move el-art(idx-art) to como-art    
                    if idx-art > 1
                       move " - " to spl-riga-stampa(IdxChar:3)
                       add 3 to IdxChar
                    end-if
                    move 0 to NumChar
                    inspect como-art replacing leading x"30" by x"20"
                    call "C$JUSTIFY" using como-art, "L"
                    inspect como-art tallying NumChar
                                     for characters before spaces
                    move como-art(1:NumChar) 
                      to spl-riga-stampa(IdxChar:NumChar)
                    add NumChar to IdxChar      

                    move "(" to spl-riga-stampa(IdxChar:1)
                    add 1 to IdxChar
                    
                    move el-qta(idx-art) to como-art
                    move 0 to NumChar
                    inspect como-art replacing leading x"30" by x"20"
                    call "C$JUSTIFY" using como-art, "L"
                    inspect como-art tallying NumChar
                                     for characters before spaces
                    move como-art(1:NumChar) 
                      to spl-riga-stampa(IdxChar:NumChar)
                    add NumChar to IdxChar

                    move ")" to spl-riga-stampa(IdxChar:1)
                    add 1 to IdxChar

                 end-perform
              end-if
              move 4,45 to spl-riga
              move 1 to spl-colonna            
              move CourierNew8          to spl-hfont
              call "spooler" using spooler-link
              move CourierNew10         to spl-hfont
           end-if.

           if tor-cod-pagamento = tge-cod-pag-anticipato
              move 3,8 to spl-riga
              move CourierNew16B          to spl-hfont

              move "PA"                   to tblpa-codice1
              move tge-cod-pag-anticipato to tblpa-codice2
              move spaces                 to tblpa-descrizione1
              move spaces                 to tblpa-descrizione2
              read tcodpag invalid continue end-read
              initialize spl-riga-stampa
              inspect tblpa-descrizione1 replacing trailing 
                                         spaces by low-value
              string  tblpa-descrizione1 delimited low-value
                      " "                delimited size
                      tblpa-descrizione2 delimited size
                      into spl-riga-stampa
              end-string

              move 17                   to spl-tipo-colonna
              call "spooler"         using spooler-link
              move 0                    to spl-tipo-colonna
           end-if.                               

           if tot-note not = 0          
              if tot-note > 4
                 move 4 to tot-note
              end-if
              move spl-riga to save-spl-riga
              move 4,75     to spl-riga

              perform varying idx-note from 1 by 1 
                        until idx-note > tot-note  
LUBEXX           move 0             to spl-tipo-colonna
LUBEXX           move CourierNew16B to spl-hfont
                 move el-nota(idx-note)    to spl-riga-stampa 
                 move 1                    to spl-colonna
                 call "spooler"         using spooler-link
                 add 0,59 to spl-riga
              end-perform
              move save-spl-riga to spl-riga
           end-if.

           move 7,1 to spl-riga.

      * STAMPA RIGA TRATTINI                    
           move CourierNew10        to spl-hfont.
           move 1                   to spl-colonna.     
           move riga-fill           to spl-riga-stampa.
           call "spooler"        using spooler-link.

           add 0 to spl-riga.                       

      ***---                                                            
       STAMPA-CORPO.
           if spl-riga > max-righe-corpo 
              move 1 to spl-riga, spl-colonna
      *****        add  1 to num-pagina

              perform STAMPA-PIEDE
      *****        perform SALTA-PAGINA
              perform STAMPA-INTESTAZIONE
           end-if.

           perform RELAZIONE-RORDINI-ARTICOLI.

           add  0,4                 to spl-riga.

           move 1                   to spl-colonna.

LUBEXX     |Se riga OMAGGIO sostituisco il codice con "OMAGGI"
LUBEXX     if ror-no-omaggio
LUBEXX        move ror-cod-articolo    to r-cod-articolo
LUBEXX        call "C$JUSTIFY" using r-cod-articolo, "R"
LUBEXX        inspect r-cod-articolo replacing leading x"30" by x"20"
LUBEXX     else
LUBEXX        move "OMAGGI" to r-cod-articolo
LUBEXX     end-if.

           move ror-num-colli       to r-num-colli.

      *****     if bloccato and ror-bloccato
      *****        move "*"    to r-bloccato
      *****     else
      *****        move spaces to r-bloccato
      *****     end-if.
           if ror-bloccato
              move "*"    to r-bloccato
           else
              move spaces to r-bloccato
           end-if.

           move art-descrizione     to r-des-articolo.

           if art-mag-std not = "LBX"
              move "*" to r-ast
           else                      
              move spaces to r-ast
           end-if.

           move art-cod-art-frn to r-art-codfrn.

           inspect ror-des-imballo replacing trailing spaces 
                                          by low-value.
           move ror-qta-imballi to imballi-ed.
           call "C$JUSTIFY"  using imballi-ed, "L".
           initialize r-des-imballo.
           inspect ror-des-imballo replacing trailing 
                                   spaces by low-value.
           move ror-qta-imballi to imballi-ed.
           call "C$JUSTIFY"  using imballi-ed, "L".
           initialize r-des-imballo.

BLISTR     if ror-si-blister
BLISTR        string  ror-des-imballo delimited by low-value
BLISTR                into r-des-imballo
BLISTR        end-string
BLISTR     else
              string  ror-des-imballo delimited by low-value
                      " da "          delimited by size
                      imballi-ed      delimited by spaces
                      " x "           delimited by size
                      art-udm-imballo delimited by size
                      into r-des-imballo
              end-string
           end-if.

      *     move ror-peso-utf        to r-peso-utf.
           move art-unita-di-misura to r-udm.
           move ror-prg-peso to r-peso.    
           move riga-corpo          to spl-riga-stampa. 
LUBEXX     move CourierNew8         to spl-hfont.
           call "spooler"        using spooler-link.

LUBEXX     move 52            to spl-tipo-colonna.
LUBEXX     move CourierNew10B to spl-hfont.

LUBEXX     move ror-qta       to r-qta.
LUBEXX     move riga-qta      to spl-riga-stampa.
           subtract 0,05 from spl-riga.
LUBEXX     call "spooler"  using spooler-link.   
           add 0,05 to spl-riga.
LUBEXX     initialize spl-riga-stampa.
LUBEXX     move 0             to spl-tipo-colonna.
LUBEXX     move CourierNew10  to spl-hfont.

           perform CALCOLA-TOTALI-PIEDE.

      ***---
       CALCOLA-TOTALI-PIEDE.
           add ror-num-colli        to tot-colli.   
   
           compute tot-peso-utf = 
                 ( ror-peso-utf     * ror-qta ) + tot-peso-utf.

           compute tot-peso-no-utf = 
                 ( ror-peso-non-utf * ror-qta ) + tot-peso-no-utf.
           
           add tot-peso-utf         to tot-peso-no-utf giving tot-peso.
           move tot-peso            to peso-totale.

      ***---
       STAMPA-PIEDE.
           move pos-piede           to spl-riga.

      * STAMPA RIGA TRATTINI
           move 1                   to spl-colonna.     
           move riga-fill           to spl-riga-stampa.
           call "spooler"        using spooler-link.

      * NOTE
           move CourierNew12B       to spl-hfont.

           initialize spl-riga-stampa
           add 0,5                  to spl-riga.
           move 5                   to spl-colonna.
           move "******>"           to spl-riga-stampa.
           call "spooler"        using spooler-link.

           move 7                   to spl-colonna.

           move all "/"             to tor-data-note-i.
           move tor-data-note1(1:4) to tor-data-note-i(7:4).
           move tor-data-note1(5:2) to tor-data-note-i(4:2).
           move tor-data-note1(7:2) to tor-data-note-i(1:2).

           initialize spl-riga-stampa
           string  tor-note1        delimited size,
                   " "              delimited size,
                   tor-data-note-i  delimited size,
             into  spl-riga-stampa.

           call "spooler"        using spooler-link.

           move CourierNew10        to spl-hfont.
                                                    
           move 7                   to spl-colonna.
           add 0,5                  to spl-riga.
           move tor-note2           to spl-riga-stampa.
           call "spooler"        using spooler-link. 
                                                  
      *****     move CourierNew12B       to spl-hfont.  
      *****     move 15,5                to spl-colonna.
      *****     move "TOTALE"            to spl-riga-stampa.
      *****     call "spooler"        using spooler-link.   
      *****
      *****     move 17,0                to spl-colonna.
      *****     move r-tot               to spl-riga-stampa.
      *****     call "spooler"        using spooler-link. 
      *****                                            
           move CourierNew10        to spl-hfont.
           move 7                   to spl-colonna.
           add 0,5                  to spl-riga.
           move tor-note3           to spl-riga-stampa.
           call "spooler"        using spooler-link.
      *****                                            
      *****     move CourierNew12B       to spl-hfont.      
      *****     move 15,5                to spl-colonna.
      *****     move "IVATO"             to spl-riga-stampa.
      *****     call "spooler"        using spooler-link.   
      *****
      *****     move 17,0                to spl-colonna.
      *****     move r-tot-iva           to spl-riga-stampa.
      *****     call "spooler"        using spooler-link. 
                                                  
           move CourierNew10        to spl-hfont.  
           move 7                   to spl-colonna.
           add 0,5                  to spl-riga.
           move tor-note4           to spl-riga-stampa.
           call "spooler"        using spooler-link.

      * TOTALE DEI COLLI
           initialize spl-riga-stampa
           if rottura
              subtract 0,5           from spl-riga
              move 1                   to spl-colonna
              move tot-colli           to edit-tot-colli
              string  "Tot. Colli "     delimited size,
                      edit-tot-colli   delimited size
                 into spl-riga-stampa

              call "spooler"        using spooler-link

      * TOTALI 
   
      *    PESO UTF
              initialize spl-riga-stampa
              move tot-peso-utf        to edit-tot-peso
              move 15                  to spl-colonna
              add  1                   to spl-riga
              move edit-tot-peso       to spl-riga-stampa
              call "spooler"        using spooler-link

      *    PESO NON UTF
              initialize spl-riga-stampa
              move tot-peso-no-utf     to edit-tot-peso
              move 15                  to spl-colonna
              add  0,5                 to spl-riga
              move edit-tot-peso       to spl-riga-stampa
              call "spooler"        using spooler-link

      *    PESO TOTALE
              initialize spl-riga-stampa
              move peso-totale         to edit-tot-peso
              move 15                  to spl-colonna
              add  0,5                 to spl-riga
              move edit-tot-peso       to spl-riga-stampa
              call "spooler"        using spooler-link
           end-if.

      * VETTORE
           initialize spl-riga-stampa
           add 2                    to spl-riga.
           move 1                   to spl-colonna.
           move vet-descrizione     to spl-riga-stampa.
           call "spooler"        using spooler-link.
                          
           move CourierNew8       to spl-hfont.
           initialize spl-riga-stampa
           add 0,5                  to spl-riga.
           move vet-indirizzo       to spl-riga-stampa.
           call "spooler"        using spooler-link.

      * NUMERO DI PAGINA.
           initialize spl-riga-stampa.

           move 16                  to spl-colonna.
           move CourierNew16B       to spl-hfont.
           move num-pagina          to edit-num-pagina.
           move tot-pagine          to edit-num-dicui.
           inspect edit-num-dicui replacing leading x"30" by x"20".
           call "C$JUSTIFY" using edit-num-dicui, "L".
           move riga-num-pagina     to spl-riga-stampa.
           call "spooler"        using spooler-link.

      * SE SOLLECITO STAMPA LA SCRITTA SOLLECITO.
LUBEXX     initialize spl-riga-stampa.
LUBEXX     if sollecito
LUBEXX        move Font-Copia-TimesNewRoman30 to spl-hfont
LUBEXX        move 25                         to spl-riga
LUBEXX        move 06                         to spl-colonna
LUBEXX        move "SOLLECITO"                to spl-riga-stampa
LUBEXX        call "spooler"         using spooler-link
LUBEXX     end-if.

      * SE COPIA STAMPA LA SCRITTA CONTROLLO.
           initialize spl-riga-stampa.
           if copia
              move Font-Copia-TimesNewRoman30 to spl-hfont
              move 27                       to spl-riga
              move 06                         to spl-colonna
              move "CONTROLLO"                to spl-riga-stampa
              call "spooler"         using spooler-link
           end-if.

      * SE UFFICIO STAMPA LA TERZA COPIA (SOLO PER ESTERI)
           initialize spl-riga-stampa.
           if ufficio
              move Font-Copia-TimesNewRoman30 to spl-hfont
              move 27                         to spl-riga
              move 06                         to spl-colonna
              move "UFFICIO"                  to spl-riga-stampa
              call "spooler"         using spooler-link
           end-if.

      * SE CONTROLLO STAMPA LA QUARTA COPIA
      *****     initialize spl-riga-stampa.
      *****     if bloccato
      *****        move Font-Copia-TimesNewRoman30 to spl-hfont
      *****        move 25                         to spl-riga
      *****        move 06                         to spl-colonna
      *****        move "BLOCCATO"                 to spl-riga-stampa
      *****        call "spooler"         using spooler-link
      *****     end-if.

      **---
       SALTA-PAGINA.
           set spl-salto-pagina     to true.
           call "spooler"        using spooler-link.                                                            
      *    luciano
           set spl-stringa to true
           |Mi riposiziono ad inizio foglio
           move 0      to spl-riga
           move spaces to spl-riga-stampa
           call "spooler" using spooler-link.
      *    luciano


      ***---
       APRI-STAMPA.
           if stordc-stampante not = spaces
              move stordc-stampante to selprint-stampante
           else
              if stordc-tipocli not = spaces
                 move stordc-tipocli to tcl-codice
                 read ttipocli no lock
                 move tcl-stampante to selprint-stampante
              else
                 call   "selprint" using selprint-linkage
                 cancel "selprint"
              end-if
           end-if.

           if selprint-stampante not = space
              move selprint-num-copie to SPL-NUM-COPIE
              move selprint-stampante to SPL-NOME-STAMPANTE

              set spl-vertical         to true
              set spl-apertura         to true
              move 1                   to spl-margine-sinistro
              move 1                   to spl-margine-destro
              move 1                   to spl-margine-inf
              move "Stampa Ordini"     to spl-nome-job
              call "spooler"        using spooler-link

              compute max-righe-corpo = spl-altezza - 8,5
      *    luciano
      *    compute pos-piede       = spl-altezza -  7
              compute pos-piede       = spl-altezza -  8
      *    luciano
           else
              set spl-sta-annu to true
           end-if

           if spl-sta-annu 
              cancel "spooler"
      *    luciano
           else
              perform SETTA-FONT
              set spl-stringa to true
              |Mi riposiziono ad inizio foglio
              move 1            to spl-colonna
              move 1            to spl-riga
              move spaces       to spl-riga-stampa
              move CourierNew10 to spl-hfont
              call "spooler" using spooler-link
      *    luciano
           end-if.

      ***---
       CHIUDI-STAMPA.
           set spl-chiusura         to true.
           cancel "spooler".


      ***---
      * RELAZIONI DELLA TESTATA ORDINI (TORDINI)
      ***---
       RELAZIONE-TORDINI-CLIENTI.
           set cli-tipo-C        to true.

           move tor-cod-cli      to cli-codice.

           read clienti key cli-chiave
              invalid   set errori to true
           end-read.

      ***---
       RELAZIONE-TORDINI-DESTINI.
           if tor-prg-destino   not = 0
              move tor-cod-cli      to des-codice
              move tor-prg-destino  to des-prog
          
              read destini key des-chiave
                   invalid set errori to true
              end-read
           end-if.

      ***---
       RELAZIONE-TORDINI-TVETTORI.
           move tor-vettore      to vet-codice.

           if tor-vettore not = 0
              read tvettori key vet-chiave
                   invalid  set errori to true
              end-read
           end-if.

      ***---
       RELAZIONE-TORDINI-TTIPOCLI.
           move cli-tipo   to tcl-codice.

           read ttipocli key tcl-chiave
                invalid  set errori to true
           end-read.

      ***---
      * RELAZIONI DELLE RIGHE DEGLI ORDINI (RORDINI)
      ***---
       RELAZIONE-RORDINI-ARTICOLI.
           move ror-cod-articolo to art-codice.
           
           read articoli key art-chiave
              invalid 
                 initialize art-rec
           end-read.

      ***---
       SETTA-FONT.
           set tutto-ok             to true.

           initialize WFONT-DATA.
           move 8                   to WFONT-SIZE.
           move "Courier New"       to WFONT-NAME.
           set WFONT-BOLD           to false.
           set WFCHARSET-DONT-CARE  to true    
           set WFONT-ITALIC         to false   
           set WFONT-UNDERLINE      to false   
           set WFONT-STRIKEOUT      to false   
           set WFONT-FIXED-PITCH    to false   
           move ZERO                to WFONT-CHAR-SET
           set WFDEVICE-WIN-PRINTER to true.
           CALL "W$FONT" USING WFONT-GET-CLOSEST-FONT, 
                               CourierNew8, WFONT-DATA
                               giving WFONT-STATUS.


      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if WFONT-STATUS = WFONTERR-FONT-NOT-FOUND
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if.

           initialize WFONT-DATA.
           move 10                  to WFONT-SIZE.
           move "Courier New"       to WFONT-NAME.
           set WFONT-BOLD           to false.
           set WFCHARSET-DONT-CARE  to true    
           set WFONT-ITALIC         to false   
           set WFONT-UNDERLINE      to false   
           set WFONT-STRIKEOUT      to false   
           set WFONT-FIXED-PITCH    to false   
           move ZERO                to WFONT-CHAR-SET
           set WFDEVICE-WIN-PRINTER to true.
           CALL "W$FONT" USING WFONT-GET-CLOSEST-FONT, 
                               CourierNew10, WFONT-DATA
                               giving WFONT-STATUS.


      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if WFONT-STATUS = WFONTERR-FONT-NOT-FOUND
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if.

           initialize WFONT-DATA.
           move 10                  to WFONT-SIZE.
           move "Courier New"       to WFONT-NAME.
           set WFONT-BOLD           to true.
           set WFCHARSET-DONT-CARE  to true    
           set WFONT-ITALIC         to false   
           set WFONT-UNDERLINE      to false   
           set WFONT-STRIKEOUT      to false   
           set WFONT-FIXED-PITCH    to false   
           move ZERO                to WFONT-CHAR-SET
           set WFDEVICE-WIN-PRINTER to true.
           CALL "W$FONT" USING WFONT-GET-CLOSEST-FONT, 
                               CourierNew10B, WFONT-DATA
                               giving WFONT-STATUS.


      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if WFONT-STATUS = WFONTERR-FONT-NOT-FOUND
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if.

           initialize WFONT-DATA.
           move 30                  to WFONT-SIZE.
           move "Times New Roman"   to WFONT-NAME.
           set WFONT-BOLD           to true.
           set WFCHARSET-DONT-CARE  to true    
           set WFONT-ITALIC         to false   
           set WFONT-UNDERLINE      to false   
           set WFONT-STRIKEOUT      to false   
           set WFONT-FIXED-PITCH    to false   
           move ZERO                to WFONT-CHAR-SET
           set WFDEVICE-WIN-PRINTER to true.
           CALL "W$FONT" USING WFONT-GET-CLOSEST-FONT, 
                               Font-Copia-TimesNewRoman30, WFONT-DATA
                               giving WFONT-STATUS.

           if WFONT-STATUS = WFONTERR-FONT-NOT-FOUND
              set errori to true
              perform MESSAGGIO-ERR-FONT
           end-if.
                                            
           initialize WFONT-DATA.
           move 16                  to WFONT-SIZE.
           move "Courier New"       to WFONT-NAME.
           set WFONT-BOLD           to true.
           set WFCHARSET-DONT-CARE  to true    
           set WFONT-ITALIC         to false   
           set WFONT-UNDERLINE      to false   
           set WFONT-STRIKEOUT      to false   
           set WFONT-FIXED-PITCH    to false   
           move ZERO                to WFONT-CHAR-SET
           set WFDEVICE-WIN-PRINTER to true.
           CALL "W$FONT" USING WFONT-GET-CLOSEST-FONT, 
                               CourierNew16B, WFONT-DATA
                               giving WFONT-STATUS.

      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if WFONT-STATUS = WFONTERR-FONT-NOT-FOUND
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if.
           initialize WFONT-DATA.
           move 18                  to WFONT-SIZE.
           move "Courier New"       to WFONT-NAME.
           set WFONT-BOLD           to true.
           set WFCHARSET-DONT-CARE  to true    
           set WFONT-ITALIC         to false   
           set WFONT-UNDERLINE      to false   
           set WFONT-STRIKEOUT      to false   
           set WFONT-FIXED-PITCH    to false   
           move ZERO                to WFONT-CHAR-SET
           set WFDEVICE-WIN-PRINTER to true.
           CALL "W$FONT" USING WFONT-GET-CLOSEST-FONT, 
                               CourierNew18B, WFONT-DATA
                               giving WFONT-STATUS.

      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if WFONT-STATUS = WFONTERR-FONT-NOT-FOUND
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if.

           initialize WFONT-DATA.
           move 12                  to WFONT-SIZE.
           move "Courier New"       to WFONT-NAME.
           set WFONT-BOLD           to true.
           set WFCHARSET-DONT-CARE  to true    
           set WFONT-ITALIC         to false   
           set WFONT-UNDERLINE      to false   
           set WFONT-STRIKEOUT      to false   
           set WFONT-FIXED-PITCH    to false   
           move ZERO                to WFONT-CHAR-SET
           set WFDEVICE-WIN-PRINTER to true.
           CALL "W$FONT" USING WFONT-GET-CLOSEST-FONT, 
                               CourierNew12B, WFONT-DATA
                               giving WFONT-STATUS.

      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if WFONT-STATUS = WFONTERR-FONT-NOT-FOUND
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if.  

      ***---
       DISTRUGGI-FONT.         
           Destroy CourierNew8.
           Destroy CourierNew10.
           Destroy CourierNew16B.
           Destroy CourierNew18B.
           Destroy Font-Copia-TimesNewRoman30.

      ***---
       MESSAGGIO-ERR-FONT.
      * ISACCO (MESSAGGIO DI ERRORE ED USCITA SE NON TROVA UN FONT)
           initialize messaggio.

           inspect WFONT-NAME replacing trailing SPACE by LOW-VALUE.
           move WFONT-SIZE    to FONT-SIZE-DPLY.

           string  "Font: "         delimited size
                   WFONT-NAME       delimited LOW-VALUE
                   X"0D0A"          delimited size
                   "Dimensione: ",  delimited size 
                   FONT-SIZE-DPLY,  delimited size
                   X"0D0A"          delimited size
                   "Non installato. La stampa verrà abortita!"
                                    delimited size
              into messaggio

           inspect messaggio replacing trailing SPACE by LOW-VALUE.

           display message messaggio.


      ***---
       CICLO-RIEPILOGHI.
           move zero   to tmp-rbma-codice
           accept path-tmp-riep-vet from environment "PATH_ST"
           inspect path-tmp-riep-vet 
                                   replacing trailing space by low-value
           string path-tmp-riep-vet   delimited by low-value
                  "riepologo_vettori_"   delimited by size
                  como-data              delimited by size
                  "_"                    delimited by size
                  como-ora               delimited by size
                  into path-tmp-riep-vet
           inspect path-tmp-riep-vet 
                                   replacing trailing low-value by space
           open output tmp-riep-vet
           close tmp-riep-vet
           open i-o tmp-riep-vet

           accept path-tmp-riep-bli-mas from environment "PATH_ST"
           inspect path-tmp-riep-bli-mas
                                   replacing trailing space by low-value
           string path-tmp-riep-bli-mas        delimited by low-value
                  "riepologo_blister_master_"  delimited by size
                  como-data                    delimited by size
                  "_"                          delimited by size
                  como-ora                     delimited by size
                  into path-tmp-riep-bli-mas
           inspect path-tmp-riep-bli-mas
                                   replacing trailing low-value by space
           open output tmp-riep-bli-mas
           close tmp-riep-bli-mas
           open i-o tmp-riep-bli-mas

           accept path-tmp-riep-bli-man from environment "PATH_ST"
           inspect path-tmp-riep-bli-man
                                   replacing trailing space by low-value
           string path-tmp-riep-bli-man        delimited by low-value
                  "riepologo_blister_manuali_" delimited by size
                  como-data                    delimited by size
                  "_"                          delimited by size
                  como-ora                     delimited by size
                  into path-tmp-riep-bli-man
           inspect path-tmp-riep-bli-man
                                   replacing trailing low-value by space
           open output tmp-riep-bli-man
           close tmp-riep-bli-man
           open i-o tmp-riep-bli-man

           set  tutto-ok      to true.

           move stordc-da-anno to tor-anno.
           move stordc-da-num  to tor-numero.

           start tordini key >= tor-chiave
                 invalid set no-dati to true
             not invalid perform SCORRI-TORDINI-RIEPILOGHI
           end-start.

      *****     perform STAMPA-TOT-VETTORI

           if not spl-sta-annu
              perform STAMPA-TOT-BLISTER
           end-if.

      *    riazzero i totalizzatori
           move zero   to tot-colli
                          tot-peso-utf
                          tot-peso-no-utf
                          tot-peso
                          peso-totale.
           close tmp-riep-vet
           delete file tmp-riep-vet.
           close tmp-riep-bli-mas
           delete file tmp-riep-bli-mas.
           close tmp-riep-bli-man
           delete file tmp-riep-bli-man.
      
      ***---
       STAMPA-TOT-BLISTER.
           move 99  to spl-riga
           move low-value to tmp-rbm-codice
           start tmp-riep-bli-mas key not < tmp-rbm-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tmp-riep-bli-mas next
                      at end exit perform
                    end-read
                    perform SCRIVI-RIEP-BLISTER-MAS
                    if spl-sta-annu
                       exit perform
                    end-if
                 end-perform
           end-start.

           move low-value to tmp-rbma-chiave
           start tmp-riep-bli-man key not < tmp-rbma-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tmp-riep-bli-man next
                      at end exit perform
                    end-read
                    perform SCRIVI-RIEP-BLISTER-MAN
                    if spl-sta-annu
                       exit perform
                    end-if
                 end-perform
           end-start.

      ***---
       SCRIVI-RIEP-BLISTER-MAS. 
           if spl-riga > max-righe-corpo 
              perform STAMPA-INTESTAZIONE-BLI
           end-if.

           move tmp-rbm-codice  to bli-codice
           read blister no lock
                invalid continue
           end-read.

           move tmp-rbm-codice  to r-cod-bli-mas.
           move bli-descrizione to r-descr-bli-mas.
           move tmp-rbm-qta     to r-qta-bli-man.
           
           |Prima riga blister in grassetto
           add 0,6              to spl-riga.
           move riga-bli-mas    to spl-riga-stampa.
           move CourierNew12B   to spl-hfont.
           call "spooler"       using spooler-link.
           move CourierNew10    to spl-hfont.
           add 0,2              to spl-riga.

           perform varying cont from 1 by 1 until cont > 50
              if bli-el-articolo(cont) = zero
                 exit perform
              end-if
              if spl-riga > max-righe-corpo 
                 perform STAMPA-INTESTAZIONE-BLI
              end-if

              move bli-el-articolo(cont) to r-cod-bli-mas-el
                                            art-codice
              read articoli no lock
                   invalid continue
              end-read
              move art-descrizione    to r-descr-bli-mas-el
              move bli-el-qta(cont)   to r-qta-bli-man-el   
              move riga-bli-mas-el    to spl-riga-stampa
              add 0,4                    to spl-riga
              call "spooler"       using spooler-link
           end-perform.

      ***---
       SCRIVI-RIEP-BLISTER-MAN. 
           if spl-riga > max-righe-corpo 
              perform STAMPA-INTESTAZIONE-BLI
           end-if.

           if tmp-rbma-prog = 1 |sono sul primo elemento del blister
              move CourierNew10B to spl-hfont
              add 0,8            to spl-riga
              move "MANUALE"     to spl-riga-stampa
              call "spooler"  using spooler-link
              move CourierNew10  to spl-hfont
           end-if

           add 0,4                 to spl-riga
           move tmp-rbma-art       to r-cod-bli-mas-el
           move tmp-rbma-art-descr to r-descr-bli-mas-el
           move tmp-rbma-qta       to r-qta-bli-man-el   
           move riga-bli-mas-el    to spl-riga-stampa
           call "spooler"       using spooler-link.
      
      ***---
       STAMPA-INTESTAZIONE-BLI.
           if primo-passaggio
              perform APRI-STAMPA

              if spl-sta-annu                 
                 exit paragraph            
              end-if

              set primo-passaggio to false
           else
              perform SALTA-PAGINA
           end-if.

           set  spl-stringa     to true.
           move CourierNew12B   to spl-hfont.

           move stordc-da-num   to da-ed
           move stordc-a-num    to a-ed

           initialize spl-riga-stampa
           string "BOZZE DALLA "   delimited by size
                  da-ed            delimited by size
                  " ALLA "         delimited by size
                  a-ed             delimited by size
                  into spl-riga-stampa
           call "C$JUSTIFY" using spl-riga-stampa(1:80), "C"
                  
           move 0,3                to spl-riga.
           move 0,1                to spl-colonna.
           call "spooler"       using spooler-link.


           add 0,5 to spl-riga
           initialize spl-riga-stampa

      *     add 1 to spl-riga
           string "stampato il "   delimited by size
                  como-data(7:2)   delimited by size
                  "/"              delimited by size
                  como-data(5:2)   delimited by size
                  "/"              delimited by size
                  como-data(1:4)   delimited by size
                  " alle "         delimited by size
                  como-ora(1:2)    delimited by size
                  ":"              delimited by size
                  como-ora(3:2)    delimited by size
                  into spl-riga-stampa
           call "C$JUSTIFY" using spl-riga-stampa(1:80), "C"
           call "spooler"       using spooler-link.


           move "RIEPILOGO BLISTER:" to spl-riga-stampa
           move CourierNew16B         to spl-hfont.
           add 0,7 to spl-riga
           call "C$JUSTIFY" using spl-riga-stampa(1:61), "C"
           call "spooler"       using spooler-link.
           
           move 1                   to spl-colonna.     
           add 0,8 to spl-riga
           move CourierNew10B      to spl-hfont.
           move riga-bli-mas-int   to spl-riga-stampa
           call "spooler"       using spooler-link.


      * STAMPA RIGA TRATTINI
           add 0,4 to spl-riga
           move 1                   to spl-colonna.     
           move riga-fill           to spl-riga-stampa.
           call "spooler"        using spooler-link.

      ***---
       STAMPA-TOT-VETTORI.
           move 99  to spl-riga
           move low-value to tmp-rev-codice
           start TMP-RIEP-VET key not < tmp-rev-chiave
              invalid
                 continue
              not invalid
                 perform until 1 = 2
                    read tmp-riep-vet next
                       at end
                          exit perform
                    end-read
                    perform SCRIVI-RIEP-VET
                    if spl-sta-annu
                       exit perform
                    end-if
                 end-perform
           end-start.

      ***---
       SCRIVI-RIEP-VET.
           if spl-riga > max-righe-corpo 
      *        move 1 to spl-riga, spl-colonna
      *        if not primo-passaggio
      *           perform STAMPA-PIEDE
      *        end-if
              perform STAMPA-INTESTAZIONE-VET
           end-if.

           if not spl-sta-annu
              add 0,4                    to spl-riga
              move tmp-rev-codice        to r-cod-vet
              move tmp-rev-descrizione   to r-descr-vet
              move tmp-rev-tot-peso      to r-peso-vet

              move riga-vettore      to spl-riga-stampa
              call "spooler"        using spooler-link
           end-if.

      ***---
       SCORRI-TORDINI-RIEPILOGHI.
           perform until 1 = 2
              read tordini next at end exit perform end-read

              if tor-anno > stordc-a-anno
                 exit perform
              end-if

              if tor-numero > stordc-a-num
                 exit perform
              end-if

              perform VALIDA-ORDINE

              if tutto-ok
                 perform SCORRI-ORDINE-RIEPILOGO
              end-if
           end-perform.           

      ***---
       SCORRI-ORDINE-RIEPILOGO.
           move zero   to tot-colli
                          tot-peso-utf
                          tot-peso-no-utf
                          tot-peso
                          peso-totale.

           move tor-anno     to ror-anno.
           move tor-numero   to ror-num-ordine.
           move low-values   to ror-num-riga.             
      
           start rordini key >= ror-chiave
              invalid 
                 continue
              not invalid
                 perform until 1 = 2
                    read rordini next 
                       at end
                          exit perform
                          exit perform
                    end-read
                    if  ror-anno        not = tor-anno  or
                        ror-num-ordine  not = tor-numero
                        exit perform
                    end-if                                

                    perform CALCOLA-TOTALI-PIEDE
                    if ror-si-blister
                       perform VAL-TMP-BLISTER
       
                    end-if
                 end-perform
           end-start

           move tor-vettore  to tmp-rev-codice
           read tmp-riep-vet
              invalid
                 if tor-vettore = zero
                    move "Ritiro in Lubex" to tmp-rev-descrizione
                 else
                    move tor-vettore  to vet-codice
                    read tvettori
                       invalid
                          move "vettore" to tmp-rev-descrizione
                       not invalid
                          move vet-descrizione to tmp-rev-descrizione
                    end-read
                 end-if
                 move zero   to tmp-rev-tot-colli
                                tmp-rev-tot-peso
                                tmp-rev-tot-peso-UTF
                                tmp-rev-tot-peso-non-UTF
           end-read

           add tot-colli        to tmp-rev-tot-colli
           add tot-peso-utf     to tmp-rev-tot-peso-UTF
           add tot-peso-no-utf  to tmp-rev-tot-peso-non-UTF
           add tot-peso         to tmp-rev-tot-peso.

           rewrite TMP-REV-REC
              invalid
                 write TMP-REV-REC
           end-rewrite.

      ***---
       STAMPA-INTESTAZIONE-VET.
           if primo-passaggio
              perform APRI-STAMPA

              if spl-sta-annu                 
                 exit paragraph            
              end-if

              set primo-passaggio to false
           else
              perform SALTA-PAGINA
           end-if.


           set  spl-stringa        to true.
           move CourierNew12B   to spl-hfont.

           move stordc-da-num   to da-ed
           move stordc-a-num    to a-ed

           initialize spl-riga-stampa
           string "BOZZE DALLA "   delimited by size
                  da-ed            delimited by size
                  " ALLA "         delimited by size
                  a-ed             delimited by size
                  into spl-riga-stampa
           call "C$JUSTIFY" using spl-riga-stampa(1:80), "C"
                  
           move 0,3                to spl-riga.
           move 0,1                to spl-colonna.
           call "spooler"       using spooler-link.


           add 0,5 to spl-riga
           initialize spl-riga-stampa

      *     add 1 to spl-riga
           string "stampato il "   delimited by size
                  como-data(7:2)   delimited by size
                  "/"              delimited by size
                  como-data(5:2)   delimited by size
                  "/"              delimited by size
                  como-data(1:4)   delimited by size
                  " alle "         delimited by size
                  como-ora(1:2)    delimited by size
                  ":"              delimited by size
                  como-ora(3:2)    delimited by size
                  into spl-riga-stampa
           call "C$JUSTIFY" using spl-riga-stampa(1:80), "C"
           call "spooler"       using spooler-link.


           move "RIEPILOGO CORRIERI:" to spl-riga-stampa
           move CourierNew16B         to spl-hfont.
           add 0,7 to spl-riga
           call "C$JUSTIFY" using spl-riga-stampa(1:61), "C"
           call "spooler"       using spooler-link.

           move 1                   to spl-colonna.     
           add 0,8 to spl-riga
           move CourierNew10       to spl-hfont.
           move riga-vettore-int   to spl-riga-stampa
           call "spooler"       using spooler-link.


      * STAMPA RIGA TRATTINI
           add 0,4 to spl-riga
           move 1                   to spl-colonna.     
           move riga-fill           to spl-riga-stampa.
           call "spooler"        using spooler-link.

      ***---
       VAL-TMP-BLISTER.
      *     ror-si-blister = riga di un blister
           if ror-bli-codice = zero
              perform VAL-TMP-BLISTER-MAN
           else
              perform VAL-TMP-BLISTER-MASTER
           end-if.

      ***---
       VAL-TMP-BLISTER-MASTER.
      *    vado a leggere gli ordini solo per le prime righe del blister
           if ror-qta-imballi not = zero
              move ror-bli-codice  to tmp-rbm-codice
              read tmp-riep-bli-mas
                 invalid


                    move zero   to tmp-rbm-qta
              end-read
              add ror-qta-imballi to tmp-rbm-qta
              rewrite tmp-rbm-rec
                 invalid
                    write tmp-rbm-rec
              end-rewrite
           end-if.

      ***---
       VAL-TMP-BLISTER-MAN.
      *    sono sulla prima riga di un blister
           if ror-qta-imballi not = zero
              add 1       to tmp-rbma-codice
              move zero   to tmp-rbma-prog
           end-if.
           add 1       to tmp-rbma-prog
           move ror-cod-articolo   to tmp-rbma-art
                                      art-codice
           read articoli no lock
              invalid
                 initialize art-descrizione
           end-read
           move art-descrizione to tmp-rbma-art-descr
           move ror-qta            to tmp-rbma-qta

           write tmp-rbma-rec
              invalid
                 continue
           end-write.

      ***---
       RIGA-MASTER-RIFERIMENTO.
           if CallingPgm = "evacli"
              move low-value to tmp-om-chiave
              start tmp-ord-mast key >= tmp-om-chiave
                    invalid continue
                not invalid          
                    move low-value to r-master
                    perform until 1 = 2
                       read tmp-ord-mast next
                         at end exit perform
                       end-read
                       move tmp-om-numero-master to numero-x
                       inspect numero-x replacing leading x"30" by x"20"
                       call "C$JUSTIFY" using numero-x, "L"
                       inspect numero-x replacing trailing spaces 
                                                  by low-value
                       string r-master            delimited low-value
                              tmp-om-anno-master  delimited size
                              "-"                 delimited size
                              numero-x            delimited low-value
                              " SALDI:"           delimited size
                              tmp-om-promo        delimited space
                              tmp-om-banco        delimited space
                              "| "                delimited size
                              into r-master
                       end-string
                       inspect r-master 
                               replacing trailing spaces by low-value
                    end-perform
              end-start
           else                       
              if tor-chiave not = save-chiave                             
                 move tor-chiave to save-chiave
                 perform varying idx from 1 by 1
                           until idx > 50
                    initialize el-master(idx)
                 end-perform
                 move 0 to idx-tot-master
                 move low-value  to ror-rec
                 move tor-chiave to ror-chiave
                 start rordini key is >= ror-chiave
                       invalid continue
                 end-start
                 move 0 to idx-master
                 perform until 1 = 2
                    read rordini next at end exit perform end-read
         
                    if tor-anno   not = ror-anno   or
                       tor-numero not = ror-num-ordine
                       exit perform
                    end-if
         
                    if ror-anno-master not = 0      
                       set idx-master to 1
                       search el-master
                       at end add 1 to idx-tot-master
                       when el-chiave(idx-master) = 
                            ror-chiave-ordine-testa
                            continue
                       end-search
                       move ror-chiave-ordine-testa 
                         to el-master(idx-tot-master) mto-chiave
                       read mtordini no lock
                            invalid continue
                        not invalid
                            if mto-saldi-promo-si
                               move "P" 
                                 to el-saldi-promo(idx-tot-master)
                            else                                  
                               move " " 
                                 to el-saldi-promo(idx-tot-master)
                            end-if
                            if mto-saldi-banco-si
                               move "B" 
                                 to el-saldi-banco(idx-tot-master)
                            else
                               move " " 
                                 to el-saldi-banco(idx-tot-master)
                            end-if
                       end-read     
                    end-if
         
                 end-perform       
                 if idx-tot-master not = 0
                    move low-value to r-master
                    perform varying idx-master from 1 by 1 
                              until idx-master > idx-tot-master
                       move el-numero-m(idx-master) to numero-x
                       inspect numero-x replacing leading x"30" by x"20"
                       call "C$JUSTIFY" using numero-x, "L"
                       inspect numero-x replacing trailing spaces 
                                        by low-value
                       string r-master              delimited low-value
                              el-anno-m(idx-master) delimited size
                              "-"                   delimited size
                              numero-x              delimited low-value
                              " SALDI:"             delimited size
                              el-saldi-promo(idx-master) delimited space
                              el-saldi-banco(idx-master) delimited space
                              "| "                  delimited size
                              into r-master
                       end-string
                       inspect r-master 
                               replacing trailing spaces by low-value
              
                    end-perform
                    inspect r-master replacing trailing 
                                     low-value by spaces
                 end-if
              end-if
           end-if.

      ***---
       AGGIUNGI-IVA.
           move 0 to idx-iva.
           perform 3 times
              add 1 to idx-iva
              if ror-cod-iva = el-iva(idx-iva)
                 exit perform
              end-if
              if el-iva(idx-iva) = spaces

                 move "IV"        to tbliv-codice1
                 move ror-cod-iva to tbliv-codice2
                 read tivaese 
                 move tbliv-percentuale to el-aliq(idx-iva)
                 move ror-cod-iva to el-iva(idx-iva)

                 exit perform
              end-if
           end-perform.

           compute el-imponib(idx-iva) = 
                   el-imponib(idx-iva) +
                (( ror-imponib-merce   + 
                   ror-add-piombo      + 
                   ror-imp-consumo     + 
                   ror-imp-cou-cobat ) * ror-qta ).

      ***---
       CALCOLA-IVA.
           move 0 to totale.
           move 0 to idx-iva.
           perform 3 times
              add 1 to idx-iva
              move 0 to como-iva
              if el-iva(idx-iva) = spaces
                 exit perform
              end-if
              if el-aliq(idx-iva)    not = 0 and
                 el-imponib(idx-iva) not = 0
                 compute como-iva = 
                   ( el-imponib(idx-iva) * el-aliq(idx-iva) / 100 )
                 add 0,005          to como-iva
                 move como-iva      to como-iva-2dec
              end-if
              compute totale = totale + como-iva + el-imponib(idx-iva)
           end-perform.
           move totale to r-tot-iva.
