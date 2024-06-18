       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      st-ordforn.
       AUTHOR.                          Luciano.
       REMARKS. Stampa l'ordine appena dopo ch'esso viene salvato.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "clienti.sl".
           copy "destinif.sl".
           copy "destini.sl".
           copy "tordforn.sl". 
           copy "rordforn.sl".
           copy "nordforn.sl".
           copy "articoli.sl".
           copy "art-ordforn.sl".
           copy "timballi.sl".
           copy "timbalqta.sl".
           copy "progmag.sl".
           copy "rlistini.sl".
           copy "tcodpag.sl".
           copy "tivaese.sl".
           copy "tmagaz.sl".
           copy "tcaumag.sl".
           copy "impforn.sl".
           copy "user.sl".
           copy "tpromo.sl".
           copy "tgrupgdo.sl".
           copy "tparamge.sl".
           copy "lineseq.sl". 
           copy "tlistini.sl".

       SELECT fileCSV
           ASSIGN       TO path-fileCSV
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE  IS SEQUENTIAL
           FILE STATUS  IS STATUS-fileCSV.

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "clienti.fd".
           copy "destinif.fd".
           copy "destini.fd".
           copy "tordforn.fd".
           copy "rordforn.fd".
           copy "nordforn.fd".
           copy "articoli.fd".
           copy "art-ordforn.fd".
           copy "timballi.fd".
           copy "timbalqta.fd".
           copy "progmag.fd".
           copy "rlistini.fd".
           copy "tcodpag.fd".
           copy "tivaese.fd".
           copy "tmagaz.fd".
           copy "tcaumag.fd".
           copy "impforn.fd".
           copy "user.fd".
           copy "tpromo.fd".
           copy "tgrupgdo.fd".
           copy "tparamge.fd".
           copy "lineseq.fd".
           copy "tlistini.fd".

       FD  fileCSV.
       01 csv-riga        PIC  x(1000).

       WORKING-STORAGE SECTION.
           COPY "acucobol.def".
           copy "fonts.def".
           copy "acugui.def".
           copy "spooler.def".
           copy "selprint.lks".            
           copy "link-settaPDF.def".
           copy "link-readutente.def".     
   
       01  r-stof-riga.
           05 r-stof-art          pic z(6).
           05 filler pic x   value space.
           05 r-stof-des              pic x(40).         
           05 R-STOF-COD-FORN         pic x(20).
           05 r-stof-qta              pic x(7).
      *     05 r-stof-qta-eva          pic x(7).
           05 r-stof-imb              pic x(22).
      *     05 r-stof-cod-dog          pic x(8).
           05 r-stof-peso-netto       pic x(12).
           05 r-stof-peso-tot-netto   pic x(12).
           05 r-stof-peso-tot-utf     pic x(12).
           05 r-stof-prz-listino.|      pic x(13).|
              10 r-stof-prz-list   PIC  x(20).|
              10 r-stof-prz-list-e PIC  x.|
           05 r-stof-sconto-1-ed.
              10 r-stof-sconto-1      pic x(6).
              10 r-stof-perce-1       pic x.
           05 r-stof-sconto-2-ed.
              10 r-stof-sconto-2      pic x(6).
              10 r-stof-perce-2       pic x.
           05 r-stof-sconto-3-ed.
              10 r-stof-sconto-3      pic x(6).
              10 r-stof-perce-3       pic x.
           05 r-stof-sconto-4-ed.
              10 r-stof-sconto-4      pic x(6).
              10 r-stof-perce-4       pic x.
           05 r-stof-sconto-5-ed.
              10 r-stof-sconto-5      pic x(6).
              10 r-stof-perce-5       pic x.
           05 r-stof-prz-netto.|        pic x(13).|
              10 r-stof-prz-net   PIC  x(20).|
              10 r-stof-prz-net-e PIC  x.|

           05 r-stof-imp-consumo.|      pic x(12).|
              10 r-stof-imp-cons   PIC  x(20).|
              10 r-stof-imp-cons-e PIC  x.|
           05 r-stof-imp-cou-cobat.    |PIC  x(11).|
              10 r-stof-imp-cou   PIC  x(20).|
              10 r-stof-imp-cou-e PIC  x.|
           05 r-stof-add-piombo.|       PIC  x(11).|
              10 r-stof-add-pb        PIC  x(20).|
              10 r-stof-add-pb-e      PIC  x.|
           05 r-stof-costi-aggi.|       PIC  x(13).|
              10 r-stof-costi-agg   PIC  x(20).|
              10 r-stof-costi-aggi-e PIC  x.|
           05 r-stof-prz-finale.|       PIC  x(13).|
              10 r-stof-prz-fin   PIC  x(20).|
              10 r-stof-prz-fin-e PIC  x.|
           05 r-stof-prz-tot-finale.
              10 r-stof-prz-tot-fin   PIC  x(20).|
              10 r-stof-prz-tot-fin-e PIC  x.|

       01  r-stof-riga-testa.
           05 rt-stof-art             pic X(30).
           05 rt-stof-des             pic X(30).
           05 Rt-STOF-COD-FORN        pic X(30).
           05 rt-stof-qta             pic X(30).
           05 rt-stof-imb             pic X(30).
           05 rt-stof-peso-netto      pic X(30).
           05 rt-stof-peso-tot-netto  pic X(30).
           05 rt-stof-peso-tot-utf    pic X(30).
           05 rt-stof-prz-listino     pic X(30).
           05 rt-stof-sconto-1        pic X(30).
           05 rt-stof-sconto-2        pic X(30).
           05 rt-stof-sconto-3        pic X(30).
           05 rt-stof-sconto-4        pic X(30).
           05 rt-stof-sconto-5        pic X(30).
           05 rt-stof-prz-netto       pic X(30).
           05 rt-stof-imp-consumo     pic X(30).
           05 rt-stof-imp-cou-cobat   pic X(30).
           05 rt-stof-add-piombo      pic X(30).
           05 rt-stof-costi-aggi      pic X(30).
           05 rt-stof-prz-finale      pic X(30).
           05 rt-stof-prz-tot-finale  pic X(30).

       77  como-path   pic x(2000).
       77  como-idx    pic 9(5).
       77  como-chars  pic 9(5).
       77  como-num-x  pic x(8).
       77  como-num-n  pic 9(8).
       77  como-pic-z5 pic z(5).
       77  como-pic-z7 pic z(7).
       77  como-pic-z8 pic z(8).
       77  como-pic-96v93 pic ----.--9,999.

       77  como-pic-96v92 pic ----.--9,99.
       77  como-pic-96v94 pic ----.--9,9999.

       77  como-pic-97v92 pic --.---.--9,99.
       77  como-pic-97v94 pic --.---.--9,9999.

       77  como-pic-93v92 pic zzz,zz.

       77  como-pic-94v92 pic --.--9,99.
       77  como-pic-94v94 pic --.--9,9999.   
       77 numRighe                 pic 9(2) value 0.
       77 resto                    pic 9(2) value 0.
       77 totChar                  pic 9(3) value 0. 
       77 startWord                pic 9(3).
       77 iWord                    pic 9(3).
       77 totWord                  pic 9(3).
       77 idxChar                  pic 9(3).
       77 rowToWrite               pic x(60).
       77 sumChar                  pic 9(3).
       77 numChar                  pic 9(3).
       01 tab-word.
          03                       occurs 999.
             05 el-wordValue       pic x(60).
             05 el-wordEnd         pic 9(3).
       77 iRow                     pic 9(2).
       77 startChar                pic 9(3).
       01 tab-rows.
          03                       occurs 2.
              05 el-rowToWrite     pic x(100).

       01  filler         pic 9 value 0.
         88 testata-csv-fatta   value 1, false 0.

       77  como-dati-ordine       pic x(30).
       77  como-consegna          pic x(50).
       77  como-info              pic x(50).
       77  como-promo             pic x(50).
       77  como-scarico           pic x(50).
       77  separatore             pic x.
       77  DestFile               pic x(256).
       77  NomeFile               pic x(256).
       77  PathCreaPDF            pic x(256).
       77  como-nome-file         pic x(256).
       77  StatusCreaPDF          pic s9.
       77  num-edit               pic x(8).
       77  comando                pic x(200).
       77  parametri              pic x(200).
       77  filler                 pic 9.
           88 trovato             value 1, false 0.
       77  filler                 pic 9.
           88 ScartaSecondi       value 1, false 0.   
       77  filler                 pic 9.
           88 record-ok           value 1, false 0.   
       77  filler                 pic 9.
           88 trovato-prg-attivo  value 1, false 0.   
       01  como-time.
           05 como-hh             pic 99.
           05 como-mm             pic 99.
           05 como-ss             pic 99.
           05 como-cc             pic 99.
       77  como-ss-end            pic 9(8).

       01  filler                 pic 9 value 0.
         88 stampa-csv                  value 1, false 0.

       77  logo-handle            handle of bitmap.

       78  titolo             value "GESLUX - Stampa Ordine fornitore".
       78  MaxRighe           value 33.
       78  MaxRighe-articoli  value 40.
                                 
       77  status-tordforn       pic xx.
       77  status-rordforn       pic xx.
       77  status-nordforn       pic xx.
       77  status-articoli       pic xx.
       77  status-art-ordforn    pic xx.
       77  status-clienti        pic xx.
       77  status-destinif       pic xx.
       77  status-destini        pic xx.
       77  status-timballi       pic xx.
       77  status-timbalqta      pic xx.
       77  status-progmag        pic xx.
       77  status-rlistini       pic xx.
       77  status-tcodpag        pic xx.
       77  status-tivaese        pic xx.
       77  status-tmagaz         pic xx.
       77  status-tcaumag        pic xx.
       77  status-impforn        pic xx.
       77  status-user           pic xx.
       77  status-tpromo         pic xx. 
       77  status-tgrupgdo       pic xx. 
       77  status-tparamge       pic xx.   
       77  status-lineseq        pic xx.
       77  status-tlistini       pic xx.
       77  status-fileCSV        pic xx.    
       77  wstampa               pic x(256).
       77  path-fileCSV          pic x(256).


       77  Arial14BI             handle of font.
       77  Arial13BI             handle of font.
       77  Arial20BI             handle of font.
       77  arial5B               handle of font.
       77  arial7B               handle of font.
       77  arial5                handle of font.
       77  arial6                handle of font.
       77  arial6b               handle of font.
       77  arial4                handle of font.
       77  font-note             handle of font.
       77  font-noteB            handle of font.
       77  courier12             handle of font.

       77  messaggio             pic x(150)  value spaces.
       77  wfont-status          pic s9(5)   value zero.
       77  font-size-dply        pic z(5)    value zero.
       77  num-pagina            pic 9(3)    value zero.
       77  num-pag-ed            pic z(3)    value zero.

       77  como-peso              PIC 9(6)v9(3).
       77  como-data              pic 9(8).
       77  como-ora               pic 9(8).
       77  riga                   pic 99.
       77  pronostico             pic 99.
       77  imballi-ed             pic z(4).

       77  tot-imposta            pic 9(6)v9(4).
       77  tot-imposta-3dec       pic 9(6)v9(3).
       77  tot-imposta-2dec       pic 9(6)v9(2).

       77  tot-cou                pic 9(6)v9(4). 
       77  tot-cou-3dec           pic 9(6)v9(3). 
       77  tot-cou-2dec           pic 9(6)v9(2). 

       77  tot-piombo             pic 9(6)v9(4). 
       77  tot-piombo-3dec        pic 9(6)v9(3). 
       77  tot-piombo-2dec        pic 9(6)v9(2). 

       77  tot-peso-utf           pic 9(6)v999.
       77  tot-qta                pic 9(7).
      * 77  tot-qta-eva            pic 9(7).
       77  tot-peso               pic 9(6)v999.
       77  totale                 pic 9(7)v9(4).
       77  tot-iva                pic 9(9)v9(4).

      *
      
       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".

       77  filler pic 9.
           88  prima-volta       value 1, false 0.

       77  como-prezzo PIC  9(9)v9(4).
       77  como-prezzo-3dec PIC  9(9)v9(3).
       77  como-prezzo-2dec PIC  9(9)v9(2).

       01  tab-iva.
           05 cod-iva  pic x(3) occurs 10.
           05 imp-iva  pic 9(9)v9(4) occurs 10.

       77  idx   pic 9(3).

       77  como-iva              pic 9(9)v9(5).
       77  como-iva-4dec         pic 9(9)v9(4).
       77  como-iva-3dec         pic 9(9)v9(3).
       77  como-iva-2dec         pic 9(9)v9(2).

       77  cont                    pic 9(3).

       77  como-xx              pic x(2).

      * 77  save-rof-prz-unitario   PIC  9(9)v9(4).
       77  save-rof-imponib-merce  PIC  9(9)v9(4).

       01              pic 9.
           88 franco   value 1 false zero.

       77  cons-ragsoc    pic x(40).
       77  cons-ind       pic x(40).
       77  cons-localita  pic x(40).
       77  cons-piva      pic x(40).

       01  FILE-INFO.
           02  FILE-SIZE    PIC X(8) COMP-X.
           02  FILE-DATE    PIC 9(8) COMP-X.
           02  FILE-TIME    PIC 9(8) COMP-X.

       77  old-SIZE         PIC X(8) COMP-X.


       77  status-code       pic 9.

       01                    pic 9.
           88 TIME-OUT-EXIT  value 1 false zero.

       77  MINUTI-PARTENZA   pic 99.
       77  MINUTI-arrivo     pic 99.

       01  riga-articoli.
           05 ra-codice      pic z(6).
           05 filler         pic x(5).
           05 ra-descrizione pic x(50).

       77  z4                pic z(4).
       77  z2                pic z(2).

       LINKAGE SECTION.
           copy "link-st-ordforn.def".

      ******************************************************************
       PROCEDURE DIVISION using st-ordforn-linkage.

       DECLARATIVES.

      ***---
       TORDFORN-ERR SECTION.
           use after error procedure on tordforn.
           set tutto-ok  to true.
           evaluate status-tordforn 
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File delle testate [tordforn] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "39"
                display message "File [tordforn] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[tordforn] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           end-evaluate.  

      ***---      
       rordforn-ERR SECTION.
           use after error procedure on rordforn.
           set tutto-ok  to true.
           evaluate status-rordforn
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File delle righe [rordforn] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "39"
                display message "File [rordforn] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[rordforn] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           when "99" continue
           end-evaluate.  

      ***---      
       NORDFORN-ERR SECTION.
           use after error procedure on nordforn.
           set tutto-ok  to true.
           evaluate status-nordforn
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File delle righe [NORDFORN] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "39"
                display message "File [NORDFORN] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[NORDFORN] Indexed file corrupt!"
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
                display message box        "Impossibile procedere."
                  x"0d0a""Tabella Clienti [CLIENTI] inesistente"
                        title = titolo
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
       DESTINIF-ERR SECTION.
           use after error procedure on destinif.
           set tutto-ok  to true.
           evaluate status-destinif
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [DESTINIF] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "39"
                display message "File [DESTINIF] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[DESTINIF] Indexed file corrupt!"
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
                display message box        "Impossibile procedere."
                  x"0d0a""File [DESTINI] inesistente"
                        title = titolo
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
       ARTICOLI-ERR SECTION.
           use after error procedure on articoli.
           set tutto-ok  to true.
           evaluate status-articoli
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File articoli [ARTICOLI] inesistente"
                        title = titolo
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
       ART-ORDFORN-ERR SECTION.
           use after error procedure on art-ordforn.
           set tutto-ok  to true.
           evaluate status-art-ordforn
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File articoli [ART-ORDFORN] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "39"
                display message "File [ART-ORDFORN] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[ART-ORDFORN] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           end-evaluate.  

      ***---
       IMPFORN-ERR SECTION.
           use after error procedure on impforn.
           set tutto-ok  to true.
           evaluate status-impforn
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [IMPFORN] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "39"
                display message "File [IMPFORN] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[IMPFORN] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           end-evaluate.  

      ***---
       USER-ERR SECTION.
           use after error procedure on user.
           set tutto-ok  to true.
           evaluate status-user
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [USER] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "39"
                display message "File [USER] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[USER] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           end-evaluate.  

      ***---
       TPROMO-ERR SECTION.
           use after error procedure on tpromo.
           set tutto-ok  to true.
           evaluate status-tpromo
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [TPROMO] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "39"
                display message "File [TPROMO] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[TPROMO] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           end-evaluate.  

      ***---
       TGRUPGDO-ERR SECTION.
           use after error procedure on tgrupgdo.
           set tutto-ok  to true.
           evaluate status-tgrupgdo
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [TGRUPGDO] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "39"
                display message "File [TGRUPGDO] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[TGRUPGDO] Indexed file corrupt!"
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
                display message box        "Impossibile procedere."
                  x"0d0a""File [TPARAMGE] inesistente"
                        title = titolo
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
           if stof-pdf-csv
              set stampa-csv to true
              set stof-pdf   to true
           end-if.
           accept ru-user from environment "USER_CODI".
           call   "readutente" using ru-linkage.
           cancel "readutente".

           move 0 to riga.
           set prima-volta to true.
           set     tutto-ok     to true.
           accept como-data from century-date.
           accept como-ora  from time.
           COPY RESOURCE "logo-st.bmp".
           CALL "w$bitmap" USING WBITMAP-LOAD "logo-st.BMP", 
                   GIVING logo-handle.

      ***---
       OPEN-FILES.              
           open input tordforn  
           open i-o   rordforn  
           open input nordforn  
           open input clienti 
           open input destinif
           open input destini
           open input articoli
           open input art-ordforn
           open input timballi.
           open input timbalqta.
           open input progmag.
           open input rlistini.
           open input tcodpag.
           open input tivaese.
           open input tmagaz.
           open input tcaumag.
           open input impforn.
           open input user.
           open input tpromo.
           open input tgrupgdo.
           open input tparamge.
           open input tlistini.

           move space  to tge-codice
           read tparamge
              invalid
                 continue
           end-read.


      ****---
       ELABORAZIONE.
      *    Luciano
           if stof-si-stampa-art-no-listforn
              perform STAMPA-LISTA-ARTICOLI
           end-if.
      *    Luciano

           |NON IMPOSTO LE DATE E LO STATO (DA ALTRI PROGRAMMI CHE NO SIANO LA STAMPA "DA" "A")
           if stof-tof-data-da = 0 and
              stof-tof-data-a  = 0 
              move 99999999 to stof-tof-data-a
           end-if.
           if stof-tof-stato = spaces
              move "T" to stof-tof-stato
           end-if.
           if stof-tof-numero-a = 0
              move stof-tof-chiave to stof-tof-chiave-a
           end-if.
           move stof-tof-chiave to tof-chiave.
           start tordforn key >= tof-chiave
                 invalid set errori to true
             not invalid
                 perform until 1 = 2
                    read tordforn next at end exit perform end-read
                    if tof-anno   > stof-tof-anno-a   or
                       tof-numero > stof-tof-numero-a
                       exit perform
                    end-if
                    set record-ok to false
                    if tof-data-ordine >= stof-tof-data-da and
                       tof-data-ordine <= stof-tof-data-a
                       if stof-tof-stato not = "T"
                          if stof-tof-stato = tof-stato
                             set record-ok to true
                          end-if
                       else
                          set record-ok to true
                       end-if
                    end-if   
                    evaluate true
                    when stof-ufficio-M
                         if tof-stampante-L or tof-stampante-A
                            set record-ok to false
                         end-if
                    when stof-ufficio-L
                         if tof-stampante-M or tof-stampante-A
                            set record-ok to false
                         end-if
                    when stof-ufficio-A
                         if tof-stampante-L or tof-stampante-M
                            set record-ok to false
                         end-if
                    when other continue
                    end-evaluate
                    if record-ok

                       move tof-cod-forn to cli-codice
                                           desf-codice
                       set cli-tipo-f    to true
                       read clienti no lock 
                            invalid continue 
                       end-read
                       move tof-destino  to desf-prog
                       read destinif no lock 
                            invalid continue
                       end-read
                       move tof-causale  to tca-codice
                       read tcaumag 
                            invalid move spaces to tca-cod-magaz
                       end-read
                       move tca-cod-magaz  to mag-codice
                       read tmagaz 
                            invalid move space  to mag-descrizione
                       end-read
                    
                       if prima-volta
                          perform APRI-STAMPA
                          if spl-sta-annu
                             exit perform
                          end-if
                       end-if        

                       |Cerco il primo listino fornitore
                       move spaces to mag-indirizzo
                       move low-value  to rof-rec
                       move tof-chiave to rof-chiave
                       start rordforn key >= rof-chiave
                             invalid continue
                         not invalid
                             perform until 1 = 2
                                read rordforn next 
                                  at end exit perform 
                                end-read
                                if rof-chiave not = tof-chiave
                                   exit perform
                                end-if
                                if rof-cod-listino not = 0
                                   move rof-cod-listino to tlis-codice
                                   read tlistini no lock
                                        invalid continue
                                    not invalid
                                        move tlis-mag to mag-codice
                                        read tmagaz no lock
                                             invalid continue 
                                        end-read
                                   end-read
                                   exit perform
                                end-if
                             end-perform
                       end-start
                                        
                       if prima-volta
                          set prima-volta to false
                          perform STAMPA-TESTA
                       else
                          perform SALTO-PAGINA
                       end-if       
                       
      *                 perform STAMPA-TESTA
                       move low-value  to rof-rec
                       move tof-chiave to rof-chiave
                       start rordforn key >= rof-chiave
                             invalid continue
                         not invalid
                             move  0 to tot-qta
      *                               tot-qta-eva
                                        tot-peso
                                        tot-peso-utf
                                        totale 
                                        tot-iva
                                        tot-imposta
                                        tot-cou
                                        tot-piombo
                             initialize tab-iva
                             move zero   to riga
                             perform until 1 = 2
                                read rordforn next 
                                  at end exit perform 
                                end-read
         
                                if rof-anno   not = tof-anno   or
                                   rof-numero not = tof-numero
                                   exit perform
                                end-if
                                move rof-prg-chiave to prg-chiave
                                perform TROVA-PROGRESSIVO-ATTIVO
                                |ATTENZIONE!! Questo comando interrompe
                                |l'esecuzione del notturno, sostituire
                                |coi log in caso si verifichi lo stop
                                if not trovato-prg-attivo
                                   display message
                                   "PRG: " prg-chiave
                            X"0d0a""PROGRESSIVO ATTIVO NON TROVATO"
                                              title titolo
                                               icon 3
                                end-if
                                perform SCRIVI-RIGA
                             end-perform
                       end-start

                       if tof-da-confermare-si
                          perform ART-DA-CONF
                       end-if

                       perform SCRIVI-PIEDE
                    end-if
                    
                 end-perform
                 if not prima-volta
                    evaluate true
                    when stof-pdf                
      *****              when stof-pdf-tmp
                         if settaPDF-OK
                            perform CHIUDI-STAMPA
                            perform ASPETTA-PDF
                         end-if
                    when other
                         perform CHIUDI-STAMPA
                    end-evaluate
                 end-if
           end-start.

      ***---
       TROVA-PROGRESSIVO-ATTIVO.
           set trovato-prg-attivo to true.
           read progmag no lock invalid continue end-read.
           |Se trovo subito il progressivo attivo non devo fare nulla
           if prg-bloccato or
              prg-disattivo               
              set trovato-prg-attivo to false
              initialize prg-chiave replacing numeric data by zeroes
                                         alphanumeric data by spaces
              move rof-cod-articolo  to prg-cod-articolo
              move rof-cod-magazzino to prg-cod-magazzino
              start progmag key >= prg-chiave
                    invalid continue
                not invalid
                    perform until 1 = 2
                       read progmag next at end exit perform end-read
                       if prg-cod-articolo  not = rof-cod-articolo or
                          prg-cod-magazzino not = rof-cod-magazzino
                          exit perform
                       end-if
                       if prg-attivo             
                          set trovato-prg-attivo to true
                          |Al primo progressivo attivo che trovo
                          move prg-chiave to rof-prg-chiave
                          move rof-tipo-imballo
                            to rof-imb-ordinato
                          perform until 1 = 2
                             rewrite rof-rec
                             if status-rordforn = "00"
                                exit perform
                             end-if
                          end-perform
                          exit perform
                       end-if
                    end-perform
              end-start
           end-if.

      ***---
       SCRIVI-RIGA.
           add 1 to riga
           if riga > MaxRighe
              perform SALTO-PAGINA
              move 1   to riga
           end-if.

           move rof-cod-articolo to r-stof-art 
                                    art-codice
                                    rlis-articolo
           read articoli no lock 
              invalid 
                 move spaces to art-descrizione 
           end-read

           move rof-cod-listino to rlis-codice
           read rlistini
              invalid
                 move zero   to rlis-tipo-tratt-imposte
                 move space  to rlis-art-forn
           end-read

           move rlis-tipo-tratt-imposte  to imf-codice
           read impforn
              invalid
                 set imf-prz-reale-utf-zero    to true
                 set imf-conf-utf-no           to true
                 set imf-prz-reale-cou-zero    to true
                 set imf-conf-cou-no           to true
                 set imf-prz-reale-cobat-zero  to true 
                 set imf-conf-cobat-no         to true
                 set imf-prz-reale-pb-zero     to true
                 set imf-conf-pb-no            to true
           end-read.

           move rof-prg-chiave  to prg-chiave
           read progmag invalid continue end-read.

           subtract rof-qta-evasa from rof-qta-ord.

           add rof-qta-ord to tot-qta.

           compute tot-peso =
                   tot-peso +
                 ( rof-peso * rof-qta-ord).

           compute tot-peso-utf =
                   tot-peso-utf +
                 ( rof-peso-utf * rof-qta-ord ).

           compute tot-imposta = tot-imposta +
                                (rof-imp-consumo   * rof-qta-ord).
           compute tot-cou     = tot-cou +
                                (rof-imp-cou-cobat * rof-qta-ord).
           compute tot-piombo  = tot-piombo +
                                (rof-add-piombo    * rof-qta-ord).

           move art-descrizione to r-stof-des.
      *    personalizzaizone bosch
      *     if tof-cod-forn = 431 and rlis-art-forn not = space
      *        initialize r-stof-des
      *        inspect rlis-art-forn 
      *                          replacing trailing space by low-value
      *        string rlis-art-forn    delimited by low-value
      *               " - "            delimited by size
      *               art-descrizione  delimited by size
      *               into r-stof-des
      *     end-if

           move rof-imb-ordinato   to imq-codice.
           read timbalqta
                invalid
                move space to imq-tipo
                move zero  to imq-qta-imb
           end-read.
           move imq-tipo   to imb-codice.
           read timballi
                invalid move spaces to imb-descrizione
           end-read

           move imq-qta-imb    to imballi-ed.
           call "C$JUSTIFY" using imballi-ed, "R".
           inspect imb-descrizione 
                   replacing trailing spaces by low-value.
           initialize r-stof-imb
           string imballi-ed       delimited by low-value
                  " x "            delimited by size
                  art-udm-imballo  delimited by size
                  into r-stof-imb.
     
           move rlis-art-forn to r-stof-cod-forn.

           move space  to r-stof-qta.

           if rof-peso = zero
              move space           to r-stof-peso-netto
           else
              move rof-peso        to como-pic-96v93
              move como-pic-96v93(2:11)  to r-stof-peso-netto
           end-if.

           compute como-peso = rof-peso * rof-qta-ord.
           if como-peso = zero
              move space  to r-stof-peso-tot-netto
           else
              move como-peso       to como-pic-96v93
              move como-pic-96v93(2:11)  to r-stof-peso-tot-netto
           end-if.

           compute como-peso = rof-peso-utf * rof-qta-ord.

           if como-peso = zero
              move space           to r-stof-peso-tot-utf
           else
              move como-peso       to como-pic-96v93
              move como-pic-96v93(2:11)  to r-stof-peso-tot-utf
           end-if.

           if rof-prz-unitario = zero
              move space  to r-stof-prz-listino
           else
              move rof-prz-unitario(12:2)  to como-xx
              if como-xx not = "00"
                 move rof-prz-unitario   to como-pic-96v94
                 move como-pic-96v94(2:) to r-stof-prz-list
              else
                 move rof-prz-unitario   to como-pic-96v92
                 move como-pic-96v92     to r-stof-prz-list
              end-if
              call "C$JUSTIFY" using r-stof-prz-list, "R"
              move "€"                   to r-stof-prz-list-e
           end-if.

           if rof-sconto-1 = zero 
              move space           to r-stof-perce-1
                                      r-stof-sconto-1
           else
              move rof-sconto-1    to como-pic-93v92
              move como-pic-93v92  to r-stof-sconto-1
              move "%"             to r-stof-perce-1
           end-if.

           if rof-sconto-2 = zero 
              move space           to r-stof-perce-2
                                      r-stof-sconto-2
           else
              move rof-sconto-2    to como-pic-93v92
              move como-pic-93v92  to r-stof-sconto-2
              move "%"             to r-stof-perce-2
           end-if.

           if rof-sconto-3 = zero 
              move space           to r-stof-perce-3
                                      r-stof-sconto-3
           else
              move rof-sconto-3    to como-pic-93v92
              move como-pic-93v92  to r-stof-sconto-3
              move "%"             to r-stof-perce-3
           end-if.

           if rof-sconto-4 = zero 
              move space           to r-stof-perce-4
                                      r-stof-sconto-4
           else
              move rof-sconto-4    to como-pic-93v92
              move como-pic-93v92  to r-stof-sconto-4
              move "%"             to r-stof-perce-4
           end-if.

           if rof-sconto-5 = 0
              move space           to r-stof-perce-5
                                      r-stof-sconto-5
           else
              move rof-sconto-5    to como-pic-93v92
              move como-pic-93v92  to r-stof-sconto-5
              move "%"             to r-stof-perce-5
           end-if.

      *    patch non devo più user l'imponibile
           move rof-imponib-merce     to save-rof-imponib-merce.
           subtract rof-costi-aggiuntivi from rof-imponib-merce.

           if rof-imponib-merce = 0
      *        move space  to r-stof-prz-netto
              move "OMAGGIO"          to r-stof-prz-netto
           else
              move rof-imponib-merce(12:2)  to como-xx
              if como-xx not = "00"
                 move rof-imponib-merce  to como-pic-96v94
                 move como-pic-96v94(2:) to r-stof-prz-net
              else
                 move rof-imponib-merce  to como-pic-96v92
                 move como-pic-96v92     to r-stof-prz-net
              end-if
              call "C$JUSTIFY" using r-stof-prz-net, "R"
              move "€"                   to r-stof-prz-net-e

           end-if.

           move save-rof-imponib-merce  to rof-imponib-merce.

      *    
           if rof-imp-consumo = 0
              move space              to r-stof-imp-consumo
           else
              move rof-imp-consumo(7:2)  to como-xx
              if como-xx not = "00"
                 move rof-imp-consumo    to como-pic-94v94
                 move como-pic-94v94     to r-stof-imp-cons
              else
                 move rof-imp-consumo    to como-pic-94v92
                 move como-pic-94v92     to r-stof-imp-cons
              end-if
              call "C$JUSTIFY" using r-stof-imp-cons, "R"
              move "€"                to r-stof-imp-cons-e
           end-if.

           if rof-imp-cou-cobat = zero
              move space  to r-stof-imp-cou-cobat
           else
              move rof-imp-cou-cobat(7:2)  to como-xx
              if como-xx not = "00"
                 move rof-imp-cou-cobat  to como-pic-94v94
                 move como-pic-94v94     to r-stof-imp-cou
              else
                 move rof-imp-cou-cobat  to como-pic-94v92
                 move como-pic-94v92     to r-stof-imp-cou
              end-if
              call "C$JUSTIFY" using r-stof-imp-cou, "R"
              move "€"                   to r-stof-imp-cou-e
           end-if.

           if rof-add-piombo = zero
              move space           to r-stof-add-piombo
           else
              move rof-add-piombo(7:2)  to como-xx
              if como-xx not = "00"
                 move rof-add-piombo  to como-pic-94v94
                 move como-pic-94v94     to r-stof-add-pb
              else
                 move rof-add-piombo  to como-pic-94v92
                 move como-pic-94v92     to r-stof-add-pb
              end-if
              call "C$JUSTIFY" using r-stof-add-pb, "R"
              move "€"                   to r-stof-add-pb-e
           end-if.

           compute como-prezzo = rof-imponib-merce + 
                                 rof-imp-consumo.

           if rof-costi-aggiuntivi = 0
              move space to r-stof-costi-aggi
           else
              move rof-costi-aggiuntivi(11:2)  to como-xx
              if como-xx not = "00"
                 move rof-costi-aggiuntivi  to como-pic-96v94
                 move como-pic-96v94     to r-stof-costi-agg
              else
                 move rof-costi-aggiuntivi  to como-pic-96v92
                 move como-pic-96v92     to r-stof-costi-agg
              end-if
              call "C$JUSTIFY" using r-stof-costi-agg, "R"
              move "€"                   to r-stof-costi-aggi-e
           end-if.

           compute como-prezzo = rof-imponib-merce +  
                                 rof-imp-consumo   + 
                                 rof-imp-cou-cobat + 
                                 rof-add-piombo.
      
           if como-prezzo = zero
              move space           to r-stof-prz-finale
           else
              move space           to r-stof-prz-finale
              move como-prezzo(12:2)  to como-xx
              if como-xx not = "00"
                 move como-prezzo  to como-pic-96v94
                 move como-pic-96v94     to r-stof-prz-fin
              else
                 move como-prezzo  to como-pic-96v92
                 move como-pic-96v92     to r-stof-prz-fin
              end-if
              call "C$JUSTIFY" using r-stof-prz-fin, "R"
              move "€"                   to r-stof-prz-fin-e
           end-if.
      
           compute como-prezzo = como-prezzo * rof-qta-ord
           if como-prezzo = zero
              move space           to r-stof-prz-tot-finale
           else
              move space  to r-stof-prz-tot-fin
                             r-stof-prz-tot-fin-e
      *        move como-prezzo(12:2)  to como-xx
      *        if como-xx not = "00"
      *           move como-prezzo  to como-pic-97v94
      *           move como-pic-97v94     to r-stof-prz-tot-fin
      *        else

                 add 0,0005              to como-prezzo
                 move como-prezzo        to como-prezzo-3dec
                 add 0,005               to como-prezzo-3dec
                 move como-prezzo-3dec   to como-prezzo-2dec
                 move como-prezzo-2dec   to como-prezzo

                 move como-prezzo  to como-pic-97v92
                 move como-pic-97v92     to r-stof-prz-tot-fin
      *        end-if
              call "C$Justify" using r-stof-prz-tot-fin, "R"
              move "€"                   to r-stof-prz-tot-fin-e
           end-if.

           compute totale = totale + como-prezzo.

           perform varying idx from 1 by 1 until idx > 10
              if rof-cod-iva = cod-iva(idx)
                 add como-prezzo   to imp-iva(idx)
                 exit perform
              else
                 if cod-iva(idx) = space
                    move rof-cod-iva  to cod-iva(idx)
                    move como-prezzo  to imp-iva(idx)
                    exit perform
                 end-if
              end-if
           end-perform.
                        
           move rof-qta-ord  to como-pic-z7.
           move como-pic-z7  to r-stof-qta.           
           perform SCRIVI-RIGA-CSV.
           move spaces to r-stof-qta.

           move r-stof-riga to spl-riga-stampa.
           set  spl-oggetto     to true.
           set  SPL-linea       to true.
           set  spl-brush-null  to true.
           set  spl-nero        to true.

           move spl-riga-fine   to spl-riga.
           add 0,4              to spl-riga-fine.
           perform RIGHE-VERTICALI.

      *    riga orizzontale
           move spl-riga-fine   to spl-riga.
           move spl-riga        to spl-riga-fine.
           move 0,1             to spl-colonna.
           move 28,5            to spl-colonna-fine.
           call "spooler"    using spooler-link.

           set spl-stringa   to true.
           move arial6       to spl-hfont.
           move 1            to spl-tipo-colonna
           subtract 0,3    from spl-riga.
           call "spooler" using spooler-link.


           initialize r-stof-riga.
           move rof-qta-ord  to como-pic-z7.
           move como-pic-z7  to r-stof-qta.
           move r-stof-riga  to spl-riga-stampa.
           move arial6b      to spl-hfont.
           move 1            to spl-tipo-colonna.

           call "spooler" using spooler-link.   

           
           if rof-manuale-si 
              move Arial20BI to spl-hfont
              move 28,6    to spl-colonna
              subtract 0,1   from spl-riga
              move zero   to spl-tipo-colonna
              move "*"   to spl-riga-stampa
              call "spooler" using spooler-link
              add 0,1   to spl-riga
           end-if.

      ***---
       APRI-STAMPA.
           evaluate true
           when stof-normale
                evaluate true
                when ru-SO-XP
                     accept selprint-stampante from environment
                            "STAMPANTE_DIRETTA_ORDINI_FORN_XP"
                when ru-SO-VISTA
                     accept selprint-stampante from environment
                            "STAMPANTE_DIRETTA_ORDINI_FORN_V"
                when ru-SO-7
                     accept selprint-stampante from environment
                            "STAMPANTE_DIRETTA_ORDINI_FORN_7"
                end-evaluate
           when stof-pdf
      *****     when stof-pdf-tmp
                perform CREA-PDF
                if settaPDF-OK  

                   if stampa-csv             
                      accept separatore from environment "SEPARATORE"
                             on exception move "," to separatore
                         not on exception
                             if separatore = space
                                move "," to separatore
                             end-if
                      end-accept
           
                      initialize path-fileCSV
                      string settaPDF-percorso  delimited low-value
                             settaPDF-nome-file delimited low-value
                             ".csv"             delimited size
                        into path-fileCSV 
                      end-string                              
                      open output fileCSV
                   end-if

                   accept selprint-stampante 
                   from environment "STAMPANTE_DIRETTA_ORDINI_PDF"
                else
                   move spaces to selprint-stampante
                end-if               
           when stof-scegli-stampante
                if stof-selprint-stampante = space
                   call   "selprint" using selprint-linkage
                   cancel "selprint"
                else
                   move stof-selprint-linkage   to selprint-linkage
                end-if
                move selprint-linkage to stof-selprint-linkage
           when stof-anteprima  
                accept selprint-stampante 
                       from environment "STAMPANTE_ANTEPRIMA"
           when stof-ufficio-M
                evaluate true
                when ru-SO-XP
                     accept selprint-stampante from environment
                            "STAMPANTE_MASSIMO_XP"
                when ru-SO-VISTA
                     accept selprint-stampante from environment
                            "STAMPANTE_MASSIMO_V"
                when ru-SO-7
                     accept selprint-stampante from environment
                            "STAMPANTE_MASSIMO_7"
                end-evaluate
           when stof-ufficio-L
                evaluate true
                when ru-SO-XP
                     accept selprint-stampante from environment
                            "STAMPANTE_LUCA_XP"
                when ru-SO-VISTA
                     accept selprint-stampante from environment
                            "STAMPANTE_LUCA_V"
                when ru-SO-7
                     accept selprint-stampante from environment
                            "STAMPANTE_LUCA_7"
                end-evaluate
           when stof-ufficio-A
                evaluate true
                when ru-SO-XP
                     accept selprint-stampante from environment
                            "STAMPANTE_ALTRO_XP"
                when ru-SO-VISTA
                     accept selprint-stampante from environment
                            "STAMPANTE_ALTRO_V"
                when ru-SO-7
                     accept selprint-stampante from environment
                            "STAMPANTE_ALTRO_7"
                end-evaluate
                
           end-evaluate.

           if selprint-stampante not = space
              move selprint-num-copie to SPL-NUM-COPIE
              move selprint-stampante to SPL-NOME-STAMPANTE
      
              set spl-horizontal      to true
              set spl-apertura        to true
              move 1                  to spl-margine-sinistro
              move 1                  to spl-margine-destro
              move 1                  to spl-margine-inf
              move "Stampa Ordini fornitori"
                                      to spl-nome-job
              call "spooler"        using spooler-link
      
           else
              set spl-sta-annu to true
           end-if.
      
           if spl-sta-annu 
              cancel "spooler"
           else
              perform SETTA-FONT
              set spl-stringa to true
              |Mi riposiziono ad inizio foglio
              move 1            to spl-colonna
              move 1            to spl-riga
              move spaces       to spl-riga-stampa
              move arial6 to spl-hfont
              call "spooler" using spooler-link
           end-if.
      *
      ***---
       CHIUDI-STAMPA.
           set spl-chiusura to true.
           cancel "spooler".
      *
      ***---
       STAMPA-TESTA.
           add 1 to num-pagina

           move 0        to riga.
           perform FINCATURA.

           move tof-cod-forn to cli-codice
                                desf-codice
           set cli-tipo-F    to true
           read CLIENTI 
              invalid
                 move space  to cli-ragsoc-1
           end-read
           move tof-destino  to desf-prog
           read destinif no lock
              invalid
                 move space  to desf-ragsoc-1
           end-read

           set spl-stringa   to true.
           move arial6       to spl-hfont.
           move 2,35          to spl-riga.
           move 1,9          to spl-colonna.
           move desf-ragsoc-1 to spl-riga-stampa
           call "spooler" using spooler-link.

           add 0,3              to spl-riga.
           move tof-referente   to spl-riga-stampa
           call "spooler" using spooler-link.

           add 0,3           to spl-riga.
           move tof-tel-dir  to spl-riga-stampa
           call "spooler" using spooler-link.

           add 0,3           to spl-riga.
           move tof-email  to spl-riga-stampa
           call "spooler" using spooler-link.

           move 2,35          to spl-riga.
           move 12,7          to spl-colonna.
           initialize como-dati-ordine
           if tof-rivisto-si
              string tof-numero(4:5)        delimited by size
                     "-"                    delimited by size
                     tof-anno               delimited by size
                     "          *RIVISTO*"  delimited by size
                into como-dati-ordine
           else
              string tof-numero(4:5)        delimited by size
                     "-"                    delimited by size
                     tof-anno               delimited by size
                into como-dati-ordine
           end-if
           move como-dati-ordine to spl-riga-stampa
           call "spooler" using spooler-link.

           add 0,3           to spl-riga.
           initialize spl-riga-stampa
           string tof-data-ordine(7:2)  delimited by size
                  "/"                    delimited by size
                  tof-data-ordine(5:2)  delimited by size
                  "/"                    delimited by size
                  tof-data-ordine(1:4)  delimited by size
                  into spl-riga-stampa
           call "spooler" using spooler-link.

           add 0,3                    to spl-riga.

           move tof-utente-creazione  to user-cod
           read user
              invalid
                 move tof-utente-creazione  to user-name
                 initialize user-tel-diretto
           end-read
           move user-name                   to spl-riga-stampa
           if user-tel-diretto = space
              accept user-tel-diretto
                                from environment "ORDINI_FORN_TEL_DIR"
           end-if

           if spl-riga-stampa = space
              move user-tel-diretto  to spl-riga-stampa
           else
              inspect spl-riga-stampa 
                                replacing trailing space by low-value
              string spl-riga-stampa     delimited by low-value
                        " - "            delimited by size
                        user-tel-diretto delimited by size
                        into spl-riga-stampa
                 inspect spl-riga-stampa 
                                replacing trailing low-value by space
           end-if
           move spl-riga-stampa to como-info

           call "spooler" using spooler-link.

           add 0,3                    to spl-riga.

           initialize spl-riga-stampa
           if tof-promo not = zero
              move tof-promo to tpr-codice
              read tpromo
                 invalid
                    continue
                 not invalid
                     perform DESCRIZIONE-PROMO
              end-read
           end-if

           call "spooler" using spooler-link.

           move 2,35          to spl-riga.
           move 23,3          to spl-colonna.
                  
      *    decido se stampare il magazzino o i riferimenti dei clienti
           set franco  to true
           if tca-si-ord-forn and tca-cod-magaz = "EXD"
              if tof-cliente not = zero
                 set franco  to false
              end-if
           end-if

      *****     if franco
      *****        if tof-franco-part-si
      *****           move "PARTENZA"      to spl-riga-stampa
      *****        else
      *****           move mag-descrizione to spl-riga-stampa
      *****        end-if
      *****     else  
      *****        move "DESTINO CLIENTE FINALE - VEDI NOTE" 
      *****                                               to spl-riga-stampa
      *****     end-if
           if mag-indirizzo = spaces
              move spaces       to spl-riga-stampa
              call "spooler" using spooler-link
              add 0,3           to spl-riga

              move spaces       to spl-riga-stampa
              call "spooler" using spooler-link
              add 0,3           to spl-riga
           else

              perform SPLIT-RIGA
           
              move el-rowToWrite(1) to spl-riga-stampa
              call "spooler" using spooler-link
              add 0,3           to spl-riga
                                                      
              move el-rowToWrite(2) to spl-riga-stampa
              call "spooler" using spooler-link
              add 0,3           to spl-riga
           end-if.

           move tof-cod-pagamento  to tblpa-codice2
           move "PA"       to tblpa-codice1
           read tcodpag
              invalid
                 initialize tblpa-descrizione1
                            tblpa-descrizione2
           end-read
           initialize spl-riga-stampa
           inspect tblpa-descrizione1 replacing trailing 
                                         spaces by low-value.
           string  |tof-cod-pagamento  delimited by size
                   |" - "              delimited by size
                   tblpa-descrizione1 delimited by low-value
                   " "                delimited by size
                   tblpa-descrizione2 delimited by size
              into spl-riga-stampa
           call "spooler" using spooler-link.

           add 0,3                    to spl-riga.
           if tof-urgente
              move "URGENTE" to como-consegna
           else
      *        move space     to spl-riga-stampa
              initialize como-consegna
              string "TASSATIVA PER IL "    delimited size
                     tof-data-consegna(7:2) delimited size
                     "/"                    delimited size
                     tof-data-consegna(5:2) delimited size
                     "/"                    delimited size
                     tof-data-consegna(1:4) delimited size
                     into como-consegna
              end-string
           end-if
           move como-consegna to spl-riga-stampa
           call "spooler" using spooler-link.

      *     add 0,3                    to spl-riga.
      *     accept spl-riga-stampa from environment "ORDINI_FORN_SCARICO"
      *     call "spooler" using spooler-link.
      *     move spl-riga-stampa to como-scarico.

           move 18,9  to spl-riga
           move 9,9   to spl-colonna
           move arial4       to spl-hfont.
           move "LA LUBEX S.p.A. NON SI ASSUME ALCUNA RESPONSABILITA' PE
      -         "R GLI ORDINI NON ACCETTATI E FIRMATI DALLA DIREZIONE"
                    to spl-riga-stampa
           call "spooler" using spooler-link.

           move arial6       to spl-hfont.
           move 27,3  to spl-colonna
      *     move 0,6  to spl-colonna
           initialize spl-riga-stampa
           move num-pagina   to num-pag-ed
           string "PAGINA "  delimited by size
                  num-pag-ed delimited by size
                  into spl-riga-stampa
           call "spooler" using spooler-link.

      *    posizionamento per partenza righe
           move 5,1 to spl-riga-fine.

           if stampa-csv and not testata-csv-fatta               
              set testata-csv-fatta to true
              initialize csv-riga
              string "Spett.le"        delimited size
                     separatore        delimited size
                     desf-ragsoc-1     delimited size
                     separatore        delimited size
                     "Ordine n."       delimited size
                     separatore        delimited size
                     como-dati-ordine  delimited size
                     separatore        delimited size
                     "Mag. consegna"   delimited size
                     separatore        delimited size
                     mag-indirizzo     delimited size
                     separatore        delimited size
                into csv-riga
              end-string
              write csv-riga
              initialize csv-riga
              string "Signor"             delimited size
                     separatore           delimited size
                     tof-referente        delimited size
                     separatore           delimited size
                     "Data ordine"        delimited size
                     separatore           delimited size
                     tof-data-ordine(7:2) delimited size
                     "/"                  delimited size
                     tof-data-ordine(5:2) delimited size
                     "/"                  delimited size
                     tof-data-ordine(1:4) delimited size
                     separatore           delimited size
                     "Pagamento"          delimited size       
                     separatore           delimited size
                      tblpa-descrizione1  delimited low-value
                      " "                 delimited size
                      tblpa-descrizione2  delimited size
                     separatore           delimited size
                into csv-riga
              end-string
              write csv-riga
              initialize csv-riga
              string "Tel"                delimited size
                     separatore           delimited size
                     tof-tel-dir          delimited size
                     separatore           delimited size
                     "Info utente"        delimited size
                     separatore           delimited size
                     como-info            delimited size
                     separatore           delimited size
                     "Consegna"           delimited size       
                     separatore           delimited size
                     como-consegna        delimited by size
                     separatore           delimited size
                into csv-riga
              end-string
              write csv-riga
              initialize csv-riga
              string "Mail"               delimited size
                     separatore           delimited size
                     tof-email            delimited size
                     separatore           delimited size
                     "Info promo"         delimited size
                     separatore           delimited size

                     como-promo           delimited size
                     separatore           delimited size
                     "Scarico"            delimited size       
                     separatore           delimited size
                     como-scarico         delimited size
                     separatore           delimited size
                into csv-riga
              end-string
              write csv-riga
              write csv-riga from spaces

              initialize csv-riga
              string "COD LUBEX"            delimited size
                     separatore             delimited size
                     "DESCRIZIONE PRODOTTO" delimited size
                     separatore             delimited size
                     "CODICE FORNITORE"     delimited size
                     separatore             delimited size
                     "SALDO PEZZI"          delimited size
                     separatore             delimited size
                     "PZ X LT/KG"           delimited size
                     separatore             delimited size
                     "PESO NETTO"           delimited size
                     separatore             delimited size
                     "TOTALE KG (NETTI)"    delimited size
                     separatore             delimited size
                     "TOTALE KG (UTF)"      delimited size
                     separatore             delimited size
                     "LISTINO RISERVATO"    delimited size
                     separatore             delimited size
                     "SC.%"                 delimited size
                     separatore             delimited size
                     "SC.%"                 delimited size
                     separatore             delimited size
                     "SC.%"                 delimited size
                     separatore             delimited size
                     "SC.%"                 delimited size
                     separatore             delimited size
                     "SC.%"                 delimited size
                     separatore             delimited size
                     "NETTO"                delimited size
                     separatore             delimited size
                     "IMPOSTA AL PEZZO"     delimited size
                     separatore             delimited size
                     "CONTRIBUTO CONSORZIO" delimited size
                     separatore             delimited size
                     "ADD. PIOMBO"          delimited size
                     separatore             delimited size
                     "COSTI AGGIUNTIVI"     delimited size
                     separatore             delimited size
                     "TOTALE UNITARIO"      delimited size
                     separatore             delimited size
                     "IMPORTO TOTALE"       delimited size
                     separatore             delimited size
                into csv-riga
              end-string
              write csv-riga
           end-if.

      ***---
       SALTO-PAGINA.
           set spl-salto-pagina     to true.
           call "spooler"        using spooler-link.                                                            
           move arial6       to spl-hfont.
           set spl-stringa to true
           move 0      to spl-riga
           move spaces to spl-riga-stampa
           call "spooler" using spooler-link.
           perform STAMPA-TESTA
           move 0 to riga.

      ***---
       SETTA-FONT.
           set tutto-ok             to true.
           initialize wfont-data.
           move 14                  to wfont-size.
           move "Arial"             to wfont-name.
           set wfont-bold           to true.
           set wfcharset-dont-care  to true    
           set wfont-italic         to true
           set wfont-underline      to false   
           set wfont-strikeout      to false   
           set wfont-fixed-pitch    to false   
           move zero                to wfont-char-set
           set wfdevice-win-printer to true.
           CALL "W$FONT" USING wfont-get-closest-font, 
                               Arial14BI, wfont-data
                               giving wfont-status.
      
           if wfont-status = wfonterr-font-not-found
              set errori to true
              perform MESSAGGIO-ERR-FONT
           end-if.

           initialize wfont-data.
           move 13                  to wfont-size.
           move "Arial"             to wfont-name.
           set wfont-bold           to true.
           set wfcharset-dont-care  to true    
           set wfont-italic         to true
           set wfont-underline      to false   
           set wfont-strikeout      to false   
           set wfont-fixed-pitch    to false   
           move zero                to wfont-char-set
           set wfdevice-win-printer to true.
           CALL "W$FONT" USING wfont-get-closest-font, 
                               Arial13BI, wfont-data
                               giving wfont-status.
      
           if wfont-status = wfonterr-font-not-found
              set errori to true
              perform MESSAGGIO-ERR-FONT
           end-if.

           initialize wfont-data.
           move 20                  to wfont-size.
           move "Arial"             to wfont-name.
           set wfont-bold           to true.
           set wfcharset-dont-care  to true    
           set wfont-italic         to true
           set wfont-underline      to false   
           set wfont-strikeout      to false   
           set wfont-fixed-pitch    to false   
           move zero                to wfont-char-set
           set wfdevice-win-printer to true.
           CALL "W$FONT" USING wfont-get-closest-font, 
                               Arial20BI, wfont-data
                               giving wfont-status.
      
           if wfont-status = wfonterr-font-not-found
              set errori to true
              perform MESSAGGIO-ERR-FONT
           end-if.

           initialize wfont-data.
           move 5                  to wfont-size.
           move "Arial"   to wfont-name.
           set wfont-bold           to true.
           set wfcharset-dont-care  to true    
           set wfont-italic         to false   
           set wfont-underline      to false   
           set wfont-strikeout      to false   
           set wfont-fixed-pitch    to false   
           move zero                to wfont-char-set
           set wfdevice-win-printer to true.
           CALL "W$FONT" USING wfont-get-closest-font, 
                               arial5B, wfont-data
                               giving wfont-status.
      
           if wfont-status = wfonterr-font-not-found
              set errori to true
              perform MESSAGGIO-ERR-FONT
           end-if.

           initialize wfont-data.
           move 7                  to wfont-size.
           move "Arial"   to wfont-name.
           set wfont-bold           to true.
           set wfcharset-dont-care  to true    
           set wfont-italic         to false   
           set wfont-underline      to false   
           set wfont-strikeout      to false   
           set wfont-fixed-pitch    to false   
           move zero                to wfont-char-set
           set wfdevice-win-printer to true.
           CALL "W$FONT" USING wfont-get-closest-font, 
                               arial7B, wfont-data
                               giving wfont-status.
      
           if wfont-status = wfonterr-font-not-found
              set errori to true
              perform MESSAGGIO-ERR-FONT
           end-if.

           initialize wfont-data.
           move 5                  to wfont-size.
           move "Arial"   to wfont-name.
           set wfont-bold           to false.
           set wfcharset-dont-care  to true    
           set wfont-italic         to false   
           set wfont-underline      to false   
           set wfont-strikeout      to false   
           set wfont-fixed-pitch    to false   
           move zero                to wfont-char-set
           set wfdevice-win-printer to true.
           CALL "W$FONT" USING wfont-get-closest-font, 
                               arial5, wfont-data
                               giving wfont-status.
      
           if wfont-status = wfonterr-font-not-found
              set errori to true
              perform MESSAGGIO-ERR-FONT
           end-if.

           initialize wfont-data.
           move 6                  to wfont-size.
           move "Arial"   to wfont-name.
           set wfont-bold           to false.
           set wfcharset-dont-care  to true    
           set wfont-italic         to false   
           set wfont-underline      to false   
           set wfont-strikeout      to false   
           set wfont-fixed-pitch    to false   
           move zero                to wfont-char-set
           set wfdevice-win-printer to true.
           CALL "W$FONT" USING wfont-get-closest-font, 
                               arial6, wfont-data
                               giving wfont-status.
      
           if wfont-status = wfonterr-font-not-found
              set errori to true
              perform MESSAGGIO-ERR-FONT
           end-if.

           initialize wfont-data.
           move 6                  to wfont-size.
           move "Arial"   to wfont-name.
           set wfont-bold           to true.
           set wfcharset-dont-care  to true    
           set wfont-italic         to false   
           set wfont-underline      to false   
           set wfont-strikeout      to false   
           set wfont-fixed-pitch    to false   
           move zero                to wfont-char-set
           set wfdevice-win-printer to true.
           CALL "W$FONT" USING wfont-get-closest-font, 
                               arial6b, wfont-data
                               giving wfont-status.
      
           if wfont-status = wfonterr-font-not-found
              set errori to true
              perform MESSAGGIO-ERR-FONT
           end-if.

           initialize wfont-data.
           move 4                  to wfont-size.
           move "Arial"   to wfont-name.
           set wfont-bold           to false.
           set wfcharset-dont-care  to true    
           set wfont-italic         to false   
           set wfont-underline      to false   
           set wfont-strikeout      to false   
           set wfont-fixed-pitch    to false   
           move zero                to wfont-char-set
           set wfdevice-win-printer to true.
           CALL "W$FONT" USING wfont-get-closest-font, 
                               arial4, wfont-data
                               giving wfont-status.
      
           if wfont-status = wfonterr-font-not-found
              set errori to true
              perform MESSAGGIO-ERR-FONT
           end-if.


           set tutto-ok             to true.
           initialize wfont-data.
           move 6                   to wfont-size.
           move "Arial"             to wfont-name.
           set wfont-bold           to false.
           set wfcharset-dont-care  to true    
           set wfont-italic         to false
           set wfont-underline      to false   
           set wfont-strikeout      to false   
           set wfont-fixed-pitch    to false   
           move zero                to wfont-char-set
           set wfdevice-win-printer to true.
           CALL "W$FONT" USING wfont-get-closest-font, 
                               font-note, wfont-data
                               giving wfont-status.
      
           if wfont-status = wfonterr-font-not-found
              set errori to true
              perform MESSAGGIO-ERR-FONT
           end-if.

           set tutto-ok             to true.
           initialize wfont-data.
           move 6                   to wfont-size.
           move "Arial"             to wfont-name.
           set wfont-bold           to true.
           set wfcharset-dont-care  to true    
           set wfont-italic         to false
           set wfont-underline      to false   
           set wfont-strikeout      to false   
           set wfont-fixed-pitch    to false   
           move zero                to wfont-char-set
           set wfdevice-win-printer to true.
           CALL "W$FONT" USING wfont-get-closest-font, 
                               font-noteB, wfont-data
                               giving wfont-status.
      
           if wfont-status = wfonterr-font-not-found
              set errori to true
              perform MESSAGGIO-ERR-FONT
           end-if.

           set tutto-ok             to true.
           initialize wfont-data.
           move 12                  to wfont-size.
           move "courier new"       to wfont-name.
           set wfont-bold           to false.
           set wfcharset-dont-care  to true    
           set wfont-italic         to false
           set wfont-underline      to false   
           set wfont-strikeout      to false   
           set wfont-fixed-pitch    to false   
           move zero                to wfont-char-set
           set wfdevice-win-printer to true.
           CALL "W$FONT" USING wfont-get-closest-font, 
                               courier12, wfont-data
                               giving wfont-status.
      
           if wfont-status = wfonterr-font-not-found
              set errori to true
              perform MESSAGGIO-ERR-FONT
           end-if.

      *
      ***---
       DISTRUGGI-FONT.
           Destroy Arial14BI.
           Destroy Arial13BI.
           Destroy Arial20BI.
           Destroy arial5B.
           Destroy arial7B.
           Destroy arial5.
           Destroy arial6.
           Destroy arial6b.
           Destroy font-note.
           Destroy font-noteB.
           Destroy arial4.
           destroy courier12.
      *
      ***---
       MESSAGGIO-ERR-FONT.
      * ISACCO (MESSAGGIO DI ERRORE ED USCITA SE NON TROVA UN FONT)
           initialize messaggio.
      
           inspect wfont-name replacing trailing space by low-value.
           move wfont-size    to font-size-dply.
      
           string  "Font: "         delimited size
                   wfont-name       delimited low-value
                   X"0D0A"          delimited size
                   "Dimensione: ",  delimited size 
                   font-size-dply,  delimited size
                   X"0D0A"          delimited size
                   "Non installato. La stampa verrà abortita!"
                                    delimited size
              into messaggio.
      
           inspect messaggio replacing trailing SPACE by LOW-VALUE.
      
           display message messaggio.
      
      ***---
       CLOSE-FILES.
           close tordforn 
           close rordforn  
           close nordforn  
           close articoli 
           close art-ordforn
           close clienti.
           close destinif.
           close destini.
           close timballi.
           close timbalqta.
           close progmag.
           close rlistini.
           close tcodpag.
           close tivaese.
           close tmagaz.
           close tcaumag.
           close impforn.
           close user.
           close tpromo.
           close tgrupgdo.
           close tparamge.
           close tlistini.
           if stampa-csv
              close fileCSV
           end-if.

      ***---
       EXIT-PGM.
           destroy Arial14BI 
                   Arial13BI 
                   Arial20BI 
                   arial5b
                   arial5
                   arial6
                   font-note.

           destroy logo-handle.

           goback.

      ***---  
       SCRIVI.
           call "spooler" using spooler-link.

      ***---
       FINCATURA.
      *    logo 
           move logo-handle to spl-hbitmap
           set  spl-bitmap  to true
           move 2,0 to spl-riga
           move 6,5 to spl-colonna
           move 1,2 to spl-bitmap-height
           move 2,8 to spl-bitmap-width
           call "spooler" using spooler-link.

      *    dati Lubex
           perform DATI-LUBEX.

      *    riga Ordine
           set spl-stringa            to true.
           move Arial14BI             to spl-hfont.
           move 1,4                   to spl-riga.
           move 17,0                  to spl-colonna.
           move "ORDINE D'ACQUISTO"   to spl-riga-stampa
           call "spooler" using spooler-link.


           set spl-nero   to true.
           move 20                 to spl-pen-with.
           move 0,6                to spl-colonna.
           move 16,5               to spl-colonna-fine.
           move 1,8   to spl-riga 
                         spl-riga-fine.
           set  spl-oggetto        to true.
           set  spl-linea          to true.
           set  spl-pen-solid      to true.
           call "spooler"       using spooler-link.

           move 22,8                to spl-colonna.
           move 28,5               to spl-colonna-fine.
           call "spooler"       using spooler-link.

           add 2,2   to spl-riga 
           move spl-riga  to spl-riga-fine.
           move 0,6                to spl-colonna.
           move 28,5               to spl-colonna-fine.
           call "spooler"       using spooler-link.


           add 14,8       to spl-riga 
           move 28,5      to spl-colonna-fine.
           move spl-riga  to spl-riga-fine.
quii       call "spooler"       using spooler-link.

           perform RIQUARDO-FORNITORE.
           perform RIQUADRO-ORDINE.
           perform RIQUADRO-CONSEGNA.

           perform GRIGLIA-RIGHE.

      ***---
       DATI-LUBEX.
           move zero   to spl-tipo-colonna
           set spl-stringa       to true.
           move arial5B  to spl-hfont.
           move 0,1              to spl-riga.
           move 3,4              to spl-colonna.
           move "LUBEX S.p.a."  to spl-riga-stampa
           call "spooler" using spooler-link.

           set spl-stringa       to true.
           move arial5  to spl-hfont.
           add 0,2              to spl-riga.
           move "Via G. Di Vittorio, 13/15"  to spl-riga-stampa
           call "spooler" using spooler-link.

           add 0,2              to spl-riga.
           move "20090 VIMODRONE"  to spl-riga-stampa
           call "spooler" using spooler-link.

           add 0,2              to spl-riga.
           move "(MILANO - ITALY)"  to spl-riga-stampa
           call "spooler" using spooler-link.

           add 0,4              to spl-riga.
           move ">> Tel:  02 26 51 551"  to spl-riga-stampa
           call "spooler" using spooler-link.

           add 0,2              to spl-riga.
           move ">> Fax: 02 26 515 549"  to spl-riga-stampa
           call "spooler" using spooler-link.

           add 4,1  to spl-colonna
           move 0,1              to spl-riga.

           add 0,2              to spl-riga.
           move "Capitale Sociale € 2.180.000 i.v."  to spl-riga-stampa
           call "spooler" using spooler-link.
           add 0,2              to spl-riga.
           move "P.IVA:   IT00785630963"  to spl-riga-stampa
           call "spooler" using spooler-link.


           add 3,1  to spl-colonna
           move 0,1              to spl-riga.

           add 0,2              to spl-riga.
           add 0,2              to spl-riga.

           initialize spl-riga-stampa
           string "Vimodrone, " delimited by size
                  como-data(7:2)   delimited by size
                  "/"              delimited by size
                  como-data(5:2)   delimited by size
                  "/"              delimited by size
                  como-data(1:4)   delimited by size
                  " "              delimited by size
                  como-ora(1:2)    delimited by size
                  "."              delimited by size
                  como-ora(3:2)    delimited by size
                  into spl-riga-stampa
           call "spooler" using spooler-link.

      ***---
       RIQUARDO-FORNITORE.
           move 8     to spl-pen-with.
           move 2,1  to spl-riga.
           add 1,7  to spl-riga giving spl-riga-fine

           move 0,7                to spl-colonna.
           add 10,3 to spl-colonna giving spl-colonna-fine.

           set  spl-oggetto        to true.
           set  SPL-RETTANGOLO-ROUND  to true.
           set  spl-brush-null     to true.

           set  spl-nero           to true.
           call "spooler"         using spooler-link.


           set  SPL-linea to true.
           move 4         to spl-pen-with.
           add 1,1        to spl-colonna
           subtract 0,2   from spl-colonna-fine

           add 0,5        to spl-riga
           move spl-riga  to spl-riga-fine
           call "spooler"         using spooler-link.

           add 0,3        to spl-riga
           move spl-riga  to spl-riga-fine
           call "spooler"         using spooler-link.
           add 0,3        to spl-riga
           move spl-riga  to spl-riga-fine
           call "spooler"         using spooler-link.
           add 0,3        to spl-riga
           move spl-riga  to spl-riga-fine
           call "spooler"         using spooler-link.

           set spl-stringa   to true.
           move arial6       to spl-hfont.
           subtract 1,15      from spl-riga.
           subtract 0,9       from spl-colonna.
           move "Spett.le:"  to spl-riga-stampa
           call "spooler" using spooler-link.


           add 0,3           to spl-riga.
           add 0,4           to spl-colonna.
           move "Sig.:"      to spl-riga-stampa
           call "spooler" using spooler-link.

           add 0,3           to spl-riga.
           add 0,1           to spl-colonna.
           move "Tel:"       to spl-riga-stampa
           call "spooler" using spooler-link.

           add 0,3           to spl-riga.
           subtract 0,1      from spl-colonna.
           move "Mail:"      to spl-riga-stampa
           call "spooler" using spooler-link.

      ***---
       RIQUADRO-ORDINE.
           move 8     to spl-pen-with.
           move 2,1  to spl-riga.
           add 1,7  to spl-riga giving spl-riga-fine

           move 11,2                to spl-colonna.
           add 10,6 to spl-colonna giving spl-colonna-fine.

           set  spl-oggetto        to true.
           set  SPL-RETTANGOLO-ROUND  to true.
           set  spl-brush-null     to true.
           set  spl-nero           to true.
           call "spooler"         using spooler-link.

           set  SPL-linea to true.
           move 4         to spl-pen-with.
           add 1,5        to spl-colonna
           subtract 0,2   from spl-colonna-fine

           add 0,5        to spl-riga
           move spl-riga  to spl-riga-fine
           call "spooler"         using spooler-link.

           add 0,3        to spl-riga
           move spl-riga  to spl-riga-fine
           call "spooler"         using spooler-link.
           add 0,3        to spl-riga
           move spl-riga  to spl-riga-fine
           call "spooler"         using spooler-link.
           add 0,3        to spl-riga
           move spl-riga  to spl-riga-fine
           call "spooler"         using spooler-link.

           set spl-stringa   to true.
           move arial6       to spl-hfont.
           subtract 1,15      from spl-riga.
           subtract 1,1       from spl-colonna.
           move "Ordine n°:"  to spl-riga-stampa
           call "spooler" using spooler-link.

           add 0,3              to spl-riga.
           subtract 0,25         from  spl-colonna.
           move "Data Ordine:"  to spl-riga-stampa
           call "spooler" using spooler-link.


           add 0,3           to spl-riga.
           add 0,1           to spl-colonna.
           move "Info Utente:"  to spl-riga-stampa
           call "spooler" using spooler-link.


           add 0,3              to spl-riga.
      *     add 0,1         to spl-colonna.
           move "Info Promo:"  to spl-riga-stampa
           call "spooler" using spooler-link.

      ***---
       RIQUADRO-CONSEGNA.
           move 8     to spl-pen-with.
           move 2,1  to spl-riga.
           add 1,7  to spl-riga giving spl-riga-fine

           move 22,0                to spl-colonna.
           add 6,5 to spl-colonna giving spl-colonna-fine.

           set  spl-oggetto        to true.
           set  SPL-RETTANGOLO-ROUND  to true.
           set  spl-brush-null     to true.
           set  spl-nero           to true.
           call "spooler"         using spooler-link.

           set  SPL-linea to true.
           move 4         to spl-pen-with.
           add 1,3        to spl-colonna
           subtract 0,2   from spl-colonna-fine

           add 0,5        to spl-riga
           move spl-riga  to spl-riga-fine
           call "spooler"         using spooler-link.

           add 0,3        to spl-riga
           move spl-riga  to spl-riga-fine
           call "spooler"         using spooler-link.
           add 0,3        to spl-riga
           move spl-riga  to spl-riga-fine
           call "spooler"         using spooler-link.
           add 0,3        to spl-riga
           move spl-riga  to spl-riga-fine
           call "spooler"         using spooler-link.

           set spl-stringa   to true.
           move arial6       to spl-hfont.
           subtract 1,15      from spl-riga.
           subtract 1,1      from spl-colonna.
           move "Magazzino"  to spl-riga-stampa
           call "spooler" using spooler-link. 

           add 0,3              to spl-riga.
           subtract 0,15        from  spl-colonna.
           move "di consegna:"  to spl-riga-stampa
           call "spooler" using spooler-link.

           add 0,3              to spl-riga.
           add 0,1              to spl-colonna.
           move "Pagamento:"    to spl-riga-stampa
           call "spooler" using spooler-link.

           add 0,3           to spl-riga.
           add 0,1           to spl-colonna.
           move "Consegna:"  to spl-riga-stampa
           call "spooler" using spooler-link.

      *     add 0,3              to spl-riga.
      *
      *     add 0,25              to spl-colonna.
      *     move "Scarico:"      to spl-riga-stampa
      *     call "spooler" using spooler-link.

      ***---
       GRIGLIA-RIGHE.
           set  spl-oggetto     to true.
           set  SPL-linea       to true.
           set  spl-brush-null  to true.
           set  spl-nero        to true.
           
      *    righe orizzontali
           move 4         to spl-pen-with.
           move 4,5       to spl-riga 
           move spl-riga  to spl-riga-fine.
           move 0,1       to spl-colonna.
           move 28,5      to spl-colonna-fine.
           call "spooler"       using spooler-link.

           add 0,6   to spl-riga 
           move spl-riga  to spl-riga-fine
           call "spooler"       using spooler-link

           move 4,5 to spl-riga

           perform RIGHE-VERTICALI

      *    intestazioni colonne

           set spl-stringa   to true.
           move arial5B      to spl-hfont.
           move 2            to spl-tipo-colonna

      *    riga alta per intestazioni doppie
           move 4,55          to spl-riga.
           initialize r-stof-riga-testa.
           move "COD."                   to rt-stof-art
           move "SALDO"                  to rt-stof-qta
           move "PZ X "                  to rt-stof-imb
           move "PESO"                   to rt-stof-peso-netto
           move "TOTALE KG"              to rt-stof-peso-tot-netto
           move "TOTALE KG"              to rt-stof-peso-tot-utf
           move "LISTINO"                to rt-stof-prz-listino
           move "IMPOSTA AL"             to rt-stof-imp-consumo
           move "CONTRIBUTO"             to rt-stof-imp-cou-cobat
           move "ADD."                   to rt-stof-add-piombo
           move "COSTI"                  to rt-stof-costi-aggi
           move "TOTALE"                 to rt-stof-prz-finale

           move r-stof-riga-testa        to spl-riga-stampa
           call "spooler" using spooler-link.

      *    riga bassa per intestazioni doppie
           move 4,8          to spl-riga.
           initialize r-stof-riga-testa.
           move "LUBEX"                  to rt-stof-art
           move "PEZZI"                  to rt-stof-qta
           move "LT/KG"                  to rt-stof-imb
           move "NETTO"                  to rt-stof-peso-netto
           move "(NETTI)"                to rt-stof-peso-tot-netto
           move "(U.T.F.)"               to rt-stof-peso-tot-utf
           move "RISERVATO"              to rt-stof-prz-listino 
           move "PEZZO"                  to rt-stof-imp-consumo
           move "CONSORZIO"              to rt-stof-imp-cou-cobat
           move "PIOMBO"                 to rt-stof-add-piombo
           move "AGGIUNTIVI"             to rt-stof-costi-aggi
           move "UNITARIO"               to rt-stof-prz-finale

           move r-stof-riga-testa        to spl-riga-stampa
           call "spooler" using spooler-link.
                              
      *    riga per intestazioni singole
           move 4,7          to spl-riga.
           initialize r-stof-riga-testa.
           move "DESCRIZIONE PRODOTTO"   to rt-stof-des
           move "CODICE FORNITORE"       to Rt-STOF-COD-FORN
           move "SC.%"                   to rt-stof-sconto-1
           move "SC.%"                   to rt-stof-sconto-2
           move "SC.%"                   to rt-stof-sconto-3
           move "SC.%"                   to rt-stof-sconto-4
           move "SC.%"                   to rt-stof-sconto-5
           move "NETTO"                  to rt-stof-prz-netto
           move "IMPORTO TOTALE"         to rt-stof-prz-tot-finale

           move r-stof-riga-testa        to spl-riga-stampa
           call "spooler" using spooler-link.

           move zero                     to spl-tipo-colonna.

      *     move 4,55          to spl-riga.
      *     move 0,22          to spl-colonna.
      *     move "COD."       to spl-riga-stampa
      *     call "spooler" using spooler-link.

      *     move 4,8          to spl-riga.
      *     move 0,15         to spl-colonna.
      *     move "LUBEX"     to spl-riga-stampa
      *     call "spooler" using spooler-link.


      *     move 4,7          to spl-riga.
      *     move 0,9          to spl-colonna.
      *     move "DESCRIZIONE PRODOTTO"     to spl-riga-stampa
      *     call "spooler" using spooler-link.


      *     move 4,7          to spl-riga.
      *     move 5,85          to spl-colonna.
      *     move "CODICE FORNITORE"     to spl-riga-stampa
      *     call "spooler" using spooler-link.


      *     move 4,55          to spl-riga.
      *     |move 6,35          to spl-colonna.
      *     move 7,90          to spl-colonna.
      *     move "SALDO"     to spl-riga-stampa
      *     call "spooler" using spooler-link.

      *     move 4,8          to spl-riga.
      *     |move 6,35          to spl-colonna.
      *     move 7,95          to spl-colonna.
      *     move "PEZZI"     to spl-riga-stampa
      *     call "spooler" using spooler-link.


      *     move 4,55          to spl-riga.
      *     move 8,95          to spl-colonna.
      *     move "PZ X "     to spl-riga-stampa
      *     call "spooler" using spooler-link.
      
      *     move 4,8          to spl-riga.
      *     move 8,90          to spl-colonna.
      *     move "LT/KG"   to spl-riga-stampa
      *     call "spooler" using spooler-link.

      ***     move 4,55          to spl-riga.
      ***     move 8,85          to spl-colonna.
      ***     |move 9,15          to spl-colonna.
      ***     move "TARIFFA"     to spl-riga-stampa
      ***     call "spooler" using spooler-link.
      ***
      ***     move 4,8          to spl-riga.
      ***     |move 8,75          to spl-colonna.
      ***     move 8,70          to spl-colonna.
      ***     move "DOGANALE"   to spl-riga-stampa
      ***     call "spooler" using spooler-link.

      *     move 4,55          to spl-riga.
      *     move 10,00          to spl-colonna.
      *     move "PESO"   to spl-riga-stampa
      *     call "spooler" using spooler-link.

      *     move 4,8          to spl-riga.
      *     |move 10,0          to spl-colonna.
      *     move 9,95          to spl-colonna.
      *     move "NETTO"   to spl-riga-stampa
      *     call "spooler" using spooler-link.

      *     move 4,55          to spl-riga.
      *     move 10,9          to spl-colonna.
      **     move  11,00          to spl-colonna.
      *     move "TOTALE KG"   to spl-riga-stampa
      *     call "spooler" using spooler-link.

      *     move 4,8          to spl-riga.
      *     |move 11,1          to spl-colonna.
      *     move 11,15         to spl-colonna.
      *     move "(NETTI)"   to spl-riga-stampa
      *     call "spooler" using spooler-link.

      *     move 4,55          to spl-riga.
      *     |move 12,1          to spl-colonna.
      *     move 12,20         to spl-colonna.
      *     move "TOTALE KG"   to spl-riga-stampa
      *     call "spooler" using spooler-link.

      *     move 4,8          to spl-riga.
      *     |move 12,3          to spl-colonna.
      *     move 12,4          to spl-colonna.
      *     move "(U.T.F.)"   to spl-riga-stampa
      *     call "spooler" using spooler-link.

      *     move 4,55          to spl-riga.
      *     move 13,60          to spl-colonna.
      *     move "LISTINO"   to spl-riga-stampa
      *     call "spooler" using spooler-link.

      *     move 4,8          to spl-riga.
      *     move 13,40          to spl-colonna.
      *     move "RISERVATO"   to spl-riga-stampa
      *     call "spooler" using spooler-link.

      *     move 4,7          to spl-riga.
      *     move 14,80          to spl-colonna.
      *     move "SC.%"   to spl-riga-stampa
      *     call "spooler" using spooler-link.
      *
      *     move 4,7          to spl-riga.
      *     move 15,70          to spl-colonna.
      *     move "SC.%"   to spl-riga-stampa
      *     call "spooler" using spooler-link.
      *
      *     move 4,7          to spl-riga.
      *     move 16,60          to spl-colonna.
      *     move "SC.%"   to spl-riga-stampa
      *     call "spooler" using spooler-link.
      *
      *     move 4,7          to spl-riga.
      *     move 17,50          to spl-colonna.
      *     move "SC.%"   to spl-riga-stampa
      *     call "spooler" using spooler-link.
      *
      *     move 4,7          to spl-riga.
      *     move 18,40          to spl-colonna.
      *     move "SC.%"   to spl-riga-stampa
      *     call "spooler" using spooler-link.

      *     move 4,7          to spl-riga.
      *     move 19,35          to spl-colonna.
      *     move "NETTO"   to spl-riga-stampa
      *     call "spooler" using spooler-link.

      *     move 4,55          to spl-riga.
      *     move 20,35          to spl-colonna.
      *     move "IMPOSTA AL"   to spl-riga-stampa
      *     call "spooler" using spooler-link.

      *     move 4,8          to spl-riga.
      *     move 20,55          to spl-colonna.
      *     move "PEZZO"   to spl-riga-stampa
      *     call "spooler" using spooler-link.

      *     move 4,55          to spl-riga.
      *     move 21,65          to spl-colonna.
      *     move "CONTRIBUTO"   to spl-riga-stampa
      *     call "spooler" using spooler-link.

      *     move 4,8          to spl-riga.
      *     move 21,70          to spl-colonna.
      *     move "CONSORZIO"   to spl-riga-stampa
      *     call "spooler" using spooler-link.

      *     move 4,7          to spl-riga.
      *     move 23,00          to spl-colonna.
      *     move "ADD.PIOMBO"   to spl-riga-stampa
      *     call "spooler" using spooler-link.

      *     move 4,60          to spl-riga.
      *     move 24,55          to spl-colonna.
      *     move "COSTI"   to spl-riga-stampa
      *     call "spooler" using spooler-link.

      *     move 4,8          to spl-riga.
      *     move 24,30          to spl-colonna.
      *     move "AGGIUNTIVI"   to spl-riga-stampa
      *     call "spooler" using spooler-link.

      *     move 4,55          to spl-riga.
      *     move 25,70          to spl-colonna.
      *     move "TOTALE"   to spl-riga-stampa
      *     call "spooler" using spooler-link.

      *     move 4,8          to spl-riga.
      *     move 25,65          to spl-colonna.
      *     move "UNITARIO"   to spl-riga-stampa
      *     call "spooler" using spooler-link.

      *     move 4,7          to spl-riga.
      *     move 26,80        to spl-colonna.
      *     move "IMPORTO TOTALE"   to spl-riga-stampa
      *     call "spooler" using spooler-link.

      ***---
       RIGHE-VERTICALI.
      *    ultima riga
           move 28,5       to spl-colonna
                              spl-colonna-fine.
           call "spooler"       using spooler-link

      *    1 riga codice articolo
           move 0,1       to spl-colonna
                             spl-colonna-fine.
           call "spooler"       using spooler-link

      *    2 riga descrizione articolo
           |add 1 to spl-colonna
           add 0,7 to spl-colonna
           move spl-colonna  to spl-colonna-fine
           call "spooler"       using spooler-link

      *    3 riga pezzi 
           |add 5,05 to spl-colonna
           add 4,95 to spl-colonna
           move spl-colonna  to spl-colonna-fine
           call "spooler"       using spooler-link

           |add 2,1 to spl-colonna
           add 2,0 to spl-colonna
           move spl-colonna  to spl-colonna-fine
           call "spooler"       using spooler-link

      **    4 riga pezzi evasi
      *     add 0,8 to spl-colonna
      *     move spl-colonna  to spl-colonna-fine
      *     call "spooler"       using spooler-link

      *    5 riga imballo
           add 0,9 to spl-colonna
           move spl-colonna  to spl-colonna-fine
           call "spooler"       using spooler-link

      **    6 riga doganale
      *     add 1,2 to spl-colonna
      *     move spl-colonna  to spl-colonna-fine
      *     call "spooler"       using spooler-link

      *    7 riga peso netto
           add 1,1 to spl-colonna
           |add 0,9 to spl-colonna
           move spl-colonna  to spl-colonna-fine
           call "spooler"       using spooler-link

      *    8 riga riga peso netto totale
           add 1 to spl-colonna
           move spl-colonna  to spl-colonna-fine
           call "spooler"       using spooler-link

      *    9 riga peso UTF totale
           add 1,3 to spl-colonna
           move spl-colonna  to spl-colonna-fine
           call "spooler"       using spooler-link

      *    10 riga listino
           add 1,3 to spl-colonna
           move spl-colonna  to spl-colonna-fine
           call "spooler"       using spooler-link

      *    11 riga sc 1
           add 1,2 to spl-colonna
           move spl-colonna  to spl-colonna-fine
           call "spooler"       using spooler-link

      *    12 riga sc 2
           add 0,9 to spl-colonna
           move spl-colonna  to spl-colonna-fine
           call "spooler"       using spooler-link

      *    13 riga sc 3
           add 0,9 to spl-colonna
           move spl-colonna  to spl-colonna-fine
           call "spooler"       using spooler-link

      *    14 riga sc 4
           add 0,9 to spl-colonna
           move spl-colonna  to spl-colonna-fine
           call "spooler"       using spooler-link

      *    15 riga sc 5
           add 0,9 to spl-colonna
           move spl-colonna  to spl-colonna-fine
           call "spooler"       using spooler-link

      *    16 riga prz-uni
           add 0,9 to spl-colonna
           move spl-colonna  to spl-colonna-fine
           call "spooler"       using spooler-link

      *    17 riga imposta
           add 1,2 to spl-colonna
           move spl-colonna  to spl-colonna-fine
           call "spooler"       using spooler-link.

      *    18 riga contributo consorzio
           add 1,3 to spl-colonna
           move spl-colonna  to spl-colonna-fine
           call "spooler"       using spooler-link.

      *    19 riga add piombo
           add 1,4 to spl-colonna
           move spl-colonna  to spl-colonna-fine
           call "spooler"       using spooler-link.

      *    20 costi aggiuntivi
           add 1,25 to spl-colonna
           move spl-colonna  to spl-colonna-fine
           call "spooler"       using spooler-link.

      *    21 tot unitario
           add 1,25 to spl-colonna
           move spl-colonna  to spl-colonna-fine
           call "spooler"       using spooler-link.

      *    22 importo totale
           add 1,25 to spl-colonna
           move spl-colonna  to spl-colonna-fine
           call "spooler"       using spooler-link.

      ***---
       CREA-PDF.
           move tof-numero   to num-edit.
           inspect num-edit replacing leading x"30" by x"20".
           call "C$JUSTIFY" using num-edit, "L".

           evaluate true
           when stof-pdf
      *          move cli-path-pdf to DestFile 
                accept DestFile from environment "PATH_PDF_ORDFORN"
      ******    tolgo l'eventuale barra finale
      *****          inspect DestFile replacing trailing spaces by low-value
      *****          initialize cont
      *****          inspect DestFile tallying cont
      *****                    for characters before low-value
      *****          if DestFile(cont:1) = "\" 
      *****             move low-value  to DestFile(cont:1)
      *****          end-if
      *****     when stof-pdf-tmp
      *****          accept DestFile from environment "PATH_ST_CLIENT"
      ******    tolgo la barra finale
      *****          move zero to cont
      *****          inspect DestFile replacing trailing spaces by low-value
      *****          inspect DestFile tallying cont
      *****                    for characters before low-value
      *****          move low-value  to DestFile(cont:1)
           end-evaluate
      *****     inspect DestFile replacing trailing 
      *****                               low-value by spaces.

           inspect cli-ragsoc-1 replacing trailing space by low-value
           inspect cli-ragsoc-1 replacing all space by "_"
                            
           string num-edit         delimited spaces
                  "_"              delimited size
                  tof-anno         delimited size  
                  "_"              delimited size
                  cli-ragsoc-1     delimited low-value
                  "_("             delimited size
                  como-data(7:2)   delimited size
                  "-"              delimited size
                  como-data(5:2)   delimited size
                  "-"              delimited size
                  como-data(1:4)   delimited size
                  "_"              delimited size
                  como-ora(1:2)    delimited size
                  "-"              delimited size   
                  como-ora(3:2)    delimited size
                  "-"              delimited size   
                  como-ora(5:2)    delimited size

                  ")"              delimited size
      *****            ".pdf"          delimited size
                  into NomeFile
           end-string.
                            
                  
      *     accept selprint-stampante
      *            from environment "STAMPANTE_DIRETTA_ORDINI_PDF".
      *     inspect NomeFile 
      *             replacing trailing spaces by low-value.
      *     inspect DestFile 
      *             replacing trailing spaces by low-value.
      *     inspect selprint-stampante 
      *             replacing trailing spaces by low-value.

      *     initialize parametri.
      *     string NomeFile           delimited low-value
      *            "§"                delimited size
      *            DestFile           delimited low-value
      *            "§"                delimited size
      *            selprint-stampante delimited low-value
      *            into parametri
      *     end-string.

           
           set settaPDF-setta   to true

           move NomeFile  to settaPDF-nome-file
           move DestFile  to settaPDF-percorso
           call   "settaPDF2" using settaPDF-linkage
           cancel "settaPDF2".

      *****     inspect NomeFile 
      *****             replacing trailing spaces by low-value.
      *****     string NomeFile   delimited low-value
      *****            ".pdf"     delimited size
      *****            into NomeFile.
      *****
      *****     inspect DestFile 
      *****             replacing trailing spaces by low-value.

      *****     if not settaPDF-OK       
      *****        display message "Archiviazione PDF fallita!"
      *****                  title titolo
      *****                   icon 2
      *****     end-if.
      *****                       
      *****     initialize stof-path-pdf
      *****     string DestFile   delimited by low-value
      *****            "\"        delimited by size
      *****            NomeFile   delimited by low-value
      *****            into stof-path-pdf.

      *     accept  PathCreaPDF from environment "PATH_EXE_PDF".
      *     inspect PathCreaPDF replacing trailing spaces by low-value.
      *
      *     initialize comando.
      *     string PathCreaPDF delimited low-value
      *            " "         delimited size
      *            parametri   delimited size
      *            into comando
      *     end-string.
      *                       
      *     move 0 to StatusCreaPDF.
      *     call "C$SYSTEM" using comando, 129
      *                    giving StatusCreaPDF
      *     if StatusCreaPDF = -1
      *        display message "Archiviazione PDF fallita!"
      *                  title titolo
      *                   icon 2
      *     end-if.
      *     perform WAIT-3-SECS.

      ***---
      * C$SLEEP ha problemi su thin-client e ritorna il controllo 
      * al menu perciò viene sostituita da questa routine
       WAIT-3-SECS.
           set ScartaSecondi to false.
           accept como-time from time.
           evaluate como-ss
           when 57    move 0 to como-ss-end
                      set ScartaSecondi to true
           when 58    move 1 to como-ss-end
                      set ScartaSecondi to true
           when 59    move 2 to como-ss-end
                      set ScartaSecondi to true
           when 60    move 3 to como-ss-end
                      set ScartaSecondi to true
           when other add  3 to como-ss giving como-ss-end
           end-evaluate.
           perform until 1 = 2
              accept como-time from time
              if como-ss >= como-ss-end
                 if not ScartaSecondi
                    exit perform
                 else
                    |Altrimenti se i secondi iniziali sono 57
                    |al 58(> di 0) uscirebbe subito senza attendere
                    if como-ss not = 57 and
                       como-ss not = 58 and
                       como-ss not = 59 and
                       como-ss not = 60
                       exit perform
                    end-if
                 end-if
              end-if
           end-perform.

      ***---
       SCRIVI-PIEDE.
      *    calcolo il totale dell'IVA
           perform varying idx from 1 by 1 until idx > 10
              if cod-iva(idx) = space
                 exit perform
              end-if

              move "IV"         to tbliv-codice1
              move cod-iva(idx) to tbliv-codice2   
              read tivaese no lock 
                   invalid move 0 to TBLIV-PERCENTUALE
              end-read

              compute como-iva = 
                   ( ( imp-iva(idx) * tbliv-percentuale ) / 100 )

              add 0,00005          to como-iva
              move como-iva        to como-iva-4dec

      *        add 0,0005           to como-iva-4dec
      *        move como-iva-4dec   to como-iva-3dec
      *
      *        add 0,005            to como-iva-3dec
      *        move como-iva-3dec   to como-iva-2dec

              add como-iva-4dec to tot-iva
           end-perform.

           add 0,0005           to tot-iva
           move tot-iva         to como-iva-3dec

           add 0,005            to como-iva-3dec
           move como-iva-3dec   to como-iva-2dec

           move como-iva-2dec   to tot-iva


           perform SCRIVI-TOTALI.

           perform GRIGLIA-NOTE.

      ***---
       SCRIVI-TOTALI.
      *    guardo se ci sto con il piede nella stampa se no cambio pagina
           add 5 to riga giving pronostico
           if pronostico > maxrighe
              perform SALTO-PAGINA
           end-if.

      *    titoli dei totali
           move spl-riga-fine   to spl-riga
           add 0,4              to spl-riga-fine

           set  spl-oggetto     to true.
           set  SPL-RETTANGOLO  to true.
           set  spl-brush-null  to true.
           set  spl-nero        to true.
           move 28,5  to spl-colonna-fine
           subtract 1,8   from spl-colonna-fine giving spl-colonna
           call "spooler"       using spooler-link.

           add 0,1              to spl-riga

           set spl-stringa   to true.
           move zero         to spl-tipo-colonna
           move arial5       to spl-hfont.
      *     move  5,65        to spl-colonna
      *     move "TOT. PEZZI"  to spl-riga-stampa
      *     call "spooler" using spooler-link.


           move  7,72          to spl-colonna
           move "TOT. PEZZI"   to spl-riga-stampa
           call "spooler" using spooler-link.



           |move  10,9          to spl-colonna
           move  10,85          to spl-colonna
           move "TOTALE KG"  to spl-riga-stampa
           call "spooler" using spooler-link.

           |move  12,2          to spl-colonna
           move  12,30          to spl-colonna
           move "KG U.T.F."  to spl-riga-stampa
           call "spooler" using spooler-link.

      *     move  16,0          to spl-colonna
           move  20,30          to spl-colonna
           move "TOT.IMPOSTA"  to spl-riga-stampa
           call "spooler" using spooler-link.

      *     move  18,5          to spl-colonna
           move  21,75         to spl-colonna
           move "TOT.CONS."  to spl-riga-stampa
           call "spooler" using spooler-link.

           move  23,00         to spl-colonna
           move "TOT.ADD.PIOMBO"  to spl-riga-stampa
           call "spooler" using spooler-link.

      *    rettangoli prima linea
           set  spl-oggetto     to true.
           set  SPL-RETTANGOLO  to true.
           set  spl-brush-null  to true.
           set  spl-nero        to true.

           add 0,4              to spl-riga-fine
           subtract 0,4         from spl-riga-fine giving spl-riga


      *    totale pezzi
      *     move 7,95 to spl-colonna
           move 7,75 to spl-colonna
           add 0,9 to spl-colonna giving spl-colonna-fine
           call "spooler"       using spooler-link
      *     add 0,8 to spl-colonna 
      *     add 0,8 to spl-colonna giving spl-colonna-fine
      *     call "spooler"       using spooler-link

      *    peso netto
           |add 2,8  to spl-colonna
           add 3,0  to spl-colonna
           add 1,3  to spl-colonna giving  spl-colonna-fine
           call "spooler"       using spooler-link.
      
      *    peso U.T.F.
           set  SPL-BRUSH-LTGRAY   to true.
           add 1,3  to spl-colonna
           add 1,3  to spl-colonna-fine
           call "spooler"       using spooler-link.

           set  spl-brush-null  to true.

      *    Imposta
           |add 5,2  to spl-colonna
           add 8,2  to spl-colonna
      *     add 1,25  to spl-colonna giving  spl-colonna-fine
           add 1,3  to spl-colonna giving  spl-colonna-fine
           call "spooler"       using spooler-link

      *    cou/cobat
           add 1,30   to spl-colonna
           add 1,40   to spl-colonna giving  spl-colonna-fine
           call "spooler"       using spooler-link.

      *    cou/cobat
           add 1,40   to spl-colonna
           add 1,25   to spl-colonna giving  spl-colonna-fine
           call "spooler"       using spooler-link.

      *    totale
           move 28,5  to spl-colonna-fine
           subtract 1,8   from spl-colonna-fine giving spl-colonna
           call "spooler"       using spooler-link.

      *    totali prima linea
           initialize r-stof-riga.

           move space           to r-stof-qta

           move tot-peso              to como-pic-96v93
           move como-pic-96v93(2:11)  to r-stof-peso-tot-netto
           move tot-peso-utf          to como-pic-96v93
           move como-pic-96v93(2:11)  to r-stof-peso-tot-utf

           add 0,0005                 to tot-imposta
           move tot-imposta           to tot-imposta-3dec
           add 0,005                  to tot-imposta-3dec
           move tot-imposta-3dec      to tot-imposta-2dec
           move tot-imposta-2dec      to tot-imposta
           if tot-imposta = zero
              move space              to r-stof-imp-consumo
           else
              move tot-imposta(9:2)   to como-xx
              if como-xx not = "00"
                 move tot-imposta     to como-pic-96v94
                 move como-pic-96v94(2:12)  to r-stof-imp-consumo|(1:11)
      *           move como-pic-96v94  to r-stof-imp-cons|r-stof-imp-consumo(1:10)
              else
                 move tot-imposta     to como-pic-96v92
                 |move como-pic-96v92(1:11)  to r-stof-imp-cons|r-stof-imp-consumo(1:10)
                 move como-pic-96v92  to r-stof-imp-cons|r-stof-imp-consumo(1:10)
              end-if

              call "C$JUSTIFY" using r-stof-imp-cons, "R"
              move "€"                to r-stof-imp-cons-e
           end-if


           add 0,0005                 to tot-cou
           move tot-cou               to tot-cou-3dec
           add 0,005                  to tot-cou-3dec
           move tot-cou-3dec          to tot-cou-2dec
           move tot-cou-2dec          to tot-cou

           if tot-cou = zero
              move space  to r-stof-imp-cou-cobat
           else
              move tot-cou(9:2)  to como-xx
              if como-xx not = "00"
                 move tot-cou  to como-pic-94v94
                 move como-pic-94v94     to r-stof-imp-cou|r-stof-imp-cou-cobat(1:10)
              else
                 move tot-cou  to como-pic-94v92
                 move como-pic-94v92     to r-stof-imp-cou|r-stof-imp-cou-cobat(3:8)
              end-if
              call "C$JUSTIFY" using r-stof-imp-cou, "R"
              move "€"                   to r-stof-imp-cou-e
           end-if

           if totale = zero
              move space           to r-stof-prz-tot-finale
           else
              move space  to r-stof-prz-tot-finale
              move totale(10:2)  to como-xx
              if como-xx not = "00"
                 move totale  to como-pic-97v94
                 move como-pic-97v94     to r-stof-prz-tot-fin|r-stof-prz-tot-finale(1:13)
              else
                 move totale  to como-pic-97v92
                 move como-pic-97v92     to r-stof-prz-tot-fin|r-stof-prz-tot-finale(2:12)
              end-if
              call "C$Justify" using r-stof-prz-tot-fin, "R"
              move "€"                   to r-stof-prz-tot-fin-e
           end-if.

           add 0,0005                    to tot-piombo
           move tot-piombo               to tot-piombo-3dec
           add 0,005                     to tot-piombo-3dec
           move tot-piombo-3dec          to tot-piombo-2dec
           move tot-piombo-2dec          to tot-piombo

           if tot-piombo = zero
              move space           to r-stof-add-piombo
           else
              move tot-piombo(9:2)  to como-xx
              if como-xx not = "00"
                 move tot-piombo  to como-pic-96v94
                 move como-pic-96v94     to r-stof-add-pb|r-stof-add-piombo(1:10)
              else
                 move tot-piombo  to como-pic-96v92
                 move como-pic-96v92     to r-stof-add-pb|r-stof-add-piombo(3:8)
              end-if
              call "C$JUSTIFY" using r-stof-add-pb, "R"
              move "€"                   to r-stof-add-pb-e
           end-if

           set spl-stringa   to true.
           move 1            to spl-tipo-colonna
           move arial6       to spl-hfont.
           move 1            to spl-tipo-colonna
           add 0,1           to  spl-riga.
      *     subtract 0,3      from spl-riga.
           
           move "TOTALI"     to r-stof-des.
           move "SUB TOTALE" to r-stof-prz-fin.
           move tot-qta               to como-pic-z7
           move como-pic-z7           to r-stof-qta
           perform SCRIVI-RIGA-CSV.            
           move spaces to r-stof-des.
           move spaces to r-stof-prz-fin.
           move spaces to r-stof-qta

           move r-stof-riga  to spl-riga-stampa
           call "spooler" using spooler-link.

           initialize r-stof-riga
           move tot-qta               to como-pic-z7
           move como-pic-z7           to r-stof-qta
                                       
           move r-stof-riga to spl-riga-stampa
           move arial6b       to spl-hfont.
           move 1            to spl-tipo-colonna.
           call "spooler" using spooler-link.

           move zero         to spl-tipo-colonna
           move "SUB TOTALE" to spl-riga-stampa
           move 25,3           to spl-colonna
           call "spooler" using spooler-link.

      **    spese di spedizione (sempre vuote)
      *     set  spl-oggetto     to true.
      *     set  SPL-RETTANGOLO  to true.
      *     set  spl-brush-null  to true.
      *     set  spl-nero        to true.
      *
      *     add 0,4              to spl-riga-fine
      *     subtract 0,4         from spl-riga-fine giving spl-riga
      *
      *     move 28  to spl-colonna-fine
      *     subtract 1,5   from spl-colonna-fine giving spl-colonna
      *     call "spooler"       using spooler-link.
      *
      *     set spl-stringa   to true.
      *     move arial6       to spl-hfont.
      *     add 0,1           to  spl-riga.
      *     move zero         to spl-tipo-colonna
      *     move "SPESE DI TRASPORTO"to spl-riga-stampa  
      *     move 24,1           to spl-colonna
      *     call "spooler" using spooler-link.

      *    IVA
           set  spl-oggetto     to true.
           set  SPL-RETTANGOLO  to true.
           set  spl-brush-null  to true.
           set  spl-nero        to true.

           add 0,4              to spl-riga-fine
           subtract 0,4         from spl-riga-fine giving spl-riga

           move 28,5  to spl-colonna-fine
           subtract 1,8   from spl-colonna-fine giving spl-colonna
           call "spooler"       using spooler-link.

           initialize r-stof-riga.

           if tot-iva = zero
              move space           to r-stof-prz-tot-finale
           else
              move space  to r-stof-prz-tot-finale
      *        move tot-iva(09:2)  to como-xx
      *        if como-xx not = "00"
      *           move tot-iva  to como-pic-97v94
      *           move como-pic-97v94     to r-stof-prz-tot-finale(1:13)
      *        else
                 move tot-iva  to como-pic-97v92
                 move como-pic-97v92     to r-stof-prz-tot-fin|r-stof-prz-tot-finale(3:12)
      *        end-if
              call "C$JUSTIFY" using r-stof-prz-tot-fin, "R"
              move "€"                   to r-stof-prz-tot-fin-e
           end-if.

           set spl-stringa   to true.
           move 1            to spl-tipo-colonna
           move arial6       to spl-hfont.
           move 1            to spl-tipo-colonna
           add 0,1           to  spl-riga.
      *     subtract 0,3      from spl-riga.  
           move "IVA" to r-stof-prz-fin.
           perform SCRIVI-RIGA-CSV.           
           move spaces to r-stof-prz-fin.

           move r-stof-riga  to spl-riga-stampa
           call "spooler" using spooler-link.

           move zero         to spl-tipo-colonna
           move "IVA"        to spl-riga-stampa  
           move 26,3         to spl-colonna
           call "spooler" using spooler-link.

      *    TOTALE FATTURA
           set  spl-oggetto     to true.
           set  SPL-RETTANGOLO  to true.
           set  spl-brush-null  to true.
           set  spl-nero        to true.

           add 0,4              to spl-riga-fine
           subtract 0,4         from spl-riga-fine giving spl-riga

           move 28,5  to spl-colonna-fine
           subtract 1,8   from spl-colonna-fine giving spl-colonna
           call "spooler"       using spooler-link.

           initialize r-stof-riga.

           add tot-iva to totale
      *     move totale          to como-pic-97v92
      *     move como-pic-97v92  to r-stof-prz-tot-finale(1:13)
      *     move "€"             to r-stof-prz-tot-finale(14:1)

              move space  to r-stof-prz-tot-finale
      *        move totale(10:2)  to como-xx
      *        if como-xx not = "00"
      *           move totale  to como-pic-97v94
      *           move como-pic-97v94     to r-stof-prz-tot-finale(1:13)
      *        else
                 move totale  to como-pic-97v92
                 move como-pic-97v92     to r-stof-prz-tot-fin|r-stof-prz-tot-finale(3:12)
      *        end-if
              call "C$JUSTIFY" using r-stof-prz-tot-fin, "R"
              move "€"                   to r-stof-prz-tot-fin-e

           set spl-stringa   to true.
           move 1            to spl-tipo-colonna
           move arial7B      to spl-hfont.
           move 1            to spl-tipo-colonna
           add 0,1           to  spl-riga.
      *     subtract 0,3      from spl-riga.  
           move "TOTALE FATTURA" to r-stof-prz-fin.
           perform SCRIVI-RIGA-CSV.                
           move spaces to r-stof-prz-fin.
           move r-stof-riga        to spl-riga-stampa
           call "spooler" using spooler-link.

           move zero               to spl-tipo-colonna
           move "TOTALE FATTURA"   to spl-riga-stampa  
           move 24,45              to spl-colonna
           call "spooler" using spooler-link.

      ***---
       GRIGLIA-NOTE.
           if riga > maxrighe - 10
              perform SALTO-PAGINA
           end-if.

           move zero   to spl-tipo-colonna
           move 8     to spl-pen-with.
           move 15,4  to spl-riga.
           add 3,2  to spl-riga giving spl-riga-fine
      
           move 0,6                to spl-colonna.
           add 22,8 to spl-colonna giving spl-colonna-fine.
      
           set  spl-oggetto        to true.
           set  SPL-RETTANGOLO-ROUND  to true.
           set  spl-brush-null     to true.
           set  spl-nero           to true.
           call "spooler"         using spooler-link.
      
           set  SPL-linea to true.
           move 4         to spl-pen-with.
           add 0,7        to spl-colonna
           subtract 0,1   from spl-colonna-fine
      
      *    righe interne
           add 0,3        to spl-riga
           perform 9 times
              add 0,3        to spl-riga
              move spl-riga  to spl-riga-fine
              call "spooler"         using spooler-link
           end-perform.

           set spl-stringa   to true.
           |move arial6       to spl-hfont.
           move font-note     to spl-hfont.
           subtract 2,65      from spl-riga.
           subtract 0,6      from spl-colonna.
           move "Note:"      to spl-riga-stampa
           call "spooler" using spooler-link.

           subtract 0,3      from spl-riga.
           add 0,6           to spl-colonna

      *    scrivo le 3 note fisse
           add 0,3  to spl-riga.
           move spaces to spl-riga-stampa.
           accept spl-riga-stampa from environment "RIGA_NOTE_ORDF_1".
           if spl-riga-stampa = spaces
              move "Il presente ordine si considera da Voi accettato se 
      -      "non contestato entro 3 giorni. CONAI compreso ove dovuto."
                                                  to spl-riga-stampa
           end-if.                                         
           call "spooler" using spooler-link
                              
           if stampa-csv 
              initialize csv-riga
              string separatore delimited size
                     "Note"     delimited size
                into csv-riga
              end-string
              write csv-riga
           end-if.

           if stampa-csv 
              initialize csv-riga
              string separatore      delimited size
                     spl-riga-stampa delimited size
                into csv-riga
              end-string
              write csv-riga
           end-if.

           add 0,3  to spl-riga.
           move spaces to spl-riga-stampa.
           accept spl-riga-stampa from environment "RIGA_NOTE_ORDF_2".
           if spl-riga-stampa = spaces
              move "PRENOTAZIONI: LUBEX 02-26515539, C/DEPOSITO MTN 0371
      -         "-697620, C/DEPOSITO SLI 0143-677767 - lo scarico non pr
      -         "enotato verrà respinto" to spl-riga-stampa
           end-if
           call "spooler" using spooler-link       

           if stampa-csv 
              initialize csv-riga
              string separatore      delimited size
                     spl-riga-stampa delimited size
                into csv-riga
              end-string
              write csv-riga
           end-if.

           add 0,3  to spl-riga.                                      
           move spaces to spl-riga-stampa.
           accept spl-riga-stampa from environment "RIGA_NOTE_ORDF_3".
           if spl-riga-stampa = spaces
              move "I BANCALI RICEVUTI SU CUI E' POSTA LA MERCE VERRANNO
      -         " CONSIDERATI A PERDERE"             to spl-riga-stampa
           end-if.
           call "spooler" using spooler-link    

           if stampa-csv 
              initialize csv-riga
              string separatore      delimited size
                     spl-riga-stampa delimited size
                into csv-riga
              end-string
              write csv-riga
           end-if.
      *
           move tof-chiave to nof-chiave-ordine
           move low-value  to nof-num-nota
           start nordforn key >= nof-chiave
                 invalid continue
             not invalid
                 perform 6 times
                    read nordforn next at end exit perform end-read
                    if tof-chiave not = nof-chiave-ordine
                       exit perform
                    end-if
                    add 0,3  to spl-riga
                    move nof-nota  to spl-riga-stampa
                    call "spooler" using spooler-link

                    if stampa-csv 
                       initialize csv-riga
                       string separatore      delimited size
                              spl-riga-stampa delimited size
                         into csv-riga
                       end-string
                       write csv-riga
                    end-if

                 end-perform
           end-start.

           if not franco
              perform DATI-CONSEGNA
           end-if.
           .

      **    firma del BUYER
      *     move Arial13BI          to spl-hfont.
      *     move 17,5               to spl-riga.
      *     move 23,9               to spl-colonna
      *     move "FIRMA DEL BUYER"  to spl-riga-stampa
      *     call "spooler" using spooler-link.

      ***----
       DATI-CONSEGNA.
           move zero   to spl-tipo-colonna
           move 8     to spl-pen-with.
           move 15,4  to spl-riga.
           add 3,2  to spl-riga giving spl-riga-fine
      
           move 23,6                to spl-colonna.
           add 5,0 to spl-colonna giving spl-colonna-fine.
      
           set  spl-oggetto        to true.
           set  SPL-RETTANGOLO-ROUND  to true.
           set  spl-brush-null     to true.
           set  spl-nero           to true.
           call "spooler"         using spooler-link.
      
           set  SPL-linea to true.
           move 4         to spl-pen-with.
           add 0,1        to spl-colonna
           subtract 0,1   from spl-colonna-fine
      
      *    righe interne
           add 0,6 to spl-riga.
           perform 3 times
              add 0,3           to spl-riga
              move spl-riga     to spl-riga-fine
              call "spooler" using spooler-link
           end-perform.

           if tof-st-dati-fatt-si
              add 0,3        to spl-riga
              perform 4 times
                 add 0,3        to spl-riga
                 move spl-riga  to spl-riga-fine
                 call "spooler"         using spooler-link
              end-perform
           else
              add 1,5 to spl-riga
           end-if

           set spl-stringa to true.
           |move arial6       to spl-hfont.
           move font-noteB to spl-hfont.
           subtract 2,65 from spl-riga.

           add 1,4                 to spl-colonna
           move "DATI DI CONSEGNA" to spl-riga-stampa
           call "spooler"       using spooler-link.
           subtract  1,4         from spl-colonna       
                         
           if stampa-csv           
              write csv-riga from spaces
              initialize csv-riga
              string separatore      delimited size
                     spl-riga-stampa delimited size
                into csv-riga
              end-string
              write csv-riga
           end-if.

           if tca-cod-magaz = "EXD"
              move tof-cliente     to z4
              move tof-destino-c   to z2
              initialize spl-riga-stampa

              call "C$JUSTIFY" using z4, "L"
              call "C$JUSTIFY" using z2, "L"
              inspect z4 replacing trailing space by low-value
              if tof-destino-c not = zero
                 string "Cod."  delimited by size
                         z4     delimited by low-value
                        "-"     delimited by size
                         z2     delimited by size
                       into spl-riga-stampa
              else
                 string "Cod." delimited by size
                         z4    delimited by low-value
                       into spl-riga-stampa
              end-if

              add 3,6 to spl-colonna
              call "spooler"   using spooler-link
              subtract 3,6 from spl-colonna

              if stampa-csv 
                 initialize csv-riga
                 string separatore      delimited size
                        spl-riga-stampa delimited size
                   into csv-riga
                 end-string
                 write csv-riga
              end-if

           end-if


           move font-note     to spl-hfont.
           if tof-destino-c not = 0
              move tof-cliente     to des-codice
              move tof-destino-c   to des-prog

              read destini invalid continue end-read

              move des-ragsoc-1 to cons-ragsoc
              move des-indirizzo   to cons-ind
              initialize cons-localita
              inspect des-localita replacing trailing space by low-value
              string des-cap       delimited by size
                     " "           delimited by size
                     des-localita  delimited by low-value
                     " ("          delimited by size
                     des-prov      delimited by size
                     ") "          delimited by size
                     des-nazione   delimited by size
                     into cons-localita
      *        initialize cons-piva
      *        if des-piva not = zero
      *           string "P.IVA "      delimited by size
      *                  des-piva      delimited by size
      *                  into cons-piva
      *        end-if
 
           else
              set cli-tipo-c    to true
              move tof-cliente  to cli-codice
              read clienti invalid continue end-read

              move cli-ragsoc-1  to cons-ragsoc
              move cli-indirizzo to cons-ind
              initialize cons-localita
              inspect cli-localita replacing trailing space by low-value
              string cli-cap       delimited by size
                     " "           delimited by size
                     cli-localita  delimited by low-value
                     " ("          delimited by size
                     cli-prov      delimited by size
                     ") "          delimited by size
                     cli-nazione   delimited by size
                     into cons-localita
      *        initialize cons-piva
      *        if cli-piva not = zero
      *           string "P.IVA "      delimited by size
      *                  cli-piva      delimited by size
      *                  into cons-piva
      *        end-if
           end-if.

      *     subtract 0,3      from spl-riga.
      *     add 0,6           to spl-colonna

      *    scrivo i dati di consegna
           add 0,3             to spl-riga.
           move cons-ragsoc    to spl-riga-stampa.
           call "spooler"   using spooler-link.       
                              
           if stampa-csv        
              initialize csv-riga
              string separatore      delimited size
                     spl-riga-stampa delimited size
                into csv-riga
              end-string
              write csv-riga
           end-if.

           add 0,3             to spl-riga.
           move cons-ind       to spl-riga-stampa.
           call "spooler"   using spooler-link.       
           
           if stampa-csv 
              initialize csv-riga
              string separatore      delimited size
                     spl-riga-stampa delimited size
                into csv-riga
              end-string
              write csv-riga
           end-if.

           add 0,3             to spl-riga.
           move cons-localita  to spl-riga-stampa.
           call "spooler"   using spooler-link.       
           
           if stampa-csv 
              initialize csv-riga
              string separatore      delimited size
                     spl-riga-stampa delimited size
                into csv-riga
              end-string
              write csv-riga
           end-if.
           
           set cli-tipo-c    to true.
           if tca-cod-magaz = "EXD"
              move tge-cliente-corrisp   to cli-codice
           else
              move tof-cliente  to cli-codice
           end-if
           read clienti invalid continue end-read.

           move cli-ragsoc-1  to cons-ragsoc.
           move cli-indirizzo to cons-ind.
           initialize cons-localita.
           inspect cli-localita replacing trailing space by low-value.
           string cli-cap       delimited by size
                  " "           delimited by size
                  cli-localita  delimited by low-value
                  " ("          delimited by size
                  cli-prov      delimited by size
                  ") "          delimited by size
                  cli-nazione   delimited by size
                  into cons-localita.
           initialize cons-piva.
           if cli-piva not = zero
              string "P.IVA "      delimited by size
                     cli-piva      delimited by size
                     into cons-piva
           end-if.

           if tof-st-dati-fatt-si
              add 0,3  to spl-riga
              move font-noteB    to spl-hfont
              add 1,2        to spl-colonna
              move "DATI DI FATTURAZIONE"   to spl-riga-stampa  

              if stampa-csv 
                 write csv-riga from spaces
                 initialize csv-riga
                 string separatore      delimited size
                        spl-riga-stampa delimited size
                   into csv-riga
                 end-string
                 write csv-riga
              end-if

              call "spooler" using spooler-link
              subtract  1,2        from spl-colonna
              move font-note     to spl-hfont
              add 0,3  to spl-riga
              move cons-ragsoc     to spl-riga-stampa
              call "spooler" using spooler-link

              if stampa-csv 
                 initialize csv-riga
                 string separatore      delimited size
                        spl-riga-stampa delimited size
                   into csv-riga
                 end-string
                 write csv-riga
              end-if

              add 0,3  to spl-riga
              move cons-ind        to spl-riga-stampa
              call "spooler" using spooler-link

              if stampa-csv 
                 initialize csv-riga
                 string separatore      delimited size
                        spl-riga-stampa delimited size
                   into csv-riga
                 end-string
                 write csv-riga
              end-if

              add 0,3  to spl-riga
              move cons-localita   to spl-riga-stampa
              call "spooler" using spooler-link

              if stampa-csv 
                 initialize csv-riga
                 string separatore      delimited size
                        spl-riga-stampa delimited size
                   into csv-riga
                 end-string
                 write csv-riga
              end-if
                                               
              add 0,3  to spl-riga
              move cons-piva       to spl-riga-stampa
              call "spooler" using spooler-link

              if stampa-csv 
                 initialize csv-riga
                 string separatore      delimited size
                        spl-riga-stampa delimited size
                   into csv-riga
                 end-string
                 write csv-riga
              end-if

           end-if.

      ***---
       ART-DA-CONF.
           move tof-chiave   to aor-chiave-testa.
           move low-value    to aor-prog.

           start art-ordforn key not < aor-chiave
              invalid 
                 continue
              not invalid
                 perform until 1 = 2
                    read art-ordforn next 
                       at end 
                          exit perform 
                    end-read
              
                    if tof-chiave not = aor-chiave-testa
                       exit perform
                    end-if
                    perform SCRIVI-RIGA-ART-DA-CONF
                 end-perform
           end-start.

      ***---
       SCRIVI-RIGA-ART-DA-CONF.
           add 1 to riga
           if riga > MaxRighe
              perform SALTO-PAGINA
              move 1   to riga
           end-if.

           move aor-articolo       to r-stof-art.

           move aor-chiave-progmag to prg-chiave.
           read progmag invalid continue end-read.

           add aor-qta   to tot-qta.

           compute tot-peso =
                   tot-peso + 
                 ( aor-peso * aor-qta).

           compute tot-imposta = tot-imposta + 
                                (aor-imp-cons * aor-qta).
           compute tot-cou = tot-cou + 
                                (aor-coubat * aor-qta).

           compute tot-piombo = tot-piombo + 
                                (aor-add-pb * aor-qta).

           move aor-descrizione to r-stof-des.

           if aor-imballo not = space
              move aor-imballo  to imq-codice
              read timbalqta
                 invalid
                    move space  to imq-tipo
                    move zero   to imq-qta-imb
              end-read
              move imq-tipo  to imb-codice
              read timballi
                 invalid
                    move space  to imb-descrizione
              end-read

              move imq-qta-imb to imballi-ed
              call "C$JUSTIFY" using imballi-ed, "R"
              inspect imb-descrizione replacing trailing 
                                                spaces by low-value
              initialize r-stof-imb
              string imballi-ed       delimited by low-value
                     " x "            delimited by size
                     art-udm-imballo  delimited by size
                     into r-stof-imb
           else
              move space  to r-stof-imb
           end-if.

     
      *     move aor-qta               to como-pic-z5
      *     move como-pic-z5           to r-stof-qta
           move space  to r-stof-qta.

      *     move space  to r-stof-cod-dog

           if aor-peso = zero
              move space           to r-stof-peso-netto
           else
              move aor-peso        to como-pic-96v93
              move como-pic-96v93  to r-stof-peso-netto
           end-if.

           compute como-peso = aor-peso * aor-qta.
           if como-peso = zero
              move space  to r-stof-peso-tot-netto
           else
              move como-peso       to como-pic-96v93
              move como-pic-96v93  to r-stof-peso-tot-netto
           end-if.

           move space                 to r-stof-peso-tot-utf.
           move space                 to r-stof-prz-listino.
           move space                 to r-stof-perce-1
                                         r-stof-sconto-1.
           move space                 to r-stof-perce-2
                                         r-stof-sconto-2.
           move space                 to r-stof-perce-3
                                         r-stof-sconto-3.
           move space                 to r-stof-perce-4
                                         r-stof-sconto-4.
           move space                 to r-stof-perce-5
                                         r-stof-sconto-5.

           if aor-netto = 0
              |move space              to r-stof-prz-netto
              move "OMAGGIO"       to r-stof-prz-netto
           else
              move aor-netto(12:2) to como-xx
              if como-xx not = "00"
                 move aor-netto          to como-pic-96v94
                 move como-pic-96v94(2:) to r-stof-prz-net
              else
                 move aor-netto      to como-pic-96v92
                 move como-pic-96v92 to r-stof-prz-net
              end-if
              call "C$JUSTIFY" using r-stof-prz-net, "R"
              move "€"            to r-stof-prz-net-e

           end-if.

           if aor-imp-cons = 0
              move space             to r-stof-imp-consumo
           else
              move aor-imp-cons(7:2) to como-xx
              if como-xx not = "00"
                 move aor-imp-cons   to como-pic-94v94
                 move como-pic-94v94 to r-stof-imp-cons|r-stof-imp-consumo(1:10)
              else
                 move aor-imp-cons   to como-pic-94v92
                 move como-pic-94v92 to r-stof-imp-cons|r-stof-imp-consumo(3:8)
              end-if
              call "C$JUSTIFY" using r-stof-imp-cons, "R"
              move "€"            to r-stof-imp-cons-e
           end-if.

           if aor-coubat = 0
              move space             to r-stof-imp-cou-cobat
           else
              move aor-coubat(7:2)   to como-xx
              if como-xx not = "00"
                 move aor-coubat     to como-pic-94v94
                 move como-pic-94v94 to r-stof-imp-cou|r-stof-imp-cou-cobat(1:10)
              else
                 move aor-coubat     to como-pic-94v92
                 move como-pic-94v92 to r-stof-imp-cou|r-stof-imp-cou-cobat(3:8)
              end-if
              call "C$JUSTIFY" using r-stof-imp-cou, "R"
              move "€"            to r-stof-imp-cou-e
           end-if.

           if Aor-Add-Pb = 0
              move space             to r-stof-add-piombo
           else
              move aor-add-pb(7:2)   to como-xx
              if como-xx not = "00"
                 move aor-add-pb     to como-pic-94v94
                 move como-pic-94v94 to r-stof-add-pb|r-stof-add-piombo(1:10)
              else
                 move aor-add-pb     to como-pic-94v92
                 move como-pic-94v92 to r-stof-add-pb|r-stof-add-piombo(3:8)
              end-if
              call "C$JUSTIFY" using r-stof-add-pb, "R"
              move "€"            to r-stof-add-pb-e
           end-if.

           if aor-costi-agg = 0
              move space             to r-stof-costi-aggi
           else
              move aor-costi-agg(7:2)to como-xx
              if como-xx not = "00"
                 move aor-costi-agg  to como-pic-96v94
                 move como-pic-96v94 to r-stof-costi-agg
              else
                 move aor-costi-agg  to como-pic-96v92
                 move como-pic-96v92 to r-stof-costi-agg
              end-if
              call "C$JUSTIFY" using r-stof-costi-agg, "R"
              move "€"            to r-stof-costi-aggi-e
           end-if.

           move aor-prz-unit to como-prezzo.

           if como-prezzo = zero
              move space           to r-stof-prz-finale
           else
              move space           to r-stof-prz-finale
              move como-prezzo(12:2)  to como-xx
              if como-xx not = "00"
                 move como-prezzo  to como-pic-96v94
                 move como-pic-96v94     to r-stof-prz-fin|r-stof-prz-finale(1:12)
              else
                 move como-prezzo  to como-pic-96v92
                 move como-pic-96v92     to r-stof-prz-fin|r-stof-prz-finale(3:10)
              end-if
              call "C$JUSTIFY" using r-stof-prz-fin, "R"
              move "€"                   to r-stof-prz-fin-e
           end-if.

           compute como-prezzo = como-prezzo * aor-qta.
           if como-prezzo = zero
              move space           to r-stof-prz-tot-finale
           else
      *        move como-prezzo     to como-pic-97v92
      *        move como-pic-97v92  to r-stof-prz-tot-finale(1:13)
      *        move "€"             to r-stof-prz-tot-finale(14:1)
      *        move space  to r-stof-prz-tot-finale
      *        move como-prezzo(12:2)  to como-xx
      *        if como-xx not = "00"
      *           move como-prezzo  to como-pic-97v94
      *           move como-pic-97v94     to r-stof-prz-tot-fin|r-stof-prz-tot-finale(1:13)
      *        else

                 add 0,0005              to como-prezzo
                 move como-prezzo        to como-prezzo-3dec
                 add 0,005               to como-prezzo-3dec
                 move como-prezzo-3dec   to como-prezzo-2dec
                 move como-prezzo-2dec   to como-prezzo


                 move como-prezzo  to como-pic-97v92
                 move como-pic-97v92     to r-stof-prz-tot-fin|r-stof-prz-tot-finale(3:12)
      *        end-if
              call "C$JUSTIFY" using r-stof-prz-tot-fin, "R"
              move "€"                   to r-stof-prz-tot-fin-e
           end-if.

           compute totale = totale + como-prezzo.

           perform varying idx from 1 by 1 until idx > 10
              if tge-cod-iva-std = cod-iva(idx)
                 add como-prezzo   to imp-iva(idx)
                 exit perform
              else
                 if cod-iva(idx) = space
                    move tge-cod-iva-std to cod-iva(idx)
                    move como-prezzo     to imp-iva(idx)
                    exit perform
                 end-if
              end-if
           end-perform.     
                             
           move aor-qta               to como-pic-z7.
           move como-pic-z7           to r-stof-qta.
           perform SCRIVI-RIGA-CSV.
           move spaces to r-stof-qta.

           move r-stof-riga    to spl-riga-stampa.

           set  spl-oggetto    to true.
           set  SPL-linea      to true.
           set  spl-brush-null to true.
           set  spl-nero       to true.

           move spl-riga-fine  to spl-riga.
           add 0,4             to spl-riga-fine.
           perform RIGHE-VERTICALI.

      *    riga orizzontale
           move spl-riga-fine to spl-riga
           move spl-riga      to spl-riga-fine
           move 0,1           to spl-colonna.
           move 28,5          to spl-colonna-fine.
           call "spooler"  using spooler-link

           set spl-stringa   to true.
           move 1            to spl-tipo-colonna
           move arial6       to spl-hfont.
           move 1            to spl-tipo-colonna
           subtract 0,3    from spl-riga.
           call "spooler" using spooler-link.

           initialize r-stof-riga.
           move aor-qta               to como-pic-z7.
           move como-pic-z7           to r-stof-qta.

           move r-stof-riga  to spl-riga-stampa
           move arial6b      to spl-hfont.
           move 1            to spl-tipo-colonna.
           call "spooler" using spooler-link.

      ***---
       ASPETTA-PDF.
      *****     move stof-path-pdf to como-nome-file.
      *****     move 0 to cont.
      *****     inspect como-nome-file 
      *****             tallying cont for characters before ")".
      *****     move ".pdf " to como-nome-file(cont + 1: 5).
      *****
      *****     set trovato to false.
      *****     perform 60 times
      *****        CALL "C$FILEINFO" USING stof-path-pdf,
      *****                                file-info, 
      *****                         GIVING status-code
      *****        if status-code = 0
      *****           set trovato to true
      *****           exit perform
      *****        else
      *****           CALL "C$FILEINFO" USING como-nome-file,
      *****                                   file-info, 
      *****                            GIVING status-code
      *****           if status-code = 0
      *****              move como-nome-file to stof-path-pdf
      *****              set trovato to true
      *****              exit perform
      *****           end-if
      *****        end-if
      *****        call "c$sleep" using 1
      *****     end-perform.
      *****
      *****     if trovato
      *****        move 0  to old-size
      ******       aspetto finchè non esiste fisicamente il file
      *****        move 99 to minuti-partenza
      *****        perform until 1 = 2
      *****           CALL "C$FILEINFO" USING stof-path-pdf,
      *****                                   file-info, 
      *****                            GIVING status-code
      *****        
      *****           if status-code = 0
      *****              if FILE-SIZE not = 0
      *****                 if FILE-SIZE = old-size
      *****                    exit perform
      *****                 else
      *****                    move FILE-SIZE to old-size
      *****                    call "c$sleep" using 1
      *****                 end-if
      *****              end-if
      *****           else
      *****              perform TIME-OUT
      *****              if time-out-exit
      *****                 exit perform
      *****              end-if
      *****           end-if
      *****        
      *****        end-perform
      *****     end-if.

           set settaPDF-resetta   to true.
           call   "settaPDF2" using settaPDF-linkage
           cancel "settaPDF2".

           if settaPDF-OK

              |Controllo che il numero contenuto nel nome del file
              |corrisponde con quello stampato
              move 0 to como-chars como-idx
              move settaPDF-nome-file to como-path
              inspect como-path replacing trailing spaces by low-value
              inspect como-path tallying como-chars 
                      for characters before low-value
              perform varying como-idx from como-chars by -1 
                        until como-idx = 1
                 if como-path(como-idx:1) = "\"
                    exit perform
                 end-if
              end-perform
              move 0 to como-chars
              perform until 1 = 2
                 add 1 to como-idx
                 if como-path(como-idx:1) = "_"
                    exit perform
                 end-if
                 add 1 to como-chars
                 move como-path(como-idx:1) to como-num-x(como-chars:1)
              end-perform
              call "C$JUSTIFY" using como-num-x, "R"
              inspect como-num-x replacing leading x"20" by x"30"
              move como-num-x to como-num-n
              if como-num-n not = stof-tof-numero
                 move spaces to stof-path-pdf
                 move 0      to stof-tof-numero
              else
                 move settaPDF-nome-file to stof-path-pdf
              end-if
           end-if.

      ********---
      ***** TIME-OUT.
      ******    tengo 2 minuti di time-out
      *****     set time-out-exit to false.
      *****     accept como-time from time.
      *****
      *****     if minuti-partenza = 99
      *****        move como-time(3:2)  to minuti-partenza
      *****     end-if.
      *****
      *****     move como-time(3:2)  to minuti-arrivo.
      *****
      *****     if minuti-arrivo < minuti-partenza
      *****        add 60 to minuti-arrivo
      *****     end-if.
      *****     subtract minuti-partenza from minuti-arrivo.
      *****
      *****     if minuti-arrivo >= 3
      *****        set time-out-exit to true
      *****     else
      *****        call "c$sleep" using 1
      *****     end-if.

      ***---
       DESCRIZIONE-PROMO.
           move tpr-gdo  to gdo-codice.
           read tgrupgdo 
                invalid initialize gdo-intestazione 
           end-read.

           inspect gdo-intestazione 
                                   replacing trailing space by low-value
           inspect tpr-descrizione replacing trailing space by low-value

           string gdo-intestazione        delimited by low-value
                  " - "                   delimited by size
                  tpr-descrizione         delimited by low-value
                  " - dal "               delimited by size
                  tpr-ini-volantino(7:2)  delimited by size
                  "/"                     delimited by size
                  tpr-ini-volantino(5:2)  delimited by size
                  "/"                     delimited by size
                  tpr-ini-volantino(1:4)  delimited by size
                  " al "                  delimited by size
                  tpr-fine-volantino(7:2) delimited by size
                  "/"                     delimited by size
                  tpr-fine-volantino(5:2) delimited by size
                  "/"                     delimited by size
                  tpr-fine-volantino(1:4) delimited by size
             into spl-riga-stampa.
           move spl-riga-stampa to como-promo.

      ***---
       STAMPA-LISTA-ARTICOLI.
           if stof-path-art-no-listforn = space
              exit paragraph
           end-if

           move zero   to riga
           perform APRI-STAMPA
           if spl-sta-annu
              exit paragraph
           end-if
           set prima-volta to false
           perform STAMPA-TESTA-ARTICOLI.

           move stof-path-art-no-listforn   to wstampa
           open input lineseq
           perform until 1 = 2
              initialize line-riga
              read lineseq next
                 at end
                    exit perform
              end-read
              perform SCRIVI-RIGA-ART

           end-perform.
           close lineseq.
           move 0 to num-pagina riga.

      ***---
       SCRIVI-RIGA-ART.
           add 1 to riga
           if riga > MaxRighe-articoli
              perform SALTO-PAGINA-ARTICOLI
              move 1   to riga
           end-if.

           move line-riga(1:6)  to ra-codice
           move line-riga(8:50) to ra-descrizione
           move riga-articoli   to spl-riga-stampa

           move 1,5          to spl-colonna
           add 0,4           to spl-riga
           set spl-stringa   to true.
           move courier12    to spl-hfont.

           move zero         to spl-tipo-colonna
           call "spooler" using spooler-link.

      ***---
       STAMPA-TESTA-ARTICOLI.
           move arial6       to spl-hfont.
           set spl-stringa to true
           move 0      to spl-riga
           move spaces to spl-riga-stampa
           call "spooler" using spooler-link.


           add 1 to num-pagina
           move 0        to riga.
      *     perform FINCATURA.

      *    logo 
           move logo-handle to spl-hbitmap
           set  spl-bitmap  to true
           move 2,0 to spl-riga
           move 6,5 to spl-colonna
           move 1,2 to spl-bitmap-height
           move 2,8 to spl-bitmap-width
           call "spooler" using spooler-link.

      *    dati Lubex
           perform DATI-LUBEX.

           move Arial14BI             to spl-hfont.
           move 1,5                   to spl-riga.
           move 9,0                   to spl-colonna.
           move 
            "Elenco articoli non ordinati in quanto privi di listino"
                    to spl-riga-stampa
           call "spooler" using spooler-link.

      ***     add 0,8                 to spl-riga
           set spl-nero            to true.
           move 20                 to spl-pen-with.
           move 0,6                to spl-colonna.
           move 28,5            to spl-colonna-fine.
           move 2,1   to spl-riga 
                         spl-riga-fine.
           set  spl-oggetto        to true.
           set  spl-linea          to true.
           set  spl-pen-solid      to true.
           call "spooler"       using spooler-link.

           add 16,6       to spl-riga 
           move spl-riga  to spl-riga-fine.
quii       call "spooler"       using spooler-link.

      *    riga Ordine
           set spl-stringa            to true.

      *    
           add 0,1 to spl-riga
           move arial6       to spl-hfont.
           move 27,3  to spl-colonna
      *     move 0,6  to spl-colonna
           initialize spl-riga-stampa
           move num-pagina   to num-pag-ed
           string "PAGINA "  delimited by size
                  num-pag-ed delimited by size
                  into spl-riga-stampa
           call "spooler" using spooler-link.

      *    posizionamento per partenza righe
           move 2,0 to spl-riga.

      ***---
       SCRIVI-RIGA-CSV.
           if not stampa-csv exit paragraph end-if.
           initialize csv-riga.
           string r-stof-art            delimited size
                  separatore            delimited size
                  r-stof-des            delimited size
                  separatore            delimited size      
                  R-STOF-COD-FORN       delimited size
                  separatore            delimited size
                  r-stof-qta            delimited size
                  separatore            delimited size
                  r-stof-imb            delimited size
                  separatore            delimited size
                  r-stof-peso-netto     delimited size
                  separatore            delimited size
                  r-stof-peso-tot-netto delimited size
                  separatore            delimited size
                  r-stof-peso-tot-utf   delimited size
                  separatore            delimited size
                  r-stof-prz-list       delimited size
      *****            separatore            delimited size
      *****            r-stof-prz-list-e     delimited size
                  separatore            delimited size
                  r-stof-sconto-1       delimited size
                  separatore            delimited size
      *****            r-stof-perce-1        delimited size
      *****            separatore            delimited size
                  r-stof-sconto-2       delimited size
                  separatore            delimited size
      *****            r-stof-perce-2        delimited size
      *****            separatore            delimited size
                  r-stof-sconto-3       delimited size
                  separatore            delimited size
      *****            r-stof-perce-3        delimited size
      *****            separatore            delimited size
                  r-stof-sconto-4       delimited size
                  separatore            delimited size
      *****            r-stof-perce-4        delimited size
      *****            separatore            delimited size
                  r-stof-sconto-5       delimited size
                  separatore            delimited size
      *****            r-stof-perce-5        delimited size
      *****            separatore            delimited size
                  r-stof-prz-net        delimited size
                  separatore            delimited size
      *****            r-stof-prz-net-e      delimited size
      *****            separatore            delimited size
                  r-stof-imp-cons       delimited size
                  separatore            delimited size
      *****            r-stof-imp-cons-e     delimited size
      *****            separatore            delimited size
                  r-stof-imp-cou        delimited size
                  separatore            delimited size
      *****            r-stof-imp-cou-e      delimited size
      *****            separatore            delimited size
                  r-stof-add-pb         delimited size
                  separatore            delimited size
      *****            r-stof-add-pb-e       delimited size
      *****            separatore            delimited size
                  r-stof-costi-agg      delimited size
                  separatore            delimited size
      *****            r-stof-costi-aggi-e   delimited size
      *****            separatore            delimited size
                  r-stof-prz-fin        delimited size
                  separatore            delimited size
      *****            r-stof-prz-fin-e      delimited size
      *****            separatore            delimited size
                  r-stof-prz-tot-fin    delimited size
                  separatore            delimited size
      *****            r-stof-prz-tot-fin-e  delimited size
      *****            separatore            delimited size
             into csv-riga
           end-string.
           write csv-riga.


      ***---
       SALTO-PAGINA-ARTICOLI.
           set spl-salto-pagina     to true.
           call "spooler"        using spooler-link.                                                            
      *     set spl-stringa to true
      *     move 0      to spl-riga
      *     move spaces to spl-riga-stampa
      *     call "spooler" using spooler-link.
           perform STAMPA-TESTA-ARTICOLI
           move 0 to riga.  
                  

E5190 ***---
       SPLIT-RIGA. 
           move  0 to numRighe resto totChar
           inspect mag-indirizzo replacing trailing spaces by low-value
           inspect mag-indirizzo tallying totChar 
               for characters before low-value
           inspect mag-indirizzo replacing trailing low-value by spaces
           initialize tab-word 
                      tab-rows replacing numeric data by zeroes
                                    alphanumeric data by spaces
           if totChar > 50
              move 0 to startWord
              move 1 to iWord
              move 0 to numChar
              perform varying idxChar from 1 by 1 
                        until idxChar > totChar
                 add 1 to numChar
                 if mag-indirizzo(idxChar:1) = space 
                    move 0 to startWord numChar
                    add  1 to iWord
                    exit perform cycle 
                 end-if
                 if numChar > 50
                    move 0 to startWord numChar
                    add  1 to iWord
                    subtract 1 from idxChar
                    exit perform cycle 
                 end-if
                 add 1 to startWord
                 move mag-indirizzo(idxChar:1)
                   to el-wordValue(iWord)(startWord:1)
                 move idxChar to el-wordEnd(iWord)
              end-perform
              move iWord to totWord
              move 1 to rowToWrite iRow
              move 1 to startChar 
              move 0 to sumChar
              perform varying iWord from 1 by 1 
                        until iWord > totWord
                 move 0 to totChar
                 inspect el-wordValue(iWord) tallying totChar 
                         for characters before spaces

                 if sumChar = 0
                    move totChar to sumChar
                 else  
                    compute sumChar = sumChar + totChar + 1
                 end-if

                 if sumChar  > 53
                    if el-rowToWrite(iRow) = spaces
                       move el-wordValue(iWord) to el-rowToWrite(iRow) 
                    else
                       subtract 1 from iWord
                    end-if
                    add 1 to iRow
                    move 1 to startChar
                    move 0 to sumChar
                 else  
                    move el-wordValue(iWord) 
                      to el-rowToWrite(iRow) 
                        (startChar : totChar)
                    compute startChar = startChar + totChar + 1
                 end-if
              end-perform

              move iRow to numRighe

           else  
              move mag-indirizzo to el-rowToWrite(1)
              move 1 to numRighe
           end-if.
