       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      stgiang.
       AUTHOR.                          Luciano.
      ******************************************************************

       SPECIAL-NAMES. 
           decimal-point is comma.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tmp-giang.sl".
           copy "articoli.sl".
           copy "tgrupgdo.sl".
           copy "clienti.sl".


      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tmp-giang.fd".
           copy "articoli.fd".
           copy "tgrupgdo.fd".
           copy "clienti.fd".

       WORKING-STORAGE SECTION. 
           copy "acucobol.def".
           copy "acugui.def".
           copy "spooler.def".
           copy "fonts.def".
           copy "selprint.lks".

       01  save-tgia-chiave.
           10 save-tgia-cod-articolo            PIC  9(6).
           10 save-tgia-tipo-rec    PIC  x.
               88 save-tgia-articolo VALUE IS "A". 
               88 save-tgia-ordfor VALUE IS "F". 
               88 save-tgia-master VALUE IS "M". 
           10 save-tgia-master-ordf.
               15 save-tgia-testa-ordf.
                   20 save-tgia-anno        PIC  9(4).
                   20 save-tgia-numero      PIC  9(8).
               15 save-tgia-riga        PIC  9(5).

       77  como-ora pic 9(8).

       78  titolo             value "Stampa Giang".

       77  status-tmp-giang     pic xx.
       77  status-articoli      pic xx.
       77  status-tgrupgdo      pic xx.
       77  status-clienti       pic xx.
       77  idx                  pic 9(5).
       77  path-tmp-giang       pic x(256).

       77  messaggio          pic X(150) value spaces.
       77  font-size-dply     pic Z(5).
       77  WFONT-STATUS       pic s9(5) value 0.

       77  como-data          pic 9(8).


       01  riga-articolo.
           05 ra-articolo    PIC  x(40).
           05 ra-inevaso     PIC  x(13).
           05 ra-ord-gdo     PIC  x(30).
      *     05 ra-ordini      pic x(100).

       01  riga-ord.
           10 ro-chiave-ordf PIC  x(15).
           10 ro-qta         PIC  x(10).
           10 ro-qtap-dt-ord PIC  x(10).
           10 ro-dt-arr-cons PIC  x(10).
           10 ro-note-promo  PIC  x(100).
           10 ro-for-cli     PIC  x(50).
           10 ro-destino-CF  PIC  x(50).

       01  riga-master.
           05 rm-master   PIC  x(13).
           05 rm-gdo      PIC  x(30).
           05 rm-qta      PIC  x(10).
           05 rm-dt-ord   PIC  x(10).
           05 rm-dt-cons  PIC  x(10).
           05 rm-promo    PIC  x(100).
           05 rm-cli      PIC  x(50).
           05 rm-dest     PIC  x(50).
           05 rm-azione   PIC  x(15).



       77  Arial14BI     handle of font.
       77  Arial10b      handle of font.
       77  Arial7        handle of font.
       77  Arial7b       handle of font.
       77  arial5B       handle of font.
       77  arial5        handle of font.
       77  arial6        handle of font.


       77  MaxRighe           pic 99,99.

       01  filler             pic 9.
         88 Prima-Volta       value 1, false 0.

       01  filler             pic 9.
         88 primo-passaggio   value 1, false 0.

         
       01  controlli          pic xx.
         88 tutto-ok          value "OK".
         88 errori            value "ER".


       77  num-pagina           pic 9(3)    value zero.
       77  num-pag-ed           pic z(3)    value zero.
       77  pronostico           pic 99,99.
       77  logo-handle          handle of bitmap.


       77  como-riga            pic 9(3).
       77  como-colonna         pic 9(3).
       77  art-ed               pic z(6).

       LINKAGE SECTION.
           copy "link-stgiang.def".

      ******************************************************************
       PROCEDURE DIVISION using stgiang-linkage.

       DECLARATIVES.
       END DECLARATIVES.


      ***--- 
       MAIN-PRG.
           perform INIT.
           if tutto-ok
              perform OPEN-FILES
              perform ELABORAZIONE
              perform CLOSE-FILES
           end-if
           perform EXIT-PGM.

      ***---
       INIT.
           move 18,4   to MaxRighe.

           accept como-data from century-date
           accept como-ora  from time
       
           COPY RESOURCE "logo-st.bmp".

           set tutto-ok   to true.
           set Prima-Volta to true.

           set environment "PRINTER" to "-P SPOOLER"
           CALL "w$bitmap" USING  WBITMAP-LOAD "logo-st.BMP", 
                           GIVING logo-handle
           move stgiang-path-tmp   to path-tmp-giang.

      ***---
       OPEN-FILES.
           open input tmp-giang  
           open input articoli.
           open input tgrupgdo.
           open input clienti.

      ***---
       ELABORAZIONE.
           move 90   to spl-riga
           perform STAMPA-RIGHE
           if not prima-volta
              set spl-chiusura to true
              call   "spooler" using spooler-link
              cancel "spooler"
           end-if.

      ***---
       STAMPA-INTESTAZIONE.
           add 1 to num-pagina

           perform FINCATURA.
           perform RIQUADRO-FILTRI.
           perform RIQUADRO-ARTICOLI.

           set spl-stringa   to true.
           move arial6       to spl-hfont.
           move 2,2          to spl-riga.
           move 17 to spl-colonna

           move 18,9  to spl-riga

           move arial6       to spl-hfont.
           move 27,3  to spl-colonna
           initialize spl-riga-stampa
           move num-pagina   to num-pag-ed
           string "PAGINA "  delimited by size
                  num-pag-ed delimited by size
                  into spl-riga-stampa
           call "spooler" using spooler-link.

      *    posizionamento per partenza righe
           move 4,30 to spl-riga.
           move arial10b   to spl-hfont.
           move 1   to spl-tipo-colonna

           move "Articolo"   to ra-articolo
           move "Inevaso"    to ra-inevaso
           move "Ord(6)"     to ra-ord-gdo
      *     move space        to ra-ordini

           set spl-stringa   to true.
           move riga-articolo  to spl-riga-stampa.
           call "spooler" using spooler-link. 

           add 0,10 to spl-riga.

      ***---
       RIQUADRO-FILTRI.
           move 8     to spl-pen-with.
           move 2,1  to spl-riga.
           add 2,0  to spl-riga giving spl-riga-fine

           move 0,9                to spl-colonna.
           add 13,0 to spl-colonna giving spl-colonna-fine.

           set  spl-oggetto           to true.
           set  SPL-RETTANGOLO-ROUND  to true.
           set  spl-brush-null        to true.
           set  spl-nero              to true.
           call "spooler"         using spooler-link.

           set  SPL-linea to true.
           move 4         to spl-pen-with.
           add 3,2        to spl-colonna
           subtract 0,1   from spl-colonna-fine
      
      *    righe interne
           add 0,3        to spl-riga
           perform 5 times
              add 0,3        to spl-riga
              move spl-riga  to spl-riga-fine
              call "spooler"         using spooler-link
           end-perform.

           subtract 3,1        from spl-colonna

           set spl-stringa   to true.
           move arial6       to spl-hfont.
           subtract 1,75      from spl-riga.
      *     subtract 0,6      from spl-colonna.
           move "FILTRI DI VISUALIZZAZIONE"  to spl-riga-stampa
           call "spooler" using spooler-link.

           move "Data di Consegna:"   to spl-riga-stampa
           add 0,3  to spl-riga
           call "spooler" using spooler-link.

           move "Promo: "             to spl-riga-stampa
           add 0,3  to spl-riga
           call "spooler" using spooler-link.

           move "Gruppo GDO: "        to spl-riga-stampa
           add 0,3  to spl-riga
           call "spooler" using spooler-link.

           move "Cliente: "           to spl-riga-stampa
           add 0,3  to spl-riga
           call "spooler" using spooler-link.

           move "Data di consegna successiva al: "   to spl-riga-stampa
           add 0,3  to spl-riga
           call "spooler" using spooler-link.

           subtract 1,5  from spl-riga
           add 3,2        to spl-colonna

           initialize spl-riga-stampa
           inspect cli-localita replacing trailing space by low-value
           string "dal "  delimited by size
                  stgiang-dt-cons-da        delimited by low-value
                  " al "                    delimited by size
                  stgiang-dt-cons-a         delimited by size
                  into spl-riga-stampa
           add 0,3  to spl-riga
           call "spooler" using spooler-link.

           move stgiang-promo   to spl-riga-stampa
           add 0,3  to spl-riga
           call "spooler" using spooler-link.

           move stgiang-gdo  to gdo-codice
           read tgrupgdo
              invalid
                 initialize gdo-intestazione 
           end-read
           move gdo-intestazione to spl-riga-stampa
           add 0,3  to spl-riga
           call "spooler" using spooler-link.

           move stgiang-cli  to cli-codice
           set cli-tipo-c    to true
           read clienti
              invalid
                 initialize cli-ragsoc-1
           end-read
           move cli-ragsoc-1  to spl-riga-stampa
           add 0,3  to spl-riga
           call "spooler" using spooler-link.

           move stgiang-data to spl-riga-stampa
           add 0,3  to spl-riga
           call "spooler" using spooler-link.

      ***---
       RIQUADRO-ARTICOLI.
           move 8     to spl-pen-with.
           move 2,1  to spl-riga.
           add 2,0  to spl-riga giving spl-riga-fine

           move 14,5                to spl-colonna.
           add 13,0 to spl-colonna giving spl-colonna-fine.

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
           add 0,3        to spl-riga
           perform 5 times
              add 0,3        to spl-riga
              move spl-riga  to spl-riga-fine
              call "spooler"         using spooler-link
           end-perform.

           set spl-stringa   to true.
           move arial6       to spl-hfont.
           subtract 1,75      from spl-riga.
           move "FILTRI ARTICOLI"  to spl-riga-stampa
           call "spooler" using spooler-link.

           move 1      to como-riga
           move zero   to como-colonna
           initialize spl-riga-stampa
           move 2   to spl-tipo-colonna
           perform varying idx from 1 by 1 until idx > 10000
              if stgiang-el-articolo(idx) = zero
                 exit perform
              end-if
              if como-riga > 5
                 exit perform
              end-if
              add 1 to como-colonna
              if como-colonna > 11
                 add 0,3  to spl-riga
                 inspect spl-riga-stampa 
                                replacing trailing low-value by space
                 call "spooler" using spooler-link
                 initialize spl-riga-stampa
                 add 1 to como-riga
                 move 1   to como-colonna
              end-if

              move stgiang-el-articolo(idx) to art-ed
              inspect spl-riga-stampa 
                             replacing trailing space by low-value
              string spl-riga-stampa  delimited by low-value
                     art-ed           delimited by size
                     into spl-riga-stampa
           end-perform.
           if como-riga < 5
              inspect spl-riga-stampa 
                                replacing trailing low-value by space
              add 0,3  to spl-riga
              call "spooler" using spooler-link
           end-if.
                 

           move zero   to spl-tipo-colonna.

      ***---
       FINCATURA.
      *    dati Lubex
           perform DATI-LUBEX.

      *    logo 
           move logo-handle to spl-hbitmap
           set  spl-bitmap  to true
           move 2,0 to spl-riga
           move 6,5 to spl-colonna
           move 1,2 to spl-bitmap-height
           move 2,8 to spl-bitmap-width
           call "spooler" using spooler-link.

      *    riga Ordine
           set spl-stringa            to true.
           move Arial14BI             to spl-hfont.
           move 1,4                   to spl-riga.
           move 17,0                  to spl-colonna.
           move "STAMPA GIANG"   to spl-riga-stampa
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

           move 21,6               to spl-colonna.
           move 28,5               to spl-colonna-fine.
           call "spooler"       using spooler-link.

           add 2,4   to spl-riga 
           move spl-riga  to spl-riga-fine.
           move 0,6                to spl-colonna.
           move 28,5               to spl-colonna-fine.
           call "spooler"       using spooler-link.


           add 14,6       to spl-riga 
           move 28,5      to spl-colonna-fine.
           move spl-riga  to spl-riga-fine.
           call "spooler"       using spooler-link.

           move 4         to spl-pen-with.
           move 4,7       to spl-riga 
           move spl-riga  to spl-riga-fine.
           call "spooler"       using spooler-link.

      ***---
       STAMPA-RIGHE.
           move low-value to tgia-rec.
           set tgia-articolo to true.
           |start tmp-giang key >= tgia-k-articolo
           start tmp-giang key >= tgia-k-forn
              invalid 
                 continue
              not invalid
                 perform until 1 = 2
                    read tmp-giang next 
                       at end      
                          exit perform 
                    end-read
                    if tgia-ordfor or tgia-master 
                       exit perform 
                    end-if

                    perform SCRIVI
                    if prima-volta
                       exit perform
                    end-if
                 end-perform
           end-start.

      ***---
       SCRIVI.
           perform SCRIVI-RIGA-ARTICOLO
           if prima-volta
              exit paragraph
           end-if
      
           set primo-passaggio  to true
           evaluate tgia-tot-ordfor
           when zero
                continue
           when 1
                move tgia-anno-ordf   to tgia-anno
                move tgia-numero-ordf to tgia-numero 
                perform SCRIVI-RIGA-ORDF
           when other
                continue
           end-evaluate.

           perform APRI-RIGHE.

      *    riga separazione articoli
           add 0,4   to spl-riga 
           perform LINEA-DIVISORIA-1.

      ***---
       LINEA-DIVISORIA-1.
           set spl-nero         to true.
           set  spl-oggetto     to true.
           set  spl-linea       to true.
           set  SPL-PEN-solid   to true.

           move 15              to spl-pen-with.
           move spl-riga        to spl-riga-fine.
           move 0,6             to spl-colonna.
           move 28,5            to spl-colonna-fine.
           call "spooler"       using spooler-link.

      ***---
       LINEA-DIVISORIA.
           set spl-nero         to true.
           set  spl-oggetto     to true.
           set  spl-linea       to true.
           set  SPL-PEN-solid   to true.

           move 4               to spl-pen-with.
           move spl-riga        to spl-riga-fine.
           move 0,6             to spl-colonna.
           move 28,5            to spl-colonna-fine.
           call "spooler"       using spooler-link.

      ***---
       SCRIVI-RIGA-ARTICOLO.
           add 1,5 to spl-riga giving pronostico
      
           if pronostico > maxrighe
              perform SALTO-PAGINA
           end-if.
           if prima-volta
              exit paragraph
           end-if

           initialize riga-articolo.

           move tgia-cod-articolo to art-codice 
           initialize ra-articolo
           read articoli no lock invalid continue end-read.
           move tgia-cod-articolo to ra-articolo
           call "C$JUSTIFY" using ra-articolo, "L"
           inspect ra-articolo replacing leading x"30" by x"20"
           call "C$JUSTIFY" using ra-articolo, "L"
           inspect ra-articolo  replacing trailing spaces by low-value
           string  ra-articolo  delimited low-value
                  " - "           delimited size
                  art-descrizione delimited size
                  into ra-articolo
           end-string.

           move tgia-inevaso      to ra-inevaso    
           call "C$JUSTIFY"    using ra-inevaso, "L"
           inspect ra-inevaso replacing leading x"30" by x"20"

           move tgia-ordinato     to ra-ord-gdo
           call "C$JUSTIFY"    using ra-ord-gdo, "L"
           inspect ra-ord-gdo replacing leading x"30" by x"20".

      *     if tgia-tot-ordfor = 0
      *        move "*** NON RISULTANO ORDINI AL FORNITORE SU QUESTO CODI
      *-        "CE ARTICOLO ***" to ra-ordini
      *     end-if.

           move 1       to spl-tipo-colonna
           add  0,40    to spl-riga.
           move Arial10b to spl-hfont

           set spl-stringa   to true.
           move riga-articolo  to spl-riga-stampa.
           call "spooler" using spooler-link. 


           if tgia-tot-ordfor = 0
              add 0,1     to spl-riga
              move "*** NON RISULTANO ORDINI AL FORNITORE SU QUESTO CODI
      -        "CE ARTICOLO ***" to spl-riga-stampa
              move zero      to spl-tipo-colonna
              move Arial7B   to spl-hfont
              move 16,70     to spl-colonna
              set spl-stringa   to true
              call "spooler" using spooler-link
              subtract 0,1  from spl-riga

           end-if.


      ***---
       APRI-RIGHE.
           move tgia-chiave  to save-tgia-chiave

           if tgia-tot-ordfor > 1
              set tgia-ordfor       to true
              move save-tgia-cod-articolo to tgia-cod-articolo
              start tmp-giang key >= tgia-k-articolo
                 invalid 
                    continue
                 not invalid
                    perform until 1 = 2
                       read tmp-giang next 
                          at end 
                             exit perform 
                       end-read
                       if tgia-cod-articolo 
                                      not = save-tgia-cod-articolo or
                          tgia-articolo                            or
                          tgia-master
                          exit perform
                       end-if
                       perform SCRIVI-RIGA-ORDF
                    end-perform
              end-start
           end-if.

           set primo-passaggio  to true

           set tgia-master      to true
           move save-tgia-cod-articolo to tgia-cod-articolo
           start tmp-giang key >= tgia-k-articolo
              invalid 
                 continue
              not invalid
                 perform until 1 = 2
                    read tmp-giang next 
                       at end 
                          exit perform 
                    end-read
                    if tgia-cod-articolo not = save-tgia-cod-articolo or
                       tgia-articolo                            or
                       tgia-ordfor
                       exit perform
                    end-if

                    perform SCRIVI-RIGA-MASTER

                 end-perform
           end-start.
           move save-tgia-chiave  to tgia-chiave
           read TMP-GIANG key is tgia-k-articolo
           read TMP-GIANG key is tgia-k-forn
              invalid
                 continue
           end-read.

      ***---
       SCRIVI-RIGA-ORDF.
           if primo-passaggio
              set primo-passaggio  to false
              perform INTESTA-ORDF
           end-if

           add 0,30 to spl-riga giving pronostico
           if pronostico > maxrighe
              perform SALTO-PAGINA
           end-if.

           initialize riga-ord

           move tgia-anno   to tgia-anno-ordf
           move tgia-numero to tgia-numero-ordf


           move tgia-qta         to ro-qta.
           call "C$JUSTIFY"   using ro-qta, "L".
           inspect ro-qta replacing leading x"30" by x"20".

           move tgia-qta-p          to ro-qtap-dt-ord.
           call "C$JUSTIFY"   using ro-qtap-dt-ord, "L".
           inspect ro-qtap-dt-ord replacing leading x"30" by x"20".
           move tgia-dt-arrivo(1:4) to ro-dt-arr-cons(7:4).
           move "/"                 to ro-dt-arr-cons(6:1).
           move tgia-dt-arrivo(5:2) to ro-dt-arr-cons(4:2).
           move "/"                 to ro-dt-arr-cons(3:1).
           move tgia-dt-arrivo(7:2) to ro-dt-arr-cons(1:2).
           move tgia-note           to ro-note-promo.
           move tgia-ragsoc         to ro-for-cli    .
           move tgia-destino        to ro-destino-CF.
           inspect ro-destino-CF replacing all low-value by space
           initialize ro-chiave-ordf.
           string "00"             delimited size
                  tgia-numero-ordf delimited size
                  "-"              delimited size
                  tgia-anno-ordf   delimited size
                  into ro-chiave-ordf
           end-string.
           inspect ro-chiave-ordf replacing leading x"30" by x"20".

           move 3            to spl-tipo-colonna
           move 0,5          to spl-colonna
           move Arial7       to spl-hfont

           add 0,30          to spl-riga
           set spl-stringa   to true.
           move riga-ord  to spl-riga-stampa.
           call "spooler" using spooler-link. 

      ***---
       SCRIVI-RIGA-MASTER.
           if primo-passaggio
              set primo-passaggio  to false
              perform INTESTA-MASTER
           end-if

           add 0,30 to spl-riga giving pronostico
           if pronostico > maxrighe
              perform SALTO-PAGINA
           end-if.

           initialize riga-master
           string tgia-numero delimited size
                  "-"         delimited size
                  tgia-anno   delimited size
                  into rm-master
           end-string
           inspect rm-master replacing leading x"30" by x"20"
           move tgia-gdo to rm-gdo

           move tgia-qta         to rm-qta
           call "C$JUSTIFY"   using rm-qta, "L"
           inspect rm-qta replacing leading x"30" by x"20"

           move tgia-dt-ord(1:4) to rm-dt-ord(7:4)
           move "/"              to rm-dt-ord(6:1)
           move tgia-dt-ord(5:2) to rm-dt-ord(4:2)
           move "/"              to rm-dt-ord(3:1)
           move tgia-dt-ord(7:2) to rm-dt-ord(1:2)

           move tgia-dt-cons(1:4) to rm-dt-cons(7:4)
           move "/"               to rm-dt-cons(6:1)
           move tgia-dt-cons(5:2) to rm-dt-cons(4:2)
           move "/"               to rm-dt-cons(3:1)
           move tgia-dt-cons(7:2) to rm-dt-cons(1:2)

           move tgia-des-promo    to rm-promo
           move tgia-ragsoc       to rm-cli
           move tgia-destino      to rm-dest
           evaluate true
           when tgia-attesa          
                move "RESTA IN ATTESA" to rm-azione
           when tgia-tagliare-merce  
                move "TAGLIARE MERCE"  to rm-azione
           when tgia-tenere-saldo    
                move "TENERE SALDO"    to rm-azione
           when tgia-sostituzione    
                move "SOSTITUZIONE"    to rm-azione
           end-evaluate

           move 4            to spl-tipo-colonna
           move 0,5          to spl-colonna
           move Arial7       to spl-hfont

           add 0,30          to spl-riga
           set spl-stringa   to true.
           move riga-master  to spl-riga-stampa.
           call "spooler" using spooler-link. 

      ***---
       INTESTA-MASTER.
           add 1,00 to spl-riga giving pronostico
           if pronostico > maxrighe
              perform SALTO-PAGINA
           end-if.

           move "Master"     to rm-master 
           move "GDO"        to rm-gdo    
           move "Qta"        to rm-qta    
           move "Dt Ordine"  to rm-dt-ord 
           move "Dt Cons"    to rm-dt-cons
           move "Promo"      to rm-promo  
           move "Cliente"    to rm-cli    
           move "Destino"    to rm-dest   
           move "AZIONE"     to rm-azione 

           move 4    to spl-tipo-colonna
           move 0,5     to spl-colonna
           move Arial7b to spl-hfont

           set spl-stringa   to true.
           move riga-master  to spl-riga-stampa.
           add 0,50       to spl-riga
           call "spooler" using spooler-link. 

      *    riga separazione articoli
           add 0,3   to spl-riga 
           perform LINEA-DIVISORIA.

           subtract 0,2  from spl-riga.

      ***---
       INTESTA-ORDF.
           add 1,00 to spl-riga giving pronostico
           if pronostico > maxrighe
              perform SALTO-PAGINA
           end-if.
      
           move "Qta Ord"    to ro-qta         
           move "Qta Pre"    to ro-qtap-dt-ord 
           move "Dt Arrivo"  to ro-dt-arr-cons 
           move "Note"       to ro-note-promo  
           move "Fornitore"  to ro-for-cli     
           move "Destino"    to ro-destino-CF  
           move "Ord. For."  to ro-chiave-ordf 

           move 3    to spl-tipo-colonna
           move 0,5     to spl-colonna
           move Arial7b to spl-hfont

           set spl-stringa   to true.
           move riga-ord  to spl-riga-stampa.
           add 0,50       to spl-riga
           call "spooler" using spooler-link. 

      *    riga separazione articoli
           add 0,3   to spl-riga 
           perform LINEA-DIVISORIA.

           subtract 0,2  from spl-riga.


      ***---
       SALTO-PAGINA.
           if Prima-Volta
              initialize spooler-link

              move "Stampa GIANG" to spl-nome-job
      *    nuova selezione
              call   "selprint" using selprint-linkage
              cancel "selprint"

              if selprint-stampante not = space
                 move selprint-num-copie to SPL-NUM-COPIE
                 move selprint-stampante to SPL-NOME-STAMPANTE
      *    NUOVA SELEZIONE
                 set spl-apertura to true
                 set SPL-HORIZONTAL to true
                 set WFDEVICE-WIN-PRINTER    to true
                 call "spooler" using spooler-link
                 set Prima-Volta   to false
                 if spl-sta-annu 
                    exit paragraph 
                 else
                    perform CARICA-FONT
                 end-if
      *    NUOVA SELEZIONE
              else
                 set spl-sta-annu to true
              end-if
              if spl-sta-annu
                 display message box
                                   "Procedura interrotta dall'utente"
                        title = titolo
                        icon 2
              else
                 set prima-volta   to false
              end-if
           else
              set spl-salto-pagina     to true
              call "spooler"        using spooler-link
              set spl-stringa to true
              move 0      to spl-riga
              move spaces to spl-riga-stampa
              call "spooler" using spooler-link
           end-if.

           if not prima-volta
              perform STAMPA-INTESTAZIONE
           end-if.

      ***---
       CARICA-FONT.
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

           set tutto-ok             to true.
           initialize wfont-data.
           move 10                  to wfont-size.
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
                               Arial10b, wfont-data
                               giving wfont-status.
      
           if wfont-status = wfonterr-font-not-found
              set errori to true
              perform MESSAGGIO-ERR-FONT
           end-if.

           set tutto-ok             to true.
           initialize wfont-data.
           move 7                  to wfont-size.
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
                               Arial7, wfont-data
                               giving wfont-status.
      
           if wfont-status = wfonterr-font-not-found
              set errori to true
              perform MESSAGGIO-ERR-FONT
           end-if.

           set tutto-ok             to true.
           initialize wfont-data.
           move 7                  to wfont-size.
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
                               Arial7b, wfont-data
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

      ***---
       MESSAGGIO-ERR-FONT.
      * ISACCO (MESSAGGIO DI ERRORE ED USCITA SE NON TROVA UN FONT)
           initialize messaggio.

           inspect WFONT-NAME replacing trailing space by low-value.
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

           inspect messaggio replacing trailing space by low-value.

           display message messaggio.

      ***---
       CLOSE-FILES.
           close tmp-giang    
                 articoli
                 tgrupgdo
                 clienti.

      ***---
       EXIT-PGM.
           set environment "PRINTER" to "-P SPOOLER-DIRECT".
           destroy arial14bi.
           destroy arial10b.
           destroy Arial7.
           destroy arial5B.
           destroy arial5.
           destroy arial6.
           goback.

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
