       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      stsolleciti.
       AUTHOR.                          Luciano.
      ******************************************************************

       SPECIAL-NAMES. 
           decimal-point is comma.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tmp-sol.sl".

           COPY "tmp-sol.sl"
                REPLACING ==tmp-sol== BY ==tmp-sol1==,
                          ==status-tmp-solleciti-m== BY 
           ==status-tmp-sol1leciti-m==.
           COPY "tmp-sol.sl"
                REPLACING ==tmp-sol== BY ==tmp-sol2==,
                          ==status-tmp-solleciti-m== BY 
           ==status-tmp-sol2leciti-m==.
           copy "articoli.sl".
           copy "tgrupgdo.sl".
           copy "clienti.sl".
           copy "destini.sl".
           copy "mtordini.sl".
           copy "mrordini.sl".


      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tmp-sol.fd".

           COPY "tmp-sol.fd"
                REPLACING ==tmp-sol== BY ==tmp-sol1==,
                          ==status-tmp-solleciti-m== BY 
           ==status-tmp-sol1leciti-m==.
           COPY "tmp-sol.fd"
                REPLACING ==tmp-sol== BY ==tmp-sol2==,
                          ==status-tmp-solleciti-m== BY 
           ==status-tmp-sol2leciti-m==.

           copy "articoli.fd".
           copy "tgrupgdo.fd".
           copy "clienti.fd".
           copy "destini.fd".
           copy "mtordini.fd".
           copy "mrordini.fd".

       WORKING-STORAGE SECTION. 
           copy "acucobol.def".
           copy "acugui.def".
           copy "spooler.def".
           copy "fonts.def".
           copy "selprint.lks".

       01  save-sol-chiave.
           10 save-sol-data-ordine  PIC  9(8).
           10 save-sol-ordine.
               15 save-sol-anno         PIC  9(4).
               15 save-sol-numero       PIC  9(8).
           10 save-sol-prog-art     PIC  9(8).
           10 save-sol-prog-eva     PIC  9(8).
           10 save-sol-tipo         PIC  9.
               88 save-sol-testa VALUE IS 1. 
               88 save-sol-articolo VALUE IS 2. 
               88 save-sol-evas VALUE IS 3. 
       01 FILLER           PIC  9.
           88 evasa-tutto VALUE IS 1    WHEN SET TO FALSE  0. 
       77  tot-evasa        PIC  9(10).
       77  num-evasioni     PIC  999.
       77 como-evasa       PIC  9(8).

       77  como-ora pic 9(8).

       78  titolo             value "Stampa Solleciti".

       01 FILLER           PIC  9.
           88 FromFile VALUE IS 1    WHEN SET TO FALSE  0. 
           88 FromGrid VALUE IS 2    WHEN SET TO FALSE  0. 

       77  status-tmp-solleciti-m  pic xx.
       77  status-tmp-sol1leciti-m pic xx.
       77  status-tmp-sol2leciti-m pic xx.
       77  status-articoli         pic xx.
       77  status-tgrupgdo         pic xx.
       77  status-clienti          pic xx.
       77  status-destini          pic xx.
       77  status-mtordini         pic xx.
       77  status-mrordini         pic xx.
       77  idx                     pic 9(5).
       77  path-tmp-solleciti-m    pic x(256).

       77 evasioni-valide  PIC  9(3).
       77 articoli-validi  PIC  9(3).
       77 tot-idx-art      PIC  9(3).
       77 como-esito       PIC  x(20).

       78 78-tutti-vol VALUE IS "Tutti i volantini". 

       77 como-data-x8     PIC  x(8).



       77  messaggio          pic X(150) value spaces.
       77  font-size-dply     pic Z(5).
       77  WFONT-STATUS       pic s9(5) value 0.

       77  como-data          pic 9(8).

       01 FILLER           PIC  9.
           88 valuta VALUE IS 1    WHEN SET TO FALSE  0. 
           88 annulla VALUE IS 2    WHEN SET TO FALSE  0. 

       77  como-numero          pic 9(8).


       01  riga-ordine.
           05 ro-num          PIC  x(6).
           05 ro-clides       PIC  x(40).
           05 filler          PIC  x.
           05 ro-localita     PIC  x(100).
           05 filler          PIC  x.
           05 ro-ord-cli      PIC  x(10).
           05 ro-data-ord     PIC  x(10).
           05 ro-stato        PIC  x(10).
           05 ro-nr-e         PIC  x(8).
           05 ro-data-b       PIC  x(8).

       77  z5                pic z(5).
       77  z8                pic z(8).

       01  riga-art.
           05 ra-art      PIC  x(40).
           05 filler      PIC  x.
           05 ra-promo    PIC  x(100).
           05 ra-ord      PIC  x(10).
           05 ra-eva      PIC  x(10).
           05 ra-stato    PIC  x(10).
           05 ra-nr-e     PIC  x(8).
           05 ra-data-e   PIC  x(8).
           05 ra-nr-b     PIC  x(8).
           05 ra-data-b   PIC  x(8).
           05 ra-nr-f     PIC  x(8).
           05 ra-data-f   PIC  x(8).
           05 ra-vet      PIC  x(3).
           05 ra-esito    PIC  x(30).


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

       01  filler             pic 9.
         88 rec-ok            value 1, false 0.

       01  filler             pic 9.
         88 filtra-evasioni   value 1, false 0.
         
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


       77 save-stato       PIC  x(10).
       77 save-blocco      PIC  x.
           88 save-blocco-si VALUE IS "S". 
           88 save-blocco-no VALUE IS "N". 
       01 FILLER           PIC  9.
           88 sostituito VALUE IS 1    WHEN SET TO FALSE  0. 


       77  save-sol-desart       PIC  x(40).
       77  save-sol-promo        PIC  x(100).
       77  save-sol-qta-o        PIC  9(8).

       77  num-righe               pic 9(3).


       LINKAGE SECTION.
           copy "link-stsolleciti.def".

      ******************************************************************
       PROCEDURE DIVISION using stsolleciti-linkage.

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
           move stsolleciti-path-tmp   to path-tmp-solleciti-m.


           if stsolleciti-si-f-art
              perform varying tot-idx-art from 1 by 1 
                          until tot-idx-art > 10000
                 if stsolleciti-el-articolo(tot-idx-art) = zero
                    exit perform
                 end-if
              end-perform
              subtract 1 from tot-idx-art
           else
              move zero   to tot-idx-art
           end-if.


      ***---
       OPEN-FILES.
           open input tmp-sol  
           open i-o tmp-sol2
           open i-o tmp-sol1  

           open input articoli.
           open input tgrupgdo.
           open input clienti.
           open input destini.
           open input mtordini.
           open input mrordini.

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

           move "Master"     to ro-num
           move "Destino"    to ro-clides
           move "Cliente"    to ro-localita
           move "Nr. Ord"    to ro-ord-cli
           move "Data O"     to ro-data-ord
           move "Stato"      to ro-stato
           move "Bloccato"   to ro-nr-e
      *     move "Creato"     to ro-data-e
      *     move space        to ro-nr-b
           move "Cons."      to ro-data-b


           set spl-stringa   to true.
           move riga-ordine  to spl-riga-stampa.
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

           move "Data Ordine:"   to spl-riga-stampa
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
                  stsolleciti-dt-da        delimited by low-value
                  " al "                    delimited by size
                  stsolleciti-dt-a         delimited by size
                  into spl-riga-stampa
           add 0,3  to spl-riga
           call "spooler" using spooler-link.

           move stsolleciti-promo   to spl-riga-stampa
           add 0,3  to spl-riga
           call "spooler" using spooler-link.

           move stsolleciti-gdo  to gdo-codice
           read tgrupgdo
              invalid
                 initialize gdo-intestazione 
           end-read
           move gdo-intestazione to spl-riga-stampa
           add 0,3  to spl-riga
           call "spooler" using spooler-link.

           move stsolleciti-cli  to cli-codice
           set cli-tipo-c    to true
           read clienti
              invalid
                 initialize cli-ragsoc-1
           end-read
           move cli-ragsoc-1  to spl-riga-stampa
           add 0,3  to spl-riga
           call "spooler" using spooler-link.

           move stsolleciti-data to spl-riga-stampa
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
           if stsolleciti-si-f-art
              perform varying idx from 1 by 1 until idx > 10000
                 if stsolleciti-el-articolo(idx) = zero
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
           
                 move stsolleciti-el-articolo(idx) to art-ed
                 inspect spl-riga-stampa 
                                replacing trailing space by low-value
                 string spl-riga-stampa  delimited by low-value
                        art-ed           delimited by size
                        into spl-riga-stampa
              end-perform
              if como-riga < 5
                 inspect spl-riga-stampa 
                                   replacing trailing low-value by space
                 add 0,3  to spl-riga
                 call "spooler" using spooler-link
              end-if
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
           move "STAMPA SOLLECITI"   to spl-riga-stampa
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

           move 22,3               to spl-colonna.
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
           perform SETTA-FILTRI.


           move low-value to sol-rec of tmp-sol.
           set sol-testa  of tmp-sol to true.
           start tmp-sol key >= k-dt-testa 
              invalid 
                 continue 
              not invalid
                 perform until 1 = 2
                    read tmp-sol next no lock
                       at end      
                          exit perform 
                    end-read
                    if not sol-testa of tmp-sol
                       exit perform 
                    end-if

                    perform SCRIVI
                    if spl-sta-annu
                       exit perform
                    end-if
                 end-perform
           end-start.

      ***---
       SCRIVI.
           set fromfile   to true
           perform VALUTA-PADRE.
           if rec-ok
              perform SCRIVI-RIGA-ORDINE
              if prima-volta
                 exit paragraph
              end-if
      
              set primo-passaggio  to true

              perform APRI-RIGHE

      *    riga separazione articoli
              add 0,4   to spl-riga 
              perform LINEA-DIVISORIA-1

           end-if.

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
       SCRIVI-RIGA-ORDINE.
           perform CONTROLLO-SALTO-PAGINA

      *     if pronostico > maxrighe
      *        perform SALTO-PAGINA
      *     end-if.
           if prima-volta
              exit paragraph
           end-if

           initialize riga-ordine.

           move sol-numero of tmp-sol         to z5
           move z5                            to ro-num
           move sol-clides of tmp-sol         to ro-clides
           move sol-localita of tmp-sol       to ro-localita
           move sol-numord-cli of tmp-sol     to ro-ord-cli
           move sol-data-video of tmp-sol     to ro-data-ord
           move sol-data-cons of tmp-sol      to ro-data-b
           move sol-stato of tmp-sol          to ro-stato


           if sol-blocco-si of tmp-sol
              move "SI"   to ro-nr-e
           end-if.


           add  0,40    to spl-riga.

           move 1               to SPL-PEN-WITH
           set spl-oggetto      to true
           set SPL-RETTANGOLO   to true
           set SPL-BRUSH-LTGRAY to true
           move 0,6   to spl-colonna 
           move 28,5  to spl-colonna-fine
           add 0,4  to spl-riga giving spl-riga-fine
           call "spooler" using spooler-link. 

           move 1       to spl-tipo-colonna
      *     add  0,40    to spl-riga.
           move Arial10b to spl-hfont

           set spl-stringa   to true.
           move riga-ordine  to spl-riga-stampa.

colore*     set spl-nero   to true
      *     evaluate sol-stato of tmp-sol
      *     when "REG"    
      *           set spl-nero      to true
      *     when "LAV"    
      *           |set SPL-GIALLO    to true
      *           set SPL-ARANCIONE to true
      *     when "SP"     
      *          set SPL-AZZURRO    to true
      *     when "ST"     
      *           set SPL-BLU       to true
      *     when "FP"  
      *           set SPL-ARANCIONE to true
      *     when "FT"     
      *           set spl-rosso     to true
      *     when "CHIUSO" 
      *           set SPL-VERDE     to true
      *     end-evaluate.

           call "spooler" using spooler-link. 

      *     set spl-nero   to true.

      ***---
       APRI-RIGHE.
           move sol-chiave of tmp-sol   to save-sol-chiave

           move sol-stato   of tmp-sol to save-stato.
           move sol-blocco  of tmp-sol to save-blocco.

           move low-value             to sol-rec      of tmp-sol.
           set  sostituito            to false.
           move save-sol-ordine       to sol-ordine   of tmp-sol.
           move save-sol-data-ordine  to sol-data-ordine of tmp-sol.

           move 1               to sol-prog-art of tmp-sol.
           move 0               to sol-prog-eva of tmp-sol.
           start tmp-sol key >= sol-chiave  of tmp-sol
              invalid 
                 continue
           end-start.
           perform until 1 = 2
              read tmp-sol next no lock 
                 at end 
                    exit perform 
              end-read
              if sol-ordine      of tmp-sol not = save-sol-ordine   or
                 sol-data-ordine of tmp-sol not = save-sol-data-ordine
                 exit perform 
              end-if
              if sol-sost-si of tmp-sol
                 set sostituito to true
              end-if

              if sol-qta-o of tmp-sol <= sol-tot-evasa of tmp-sol
                 set evasa-tutto to true
              else
                 set evasa-tutto to false
              end-if


              if sol-valido-si of tmp-sol
                 move sol-tot-evasa of tmp-sol to tot-evasa

                 move 0 to num-evasioni
                 
                 move sol-desart    of tmp-sol to save-sol-desart   |ra-art
                 move sol-promo     of tmp-sol to save-sol-promo    |ra-promo
                 move sol-qta-o     of tmp-sol to save-sol-qta-o        |ra-ord 
                                                  como-evasa
                 inspect ra-ord replacing leading x"30" by x"20"
                 call "C$JUSTIFY" using ra-ord, "R"


                 perform sol-evasioni of tmp-sol times
                    read tmp-sol next no lock 
                       at end 
                          exit perform 
                    end-read
                          
                    if sol-valido-si of tmp-sol
                       add 1         to num-evasioni

                       perform SCRIVI-RIGA-ART
                    end-if
                 end-perform
                 evaluate num-evasioni
                 when 0
                      if not filtra-evasioni
                         perform SCRIVI-RIGA-ART-NO-EVA
                      end-if
                 when 1 
                      if not evasa-tutto
                         perform RIGA-SALDO
                      end-if
                 when other
                      if not evasa-tutto
                         perform RIGA-SALDO
                      end-if

                 end-evaluate
                 set sostituito to false
              end-if
           end-perform.

           move save-sol-chiave to sol-chiave of tmp-sol   
           read TMP-SOL no lock key is k-dt-testa 
              invalid
                 continue
           end-read.



      ***---
       RIGA-SALDO.
           perform SCRIVI-RIGA-ART-INIT.
           compute como-evasa = como-evasa - tot-evasa.
           move como-evasa to ra-eva.
           inspect ra-eva replacing leading x"30" by x"20".
           call "C$JUSTIFY" using ra-eva, "R".
           if save-blocco-si
      *        move bloccato-bmp to spl-hbitmap
      *        perform SCRIVI-BMP-STATO
              move ro-nr-e     to ra-stato
              continue
           else
              if mto-chiuso
                 move "CHIUSO" to ra-stato
              else
                 if sostituito
                    move "SOST" to ra-stato
                 else
                    move "REG"  to ra-stato
                 end-if
              end-if
              move sol-ordine of tmp-sol to mto-chiave
              read mtordini no lock invalid continue end-read
              perform IMPOSTA-CLIENTE-BLOCCATO
           end-if.

           perform SCRIVI-RETTANGOLI-COLORATI

           set spl-stringa   to true
           perform SCRIVI-RIGA-ART-FINE.



      ***---
       SCRIVI-RIGA-ART-NO-EVA.
           perform SCRIVI-RIGA-ART-INIT.


           move sol-desart    of tmp-sol to ra-art
           move sol-promo     of tmp-sol to ra-promo
           move sol-qta-o     of tmp-sol to ra-ord como-evasa
           inspect ra-ord replacing leading x"30" by x"20"
           call "C$JUSTIFY" using ra-ord, "R"

           move sol-ordine       of tmp-sol to mto-chiave
           read mtordini no lock 
                invalid 
                 display message "Contattare assistenza con codice K1"
           end-read
           if mto-chiuso
              move "CHIUSO" to ra-stato
           else
              if sostituito
                 move "SOST" to ra-stato
              else
                 move "REG"  to ra-stato
              end-if
              perform IMPOSTA-CLIENTE-BLOCCATO
           end-if

           move sol-ordine       of tmp-sol to mro-chiave-testa
           move sol-progr-master of tmp-sol to mro-progr
           read mrordini key mro-k-progr
              invalid 
                 display message "Contattare assistenza con codice K2"
           end-read
           accept como-data from century-date
           if mro-evadi-dal > como-data
              initialize ra-esito
              string "Posticipata al "  delimited size
                     mro-evadi-dal(7:2) delimited size
                     "/"                delimited size
                     mro-evadi-dal(5:2) delimited size
                     "/"                delimited size
                     mro-evadi-dal(1:4) delimited size
                     into ra-esito
              end-string
           else
              move como-esito to ra-esito
           end-if

           perform SCRIVI-RETTANGOLI-COLORATI

           perform SCRIVI-RIGA-ART-FINE.


      ***---
       IMPOSTA-CLIENTE-BLOCCATO.
           move spaces to como-esito.
           set cli-tipo-C   to true
           move mto-cod-cli to cli-codice
           read clienti no lock invalid continue end-read
           if cli-attivo or cli-fuori-fido
              if mto-prg-destino not = 0
                 move mto-cod-cli     to des-codice
                 move mto-prg-destino to des-prog
                 read destini no lock invalid continue end-read
                 move des-stato to cli-stato
              end-if
           end-if.
           if cli-bloccato or cli-disattivo
              set mto-bloccato to true
              if cli-fuori-fido
                 continue
              else
                 move "BLOC" to ra-stato
              end-if

              if cli-disattivo
                 if des-disattivo
                    move "DESTINO DISATTIVO"   to como-esito
                 else
                    move "CLIENTE DISATTIVO"   to como-esito
                 end-if
              else
                 evaluate true
                 when cli-no-angraf
                      move "ANAGRAFICA ERRATA"   to como-esito
                 when cli-prob-pag
                      move "PROBLEMi PAGAMENTO"  to como-esito
                 when cli-nuovo-ragsoc
                      move "NUOVA RAG. SOCIALE"  to como-esito
                 when cli-fuori-fido
                      move "FUORI FIDO"          to como-esito
                 when other
                      move "DESTINO BLOCCATO"    to como-esito
                 end-evaluate
              end-if
           end-if.

      ***---
       SCRIVI-RIGA-ART.
           perform SCRIVI-RIGA-ART-INIT.
      
           move save-sol-desart to ra-art
           move save-sol-promo  to ra-promo
           move save-sol-qta-o  to ra-ord como-evasa
           inspect ra-ord replacing leading x"30" by x"20"
           call "C$JUSTIFY" using ra-ord, "R"

           if sol-data-b of tmp-sol = 0 and 
              sol-data-f of tmp-sol = 0
              move "LAV." to ra-stato
           else
              if sol-data-f  of tmp-sol not = 0
                 move "FATT" to ra-stato
              else
                 if sol-data-b  of tmp-sol not = 0
                    move "BOLL" to ra-stato
                 end-if
              end-if
           end-if
           
           move sol-qta-e of tmp-sol to ra-eva
           
           inspect ra-eva replacing leading x"30" by x"20"
           call "C$JUSTIFY" using ra-eva, "R"
           
           move sol-num-e of tmp-sol to z8
           move z8                    to ra-nr-e
           if sol-data-e  of tmp-sol = 0
              move spaces to ra-data-e
           else
              string sol-data-e of tmp-sol(7:2) delimited size
                     "/"                        delimited size
                     sol-data-e of tmp-sol(5:2) delimited size
                     "/"                        delimited size
                     sol-data-e of tmp-sol(3:2) delimited size
                     into ra-data-e
              end-string
           end-if
           
           move sol-num-b of tmp-sol to z8
           move z8                    to ra-nr-b
           if sol-data-b  of tmp-sol = 0
              move spaces to ra-data-b
           else
              string sol-data-b of tmp-sol(7:2) delimited size
                     "/"                        delimited size
                     sol-data-b of tmp-sol(5:2) delimited size
                     "/"                        delimited size
                     sol-data-b of tmp-sol(3:2) delimited size
                     into ra-data-b
              end-string
           end-if
           
           move sol-num-f of tmp-sol to z8
           move z8                    to ra-nr-f   
           if sol-data-f  of tmp-sol = 0
              move spaces to ra-data-f
           else
              string sol-data-f of tmp-sol(7:2) delimited size
                     "/"                        delimited size
                     sol-data-f of tmp-sol(5:2) delimited size
                     "/"                        delimited size
                     sol-data-f of tmp-sol(3:2) delimited size
                     into ra-data-f
              end-string
           end-if
           
           move sol-vett  of tmp-sol to ra-vet    
           move sol-esito of tmp-sol to ra-esito  
           
           if num-evasioni not = 1
              move spaces  to ra-art
              move spaces  to ra-promo
              move spaces  to ra-ord
           end-if

           perform SCRIVI-RETTANGOLI-COLORATI

           perform SCRIVI-RIGA-ART-FINE.

      ***---
       SCRIVI-RIGA-ART-FINE.
           move 3            to spl-tipo-colonna
           move 0,5          to spl-colonna
           move Arial7       to spl-hfont
      
           add 0,30          to spl-riga
           set spl-stringa   to true.
           move riga-art     to spl-riga-stampa.
           call "spooler" using spooler-link. 

      ***---
       SCRIVI-RIGA-ART-INIT.
           if primo-passaggio
              set primo-passaggio  to false
              perform INTESTA-ART
           end-if
      
           add 0,30 to spl-riga giving pronostico
           if pronostico > maxrighe
              perform SALTO-PAGINA
              perform INTESTA-ART
           end-if.
      
           initialize riga-art.


      ***---
       INTESTA-ART.
           add 1,00 to spl-riga giving pronostico
           if pronostico > maxrighe
              perform SALTO-PAGINA
           end-if.
      
           move "ARTICOLO"         to ra-art    
           move "PROMO"            to ra-promo  
           move "ORD"              to ra-ord    
           move "EVA"              to ra-eva    
           move "Stato"            to ra-stato  
           move "Nr. E"            to ra-nr-e   
           move "Data E"           to ra-data-e 
           move "Bolla"            to ra-nr-b   
           move "Data B"           to ra-data-b 
           move "Fatt."            to ra-nr-f   
           move "Data F"           to ra-data-f 
           move "Vet"              to ra-vet    
           move "Stato Consegna"   to ra-esito  

           move 3    to spl-tipo-colonna
           move 0,5     to spl-colonna
           move Arial7b to spl-hfont
      
           set spl-stringa   to true.
           move riga-art  to spl-riga-stampa.
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

              move "Stampa Solleciti" to spl-nome-job
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
           close tmp-sol    
                 tmp-sol2
                 tmp-sol1  
                 articoli
                 tgrupgdo
                 clienti
                 destini
                 mtordini
                 mrordini.

      ***---
       EXIT-PGM.
           set environment "PRINTER" to "-P SPOOLER-DIRECT".
           destroy arial14bi.
           destroy arial10b.
           destroy Arial7.
           destroy arial5B.
           destroy arial5.
           destroy arial6.

      *     call "W$BITMAP" using wbitmap-destroy, bloccato-REG-bmp.
      *     call "W$BITMAP" using wbitmap-destroy, bloccato-LAV-bmp.
      *     call "W$BITMAP" using wbitmap-destroy, bloccato-SP-bmp.
      *     call "W$BITMAP" using wbitmap-destroy, bloccato-ST-bmp.
      *     call "W$BITMAP" using wbitmap-destroy, bloccato-FP-bmp.
      *     call "W$BITMAP" using wbitmap-destroy, bloccato-FT-bmp.
      *     call "W$BITMAP" using wbitmap-destroy, bloccato-CHIUSO-bmp.

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

      ***---
       SETTA-FILTRI.
           set filtra-evasioni to false.
           if stsolleciti-esito   not = spaces or
              stsolleciti-vet     not = spaces or
              stsolleciti-data-f  not = spaces or
              stsolleciti-num-f   not = spaces or
              stsolleciti-data-b  not = spaces or
              stsolleciti-num-b   not = spaces or
              stsolleciti-data-e  not = spaces or
              stsolleciti-num-e   not = spaces
              set filtra-evasioni to true
           end-if.

      ***---
       VALUTA-PADRE.
           set rec-ok  to true.

      *        10 stsolleciti-data-e   pic x(10).
      *        10 stsolleciti-num-b    pic z(8).
      *        10 stsolleciti-data-b   pic x(10).
      *        10 stsolleciti-num-f    pic z(8).
      *        10 stsolleciti-data-f   pic x(10).
      *        10 stsolleciti-vet      pic x(3).
      *        10 stsolleciti-esito    pic x(20).

      *      move sol-data-creazione of tmp-sol  to ra-data-e
      *           move sol-data-cons      of tmp-sol  to ra-data-b
      *           move sol-esito          of tmp-sol  to ra-esito


           set rec-ok to false.
           if stsolleciti-num not = zero and
              stsolleciti-num not = sol-numero of tmp-sol
              set rec-ok  to false
              exit paragraph
           end-if.
           if stsolleciti-clides not = spaces and
              stsolleciti-clides not = sol-clides of tmp-sol
              set rec-ok  to false
              exit paragraph
           end-if.
           if stsolleciti-numord not = spaces and
              stsolleciti-numord not = sol-numord-cli of tmp-sol
              set rec-ok  to false
              exit paragraph
           end-if.
           if stsolleciti-loca not = spaces and
              stsolleciti-loca not = sol-localita of tmp-sol
              set rec-ok  to false
              exit paragraph
           end-if.
           if stsolleciti-data not = spaces and
              stsolleciti-data not = sol-data-video of tmp-sol
              set rec-ok  to false
              exit paragraph
           end-if.
           if stsolleciti-stato not = spaces and
              stsolleciti-stato not = sol-stato of tmp-sol
              set rec-ok  to false
              exit paragraph
           end-if.

           evaluate sol-stato-ordine of tmp-sol
           when 1
                if stsolleciti-reg = 0
                   set rec-ok  to false
                   exit paragraph
                end-if
           when 2
                if stsolleciti-il = 0
                   set rec-ok  to false
                   exit paragraph
                end-if
           when 3
                if stsolleciti-sp = 0
                   set rec-ok  to false
                   exit paragraph
                end-if
           when 4
                if stsolleciti-st = 0
                   set rec-ok  to false
                   exit paragraph
                end-if
           when 7
                if stsolleciti-chiuso = 0
                   set rec-ok  to false
                   exit paragraph
                end-if
           end-evaluate.

           move sol-chiave  of tmp-sol to save-sol-chiave.
           move low-value   to sol-rec of tmp-sol2
           move sol-ordine  of tmp-sol to
                sol-ordine  of tmp-sol2.
           move sol-data-ordine of tmp-sol to
                sol-data-ordine of tmp-sol2.
           set sol-articolo of tmp-sol2 to true.
           move 0 to evasioni-valide articoli-validi.
           start tmp-sol2 key >= k-dt-testa
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tmp-sol2 next no lock
                       at end 
                          exit perform 
                    end-read
                    if sol-ordine  of tmp-sol2 not =
                       sol-ordine  of tmp-sol
                       exit perform
                    end-if
                    if sol-evas  of tmp-sol2 or
                       sol-testa of tmp-sol2
                       exit perform
                    end-if

                    if sol-valido-si   of tmp-sol2 
                       if sol-evasioni of tmp-sol2 = 0 and 
                                            filtra-evasioni
                          set sol-valido-no of tmp-sol2 to true
                          rewrite sol-rec   of tmp-sol2
                       end-if
                    else
                       if sol-evasioni      of tmp-sol2 = 0 and not 
                          filtra-evasioni
                          set sol-valido-si of tmp-sol2 to true
                          rewrite sol-rec   of tmp-sol2
                       end-if
                       if sol-evasioni      of tmp-sol2 not = 0
                          set sol-valido-si of tmp-sol2 to true
                          rewrite sol-rec   of tmp-sol2
                       end-if
                    end-if
                    if sol-valido-si of tmp-sol2
                       if stsolleciti-promo not = 78-tutti-vol
                          if sol-promo of tmp-sol2 
                                               not = stsolleciti-promo
                             set sol-valido-no of tmp-sol2 to true
                             rewrite sol-rec   of tmp-sol2
                          end-if
                       end-if
                    end-if

                    if sol-valido-si of tmp-sol2 and tot-idx-art not = 0
                       set st-idx-art to 1
                       search stsolleciti-el-articolo
                       at end
                          set sol-valido-no of tmp-sol2 to true
                          rewrite sol-rec   of tmp-sol2
                       when stsolleciti-el-articolo(st-idx-art) = 
                                               sol-art of tmp-sol2
                            continue
                       end-search
                    end-if

                    if sol-valido-si of tmp-sol2
                       if sol-evasioni of tmp-sol2 not = 0
                          set valuta to true
                          perform VALUTA-ANNULLA-EVASIONI
                       else
                          move 1 to evasioni-valide
                          set sol-valido-si of tmp-sol2 to true
                          rewrite sol-rec   of tmp-sol2
                       end-if
                       add evasioni-valide to articoli-validi
                       if evasioni-valide = 0
                          set sol-valido-no of tmp-sol2 to true
                          rewrite sol-rec of tmp-sol2
                       end-if
                    else
                       set annulla to true
                       perform VALUTA-ANNULLA-EVASIONI
                    end-if
                 end-perform
           end-start.

           if articoli-validi = 0
              if FromGrid
                 set rec-ok  to false
              end-if
           else
              if FromFile
                 set rec-ok to true
              end-if
           end-if.


      ***---
       VALUTA-ANNULLA-EVASIONI.
           move low-value  to sol-rec of tmp-sol1
           move sol-ordine of tmp-sol2 to
                sol-ordine of tmp-sol1
           set sol-evas of tmp-sol1 to true.
           move sol-prog-art of tmp-sol2 to sol-prog-art of tmp-sol1.
           move 0 to evasioni-valide.
           start tmp-sol1 key >= k-eva
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tmp-sol1 next no lock 
                       at end 
                          exit perform 
                    end-read
                    if sol-ordine of tmp-sol1 not =
                       sol-ordine of tmp-sol2
                       exit perform
                    end-if
                    if sol-testa     of tmp-sol1 or
                       sol-articolo  of tmp-sol1
                       exit perform
                    end-if
                    if sol-prog-art of tmp-sol1 not =
                       sol-prog-art of tmp-sol2
                       exit perform
                    end-if
                    if valuta
                       perform VALUTA-EVASIONE
                    else
                       set sol-valido-no of tmp-sol1 to true
                       rewrite sol-rec   of tmp-sol1
                    end-if
                end-perform
                
           end-start.

      ***---
       VALUTA-EVASIONE.
           if stsolleciti-esito  not = spaces and
              stsolleciti-esito  not = sol-esito of tmp-sol1 
              set sol-valido-no of tmp-sol1 to true
              rewrite sol-rec   of tmp-sol1
              exit paragraph
           end-if.

           if stsolleciti-vet   not = spaces and
              stsolleciti-vet   not = sol-vett of tmp-sol1
              set sol-valido-no of tmp-sol1 to true
              rewrite sol-rec   of tmp-sol1
              exit paragraph
           end-if.

           string sol-data-f of tmp-sol1(7:2) delimited size
                  "/"                         delimited size
                  sol-data-f of tmp-sol1(5:2) delimited size
                  "/"                         delimited size
                  sol-data-f of tmp-sol1(3:2) delimited size
                  into como-data-x8
           end-string.
           if stsolleciti-data-f not = spaces and
              stsolleciti-data-f not = como-data-x8
              set sol-valido-no of tmp-sol1 to true
              rewrite sol-rec   of tmp-sol1
              exit paragraph
           end-if.
           
           move stsolleciti-num-f to como-numero.
           if como-numero  not = 0 and
              como-numero  not = sol-num-f of tmp-sol1
              set sol-valido-no of tmp-sol1 to true
              rewrite sol-rec   of tmp-sol1
              exit paragraph
           end-if. 
                   
           string sol-data-b of tmp-sol1(7:2) delimited size
                  "/"                         delimited size
                  sol-data-b of tmp-sol1(5:2) delimited size
                  "/"                         delimited size
                  sol-data-b of tmp-sol1(3:2) delimited size
                  into como-data-x8
           end-string.
           if stsolleciti-data-b not = spaces and
              stsolleciti-data-b not = como-data-x8
              set sol-valido-no of tmp-sol1 to true
              rewrite sol-rec   of tmp-sol1
              exit paragraph
           end-if.

           move stsolleciti-num-b to como-numero.
           if como-numero  not = 0 and
              como-numero  not = sol-num-b  of tmp-sol1
              set sol-valido-no of tmp-sol1 to true
              rewrite sol-rec   of tmp-sol1
              exit paragraph
           end-if.

           string sol-data-e of tmp-sol1(7:2) delimited size
                  "/"                         delimited size
                  sol-data-e of tmp-sol1(5:2) delimited size
                  "/"                         delimited size
                  sol-data-e of tmp-sol1(3:2) delimited size
                  into como-data-x8
           end-string.
           if stsolleciti-data-e not = spaces and
              stsolleciti-data-e not = como-data-x8
              set sol-valido-no of tmp-sol1 to true
              rewrite sol-rec   of tmp-sol1
              exit paragraph
           end-if.

           move stsolleciti-num-e to como-numero.
           if como-numero  not = 0 and
              como-numero  not = sol-num-e  of tmp-sol1
              set sol-valido-no of tmp-sol1 to true
              rewrite sol-rec   of tmp-sol1
              exit paragraph
           end-if.
                  
           set sol-valido-si of tmp-sol1 to true.
           rewrite sol-rec   of tmp-sol1.
           add 1 to evasioni-valide. 

      ****---
      * SCRIVI-BMP-STATO.
      *     set spl-oggetto   to true
      *     set  spl-bitmap  to true
      **     move 2,0 to spl-riga
      *     move 108 to spl-colonna
      *     move 0,4 to spl-bitmap-height
      *     move 0,9 to spl-bitmap-width
      *
      *     add 10,6   to spl-riga
      *     call "spooler" using spooler-link
      *     subtract 10,6   from spl-riga.
      *     set spl-stringa   to true.

       CONTROLLO-SALTO-PAGINA.
           add 1,5 to spl-riga giving pronostico
      
           if pronostico > maxrighe
              perform SALTO-PAGINA
           else
              PERFORM CONTA-RIGHE-APERTE
              compute pronostico = spl-riga + |riga di partenza
                                   1,5 +      |spazio per la riga dell'ordine
                                   (num-righe * 0,3) + |spazio per le righe dettaglio
                                   1          |spazio per l'intestazione del dettalio

              if pronostico > maxrighe
                 perform SALTO-PAGINA
              end-if
           end-if.


      ***---
       CONTA-RIGHE-APERTE.
           move zero   to num-righe.

           move sol-chiave of tmp-sol   to save-sol-chiave

           move sol-stato   of tmp-sol to save-stato.
           move sol-blocco  of tmp-sol to save-blocco.

           move low-value             to sol-rec      of tmp-sol.
           set  sostituito            to false.
           move save-sol-ordine       to sol-ordine   of tmp-sol.
           move save-sol-data-ordine  to sol-data-ordine of tmp-sol.

           move 1               to sol-prog-art of tmp-sol.
           move 0               to sol-prog-eva of tmp-sol.
           start tmp-sol key >= sol-chiave  of tmp-sol
              invalid 
                 continue
           end-start.
           perform until 1 = 2
              read tmp-sol next no lock 
                 at end 
                    exit perform 
              end-read
              if sol-ordine      of tmp-sol not = save-sol-ordine   or
                 sol-data-ordine of tmp-sol not = save-sol-data-ordine
                 exit perform 
              end-if
              if sol-sost-si of tmp-sol
                 set sostituito to true
              end-if

              if sol-qta-o of tmp-sol <= sol-tot-evasa of tmp-sol
                 set evasa-tutto to true
              else
                 set evasa-tutto to false
              end-if


              if sol-valido-si of tmp-sol
                 perform sol-evasioni of tmp-sol times
                    read tmp-sol next no lock 
                       at end 
                          exit perform 
                    end-read
                          
                    if sol-valido-si of tmp-sol
                       add 1         to num-righe
                    end-if
                 end-perform
                 evaluate num-evasioni
                 when 0
                      if not filtra-evasioni
                         add 1         to num-righe
                      end-if
                 when 1 
                      if not evasa-tutto
                         add 1         to num-righe
                      end-if
                 when other
                      if not evasa-tutto
                         add 1         to num-righe
                      end-if

                 end-evaluate
                 set sostituito to false
              end-if
           end-perform.

           move save-sol-chiave to sol-chiave of tmp-sol   
           read TMP-SOL no lock key is k-dt-testa 
              invalid
                 continue
           end-read.

      ***---
       SCRIVI-RETTANGOLI-COLORATI.
           move zero            to SPL-PEN-WITH
      *     set SPL-BRUSH-BDIAGONAL to true
           set SPL-BRUSH-SOLID     to true

           set SPL-PEN-SOLID    to true
           set spl-oggetto      to true
           set SPL-RETTANGOLO   to true
           set spl-giallo-2     to true
           move 16,1   to spl-colonna 
           move 19,0   to spl-colonna-fine


           add 0,3  to spl-riga 
           add 0,3  to spl-riga giving spl-riga-fine

           call "spooler" using spooler-link. 



           set spl-verde       to true
           move 19,0   to spl-colonna 
           move 22,0  to spl-colonna-fine
           call "spooler" using spooler-link. 
      *
           set spl-azzurro-2    to true
           move 22,0   to spl-colonna 
           move 24,9  to spl-colonna-fine
           call "spooler" using spooler-link. 

           subtract 0,3  from spl-riga 


           set spl-stringa   to true.
           set spl-nero      to true.
