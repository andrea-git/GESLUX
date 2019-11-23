      ***---
       SETTA-COLONNE-BOZZE-EVASIONE-ORDFORN.
           perform PULISCI-COLONNE
           initialize winprint-column
           move spl-hfont             to winprint-col-font
           move wprtunits-centimeters to winprint-col-units     
           move wprtalign-none        to winprint-col-alignment
           set  winprint-transparent  to true
           evaluate spl-tipo-colonna
           when 1
                call "WIN$PRINTER"  using winprint-set-data-columns,
                               record-position of rb-stof-art
                               record-position of rb-stof-des     
                               record-position of rb-stof-cod-forn
                               record-position of rb-stof-data
                               record-position of rb-stof-qta
                               record-position of rb-stof-imb
                               record-position of rb-stof-peso-netto
                               record-position of rb-stof-peso-tot-netto
                               record-position of rb-stof-peso-tot-utf
                               record-position of rb-stof-prz-listino
                               record-position of rb-stof-sconto-1-ed
                               record-position of rb-stof-sconto-2-ed
                               record-position of rb-stof-sconto-3-ed
                               record-position of rb-stof-sconto-4-ed
                               record-position of rb-stof-sconto-5-ed
                               record-position of rb-stof-prz-netto
                               record-position of rb-stof-imp-consumo
                               record-position of rb-stof-imp-cou-cobat
                               record-position of rb-stof-add-piombo
                               record-position of rb-stof-costi-aggi
                               record-position of rb-stof-prz-finale
                               record-position of rb-stof-prz-tot-finale
                                giving return-code

                 move 0,1   to WINPRINT-COL-SEPARATION

                 move WPRTUNITS-CENTIMETERS to WINPRINT-COL-UNITS 


                 set WINPRINT-TRANSPARENT to true

      *    codice articolo
                move 0,1              to winprint-col-start
                move wprtalign-center to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column             
                                   giving return-code
      *    descrizione articolo                                         
                add 78-rb-stof-art    to winprint-col-start
                move wprtalign-left   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
      *    cod fornitore
                add 78-rb-stof-des    to winprint-col-start
      *          move wprtalign-left  to winprint-col-alignment
                move wprtalign-center to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code       

      *    qta inevasi
                |add 2,1                 to winprint-col-start
                add 78-rb-stof-cod-forn  to winprint-col-start
                move wprtalign-center    to winprint-col-alignment
      *          move wprtalign-right  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code       
      *    data
                add 78-rb-stof-data    to winprint-col-start
      *          move wprtalign-left  to winprint-col-alignment
                move wprtalign-center to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code


      *    imballo|
                add 78-rb-stof-qta    to    winprint-col-start
                move wprtalign-center to winprint-col-alignment
      *          move wprtalign-left to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
      *    peso netto
                add 78-rb-stof-imb    to    winprint-col-start
                move wprtalign-center to winprint-col-alignment
      *          move wprtalign-right to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
      *    peso netto totale
                add 78-rb-stof-peso-netto   to    winprint-col-start
                move wprtalign-center       to winprint-col-alignment
      *          move wprtalign-right to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
      *    peso UTF totale
                add 78-rb-stof-peso-tot-netto  to    winprint-col-start
                move wprtalign-center to winprint-col-alignment
      *          move wprtalign-right to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
      *    listino
                |add 0,95             to    winprint-col-start
                add 78-rb-stof-peso-tot-utf to    winprint-col-start
      *          move wprtalign-right        to winprint-col-alignment
                move wprtalign-center        to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
      *    sc 1
                add 78-rb-stof-prz-listino  to winprint-col-start
                move wprtalign-center       to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
      *    sc 2
                add 78-rb-stof-sconto-1  to    winprint-col-start
                move wprtalign-center    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
      *    sc 3
                add 78-rb-stof-sconto-2  to    winprint-col-start
                move wprtalign-center to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
      *    sc 4
                add 78-rb-stof-sconto-3  to    winprint-col-start
                move wprtalign-center to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
      *    sc 5
                add 78-rb-stof-sconto-4  to winprint-col-start
                move wprtalign-center to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
      *    prezzo netto
                add 78-rb-stof-sconto-5  to winprint-col-start
      *          move wprtalign-right        to winprint-col-alignment
                move wprtalign-center        to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
      *    imposta consumo
                add 78-rb-stof-prz-netto to winprint-col-start
      *          move wprtalign-right        to winprint-col-alignment
                move wprtalign-center        to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                        winprint-column
                                   giving return-code
                                   
      *     netto con imposta
                add 78-rb-stof-imp-consumo  to winprint-col-start
      *          move wprtalign-right        to winprint-col-alignment
                move wprtalign-center        to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
      **    cou/cobat
      *          add 1,6                    to    winprint-col-start
      *          move wprtalign-right to winprint-col-alignment
      *          call "WIN$PRINTER"  using winprint-set-page-column, 
      *                                    winprint-column
      *                             giving return-code
                                   
      *     05 piombio  pic ---.--9,99.
                add 78-rb-stof-imp-cou-cobat   to winprint-col-start
      *          move wprtalign-right        to winprint-col-alignment
                move wprtalign-center        to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
      *     05 r-costi-aggi   PIC  ---.--9,99.
                add 78-rb-stof-add-piombo   to    winprint-col-start
      *          move wprtalign-right        to winprint-col-alignment
                move wprtalign-center        to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
      *     05 r-prz-finale      PIC  ---.--9,99.
                add 78-rb-stof-costi-aggi   to winprint-col-start
      *          move wprtalign-right        to winprint-col-alignment
                move wprtalign-center        to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
      *     05 r-prz-tot-finale  PIC  ---.--9,99.
                add 78-rb-stof-prz-finale   to winprint-col-start
      *          move wprtalign-right        to winprint-col-alignment
                move wprtalign-center        to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code

      *    ultima colonna
                move 78-rb-stof-prz-tot-finale to winprint-col-start
                move wprtalign-right to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code


           when 2
                call "WIN$PRINTER"  using winprint-set-data-columns,
                             record-position of rbt-stof-art
                             record-position of rbt-stof-des
                             record-position of rbt-stof-cod-forn
                             record-position of rbt-stof-data
                             record-position of rbt-stof-qta
                             record-position of rbt-stof-imb
                             record-position of rbt-stof-peso-netto
                             record-position of rbt-stof-peso-tot-netto
                             record-position of rbt-stof-peso-tot-utf
                             record-position of rbt-stof-prz-listino
                             record-position of rbt-stof-sconto-1
                             record-position of rbt-stof-sconto-2
                             record-position of rbt-stof-sconto-3
                             record-position of rbt-stof-sconto-4
                             record-position of rbt-stof-sconto-5
                             record-position of rbt-stof-prz-netto
                             record-position of rbt-stof-imp-consumo
                             record-position of rbt-stof-imp-cou-cobat
                             record-position of rbt-stof-add-piombo
                             record-position of rbt-stof-costi-aggi
                             record-position of rbt-stof-prz-finale
                             record-position of rbt-stof-prz-tot-finale
                             giving return-code

                 move 0,1   to WINPRINT-COL-SEPARATION

                 move WPRTUNITS-CENTIMETERS to WINPRINT-COL-UNITS 


                 set WINPRINT-TRANSPARENT to true

      *    codice articolo
                move 0,1              to winprint-col-start
                move wprtalign-center to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column             
                                   giving return-code
      *    descrizione articolo                                         
                add 78-rb-stof-art    to winprint-col-start
                move wprtalign-left   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
      *    cod fornitore
                add 78-rb-stof-des       to winprint-col-start
                move wprtalign-center   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code           

      *    qta inevasi
                add 78-Rb-STOF-COD-FORN  to winprint-col-start
                move wprtalign-center    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code

      *    qta inevasi
                add 78-Rb-STOF-data  to winprint-col-start
                move wprtalign-center    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code


      *    imballo|
                add 78-rb-stof-qta    to    winprint-col-start
                move wprtalign-center to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
      *    peso netto
                add 78-rb-stof-imb    to    winprint-col-start
                move wprtalign-center to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
      *    peso netto totale
                add 78-rb-stof-peso-netto   to    winprint-col-start
                move wprtalign-center       to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
      *    peso UTF totale
                add 78-rb-stof-peso-tot-netto  to    winprint-col-start
                move wprtalign-center          to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
      *    listino
                add 78-rb-stof-peso-tot-utf to    winprint-col-start
                move wprtalign-center       to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
      *    sc 1
                add 78-rb-stof-prz-listino  to    winprint-col-start
                move wprtalign-center       to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
      *    sc 2
                add 78-rb-stof-sconto-1  to    winprint-col-start
                move wprtalign-center    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
      *    sc 3
                add 78-rb-stof-sconto-2  to    winprint-col-start
                move wprtalign-center    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
      *    sc 4
                add 78-rb-stof-sconto-3  to    winprint-col-start
                move wprtalign-center    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
      *    sc 5
                add 78-rb-stof-sconto-4  to    winprint-col-start
                move wprtalign-center    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
      *    prezzo netto
                add 78-rb-stof-sconto-5  to    winprint-col-start
                move wprtalign-center    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
      *    imposta consumo
                add 78-rb-stof-prz-netto to    winprint-col-start
                move wprtalign-center    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                        winprint-column
                                   giving return-code
                                   
      *     netto con imposta
                add 78-rb-stof-imp-consumo  to    winprint-col-start
                move wprtalign-center       to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
      *     05 piombio  pic ---.--9,99.
                add 78-rb-stof-imp-cou-cobat   to    winprint-col-start
                move wprtalign-center          to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
      *     05 r-costi-aggi   PIC  ---.--9,99.
                add 78-rb-stof-add-piombo   to    winprint-col-start
                move wprtalign-center       to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
      *     05 r-prz-finale      PIC  ---.--9,99.
                add 78-rb-stof-costi-aggi   to    winprint-col-start
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
      *     05 r-prz-tot-finale  PIC  ---.--9,99.
                add 78-rb-stof-prz-finale   to    winprint-col-start
                move wprtalign-center   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code


      *    ultima colonna
                move 78-rb-stof-prz-tot-finale to winprint-col-start
                move wprtalign-center          to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code


           end-evaluate. 


      ***---
       SETTA-COLONNE-ORDFORN.
           perform PULISCI-COLONNE
           initialize winprint-column
           move spl-hfont             to winprint-col-font
           move wprtunits-centimeters to winprint-col-units     
           move wprtalign-none        to winprint-col-alignment
           set  winprint-transparent  to true
           evaluate spl-tipo-colonna
           when 1
                call "WIN$PRINTER"  using winprint-set-data-columns,
                             record-position of ro-stof-art
                             record-position of ro-stof-des
                             record-position of ro-stof-cod-forn
                             record-position of ro-stof-qta
                             record-position of ro-stof-imb
                             record-position of ro-stof-peso-netto
                             record-position of ro-stof-peso-tot-netto
                             record-position of ro-stof-peso-tot-utf
                             record-position of ro-stof-prz-listino
                             record-position of ro-stof-sconto-1-ed
                             record-position of ro-stof-sconto-2-ed
                             record-position of ro-stof-sconto-3-ed
                             record-position of ro-stof-sconto-4-ed
                             record-position of ro-stof-sconto-5-ed
                             record-position of ro-stof-prz-netto
                             record-position of ro-stof-imp-consumo
                             record-position of ro-stof-imp-cou-cobat
                             record-position of ro-stof-add-piombo
                             record-position of ro-stof-costi-aggi
                             record-position of ro-stof-prz-finale
                             record-position of ro-stof-prz-tot-finale
                             giving return-code

                 move 0,1   to WINPRINT-COL-SEPARATION

                 move WPRTUNITS-CENTIMETERS to WINPRINT-COL-UNITS 


                 set WINPRINT-TRANSPARENT to true

      *    codice articolo
                move 0,1              to winprint-col-start
                move wprtalign-center to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column             
                                   giving return-code
      *    descrizione articolo                                         
                add 78-ro-stof-art    to winprint-col-start
                move wprtalign-left   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
      *    cod fornitore
                add 78-ro-stof-des    to winprint-col-start
      *          move wprtalign-left  to winprint-col-alignment
                move wprtalign-center to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code

      *    qta inevasi
                |add 2,1                 to winprint-col-start
                add 78-ro-stof-cod-forn  to winprint-col-start
                move wprtalign-center    to winprint-col-alignment
      *          move wprtalign-right  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code


      *    imballo|
                add 78-ro-stof-qta    to    winprint-col-start
                move wprtalign-center to winprint-col-alignment
      *          move wprtalign-left to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
      *    peso netto
                add 78-ro-stof-imb    to    winprint-col-start
                move wprtalign-center to winprint-col-alignment
      *          move wprtalign-right to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
      *    peso netto totale
                add 78-ro-stof-peso-netto   to    winprint-col-start
                move wprtalign-center       to winprint-col-alignment
      *          move wprtalign-right to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
      *    peso UTF totale
                add 78-ro-stof-peso-tot-netto  to    winprint-col-start
                move wprtalign-center to winprint-col-alignment
      *          move wprtalign-right to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
      *    listino
                |add 0,95             to    winprint-col-start
                add 78-ro-stof-peso-tot-utf to    winprint-col-start
      *          move wprtalign-right        to winprint-col-alignment
                move wprtalign-center        to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
      *    sc 1
                add 78-ro-stof-prz-listino  to winprint-col-start
                move wprtalign-center       to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
      *    sc 2
                add 78-ro-stof-sconto-1  to    winprint-col-start
                move wprtalign-center    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
      *    sc 3
                add 78-ro-stof-sconto-2  to    winprint-col-start
                move wprtalign-center to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
      *    sc 4
                add 78-ro-stof-sconto-3  to    winprint-col-start
                move wprtalign-center to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
      *    sc 5
                add 78-ro-stof-sconto-4  to winprint-col-start
                move wprtalign-center to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
      *    prezzo netto
                add 78-ro-stof-sconto-5  to winprint-col-start
      *          move wprtalign-right        to winprint-col-alignment
                move wprtalign-center        to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
      *    imposta consumo
                add 78-ro-stof-prz-netto to winprint-col-start
      *          move wprtalign-right        to winprint-col-alignment
                move wprtalign-center        to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                        winprint-column
                                   giving return-code
                                   
      *     netto con imposta
                add 78-ro-stof-imp-consumo  to winprint-col-start
      *          move wprtalign-right        to winprint-col-alignment
                move wprtalign-center        to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
      **    cou/cobat
      *          add 1,6                    to    winprint-col-start
      *          move wprtalign-right to winprint-col-alignment
      *          call "WIN$PRINTER"  using winprint-set-page-column, 
      *                                    winprint-column
      *                             giving return-code
                                   
      *     05 piombio  pic ---.--9,99.
                add 78-ro-stof-imp-cou-cobat   to winprint-col-start
      *          move wprtalign-right        to winprint-col-alignment
                move wprtalign-center        to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
      *     05 r-costi-aggi   PIC  ---.--9,99.
                add 78-ro-stof-add-piombo   to    winprint-col-start
      *          move wprtalign-right        to winprint-col-alignment
                move wprtalign-center        to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
      *     05 r-prz-finale      PIC  ---.--9,99.
                add 78-ro-stof-costi-aggi   to winprint-col-start
      *          move wprtalign-right        to winprint-col-alignment
                move wprtalign-center        to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
      *     05 r-prz-tot-finale  PIC  ---.--9,99.
                add 78-ro-stof-prz-finale   to winprint-col-start
      *          move wprtalign-right        to winprint-col-alignment
                move wprtalign-center        to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code

      *    ultima colonna
                move 78-ro-stof-prz-tot-finale to winprint-col-start
                move wprtalign-right to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code


           when 2
                call "WIN$PRINTER"  using winprint-set-data-columns,
                             record-position of rot-stof-art
                             record-position of rot-stof-des
                             record-position of rot-stof-cod-forn
                             record-position of rot-stof-qta
                             record-position of rot-stof-imb
                             record-position of rot-stof-peso-netto
                             record-position of rot-stof-peso-tot-netto
                             record-position of rot-stof-peso-tot-utf
                             record-position of rot-stof-prz-listino
                             record-position of rot-stof-sconto-1
                             record-position of rot-stof-sconto-2
                             record-position of rot-stof-sconto-3
                             record-position of rot-stof-sconto-4
                             record-position of rot-stof-sconto-5
                             record-position of rot-stof-prz-netto
                             record-position of rot-stof-imp-consumo
                             record-position of rot-stof-imp-cou-cobat
                             record-position of rot-stof-add-piombo
                             record-position of rot-stof-costi-aggi
                             record-position of rot-stof-prz-finale
                             record-position of rot-stof-prz-tot-finale
                             giving return-code

                 move 0,1   to WINPRINT-COL-SEPARATION

                 move WPRTUNITS-CENTIMETERS to WINPRINT-COL-UNITS 


                 set WINPRINT-TRANSPARENT to true

      *    codice articolo
                move 0,1              to winprint-col-start
                move wprtalign-center to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column             
                                   giving return-code
      *    descrizione articolo                                         
                add 78-ro-stof-art    to winprint-col-start
                move wprtalign-left   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
      *    cod fornitore
                add 78-ro-stof-des       to winprint-col-start
                move wprtalign-center   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code

      *    qta inevasi
                add 78-ro-STOF-COD-FORN  to winprint-col-start
                move wprtalign-center    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code


      *    imballo|
                add 78-ro-stof-qta    to    winprint-col-start
                move wprtalign-center to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
      *    peso netto
                add 78-ro-stof-imb    to    winprint-col-start
                move wprtalign-center to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
      *    peso netto totale
                add 78-ro-stof-peso-netto   to    winprint-col-start
                move wprtalign-center       to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
      *    peso UTF totale
                add 78-ro-stof-peso-tot-netto  to    winprint-col-start
                move wprtalign-center          to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
      *    listino
                add 78-ro-stof-peso-tot-utf to    winprint-col-start
                move wprtalign-center       to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
      *    sc 1
                add 78-ro-stof-prz-listino  to    winprint-col-start
                move wprtalign-center       to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
      *    sc 2
                add 78-ro-stof-sconto-1  to    winprint-col-start
                move wprtalign-center    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
      *    sc 3
                add 78-ro-stof-sconto-2  to    winprint-col-start
                move wprtalign-center    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
      *    sc 4
                add 78-ro-stof-sconto-3  to    winprint-col-start
                move wprtalign-center    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
      *    sc 5
                add 78-ro-stof-sconto-4  to    winprint-col-start
                move wprtalign-center    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
      *    prezzo netto
                add 78-ro-stof-sconto-5  to    winprint-col-start
                move wprtalign-center    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
      *    imposta consumo
                add 78-ro-stof-prz-netto to    winprint-col-start
                move wprtalign-center    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                        winprint-column
                                   giving return-code
                                   
      *     netto con imposta
                add 78-ro-stof-imp-consumo  to    winprint-col-start
                move wprtalign-center       to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
      *     05 piombio  pic ---.--9,99.
                add 78-ro-stof-imp-cou-cobat   to    winprint-col-start
                move wprtalign-center          to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
      *     05 r-costi-aggi   PIC  ---.--9,99.
                add 78-ro-stof-add-piombo   to    winprint-col-start
                move wprtalign-center       to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
      *     05 r-prz-finale      PIC  ---.--9,99.
                add 78-ro-stof-costi-aggi   to    winprint-col-start
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
      *     05 r-prz-tot-finale  PIC  ---.--9,99.
                add 78-ro-stof-prz-finale   to    winprint-col-start
                move wprtalign-center   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code


      *    ultima colonna
                move 78-ro-stof-prz-tot-finale to winprint-col-start
                move wprtalign-center          to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code


           end-evaluate.
