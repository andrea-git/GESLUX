      ***---
       SETTA-COLONNE-BOZZE.
           perform PULISCI-COLONNE
           initialize winprint-column
           move spl-hfont             to winprint-col-font
           move wprtunits-centimeters to winprint-col-units     
           move wprtalign-none        to winprint-col-alignment
           set  winprint-transparent  to true
           evaluate spl-tipo-colonna
           when 1
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                record-position of r-steva-art
                                record-position of r-steva-des
                                record-position of r-steva-qta
                                record-position of r-steva-imb
                                record-position of r-steva-cod-dog
                                record-position of r-steva-peso-netto
                               record-position of r-steva-peso-tot-netto
                                record-position of r-steva-peso-tot-utf
      *                          record-position of r-steva-prz-listino
                                record-position of r-steva-prz-netto
                                record-position of r-steva-imp-consumo
                                record-position of r-steva-prz-netto-imp
                                record-position of r-steva-imp-cou-cobat
                                record-position of r-steva-add-piombo
                                record-position of r-steva-prz-finale
                               record-position of r-steva-prz-tot-finale
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
                add 1,2              to winprint-col-start
                move wprtalign-left   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
      *    qta ordinato
      *          add 3,2                 to winprint-col-start
                add 7,2                to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
      *    imballo
                add 1,10                 to winprint-col-start
      *          move wprtalign-left   to winprint-col-alignment
                move wprtalign-center   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
      *    codice doganale 
                add 1,45               to    winprint-col-start
                move wprtalign-center to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
      *    peso netto
                add 1,2             to    winprint-col-start
                move wprtalign-center to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
      *    peso netto totale
                add 1,3               to    winprint-col-start
                move wprtalign-center to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
      *    peso UTF totale
                add 1,3             to    winprint-col-start
                move wprtalign-center to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
      **    listino
      *          add 1,3             to    winprint-col-start
      *          move wprtalign-right to winprint-col-alignment
      *          call "WIN$PRINTER"  using winprint-set-page-column, 
      *                                    winprint-column
      *                             giving return-code
                                   
                                   
      *    prezzo netto
                add 1,3                    to    winprint-col-start
                move wprtalign-right to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
      *    imposta consumo
                add 1,7                    to    winprint-col-start
                move wprtalign-right to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                        winprint-column
                                   giving return-code
                                   
      *     netto con imposta
                add 1,7                    to    winprint-col-start
                move wprtalign-right to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
      *    cou/cobat
                add 1,7                    to    winprint-col-start
                move wprtalign-right to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
      *     05 piombio  pic ---.--9,99.
                add 1,7                    to    winprint-col-start
                move wprtalign-right to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
      **     05 r-costi-aggi   PIC  ---.--9,99.
      *          add 1,2                    to    winprint-col-start
      *          move wprtalign-right to winprint-col-alignment
      *          call "WIN$PRINTER"  using winprint-set-page-column, 
      *                                    winprint-column
      *                             giving return-code
                                   
      *     05 r-prz-finale      PIC  ---.--9,99.
                add 1,7                    to    winprint-col-start
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
      *     05 r-prz-tot-finale  PIC  ---.--9,99.
                add 1,7                    to    winprint-col-start
                move wprtalign-right to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code


      *    ultima colonna
                move 28,0       to winprint-col-start
                move wprtalign-right to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code


           end-evaluate.

      ***---
       SETTA-COLONNE-FIDO.
           perform PULISCI-COLONNE
           initialize winprint-column
           move spl-hfont             to winprint-col-font
           move wprtunits-centimeters to winprint-col-units     
           move wprtalign-none        to winprint-col-alignment
           set  winprint-transparent  to true
           evaluate spl-tipo-colonna
           when 1
      *    centratura nel foglio per titoli
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                          1,
                                          200
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
      *     05 piombio  pic ---.--9,99.
                add 19,1                    to    winprint-col-start
                move wprtalign-right to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
           when 2
      *    centratura nel foglio per titoli
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                          1,
                                          15
                                giving return-code

                 move 0,1   to WINPRINT-COL-SEPARATION

                 move WPRTUNITS-CENTIMETERS to WINPRINT-COL-UNITS 

                 set WINPRINT-TRANSPARENT to true

      *    codice articolo
                move 5,0              to winprint-col-start
                move wprtalign-right to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column             
                                   giving return-code
      *     05 piombio  pic ---.--9,99.
                add 2,0                    to    winprint-col-start
                move wprtalign-right to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code

                add 10,0                    to    winprint-col-start
                move wprtalign-right to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code


           end-evaluate.
