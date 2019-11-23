      * Paragrafo per la struttura dei controlli sulla screen Form1
           evaluate control-id
           |78-ID-EF-ANNO è l'ID del control EF-ANNO
           when 78-ID-EF-ANNO
                inquire EF-ANNO, value in EF-ANNO-buf

           |78-ID-EF-ordine-FROM è l'ID del control EF-ordine-FROM
           when 78-ID-EF-ordine-FROM
                inquire EF-ordine-FROM, value in EF-ordine-FROM-buf

           |78-ID-EF-ordine-TO è l'ID del control EF-ordine-TO
           when 78-ID-EF-ordine-TO
                inquire EF-ordine-TO, value in EF-ordine-TO-buf

           |78-ID-EF-data-FROM è l'ID del control EF-data-FROM
           when 78-ID-EF-data-FROM
                inquire EF-data-FROM, value in EF-data-FROM-buf

           |78-ID-EF-DATA-TO è l'ID del control EF-DATA-TO
           when 78-ID-EF-DATA-TO
                inquire EF-DATA-TO, value in EF-DATA-TO-buf

           |78-ID-EF-FORN-FROM è l'ID del control EF-FORN-FROM
           when 78-ID-EF-FORN-FROM
                inquire EF-FORN-FROM, value in EF-FORN-FROM-buf

           |78-ID-EF-DEST-FORN-FROM è l'ID del control EF-DEST-FORN-FROM
           when 78-ID-EF-DEST-FORN-FROM
                inquire EF-DEST-FORN-FROM, value in 
           EF-DEST-FORN-FROM-buf

           |78-ID-EF-CLIENTE-FROM è l'ID del control EF-CLIENTE-FROM
           when 78-ID-EF-CLIENTE-FROM
                inquire EF-CLIENTE-FROM, value in EF-CLIENTE-FROM-buf

           |78-ID-EF-DEST-CLI-FROM è l'ID del control EF-DEST-CLI-FROM
           when 78-ID-EF-DEST-CLI-FROM
                inquire EF-DEST-CLI-FROM, value in EF-DEST-CLI-FROM-buf

           end-evaluate.

      * Paragrafo per la struttura dei controlli sulla screen form3
           evaluate control-id
           end-evaluate.

