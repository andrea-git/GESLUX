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

           |78-ID-EF-CAUSALE-FROM è l'ID del control EF-CAUSALE-FROM
           when 78-ID-EF-CAUSALE-FROM
                inquire EF-CAUSALE-FROM, value in EF-CAUSALE-FROM-buf

           |78-ID-EF-CAUSALE-TO è l'ID del control EF-CAUSALE-TO
           when 78-ID-EF-CAUSALE-TO
                inquire EF-CAUSALE-TO, value in EF-CAUSALE-TO-buf

           end-evaluate.

      * Paragrafo per la struttura dei controlli sulla screen form3
           evaluate control-id
           end-evaluate.

