      * Paragrafo per la struttura dei controlli sulla screen Form1
           evaluate control-id
           |78-ID-EF-ANNO è l'ID del control EF-ANNO
           when 78-ID-EF-ANNO
                inquire EF-ANNO, value in EF-ANNO-buf

           |78-ID-EF-fatman-FROM è l'ID del control EF-fatman-FROM
           when 78-ID-EF-fatman-FROM
                inquire EF-fatman-FROM, value in EF-fatman-FROM-buf

           |78-ID-EF-fatman-TO è l'ID del control EF-fatman-TO
           when 78-ID-EF-fatman-TO
                inquire EF-fatman-TO, value in EF-fatman-TO-buf

           |78-ID-EF-data-FROM è l'ID del control EF-data-FROM
           when 78-ID-EF-data-FROM
                inquire EF-data-FROM, value in EF-data-FROM-buf

           |78-ID-EF-DATA-TO è l'ID del control EF-DATA-TO
           when 78-ID-EF-DATA-TO
                inquire EF-DATA-TO, value in EF-DATA-TO-buf

           |78-ID-EF-CLIENTE-FROM è l'ID del control EF-CLIENTE-FROM
           when 78-ID-EF-CLIENTE-FROM
                inquire EF-CLIENTE-FROM, value in EF-CLIENTE-FROM-buf

           |78-ID-EF-CLIENTE-TO è l'ID del control EF-CLIENTE-TO
           when 78-ID-EF-CLIENTE-TO
                inquire EF-CLIENTE-TO, value in EF-CLIENTE-TO-buf

           end-evaluate.

      * Paragrafo per la struttura dei controlli sulla screen form3
           evaluate control-id
           end-evaluate.

