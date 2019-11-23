      * Paragrafo per la struttura dei controlli sulla screen Form1
           evaluate control-id
           |78-ID-EF-data-FROM è l'ID del control EF-data-FROM
           when 78-ID-EF-data-FROM
                inquire EF-data-FROM, value in EF-data-FROM-buf

           |78-ID-EF-DATA-TO è l'ID del control EF-DATA-TO
           when 78-ID-EF-DATA-TO
                inquire EF-DATA-TO, value in EF-DATA-TO-buf

           end-evaluate.

      * Paragrafo per la struttura dei controlli sulla screen Form2
           evaluate control-id
           end-evaluate.

      * Paragrafo per la struttura dei controlli sulla screen form3
           evaluate control-id
           end-evaluate.

