      * Paragrafo per la struttura dei controlli sulla screen Form1
           evaluate control-id
           |78-ID-EF-ANNO � l'ID del control EF-ANNO
           when 78-ID-EF-ANNO
                inquire EF-ANNO, value in EF-ANNO-buf

           |78-ID-EF-nota-FROM � l'ID del control EF-nota-FROM
           when 78-ID-EF-nota-FROM
                inquire EF-nota-FROM, value in EF-nota-FROM-buf

           |78-ID-EF-nota-TO � l'ID del control EF-nota-TO
           when 78-ID-EF-nota-TO
                inquire EF-nota-TO, value in EF-nota-TO-buf

           |78-ID-EF-data-FROM � l'ID del control EF-data-FROM
           when 78-ID-EF-data-FROM
                inquire EF-data-FROM, value in EF-data-FROM-buf

           |78-ID-EF-DATA-TO � l'ID del control EF-DATA-TO
           when 78-ID-EF-DATA-TO
                inquire EF-DATA-TO, value in EF-DATA-TO-buf

           |78-ID-EF-CLIENTE-FROM � l'ID del control EF-CLIENTE-FROM
           when 78-ID-EF-CLIENTE-FROM
                inquire EF-CLIENTE-FROM, value in EF-CLIENTE-FROM-buf

           |78-ID-EF-CLIENTE-TO � l'ID del control EF-CLIENTE-TO
           when 78-ID-EF-CLIENTE-TO
                inquire EF-CLIENTE-TO, value in EF-CLIENTE-TO-buf

           end-evaluate.

      * Paragrafo per la struttura dei controlli sulla screen form3
           evaluate control-id
           end-evaluate.

