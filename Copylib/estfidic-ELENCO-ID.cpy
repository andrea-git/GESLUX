      * Paragrafo per la struttura dei controlli sulla screen Form1
           evaluate control-id
           |78-ID-ef-data-from è l'ID del control ef-data-from
           when 78-ID-ef-data-from
                inquire ef-data-from, value in ef-data-from-buf

           |78-ID-ef-data-to è l'ID del control ef-data-to
           when 78-ID-ef-data-to
                inquire ef-data-to, value in ef-data-to-buf

           |78-ID-ef-tipo è l'ID del control ef-tipo
           when 78-ID-ef-tipo
                inquire ef-tipo, value in ef-tipo-buf

           |78-ID-ef-age è l'ID del control ef-age
           when 78-ID-ef-age
                inquire ef-age, value in ef-age-buf

           end-evaluate.

      * Paragrafo per la struttura dei controlli sulla screen form3
           evaluate control-id
           end-evaluate.

