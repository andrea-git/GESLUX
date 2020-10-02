      * Paragrafo per la struttura dei controlli sulla screen Form1
           evaluate control-id
           |78-ID-ef-data-from è l'ID del control ef-data-from
           when 78-ID-ef-data-from
                inquire ef-data-from, value in ef-data-from-buf

           |78-ID-ef-data-to è l'ID del control ef-data-to
           when 78-ID-ef-data-to
                inquire ef-data-to, value in ef-data-to-buf

           |78-ID-ef-age è l'ID del control ef-age
           when 78-ID-ef-age
                inquire ef-age, value in ef-age-buf

           |78-ID-ef-marca è l'ID del control ef-marca
           when 78-ID-ef-marca
                inquire ef-marca, value in ef-marca-buf

           |78-ID-ef-art è l'ID del control ef-art
           when 78-ID-ef-art
                inquire ef-art, value in ef-art-buf

           end-evaluate.

      * Paragrafo per la struttura dei controlli sulla screen form3
           evaluate control-id
           end-evaluate.

      * Paragrafo per la struttura dei controlli sulla screen form4
           evaluate control-id
           end-evaluate.

