      * Paragrafo per la struttura dei controlli sulla screen Form1
           evaluate control-id
           |78-ID-ef-data-from è l'ID del control ef-data-from
           when 78-ID-ef-data-from
                inquire ef-data-from, value in ef-data-from-buf

           |78-ID-ef-data-to è l'ID del control ef-data-to
           when 78-ID-ef-data-to
                inquire ef-data-to, value in ef-data-to-buf

           |78-ID-ef-tipocli è l'ID del control ef-tipocli
           when 78-ID-ef-tipocli
                inquire ef-tipocli, value in ef-tipocli-buf

           |78-ID-ef-gdo è l'ID del control ef-gdo
           when 78-ID-ef-gdo
                inquire ef-gdo, value in ef-gdo-buf

           |78-ID-ef-cod è l'ID del control ef-cod
           when 78-ID-ef-cod
                inquire ef-cod, value in ef-cod-buf

           |78-ID-ef-des è l'ID del control ef-des
           when 78-ID-ef-des
                inquire ef-des, value in ef-des-buf

           |78-ID-ef-scad è l'ID del control ef-scad
           when 78-ID-ef-scad
                inquire ef-scad, value in ef-scad-buf

           end-evaluate.

      * Paragrafo per la struttura dei controlli sulla screen form3
           evaluate control-id
           end-evaluate.

      * Paragrafo per la struttura dei controlli sulla screen Form2
           evaluate control-id
           end-evaluate.

