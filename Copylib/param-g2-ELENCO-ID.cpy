      * Paragrafo per la struttura dei controlli sulla screen Form1
           evaluate control-id
           |78-ID-ef-codice è l'ID del control ef-codice
           when 78-ID-ef-codice
                inquire ef-codice, value in ef-codice-buf

           |78-ID-ef-fat è l'ID del control ef-fat
           when 78-ID-ef-fat
                inquire ef-fat, value in ef-fat-buf

           |78-ID-ef-nc è l'ID del control ef-nc
           when 78-ID-ef-nc
                inquire ef-nc, value in ef-nc-buf

           end-evaluate.

