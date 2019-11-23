      * Paragrafo per la struttura dei controlli sulla screen Form1
           evaluate control-id
           |78-ID-ef-codice è l'ID del control ef-codice
           when 78-ID-ef-codice
                inquire ef-codice, value in ef-codice-buf

           |78-ID-ef-descrizione è l'ID del control ef-descrizione
           when 78-ID-ef-descrizione
                inquire ef-descrizione, value in ef-descrizione-buf

           |78-ID-ef-caus-trasp è l'ID del control ef-caus-trasp
           when 78-ID-ef-caus-trasp
                inquire ef-caus-trasp, value in ef-caus-trasp-buf

           |78-ID-chk-movim-magaz è l'ID del control chk-movim-magaz
           when 78-ID-chk-movim-magaz
                inquire chk-movim-magaz, value in chk-movim-magaz-buf

           |78-ID-ef-cod-magaz è l'ID del control ef-cod-magaz
           when 78-ID-ef-cod-magaz
                inquire ef-cod-magaz, value in ef-cod-magaz-buf

           end-evaluate.

