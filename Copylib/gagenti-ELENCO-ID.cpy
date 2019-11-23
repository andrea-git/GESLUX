      * Paragrafo per la struttura dei controlli sulla screen Form1
           evaluate control-id
           |78-ID-ef-codice è l'ID del control ef-codice
           when 78-ID-ef-codice
                inquire ef-codice, value in ef-codice-buf

           |78-ID-ef-ragsoc-1 è l'ID del control ef-ragsoc-1
           when 78-ID-ef-ragsoc-1
                inquire ef-ragsoc-1, value in ef-ragsoc-1-buf

           |78-ID-ef-provincia è l'ID del control ef-provincia
           when 78-ID-ef-provincia
                inquire ef-provincia, value in ef-provincia-buf

           |78-ID-ef-nazione è l'ID del control ef-nazione
           when 78-ID-ef-nazione
                inquire ef-nazione, value in ef-nazione-buf

           |78-ID-ef-marg è l'ID del control ef-marg
           when 78-ID-ef-marg
                inquire ef-marg, value in ef-marg-buf

           |78-ID-ef-listino è l'ID del control ef-listino
           when 78-ID-ef-listino
                inquire ef-listino, value in ef-listino-buf

           end-evaluate.

