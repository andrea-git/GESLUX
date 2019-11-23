      * Paragrafo per la struttura dei controlli sulla screen Form1
           evaluate control-id
           |78-ID-ef-codice è l'ID del control ef-codice
           when 78-ID-ef-codice
                inquire ef-codice, value in ef-codice-buf

           |78-ID-ef-descrizione1 è l'ID del control ef-descrizione1
           when 78-ID-ef-descrizione1
                inquire ef-descrizione1, value in ef-descrizione1-buf

           end-evaluate.

