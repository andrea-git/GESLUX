      * Paragrafo per la struttura dei controlli sulla screen Form1
           evaluate control-id
           |78-ID-ef-codice è l'ID del control ef-codice
           when 78-ID-ef-codice
                inquire ef-codice, value in ef-codice-buf

           |78-ID-ef-des è l'ID del control ef-des
           when 78-ID-ef-des
                inquire ef-des, value in ef-des-buf

           end-evaluate.

