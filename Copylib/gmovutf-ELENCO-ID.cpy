      * Paragrafo per la struttura dei controlli sulla screen Form1
           evaluate control-id
           |78-ID-ef-prog � l'ID del control ef-prog
           when 78-ID-ef-prog
                inquire ef-prog, value in ef-prog-buf

           |78-ID-ef-data � l'ID del control ef-data
           when 78-ID-ef-data
                inquire ef-data, value in ef-data-buf

           |78-ID-ef-kg � l'ID del control ef-kg
           when 78-ID-ef-kg
                inquire ef-kg, value in ef-kg-buf

           |78-ID-ef-codice � l'ID del control ef-codice
           when 78-ID-ef-codice
                inquire ef-codice, value in ef-codice-buf

           end-evaluate.

