      * Paragrafo per la struttura dei controlli sulla screen Form1
           evaluate control-id
           |78-ID-ef-codice è l'ID del control ef-codice
           when 78-ID-ef-codice
                inquire ef-codice, value in ef-codice-buf

           |78-ID-ef-molt è l'ID del control ef-molt
           when 78-ID-ef-molt
                inquire ef-molt, value in ef-molt-buf

           |78-ID-ef-perce-confronto è l'ID del control ef-perce-confronto
           when 78-ID-ef-perce-confronto
                inquire ef-perce-confronto, value in 
           ef-perce-confronto-buf

           end-evaluate.

