      * Paragrafo per la struttura dei controlli sulla screen Form1
           evaluate control-id
           |78-ID-ef-gdo è l'ID del control ef-gdo
           when 78-ID-ef-gdo
                inquire ef-gdo, value in ef-gdo-buf

           |78-ID-ef-cli è l'ID del control ef-cli
           when 78-ID-ef-cli
                inquire ef-cli, value in ef-cli-buf

           |78-ID-ef-dest è l'ID del control ef-dest
           when 78-ID-ef-dest
                inquire ef-dest, value in ef-dest-buf

           |78-ID-ef-art è l'ID del control ef-art
           when 78-ID-ef-art
                inquire ef-art, value in ef-art-buf

           |78-ID-ef-data è l'ID del control ef-data
           when 78-ID-ef-data
                inquire ef-data, value in ef-data-buf

           |78-ID-ef-prezzo è l'ID del control ef-prezzo
           when 78-ID-ef-prezzo
                inquire ef-prezzo, value in ef-prezzo-buf

           |78-ID-ef-sconto è l'ID del control ef-sconto
           when 78-ID-ef-sconto
                inquire ef-sconto, value in ef-sconto-buf

           |78-ID-cbo-stato è l'ID del control cbo-stato
           when 78-ID-cbo-stato
                inquire cbo-stato, value in cbo-stato-buf

           end-evaluate.

