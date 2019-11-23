      * Paragrafo per la struttura dei controlli sulla screen Form1
           evaluate control-id
           |78-ID-ef-codice è l'ID del control ef-codice
           when 78-ID-ef-codice
                inquire ef-codice, value in ef-codice-buf

           |78-ID-ef-capo è l'ID del control ef-capo
           when 78-ID-ef-capo
                inquire ef-capo, value in ef-capo-buf

           |78-ID-ef-data è l'ID del control ef-data
           when 78-ID-ef-data
                inquire ef-data, value in ef-data-buf

           |78-ID-ef-tipocli è l'ID del control ef-tipocli
           when 78-ID-ef-tipocli
                inquire ef-tipocli, value in ef-tipocli-buf

           |78-ID-ef-gruppo è l'ID del control ef-gruppo
           when 78-ID-ef-gruppo
                inquire ef-gruppo, value in ef-gruppo-buf

           |78-ID-ef-cli-rifer è l'ID del control ef-cli-rifer
           when 78-ID-ef-cli-rifer
                inquire ef-cli-rifer, value in ef-cli-rifer-buf

           end-evaluate.

