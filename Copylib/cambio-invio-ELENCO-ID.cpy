      * Paragrafo per la struttura dei controlli sulla screen Form1
           evaluate control-id
           |78-ID-ef-data-from è l'ID del control ef-data-from
           when 78-ID-ef-data-from
                inquire ef-data-from, value in ef-data-from-buf

           |78-ID-ef-data-to è l'ID del control ef-data-to
           when 78-ID-ef-data-to
                inquire ef-data-to, value in ef-data-to-buf

           |78-ID-ef-cli è l'ID del control ef-cli
           when 78-ID-ef-cli
                inquire ef-cli, value in ef-cli-buf

           |78-ID-ef-des è l'ID del control ef-des
           when 78-ID-ef-des
                inquire ef-des, value in ef-des-buf

           end-evaluate.

