      * Paragrafo per la struttura dei controlli sulla screen Form1
           evaluate control-id
           |78-ID-ef-data-from è l'ID del control ef-data-from
           when 78-ID-ef-data-from
                inquire ef-data-from, value in ef-data-from-buf

           |78-ID-ef-data-to è l'ID del control ef-data-to
           when 78-ID-ef-data-to
                inquire ef-data-to, value in ef-data-to-buf

           |78-ID-ef-cod è l'ID del control ef-cod
           when 78-ID-ef-cod
                inquire ef-cod, value in ef-cod-buf

           |78-ID-ef-des-forn è l'ID del control ef-des-forn
           when 78-ID-ef-des-forn
                inquire ef-des-forn, value in ef-des-forn-buf

           |78-ID-ef-cliente è l'ID del control ef-cliente
           when 78-ID-ef-cliente
                inquire ef-cliente, value in ef-cliente-buf

           |78-ID-ef-des-cli è l'ID del control ef-des-cli
           when 78-ID-ef-des-cli
                inquire ef-des-cli, value in ef-des-cli-buf

           |78-ID-ef-marca è l'ID del control ef-marca
           when 78-ID-ef-marca
                inquire ef-marca, value in ef-marca-buf

           |78-ID-ef-art è l'ID del control ef-art
           when 78-ID-ef-art
                inquire ef-art, value in ef-art-buf

           end-evaluate.

      * Paragrafo per la struttura dei controlli sulla screen form3
           evaluate control-id
           end-evaluate.

