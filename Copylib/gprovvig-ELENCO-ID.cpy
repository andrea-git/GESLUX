      * Paragrafo per la struttura dei controlli sulla screen Form1
           evaluate control-id
           |78-ID-ef-riga è l'ID del control ef-riga
           when 78-ID-ef-riga
                inquire ef-riga, value in ef-riga-buf

           |78-ID-ef-data è l'ID del control ef-data
           when 78-ID-ef-data
                inquire ef-data, value in ef-data-buf

           |78-ID-ef-age è l'ID del control ef-age
           when 78-ID-ef-age
                inquire ef-age, value in ef-age-buf

           |78-ID-ef-cli è l'ID del control ef-cli
           when 78-ID-ef-cli
                inquire ef-cli, value in ef-cli-buf

           |78-ID-ef-art è l'ID del control ef-art
           when 78-ID-ef-art
                inquire ef-art, value in ef-art-buf

           |78-ID-ef-qta è l'ID del control ef-qta
           when 78-ID-ef-qta
                inquire ef-qta, value in ef-qta-buf

           |78-ID-ef-prz-ven è l'ID del control ef-prz-ven
           when 78-ID-ef-prz-ven
                inquire ef-prz-ven, value in ef-prz-ven-buf

           |78-ID-ef-prz-age è l'ID del control ef-prz-age
           when 78-ID-ef-prz-age
                inquire ef-prz-age, value in ef-prz-age-buf

           |78-ID-ef-lis è l'ID del control ef-lis
           when 78-ID-ef-lis
                inquire ef-lis, value in ef-lis-buf

           |78-ID-ef-liq è l'ID del control ef-liq
           when 78-ID-ef-liq
                inquire ef-liq, value in ef-liq-buf

           end-evaluate.

