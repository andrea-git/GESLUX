      * Paragrafo per la struttura dei controlli sulla screen scr-ordini
           evaluate control-id
           |78-ID-ef-gdo è l'ID del control ef-gdo
           when 78-ID-ef-gdo
                inquire ef-gdo, value in ef-gdo-buf

           |78-ID-ef-cli è l'ID del control ef-cli
           when 78-ID-ef-cli
                inquire ef-cli, value in ef-cli-buf

           |78-ID-ef-data è l'ID del control ef-data
           when 78-ID-ef-data
                inquire ef-data, value in ef-data-buf

           end-evaluate.

      * Paragrafo per la struttura dei controlli sulla screen scr-azione
           evaluate control-id
           end-evaluate.

