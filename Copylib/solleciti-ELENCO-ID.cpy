      * Paragrafo per la struttura dei controlli sulla screen Form1
           evaluate control-id
           |78-ID-EF-data-FROM è l'ID del control EF-data-FROM
           when 78-ID-EF-data-FROM
                inquire EF-data-FROM, value in EF-data-FROM-buf

           |78-ID-EF-DATA-TO è l'ID del control EF-DATA-TO
           when 78-ID-EF-DATA-TO
                inquire EF-DATA-TO, value in EF-DATA-TO-buf

           |78-ID-ef-cli è l'ID del control ef-cli
           when 78-ID-ef-cli
                inquire ef-cli, value in ef-cli-buf

           |78-ID-ef-dest è l'ID del control ef-dest
           when 78-ID-ef-dest
                inquire ef-dest, value in ef-dest-buf

           |78-ID-ef-age è l'ID del control ef-age
           when 78-ID-ef-age
                inquire ef-age, value in ef-age-buf

           |78-ID-ef-gdo è l'ID del control ef-gdo
           when 78-ID-ef-gdo
                inquire ef-gdo, value in ef-gdo-buf

           |78-ID-ef-tipo è l'ID del control ef-tipo
           when 78-ID-ef-tipo
                inquire ef-tipo, value in ef-tipo-buf

           |78-ID-ef-art è l'ID del control ef-art
           when 78-ID-ef-art
                inquire ef-art, value in ef-art-buf

           end-evaluate.

      * Paragrafo per la struttura dei controlli sulla screen scr-elab
           evaluate control-id
           end-evaluate.

      * Paragrafo per la struttura dei controlli sulla screen scr-ordini
           evaluate control-id
           end-evaluate.

      * Paragrafo per la struttura dei controlli sulla screen scr-excel
           evaluate control-id
           end-evaluate.

