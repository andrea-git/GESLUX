      * Paragrafo per la struttura dei controlli sulla screen form-solleciti
           evaluate control-id
           |78-ID-ef-data è l'ID del control ef-data
           when 78-ID-ef-data
                inquire ef-data, value in ef-data-buf

           |78-ID-ef-qta è l'ID del control ef-qta
           when 78-ID-ef-qta
                inquire ef-qta, value in ef-qta-buf

           end-evaluate.

