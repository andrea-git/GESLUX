      * Paragrafo per la struttura dei controlli sulla screen Screen1
           evaluate control-id
           |78-ID-ef-data-bolla è l'ID del control ef-data-bolla
           when 78-ID-ef-data-bolla
                inquire ef-data-bolla, value in ef-data-bolla-buf

           |78-ID-ef-num-bolla è l'ID del control ef-num-bolla
           when 78-ID-ef-num-bolla
                inquire ef-num-bolla, value in ef-num-bolla-buf

           end-evaluate.

