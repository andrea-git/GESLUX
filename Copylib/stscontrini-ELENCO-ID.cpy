      * Paragrafo per la struttura dei controlli sulla screen Screen2
           evaluate control-id
           |78-ID-ef-anno � l'ID del control ef-anno
           when 78-ID-ef-anno
                inquire ef-anno, value in ef-anno-buf

           |78-ID-ef-num-to � l'ID del control ef-num-to
           when 78-ID-ef-num-to
                inquire ef-num-to, value in ef-num-to-buf

           |78-ID-ef-data-to � l'ID del control ef-data-to
           when 78-ID-ef-data-to
                inquire ef-data-to, value in ef-data-to-buf

           |78-ID-ef-cli � l'ID del control ef-cli
           when 78-ID-ef-cli
                inquire ef-cli, value in ef-cli-buf

           end-evaluate.

