      * Paragrafo per la struttura dei controlli sulla screen Screen2
           evaluate control-id
           |78-ID-ef-anno � l'ID del control ef-anno
           when 78-ID-ef-anno
                inquire ef-anno, value in ef-anno-buf

           |78-ID-ef-caus � l'ID del control ef-caus
           when 78-ID-ef-caus
                inquire ef-caus, value in ef-caus-buf

           |78-ID-ef-num-from � l'ID del control ef-num-from
           when 78-ID-ef-num-from
                inquire ef-num-from, value in ef-num-from-buf

           |78-ID-ef-num-to � l'ID del control ef-num-to
           when 78-ID-ef-num-to
                inquire ef-num-to, value in ef-num-to-buf

           end-evaluate.

