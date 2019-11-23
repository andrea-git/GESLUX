      * Paragrafo per la struttura dei controlli sulla screen form1
           evaluate control-id
           |78-ID-ef-anno è l'ID del control ef-anno
           when 78-ID-ef-anno
                inquire ef-anno, value in ef-anno-buf

           |78-ID-ef-num-da è l'ID del control ef-num-da
           when 78-ID-ef-num-da
                inquire ef-num-da, value in ef-num-da-buf

           |78-ID-ef-num-a è l'ID del control ef-num-a
           when 78-ID-ef-num-a
                inquire ef-num-a, value in ef-num-a-buf

           |78-ID-ef-num-da-e è l'ID del control ef-num-da-e
           when 78-ID-ef-num-da-e
                inquire ef-num-da-e, value in ef-num-da-e-buf

           |78-ID-ef-num-a-e è l'ID del control ef-num-a-e
           when 78-ID-ef-num-a-e
                inquire ef-num-a-e, value in ef-num-a-e-buf

           end-evaluate.

      * Paragrafo per la struttura dei controlli sulla screen scr-fine
           evaluate control-id
           end-evaluate.

      * Paragrafo per la struttura dei controlli sulla screen scr-elab-postel
           evaluate control-id
           end-evaluate.

      * Paragrafo per la struttura dei controlli sulla screen scr-elab-edi
           evaluate control-id
           end-evaluate.

