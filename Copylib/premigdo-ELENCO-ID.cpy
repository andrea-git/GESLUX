      * Paragrafo per la struttura dei controlli sulla screen Form1
           evaluate control-id
           |78-ID-ef-data-from � l'ID del control ef-data-from
           when 78-ID-ef-data-from
                inquire ef-data-from, value in ef-data-from-buf

           |78-ID-ef-data-to � l'ID del control ef-data-to
           when 78-ID-ef-data-to
                inquire ef-data-to, value in ef-data-to-buf

           |78-ID-ef-tipo � l'ID del control ef-tipo
           when 78-ID-ef-tipo
                inquire ef-tipo, value in ef-tipo-buf

           end-evaluate.

      * Paragrafo per la struttura dei controlli sulla screen form2
           evaluate control-id
           end-evaluate.

