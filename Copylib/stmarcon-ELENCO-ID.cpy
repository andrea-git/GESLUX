      * Paragrafo per la struttura dei controlli sulla screen Form1
           evaluate control-id
           |78-ID-ef-tipo è l'ID del control ef-tipo
           when 78-ID-ef-tipo
                inquire ef-tipo, value in ef-tipo-buf

           |78-ID-ef-marca è l'ID del control ef-marca
           when 78-ID-ef-marca
                inquire ef-marca, value in ef-marca-buf

           end-evaluate.

      * Paragrafo per la struttura dei controlli sulla screen form3
           evaluate control-id
           end-evaluate.

