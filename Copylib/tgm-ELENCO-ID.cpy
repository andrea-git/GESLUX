      * Paragrafo per la struttura dei controlli sulla screen Form1
           evaluate control-id
           |78-ID-ef-mag è l'ID del control ef-mag
           when 78-ID-ef-mag
                inquire ef-mag, value in ef-mag-buf

           |78-ID-ef-anno è l'ID del control ef-anno
           when 78-ID-ef-anno
                inquire ef-anno, value in ef-anno-buf

           |78-ID-ef-mese è l'ID del control ef-mese
           when 78-ID-ef-mese
                inquire ef-mese, value in ef-mese-buf

           |78-ID-ef-pag è l'ID del control ef-pag
           when 78-ID-ef-pag
                inquire ef-pag, value in ef-pag-buf

           end-evaluate.

