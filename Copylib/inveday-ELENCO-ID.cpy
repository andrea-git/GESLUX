      * Paragrafo per la struttura dei controlli sulla screen Screen2
           evaluate control-id
           |78-ID-ef-data è l'ID del control ef-data
           when 78-ID-ef-data
                inquire ef-data, value in ef-data-buf

           |78-ID-ef-pag è l'ID del control ef-pag
           when 78-ID-ef-pag
                inquire ef-pag, value in ef-pag-buf

           |78-ID-ef-mag è l'ID del control ef-mag
           when 78-ID-ef-mag
                inquire ef-mag, value in ef-mag-buf

           |78-ID-ef-marca è l'ID del control ef-marca
           when 78-ID-ef-marca
                inquire ef-marca, value in ef-marca-buf

           end-evaluate.

