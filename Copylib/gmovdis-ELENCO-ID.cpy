      * Paragrafo per la struttura dei controlli sulla screen Form1
           evaluate control-id
           |78-ID-ef-data è l'ID del control ef-data
           when 78-ID-ef-data
                inquire ef-data, value in ef-data-buf

           |78-ID-ef-causale è l'ID del control ef-causale
           when 78-ID-ef-causale
                inquire ef-causale, value in ef-causale-buf

           |78-ID-ef-for è l'ID del control ef-for
           when 78-ID-ef-for
                inquire ef-for, value in ef-for-buf

           |78-ID-ef-num è l'ID del control ef-num
           when 78-ID-ef-num
                inquire ef-num, value in ef-num-buf

           |78-ID-ef-data-doc è l'ID del control ef-data-doc
           when 78-ID-ef-data-doc
                inquire ef-data-doc, value in ef-data-doc-buf

           |78-ID-ef-pag è l'ID del control ef-pag
           when 78-ID-ef-pag
                inquire ef-pag, value in ef-pag-buf

           |78-ID-ef-art è l'ID del control ef-art
           when 78-ID-ef-art
                inquire ef-art, value in ef-art-buf

           |78-ID-ef-qta è l'ID del control ef-qta
           when 78-ID-ef-qta
                inquire ef-qta, value in ef-qta-buf

           |10 è l'ID del control ef-art-v
           when 10
                inquire ef-art-v, value in ef-art-v-buf

           |11 è l'ID del control ef-mag
           when 11
                inquire ef-mag, value in ef-mag-buf

           |12 è l'ID del control ef-imb
           when 12
                inquire ef-imb, value in ef-imb-buf

           end-evaluate.

      * Paragrafo per la struttura dei controlli sulla screen form-num
           evaluate control-id
           end-evaluate.

