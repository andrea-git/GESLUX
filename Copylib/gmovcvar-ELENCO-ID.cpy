      * Paragrafo per la struttura dei controlli sulla screen Form1
           evaluate control-id
           |78-ID-ef-data è l'ID del control ef-data
           when 78-ID-ef-data
                inquire ef-data, value in ef-data-buf

           |78-ID-ef-causale è l'ID del control ef-causale
           when 78-ID-ef-causale
                inquire ef-causale, value in ef-causale-buf

           |78-ID-ef-clifor è l'ID del control ef-clifor
           when 78-ID-ef-clifor
                inquire ef-clifor, value in ef-clifor-buf

           |78-ID-ef-destino è l'ID del control ef-destino
           when 78-ID-ef-destino
                inquire ef-destino, value in ef-destino-buf

           |78-ID-ef-vettore è l'ID del control ef-vettore
           when 78-ID-ef-vettore
                inquire ef-vettore, value in ef-vettore-buf

           |78-ID-ef-num è l'ID del control ef-num
           when 78-ID-ef-num
                inquire ef-num, value in ef-num-buf

           |78-ID-ef-data-doc è l'ID del control ef-data-doc
           when 78-ID-ef-data-doc
                inquire ef-data-doc, value in ef-data-doc-buf

           |78-ID-ef-via è l'ID del control ef-via
           when 78-ID-ef-via
                inquire ef-via, value in ef-via-buf

           |78-ID-ef-data-via è l'ID del control ef-data-via
           when 78-ID-ef-data-via
                inquire ef-data-via, value in ef-data-via-buf

           |78-ID-ef-pag è l'ID del control ef-pag
           when 78-ID-ef-pag
                inquire ef-pag, value in ef-pag-buf

           |78-ID-ef-mag è l'ID del control ef-mag
           when 78-ID-ef-mag
                inquire ef-mag, value in ef-mag-buf

           |78-ID-cbo-stato è l'ID del control cbo-stato
           when 78-ID-cbo-stato
                inquire cbo-stato, value in cbo-stato-buf

           |78-ID-ef-art è l'ID del control ef-art
           when 78-ID-ef-art
                inquire ef-art, value in ef-art-buf

           |78-ID-ef-qta è l'ID del control ef-qta
           when 78-ID-ef-qta
                inquire ef-qta, value in ef-qta-buf

           |78-ID-ef-listino è l'ID del control ef-listino
           when 78-ID-ef-listino
                inquire ef-listino, value in ef-listino-buf

           |78-ID-ef-sconto è l'ID del control ef-sconto
           when 78-ID-ef-sconto
                inquire ef-sconto, value in ef-sconto-buf

           |78-ID-ef-cons è l'ID del control ef-cons
           when 78-ID-ef-cons
                inquire ef-cons, value in ef-cons-buf

           |78-ID-ef-coubat è l'ID del control ef-coubat
           when 78-ID-ef-coubat
                inquire ef-coubat, value in ef-coubat-buf

           |24 è l'ID del control ef-imp-merce
           when 24
                inquire ef-imp-merce, value in ef-imp-merce-buf

           end-evaluate.

      * Paragrafo per la struttura dei controlli sulla screen Form2
           evaluate control-id
           end-evaluate.

