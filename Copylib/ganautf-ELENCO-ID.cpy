      * Paragrafo per la struttura dei controlli sulla screen Form1
           evaluate control-id
           |78-ID-ef-num-reg è l'ID del control ef-num-reg
           when 78-ID-ef-num-reg
                inquire ef-num-reg, value in ef-num-reg-buf

           |78-ID-ef-provincia è l'ID del control ef-provincia
           when 78-ID-ef-provincia
                inquire ef-provincia, value in ef-provincia-buf

           |78-ID-ef-partita-iva è l'ID del control ef-partita-iva
           when 78-ID-ef-partita-iva
                inquire ef-partita-iva, value in ef-partita-iva-buf

           |78-ID-ef-data-lic è l'ID del control ef-data-lic
           when 78-ID-ef-data-lic
                inquire ef-data-lic, value in ef-data-lic-buf

           |78-ID-ef-serie-1 è l'ID del control ef-serie-1
           when 78-ID-ef-serie-1
                inquire ef-serie-1, value in ef-serie-1-buf

           |78-ID-ef-serie-2 è l'ID del control ef-serie-2
           when 78-ID-ef-serie-2
                inquire ef-serie-2, value in ef-serie-2-buf

           |78-ID-ef-serie-3 è l'ID del control ef-serie-3
           when 78-ID-ef-serie-3
                inquire ef-serie-3, value in ef-serie-3-buf

           |78-ID-ef-data-ult-stampa è l'ID del control ef-data-ult-stampa
           when 78-ID-ef-data-ult-stampa
                inquire ef-data-ult-stampa, value in 
           ef-data-ult-stampa-buf

           end-evaluate.

      * Paragrafo per la struttura dei controlli sulla screen scr-anno
           evaluate control-id
           end-evaluate.

