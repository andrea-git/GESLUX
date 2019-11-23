      * Paragrafo per la struttura dei controlli sulla screen Form1
           evaluate control-id
           |78-ID-EF-COD-ARTICOLO è l'ID del control EF-COD-ARTICOLO
           when 78-ID-EF-COD-ARTICOLO
                inquire EF-COD-ARTICOLO, value in EF-COD-ARTICOLO-buf

           |78-ID-EF-COD-MAGAZZ è l'ID del control EF-COD-MAGAZZ
           when 78-ID-EF-COD-MAGAZZ
                inquire EF-COD-MAGAZZ, value in EF-COD-MAGAZZ-buf

           |78-ID-EF-TIPO-IMBALLO è l'ID del control EF-TIPO-IMBALLO
           when 78-ID-EF-TIPO-IMBALLO
                inquire EF-TIPO-IMBALLO, value in EF-TIPO-IMBALLO-buf

           |78-ID-EF-PESO è l'ID del control EF-PESO
           when 78-ID-EF-PESO
                inquire EF-PESO, value in EF-PESO-buf

           |78-ID-ef-peso-utf è l'ID del control ef-peso-utf
           when 78-ID-ef-peso-utf
                inquire ef-peso-utf, value in ef-peso-utf-buf

           |78-ID-ef-peso-non-utf è l'ID del control ef-peso-non-utf
           when 78-ID-ef-peso-non-utf
                inquire ef-peso-non-utf, value in ef-peso-non-utf-buf

           |78-ID-EF-COSTO-ULTIMO è l'ID del control EF-COSTO-ULTIMO
           when 78-ID-EF-COSTO-ULTIMO
                inquire EF-COSTO-ULTIMO, value in EF-COSTO-ULTIMO-buf

           |78-ID-EF-COSTO-INIZIO è l'ID del control EF-COSTO-INIZIO
           when 78-ID-EF-COSTO-INIZIO
                inquire EF-COSTO-INIZIO, value in EF-COSTO-INIZIO-buf

           |42 è l'ID del control EF-COSTO-MEDIO
           when 42
                inquire EF-COSTO-MEDIO, value in EF-COSTO-MEDIO-buf

           end-evaluate.

      * Paragrafo per la struttura dei controlli sulla screen Screen1
           evaluate control-id
           end-evaluate.

