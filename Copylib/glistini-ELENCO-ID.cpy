      * Paragrafo per la struttura dei controlli sulla screen Form1
           evaluate control-id
           |78-ID-ef-codice è l'ID del control ef-codice
           when 78-ID-ef-codice
                inquire ef-codice, value in ef-codice-buf

           |78-ID-ef-forn è l'ID del control ef-forn
           when 78-ID-ef-forn
                inquire ef-forn, value in ef-forn-buf

           |78-ID-ef-destino è l'ID del control ef-destino
           when 78-ID-ef-destino
                inquire ef-destino, value in ef-destino-buf

           |78-ID-ef-note è l'ID del control ef-note
           when 78-ID-ef-note
                inquire ef-note, value in ef-note-buf

           |78-ID-ef-ini-val è l'ID del control ef-ini-val
           when 78-ID-ef-ini-val
                inquire ef-ini-val, value in ef-ini-val-buf

           |78-ID-ef-fine-val è l'ID del control ef-fine-val
           when 78-ID-ef-fine-val
                inquire ef-fine-val, value in ef-fine-val-buf

           end-evaluate.

      * Paragrafo per la struttura dei controlli sulla screen form-copia
           evaluate control-id
           |78-ID-EF-cli-copia è l'ID del control EF-cli-copia
           when 78-ID-EF-cli-copia
                inquire EF-cli-copia, value in EF-cli-copia-buf

           end-evaluate.

