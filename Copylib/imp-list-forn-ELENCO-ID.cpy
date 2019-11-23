      * Paragrafo per la struttura dei controlli sulla screen form-nuovo
           evaluate control-id
           |78-ID-ef-forn-nuovo è l'ID del control ef-forn-nuovo
           when 78-ID-ef-forn-nuovo
                inquire ef-forn-nuovo, value in ef-forn-nuovo-buf

           |78-ID-ef-dest-nuovo è l'ID del control ef-dest-nuovo
           when 78-ID-ef-dest-nuovo
                inquire ef-dest-nuovo, value in ef-dest-nuovo-buf

           |78-ID-EF-dt-val-da è l'ID del control EF-dt-val-da
           when 78-ID-EF-dt-val-da
                inquire EF-dt-val-da, value in EF-dt-val-da-buf

           |78-ID-EF-dt-val-a è l'ID del control EF-dt-val-a
           when 78-ID-EF-dt-val-a
                inquire EF-dt-val-a, value in EF-dt-val-a-buf

           end-evaluate.

