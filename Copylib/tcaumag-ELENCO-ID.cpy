      * Paragrafo per la struttura dei controlli sulla screen Form1
           evaluate control-id
           |78-ID-ef-codice è l'ID del control ef-codice
           when 78-ID-ef-codice
                inquire ef-codice, value in ef-codice-buf

           |78-ID-ef-descr è l'ID del control ef-descr
           when 78-ID-ef-descr
                inquire ef-descr, value in ef-descr-buf

           |78-ID-ef-tras è l'ID del control ef-tras
           when 78-ID-ef-tras
                inquire ef-tras, value in ef-tras-buf

           |78-ID-ef-contropartita è l'ID del control ef-contropartita
           when 78-ID-ef-contropartita
                inquire ef-contropartita, value in ef-contropartita-buf

           |78-ID-ef-codmag è l'ID del control ef-codmag
           when 78-ID-ef-codmag
                inquire ef-codmag, value in ef-codmag-buf

           |78-ID-ef-codpag è l'ID del control ef-codpag
           when 78-ID-ef-codpag
                inquire ef-codpag, value in ef-codpag-buf

           |78-ID-ef-cod-edi è l'ID del control ef-cod-edi
           when 78-ID-ef-cod-edi
                inquire ef-cod-edi, value in ef-cod-edi-buf

           end-evaluate.

