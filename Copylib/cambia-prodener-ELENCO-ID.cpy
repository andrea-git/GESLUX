      * Paragrafo per la struttura dei controlli sulla screen form1
           evaluate control-id
           |78-ID-ef-num-from è l'ID del control ef-num-from
           when 78-ID-ef-num-from
                inquire ef-num-from, value in ef-num-from-buf

           |78-ID-ef-num-to è l'ID del control ef-num-to
           when 78-ID-ef-num-to
                inquire ef-num-to, value in ef-num-to-buf

           |78-ID-ef-prodener-old è l'ID del control ef-prodener-old
           when 78-ID-ef-prodener-old
                inquire ef-prodener-old, value in ef-prodener-old-buf

           |78-ID-ef-prodener-new è l'ID del control ef-prodener-new
           when 78-ID-ef-prodener-new
                inquire ef-prodener-new, value in ef-prodener-new-buf

           end-evaluate.

