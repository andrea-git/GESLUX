      * Paragrafo per la struttura dei controlli sulla screen Form1
           evaluate control-id
           |78-ID-ef-codice � l'ID del control ef-codice
           when 78-ID-ef-codice
                inquire ef-codice, value in ef-codice-buf

           |78-ID-ef-dt-val-da � l'ID del control ef-dt-val-da
           when 78-ID-ef-dt-val-da
                inquire ef-dt-val-da, value in ef-dt-val-da-buf

           |78-ID-ef-dt-val-a � l'ID del control ef-dt-val-a
           when 78-ID-ef-dt-val-a
                inquire ef-dt-val-a, value in ef-dt-val-a-buf

           end-evaluate.

