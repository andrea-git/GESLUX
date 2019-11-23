      * Paragrafo per la struttura dei controlli sulla screen Form1
           evaluate control-id
           |78-ID-ef-user è l'ID del control ef-user
           when 78-ID-ef-user
                inquire ef-user, value in ef-user-buf

           |78-ID-ef-pass è l'ID del control ef-pass
           when 78-ID-ef-pass
                inquire ef-pass, value in ef-pass-buf

           end-evaluate.

