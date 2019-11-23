      * Paragrafo per la struttura dei controlli sulla screen Form1
           evaluate control-id
           |78-ID-ef-codice è l'ID del control ef-codice
           when 78-ID-ef-codice
                inquire ef-codice, value in ef-codice-buf

           |78-ID-ef-nome è l'ID del control ef-nome
           when 78-ID-ef-nome
                inquire ef-nome, value in ef-nome-buf

           |78-ID-ef-pwd è l'ID del control ef-pwd
           when 78-ID-ef-pwd
                inquire ef-pwd, value in ef-pwd-buf

           end-evaluate.

      * Paragrafo per la struttura dei controlli sulla screen Form-abil
           evaluate control-id
           end-evaluate.

