      * Paragrafo per la struttura dei controlli sulla screen Form1
           evaluate control-id
           |78-ID-ef-codice è l'ID del control ef-codice
           when 78-ID-ef-codice
                inquire ef-codice, value in ef-codice-buf

           |78-ID-ef-gdo è l'ID del control ef-gdo
           when 78-ID-ef-gdo
                inquire ef-gdo, value in ef-gdo-buf

           |78-ID-ef-ini-dpo è l'ID del control ef-ini-dpo
           when 78-ID-ef-ini-dpo
                inquire ef-ini-dpo, value in ef-ini-dpo-buf

           |78-ID-ef-fine-dpo è l'ID del control ef-fine-dpo
           when 78-ID-ef-fine-dpo
                inquire ef-fine-dpo, value in ef-fine-dpo-buf

           |78-ID-ef-nome è l'ID del control ef-nome
           when 78-ID-ef-nome
                inquire ef-nome, value in ef-nome-buf

           |78-ID-ef-ini-vol è l'ID del control ef-ini-vol
           when 78-ID-ef-ini-vol
                inquire ef-ini-vol, value in ef-ini-vol-buf

           |78-ID-ef-fine-vol è l'ID del control ef-fine-vol
           when 78-ID-ef-fine-vol
                inquire ef-fine-vol, value in ef-fine-vol-buf

           end-evaluate.

      * Paragrafo per la struttura dei controlli sulla screen Screen1
           evaluate control-id
           end-evaluate.

      * Paragrafo per la struttura dei controlli sulla screen scr-ricerca
           evaluate control-id
           end-evaluate.

