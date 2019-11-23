      * Paragrafo per la struttura dei controlli sulla screen Form1
           evaluate control-id
           |78-ID-ef-art è l'ID del control ef-art
           when 78-ID-ef-art
                inquire ef-art, value in ef-art-buf

           |78-ID-ef-des è l'ID del control ef-des
           when 78-ID-ef-des
                inquire ef-des, value in ef-des-buf

           |78-ID-ef-ini-old è l'ID del control ef-ini-old
           when 78-ID-ef-ini-old
                inquire ef-ini-old, value in ef-ini-old-buf

           |78-ID-ef-fine-old è l'ID del control ef-fine-old
           when 78-ID-ef-fine-old
                inquire ef-fine-old, value in ef-fine-old-buf

           |78-ID-ef-prezzo-old è l'ID del control ef-prezzo-old
           when 78-ID-ef-prezzo-old
                inquire ef-prezzo-old, value in ef-prezzo-old-buf

           |78-ID-ef-ini-new è l'ID del control ef-ini-new
           when 78-ID-ef-ini-new
                inquire ef-ini-new, value in ef-ini-new-buf

           |78-ID-ef-fine-new è l'ID del control ef-fine-new
           when 78-ID-ef-fine-new
                inquire ef-fine-new, value in ef-fine-new-buf

           |78-ID-ef-prezzo-new è l'ID del control ef-prezzo-new
           when 78-ID-ef-prezzo-new
                inquire ef-prezzo-new, value in ef-prezzo-new-buf

           end-evaluate.

