      * Paragrafo per la struttura dei controlli sulla screen Form1
           evaluate control-id
           |78-ID-ef-codice è l'ID del control ef-codice
           when 78-ID-ef-codice
                inquire ef-codice, value in ef-codice-buf

           |78-ID-ef-descrizione è l'ID del control ef-descrizione
           when 78-ID-ef-descrizione
                inquire ef-descrizione, value in ef-descrizione-buf

           |78-ID-ef-cpa è l'ID del control ef-cpa
           when 78-ID-ef-cpa
                inquire ef-cpa, value in ef-cpa-buf

           |78-ID-ef-nc è l'ID del control ef-nc
           when 78-ID-ef-nc
                inquire ef-nc, value in ef-nc-buf

           |78-ID-ef-taric è l'ID del control ef-taric
           when 78-ID-ef-taric
                inquire ef-taric, value in ef-taric-buf

           |78-ID-ef-dac è l'ID del control ef-dac
           when 78-ID-ef-dac
                inquire ef-dac, value in ef-dac-buf

           |78-ID-ef-suf-reg è l'ID del control ef-suf-reg
           when 78-ID-ef-suf-reg
                inquire ef-suf-reg, value in ef-suf-reg-buf

           end-evaluate.

