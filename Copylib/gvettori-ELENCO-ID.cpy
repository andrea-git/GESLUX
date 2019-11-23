      * Paragrafo per la struttura dei controlli sulla screen Form1
           evaluate control-id
           |78-ID-ef-codice è l'ID del control ef-codice
           when 78-ID-ef-codice
                inquire ef-codice, value in ef-codice-buf

           |78-ID-ef-des è l'ID del control ef-des
           when 78-ID-ef-des
                inquire ef-des, value in ef-des-buf

           |78-ID-ef-indirizzo è l'ID del control ef-indirizzo
           when 78-ID-ef-indirizzo
                inquire ef-indirizzo, value in ef-indirizzo-buf

           |78-ID-ef-sigla è l'ID del control ef-sigla
           when 78-ID-ef-sigla
                inquire ef-sigla, value in ef-sigla-buf

           |78-ID-ef-piva è l'ID del control ef-piva
           when 78-ID-ef-piva
                inquire ef-piva, value in ef-piva-buf

           end-evaluate.

      * Paragrafo per la struttura dei controlli sulla screen scr-stampa
           evaluate control-id
           end-evaluate.

