      * Paragrafo per la struttura del codice in BEFORE sulla screen Form1
      ***---
       Form1-BEFORE-SCREEN.
           evaluate control-id
           end-evaluate.

      * Paragrafo per la struttura del codice in AFTER sulla screen Form1
      ***---
       Form1-AFTER-SCREEN.
           evaluate control-id
           end-evaluate.

      * Generazione stringa perform CONTROLLO
           evaluate control-id
           |78-ID-ef-codice è l'ID del campo ef-codice
           when 78-ID-ef-codice
                perform CONTROLLO
           |78-ID-ef-imp-consumo è l'ID del campo ef-imp-consumo
           when 78-ID-ef-imp-consumo
                perform CONTROLLO
           end-evaluate.                      

