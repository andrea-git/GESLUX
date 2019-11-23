      * Paragrafo per la struttura dei controlli sulla screen Form1
           evaluate control-id
           |78-ID-ef-codice è l'ID del control ef-codice
           when 78-ID-ef-codice
                inquire ef-codice, value in ef-codice-buf

           |78-ID-ef-des è l'ID del control ef-des
           when 78-ID-ef-des
                inquire ef-des, value in ef-des-buf

           |78-ID-ef-mag è l'ID del control ef-mag
           when 78-ID-ef-mag
                inquire ef-mag, value in ef-mag-buf

           |78-ID-cbo-stato è l'ID del control cbo-stato
           when 78-ID-cbo-stato
                inquire cbo-stato, value in cbo-stato-buf

           end-evaluate.

