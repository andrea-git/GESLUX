      * Paragrafo per la struttura dei controlli sulla screen Form1
           evaluate control-id
           |8 è l'ID del control ef-peso-utf
           when 8
                inquire ef-peso-utf, value in ef-peso-utf-buf

           |42 è l'ID del control cbo-stato
           when 42
                inquire cbo-stato, value in cbo-stato-buf

           end-evaluate.

