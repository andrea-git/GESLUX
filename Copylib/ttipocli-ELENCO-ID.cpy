      * Paragrafo per la struttura dei controlli sulla screen Form1
           evaluate control-id
           |78-ID-ef-abi è l'ID del control ef-abi
           when 78-ID-ef-abi
                inquire ef-abi, value in ef-abi-buf

           |78-ID-ef-cab è l'ID del control ef-cab
           when 78-ID-ef-cab
                inquire ef-cab, value in ef-cab-buf

           end-evaluate.

      * Paragrafo per la struttura dei controlli sulla screen SCREEN-SEARCH
           evaluate control-id
           end-evaluate.

