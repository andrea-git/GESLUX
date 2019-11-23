      * Paragrafo per la struttura dei controlli sulla screen Form1
           evaluate control-id
           |6001 è l'ID del control ef-mag
           when 6001
                inquire ef-mag, value in ef-mag-buf

           |6002 è l'ID del control ef-eva
           when 6002
                inquire ef-eva, value in ef-eva-buf

           end-evaluate.

      * Paragrafo per la struttura dei controlli sulla screen Screen2
           evaluate control-id
           end-evaluate.

      * Paragrafo per la struttura dei controlli sulla screen scr-elab
           evaluate control-id
           end-evaluate.

      * Paragrafo per la struttura dei controlli sulla screen scr-elab-evasioni
           evaluate control-id
           end-evaluate.

      * Paragrafo per la struttura dei controlli sulla screen scr-fine
           evaluate control-id
           end-evaluate.

      * Paragrafo per la struttura dei controlli sulla screen scr-elab-tprev
           evaluate control-id
           end-evaluate.

