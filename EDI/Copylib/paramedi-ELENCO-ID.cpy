      * Paragrafo per la struttura dei controlli sulla screen Form1
           evaluate control-id
           |78-ID-ef-cod-uff è l'ID del control ef-cod-uff
           when 78-ID-ef-cod-uff
                inquire ef-cod-uff, value in ef-cod-uff-buf

           |78-ID-ef-tipo-reg è l'ID del control ef-tipo-reg
           when 78-ID-ef-tipo-reg
                inquire ef-tipo-reg, value in ef-tipo-reg-buf

           |78-ID-ef-cod-reg è l'ID del control ef-cod-reg
           when 78-ID-ef-cod-reg
                inquire ef-cod-reg, value in ef-cod-reg-buf

           |78-ID-ef-tipo-doc è l'ID del control ef-tipo-doc
           when 78-ID-ef-tipo-doc
                inquire ef-tipo-doc, value in ef-tipo-doc-buf

           |78-ID-ef-tipo-doc-int è l'ID del control ef-tipo-doc-int
           when 78-ID-ef-tipo-doc-int
                inquire ef-tipo-doc-int, value in ef-tipo-doc-int-buf

           |78-ID-ef-sfuso è l'ID del control ef-sfuso
           when 78-ID-ef-sfuso
                inquire ef-sfuso, value in ef-sfuso-buf

           |78-ID-ef-conf è l'ID del control ef-conf
           when 78-ID-ef-conf
                inquire ef-conf, value in ef-conf-buf

           |78-ID-ef-assolta è l'ID del control ef-assolta
           when 78-ID-ef-assolta
                inquire ef-assolta, value in ef-assolta-buf

           |78-ID-ef-non-sog è l'ID del control ef-non-sog
           when 78-ID-ef-non-sog
                inquire ef-non-sog, value in ef-non-sog-buf

           |78-ID-ef-path è l'ID del control ef-path
           when 78-ID-ef-path
                inquire ef-path, value in ef-path-buf

           end-evaluate.

