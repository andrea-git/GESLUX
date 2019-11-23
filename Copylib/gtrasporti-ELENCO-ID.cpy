      * Paragrafo per la struttura dei controlli sulla screen Form1
           evaluate control-id
           |78-ID-ef-prog-bolla è l'ID del control ef-prog-bolla
           when 78-ID-ef-prog-bolla
                inquire ef-prog-bolla, value in ef-prog-bolla-buf

           |78-ID-ef-data è l'ID del control ef-data
           when 78-ID-ef-data
                inquire ef-data, value in ef-data-buf

           |78-ID-ef-data-fatt è l'ID del control ef-data-fatt
           when 78-ID-ef-data-fatt
                inquire ef-data-fatt, value in ef-data-fatt-buf

           |78-ID-ef-vet è l'ID del control ef-vet
           when 78-ID-ef-vet
                inquire ef-vet, value in ef-vet-buf

           |78-ID-ef-cli è l'ID del control ef-cli
           when 78-ID-ef-cli
                inquire ef-cli, value in ef-cli-buf

           |78-ID-ef-dest è l'ID del control ef-dest
           when 78-ID-ef-dest
                inquire ef-dest, value in ef-dest-buf

           |78-ID-ef-reg è l'ID del control ef-reg
           when 78-ID-ef-reg
                inquire ef-reg, value in ef-reg-buf

           |78-ID-ef-prov è l'ID del control ef-prov
           when 78-ID-ef-prov
                inquire ef-prov, value in ef-prov-buf

           |78-ID-ef-qta-kg è l'ID del control ef-qta-kg
           when 78-ID-ef-qta-kg
                inquire ef-qta-kg, value in ef-qta-kg-buf

           |78-ID-ef-qta-arrot è l'ID del control ef-qta-arrot
           when 78-ID-ef-qta-arrot
                inquire ef-qta-arrot, value in ef-qta-arrot-buf

           |78-ID-ef-qta-kg-SHI è l'ID del control ef-qta-kg-SHI
           when 78-ID-ef-qta-kg-SHI
                inquire ef-qta-kg-SHI, value in ef-qta-kg-SHI-buf

           |78-ID-ef-qta-arrot-SHI è l'ID del control ef-qta-arrot-SHI
           when 78-ID-ef-qta-arrot-SHI
                inquire ef-qta-arrot-SHI, value in ef-qta-arrot-SHI-buf

           |78-ID-ef-qta-kg-GET è l'ID del control ef-qta-kg-GET
           when 78-ID-ef-qta-kg-GET
                inquire ef-qta-kg-GET, value in ef-qta-kg-GET-buf

           |78-ID-ef-qta-arrot-GET è l'ID del control ef-qta-arrot-GET
           when 78-ID-ef-qta-arrot-GET
                inquire ef-qta-arrot-GET, value in ef-qta-arrot-GET-buf

           end-evaluate.

