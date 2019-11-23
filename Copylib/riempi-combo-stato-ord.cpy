      ***---
       RIEMPI-COMBO-STATO-ORD.
           Modify cbo-stato-ord, item-to-add 78-inevasi.
           Modify cbo-stato-ord, item-to-add 78-emessa-bolla.
           Modify cbo-stato-ord, item-to-add 78-fatturati.
           Modify cbo-stato-ord, item-to-add 78-corrispettivi.    
           Modify cbo-stato-ord, item-to-add 78-tutti.

      ***---
       CARICA-COMBO-STATO-ORD.
           evaluate true
           when inevasi       move 78-inevasi       to cbo-stato-ord-buf
           when emessa-bolla  move 78-emessa-bolla  to cbo-stato-ord-buf
           when fatturati     move 78-fatturati     to cbo-stato-ord-buf
           when corrispettivi move 78-corrispettivi to cbo-stato-ord-buf
           when tutti         move 78-tutti         to cbo-stato-ord-buf
           end-evaluate.
           modify cbo-stato-ord,      value cbo-stato-ord-buf.
           
      ***---
       SCARICA-COMBO-STATO-ORD.     
           inquire cbo-stato-ord,      value cbo-stato-ord-buf.
           evaluate cbo-stato-ord-buf
           when 78-inevasi         set inevasi        to true
           when 78-emessa-bolla    set emessa-bolla   to true
           when 78-fatturati       set fatturati      to true
           when 78-corrispettivi   set corrispettivi  to true
           when 78-tutti           set tutti          to true
           end-evaluate.
