      ***---
       RIEMPI-COMBO-STATO-BOZZE.
           Modify cbo-stato-ord, item-to-add 78-aperte.
           Modify cbo-stato-ord, item-to-add 78-chiuse.
           Modify cbo-stato-ord, item-to-add 78-tutte.

      ***---
       CARICA-COMBO-STATO-ORD.
           evaluate true
           when aperte
                move 78-aperte  to cbo-stato-ord-buf
           when chiuse
                move 78-chiuse  to cbo-stato-ord-buf
           when tutte
                move 78-tutte   to cbo-stato-ord-buf
           end-evaluate.
           modify cbo-stato-ord,      value cbo-stato-ord-buf.
           
      ***---
       SCARICA-COMBO-STATO-ORD.     
           inquire cbo-stato-ord,      value cbo-stato-ord-buf.
           evaluate cbo-stato-ord-buf
           when 78-aperte
                set aperte   to true
           when 78-chiuse
                set chiuse   to true
           when 78-tutte
                set tutte    to true
           end-evaluate.
