      ***---
       RIEMPI-COMBO-STATO-ORD.
           Modify cbo-stato-ord, item-to-add 78-inseriti.
      ****     Modify cbo-stato-ord, item-to-add 78-accettati.     
           Modify cbo-stato-ord, item-to-add 78-inviati.
           Modify cbo-stato-ord, item-to-add 78-in-lavorazioni.
           Modify cbo-stato-ord, item-to-add 78-chiusi.
           Modify cbo-stato-ord, item-to-add 78-tutti.

      ***---
       CARICA-COMBO-STATO-ORD.
           evaluate true

           when inseriti
                move 78-inseriti         to cbo-stato-ord-buf
      *****     when accettati
      *****          move 78-accettati        to cbo-stato-ord-buf
           when inviati
                move 78-inviati          to cbo-stato-ord-buf
           when in-lavorazioni
                move 78-in-lavorazioni   to cbo-stato-ord-buf
           when chiusi
                move 78-chiusi           to cbo-stato-ord-buf
           when tutti         
                move 78-tutti            to cbo-stato-ord-buf
           end-evaluate.
           modify cbo-stato-ord,      value cbo-stato-ord-buf.
           
      ***---
       SCARICA-COMBO-STATO-ORD.     
           inquire cbo-stato-ord,      value cbo-stato-ord-buf.
           evaluate cbo-stato-ord-buf
           when 78-inseriti
                set inseriti       to true
      *****     when 78-accettati
      *****          set accettati      to true
           when 78-inviati
                set inviati        to true
           when 78-in-lavorazioni
                set in-lavorazioni to true
           when 78-chiusi
                set chiusi         to true
           when 78-tutti           
                set tutti          to true
           end-evaluate.
