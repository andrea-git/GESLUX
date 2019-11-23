      ***---
       RIEMPI-COMBO-STATO-CONT.
           modify cbo-stato,  item-to-add "Aperta".
           modify cbo-stato,  item-to-add "Chiusa".
           
      ***---
       CARICA-COMBO-STATO-CONT.
           evaluate true
           when aperta  move "Aperta" to cbo-stato-buf
           when chiusa  move "Chiusa" to cbo-stato-buf
           end-evaluate.
           modify cbo-stato,  value cbo-stato-buf.
           
      ***---
       SCARICA-COMBO-STATO-CONT.
           inquire cbo-stato, value cbo-stato-buf.
           evaluate cbo-stato-buf
           when "Aperta" set aperta to true
           when "Chiusa" set chiusa to true
           end-evaluate.
