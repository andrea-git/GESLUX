      ***---
       RIEMPI-COMBO-COBAT.
           Modify cbo-cobat,        item-to-add "Auto".
           Modify cbo-cobat,        item-to-add "Moto".
           
      ***---
       CARICA-COMBO-COBAT.
           evaluate true
           when auto-w
                move "Auto"  to cbo-cobat-buf
           when moto
                move "Moto"  to cbo-cobat-buf
           end-evaluate.
           modify cbo-cobat,      value cbo-cobat-buf.
           
      ***---
       SCARICA-COMBO-COBAT.     
           inquire cbo-cobat,      value cbo-cobat-buf.
           evaluate cbo-cobat-buf               
           when "Auto"        set auto-w       to true
           when "Moto"        set moto         to true
           end-evaluate.  
