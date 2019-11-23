      ***---
       RIEMPI-COMBO-UTF.
           Modify cbo-utf,    item-to-add "Soggetto".
           Modify cbo-utf,    item-to-add "Misto".
           
      ***---
       RIEMPI-COMBO-NON-UTF.
           Modify cbo-utf,    item-to-add "Non soggetto".
           
      ***---
       CARICA-COMBO-UTF.
           evaluate true
           when soggetto      move "Soggetto"      to cbo-utf-buf
           when misto         move "Misto"         to cbo-utf-buf
           when non-soggetto  move "Non soggetto"  to cbo-utf-buf
           end-evaluate.
           modify cbo-utf,    value cbo-utf-buf.
           
      ***---
       SCARICA-COMBO-UTF.
           inquire cbo-utf,       value cbo-utf-buf.
           evaluate cbo-utf-buf
           when "Soggetto"        set soggetto       to true
           when "Misto"           set misto          to true              
           when "Non soggetto"    set non-soggetto   to true             
           end-evaluate.  
