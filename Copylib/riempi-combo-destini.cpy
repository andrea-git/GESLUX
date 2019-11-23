      ***---
       RIEMPI-COMBO-DESTINI.
           Modify cbo-stato-d,    item-to-add "Disattivo".
           Modify cbo-stato-d,    item-to-add "Attivo".
           Modify cbo-stato-d,    item-to-add "Bloccato".
           
      ***---
       CARICA-COMBO-DESTINI.
           evaluate true
           when disattivo-d  
                move "Disattivo"  to cbo-stato-d-buf
           when attivo-d
                move "Attivo"     to cbo-stato-d-buf
           when bloccato-d
                move "Bloccato"   to cbo-stato-d-buf
           end-evaluate.
           modify cbo-stato-d,    value cbo-stato-d-buf.
           
      ***---
       SCARICA-COMBO-DESTINI.     
           inquire cbo-stato-d,      value cbo-stato-d-buf.
           evaluate cbo-stato-d-buf
           when "Disattivo"        set disattivo-d       to true
           when "Attivo"           set attivo-d          to true              
           when "Bloccato"         set bloccato-d        to true             
           end-evaluate.  
