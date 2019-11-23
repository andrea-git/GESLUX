      ***---
       RIEMPI-COMBO-UFFICIO-D.
           Modify cbo-ufficio-d,    item-to-add "Non Gestito".
           Modify cbo-ufficio-d,    item-to-add "Luca".
           Modify cbo-ufficio-d,    item-to-add "Massimo".
           
      ***---
       CARICA-COMBO-UFFICIO-D.
           evaluate true
           when non-gestito-d  
                move "Non Gestito"  to cbo-ufficio-d-buf
           when luca-d
                move "Luca"         to cbo-ufficio-d-buf
           when massimo-d
                move "Massimo"      to cbo-ufficio-d-buf
           end-evaluate.
           modify cbo-ufficio-d,    value cbo-ufficio-d-buf.
           
      ***---
       SCARICA-COMBO-UFFICIO-D.
           inquire cbo-ufficio-d,      value cbo-ufficio-d-buf.
           evaluate cbo-ufficio-d-buf
           when "Non Gestito"     set non-gestito-d  to true
           when "Luca"            set luca-d         to true              
           when "Massimo"         set massimo-d      to true             
           end-evaluate.  
