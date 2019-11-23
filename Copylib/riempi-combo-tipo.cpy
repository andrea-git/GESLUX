      ***---
       RIEMPI-COMBO-TIPO.
           modify cbo-tipo,  item-to-add "Prezzo".
           modify cbo-tipo,  item-to-add "Merce".
           modify cbo-tipo,  item-to-add "Reso".
           modify cbo-tipo,  item-to-add "Addebito C.".
           modify cbo-tipo,  item-to-add "Altro".
           
      ***---
       CARICA-COMBO-TIPO.
           evaluate true
           when prezzo   move "Prezzo"        to cbo-tipo-buf
           when merce    move "Merce"         to cbo-tipo-buf
           when reso     move "Reso"          to cbo-tipo-buf
           when corriere move "Addebito C."   to cbo-tipo-buf
           when altro    move "Altro"         to cbo-tipo-buf
           end-evaluate.
           modify cbo-tipo,  value cbo-tipo-buf.
           
      ***---
       SCARICA-COMBO-TIPO.
           inquire cbo-tipo, value cbo-tipo-buf.
           evaluate cbo-tipo-buf
           when "Prezzo"        set prezzo   to true
           when "Merce"         set merce    to true
           when "Reso"          set reso     to true
           when "Addebito C."   set corriere to true
           when "Altro"         set altro    to true
           end-evaluate.
