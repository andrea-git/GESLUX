      ***---
       RIEMPI-COMBO-STATO-ORDINE.           
           Modify cbo-stato-ordine, item-to-add 78-tutti.
           Modify cbo-stato-ordine, item-to-add 78-registrato.
           Modify cbo-stato-ordine, item-to-add 78-in-lavorazione.
           Modify cbo-stato-ordine, item-to-add 78-sped-parz.
           Modify cbo-stato-ordine, item-to-add 78-sped-tot.
           Modify cbo-stato-ordine, item-to-add 78-chiuso.
           Modify cbo-stato-ordine, item-to-add 78-bloccato.

      ***---
       CARICA-COMBO-STATO-ORDINE.
           evaluate true
           when registrato     
                move 78-registrato     to cbo-stato-ordine-buf
           when in-lavorazione 
                move 78-in-lavorazione to cbo-stato-ordine-buf
           when sped-parz      
                move 78-sped-parz      to cbo-stato-ordine-buf
           when sped-tot       
                move 78-sped-tot       to cbo-stato-ordine-buf
           when chiuso
                move 78-chiuso         to cbo-stato-ordine-buf
           when tutti          
                move 78-tutti          to cbo-stato-ordine-buf
           when bloccato 
                move 78-bloccato       to cbo-stato-ordine-buf
           end-evaluate.       
           modify cbo-stato-ordine, value cbo-stato-ordine-buf.
           
      ***---
       SCARICA-COMBO-STATO-ORDINE.     
           inquire cbo-stato-ordine, value cbo-stato-ordine-buf.
           evaluate cbo-stato-ordine-buf
           when 78-registrato      set registrato     to true
           when 78-in-lavorazione  set in-lavorazione to true
           when 78-sped-parz       set sped-parz      to true
           when 78-sped-tot        set sped-tot       to true
           when 78-chiuso          set chiuso         to true
           when 78-tutti           set tutti          to true
           when 78-bloccato        set bloccato       to true
           end-evaluate.
