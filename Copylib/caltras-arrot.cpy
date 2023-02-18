

      ***---
       CALCOLA-QTA-ARROTONDATA-TARIFFA.
           divide tot-peso-kg by 100 giving tot-peso-qli.
           divide tot-peso-kg by 100 giving tot-peso-qli-arrot rounded.
           move trs-vettore to vet-codice.
           read tvettori no lock
                invalid move 0 to trs-qta-arrot
                        move 0 to trs-tariffa
            not invalid
                if tot-peso-qli < vet-min-tass
                   move vet-min-tass to trs-qta-arrot
                else
                   move 1 to idx
                   set esiste-scaglione  to false
                   set trovato-scaglione to false
                   perform 10 times
                      if vet-qli-a-arrot(idx)  not = 0 or
                         vet-qli-da-arrot(idx) not = 0
                         set esiste-scaglione to true
                      end-if
                      if tot-peso-qli-arrot <= vet-qli-a-arrot(idx) and
                         tot-peso-qli-arrot >= vet-qli-da-arrot(idx)
                         set trovato-scaglione to true
                         exit perform
                      end-if
                      add 1 to idx 
                   end-perform
                   if trovato-scaglione
                      move tot-peso-qli to s-tot-peso-qli
                      perform ARROTONDA                     
                      move s-tot-peso-qli to trs-qta-arrot
                   else
                      if not esiste-scaglione
                         move tot-peso-qli to trs-qta-arrot
                      end-if
                   end-if
                end-if
           end-read.

      ***---
       CALCOLA-QTA-ARROTONDATA-TARIFFA-SHI.
           divide tot-peso-kg-SHI by 100 giving tot-peso-qli-SHI.  
           divide tot-peso-kg-SHI by 100 giving tot-peso-qli-arrot 
                                                rounded.
           move trs-vettore to vet-codice.
           read tvettori no lock
                invalid move 0 to trs-qta-arrot-SHI
                        move 0 to trs-tariffa-SHI
            not invalid
                if tot-peso-qli-SHI < vet-min-tass and
                   tot-peso-qli-SHI not = 0
                   move vet-min-tass to trs-qta-arrot-SHI
                else
                   move 1 to idx
                   set esiste-scaglione  to false
                   set trovato-scaglione to false
                   perform 10 times
                      if vet-qli-a-arrot(idx)  not = 0 or
                         vet-qli-da-arrot(idx) not = 0
                         set esiste-scaglione to true
                      end-if
                      if tot-peso-qli-arrot <= vet-qli-a-arrot(idx)  and
                         tot-peso-qli-arrot >= vet-qli-da-arrot(idx)
                         set trovato-scaglione to true
                         exit perform
                      end-if
                      add 1 to idx 
                   end-perform
                   if trovato-scaglione                        
                      move tot-peso-qli-SHI to s-tot-peso-qli
                      perform ARROTONDA                      
                      move s-tot-peso-qli to trs-qta-arrot-SHI
                   else
                      if not esiste-scaglione
                         move tot-peso-qli-SHI to trs-qta-arrot-SHI
                      end-if
                   end-if
                end-if
           end-read.

      ***---
       CALCOLA-QTA-ARROTONDATA-TARIFFA-GET.
           divide tot-peso-kg-GET by 100 giving tot-peso-qli-GET.  
           divide tot-peso-kg-GET by 100 giving tot-peso-qli-arrot 
                                                rounded.
           move trs-vettore to vet-codice.
           read tvettori no lock
                invalid move 0 to trs-qta-arrot-GET
                        move 0 to trs-tariffa-GET
            not invalid
                if tot-peso-qli-GET < vet-min-tass and
                   tot-peso-qli-GET not = 0
                   move vet-min-tass to trs-qta-arrot-GET
                else
                   move 1 to idx
                   set esiste-scaglione  to false
                   set trovato-scaglione to false
                   perform 10 times
                      if vet-qli-a-arrot(idx)  not = 0 or
                         vet-qli-da-arrot(idx) not = 0
                         set esiste-scaglione to true
                      end-if
                      if tot-peso-qli-arrot <= vet-qli-a-arrot(idx)  and
                         tot-peso-qli-arrot >= vet-qli-da-arrot(idx)
                         set trovato-scaglione to true
                         exit perform
                      end-if
                      add 1 to idx 
                   end-perform
                   if trovato-scaglione             
                      move tot-peso-qli-GET to s-tot-peso-qli
                      perform ARROTONDA                      
                      move s-tot-peso-qli to trs-qta-arrot-GET
                   else
                      if not esiste-scaglione
                         move tot-peso-qli-GET to trs-qta-arrot-GET
                      end-if
                   end-if
                end-if
           end-read.

      ***---                                                     
       ARROTONDA.               stop "K"                 
           |Se ho 57,000 ho comunque
           |un valore già arrotondato
           if cifra(10) = 0 and
              cifra(11) = 0 and 
              cifra(12) = 0 and 
              cifra(13) = 0 and 
              cifra(14) = 0 
              exit paragraph
           end-if.

           move vet-valore-arrot(idx) to como-arrot.
           evaluate como-arrot
           when 0   exit paragraph
           when 0,1 
                if cifra(11) = 0 and 
                   cifra(12) = 0 and 
                   cifra(13) = 0 and 
                   cifra(14) = 0
                   continue
                else
                   move 0 to cifra(11) cifra(12) cifra(13) cifra(14)
                   if cifra(10) = 9
                      move 0 to cifra(10)
                      add  1 to cifra(9)
                   else
                      add  1 to cifra(10)
                   end-if                  
                end-if

           when 0,2                        
                if cifra(11) = 0 and 
                   cifra(12) = 0 and 
                   cifra(13) = 0 and 
                   cifra(14) = 0
                   evaluate cifra(10)              
                   when 1 move 2 to cifra(10)
                   when 3 move 4 to cifra(10)
                   when 5 move 6 to cifra(10)
                   when 7 move 8 to cifra(10)
                   when 9 move 0 to cifra(10)
                          add  1 to cifra(9)
                   end-evaluate            
                else                                                
                   move 0 to cifra(11) cifra(12) cifra(13) cifra(14)
                   evaluate cifra(10)              
                   when 0 
                   when 1 move 2 to cifra(10)
                   when 2
                   when 3 move 4 to cifra(10)
                   when 4
                   when 5 move 6 to cifra(10)
                   when 6
                   when 7 move 8 to cifra(10)
                   when 8
                   when 9 move 0 to cifra(10)
                          add  1 to cifra(9)
                   end-evaluate               
                end-if
           when 0,5                               
                if cifra(11) = 0 and 
                   cifra(12) = 0 and 
                   cifra(13) = 0 and 
                   cifra(14) = 0
                   evaluate cifra(10)  
                   when 0 
                   when 1 
                   when 2  
                   when 3 
                   when 4 move 5 to cifra(10)
                   when 5 continue
                   when other move 0 to cifra(10)
                              add  1 to cifra(9)
                   end-evaluate             
                else                                  
                   move 0 to cifra(11) cifra(12) cifra(13) cifra(14)
                   evaluate cifra(10)  
                   when 0 
                   when 1 
                   when 2  
                   when 3 
                   when 4 move 5 to cifra(10)
                   when other move 0 to cifra(10)
                              add  1 to cifra(9)
                   end-evaluate               
                end-if
           when 1            
                move 0 to cifra(10) 
                          cifra(11) 
                          cifra(12) 
                          cifra(13) 
                          cifra(13)  
                add  1 to cifra(9)
           end-evaluate           
  
      *****     move 245,85 to tot-peso-kg.
      *****     perform ARRTOP.
      *****     move 255 to tot-peso-kg.
      *****     perform ARRTOP.
      *****     move 301 to tot-peso-kg.
      *****     perform ARRTOP.
      *****     move 300 to tot-peso-kg.
      *****     perform ARRTOP.
      *****     move 295 to tot-peso-kg.
      *****     perform ARRTOP.                       
      *****     move 260 to tot-peso-kg.
      *****     perform ARRTOP.   
      *****     move 265 to tot-peso-kg.
      *****     perform ARRTOP.     
      *****     move 250 to tot-peso-kg.
      *****     perform ARRTOP.
      *****     move 280 to tot-peso-kg.
      *****     perform ARRTOP.
      *****     move 290 to tot-peso-kg.
      *****     perform ARRTOP.
      *****     move 191,56 to tot-peso-kg.
      *****     perform ARRTOP.
      *****     goback.
      *****
      ********---
      ***** ARRTOP.
      *****     divide tot-peso-kg by 100 giving tot-peso-qli.
      *****     |Se ho 57,000 ho comunque
      *****     |un valore già arrotondato
      *****     if cifra(10) = 0 and
      *****        cifra(11) = 0 and 
      *****        cifra(12) = 0 
      *****        move tot-peso-qli to p10 p20 p50 p100
      *****
      *****     else               
      *****        move tot-peso-qli to s-tot-peso-qli
      *****
      *****        if cifra(11) = 0 and cifra(12) = 0   
      *****           move tot-peso-qli to p10
      *****        else
      *****           move 0 to cifra(11) cifra(12)
      *****           if cifra(10) = 9
      *****              move 0 to cifra(10)
      *****              add  1 to cifra(9)
      *****           else
      *****              add  1 to cifra(10)
      *****           end-if
      *****           move tot-peso-qli to p10
      *****        end-if
      *****                                              
      *****        move s-tot-peso-qli to tot-peso-qli
      *****        if cifra(11) = 0 and cifra(12) = 0
      *****           evaluate cifra(10)              
      *****           when 1 move 2 to cifra(10)
      *****           when 3 move 4 to cifra(10)
      *****           when 5 move 6 to cifra(10)
      *****           when 7 move 8 to cifra(10)
      *****           when 9 move 0 to cifra(10)
      *****                  add  1 to cifra(9)
      *****           end-evaluate            
      *****           move tot-peso-qli to p20
      *****        else                                  
      *****           move 0 to cifra(11) cifra(12)
      *****           evaluate cifra(10)              
      *****           when 0 
      *****           when 1 move 2 to cifra(10)
      *****           when 2
      *****           when 3 move 4 to cifra(10)
      *****           when 4
      *****           when 5 move 6 to cifra(10)
      *****           when 6
      *****           when 7 move 8 to cifra(10)
      *****           when 8
      *****           when 9 move 0 to cifra(10)
      *****                  add  1 to cifra(9)
      *****           end-evaluate      
      *****           move tot-peso-qli to p20   
      *****        end-if
      *****         
      *****        move s-tot-peso-qli to tot-peso-qli
      *****                                              
      *****        if cifra(11) = 0 and cifra(12) = 0
      *****           evaluate cifra(10)  
      *****           when 0 
      *****           when 1 
      *****           when 2  
      *****           when 3 
      *****           when 4 move 5 to cifra(10)
      *****           when 5 continue
      *****           when other move 0 to cifra(10)
      *****                      add  1 to cifra(9)
      *****           end-evaluate            
      *****           move tot-peso-qli to p50
      *****        else                                  
      *****           move 0 to cifra(11) cifra(12)  
      *****           evaluate cifra(10)  
      *****           when 0 
      *****           when 1 
      *****           when 2  
      *****           when 3 
      *****           when 4 move 5 to cifra(10)
      *****           when other move 0 to cifra(10)
      *****                      add  1 to cifra(9)
      *****           end-evaluate            
      *****           move tot-peso-qli to p50   
      *****        end-if
      *****
      *****                                        
      *****        move s-tot-peso-qli to tot-peso-qli
      *****        move 0 to cifra(10) cifra(11) cifra(12)  
      *****        add  1 to cifra(9)
      *****        move tot-peso-qli to p100   
      *****     end-if.                          
      *****
      *****           display message "PESO: " s-tot-peso-qli
      *****           x"0d0a""100 - "p100
      *****           x"0d0a""50 - "p50
      *****           x"0d0a""20 - "p20
      *****                      x"0d0a""10 - " p10
