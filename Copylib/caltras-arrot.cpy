      ***---
       CALCOLA-QTA-ARROTONDATA-TARIFFA.
           divide tot-peso-kg(idx-serie) by 100 
                  giving tot-peso-qli.
           divide tot-peso-kg(idx-serie) by 100 
                  giving tot-peso-qli-arrot rounded.
           move trs-vettore to vet-codice.      
           read tvettori no lock                
                invalid 
                move 0 to trs-qta-arrot-s1 
                          trs-qta-arrot-s2 
                          trs-qta-arrot-s3
                          trs-tariffa-s1 
                          trs-tariffa-s2 
                          trs-tariffa-s3
                evaluate idx-serie
                when 1 move tot-peso-qli-arrot to trs-qta-arrot-s1
                when 2 move tot-peso-qli-arrot to trs-qta-arrot-s2
                when 3 move tot-peso-qli-arrot to trs-qta-arrot-s3
                end-evaluate                            
            not invalid
                if tot-peso-qli < vet-min-tass
                   evaluate idx-serie                       
                   when 1 move vet-min-tass to trs-qta-arrot-s1
                   when 2 move vet-min-tass to trs-qta-arrot-s2
                   when 3 move vet-min-tass to trs-qta-arrot-s3
                   end-evaluate
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
                      evaluate idx-serie                                  
                      when 1 move s-tot-peso-qli to trs-qta-arrot-s1
                      when 2 move s-tot-peso-qli to trs-qta-arrot-s2
                      when 3 move s-tot-peso-qli to trs-qta-arrot-s3
                      end-evaluate
                   else
                      if not esiste-scaglione
                         evaluate idx-serie                                
                         when 1 move tot-peso-qli to trs-qta-arrot-s1
                         when 2 move tot-peso-qli to trs-qta-arrot-s2
                         when 3 move tot-peso-qli to trs-qta-arrot-s3
                         end-evaluate
                      end-if
                   end-if
                end-if
           end-read.                   

      ***---                                                     
       ARROTONDA.               
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
                          perform ADD-1
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
                          perform ADD-1
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
                              perform ADD-1
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
                              perform ADD-1
                   end-evaluate               
                end-if
           when 1            
                move 0 to cifra(10) 
                          cifra(11) 
                          cifra(12) 
                          cifra(13) 
                          cifra(13)  
                perform ADD-1
           end-evaluate.

      ***---
       ADD-1.                             
           move cifra(1) to como-cifra(1).
           move cifra(2) to como-cifra(2).
           move cifra(3) to como-cifra(3).
           move cifra(4) to como-cifra(4).
           move cifra(5) to como-cifra(5).
           move cifra(6) to como-cifra(6).
           move cifra(7) to como-cifra(7).
           move cifra(8) to como-cifra(8).
           move cifra(9) to como-cifra(9).
           add 1 to como-num.             
           move como-cifra(1) to cifra(1).
           move como-cifra(2) to cifra(2).
           move como-cifra(3) to cifra(3).
           move como-cifra(4) to cifra(4).
           move como-cifra(5) to cifra(5).
           move como-cifra(6) to cifra(6).
           move como-cifra(7) to cifra(7).
           move como-cifra(8) to cifra(8).
           move como-cifra(9) to cifra(9).
  
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

      ***---
       TROVA-TARIFFA-E-VALORIZZA-CAMPO.
           move 0          to trs-tariffa-s1.
           move low-value  to tfv-rec.
           move vet-codice to tfv-codice.

           start tarifvet key is >= tfv-chiave
                 invalid continue
             not invalid
LUBEXX           set trovata-tariffa to false
                 perform until 1 = 2
                    read tarifvet next no lock
                         at end exit perform
                    end-read
                    if tfv-codice not = vet-codice
                       exit perform
                    end-if

LUBEXX              evaluate true
LUBEXX              when vet-regione
LUBEXX                   if trs-regione = tfv-campo1
LUBEXX                      set trovata-tariffa to true
LUBEXX                   end-if
LUBEXX              when vet-prov
LUBEXX                   if trs-provincia = tfv-prov
LUBEXX                      set trovata-tariffa to true
LUBEXX                   end-if
LUBEXX              when vet-cliente
LUBEXX                   if trs-cliente = tfv-campo1
LUBEXX                      set trovata-tariffa to true
LUBEXX                   end-if
LUBEXX              when vet-clides
LUBEXX                   if trs-cliente = tfv-campo1 and
LUBEXX                      trs-destino = tfv-campo2
LUBEXX                      set trovata-tariffa to true
LUBEXX                   end-if
LUBEXX              end-evaluate

LUBEXX              if trovata-tariffa
                       if qta-arrot(idx-serie) >= tfv-qli-da and
                          qta-arrot(idx-serie) <= tfv-qli-a
                          evaluate idx-serie                 
                          when 1 move tfv-euro to trs-tariffa-s1
                          when 2 move tfv-euro to trs-tariffa-s2
                          when 3 move tfv-euro to trs-tariffa-s3
                          end-evaluate
                          exit perform
                       end-if
LUBEXX              end-if

                 end-perform
           end-start.                 
