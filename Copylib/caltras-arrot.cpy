

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
                      move vet-valore-arrot(idx) to como-arrot
                      evaluate como-arrot
                      when 0   move  0     to idx
                               set nessuno to true
                      when 0,1 move 10     to idx
                               set  10-kg  to true
                      when 0,2 move 10     to idx
                               set  20-kg  to true
                      when 0,5 move 10     to idx
                               set  50-kg  to true
                      when 1   move  9     to idx
                               set 100-kg  to true
                      end-evaluate

                      if idx > 0
                         add  1 to idx giving como-idx
                         move 0 to comodo1
                         perform varying como-idx from como-idx by 1 
                                   until como-idx > 15
                            if cifra(como-idx) > 0
                               move cifra(idx) to comodo1
                               exit perform
                            end-if
                         end-perform
LUBEXX*****                         if comodo1 = 0 |E' già arrotondato
LUBEXX*****                            move 0 to idx
LUBEXX*****                         end-if
                      end-if

                      if idx > 0
                         |Se ho 57,000 ho comunque
                         |un valore già arrotondato
                         if cifra(10) not = 0 or
                            cifra(11) not = 0 or 
                            cifra(12) not = 0 

                            if 10-kg
                               |Ad es. 1,600 è già a posto
                               if cifra(11) not = 0 or
                                  cifra(12) not = 0
                                  add como-arrot to tot-peso-qli
                               end-if
                            else
                               add como-arrot to tot-peso-qli
                            end-if

                            if 20-kg

                               |Ad es. 1,600 è già a posto
                               if cifra(11) not = 0 or
                                  cifra(12) not = 0
                                  add como-arrot to tot-peso-qli
                               end-if
                            else
                               add como-arrot to tot-peso-qli
                            end-if

                            if 50-kg
                               evaluate true
                               when cifra(10) > 5
                                    move 5 to cifra(10)
                               when cifra(10) = 5
                                    if cifra(11) = 0 and
                                       cifra(12) = 0
                                       move 0 to cifra(10)
                                    else
                                       move 5 to cifra(10)
                                    end-if
                               when cifra(10) = 0
                                    move 0 to cifra(10)
                                    |Rientra ad esempio un 
                                    |peso di 150,000 kg.
                                    if cifra(11) = 0 and
                                       cifra(12) = 0
                                       subtract 1 from cifra(9)
                                       move 5 to cifra(10)
                                    end-if
                               when cifra(10) < 5
                                    move 0 to cifra(10)
                               end-evaluate
                            end-if

                            add 1 to idx giving como-idx
                            perform varying como-idx from como-idx by 1 
                                      until como-idx > 15
                               move 0 to cifra(como-idx)
                            end-perform

                         end-if
                      end-if

                      move tot-peso-qli to trs-qta-arrot
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
                      move vet-valore-arrot(idx) to como-arrot
                      evaluate como-arrot
                      when 0   move  0     to idx
                               set nessuno to true    
                      when 0,1 move 10     to idx
                               set  10-kg  to true
                      when 0,2 move 10     to idx
                               set  20-kg  to true
                      when 0,5 move 10     to idx
                               set  50-kg  to true
                      when 1   move  9     to idx
                               set 100-kg  to true
                      end-evaluate

                      if idx > 0
                         add  1 to idx giving como-idx
                         move 0 to comodo1
                         perform varying como-idx from como-idx by 1 
                                   until como-idx > 15
                            if cifra-SHI(como-idx) > 0
                               move cifra-SHI(idx) to comodo1
                               exit perform
                            end-if
                         end-perform
LUBEXX*****                         if comodo1 = 0 |E' già arrotondato
LUBEXX*****                            move 0 to idx
LUBEXX*****                         end-if
                      end-if

                      if idx > 0
                         |Se ho 57,000 ho comunque
                         |un valore già arrotondato
                         if cifra-SHI(10) not = 0 or
                            cifra-SHI(11) not = 0 or 
                            cifra-SHI(12) not = 0

                            if 10-kg
                               |Ad es. 1,600 è già a posto
                               if cifra-SHI(11) not = 0 or
                                  cifra-SHI(12) not = 0
                                  add como-arrot to tot-peso-qli-SHI
                               end-if
                            else
                               add como-arrot to tot-peso-qli-SHI
                            end-if

                            if 20-kg
                               |Ad es. 1,600 è già a posto
                               if cifra-SHI(11) not = 0 or
                                  cifra-SHI(12) not = 0
                                  add como-arrot to tot-peso-qli-SHI
                               end-if
                            else
                               add como-arrot to tot-peso-qli-SHI
                            end-if

                            if 50-kg
                               evaluate true
                               when cifra-SHI(10) > 5
                                    move 5 to cifra-SHI(10)
                               when cifra-SHI(10) = 5
                                    if cifra-SHI(11) = 0 and
                                       cifra-SHI(12) = 0
                                       move 0 to cifra-SHI(10)
                                    else
                                       move 5 to cifra-SHI(10)
                                    end-if
                               when cifra-SHI(10) = 0
                                    move 0 to cifra-SHI(10)
                                    |Rientra ad esempio un 
                                    |peso di 150,000 kg.
                                    if cifra-SHI(11) = 0 and
                                       cifra-SHI(12) = 0
                                       subtract 1 from cifra-SHI(9)
                                       move 5 to cifra-SHI(10)
                                    end-if
                               when cifra-SHI(10) < 5
                                    move 0 to cifra-SHI(10)
                               end-evaluate
                            end-if

                            add 1 to idx giving como-idx
                            perform varying como-idx from como-idx by 1 
                                      until como-idx > 15
                               move 0 to cifra-SHI(como-idx)
                            end-perform

                         end-if
                      end-if

                      move tot-peso-qli-SHI to trs-qta-arrot-SHI
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
                      move vet-valore-arrot(idx) to como-arrot
                      evaluate como-arrot
                      when 0   move  0     to idx
                               set nessuno to true
                      when 0,1 move 10     to idx
                               set  10-kg  to true
                      when 0,2 move 10     to idx
                               set  20-kg  to true
                      when 0,5 move 10     to idx
                               set  50-kg  to true
                      when 1   move  9     to idx
                               set 100-kg  to true
                      end-evaluate

                      if idx > 0
                         add  1 to idx giving como-idx
                         move 0 to comodo1
                         perform varying como-idx from como-idx by 1 
                                   until como-idx > 15
                            if cifra-GET(como-idx) > 0
                               move cifra-GET(idx) to comodo1
                               exit perform
                            end-if
                         end-perform
LUBEXX*****                         if comodo1 = 0 |E' già arrotondato
LUBEXX*****                            move 0 to idx
LUBEXX*****                         end-if
                      end-if

                      if idx > 0
                         |Se ho 57,000 ho comunque
                         |un valore già arrotondato
                         if cifra-GET(10) not = 0 or
                            cifra-GET(11) not = 0 or 
                            cifra-GET(12) not = 0  

                            if 10-kg
                               |Ad es. 1,600 è già a posto
                               if cifra-GET(11) not = 0 or
                                  cifra-GET(12) not = 0
                                  add como-arrot to tot-peso-qli-GET
                               end-if
                            else
                               add como-arrot to tot-peso-qli-GET
                            end-if

                            if 20-kg
                               |Ad es. 1,600 è già a posto
                               if cifra-GET(11) not = 0 or
                                  cifra-GET(12) not = 0
                                  add como-arrot to tot-peso-qli-GET
                               end-if
                            else
                               add como-arrot to tot-peso-qli-GET
                            end-if

                            if 50-kg
                               evaluate true
                               when cifra-GET(10) > 5
                                    move 5 to cifra-GET(10)
                               when cifra-GET(10) = 5
                                    if cifra-GET(11) = 0 and
                                       cifra-GET(12) = 0
                                       move 0 to cifra-GET(10)
                                    else
                                       move 5 to cifra-GET(10)
                                    end-if
                               when cifra-GET(10) = 0
                                    move 0 to cifra-GET(10)
                                    |Rientra ad esempio un 
                                    |peso di 150,000 kg.
                                    if cifra-GET(11) = 0 and
                                       cifra-GET(12) = 0
                                       subtract 1 from cifra-GET(9)
                                       move 5 to cifra-GET(10)
                                    end-if
                               when cifra-GET(10) < 5
                                    move 0 to cifra-GET(10)
                               end-evaluate
                            end-if

                            add 1 to idx giving como-idx
                            perform varying como-idx from como-idx by 1 
                                      until como-idx > 15
                               move 0 to cifra-GET(como-idx)
                            end-perform

                         end-if
                      end-if

                      move tot-peso-qli-GET to trs-qta-arrot-GET
                   else
                      if not esiste-scaglione
                         move tot-peso-qli-GET to trs-qta-arrot-GET
                      end-if
                   end-if
                end-if
           end-read.
