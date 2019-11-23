       PROGRAM-ID.     codfis.
       AUTHOR. Giangio.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       FILE-CONTROL.
       copy "comuni.sl".
       copy "pro_piva.sl".

       FILE SECTION.           
       copy "comuni.fd".
       copy "pro_piva.fd".
       
      *************************
       WORKING-STORAGE SECTION.
       
       01  somma1          pic 99 value 0.        
       01  comodo.
           03 comodo-1    pic 9  value 0.
           03 comodo-2    pic 9  value 0. 
       01  comodo-3       pic 99 value 0.
       01  cont           pic 99 value 0.
       01  controllo-2    pic 9  value 0.
       01  a              pic x.
       01  iniziale-1     pic x.
       01  iniziale-2     pic x.
       01  iniziale-3     pic x.
       01  mese           pic x.
       01  g1             pic 9 value 0.
       01  g2             pic 9 value 0.
       01  codice         pic x(4).
       01  controllo      pic 9.
       01  somma          pic 9(4) value 0.
       01  resto          pic 99.
       01  cont-car       pic 99.
       01  cont-car2      pic 99.   
       01  c2             pic 99.
       01  c1             pic 99.   
       01  status-comuni    pic xx.    
       01  status-pro-piva  pic xx.  
       01  numero-cons    pic 99.  
                            
       LINKAGE SECTION.
           copy "codfis.lks".
         
      ************************************************************************
       procedure division using link-codfis.
      ************************************************************************
       main-logic.
           call "C$TOUPPER" using cf-nome,    value 50.
           call "C$TOUPPER" using cf-cognome, value 50.
           call "C$TOUPPER" using cf-prov,    value 2.    
           call "C$TOUPPER" using cf-loc,     value 55.    
           open input comuni.
           open input pro_piva.
           initialize c1, c2.
           move 0 to cf-risultato.
           evaluate cf-operazione
           when 0
                perform CREA-CODICE-FISCALE
           when 1                          
                perform CONTROLLO-MIN      
           when 2                          
                perform CONTROLLO-PREC     
           when 3                          
                perform CONTROLLO-PIVA     
           when other
                exit program
           end-evaluate.
           close comuni.    
           close pro_piva.
           goback cf-risultato.

      ***---
       CONTROLLO-PIVA.
           move cf-piva(8:3) to piva-chiave.
           read pro_piva key is piva-chiave
              invalid 
                 move 6 to cf-risultato
                 exit paragraph
              not invalid
                 continue
           end-read.
           perform varying cont from 1 by 2 until cont > 9
              move cf-piva(cont:1) to c1
              add c1 to somma1
           end-perform.
           perform varying cont from 2 by 2 until cont > 10
              move cf-piva(cont:1) to comodo-3
              compute comodo-3 = comodo-3 * 2
              move comodo-3 to comodo
              add comodo-1 to somma1
              add comodo-2 to somma1
           end-perform.
           move somma1(2:1) to comodo-2.
           compute controllo-2 = 10 - comodo-2.
           move cf-piva(11:1) to comodo-1.
           if controllo-2 not = comodo-1
              move 4 to cf-risultato
           end-if.

      ***---
       CONTROLLO-PREC.
           perform P-MESE.
           if mese not = cf-codice-fiscale(9:1)       
              move 1 to cf-risultato
              exit paragraph
           end-if.
           perform CARATTERE-CONTROLLO.

      ***---
       CONTROLLO-MIN.
           initialize cont-car, somma, resto.
           perform varying cont-car from 1 by 1 until cont-car > 16
              move cf-codice-fiscale(cont-car:1) to a
              if cont-car < 7
                 if a is numeric 
                    move 2 to cf-risultato
                    exit paragraph
                 end-if 
              end-if
              if cont-car = 9                    
                 if a not = "A" and a not = "B" and
                    a not = "C" and a not = "D" and
                    a not = "E" and a not = "H" and
                    a not = "L" and a not = "M" and
                    a not = "P" and a not = "R" and
                    a not = "S" and a not = "T" 
                    move 1 to cf-risultato
                    exit paragraph
                 else
                    move a to mese
                 end-if
              end-if              
              if cont-car = 10 
                 move a to g1
                 if g1 > 7               
                    move 3 to cf-risultato
                    exit paragraph
                 end-if
              end-if
              if cont-car = 11
                 move a to g2
                 if g1 = 3 or g1 = 7
                    if g2 > 1
                       move 3 to cf-risultato
                       exit paragraph
                    end-if
                    if g2 = 1 
                       if mese = "D" or mese = "H"  
                       or mese = "P" or mese = "S"  
                          move 3 to cf-risultato
                          exit paragraph
                       end-if
                    end-if
                 end-if
                 if mese = "B"
                    if g1 = 3 or g1 = 7  
                       move 3 to cf-risultato
                       exit paragraph
                    end-if
                 end-if
              end-if                                           
              perform TABELLE           
           end-perform.            
           move cf-codice-fiscale(12:1) to codice(1:1)    
           move cf-codice-fiscale(13:1) to codice(2:1)  
           move cf-codice-fiscale(14:1) to codice(3:1)  
           move cf-codice-fiscale(15:1) to codice(4:1)
           move codice to com-chiave
           read comuni
              invalid
                 move 5 to cf-risultato
              not invalid
                 continue
           end-read.
           perform CARATTERE-CONTROLLO.

      ***---
       CREA-CODICE-FISCALE.
           perform P-COGNOME.
           perform P-NOME.                  
           move cf-aa2-n to cf-codice-fiscale(7:2).
           perform P-MESE.
           move mese to cf-codice-fiscale(9:1).
           if cf-femmina
              add 40 to cf-gg-n
           end-if.
           move cf-gg-n to cf-codice-fiscale(10:2). 
           move cf-prov to com-prov.
           move cf-loc  to com-descr.
           read comuni key is com-dati
              invalid   
                 move 5 to cf-risultato
                 exit paragraph
              not invalid
                 move com-codice to codice
           end-read.
           move codice to cf-codice-fiscale(12:4).
           perform varying cont-car from 1 by 1 until cont-car > 16 
              move cf-codice-fiscale(cont-car:1) to a
              perform TABELLE
           end-perform.   
           evaluate resto
           when 0  move "A" to a
           when 1  move "B" to a 
           when 2  move "C" to a
           when 3  move "D" to a
           when 4  move "E" to a
           when 5  move "F" to a
           when 6  move "G" to a
           when 7  move "H" to a
           when 8  move "I" to a
           when 9  move "J" to a
           when 10 move "K" to a
           when 11 move "L" to a
           when 12 move "M" to a
           when 13 move "N" to a
           when 14 move "O" to a
           when 15 move "P" to a
           when 16 move "Q" to a
           when 17 move "R" to a
           when 18 move "S" to a
           when 19 move "T" to a
           when 20 move "U" to a
           when 21 move "V" to a
           when 22 move "W" to a
           when 23 move "X" to a
           when 24 move "Y" to a
           when 25 move "Z" to a
           end-evaluate.                    
           move a to cf-codice-fiscale(16:1).   

      ***---
       P-COGNOME.
           move space to iniziale-1.
           move space to iniziale-2.
           move space to iniziale-3.
           initialize cont-car2, c1, c2.
           perform varying cont-car2 from 1 by 1 until cont-car2 > 50
              if cf-cognome(cont-car2:1) not = "A" and
                 cf-cognome(cont-car2:1) not = "E" and
                 cf-cognome(cont-car2:1) not = "I" and
                 cf-cognome(cont-car2:1) not = "O" and
                 cf-cognome(cont-car2:1) not = "U" and
                 cf-cognome(cont-car2:1) not = spaces
                 if iniziale-1 = spaces  
                    move cf-cognome(cont-car2:1) to iniziale-1
                 else
                    if iniziale-2 = spaces 
                       move cf-cognome(cont-car2:1) to iniziale-2
                    else
                       if iniziale-3 = spaces
                          move cf-cognome(cont-car2:1) to iniziale-3
                       end-if
                    end-if
                 end-if
              end-if
           end-perform.
           if iniziale-1 = space or iniziale-2 = spaces or
              iniziale-3 = space
              initialize cont-car2
              perform varying cont-car2 from 1 by 1 until cont-car2 > 50
                 if cf-cognome(cont-car2:1) = "A" or
                    cf-cognome(cont-car2:1) = "E" or
                    cf-cognome(cont-car2:1) = "I" or
                    cf-cognome(cont-car2:1) = "O" or
                    cf-cognome(cont-car2:1) = "U" or
                    cf-cognome(cont-car2:1) = spaces
                    if iniziale-1 = spaces 
                       move cf-cognome(cont-car2:1) to iniziale-1       
                    else
                       if iniziale-2 = spaces  
                          move cf-cognome(cont-car2:1) to iniziale-2
                       else
                          if iniziale-3 = spaces   
                              move cf-cognome(cont-car2:1) to iniziale-3 
                          end-if
                       end-if
                    end-if
                 end-if
              end-perform
           end-if.
           if iniziale-1 = space
              move "X" to iniziale-1
           end-if.
           if iniziale-2 = spaces
              move "X" to iniziale-2
           end-if.
           if iniziale-3 = space
              move "X" to iniziale-3
           end-if.
           move iniziale-1 to cf-codice-fiscale(1:1).
           move iniziale-2 to cf-codice-fiscale(2:1).
           move iniziale-3 to cf-codice-fiscale(3:1).

      ***---
       P-NOME.
           move space to iniziale-1.
           move space to iniziale-2.
           move space to iniziale-3.
           move 0 to cont-car.
           move 0 to numero-cons.
           initialize cont-car2.
           perform varying cont-car2 from 1 by 1 until cont-car2 > 50
              if cf-nome(cont-car2:1) not = "A" and
                 cf-nome(cont-car2:1) not = "E" and
                 cf-nome(cont-car2:1) not = "I" and
                 cf-nome(cont-car2:1) not = "O" and
                 cf-nome(cont-car2:1) not = "U" and
                 cf-nome(cont-car2:1) not = spaces
                 add 1 to numero-cons
              end-if
           end-perform.

           initialize cont-car2.
           perform varying cont-car2 from 1 by 1 until cont-car2 > 50
              if cf-nome(cont-car2:1) not = "A" and
                 cf-nome(cont-car2:1) not = "E" and
                 cf-nome(cont-car2:1) not = "I" and
                 cf-nome(cont-car2:1) not = "O" and
                 cf-nome(cont-car2:1) not = "U" and
                 cf-nome(cont-car2:1) not = spaces
                 if iniziale-1 = spaces    
                    move cont-car2 to c1
                    move cf-nome(cont-car2:1) to iniziale-1
                 else
                    if iniziale-2 = spaces
                       if numero-cons = 2 or numero-cons = 3
                          move 1 to cont-car
                       end-if
                       if cont-car = 1   
                          move cont-car2 to c2
                          move cf-nome(cont-car2:1) to iniziale-2
                       else
                          move 1 to cont-car
                       end-if
                    else
                       if iniziale-3 = spaces                          
                          move cf-nome(cont-car2:1) to iniziale-3                            
                       end-if
                    end-if
                 end-if
              end-if
           end-perform.
           perform varying cont-car2 from 1 by 1 until cont-car2 > 50
              if cf-nome(cont-car2:1) not = "A" and
                 cf-nome(cont-car2:1) not = "E" and
                 cf-nome(cont-car2:1) not = "I" and
                 cf-nome(cont-car2:1) not = "O" and
                 cf-nome(cont-car2:1) not = "U" and
                 cf-nome(cont-car2:1) not = spaces
                 if iniziale-1 = spaces
                    move cf-nome(cont-car2:1) to iniziale-1
                 else
                    if iniziale-2 = spaces and c1 not = cont-car2
                       move cf-nome(cont-car2:1) to iniziale-2
                    else
                       if iniziale-3 = spaces  and c1 not = cont-car2 
                             if c2 not = cont-car2                          
                                move cf-nome(cont-car2:1) to iniziale-3
                             end-if
                       end-if
                    end-if
                 end-if
              end-if
           end-perform.
           if iniziale-1 = space or iniziale-2 = spaces or
              iniziale-3 = space
              initialize cont-car2
              perform varying cont-car2 from 1 by 1 until cont-car2 > 50
                 if cf-nome(cont-car2:1) = "A" or
                    cf-nome(cont-car2:1) = "E" or
                    cf-nome(cont-car2:1) = "I" or
                    cf-nome(cont-car2:1) = "O" or
                    cf-nome(cont-car2:1) = "U" or
                    cf-nome(cont-car2:1) = spaces
                    if iniziale-1 = spaces
                       move cf-nome(cont-car2:1) to iniziale-1
                    else
                       if iniziale-2 = spaces
                          move cf-nome(cont-car2:1) to iniziale-2
                       else
                          if iniziale-3 = spaces
                             move cf-nome(cont-car2:1) to iniziale-3                              
                          end-if
                       end-if
                    end-if
                 end-if
              end-perform
           end-if.
           if iniziale-1 = space
              move "X" to iniziale-1
           end-if.
           if iniziale-2 = spaces
              move "X" to iniziale-2
           end-if.
           if iniziale-3 = space
              move "X" to iniziale-3
           end-if.
           move iniziale-1 to cf-codice-fiscale(4:1).
           move iniziale-2 to cf-codice-fiscale(5:1).
           move iniziale-3 to cf-codice-fiscale(6:1).

      ****---
       P-MESE.
           evaluate cf-mm-n
           when 01 move "A" to mese
           when 02 move "B" to mese
           when 03 move "C" to mese
           when 04 move "D" to mese
           when 05 move "E" to mese
           when 06 move "H" to mese
           when 07 move "L" to mese
           when 08 move "M" to mese
           when 09 move "P" to mese
           when 10 move "R" to mese
           when 11 move "S" to mese
           when 12 move "T" to mese
           end-evaluate.

      ***---
       CARATTERE-CONTROLLO.
           move cf-codice-fiscale(16:1) to a.
           evaluate resto
           when 0  
              if a not = "A"
                 move "." to a
              end-if
           when 1
              if a not = "B"
                 move "." to a
              end-if
           when 2
              if a not = "C"
                 move "." to a
              end-if
           when 3
              if a not = "D"
                 move "." to a
              end-if
           when 4
              if a not = "E"
                 move "." to a
              end-if
           when 5
              if a not = "F"
                 move "." to a
              end-if
           when 6
              if a not = "G"
                 move "." to a
              end-if
           when 7
              if a not = "H"
                 move "." to a
              end-if
           when 8
              if a not = "I"
                 move "." to a
              end-if
           when 9
              if a not = "J"
                 move "." to a
              end-if
           when 10
              if a not = "K"    
                 move "." to a
              end-if
           when 11
              if a not = "L"    
                 move "." to a
              end-if
           when 12
              if a not = "M"
                 move "." to a
              end-if
           when 13
              if a not = "N"
                 move "." to a
              end-if
           when 14
              if a not = "O"
                 move "." to a
              end-if
           when 15
              if a not = "P"
                 move "." to a
              end-if
           when 16
              if a not = "Q"
                 move "." to a
              end-if
           when 17
              if a not = "R"
                 move "." to a
              end-if
           when 18
              if a not = "S"
                 move "." to a
              end-if
           when 19
              if a not = "T"
                 move "." to a
              end-if
           when 20
              if a not = "U"
                 move "." to a
              end-if
           when 21
              if a not = "V"
                 move "." to a
              end-if
           when 22              
              if a not = "W"
                 move "." to a
              end-if
           when 23
              if a not = "X"
                 move "." to a
              end-if
           when 24
              if a not = "Y"
                 move "." to a
              end-if
           when 25
              if a not = "Z"
                 move "." to a
              end-if
           end-evaluate
           if a = "."                 
              move 4 to cf-risultato
              exit paragraph
           end-if.                   
           
      ***---
      * calcolo.
       TABELLE.
           if cont-car = 16                      
              divide 26 into somma giving somma remainder resto
              exit paragraph
           end-if.
           if cont-car = 2  or cont-car = 4  or cont-car = 6  or
              cont-car = 8  or cont-car = 10 or cont-car = 12 or
              cont-car = 14
              evaluate a
              when "0" add 0 to somma
              when "1" add 1 to somma
              when "2" add 2 to somma
              when "3" add 3 to somma
              when "4" add 4 to somma
              when "5" add 5 to somma
              when "6" add 6 to somma
              when "7" add 7 to somma
              when "8" add 8 to somma
              when "9" add 9 to somma
              when "A" add 0 to somma
              when "B" add 1 to somma
              when "C" add 2 to somma
              when "D" add 3 to somma
              when "E" add 4 to somma
              when "F" add 5 to somma
              when "G" add 6 to somma
              when "H" add 7 to somma
              when "I" add 8 to somma
              when "J" add 9 to somma
              when "K" add 10 to somma
              when "L" add 11 to somma
              when "M" add 12 to somma
              when "N" add 13 to somma
              when "O" add 14 to somma
              when "P" add 15 to somma
              when "Q" add 16 to somma
              when "R" add 17 to somma
              when "S" add 18 to somma
              when "T" add 19 to somma
              when "U" add 20 to somma
              when "V" add 21 to somma
              when "W" add 22 to somma
              when "X" add 23 to somma
              when "Y" add 24 to somma
              when "Z" add 25 to somma
              end-evaluate
           else
              evaluate a
              when "0" add 1 to somma
              when "1" add 0 to somma
              when "2" add 5 to somma
              when "3" add 7 to somma
              when "4" add 9 to somma
              when "5" add 13 to somma
              when "6" add 15 to somma
              when "7" add 17 to somma
              when "8" add 19 to somma
              when "9" add 21 to somma
              when "A" add 1 to somma
              when "B" add 0 to somma
              when "C" add 5 to somma
              when "D" add 7 to somma
              when "E" add 9 to somma
              when "F" add 13 to somma
              when "G" add 15 to somma
              when "H" add 17 to somma
              when "I" add 19 to somma
              when "J" add 21 to somma
              when "K" add 2 to somma
              when "L" add 4 to somma
              when "M" add 18 to somma
              when "N" add 20 to somma
              when "O" add 11 to somma
              when "P" add 3 to somma
              when "Q" add 6 to somma
              when "R" add 8 to somma
              when "S" add 12 to somma
              when "T" add 14 to somma
              when "U" add 16 to somma
              when "V" add 10 to somma
              when "W" add 22 to somma
              when "X" add 25 to somma
              when "Y" add 24 to somma
              when "Z" add 23 to somma
              end-evaluate
           end-if.
