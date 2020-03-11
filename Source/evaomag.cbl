       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      evaomag.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tordini.sl". 
           copy "rordini.sl".
           copy "tmagaz.sl".
           copy "tcaumag.sl".
      *****     copy "btordini.sl".
      *****     copy "brordini.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tordini.fd".
           copy "rordini.fd".
           copy "tmagaz.fd".  
           copy "tcaumag.fd".
      *****     copy "btordini.fd".
      *****     copy "brordini.fd".

       WORKING-STORAGE SECTION.
           COPY "acucobol.def".

       77  status-tordini   pic xx.  
       77  status-rordini   pic xx.  
       77  status-tmagaz    pic xx.
       77  status-tcaumag   pic xx.

       77  totale           pic 9(9)v99.

       LINKAGE SECTION.                                 
       77  eva-anno         pic 9(4).
       77  eva-from         pic 9(8).
       77  eva-to           pic 9(8).                   

      ******************************************************************
       PROCEDURE DIVISION using eva-anno eva-from eva-to.

       MAIN-PRG.
           perform INIT.
           perform OPEN-FILES.
           perform ELABORAZIONE.
           perform CLOSE-FILES.
           perform EXIT-PGM.

      ***---
       INIT.

      ***---
       OPEN-FILES.
           open input rordini tmagaz tcaumag.
           open i-o tordini.
      
      ***---
       ELABORAZIONE.
           move eva-anno to tor-anno.
           move eva-from to tor-numero.
           start tordini key >= tor-chiave
                invalid continue
            not invalid
                perform until 1 = 2
                   read tordini next at end exit perform end-read
                   if tor-anno not = eva-anno or
                      tor-numero > eva-to
                      exit perform
                   end-if            
                   move tor-causale to tca-codice
                   read tcaumag no lock 
                        invalid continue
                    not invalid
                        move tca-cod-magaz to mag-codice
                        read tmagaz no lock
                             invalid continue
                         not invalid
                             perform LOOP-RIGHE
                             if totale = 0 and
                                tor-causale not = mag-causale-omag
                                move mag-causale-omag to tor-causale
                                rewrite tor-rec
                             end-if
                             if totale > 0 and
                                tor-causale not = mag-causale-eva
                                move mag-causale-eva to tor-causale
                                rewrite tor-rec
                             end-if
                        end-read
                   end-read
                end-perform
           end-start.

      ***---
       LOOP-RIGHE.
           move 0 to totale.
           move low-value  to ror-rec
           move tor-chiave to ror-chiave
           start rordini key is >= ror-chiave
                 invalid continue
           end-start
           perform until 1 = 2

              read rordini next no lock at end exit perform end-read
              if ror-anno       not = tor-anno      or
                 ror-num-ordine not = tor-numero
                 exit perform
              end-if 
  
              |non mi importa calcaolare le quantità
              compute totale = totale + 
                               ror-prz-unitario  + 
                               ror-imponib-merce +
                               ror-imp-cou-cobat +
                               ror-imp-consumo   +
                               ror-add-piombo
              if totale > 0
                 exit perform
              end-if   
           end-perform.

      ***---
       CLOSE-FILES.
           close tordini rordini tmagaz tcaumag.

      ***---
       EXIT-PGM.                                
           goback.
