       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      caltotiva.
       AUTHOR.                          Andrea.
       REMARKS. Restituisce il totale ivato di fattura e nota credito
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tordini.sl".
           copy "tnotacr.sl". 
           copy "rordini.sl".
           copy "rnotacr.sl".
           copy "tivaese.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tordini.fd".
           copy "tnotacr.fd". 
           copy "rordini.fd".
           copy "rnotacr.fd".
           copy "tivaese.fd". 

       WORKING-STORAGE SECTION.
       77  status-tordini    pic xx.
       77  status-tnotacr    pic xx.  
       77  status-rordini    pic xx.
       77  status-rnotacr    pic xx.
       77  status-tivaese    pic xx.  
                                             
       77  idx               pic 9 value 0.
       77  como-iva          pic x(3).

       01  tab-iva           occurs 3.
         05 el-cod-iva       pic x(3).
         05 el-perce-iva     pic 9(3)v99.
         05 el-impon         pic 9(10)v999.
         05 el-iva           pic 9(10)v999.
           
       LINKAGE SECTION.
       copy "link-caltotiva.def".

      ******************************************************************
       PROCEDURE DIVISION USING caltotiva-linkage.

       MAIN-PRG.
           perform INIT.
           perform OPEN-FILES.            
           if caltotiva-tipo = "F"
              perform ELABORAZIONE-F
           else
              perform ELABORAZIONE-N
           end-if.
           perform CALCOLO-TOTALE.
           perform CLOSE-FILES.   
           perform EXIT-PGM.

      ***---
       INIT.
           move 0 to caltotiva-tot.

      ***---
       OPEN-FILES.   
           open input tordini tnotacr rordini rnotacr tivaese.
                      
      ***---
       ELABORAZIONE-F.
           move caltotiva-doc to tor-chiave.
           read tordini no lock
                invalid continue
            not invalid
                move low-value  to ror-rec
                move tor-chiave to ror-chiave
                start rordini key >= ror-chiave
                      invalid continue
                  not invalid
                      perform until 1 = 2
                         read rordini next at end exit perform end-read
                         if ror-anno       not = tor-anno or
                            ror-num-ordine not = tor-numero
                            exit perform
                         end-if 
                         move ror-cod-iva to como-iva
                         perform VALORIZZA-IVA        
                         compute el-impon(idx) =
                                 el-impon(idx) +
                               ( ror-qta * ( ror-imponib-merce + 
                                             ror-imp-consumo   +
                                             ror-imp-cou-cobat +
                                             ror-add-piombo ) )
                      end-perform
                end-start
           end-read.

      ***---
       ELABORAZIONE-N.   
           move caltotiva-doc to tno-chiave.
           read tnotacr no lock
                invalid continue
            not invalid
                move low-value    to rno-rec
                move tno-chiave   to rno-chiave
                start rnotacr key >= rno-chiave
                      invalid continue
                  not invalid
                      perform until 1 = 2
                         read rnotacr next at end exit perform end-read
                         if rno-anno   not = tno-anno or
                            rno-numero not = tno-numero
                            exit perform
                         end-if         
                         move rno-cod-iva to como-iva
                         perform VALORIZZA-IVA           
                         compute el-impon(idx) =
                                 el-impon(idx) +
                               ( rno-qta * ( rno-prz-unitario + 
                                             rno-imp-consumo   +
                                             rno-imp-cou-cobat +
                                             rno-add-piombo ) )
                      end-perform
                end-start
           end-read.

      ***---
       VALORIZZA-IVA.
           perform varying idx from 1 by 1 
                     until idx > 3
              if como-iva = el-cod-iva(idx)
                 exit perform
              end-if
           end-perform
           if idx > 3   
              perform varying idx from 1 by 1 
                        until idx > 3
                 if el-cod-iva(idx) = spaces
                    move "IV"        to tbliv-codice1
                    move como-iva to tbliv-codice2
                    read tivaese no lock 
                         invalid move 0 to tbliv-percentuale 
                    end-read
                    move tbliv-percentuale 
                      to el-perce-iva(idx)
                    move como-iva to el-cod-iva(idx)
                    exit perform
                 end-if
              end-perform
           end-if.   

      ***---
       CALCOLO-TOTALE. 
           perform varying idx from 1 by 1 
                     until idx > 3
              compute el-iva(idx) =
                      el-impon(idx) * el-perce-iva(idx) / 100
           end-perform.
           perform varying idx from 1 by 1 
                     until idx > 3
              compute caltotiva-tot =
                      caltotiva-tot + el-impon(idx) + el-iva(idx)
           end-perform.
           

      ***---
       CLOSE-FILES.  
           close tordini tnotacr rordini rnotacr tivaese.

      ***---
       EXIT-PGM.
           goback.
