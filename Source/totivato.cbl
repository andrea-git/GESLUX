       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      totivato.
       AUTHOR.                          Andrea.
       REMARKS. Restituisce il totale del documento ivato
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "rordini.sl".
           copy "tivaese.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "rordini.fd".
           copy "tivaese.fd".

       WORKING-STORAGE SECTION.
       77  status-rordini            pic xx.
       77  status-tivaese            pic xx.
       77  tot-riga                  pic 9(13)v99.
       77  como-iva-3dec             pic 9(9)v999.
                                           
       77  idx                       pic 9.
       77  tot-idx                   pic 9.

       01  tab-aliquote              occurs 3.
         03 el-importo-riga          pic 9(13)v99.
         03 el-importo-iva           pic 9(13)v99.
         03 el-cod-iva               pic x(3).

       LINKAGE SECTION.
       copy "link-totivato.def".

      ******************************************************************
       PROCEDURE DIVISION using link-ti-linkage.
       MAIN-PRG.
           perform OPEN-FILES.
           perform ELABORAZIONE.
           perform CLOSE-FILES.
           perform EXIT-PGM.

      ***---
       OPEN-FILES.
           open input rordini tivaese.

      ***---
       ELABORAZIONE.
           move 0              to link-ti-tot-doc.
           move link-ti-anno   to ror-anno.
           move link-ti-numero to ror-num-ordine.
           move low-value    to ror-num-riga.
           start rordini key not < ror-chiave
                 invalid continue
             not invalid 
                 perform until 1 = 2
                    read rordini next
                       at end
                          exit perform
                    end-read
                    if ror-anno       not = link-ti-anno   or
                       ror-num-ordine not = link-ti-numero
                       exit perform
                    end-if  
                    if ror-si-omaggio
                       exit perform cycle
                    end-if
                    compute tot-riga = ( ror-imponib-merce +
                                         ror-imp-consumo   +
                                         ror-imp-cou-cobat +
                                         ror-add-piombo )  * ror-qta
                    perform varying idx from 1 by 1 
                              until idx > 3
                       if ror-cod-iva = el-cod-iva(idx)
                          compute el-importo-riga(idx) = 
                                  el-importo-riga(idx) + tot-riga
                          exit perform
                       end-if
                    end-perform
                    if idx = 4
                       add 1 to tot-idx
                       move ror-cod-iva to el-cod-iva(tot-idx)
                       move tot-riga to el-importo-riga(tot-idx)
                    end-if
                 end-perform
           end-start.

           perform varying idx from 1 by 1 
                     until idx > tot-idx
              move "IV"            to tbliv-codice1
              move el-cod-iva(idx) to tbliv-codice2
              read tivaese 
                   invalid initialize record-tbliv 
              end-read
              if tbliv-percentuale > 0
                 compute como-iva-3dec =
                         tbliv-percentuale * el-importo-riga(idx) / 100
                 add 0,005                to como-iva-3dec
                 move como-iva-3dec       to el-importo-iva(idx)
              end-if
           end-perform.

           compute link-ti-tot-doc = el-importo-riga(1) +
                                     el-importo-iva(1)  +
                                     el-importo-riga(2) +
                                     el-importo-iva(2)  +
                                     el-importo-riga(3) +
                                     el-importo-iva(3).

      ***---
       CLOSE-FILES.
           close rordini tivaese.

      ***---
       EXIT-PGM.
           goback.
