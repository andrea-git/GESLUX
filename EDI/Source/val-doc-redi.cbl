       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      val-doc-redi.
       AUTHOR.                          Luciano.
  
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "prodener.sl".
           copy "redi.sl".
           copy "paramedi.sl".


      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "prodener.fd".
           copy "redi.fd".
           copy "paramedi.fd".

       working-storage section.
           copy "acugui.def".

       77  status-prodener   pic xx.
       77  status-redi       pic xx.
       77  status-paramedi   pic xx.

      * 01  cont-x      pic x(3).
       01  CONT        PIC 9(5).| redefines cont-x.
       77  CONT-ED              PIC Z(5).
       77  scelta               pic 9.

       procedure division.
      ***---
       MAIN.
           display message box 
                 "Confermi la valorizzazione del tipo documento e "
                 "del suffisso del registro?"
                          type mb-yes-no
                          default mb-no
                          giving scelta
                          icon 2
           if scelta = mb-yes
              perform CONVERSIONE
           end-if.

           goback.


      ***---
       CONVERSIONE.
           open i-o redi.
           open input paramedi
           open input prodener

           move space  to pae-codice
           read paramedi
              invalid
                 continue
           end-read.


           move 0 to cont.

           move low-value to redi-chiave
           start redi key not < redi-chiave
              invalid
                 continue
              not invalid
                 perform until 1 = 2
                    read redi next
                       at end
                          exit perform
                    end-read
                    perform TRATTA-RECORD
                 end-perform
           end-start.

           close redi.
           close paramedi.
           close prodener.

           move cont   to cont-ed
           display message box "Valorizzati " cont-ed " record.".

      ***---
       TRATTA-RECORD.
           add 1       to cont.
           
           if redi-suf-reg = space
              move redi-cpa     to pen-cpa
              move redi-nc      to pen-nc
              move redi-taric   to pen-taric
              move redi-dac     to pen-dac

              read PRODENER key is pen-k-nc
                 invalid
                    continue
                 not invalid
                    move pen-suf-reg to redi-suf-reg
              end-read

           end-if

           if redi-tipo-doc = space
              if redi-scarico and redi-cau-movim = "062"
                 move pae-tipo-doc-int   to redi-tipo-doc
              else
                 move pae-tipo-doc       to redi-tipo-doc
              end-if
           end-if.

           rewrite redi-rec
              invalid
                 continue
           end-rewrite.
