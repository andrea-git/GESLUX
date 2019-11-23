       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      val-suf-reg.
       AUTHOR.                          Luciano.
  
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "prodener.sl".


      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "prodener.fd".

       working-storage section.
           copy "acugui.def".

       77  status-prodener   pic xx.

       01  cont-x      pic x(3).
       01  CONT        PIC 9(3) redefines cont-x.
       77  CONT-ED              PIC Z(3).
       77  scelta               pic 9.

       procedure division.
      ***---
       MAIN.
           display message box 
                 "Confermi la valorizzazione del suffisso del registro?"
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
           open i-o prodener.

           move 0 to cont.

           move low-value to pen-codice
           start prodener key not < pen-chiave
              invalid
                 continue
              not invalid
                 perform until 1 = 2
                    read prodener next
                       at end
                          exit perform
                    end-read
                    add 1       to cont
                    move cont-x to pen-suf-reg
                    rewrite pen-rec
                       invalid
                          continue
                    end-rewrite
                 end-perform
           end-start.

           close    prodener.

           move cont   to cont-ed
           display message box "Valorizzati " cont-ed " record.".



