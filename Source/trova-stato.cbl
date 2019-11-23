       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      trova-stato.
       REMARKS.
           Partendo da un'evasione imposta lo stato in base a tutti i 
           master collegati (se uno solo è bloccato blocca anche l'evasione)
       SPECIAL-NAMES. decimal-point is comma.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "mtordini.sl".
           copy "rordini.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "mtordini.fd".
           copy "rordini.fd".

       WORKING-STORAGE SECTION.

      * Status Files GESLUX
       77  status-mtordini   pic X(2).
       77  status-rordini    pic X(2).

       01  filler            pic 9.
         88 bloccato         value 1, false 0.

       LINKAGE SECTION.
       01  link-chiave.
         05 link-anno        pic 9(4).
         05 link-numero      pic 9(8).
       77  link-stato        pic x.

      ******************************************************************
       PROCEDURE DIVISION USING link-chiave link-stato.

       MAIN-PRG.
           set bloccato to false.
           open input rordini mtordini.
           move low-value   to ror-rec.
           move link-chiave to ror-chiave.
           start rordini key >= ror-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rordini next at end exit perform end-read
                    if ror-anno       not = link-anno or
                       ror-num-ordine not = link-numero
                       exit perform
                    end-if
                    move ror-chiave-ordine-testa to mto-chiave
                    read mtordini no lock invalid continue end-read
                    if mto-bloccato
                       set bloccato to true
                       exit perform
                    end-if
                 end-perform
           end-start.
           close rordini mtordini.
           if bloccato
              move "B" to link-stato
           else
              move "A" to link-stato
           end-if.

           goback.
