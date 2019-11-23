       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      vrecup.
       AUTHOR.                          Andrea.
       REMARKS. Visualizza il contenuto del report sui prezzi 
           recuperati da anagrafica durante il ricalcolo nottuno.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "lineseq.sl".

       select rep-recupero
           assign       to path-rep-recupero
           organization is line sequential
           access mode  is sequential
           file status  is status-rep-recupero.

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "lineseq.fd".

       FD  rep-recupero.
       01 riga-recupero             pic x(100).

       WORKING-STORAGE SECTION.
           copy "acugui.def".
           copy "common-excel.def".
       78  titolo                   value "Report recupero prezzi".

       77  status-rep-recupero      pic xx.
       77  status-lineseq           pic xx.

       77  path-rep-recupero        pic x(256).
       77  wstampa                  pic x(256).

       01  controlli       pic xx.
           88 errori       value "ER".
           88 tutto-ok     value "OK".

       01  filler          pic 9.
           88 PrimaVolta   value 1, false 0.
       
       01 r-stampa.
         05 r-tipo         pic x.
         05 r-codice       pic z(6).
         05 filler         pic x(3) value " - ".
         05 r-descrizione  pic x(30).
         05 filler         pic x(2). 
         05 r-marca        pic x(25).
         05 filler         pic x(2).
         05 r-prz          pic ----.--9,99.
         05 filler         pic x(2).
         05 r-mese         pic x.

       LINKAGE SECTION.
           COPY  "COMMON-LINKAGE.DEF".

      ******************************************************************
       PROCEDURE DIVISION USING LK-BLOCKPGM, USER-CODI, LIVELLO-ABIL.

       DECLARATIVES.

      ***---
       REP-RECUPERO-ERR SECTION.
           use after error procedure on rep-recupero.
           set tutto-ok  to true.
           evaluate status-rep-recupero
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File report inesistente"
                          title titolo
                           icon 2
                set errori to true
           end-evaluate.

      ***---
       LINESEQ-ERR SECTION.
           use after error procedure on lineseq.
           set tutto-ok  to true.
           evaluate status-lineseq
           when "93"
                display message "Impossibile procedere."
                  x"0d0a""Chiudere file Excel!!!"
                          title titolo
                           icon 2
                set errori to true
           end-evaluate.

       END DECLARATIVES.

       MAIN-PRG. 
           set PrimaVolta        to true.   
           SET LK-BL-SCRITTURA   TO TRUE.
           MOVE "vrecup"         TO LK-BL-PROG-ID.
           MOVE spaces           TO LK-HND-WIN.
           CALL   "BLOCKPGM"  USING LK-BLOCKPGM.
           CANCEL "BLOCKPGM".

           accept path-rep-recupero from environment "CALMAR_RECUPERO".
           open input rep-recupero.
           if status-rep-recupero = "00"
              move path-rep-recupero to wstampa
              inspect wstampa
                      replacing all ".txt" by ".csv"
              open output lineseq
              if status-lineseq = "00"
                 perform ACCETTA-SEPARATORE
                 perform until 1 = 2
                    read rep-recupero next at end exit perform end-read
                    move riga-recupero to r-stampa
                    if PrimaVolta
                       set PrimaVolta to false
                       initialize line-riga
                       string separatore    delimited size
                              separatore    delimited size
                              riga-recupero delimited size
                              separatore    delimited size
                              into line-riga
                       end-string
                       write line-riga
                       write line-riga from spaces
                       initialize line-riga
                       string "COSTO"       delimited size
                              separatore    delimited size
                              "ARTICOLO"    delimited size
                              separatore    delimited size
                              "DESCRIZIONE" delimited size
                              separatore    delimited size
                              "MARCA"       delimited size
                              separatore    delimited size
                              "PREZZO"      delimited size
                              separatore    delimited size
                              "NEL MESE"    delimited size
                              into line-riga
                       end-string
                       write line-riga
                       |Salto la riga vuota
                       read rep-recupero next
                    else
                       initialize line-riga
                       string r-tipo        delimited size
                              separatore    delimited size
                              r-codice      delimited size
                              separatore    delimited size
                              r-descrizione delimited size
                              separatore    delimited size
                              r-marca       delimited size
                              separatore    delimited size
                              r-prz         delimited size
                              separatore    delimited size
                              r-mese        delimited size
                              into line-riga
                       end-string
                       write line-riga
                    end-if
                 end-perform
                 close rep-recupero
                 close lineseq
                 perform CALL-EXCEL
              end-if
           end-if.

           SET LK-BL-CANCELLAZIONE TO TRUE.
           MOVE "vrecup"       TO LK-BL-PROG-ID.
           CALL "BLOCKPGM"  USING LK-BLOCKPGM 
           goback.

      ***--- 
       PARAGRAFO-COPY.
           copy "common-excel.cpy".
