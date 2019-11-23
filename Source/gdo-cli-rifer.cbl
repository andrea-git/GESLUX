       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      gdo-cli-rifer.
       REMARKS. BATCH impostazione cliente di riferimento (il primo 
           trovato avente lo stesso gruppo GDO) sui gruppi GDO
       SPECIAL-NAMES. decimal-point is comma.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tgrupgdo.sl".
           copy "clienti.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tgrupgdo.fd".
           copy "clienti.fd".

       WORKING-STORAGE SECTION.

      * Status Files GESLUX
       77  status-tgrupgdo   pic X(2).
       77  status-clienti    pic X(2).

       78  titolo            value "Impostazione cliente riferimento".

      ******************************************************************
       PROCEDURE DIVISION.

       MAIN-PRG.
           open i-o tgrupgdo.
           open input clienti.

           move low-value to cli-rec.
           start clienti key >= cli-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read clienti next at end exit perform end-read
                    if cli-tipo-F exit perform end-if
                    if cli-gdo not = spaces
                       move  cli-gdo to gdo-codice
                       read tgrupgdo 
                            invalid continue
                        not invalid              
                            if gdo-cli-rifer = 0 or
                               gdo-cli-rifer = spaces
                               if gdo-tipocli   = cli-tipo
                                  move cli-codice to gdo-cli-rifer
                                  rewrite gdo-rec
                               end-if
                            end-if
                       end-read
                    end-if
                 end-perform
           end-start.
           
           close tgrupgdo clienti.

           goback.
