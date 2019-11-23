       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      val-redi-cf.
       ENVIRONMENT          DIVISION.
       CONFIGURATION        SECTION.
       SPECIAL-NAMES. decimal-point is comma.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "clienti.sl".
           copy "tmovmag.sl".
           copy "redi.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "clienti.fd".
           copy "tmovmag.fd".
           copy "redi.fd".

       WORKING-STORAGE SECTION.
           copy "acucobol.def".
           copy "acugui.def".
       77  status-clienti       pic X(2).
       77  status-tmovmag       pic X(2).
       77  status-redi          pic X(2).

       77  CONT                 PIC 9(4).
       77  CONT-ED              PIC Z(4).
       77  scelta               pic 9.

      ******************************************************************
       PROCEDURE DIVISION.     
           COPY "DECLXER1".
                     redi
                     tmovmag
                     clienti
                     .
           COPY "DECLXER2".

       MAIN-PRG.           
           display message box  "Confermi la valorizzazione dei "
                                "codici fiscali sul file REDI?"
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
           move zero   to cont

           open input clienti.
           open input tmovmag
           open i-o redi


           move low-value to redi-chiave.

           start redi key not less redi-chiave
              invalid
                 continue
              not invalid
                 perform until 1 = 2
                    read redi next
                       at end
                          exit perform
                    end-read

                    add 1    to cont

                    if redi-dest = space
                       perform VAL-RECORD
                    end-if
                 end-perform
           end-start.

           close clienti
                 tmovmag
                 redi.

           move cont   to cont-ed
           display message box "Controllati " cont-ed " record.".


      ***---
       VAL-RECORD.
           move redi-tmo-chiave to tmo-chiave

           read tmovmag
              invalid
                 initialize tmo-cod-clifor
           end-read.

           move tmo-tipo  to cli-tipo-CF
           move tmo-cod-clifor  to cli-codice

           read clienti
              invalid
                 initialize cli-piva
                            cli-codfis
           end-read.

           if cli-piva not = space
              move cli-piva           to redi-dest
           else
              move cli-codfis         to redi-dest
           end-if.

           rewrite redi-rec
              invalid
                 continue
           end-rewrite.


