       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      assimposte.
       AUTHOR.                          Andrea.
       REMARKS. Controlla che l'assoggettamento imposte sugli 
                articoli rispetti quanto indicato nella tabella 
                dei codici dognali. Invio mail in caso contrario.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tsetinvio.sl".
           copy "tnomen.sl".
           copy "articoli.sl".
           copy "lineseq.sl".
           copy "lineseq-mail.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tsetinvio.fd".
           copy "tnomen.fd".
           copy "articoli.fd".
           copy "lineseq.fd".     
           copy "lineseq-mail.fd".

       WORKING-STORAGE SECTION.
           copy "mail.def".

       78  titolo                value"Check assoggettamento imposte".
       77  status-tnomen         pic xx.
       77  status-articoli       pic xx.
       77  status-lineseq        pic xx.
       77  status-tsetinvio      pic xx.
       77  status-lineseq-mail   pic xx.
       77  wstampa               pic x(256).
       77  path-lineseq-mail     pic x(256).

       77  oper                  pic x(4).
       77  r-perce               pic zz9,99.
       77  r-perce1              pic zz9,99.

       77  como-data             pic 9(8).
       77  como-ora              pic 9(8).

       01  filler                pic 9.
           88 prima-volta        value 1 false 0.

       01  controlli             pic xx.
           88 tutto-ok           value "OK".
           88 errori             value "ER". 

      ******************************************************************
       PROCEDURE DIVISION.
       DECLARATIVES.    
       copy "mail-decl.cpy".
       END DECLARATIVES.

      ***---
       MAIN-PRG.
           perform INIT.
           perform OPEN-FILES.
           if tutto-ok
              perform ELABORAZIONE
              perform CLOSE-FILES
           end-if.
           perform EXIT-PGM.

      ***---
       INIT.                             
           set tutto-ok     to true.
           set prima-volta  to true.
           accept wstampa   from environment "PATH_ST".
           accept como-data from century-date.
           accept como-ora  from time.
           inspect wstampa  replacing trailing spaces by low-value.
           string  wstampa       delimited low-value
                   "ASSIMPOSTE_" delimited size
                   como-data     delimited size
                   "_"           delimited size
                   como-ora      delimited size
                   ".csv"        delimited size
                   into wstampa
           end-string.

      ***---
       OPEN-FILES.
           open input tnomen articoli.

      ***---
       ELABORAZIONE.
           move low-value to art-rec.
           start articoli key >= art-codice invalid continue end-start.
           perform until 1 = 2
              read articoli next at end exit perform end-read
              if art-si-imposte
                 move art-cod-doganale to nom-codice
                 read tnomen no lock invalid continue end-read
                 set errori to true
                 evaluate true
                 when nom-uguale
                      if art-perce-imposte = nom-perce
                         set tutto-ok to true
                      end-if
                 when nom-maggiore
                      if art-perce-imposte > nom-perce
                         set tutto-ok to true
                      end-if
                 when nom-minore
                      if art-perce-imposte < nom-perce
                         set tutto-ok to true
                      end-if
                 when nom-mag-u
                      if art-perce-imposte >= nom-perce
                         set tutto-ok to true
                      end-if
                 when nom-min-u
                      if art-perce-imposte <= nom-perce
                         set tutto-ok to true
                      end-if
                 end-evaluate
                 if errori
                    perform SCRIVI-ERRORE
                 end-if
              end-if
           end-perform.

           perform INVIO-MAIL.

      ***---
       SCRIVI-ERRORE.
           if prima-volta
              set prima-volta to false
              open output lineseq
              string "CODICE"          delimited size
                     ";"               delimited size
                     "DESCRIZIONE"     delimited size
                     ";"               delimited size
                     "COD. DOGANALE"   delimited size
                     ";"               delimited size
                     "ASSOGGETTAMENTO" delimited size
                     ";"               delimited size
                     "VALORE CORRETTO" delimited size
                     into line-riga
              end-string
              write line-riga
              write line-riga from spaces
           end-if.
           evaluate true
           when nom-maggiore move "* >"  to oper
           when nom-minore   move "* >"  to oper
           when nom-uguale   move "* ="  to oper
           when nom-mag-u    move "* >=" to oper
           when nom-min-u    move "* <=" to oper
           end-evaluate.
           move art-perce-imposte to r-perce.
           move nom-perce         to r-perce1.

           string art-codice       delimited size
                  ";"              delimited size
                  art-descrizione  delimited size
                  ";"              delimited size
                  art-cod-doganale delimited size
                  ";"              delimited size
                  r-perce          delimited size
                  ";"              delimited size
                  oper             delimited size
                  " "              delimited size
                  r-perce1         delimited size
                  into line-riga
           end-string.
           write line-riga.

      ***---
       INVIO-MAIL.
           if errori 
              close lineseq
              move "DIAGNOSTICA ASSOGGETTAMENTO ARTICOLI - ATTENZIONE"
                to LinkSubject
              move "In allegato discordanze riscontrate" 
                to LinkBody
              move wstampa to LinkAttach
           else
              move "RICALCOLO NOTTURNO EFFETUATO - OK" 
                to LinkSubject
           end-if.
           
           move "gds@lubex.it" to LinkAddress.
                               
           move "assimposte" to NomeProgramma.

           set errori to true.
           move 5 to tentativi-mail.
           perform CICLO-SEND-MAIL.

      ***---
       AFTER-SEND-MAIL.

      ***---
       CLOSE-FILES.
           close articoli tnomen.

      ***---
       EXIT-PGM.
           goback.

      ***---
       PARAGRAFO-COPY.
           copy "mail.cpy".
