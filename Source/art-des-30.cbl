       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      art-des-30.
       REMARKS. Controlla quali codici hanno la descrizione > 30 crt.
                Avendo esaurito lo spazio in bolla la descrizione
                è stata riportata da 29 a 30 caratteri. Genero un report
                per allineare le descrizioni.

       SPECIAL-NAMES. decimal-point is comma.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "articoli.sl".
           copy "lineseq.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "articoli.fd".
           copy "lineseq.fd".

       WORKING-STORAGE SECTION.

      * Status Files GESLUX
       77  status-articoli   pic X(2).
       77  status-lineseq    pic X(2).
       77  wstampa           pic x(256).
       77  filler            pic 9.
         88 trovato          value 1, false 0.
       77  como-data         pic 9(8).
       77  como-ora          pic 9(8).
       77  cmd-lancio        pic x(200) value spaces.
       77  status-run        signed-short.
       77  counter           pic 9(5) value 0.
       78  titolo            value "Controllo descrizione 30 caratteri".

      ******************************************************************
       PROCEDURE DIVISION.

       MAIN-PRG.
           set trovato to false.
           accept como-data from century-date.
           accept como-ora  from time.

           initialize wstampa.
           accept  wstampa from environment "PATH_ST".
           inspect wstampa replacing trailing spaces by low-value.
           string  wstampa delimited low-value
                   "DESCRIZIONE_ARTICOLO_30_" delimited size
                   "_________"                delimited size
                   como-data                  delimited size
                   "_"                        delimited size
                   como-ora                   delimited size
                   ".txt"                     delimited size
                   into wstampa
           end-string.
           open output lineseq.
           open  input articoli.

           move low-value to art-codice.
           start articoli key >= art-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read articoli next at end exit perform end-read
                    if art-descrizione2 not = spaces
                       set trovato to true
                       initialize line-riga
                       string art-stato        delimited size
                              " "              delimited size
                              art-codice       delimited size
                              " - "            delimited size
                              art-descrizione1 delimited size
                              " ** "           delimited size
                              art-descrizione2 delimited size
                         into line-riga
                      end-string       
                      add 1 to counter
                      write line-riga
                   end-if
                 end-perform
           end-start.            
           close articoli.
           if trovato
              initialize line-riga
              string "TOTALE: " counter into line-riga
              write line-riga
              close lineseq
              inspect wstampa replacing trailing spaces by low-value
              string "START "      delimited size
                    x"22222022"    delimited size
                      wstampa      delimited low-value
                    x"22"          delimited size
               into cmd-lancio
              end-string
              call "C$SYSTEM" using cmd-lancio, 225
                             giving status-run
              if status-run = -1
                 call "C$SYSTEM" using cmd-lancio, 64
                                giving status-run
              end-if
           else
              close lineseq
              delete file lineseq
           end-if.

           goback.
