       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      ordf-ord.
       AUTHOR.                          Andrea.
       REMARKS. Per gli ordini passati in linkage da crea-ordfor
                riordina le righe per descrizione articolo
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "rordforn.sl".
           copy "articoli.sl".

       SELECT des-rordforn
           ASSIGN       TO path-des-rordforn
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-des-rordforn
           RECORD KEY   IS drof-chiave.

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "rordforn.fd".
           copy "articoli.fd".

       FD  des-rordforn.
       01 drof-rec.
          05 drof-chiave.    
             10 drof-chiave-testa.
                15 drof-anno           pic  9(4).
                15 drof-numero         pic  9(8).
             10 drof-art-descrizione   pic x(100).
             10 drof-cod-articolo      pic 9(6).
             10 drof-prog              pic 9(6).
          05 drof-rof-dati             pic x(2000).

       WORKING-STORAGE SECTION.

      * COSTANTI
       78  titolo value "Ordinamento automatici".

      * FILE STATUS
       77  status-rordforn      pic xx.  
       77  status-articoli      pic xx.
       77  status-des-rordforn  pic xx.

      * VARIABILI 
       77  como-data pic 9(8).
       77  como-ora  pic 9(8).
       77  s-numero  pic 9(8) value 0.

      * FLAGS
       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".

       LINKAGE SECTION.    
       77  link-anno            pic 9(4).        
       77  link-primo           pic 9(8).
       77  link-ultimo          pic 9(8).

      ******************************************************************
       PROCEDURE DIVISION USING link-anno link-primo link-ultimo.

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
           set tutto-ok to true.
           accept path-des-rordforn from environment "PATH_ST".
           accept como-data from century-date.
           accept como-ora  from time.                    
           inspect path-des-rordforn  
                   replacing trailing spaces by low-value.
           string path-des-rordforn delimited low-value
                  "des-rof_"        delimited size
                  como-data         delimited size
                  "_"               delimited size
                  como-ora          delimited size
             into path-des-rordforn
           end-string.
           inspect path-des-rordforn  
                   replacing trailing low-value by spaces.

      ***---
       OPEN-FILES.               
           open i-o    rordforn.
           open input  articoli.
           open output des-rordforn.
           close       des-rordforn.
           open i-o    des-rordforn.

      ***---
       ELABORAZIONE.
           move low-value  to rof-rec.
           move link-anno  to rof-anno.
           move link-primo to rof-numero.
           start rordforn key >= rof-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rordforn next at end exit perform end-read
                    if rof-numero > link-ultimo
                       exit perform
                    end-if
                    move rof-dati to drof-rof-dati
                    move rof-cod-articolo to art-codice
                    read articoli no lock
                         invalid move "NON TROVATO" to art-descrizione
                    end-read         
                    move rof-chiave-testa to drof-chiave-testa
                    move art-descrizione  to drof-art-descrizione
                    move rof-cod-articolo to drof-cod-articolo
                    move 0 to drof-prog
                    perform until 1 = 2
                       add 1 to drof-prog
                       write drof-rec
                             invalid continue
                         not invalid exit perform
                       end-write
                    end-perform
                    delete rordforn record
                 end-perform
           end-start.

           move low-value to drof-chiave.
           start des-rordforn key >= drof-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read des-rordforn next at end exit perform end-read
                    if s-numero = 0 or drof-numero not = s-numero  
                       move drof-numero to s-numero
                       move 0 to rof-riga
                    end-if
                    add 1 to rof-riga
                    move drof-chiave-testa to rof-chiave-testa
                    move drof-rof-dati to rof-dati
                    write rof-rec
                 end-perform
           end-start.

      ***---
       CLOSE-FILES.
           close rordforn articoli des-rordforn.
           delete file des-rordforn.

      ***---
       EXIT-PGM.
           goback.
