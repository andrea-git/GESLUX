       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      imp-prodener-p.
       AUTHOR.                          Luciano.
       REMARKS.                         
           Importazione dati fornitori. 

      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "prodener.sl".
           copy "lineseq.sl".

       SELECT rep-listini
           ASSIGN       TO  path-rep-listini
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE  IS SEQUENTIAL
           FILE STATUS  IS STATUS-rep-listini.


      *****************************************************************

       DATA DIVISION.
       FILE SECTION.
           copy "prodener.fd".
           copy "lineseq.fd".

       FD  rep-listini.
       01 rlst-rec         PIC  x(300).


      *****************************************************************

       WORKING-STORAGE SECTION.
      * COPY
           copy "acugui.def".
           copy "common-excel.def".
      * COSTANTI
       78  titolo 
                 value "Importazione prodotti energetici".

       01        pic 9.
           88 rec-locked value 1 false zero.

      * FILE-STATUS
       77  status-prodener         pic xx.
       77  status-lineseq          pic xx.
       77  status-rep-listini      pic xx.
       77  wstampa                 pic x(256).
       77  path-rep-listini        pic x(256).
       77  user-codi               pic x(10).

      * VARIABILI
       01  riga-errore.
           05 filler               pic x(10) value "Record n° ".
           05 re-num-rec           pic z(6).
           05 filler               pic x(2) value ": ".
           05 r-err                pic x(80).

       01  riga-prodener.
           10 rl-codice       PIC  x(10).
           10 rl-descrizione  PIC  x(50).
           10 rl-cpa          PIC  x(4).
           10 rl-nc           PIC  9(8).
           10 rl-taric        PIC  9(2).
           10 rl-dac          PIC  x(4).
           10 rl-invio-edi    PIC  x.
           10 rl-sdoppia-riga PIC  x.
      *
       77  como-data                 pic 9(8).
       77  como-ora                  pic 9(8).
       77  rec-ko                    pic 9(5) value 0.
       77  rec-ok                    pic 9(5) value 0.
       77  num-rec                   pic 9(5) value 0.
       77  num-rec-ed                pic zz.zzz.
       77  counter                   pic 9(10).
       77  counter2                  pic 9(10).
       77  counter-edit              pic z(10).

      * FLAGS
       01  controlli                 pic xx.
         88 tutto-ok                 value "OK".
         88 errori                   value "ER".

       01  filler                    pic 9.
         88 record-ok                value 1, false 0.

       01  filler                    pic 9.
         88 primo-errore             value 1, false 0.

       01  tipo-errore               pic 9.
           88 no-fornitore           value 1.
           88 fornitore-lock         value 2.
      *     88 no-mag-std             value 3.
      *     88 mag-errato             value 4.

      *****************************************************************

       LINKAGE SECTION.
           copy "link-imp-files.def".

       PROCEDURE DIVISION USING  imp-files-linkage.

       DECLARATIVES.
      ***---
       PRODENER-ERR SECTION.
           use after error procedure on PRODENER.
           set tutto-ok  to true.
           evaluate status-PRODENER
           when "35"
                set errori to true
                display message "File [PROEDENER] not found!"
                          title titolo
                           icon 3
           when "39"
                set errori to true
                display message "File [PROEDENER] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[PROEDENER] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "93"
           when "99"
                SET rec-locked  to true
           end-evaluate.


      ***---
       LINESEQ-ERR SECTION.
           use after error procedure on lineseq.
           set tutto-ok  to true.
           evaluate status-lineseq
           when "35"
                set errori to true
                display message "File [LINESEQ] not found!"
                          title titolo
                           icon 3
           when "39"
                set errori to true
                display message "File [LINESEQ] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[LINESEQ] Indexed file corrupt!"
                          title titolo
                           icon 3
           end-evaluate.

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
           move ifs-user to user-codi.
           perform ACCETTA-SEPARATORE.
           set rec-locked to false
           set primo-errore  to true.
           accept como-data  from century-date.
           accept como-ora   from time.
           move 0 to counter counter2.
           set tutto-ok to true.

           move ifs-path  to wstampa.

           initialize path-rep-listini.
           accept  path-rep-listini from environment "PATH_ST".
           inspect path-rep-listini replacing trailing 
                                    spaces by low-value.
           string  path-rep-listini delimited low-value
                   "REP_IMP_PRODENER"    delimited size
                   "_"                   delimited size
                   como-data             delimited size
                   "_"                   delimited size
                   como-ora              delimited size
                   ".txt"                delimited size
                   into path-rep-listini
           end-string.

      ***---
       OPEN-FILES.
           open i-o prodener.
           if tutto-ok
              open input lineseq 
              if tutto-ok
      *    tolgo l'intestazione
                 initialize line-riga
                 read LINESEQ next
                    at end
                       continue
                 end-read
              end-if
           end-if.

           if errori 
              goback 
           end-if.

      ***---
       ELABORAZIONE.
           set tutto-ok   to true
           accept como-data from century-date
           accept como-ora  from time

           move 0 to num-rec.
           perform until 1 = 2 
              perform DISPLAY-UPON-SCREEN

              set record-ok to true
              initialize tipo-errore

              initialize line-riga
              read lineseq next 
                 at end 
                    exit perform 
              end-read
              if line-riga = spaces    
                 exit perform 
              end-if
              unstring line-riga delimited by separatore
                       into rl-codice
                            rl-descrizione
                            rl-cpa
                            rl-nc
                            rl-taric
                            rl-dac
                            rl-invio-edi
                            rl-sdoppia-riga
              end-unstring
              perform TRATTA-RECORD

           end-perform.

           if rec-ko > 0
              write rlst-rec from spaces
              move all "-" to rlst-rec
              write rlst-rec 
              write rlst-rec from spaces
              move rec-ko to num-rec-ed
              string "Totale righe scartate: " delimited size
                     num-rec-ed                delimited size
                     into rlst-rec
              end-string
              write rlst-rec  after 2
              close REP-LISTINI
           end-if.

           if rec-ko > 0
              display message "Operazione terminata con errori!"
                      x"0d0a""====================="
                      x"0d0a""RIEPILOGO:"
                      x"0d0a"
                     x"0d0a""Totale righe trattate " num-rec, " di cui:"
                      x"0d0a"" - " rec-ok, " importate"
                      x"0d0a"" - " rec-ko, " errate"
                      x"0d0a""Sarà visualizzato report riepilogativo..."
                      title titolo
                       icon 2
           else
              display message "Operazione conclusa con sucesso!"
                      x"0d0a""====================="
                      x"0d0a""RIEPILOGO:"
                      x"0d0a"
                      x"0d0a""Totale righe trattate " num-rec, 
                      title titolo
           end-if.


      ***---
       CLOSE-FILES.
           close prodener
                 lineseq 
           if rec-ko > 0 
              call   "spooler-a" using "A", path-rep-listini, "O"
              cancel "spooler-a"
              delete file rep-listini
           end-if.
              

      ***---
       EXIT-PGM.
           goback.

      ***---
       TRATTA-RECORD.
           set tutto-ok   to true
           add 1 to num-rec.

           perform VALIDA-PRODENER

           if tutto-ok
              add 1 to rec-ok

              move rl-descrizione  to pen-descrizione 
              move rl-cpa          to pen-cpa         
              move rl-nc           to pen-nc          
              move rl-taric        to pen-taric       
              move rl-dac          to pen-dac         

              if rl-invio-edi = "1" or rl-invio-edi = "S"
                 set pen-si-invio-edi to true
              else
                 set pen-si-invio-edi to false
              end-if

              if rl-sdoppia-riga = "1" or rl-sdoppia-riga = "S"
                 set pen-si-sdoppia-riga to true
              else
                 set pen-si-sdoppia-riga to false
              end-if

              rewrite pen-rec
                 invalid
                    continue
              end-rewrite
           end-if.
           unlock prodener all records.            

      ***---
       SCRIVI-ERRORI.
           if primo-errore
              set primo-errore  to false
              open output rep-listini

              move "Importazione Prodotti energetici: riepilogo errori"  
                                   to rlst-rec
              call "c$justify" using rlst-rec, "C"
              write rlst-rec
              move all "-"   to rlst-rec
              write rlst-rec
           end-if

           move num-rec   to re-num-rec

           evaluate true
      *     when no-fornitore
      *          move "Destinto inesistente"                   to r-err
           when fornitore-lock
                move "Prodotto energetico in uso su un altro termanile"  
                          to r-err
           end-evaluate

           write rlst-rec from riga-errore.

      ***---
       VALIDA-PRODENER.
      *    controllo l'esistenza dell'articolo
           move rl-codice    to pen-codice
           read prodener no lock
              invalid
                 accept pen-data-creazione from century-date
                 accept pen-ora-creazione  from time
                 accept pen-data-ultima-modifica from century-date
                 accept pen-ora-ultima-modifica from time
                 move "BOSS"          to pen-utente-creazione
                                         pen-utente-ultima-modifica
                 write pen-rec                 
           end-read.

      *    controllo che il fornitore non sia lockato
           if tutto-ok
              set Rec-Locked       to false
              read prodener with lock
                 invalid
                    continue
              end-read
      
              if rec-locked
                 set errori           to true
                 add 1                to rec-ko
                 set fornitore-lock   to true
                 perform SCRIVI-ERRORI
              end-if
           end-if.

      ***---
           copy "common-excel.cpy".
           copy "imp-files-procedure.cpy".

