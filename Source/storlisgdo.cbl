       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      storlisgdo.
       REMARKS. Storicizzazione listini GDO.
                Data una lista csv, prende le righe presenti e le 
                trasferisce su un altro file.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "listini.sl". 
           copy "lineseq.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "listini.fd".
           copy "lineseq.fd".

       WORKING-STORAGE SECTION.
           COPY "acucobol.def".
           copy "comune.def".
           copy "fonts.def".
           copy "link-geslock.def".

       78  titolo    value "Storicizzazione listini GDO".

       77  counter          pic 9(10).
       77  counter-edit     pic zzz.zzz.zz9.

       77  status-listini   pic xx.  
       77  status-lineseq   pic xx.
       77  wstampa          pic x(256).
       77  form1-handle     handle of window.

       77  r-gdo            pic x(5).
       77  r-art            pic x(6).
       77  como-articolo    pic 9(6).

      ******************************************************************
       PROCEDURE DIVISION.

       DECLARATIVES.
       LISTINI-ERR SECTION.
           use after error procedure on listini.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-listini 
           when "39"
                set errori to true
                display message "File [LISTINI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[LISTINI] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [LISTINI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.    
       END DECLARATIVES.

       MAIN-PRG.
           perform INIT.
           perform OPEN-FILES.
           if tutto-ok
              perform ELABORAZIONE
              perform EXIT-PGM
           end-if.

      ***---
       INIT.                           
           set RecLocked to false.
           set tutto-ok  to true.      

      ***---
       OPEN-FILES.
           perform OPEN-IO-LISTINI.
           if tutto-ok     
              move "gdoart.csv" to wstampa
              open input lineseq
           else
              goback
           end-if.

      ***---
       OPEN-IO-LISTINI.
           string   "Il file dei listini GDO" 
             x"0d0a""risulta in uso su altro terminale."
             x"0d0a""Questo comporta l'impossibilità ad"
             x"0d0a""aggiornare gli articoli." delimited size
                 into geslock-messaggio
           end-string.

           set tutto-ok   to true.
           move "LISTINI" to geslock-nome-file.
           perform until 1 = 2
              set RecLocked to false
              open i-o listini allowing readers
              if not RecLocked exit perform end-if
              move 1 to geslock-v-riprova
              move 0 to geslock-v-ignora
              move 1 to geslock-v-termina
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova continue
              when termina set errori to true
                           display message box "Operazione interrotta!"
                                   title titolo
                                   icon 2
                           exit perform
              end-evaluate
           end-perform.              
      
      ***---
       ELABORAZIONE.
           set ElaborazioneXX to true.
           perform ACCESSOXX.
           perform until 1 = 2
              read lineseq next at end exit perform end-read
              unstring line-riga delimited by ";" into r-gdo r-art
              call "C$JUSTIFY" using r-art, "R"
              inspect r-art replacing leading x"20" by x"30"
              move r-art to como-articolo
              move low-value     to lst-rec
              move r-gdo         to lst-gdo
              move como-articolo to lst-articolo
              start listini key >= lst-k-articolo
                    invalid continue
                not invalid
                    perform until 1 = 2
                       read listini next at end exit perform end-read
                       if lst-gdo      not = r-gdo or
                          lst-articolo not = como-articolo
                          exit perform
                       end-if
                       add 1 to counter
                       move counter to counter-edit
                       display counter-edit upon form1-handle 
                               at column 15,00
                                    line  5,00
                    end-perform
              end-start
           end-perform.
           perform DESTROYXX.

      ***---
       EXIT-PGM.                
           close listini lineseq.
           goback.

           copy "accessoxx.cpy".                                                 
