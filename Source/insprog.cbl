       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      insprog.
       AUTHOR.                          Andrea.
       REMARKS. 
           data una lista di progressivi vado a bloccare tutti i figli
           ed a crearne due (uno LBX uno EXD) attivi
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "progmag.sl". 
           copy "lineseq.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "progmag.fd". 
           copy "lineseq.fd".

       WORKING-STORAGE SECTION.

       78  titolo    value "Inserimento progressivi".
       77  status-progmag   pic xx.  
       77  status-lineseq   pic xx.
       77  wstampa          pic x(256).
       77  prg-ok           pic 9(5) value 0.
       77  prg-esiste       pic 9(5) value 0.
       77  padre-non-esiste pic 9(5) value 0.

       01 como-chiave.
          03 como-cod-articolo   pic 9(6).
          03 como-cod-magazzino  pic x(3).
          03 como-tipo-imballo   pic x(3).
          03 como-peso           pic 9(5)v9(3).

       01  filler   pic 9.
         88 trovato value 1, false 0.       

      ******************************************************************
       PROCEDURE DIVISION.

       MAIN-PRG.
           perform INIT.
           perform OPEN-FILES.
           perform ELABORAZIONE.
           perform CLOSE-FILES.
           perform EXIT-PGM.

      ***---
       INIT.
           move "insprog.csv" to wstampa.

      ***---
       OPEN-FILES.
           open input lineseq.
           open i-o   progmag.

      ***---
       ELABORAZIONE.
           perform until 1 = 2
              read lineseq next at end exit perform end-read
              unstring line-riga delimited by ";"
                  into como-cod-articolo
                       como-peso
                       como-tipo-imballo
              end-unstring
              divide como-peso by 1000 giving como-peso
              move "LBX" to como-cod-magazzino
              perform TROVA-PRG
              if trovato
                 add 1 to prg-esiste
              else
                 move "EXD" to como-cod-magazzino
                 perform TROVA-PRG
                 if trovato
                    add 1 to prg-esiste
                 else
                    move spaces to como-cod-magazzino
                    perform TROVA-PRG
                    if not trovato
                       add 1 to padre-non-esiste
                    else
                       move low-value to prg-chiave
                       move como-cod-articolo to prg-cod-articolo
                       start progmag key >= prg-chiave
                             invalid continue
                         not invalid
                             perform until 1 = 2
                                read progmag next 
                                  at end exit perform 
                                end-read
                                if prg-cod-articolo not = 
                                   como-cod-articolo
                                   exit perform
                                end-if
                                if prg-peso = 0
                                   exit perform cycle
                                end-if
                                set prg-bloccato to true
                                rewrite prg-rec
                             end-perform                  
                             add 1 to prg-ok
                             move "LBX" to como-cod-magazzino
                             perform AGGIUNGI-PRG
                             move "EXD" to como-cod-magazzino
                             perform AGGIUNGI-PRG
                       end-start
                    end-if
                 end-if
              end-if
           end-perform.

      ***---
       TROVA-PRG.                                      
           set trovato to false
           initialize prg-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.
           move como-cod-articolo  to prg-cod-articolo.
           if como-cod-magazzino not = spaces
              move como-cod-magazzino to prg-cod-magazzino
              move como-tipo-imballo  to prg-tipo-imballo
              move como-peso          to prg-peso
           end-if.
           read progmag 
                invalid continue
            not invalid set trovato to true
           end-read.
                    
      ***---
       AGGIUNGI-PRG.
           initialize prg-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.
           move como-cod-articolo  to prg-cod-articolo.
           move como-cod-magazzino to prg-cod-magazzino.
           move como-tipo-imballo  to prg-tipo-imballo.
           move como-peso          to prg-peso.
           accept prg-data-creazione from century-date.
           accept prg-ora-creazione  from time.
           move "INSPROG" to prg-utente-creazione.
           write prg-rec.

      ***---
       CLOSE-FILES.
           close progmag lineseq.

      ***---
       EXIT-PGM.
           display message "ELABORAZIONE TERMINATA"
                    x"0d0a""RIEPILOGO: "     
                    x"0d0a""PROGRESSIVI GIA PRESENTI " prg-esiste
                    x"0d0a""PADRE NON ESISTENTE " padre-non-esiste
                    x"0d0a""IMPORTATI " prg-ok
                     title titolo
           goback.
