       IDENTIFICATION       DIVISION.
       PROGRAM-ID.          prg-costo-mp.
       AUTHOR.              Andrea.
       REMARKS.
           Calcola il valore cel costo medio e lo inserisce in tabella.
           
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "progmag.sl". 
           COPY "articoli.sl".   
           copy "timposte.sl".
           copy "tmarche.sl".

       DATA DIVISION.
       FILE SECTION.
           COPY "progmag.fd".
           COPY "articoli.fd".
           copy "timposte.fd".
           copy "tmarche.fd".

       WORKING-STORAGE      SECTION.
       copy "costo-medio.def".      
       copy "imposte.def".  
       77  status-progmag    pic xx.
       77  status-articoli   pic xx.
       77  status-timposte   pic xx.
       77  status-tmarche    pic xx.

       01  r-inizio              pic x(25).

       77  como-riga       pic x(200).
       77  riga-stampa     pic x(200).
       77  tipo            pic x(20).
       77  costo-mp-z      pic ----.---.--9,99.
       77  prg-peso-z      pic --.--9,99.
       77  art-errato      pic 9(6).
       77  s-prg-costo-mp  pic  S9(9)V9(2).


       01  controllo    pic xx.    
         88 tutto-ok    value "OK".
         88 errori      value "ER".
                                         
       77  filler       pic 9.
         88 RecLocked   value 1, false 0.
       77  filler       pic 9.
         88 RichiamoSchedulato value 1, false 0.
                                 
       77  como-data    pic 9(8).
       77  como-ora     pic 9(8). 
                                         
       77  n-n          pic 9(7) value 0.
       77  n-agg        pic 9(7) value 0.
       77  n-err        pic 9(7) value 0.
       77  n-elab       pic 9(7) value 0.    
 
       77  counter      pic 9(10).
       77  counter2     pic 9(10).
       77  counter-edit pic z(10).

       77  nargs        pic 99 comp-1 value 0.

       LINKAGE SECTION. 
       77  link-user             pic x(10).
       77  link-result           pic 9.
       77  scr-oper-handle       handle of window.

       PROCEDURE DIVISION USING link-user      
                                link-result    
                                scr-oper-handle.
                
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
           move 1 to link-result.

           |RIPULISCO LA SCREEN DAL CONTATORE
           display "                          "
              upon scr-oper-handle at column 34
                                        line 25
           display "                          "
              upon scr-oper-handle at column 30
                                        line 26
           ||||||||
           .

      ***---
       OPEN-FILES.    
           open i-o progmag.
           open input articoli tmarche timposte.

      ***---
       ELABORAZIONE.                 
           accept imp-data from century-date.
           start timposte key <= imp-chiave
                 invalid continue
             not invalid
                 read timposte previous
           end-start.    
           move low-value to prg-rec. 
           move 0 to art-codice art-errato.
           start progmag key >= prg-chiave.

           perform until 1 = 2              

              read progmag next at end exit perform end-read
              add 1 to n-elab

              if prg-cod-articolo = art-errato
                 exit perform cycle
              end-if

              move prg-costo-mp to s-prg-costo-mp

              if prg-cod-articolo not = art-codice
                 move 0 to link-result
                 move prg-cod-articolo to art-codice
                 read articoli no lock
                      invalid                     
                     display message "ARTICOLO: " art-codice 
                                     " NON TROVATO"
                              title "GESLUX - Ricalcolo costo mp"
                               icon 2
                      move prg-cod-articolo to art-errato
                      set errori to true
                      exit perform cycle
                 end-read
              end-if
              add 1 to n-n    

              add 1 to counter
              add 1 to counter2
              if counter2 = 100
                 move counter to counter-edit
                 display counter-edit
                    upon scr-oper-handle at column 34
                                              line 25
                 move 0 to counter2
                 if counter = 100
                    display "COSTO-MP COUNTER"
                       upon scr-oper-handle at column 30
                                                 line 26
                 end-if
              end-if

              perform CALCOLA-COSTO-MP-COMPLETO    
              add 0,005 to costo-mp giving costo-mp-2dec
              move costo-mp-2dec to prg-costo-mp costo-mp-z

              accept prg-data-ultima-modifica from century-date
              accept prg-ora-ultima-modifica  from time
              move link-user to prg-utente-ultima-modifica

              rewrite prg-rec
           end-perform.

      ***---
       CLOSE-FILES.
           close progmag articoli tmarche timposte.    

      ***---
       EXIT-PGM.
           |RIPULISCO LA SCREEN DAL CONTATORE
           display "                          "
              upon scr-oper-handle at column 34
                                        line 25
           display "                          "
              upon scr-oper-handle at column 30
                                        line 26
           ||||||||
           goback.

      ***---
       PARAGRAFO-COPY.
           copy "setta-inizio-riga.cpy".
           copy "costo-medio.cpy".
           copy "recupero-anagrafica.cpy".
