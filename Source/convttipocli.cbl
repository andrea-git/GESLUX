       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      convttipocli.

       ENVIRONMENT          DIVISION.
       CONFIGURATION        SECTION.
       SPECIAL-NAMES. decimal-point is comma.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "ttipocli.sl".
           
       SELECT ttipocli-old
           ASSIGN       TO  "ttipocli-old"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS old-STATUS-ttipocli
           RECORD KEY   IS old-tcl-chiave.


      *****************************************************************
       DATA DIVISION.
       FILE SECTION.          
           copy "ttipocli.fd".

      *
      *
      *
      *0
      *
       FD  ttipocli-old.
       01 old-tcl-rec.
           05 old-tcl-chiave.
               10 old-tcl-codice       PIC  X(2).
           05 old-tcl-dati.
               10 old-tcl-descrizione  PIC  X(30).
               10 old-tcl-agente       PIC  x.
                   88 old-tcl-agente-si VALUE IS "S". 
                   88 old-tcl-agente-no VALUE IS "N". 
                   88 old-tcl-agente-opz VALUE IS "O". 
               10 old-tcl-gdo          PIC  x.
                   88 old-tcl-gdo-si VALUE IS "S". 
                   88 old-tcl-gdo-no VALUE IS "N". 
                   88 old-tcl-gdo-opz VALUE IS "O". 
               10 old-tcl-tipologia-tratt-imposte  PIC  x.
                   88 ttipocli-standard VALUE IS "S". 
                   88 ttipocli-gdo VALUE IS "G". 
                   88 ttipocli-estero-EX VALUE IS "E". 
               10 old-tcl-gestione-fido            PIC  x.
                   88 old-tcl-si-gestione-fido VALUE IS "S". 
                   88 old-tcl-no-gestione-fido VALUE IS "N". 
               10 old-tcl-dati-comuni.
                   15 old-tcl-data-creazione           PIC  9(8).
                   15 old-tcl-ora-creazione            PIC  9(8).
                   15 old-tcl-utente-creazione         PIC  X(10).
                   15 old-tcl-data-ultima-modifica     PIC  9(8).
                   15 old-tcl-ora-ultima-modifica      PIC  9(8).
                   15 old-tcl-utente-ultima-modifica   PIC  X(10).
               10 old-tcl-vuoti.
                   15 old-tcl-recupero-listino         PIC  x.
                       88 old-tcl-si-recupero VALUE IS "S". 
                       88 old-tcl-no-recupero VALUE IS "N". 
                   15 old-tcl-gest-plus    PIC  x.
                       88 old-tcl-si-gest-plus VALUE IS "S". 
                       88 old-tcl-no-gest-plus VALUE IS "N". 
                   15 old-tcl-piombo       PIC  x.
                       88 old-tcl-si-piombo VALUE IS "S". 
                       88 old-tcl-no-piombo VALUE IS "N". 
                   15 old-tcl-serie-bolle  PIC  9.
                   15 old-tcl-num-vuoto-1  PIC  9(11).
                   15 old-tcl-num-vuoto-2  PIC  9(15).
                   15 old-tcl-num-vuoto-3  PIC  9(15).
                   15 old-tcl-impegnato    PIC  x.
                       88 old-tcl-imp-GDO VALUE IS "G". 
                       88 old-tcl-imp-trad VALUE IS "T". 
                   15 old-tcl-manuale      PIC  x.
                       88 old-tcl-manuale-si VALUE IS "S". 
                       88 old-tcl-manuale-no VALUE IS "N". 
                   15 old-tcl-bloc-auto    PIC  x(1).
                       88 old-tcl-bloc-auto-si VALUE IS "S". 
                       88 old-tcl-bloc-auto-no VALUE IS "N". 
                   15 old-tcl-brogliaccio  PIC  X(1).
                       88 old-tcl-brogliaccio-GDO VALUE IS "G". 
                       88 old-tcl-brogliaccio-NORMALE VALUE IS "N". 
      *(( XFD NAME = old-tcl-brogliaccio_1 ))
                   15 old-tcl-evasione     PIC  X(1).
                       88 old-tcl-evasione-GDO VALUE IS "G". 
                       88 old-tcl-evasione-TRAD VALUE IS "T". 
                       88 old-tcl-evasione-ESTERO VALUE IS "E". 
      *(( XFD NAME = old-tcl-brogliaccio_1_ ))
                   15 old-tcl-stampante    PIC  X(50).
                   15 old-tcl-stampa-brogm PIC  x.
                       88 old-tcl-stampa-brogm-si VALUE IS "S". 
                       88 old-tcl-stampa-brogm-no VALUE IS "N" " ". 
      *(( XFD NAME = old-tcl-brogliaccio_1_ ))
      *(( XFD NAME = old-tcl-brogliaccio_1_ ))
                   15 FILLER           PIC  X(4).

       WORKING-STORAGE SECTION.
           copy "acucobol.def".
           copy "acugui.def".
       77  status-ttipocli      pic X(2).
       77  old-STATUS-ttipocli  pic X(2).

       77  cont                 PIC 9(6).
       77  cont-ed              PIC zzz.zz9.
       77  scelta               pic 9.

      ******************************************************************
       PROCEDURE DIVISION.     
           COPY "DECLXER1".
                     ttipocli
                     ttipocli-old
                     .
           COPY "DECLXER2".

       MAIN-PRG.           
           display message box 
                          "Confermi la conversione del file ttipocli?"
                          x"0D0A"
                          "Prima di procedere con la conversione"
                          x"0D0A"
                          "rinominare il file "
                          x"22"
                          "ttipocli"
                          x"22"
                          " in "
                          x"22"
                          "ttipocli-old"
                          x"22"
                          "."
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
           move 0 to cont

           open input  ttipocli-old.
           open output ttipocli.


           move low-value to old-tcl-chiave.

           start ttipocli-old key >= old-tcl-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read ttipocli-old next at end exit perform end-read

                    perform MUOVI-RECORD

                 end-perform
           end-start.

           close ttipocli
                 ttipocli-old.

           move cont   to cont-ed
           display message box "Convertiti " cont-ed " record.".


      ***---
       MUOVI-RECORD.
           add 1 to cont.
           initialize tcl-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.

           move old-tcl-codice                  to tcl-codice                 
           move old-tcl-descrizione             to tcl-descrizione            
           move old-tcl-agente                  to tcl-agente                 
           move old-tcl-gdo                     to tcl-gdo                    
           move old-tcl-tipologia-tratt-imposte 
             to tcl-tipologia-tratt-imposte  
           move old-tcl-gestione-fido           to tcl-gestione-fido              
           move old-tcl-dati-comuni             to tcl-dati-comuni            
           move old-tcl-recupero-listino        to tcl-recupero-listino       
           move old-tcl-gest-plus               to tcl-gest-plus              
           move old-tcl-piombo                  to tcl-piombo                 
           move old-tcl-serie-bolle             to tcl-serie-bolle            
           move old-tcl-num-vuoto-1             to tcl-num-vuoto-1            
           move old-tcl-num-vuoto-2             to tcl-num-vuoto-2            
           move old-tcl-num-vuoto-3             to tcl-num-vuoto-3            
           move old-tcl-impegnato               to tcl-impegnato              
           move old-tcl-manuale                 to tcl-manuale                
           move old-tcl-bloc-auto               to tcl-bloc-auto              
           move old-tcl-brogliaccio             to tcl-brogliaccio            
           move old-tcl-evasione                to tcl-evasione               
           move old-tcl-stampante               to tcl-stampante              
           move old-tcl-stampa-brogm            to tcl-stampa-brogm.

           write tcl-rec.
