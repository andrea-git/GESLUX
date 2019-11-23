       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      SHI-convparamshi.
       REMARKS.  
       ENVIRONMENT          DIVISION.
       CONFIGURATION        SECTION.
       SPECIAL-NAMES. decimal-point is comma.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "paramshi.sl".
       SELECT paramSHI-old
           ASSIGN       TO  "paramSHI-old"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-paramshi-old
           RECORD KEY   IS old-shi-chiave OF PARAMSHI-old.

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "paramshi.fd".
       FD  paramSHI-old.
       01 old-shi-rec.
           05 old-shi-chiave.
               10 old-shi-codice       PIC  X(2).
           05 old-shi-dati.
               10 old-shi-cod-SHI      PIC  x(15).
               10 old-shi-mag-SHI      PIC  x(3).
               10 old-shi-nome-semaforo            PIC  x(50).
               10 old-shi-path-report-server       PIC  x(256).
               10 old-shi-path-report-client       PIC  x(256).
      *(( XFD NAME = pae-cap-imp-trubut ))
               10 old-shi-dati-exp.
                   15 old-shi-path-exp     PIC  x(256).
                   15 old-shi-path-backup-exp          PIC  x(256).
                   15 old-shi-file-articoli            PIC  x(50).
                   15 old-shi-file-ean     PIC  x(50).
                   15 old-shi-file-prodener            PIC  x(50).
                   15 old-shi-file-vettori PIC  x(50).
                   15 old-shi-file-fornitori           PIC  x(50).
                   15 old-shi-file-tordini PIC  x(50).
                   15 old-shi-file-rordini PIC  x(50).
                   15 old-shi-file-note-ordini         PIC  x(50).
                   15 old-shi-file-classi  PIC  x(50).
                   15 old-shi-file-vuoto-exp-1         PIC  x(50).
                   15 old-shi-file-vuoto-exp-2         PIC  x(50).
                   15 old-shi-file-vuoto-exp-3         PIC  x(50).
                   15 old-shi-file-vuoto-exp-4         PIC  x(50).
                   15 old-shi-file-vuoto-exp-5         PIC  x(50).
                   15 old-shi-ftp-exp.
                       20 old-shi-sito-exp     PIC  x(100).
                       20 old-shi-user-exp     PIC  x(50).
                       20 old-shi-pwd-exp      PIC  x(50).
                       20 old-shi-path-remota-exp          PIC  x(100).
               10 old-shi-dati-import.
                   15 old-shi-path-imp     PIC  x(256).
                   15 old-shi-path-backup-imp          PIC  x(256).
                   15 old-shi-file-articoli-imp        PIC  x(50).
                   15 old-shi-file-tordini-imp         PIC  x(50).
                   15 old-shi-file-rordini-imp         PIC  x(50).
                   15 old-shi-file-vuoto-imp-1         PIC  x(50).
      *(( XFD NAME = old-shi-file-vuoto-exp ))
                   15 old-shi-file-vuoto-imp-2         PIC  x(50).
      *(( XFD NAME = old-shi-file-vuoto-exp ))
                   15 old-shi-file-vuoto-imp-3         PIC  x(50).
      *(( XFD NAME = old-shi-file-vuoto-exp ))
                   15 old-shi-file-vuoto-imp-4         PIC  x(50).
      *(( XFD NAME = old-shi-file-vuoto-exp ))
                   15 old-shi-file-vuoto-imp-5         PIC  x(50).
                   15 old-shi-ftp-imp.
                       20 old-shi-sito-imp     PIC  x(100).
                       20 old-shi-user-imp     PIC  x(50).
                       20 old-shi-pwd-imp      PIC  x(50).
                       20 old-shi-path-remota-imp          PIC  x(100).
               10 old-shi-dati-comuni.
                   15 old-shi-data-creazione           PIC  9(8).
                   15 old-shi-ora-creazione            PIC  9(8).
                   15 old-shi-utente-creazione         PIC  X(10).
                   15 old-shi-data-ultima-modifica     PIC  9(8).
                   15 old-shi-ora-ultima-modifica      PIC  9(8).
                   15 old-shi-utente-ultima-modifica   PIC  X(10).
               10 old-shi-vuoti.
                   15 old-shi-num-vuoto-1  PIC  9(18).
                   15 old-shi-num-vuoto-2  PIC  9(18).
                   15 old-shi-num-vuoto-3  PIC  9(18).
                   15 old-shi-alfa-vuoto   PIC  x(5000).

       WORKING-STORAGE SECTION.
           copy "acucobol.def".
           copy "acugui.def".
       77  status-paramshi      pic X(2).
       77  status-paramshi-old  pic X(2).

       77  CONT                 PIC 9(4).
       77  CONT-ED              PIC Z(4).
       77  scelta               pic 9.
       77  idx                  pic 9(3).

      ******************************************************************
       PROCEDURE DIVISION.     
           COPY "DECLXER1".
                     paramshi
                     paramshi-old
                     .
           COPY "DECLXER2".

       MAIN-PRG.           
           display message box 
                          "Confermi la conversione del file paramshi?"
                          x"0D0A"
                          "Prima di procedere con la conversione"
                          x"0D0A"
                          "rinominare il file "
                          x"22"
                          "paramshi"
                          x"22"
                          " in "
                          x"22"
                          "paramshi-old"
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
           move zero   to cont

           open input paramshi-old.
           open output paramshi


           move low-value to old-shi-chiave.

           start paramshi-old key not less old-shi-chiave
              invalid
                 continue
              not invalid
                 perform until 1 = 2
                    read paramshi-old next
                       at end
                          exit perform
                    end-read

                    perform MUOVI-RECORD

                 end-perform
           end-start.

           close paramshi
                 paramshi-old.

           move cont   to cont-ed
           display message box "Convertiti " cont-ed " record.".


      ***---
       MUOVI-RECORD.
           add 1 to cont.

           move old-shi-codice              to shi-codice            
           move old-shi-cod-SHI             to shi-cod-SHI           
           move old-shi-mag-SHI             to shi-mag-SHI           
           move old-shi-nome-semaforo       to shi-nome-semaforo     
           move old-shi-path-report-server  to shi-path-report-server
           move old-shi-path-report-client  to shi-path-report-client
           move old-shi-path-exp            to shi-path-exp          
           move old-shi-path-backup-exp     to shi-path-backup-exp   
           move old-shi-file-articoli       to shi-file-articoli     
           move old-shi-file-ean            to shi-file-ean          
           move old-shi-file-prodener       to shi-file-prodener     
           move old-shi-file-vettori        to shi-file-vettori      
           move old-shi-file-fornitori      to shi-file-fornitori    
           move old-shi-file-tordini        to shi-file-tordini      
           move old-shi-file-rordini        to shi-file-rordini      
           move old-shi-file-note-ordini    to shi-file-note-ordini  
           move old-shi-file-classi         to shi-file-classi       
           move old-shi-file-vuoto-exp-1    to shi-file-vuoto-exp-1  
           move old-shi-file-vuoto-exp-2    to shi-file-vuoto-exp-2  
           move old-shi-file-vuoto-exp-3    to shi-file-vuoto-exp-3  
           move old-shi-file-vuoto-exp-4    to shi-file-vuoto-exp-4  
           move old-shi-file-vuoto-exp-5    to shi-file-vuoto-exp-5  
           move old-shi-sito-exp            to shi-sito-exp          
           move old-shi-user-exp            to shi-user-exp          
           move old-shi-pwd-exp             to shi-pwd-exp           
           move old-shi-path-remota-exp     to shi-path-remota-exp   
           move old-shi-path-imp            to shi-path-imp          
           move old-shi-path-backup-imp     to shi-path-backup-imp   
           move old-shi-file-articoli-imp   to shi-file-articoli-imp 
           move old-shi-file-tordini-imp    to shi-file-tordini-imp  
           move old-shi-file-rordini-imp    to shi-file-rordini-imp  
           move old-shi-file-vuoto-imp-1    to shi-file-vuoto-imp-1  
           move old-shi-file-vuoto-imp-2    to shi-file-vuoto-imp-2  
           move old-shi-file-vuoto-imp-3    to shi-file-vuoto-imp-3  
           move old-shi-file-vuoto-imp-4    to shi-file-vuoto-imp-4  
           move old-shi-file-vuoto-imp-5    to shi-file-vuoto-imp-5  
           move old-shi-sito-imp            to shi-sito-imp          
           move old-shi-user-imp            to shi-user-imp          
           move old-shi-pwd-imp             to shi-pwd-imp           
           move old-shi-path-remota-imp     to shi-path-remota-imp   
           move old-shi-dati-comuni         to shi-dati-comuni       
                                                                     
                                                                     
                                                                     

           write shi-rec.                      

