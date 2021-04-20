       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      sost-art-batch.
       AUTHOR.                          Andrea.
       REMARKS. Batch notturno su tutti gli ordini NON chiusi. Richiamo
                a "sost-art" per l'operazione vera e propria
  
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "mtordini.sl".

       SELECT logfile
           ASSIGN       TO path-log
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE  IS SEQUENTIAL
           FILE STATUS  IS STATUS-logfile.

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "mtordini.fd".         

       FD  logfile.
       01 riga-log        PIC  x(900).


       working-storage section.
           copy "link-sost-art.def".   
       01  r-inizio.
         05 filler                 pic x(2)  value " [".
         05 r-data.
            10 r-gg                pic xx.
            10 filler              pic x     value "/".
            10 r-mm                pic xx.
            10 filler              pic x     value "/".
            10 r-aa                pic xx.
         05 filler                 pic x(5)  value "] - [".
         05 r-ora.
            10 r-hh                pic xx.
            10 filler              pic x     value X"22".
            10 r-min               pic xx.
            10 filler              pic x     value "'".
            10 r-sec               pic xx.
         05 filler                 pic x(2)  value "] ".

       77  como-riga        pic x(200).

       77  status-logfile   pic xx.
       77  path-log         pic x(256).
       77  status-mtordini  pic xx.
       77  nargs            pic 99 comp-1.   
       77  counter          pic 9(9).
       77  counter2         pic 9(9).
       77  counter-edit     pic zzz.zzz.zz9.
       77  como-data        pic 9(8).
       77  como-ora         pic 9(8).

       01  filler           pic 9.
           88 RichiamoSchedulato    value 1, false 0.

       linkage section.
       copy "link-batch.def".

       procedure division using batch-linkage.
      ***---
       MAIN.     
           CALL "C$NARG" USING NARGS.
           if nargs not = 0
              set RichiamoSchedulato to true
              initialize path-log
              accept como-data from century-date
              accept como-ora  from time
              accept  path-log from environment "SCHEDULER_PATH_LOG"
              inspect path-log replacing trailing spaces by low-value
              string  path-log      delimited low-value
                      "SOST-ART_"  delimited size
                      como-data    delimited size
                      "_"          delimited size
                      como-ora     delimited size
                      ".log"       delimited size
                      into path-log
              end-string
              set RichiamoSchedulato to true
              move path-log to batch-log
              open output logfile    
              move "INIZIO PROGRAMMA" to como-riga
              perform SCRIVI-RIGA-LOG
           else
              set RichiamoSchedulato to false
           end-if.

           set sost-art-batch to true.
           move "BATCH" to sost-art-user.
           open input mtordini.
           set mto-registrato to true.
           move low-value to mto-rec.

           start mtordini key >= k-mto-stato.
           perform until 1 = 2
              read mtordini next at end exit perform end-read
              if mto-chiuso 
                 exit perform
              end-if
              if RichiamoSchedulato
                 initialize como-riga
                 string "INIZIO ELABORAZIONE ORDINE #" delimited size
                        mto-numero                     delimited size
                   into como-riga
                 end-string
                 perform SCRIVI-RIGA-LOG        
                 close logfile
                 move path-log to sost-art-log
              end-if

              move mto-chiave to sost-art-chiave
              call   "sost-art" using sost-art-linkage
              cancel "sost-art"    
              if RichiamoSchedulato      
                 open extend logfile
                 initialize como-riga
                 string "FINE ELABORAZIONE ORDINE #" delimited size
                        mto-numero                   delimited size
                   into como-riga
                 end-string
                 perform SCRIVI-RIGA-LOG

                 add 1 to counter counter2
                 if counter2 = 100
                    move counter to counter-edit
                    display counter-edit 
                            upon batch-win-handle
                            line 25,00
                          column 38,00
                    move 0 to counter2
                 end-if
              end-if
           end-perform.
                 
           close mtordini.    

           if RichiamoSchedulato
              move  0 to batch-status
              display "                                             "
                 upon batch-win-handle
                   line 25,00
                 column 35,00
              move "FINE PROGRAMMA" to como-riga
              perform SCRIVI-RIGA-LOG
              close logfile
           end-if.

           goback.

      ***---
       SCRIVI-RIGA-LOG.             
           initialize riga-log.
           perform SETTA-INIZIO-RIGA.
           string r-inizio  delimited size
                  como-riga delimited size
                  into riga-log
           end-string.
           write riga-log.

      ***---
       PARAGRAFO-COPY.
           copy "setta-inizio-riga.cpy".
