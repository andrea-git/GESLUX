       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      NOTRS.
       AUTHOR.                          Andrea.
       REMARKS. Utilizzato al momento del riscontro di un errore nelle transazioni
      ********************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       77  scelta                pic 9.

       PROCEDURE DIVISION.
       MAIN.
           display message 
                  "ATTENZIONE!!!!"                            
           x"0d0a""Verrà eseguito un riavvio del servizio ACU."
           x"0d0a""E' consigliabile far uscire tutti gli"
           x"0d0a""utenti dal gestionale prima di procedere."
           x"0d0a""Confermi?"
                     title "NOTRS"
                      type 2
                      icon 2
                   default 2
                    giving scelta
           if scelta = 1             
              display message 
                     "Premere OK per procedere con l'elaborazione"
              x"0d0a""ed attendere conferma di fine elaborazione."
                     title "NOTRS"
                      icon 2                         
              call "C$SYSTEM" using "E:\GESLUX\acu-stop.bat"
              call "C$SLEEP"  using 5
              call "C$SYSTEM" using "E:\GESLUX\del-trs.bat"  
              call "C$SLEEP"  using 15
              call "C$SYSTEM" using "E:\GESLUX\acu-start.bat"
              call "C$SLEEP"  using 5
              display message "Procedura terminata!" 
                        title "NOTRS"
           end-if.
           goback.
