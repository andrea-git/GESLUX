       Program-id.   mkdir-vet.
       Author.       luciano.

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       FILE-CONTROL.
      *     copy "vettel.sl".

       FILE SECTION.
      *     copy "vettel.fd".

       WORKING-STORAGE SECTION.
           |copy "componi-path.def".
           copy "comune.def".
           copy "acucobol.def".

      * CONST
       77  TITOLO pic x(50).

      * FILE STATUS                        
      * 77  status-vettel   pic xx.


       01  EXTEND-STAT.      
           03 primary-err   pic XX.
           03 sec-err       pic X(10).

       77  TEXT-MESSAGE     pic x.

       77  CommandLine      pic x(200).
       77  command-status   pic s9.

       77  pid              pic x(5).
       77  como-var         pic x(256).
       77  status-code      pic 9.

       77  como-var-handle usage handle.
       LINKAGE SECTION.
           copy "link-mkdir-vettel.def".


      *-----------------------------------------------------------------

       PROCEDURE DIVISION using mkdir-vettel-linkage.
       DECLARATIVES.

      * CLIENTI-ERR section.
      *     use error procedure on vettel.
      *
      *     evaluate status-vettel
      *     when "93"
      *     when "99"
      *          set errori to true
      *          display message "Errore ", status-vettel, 
      *                          " sul file vettel.", X"0A",
      *                          "File in uso su altro terminale."
      *                    title titolo
      *     when "9D"
      *     when other
      *          call "C$RERR" using EXTEND-STAT, TEXT-MESSAGE
      *          set errori to true
      *          display message "Errore ", status-vettel,  ","
      *                          EXTEND-STAT(3:2)
      *                          " sul file vettel.", X"0A",
      *                         "Contattare l'amministratore di Sistema."
      *                    title titolo
      *     end-evaluate.
       END DECLARATIVES.

      ***---
       MAIN.                    
           perform INIT
      *     perform OPEN-FILES

           if tutto-ok
              perform ELABORAZIONE
      *        perform CLOSE-FILES
           end-if
           goback
           .

      ***---
       INIT.
           move "GESLUX - Creazione cartelle Vettore"   to titolo
           set tutto-ok to true
           .

      ****---
      * OPEN-FILES.  
      *     open input vettel
      *     .

      ***---
       ELABORAZIONE.                                               
           set  tutto-ok        to true.

      *     move lk-mkv-cod      to mkv-codice.           
      *     read vettel no lock 
      *        invalid 
      *           set errori to true 
      *     end-read

           if tutto-ok
              move mkv-path-environment to como-var
              perform CONTROLLA-DIR
           end-if
           inspect mkv-path-environment  replacing trailing space 
                                         by low-value
      *     if tutto-ok
      *        initialize como-var
      *        string mkv-path-environment delimited by low-value
      *               "/"                       delimited by size
      *               mkv-path-suff-orig-rete   delimited by size
      *               into como-var
      *        end-string
      *        perform CONTROLLA-DIR
      *     end-if
           if tutto-ok
              initialize como-var
              string mkv-path-environment delimited by low-value
                     "/"                       delimited by size
                     mkv-path-suff-tmp         delimited by size
                into como-var
              end-string
              perform CONTROLLA-DIR
           end-if
           if tutto-ok
              initialize como-var
              string mkv-path-environment delimited by low-value
                     "/"                       delimited by size
                     mkv-path-suff-log         delimited by size
                into como-var
              end-string
              perform CONTROLLA-DIR
           end-if
           if tutto-ok
              initialize como-var
              string mkv-path-environment delimited by low-value
                     "/"                       delimited by size
                     mkv-path-suff-orig        delimited by size
                into como-var
              end-string
              perform CONTROLLA-DIR
           end-if
           if tutto-ok
              initialize como-var
              string mkv-path-environment delimited by low-value
                     "/"                       delimited by size
                     mkv-path-suff-dest        delimited by size
                into como-var
              end-string
              perform CONTROLLA-DIR
           end-if

           if tutto-ok
              inspect como-var replacing trailing space by low-value
              string como-var                  delimited by low-value
                     "/pdf"                    delimited by size
                     into como-var
              end-string
              inspect como-var replacing trailing low-value by space
              perform CONTROLLA-DIR
           end-if



           if tutto-ok
              initialize como-var
              string mkv-path-environment delimited by low-value
                     "/"                       delimited by size
                     mkv-path-suff-err         delimited by size
                into como-var
              end-string
              perform CONTROLLA-DIR
           end-if
           if tutto-ok
              initialize como-var
              string mkv-path-environment delimited by low-value
                     "/"                       delimited by size
                     mkv-path-suff-invio       delimited by size
                into como-var
              end-string
              perform CONTROLLA-DIR
           end-if

           if tutto-ok
              inspect como-var replacing trailing space by low-value
              string como-var                  delimited by low-value
                     "-pdf"                    delimited by size
                     into como-var
              end-string
              inspect como-var replacing trailing low-value by space
              perform CONTROLLA-DIR
           end-if
           .

      *    do i permessi alle cartelle appena create
      **     initialize como-var.
      **     string "chmod -R 777 "        delimited by size
      **            mkv-path-environment   delimited by low-value
      **            "/*"          delimited by size
      **            into como-var.
      **
      **     call "SYSTEM" using como-var.

      ***---
       CONTROLLA-DIR.
           call "c$list-directory" using listdir-open, 
                                   como-var, 
                                   "*.*"
           move RETURN-CODE        to como-var-handle
           if como-var-handle = null
              call "c$makedir" using como-var giving status-code
              if status-code not = 0
                 inspect como-var replacing trailing space by low-value
                 display message box "Errore nella creazione della carte
      -          "lla:", x"0D0A", como-var
                          title titolo
                 set errori to true
              end-if
           end-if
           .

      ****---
      * CLOSE-FILES.
      *     close vettel
      *     .
