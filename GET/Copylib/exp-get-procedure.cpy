      ***---
       CREA-LOG.
           accept como-data from century-date
           accept como-ora  from time

           inspect get-path-report-server
                                replacing trailing space by low-value
           inspect get-path-report-client   
                                replacing trailing space by low-value
           string get-path-report-server delimited by low-value
                  barra                  delimited by size
                  "exportlog_"           delimited by size
                  como-data              delimited by size
                  "_"                    delimited by size
                  como-ora               delimited by size
                  ".txt"                 delimited by size
                  into wstampa

           move wstampa   to splcrt2graf-percorso-stampa-u

           string get-path-report-client delimited by low-value
                  barra                  delimited by size
                  "exportlog_"           delimited by size
                  como-data              delimited by size
                  "_"                    delimited by size
                  como-ora               delimited by size
                  ".txt"                 delimited by size
                  into splcrt2graf-percorso-stampa

           open output lineseq
           close lineseq.

      ***---
       CHECK-CARTELLE.
      * CONTROLLO L'ESISTENZA DELLE CARTELLE 
           move "*.*"  to pattern

      *    cartella di export
           call "C$LIST-DIRECTORY" using LISTDIR-OPEN,
                                         get-path-exp,
                                         pattern.

           move RETURN-CODE        to Dir-Handle.

           if Dir-Handle = ZERO
              set errori             to true
              move get-path-exp  to MSG-Folder-Name
              perform MSG-DIR-ERR
           else
              call "C$LIST-DIRECTORY" using LISTDIR-CLOSE, Dir-Handle
           end-if.
         

      *    cartella di backup
           call "C$LIST-DIRECTORY" using LISTDIR-OPEN,
                                         get-path-backup-exp,
                                         pattern.

           move RETURN-CODE        to Dir-Handle.

           if Dir-Handle =   ZERO
              set errori             to true
              move get-path-backup-exp  to MSG-Folder-Name
              perform MSG-DIR-ERR
           else
              call "C$LIST-DIRECTORY" using LISTDIR-CLOSE, Dir-Handle
           end-if.

      ***---
       COPIA-FILES.
           accept como-data  from century-date
           accept como-ora   from time

           evaluate true
           when crea-ordini
                move get-file-tordini      to como-nome
                perform COPIA-FILE
                move get-file-rordini      to como-nome
                perform COPIA-FILE
                move get-file-note-ordini  to como-nome
                perform COPIA-FILE
                move get-file-articoli     to como-nome
                perform COPIA-FILE
                move get-file-ean          to como-nome
                perform COPIA-FILE
                move get-file-prodener     to como-nome
                perform COPIA-FILE
           when crea-articoli
                move get-file-articoli     to como-nome
                perform COPIA-FILE
                move get-file-ean          to como-nome
                perform COPIA-FILE
                move get-file-prodener     to como-nome
                perform COPIA-FILE
           when crea-anagrafiche
                move get-file-vettori      to como-nome
                perform COPIA-FILE
                move get-file-fornitori    to como-nome
                perform COPIA-FILE
                move get-file-classi       to como-nome
                perform COPIA-FILE
           end-evaluate.


      ***---
       COPIA-FILE.
           inspect como-nome replacing trailing space by low-value
           initialize cont
           inspect como-nome tallying cont 
                                      for characters before low-value
           inspect como-nome replacing trailing low-value by space  
           subtract 3 from cont
           move como-nome to como-nome-2
           initialize como-estensione
      *    controllo se ho l'estensione
           if como-nome(cont:1) = "."
              move space  to como-nome-2(cont:)
              add 1 to cont
              move como-nome(cont:3)  to como-estensione
           end-if.
              


           initialize origine
                      destinazione

           inspect get-path-exp replacing trailing space by low-value      
           inspect get-path-backup-exp
                                replacing trailing space by low-value      
           string get-path-exp  delimited LOW-VALUE,
                  barra         delimited size,
                  como-nome     delimited size
                  into origine
                  
           inspect como-nome-2  replacing trailing space by low-value

           if como-estensione = space
              string get-path-backup-exp delimited LOW-VALUE,
                     barra               delimited size,
                     como-nome-2         delimited low-value
                     "_"                 delimited by size        
                     como-data           delimited by size        
                     "_"                 delimited by size        
                     como-ora            delimited by size        
                     into Destinazione
           else
              string get-path-backup-exp delimited LOW-VALUE,
                     barra               delimited size,
                     como-nome-2         delimited low-value
                     "_"                 delimited by size        
                     como-data           delimited by size        
                     "_"                 delimited by size        
                     como-ora            delimited by size        
                     "."                 delimited by size        
                     como-estensione     delimited by size        
                     into Destinazione
           end-if

      *     inspect File-Name replacing trailing low-value by space
           if si-invio-ftp
              call "RENAME" using origine, 
                                  Destinazione, 
                                  RENAME-STATUS
           else
              call "C$COPY" using origine, 
                                  Destinazione, 
                                  RENAME-STATUS
           end-if.

