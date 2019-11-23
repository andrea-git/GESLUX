      ***---
       CREA-LOG.
           accept como-data from century-date
           accept como-ora  from time

           inspect shi-path-report-server
                                replacing trailing space by low-value
           inspect shi-path-report-client   
                                replacing trailing space by low-value
           string shi-path-report-server delimited by low-value
                  barra                  delimited by size
                  "exportlog_"           delimited by size
                  como-data              delimited by size
                  "_"                    delimited by size
                  como-ora               delimited by size
                  ".txt"                 delimited by size
                  into wstampa

           move wstampa   to splcrt2graf-percorso-stampa-u

           string shi-path-report-client delimited by low-value
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
                                         shi-path-exp,
                                         pattern.

           move RETURN-CODE        to Dir-Handle.

           if Dir-Handle = ZERO
              set errori             to true
              move shi-path-exp  to MSG-Folder-Name
              perform MSG-DIR-ERR
           else
              call "C$LIST-DIRECTORY" using LISTDIR-CLOSE, Dir-Handle
           end-if.
         

      *    cartella di backup
           call "C$LIST-DIRECTORY" using LISTDIR-OPEN,
                                         shi-path-backup-exp,
                                         pattern.

           move RETURN-CODE        to Dir-Handle.

           if Dir-Handle =   ZERO
              set errori             to true
              move shi-path-backup-exp  to MSG-Folder-Name
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
                move shi-file-tordini      to como-nome
                perform COPIA-FILE
                move shi-file-rordini      to como-nome
                perform COPIA-FILE
                move shi-file-note-ordini  to como-nome
                perform COPIA-FILE
                move shi-file-articoli     to como-nome
                perform COPIA-FILE
                move shi-file-ean          to como-nome
                perform COPIA-FILE
                move shi-file-prodener     to como-nome
                perform COPIA-FILE
           when crea-articoli
                move shi-file-articoli     to como-nome
                perform COPIA-FILE
                move shi-file-ean          to como-nome
                perform COPIA-FILE
                move shi-file-prodener     to como-nome
                perform COPIA-FILE
           when crea-anagrafiche
                move shi-file-vettori      to como-nome
                perform COPIA-FILE
                move shi-file-fornitori    to como-nome
                perform COPIA-FILE
                move shi-file-classi       to como-nome
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

           inspect shi-path-exp replacing trailing space by low-value      
           inspect shi-path-backup-exp
                                replacing trailing space by low-value      
           string shi-path-exp  delimited LOW-VALUE,
                  barra         delimited size,
                  como-nome     delimited size
                  into origine
                  
           inspect como-nome-2  replacing trailing space by low-value

           if como-estensione = space
              string shi-path-backup-exp delimited LOW-VALUE,
                     barra               delimited size,
                     como-nome-2         delimited low-value
                     "_"                 delimited by size        
                     como-data           delimited by size        
                     "_"                 delimited by size        
                     como-ora            delimited by size        
                     into Destinazione
           else
              string shi-path-backup-exp delimited LOW-VALUE,
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

