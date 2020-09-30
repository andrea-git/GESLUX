      ***---
       CREA-LOG.
           accept como-data from century-date
           accept como-ora  from time

           inspect shi-path-report-server
                                replacing trailing space by low-value
           inspect shi-path-report-client   
                                replacing trailing space by low-value
           
           initialize wstampa.
           string shi-path-report-server delimited by low-value
                  barra                  delimited by size
                  "exportlog_"           delimited by size
                  como-data              delimited by size
                  "_"                    delimited by size
                  como-ora               delimited by size
                  ".txt"                 delimited by size
             into wstampa
           end-string.

           move wstampa   to splcrt2graf-percorso-stampa-u

           initialize splcrt2graf-percorso-stampa.
           string shi-path-report-client delimited by low-value
                  barra                  delimited by size
                  "exportlog_"           delimited by size
                  como-data              delimited by size
                  "_"                    delimited by size
                  como-ora               delimited by size
                  ".txt"                 delimited by size
             into splcrt2graf-percorso-stampa
           end-string.

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
                move exp-shi-ordini-file-tordini      to como-nome
                perform COPIA-FILE
                move exp-shi-ordini-file-rordini      to como-nome
                perform COPIA-FILE
                move exp-shi-ordini-file-note-ordini  to como-nome
                perform COPIA-FILE
                move exp-shi-ordini-file-articoli     to como-nome
                perform COPIA-FILE
                move exp-shi-ordini-file-ean          to como-nome
                perform COPIA-FILE
                move exp-shi-ordini-file-prodener     to como-nome
                perform COPIA-FILE
           when crea-articoli
                move exp-shi-articoli-file-articoli     to como-nome
                perform COPIA-FILE
                move exp-shi-articoli-file-ean          to como-nome
                perform COPIA-FILE
                move exp-shi-articoli-file-prodener     to como-nome
                perform COPIA-FILE
           when crea-anagrafiche
                move exp-shi-anagrafiche-file-vettori      to como-nome
                perform COPIA-FILE
                move exp-shi-anagrafiche-file-fornitori    to como-nome
                perform COPIA-FILE
                move exp-shi-anagrafiche-file-classi       to como-nome
                perform COPIA-FILE
           end-evaluate.


      ***---
       COPIA-FILE.
           inspect como-nome replacing trailing space by low-value.
           |Siccome alcuni files hanno già data ed ora estraggo il nome
           |dal path già creato in modo corretto per l'export.
           move 0 to cont.
           inspect como-nome 
                   tallying cont for characters before low-value.

           perform until 1 = 2
              if como-nome(cont:1) = "/" or
                 como-nome(cont:1) = "\"
                 add 1 to cont 
                 exit perform
              end-if
              subtract 1 from cont
           end-perform.
           move como-nome(cont:) to como-nome-2.
           inspect como-nome-2 replacing trailing spaces by low-value.

           move como-nome to origine.
           inspect origine replacing trailing low-value by spaces.
           inspect shi-path-exp 
                   replacing trailing space by low-value.
           inspect shi-path-backup-exp
                   replacing trailing space by low-value.

           initialize destinazione.           
           string shi-path-exp delimited low-value,
                  barra        delimited size,
                  como-nome-2  delimited low-value   
             into destinazione
           end-string.

           call "C$COPY" using origine,
                               destinazione, 
                               rename-status.
                 
           initialize destinazione.
           string shi-path-backup-exp delimited low-value,
                  barra               delimited size,
                  como-nome-2         delimited low-value
                  "_"                 delimited size        
                  como-data           delimited size        
                  "_"                 delimited size        
                  como-ora            delimited size        
             into destinazione
           end-string.

           call "C$COPY" using origine,
                               destinazione, 
                               rename-status.
           call "C$DELETE" using origine.

