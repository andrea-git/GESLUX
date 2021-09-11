       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      storart-p.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "mtordini.sl".
           copy "mrordini.sl".
           copy "tordini.sl".
           copy "rordini.sl".
           copy "trasporti.sl".
           copy "tmovmag.sl".
           copy "rmovmag.sl".
           copy "giormag.sl".
           copy "tedi.sl".
           copy "redi.sl".
           copy "tnotacr.sl".
           copy "rnotacr.sl".
           copy "movutf.sl".
           copy "tordforn.sl".
           copy "rordforn.sl".
           copy "nordforn.sl".    
           copy "sordforn.sl".
           copy "provvig.sl".
           copy "eordini.sl".
           copy "teva.sl".
           copy "reva.sl".    
           copy "btnotacr.sl".
           copy "brnotacr.sl".    
           copy "contestazioni.sl".
           copy "tagli.sl".       
           copy "edi-mtordini.sl".
           copy "edi-mrordini.sl".
                                    
           copy "sto-tordini.sl".
           copy "sto-rordini.sl".
           copy "sto-mtordini.sl".
           copy "sto-mrordini.sl".
           copy "sto-trasporti.sl".
           copy "sto-tmovmag.sl".
           copy "sto-rmovmag.sl".
           copy "sto-giormag.sl".
           copy "sto-tedi.sl".
           copy "sto-redi.sl".
           copy "sto-tnotacr.sl".
           copy "sto-rnotacr.sl".
           copy "sto-movutf.sl".
           copy "sto-tordforn.sl".
           copy "sto-rordforn.sl".
           copy "sto-nordforn.sl".    
           copy "sto-sordforn.sl".
           copy "sto-provvig.sl".
           copy "sto-eordini.sl".
           copy "sto-teva.sl".
           copy "sto-reva.sl".    
           copy "sto-btnotacr.sl".
           copy "sto-brnotacr.sl".    
           copy "sto-contestazioni.sl".
           copy "sto-tagli.sl".  
           copy "sto-edi-mtordini.sl".
           copy "sto-edi-mrordini.sl".

      ******************************************************************
       DATA DIVISION.
       FILE SECTION.    
           copy "mtordini.fd".
           copy "mrordini.fd".
           copy "tordini.fd".
           copy "rordini.fd". 
           copy "trasporti.fd".
           copy "tmovmag.fd".
           copy "rmovmag.fd".
           copy "giormag.fd".
           copy "tedi.fd".
           copy "redi.fd".
           copy "tnotacr.fd".
           copy "rnotacr.fd".
           copy "movutf.fd".
           copy "tordforn.fd".
           copy "rordforn.fd".
           copy "nordforn.fd".
           copy "sordforn.fd".
           copy "provvig.fd".
           copy "eordini.fd".  
           copy "teva.fd".
           copy "reva.fd".  
           copy "btnotacr.fd".
           copy "brnotacr.fd".
           copy "contestazioni.fd".
           copy "tagli.fd".  
           copy "edi-mtordini.fd".
           copy "edi-mrordini.fd". 

           copy "sto-tordini.fd".
           copy "sto-rordini.fd".
           copy "sto-mtordini.fd".
           copy "sto-mrordini.fd".
           copy "sto-trasporti.fd". 
           copy "sto-tmovmag.fd".
           copy "sto-rmovmag.fd".
           copy "sto-giormag.fd".
           copy "sto-tedi.fd".
           copy "sto-redi.fd".
           copy "sto-tnotacr.fd".
           copy "sto-rnotacr.fd".
           copy "sto-movutf.fd".
           copy "sto-tordforn.fd".
           copy "sto-rordforn.fd".
           copy "sto-nordforn.fd".
           copy "sto-sordforn.fd".
           copy "sto-provvig.fd".
           copy "sto-eordini.fd".  
           copy "sto-teva.fd".
           copy "sto-reva.fd".  
           copy "sto-btnotacr.fd".
           copy "sto-brnotacr.fd".
           copy "sto-contestazioni.fd".
           copy "sto-tagli.fd". 
           copy "sto-edi-mtordini.fd".
           copy "sto-edi-mrordini.fd".  

      ******************************************************************
       WORKING-STORAGE SECTION.
           COPY "acucobol.def".
           copy "link-geslock.def".
           copy "comune.def".

       77  counter             pic 9(12).
       77  counter2            pic 9(12).
       77  counter-edit        pic z(12).
       77  comando             pic x(200).
       77  path-vutil          pic x(200).

       78  interlinea          value 2.
       78  titolo    value "GESLUX - Storicizzazione archivi".

       77  status-mtordini      pic xx.
       77  status-mrordini      pic xx.
       77  status-tordini       pic xx.
       77  status-rordini       pic xx.
       77  status-trasporti     pic xx.
       77  status-tmovmag       pic xx.
       77  status-rmovmag       pic xx.
       77  status-giormag       pic xx.
       77  status-tedi          pic xx.
       77  status-redi          pic xx.
       77  status-tnotacr       pic xx.
       77  status-rnotacr       pic xx.
       77  status-movutf        pic xx.
       77  status-tordforn      pic xx.
       77  status-rordforn      pic xx.
       77  status-nordforn      pic xx.
       77  status-sordforn      pic xx.
       77  status-provvig       pic xx.
       77  status-eordini       pic xx.
       77  status-teva          pic xx.
       77  status-reva          pic xx.      
       77  status-btnotacr      pic xx.
       77  status-brnotacr      pic xx.
       77  status-contestazioni pic xx.
       77  status-tagli         pic xx.
       77  status-edi-mtordini  pic xx.
       77  status-edi-mrordini  pic xx.
                                        
       77  status-STO-tordini       pic xx.
       77  status-STO-rordini       pic xx.
       77  status-STO-mtordini      pic xx.
       77  status-STO-mrordini      pic xx.
       77  status-STO-trasporti     pic xx.    
       77  status-sto-tmovmag       pic xx.
       77  status-sto-rmovmag       pic xx.
       77  status-sto-giormag       pic xx.
       77  status-sto-tedi          pic xx.
       77  status-sto-redi          pic xx.
       77  status-sto-tnotacr       pic xx.
       77  status-sto-rnotacr       pic xx.
       77  status-sto-movutf        pic xx.
       77  status-sto-tordforn      pic xx.
       77  status-sto-rordforn      pic xx.
       77  status-sto-nordforn      pic xx.
       77  status-sto-sordforn      pic xx.
       77  status-sto-provvig       pic xx.
       77  status-sto-eordini       pic xx.
       77  status-sto-teva          pic xx.
       77  status-sto-reva          pic xx.      
       77  status-sto-btnotacr      pic xx.
       77  status-sto-brnotacr      pic xx.
       77  status-sto-contestazioni pic xx.
       77  status-sto-tagli         pic xx.  
       77  status-sto-edi-mtordini  pic xx.
       77  status-sto-edi-mrordini  pic xx.

       77  path-archivi           pic x(256).
       77  path-archivi-sto       pic x(256).
       77  nome-file              pic x(20).
       77  line-file              pic 99v99.
       77  path-file              pic x(256).
                                       
       77  path-sto-mtordini      pic x(256).
       77  path-sto-mrordini      pic x(256).
       77  path-sto-tordini       pic x(256).
       77  path-sto-rordini       pic x(256).
       77  path-sto-trasporti     pic x(256).
       77  path-sto-tmovmag       pic x(256).
       77  path-sto-rmovmag       pic x(256).
       77  path-sto-giormag       pic x(256).
       77  path-sto-tedi          pic x(256).
       77  path-sto-redi          pic x(256).
       77  path-sto-tnotacr       pic x(256).
       77  path-sto-rnotacr       pic x(256).
       77  path-sto-movutf        pic x(256).
       77  path-sto-tordforn      pic x(256).
       77  path-sto-rordforn      pic x(256).
       77  path-sto-nordforn      pic x(256).
       77  path-sto-sordforn      pic x(256).
       77  path-sto-provvig       pic x(256).
       77  path-sto-eordini       pic x(256).
       77  path-sto-teva          pic x(256).
       77  path-sto-reva          pic x(256).      
       77  path-sto-btnotacr      pic x(256).
       77  path-sto-brnotacr      pic x(256).
       77  path-sto-contestazioni pic x(256).   
       77  path-sto-tagli         pic x(256).
       77  path-sto-edi-mtordini  pic x(256).
       77  path-sto-edi-mrordini  pic x(256).

       77 EXTEND-STAT   pic x(5).
       77 TEXT-MESSAGE  pic x(50).
       77 STATUS-TYPE   signed-short.

       LINKAGE SECTION.
           copy "link-storart.def".

      ******************************************************************
       PROCEDURE DIVISION USING storart-linkage.

       DECLARATIVES.
           copy "storart-p-decla.cpy".
       END DECLARATIVES.

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
           set RecLocked to false.
           set tutto-ok  to true.                             
           accept  path-vutil       from environment "PATH_VUTIL".
           accept  path-archivi     from environment "PATH_ARCHIVI".
           accept  path-archivi-sto from environment "PATH_ARCHIVI_STO"
           inspect path-archivi-sto 
                   replacing trailing spaces by low-value.
           inspect path-archivi 
                   replacing trailing spaces by low-value.
           inspect path-vutil replacing trailing spaces by low-value.

      ***---
       OPEN-FILES.
           perform OPEN-TORDINI-LOCK
           if tutto-ok
            perform OPEN-RORDINI-LOCK
             if tutto-ok
              perform OPEN-MTORDINI-LOCK
               if tutto-ok
                perform OPEN-MRORDINI-LOCK
                 if tutto-ok
                  perform OPEN-TRASPORTI-LOCK
                   if tutto-ok
                    perform OPEN-TMOVMAG-LOCK
                     if tutto-ok
                      perform OPEN-RMOVMAG-LOCK
                       if tutto-ok
                        perform OPEN-GIORMAG-LOCK
                         if tutto-ok
                          perform OPEN-TEDI-LOCK
                           if tutto-ok
                            perform OPEN-REDI-LOCK
                             if tutto-ok
                              perform OPEN-TNOTACR-LOCK
                               if tutto-ok
                                perform OPEN-RNOTACR-LOCK
                                 if tutto-ok
                                  perform OPEN-MOVUTF-LOCK
                                   if tutto-ok
                                    perform OPEN-TORDFORN-LOCK
                                     if tutto-ok
                                      perform OPEN-RORDFORN-LOCK
                                       if tutto-ok
                                        perform OPEN-NORDFORN-LOCK
                                         if tutto-ok
                                          perform OPEN-EORDINI-LOCK
                                           if tutto-ok
                                            perform OPEN-PROVVIG-LOCK
                                             if tutto-ok
                                              perform OPEN-TEVA-LOCK
                                               if tutto-ok
                                                perform OPEN-REVA-LOCK
                                                 if tutto-ok 
                                                  perform 
                                                  OPEN-BTNOTACR-LOCK
                                                   if tutto-ok
                                                    perform
                                                    OPEN-BRNOTACR-LOCK
                                                     if tutto-ok
                                                      perform
                                               OPEN-CONTESTAZIONI-LOCK
                                                       if tutto-ok
                                                        perform
                                                    OPEN-SORDFORN-LOCK
                                                         if tutto-ok
                                                          perform
                                                     OPEN-TAGLI-LOCK
                                                          if tutto-ok
                                                             perform
                                                  OPEN-EDI-MTORDINI-LOCK
                                                            if tutto-ok
                                                               perform   
                                                  OPEN-EDI-MRORDINI-LOCK
                                                            end-if
                                                          end-if
                                                         end-if
                                                       end-if
                                                     end-if
                                                   end-if
                                                 end-if
                                               end-if
                                             end-if
                                           end-if
                                         end-if
                                       end-if
                                     end-if
                                   end-if
                                 end-if
                               end-if
                             end-if
                           end-if
                         end-if
                       end-if
                     end-if
                   end-if
                 end-if
               end-if
             end-if
           end-if.  
           
      ***---
       COMPONI-PATH.
           initialize path-file.
           inspect geslock-nome-file 
                   replacing trailing spaces by low-value.
           string path-archivi-sto  delimited low-value
                  geslock-nome-file delimited low-value
                  into path-file
           end-string.

      ***---
       OPEN-TORDINI-LOCK.
           string   "Il file degli ordini" 
             x"0d0a""risulta in uso su altro terminale."
                 delimited size
                 into geslock-messaggio
           end-string.
           move "tordini" to geslock-nome-file.

           set tutto-ok   to true.
           perform until 1 = 2
              set RecLocked to false
              open i-o tordini allowing readers
              if not RecLocked
                 exit perform
              end-if

              move 1 to geslock-v-riprova
              move 0 to geslock-v-ignora
              move 1 to geslock-v-termina
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova continue
              when termina 
                   set errori to true
                   exit perform
              end-evaluate
           end-perform.
           if tutto-ok
              perform COMPONI-PATH
              move path-file to path-sto-tordini
              open i-o sto-tordini
              if status-sto-tordini = "35"
                 open output sto-tordini
                 close       sto-tordini
                 open i-o    sto-tordini
              end-if
           end-if.

      ***---
       OPEN-RORDINI-LOCK.
           string   "Il file degli ordini" 
             x"0d0a""risulta in uso su altro terminale."
                 delimited size
                 into geslock-messaggio
           end-string.
           move "rordini" to geslock-nome-file.

           set tutto-ok   to true.
           perform until 1 = 2
              set RecLocked to false
              open i-o rordini allowing readers
              if not RecLocked
                 exit perform
              end-if

              move 1 to geslock-v-riprova
              move 0 to geslock-v-ignora
              move 1 to geslock-v-termina
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova continue
              when termina 
                   set errori to true
                   exit perform
              end-evaluate
           end-perform.  
           if tutto-ok
              perform COMPONI-PATH
              move path-file to path-sto-rordini
              open i-o sto-rordini
              if status-sto-rordini = "35"
                 open output sto-rordini
                 close       sto-rordini
                 open i-o    sto-rordini
              end-if
           end-if. 

      ***---
       OPEN-MTORDINI-LOCK.
           string   "Il file dei master" 
             x"0d0a""risulta in uso su altro terminale."
                 delimited size
                 into geslock-messaggio
           end-string.
           move "mtordini" to geslock-nome-file.

           set tutto-ok   to true.
           perform until 1 = 2
              set RecLocked to false
              open i-o mtordini allowing readers
              if not RecLocked
                 exit perform
              end-if

              move 1 to geslock-v-riprova
              move 0 to geslock-v-ignora
              move 1 to geslock-v-termina
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova continue
              when termina 
                   set errori to true
                   exit perform
              end-evaluate
           end-perform.  
           if tutto-ok
              perform COMPONI-PATH
              move path-file to path-sto-mtordini
              open i-o sto-mtordini
              if status-sto-mtordini = "35"
                 open output sto-mtordini
                 close       sto-mtordini
                 open i-o    sto-mtordini
              end-if
           end-if.

      ***---
       OPEN-MRORDINI-LOCK.
           string   "Il file delle righe master" 
             x"0d0a""risulta in uso su altro terminale."
                 delimited size
                 into geslock-messaggio
           end-string.
           move "mrordini" to geslock-nome-file.

           set tutto-ok   to true.
           perform until 1 = 2
              set RecLocked to false
              open i-o mrordini allowing readers
              if not RecLocked
                 exit perform
              end-if

              move 1 to geslock-v-riprova
              move 0 to geslock-v-ignora
              move 1 to geslock-v-termina
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova continue
              when termina 
                   set errori to true
                   exit perform
              end-evaluate
           end-perform.   
           if tutto-ok
              perform COMPONI-PATH
              move path-file to path-sto-mrordini
              open i-o sto-mrordini     
              if status-sto-mrordini = "35"
                 open output sto-mrordini
                 close       sto-mrordini
                 open i-o    sto-mrordini
              end-if
           end-if.                

      ***---
       OPEN-EDI-MTORDINI-LOCK.
           string   "Il file dei master EDI " 
             x"0d0a""risulta in uso su altro terminale."
                 delimited size
                 into geslock-messaggio
           end-string.
           move "edi-mtordini" to geslock-nome-file.

           set tutto-ok   to true.
           perform until 1 = 2
              set RecLocked to false
              open i-o edi-mtordini allowing readers
              if not RecLocked
                 exit perform
              end-if

              move 1 to geslock-v-riprova
              move 0 to geslock-v-ignora
              move 1 to geslock-v-termina
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova continue
              when termina 
                   set errori to true
                   exit perform
              end-evaluate
           end-perform.  
           if tutto-ok
              perform COMPONI-PATH
              move path-file to path-sto-edi-mtordini
              open i-o sto-edi-mtordini
              if status-sto-edi-mtordini = "35"
                 open output sto-edi-mtordini
                 close       sto-edi-mtordini
                 open i-o    sto-edi-mtordini
              end-if
           end-if.

      ***---
       OPEN-EDI-MRORDINI-LOCK.
           string   "Il file dei master EDI " 
             x"0d0a""risulta in uso su altro terminale."
                 delimited size
                 into geslock-messaggio
           end-string.
           move "edi-mrordini" to geslock-nome-file.

           set tutto-ok   to true.
           perform until 1 = 2
              set RecLocked to false
              open i-o edi-mrordini allowing readers
              if not RecLocked
                 exit perform
              end-if

              move 1 to geslock-v-riprova
              move 0 to geslock-v-ignora
              move 1 to geslock-v-termina
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova continue
              when termina 
                   set errori to true
                   exit perform
              end-evaluate
           end-perform.  
           if tutto-ok
              perform COMPONI-PATH
              move path-file to path-sto-edi-mrordini
              open i-o sto-edi-mrordini
              if status-sto-edi-mrordini = "35"
                 open output sto-edi-mrordini
                 close       sto-edi-mrordini
                 open i-o    sto-edi-mrordini
              end-if
           end-if.

      ***---
       OPEN-TRASPORTI-LOCK.
           string   "Il file dei trasporti" 
             x"0d0a""risulta in uso su altro terminale."
                 delimited size
                 into geslock-messaggio
           end-string.
           move "trasporti" to geslock-nome-file.

           set tutto-ok   to true.
           perform until 1 = 2
              set RecLocked to false
              open i-o trasporti allowing readers
              if not RecLocked
                 exit perform
              end-if

              move 1 to geslock-v-riprova
              move 0 to geslock-v-ignora
              move 1 to geslock-v-termina
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova continue
              when termina 
                   set errori to true
                   exit perform
              end-evaluate
           end-perform.      
           if tutto-ok
              perform COMPONI-PATH
              move path-file to path-sto-trasporti
              open i-o sto-trasporti
              if status-sto-trasporti = "35"
                 open output sto-trasporti
                 close       sto-trasporti
                 open i-o    sto-trasporti
              end-if
           end-if.

      ***---
       OPEN-TMOVMAG-LOCK.
           string   "Il file dei movimenti di magazzino" 
             x"0d0a""risulta in uso su altro terminale."
                 delimited size
                 into geslock-messaggio
           end-string.
           move "tmovmag" to geslock-nome-file.

           set tutto-ok   to true.
           perform until 1 = 2
              set RecLocked to false
              open i-o tmovmag allowing readers
              if not RecLocked
                 exit perform
              end-if

              move 1 to geslock-v-riprova
              move 0 to geslock-v-ignora
              move 1 to geslock-v-termina
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova continue
              when termina 
                   set errori to true
                   exit perform
              end-evaluate
           end-perform.   
           if tutto-ok
              perform COMPONI-PATH
              move path-file to path-sto-tmovmag
              open i-o sto-tmovmag
              if status-sto-tmovmag = "35"
                 open output sto-tmovmag
                 close       sto-tmovmag
                 open i-o    sto-tmovmag
              end-if
           end-if. 

      ***---
       OPEN-RMOVMAG-LOCK.
           string   "Il file dei movimenti di magazzino" 
             x"0d0a""risulta in uso su altro terminale."
                 delimited size
                 into geslock-messaggio
           end-string.
           move "rmovmag" to geslock-nome-file.

           set tutto-ok   to true.
           perform until 1 = 2
              set RecLocked to false
              open i-o rmovmag allowing readers
              if not RecLocked
                 exit perform
              end-if

              move 1 to geslock-v-riprova
              move 0 to geslock-v-ignora
              move 1 to geslock-v-termina
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova continue
              when termina 
                   set errori to true
                   exit perform
              end-evaluate
           end-perform.      
           if tutto-ok
              perform COMPONI-PATH
              move path-file to path-sto-rmovmag
              open i-o sto-rmovmag
              if status-sto-rmovmag = "35"
                 open output sto-rmovmag
                 close       sto-rmovmag
                 open i-o    sto-rmovmag
              end-if
           end-if. 

      ***---
       OPEN-GIORMAG-LOCK.
           string   "Il file del giornale di magazzino" 
             x"0d0a""risulta in uso su altro terminale."
                 delimited size
                 into geslock-messaggio
           end-string.
           move "giormag" to geslock-nome-file.

           set tutto-ok   to true.
           perform until 1 = 2
              set RecLocked to false
              open i-o giormag allowing readers
              if not RecLocked
                 exit perform
              end-if

              move 1 to geslock-v-riprova
              move 0 to geslock-v-ignora
              move 1 to geslock-v-termina
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova continue
              when termina 
                   set errori to true
                   exit perform
              end-evaluate
           end-perform.      
           if tutto-ok
              perform COMPONI-PATH
              move path-file to path-sto-giormag
              open i-o sto-giormag
              if status-sto-giormag = "35"
                 open output sto-giormag
                 close       sto-giormag
                 open i-o    sto-giormag
              end-if
           end-if. 

      ***---
       OPEN-TEDI-LOCK.
           string   "Il file TEDI" 
             x"0d0a""risulta in uso su altro terminale."
                 delimited size
                 into geslock-messaggio
           end-string.
           move "tedi" to geslock-nome-file.

           set tutto-ok   to true.
           perform until 1 = 2
              set RecLocked to false
              open i-o tedi allowing readers
              if not RecLocked
                 exit perform
              end-if

              move 1 to geslock-v-riprova
              move 0 to geslock-v-ignora
              move 1 to geslock-v-termina
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova continue
              when termina 
                   set errori to true
                   exit perform
              end-evaluate
           end-perform.   
           if tutto-ok
              perform COMPONI-PATH
              move path-file to path-sto-tedi
              open i-o sto-tedi
              if status-sto-tedi = "35"
                 open output sto-tedi
                 close       sto-tedi
                 open i-o    sto-tedi
              end-if
           end-if.  

      ***---
       OPEN-REDI-LOCK.
           string   "Il file REDI" 
             x"0d0a""risulta in uso su altro terminale."
                 delimited size
                 into geslock-messaggio
           end-string.
           move "redi" to geslock-nome-file.

           set tutto-ok   to true.
           perform until 1 = 2
              set RecLocked to false
              open i-o redi allowing readers
              if not RecLocked
                 exit perform
              end-if

              move 1 to geslock-v-riprova
              move 0 to geslock-v-ignora
              move 1 to geslock-v-termina
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova continue
              when termina 
                   set errori to true
                   exit perform
              end-evaluate
           end-perform.    
           if tutto-ok
              perform COMPONI-PATH
              move path-file to path-sto-redi
              open i-o sto-redi
              if status-sto-redi = "35"
                 open output sto-redi
                 close       sto-redi
                 open i-o    sto-redi
              end-if
           end-if.  

      ***---
       OPEN-TNOTACR-LOCK.
           string   "Il file delle note credito" 
             x"0d0a""risulta in uso su altro terminale."
                 delimited size
                 into geslock-messaggio
           end-string.
           move "tnotacr" to geslock-nome-file.

           set tutto-ok   to true.
           perform until 1 = 2
              set RecLocked to false
              open i-o tnotacr allowing readers
              if not RecLocked
                 exit perform
              end-if

              move 1 to geslock-v-riprova
              move 0 to geslock-v-ignora
              move 1 to geslock-v-termina
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova continue
              when termina 
                   set errori to true
                   exit perform
              end-evaluate
           end-perform.   
           if tutto-ok
              perform COMPONI-PATH
              move path-file to path-sto-tnotacr
              open i-o sto-tnotacr
              if status-sto-tnotacr = "35"
                 open output sto-tnotacr
                 close       sto-tnotacr
                 open i-o    sto-tnotacr
              end-if
           end-if.   

      ***---
       OPEN-RNOTACR-LOCK.
           string   "Il file delle note credito"
             x"0d0a""risulta in uso su altro terminale."
                 delimited size
                 into geslock-messaggio
           end-string.
           move "rnotacr" to geslock-nome-file.

           set tutto-ok   to true.
           perform until 1 = 2
              set RecLocked to false
              open i-o rnotacr allowing readers
              if not RecLocked
                 exit perform
              end-if

              move 1 to geslock-v-riprova
              move 0 to geslock-v-ignora
              move 1 to geslock-v-termina
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova continue
              when termina 
                   set errori to true
                   exit perform
              end-evaluate
           end-perform.     
           if tutto-ok
              perform COMPONI-PATH
              move path-file to path-sto-rnotacr
              open i-o sto-rnotacr
              if status-sto-rnotacr = "35"
                 open output sto-rnotacr
                 close       sto-rnotacr
                 open i-o    sto-rnotacr
              end-if
           end-if.   

      ***---
       OPEN-MOVUTF-LOCK.
           string   "Il file dei movimenti UTF" 
             x"0d0a""risulta in uso su altro terminale."
                 delimited size
                 into geslock-messaggio
           end-string.
           move "movutf" to geslock-nome-file.

           set tutto-ok   to true.
           perform until 1 = 2
              set RecLocked to false
              open i-o movutf allowing readers
              if not RecLocked
                 exit perform
              end-if

              move 1 to geslock-v-riprova
              move 0 to geslock-v-ignora
              move 1 to geslock-v-termina
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova continue
              when termina 
                   set errori to true
                   exit perform
              end-evaluate
           end-perform.     
           if tutto-ok
              perform COMPONI-PATH
              move path-file to path-sto-movutf
              open i-o sto-movutf
              if status-sto-movutf = "35"
                 open output sto-movutf
                 close       sto-movutf
                 open i-o    sto-movutf
              end-if
           end-if.      

      ***---
       OPEN-TORDFORN-LOCK.
           string   "Il file degli ordini fornitori" 
             x"0d0a""risulta in uso su altro terminale."
                 delimited size
                 into geslock-messaggio
           end-string.
           move "tordforn" to geslock-nome-file.

           set tutto-ok   to true.
           perform until 1 = 2
              set RecLocked to false
              open i-o tordforn allowing readers
              if not RecLocked
                 exit perform
              end-if

              move 1 to geslock-v-riprova
              move 0 to geslock-v-ignora
              move 1 to geslock-v-termina
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova continue
              when termina 
                   set errori to true
                   exit perform
              end-evaluate
           end-perform.   
           if tutto-ok
              perform COMPONI-PATH
              move path-file to path-sto-tordforn
              open i-o sto-tordforn
              if status-sto-tordforn = "35"
                 open output sto-tordforn
                 close       sto-tordforn
                 open i-o    sto-tordforn
              end-if
           end-if.       

      ***---
       OPEN-RORDFORN-LOCK.
           string   "Il file degli ordini fornitori" 
             x"0d0a""risulta in uso su altro terminale."
                 delimited size
                 into geslock-messaggio
           end-string.
           move "rordforn" to geslock-nome-file.

           set tutto-ok   to true.
           perform until 1 = 2
              set RecLocked to false
              open i-o rordforn allowing readers
              if not RecLocked
                 exit perform
              end-if

              move 1 to geslock-v-riprova
              move 0 to geslock-v-ignora
              move 1 to geslock-v-termina
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova continue
              when termina 
                   set errori to true
                   exit perform
              end-evaluate
           end-perform.      
           if tutto-ok
              perform COMPONI-PATH
              move path-file to path-sto-rordforn
              open i-o sto-rordforn
              if status-sto-rordforn = "35"
                 open output sto-rordforn
                 close       sto-rordforn
                 open i-o    sto-rordforn
              end-if
           end-if.       

      ***---
       OPEN-NORDFORN-LOCK.
           string   "Il file delle note ordini fornitori" 
             x"0d0a""risulta in uso su altro terminale."
                 delimited size
                 into geslock-messaggio
           end-string.
           move "nordforn" to geslock-nome-file.

           set tutto-ok   to true.
           perform until 1 = 2
              set RecLocked to false
              open i-o nordforn allowing readers
              if not RecLocked
                 exit perform
              end-if

              move 1 to geslock-v-riprova
              move 0 to geslock-v-ignora
              move 1 to geslock-v-termina
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova continue
              when termina 
                   set errori to true
                   exit perform
              end-evaluate
           end-perform.   
           if tutto-ok
              perform COMPONI-PATH
              move path-file to path-sto-nordforn
              open i-o sto-nordforn
              if status-sto-nordforn = "35"
                 open output sto-nordforn
                 close       sto-nordforn
                 open i-o    sto-nordforn
              end-if
           end-if.         

      ***---
       OPEN-EORDINI-LOCK.
           string   "Il file degli esiti ordini" 
             x"0d0a""risulta in uso su altro terminale."
                 delimited size
                 into geslock-messaggio
           end-string.
           move "eordini" to geslock-nome-file.

           set tutto-ok   to true.
           perform until 1 = 2
              set RecLocked to false
              open i-o eordini allowing readers
              if not RecLocked
                 exit perform
              end-if

              move 1 to geslock-v-riprova
              move 0 to geslock-v-ignora
              move 1 to geslock-v-termina
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova continue
              when termina 
                   set errori to true
                   exit perform
              end-evaluate
           end-perform.  
           if tutto-ok
              perform COMPONI-PATH
              move path-file to path-sto-eordini
              open i-o sto-eordini
              if status-sto-eordini = "35"
                 open output sto-eordini
                 close       sto-eordini
                 open i-o    sto-eordini
              end-if
           end-if.       

      ***---
       OPEN-PROVVIG-LOCK.
           string   "Il file delle provvigioni" 
             x"0d0a""risulta in uso su altro terminale."
                 delimited size
                 into geslock-messaggio
           end-string.
           move "provvig" to geslock-nome-file.

           set tutto-ok   to true.
           perform until 1 = 2
              set RecLocked to false
              open i-o provvig allowing readers
              if not RecLocked
                 exit perform
              end-if

              move 1 to geslock-v-riprova
              move 0 to geslock-v-ignora
              move 1 to geslock-v-termina
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova continue
              when termina 
                   set errori to true
                   exit perform
              end-evaluate
           end-perform.   
           if tutto-ok
              perform COMPONI-PATH
              move path-file to path-sto-provvig
              open i-o sto-provvig
              if status-sto-provvig = "35"
                 open output sto-provvig
                 close       sto-provvig
                 open i-o    sto-provvig
              end-if
           end-if.       

      ***---
       OPEN-TEVA-LOCK.
           string   "Il file delle evasioni ordini f" 
             x"0d0a""risulta in uso su altro terminale."
                 delimited size
                 into geslock-messaggio
           end-string.
           move "teva" to geslock-nome-file.

           set tutto-ok   to true.
           perform until 1 = 2
              set RecLocked to false
              open i-o teva allowing readers
              if not RecLocked
                 exit perform
              end-if

              move 1 to geslock-v-riprova
              move 0 to geslock-v-ignora
              move 1 to geslock-v-termina
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova continue
              when termina 
                   set errori to true
                   exit perform
              end-evaluate
           end-perform.   
           if tutto-ok
              perform COMPONI-PATH
              move path-file to path-sto-teva
              open i-o sto-teva
              if status-sto-teva = "35"
                 open output sto-teva
                 close       sto-teva
                 open i-o    sto-teva
              end-if
           end-if.       

      ***---
       OPEN-REVA-LOCK.
           string   "Il file delle evasioni ordini f" 
             x"0d0a""risulta in uso su altro terminale."
                 delimited size
                 into geslock-messaggio
           end-string.
           move "reva" to geslock-nome-file.

           set tutto-ok   to true.
           perform until 1 = 2
              set RecLocked to false
              open i-o reva allowing readers
              if not RecLocked
                 exit perform
              end-if

              move 1 to geslock-v-riprova
              move 0 to geslock-v-ignora
              move 1 to geslock-v-termina
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova continue
              when termina 
                   set errori to true
                   exit perform
              end-evaluate
           end-perform.   
           if tutto-ok
              perform COMPONI-PATH
              move path-file to path-sto-reva
              open i-o sto-reva
              if status-sto-reva = "35"
                 open output sto-reva
                 close       sto-reva
                 open i-o    sto-reva
              end-if
           end-if.       

      ***---
       OPEN-BTNOTACR-LOCK.
           string   "Il file delle bozze bozze note cr" 
             x"0d0a""risulta in uso su altro terminale."
                 delimited size
                 into geslock-messaggio
           end-string.
           move "btnotacr" to geslock-nome-file.

           set tutto-ok   to true.
           perform until 1 = 2
              set RecLocked to false
              open i-o btnotacr allowing readers
              if not RecLocked
                 exit perform
              end-if

              move 1 to geslock-v-riprova
              move 0 to geslock-v-ignora
              move 1 to geslock-v-termina
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova continue
              when termina 
                   set errori to true
                   exit perform
              end-evaluate
           end-perform.    
           if tutto-ok
              perform COMPONI-PATH
              move path-file to path-sto-btnotacr
              open i-o sto-btnotacr
              if status-sto-btnotacr = "35"
                 open output sto-btnotacr
                 close       sto-btnotacr
                 open i-o    sto-btnotacr
              end-if
           end-if.         

      ***---
       OPEN-BRNOTACR-LOCK.
           string   "Il file delle bozze note cr" 
             x"0d0a""risulta in uso su altro terminale."
                 delimited size
                 into geslock-messaggio
           end-string.
           move "brnotacr" to geslock-nome-file.

           set tutto-ok   to true.
           perform until 1 = 2
              set RecLocked to false
              open i-o brnotacr allowing readers
              if not RecLocked
                 exit perform
              end-if

              move 1 to geslock-v-riprova
              move 0 to geslock-v-ignora
              move 1 to geslock-v-termina
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova continue
              when termina 
                   set errori to true
                   exit perform
              end-evaluate
           end-perform.    
           if tutto-ok
              perform COMPONI-PATH
              move path-file to path-sto-brnotacr
              open i-o sto-brnotacr
              if status-sto-brnotacr = "35"
                 open output sto-brnotacr
                 close       sto-brnotacr
                 open i-o    sto-brnotacr
              end-if
           end-if.           

      ***---
       OPEN-CONTESTAZIONI-LOCK.
           string   "Il file delle contestazioni" 
             x"0d0a""risulta in uso su altro terminale."
                 delimited size
                 into geslock-messaggio
           end-string.
           move "contestazioni" to geslock-nome-file.

           set tutto-ok   to true.
           perform until 1 = 2
              set RecLocked to false
              open i-o contestazioni allowing readers
              if not RecLocked
                 exit perform
              end-if

              move 1 to geslock-v-riprova
              move 0 to geslock-v-ignora
              move 1 to geslock-v-termina
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova continue
              when termina 
                   set errori to true
                   exit perform
              end-evaluate
           end-perform.  
           if tutto-ok
              perform COMPONI-PATH
              move path-file to path-sto-contestazioni
              open i-o sto-contestazioni
              if status-sto-contestazioni = "35"
                 open output sto-contestazioni
                 close       sto-contestazioni
                 open i-o    sto-contestazioni
              end-if
           end-if.         

      ***---
       OPEN-SORDFORN-LOCK.
           string   "Il file dei solleciti ordini f" 
             x"0d0a""risulta in uso su altro terminale."
                 delimited size
                 into geslock-messaggio
           end-string.
           move "sordforn" to geslock-nome-file.

           set tutto-ok   to true.
           perform until 1 = 2
              set RecLocked to false
              open i-o sordforn allowing readers
              if not RecLocked
                 exit perform
              end-if

              move 1 to geslock-v-riprova
              move 0 to geslock-v-ignora
              move 1 to geslock-v-termina
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova continue
              when termina 
                   set errori to true
                   exit perform
              end-evaluate
           end-perform.                       
           if tutto-ok
              perform COMPONI-PATH
              move path-file to path-sto-sordforn
              open i-o sto-sordforn
              if status-sto-sordforn = "35"
                 open output sto-sordforn
                 close       sto-sordforn
                 open i-o    sto-sordforn
              end-if
           end-if.         

      ***---
       OPEN-TAGLI-LOCK.
           string   "Il file deli tagli" 
             x"0d0a""risulta in uso su altro terminale."
                 delimited size
                 into geslock-messaggio
           end-string.
           move "tagli" to geslock-nome-file.

           set tutto-ok   to true.
           perform until 1 = 2
              set RecLocked to false
              open i-o tagli allowing readers
              if not RecLocked
                 exit perform
              end-if

              move 1 to geslock-v-riprova
              move 0 to geslock-v-ignora
              move 1 to geslock-v-termina
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova continue
              when termina 
                   set errori to true
                   exit perform
              end-evaluate
           end-perform.                  
           if tutto-ok
              perform COMPONI-PATH
              move path-file to path-sto-tagli
              open i-o sto-tagli
              if status-sto-tagli = "35"
                 open output sto-tagli
                 close       sto-tagli
                 open i-o    sto-tagli
              end-if
           end-if.         
      
      ***---
       ELABORAZIONE.
           if storart-storicizzazione
              perform STORICIZZA
           else
              perform RIPRISTINA
           end-if.

      ***---
       STORICIZZA.                          
           move "TORDINI"       to nome-file.
           move 6               to line-file.
           move low-value       to tor-rec.
           move storart-anno-da to tor-anno.
           start tordini key >= tor-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tordini next at end exit perform end-read
                    if tor-anno > storart-anno-a
                       exit perform
                    end-if
                    perform CONTATORE-SCREEN
                    move tor-rec to sto-tor-rec
                    write sto-tor-rec 
                          invalid rewrite sto-tor-rec
                    end-write
                    delete tordini record
                 end-perform
           end-start.          

           move "RORDINI"       to nome-file.
           add  interlinea      to line-file.
           move low-value       to ror-rec.
           move storart-anno-da to ror-anno.
           move 0 to counter counter2.
           start rordini key >= ror-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rordini next at end exit perform end-read
                    if ror-anno > storart-anno-a
                       exit perform
                    end-if
                    perform CONTATORE-SCREEN 
                    move ror-rec to sto-ror-rec
                    write sto-ror-rec 
                          invalid rewrite sto-ror-rec
                    end-write
                    delete rordini record
                 end-perform
           end-start.    
                               
           move "MTORDINI"      to nome-file.
           add  interlinea      to line-file.
           move low-value       to mto-rec.
           move storart-anno-da to mto-anno.
           move 0 to counter counter2.
           start mtordini key >= mto-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read mtordini next at end exit perform end-read
                    if mto-anno > storart-anno-a
                       exit perform
                    end-if
                    perform CONTATORE-SCREEN 
                    move mto-rec to sto-mto-rec
                    write sto-mto-rec 
                          invalid rewrite sto-mto-rec
                    end-write
                    delete mtordini record
                 end-perform
           end-start.     

           move "MRORDINI"      to nome-file.
           add  interlinea      to line-file.
           move low-value       to mro-rec.
           move storart-anno-da to mro-anno.
           move 0 to counter counter2.
           start mrordini key >= mro-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read mrordini next at end exit perform end-read
                    if mro-anno > storart-anno-a
                       exit perform
                    end-if
                    perform CONTATORE-SCREEN  
                    move mro-rec to sto-mro-rec
                    write sto-mro-rec 
                          invalid rewrite sto-mro-rec
                    end-write
                    delete mrordini record
                 end-perform
           end-start.    
                               
           move "EDI-MTORDINI"  to nome-file.
           add  interlinea      to line-file.
           move low-value       to emto-rec.
           move storart-anno-da to emto-anno.
           move 0 to counter counter2.
           start edi-mtordini key >= emto-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read edi-mtordini next at end exit perform end-read
                    if emto-anno > storart-anno-a
                       exit perform
                    end-if
                    perform CONTATORE-SCREEN 
                    move emto-rec to sto-emto-rec
                    write sto-emto-rec 
                          invalid rewrite sto-emto-rec
                    end-write
                    delete edi-mtordini record
                 end-perform
           end-start.  
                               
           move "EDI-MRORDINI"  to nome-file.
           add  interlinea      to line-file.
           move low-value       to emro-rec.
           move storart-anno-da to emro-anno.
           move 0 to counter counter2.
           start edi-mrordini key >= emro-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read edi-mrordini next at end exit perform end-read
                    if emro-anno > storart-anno-a
                       exit perform
                    end-if
                    perform CONTATORE-SCREEN 
                    move emro-rec to sto-emro-rec
                    write sto-emro-rec 
                          invalid rewrite sto-emro-rec
                    end-write
                    delete edi-mrordini record
                 end-perform
           end-start.           

           move "TRASPORTI"     to nome-file.
           add  interlinea      to line-file.
           move low-value       to trs-rec.
           move storart-anno-da to trs-anno.
           move 0 to counter counter2.
           start trasporti key >= trs-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read trasporti next at end exit perform end-read
                    if trs-anno > storart-anno-a
                       exit perform
                    end-if
                    perform CONTATORE-SCREEN  
                    move trs-rec to sto-trs-rec
                    write sto-trs-rec 
                          invalid rewrite sto-trs-rec
                    end-write
                    delete trasporti record
                 end-perform
           end-start.    

           move "TMOVMAG"       to nome-file.
           add  interlinea      to line-file.
           move low-value       to tmo-rec.
           move storart-anno-da to tmo-anno.
           move 0 to counter counter2.
           start tmovmag key >= tmo-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tmovmag next at end exit perform end-read
                    if tmo-anno > storart-anno-a
                       exit perform
                    end-if
                    perform CONTATORE-SCREEN  
                    move tmo-rec to sto-tmo-rec
                    write sto-tmo-rec 
                          invalid rewrite sto-tmo-rec
                    end-write
                    delete tmovmag record
                 end-perform
           end-start.         

           move "RMOVMAG"       to nome-file.
           add  interlinea      to line-file.
           move low-value       to rmo-rec.
           move storart-anno-da to rmo-anno.
           move 0 to counter counter2.
           start rmovmag key >= rmo-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rmovmag next at end exit perform end-read
                    if rmo-anno > storart-anno-a
                       exit perform
                    end-if
                    perform CONTATORE-SCREEN  
                    move rmo-rec to sto-rmo-rec
                    write sto-rmo-rec 
                          invalid rewrite sto-rmo-rec
                    end-write
                    delete rmovmag record
                 end-perform
           end-start.        

           move "GIORMAG"       to nome-file.
           add  interlinea      to line-file.
           move low-value       to gio-rec.
           move storart-anno-da to gio-aaaa.
           move 0 to counter counter2.
           start giormag key >= k-data
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read giormag next at end exit perform end-read
                    if gio-aaaa > storart-anno-a
                       exit perform
                    end-if
                    perform CONTATORE-SCREEN  
                    move gio-rec to sto-gio-rec
                    write sto-gio-rec 
                          invalid rewrite sto-gio-rec
                    end-write
                    delete giormag record
                 end-perform
           end-start.    

           move "TEDI"          to nome-file.
           add  interlinea      to line-file.
           move low-value       to tedi-rec.
           move storart-anno-da to tedi-anno.
           move 0 to counter counter2.
           start tedi key >= tedi-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tedi next at end exit perform end-read
                    if tedi-anno > storart-anno-a
                       exit perform
                    end-if
                    perform CONTATORE-SCREEN 
                    move tedi-rec to sto-tedi-rec
                    write sto-tedi-rec 
                          invalid rewrite sto-tedi-rec
                    end-write
                    delete tedi record
                 end-perform      
           end-start.          

           move "REDI"          to nome-file.
           add  interlinea      to line-file.
           move low-value       to redi-rec.
           move storart-anno-da to redi-anno.
           move 0 to counter counter2.
           start redi key >= redi-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read redi next at end exit perform end-read
                    if redi-anno > storart-anno-a
                       exit perform
                    end-if
                    perform CONTATORE-SCREEN  
                    move redi-rec to sto-redi-rec
                    write sto-redi-rec 
                          invalid rewrite sto-redi-rec
                    end-write
                    delete redi record
                 end-perform
           end-start.          

           move "TNOTACR"       to nome-file.
           add  interlinea      to line-file.
           move low-value       to tno-rec.
           move storart-anno-da to tno-anno.
           move 0 to counter counter2.
           start tnotacr key >= tno-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tnotacr next at end exit perform end-read
                    if tno-anno > storart-anno-a
                       exit perform
                    end-if
                    perform CONTATORE-SCREEN    
                    move tno-rec to sto-tno-rec
                    write sto-tno-rec 
                          invalid rewrite sto-tno-rec
                    end-write
                    delete tnotacr record
                 end-perform
           end-start.         

           move "RNOTACR"       to nome-file.
           add  interlinea      to line-file.
           move low-value       to rno-rec.
           move storart-anno-da to rno-anno.
           move 0 to counter counter2.
           start rnotacr key >= rno-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rnotacr next at end exit perform end-read
                    if rno-anno > storart-anno-a
                       exit perform
                    end-if
                    perform CONTATORE-SCREEN       
                    move rno-rec to sto-rno-rec
                    write sto-rno-rec 
                          invalid rewrite sto-rno-rec
                    end-write
                    delete rnotacr record
                 end-perform
           end-start.    

           move "MOVUTF"        to nome-file.
           add  interlinea      to line-file.
           move low-value       to mov-rec.
           move storart-anno-da to mov-anno.
           move 0 to counter counter2.
           start movutf key >= mov-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read movutf next at end exit perform end-read
                    if mov-anno > storart-anno-a
                       exit perform
                    end-if
                    perform CONTATORE-SCREEN    
                    move mov-rec to sto-mov-rec
                    write sto-mov-rec 
                          invalid rewrite sto-mov-rec
                    end-write
                    delete movutf record
                 end-perform
           end-start.         

           move "TORDFORN"      to nome-file.
           add  interlinea      to line-file.
           move low-value       to tof-rec.
           move storart-anno-da to tof-anno.
           move 0 to counter counter2.
           start tordforn key >= tof-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tordforn next at end exit perform end-read
                    if tof-anno > storart-anno-a
                       exit perform
                    end-if
                    perform CONTATORE-SCREEN     
                    move tof-rec to sto-tof-rec
                    write sto-tof-rec 
                          invalid rewrite sto-tof-rec
                    end-write
                    delete tordforn record
                 end-perform
           end-start.         

           move "RORDFORN"      to nome-file.
           add  interlinea      to line-file.
           move low-value       to rof-rec.
           move storart-anno-da to rof-anno.
           move 0 to counter counter2.
           start rordforn key >= rof-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rordforn next at end exit perform end-read
                    if rof-anno > storart-anno-a
                       exit perform
                    end-if
                    perform CONTATORE-SCREEN      
                    move rof-rec to sto-rof-rec
                    write sto-rof-rec 
                          invalid rewrite sto-rof-rec
                    end-write
                    delete rordforn record
                 end-perform
           end-start.         

           move "NORDFORN"      to nome-file.
           add  interlinea      to line-file.
           move low-value       to nof-rec.
           move storart-anno-da to nof-anno.
           move 0 to counter counter2.
           start nordforn key >= nof-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read nordforn next at end exit perform end-read
                    if nof-anno > storart-anno-a
                       exit perform
                    end-if
                    perform CONTATORE-SCREEN        
                    move nof-rec to sto-nof-rec
                    write sto-nof-rec 
                          invalid rewrite sto-nof-rec
                    end-write
                    delete nordforn record
                 end-perform
           end-start.             

           move "SORDFORN"      to nome-file.
           add  interlinea      to line-file.
           move low-value       to sof-rec.
           move storart-anno-da to sof-anno.
           move 0 to counter counter2.
           start sordforn key >= sof-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read sordforn next at end exit perform end-read
                    if sof-anno > storart-anno-a
                       exit perform
                    end-if
                    perform CONTATORE-SCREEN     
                    move sof-rec to sto-sof-rec
                    write sto-sof-rec 
                          invalid rewrite sto-sof-rec
                    end-write
                    delete sordforn record
                 end-perform     
           end-start.                         

           move "EORDINI"       to nome-file.
           add  interlinea      to line-file.
           move low-value       to eor-rec.
           move storart-anno-da to eor-anno.
           move 0 to counter counter2.
           start eordini key >= eor-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read eordini next at end exit perform end-read
                    if eor-anno > storart-anno-a
                       exit perform
                    end-if
                    perform CONTATORE-SCREEN      
                    move eor-rec to sto-eor-rec
                    write sto-eor-rec 
                          invalid rewrite sto-eor-rec
                    end-write
                    delete eordini record
                 end-perform
           end-start.

           move "PROVVIG"       to nome-file.
           add  interlinea      to line-file.
           move low-value       to pvv-rec.
           move storart-anno-da to pvv-anno-fat.
           move 0 to counter counter2.
           start provvig key >= pvv-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read provvig next at end exit perform end-read
                    if pvv-anno-fat > storart-anno-a
                       exit perform
                    end-if
                    perform CONTATORE-SCREEN      
                    move pvv-rec to sto-pvv-rec
                    write sto-pvv-rec 
                          invalid rewrite sto-pvv-rec
                    end-write
                    delete provvig record
                 end-perform
           end-start.                 

           move "TEVA"          to nome-file.
           add  interlinea      to line-file.
           move low-value       to teva-rec.
           move storart-anno-da to teva-anno.
           move 0 to counter counter2.
           start teva key >= teva-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read teva next at end exit perform end-read
                    if teva-anno > storart-anno-a
                       exit perform
                    end-if
                    perform CONTATORE-SCREEN       
                    move teva-rec to sto-teva-rec
                    write sto-teva-rec 
                          invalid rewrite sto-teva-rec
                    end-write
                    delete teva record
                 end-perform
           end-start.      

           move "REVA"          to nome-file.
           add  interlinea      to line-file.
           move low-value       to reva-rec.
           move storart-anno-da to reva-anno.
           move 0 to counter counter2.
           start reva key >= reva-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read reva next at end exit perform end-read
                    if reva-anno > storart-anno-a
                       exit perform
                    end-if
                    perform CONTATORE-SCREEN        
                    move reva-rec to sto-reva-rec
                    write sto-reva-rec 
                          invalid rewrite sto-reva-rec
                    end-write
                    delete reva record
                 end-perform
           end-start.      

           move "BTNOTACR"      to nome-file.
           add  interlinea      to line-file.
           move low-value       to btno-rec.
           move storart-anno-da to btno-anno.
           move 0 to counter counter2.
           start btnotacr key >= btno-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read btnotacr next at end exit perform end-read
                    if btno-anno > storart-anno-a
                       exit perform
                    end-if
                    perform CONTATORE-SCREEN       
                    move btno-rec to sto-btno-rec
                    write sto-btno-rec 
                          invalid rewrite sto-btno-rec
                    end-write
                    delete btnotacr record
                 end-perform
           end-start.                

           move "BRNOTACR"      to nome-file.
           add  interlinea      to line-file.
           move low-value       to brno-rec.
           move storart-anno-da to brno-anno.
           move 0 to counter counter2.
           start brnotacr key >= brno-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read brnotacr next at end exit perform end-read
                    if brno-anno > storart-anno-a
                       exit perform
                    end-if
                    perform CONTATORE-SCREEN         
                    move brno-rec to sto-brno-rec
                    write sto-brno-rec 
                          invalid rewrite sto-brno-rec
                    end-write
                    delete brnotacr record
                 end-perform
           end-start.           

           move "CONTESTAZIONI" to nome-file.
           add  interlinea      to line-file.
           move low-value       to cnt-rec.
           move storart-anno-da to cnt-anno.
           move 0 to counter counter2.
           start contestazioni key >= cnt-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read contestazioni next at end exit perform end-read
                    if cnt-anno > storart-anno-a
                       exit perform
                    end-if
                    perform CONTATORE-SCREEN       
                    move cnt-rec to sto-cnt-rec
                    write sto-cnt-rec 
                          invalid rewrite sto-cnt-rec
                    end-write
                    delete contestazioni record
                 end-perform
           end-start.       

           move "TAGLI"         to nome-file.
           add  interlinea      to line-file.
           move low-value       to tag-rec.
           move storart-anno-da to tag-data.
           move 0 to counter counter2.
           start tagli key >= tag-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tagli next at end exit perform end-read
                    if tag-data(1:4) > storart-anno-a
                       exit perform
                    end-if
                    perform CONTATORE-SCREEN            
                    move tag-rec to sto-tag-rec
                    write sto-tag-rec 
                          invalid rewrite sto-tag-rec
                    end-write
                    delete tagli record
                 end-perform
           end-start.

      ***---
       RIPRISTINA.                   
           move "TORDINI"       to nome-file.
           move 6               to line-file.
           move low-value       to sto-tor-rec.
           move storart-anno-da to sto-tor-anno.
           start sto-tordini key >= sto-tor-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read sto-tordini next at end exit perform end-read
                    if sto-tor-anno > storart-anno-a
                       exit perform
                    end-if
                    perform CONTATORE-SCREEN
                    move sto-tor-rec to tor-rec
                    write tor-rec 
                          invalid rewrite tor-rec
                    end-write
                    delete sto-tordini record
                 end-perform
           end-start.  
                               
           move "RORDINI"       to nome-file.
           add  interlinea      to line-file.
           move low-value       to sto-ror-rec.
           move storart-anno-da to sto-ror-anno.
           move 0 to counter counter2.
           start sto-rordini key >= sto-ror-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read sto-rordini next at end exit perform end-read
                    if sto-ror-anno > storart-anno-a
                       exit perform
                    end-if
                    perform CONTATORE-SCREEN 
                    move sto-ror-rec to ror-rec
                    write ror-rec 
                          invalid rewrite ror-rec
                    end-write
                    delete sto-rordini record
                 end-perform
           end-start. 
        
           move "MTORDINI"      to nome-file.
           add  interlinea      to line-file.
           move low-value       to sto-mto-rec.
           move storart-anno-da to sto-mto-anno.
           start sto-mtordini key >= sto-mto-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read sto-mtordini next at end exit perform end-read
                    if sto-mto-anno > storart-anno-a
                       exit perform
                    end-if
                    perform CONTATORE-SCREEN
                    move sto-mto-rec to mto-rec
                    write mto-rec 
                          invalid rewrite mto-rec
                    end-write
                    delete sto-mtordini record
                 end-perform
           end-start.  
                               
           move "MRORDINI"      to nome-file.
           add  interlinea      to line-file.
           move low-value       to sto-mro-rec.
           move storart-anno-da to sto-mro-anno.
           move 0 to counter counter2.
           start sto-mrordini key >= sto-mro-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read sto-mrordini next at end exit perform end-read
                    if sto-mro-anno > storart-anno-a
                       exit perform
                    end-if
                    perform CONTATORE-SCREEN 
                    move sto-mro-rec to mro-rec
                    write mro-rec 
                          invalid rewrite mro-rec
                    end-write
                    delete sto-mrordini record
                 end-perform
           end-start.    
        
           move "EDI-MTORDINI"  to nome-file.
           add  interlinea      to line-file.
           move low-value       to sto-emto-rec.
           move storart-anno-da to sto-emto-anno.
           start sto-edi-mtordini key >= sto-emto-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read sto-edi-mtordini next 
                         at end exit perform 
                    end-read
                    if sto-emto-anno > storart-anno-a
                       exit perform
                    end-if
                    perform CONTATORE-SCREEN
                    move sto-emto-rec to emto-rec
                    write emto-rec 
                          invalid rewrite emto-rec
                    end-write
                    delete sto-edi-mtordini record
                 end-perform
           end-start.    
        
           move "EDI-MRORDINI"  to nome-file.
           add  interlinea      to line-file.
           move low-value       to sto-emro-rec.
           move storart-anno-da to sto-emro-anno.
           start sto-edi-mrordini key >= sto-emro-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read sto-edi-mrordini next 
                         at end exit perform 
                    end-read
                    if sto-emro-anno > storart-anno-a
                       exit perform
                    end-if
                    perform CONTATORE-SCREEN
                    move sto-emro-rec to emro-rec
                    write emro-rec 
                          invalid rewrite emro-rec
                    end-write
                    delete sto-edi-mrordini record
                 end-perform
           end-start.       
                               
           move "TRASPORTI"     to nome-file.
           add  interlinea      to line-file.
           move low-value       to sto-trs-rec.
           move storart-anno-da to sto-trs-anno.
           move 0 to counter counter2.
           start sto-trasporti key >= sto-trs-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read sto-trasporti next at end exit perform end-read
                    if sto-trs-anno > storart-anno-a
                       exit perform
                    end-if
                    perform CONTATORE-SCREEN 
                    move sto-trs-rec to trs-rec
                    write trs-rec 
                          invalid rewrite trs-rec
                    end-write
                    delete sto-trasporti record
                 end-perform
           end-start.                 
                               
           move "TMOVMAG"       to nome-file.
           add  interlinea      to line-file.
           move low-value       to sto-tmo-rec.
           move storart-anno-da to sto-tmo-anno.
           move 0 to counter counter2.
           start sto-tmovmag key >= sto-tmo-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read sto-tmovmag next at end exit perform end-read
                    if sto-tmo-anno > storart-anno-a
                       exit perform
                    end-if
                    perform CONTATORE-SCREEN 
                    move sto-tmo-rec to tmo-rec
                    write tmo-rec 
                          invalid rewrite tmo-rec
                    end-write
                    delete sto-tmovmag record
                 end-perform
           end-start.         
                               
           move "RMOVMAG"       to nome-file.
           add  interlinea      to line-file.
           move low-value       to sto-rmo-rec.
           move storart-anno-da to sto-rmo-anno.
           move 0 to counter counter2.
           start sto-rmovmag key >= sto-rmo-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read sto-rmovmag next at end exit perform end-read
                    if sto-rmo-anno > storart-anno-a
                       exit perform
                    end-if
                    perform CONTATORE-SCREEN 
                    move sto-rmo-rec to rmo-rec
                    write rmo-rec 
                          invalid rewrite rmo-rec
                    end-write
                    delete sto-rmovmag record
                 end-perform
           end-start.      

           move "GIORMAG"       to nome-file.
           add  interlinea      to line-file.
           move low-value       to sto-gio-rec.
           move storart-anno-da to sto-gio-aaaa.
           move 0 to counter counter2.
           start sto-giormag key >= k-data
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read sto-giormag next at end exit perform end-read
                    if sto-gio-aaaa > storart-anno-a
                       exit perform
                    end-if
                    perform CONTATORE-SCREEN  
                    move sto-gio-rec to gio-rec
                    write gio-rec 
                          invalid rewrite gio-rec
                    end-write
                    delete sto-giormag record
                 end-perform
           end-start.    

           move "TEDI"          to nome-file.
           add  interlinea      to line-file.
           move low-value       to sto-tedi-rec.
           move storart-anno-da to sto-tedi-anno.
           move 0 to counter counter2.
           start sto-tedi key >= sto-tedi-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read sto-tedi next at end exit perform end-read
                    if sto-tedi-anno > storart-anno-a
                       exit perform
                    end-if
                    perform CONTATORE-SCREEN 
                    move sto-tedi-rec to tedi-rec
                    write tedi-rec 
                          invalid rewrite tedi-rec
                    end-write
                    delete sto-tedi record
                 end-perform      
           end-start.          

           move "REDI"          to nome-file.
           add  interlinea      to line-file.
           move low-value       to sto-redi-rec.
           move storart-anno-da to sto-redi-anno.
           move 0 to counter counter2.
           start sto-redi key >= sto-redi-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read sto-redi next at end exit perform end-read
                    if sto-redi-anno > storart-anno-a
                       exit perform
                    end-if
                    perform CONTATORE-SCREEN  
                    move sto-redi-rec to redi-rec
                    write redi-rec 
                          invalid rewrite redi-rec
                    end-write
                    delete sto-redi record
                 end-perform
           end-start.          

           move "TNOTACR"       to nome-file.
           add  interlinea      to line-file.
           move low-value       to sto-tno-rec.
           move storart-anno-da to sto-tno-anno.
           move 0 to counter counter2.
           start sto-tnotacr key >= sto-tno-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read sto-tnotacr next at end exit perform end-read
                    if sto-tno-anno > storart-anno-a
                       exit perform
                    end-if
                    perform CONTATORE-SCREEN    
                    move sto-tno-rec to tno-rec
                    write tno-rec 
                          invalid rewrite tno-rec
                    end-write
                    delete sto-tnotacr record
                 end-perform
           end-start.         

           move "RNOTACR"       to nome-file.
           add  interlinea      to line-file.
           move low-value       to sto-rno-rec.
           move storart-anno-da to sto-rno-anno.
           move 0 to counter counter2.
           start sto-rnotacr key >= sto-rno-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read sto-rnotacr next at end exit perform end-read
                    if sto-rno-anno > storart-anno-a
                       exit perform
                    end-if
                    perform CONTATORE-SCREEN       
                    move sto-rno-rec to rno-rec
                    write rno-rec 
                          invalid rewrite rno-rec
                    end-write
                    delete sto-rnotacr record
                 end-perform
           end-start.    

           move "MOVUTF"        to nome-file.
           add  interlinea      to line-file.
           move low-value       to sto-mov-rec.
           move storart-anno-da to sto-mov-anno.
           move 0 to counter counter2.
           start sto-movutf key >= sto-mov-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read sto-movutf next at end exit perform end-read
                    if sto-mov-anno > storart-anno-a
                       exit perform
                    end-if
                    perform CONTATORE-SCREEN    
                    move sto-mov-rec to mov-rec
                    write mov-rec 
                          invalid rewrite mov-rec
                    end-write
                    delete sto-movutf record
                 end-perform
           end-start.         

           move "TORDFORN"      to nome-file.
           add  interlinea      to line-file.
           move low-value       to sto-tof-rec.
           move storart-anno-da to sto-tof-anno.
           move 0 to counter counter2.
           start sto-tordforn key >= sto-tof-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read sto-tordforn next at end exit perform end-read
                    if sto-tof-anno > storart-anno-a
                       exit perform
                    end-if
                    perform CONTATORE-SCREEN     
                    move sto-tof-rec to tof-rec
                    write tof-rec 
                          invalid rewrite tof-rec
                    end-write
                    delete sto-tordforn record
                 end-perform
           end-start.         

           move "RORDFORN"      to nome-file.
           add  interlinea      to line-file.
           move low-value       to sto-rof-rec.
           move storart-anno-da to sto-rof-anno.
           move 0 to counter counter2.
           start sto-rordforn key >= sto-rof-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read sto-rordforn next at end exit perform end-read
                    if sto-rof-anno > storart-anno-a
                       exit perform
                    end-if
                    perform CONTATORE-SCREEN      
                    move sto-rof-rec to rof-rec
                    write rof-rec 
                          invalid rewrite rof-rec
                    end-write
                    delete sto-rordforn record
                 end-perform
           end-start.         

           move "NORDFORN"      to nome-file.
           add  interlinea      to line-file.
           move low-value       to sto-nof-rec.
           move storart-anno-da to sto-nof-anno.
           move 0 to counter counter2.
           start sto-nordforn key >= sto-nof-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read sto-nordforn next at end exit perform end-read
                    if sto-nof-anno > storart-anno-a
                       exit perform
                    end-if
                    perform CONTATORE-SCREEN        
                    move sto-nof-rec to nof-rec
                    write nof-rec 
                          invalid rewrite nof-rec
                    end-write
                    delete sto-nordforn record
                 end-perform
           end-start.             

           move "SORDFORN"      to nome-file.
           add  interlinea      to line-file.
           move low-value       to sto-sof-rec.
           move storart-anno-da to sto-sof-anno.
           move 0 to counter counter2.
           start sto-sordforn key >= sto-sof-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read sto-sordforn next at end exit perform end-read
                    if sto-sof-anno > storart-anno-a
                       exit perform
                    end-if
                    perform CONTATORE-SCREEN     
                    move sto-sof-rec to sof-rec
                    write sof-rec 
                          invalid rewrite sof-rec
                    end-write
                    delete sto-sordforn record
                 end-perform     
           end-start.                         

           move "EORDINI"       to nome-file.
           add  interlinea      to line-file.
           move low-value       to sto-eor-rec.
           move storart-anno-da to sto-eor-anno.
           move 0 to counter counter2.
           start sto-eordini key >= sto-eor-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read sto-eordini next at end exit perform end-read
                    if sto-eor-anno > storart-anno-a
                       exit perform
                    end-if
                    perform CONTATORE-SCREEN      
                    move sto-eor-rec to eor-rec
                    write eor-rec 
                          invalid rewrite eor-rec
                    end-write
                    delete sto-eordini record
                 end-perform
           end-start.

           move "PROVVIG"       to nome-file.
           add  interlinea      to line-file.
           move low-value       to sto-pvv-rec.
           move storart-anno-da to sto-pvv-anno-fat.
           move 0 to counter counter2.
           start sto-provvig key >= sto-pvv-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read sto-provvig next at end exit perform end-read
                    if sto-pvv-anno-fat > storart-anno-a
                       exit perform
                    end-if
                    perform CONTATORE-SCREEN      
                    move sto-pvv-rec to pvv-rec
                    write pvv-rec 
                          invalid rewrite pvv-rec
                    end-write
                    delete sto-provvig record
                 end-perform
           end-start.                 

           move "TEVA"          to nome-file.
           add  interlinea      to line-file.
           move low-value       to sto-teva-rec.
           move storart-anno-da to sto-teva-anno.
           move 0 to counter counter2.
           start sto-teva key >= sto-teva-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read sto-teva next at end exit perform end-read
                    if sto-teva-anno > storart-anno-a
                       exit perform
                    end-if
                    perform CONTATORE-SCREEN       
                    move sto-teva-rec to teva-rec
                    write teva-rec 
                          invalid rewrite teva-rec
                    end-write
                    delete sto-teva record
                 end-perform
           end-start.      

           move "REVA"          to nome-file.
           add  interlinea      to line-file.
           move low-value       to sto-reva-rec.
           move storart-anno-da to sto-reva-anno.
           move 0 to counter counter2.
           start sto-reva key >= sto-reva-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read sto-reva next at end exit perform end-read
                    if sto-reva-anno > storart-anno-a
                       exit perform
                    end-if
                    perform CONTATORE-SCREEN        
                    move sto-reva-rec to reva-rec
                    write reva-rec 
                          invalid rewrite reva-rec
                    end-write
                    delete sto-reva record
                 end-perform
           end-start.      

           move "BTNOTACR"      to nome-file.
           add  interlinea      to line-file.
           move low-value       to sto-btno-rec.
           move storart-anno-da to sto-btno-anno.
           move 0 to counter counter2.
           start sto-btnotacr key >= sto-btno-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read sto-btnotacr next at end exit perform end-read
                    if sto-btno-anno > storart-anno-a
                       exit perform
                    end-if
                    perform CONTATORE-SCREEN       
                    move sto-btno-rec to btno-rec
                    write btno-rec 
                          invalid rewrite btno-rec
                    end-write
                    delete sto-btnotacr record
                 end-perform
           end-start.                

           move "BRNOTACR"      to nome-file.
           add  interlinea      to line-file.
           move low-value       to sto-brno-rec.
           move storart-anno-da to sto-brno-anno.
           move 0 to counter counter2.
           start sto-brnotacr key >= sto-brno-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read sto-brnotacr next at end exit perform end-read
                    if sto-brno-anno > storart-anno-a
                       exit perform
                    end-if
                    perform CONTATORE-SCREEN         
                    move sto-brno-rec to brno-rec
                    write brno-rec 
                          invalid rewrite brno-rec
                    end-write
                    delete sto-brnotacr record
                 end-perform
           end-start.           

           move "CONTESTAZIONI" to nome-file.
           add  interlinea      to line-file.
           move low-value       to sto-cnt-rec.
           move storart-anno-da to sto-cnt-anno.
           move 0 to counter counter2.
           start sto-contestazioni key >= sto-cnt-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read sto-contestazioni next 
                         at end exit perform 
                    end-read
                    if sto-cnt-anno > storart-anno-a
                       exit perform
                    end-if
                    perform CONTATORE-SCREEN       
                    move sto-cnt-rec to cnt-rec
                    write cnt-rec 
                          invalid rewrite cnt-rec
                    end-write
                    delete sto-contestazioni record
                 end-perform
           end-start.       

           move "TAGLI"         to nome-file.
           add  interlinea      to line-file.
           move low-value       to sto-tag-rec.
           move storart-anno-da to sto-tag-data.
           move 0 to counter counter2.
           start sto-tagli key >= sto-tag-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read sto-tagli next at end exit perform end-read
                    if sto-tag-data(1:4) > storart-anno-a
                       exit perform
                    end-if
                    perform CONTATORE-SCREEN            
                    move sto-tag-rec to tag-rec
                    write tag-rec 
                          invalid rewrite tag-rec
                    end-write
                    delete sto-tagli record
                 end-perform
           end-start.       

      ***---
       CONTATORE-SCREEN.
           add 1 to counter
           add 1 to counter2
           if counter2 = 50
              move counter to counter-edit                   
              display nome-file 
                      upon storart-handle at column 3,00
                                               line line-file
              display counter-edit 
                      upon storart-handle at column 31,50
                                               line line-file
              move 0 to counter2
           end-if.

      ***---
       CLOSE-FILES.                          
           unlock tordini        all records.
           unlock rordini        all records.
           unlock mtordini       all records.
           unlock mrordini       all records.
           unlock trasporti      all records.
           unlock tmovmag        all records.
           unlock rmovmag        all records.
           unlock giormag        all records.
           unlock tedi           all records.
           unlock redi           all records.
           unlock tnotacr        all records.
           unlock rnotacr        all records.
           unlock movutf         all records.
           unlock tordforn       all records.
           unlock rordforn       all records.
           unlock nordforn       all records. 
           unlock sordforn       all records.
           unlock eordini        all records.
           unlock provvig        all records.
           unlock teva           all records.
           unlock reva           all records.
           unlock btnotacr       all records.
           unlock brnotacr       all records. 
           unlock contestazioni  all records.
           unlock tagli          all records.
           unlock edi-mtordini   all records.
           unlock edi-mrordini   all records.

           close mtordini.            
           close mrordini.
           close tordini.
           close rordini.
           close trasporti.
           close tmovmag.
           close rmovmag.
           close giormag.
           close tedi.
           close redi.
           close tnotacr.
           close rnotacr.
           close movutf.
           close tordforn.
           close rordforn.  
           close nordforn.
           close sordforn.          
           close eordini.      
           close provvig.
           close teva.
           close reva.
           close btnotacr.
           close brnotacr.
           close contestazioni.
           close tagli.
           close edi-mtordini.
           close edi-mrordini.
                         
           unlock STO-tordini        all records.
           unlock STO-rordini        all records.
           unlock STO-mtordini       all records.
           unlock STO-mrordini       all records.
           unlock STO-trasporti      all records.
           unlock STO-tmovmag        all records.
           unlock STO-rmovmag        all records.
           unlock STO-giormag        all records.
           unlock STO-tedi           all records.
           unlock STO-redi           all records.
           unlock STO-tnotacr        all records.
           unlock STO-rnotacr        all records.
           unlock STO-movutf         all records.
           unlock STO-tordforn       all records.
           unlock STO-rordforn       all records.
           unlock STO-nordforn       all records.
           unlock STO-sordforn       all records.
           unlock STO-eordini        all records.
           unlock STO-provvig        all records.
           unlock STO-teva           all records.
           unlock STO-reva           all records.
           unlock STO-btnotacr       all records.
           unlock STO-brnotacr       all records.
           unlock STO-contestazioni  all records.
           unlock STO-tagli          all records.
           unlock STO-edi-mtordini   all records.
           unlock STO-edi-mrordini   all records.
    
           close STO-tordini.
           close STO-rordini.
           close STO-mtordini. 
           close STO-mrordini.     
           close STO-trasporti.
           close STO-tmovmag.
           close STO-rmovmag.  
           close STO-giormag.
           close STO-tedi.
           close STO-redi.
           close STO-tnotacr.
           close STO-rnotacr.
           close STO-movutf.
           close STO-tordforn.
           close STO-rordforn.
           close STO-nordforn.
           close STO-sordforn.
           close STO-eordini.  
           close STO-provvig.
           close STO-teva.
           close STO-reva.
           close STO-btnotacr.
           close STO-brnotacr. 
           close STO-contestazioni.
           close STO-tagli.       
           close STO-edi-mtordini.
           close STO-edi-mrordini.

           if tutto-ok                 
               move "tordini"       to nome-file
               perform CALL-REBUILD
               move "rordini"       to nome-file
               perform CALL-REBUILD                 
               move "mtordini"      to nome-file
               perform CALL-REBUILD  
               move "mrordini"      to nome-file
               perform CALL-REBUILD
               move "trasporti"     to nome-file
               perform CALL-REBUILD
               move "tmovmag"       to nome-file
               perform CALL-REBUILD
               move "rmovmag"       to nome-file
               perform CALL-REBUILD     
               move "giormag"       to nome-file
               perform CALL-REBUILD     
               move "tedi"          to nome-file
               perform CALL-REBUILD     
               move "redi"          to nome-file
               perform CALL-REBUILD     
               move "tnotacr"       to nome-file
               perform CALL-REBUILD     
               move "rnotacr"       to nome-file
               perform CALL-REBUILD     
               move "movutf"        to nome-file
               perform CALL-REBUILD     
               move "tordforn"      to nome-file
               perform CALL-REBUILD     
               move "rordforn"      to nome-file
               perform CALL-REBUILD             
               move "nordforn"      to nome-file
               perform CALL-REBUILD     
               move "sordforn"      to nome-file
               perform CALL-REBUILD       
               move "eordini"       to nome-file
               perform CALL-REBUILD     
               move "provvig"       to nome-file
               perform CALL-REBUILD     
               move "teva"          to nome-file
               perform CALL-REBUILD     
               move "reva"          to nome-file
               perform CALL-REBUILD     
               move "btnotacr"      to nome-file
               perform CALL-REBUILD     
               move "brnotacr"      to nome-file
               perform CALL-REBUILD              
               move "contestazioni" to nome-file
               perform CALL-REBUILD     
               move "tagli"         to nome-file
               perform CALL-REBUILD     
               move "EDI-mtordini"  to nome-file
               perform CALL-REBUILD     
               move "EDI-mrordini"  to nome-file
               perform CALL-REBUILD     
           end-if.   

      ***---
       CALL-REBUILD.      
           inspect nome-file replacing trailing spaces by low-value.        
           initialize comando.
           string path-vutil        delimited low-value
                  " "               delimited size
                  "-rebuild"        delimited size
                  " -a -q "         delimited size
                  path-archivi      delimited low-value
                  nome-file         delimited low-value
                  into comando
           end-string               
           call "C$SYSTEM" using comando, 4.
           initialize comando.
           string path-vutil        delimited low-value
                  " "               delimited size
                  "-rebuild"        delimited size
                  " -a -q "         delimited size
                  path-archivi-sto  delimited low-value
                  nome-file         delimited low-value
                  into comando
           end-string               
           call "C$SYSTEM" using comando, 4.

      ***---
       EXIT-PGM.
           goback.
