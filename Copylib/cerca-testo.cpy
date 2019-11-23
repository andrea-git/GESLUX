      ***---
       CERCA-TESTO.
           perform SCREEN-SEARCH-OPEN-ROUTINE.

           if ricerca
              move 0 to ResultSearch

              inquire form1-gd-1, cursor-y in riga

              inquire form1-gd-1, search-options in grid-search-options
              
              |direzione di ricerca
              set GRID-SEARCH-FORWARDS to true
              set GRID-SEARCH-WRAP     to true

              |Maiuscolo/minuscolo
              if MatchCase = 0
                 set GRID-SEARCH-IGNORE-CASE to true
              else                                                        
                 set GRID-SEARCH-IGNORE-CASE to false
              end-if

              |trova la parola nella cella
              set GRID-SEARCH-MATCH-ANY to true

              |ricerca nelle sole celle visible 
              |ignorando gli eventuali hidden-data
              set GRID-SEARCH-VISIBLE to true

              |non considerare quello su cui sono posizionato
              set GRID-SEARCH-SKIP-CURRENT   to true

              |si posiziona sulla cella trovata
              set GRID-SEARCH-MOVES-CURSOR   to true 

              |num colonna sulla quale fare la ricerca
              move tipo-ricerca to GRID-SEARCH-COLUMN 

              modify form1-gd-1, search-options GRID-SEARCH-OPTIONS

              modify form1-gd-1(riga, tipo-ricerca), 
                     search-text TextToSearch |testo da ricercare
                          giving ResultSearch |1, 2 = trovato 0 = non trovato

              if ResultSearch = GRDSRCH-NOT-FOUND
                 display message box MSG-Testo-non-trovato
                         title = titolo
              else
                 inquire form1-gd-1, cursor-y in riga
                 move riga to event-data-2
                 perform SPOSTAMENTO
              end-if
           end-if.
