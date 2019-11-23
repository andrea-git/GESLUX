      ***---
       APRI-NOTE.                                           
           initialize opensave-data.
           if num-campo = 0
              inquire ef-note-agg, value in path-note
           end-if.

           if path-note = spaces
              accept path-note from environment "PATH-NOTE"           
              inspect path-note replacing trailing spaces by low-value
              move path-note to opnsav-default-dir
              initialize opnsav-filters
              string "TEXT files (*.txt)|*.txt"         delimited size
                     "|File Adobe PDF (*.pdf)|*.pdf"    delimited size
                     "|Documenti di Word (*.doc)|*.doc" delimited size
                     "|All files (*.*)|*.*"             delimited size
                     into  opnsav-filters
              end-string
              move 1 to opnsav-default-filter
              move "Geslux -  Selezione note aggiuntive" to opnsav-title
              call "C$OPENSAVEBOX" using opensave-save-box, 
                                         opensave-data,
                                  giving opensave-status
      
              if opensave-status = 1
                 if e-campo = 1
                    if num-campo = 0
                       move opnsav-filename to ef-note-agg-buf
                       display ef-note-agg
                    end-if
                 end-if
              end-if
           else
              initialize comando
              move path-note to opnsav-filename
              inspect path-note replacing trailing spaces by low-value
              string "START "   delimited size
                    x"22222022" delimited size
                     path-note  delimited low-value
                    x"22"       delimited size
                     into comando
              end-string
              call "C$SYSTEM" using comando, 64
                             giving ReturnCode
              if ReturnCode = -1
                 display message box "Comando fallito!"
                         title = tit-err
                         icon mb-error-icon
              end-if
           end-if.
