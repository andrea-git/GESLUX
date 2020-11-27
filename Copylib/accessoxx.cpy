      ***---
       ACCESSOXX.
           INITIALIZE WFONT-DATA font-accesso-handle
           call "nomepgm" using NomeProgrammaAccesso.
           MOVE 16                 TO WFONT-SIZE.
           if NomeProgrammaAccesso = "ricalimp-art"
              MOVE 10              TO WFONT-SIZE
           end-if.
           MOVE "Verdana"          TO WFONT-NAME
           SET WFCHARSET-DONT-CARE TO TRUE
           SET WFONT-BOLD          TO false
           SET WFONT-ITALIC        TO true
           SET WFONT-UNDERLINE     TO FALSE
           SET WFONT-STRIKEOUT     TO FALSE
           SET WFONT-FIXED-PITCH   TO FALSE
           MOVE 0                  TO WFONT-CHAR-SET
           CALL "W$FONT" USING WFONT-GET-FONT, 
                               font-accesso-handle, 
                               WFONT-DATA.

           inquire form1-handle, screen line   in LineaInizio, 
                                 screen column in ColonnaInizio.

           Display floating WINDOW
              SCREEN LINE   LineaInizio,
              SCREEN COLUMN ColonnaInizio,
              LINES 5,00,
              SIZE 55,00,
              HANDLE IS Form-accesso-Handle,              
              HEIGHT-IN-CELLS,
              WIDTH-IN-CELLS
              CONTROL FONT font-accesso-handle.

           evaluate true
           when SaveXX    
                move "SALVATAGGIO IN CORSO..."   to AccessType 
           when DeleteXX  
                move "CANCELLAZIONE IN CORSO..." to AccessType
           when PrintXX  
                move "STAMPA IN CORSO..."        to AccessType
           when InvioXX                                         
                move "INVIO IN CORSO..."      to AccessType
           when RicalcoloXX                                         
                move "RICALCOLO IN CORSO..."  to AccessType
           when other continue
           end-evaluate.

           DISPLAY LABEL UPON form-accesso-handle
              CENTER,
              COL 1, 
              LINE 2,
              LINES 3,
              SIZE 55,00 ,
              FONT IS font-accesso-handle,
              HEIGHT-IN-CELLS,
              WIDTH-IN-CELLS 
              TITLE AccessType.

      ***---
       DESTROYXX.
           destroy font-accesso-handle 
           destroy form-accesso-handle.
