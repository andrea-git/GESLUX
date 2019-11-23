       PROGRAM-ID. Splash.   

       WORKING-STORAGE SECTION.
       77  cursore      pic 9.
      *handle e crt 
       77  h-bmp-splash pic S9(9) comp-4.
       77  crt-status is special-names crt status pic 9(5) VALUE ZERO.

       78  78-bmp-width   value 500.
       78  78-bmp-height  value 350.

      *misure
       77  bmp-width    pic 9(3) value 78-bmp-width.
       77  bmp-height   pic 9(3) value 78-bmp-height.
       77  splash-lines pic 9(3).
       77  splash-size  pic 9(3).
       77  splash-col   pic 9(3).
       77  splash-line  pic 9(3).
       77  label-line   pic 9(3).
       77  label-col    pic 9(3).
       77  label-size   pic 9(3). 
       77  unit-x pic 9(3).
       77  unit-y pic 9(3).

       77  path_splash    pic x(256).
      *messaggi trhead
       77  messaggio pic x.
           78 stop-splash value "s".
           78 sono-a-video value "b".

      *copy
       copy "acucobol.def".
       copy "acugui.def".
       copy "stdfonts.def".

       LINKAGE SECTION.
       77  h-splash     handle of window.
       

      *****
       PROCEDURE DIVISION using h-splash.
       MAIN.
           accept cursore from environment "cursor_mode".
           set environment "cursor_mode" to 2. |serve per l'accept omitted, viene ripristinato alla fine

           display floating window 
                   lines 10
                   size  20
                   handle h-splash
                   visible 0
                   .
      *centra la splash screen nello schermo  
           move fixed-font to textsize-font.
           move h-splash   to textsize-window.
           set textsize-strip-spaces to true.
           call "W$TEXTSIZE" using  0, textsize-data.
           compute unit-x rounded = textsize-base-x / textsize-cells-x. 
           compute unit-y rounded = textsize-base-y / textsize-cells-y.

           accept terminal-abilities from terminal-info. 
           compute splash-lines = (bmp-height + 2) / unit-y.
           compute splash-size  = (bmp-width + 2) / unit-x.
           compute splash-col   = (physical-screen-width - bmp-width) 
                                  / 2.                             
           compute splash-line  = (physical-screen-height - bmp-height)
                                  / 2.

           compute label-line   = splash-lines - 1.
           compute label-col    = 1.
           compute label-size   = splash-size.

           modify h-splash screen line   splash-line
                           screen column splash-col
                           lines         splash-lines
                           size          splash-size.

           accept path_splash from environment "PATH_SPLASH"
           call "W$BITMAP" using wbitmap-load, 
                            path_splash
                          giving h-bmp-splash.
           display bitmap line 1 
                          col 1 
                          lines bmp-height 
                          size bmp-width
                          bitmap-handle h-bmp-splash 
                          bitmap-number 1.
           display label  line label-line
                          col label-col
                          size label-size 
                          centered
                          transparent 
                          title "   Loading...".
      *ora la screen può essere mostrata
           modify h-splash visible 1. 
           send sono-a-video to last thread.
      *aspetta che finisca il programma main e killati 
           perform until 1 = 2
              accept omitted at 0101 allowing messages from last thread 
              if crt-status = 95
                 receive messaggio from last thread  before time 1
                 evaluate messaggio
                 when stop-splash exit perform
                 end-evaluate
              end-if
           end-perform.
           
           set environment "cursor_mode" to cursore.
           destroy h-splash.

           goback.


        
