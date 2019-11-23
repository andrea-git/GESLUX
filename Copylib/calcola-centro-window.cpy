      ***---
       CALCOLA-CENTRO-WINDOW.
           move fixed-font to textsize-font.

           set textsize-strip-spaces to true.

           call "W$TEXTSIZE" using  0, textsize-data.

           compute unit-x rounded = textsize-base-x / textsize-cells-x. 
           compute unit-y rounded = textsize-base-y / textsize-cells-y.

           compute Window-Size    = unit-x * window-Size.
           compute Window-Height  = unit-y * window-Height.


           accept terminal-abilities from terminal-info. 

           compute Window-col  = (physical-screen-width  - 
                                  Window-Size)  / 2.                             
           compute Window-line = (physical-screen-height - 
                                  Window-height) / 2.
