      ***---
       CHECK-MOUSE.
           move zero to flag-old-mouse.

           call "W$MOUSE" using  GET-MOUSE-SHAPE, return-code.

           if return-code not = ARROW-POINTER    
              move return-code to flag-old-mouse
              call "W$MOUSE" using SET-MOUSE-SHAPE, arrow-pointer
           end-if.

      ***---
       USCITA.
           if flag-old-mouse not = zero
              call "W$MOUSE" using SET-MOUSE-SHAPE, flag-old-mouse
           end-if.
