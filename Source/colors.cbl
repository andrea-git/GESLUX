       program-id.                      colors.
       special-names. decimal-point is comma.
       working-storage section.
       copy "palette.def".

LUBEXX 77  test-version               pic x   value spaces.

       procedure division.
       CHANGE-COLOR.
LUBEXX     accept test-version from environment "TEST_VERSION".
LUBEXX     if test-version not = spaces
LUBEXX        display message "QUESTA E' UNA VERSIONE DI TEST!!!"
LUBEXX                  title "** ATTENZIONE **"
LUBEXX                   icon 2
LUBEXX     end-if.
                  
      *    GIALLO HIGH-DENSITY -->  GIALLINO
           initialize  wpalette-data.
           move  15 to wpal-color-id.
           move 255 to wpal-red.
           move 255 to wpal-green.
           move 175 to wpal-blue.

           call "w$palette" using wpalette-set-color, wpalette-data.

      *    RED LOW-DENSITY -->  ROSINO
           initialize  wpalette-data.
           move   5 to wpal-color-id.
           move 200 to wpal-red.
           move  50 to wpal-green.
           move  50 to wpal-blue.

           call "w$palette" using wpalette-set-color, wpalette-data.

      *    GREEN LOW-DENSITY -->  VERDONE
           initialize  wpalette-data.
           move   3 to wpal-color-id.
           move  11 to wpal-red.
           move 142 to wpal-green.
           move  11 to wpal-blue.

           call "w$palette" using wpalette-set-color, wpalette-data.

      *    GREEN HIGH-DENSITY --> VERDINO
           initialize  wpalette-data.
           move  11 to wpal-color-id.
           move 128 to wpal-red.
           move 255 to wpal-green.
           move 128 to wpal-blue.

           call "w$palette" using wpalette-set-color, wpalette-data.

      *    BLUE LOW-DENSITY -->  BLU
           initialize  wpalette-data.
           move   2 to wpal-color-id.
           move  18 to wpal-red.
           move  60 to wpal-green.
           move 181 to wpal-blue.

           call "w$palette" using wpalette-set-color, wpalette-data.

      *    DARK MAGENTA -->  ARANCIONE MOLTO CHIARO
           initialize  wpalette-data.
           move   6 to wpal-color-id.
           move 253 to wpal-red.
           move 217 to wpal-green.
           move 200 to wpal-blue.

           call "w$palette" using wpalette-set-color, wpalette-data.
                                                                    
      *    BLACK -->  DARK GREY
           initialize  wpalette-data.
           move   1 to wpal-color-id.
           move  33 to wpal-red.
           move  35 to wpal-green.
           move  41 to wpal-blue.

           call "w$palette" using wpalette-set-color, wpalette-data.

      *    CYAN HIGH-DENSITY  -->  AZZURRINO
           initialize  wpalette-data.
           move  12 to wpal-color-id.
           move 105 to wpal-red.
           move 166 to wpal-green.
           move 249 to wpal-blue.
           call "w$palette" using wpalette-set-color, wpalette-data.

      *    MAGENTA HIGH-DENSITY  -->  ARANCIONE
           initialize  wpalette-data.
           move  14 to wpal-color-id.
           move 250 to wpal-red.
           move 151 to wpal-green.
           move 96 to wpal-blue.
           call "w$palette" using wpalette-set-color, wpalette-data.

      *    DARK BROWN -->  BLU-GRIGIO SOFT
           initialize  wpalette-data.
           move   7 to wpal-color-id.
           move 155 to wpal-red.
           move 176 to wpal-green.
           move 193 to wpal-blue.
           call "w$palette" using wpalette-set-color, wpalette-data.
      *
           goback.
