      ***---
       TRATTA-NUMERICO.
           set NotNumericFound to false.
           move 0 to como-numero.
           if NumericEDI = spaces
              exit paragraph
           end-if.
           call "C$JUSTIFY" using NumericEDI, "L".
           inspect NumericEDI replacing trailing spaces by low-value.
           move 0 to CountDot CountChar CountCharBeforeDot IdxChar. 
           inspect NumericEDI tallying CountDot 
                              for all 78-comma before low-value.
           inspect NumericEDI tallying CountChar
                              for characters before low-value.
           perform varying IdxChar from 1 by 1 
                     until IdxChar > CountChar
              if NumericEDI(IdxChar:1) is not numeric and 
                 NumericEDI(IdxChar:1) not = 78-comma
                 set NotNumericFound to true
                 exit perform
              end-if
           end-perform.

           initialize como-numero-x.
           evaluate CountDot
           when 0 
                move NumericEdi(1:CountChar) to como-int
                move "0"                     to como-dec
                call "C$JUSTIFY" using como-int, "R"
                inspect como-int replacing leading x"20" by x"30"
                move como-int to como-numero
           when 1                 
                inspect NumericEDI tallying CountCharBeforeDot
                                   for characters before 78-comma
                move NumericEdi(1:CountCharBeforeDot)  to como-int
                add 1 to CountCharBeforeDot
                compute CountChar = CountChar - CountCharBeforeDot
                add 1 to CountCharBeforeDot
                move NumericEdi(CountCharBeforeDot:CountChar)to como-dec
                inspect como-dec replacing trailing x"20" by x"30"
                call "C$JUSTIFY" using como-int, "R"
                inspect como-int replacing leading x"20" by x"30"
                move como-numero-x to como-numero-z
                move como-numero-z to como-numero
           end-evaluate.
