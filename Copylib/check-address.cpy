      ***---
       CHECK-VALIDITA-INDIRIZZO.
           set tutto-ok to true.
           move 0 to FindChar ContaCrt.
           inspect como-mail replacing trailing spaces by low-value.
           inspect como-mail tallying ContaCrt
                             for characters before low-value.
           inspect como-mail tallying FindChar
                             for all "@"    before low-value.
           if FindChar = 1
              inspect como-mail tallying FindChar
                                for all ";"    before low-value
              inspect como-mail tallying FindChar
                                for all ","    before low-value
           end-if.

           if FindChar = 1
              move 0 to FindChar
              inspect como-mail tallying FindChar
                                for characters before "@"
              |Vuol dire che la chiocciola è l'ultimo o il primo crt
              if FindChar = ( ContaCrt - 1 ) or
                 FindChar = 0
                 set errori to true
              else
                 move 0   to FindChar
                 inspect como-mail tallying FindChar
                                   for all "." before low-value
                 if FindChar = 0
                    set errori to true
                 else
                    move 0 to FindChar
                    inspect como-mail tallying FindChar
                                      for characters before "@"
                    add 2 to FindChar
                    |Appena dopo "@" non dev'esserci il punto
                    if como-mail(FindChar:1) = "."
                       set errori to true
                    else
                       if como-mail(ContaCrt:1) = "."
                          set errori to true
                       else
                          set errori to true
                          |Salto il controllo sull'ultimo crt x'
                          |anche se fosse punto non m'interessa
                          |x' l'ho appena controllato
                          perform varying FindChar from FindChar by 1 
                                    until FindChar = ContaCrt
                             if como-mail(FindChar:1) = "."
                                if como-mail(FindChar + 1:1) not = "."
                                   set tutto-ok to true
                                else
                                   set errori to true
                                   exit perform
                                end-if
                             end-if
                          end-perform
                       end-if
                    end-if
                 end-if
              end-if
           else
              set errori to true
           end-if.
