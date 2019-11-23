      ***---
       TROVA-CONTATORE-ANNO-ESERCIZIO.
           accept esercizio-x from environment "ESERCIZIO".
           move   esercizio-x to esercizio.

           set tutto-ok to true.
           open input tcontat.
           move esercizio to con-anno
           read tcontat no lock
                invalid
                display message "Contatori per l'anno "
                                "d'esericzio NON trovati!"
                         x"0d0a""IMPOSSIBILE PROCEDERE!!"
                          title tit-err
                           icon 3
                set errori to true
           end-read.
           close tcontat.
