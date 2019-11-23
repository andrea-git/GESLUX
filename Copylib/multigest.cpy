      ***---
      *IN caso di modifiche allineare anche pordini
       CONTROLLA-ESEGUIBILITA.
           set RecLocked to false.
           open i-o multigest.
           move spaces to mul-chiave.
           read multigest lock invalid continue end-read.
           if RecLocked
              read multigest no lock invalid continue end-read
              if mul-ricalcolo
                 display message "Ricalcolo impegnato in corso..."
                          x"0d0a""Attendere e poi riprovare"
                           title titolo
                            icon 2
                 close multigest
                 SET LK-BL-CANCELLAZIONE TO TRUE
                 MOVE COMO-PROG-ID       TO LK-BL-PROG-ID
                 CALL "BLOCKPGM"  USING LK-BLOCKPGM
                 goback
              end-if
           else
              set mul-funzione to true
              rewrite mul-rec
              read multigest lock
           end-if.
