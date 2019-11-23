      ***---
       CALL-EXCEL.
           initialize ru-linkage replacing numeric data by zeroes
                                      alphanumeric data by spaces.
           move user-codi to ru-user.
           call   "readutente" using ru-linkage.
           cancel "readutente".
           evaluate true
           when ru-Office-2003
                accept row-command   from environment "PR-EXCEL_3"
           when ru-Office-2007
                accept row-command   from environment "PR-EXCEL_7"
           when ru-Office-2010
                accept row-command   from environment "PR-EXCEL_10"
           end-evaluate.
           accept path-to-excel from environment "PATH_TO_EXCEL".
           accept path-to-subst from environment "PATH_ST".

           if row-command = null or spaces
              perform FIND-REGISTRY-EXCEL
           end-if.
           if row-command not = spaces
              initialize cmd-lancio
              inspect row-command replacing trailing spaces
                                                  by low-value
              |Sostituisco il path locale col path di rete
              move 0 to cont
              move 0 to cont2

              inspect path-to-subst replacing trailing spaces 
                                                    by low-value
              inspect path-to-subst tallying cont  for characters 
                                                before low-value

              inspect path-to-excel replacing trailing spaces 
                                                    by low-value
              inspect path-to-excel tallying cont2 for characters 
                                                before low-value

              initialize path-launch
              move wstampa(cont + 1:)      to path-launch(cont2 + 1:)
              move path-to-excel           to path-launch(1:cont2)

              string row-command delimited by low-value
                     " "         delimited by size
                     path-launch delimited by size
                     into cmd-lancio
              end-string
              call "C$SYSTEM" using cmd-lancio, 129
                             giving Status-Run
              if status-run not = 0
                 initialize cmd-lancio
                 inspect path-launch 
                         replacing trailing spaces by low-value
                 string "START "      delimited size
                       x"22222022"    delimited size
                         path-launch  delimited low-value
                       x"22"          delimited size
                   into cmd-lancio
                 end-string
                 call "C$SYSTEM" using cmd-lancio, 225
                                giving status-run
                 if status-run not = 0
                    perform DISPLAY-MSG-BOX
                 end-if
              end-if
           end-if.

      ***---
       FIND-REGISTRY-EXCEL.
           move HKEY_CLASSES_ROOT          to open-key-handle.
           move KEY_EXECUTE                to sam-desired.
           move "Excel.Sheet\CurVer"       to subkey-to-be-opened.
           perform APRI-E-LEGGI-CHIAVE.
           if value-key not = spaces
              move value-key to new-registry
              string new-registry           delimited by low-value
                     "\shell\open\command"  delimited by size
                into subkey-to-be-opened
              perform APRI-E-LEGGI-CHIAVE
              if value-key not = spaces
                 move value-key to row-command
                 set environment "PR-EXCEL" to row-command
              end-if
           end-if.

      ***---
       APRI-E-LEGGI-CHIAVE.
           initialize value-key row-command.
           inspect subkey-to-be-opened replacing trailing spaces
                                       by low-values.
           CALL "REG_OPEN_KEY_EX"  using open-key-handle
                                         subkey-to-be-opened
                                         sam-desired
                                         subkey-handle
                                  giving status-code.
           if status-code = 0
              move low-value        to value-data
              move low-value        to subkey-to-be-opened
              move REG_NONE         to data-type
              set data-size to size of value-data
              CALL "REG_QUERY_VALUE_EX" using  subkey-handle
                                               subkey-to-be-opened
                                               data-type
                                               value-data
                                               data-size
                                        giving status-code
              if status-code = 0
                 if value-data(1:1) = x"22"
                    inspect value-data tallying cont for all x"22"
                    if cont > 0
                       unstring value-data delimited by x"22"
                           into row-empty
                                value-key
                    else
                       unstring value-data delimited by " "
                           into value-key
                    end-if
                 else
                    unstring value-data delimited by " "
                        into value-key
                 end-if
              else
                 perform DISPLAY-MSG-BOX
              end-if
              CALL "REG_CLOSE_KEY"  using subkey-handle
                                   giving status-code
           else
              perform DISPLAY-MSG-BOX
           end-if.

      ***---
       DISPLAY-MSG-BOX.
           move "Excel"                  to descr-prog.
           move "PR-EXCEL"               to var-prog.

           inspect descr-prog replacing trailing spaces by low-values.
           inspect var-prog   replacing trailing spaces by low-values.
           display message box "Enable to find ", descr-prog, " !",
                     title titolo
                      icon 2.
      ***---
       ACCETTA-SEPARATORE.
           accept separatore from environment "SEPARATORE"
                  on exception move "," to separatore
              not on exception
                  if separatore = space
                     move "," to separatore
                  end-if
           end-accept.
