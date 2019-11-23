       IDENTIFICATION DIVISION.
       PROGRAM-ID.  RunProg is initial program.

       Working-Storage Section.
       copy "Acugui.def".
       copy "link-readutente.def".

       77  open-key-handle         usage unsigned-long.
       77  sam-desired             usage unsigned-long.
       77  subkey-handle           usage unsigned-long.
       77  data-type               usage unsigned-long.
       77  data-size               usage unsigned-long.
       77  subkey-to-be-opened     pic x(256).
       77  value-data              pic x(256).
       77  new-registry            pic x(256).
       77  value-key               pic x(256).
       77  row-command             pic x(256).
       77  descr-prog              pic x(50).
       77  var-prog                pic x(20).
       77  status-code             pic s9.
       77  status-run              pic s9.
       77  cont                    pic 99.
       77  row-empty               pic x.
       77  user-codi               pic x(10).

       01  CurrentDate.
           03 aaaa                     pic 9999.
           03 mm                       pic 99.
           03 gg                       pic 99.
       01  CurrentDateRet.
           03 gg-ret                   pic 99.
           03 mm-ret                   pic 99.
           03 aa-ret                   pic 9999.
       01  CurrentDateReturn           redefines CurrentDateRet
                                       pic 9(8).
       01  tool-data-w                 pic 9(8) is external.
       01  Calgiorno                   pic 9.
       01  CalColonna                  pic s999v99 value 998.
       01  CalRiga                     pic s999v99 value 998.
       
       Linkage Section.
       77  par-call                pic 9999.

      *****************************************************************
       PROCEDURE DIVISION using par-call.
       MainLanc.
           initialize cont.
           evaluate par-call
           when 9001
                accept row-command from environment "PR-NOTEP"
                if row-command = null or spaces
                   move "Notepad.exe" to row-command
                   set environment "PR-NOTEP" to row-command
                end-if
           when 9002
                accept row-command from environment "PR-CALC"
                if row-command = null or spaces
                   move "Calc.exe" to row-command
                   set environment "PR-CALC" to row-command
                end-if
           when 9003
                accept row-command from environment "PR-WORD"
                if row-command = null or spaces
                   perform FIND-REGISTRY-WORD
                end-if
           when 9004
                initialize ru-linkage replacing numeric data by zeroes
                                           alphanumeric data by spaces
                accept user-codi from environment "USER_CODI"
                move user-codi to ru-user
                call   "readutente" using ru-linkage
                cancel "readutente"
                evaluate true
                when ru-Office-2003
                     accept row-command from environment "PR-EXCEL_3"
                when ru-Office-2007
                    accept row-command  from environment "PR-EXCEL_7"
                when ru-Office-2010
                     accept row-command from environment "PR-EXCEL_10"
                end-evaluate
                if row-command = null or spaces
                   perform FIND-REGISTRY-EXCEL
                end-if
           when 9007
                accept row-command from environment "PR-MSDOS"
                if row-command = null or spaces
                   move "Command.com" to row-command
                   set environment "PR-MSDOS" to row-command
                end-if
           when 9005
                accept row-command from environment "PR-BROWS"
                if row-command = null or spaces
                   perform FIND-REGISTRY-BROWS
                end-if
           when 9006
                accept row-command from environment "PR-MAIL"
                if row-command = null or spaces
                   perform FIND-REGISTRY-MAIL
                end-if
           when 9008
                initialize row-command
                perform CALENDAR
           when other
                display message box "Parameter not valid!"
           end-evaluate.

           if par-call not = 9008
              if row-command not = spaces
      *****           call "C$RUN" using row-command giving status-run

      * ISACCO (ADATTAMENTO ANCHE PER ACUTHIN)
                 call "C$SYSTEM" using row-command, 129
                                giving Status-Run
                 if status-run not = 0
                    perform DISPLAY-MSG-BOX
                 end-if
              end-if
           end-if.

           goback.

      ***---
       FIND-REGISTRY-WORD.
      *****     move HKEY_CLASSES_ROOT          to open-key-handle.
           move HKEY_LOCAL_MACHINE     to open-key-handle.
           move KEY_EXECUTE            to sam-desired.
           move "Word.Document\CurVer" to subkey-to-be-opened.
           perform APRI-E-LEGGI-CHIAVE.
           if value-key not = spaces
              move value-key to new-registry
              string new-registry          delimited low-value
                     "\shell\open\command" delimited size
                into subkey-to-be-opened
              end-string
              perform APRI-E-LEGGI-CHIAVE
              if value-key not = spaces
                 move value-key            to row-command
                 set environment "PR-WORD" to row-command
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
              string new-registry         delimited by low-value
                     "\shell\open\command"  delimited by size
                into subkey-to-be-opened
              perform APRI-E-LEGGI-CHIAVE
              if value-key not = spaces
                 move value-key to row-command
                 set environment "PR-EXCEL" to row-command
              end-if
           end-if.

      ***---
       FIND-REGISTRY-BROWS.
           move HKEY_CLASSES_ROOT          to open-key-handle.
           move KEY_EXECUTE                to sam-desired.
           move "HTTP\shell\open\command"  to subkey-to-be-opened.
           perform APRI-E-LEGGI-CHIAVE.
           if value-key not = spaces
              move value-key to row-command
              set environment "PR-BROWS" to row-command
           end-if.

      ***---
       FIND-REGISTRY-MAIL.
           move HKEY_CLASSES_ROOT           to open-key-handle.
           move KEY_EXECUTE                 to sam-desired.
           move "mailto\shell\open\command" to subkey-to-be-opened.
           perform APRI-E-LEGGI-CHIAVE.
           if value-key not = spaces
              move value-key to row-command
              set environment "PR-MAIL" to row-command
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
       CALENDAR.
           CALL "SELDATA" USING CurrentDateRet, 1.


      ***---
       DISPLAY-MSG-BOX.
           evaluate par-call
           when 1  move "Notepad"                to descr-prog
                   move "PR-NOTEP"               to var-prog
           when 2  move "Calculator"             to descr-prog
                   move "PR-CALC"                to var-prog
           when 3  move "Word"                   to descr-prog
                   move "PR-WORD"                to var-prog
           when 4  move "Excel"                  to descr-prog
                   move "PR-EXCEL"               to var-prog
           when 5  move "MS-Dos"                 to descr-prog
                   move "PR-MSDOS"               to var-prog
           when 6  move "Browser"                to descr-prog
                   move "PR-BROWS"               to var-prog
           when 7  move "E-Mail"                 to descr-prog
                   move "PR-MAIL"                to var-prog
           end-evaluate.

           inspect descr-prog replacing trailing spaces by low-values.
           inspect var-prog   replacing trailing spaces by low-values.
           display message box
                   "Enable to find",
                   descr-prog,
                   " !",
                   title "Message"
                   .
