       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      rentrial.

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.                          

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.         

       WORKING-STORAGE SECTION.
           copy "acucobol.def".                               

       78  dir-m                 value
           "C:\Users\andre\Music\Heavy\".         

       77  nFiles                pic 9999 value 0.
       77  n                     pic 9999 value 0.
       77  el-n                  pic x occurs 9999.

       77  dir-Handle            handle.
       77  cmd                   pic x(1000).
       77  nome-file             pic x(300).
       77  nome-file2            pic x(300).
       77  MSG-Folder-Name       pic x(256)  value spaces.

      ******************************************************************
       PROCEDURE DIVISION.

      ***---
       MAIN-PRG.            
           call "C$LIST-DIRECTORY" using LISTDIR-OPEN,
                                   dir-m
                                   "*.mp3"

           move return-code to dir-handle.

           if dir-handle = 0
              goback
           end-if

           perform until 1 = 2
              call "C$LIST-DIRECTORY" using LISTDIR-NEXT,
                                            dir-handle,
                                            nome-file

              if nome-file = spaces exit perform end-if

              if nome-file = "."      or
                           = ".."     or
                           = "Backup" or
                           = ".DS_Store"
                 exit perform cycle
              end-if
              add 1 to nFiles
              move "Heavy - "  to nome-file2
              move nome-file to nome-file2(9:)
              initialize cmd                                          
              inspect nome-file  replacing trailing spaces by low-value
              inspect nome-file2 replacing trailing spaces by low-value
              string "move "    delimited size
                    x"22"       delimited size
                     dir-m      delimited size
                     nome-file  delimited low-value
                    x"22"       delimited size
                     " "        delimited size                  
                    x"22"       delimited size
                     dir-m      delimited size
                     nome-file2 delimited low-value
                    x"22"       delimited size
                into cmd
              end-string
              call "C$SYSTEM" using cmd, 225
           end-perform.

           call "C$LIST-DIRECTORY" using LISTDIR-CLOSE, dir-handle.
      *****     call "C$LIST-DIRECTORY" using LISTDIR-OPEN,
      *****                             dir-m
      *****                             "*.mp3"
      *****
      *****     move return-code to dir-handle.
      *****
      *****     if dir-handle = 0
      *****        goback
      *****     end-if
      *****
      *****     perform until 1 = 2
      *****        call "C$LIST-DIRECTORY" using LISTDIR-NEXT,
      *****                                      dir-handle,
      *****                                      nome-file
      *****
      *****        if nome-file = spaces exit perform end-if
      *****
      *****        if nome-file = "."      or
      *****                     = ".."     or
      *****                     = "Backup" or
      *****                     = ".DS_Store"
      *****           exit perform cycle
      *****        end-if
      *****
      *****        perform until 1 = 2
      *****           compute n = function random * (nFiles)
      *****           if n > nFiles stop "L" end-if
      *****           add 1 to n
      *****           if el-n(n) not = "X"
      *****              if n = 2 or 4 or 6 or 8 stop n end-if
      *****              move "X" to el-n(n)
      *****              exit perform
      *****           end-if
      *****        end-perform
      *****        move n         to nome-file2
      *****        move " - "     to nome-file2(5:)
      *****        move nome-file to nome-file2(9:)
      **********        move "Heavy-Gain-"  to nome-file2
      **********        move nome-file(7:)  to nome-file2(12:)
      *****        initialize cmd                                          
      *****        inspect nome-file  replacing trailing spaces by low-value
      *****        inspect nome-file2 replacing trailing spaces by low-value
      *****        string "move "    delimited size
      *****              x"22"       delimited size
      *****               dir-m      delimited size
      *****               nome-file  delimited low-value
      *****              x"22"       delimited size
      *****               " "        delimited size                  
      *****              x"22"       delimited size
      *****               dir-m      delimited size
      *****               nome-file2 delimited low-value
      *****              x"22"       delimited size
      *****          into cmd
      *****        end-string
      *****        call "C$SYSTEM" using cmd, 225
      *****     end-perform.

      *****     call "C$LIST-DIRECTORY" using LISTDIR-CLOSE, dir-handle.

           goback.
