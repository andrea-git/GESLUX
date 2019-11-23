       program-id. bianrio.
       file-control.
           select print-file
           assign to print "-P SPOOLER-DIRECT"
           line sequential.

           select file-da-stampare
           assign to NomeFile
           organization binary sequential
           access mode is sequential.

       file section.
       fd  print-file.
       01  print-line           pic x(32000).

       fd  file-da-stampare.
       01  linea                pic x(32000).

       working-storage section.
       01  NomeFile             pic x(80) 
           value "C:\WINDOWS\Temp\listadoc_20040227_15112235.txt".
       78  RecLen               value 32000.
       01  RecordInteri         pic 9(18).
       01  LunghezzaUltimo      pic 9(3).
       01  file-info.
           02 file-size         pic x(8) comp-x.
           02 file-date         pic 9(8) comp-x.
           02 file-time         pic 9(8) comp-x.
       procedure division.
       main-logic.
           call "C$FILEINFO" using NomeFile, file-info.
           divide RecLen into file-size
                  giving RecordInteri
                  remainder LunghezzaUltimo.
           set environment "STRIP_TRAILING_SPACES" to "1"
                           "MIN_REC_SIZE"          to RecLen.
           open input  file-da-stampare.
           open output print-file.
           display "Stampa in corso".
           perform RecordInteri times
              move spaces to linea
              read file-da-stampare
              move linea to print-line
              write print-line with no control
           end-perform.
           if LunghezzaUltimo > 0
              set environment "MIN_REC_SIZE" to LunghezzaUltimo
              move spaces to linea
              read file-da-stampare
              move linea to print-line
              write print-line with no control
           end-if.
           close file-da-stampare
           close print-file
           stop run.

