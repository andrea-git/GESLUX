       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      readutente.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "user.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "user.fd". 

       WORKING-STORAGE SECTION.
       77  status-user  pic xx.

       LINKAGE SECTION.
           copy "link-readutente.def".

      ******************************************************************
       PROCEDURE DIVISION USING ru-linkage.

       MAIN-PRG.
           perform INIT.
           perform OPEN-FILES.
           perform ELABORAZIONE.
           perform CLOSE-FILES.
           perform EXIT-PGM.

      ***---
       INIT.
           call "C$JUSTIFY" using ru-user, "R".
           inspect ru-user replacing trailing low-value by spaces.
           call "C$JUSTIFY" using ru-user, "L".
           initialize ru-SO ru-Office.

      ***---
       OPEN-FILES.
           open  input user.

      ***---
       ELABORAZIONE.
           move ru-user to user-cod.
           read user no lock invalid continue end-read.
      *    Quando schedulato da server, dal programma chiamante 
      *    dirotta la variabile su quella dedicata per il server
           move user-Office  to ru-Office.
           move user-SO      to ru-SO.

      ***---
       CLOSE-FILES.
           close user.

      ***---
       EXIT-PGM.
           goback.
