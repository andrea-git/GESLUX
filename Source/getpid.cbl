       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      getpid.

      ******************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
      *     copy "GetPid.def".
           decimal-point is comma.

       INPUT-OUTPUT SECTION.
       DATA DIVISION.


       WORKING-STORAGE SECTION.
      * 01 key-pressed pic 9(4) is special-names crt status.
       01  old-code-prefix pic x(4000).
       01  old-dll-convention pic 9.
       01  lk-pid pic 9(9) comp-5.
       77  pid-sessione pic z(10).

      ******************************************************************
       PROCEDURE DIVISION.
       Main Section.

           accept old-code-prefix from environment "code-prefix"
           accept old-dll-convention from environment "dll-convention"
           set environment "code-prefix" to "."
           set environment "dll-convention" to "1"
           call "msvcrt.dll"
           set environment "code-prefix" to old-code-prefix
           call "_getpid" giving  lk-pid
           cancel "msvcrt.dll"
           set environment "dll-convention" to old-dll-convention
           
           move lk-pid to pid-sessione       
           call "C$JUSTIFY" using pid-sessione, "L"

           set environment "PID_SESSIONE"   to pid-sessione

      *     display message box "PID processo corrente: "
      *                          como-pid

           goback.


