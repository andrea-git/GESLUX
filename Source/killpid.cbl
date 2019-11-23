       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      killpid.

      ******************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
      *     copy "GetPid.def".
           decimal-point is comma.

       INPUT-OUTPUT SECTION.
       DATA DIVISION.


       WORKING-STORAGE SECTION.
       77 comando      PIC  x(200).
       77 path-kill    PIC  x(256).
       77 como-pid     PIC  z(10).
       77 status-call  USAGE IS SIGNED-SHORT.

       LINKAGE          SECTION.
           COPY "link-killpid.def".

      ******************************************************************
       PROCEDURE DIVISION using killpid-linkage.
       Main Section.
           initialize path-kill comando status-call.
           accept  path-kill from environment "PATH_KILL".
           inspect path-kill replacing trailing space by low-value


           move killpid-pid-pgm to como-pid
           call "C$JUSTIFY" using como-pid, "L"
           inspect como-pid replacing trailing spaces by low-value
           initialize comando
           string path-kill    delimited low-value
                  "pskill -t " delimited low-value
                  como-pid     delimited low-value
                  into comando
           end-string
           move 0 to status-call
           call "C$SYSTEM" using comando, 97
                          giving status-call

           goback.


