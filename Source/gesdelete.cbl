      *{TOTEM}PRG-COMMENT
      * gesdelete.Cbl
      * gesdelete.Cbl is generated by TOTEM
      * This is a generated file. DO NOT modify this file directly.
      *{TOTEM}END
       IDENTIFICATION       DIVISION.
      *{TOTEM}PRGID
       PROGRAM-ID.          gesdelete.
       AUTHOR.              ANDREA EVENTI.
       DATE-WRITTEN.        marted� 1 aprile 2014 19:15:03.
       REMARKS.
      *{TOTEM}END

       ENVIRONMENT          DIVISION.
       CONFIGURATION        SECTION.
       SPECIAL-NAMES.
      *{TOTEM}SPECIAL-NAME
      * <TOTEM:EPT. INIT:gesdelete, INIT:gesdelete, SpecialName>
      * <TOTEM:END>
      *{TOTEM}END
      *{TOTEM}ACTIVEX-DEF
      *{TOTEM}END
      *{TOTEM}DECIMAL-POINT
           DECIMAL-POINT IS COMMA.
      *{TOTEM}END

       INPUT-OUTPUT         SECTION.
       FILE-CONTROL.
      *{TOTEM}FILE-CONTROL
      *{TOTEM}END
       DATA                 DIVISION.
       FILE                 SECTION.
      *{TOTEM}FILE
      *{TOTEM}END

       WORKING-STORAGE      SECTION.
      *{TOTEM}ACU-DEF
               COPY "acugui.def".
               COPY "acucobol.def".
               COPY "fonts.def".
               COPY "crtvars.def".
               COPY "showmsg.def".
               COPY "totem.def".
               COPY "F:\Lubex\GESLUX\Copylib\UTYDATA.DEF".
               COPY "F:\Lubex\GESLUX\Copylib\comune.def".
               COPY "F:\Lubex\GESLUX\Copylib\custom.def".
      *{TOTEM}END

      *{TOTEM}COPY-WORKING
      * Key Status
       77 Key-Status IS SPECIAL-NAMES CRT STATUS PIC 9(5) VALUE 0.
          88 Enter-Pushed VALUE 13.
          88 Exit-Pushed VALUE 27.
          88 Message-Received VALUE 95.
          88 Event-Occurred VALUE 96.
          88 Screen-No-Input-Field VALUE 97.
          88 Screen-Time-Out VALUE 99.
      * Properties & User defined Working Stoarge
       77 Screen1-Handle
                  USAGE IS HANDLE OF WINDOW.
       77 Default-Font
                  USAGE IS HANDLE OF FONT DEFAULT-FONT.
       77 Small-Font
                  USAGE IS HANDLE OF FONT SMALL-FONT.
       77 titolo           PIC  x(50).
       77 FORM1-HANDLE
                  USAGE IS HANDLE OF WINDOW.
       77 Screen1-Bt-1-Handle          PIC  S9(9)
                  USAGE IS COMP-4
                  VALUE IS 0.
       77 lab-messaggio-buf            PIC  X(50)
                  VALUE IS "Cancellazione file TARIFVET".
       77 Verdana12-Occidentale
                  USAGE IS HANDLE OF FONT.
       77 strip_gesdelete-bmp          PIC  S9(9)
                  USAGE IS COMP-4
                  VALUE IS 0.

      ***********************************************************
      *   Code Gen's Buffer                                     *
      ***********************************************************
       77 STATUS-Form1-FLAG-REFRESH PIC  9.
          88 Form1-FLAG-REFRESH  VALUE 1 FALSE 0. 
      *{TOTEM}END

      *{TOTEM}ID-LOGICI
      ***** Elenco ID Logici *****
      ***** Fine ID Logici *****
      *{TOTEM}END

       LINKAGE          SECTION.
      *{TOTEM}LINKAGE
           COPY  "LINK-GESDELETE.DEF".
      *{TOTEM}END

       SCREEN           SECTION.
      *{TOTEM}COPY-SCREEN
      * FORM
       01 
           Form1, 
           .

      * LABEL
       05
           Screen1-La-1, 
           Label, 
           COL 1,00, 
           LINE 3,22,
           LINES 1,31 ,
           SIZE 30,00 ,
           ID IS 1,
           HEIGHT-IN-CELLS,
           WIDTH-IN-CELLS,
           CENTER,
           TRANSPARENT,
           TITLE "Cosa s'intende cancellare?",
           .

      * LABEL
       05
           lab-messaggio, 
           Label, 
           COL 1,00, 
           LINE 1,44,
           LINES 1,13 ,
           SIZE 30,00 ,
           ID IS 3,
           HEIGHT-IN-CELLS,
           WIDTH-IN-CELLS,
           CENTER,
           TRANSPARENT,
           TITLE lab-messaggio-buf,
           .

      * FRAME
       05
           Screen1-Fr-1, 
           Frame, 
           COL 2,10, 
           LINE 4,33,
           LINES 8,44 ,
           SIZE 27,80 ,
           ID IS 2,
           HEIGHT-IN-CELLS,
           WIDTH-IN-CELLS,
           TITLE-POSITION 2,
           .

      * PUSH BUTTON
       05
           pb-tasto1, 
           Push-Button, 
           COL 6,90, 
           LINE 5,33,
           LINES 30,00 ,
           SIZE 174,00 ,
           BITMAP-HANDLE STRIP_GESDELETE-BMP,
           FRAMED,
           SQUARE,
           ENABLED 1,
           EXCEPTION-VALUE 1001,
           FLAT,
           ID IS 1000,
           SELF-ACT,
           TERMINATION-VALUE 13,
           VISIBLE gesdelete-v-tasto1,
           BITMAP-NUMBER gesdelete-bitmap-tasto1
           AFTER PROCEDURE pb-tasto1-AfterProcedure, 
           BEFORE PROCEDURE pb-tasto1-BeforeProcedure, 
           .

      * PUSH BUTTON
       05
           pb-tasto2, 
           Push-Button, 
           COL 6,90, 
           LINE 7,83,
           LINES 30,00 ,
           SIZE 174,00 ,
           BITMAP-HANDLE STRIP_GESDELETE-BMP,
           FRAMED,
           SQUARE,
           ENABLED 1,
           EXCEPTION-VALUE 1002,
           FLAT,
           ID IS 2000,
           NO-AUTO-DEFAULT,
           SELF-ACT,
           VISIBLE gesdelete-v-tasto2,
           BITMAP-NUMBER gesdelete-bitmap-tasto2
           AFTER PROCEDURE pb-tasto2-AfterProcedure, 
           BEFORE PROCEDURE pb-tasto2-BeforeProcedure, 
           .

      * PUSH BUTTON
       05
           pb-tasto3, 
           Push-Button, 
           COL 6,90, 
           LINE 10,33,
           LINES 30,00 ,
           SIZE 174,00 ,
           BITMAP-HANDLE STRIP_GESDELETE-BMP,
           FRAMED,
           SQUARE,
           ENABLED 1,
           EXCEPTION-VALUE 1000,
           FLAT,
           ID IS 3000,
           SELF-ACT,
           TERMINATION-VALUE 13,
           VISIBLE gesdelete-v-tasto3,
           BITMAP-NUMBER gesdelete-bitmap-tasto3
           AFTER PROCEDURE pb-tasto3-AfterProcedure, 
           BEFORE PROCEDURE pb-tasto3-BeforeProcedure, 
           .

      *{TOTEM}END

      *{TOTEM}LINKPARA
       PROCEDURE  DIVISION USING gesdelete-linkage.
      *{TOTEM}END

      *{TOTEM}DECLARATIVE
      *{TOTEM}END

       MAIN-LOGIC.
      *{TOTEM}ENTRY-BEFPRG
      *    Before-Program
      *    Before Program
      *{TOTEM}END
           PERFORM INITIALIZE-ROUTINE.
      * run main screen
      *{TOTEM}RUN-MAINSCR
           PERFORM Form1-OPEN-ROUTINE.
      *{TOTEM}END

      *{TOTEM}COPY-PROCEDURE
       EXIT-STOP-ROUTINE.
      * <TOTEM:EPT. INIT:gesdelete, INIT:gesdelete, BeforeDestroyResource>
      * <TOTEM:END>
           DESTROY Verdana12-Occidentale
           CALL "w$bitmap" USING WBITMAP-DESTROY, STRIP_GESDELETE-BMP
      *    After-Program
           EXIT PROGRAM TOTEM-PgmStatus
           STOP RUN.

       INITIALIZE-ROUTINE.
      *    Before Init
      * initialize program status variable
           Initialize TOTEM-PgmStatus.
      * get system information
           ACCEPT SYSTEM-INFORMATION FROM SYSTEM-INFO.
      * get terminal information
           ACCEPT TERMINAL-ABILITIES FROM TERMINAL-INFO.
      * set font
           PERFORM INIT-FONT.
      * load bitmap
           PERFORM INIT-BMP.
      * load resource
           PERFORM INIT-RES.
      * create pop-up menu
           PERFORM INIT-POPUP.
      *    After Init
           .
    
       INIT-FONT.
      * Verdana12-Occidentale
           INITIALIZE WFONT-DATA Verdana12-Occidentale
           MOVE 12 TO WFONT-SIZE
           MOVE "Verdana" TO WFONT-NAME
           SET WFCHARSET-DONT-CARE TO TRUE
           SET WFONT-BOLD TO FALSE
           SET WFONT-ITALIC TO FALSE
           SET WFONT-UNDERLINE TO FALSE
           SET WFONT-STRIKEOUT TO FALSE
           SET WFONT-FIXED-PITCH TO FALSE
           MOVE 0 TO WFONT-CHAR-SET
           CALL "W$FONT" USING WFONT-GET-FONT, 
                     Verdana12-Occidentale, WFONT-DATA
           .

       INIT-BMP.
      * pb-tasto1
           COPY RESOURCE "STRIP_GESDELETE.BMP".
           CALL "w$bitmap" USING WBITMAP-LOAD "STRIP_GESDELETE.BMP", 
                   GIVING STRIP_GESDELETE-BMP.
           .

       INIT-RES.
           .

       INIT-POPUP.
           .


       Form1-Open-Routine.
           PERFORM Form1-Scrn
           PERFORM Form1-Proc
           .

       Form1-Scrn.
           PERFORM Form1-Create-Win
           PERFORM Form1-Init-Value
           PERFORM Form1-Init-Data
      * Tab keystrok settings
      * Tool Bar
           PERFORM Form1-DISPLAY
           .

       Form1-Create-Win.
           Display Independent GRAPHICAL WINDOW
              LINES 12,56,
              SIZE 30,00,
              HEIGHT-IN-CELLS,
              WIDTH-IN-CELLS,
              COLOR 65793,
              CONTROL FONT Verdana12-Occidentale,
              LINK TO THREAD,
              NO SCROLL,
              TITLE-BAR,
              WITH SYSTEM MENU,
              USER-GRAY,
              USER-WHITE,
              No WRAP,
              EVENT PROCEDURE Screen1-Event-Proc,
              HANDLE IS FORM1-HANDLE,
      * <TOTEM:EPT. FORM:Form1, FORM:Form1, AfterCreateWin>
      * <TOTEM:END>


      * Tool Bar    
      * Status-bar
           DISPLAY Form1 UPON FORM1-HANDLE
      * DISPLAY-COLUMNS settings
           .

       Form1-PROC.
      * <TOTEM:EPT. FORM:Form1, FORM:Form1, BeforeAccept>
           move gesdelete-titolo to titolo.
                  
           initialize lab-messaggio-buf.
           call "C$TOUPPER" using gesdelete-nome-file, value 20
           string "Cancellazione file " delimited size
                  gesdelete-nome-file   delimited size
                  into lab-messaggio-buf
           end-string.

           move 3000 to control-id.
           move    4 to accept-control.

           display form1.

           modify form1-handle, title = titolo.

           .
      * <TOTEM:END>
           PERFORM UNTIL Exit-Pushed
              ACCEPT Form1
                 ON EXCEPTION
                    PERFORM Form1-Evaluate-Func
                 MOVE 1 TO TOTEM-Form-Index
              END-ACCEPT
      * <TOTEM:EPT. FORM:Form1, FORM:Form1, AfterEndAccept>
      * <TOTEM:END>
           END-PERFORM
      * <TOTEM:EPT. FORM:Form1, FORM:Form1, BeforeDestroyWindow>
      * <TOTEM:END>
           DESTROY FORM1-HANDLE
           INITIALIZE Key-Status
           .

       Form1-Evaluate-Func.
      * <TOTEM:EPT. FORM:Form1, FORM:Form1, AfterAccept>
      * <TOTEM:END>
           EVALUATE TRUE
              WHEN Exit-Pushed
                 PERFORM Form1-Exit
              WHEN Event-Occurred
                 IF Event-Type = Cmd-Close
                    PERFORM Form1-Exit
                 END-IF
              WHEN Key-Status = 1001
                 PERFORM pb-tasto1-LinkTo
              WHEN Key-Status = 1002
                 PERFORM pb-tasto2-LinkTo
              WHEN Key-Status = 1000
                 PERFORM pb-tasto3-LinkTo
           END-EVALUATE
      * avoid changing focus
           MOVE 4 TO Accept-Control
           .

       Form1-CLEAR.
           PERFORM Form1-INIT-VALUE
           PERFORM Form1-DISPLAY
           .

       Form1-DISPLAY.
      * <TOTEM:EPT. FORM:Form1, FORM:Form1, BeforeDisplay>
      * <TOTEM:END>
           DISPLAY Form1 UPON FORM1-HANDLE
      * <TOTEM:EPT. FORM:Form1, FORM:Form1, AfterDisplay>
      * <TOTEM:END>
           .

       Form1-Exit.
      * for main screen
      * <TOTEM:EPT. FORM:Form1, FORM:Form1, BeforeExit>
           perform PB-TASTO3-LINKTO.

           .
      * <TOTEM:END>
           MOVE 27 TO Key-Status
           .

       Form1-Init-Data.
           MOVE 1 TO TOTEM-Form-Index
           MOVE 0 TO TOTEM-Frame-Index
           .

       Form1-Init-Value.
      * <TOTEM:EPT. FORM:Form1, FORM:Form1, SetDefault>
      * <TOTEM:END>
           PERFORM Form1-FLD-TO-BUF
           .


       Form1-ALLGRID-RESET.
           .

      * for Form's Validation
       Form1-VALIDATION-ROUTINE.
           SET TOTEM-CHECK-OK TO TRUE
           .


       Form1-Buf-To-Fld.
      * <TOTEM:EPT. FORM:Form1, FORM:Form1, BeforeBufToFld>
      * <TOTEM:END>
      * <TOTEM:EPT. FORM:Form1, FORM:Form1, AfterBufToFld>
      * <TOTEM:END>
           .

       Form1-Fld-To-Buf.
      * <TOTEM:EPT. FORM:Form1, FORM:Form1, BeforeFldToBuf>
      * <TOTEM:END>
      * <TOTEM:EPT. FORM:Form1, FORM:Form1, AfterFldToBuf>
      * <TOTEM:END>
           .

       Form1-CONTROLLO-OLD.
           set SiSalvato to true.
           if mod = 0 exit paragraph end-if.
           perform Form1-BUF-TO-FLD.
           move 0 to scelta.
           .
       Form1-EXTENDED-FILE-STATUS.
           CALL "C$RERRNAME" USING TOTEM-MSG-ERR-FILE
           CALL "C$RERR" USING EXTEND-STAT, TEXT-MESSAGE
           MOVE PRIMARY-ERROR TO TOTEM-MSG-ID
           PERFORM Form1-SHOW-MSG-ROUTINE
           .

       Form1-SHOW-MSG-ROUTINE.
           PERFORM SHOW-MSG-ROUTINE
           PERFORM Form1-DISPLAY-MESSAGE
           .

       Form1-DISPLAY-MESSAGE.
           PERFORM MESSAGE-BOX-ROUTINE
           DISPLAY MESSAGE BOX TOTEM-MSG-TEXT
               TITLE IS TOTEM-MSG-TITLE
               TYPE  IS TOTEM-MSG-BUTTON-TYPE
               ICON  IS TOTEM-MSG-DEFAULT-BUTTON
               RETURNING TOTEM-MSG-RETURN-VALUE
           .

       Form1-Save-Status.
           .             

       Form1-Restore-Status.
           .



       Screen1-Event-Proc.
           .

      * USER DEFINE PARAGRAPH
      * EVENT PARAGRAPH
       pb-tasto1-BeforeProcedure.
      * <TOTEM:PARA. pb-tasto1-BeforeProcedure>
           modify pb-tasto1, bitmap-number = gesdelete-bitmap-tasto1 + 1
           .
      * <TOTEM:END>
       pb-tasto2-BeforeProcedure.
      * <TOTEM:PARA. pb-tasto2-BeforeProcedure>
           modify pb-tasto2, bitmap-number = gesdelete-bitmap-tasto2 + 1
           .
      * <TOTEM:END>
       pb-tasto3-BeforeProcedure.
      * <TOTEM:PARA. pb-tasto3-BeforeProcedure>
           modify pb-tasto3, bitmap-number = gesdelete-bitmap-tasto3 + 1
           .
      * <TOTEM:END>
       pb-tasto1-AfterProcedure.
      * <TOTEM:PARA. pb-tasto1-AfterProcedure>
           modify pb-tasto1, bitmap-number = gesdelete-bitmap-tasto1 
           .
      * <TOTEM:END>
       pb-tasto2-AfterProcedure.
      * <TOTEM:PARA. pb-tasto2-AfterProcedure>
           modify pb-tasto2, bitmap-number = gesdelete-bitmap-tasto2 
           .
      * <TOTEM:END>
       pb-tasto3-AfterProcedure.
      * <TOTEM:PARA. pb-tasto3-AfterProcedure>
           modify pb-tasto3, bitmap-number = gesdelete-bitmap-tasto3 
           .
      * <TOTEM:END>
       pb-tasto3-LinkTo.
      * <TOTEM:PARA. pb-tasto3-LinkTo>
           set gesdelete-no-delete to true.
           move 27 to key-status 
           .
      * <TOTEM:END>
       pb-tasto1-LinkTo.
      * <TOTEM:PARA. pb-tasto1-LinkTo>
           set gesdelete-yes-delete to true.
           move  1 to gesdelete-scelta.
           move 27 to key-status 
           .
      * <TOTEM:END>
       pb-tasto2-LinkTo.
      * <TOTEM:PARA. pb-tasto2-LinkTo>
           set gesdelete-yes-delete to true.
           move  2 to gesdelete-scelta.
           move 27 to key-status 
           .
      * <TOTEM:END>

      *{TOTEM}END

      *{TOTEM}SHOW-MSG
       MESSAGE-FOR-FILEERROR.
           CALL "C$RERRNAME" USING TOTEM-MSG-ERR-FILE
           CALL "C$RERR" USING EXTEND-STAT, TEXT-MESSAGE
              MOVE PRIMARY-ERROR TO TOTEM-MSG-ID
           .

       SHOW-TRANSACTION-ROUTINE.
           IF TOTEM-TRANSACTION-FLAG = SPACES
              IF TRANSACTION-STATUS NOT = 0
                 STRING "TRANSACTION ERROR ", TRANSACTION-STATUS
                    DELIMITED BY SIZE INTO TOTEM-MSG-1
              END-IF
           ELSE
              PERFORM SHOW-MSG-ROUTINE
              IF TRANSACTION-STATUS = 0
                 MOVE "Transazione terminata con Rollback." TO 
           TOTEM-MSG-3
              ELSE
                 STRING "TRANSACTION ERROR ", TRANSACTION-STATUS
                    DELIMITED BY SIZE INTO TOTEM-MSG-3
              END-IF
           END-IF
           .

       SHOW-MSG-ROUTINE.
           MOVE SPACE TO TOTEM-MSG-1 TOTEM-MSG-2 TOTEM-MSG-3
           EVALUATE TOTEM-MSG-ID
               WHEN "10"
                    STRING "File:" TOTEM-MSG-ERR-FILE DELIMITED BY SPACE
                       INTO TOTEM-MSG-1
                    MOVE "Nessun altro dato." TO TOTEM-MSG-2
                    MOVE MB-DEFAULT-ICON TO TOTEM-MSG-ICON-TYPE
                    MOVE MB-OK TO TOTEM-MSG-BUTTON-TYPE
               WHEN "22"
                    STRING "File:" TOTEM-MSG-ERR-FILE DELIMITED BY SPACE
                       INTO TOTEM-MSG-1
                    MOVE "Chiave Duplicata." TO TOTEM-MSG-2
                    MOVE MB-ERROR-ICON TO TOTEM-MSG-ICON-TYPE
                    MOVE MB-OK TO TOTEM-MSG-BUTTON-TYPE
               WHEN "23"
                    STRING "File:" TOTEM-MSG-ERR-FILE DELIMITED BY SPACE
                       INTO TOTEM-MSG-1
                    MOVE "Raggiunta fine file." TO TOTEM-MSG-2
                    MOVE MB-WARNING-ICON TO TOTEM-MSG-ICON-TYPE
                    MOVE MB-OK TO TOTEM-MSG-BUTTON-TYPE
               WHEN "101"
                    MOVE "Terminare l'applicazione?" TO TOTEM-MSG-1
                    MOVE 4 TO TOTEM-MSG-ICON-TYPE
                    MOVE MB-YES-NO TO TOTEM-MSG-BUTTON-TYPE
               WHEN "201"
                    MOVE "Aggiungere un nuovo record?" TO TOTEM-MSG-1
                    MOVE 4 TO TOTEM-MSG-ICON-TYPE
                    MOVE MB-YES-NO TO TOTEM-MSG-BUTTON-TYPE
               WHEN "202"
                    MOVE "Aggiornare il Record?" TO TOTEM-MSG-1
                    MOVE 4 TO TOTEM-MSG-ICON-TYPE
                    MOVE MB-YES-NO TO TOTEM-MSG-BUTTON-TYPE
               WHEN "203"
                    MOVE "Cancellare il Record?" TO TOTEM-MSG-1
                    MOVE 4 TO TOTEM-MSG-ICON-TYPE
                    MOVE MB-YES-NO TO TOTEM-MSG-BUTTON-TYPE
               WHEN "204"
                    MOVE "Chiave duplicata." TO TOTEM-MSG-1
                    MOVE MB-WARNING-ICON TO TOTEM-MSG-ICON-TYPE
                    MOVE MB-OK TO TOTEM-MSG-BUTTON-TYPE
               WHEN "301"
                    MOVE "Inserimento avvenuto con Successo." TO 
           TOTEM-MSG-1
                    MOVE MB-DEFAULT-ICON TO TOTEM-MSG-ICON-TYPE
                    MOVE MB-OK TO TOTEM-MSG-BUTTON-TYPE
               WHEN "302"
                    MOVE "Aggiornamento avvenuto con Successo." TO 
           TOTEM-MSG-1
                    MOVE MB-DEFAULT-ICON TO TOTEM-MSG-ICON-TYPE
                    MOVE MB-OK TO TOTEM-MSG-BUTTON-TYPE
               WHEN "303"
                    MOVE "Cancellazione avvenuta con Successo." TO 
           TOTEM-MSG-1
                    MOVE MB-DEFAULT-ICON TO TOTEM-MSG-ICON-TYPE
                    MOVE MB-OK TO TOTEM-MSG-BUTTON-TYPE
               WHEN "401"
                    MOVE "Shell non trovata." TO TOTEM-MSG-1
                    MOVE MB-ERROR-ICON TO TOTEM-MSG-ICON-TYPE
                    MOVE MB-OK TO TOTEM-MSG-BUTTON-TYPE
               WHEN OTHER
                    MOVE TEXT-MESSAGE TO TOTEM-MSG-1
                    STRING "FILE:" TOTEM-MSG-ERR-FILE DELIMITED BY SPACE
                       INTO TOTEM-MSG-3
                    MOVE 0 TO TOTEM-IDX1
                    INSPECT TOTEM-MSG-3 TALLYING TOTEM-IDX1
                       FOR TRAILING SPACE
                    STRING TOTEM-MSG-3(1:TOTEM-MSG-LENGTH - 
           TOTEM-IDX1), 
                       ", FILE STATUS ", PRIMARY-ERROR "," 
                       SECONDARY-ERROR
                    DELIMITED BY SIZE INTO TOTEM-MSG-2
                    MOVE SPACES TO TOTEM-MSG-3
                    MOVE MB-ERROR-ICON TO TOTEM-MSG-ICON-TYPE
                    MOVE MB-OK TO TOTEM-MSG-BUTTON-TYPE
           END-EVALUATE
           PERFORM MESSAGE-BOX-ROUTINE
           .

       MESSAGE-BOX-ROUTINE.
           MOVE 1 TO TOTEM-MSG-TEXT-POINTER
           IF TOTEM-MSG-1 NOT = SPACE
              MOVE 0 TO TOTEM-MSG-SIZE
              INSPECT TOTEM-MSG-1 TALLYING TOTEM-MSG-SIZE FOR TRAILING 
           SPACE
              STRING TOTEM-MSG-1( 1 : TOTEM-MSG-LENGTH - TOTEM-MSG-SIZE 
           )
                 DELIMITED BY SIZE
                 INTO TOTEM-MSG-TEXT, POINTER TOTEM-MSG-TEXT-POINTER
           END-IF

           IF TOTEM-MSG-2 NOT = SPACE
              MOVE 0 TO TOTEM-MSG-SIZE
              INSPECT TOTEM-MSG-2 TALLYING TOTEM-MSG-SIZE FOR TRAILING 
           SPACE
              IF TOTEM-MSG-TEXT-POINTER > 1
                 STRING X"0A" DELIMITED BY SIZE
                     INTO TOTEM-MSG-TEXT, POINTER TOTEM-MSG-TEXT-POINTER
              END-IF
              STRING TOTEM-MSG-2( 1 : TOTEM-MSG-LENGTH - TOTEM-MSG-SIZE 
           )
                  DELIMITED BY SIZE
                  INTO TOTEM-MSG-TEXT, POINTER TOTEM-MSG-TEXT-POINTER
           END-IF

           IF TOTEM-MSG-3 NOT = SPACE
              MOVE 0 TO TOTEM-MSG-SIZE
              INSPECT TOTEM-MSG-3 TALLYING TOTEM-MSG-SIZE FOR TRAILING 
           SPACE
              IF TOTEM-MSG-TEXT-POINTER > 1
                 STRING X"0A" DELIMITED BY SIZE
                     INTO TOTEM-MSG-TEXT, POINTER TOTEM-MSG-TEXT-POINTER
              END-IF
              STRING TOTEM-MSG-3( 1 : TOTEM-MSG-LENGTH - TOTEM-MSG-SIZE 
           )
                  DELIMITED BY SIZE
                  INTO TOTEM-MSG-TEXT, POINTER TOTEM-MSG-TEXT-POINTER
           END-IF

           IF TOTEM-MSG-TEXT-POINTER = 1
              MOVE 0 TO TOTEM-MSG-SIZE
              INSPECT TOTEM-MSG-TEXT TALLYING TOTEM-MSG-SIZE FOR 
           TRAILING SPACE
                  COMPUTE TOTEM-MSG-TEXT-POINTER = 
           TOTEM-MSG-FULL-LENGTH - TOTEM-MSG-SIZE + 1
           END-IF
           MOVE LOW-VALUES TO TOTEM-MSG-TEXT(TOTEM-MSG-TEXT-POINTER : 1 
           )
           .

       MESSAGE-BOX-DISPLAY.
           PERFORM SHOW-MSG-ROUTINE
           PERFORM MESSAGE-BOX-ROUTINE
           DISPLAY MESSAGE BOX TOTEM-MSG-TEXT
               TITLE IS TOTEM-MSG-TITLE
               TYPE  IS TOTEM-MSG-BUTTON-TYPE
               ICON  IS TOTEM-MSG-DEFAULT-BUTTON
               RETURNING TOTEM-MSG-RETURN-VALUE
           .

      *{TOTEM}END

