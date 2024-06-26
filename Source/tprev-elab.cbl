      *{TOTEM}PRG-COMMENT
      * tprev-elab.Cbl
      * tprev-elab.Cbl is generated by TOTEM
      * This is a generated file. DO NOT modify this file directly.
      *{TOTEM}END
       IDENTIFICATION       DIVISION.
      *{TOTEM}PRGID
       PROGRAM-ID.          tprev-elab.
       AUTHOR.              ANDREA EVENTI.
       DATE-WRITTEN.        marted� 1 aprile 2014 19:20:17.
       REMARKS.
      *{TOTEM}END

       ENVIRONMENT          DIVISION.
       CONFIGURATION        SECTION.
       SPECIAL-NAMES.
      *{TOTEM}SPECIAL-NAME
      * <TOTEM:EPT. INIT:tprev-elab, INIT:tprev-elab, SpecialName>
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
           COPY  "LINK-TPREV-P.DEF".
       77 Verdana12B-Occidentale
                  USAGE IS HANDLE OF FONT.
       77 SCR-ELAB-TPREV-HANDLE
                  USAGE IS HANDLE OF WINDOW.

      ***********************************************************
      *   Code Gen's Buffer                                     *
      ***********************************************************
       77 STATUS-scr-elab-FLAG-REFRESH PIC  9.
          88 scr-elab-FLAG-REFRESH  VALUE 1 FALSE 0. 
      *{TOTEM}END

      *{TOTEM}ID-LOGICI
      ***** Elenco ID Logici *****
      ***** Fine ID Logici *****
      *{TOTEM}END

       LINKAGE          SECTION.
      *{TOTEM}LINKAGE
       01 link-user        PIC  x(10).
      *{TOTEM}END

       SCREEN           SECTION.
      *{TOTEM}COPY-SCREEN
      * FORM
       01 
           scr-elab, 
           HELP-ID 1,
           .

      * LABEL
       05
           scr-elab-La-2a, 
           Label, 
           COL 3,00, 
           LINE 2,50,
           LINES 1,90 CELLS,
           SIZE 45,90 CELLS,
           COLOR IS 1,
           FONT IS Verdana12B-Occidentale,
           ID IS 15,
           HEIGHT-IN-CELLS,
           WIDTH-IN-CELLS,
           CENTER,
           TRANSPARENT,
           TITLE "Creazione base dati in corso. Attendere prego...",
           .

      *{TOTEM}END

      *{TOTEM}LINKPARA
       PROCEDURE  DIVISION USING link-user.
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
           PERFORM scr-elab-OPEN-ROUTINE.
      *{TOTEM}END

      *{TOTEM}COPY-PROCEDURE
       EXIT-STOP-ROUTINE.
      * <TOTEM:EPT. INIT:tprev-elab, INIT:tprev-elab, BeforeDestroyResource>
      * <TOTEM:END>
           DESTROY Verdana12B-Occidentale
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
      * Verdana12B-Occidentale
           INITIALIZE WFONT-DATA Verdana12B-Occidentale
           MOVE 12 TO WFONT-SIZE
           MOVE "Verdana" TO WFONT-NAME
           SET WFCHARSET-DONT-CARE TO TRUE
           SET WFONT-BOLD TO TRUE
           SET WFONT-ITALIC TO FALSE
           SET WFONT-UNDERLINE TO FALSE
           SET WFONT-STRIKEOUT TO FALSE
           SET WFONT-FIXED-PITCH TO FALSE
           MOVE 0 TO WFONT-CHAR-SET
           CALL "W$FONT" USING WFONT-GET-FONT, 
                     Verdana12B-Occidentale, WFONT-DATA
           .

       INIT-BMP.
           .

       INIT-RES.
           .

       INIT-POPUP.
           .


       scr-elab-Open-Routine.
           PERFORM scr-elab-Scrn
           PERFORM scr-elab-Proc
           .

       scr-elab-Scrn.
           PERFORM scr-elab-Create-Win
           PERFORM scr-elab-Init-Value
           PERFORM scr-elab-Init-Data
      * Tab keystrok settings
      * Tool Bar
           PERFORM scr-elab-DISPLAY
           .

       scr-elab-Create-Win.
           Display Floating GRAPHICAL WINDOW
              LINES 5,00,
              SIZE 50,00,
              CELL HEIGHT 10,
              CELL WIDTH 10,
              HEIGHT-IN-CELLS,
              WIDTH-IN-CELLS,
              COLOR 65793,
              ERASE,
              LABEL-OFFSET 0,
              LINK TO THREAD,
              MODELESS,
              RESIZABLE,
              NO SCROLL,
              USER-GRAY,
              USER-WHITE,
              No WRAP,
              HANDLE IS SCR-ELAB-TPREV-HANDLE,
      * <TOTEM:EPT. FORM:scr-elab, FORM:scr-elab, AfterCreateWin>
      * <TOTEM:END>


      * Tool Bar    
      * Status-bar
           DISPLAY scr-elab UPON SCR-ELAB-TPREV-HANDLE
      * DISPLAY-COLUMNS settings
           .

       scr-elab-PROC.
      * <TOTEM:EPT. FORM:scr-elab, FORM:scr-elab, BeforeAccept>
           move link-user             to link-tprev-user.
           move scr-elab-tprev-handle to link-tprev-handle.
           call   "tprev-p"  using tprev-linkage.
           cancel "tprev-p".
           move 27 to key-status.

           .
      * <TOTEM:END>
           PERFORM UNTIL Exit-Pushed
              ACCEPT OMITTED LINE 1 COL 1
                 ON EXCEPTION
                    PERFORM scr-elab-Evaluate-Func
                 MOVE 1 TO TOTEM-Form-Index
              END-ACCEPT
      * <TOTEM:EPT. FORM:scr-elab, FORM:scr-elab, AfterEndAccept>
      * <TOTEM:END>
           END-PERFORM
      * <TOTEM:EPT. FORM:scr-elab, FORM:scr-elab, BeforeDestroyWindow>
      * <TOTEM:END>
           DESTROY SCR-ELAB-TPREV-HANDLE
           INITIALIZE Key-Status
           .

       scr-elab-Evaluate-Func.
      * <TOTEM:EPT. FORM:scr-elab, FORM:scr-elab, AfterAccept>
      * <TOTEM:END>
           EVALUATE TRUE
              WHEN Exit-Pushed
                 PERFORM scr-elab-Exit
              WHEN Event-Occurred
                 IF Event-Type = Cmd-Close
                    PERFORM scr-elab-Exit
                 END-IF
           END-EVALUATE
      * avoid changing focus
           MOVE 4 TO Accept-Control
           .

       scr-elab-CLEAR.
           PERFORM scr-elab-INIT-VALUE
           PERFORM scr-elab-DISPLAY
           .

       scr-elab-DISPLAY.
      * <TOTEM:EPT. FORM:scr-elab, FORM:scr-elab, BeforeDisplay>
      * <TOTEM:END>
           DISPLAY scr-elab UPON SCR-ELAB-TPREV-HANDLE
      * <TOTEM:EPT. FORM:scr-elab, FORM:scr-elab, AfterDisplay>
      * <TOTEM:END>
           .

       scr-elab-Exit.
      * for main screen
      * <TOTEM:EPT. FORM:scr-elab, FORM:scr-elab, BeforeExit>
      * <TOTEM:END>
           MOVE 27 TO Key-Status
           .

       scr-elab-Init-Data.
           MOVE 1 TO TOTEM-Form-Index
           MOVE 0 TO TOTEM-Frame-Index
           .

       scr-elab-Init-Value.
      * <TOTEM:EPT. FORM:scr-elab, FORM:scr-elab, SetDefault>
      * <TOTEM:END>
           PERFORM scr-elab-FLD-TO-BUF
           .


       scr-elab-ALLGRID-RESET.
           .

      * for Form's Validation
       scr-elab-VALIDATION-ROUTINE.
           SET TOTEM-CHECK-OK TO TRUE
           .


       scr-elab-Buf-To-Fld.
      * <TOTEM:EPT. FORM:scr-elab, FORM:scr-elab, BeforeBufToFld>
      * <TOTEM:END>
      * <TOTEM:EPT. FORM:scr-elab, FORM:scr-elab, AfterBufToFld>
      * <TOTEM:END>
           .

       scr-elab-Fld-To-Buf.
      * <TOTEM:EPT. FORM:scr-elab, FORM:scr-elab, BeforeFldToBuf>
      * <TOTEM:END>
      * <TOTEM:EPT. FORM:scr-elab, FORM:scr-elab, AfterFldToBuf>
      * <TOTEM:END>
           .

       scr-elab-CONTROLLO-OLD.
           set SiSalvato to true.
           if mod = 0 exit paragraph end-if.
           perform scr-elab-BUF-TO-FLD.
           move 0 to scelta.
           .
       scr-elab-EXTENDED-FILE-STATUS.
           CALL "C$RERRNAME" USING TOTEM-MSG-ERR-FILE
           CALL "C$RERR" USING EXTEND-STAT, TEXT-MESSAGE
           MOVE PRIMARY-ERROR TO TOTEM-MSG-ID
           PERFORM scr-elab-SHOW-MSG-ROUTINE
           .

       scr-elab-SHOW-MSG-ROUTINE.
           PERFORM SHOW-MSG-ROUTINE
           PERFORM scr-elab-DISPLAY-MESSAGE
           .

       scr-elab-DISPLAY-MESSAGE.
           PERFORM MESSAGE-BOX-ROUTINE
           DISPLAY MESSAGE BOX TOTEM-MSG-TEXT
               TITLE IS TOTEM-MSG-TITLE
               TYPE  IS TOTEM-MSG-BUTTON-TYPE
               ICON  IS TOTEM-MSG-DEFAULT-BUTTON
               RETURNING TOTEM-MSG-RETURN-VALUE
           .

       scr-elab-Save-Status.
           .             

       scr-elab-Restore-Status.
           .



      * USER DEFINE PARAGRAPH
      * EVENT PARAGRAPH

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

