      *{TOTEM}PRG-COMMENT
      * prenbf.Cbl
      * prenbf.Cbl is generated by TOTEM
      * This is a generated file. DO NOT modify this file directly.
      *{TOTEM}END
       IDENTIFICATION       DIVISION.
      *{TOTEM}PRGID
       PROGRAM-ID.          prenbf.
       AUTHOR.              ANDREA EVENTI.
       DATE-WRITTEN.        gioved� 10 gennaio 2019 15:35:07.
       REMARKS.
      *{TOTEM}END

       ENVIRONMENT          DIVISION.
       CONFIGURATION        SECTION.
       SPECIAL-NAMES.
      *{TOTEM}SPECIAL-NAME
      * <TOTEM:EPT. INIT:prenbf, INIT:prenbf, SpecialName>
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
               COPY "F:\lubex\geslux\Copylib\standard.def".
      *{TOTEM}END

      *{TOTEM}COPY-WORKING
      * Key Status
       77 Key-Status IS SPECIAL-NAMES CRT STATUS PIC 9(5) VALUE 0.
          88 Enter-Pushed VALUE 13.
          88 Exit-Pushed VALUE 27.
          88 Message-Received VALUE 95.
          88 Event-Occurred VALUE 96.
          88 Screen-No-Input-Field VALUE 97.
      * Properties & User defined Working Stoarge
       77 strip-prenbf-bmp PIC  S9(9)
                  USAGE IS COMP-4
                  VALUE IS 0.
       77 Verdana12-Occidentale
                  USAGE IS HANDLE OF FONT.
       77 link-security    PIC  9.
       77 nome-pgm         PIC  x(20).
       77 Form1-St-1-Handle
                  USAGE IS HANDLE OF STATUS-BAR.
       77 Form1-Handle
                  USAGE IS HANDLE OF WINDOW.
           COPY  "LINK-BPRENF.DEF".

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
           COPY  "COMMON-LINKAGE.DEF".
      *{TOTEM}END

       SCREEN           SECTION.
      *{TOTEM}COPY-SCREEN
      * FORM
       01 
           Form1, 
           BEFORE PROCEDURE Form1-BeforeProcedure,
           .

      * FRAME
       05
           Form1-Fr-1, 
           Frame, 
           COL 2,60, 
           LINE 2,11,
           LINES 12,22 ,
           SIZE 51,40 ,
           COLOR IS 2,
           ID IS 2,
           HEIGHT-IN-CELLS,
           WIDTH-IN-CELLS,
           TITLE "Esc",
           TITLE-POSITION 6,
           .

      * PUSH BUTTON
       05
           pb-prenb, 
           Push-Button, 
           COL 6,60, 
           LINE 4,11,
           LINES 29,00 ,
           SIZE 195,00 ,
           BITMAP-HANDLE STRIP_PRENBF-BMP,
           BITMAP-NUMBER 1,
           UNFRAMED,
           SQUARE,
           EXCEPTION-VALUE 1000,
           FLAT,
           ID IS 1,
           TITLE "Prenotazione Bolle",
           AFTER PROCEDURE pb-prenb-AfterProcedure, 
           .

      * PUSH BUTTON
       05
           pb-azzprenb, 
           Push-Button, 
           COL 29,60, 
           LINE 4,11,
           LINES 29,00 ,
           SIZE 195,00 ,
           BITMAP-HANDLE STRIP_PRENBF-BMP,
           BITMAP-NUMBER 3,
           UNFRAMED,
           SQUARE,
           EXCEPTION-VALUE 1001,
           FLAT,
           ID IS 3,
           TITLE "Azzeramento Prenotazione Bolle",
           AFTER PROCEDURE pb-azzprenb-AfterProcedure, 
           .

      * PUSH BUTTON
       05
           pb-prenf, 
           Push-Button, 
           COL 6,60, 
           LINE 7,11,
           LINES 29,00 ,
           SIZE 195,00 ,
           BITMAP-HANDLE STRIP_PRENBF-BMP,
           BITMAP-NUMBER 5,
           UNFRAMED,
           SQUARE,
           EXCEPTION-VALUE 1002,
           FLAT,
           ID IS 4,
           TITLE "Prenotazione Fatture",
           AFTER PROCEDURE pb-prenf-AfterProcedure, 
           .

      * PUSH BUTTON
       05
           pb-azzprenf, 
           Push-Button, 
           COL 29,60, 
           LINE 7,11,
           LINES 29,00 ,
           SIZE 195,00 ,
           BITMAP-HANDLE STRIP_PRENBF-BMP,
           BITMAP-NUMBER 7,
           UNFRAMED,
           SQUARE,
           EXCEPTION-VALUE 1003,
           FLAT,
           ID IS 5,
           TITLE "Azzeramento Prenotazione Fatture",
           AFTER PROCEDURE pb-azzprenf-AfterProcedure, 
           .

      * PUSH BUTTON
       05
           pb-modposte, 
           Push-Button, 
           COL 18,10, 
           LINE 10,11,
           LINES 29,00 ,
           SIZE 195,00 ,
           BITMAP-HANDLE STRIP_PRENBF-BMP,
           BITMAP-NUMBER 9,
           UNFRAMED,
           SQUARE,
           EXCEPTION-VALUE 1004,
           FLAT,
           ID IS 6,
           TITLE "Modifica Stato POSTEL",
           AFTER PROCEDURE pb-modposte-AfterProcedure, 
           .

      * LABEL
       05
           Form1-blockpgm-1, 
           Label, 
           COL 28,10, 
           LINE 14,05,
           LINES 0,28 ,
           SIZE 8,20 ,
           ID IS 7,
           HEIGHT-IN-CELLS,
           WIDTH-IN-CELLS,
           TRANSPARENT,
           TITLE "BlockPgm",
           VISIBLE v-custom,
           .

      *{TOTEM}END

      *{TOTEM}LINKPARA
       PROCEDURE  DIVISION USING LK-BLOCKPGM, USER-CODI, LIVELLO-ABIL.
      *{TOTEM}END

      *{TOTEM}DECLARATIVE
      *{TOTEM}END

       MAIN-LOGIC.
      *{TOTEM}ENTRY-BEFPRG
      *    Before-Program
           PERFORM prenbf-Ev-Before-Program
      *{TOTEM}END
           PERFORM INITIALIZE-ROUTINE.
      * run main screen
      *{TOTEM}RUN-MAINSCR
           PERFORM Form1-OPEN-ROUTINE.
      *{TOTEM}END

      *{TOTEM}COPY-PROCEDURE
       EXIT-STOP-ROUTINE.
      * <TOTEM:EPT. INIT:prenbf, INIT:prenbf, BeforeDestroyResource>
      * <TOTEM:END>
           DESTROY Verdana12-Occidentale
           CALL "w$bitmap" USING WBITMAP-DESTROY, STRIP_PRENBF-BMP
      *    After-Program
           PERFORM prenbf-Ev-After-Program
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
      * pb-prenb
           COPY RESOURCE "STRIP_PRENBF.BMP".
           CALL "w$bitmap" USING WBITMAP-LOAD "STRIP_PRENBF.BMP", 
                   GIVING STRIP_PRENBF-BMP.
           .

       INIT-RES.
           .

       INIT-POPUP.
           .


       Form1-DISPLAY-STATUS-MSG.
            MODIFY Form1-St-1-Handle PANEL-INDEX = 1
                PANEL-TEXT = TOTEM-HINT-TEXT
           .

       Form1-CLEAR-STATUS-MSG.
           MOVE SPACES TO TOTEM-MSG-1, TOTEM-MSG-2, TOTEM-MSG-3, 
           TOTEM-MSG-TEXT
           PERFORM Form1-DISPLAY-STATUS-MSG
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
              SCREEN LINE 1,
              SCREEN COLUMN 0,
              LINES 15,67,
              SIZE 54,60,
              HEIGHT-IN-CELLS,
              WIDTH-IN-CELLS,
              COLOR 65793,
              CONTROL FONT Verdana12-Occidentale,
              LABEL-OFFSET 23,
              LINK TO THREAD,
              MODELESS,
              NO SCROLL,
              TITLE-BAR,
              TITLE "Prenotazione Bolla/Fattura e Modifica Stato POSTEL"
           ,
              AUTO-MINIMIZE,
              WITH SYSTEM MENU,
              USER-GRAY,
              USER-WHITE,
              No WRAP,
              EVENT PROCEDURE Form1-Event-Proc,
              HANDLE IS Form1-Handle,
      * <TOTEM:EPT. FORM:Form1, FORM:Form1, AfterCreateWin>
      * <TOTEM:END>


      * Tool Bar    
      * Status-bar
           DISPLAY STATUS-BAR
              GRIP,
              PANEL-WIDTHS 999,
              PANEL-STYLE  1,
              PANEL-TEXT   (SPACE),
              HANDLE IS Form1-St-1-Handle
           DISPLAY Form1 UPON Form1-Handle
      * DISPLAY-COLUMNS settings
           .

       Form1-PROC.
      * <TOTEM:EPT. FORM:Form1, FORM:Form1, BeforeAccept>
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
           DESTROY Form1-Handle
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
              WHEN Key-Status = 1000
                 PERFORM pb-prenb-LinkTo
              WHEN Key-Status = 1001
                 PERFORM pb-azzprenb-LinkTo
              WHEN Key-Status = 1002
                 PERFORM pb-prenf-LinkTo
              WHEN Key-Status = 1003
                 PERFORM pb-azzprenf-LinkTo
              WHEN Key-Status = 1004
                 PERFORM pb-modposte-LinkTo
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
           DISPLAY Form1 UPON Form1-Handle
      * <TOTEM:EPT. FORM:Form1, FORM:Form1, AfterDisplay>
           SET LK-BL-SCRITTURA     TO TRUE.
           MOVE COMO-PROG-ID       TO LK-BL-PROG-ID.
           MOVE FORM1-HANDLE       TO LK-HND-WIN.
           CALL "BLOCKPGM"  USING LK-BLOCKPGM.
           CANCEL "BLOCKPGM".

           .
      * <TOTEM:END>
           .

       Form1-Exit.
      * for main screen
      * <TOTEM:EPT. FORM:Form1, FORM:Form1, BeforeExit>
      * <TOTEM:END>
           MOVE 27 TO Key-Status
           .

       Form1-Init-Data.
           MOVE 1 TO TOTEM-Form-Index
           MOVE 0 TO TOTEM-Frame-Index
           .

       Form1-Init-Value.
           MOVE "Prenotazione Bolla/Fattura e Modifica Stato POSTEL" TO 
           TOTEM-MSG-TITLE
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



       Form1-BeforeProcedure.
           EVALUATE Control-Id
           WHEN 1 MOVE "Attiva la prenotazione della bolla per l'ordine 
      -    "selezionato" to TOTEM-HINT-TEXT
           WHEN 3 MOVE "Disattiva la prenotazione della bolla per l'ordi
      -    "ne selezionato" to TOTEM-HINT-TEXT
           WHEN 4 MOVE "Attiva la prenotazione della fattura per l'ordin
      -    "e selezionato" to TOTEM-HINT-TEXT
           WHEN 5 MOVE "Disattiva la prenotazione della fattura per l'or
      -    "dine selezionato" to TOTEM-HINT-TEXT
           WHEN 6 MOVE "Modifica lo stato di invio POSTEL per la fattura
      -    " selezionata" to TOTEM-HINT-TEXT
           WHEN OTHER MOVE SPACES TO TOTEM-HINT-TEXT
           END-EVALUATE
           EVALUATE Control-Id
           When 1 PERFORM pb-prenb-BeforeProcedure
           When 3 PERFORM pb-azzprenb-BeforeProcedure
           When 4 PERFORM pb-prenf-BeforeProcedure
           When 5 PERFORM pb-azzprenf-BeforeProcedure
           When 6 PERFORM pb-modposte-BeforeProcedure
           END-EVALUATE
           PERFORM Form1-DISPLAY-STATUS-MSG
           .

       Form1-Event-Proc.
           .

      * USER DEFINE PARAGRAPH
       CALL-PROGRAM.
      * <TOTEM:PARA. CALL-PROGRAM>
           modify form1-handle, visible = 0.
           call   nome-pgm using user-codi.
           cancel nome-pgm.
           modify form1-handle, visible = 1 
           .
      * <TOTEM:END>

      * EVENT PARAGRAPH
       pb-prenb-BeforeProcedure.
      * <TOTEM:PARA. pb-prenb-BeforeProcedure>
           modify pb-prenb, bitmap-number = 2 
           .
      * <TOTEM:END>
       pb-prenb-AfterProcedure.
      * <TOTEM:PARA. pb-prenb-AfterProcedure>
           modify pb-prenb, bitmap-number = 1 
           .
      * <TOTEM:END>
       pb-azzprenb-BeforeProcedure.
      * <TOTEM:PARA. pb-azzprenb-BeforeProcedure>
           modify pb-azzprenb, bitmap-number = 4 
           .
      * <TOTEM:END>
       pb-azzprenb-AfterProcedure.
      * <TOTEM:PARA. pb-azzprenb-AfterProcedure>
           modify pb-azzprenb, bitmap-number = 3 
           .
      * <TOTEM:END>
       pb-prenf-BeforeProcedure.
      * <TOTEM:PARA. pb-prenf-BeforeProcedure>
           modify pb-prenf, bitmap-number = 6 
           .
      * <TOTEM:END>
       pb-prenf-AfterProcedure.
      * <TOTEM:PARA. pb-prenf-AfterProcedure>
           modify pb-prenf, bitmap-number = 5 
           .
      * <TOTEM:END>
       pb-azzprenf-BeforeProcedure.
      * <TOTEM:PARA. pb-azzprenf-BeforeProcedure>
           modify pb-azzprenf, bitmap-number = 8 
           .
      * <TOTEM:END>
       pb-azzprenf-AfterProcedure.
      * <TOTEM:PARA. pb-azzprenf-AfterProcedure>
           modify pb-azzprenf, bitmap-number = 7 
           .
      * <TOTEM:END>
       pb-modposte-BeforeProcedure.
      * <TOTEM:PARA. pb-modposte-BeforeProcedure>
           modify pb-modposte, bitmap-number = 10 
           .
      * <TOTEM:END>
       pb-modposte-AfterProcedure.
      * <TOTEM:PARA. pb-modposte-AfterProcedure>
           modify pb-modposte, bitmap-number = 9 
           .
      * <TOTEM:END>
       prenbf-Ev-Before-Program.
      * <TOTEM:PARA. prenbf-Ev-Before-Program>
           move LK-BL-PROG-ID    TO COMO-PROG-ID 
           .
      * <TOTEM:END>
       prenbf-Ev-After-Program.
      * <TOTEM:PARA. prenbf-Ev-After-Program>
           SET LK-BL-CANCELLAZIONE TO TRUE.
           MOVE COMO-PROG-ID       TO LK-BL-PROG-ID.
           CALL "BLOCKPGM"  USING LK-BLOCKPGM 
           .
      * <TOTEM:END>
       pb-prenb-LinkTo.
      * <TOTEM:PARA. pb-prenb-LinkTo>
           move "bprepb" to nome-pgm.
           perform CALL-PROGRAM 
           .
      * <TOTEM:END>
       pb-azzprenb-LinkTo.
      * <TOTEM:PARA. pb-azzprenb-LinkTo>
           move "azzprenb" to nome-pgm.
           perform CALL-PROGRAM 
           .
      * <TOTEM:END>
       pb-prenf-LinkTo.
      * <TOTEM:PARA. pb-prenf-LinkTo>
           move "bprenf" to nome-pgm.
           modify form1-handle, visible = 0.
           initialize link-bprenf replacing numeric data by zeroes
                                       alphanumeric data by spaces.
           move user-codi  to lprenf-user-codi.
           call   nome-pgm using link-bprenf.
           cancel nome-pgm.
           modify form1-handle, visible = 1 
           .
      * <TOTEM:END>
       pb-azzprenf-LinkTo.
      * <TOTEM:PARA. pb-azzprenf-LinkTo>
           move "azzprenf" to nome-pgm.
           perform CALL-PROGRAM 
           .
      * <TOTEM:END>
       pb-modposte-LinkTo.
      * <TOTEM:PARA. pb-modposte-LinkTo>
           move "modposte" to nome-pgm.
           perform CALL-PROGRAM 
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

