      *{TOTEM}PRG-COMMENT
      * sts-annuale.Cbl
      * sts-annuale.Cbl is generated by TOTEM
      * This is a generated file. DO NOT modify this file directly.
      *{TOTEM}END
       IDENTIFICATION       DIVISION.
      *{TOTEM}PRGID
       PROGRAM-ID.          sts-annuale.
       AUTHOR.              ANDREA EVENTI.
       DATE-WRITTEN.        marted� 1 aprile 2014 19:19:30.
       REMARKS.
      *{TOTEM}END

       ENVIRONMENT          DIVISION.
       CONFIGURATION        SECTION.
       SPECIAL-NAMES.
      *{TOTEM}SPECIAL-NAME
      * <TOTEM:EPT. INIT:sts-annuale, INIT:sts-annuale, SpecialName>
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
       77 AUTO-ID          PIC  9(6)
                  VALUE IS 0.
       77 default-font
                  USAGE IS HANDLE OF FONT.
       77 bottone-ok-bmp
                  USAGE IS HANDLE OF BITMAP.
       77 bottone-cancel-bmp
                  USAGE IS HANDLE OF BITMAP.
       77 FORM1-HANDLE
                  USAGE IS HANDLE OF WINDOW.
       77 Small-Font
                  USAGE IS HANDLE OF FONT SMALL-FONT.
       77 Verdana12-Occidentale
                  USAGE IS HANDLE OF FONT.
       77 mese PIC  99
                  VALUE IS 1.
       77 link-user        PIC  x(10).
       77 link-data        PIC  9(8).
       77 link-result      PIC  9.
       77 V-TASTI          PIC  9
                  VALUE IS 1.
       77 v-elab           PIC  9
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
      *{TOTEM}END

       SCREEN           SECTION.
      *{TOTEM}COPY-SCREEN
      * FORM
       01 
           Form1, 
           .

      * FRAME
       05
           Screen1-Fr-2, 
           Frame, 
           COL 1,00, 
           LINE 7,72,
           LINES 2,78 ,
           SIZE 52,00 ,
           LOWERED,
           ID IS 8,
           HEIGHT-IN-CELLS,
           WIDTH-IN-CELLS,
           .

      * PUSH BUTTON
       05
           PB-ESEGUI, 
           Push-Button, 
           COL 36,80, 
           LINE 8,39,
           LINES 30,00 ,
           SIZE 73,00 ,
           BITMAP-HANDLE BOTTONE-OK-BMP,
           BITMAP-NUMBER 1,
           FRAMED,
           SQUARE,
           EXCEPTION-VALUE 1001,
           FLAT,
           ID IS 500,
           SELF-ACT,
           TERMINATION-VALUE 13,
           TITLE "E&segue il programma selezionato",
           VISIBLE V-TASTI,
           AFTER PROCEDURE PB-ESEGUI-AfterProcedure, 
           BEFORE PROCEDURE PB-ESEGUI-BeforeProcedure, 
           .

      * PUSH BUTTON
       05
           Form1-Pb-2, 
           Push-Button, 
           COL 44,70, 
           LINE 8,39,
           LINES 30,00 ,
           SIZE 73,00 ,
           BITMAP-HANDLE BOTTONE-CANCEL-BMP,
           BITMAP-NUMBER 1,
           FRAMED,
           SQUARE,
           EXCEPTION-VALUE 27,
           FLAT,
           ID IS 501,
           SELF-ACT,
           ESCAPE-BUTTON,
           TITLE "&Esce dall'applicativo",
           VISIBLE V-TASTI,
           AFTER PROCEDURE Form1-Pb-2-AfterProcedure, 
           BEFORE PROCEDURE Form1-Pb-2-BeforeProcedure, 
           .

      * LABEL
       05
           Screen1-La-1, 
           Label, 
           COL 1,00, 
           LINE 8,72,
           LINES 1,00 ,
           SIZE 51,00 ,
           ID IS 5,
           HEIGHT-IN-CELLS,
           WIDTH-IN-CELLS,
           RIGHT,
           TRANSPARENT,
           TITLE "Elaborazione in corso...",
           VISIBLE v-elab,
           .

      * RADIO BUTTON
       05
           Screen1-Rb-1, 
           Radio-Button, 
           COL 2,00, 
           LINE 2,00,
           LINES 1,44 ,
           SIZE 11,00 ,
           FLAT,
           GROUP 1,
           GROUP-VALUE 1,
           ID IS 1,
           HEIGHT-IN-CELLS,
           WIDTH-IN-CELLS,
           TITLE "G&ennaio",
           VALUE mese,
           AFTER PROCEDURE Screen1-Rb-1-AfterProcedure, 
           BEFORE PROCEDURE Screen1-Rb-1-BeforeProcedure, 
           .
      * RADIO BUTTON
       05
           Screen1-Rb-1a, 
           Radio-Button, 
           COL 2,00, 
           LINE 4,00,
           LINES 1,44 ,
           SIZE 11,00 ,
           FLAT,
           GROUP 1,
           GROUP-VALUE 2,
           ID IS 2,
           HEIGHT-IN-CELLS,
           WIDTH-IN-CELLS,
           TITLE "&Febbraio",
           VALUE mese,
           AFTER PROCEDURE Screen1-Rb-1-AfterProcedure, 
           BEFORE PROCEDURE Screen1-Rb-1-BeforeProcedure, 
           .
      * RADIO BUTTON
       05
           Screen1-Rb-1aa, 
           Radio-Button, 
           COL 2,00, 
           LINE 6,00,
           LINES 1,44 ,
           SIZE 11,00 ,
           FLAT,
           GROUP 1,
           GROUP-VALUE 3,
           ID IS 3,
           HEIGHT-IN-CELLS,
           WIDTH-IN-CELLS,
           TITLE "&Marzo",
           VALUE mese,
           AFTER PROCEDURE Screen1-Rb-1-AfterProcedure, 
           BEFORE PROCEDURE Screen1-Rb-1-BeforeProcedure, 
           .
      * RADIO BUTTON
       05
           Screen1-Rb-1ab, 
           Radio-Button, 
           COL 15,00, 
           LINE 2,00,
           LINES 1,44 ,
           SIZE 11,00 ,
           FLAT,
           GROUP 1,
           GROUP-VALUE 4,
           ID IS 6,
           HEIGHT-IN-CELLS,
           WIDTH-IN-CELLS,
           TITLE "&Aprile",
           VALUE mese,
           AFTER PROCEDURE Screen1-Rb-1-AfterProcedure, 
           BEFORE PROCEDURE Screen1-Rb-1-BeforeProcedure, 
           .
      * RADIO BUTTON
       05
           Screen1-Rb-1ac, 
           Radio-Button, 
           COL 15,00, 
           LINE 4,00,
           LINES 1,44 ,
           SIZE 11,00 ,
           FLAT,
           GROUP 1,
           GROUP-VALUE 5,
           ID IS 7,
           HEIGHT-IN-CELLS,
           WIDTH-IN-CELLS,
           TITLE "Ma&ggio",
           VALUE mese,
           AFTER PROCEDURE Screen1-Rb-1-AfterProcedure, 
           BEFORE PROCEDURE Screen1-Rb-1-BeforeProcedure, 
           .
      * RADIO BUTTON
       05
           Screen1-Rb-1ad, 
           Radio-Button, 
           COL 15,00, 
           LINE 6,00,
           LINES 1,44 ,
           SIZE 11,00 ,
           FLAT,
           GROUP 1,
           GROUP-VALUE 6,
           ID IS 9,
           HEIGHT-IN-CELLS,
           WIDTH-IN-CELLS,
           TITLE "G&iugno",
           VALUE mese,
           AFTER PROCEDURE Screen1-Rb-1-AfterProcedure, 
           BEFORE PROCEDURE Screen1-Rb-1-BeforeProcedure, 
           .
      * RADIO BUTTON
       05
           Screen1-Rb-1ae, 
           Radio-Button, 
           COL 28,00, 
           LINE 2,00,
           LINES 1,44 ,
           SIZE 11,00 ,
           FLAT,
           GROUP 1,
           GROUP-VALUE 7,
           ID IS 10,
           HEIGHT-IN-CELLS,
           WIDTH-IN-CELLS,
           TITLE "&Luglio",
           VALUE mese,
           AFTER PROCEDURE Screen1-Rb-1-AfterProcedure, 
           BEFORE PROCEDURE Screen1-Rb-1-BeforeProcedure, 
           .
      * RADIO BUTTON
       05
           Screen1-Rb-1af, 
           Radio-Button, 
           COL 28,00, 
           LINE 4,00,
           LINES 1,44 ,
           SIZE 11,00 ,
           FLAT,
           GROUP 1,
           GROUP-VALUE 8,
           ID IS 11,
           HEIGHT-IN-CELLS,
           WIDTH-IN-CELLS,
           TITLE "Ag&osto",
           VALUE mese,
           AFTER PROCEDURE Screen1-Rb-1-AfterProcedure, 
           BEFORE PROCEDURE Screen1-Rb-1-BeforeProcedure, 
           .
      * RADIO BUTTON
       05
           Screen1-Rb-1ag, 
           Radio-Button, 
           COL 28,00, 
           LINE 6,00,
           LINES 1,44 ,
           SIZE 11,00 ,
           FLAT,
           GROUP 1,
           GROUP-VALUE 9,
           ID IS 12,
           HEIGHT-IN-CELLS,
           WIDTH-IN-CELLS,
           TITLE "&Settembre",
           VALUE mese,
           AFTER PROCEDURE Screen1-Rb-1-AfterProcedure, 
           BEFORE PROCEDURE Screen1-Rb-1-BeforeProcedure, 
           .
      * RADIO BUTTON
       05
           Screen1-Rb-1ah, 
           Radio-Button, 
           COL 41,00, 
           LINE 2,00,
           LINES 1,44 ,
           SIZE 11,00 ,
           FLAT,
           GROUP 1,
           GROUP-VALUE 10,
           ID IS 13,
           HEIGHT-IN-CELLS,
           WIDTH-IN-CELLS,
           TITLE "O&ttobre",
           VALUE mese,
           AFTER PROCEDURE Screen1-Rb-1-AfterProcedure, 
           BEFORE PROCEDURE Screen1-Rb-1-BeforeProcedure, 
           .
      * RADIO BUTTON
       05
           Screen1-Rb-1ai, 
           Radio-Button, 
           COL 41,00, 
           LINE 4,00,
           LINES 1,44 ,
           SIZE 11,00 ,
           FLAT,
           GROUP 1,
           GROUP-VALUE 11,
           ID IS 14,
           HEIGHT-IN-CELLS,
           WIDTH-IN-CELLS,
           TITLE "&Novembre",
           VALUE mese,
           AFTER PROCEDURE Screen1-Rb-1-AfterProcedure, 
           BEFORE PROCEDURE Screen1-Rb-1-BeforeProcedure, 
           .
      * RADIO BUTTON
       05
           Screen1-Rb-1aj, 
           Radio-Button, 
           COL 41,00, 
           LINE 6,00,
           LINES 1,44 ,
           SIZE 11,00 ,
           FLAT,
           GROUP 1,
           GROUP-VALUE 12,
           ID IS 15,
           HEIGHT-IN-CELLS,
           WIDTH-IN-CELLS,
           TITLE "&Dicembre",
           VALUE mese,
           AFTER PROCEDURE Screen1-Rb-1-AfterProcedure, 
           BEFORE PROCEDURE Screen1-Rb-1-BeforeProcedure, 
           .
      * LABEL
       05
           Screen1-Custom1-1, 
           Label, 
           COL 13,50, 
           LINE 1,00,
           LINES 0,28 ,
           SIZE 7,60 ,
           FONT IS Default-Font,
           ID IS 4,
           TRANSPARENT,
           TITLE "CUSTOM CONTROL",
           VISIBLE v-custom,
           .

      *{TOTEM}END

      *{TOTEM}LINKPARA
       PROCEDURE        DIVISION.
      *{TOTEM}END

      *{TOTEM}DECLARATIVE
      *{TOTEM}END

       MAIN-LOGIC.
      *{TOTEM}ENTRY-BEFPRG
      *    Before-Program
           PERFORM stbolle-Ev-Before-Program
      *{TOTEM}END
           PERFORM INITIALIZE-ROUTINE.
      * run main screen
      *{TOTEM}RUN-MAINSCR
           PERFORM Form1-OPEN-ROUTINE.
      *{TOTEM}END

      *{TOTEM}COPY-PROCEDURE
       EXIT-STOP-ROUTINE.
      * <TOTEM:EPT. INIT:sts-annuale, INIT:sts-annuale, BeforeDestroyResource>
      * <TOTEM:END>
           DESTROY Verdana12-Occidentale
           CALL "w$bitmap" USING WBITMAP-DESTROY, BOTTONE-OK-BMP
           CALL "w$bitmap" USING WBITMAP-DESTROY, BOTTONE-CANCEL-BMP
      *    After-Program
           PERFORM stbolle-Ev-After-Program
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
      * PB-ESEGUI
           COPY RESOURCE "BOTTONE-OK.BMP".
           CALL "w$bitmap" USING WBITMAP-LOAD "BOTTONE-OK.BMP", 
                   GIVING BOTTONE-OK-BMP.
      * Form1-Pb-2
           COPY RESOURCE "BOTTONE-CANCEL.BMP".
           CALL "w$bitmap" USING WBITMAP-LOAD "BOTTONE-CANCEL.BMP", 
                   GIVING BOTTONE-CANCEL-BMP.
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
              LINES 9,50,
              SIZE 52,00,
              HEIGHT-IN-CELLS,
              WIDTH-IN-CELLS,
              COLOR 65793,
              CONTROL FONT Verdana12-Occidentale,
              LINK TO THREAD,
              MODELESS,
              NO SCROLL,
              TITLE-BAR,
              TITLE "GESLUX - Aggiornamento annuale statistiche settoria
      -    "li",
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
           move 1 to mese.

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
                 PERFORM PB-ESEGUI-LinkTo
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
      * <TOTEM:END>
           MOVE 27 TO Key-Status
           .

       Form1-Init-Data.
           MOVE 1 TO TOTEM-Form-Index
           MOVE 0 TO TOTEM-Frame-Index
           .

       Form1-Init-Value.
           MOVE "GESLUX - Aggiornamento annuale statistiche settoriali" 
           TO TOTEM-MSG-TITLE
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
       stbolle-Ev-Before-Program.
      * <TOTEM:PARA. stbolle-Ev-Before-Program>
           .
      * <TOTEM:END>
       stbolle-Ev-After-Program.
      * <TOTEM:PARA. stbolle-Ev-After-Program>
           .
      * <TOTEM:END>
       PB-ESEGUI-BeforeProcedure.
      * <TOTEM:PARA. PB-ESEGUI-BeforeProcedure>
           modify PB-ESEGUI, bitmap-number 2 
           .
      * <TOTEM:END>
       PB-ESEGUI-AfterProcedure.
      * <TOTEM:PARA. PB-ESEGUI-AfterProcedure>
           modify PB-ESEGUI, bitmap-number 1 
           .
      * <TOTEM:END>
       PB-ESEGUI-LinkTo.
      * <TOTEM:PARA. PB-ESEGUI-LinkTo>
           move 0 to v-tasti.
           move 1 to v-elab.
           display form1.

           move 2006    to link-data(1:4).
           move mese    to link-data(5:2).

           evaluate mese
           when 11
           when 04
           when 06
           when 09 move 30 to link-data(7:2)

           when 02 move 28 to link-data(7:2)

           when 01
           when 03
           when 05
           when 07
           when 08
           when 10
           when 12 move 31 to link-data(7:2)
           end-evaluate.

           move "BOSS"  to link-user.
           move 0       to link-result.

           call   "aggstatsett" using link-user,
                                      link-data,
                                      link-result,
                                      form1-handle.

           cancel "aggstatsett".

           call   "aggstatmese" using link-data,
                                      form1-handle.

           cancel "aggstatmese".

           move 1 to v-tasti.
           move 0 to v-elab.
           display form1 
           .
      * <TOTEM:END>
       Form1-Pb-2-BeforeProcedure.
      * <TOTEM:PARA. Form1-Pb-2-BeforeProcedure>
           modify Form1-Pb-2, bitmap-number 2 
           .
      * <TOTEM:END>
       Form1-Pb-2-AfterProcedure.
      * <TOTEM:PARA. Form1-Pb-2-AfterProcedure>
           modify Form1-Pb-2, bitmap-number 1 
           .
      * <TOTEM:END>
       Screen1-Rb-1-BeforeProcedure.
      * <TOTEM:PARA. Screen1-Rb-1-BeforeProcedure>
           modify control-handle, color = colore-nu
           .
      * <TOTEM:END>
       Screen1-Rb-1-AfterProcedure.
      * <TOTEM:PARA. Screen1-Rb-1-AfterProcedure>
           modify control-handle, color = colore-or
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

