      *{TOTEM}PRG-COMMENT
      * stordf.Cbl
      * stordf.Cbl is generated by TOTEM
      * This is a generated file. DO NOT modify this file directly.
      *{TOTEM}END
       IDENTIFICATION       DIVISION.
      *{TOTEM}PRGID
       PROGRAM-ID.          stordf.
       AUTHOR.              ANDREA EVENTI.
       DATE-WRITTEN.        marted� 1 aprile 2014 19:19:23.
       REMARKS.
      *{TOTEM}END

       ENVIRONMENT          DIVISION.
       CONFIGURATION        SECTION.
       SPECIAL-NAMES.
      *{TOTEM}SPECIAL-NAME
      * <TOTEM:EPT. INIT:stordf, INIT:stordf, SpecialName>
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
       77 BLUE-FINO-28X24-BMP          PIC  S9(9)
                  USAGE IS COMP-4
                  VALUE IS 0.
           COPY  "LINK-ST-ORDFORN.DEF".
       77 Default-Font
                  USAGE IS HANDLE OF FONT DEFAULT-FONT.
       77 BLUE-DA-28X24-BMP            PIC  S9(9)
                  USAGE IS COMP-4
                  VALUE IS 0.
       77 ok_73x21-bmp     PIC  S9(9)
                  USAGE IS COMP-4
                  VALUE IS 0.
       77 cancel_73x21-bmp PIC  S9(9)
                  USAGE IS COMP-4
                  VALUE IS 0.
       77 ESCI_73X21-BMP   PIC  S9(9)
                  USAGE IS COMP-4
                  VALUE IS 0.
       77 ESEGUI_73X21-BMP PIC  S9(9)
                  USAGE IS COMP-4
                  VALUE IS 0.
       77 Small-Font
                  USAGE IS HANDLE OF FONT SMALL-FONT.
       77 Form1-Handle
                  USAGE IS HANDLE OF WINDOW.
       77 Screen1-SF-HANDLE
                  USAGE IS HANDLE OF WINDOW.
       77 Verdana10-Occidentale
                  USAGE IS HANDLE OF FONT.
       78 titolo VALUE IS "Stampa Ordini". 
       77 anno-to          PIC  9(4).
       77 num-from         PIC  9(8).
       77 num-to           PIC  9(8).
       77 screen-2-handle
                  USAGE IS HANDLE OF WINDOW.
       77 AUTO-ID          PIC  9(6)
                  VALUE IS 0,00.
       77 bottone-ok-bmp   PIC  S9(9)
                  USAGE IS COMP-4
                  VALUE IS 0.
       77 bottone-cancel-bmp           PIC  S9(9)
                  USAGE IS COMP-4
                  VALUE IS 0.
       77 Verdana12-Occidentale
                  USAGE IS HANDLE OF FONT.
       77 Form1-Tb-1-Handlea
                  USAGE IS HANDLE OF WINDOW.
       77 toolbar-bmp      PIC  S9(9)
                  USAGE IS COMP-4
                  VALUE IS 0.
       77 E-ESCI           PIC  9
                  VALUE IS 1.
       77 MOD-LIMITI       PIC  9
                  VALUE IS 1.
       77 CBO-stato-ord-BUF            PIC  X(50).
       77 data-from        PIC  9(8).
       77 data-to          PIC  9(8).
       77 data-to-AAAAMMGG PIC  9(8).
       77 data-from-AAAAMMGG           PIC  9(8).
       77 CBO-stato-ordf-BUF           PIC  X(50).

      ***********************************************************
      *   Code Gen's Buffer                                     *
      ***********************************************************
       77 STATUS-Screen2-FLAG-REFRESH PIC  9.
          88 Screen2-FLAG-REFRESH  VALUE 1 FALSE 0. 
      *
       01  stato-ordf        pic  x.
         88 inseriti         VALUE IS "I". 
         88 inviati          VALUE IS "S". 
         88 in-lavorazione   VALUE IS "L". 
         88 chiusi           VALUE IS "C". 
         88 tutti            VALUE IS "T".

       78  78-inseriti            value "Inseriti". 
       78  78-inviati             value "Inviati".
       78  78-chiusi              value "Chiusi".
       78  78-in-lavorazione      value "In Lavorazione".
       78  78-tutti               value "Tutti".
      *{TOTEM}END

      *{TOTEM}ID-LOGICI
      ***** Elenco ID Logici *****
       78  78-ID-ef-anno VALUE 5001.
       78  78-ID-ef-num-from VALUE 5002.
       78  78-ID-ef-num-to VALUE 5003.
       78  78-ID-ef-data-from VALUE 5004.
       78  78-ID-ef-data-to VALUE 5005.
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
           Screen2, 
           AFTER PROCEDURE  Screen2-AFTER-SCREEN
           .

      * ENTRY FIELD
       05
           ef-anno, 
           Entry-Field, 
           COL 7,00, 
           LINE 2,00,
           LINES 1,33 ,
           SIZE 5,00 ,
           BOXED,
           ID IS 78-ID-ef-anno,                
           HEIGHT-IN-CELLS,
           WIDTH-IN-CELLS,
           RIGHT,
           MAX-TEXT 4,
           VALUE anno-to,
           AFTER PROCEDURE ef-con-to-AfterProcedure, 
           BEFORE PROCEDURE ef-con-to-BeforeProcedure, 
           .


      * ENTRY FIELD
       05
           ef-num-from, 
           Entry-Field, 
           COL 2,00, 
           LINE 4,00,
           LINES 1,33 ,
           SIZE 10,00 ,
           BOXED,
           ID IS 78-ID-ef-num-from,                
           HEIGHT-IN-CELLS,
           WIDTH-IN-CELLS,
           RIGHT,
           MAX-TEXT 8,
           VALUE num-from,
           pic Z(7)9
           AFTER PROCEDURE ef-data-from-AfterProcedure, 
           BEFORE PROCEDURE ef-num-from-BeforeProcedure, 
           .

      * ENTRY FIELD
       05
           ef-num-to, 
           Entry-Field, 
           COL 29,00, 
           LINE 4,00,
           LINES 1,33 ,
           SIZE 10,00 ,
           BOXED,
           ID IS 78-ID-ef-num-to,                
           HEIGHT-IN-CELLS,
           WIDTH-IN-CELLS,
           RIGHT,
           MAX-TEXT 8,
           VALUE num-to,
           pic Z(7)9
           AFTER PROCEDURE ef-num-to-AfterProcedure, 
           BEFORE PROCEDURE ef-num-to-BeforeProcedure, 
           .


      * ENTRY FIELD
       05
           ef-data-from, 
           Entry-Field, 
           COL 2,00, 
           LINE 6,00,
           LINES 1,33 ,
           SIZE 10,00 ,
           BOXED,
           ID IS 78-ID-ef-data-from,                
           HEIGHT-IN-CELLS,
           WIDTH-IN-CELLS,
           RIGHT,
           PIC 99/99/9999,
           MAX-TEXT 8,
           VALUE data-from,
           AFTER PROCEDURE ef-data-from-AfterProcedure, 
           BEFORE PROCEDURE ef-data-from-BeforeProcedure, 
           .

      * ENTRY FIELD
       05
           ef-data-to, 
           Entry-Field, 
           COL 29,00, 
           LINE 6,00,
           LINES 1,33 ,
           SIZE 10,00 ,
           BOXED,
           ID IS 78-ID-ef-data-to,                
           HEIGHT-IN-CELLS,
           WIDTH-IN-CELLS,
           RIGHT,
           PIC 99/99/9999,
           MAX-TEXT 8,
           VALUE data-to,
           AFTER PROCEDURE ef-data-to-AfterProcedure, 
           BEFORE PROCEDURE ef-data-to-BeforeProcedure, 
           .


      * COMBO-BOX
       05
           CBO-STATO-ORDF, 
           Combo-Box, 
           COL 12,90, 
           LINE 8,00,
           LINES 5,00 ,
           SIZE 15,20 ,
           3-D,
           COLOR IS 513,
           ENABLED MOD-LIMITI,
           ID IS 20,
           HEIGHT-IN-CELLS,
           WIDTH-IN-CELLS,
           MASS-UPDATE 0,
           DROP-LIST,
           UNSORTED,
           VALUE CBO-stato-ordf-BUF,
           AFTER PROCEDURE Form1-Cm-1-AfterProcedure, 
           BEFORE PROCEDURE Form1-Cm-1-BeforeProcedure, 
           .
      * FRAME
       05
           Screen4-Fr-1, 
           Frame, 
           COL 1,00, 
           LINE 10,50,
           LINES 2,78 ,
           SIZE 39,00 ,
           LOWERED,
           ID IS 2,
           HEIGHT-IN-CELLS,
           WIDTH-IN-CELLS,
           .

      * PUSH BUTTON
       05
           PB-ESEGUI, 
           Push-Button, 
           COL 23,90, 
           LINE 11,17,
           LINES 30,00 ,
           SIZE 73,00 ,
           BITMAP-HANDLE BOTTONE-OK-BMP,
           BITMAP-NUMBER 1,
           FRAMED,
           SQUARE,
           ENABLED 1,
           EXCEPTION-VALUE 1001,
           FLAT,
           ID IS 500,
           SELF-ACT,
           TERMINATION-VALUE 13,
           TITLE "E&segue il programma selezionato",
           AFTER PROCEDURE PB-ESEGUI-AfterProcedure, 
           BEFORE PROCEDURE PB-ESEGUI-BeforeProcedure, 
           .

      * PUSH BUTTON
       05
           Form1-Pb-2, 
           Push-Button, 
           COL 31,80, 
           LINE 11,17,
           LINES 30,00 ,
           SIZE 73,00 ,
           BITMAP-HANDLE BOTTONE-CANCEL-BMP,
           BITMAP-NUMBER 1,
           FRAMED,
           SQUARE,
           ENABLED 1,
           EXCEPTION-VALUE 27,
           FLAT,
           ID IS 501,
           SELF-ACT,
           ESCAPE-BUTTON,
           TITLE "&Esce dall'applicativo",
           AFTER PROCEDURE Form1-Pb-2-AfterProcedure, 
           BEFORE PROCEDURE Form1-Pb-2-BeforeProcedure, 
           .

      * LABEL
       05
           Screen3-La-1a, 
           Label, 
           COL 17,00, 
           LINE 2,00,
           LINES 1,17 ,
           SIZE 7,00 ,
           ID IS 100,
           HEIGHT-IN-CELLS,
           WIDTH-IN-CELLS,
           CENTER,
           TRANSPARENT,
           TITLE "Anno",
           .

      * LABEL
       05
           Screen3-La-1aa, 
           Label, 
           COL 17,00, 
           LINE 4,00,
           LINES 1,17 ,
           SIZE 7,00 ,
           ID IS 200,
           HEIGHT-IN-CELLS,
           WIDTH-IN-CELLS,
           CENTER,
           TRANSPARENT,
           TITLE "Numero",
           .

      * BITMAP
       05
           Bitmap-freccia-finoa, 
           Bitmap, 
           COL 25,00, 
           LINE 4,00,
           LINES 24,00 ,
           SIZE 28,00 ,
           TRANSPARENT-COLOR tr-color,
           BITMAP-HANDLE BLUE-FINO-28X24-BMP,
           BITMAP-NUMBER 1,
           FONT IS Default-Font,
           ID IS 5,
           Transparent-color TR-COLOR
           .

      * BITMAP
       05
           Bitmap-freccia-daa, 
           Bitmap, 
           COL 13,00, 
           LINE 4,00,
           LINES 24,00 ,
           SIZE 28,00 ,
           TRANSPARENT-COLOR tr-color,
           BITMAP-HANDLE BLUE-DA-28X24-BMP,
           BITMAP-NUMBER 1,
           FONT IS Default-Font,
           ID IS 6,
           Transparent-color TR-COLOR
           .

      * LABEL
       05
           Screen4-Custom1-1, 
           Label, 
           COL 36,20, 
           LINE 1,17,
           LINES 0,67 ,
           SIZE 2,60 ,
           FONT IS Default-Font,
           ID IS 300,
           TRANSPARENT,
           TITLE "CUSTOM CONTROL",
           VISIBLE v-custom,
           .

      * LABEL
       05
           Screen4-blockpgm-1, 
           Label, 
           COL 35,80, 
           LINE 2,11,
           LINES 0,78 ,
           SIZE 3,30 ,
           ID IS 1,
           HEIGHT-IN-CELLS,
           WIDTH-IN-CELLS,
           TRANSPARENT,
           TITLE "BlockPgm",
           VISIBLE v-custom,
           .

      * LABEL
       05
           Screen3-La-1aaa, 
           Label, 
           COL 17,00, 
           LINE 6,00,
           LINES 1,17 ,
           SIZE 7,00 ,
           ID IS 502,
           HEIGHT-IN-CELLS,
           WIDTH-IN-CELLS,
           CENTER,
           TRANSPARENT,
           TITLE "Data",
           .

      * BITMAP
       05
           Bitmap-freccia-finoaa, 
           Bitmap, 
           COL 25,00, 
           LINE 6,00,
           LINES 24,00 ,
           SIZE 28,00 ,
           TRANSPARENT-COLOR tr-color,
           BITMAP-HANDLE BLUE-FINO-28X24-BMP,
           BITMAP-NUMBER 1,
           FONT IS Default-Font,
           ID IS 503,
           Transparent-color TR-COLOR
           .

      * BITMAP
       05
           Bitmap-freccia-daaa, 
           Bitmap, 
           COL 13,00, 
           LINE 6,00,
           LINES 24,00 ,
           SIZE 28,00 ,
           TRANSPARENT-COLOR tr-color,
           BITMAP-HANDLE BLUE-DA-28X24-BMP,
           BITMAP-NUMBER 1,
           FONT IS Default-Font,
           ID IS 504,
           Transparent-color TR-COLOR
           .

      * LABEL
       05
           Form1-La-1abc, 
           Label, 
           COL 6,40, 
           LINE 8,00,
           LINES 1,33 ,
           SIZE 5,00 ,
           ID IS 218,
           HEIGHT-IN-CELLS,
           WIDTH-IN-CELLS,
           TRANSPARENT,
           TITLE "Stato",
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
           PERFORM aggarc1-Ev-Before-Program
      *{TOTEM}END
           PERFORM INITIALIZE-ROUTINE.
      * run main screen
      *{TOTEM}RUN-MAINSCR
           PERFORM Screen2-OPEN-ROUTINE.
      *{TOTEM}END

      *{TOTEM}COPY-PROCEDURE
       EXIT-STOP-ROUTINE.
      * <TOTEM:EPT. INIT:stordf, INIT:stordf, BeforeDestroyResource>
      * <TOTEM:END>
           DESTROY Verdana12-Occidentale
           CALL "w$bitmap" USING WBITMAP-DESTROY, BOTTONE-OK-BMP
           CALL "w$bitmap" USING WBITMAP-DESTROY, BOTTONE-CANCEL-BMP
           CALL "w$bitmap" USING WBITMAP-DESTROY, BLUE-FINO-28X24-BMP
           CALL "w$bitmap" USING WBITMAP-DESTROY, BLUE-DA-28X24-BMP
      *    After-Program
           PERFORM aggarc1-Ev-After-Program
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
      * Bitmap-freccia-finoa
           COPY RESOURCE "BLUE-FINO-28X24.BMP".
           CALL "w$bitmap" USING WBITMAP-LOAD "BLUE-FINO-28X24.BMP", 
                   GIVING BLUE-FINO-28X24-BMP.
      * Bitmap-freccia-daa
           COPY RESOURCE "BLUE-DA-28X24.BMP".
           CALL "w$bitmap" USING WBITMAP-LOAD "BLUE-DA-28X24.BMP", 
                   GIVING BLUE-DA-28X24-BMP.
           .

       INIT-RES.
           .

       INIT-POPUP.
           .


      * COMBO-BOX
       CBO-STATO-ORDF-Content.
           .

       Screen2-Open-Routine.
           PERFORM Screen2-Scrn
           PERFORM Screen2-Proc
           .

       Screen2-Scrn.
           PERFORM Screen2-Create-Win
           PERFORM Screen2-Init-Value
           PERFORM Screen2-Init-Data
      * Tab keystrok settings
      * Tool Bar
           PERFORM Screen2-DISPLAY
           .

       Screen2-Create-Win.
           Display Independent GRAPHICAL WINDOW
              LINES 12,28,
              SIZE 39,00,
              HEIGHT-IN-CELLS,
              WIDTH-IN-CELLS,
              COLOR 65793,
              CONTROL FONT Verdana12-Occidentale,
              LABEL-OFFSET 23,
              LINK TO THREAD,
              NO SCROLL,
              TITLE-BAR,
              TITLE titolo,
              WITH SYSTEM MENU,
              USER-GRAY,
              USER-WHITE,
              No WRAP,
              EVENT PROCEDURE Screen4-Event-Proc,
              HANDLE IS Form1-Handle,
      * <TOTEM:EPT. FORM:Screen2, FORM:Screen2, AfterCreateWin>
      * <TOTEM:END>


      * Tool Bar    
      * Status-bar
           DISPLAY Screen2 UPON Form1-Handle
      * DISPLAY-COLUMNS settings
           .

       Screen2-PROC.
      * <TOTEM:EPT. FORM:Screen2, FORM:Screen2, BeforeAccept>
           set tutto-ok to true.

           Move como-data(1:4) to anno-to
                                  data-to(5:4)
                                  data-from(5:4).
           Move ZERO           to num-from.
           move all "9"        to num-to.

           move "3112" to data-to(1:4)
           move "0101" to data-from(1:4).


           Perform RIEMPI-COMBO-STATO-ORDF.
           move 78-tutti to cbo-stato-ordf-buf
           Modify cbo-stato-ordf   value cbo-stato-ordf-buf.


           display Screen2.

           .
      * <TOTEM:END>
           PERFORM UNTIL Exit-Pushed
              ACCEPT Screen2
                 ON EXCEPTION
                    PERFORM Screen2-Evaluate-Func
                 MOVE 1 TO TOTEM-Form-Index
              END-ACCEPT
      * <TOTEM:EPT. FORM:Screen2, FORM:Screen2, AfterEndAccept>
      * <TOTEM:END>
           END-PERFORM
      * <TOTEM:EPT. FORM:Screen2, FORM:Screen2, BeforeDestroyWindow>
      * <TOTEM:END>
           DESTROY Form1-Handle
           INITIALIZE Key-Status
           .

       Screen2-Evaluate-Func.
      * <TOTEM:EPT. FORM:Screen2, FORM:Screen2, AfterAccept>
      * <TOTEM:END>
           EVALUATE TRUE
              WHEN Exit-Pushed
                 PERFORM Screen2-Exit
              WHEN Event-Occurred
                 IF Event-Type = Cmd-Close
                    PERFORM Screen2-Exit
                 END-IF
              WHEN Key-Status = 1001
                 PERFORM PB-ESEGUI-LinkTo
           END-EVALUATE
      * avoid changing focus
           MOVE 4 TO Accept-Control
           .

       Screen2-CLEAR.
           PERFORM Screen2-INIT-VALUE
           PERFORM Screen2-DISPLAY
           .

       Screen2-DISPLAY.
      * <TOTEM:EPT. FORM:Screen2, FORM:Screen2, BeforeDisplay>
      * <TOTEM:END>
           DISPLAY Screen2 UPON Form1-Handle
      * <TOTEM:EPT. FORM:Screen2, FORM:Screen2, AfterDisplay>
           SET LK-BL-SCRITTURA     TO TRUE.
           MOVE COMO-PROG-ID       TO LK-BL-PROG-ID.
           MOVE FORM1-HANDLE       TO LK-HND-WIN.
           CALL "BLOCKPGM"  USING LK-BLOCKPGM.
           CANCEL "BLOCKPGM".

           .
      * <TOTEM:END>
           .

       Screen2-Exit.
      * for main screen
      * <TOTEM:EPT. FORM:Screen2, FORM:Screen2, BeforeExit>
      * <TOTEM:END>
           MOVE 27 TO Key-Status
           .

       Screen2-Init-Data.
           MOVE 1 TO TOTEM-Form-Index
           MOVE 0 TO TOTEM-Frame-Index
      * COMBO-BOX
           PERFORM CBO-STATO-ORDF-Content
           .

       Screen2-Init-Value.
           MOVE titolo TO TOTEM-MSG-TITLE
      * <TOTEM:EPT. FORM:Screen2, FORM:Screen2, SetDefault>
      * <TOTEM:END>
           PERFORM Screen2-FLD-TO-BUF
           .


       Screen2-ALLGRID-RESET.
           .

      * for Form's Validation
       Screen2-VALIDATION-ROUTINE.
           SET TOTEM-CHECK-OK TO TRUE
           .


       Screen2-Buf-To-Fld.
      * <TOTEM:EPT. FORM:Screen2, FORM:Screen2, BeforeBufToFld>
      * <TOTEM:END>
      * <TOTEM:EPT. FORM:Screen2, FORM:Screen2, AfterBufToFld>
      * <TOTEM:END>
           .

       Screen2-Fld-To-Buf.
      * <TOTEM:EPT. FORM:Screen2, FORM:Screen2, BeforeFldToBuf>
      * <TOTEM:END>
      * <TOTEM:EPT. FORM:Screen2, FORM:Screen2, AfterFldToBuf>
      * <TOTEM:END>
           .

       Screen2-CONTROLLO-OLD.
           set SiSalvato to true.
           if mod = 0 exit paragraph end-if.
           perform Screen2-BUF-TO-FLD.
           move 0 to scelta.
           .
       Screen2-EXTENDED-FILE-STATUS.
           CALL "C$RERRNAME" USING TOTEM-MSG-ERR-FILE
           CALL "C$RERR" USING EXTEND-STAT, TEXT-MESSAGE
           MOVE PRIMARY-ERROR TO TOTEM-MSG-ID
           PERFORM Screen2-SHOW-MSG-ROUTINE
           .

       Screen2-SHOW-MSG-ROUTINE.
           PERFORM SHOW-MSG-ROUTINE
           PERFORM Screen2-DISPLAY-MESSAGE
           .

       Screen2-DISPLAY-MESSAGE.
           PERFORM MESSAGE-BOX-ROUTINE
           DISPLAY MESSAGE BOX TOTEM-MSG-TEXT
               TITLE IS TOTEM-MSG-TITLE
               TYPE  IS TOTEM-MSG-BUTTON-TYPE
               ICON  IS TOTEM-MSG-DEFAULT-BUTTON
               RETURNING TOTEM-MSG-RETURN-VALUE
           .

       Screen2-Save-Status.
           .             

       Screen2-Restore-Status.
           .


      * Paragrafo per la struttura del codice in AFTER sulla screen Screen2
      ***---
       Screen2-AFTER-SCREEN.

      * Generazione risettaggio keyboard "." ---> "."

      * Generazione stringa perform CONTROLLO
           evaluate control-id
           |78-ID-ef-anno � l'ID del campo ef-anno
           when 78-ID-ef-anno
                perform CONTROLLO
           |78-ID-ef-num-to � l'ID del campo ef-num-to
           when 78-ID-ef-num-to
                perform CONTROLLO
           |78-ID-ef-data-to � l'ID del campo ef-data-to
           when 78-ID-ef-data-to
                perform CONTROLLO
           |99999 � un valore fittizio, che non sar� MAI usato,
           |ma mi serve per non riscontrare errori di compilazione
           |in caso non avessi generato nulla nella AFTER CONTROLLO della screen
           when 99999 continue
           when other continue
           end-evaluate.



       Screen4-Event-Proc.
           .

      * USER DEFINE PARAGRAPH
       CONTROLLO.
      * <TOTEM:PARA. CONTROLLO>
           set tutto-ok to true.

           evaluate CONTROL-ID

           when 78-ID-ef-anno
      * ANNO DI FINE SELEZIONE
                inquire ef-anno, value in anno-to
                if anno-to = ZERO
                   set errori  to true
                   display message "Inserimento anno mancante"
                           title tit-err
                           icon  mb-warning-icon
                   move 78-ID-ef-anno   to store-id
                end-if

           when 78-ID-ef-num-to
      * ORDINE DI FINE SELEZIONE
                if num-to = ZERO
                   move all "9" to num-to
                   display ef-num-to
                end-if

                if num-to < num-from
                   set errori to true
                   display message MSG-intervallo-num-ord-err
                       title tit-err
                       icon  mb-warning-icon
                       type  mb-ok
                   move 3 to STORE-ID
                end-if
           when 78-ID-ef-data-from
                if data-from not = zero
                   move data-from to como-data
                   perform DATE-FORMAT
                   move como-data to data-from
                   display ef-data-from
                end-if
           when 78-ID-ef-data-to
                if data-to = zero
                   move 99999999 to data-to
                end-if
                if data-to not = 99999999
                   move data-to to como-data
                   perform DATE-FORMAT
                   move como-data to data-to
                end-if
                display ef-data-to

                move data-from(1:2) to data-from-AAAAMMGG(7:2)
                move data-from(3:2) to data-from-AAAAMMGG(5:2)
                move data-from(5:4) to data-from-AAAAMMGG(1:4)
                move data-to(1:2)   to data-to-AAAAMMGG(7:2)
                move data-to(3:2)   to data-to-AAAAMMGG(5:2)
                move data-to(5:4)   to data-to-AAAAMMGG(1:4)

                if data-from-AAAAMMGG = 0
                   move anno-to to data-from-AAAAMMGG(1:4)
                   move "0101"    to data-from-AAAAMMGG(5:4)
                end-if

                if data-to-AAAAMMGG < data-from-AAAAMMGG
                   set errori to true
                   move 78-ID-ef-data-to to CONTROL-ID
                   display message MSG-intervallo-date-err
                      title tit-err
                      icon  MB-WARNING-ICON
                end-if
           end-evaluate.
                        
           if errori                  
              move STORE-ID to CONTROL-ID
              move 4        to ACCEPT-CONTROL
           end-if.

           perform CANCELLA-COLORE 
           .
      * <TOTEM:END>

       PARAGRAFO-COPY.
      * <TOTEM:PARA. PARAGRAFO-COPY>
           copy "utydata.cpy".
           copy "color-custom.cpy".
           copy "calcola-colore-trasparente.cpy".


      ***---
       RIEMPI-COMBO-STATO-ORDF.
           Modify cbo-stato-ordf, item-to-add 78-tutti.
           Modify cbo-stato-ordf, item-to-add 78-inseriti.
           Modify cbo-stato-ordf, item-to-add 78-inviati.
           Modify cbo-stato-ordf, item-to-add 78-in-lavorazione.
           Modify cbo-stato-ordf, item-to-add 78-chiusi.

      ***---
       CARICA-COMBO-STATO-ORDF.
           evaluate true
           when chiusi         move 78-chiusi         to 
           cbo-stato-ordf-buf
           when inseriti       move 78-inseriti       to 
           cbo-stato-ordf-buf
           when in-lavorazione move 78-in-lavorazione to 
           cbo-stato-ordf-buf
           when tutti          move 78-tutti          to 
           cbo-stato-ordf-buf
           when inviati        move 78-inviati        to 
           cbo-stato-ordf-buf
           end-evaluate.
           modify cbo-stato-ordf,      value cbo-stato-ordf-buf.
           
      ***---
       SCARICA-COMBO-STATO-ORD.     
           inquire cbo-stato-ordf,  value in cbo-stato-ordf-buf.
           evaluate cbo-stato-ordf-buf
           when 78-chiusi         set chiusi         to true
           when 78-inviati        set inviati        to true
           when 78-inseriti       set inseriti       to true
           when 78-in-lavorazione set in-lavorazione to true
           when 78-tutti          set tutti          to true
           end-evaluate 
           .
      * <TOTEM:END>

      * EVENT PARAGRAPH
       aggarc1-Ev-Before-Program.
      * <TOTEM:PARA. aggarc1-Ev-Before-Program>
           accept como-data from CENTURY-DATE.

           perform CALCOLA-COLORE-TRASPARENTE.
           move LK-BL-PROG-ID    TO COMO-PROG-ID 
           .
      * <TOTEM:END>
       aggarc1-Ev-After-Program.
      * <TOTEM:PARA. aggarc1-Ev-After-Program>
           SET LK-BL-CANCELLAZIONE TO TRUE.
           MOVE COMO-PROG-ID       TO LK-BL-PROG-ID.
           CALL "BLOCKPGM"  USING LK-BLOCKPGM 
           .
      * <TOTEM:END>
       ef-data-from-AfterProcedure.
      * <TOTEM:PARA. ef-data-from-AfterProcedure>
           MODIFY CONTROL-HANDLE COLOR = COLORE-OR
           perform CONTROLLO 
           .
      * <TOTEM:END>
       ef-con-to-BeforeProcedure.
      * <TOTEM:PARA. ef-con-to-BeforeProcedure>
           MODIFY CONTROL-HANDLE COLOR = COLORE-NU
           .
      * <TOTEM:END>
       ef-con-to-AfterProcedure.
      * <TOTEM:PARA. ef-con-to-AfterProcedure>
           MODIFY CONTROL-HANDLE COLOR = COLORE-OR
           perform CONTROLLO               
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
           perform varying CONTROL-ID from 78-ID-ef-anno by 1 
                     until CONTROL-ID    > 78-ID-ef-data-to
              perform CONTROLLO
              
              if errori exit perform end-if

           end-perform.

           if tutto-ok  
              initialize st-ordforn-linkage
              move anno-to   to stof-tof-anno
              move anno-to   to stof-tof-anno-a
              move num-from  to stof-tof-numero
              move num-to    to stof-tof-numero-a

              move data-from(1:2)     to data-from-AAAAMMGG(7:2)
              move data-from(3:2)     to data-from-AAAAMMGG(5:2)
              move data-from(5:4)     to data-from-AAAAMMGG(1:4)
              move data-from-AAAAMMGG to stof-tof-data-da
              move data-to(1:2)       to data-to-AAAAMMGG(7:2)
              move data-to(3:2)       to data-to-AAAAMMGG(5:2)
              move data-to(5:4)       to data-to-AAAAMMGG(1:4)
              move data-to-AAAAMMGG   to stof-tof-data-a

              set stof-scegli-stampante to true
              perform SCARICA-COMBO-STATO-ORD
              move stato-ordf to stof-tof-stato


              call   "st-ordforn" using st-ordforn-linkage
              cancel "st-ordforn"

              modify pb-esegui, bitmap-number = 1

              move 78-ID-ef-anno to CONTROL-ID      
              move 4             to ACCEPT-CONTROL  
           end-if 
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
       Form1-Cm-1-BeforeProcedure.
      * <TOTEM:PARA. Form1-Cm-1-BeforeProcedure>
           MODIFY CONTROL-HANDLE COLOR = COLORE-NU
           .
      * <TOTEM:END>
       Form1-Cm-1-AfterProcedure.
      * <TOTEM:PARA. Form1-Cm-1-AfterProcedure>
           MODIFY CONTROL-HANDLE COLOR = COLORE-OR
           .
      * <TOTEM:END>
       ef-num-from-BeforeProcedure.
      * <TOTEM:PARA. ef-num-from-BeforeProcedure>
           MODIFY CONTROL-HANDLE COLOR = COLORE-NU
           .
      * <TOTEM:END>
       ef-num-to-BeforeProcedure.
      * <TOTEM:PARA. ef-num-to-BeforeProcedure>
           MODIFY CONTROL-HANDLE COLOR = COLORE-NU
           .
      * <TOTEM:END>
       ef-data-from-BeforeProcedure.
      * <TOTEM:PARA. ef-data-from-BeforeProcedure>
           MODIFY CONTROL-HANDLE COLOR = COLORE-NU
           .
      * <TOTEM:END>
       ef-data-to-BeforeProcedure.
      * <TOTEM:PARA. ef-data-to-BeforeProcedure>
           MODIFY CONTROL-HANDLE COLOR = COLORE-NU
           .
      * <TOTEM:END>
       ef-num-to-AfterProcedure.
      * <TOTEM:PARA. ef-num-to-AfterProcedure>
           MODIFY CONTROL-HANDLE COLOR = COLORE-OR
           perform CONTROLLO 
           .
      * <TOTEM:END>
       ef-data-to-AfterProcedure.
      * <TOTEM:PARA. ef-data-to-AfterProcedure>
           MODIFY CONTROL-HANDLE COLOR = COLORE-OR
           perform CONTROLLO 
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

