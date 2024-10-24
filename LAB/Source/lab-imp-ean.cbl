      *{TOTEM}PRG-COMMENT
      * lab-imp-ean.Cbl
      * lab-imp-ean.Cbl is generated by TOTEM
      * This is a generated file. DO NOT modify this file directly.
      *{TOTEM}END
       IDENTIFICATION       DIVISION.
      *{TOTEM}PRGID
       PROGRAM-ID.          lab-imp-ean.
       AUTHOR.              ANDREA EVENTI.
       DATE-WRITTEN.        marted� 1 aprile 2014 17:57:06.
       REMARKS.
      *{TOTEM}END

       ENVIRONMENT          DIVISION.
       CONFIGURATION        SECTION.
       SPECIAL-NAMES.
      *{TOTEM}SPECIAL-NAME
      * <TOTEM:EPT. INIT:lab-imp-ean, INIT:lab-imp-ean, SpecialName>
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
           COPY "tgrupgdo.sl".
      *{TOTEM}END
       DATA                 DIVISION.
       FILE                 SECTION.
      *{TOTEM}FILE
           COPY "tgrupgdo.fd".
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
       78 titolo VALUE IS "Geslux LAB - Importazione codici EAN". 
       77 Small-Font
                  USAGE IS HANDLE OF FONT SMALL-FONT.
       77 Default-Font
                  USAGE IS HANDLE OF FONT DEFAULT-FONT.
       77 data-oggi        PIC  9(8).
       77 form1-Handle
                  USAGE IS HANDLE OF WINDOW.
       77 AUTO-ID          PIC  9(6)
                  VALUE IS 0.
       77 ok-73x21-bmp     PIC  S9(9)
                  USAGE IS COMP-4
                  VALUE IS 0.
       77 cancel-73x21-bmp PIC  S9(9)
                  USAGE IS COMP-4
                  VALUE IS 0.
       77 Form1-Tb-1-Handlea
                  USAGE IS HANDLE OF WINDOW.
       77 ESCI-73X21-BMP   PIC  S9(9)
                  USAGE IS COMP-4
                  VALUE IS 0.
       77 bottone-ok-bmp   PIC  S9(9)
                  USAGE IS COMP-4
                  VALUE IS 0.
       77 bottone-cancel-bmp           PIC  S9(9)
                  USAGE IS COMP-4
                  VALUE IS 0.
       77 Verdana12-Occidentale
                  USAGE IS HANDLE OF FONT.
       77 Verdana12B-Occidentale
                  USAGE IS HANDLE OF FONT.
       77 toolbar-bmp      PIC  S9(9)
                  USAGE IS COMP-4
                  VALUE IS 0.
       77 E-ESCI           PIC  9
                  VALUE IS 1.
       77 E-CERCA          PIC  9
                  VALUE IS 1.
       77 ef-gdo-buf       PIC  x(5).
       77 STATUS-tgrupgdo  PIC  X(2).
           88 Valid-STATUS-tgrupgdo VALUE IS "00" THRU "09". 
       77 lab-gdo-buf      PIC  X(50).

      ***********************************************************
      *   Code Gen's Buffer                                     *
      ***********************************************************
       77 STATUS-Form1-FLAG-REFRESH PIC  9.
          88 Form1-FLAG-REFRESH  VALUE 1 FALSE 0. 
       77 TMP-DataSet1-tgrupgdo-BUF     PIC X(1206).
      * VARIABLES FOR RECORD LENGTH.
       77  TotemFdSlRecordClearOffset   PIC 9(5) COMP-4.
       77  TotemFdSlRecordLength        PIC 9(5) COMP-4.
      * FILE'S LOCK MODE FLAG
       77 DataSet1-tgrupgdo-LOCK-FLAG   PIC X VALUE SPACE.
           88 DataSet1-tgrupgdo-LOCK  VALUE "Y".
       77 DataSet1-KEYIS   PIC 9(3) VALUE 1.
       77 DataSet1-tgrupgdo-KEY1-ORDER  PIC X VALUE "A".
          88 DataSet1-tgrupgdo-KEY1-Asc  VALUE "A".
          88 DataSet1-tgrupgdo-KEY1-Desc VALUE "D".


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
           .

      * FRAME
       05
           Screen4-Fr-1, 
           Frame, 
           COL 1,00, 
           LINE 5,44,
           LINES 2,83 ,
           SIZE 38,20 ,
           LOWERED,
           ID IS 29,
           HEIGHT-IN-CELLS,
           WIDTH-IN-CELLS,
           .

      * PUSH BUTTON
       05
           pb-annulla, 
           Push-Button, 
           COL 31,00, 
           LINE 6,13,
           LINES 30,00 ,
           SIZE 73,00 ,
           BITMAP-HANDLE BOTTONE-CANCEL-BMP,
           BITMAP-NUMBER 1,
           UNFRAMED,
           SQUARE,
           EXCEPTION-VALUE 27,
           FLAT,
           ID IS 201,
           SELF-ACT,
           ESCAPE-BUTTON,
           AFTER PROCEDURE pb-annulla-AfterProcedure, 
           BEFORE PROCEDURE pb-annulla-BeforeProcedure, 
           .

      * PUSH BUTTON
       05
           pb-ok, 
           Push-Button, 
           COL 23,20, 
           LINE 6,13,
           LINES 30,00 ,
           SIZE 73,00 ,
           BITMAP-HANDLE BOTTONE-OK-BMP,
           BITMAP-NUMBER 1,
           UNFRAMED,
           SQUARE,
           EXCEPTION-VALUE 1000,
           FLAT,
           ID IS 200,
           AFTER PROCEDURE pb-ok-AfterProcedure, 
           BEFORE PROCEDURE pb-ok-BeforeProcedure, 
           .

      * LABEL
       05
           Screen4-blockpgm-1, 
           Label, 
           COL 2,40, 
           LINE 6,66,
           LINES 0,83 ,
           SIZE 10,50 ,
           ID IS 2,
           HEIGHT-IN-CELLS,
           WIDTH-IN-CELLS,
           TRANSPARENT,
           TITLE "BlockPgm",
           VISIBLE v-custom,
           .

      * LABEL
       05
           Screen4-La-1, 
           Label, 
           COL 1,60, 
           LINE 1,80,
           LINES 3,00 ,
           SIZE 37,00 ,
           COLOR IS 5,
           FONT IS Verdana12B-Occidentale,
           ID IS 1,
           HEIGHT-IN-CELLS,
           WIDTH-IN-CELLS,
           CENTER,
           TRANSPARENT,
           TITLE "ATTENZIONE!!! I codice EAN gi� presenti verranno sovra
      -    "scritti con quelli nuovi. Procedere con l'importazione?",
           .

      *{TOTEM}END

      *{TOTEM}LINKPARA
       PROCEDURE  DIVISION USING LK-BLOCKPGM, USER-CODI, LIVELLO-ABIL.
      *{TOTEM}END

      *{TOTEM}DECLARATIVE
       DECLARATIVES.
      * <TOTEM:EPT. INIT:lab-imp-ean, INIT:lab-imp-ean, BeforeDeclarative>
      * <TOTEM:END>
       INPUT-ERROR SECTION.
           USE AFTER STANDARD ERROR PROCEDURE ON INPUT.
       0100-DECL.
           EXIT.
       I-O-ERROR SECTION.
           USE AFTER STANDARD ERROR PROCEDURE ON I-O.
       0200-DECL.
           EXIT.
       OUTPUT-ERROR SECTION.
           USE AFTER STANDARD ERROR PROCEDURE ON OUTPUT.
       0300-DECL.
           EXIT.
       TRANSACTION-ERROR SECTION.
           USE AFTER STANDARD ERROR PROCEDURE ON TRANSACTION.
       0400-DECL.
           EXIT.
       END DECLARATIVES.
      *{TOTEM}END

       MAIN-LOGIC.
      *{TOTEM}ENTRY-BEFPRG
      *    Before-Program
           PERFORM ginqui-Ev-Before-Program
      *{TOTEM}END
           PERFORM INITIALIZE-ROUTINE.
      * run main screen
      *{TOTEM}RUN-MAINSCR
           PERFORM Form1-OPEN-ROUTINE.
      *{TOTEM}END

      *{TOTEM}COPY-PROCEDURE
       EXIT-STOP-ROUTINE.
           PERFORM CLOSE-FILE-RTN
      * <TOTEM:EPT. INIT:lab-imp-ean, INIT:lab-imp-ean, BeforeDestroyResource>
      * <TOTEM:END>
           DESTROY Verdana12B-Occidentale
           DESTROY Verdana12-Occidentale
           CALL "w$bitmap" USING WBITMAP-DESTROY, BOTTONE-CANCEL-BMP
           CALL "w$bitmap" USING WBITMAP-DESTROY, BOTTONE-OK-BMP
      *    After-Program
           PERFORM ginqui-Ev-After-Program
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
      * open files
           PERFORM OPEN-FILE-RTN.
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
      * pb-annulla
           COPY RESOURCE "BOTTONE-CANCEL.BMP".
           CALL "w$bitmap" USING WBITMAP-LOAD "BOTTONE-CANCEL.BMP", 
                   GIVING BOTTONE-CANCEL-BMP.
      * pb-ok
           COPY RESOURCE "BOTTONE-OK.BMP".
           CALL "w$bitmap" USING WBITMAP-LOAD "BOTTONE-OK.BMP", 
                   GIVING BOTTONE-OK-BMP.
           .

       INIT-RES.
           .

       INIT-POPUP.
           .

       OPEN-FILE-RTN.
      *    Before Open
           PERFORM OPEN-tgrupgdo
      *    After Open
           .

       OPEN-tgrupgdo.
      * <TOTEM:EPT. INIT:lab-imp-ean, FD:tgrupgdo, BeforeOpen>
      * <TOTEM:END>
           OPEN  INPUT tgrupgdo
           IF NOT Valid-STATUS-tgrupgdo
              PERFORM  Form1-EXTENDED-FILE-STATUS
              GO TO EXIT-STOP-ROUTINE
           END-IF
      * <TOTEM:EPT. INIT:lab-imp-ean, FD:tgrupgdo, AfterOpen>
      * <TOTEM:END>
           .

       CLOSE-FILE-RTN.
      *    Before Close
           PERFORM CLOSE-tgrupgdo
      *    After Close
           .

       CLOSE-tgrupgdo.
      * <TOTEM:EPT. INIT:lab-imp-ean, FD:tgrupgdo, BeforeClose>
      * <TOTEM:END>
           CLOSE tgrupgdo
           .

       DataSet1-tgrupgdo-INITSTART.
           EVALUATE DataSet1-KEYIS
           WHEN 1
              IF DataSet1-tgrupgdo-KEY1-Asc
                 MOVE Low-Value TO gdo-chiave OF tgrupgdo
              ELSE
                 MOVE High-Value TO gdo-chiave OF tgrupgdo
              END-IF
           END-EVALUATE
           .

       DataSet1-tgrupgdo-INITEND.
           EVALUATE DataSet1-KEYIS
           WHEN 1
              IF DataSet1-tgrupgdo-KEY1-Asc
                 MOVE High-Value TO gdo-chiave OF tgrupgdo
              ELSE
                 MOVE Low-Value TO gdo-chiave OF tgrupgdo
              END-IF
           END-EVALUATE
           .

       DataSet1-CHANGETO-KEY1.
           MOVE 1 TO DataSet1-KEYIS
           .   

       DataSet1-Change-CurrentKey-Asc.
           EVALUATE DataSet1-KEYIS
           WHEN 1
              MOVE "A" TO DataSet1-tgrupgdo-KEY1-ORDER
           END-EVALUATE
           .

       DataSet1-Change-CurrentKey-Desc.
           EVALUATE DataSet1-KEYIS
           WHEN 1
              MOVE "D" TO DataSet1-tgrupgdo-KEY1-ORDER
           END-EVALUATE
           .

      * tgrupgdo
       DataSet1-tgrupgdo-START.
           EVALUATE DataSet1-KEYIS
           WHEN 1
              IF DataSet1-tgrupgdo-KEY1-Asc
                 START tgrupgdo KEY >= gdo-chiave OF tgrupgdo
              ELSE
                 START tgrupgdo KEY <= gdo-chiave OF tgrupgdo
              END-IF
           END-EVALUATE
           .

       DataSet1-tgrupgdo-START-NOTGREATER.
           EVALUATE DataSet1-KEYIS
           WHEN 1
              IF DataSet1-tgrupgdo-KEY1-Asc
                 START tgrupgdo KEY <= gdo-chiave OF tgrupgdo
              ELSE
                 START tgrupgdo KEY >= gdo-chiave OF tgrupgdo
              END-IF
           END-EVALUATE
           .

       DataSet1-tgrupgdo-START-GREATER.
           EVALUATE DataSet1-KEYIS
           WHEN 1
              IF DataSet1-tgrupgdo-KEY1-Asc
                 START tgrupgdo KEY > gdo-chiave OF tgrupgdo
              ELSE
                 START tgrupgdo KEY < gdo-chiave OF tgrupgdo
              END-IF
           END-EVALUATE
           .

       DataSet1-tgrupgdo-START-LESS.
           EVALUATE DataSet1-KEYIS
           WHEN 1
              IF DataSet1-tgrupgdo-KEY1-Asc
                 START tgrupgdo KEY < gdo-chiave OF tgrupgdo
              ELSE
                 START tgrupgdo KEY > gdo-chiave OF tgrupgdo
              END-IF
           END-EVALUATE
           .

       DataSet1-tgrupgdo-Read.
      * <TOTEM:EPT. FD:DataSet1, FD:tgrupgdo, BeforeRead>
      * <TOTEM:END>
      * <TOTEM:EPT. FD:DataSet1, FD:tgrupgdo, BeforeReadRecord>
      * <TOTEM:END>
           EVALUATE DataSet1-KEYIS
           WHEN 1
              IF DataSet1-tgrupgdo-LOCK
                 READ tgrupgdo WITH LOCK 
                 KEY gdo-chiave OF tgrupgdo
              ELSE
                 READ tgrupgdo WITH NO LOCK 
                 KEY gdo-chiave OF tgrupgdo
              END-IF
           END-EVALUATE
           MOVE STATUS-tgrupgdo TO TOTEM-ERR-STAT 
           MOVE "tgrupgdo" TO TOTEM-ERR-FILE
           MOVE "READ" TO TOTEM-ERR-MODE
      * <TOTEM:EPT. FD:DataSet1, FD:tgrupgdo, AfterRead>
      * <TOTEM:END>
      * <TOTEM:EPT. FD:DataSet1, FD:tgrupgdo, AfterReadRecord>
      * <TOTEM:END>
           .

       DataSet1-tgrupgdo-Read-Next.
      * <TOTEM:EPT. FD:DataSet1, FD:tgrupgdo, BeforeRead>
      * <TOTEM:END>
      * <TOTEM:EPT. FD:DataSet1, FD:tgrupgdo, BeforeReadNext>
      * <TOTEM:END>
           EVALUATE DataSet1-KEYIS
           WHEN 1
              IF DataSet1-tgrupgdo-KEY1-Asc
                 IF DataSet1-tgrupgdo-LOCK
                    READ tgrupgdo NEXT WITH LOCK
                 ELSE
                    READ tgrupgdo NEXT WITH NO LOCK
                 END-IF
              ELSE
                 IF DataSet1-tgrupgdo-LOCK
                    READ tgrupgdo PREVIOUS WITH LOCK
                 ELSE
                    READ tgrupgdo PREVIOUS WITH NO LOCK
                 END-IF
              END-IF
           END-EVALUATE
           MOVE STATUS-tgrupgdo TO TOTEM-ERR-STAT
           MOVE "tgrupgdo" TO TOTEM-ERR-FILE
           MOVE "READ NEXT" TO TOTEM-ERR-MODE
      * <TOTEM:EPT. FD:DataSet1, FD:tgrupgdo, AfterRead>
      * <TOTEM:END>
      * <TOTEM:EPT. FD:DataSet1, FD:tgrupgdo, AfterReadNext>
      * <TOTEM:END>
           .

       DataSet1-tgrupgdo-Read-Prev.
      * <TOTEM:EPT. FD:DataSet1, FD:tgrupgdo, BeforeRead>
      * <TOTEM:END>
      * <TOTEM:EPT. FD:DataSet1, FD:tgrupgdo, BeforeReadPrev>
      * <TOTEM:END>
           EVALUATE DataSet1-KEYIS
           WHEN 1
              IF DataSet1-tgrupgdo-KEY1-Asc
                 IF DataSet1-tgrupgdo-LOCK
                    READ tgrupgdo PREVIOUS WITH LOCK
                 ELSE
                    READ tgrupgdo PREVIOUS WITH NO LOCK
                 END-IF
              ELSE
                 IF DataSet1-tgrupgdo-LOCK
                    READ tgrupgdo NEXT WITH LOCK
                 ELSE
                    READ tgrupgdo NEXT WITH NO LOCK
                 END-IF
              END-IF
           END-EVALUATE
           MOVE STATUS-tgrupgdo TO TOTEM-ERR-STAT
           MOVE "tgrupgdo" TO TOTEM-ERR-FILE
           MOVE "READ PREVIOUS" TO TOTEM-ERR-MODE
      * <TOTEM:EPT. FD:DataSet1, FD:tgrupgdo, AfterRead>
      * <TOTEM:END>
      * <TOTEM:EPT. FD:DataSet1, FD:tgrupgdo, AfterReadPrev>
      * <TOTEM:END>
           .

       DataSet1-tgrupgdo-Rec-Write.
      * <TOTEM:EPT. FD:DataSet1, FD:tgrupgdo, BeforeWrite>
      * <TOTEM:END>
           MOVE STATUS-tgrupgdo TO TOTEM-ERR-STAT
           MOVE "tgrupgdo" TO TOTEM-ERR-FILE
           MOVE "WRITE" TO TOTEM-ERR-MODE
      * <TOTEM:EPT. FD:DataSet1, FD:tgrupgdo, AfterWrite>
      * <TOTEM:END>
           .

       DataSet1-tgrupgdo-Rec-Rewrite.
      * <TOTEM:EPT. FD:DataSet1, FD:tgrupgdo, BeforeRewrite>
      * <TOTEM:END>
           MOVE STATUS-tgrupgdo TO TOTEM-ERR-STAT
           MOVE "tgrupgdo" TO TOTEM-ERR-FILE
           MOVE "REWRITE" TO TOTEM-ERR-MODE
      * <TOTEM:EPT. FD:DataSet1, FD:tgrupgdo, AfterRewrite>
      * <TOTEM:END>
           .

       DataSet1-tgrupgdo-Rec-Delete.
      * <TOTEM:EPT. FD:DataSet1, FD:tgrupgdo, BeforeDelete>
      * <TOTEM:END>
           MOVE STATUS-tgrupgdo TO TOTEM-ERR-STAT
           MOVE "tgrupgdo" TO TOTEM-ERR-FILE
           MOVE "DELETE" TO TOTEM-ERR-MODE
      * <TOTEM:EPT. FD:DataSet1, FD:tgrupgdo, AfterDelete>
      * <TOTEM:END>
           .

       DataSet1-INIT-RECORD.
           INITIALIZE gdo-rec OF tgrupgdo
           .


      * FD's Initialize Paragraph
       DataSet1-tgrupgdo-INITREC.
           INITIALIZE gdo-rec OF tgrupgdo
               REPLACING NUMERIC       DATA BY ZEROS
                         ALPHANUMERIC  DATA BY SPACES
                         ALPHABETIC    DATA BY SPACES
           .

      *
       DataSet1-DISPATCH-BUFTOFLD.
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
              LINES 7,28,
              SIZE 38,20,
              HEIGHT-IN-CELLS,
              WIDTH-IN-CELLS,
              COLOR 65793,
              CONTROL FONT Verdana12-Occidentale,
              LABEL-OFFSET 23,
              LINK TO THREAD,
              NO SCROLL,
              TITLE-BAR,
              TITLE TITOLO,
              WITH SYSTEM MENU,
              USER-GRAY,
              USER-WHITE,
              No WRAP,
              EVENT PROCEDURE Screen4-Event-Proc,
              HANDLE IS form1-Handle,
      * <TOTEM:EPT. FORM:Form1, FORM:Form1, AfterCreateWin>
      * <TOTEM:END>


      * Tool Bar    
      * Status-bar
           DISPLAY Form1 UPON form1-Handle
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
           DESTROY form1-Handle
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
                 PERFORM pb-ok-LinkTo
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
           DISPLAY Form1 UPON form1-Handle
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
           MOVE TITOLO TO TOTEM-MSG-TITLE
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



       Screen4-Event-Proc.
           .

      * USER DEFINE PARAGRAPH
       PARAGRAFO-COPY.
      * <TOTEM:PARA. PARAGRAFO-COPY>
           copy "color-custom.cpy".

      ***--- DUMMY ---> NON TOCCARE!!!!
       STATUS-HELP.
           continue 
           .
      * <TOTEM:END>

      * EVENT PARAGRAPH
       ginqui-Ev-Before-Program.
      * <TOTEM:PARA. ginqui-Ev-Before-Program>
           move LK-BL-PROG-ID    TO COMO-PROG-ID 
           .
      * <TOTEM:END>
       ginqui-Ev-After-Program.
      * <TOTEM:PARA. ginqui-Ev-After-Program>
           SET LK-BL-CANCELLAZIONE TO TRUE.
           MOVE COMO-PROG-ID       TO LK-BL-PROG-ID.
           CALL "BLOCKPGM"  USING LK-BLOCKPGM 
           .
      * <TOTEM:END>
       pb-ok-BeforeProcedure.
      * <TOTEM:PARA. pb-ok-BeforeProcedure>
           modify pb-ok, bitmap-number 2 
           .
      * <TOTEM:END>
       pb-ok-AfterProcedure.
      * <TOTEM:PARA. pb-ok-AfterProcedure>
           modify pb-ok, bitmap-number 1 
           .
      * <TOTEM:END>
       pb-ok-LinkTo.
      * <TOTEM:PARA. pb-ok-LinkTo>
           call "W$MOUSE" using set-mouse-shape, wait-pointer.

           call   "lab-imp-ean-p" using form1-handle, user-codi.
           cancel "lab-imp-ean-p".
           call "W$MOUSE" using set-mouse-shape, arrow-pointer.

           move 27 to key-status 
           .
      * <TOTEM:END>
       pb-annulla-BeforeProcedure.
      * <TOTEM:PARA. pb-annulla-BeforeProcedure>
           modify pb-annulla, bitmap-number 2 
           .
      * <TOTEM:END>
       pb-annulla-AfterProcedure.
      * <TOTEM:PARA. pb-annulla-AfterProcedure>
           modify pb-annulla, bitmap-number 1 
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

