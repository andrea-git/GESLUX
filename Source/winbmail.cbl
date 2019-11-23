       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID.                   WINBMAIL.
      *PROGRAM-ID.                   WINBMAIL.
      *
      **--------------------------------------------------------------*
      ** KISS GmbH Personalsysteme                  66538 Neunkrichen *
      ** Wellesweiler Strasse 95                    Germany           *
      **--------------------------------------------------------------*
      **                                                              *
      ** Sending Mail via C$SOCEKT                                    *
      **                                                              *
      **--------------------------------------------------------------*
      ** Operatingsystem ........:   various supported by AcuCorp     *
      ** Compiler/Runtime .......:   AcuCorp Development System       *
      **                             Version 6.1.0 or higher          *
      **--------------------------------------------------------------*
      ** Used Files                                                   *
      **--------------------------------------------------------------*
      ** Int. Filename ! Openmode   ! Comments                        *
      **---------------+------------+---------------------------------*
      **               !            !                                 *
      ** SMAILIN       !  INPUT     ! attachment file                 *
      **               !            !                                 *
      ** SMAILPR       !  EXTEND    ! logfile                         *
      **               !            !                                 *
      **--------------------------------------------------------------*
      ** program created by ........: Doerrenbaecher-Alles, Peter     *
      ** program created at ........: 19.03.2004                      *
      ** last change at / rev. .....: 19.03.2004 / 01-10              *
      ** last change by ............: Doerrenbaecher-Alles, Peter     *
      **--------------------------------------------------------------*
      ** Date       !  rev.  ! Comment                                *
      **------------+--------+----------------------------------------*
      ** 19.03.2004 ! 01-00  ! final release                          *
      ** 04.08.2004 ! 01-10  ! minor changes; Expand logfile info     *
      **--------------------------------------------------------------*
      /
       ENVIRONMENT DIVISION.
      *
       CONFIGURATION SECTION.
      *
       SPECIAL-NAMES.
      *
           DECIMAL-POINT            IS COMMA.
      /
      *{Bench}activex-def
      *{Bench}end
       INPUT-OUTPUT SECTION.
      *
       FILE-CONTROL.
      *
           SELECT   SMAILIN      ASSIGN         TO      SMAIL-FILE
                                 ORGANIZATION   IS      SEQUENTIAL
                                 ACCESS         IS      SEQUENTIAL
                                 FILE STATUS    IS FILE-STATUS.
      *
           SELECT   SMAILPR      ASSIGN         TO      'WINBMAIL.LOG'
                                 ORGANIZATION   IS LINE SEQUENTIAL
                                 ACCESS         IS      SEQUENTIAL
                                 FILE STATUS    IS FILE-STATUS.

      /
      *{Bench}file-control
      *{Bench}end
       DATA DIVISION.
      *
       FILE SECTION.
      *
       FD  SMAILIN
      *
           RECORD CONTAINS  2040   CHARACTERS
           DATA   RECORD      IS   SMI-SATZ.
      *
       01  SMI-SATZ.
      *
           03 SMI-DATEN              PIC X(01)  OCCURS 2040 TIMES.
      /
       FD  SMAILPR
      *
           RECORD CONTAINS   256   CHARACTERS
           DATA   RECORD      IS   SMP-SATZ.
      *
       01  SMP-SATZ.
      *
           03 SMP-DATEN              PIC X(256).
      /
      *{Bench}file
      *{Bench}end
       WORKING-STORAGE SECTION.
      *{Bench}acu-def
       COPY "acugui.def".
      * COPY "acucobol.def".
       COPY "crtvars.def".
       COPY "showmsg.def".
      *{Bench}end
      *
       01  PROGRAMM                  PIC X(14)  VALUE 'WINBMAIL-01-10'.
      *                                         programname for dump
      **--------------------------------------------------------------*
      **            global dataarea                                   *
      **--------------------------------------------------------------*
       01  FILE-STATUS               PIC X(02)  VALUE '00'.
      *                                         file status
       01  ARBEITS-FELDER.
           03 WINSPLIT-DATEI         PIC X(256) VALUE SPACES.
           03 WINSPLIT-FILE          PIC X(256) VALUE SPACES.
           03 WINSPLIT-FILE-LEN      PIC 9(03)  VALUE ZEROS.
           03 W-EXT-STATUS           PIC X(10)  VALUE SPACES.
           03 W-TEXT-MESSAGE         PIC X(40)  VALUE SPACES.
           03 W-FILE-NAME            PIC X(40)  VALUE SPACES.
           03 W-PROTOKOLL            PIC 9(01)  VALUE ZEROS.
              88 W-PROT-WRITE                   VALUE 1.
           03 W-FEHLER               PIC 9(01)  VALUE ZEROS.
              88 POP3-ERROR                     VALUE 1.
           03 W-PROT-LINE            PIC X(256) VALUE SPACES.
           03 W-PROT-FUNKTION        PIC 9(01)  VALUE ZEROS.
              88 W-PROT-OUT                     VALUE 1.
              88 W-PROT-IN                      VALUE 2.
           03 W-STARS                PIC X(256) VALUE SPACES.
           03 W-PRT-NR               PIC ZZZZ9.
           03 W-ERROR                PIC 9(07)  VALUE ZEROS.
           03 W-MELDUNG              PIC X(128) VALUE SPACES.
           03 W-END-OF-MAIL          PIC X(09)  VALUE SPACES.
           03 W-EOF                  PIC 9(01)  VALUE ZEROS.
           03 WRP-IND1               PIC 9(04)  VALUE ZEROS.
           03 WRP-IND2               PIC 9(04)  VALUE ZEROS.
           03 W-FILE-SIZE            PIC 9(18)  VALUE ZEROS.
           03 W-FILE-INDEX           PIC 9(18)  VALUE ZEROS.
           03 W-FILE-SHOW            PIC 9(18)  VALUE ZEROS.
           03 W-FILE-COMP            PIC 9(10)V9(08)
                                                VALUE ZEROS.
           03 W-FILE-COMP-RED        REDEFINES  W-FILE-COMP.
              05 W-FILE-COMP-VK      PIC 9(10).
              05 W-FILE-COMP-NK      PIC 9(08).
           03 BLOCK-IND              PIC 9(05)  VALUE ZEROS.
           03 BLOCK-MAX              PIC 9(05)  VALUE ZEROS.
           03 W-FAKTOR               PIC 9(02)  VALUE ZEROS.
           03 W-ADDITION             PIC 9(03)V99
                                                VALUE ZEROS.
           03 W-ANZ-BLOCKS           PIC 9(08)  VALUE ZEROS.
           03 W-MAX-CHARS            PIC 9(08)  VALUE ZEROS.
           03 W-MAX-BLOCKS           PIC 9(08)V9(04)
                                                VALUE ZEROS.
           03 W-MAX-BLOCKS-RED       REDEFINES  W-MAX-BLOCKS.
              05 W-MAX-BLOCKS-VK     PIC 9(08).
              05 W-MAX-BLOCKS-NK     PIC 9(04).
           03 W-SIZE                 PIC 9(18)  VALUE ZEROS.
           03 W-SIZE-REST            PIC 9(02)  VALUE ZEROS.
           03 W-IND                  PIC 9(05)  VALUE ZEROS.
           03 W-IND1                 PIC 9(05)  VALUE ZEROS.
           03 W-IND2                 PIC 9(05)  VALUE ZEROS.
           03 W-IND3                 PIC 9(05)  VALUE ZEROS.
           03 W-IND4                 PIC 9(05)  VALUE ZEROS.
           03 W-IND5                 PIC 9(05)  VALUE ZEROS.
           03 W-IND6                 PIC 9(05)  VALUE ZEROS.
           03 W-BODY-LEN             PIC 9(05)  VALUE ZEROS.
           03 W-DATUM                PIC 9(08)  VALUE ZEROS.
           03 W-TIME                 PIC 9(08)  VALUE ZEROS.
           03 W-GROESSE              PIC 9(18)  VALUE ZEROS.
           03 W-BYTES.
              05 W-BYTE-IN           PIC X(08)  OCCURS 3 TIMES.
           03 W-BYTES-OUT            REDEFINES  W-BYTES.
              05 W-BYTE-OUT                     OCCURS 4 TIMES.
                 07 W-BYTE-O         PIC 9(01)  OCCURS 6 TIMES.
           03 SMAIL-FILE             PIC X(256) VALUE SPACES.
           03 W-ANHANG               PIC X(256) OCCURS 3 TIMES.
           03 W-ANHANG-NR            PIC 9(01)  VALUE ZEROS.
           03 W-AUTH-TABELLE                    VALUE SPACES.
              05 W-AUTH-ZEILE        PIC X(128) OCCURS 256 TIMES.
      **--------------------------------------------------------------*
      **            area for creating ORD (char)                      *
      **--------------------------------------------------------------*
       01  ORDINATEN-FUNKTION.
           03 ORDINATE               PIC 9(03)  USAGE IS COMP-X.
           03 ORDINATE-RED           REDEFINES  ORDINATE.
              05 FILLER              PIC X(01).
              05 ORD-ZEICHEN         PIC X(01).
      **--------------------------------------------------------------*
      **            REPLY-Fields from RECEIVE-command SMTP            *
      **--------------------------------------------------------------*
       01  W-REPLY                   PIC X(03)  VALUE SPACES.
           88 W-ERROR-CONNECT                   VALUE '421'.
           88 W-ERROR-HELO                      VALUE '500', '501',
                                                      '504', '421'.
           88 W-ERROR-MAIL                      VALUE '552', '451',
                                                      '452', '421',
                                                      '500', '501'.
           88 W-ERROR-RCPT                      VALUE '450', '451',
                                                      '452', '421',
                                                      '500', '501',
                                                      '503', '550',
                                                      '551', '552',
                                                      '553'.
           88 W-ERROR-DATA-1                    VALUE '451', '454',
                                                      '500', '501',
                                                      '503', '421'.
           88 W-ERROR-DATA-2                    VALUE '451', '452',
                                                      '552', '554'.
           88 W-ERROR-RSET                      VALUE '500', '501',
                                                      '504', '421'.
           88 W-ERROR-SEND                      VALUE '552', '451',
                                                      '452', '421',
                                                      '500', '501',
                                                      '502'.
           88 W-ERROR-SOML                      VALUE '552', '451',
                                                      '452', '421',
                                                      '500', '501',
                                                      '502'.
           88 W-ERROR-SAML                      VALUE '552', '451',
                                                      '452', '421',
                                                      '500', '501',
                                                      '502'.
           88 W-ERROR-VERIFY                    VALUE '550', '551',
                                                      '553', '421',
                                                      '500', '501',
                                                      '502', '504'.
           88 W-ERROR-EXPN                      VALUE '550', '421',
                                                      '500', '501',
                                                      '502', '504'.
           88 W-ERROR-HELP                      VALUE '500', '501',
                                                      '502', '504',
                                                      '421'.
           88 W-ERROR-NOOP                      VALUE '500', '421'.
           88 W-ERROR-QUIT                      VALUE '500'.
           88 W-ERROR-TURN                      VALUE '500', '502',
                                                      '503'.
           88 W-AUTH-OK                         VALUE '334'.
           88 W-AUTH-SUCCEEDED                  VALUE '235'.
      **--------------------------------------------------------------*
      **           mMail-spezific fields                              *
      **--------------------------------------------------------------*
       01  MAIL-FELDER.
           03 SOCKET-HANDLE                     USAGE IS HANDLE.
           03 SOCKET-NUMBER          PIC 9(09)  USAGE COMP-5
                                                VALUE ZEROS.
           03 WRK-DATA-RECORD        PIC X(999) VALUE SPACES.
           03 WRK-MULTIPART          PIC X(60)  VALUE SPACES.
           03 WRK-HOST-NAME          PIC X(256) VALUE SPACES.
           03 WRK-EOF-STRING         PIC X(05)  VALUE SPACES.
           03 WRK-EOF-STRING2        PIC X(05)  VALUE SPACES.
           03 INPUT-CHAR             PIC X(01)  VALUE SPACES.
           03 BYTES-READ             PIC 9(09)  VALUE ZEROS.
           03 BYTES-TO-SEND          PIC 9(03)  VALUE ZEROS.
           03 FINISHED               PIC 9(01)  VALUE ZEROS.
           03 BYTE-READ              PIC X(01)  VALUE SPACES.
           03 X                      PIC 9(03)  VALUE ZEROS.
           03 CRLF                   PIC X(02)  VALUE SPACES.
      **--------------------------------------------------------------*
      **            fields for display progress bar                   *
      **--------------------------------------------------------------*
       01  FRAME-TITLE.
           03 STATUS-PROZENT         PIC ZZ9.
           03 FILLER                 PIC X(01)  VALUE '%'.

       01  COLOR-1                   PIC 9(05)  VALUE ZEROS.
       01  COLOR-2                   PIC 9(05)  VALUE ZEROS.
       01  MAI-PROGRESS-FRAME                   HANDLE OF FRAME.
       01  MAI-PROGRESS-HANDLE                  HANDLE OF WINDOW.
       01  MAI-BAR-PROZENT           PIC 9(03)  VALUE ZEROS.
      **--------------------------------------------------------------*
      **            C$FILEINFO definitions                            *
      **--------------------------------------------------------------*
       01  FILE-INFO-BLOCK.
           03 FILE-INFO-SIZE         PIC X(08)  COMP-X.
           03 FILE-INFO-DATE         PIC 9(08)  COMP-X.
           03 FILE-INFO-TIME         PIC 9(08)  COMP-X.
      *
       01  FILE-INFO-STATUS          PIC 9(04).
           88 FILE-FOUND                        VALUE 0.
           88 FILE-NOT-FOUND                    VALUE 1.
           88 FILE-IS-EMPTY                     VALUE 2.
      *
       01  FILE-INFO-NAME            PIC X(256).
      **--------------------------------------------------------------*
      **            Parameter fuer die Ermittlung der String-Laenge   *
      **--------------------------------------------------------------*
       01  WINLNG-BLOCK.
           03 WINLNG-TEXT            PIC X(1024).
      *                                         Textfeld
           03 WINLNG-SIZE            PIC 9(04).
      *                                         Letztes belegtes Zeichen
      **--------------------------------------------------------------*
      **            Parameter fuer WINENV (Bearb. Umgebungsvariablen) *
      **--------------------------------------------------------------*
       01  WINENV-BLOCK.
           03 WINENV-FUNKTION        PIC X(01).
              88 WINENV-SET                     VALUE 'S'.
              88 WINENV-GET                     VALUE 'G'.
              88 WINENV-GET-PATH                VALUE 'F'.
           03 WINENV-VARNAME         PIC X(128).
      *                                         Name der Variablen
           03 WINENV-VALUE.
              05 FILLER              PIC X(1024).
      *                                         Inhalt der Variablen.
      **--------------------------------------------------------------*
      **            Parameter zum Konvertieren Text nach Zahl         *
      **--------------------------------------------------------------*
       01  WINNUM-BLOCK.
           03 WINNUM-AC.
              05 WINNUM-AC1          PIC 9(01).
              05 WUINUM-AC2          PIC 9(01).
           03 WINNUM-INP             PIC X(20).
           03 WINNUM-INP-BYTES       REDEFINES  WINNUM-INP.
              05 WINNUM-BYTE         PIC X(01)  OCCURS 20 TIMES.
           03 WINNUM-LIMITS.
              05 WINNUM-NDB          PIC 9(02).
              05 WINNUM-NDA          PIC 9(02).
           03 WINNUM-OUT             PIC S9(13)V9(05).
           03 WINNUM-OUT-R           REDEFINES  WINNUM-OUT.
              05 WINNUM-VORZAHL      PIC X(13).
              05 WINNUM-NACHZAHL     PIC X(05).
           03 WINNUM-ERRKZ.
              05 WINNUM-SYSTEM       PIC X(03).
              05 WINNUM-ERRNR        PIC 9(04).
           03 WINNUM-INTERR          PIC 9(02).
      /
      **--------------------------------------------------------------*
      **            Bit-Table for all Chars from 0 up to 255          *
      **--------------------------------------------------------------*
       01  W-BIT-TABELLE.
           03 W-BIT-DEF.
              05 FILLER              PIC X(08)  VALUE '00000000'.
              05 FILLER              PIC X(08)  VALUE '00000001'.
              05 FILLER              PIC X(08)  VALUE '00000010'.
              05 FILLER              PIC X(08)  VALUE '00000011'.
              05 FILLER              PIC X(08)  VALUE '00000100'.
              05 FILLER              PIC X(08)  VALUE '00000101'.
              05 FILLER              PIC X(08)  VALUE '00000110'.
              05 FILLER              PIC X(08)  VALUE '00000111'.
              05 FILLER              PIC X(08)  VALUE '00001000'.
              05 FILLER              PIC X(08)  VALUE '00001001'.
              05 FILLER              PIC X(08)  VALUE '00001010'.
              05 FILLER              PIC X(08)  VALUE '00001011'.
              05 FILLER              PIC X(08)  VALUE '00001100'.
              05 FILLER              PIC X(08)  VALUE '00001101'.
              05 FILLER              PIC X(08)  VALUE '00001110'.
              05 FILLER              PIC X(08)  VALUE '00001111'.
              05 FILLER              PIC X(08)  VALUE '00010000'.
              05 FILLER              PIC X(08)  VALUE '00010001'.
              05 FILLER              PIC X(08)  VALUE '00010010'.
              05 FILLER              PIC X(08)  VALUE '00010011'.
              05 FILLER              PIC X(08)  VALUE '00010100'.
              05 FILLER              PIC X(08)  VALUE '00010101'.
              05 FILLER              PIC X(08)  VALUE '00010110'.
              05 FILLER              PIC X(08)  VALUE '00010111'.
              05 FILLER              PIC X(08)  VALUE '00011000'.
              05 FILLER              PIC X(08)  VALUE '00011001'.
              05 FILLER              PIC X(08)  VALUE '00011010'.
              05 FILLER              PIC X(08)  VALUE '00011011'.
              05 FILLER              PIC X(08)  VALUE '00011100'.
              05 FILLER              PIC X(08)  VALUE '00011101'.
              05 FILLER              PIC X(08)  VALUE '00011110'.
              05 FILLER              PIC X(08)  VALUE '00011111'.
              05 FILLER              PIC X(08)  VALUE '00100000'.
              05 FILLER              PIC X(08)  VALUE '00100001'.
              05 FILLER              PIC X(08)  VALUE '00100010'.
              05 FILLER              PIC X(08)  VALUE '00100011'.
              05 FILLER              PIC X(08)  VALUE '00100100'.
              05 FILLER              PIC X(08)  VALUE '00100101'.
              05 FILLER              PIC X(08)  VALUE '00100110'.
              05 FILLER              PIC X(08)  VALUE '00100111'.
              05 FILLER              PIC X(08)  VALUE '00101000'.
              05 FILLER              PIC X(08)  VALUE '00101001'.
              05 FILLER              PIC X(08)  VALUE '00101010'.
              05 FILLER              PIC X(08)  VALUE '00101011'.
              05 FILLER              PIC X(08)  VALUE '00101100'.
              05 FILLER              PIC X(08)  VALUE '00101101'.
              05 FILLER              PIC X(08)  VALUE '00101110'.
              05 FILLER              PIC X(08)  VALUE '00101111'.
              05 FILLER              PIC X(08)  VALUE '00110000'.
              05 FILLER              PIC X(08)  VALUE '00110001'.
              05 FILLER              PIC X(08)  VALUE '00110010'.
              05 FILLER              PIC X(08)  VALUE '00110011'.
              05 FILLER              PIC X(08)  VALUE '00110100'.
              05 FILLER              PIC X(08)  VALUE '00110101'.
              05 FILLER              PIC X(08)  VALUE '00110110'.
              05 FILLER              PIC X(08)  VALUE '00110111'.
              05 FILLER              PIC X(08)  VALUE '00111000'.
              05 FILLER              PIC X(08)  VALUE '00111001'.
              05 FILLER              PIC X(08)  VALUE '00111010'.
              05 FILLER              PIC X(08)  VALUE '00111011'.
              05 FILLER              PIC X(08)  VALUE '00111100'.
              05 FILLER              PIC X(08)  VALUE '00111101'.
              05 FILLER              PIC X(08)  VALUE '00111110'.
              05 FILLER              PIC X(08)  VALUE '00111111'.
              05 FILLER              PIC X(08)  VALUE '01000000'.
              05 FILLER              PIC X(08)  VALUE '01000001'.
              05 FILLER              PIC X(08)  VALUE '01000010'.
              05 FILLER              PIC X(08)  VALUE '01000011'.
              05 FILLER              PIC X(08)  VALUE '01000100'.
              05 FILLER              PIC X(08)  VALUE '01000101'.
              05 FILLER              PIC X(08)  VALUE '01000110'.
              05 FILLER              PIC X(08)  VALUE '01000111'.
              05 FILLER              PIC X(08)  VALUE '01001000'.
              05 FILLER              PIC X(08)  VALUE '01001001'.
              05 FILLER              PIC X(08)  VALUE '01001010'.
              05 FILLER              PIC X(08)  VALUE '01001011'.
              05 FILLER              PIC X(08)  VALUE '01001100'.
              05 FILLER              PIC X(08)  VALUE '01001101'.
              05 FILLER              PIC X(08)  VALUE '01001110'.
              05 FILLER              PIC X(08)  VALUE '01001111'.
              05 FILLER              PIC X(08)  VALUE '01010000'.
              05 FILLER              PIC X(08)  VALUE '01010001'.
              05 FILLER              PIC X(08)  VALUE '01010010'.
              05 FILLER              PIC X(08)  VALUE '01010011'.
              05 FILLER              PIC X(08)  VALUE '01010100'.
              05 FILLER              PIC X(08)  VALUE '01010101'.
              05 FILLER              PIC X(08)  VALUE '01010110'.
              05 FILLER              PIC X(08)  VALUE '01010111'.
              05 FILLER              PIC X(08)  VALUE '01011000'.
              05 FILLER              PIC X(08)  VALUE '01011001'.
              05 FILLER              PIC X(08)  VALUE '01011010'.
              05 FILLER              PIC X(08)  VALUE '01011011'.
              05 FILLER              PIC X(08)  VALUE '01011100'.
              05 FILLER              PIC X(08)  VALUE '01011101'.
              05 FILLER              PIC X(08)  VALUE '01011110'.
              05 FILLER              PIC X(08)  VALUE '01011111'.
              05 FILLER              PIC X(08)  VALUE '01100000'.
              05 FILLER              PIC X(08)  VALUE '01100001'.
              05 FILLER              PIC X(08)  VALUE '01100010'.
              05 FILLER              PIC X(08)  VALUE '01100011'.
              05 FILLER              PIC X(08)  VALUE '01100100'.
              05 FILLER              PIC X(08)  VALUE '01100101'.
              05 FILLER              PIC X(08)  VALUE '01100110'.
              05 FILLER              PIC X(08)  VALUE '01100111'.
              05 FILLER              PIC X(08)  VALUE '01101000'.
              05 FILLER              PIC X(08)  VALUE '01101001'.
              05 FILLER              PIC X(08)  VALUE '01101010'.
              05 FILLER              PIC X(08)  VALUE '01101011'.
              05 FILLER              PIC X(08)  VALUE '01101100'.
              05 FILLER              PIC X(08)  VALUE '01101101'.
              05 FILLER              PIC X(08)  VALUE '01101110'.
              05 FILLER              PIC X(08)  VALUE '01101111'.
              05 FILLER              PIC X(08)  VALUE '01110000'.
              05 FILLER              PIC X(08)  VALUE '01110001'.
              05 FILLER              PIC X(08)  VALUE '01110010'.
              05 FILLER              PIC X(08)  VALUE '01110011'.
              05 FILLER              PIC X(08)  VALUE '01110100'.
              05 FILLER              PIC X(08)  VALUE '01110101'.
              05 FILLER              PIC X(08)  VALUE '01110110'.
              05 FILLER              PIC X(08)  VALUE '01110111'.
              05 FILLER              PIC X(08)  VALUE '01111000'.
              05 FILLER              PIC X(08)  VALUE '01111001'.
              05 FILLER              PIC X(08)  VALUE '01111010'.
              05 FILLER              PIC X(08)  VALUE '01111011'.
              05 FILLER              PIC X(08)  VALUE '01111100'.
              05 FILLER              PIC X(08)  VALUE '01111101'.
              05 FILLER              PIC X(08)  VALUE '01111110'.
              05 FILLER              PIC X(08)  VALUE '01111111'.
              05 FILLER              PIC X(08)  VALUE '10000000'.
              05 FILLER              PIC X(08)  VALUE '10000001'.
              05 FILLER              PIC X(08)  VALUE '10000010'.
              05 FILLER              PIC X(08)  VALUE '10000011'.
              05 FILLER              PIC X(08)  VALUE '10000100'.
              05 FILLER              PIC X(08)  VALUE '10000101'.
              05 FILLER              PIC X(08)  VALUE '10000110'.
              05 FILLER              PIC X(08)  VALUE '10000111'.
              05 FILLER              PIC X(08)  VALUE '10001000'.
              05 FILLER              PIC X(08)  VALUE '10001001'.
              05 FILLER              PIC X(08)  VALUE '10001010'.
              05 FILLER              PIC X(08)  VALUE '10001011'.
              05 FILLER              PIC X(08)  VALUE '10001100'.
              05 FILLER              PIC X(08)  VALUE '10001101'.
              05 FILLER              PIC X(08)  VALUE '10001110'.
              05 FILLER              PIC X(08)  VALUE '10001111'.
              05 FILLER              PIC X(08)  VALUE '10010000'.
              05 FILLER              PIC X(08)  VALUE '10010001'.
              05 FILLER              PIC X(08)  VALUE '10010010'.
              05 FILLER              PIC X(08)  VALUE '10010011'.
              05 FILLER              PIC X(08)  VALUE '10010100'.
              05 FILLER              PIC X(08)  VALUE '10010101'.
              05 FILLER              PIC X(08)  VALUE '10010110'.
              05 FILLER              PIC X(08)  VALUE '10010111'.
              05 FILLER              PIC X(08)  VALUE '10011000'.
              05 FILLER              PIC X(08)  VALUE '10011001'.
              05 FILLER              PIC X(08)  VALUE '10011010'.
              05 FILLER              PIC X(08)  VALUE '10011011'.
              05 FILLER              PIC X(08)  VALUE '10011100'.
              05 FILLER              PIC X(08)  VALUE '10011101'.
              05 FILLER              PIC X(08)  VALUE '10011110'.
              05 FILLER              PIC X(08)  VALUE '10011111'.
              05 FILLER              PIC X(08)  VALUE '10100000'.
              05 FILLER              PIC X(08)  VALUE '10100001'.
              05 FILLER              PIC X(08)  VALUE '10100010'.
              05 FILLER              PIC X(08)  VALUE '10100011'.
              05 FILLER              PIC X(08)  VALUE '10100100'.
              05 FILLER              PIC X(08)  VALUE '10100101'.
              05 FILLER              PIC X(08)  VALUE '10100110'.
              05 FILLER              PIC X(08)  VALUE '10100111'.
              05 FILLER              PIC X(08)  VALUE '10101000'.
              05 FILLER              PIC X(08)  VALUE '10101001'.
              05 FILLER              PIC X(08)  VALUE '10101010'.
              05 FILLER              PIC X(08)  VALUE '10101011'.
              05 FILLER              PIC X(08)  VALUE '10101100'.
              05 FILLER              PIC X(08)  VALUE '10101101'.
              05 FILLER              PIC X(08)  VALUE '10101110'.
              05 FILLER              PIC X(08)  VALUE '10101111'.
              05 FILLER              PIC X(08)  VALUE '10110000'.
              05 FILLER              PIC X(08)  VALUE '10110001'.
              05 FILLER              PIC X(08)  VALUE '10110010'.
              05 FILLER              PIC X(08)  VALUE '10110011'.
              05 FILLER              PIC X(08)  VALUE '10110100'.
              05 FILLER              PIC X(08)  VALUE '10110101'.
              05 FILLER              PIC X(08)  VALUE '10110110'.
              05 FILLER              PIC X(08)  VALUE '10110111'.
              05 FILLER              PIC X(08)  VALUE '10111000'.
              05 FILLER              PIC X(08)  VALUE '10111001'.
              05 FILLER              PIC X(08)  VALUE '10111010'.
              05 FILLER              PIC X(08)  VALUE '10111011'.
              05 FILLER              PIC X(08)  VALUE '10111100'.
              05 FILLER              PIC X(08)  VALUE '10111101'.
              05 FILLER              PIC X(08)  VALUE '10111110'.
              05 FILLER              PIC X(08)  VALUE '10111111'.
              05 FILLER              PIC X(08)  VALUE '11000000'.
              05 FILLER              PIC X(08)  VALUE '11000001'.
              05 FILLER              PIC X(08)  VALUE '11000010'.
              05 FILLER              PIC X(08)  VALUE '11000011'.
              05 FILLER              PIC X(08)  VALUE '11000100'.
              05 FILLER              PIC X(08)  VALUE '11000101'.
              05 FILLER              PIC X(08)  VALUE '11000110'.
              05 FILLER              PIC X(08)  VALUE '11000111'.
              05 FILLER              PIC X(08)  VALUE '11001000'.
              05 FILLER              PIC X(08)  VALUE '11001001'.
              05 FILLER              PIC X(08)  VALUE '11001010'.
              05 FILLER              PIC X(08)  VALUE '11001011'.
              05 FILLER              PIC X(08)  VALUE '11001100'.
              05 FILLER              PIC X(08)  VALUE '11001101'.
              05 FILLER              PIC X(08)  VALUE '11001110'.
              05 FILLER              PIC X(08)  VALUE '11001111'.
              05 FILLER              PIC X(08)  VALUE '11010000'.
              05 FILLER              PIC X(08)  VALUE '11010001'.
              05 FILLER              PIC X(08)  VALUE '11010010'.
              05 FILLER              PIC X(08)  VALUE '11010011'.
              05 FILLER              PIC X(08)  VALUE '11010100'.
              05 FILLER              PIC X(08)  VALUE '11010101'.
              05 FILLER              PIC X(08)  VALUE '11010110'.
              05 FILLER              PIC X(08)  VALUE '11010111'.
              05 FILLER              PIC X(08)  VALUE '11011000'.
              05 FILLER              PIC X(08)  VALUE '11011001'.
              05 FILLER              PIC X(08)  VALUE '11011010'.
              05 FILLER              PIC X(08)  VALUE '11011011'.
              05 FILLER              PIC X(08)  VALUE '11011100'.
              05 FILLER              PIC X(08)  VALUE '11011101'.
              05 FILLER              PIC X(08)  VALUE '11011110'.
              05 FILLER              PIC X(08)  VALUE '11011111'.
              05 FILLER              PIC X(08)  VALUE '11100000'.
              05 FILLER              PIC X(08)  VALUE '11100001'.
















              05 FILLER              PIC X(08)  VALUE '11100010'.
              05 FILLER              PIC X(08)  VALUE '11100011'.
              05 FILLER              PIC X(08)  VALUE '11100100'.
              05 FILLER              PIC X(08)  VALUE '11100101'.
              05 FILLER              PIC X(08)  VALUE '11100110'.
              05 FILLER              PIC X(08)  VALUE '11100111'.
              05 FILLER              PIC X(08)  VALUE '11101000'.
              05 FILLER              PIC X(08)  VALUE '11101001'.
              05 FILLER              PIC X(08)  VALUE '11101010'.
              05 FILLER              PIC X(08)  VALUE '11101011'.
              05 FILLER              PIC X(08)  VALUE '11101100'.
              05 FILLER              PIC X(08)  VALUE '11101101'.
              05 FILLER              PIC X(08)  VALUE '11101110'.
              05 FILLER              PIC X(08)  VALUE '11101111'.
              05 FILLER              PIC X(08)  VALUE '11110000'.
              05 FILLER              PIC X(08)  VALUE '11110001'.
              05 FILLER              PIC X(08)  VALUE '11110010'.
              05 FILLER              PIC X(08)  VALUE '11110011'.
              05 FILLER              PIC X(08)  VALUE '11110100'.
              05 FILLER              PIC X(08)  VALUE '11110101'.
              05 FILLER              PIC X(08)  VALUE '11110110'.
              05 FILLER              PIC X(08)  VALUE '11110111'.
              05 FILLER              PIC X(08)  VALUE '11111000'.
              05 FILLER              PIC X(08)  VALUE '11111001'.
              05 FILLER              PIC X(08)  VALUE '11111010'.
              05 FILLER              PIC X(08)  VALUE '11111011'.
              05 FILLER              PIC X(08)  VALUE '11111100'.
              05 FILLER              PIC X(08)  VALUE '11111101'.
              05 FILLER              PIC X(08)  VALUE '11111110'.
              05 FILLER              PIC X(08)  VALUE '11111111'.
           03 W-BITS                 REDEFINES  W-BIT-DEF
                                     PIC X(08)  OCCURS  256 TIMES.
      **--------------------------------------------------------------*
      **            Base64 - alphabet                                 *
      **--------------------------------------------------------------*
       01  W-BASE64-ALPHABET.
           03 W-BASE64-DEF.
              05 FILLER              PIC X(26)  VALUE
                                     'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
              05 FILLER              PIC X(26)  VALUE
                                     'abcdefghijklmnopqrstuvwxyz'.
              05 FILLER              PIC X(13)  VALUE
                                     '0123456789+/='.
           03 W-BASE64-TXT           REDEFINES  W-BASE64-DEF
                                     PIC X(01)  OCCURS 65 TIMES.
      **--------------------------------------------------------------*
      **            global copy-members                               *
      **--------------------------------------------------------------*
       COPY SOCKET.DEF.
      /
       COPY ACUCOBOL.DEF.
      /
      *{Bench}end
       LINKAGE SECTION.
       copy "link-winbmail.def".

       SCREEN SECTION.

       PROCEDURE DIVISION              USING WINBMAIL-BLOCK.
      *
       DECLARATIVES.
      *
       DATEI SECTION.
      *
           USE AFTER STANDARD ERROR PROCEDURE ON SMAILPR,
                                                 SMAILIN.
      *
        DS--010.
      *
           CALL     'C$RERR'           USING W-EXT-STATUS,
                                             W-TEXT-MESSAGE.
           CALL     'C$RERRNAME'       USING W-FILE-NAME.
           DISPLAY                   MESSAGE BOX
                    'Error occured during file operation on'
                    X'0A'
                    X'0A'
                    W-FILE-NAME
                    X'0A'
                    X'0A'
                    'Filestatus      : ' FILE-STATUS
                    X'0A'
                    'Extended Status : ' W-EXT-STATUS
                    X'0A'
                    'Message         : ' W-TEXT-MESSAGE
                                       TITLE
                    'Sending Emails with C$SOCKET'
                                        ICON MB-ERROR-ICON
                                        TYPE MB-CANCEL
           END-DISPLAY.
           MOVE     99                    TO WINBMAIL-RETURN.
                                       GO TO ABBRUCH.
      *
        DS--020.
      *
           EXIT.
      *
       END DECLARATIVES.
      /
        MAIN SECTION.
      *
        M--010.
      *
      **--------------------------------------------------------------*
      **            Program control                                   *
      **--------------------------------------------------------------*
           INITIALIZE                        ARBEITS-FELDER,
                                             MAIL-FELDER.
           MOVE     ALL '*'               TO W-STARS.
           STRING   X'0D'
                    X'0A'       DELIMITED BY SIZE
                                        INTO CRLF.
           MOVE     ZEROS                 TO W-PROTOKOLL.
           IF       (WINBMAIL-LOGFILE  EQUAL 'Y' OR 'J' OR 'j' OR 'y')
                    SET  ENVIRONMENT         'EXTEND_CREATES'
                                          TO '1'
                    SET  ENVIRONMENT         'STRIP_TRAILING_SPACES'
                                          TO '1'
                    OPEN EXTEND              SMAILPR
                    MOVE 1                TO W-PROTOKOLL
           END-IF.
      *
           IF       (WINBMAIL-LOGFILE  EQUAL 'O' OR 'o')
                    SET  ENVIRONMENT         'EXTEND_CREATES'
                                          TO '1'
                    SET  ENVIRONMENT         'STRIP_TRAILING_SPACES'
                                          TO '1'
                    OPEN OUTPUT              SMAILPR
                    MOVE 1                TO W-PROTOKOLL
           END-IF.
      **--------------------------------------------------------------*
      **            Display base progressbar                          *
      **--------------------------------------------------------------*
           INITIALIZE                        MAI-PROGRESS-HANDLE
                                             MAI-PROGRESS-FRAME.
           MOVE     ZEROS                 TO MAI-BAR-PROZENT.
           MOVE     MAI-BAR-PROZENT       TO STATUS-PROZENT.
           DISPLAY               INDEPENDENT WINDOW
                                       LINES 6,00
                                        SIZE 31,00
                                         COL 15,00
                                 CELL HEIGHT 10
                                  CELL WIDTH 10
                                       |BOXED
                                       ERASE
                                   USER-GRAY
                                  USER-WHITE
                                       TITLE 'Sending Mail active'
                                      HANDLE MAI-PROGRESS-HANDLE
      *
           DISPLAY                     LABEL
                                             "Invio mail..."
                                        LINE 2,00
                                         COL 5,00
                                      CENTER.
           MOVE     BLUE                  TO COLOR-1.
           MOVE     WHITE                 TO COLOR-2.
           MOVE     ZEROS                 TO STATUS-PROZENT.
           DISPLAY                     FRAME
                                        LINE 4,00
                                         COL 5,00
                                        CCOL 8
                                       LINES 1,50
                                      CLINES 3
                                        SIZE 24,00 CELLS
                                FILL-COLOR = COLOR-1
                               FILL-COLOR2 = COLOR-2
                              FILL-PERCENT = 0
                                       TITLE FRAME-TITLE
                            TITLE-POSITION = 7
                             BACKGROUND-HIGH
                                      HANDLE MAI-PROGRESS-FRAME
                                     LOWERED.
      **--------------------------------------------------------------*
      **            Check for SMTP after POP                          *
      **--------------------------------------------------------------*
           IF       WINBMAIL-USER  NOT EQUAL SPACES
                    IF NOT (WINBMAIL-SMTP-AUTH
                                       EQUAL 'J' OR 'Y' OR 'y' or 'j')
      *****                     PERFORM           POP3-CONNECT
                           MOVE 5         TO MAI-BAR-PROZENT
                           MOVE MAI-BAR-PROZENT
                                          TO STATUS-PROZENT
                           MODIFY MAI-PROGRESS-FRAME
                              FILL-PERCENT = MAI-BAR-PROZENT
                                     TITLE = FRAME-TITLE
                    END-IF
           ELSE
                    MOVE 1                TO MAI-BAR-PROZENT
                    MOVE MAI-BAR-PROZENT  TO STATUS-PROZENT
                    MODIFY MAI-PROGRESS-FRAME
                              FILL-PERCENT = MAI-BAR-PROZENT
                                     TITLE = FRAME-TITLE
           END-IF.
      *
           IF       WINBMAIL-RETURN  GREATER ZEROS
                                       GO TO ABBRUCH
           END-IF.
      **--------------------------------------------------------------*
      **            set SMTP-Servername for Connect                   *
      **--------------------------------------------------------------*
           IF       WINBMAIL-SERVER    EQUAL SPACES
                    MOVE 49               TO WINBMAIL-RETURN
                    MOVE '(int) SMTP-Server not defined'
                                          TO W-PROT-LINE
                    PERFORM                  WRITE-PROTOKOLL
                                       GO TO ABBRUCH
           END-IF.
      *
           MOVE     SPACES                TO WRK-DATA-RECORD.
           STRING   WINBMAIL-SERVER
                                DELIMITED BY SPACE
                    X'00'       DELIMITED BY SIZE
                                        INTO WRK-DATA-RECORD.
      **--------------------------------------------------------------*
      **            Get SMTP-Server-Port (default = 25)               *
      **--------------------------------------------------------------*
           MOVE     WINBMAIL-SMTP-PORT    TO SOCKET-NUMBER.
           IF       WINBMAIL-SMTP-PORT EQUAL ZEROS
                    MOVE 25               TO SOCKET-NUMBER
           END-IF.
      **--------------------------------------------------------------*
      **            connect to SMTP-server                            *
      **--------------------------------------------------------------*
           CALL     'C$SOCKET'         USING AGS-CREATE-CLIENT
                                             SOCKET-NUMBER
                                             WRK-DATA-RECORD
                                      GIVING SOCKET-HANDLE
           END-CALL.
      *
           IF       SOCKET-HANDLE          = NULL
                    MOVE 50               TO WINBMAIL-RETURN
                    MOVE SPACES           TO W-PROT-LINE
                    STRING '(int) No Connect to : '
                                DELIMITED BY SIZE
                           WINBMAIL-SERVER
                                DELIMITED BY SPACE
                                        INTO W-PROT-LINE
                    PERFORM                  WRITE-PROTOKOLL
                    MOVE   '(int) programm aborted'
                                          TO W-PROT-LINE
                    PERFORM                  WRITE-PROTOKOLL
                                       GO TO ABBRUCH
           END-IF.
      *
           MOVE     SPACES                TO W-PROT-LINE.
           MOVE     SOCKET-NUMBER         TO W-PRT-NR.
           STRING   '(int) connected to : '
                                DELIMITED BY SIZE
                           WINBMAIL-SERVER
                                DELIMITED BY SPACE
                           ' with port : '
                           W-PRT-NR
                                DELIMITED BY SIZE
                                        INTO W-PROT-LINE.
           PERFORM                           WRITE-PROTOKOLL.
      **--------------------------------------------------------------*
      **            read first server message                         *
      **--------------------------------------------------------------*
           PERFORM                           RECEIVE-DATA.
           MOVE     WRK-DATA-RECORD       TO W-REPLY.
           IF       W-ERROR-CONNECT
                    MOVE 51               TO WINBMAIL-RETURN
                    MOVE SPACES           TO W-PROT-LINE
                    STRING '(int) No connect to : '
                                DELIMITED BY SIZE
                           WINBMAIL-SERVER
                                DELIMITED BY SPACE
                           ' possible!'
                                DELIMITED BY SIZE
                                        INTO W-PROT-LINE
                    PERFORM                  WRITE-PROTOKOLL
                                       GO TO ABBRUCH
           END-IF.
      *
           ADD      2                     TO MAI-BAR-PROZENT.
           MOVE     MAI-BAR-PROZENT       TO STATUS-PROZENT.
           MODIFY   MAI-PROGRESS-FRAME
                              FILL-PERCENT = MAI-BAR-PROZENT
                                     TITLE = FRAME-TITLE.
      **--------------------------------------------------------------*
      **            get own host-name                                 *
      **--------------------------------------------------------------*
           INITIALIZE                        WRK-HOST-NAME.
           CALL     'C$SOCKET'         USING AGS-GETHOSTNAME
                                             WRK-HOST-NAME.
           IF   NOT RETURN-CODE        EQUAL ZEROS
                    MOVE 52               TO WINBMAIL-RETURN
                    MOVE '(int) Error during get own hostname'
                                          TO W-PROT-LINE
                    PERFORM                  WRITE-PROTOKOLL
                                       GO TO ABBRUCH
           END-IF.
      **--------------------------------------------------------------*
      **            send EHLO to verify the connect                   *
      **--------------------------------------------------------------*
           INITIALIZE                        WRK-DATA-RECORD.
           MOVE     1                     TO BYTES-TO-SEND.
           STRING   'EHLO '     DELIMITED BY SIZE
                    WRK-HOST-NAME
                                DELIMITED BY LOW-VALUE
                    CRLF        DELIMITED BY SIZE
                                        INTO WRK-DATA-RECORD
                                WITH POINTER BYTES-TO-SEND.
           PERFORM                           SEND-DATA.
           MOVE     WRK-DATA-RECORD(1:3)  TO W-REPLY.
           IF       W-ERROR-HELO
                    MOVE 53               TO WINBMAIL-RETURN
                    MOVE '(int) Error during SMTP-Server logon'
                                          TO W-PROT-LINE
                    PERFORM                  WRITE-PROTOKOLL
                                       GO TO ABBRUCH
           END-IF.
      *
           MOVE     ZEROS                 TO W-IND.
      *
        M--020.
      *
      **--------------------------------------------------------------*
      **            read next answer till message is '250 xxxx'       *
      **--------------------------------------------------------------*
           IF       WRK-DATA-RECORD(1:4)
                                       EQUAL '250-'
                    ADD  1                TO W-IND
                    MOVE FUNCTION UPPER-CASE(WRK-DATA-RECORD)
                                          TO W-AUTH-ZEILE(W-IND)
                    PERFORM                  RECEIVE-DATA
                                       GO TO M--020
           ELSE
                    ADD  1                TO W-IND
                    MOVE FUNCTION UPPER-CASE(WRK-DATA-RECORD)
                                          TO W-AUTH-ZEILE(W-IND)
           END-IF.
      *
           MOVE     ZEROS                 TO W-IND.
           INSPECT  W-AUTH-TABELLE  TALLYING W-IND
                                     FOR ALL 'AUTH PLAIN'.
           INSPECT  W-AUTH-TABELLE  TALLYING W-IND
                                     FOR ALL 'AUTH=PLAIN'.
           IF       (WINBMAIL-SMTP-AUTH
                                       EQUAL 'J' OR 'Y' OR 'y' or 'j')
                    IF W-IND           EQUAL ZEROS
                       MOVE 69            TO WINBMAIL-RETURN
                       MOVE '(int) SMTP-Authentification not supported'
                                          TO W-PROT-LINE
                       PERFORM               WRITE-PROTOKOLL
                                       GO TO ABBRUCH
                    END-IF
           END-IF.
      *
           ADD      1                     TO MAI-BAR-PROZENT.
           MOVE     MAI-BAR-PROZENT       TO STATUS-PROZENT.
           MODIFY   MAI-PROGRESS-FRAME
                              FILL-PERCENT = MAI-BAR-PROZENT
                                     TITLE = FRAME-TITLE.
      **--------------------------------------------------------------*
      **            send SMTP - AUTH is desired                       *
      **--------------------------------------------------------------*
           IF       (WINBMAIL-SMTP-AUTH
                                       EQUAL 'J' OR 'Y' OR 'y' or 'j')
                    PERFORM                  SEND-AUTH-COMMAND
           END-IF.
      *
           ADD      3                     TO MAI-BAR-PROZENT.
           MOVE     MAI-BAR-PROZENT       TO STATUS-PROZENT.
           MODIFY   MAI-PROGRESS-FRAME
                              FILL-PERCENT = MAI-BAR-PROZENT
                                     TITLE = FRAME-TITLE.
      **--------------------------------------------------------------*
      **            Create sender                                     *
      **--------------------------------------------------------------*
           MOVE     WINBMAIL-FROM         TO WINLNG-TEXT.
           PERFORM                           INTERNE-LAENGE.
           INITIALIZE                        WRK-DATA-RECORD.
           MOVE     1                     TO BYTES-TO-SEND.
           STRING   'MAIL FROM: <'
                                DELIMITED BY SIZE
                    WINBMAIL-FROM(1:WINLNG-SIZE)
                    '>'
                    CRLF        DELIMITED BY SIZE
                                        INTO WRK-DATA-RECORD
                                WITH POINTER BYTES-TO-SEND.
           PERFORM                           SEND-DATA.
           MOVE     WRK-DATA-RECORD(1:3)  TO W-REPLY.
           IF       W-ERROR-MAIL
                    MOVE 54               TO WINBMAIL-RETURN
                    MOVE '(int) Error in address of sender'
                                          TO W-PROT-LINE
                    PERFORM                  WRITE-PROTOKOLL
                                       GO TO ABBRUCH
           END-IF.
      **--------------------------------------------------------------*
      **            Set address of receiver                           *
      **--------------------------------------------------------------*
           MOVE     WINBMAIL-TO           TO WINLNG-TEXT.
           PERFORM                           INTERNE-LAENGE.
           INITIALIZE                        WRK-DATA-RECORD.
           MOVE     1                     TO BYTES-TO-SEND.
           STRING   'RCPT TO: <'
                    WINBMAIL-TO(1:WINLNG-SIZE)
                    '>'
                    CRLF        DELIMITED BY SIZE
                                        INTO WRK-DATA-RECORD
                                WITH POINTER BYTES-TO-SEND.
           PERFORM                           SEND-DATA.
           MOVE     WRK-DATA-RECORD(1:3)  TO W-REPLY.
           IF       W-ERROR-RCPT
                    MOVE 55               TO WINBMAIL-RETURN
                    MOVE '(int) Error in address of receipient'
                                          TO W-PROT-LINE
                    PERFORM                  WRITE-PROTOKOLL
                                       GO TO ABBRUCH
           END-IF.
      *
           ADD      1                     TO MAI-BAR-PROZENT.
           MOVE     MAI-BAR-PROZENT       TO STATUS-PROZENT.
           MODIFY   MAI-PROGRESS-FRAME
                              FILL-PERCENT = MAI-BAR-PROZENT
                                     TITLE = FRAME-TITLE.
      **--------------------------------------------------------------* ####
      **            special CRLF for MS-Exchange                      * ####
      **--------------------------------------------------------------* ####
           INITIALIZE                        WRK-DATA-RECORD.           ####
           MOVE     1                     TO BYTES-TO-SEND.             ####
           STRING   CRLF        DELIMITED BY SIZE                       ####
                                        INTO WRK-DATA-RECORD            ####
                                WITH POINTER BYTES-TO-SEND.             ####
           PERFORM                           SEND-DATA-MAIL.            ####
      **--------------------------------------------------------------*
      **            start transfer data                               *
      **--------------------------------------------------------------*
           INITIALIZE                        WRK-DATA-RECORD.
           MOVE     1                     TO BYTES-TO-SEND.
           STRING   'DATA'
                    CRLF        DELIMITED BY SIZE
                                        INTO WRK-DATA-RECORD
                                WITH POINTER BYTES-TO-SEND.
           PERFORM                           SEND-DATA.
           MOVE     WRK-DATA-RECORD(1:3)  TO W-REPLY.
           IF       W-ERROR-DATA-2
                    MOVE   56             TO WINBMAIL-RETURN
                    MOVE   SPACES         TO W-PROT-LINE
                    STRING '(int) sending mail aborted; Code : '
                           W-REPLY
                                DELIMITED BY SIZE
                                        INTO W-PROT-LINE
                    PERFORM                  WRITE-PROTOKOLL
                                       GO TO ABBRUCH
           END-IF.
      **--------------------------------------------------------------*
      **            duplicate address for display                     *
      **--------------------------------------------------------------*
           MOVE     WINBMAIL-FROM         TO WINLNG-TEXT.
           PERFORM                           INTERNE-LAENGE.
           INITIALIZE                        WRK-DATA-RECORD.
           MOVE     1                     TO BYTES-TO-SEND.
           STRING   'From: '
                    WINBMAIL-FROM(1:WINLNG-SIZE)
                    CRLF        DELIMITED BY SIZE
                                        INTO WRK-DATA-RECORD
                                WITH POINTER BYTES-TO-SEND.
           PERFORM                           SEND-DATA-MAIL.
      *
           MOVE     WINBMAIL-TO           TO WINLNG-TEXT.
           PERFORM                           INTERNE-LAENGE.
           INITIALIZE                        WRK-DATA-RECORD.
           MOVE     1                     TO BYTES-TO-SEND.
           STRING   'To: '
                    WINBMAIL-TO(1:WINLNG-SIZE)
                    CRLF        DELIMITED BY SIZE
                                        INTO WRK-DATA-RECORD
                                WITH POINTER BYTES-TO-SEND.
           PERFORM                           SEND-DATA-MAIL.

      **--------------------------------------------------------------*
      **            send subject of mail                              *
      **--------------------------------------------------------------*
           INITIALIZE                        WRK-DATA-RECORD.
           PERFORM                           CONVERT-TITEL.
      **--------------------------------------------------------------*
      **            create the body and send all data                 *
      **            (CRLF <.> CRLF terminates mail (with<.> = Point)) *
      **--------------------------------------------------------------*
      **            send basedate of mail                             *
      **--------------------------------------------------------------*
           INITIALIZE                        WRK-DATA-RECORD.
           MOVE     1                     TO BYTES-TO-SEND.
           STRING   'X-Mailer: WINBMAIL V1.000'| (KISS GmbH/2004-03-19)'
                    CRLF
                    'Mime-Version: 1.0'
                    CRLF        DELIMITED BY SIZE
                                        INTO WRK-DATA-RECORD
                                WITH POINTER BYTES-TO-SEND.
           PERFORM                           SEND-DATA-MAIL.
      **--------------------------------------------------------------*
      **            test for attachments                              *
      **--------------------------------------------------------------*
           MOVE     SPACES                TO WRK-MULTIPART.
           IF       WINBMAIL-DATEI-1   EQUAL SPACES
                AND WINBMAIL-DATEI-2   EQUAL SPACES
                AND WINBMAIL-DATEI-3   EQUAL SPACES
                                       GO TO M--030
           END-IF.
      **--------------------------------------------------------------*
      **            Send mail with attachments                        *
      **--------------------------------------------------------------*
      **            create Boundary for Multipart-Mails               *
      **--------------------------------------------------------------*
           ACCEPT   W-DATUM             FROM CENTURY-DATE.
           ACCEPT   W-TIME              FROM TIME.
           STRING   '_WINBMAIL_D_'
                    W-DATUM
                    '_T_'
                    W-TIME      DELIMITED BY SIZE
                                        INTO WRK-MULTIPART.
      **--------------------------------------------------------------*
      **            send base of Multipart-Mail                       *
      **--------------------------------------------------------------*
           MOVE     SPACES                TO WRK-DATA-RECORD.
           MOVE     1                     TO BYTES-TO-SEND.
           STRING   'Content-Type: multipart/mixed;'
                    'boundary="'
                                DELIMITED BY SIZE
                    WRK-MULTIPART
                                DELIMITED BY SPACE
                    '"'
                    CRLF        DELIMITED BY SIZE
                    CRLF        DELIMITED BY SIZE
                                        INTO WRK-DATA-RECORD
                                WITH POINTER BYTES-TO-SEND.
           PERFORM                           SEND-DATA-MAIL.
      **--------------------------------------------------------------*
      **            Send first Multipart-delimiter                    *
      **--------------------------------------------------------------*
           MOVE     SPACES                TO WRK-DATA-RECORD.
           MOVE     1                     TO BYTES-TO-SEND.
           STRING   '--'        DELIMITED BY SIZE
                    WRK-MULTIPART
                                DELIMITED BY SPACE
                    CRLF        DELIMITED BY SIZE
                                        INTO WRK-DATA-RECORD
                                WITH POINTER BYTES-TO-SEND.
           PERFORM                           SEND-DATA-MAIL.
      *
        M--030.
      *
      **--------------------------------------------------------------*
      **            Send base of mailbody                             *
      **--------------------------------------------------------------*
           MOVE     SPACES                TO WRK-DATA-RECORD.
           MOVE     1                     TO BYTES-TO-SEND.
           STRING   'Content-Type: text/plain; charset="iso-8859-1"'
                    CRLF
                    'Content-Transfer-Encoding: base64'
                    CRLF
                    CRLF        DELIMITED BY SIZE
                                        INTO WRK-DATA-RECORD
                                WITH POINTER BYTES-TO-SEND.
           PERFORM                           SEND-DATA-MAIL.
      *
           ADD      4                     TO MAI-BAR-PROZENT.
           MOVE     MAI-BAR-PROZENT       TO STATUS-PROZENT.
           MODIFY   MAI-PROGRESS-FRAME
                              FILL-PERCENT = MAI-BAR-PROZENT
                                     TITLE = FRAME-TITLE.
           MOVE     1                     TO W-FAKTOR.
           IF   NOT WINBMAIL-DATEI-1   EQUAL SPACES
                    ADD 1                 TO W-FAKTOR
           END-IF.
      *
           IF   NOT WINBMAIL-DATEI-2   EQUAL SPACES
                    ADD 1                 TO W-FAKTOR
           END-IF.
      *
           IF   NOT WINBMAIL-DATEI-3   EQUAL SPACES
                    ADD 1                 TO W-FAKTOR
           END-IF.
      *
           COMPUTE  W-ADDITION             = (90 - MAI-BAR-PROZENT)
                                           / W-FAKTOR.
      **--------------------------------------------------------------*
      **            create length of body and send it                 *
      **--------------------------------------------------------------*
           PERFORM                           BODY-BASE64-CODING.
      **--------------------------------------------------------------*
      **            check for attachments                             *
      **--------------------------------------------------------------*
           IF       WINBMAIL-DATEI-1   EQUAL SPACES
                AND WINBMAIL-DATEI-2   EQUAL SPACES
                AND WINBMAIL-DATEI-3   EQUAL SPACES
                                        NEXT SENTENCE
           ELSE
                    MOVE WINBMAIL-DATEI-1 TO W-ANHANG(1)
                    MOVE WINBMAIL-DATEI-2 TO W-ANHANG(2)
                    MOVE WINBMAIL-DATEI-3 TO W-ANHANG(3)
                    MOVE    ZEROS         TO W-ANHANG-NR
                    PERFORM                  SENDEN-ANHANG 3 TIMES
           END-IF.
      *
           IF       WINBMAIL-RETURN  GREATER ZEROS
                                       GO TO ABBRUCH
           END-IF.
      **--------------------------------------------------------------*
      **            send multipart delimiter                          *
      **--------------------------------------------------------------*
           MOVE     SPACES                TO WRK-DATA-RECORD.
           MOVE     1                     TO BYTES-TO-SEND.
           STRING   '--'        DELIMITED BY SIZE
                    WRK-MULTIPART
                                DELIMITED BY SPACE
                    '--'
                    CRLF        DELIMITED BY SIZE
                                        INTO WRK-DATA-RECORD
                                WITH POINTER BYTES-TO-SEND.
           IF   NOT WRK-MULTIPART      EQUAL SPACES
                    PERFORM                  SEND-DATA-MAIL
           END-IF.
      *
           MOVE     95                    TO MAI-BAR-PROZENT.
           MOVE     MAI-BAR-PROZENT       TO STATUS-PROZENT.
           MODIFY   MAI-PROGRESS-FRAME
                              FILL-PERCENT = MAI-BAR-PROZENT
                                     TITLE = FRAME-TITLE.
      **--------------------------------------------------------------*
      **            End of mail send CRLF . CRLF                      *
      **--------------------------------------------------------------*
           MOVE     SPACES                TO WRK-DATA-RECORD.
           MOVE     1                     TO BYTES-TO-SEND.
           STRING   CRLF
                    CRLF
                    '.'
                    CRLF
                    CRLF        DELIMITED BY SIZE
                                        INTO WRK-DATA-RECORD
                                WITH POINTER BYTES-TO-SEND.
           PERFORM                           SEND-DATA.
      *
           MOVE     WRK-DATA-RECORD(1:3)  TO W-REPLY.
           IF       W-ERROR-DATA-2
                    MOVE   SPACES         TO W-PROT-LINE
                    STRING '(int)  Error during sending mail;'
                           ' Code : '
                           W-REPLY
                                DELIMITED BY SIZE
                                        INTO W-PROT-LINE
                    PERFORM                  WRITE-PROTOKOLL
                                       GO TO ABBRUCH
           END-IF.
      **--------------------------------------------------------------*
      **            close connection to smtpserver                    *
      **--------------------------------------------------------------*
           MOVE     SPACES                TO WRK-DATA-RECORD.
           MOVE     1                     TO BYTES-TO-SEND.
           STRING   'QUIT'
                    CRLF        DELIMITED BY SIZE
                                        INTO WRK-DATA-RECORD
                                WITH POINTER BYTES-TO-SEND.
           PERFORM                           SEND-DATA-MAIL.
      *
           MOVE     100                   TO MAI-BAR-PROZENT.
           MOVE     MAI-BAR-PROZENT       TO STATUS-PROZENT.
           MODIFY   MAI-PROGRESS-FRAME
                              FILL-PERCENT = MAI-BAR-PROZENT
                                     TITLE = FRAME-TITLE.
           MOVE     '(int) EMail successfully sent'
                                          TO W-PROT-LINE.
           PERFORM                           WRITE-PROTOKOLL.
           MOVE     SPACES                TO W-PROT-LINE.
           STRING   '=================================================='
                    '=================================================='
                    '=============================='
                                DELIMITED BY SIZE
                                        INTO W-PROT-LINE.
           PERFORM                           WRITE-PROTOKOLL.
      **--------------------------------------------------------------*
      **            close socket connection                           *
      **--------------------------------------------------------------*
           CALL     'C$SOCKET'         USING AGS-CLOSE
                                             SOCKET-HANDLE.
      **--------------------------------------------------------------*
      **            close logfile                                     *
      **--------------------------------------------------------------*
           IF       W-PROT-WRITE
                    CLOSE                    SMAILPR
           END-IF.
                                       GO TO HALT.
      *
        M--040.
      *
           EXIT.
      /
       SEND-DATA SECTION.
      *
        SDA--010.
      *
      **--------------------------------------------------------------*
      **            Send given datablock with receive                 *
      **--------------------------------------------------------------*
      **            store data in socketbuffer                        *
      **--------------------------------------------------------------*
           SUBTRACT 1                   FROM BYTES-TO-SEND.
           CALL     'C$SOCKET'         USING AGS-WRITE
                                             SOCKET-HANDLE
                                             WRK-DATA-RECORD
                                             BYTES-TO-SEND.
      **--------------------------------------------------------------*
      **            flush buffer                                      *
      **--------------------------------------------------------------*
           CALL     'C$SOCKET'         USING AGS-FLUSH
                                             SOCKET-HANDLE.
      **--------------------------------------------------------------*
      **            write data to logfile                             *
      **--------------------------------------------------------------*
           MOVE     SPACES                TO W-PROT-LINE.
           MOVE     1                     TO W-PROT-FUNKTION.
           MOVE     WRK-DATA-RECORD(1:256)
                                          TO W-PROT-LINE.
           PERFORM                           WRITE-PROTOKOLL.
           MOVE     WRK-DATA-RECORD(257:256)
                                          TO W-PROT-LINE.
           PERFORM                           WRITE-PROTOKOLL.
           MOVE     WRK-DATA-RECORD(513:256)
                                          TO W-PROT-LINE.
           PERFORM                           WRITE-PROTOKOLL.
           MOVE     WRK-DATA-RECORD(768:)
                                          TO W-PROT-LINE.
           PERFORM                           WRITE-PROTOKOLL.
      **--------------------------------------------------------------*
      **            read answer of smtp-server                        *
      **--------------------------------------------------------------*
           PERFORM                           RECEIVE-DATA.
                                       GO TO SDA--020.
      *
        SDA--020.
      *
           EXIT.
      /
       SEND-DATA-MAIL SECTION.
      *
        SDM--010.
      *
      **--------------------------------------------------------------*
      **            Send data without receive                         *
      **--------------------------------------------------------------*
      **            store data in socketbuffer                        *
      **--------------------------------------------------------------*
           SUBTRACT 1                   FROM BYTES-TO-SEND.
           CALL     'C$SOCKET'         USING AGS-WRITE
                                             SOCKET-HANDLE
                                             WRK-DATA-RECORD
                                             BYTES-TO-SEND.
      **--------------------------------------------------------------*
      **            flush buffer                                      *
      **--------------------------------------------------------------*
           CALL     'C$SOCKET'         USING AGS-FLUSH
                                             SOCKET-HANDLE.
      **--------------------------------------------------------------*
      **            write data to logfile                             *
      **--------------------------------------------------------------*
           MOVE     SPACES                TO W-PROT-LINE.
           MOVE     1                     TO W-PROT-FUNKTION.
           MOVE     WRK-DATA-RECORD(1:256)
                                          TO W-PROT-LINE.
           PERFORM                           WRITE-PROTOKOLL.
           MOVE     WRK-DATA-RECORD(257:256)
                                          TO W-PROT-LINE.
           PERFORM                           WRITE-PROTOKOLL.
           MOVE     WRK-DATA-RECORD(513:256)
                                          TO W-PROT-LINE.
           PERFORM                           WRITE-PROTOKOLL.
           MOVE     WRK-DATA-RECORD(768:)
                                          TO W-PROT-LINE.
           PERFORM                           WRITE-PROTOKOLL.
           MOVE     1                     TO BYTES-TO-SEND.
                                       GO TO SDM--030.
      *
        SDM--030.
      *
           EXIT.
      /
       RECEIVE-DATA SECTION.
      *
        RDA--010.
      *
      **--------------------------------------------------------------*
      **            get answer from SMTP-Server                       *
      **--------------------------------------------------------------*
           MOVE     1                     TO BYTES-READ.
           INITIALIZE                        WRK-DATA-RECORD
                                             FINISHED.
      **--------------------------------------------------------------*
      **            read buffer until CR+LF found                     *
      **--------------------------------------------------------------*
           PERFORM                     UNTIL FINISHED = 1
                    CALL 'C$SOCKET'    USING AGS-READ
                                             SOCKET-HANDLE
                                             INPUT-CHAR
                                             1
                    END-CALL
                    SUBTRACT 1          FROM BYTES-READ
                                      GIVING X
                    IF    INPUT-CHAR   EQUAL X'0A'
                      AND WRK-DATA-RECORD(X:1)
                                       EQUAL X'0D'
                          MOVE 1          TO FINISHED
                          SUBTRACT 1    FROM BYTES-READ
                          MOVE SPACE      TO
                                          WRK-DATA-RECORD(BYTES-READ:1)
                    ELSE
                          STRING INPUT-CHAR
                                DELIMITED BY SIZE
                                        INTO WRK-DATA-RECORD
                                WITH POINTER BYTES-READ
                    END-IF
           END-PERFORM.
      **--------------------------------------------------------------*
      **            write data to logfile                             *
      **--------------------------------------------------------------*
           IF       BYTES-READ       GREATER ZERO
                    MOVE   SPACES         TO W-PROT-LINE
                    MOVE   2              TO W-PROT-FUNKTION
                    MOVE   WRK-DATA-RECORD
                                          TO W-PROT-LINE
                    PERFORM                  WRITE-PROTOKOLL
           END-IF.
                                       GO TO RDA--020.
      *
        RDA--020.
      *
           EXIT.
      /
       SENDEN-ANHANG SECTION.
      *
        SAN--010.
      *
      **--------------------------------------------------------------*
      **            sending attachments                               *
      **--------------------------------------------------------------*
           ADD      1                     TO W-ANHANG-NR.
      **--------------------------------------------------------------*
      **            only if attachment is given                       *
      **--------------------------------------------------------------*
           IF       W-ANHANG(W-ANHANG-NR)
                                       EQUAL SPACES
                                       GO TO SAN--020
           END-IF.
      **--------------------------------------------------------------*
      **            only if there was no error                        *
      **--------------------------------------------------------------*
           IF       WINBMAIL-RETURN  GREATER ZEROS
                                       GO TO SAN--020
           END-IF.
      **--------------------------------------------------------------*
      **            split filename                                    *
      **--------------------------------------------------------------*
           MOVE     W-ANHANG(W-ANHANG-NR) TO WINSPLIT-DATEI.
           PERFORM                           SPLIT-FILE.
      **--------------------------------------------------------------*
      **            get size of file                                  *
      **--------------------------------------------------------------*
           MOVE     W-ANHANG(W-ANHANG-NR) TO FILE-INFO-NAME,
           PERFORM                           GET-FILEINFO.
           IF       FILE-INFO-STATUS GREATER ZEROS
                    MOVE   SPACES         TO W-PROT-LINE
                    STRING '(int) The file : '
                           WINSPLIT-FILE(1:WINSPLIT-FILE-LEN)
                           ' doesn''t exist'
                                DELIMITED BY SIZE
                                        INTO W-PROT-LINE
                    PERFORM                  WRITE-PROTOKOLL
                    MOVE   59             TO WINBMAIL-RETURN
                                       GO TO SAN--020
           END-IF.
      *
           MOVE     FILE-INFO-SIZE        TO W-FILE-SIZE.
           COMPUTE  W-FILE-COMP            = W-FILE-SIZE / W-ADDITION.
      **--------------------------------------------------------------*
      **            send Multipart delimiter                          *
      **--------------------------------------------------------------*
           MOVE     SPACES                TO WRK-DATA-RECORD.
           MOVE     1                     TO BYTES-TO-SEND.
           STRING   CRLF        DELIMITED BY SIZE
                                        INTO WRK-DATA-RECORD
                                WITH POINTER BYTES-TO-SEND.
           PERFORM                           SEND-DATA-MAIL.
      *
           MOVE     SPACES                TO WRK-DATA-RECORD.
           MOVE     1                     TO BYTES-TO-SEND.
           STRING   CRLF        DELIMITED BY SIZE
                    '--'        DELIMITED BY SIZE
                    WRK-MULTIPART
                                DELIMITED BY SPACE
                    CRLF        DELIMITED BY SIZE
                                        INTO WRK-DATA-RECORD
                                WITH POINTER BYTES-TO-SEND.
           PERFORM                           SEND-DATA-MAIL.
      **--------------------------------------------------------------*
      **            set content-type                                  *
      **--------------------------------------------------------------*
           MOVE     SPACES                TO WRK-DATA-RECORD.
           MOVE     1                     TO BYTES-TO-SEND.
           STRING   'Content-Type: application/octet-stream; '
                    'name="'
                    WINSPLIT-FILE(1:WINSPLIT-FILE-LEN)
                    '"'
                    CRLF        DELIMITED BY SIZE
                                        INTO WRK-DATA-RECORD
                                WITH POINTER BYTES-TO-SEND.
           PERFORM                           SEND-DATA-MAIL.
      **--------------------------------------------------------------*
      **            define Content-Disposition for attachement        *
      **--------------------------------------------------------------*
           MOVE     SPACES                TO WRK-DATA-RECORD.
           MOVE     1                     TO BYTES-TO-SEND.
           STRING   'Content-Disposition: attachment; filename="'
                    WINSPLIT-FILE(1:WINSPLIT-FILE-LEN)
                    '"'
                    CRLF        DELIMITED BY SIZE
                                        INTO WRK-DATA-RECORD
                                WITH POINTER BYTES-TO-SEND.
           PERFORM                           SEND-DATA-MAIL.
      **--------------------------------------------------------------*
      **            defines encoding-mechanism                        *
      **--------------------------------------------------------------*
           MOVE     SPACES                TO WRK-DATA-RECORD.
           MOVE     1                     TO BYTES-TO-SEND.
           STRING   'Content-Transfer-Encoding: base64'
                    CRLF
                    CRLF        DELIMITED BY SIZE
                                        INTO WRK-DATA-RECORD
                                WITH POINTER BYTES-TO-SEND.
           PERFORM                           SEND-DATA-MAIL.
      **--------------------------------------------------------------*
      **            encode file                                       *
      **--------------------------------------------------------------*
           PERFORM                           BASE64-CODING.
           IF       WINBMAIL-RETURN  GREATER ZEROS
                                       GO TO ABBRUCH
           END-IF.
                                       GO TO SAN--020.
      *
        SAN--020.
      *
      **--------------------------------------------------------------*
      **            Ende send file                                    *
      **--------------------------------------------------------------*
           EXIT.
      /
      ***** POP3-CONNECT SECTION.
      ******
      *****  POP--010.
      ******
      *******--------------------------------------------------------------*
      *******            get port-number of pop3-server (default=110)      *
      *******--------------------------------------------------------------*
      *****     MOVE     WINBMAIL-POP3-PORT    TO SOCKET-NUMBER.
      *****     IF       WINBMAIL-POP3-PORT EQUAL ZEROS
      *****              MOVE 110              TO SOCKET-NUMBER
      *****     END-IF.
      *******--------------------------------------------------------------*
      *******            Server setzen fuer Verbindung                     *
      *******--------------------------------------------------------------*
      *****     IF       WINBMAIL-POP3-SERVER
      *****                                 EQUAL SPACES
      *****              MOVE 48               TO WINBMAIL-RETURN
      *****              MOVE '(int) POP3-Server not defined'
      *****                                    TO W-PROT-LINE
      *****              PERFORM                  WRITE-PROTOKOLL
      *****                                 GO TO POP--020
      *****     END-IF.
      ******
      *****     MOVE     SPACES                TO WRK-DATA-RECORD.
      *****     STRING   WINBMAIL-POP3-SERVER
      *****                          DELIMITED BY SPACE
      *****              X'00'       DELIMITED BY SIZE
      *****                                  INTO WRK-DATA-RECORD.
      *******--------------------------------------------------------------*
      *******            connect to pop3 server                            *
      *******--------------------------------------------------------------*
      *****     CALL     'C$SOCKET'         USING AGS-CREATE-CLIENT
      *****                                       SOCKET-NUMBER
      *****                                       WRK-DATA-RECORD
      *****                                GIVING SOCKET-HANDLE.
      ******
      *****     IF       SOCKET-HANDLE          = NULL
      *****              MOVE 50               TO WINBMAIL-RETURN
      *****              MOVE SPACES           TO W-PROT-LINE
      *****              STRING '(int) no connection to : '
      *****                          DELIMITED BY SIZE
      *****                     WINBMAIL-POP3-SERVER
      *****                          DELIMITED BY SPACE
      *****                                  INTO W-PROT-LINE
      *****              PERFORM                  WRITE-PROTOKOLL
      *****              MOVE   '(int) program aborted'
      *****                                    TO W-PROT-LINE
      *****              PERFORM                  WRITE-PROTOKOLL
      *****                                 GO TO POP--020
      *****     END-IF.
      ******
      *****     MOVE     SPACES                TO W-PROT-LINE.
      *****     MOVE     SOCKET-NUMBER         TO W-PRT-NR.
      *****     STRING   '(int) connected to Server : '
      *****                          DELIMITED BY SIZE
      *****              WINBMAIL-POP3-SERVER
      *****                          DELIMITED BY SPACE
      *****              ' with port : '
      *****              W-PRT-NR    DELIMITED BY SIZE
      *****                                  INTO W-PROT-LINE.
      *****     PERFORM                           WRITE-PROTOKOLL.
      *******--------------------------------------------------------------*
      *******            send USER-command                                 *
      *******--------------------------------------------------------------*
      *****     STRING   '(int) logon as user : '
      *****                          DELIMITED BY SIZE
      *****              WINBMAIL-USER
      *****                          DELIMITED BY SPACE
      *****                                  INTO W-PROT-LINE.
      *****     PERFORM                           WRITE-PROTOKOLL.
      ******
      *****     MOVE     SPACES                TO WRK-DATA-RECORD.
      *****     MOVE     1                     TO BYTES-TO-SEND.
      *****     STRING   'USER '     DELIMITED BY SIZE
      *****              WINBMAIL-USER
      *****                          DELIMITED BY SPACE
      *****              CRLF        DELIMITED BY SIZE
      *****                                  INTO WRK-DATA-RECORD
      *****                          WITH POINTER BYTES-TO-SEND.
      *****     PERFORM                           SEND-DATA.
      ******
      *****     IF   NOT (WRK-DATA-RECORD(1:3)
      *****                                 EQUAL '250' OR '251' OR '+OK')
      *****              MOVE 47               TO WINBMAIL-RETURN
      *****              MOVE '(int) Error during logon at pop3-server'
      *****                                    TO W-PROT-LINE
      *****              PERFORM                  WRITE-PROTOKOLL
      *****              MOVE '(int) invalid or unknown user'
      *****                                    TO W-PROT-LINE
      *****              PERFORM                  WRITE-PROTOKOLL
      *****                                 GO TO POP--020
      *****     END-IF.
      *******--------------------------------------------------------------*
      *******            send password                                     *
      *******--------------------------------------------------------------*
      *****     MOVE     SPACES                TO WRK-DATA-RECORD.
      *****     MOVE     1                     TO BYTES-TO-SEND.
      *****     STRING   'PASS '     DELIMITED BY SIZE
      *****              WINBMAIL-USER-PASSWORT
      *****                          DELIMITED BY SPACE
      *****              CRLF        DELIMITED BY SIZE
      *****                                  INTO WRK-DATA-RECORD
      *****                          WITH POINTER BYTES-TO-SEND.
      *****     PERFORM                           SEND-DATA.
      ******
      *****     IF   NOT (WRK-DATA-RECORD(1:3)
      *****                                 EQUAL '250' OR '251' OR '+OK')
      *****              MOVE 47               TO WINBMAIL-RETURN
      *****              MOVE '(int) Error during logon at pop3-server'
      *****                                    TO W-PROT-LINE
      *****              PERFORM                  WRITE-PROTOKOLL
      *****              MOVE '(int) illegal or invalid password'
      *****                                    TO W-PROT-LINE
      *****              PERFORM                  WRITE-PROTOKOLL
      *****                                 GO TO POP--020
      *****     END-IF.
      *******--------------------------------------------------------------*
      *******            send list command                                 *
      *******--------------------------------------------------------------*
      *****     MOVE     SPACES                TO WRK-DATA-RECORD.
      *****     MOVE     1                     TO BYTES-TO-SEND.
      *****     STRING   'LIST '     DELIMITED BY SIZE
      *****              CRLF        DELIMITED BY SIZE
      *****                                  INTO WRK-DATA-RECORD
      *****                          WITH POINTER BYTES-TO-SEND.
      *****     PERFORM                           SEND-DATA.
      ******
      *****     IF   NOT (WRK-DATA-RECORD(1:3)
      *****                                 EQUAL '250' OR '251' OR '+OK')
      *****              MOVE 47               TO WINBMAIL-RETURN
      *****              MOVE '(int) Error during list command'
      *****                                    TO W-PROT-LINE
      *****              PERFORM                  WRITE-PROTOKOLL
      *****                                 GO TO POP--020
      *****     END-IF.
      *******--------------------------------------------------------------*
      *******            Close connection                                  *
      *******--------------------------------------------------------------*
      *****     MOVE     SPACES                TO WRK-DATA-RECORD.
      *****     MOVE     1                     TO BYTES-TO-SEND.
      *****     STRING   'QUIT'
      *****              CRLF        DELIMITED BY SIZE
      *****                                  INTO WRK-DATA-RECORD
      *****                          WITH POINTER BYTES-TO-SEND.
      *****     PERFORM                           SEND-DATA.
      ******
      *****     MOVE     '(int) login successfully completed'
      *****                                    TO W-PROT-LINE.
      *****     PERFORM                           WRITE-PROTOKOLL.
      *******--------------------------------------------------------------*
      *******            close pop3 socket                                 *
      *******--------------------------------------------------------------*
      *****     CALL     'C$SOCKET'         USING AGS-CLOSE
      *****                                       SOCKET-HANDLE.
      *****                                 GO TO POP--020.
      ******
      *****  POP--020.
      ******
      *****     EXIT.
      /
       SEND-AUTH-COMMAND SECTION.
      *
        SAC--010.
      *
      **--------------------------------------------------------------*
      **            Send AUTH-command                                 *
      **--------------------------------------------------------------*
           INITIALIZE                        WRK-DATA-RECORD.
           MOVE     1                     TO BYTES-TO-SEND.
           STRING   'AUTH LOGIN'
                    CRLF        DELIMITED BY SIZE
                                        INTO WRK-DATA-RECORD
                                WITH POINTER BYTES-TO-SEND.
           PERFORM                           SEND-DATA.
           MOVE     WRK-DATA-RECORD(1:3)  TO W-REPLY.
           IF   NOT W-AUTH-OK
                    MOVE 61               TO WINBMAIL-RETURN
                    MOVE '(int) Error during SMTP authentification'
                                          TO W-PROT-LINE
                    PERFORM                  WRITE-PROTOKOLL
                                       GO TO ABBRUCH
           END-IF.
      **--------------------------------------------------------------*
      **            convert user/password to base64 strings           *
      **--------------------------------------------------------------*
      **            username                                          *
      **--------------------------------------------------------------*
           MOVE     SPACES                TO WINLNG-TEXT.
           MOVE     1                     TO WINLNG-SIZE.
           STRING   WINBMAIL-USER
                                DELIMITED BY SPACE
                                        INTO WINLNG-TEXT
                                WITH POINTER WINLNG-SIZE.
           PERFORM                           AUTH-BASE64-CODING.
           PERFORM                           SEND-DATA.
           MOVE     WRK-DATA-RECORD(1:3)  TO W-REPLY.
           IF   NOT W-AUTH-OK
                    MOVE 61               TO WINBMAIL-RETURN
                    MOVE '(int) Error during SMTP authentification'
                                          TO W-PROT-LINE
                    PERFORM                  WRITE-PROTOKOLL
                                       GO TO ABBRUCH
           END-IF.
      **--------------------------------------------------------------*
      **            password                                          *
      **--------------------------------------------------------------*
           MOVE     SPACES                TO WINLNG-TEXT.
           MOVE     1                     TO WINLNG-SIZE.
           STRING   WINBMAIL-USER-PASSWORT
                                DELIMITED BY SPACE
                                        INTO WINLNG-TEXT
                                WITH POINTER WINLNG-SIZE.
           PERFORM                           AUTH-BASE64-CODING.
           PERFORM                           SEND-DATA.
           MOVE     WRK-DATA-RECORD(1:3)  TO W-REPLY.
           IF   NOT W-AUTH-SUCCEEDED
                    MOVE 61               TO WINBMAIL-RETURN
                    MOVE '(int) Error during SMTP authentification'
                                          TO W-PROT-LINE
                    PERFORM                  WRITE-PROTOKOLL
                                       GO TO ABBRUCH
           END-IF.
                                       GO TO SAC--020.
      *
        SAC--020.
      *
      **--------------------------------------------------------------*
      **            End of SMTP-AUTH command                          *
      **--------------------------------------------------------------*
           EXIT.
      /
       HALT SECTION.
      *
        HA--010.
      *
      **--------------------------------------------------------------*
      **            end of programm                                   *
      **--------------------------------------------------------------*
           CLOSE                      WINDOW MAI-PROGRESS-HANDLE.
           DESTROY                           MAI-PROGRESS-FRAME.
           DESTROY                           MAI-PROGRESS-HANDLE.
                                       GO TO HA--020.
      *
        HA--020.
      *
           GOBACK.
      /
       WRITE-PROTOKOLL SECTION.
      *
        WRP--010.
      *
      **--------------------------------------------------------------*
      **            write logfile                                     *
      **--------------------------------------------------------------*
      **            no logfile wanted                                 *
      **--------------------------------------------------------------*
           IF       W-PROTOKOLL        EQUAL ZEROS
                    MOVE ZEROS            TO W-PROT-FUNKTION
                    MOVE SPACES           TO W-PROT-LINE
                                       GO TO WRP--020
           END-IF.
      **--------------------------------------------------------------*
      **            skip empty lines                                  *
      **--------------------------------------------------------------*
           IF       W-PROT-LINE        EQUAL SPACES
                    MOVE ZEROS            TO W-PROT-FUNKTION
                                       GO TO WRP--020
           END-IF.
      **--------------------------------------------------------------*
      **            create logfile                                    *
      **--------------------------------------------------------------*
           MOVE     W-PROT-LINE           TO WINLNG-TEXT.
           PERFORM                           INTERNE-LAENGE.
      **--------------------------------------------------------------*
      **            hide password in logfile                          *
      **--------------------------------------------------------------*
           IF       W-PROT-LINE(1:4)   EQUAL 'PASS'
                    MOVE W-STARS(1:WINLNG-SIZE - 7)
                                          TO
                                          W-PROT-LINE(6:WINLNG-SIZE - 7)
           END-IF.
      **--------------------------------------------------------------*
      **            set the approbiate Definition term to buffer      *
      **--------------------------------------------------------------*
           MOVE     ZEROS                 TO WRP-IND1,
                                             WRP-IND2.
           MOVE     SPACES                TO SMP-SATZ.
           IF       W-PROT-OUT
                    MOVE '(out) '         TO SMP-SATZ
                    MOVE 6                TO WRP-IND2
           END-IF.
      *
           IF       W-PROT-IN
                    MOVE '(in)  '         TO SMP-SATZ
                    MOVE 6                TO WRP-IND2
           END-IF.
      *
        WRP--015.
      *
      **--------------------------------------------------------------*
      **            transfer each Character to print-buffer           *
      **--------------------------------------------------------------*
           ADD      1                     TO WRP-IND1.
           IF       WRP-IND1         GREATER WINLNG-SIZE
                                       GO TO WRP--019.
      *
           IF       W-PROT-LINE(WRP-IND1:1)
                                       EQUAL X'0D'
                AND W-PROT-LINE(WRP-IND1 + 1:1)
                                       EQUAL X'0A'
                    ADD  1                TO WRP-IND2
                    ADD  1                TO WRP-IND1
                    MOVE 'CRLF'           TO SMP-SATZ(WRP-IND2:4)
                    WRITE                    SMP-SATZ
                    MOVE SPACES           TO SMP-SATZ(6:)
                    MOVE 6                TO WRP-IND2
           ELSE
                    ADD  1                TO WRP-IND2
                    MOVE W-PROT-LINE(WRP-IND1:1)
                                          TO SMP-SATZ(WRP-IND2:1)
           END-IF.
                                       GO TO WRP--015.
      *
        WRP--019.
      *
      **--------------------------------------------------------------*
      **            write logfile line                                *
      **--------------------------------------------------------------*
           IF   NOT (SMP-SATZ          EQUAL '(out) ' OR '(in)  ')
                    WRITE                    SMP-SATZ
           END-IF.
      *
           MOVE     SPACES                TO W-PROT-LINE.
           MOVE     ZEROS                 TO W-PROT-FUNKTION.
                                       GO TO WRP--020.
      *
        WRP--020.
      *
           EXIT.
      /
       ABBRUCH SECTION.
      *
        ABB--010.
      *
      **--------------------------------------------------------------*
      **            send RSET if an error occured and stop all actions*
      **--------------------------------------------------------------*
           IF       WINBMAIL-RETURN    EQUAL ZEROS
                    MOVE 99               TO WINBMAIL-RETURN
           END-IF.
      *
           MOVE     SPACES                TO WRK-DATA-RECORD.
           MOVE     1                     TO BYTES-TO-SEND.
           STRING   'RSET '     DELIMITED BY SIZE
                                        INTO WRK-DATA-RECORD
                                WITH POINTER BYTES-TO-SEND.
           PERFORM                           SEND-DATA-MAIL.
      *
           MOVE     SPACES                TO WRK-DATA-RECORD.
           MOVE     1                     TO BYTES-TO-SEND.
           STRING   'QUIT'
                    CRLF        DELIMITED BY SIZE
                                        INTO WRK-DATA-RECORD
                                WITH POINTER BYTES-TO-SEND.
           PERFORM                           SEND-DATA-MAIL.
      **--------------------------------------------------------------*
      **            Close socket                                      *
      **--------------------------------------------------------------*
           CALL     'C$SOCKET'         USING AGS-CLOSE
                                             SOCKET-HANDLE.
      **--------------------------------------------------------------*
      **            close logfile if open                             *
      **--------------------------------------------------------------*
           IF       W-PROTOKOLL        EQUAL 1
                    CLOSE                    SMAILPR
           END-IF.
      **--------------------------------------------------------------*
      **            end of program                                    *
      **--------------------------------------------------------------*
           CLOSE                      WINDOW MAI-PROGRESS-HANDLE.
           DESTROY                           MAI-PROGRESS-FRAME.
           DESTROY                           MAI-PROGRESS-HANDLE.
                                       GO TO ABB--020.
      *
        ABB--020.
      *
           GOBACK.
      /
       CONVERT-TITEL SECTION.
      *
        CVT--010.
      *
      **--------------------------------------------------------------*
      **            convert title to quoted printable                 *
      **--------------------------------------------------------------*
           MOVE     1                     TO BYTES-TO-SEND.
           STRING   'Subject: =?iso-8859-1?Q?'
                                DELIMITED BY SIZE
                                        INTO WRK-DATA-RECORD
                                WITH POINTER BYTES-TO-SEND.
           MOVE     ZEROS                 TO W-BODY-LEN,
                                             W-IND5.
           INSPECT  WINBMAIL-TITEL  TALLYING W-IND5
                                FOR TRAILING SPACES.
           COMPUTE  W-BODY-LEN             = 256 - W-IND5 + 1.
           MOVE     ZEROS                 TO W-IND1,
           MOVE     BYTES-TO-SEND         TO W-IND2.
           SUBTRACT 1                   FROM W-IND2.
      *
        CVT--020.
      *
      **--------------------------------------------------------------*
      **            check each character in TITLE                     *
      **--------------------------------------------------------------*
           ADD      1                     TO W-IND1.
           IF       W-IND1           GREATER W-BODY-LEN
                                       GO TO CVT--030
           END-IF.
      *
           ADD      1                     TO W-IND2.
           EVALUATE WINBMAIL-TITEL(W-IND1:1)
      *             * A Umlaut                                        *
               WHEN 'Ä'
                    MOVE '=C4'            TO WRK-DATA-RECORD(W-IND2:3)
                    ADD  2                TO W-IND2
      *             * O Umlaut                                        *
               WHEN 'Ö'
                    MOVE '=D6'            TO WRK-DATA-RECORD(W-IND2:3)
                    ADD  2                TO W-IND2
      *             * U Umlaut                                        *
               WHEN 'Ü'
                    MOVE '=DC'            TO WRK-DATA-RECORD(W-IND2:3)
                    ADD  2                TO W-IND2
      *             * a Umlaut                                        *
               WHEN 'ä'
                    MOVE '=E4'            TO WRK-DATA-RECORD(W-IND2:3)
                    ADD  2                TO W-IND2
      *             * o Umlaut                                        *
               WHEN 'ö'
                    MOVE '=F6'            TO WRK-DATA-RECORD(W-IND2:3)
                    ADD  2                TO W-IND2
      *             * u Umlaut                                        *
               WHEN 'ü'
                    MOVE '=FC'            TO WRK-DATA-RECORD(W-IND2:3)
                    ADD  2                TO W-IND2
      *             * double quotes                                   *
               WHEN '"'
                    MOVE '=22'            TO WRK-DATA-RECORD(W-IND2:3)
                    ADD  2                TO W-IND2
      *             * Dollar-sign                                     *
               WHEN '$'
                    MOVE '=A7'            TO WRK-DATA-RECORD(W-IND2:3)
                    ADD  2                TO W-IND2
      *             * open bracket                                    *
               WHEN '('
                    MOVE '=28'            TO WRK-DATA-RECORD(W-IND2:3)
                    ADD  2                TO W-IND2
      *             * closed bracket                                  *
               WHEN ')'
                    MOVE '=29'            TO WRK-DATA-RECORD(W-IND2:3)
                    ADD  2                TO W-IND2
      *             * equal-sign                                      *
               WHEN '='
                    MOVE '=3D'            TO WRK-DATA-RECORD(W-IND2:3)
                    ADD  2                TO W-IND2
      *             * question mark                                   *
               WHEN '?'
                    MOVE '=3F'            TO WRK-DATA-RECORD(W-IND2:3)
                    ADD  2                TO W-IND2
      *             * eckige Klammer auf                              *
               WHEN '['
                    MOVE '=5B'            TO WRK-DATA-RECORD(W-IND2:3)
                    ADD  2                TO W-IND2
      *             * eckige Klammer zu                               *
               WHEN ']'
                    MOVE '=5D'            TO WRK-DATA-RECORD(W-IND2:3)
                    ADD  2                TO W-IND2
      *             * Euro-sign                                       *
               WHEN '€'
                    MOVE '=3F'            TO WRK-DATA-RECORD(W-IND2:3)
                    ADD  2                TO W-IND2
      *             * Underscore                                      *
               WHEN '_'
                    MOVE '=5F'            TO WRK-DATA-RECORD(W-IND2:3)
                    ADD  2                TO W-IND2
      *             * comma                                           *
               WHEN ','
                    MOVE '=2C'            TO WRK-DATA-RECORD(W-IND2:3)
                    ADD  2                TO W-IND2
      *             * less than                                       *
               WHEN '<'
                    MOVE '=3C'            TO WRK-DATA-RECORD(W-IND2:3)
                    ADD  2                TO W-IND2
      *             * greater than                                    *
               WHEN '>'
                    MOVE '=3E'            TO WRK-DATA-RECORD(W-IND2:3)
                    ADD  2                TO W-IND2
      *             * sharp s                                         *
               WHEN 'ß'
                    MOVE '=DF'            TO WRK-DATA-RECORD(W-IND2:3)
                    ADD  2                TO W-IND2
      *             * umgekehrtes Hochkomma                           *
               WHEN '´'
                    MOVE '=B4'            TO WRK-DATA-RECORD(W-IND2:3)
                    ADD  2                TO W-IND2
      *             * space                                           *
               WHEN ' '
                    MOVE '_'              TO WRK-DATA-RECORD(W-IND2:1)
      *             * all other characters                            *
               WHEN OTHER
                    MOVE WINBMAIL-TITEL(W-IND1:1)
                                          TO WRK-DATA-RECORD(W-IND2:1)
           END-EVALUATE.
                                       GO TO CVT--020.
      *
        CVT--030.
      *
      **--------------------------------------------------------------*
      **            Set end of Title and send data                    *
      **--------------------------------------------------------------*
           ADD      1                     TO W-IND2.
           MOVE     '?='                  TO WRK-DATA-RECORD(W-IND2:2).
           ADD      2                     TO W-IND2.
           MOVE     X'0D'                 TO WRK-DATA-RECORD(W-IND2:1).
           ADD      1                     TO W-IND2.
           MOVE     X'0A'                 TO WRK-DATA-RECORD(W-IND2:1).
           ADD      1                     TO W-IND2.
           MOVE     W-IND2                TO BYTES-TO-SEND.
           PERFORM                           SEND-DATA-MAIL.
                                       GO TO CVT--040.
      *
        CVT--040.
      *
           EXIT.
      /
       BODY-BASE64-CODING SECTION.
      *
        D64--010.
      *
      **--------------------------------------------------------------*
      **            coding into BASE64 for the mailbody               *
      **--------------------------------------------------------------*
           MOVE     SPACES                TO WRK-DATA-RECORD.
           MOVE     ZEROS                 TO W-BODY-LEN,
                                             W-IND5.
           INSPECT  WINBMAIL-BODY   TALLYING W-IND5
                                FOR TRAILING SPACES.
           COMPUTE  W-BODY-LEN             = 64000 - W-IND5 + 1.
           DIVIDE   W-BODY-LEN            BY 3
                                      GIVING W-SIZE
                                   REMAINDER W-SIZE-REST.
           MOVE     ZEROS                 TO W-IND1,
                                             W-IND2,
                                             W-IND3,
                                             W-IND4,
                                             W-IND5,
                                             W-IND6
                                             BLOCK-IND.
           COMPUTE  W-FILE-COMP            = W-BODY-LEN / W-ADDITION.
           MOVE     ZEROS                 TO W-FILE-INDEX.
           MOVE     W-FILE-COMP-VK        TO W-FILE-SHOW.
      *
        D64--020.
      *
      **--------------------------------------------------------------*
      **            for each character in body do                     *
      **--------------------------------------------------------------*
           ADD      1                     TO BLOCK-IND.
           IF       BLOCK-IND        GREATER W-BODY-LEN
                    IF W-SIZE-REST   GREATER ZEROS
                                       GO TO D64--050
                    ELSE
                                       GO TO D64--080
                    END-IF
           END-IF.
      *
           ADD      1                     TO W-FILE-INDEX.
           COMPUTE  W-FILE-COMP            = W-FILE-INDEX / W-FILE-SHOW.
           IF       W-FILE-COMP-NK     EQUAL ZEROS
                    ADD 1                 TO MAI-BAR-PROZENT
                    MOVE   MAI-BAR-PROZENT
                                          TO STATUS-PROZENT
                    MODIFY MAI-PROGRESS-FRAME
                              FILL-PERCENT = MAI-BAR-PROZENT
                                     TITLE = FRAME-TITLE
           END-IF.
      *
           ADD      1                     TO W-IND.
           IF       W-IND               LESS 3
                    MOVE ZEROS            TO ORDINATE
                    MOVE WINBMAIL-BODY(BLOCK-IND:1)
                                          TO ORD-ZEICHEN
                    MOVE W-BITS(ORDINATE + 1)
                                          TO W-BYTE-IN(W-IND)
                                       GO TO D64--020
           END-IF.
      *
           MOVE     ZEROS                 TO ORDINATE.
           MOVE     WINBMAIL-BODY(BLOCK-IND:1)
                                          TO ORD-ZEICHEN.
           MOVE     W-BITS(ORDINATE + 1)  TO W-BYTE-IN(W-IND).
           MOVE     ZEROS                 TO W-IND1,
                                             W-IND2.
           MOVE     3                     TO W-IND4.
      *
        D64--040.
      *
      **--------------------------------------------------------------*
      **            always work with 3 Bytes                          *
      **--------------------------------------------------------------*
           ADD      1                     TO W-IND1.
           IF       W-IND1           GREATER 4
                    MOVE ALL '0'          TO W-BYTES
                    MOVE ZEROS            TO W-IND
                                       GO TO D64--020
           END-IF.
      *
           MOVE     ZEROS                 TO W-IND2.
           COMPUTE  W-IND2                 = (W-BYTE-O(W-IND1, 1) * 32)
                                           + (W-BYTE-O(W-IND1, 2) * 16)
                                           + (W-BYTE-O(W-IND1, 3) * 8 )
                                           + (W-BYTE-O(W-IND1, 4) * 4 )
                                           + (W-BYTE-O(W-IND1, 5) * 2 )
                                           + (W-BYTE-O(W-IND1, 6) * 1 ).
           ADD      1                     TO W-IND2.
           ADD      1                     TO W-IND3.
           MOVE     W-BASE64-TXT(W-IND2)  TO WRK-DATA-RECORD(W-IND3:1).
           IF       W-IND3             EQUAL 76
                    MOVE X'0D'            TO WRK-DATA-RECORD(77:1)
                    MOVE X'0A'            TO WRK-DATA-RECORD(78:1)
                    MOVE 79               TO BYTES-TO-SEND
                    PERFORM                  SEND-DATA-MAIL
                    MOVE SPACES           TO WRK-DATA-RECORD
                    MOVE ZEROS            TO W-IND3
           END-IF.
                                       GO TO D64--040.
      *
        D64--050.
      *
      **--------------------------------------------------------------*
      **            rest of body                                      *
      **--------------------------------------------------------------*
           MOVE     ZEROS                 TO W-IND1,
                                             W-IND2.
           MOVE     W-SIZE-REST           TO W-IND4.
      *
        D64--060.
      *
           ADD      1                     TO W-IND1.
           IF       W-IND1           GREATER W-SIZE-REST + 1
                    IF W-SIZE-REST     EQUAL 2
                       MOVE 1             TO W-IND4
                    END-IF
                    IF W-SIZE-REST     EQUAL 1
                       MOVE 2             TO W-IND4
                    END-IF
                    MOVE ALL '0'          TO W-BYTES
                                       GO TO D64--070
           END-IF.
      *
           MOVE     ZEROS                 TO W-IND2.
           COMPUTE  W-IND2                 = (W-BYTE-O(W-IND1, 1) * 32)
                                           + (W-BYTE-O(W-IND1, 2) * 16)
                                           + (W-BYTE-O(W-IND1, 3) * 8 )
                                           + (W-BYTE-O(W-IND1, 4) * 4 )
                                           + (W-BYTE-O(W-IND1, 5) * 2 )
                                           + (W-BYTE-O(W-IND1, 6) * 1 ).
           ADD      1                     TO W-IND2.
           ADD      1                     TO W-IND3.
           MOVE     W-BASE64-TXT(W-IND2)  TO WRK-DATA-RECORD(W-IND3:1).
           IF       W-IND3             EQUAL 76
                    MOVE X'0D'            TO WRK-DATA-RECORD(77:1)
                    MOVE X'0A'            TO WRK-DATA-RECORD(78:1)
                    MOVE 79               TO BYTES-TO-SEND
                    PERFORM                  SEND-DATA-MAIL
                    MOVE SPACES           TO WRK-DATA-RECORD
                    MOVE ZEROS            TO W-IND3
           END-IF.
                                       GO TO D64--060.
      *
        D64--070.
      *
      **--------------------------------------------------------------*
      **            insert PAD-chars ('=') if len mod 3 <> zero       *
      **--------------------------------------------------------------*
           ADD      1                     TO W-IND3.
           MOVE     '='                   TO WRK-DATA-RECORD(W-IND3:1).
           IF       W-IND3             EQUAL 76
                    MOVE X'0D'            TO WRK-DATA-RECORD(77:1)
                    MOVE X'0A'            TO WRK-DATA-RECORD(78:1)
                    MOVE 79               TO BYTES-TO-SEND
                    PERFORM                  SEND-DATA-MAIL
                    MOVE SPACES           TO WRK-DATA-RECORD
                    MOVE ZEROS            TO W-IND3
           END-IF.
      *
           SUBTRACT 1                   FROM W-IND4.
           IF       W-IND4           GREATER ZEROS
                                       GO TO D64--070
           END-IF.
      *
        D64--080.
      *
      **--------------------------------------------------------------*
      **            send the last block of data                       *
      **--------------------------------------------------------------*
           ADD      1                     TO W-IND3.
           MOVE     X'0D'                 TO WRK-DATA-RECORD(W-IND3:1).
           ADD      1                     TO W-IND3.
           MOVE     X'0A'                 TO WRK-DATA-RECORD(W-IND3:1).
           ADD      1                     TO W-IND3.
           MOVE     W-IND3                TO BYTES-TO-SEND.
           PERFORM                           SEND-DATA-MAIL.
           MOVE     SPACES                TO WRK-DATA-RECORD.
                                       GO TO D64--090.
      *
        D64--090.
      *
           EXIT.
      /
       AUTH-BASE64-CODING SECTION.
      *
        A64--010.
      *
      **--------------------------------------------------------------*
      **            coding into BASE64 for the username/password      *
      **--------------------------------------------------------------*
           MOVE     SPACES                TO WRK-DATA-RECORD.
           MOVE     ZEROS                 TO W-BODY-LEN,
                                             W-IND5.
           SUBTRACT 1                   FROM WINLNG-SIZE.
           DIVIDE   WINLNG-SIZE           BY 3
                                      GIVING W-SIZE
                                   REMAINDER W-SIZE-REST.
           MOVE     ZEROS                 TO W-IND,
                                             W-IND1,
                                             W-IND2,
                                             W-IND3,
                                             W-IND4,
                                             W-IND5,
                                             W-IND6
                                             BLOCK-IND.
      *
        A64--020.
      *
      **--------------------------------------------------------------*
      **            for each character do                             *
      **--------------------------------------------------------------*
           ADD      1                     TO BLOCK-IND.
           IF       BLOCK-IND        GREATER WINLNG-SIZE
                    IF W-SIZE-REST   GREATER ZEROS
                                       GO TO A64--050
                    ELSE
                                       GO TO A64--080
                    END-IF
           END-IF.
      *
           ADD      1                     TO W-IND.
           IF       W-IND               LESS 3
                    MOVE ZEROS            TO ORDINATE
                    MOVE WINLNG-TEXT(BLOCK-IND:1)
                                          TO ORD-ZEICHEN
                    MOVE W-BITS(ORDINATE + 1)
                                          TO W-BYTE-IN(W-IND)
                                       GO TO A64--020
           END-IF.
      *
           MOVE     ZEROS                 TO ORDINATE.
           MOVE     WINLNG-TEXT(BLOCK-IND:1)
                                          TO ORD-ZEICHEN.
           MOVE     W-BITS(ORDINATE + 1)  TO W-BYTE-IN(W-IND).
           MOVE     ZEROS                 TO W-IND1,
                                             W-IND2.
      *
        A64--040.
      *
      **--------------------------------------------------------------*
      **            always work with 3 Bytes                          *
      **--------------------------------------------------------------*
           ADD      1                     TO W-IND1.
           IF       W-IND1           GREATER 4
                    MOVE ALL '0'          TO W-BYTES
                    MOVE ZEROS            TO W-IND
                                       GO TO A64--020
           END-IF.
      *
           MOVE     ZEROS                 TO W-IND2.
           COMPUTE  W-IND2                 = (W-BYTE-O(W-IND1, 1) * 32)
                                           + (W-BYTE-O(W-IND1, 2) * 16)
                                           + (W-BYTE-O(W-IND1, 3) * 8 )
                                           + (W-BYTE-O(W-IND1, 4) * 4 )
                                           + (W-BYTE-O(W-IND1, 5) * 2 )
                                           + (W-BYTE-O(W-IND1, 6) * 1 ).
           ADD      1                     TO W-IND2.
           ADD      1                     TO W-IND3.
           MOVE     W-BASE64-TXT(W-IND2)  TO WRK-DATA-RECORD(W-IND3:1).
                                       GO TO A64--040.
      *
        A64--050.
      *
      **--------------------------------------------------------------*
      **            rest of group                                     *
      **--------------------------------------------------------------*
           MOVE     ZEROS                 TO W-IND1,
                                             W-IND2,
                                             W-IND4.
      *
        A64--060.
      *
           ADD      1                     TO W-IND1.
           IF       W-IND1           GREATER W-SIZE-REST + 1
                    IF W-SIZE-REST     EQUAL 2
                       MOVE 1             TO W-IND4
                    END-IF
                    IF W-SIZE-REST     EQUAL 1
                       MOVE 2             TO W-IND4
                    END-IF
                    MOVE ALL '0'          TO W-BYTES
                                       GO TO A64--070
           END-IF.
      *
           MOVE     ZEROS                 TO W-IND2.
           COMPUTE  W-IND2                 = (W-BYTE-O(W-IND1, 1) * 32)
                                           + (W-BYTE-O(W-IND1, 2) * 16)
                                           + (W-BYTE-O(W-IND1, 3) * 8 )
                                           + (W-BYTE-O(W-IND1, 4) * 4 )
                                           + (W-BYTE-O(W-IND1, 5) * 2 )
                                           + (W-BYTE-O(W-IND1, 6) * 1 ).
           ADD      1                     TO W-IND2.
           ADD      1                     TO W-IND3.
           MOVE     W-BASE64-TXT(W-IND2)  TO WRK-DATA-RECORD(W-IND3:1).
                                       GO TO A64--060.
      *
        A64--070.
      *
      **--------------------------------------------------------------*
      **            insert PAD-chars ('=') if len mod 3 <> zero       *
      **--------------------------------------------------------------*
           ADD      1                     TO W-IND3.
           MOVE     '='                   TO WRK-DATA-RECORD(W-IND3:1).
           SUBTRACT 1                   FROM W-IND4.
           IF       W-IND4           GREATER ZEROS
                                       GO TO A64--070
           END-IF.
      *
        A64--080.
      *
      **--------------------------------------------------------------*
      **            end of routine                                    *
      **--------------------------------------------------------------*
           ADD      1                     TO W-IND3.
           MOVE     X'0D'                 TO WRK-DATA-RECORD(W-IND3:1).
           ADD      1                     TO W-IND3.
           MOVE     X'0A'                 TO WRK-DATA-RECORD(W-IND3:1).
           COMPUTE  BYTES-TO-SEND          = W-IND3 + 1.
                                       GO TO A64--090.
      *
        A64--090.
      *
           EXIT.
      /
       BASE64-CODING SECTION.
      *
        B64--010.
      *
      **--------------------------------------------------------------*
      **            coding into BASE64 for attachment files           *
      **--------------------------------------------------------------*
           MOVE     ZEROS                 TO W-SIZE,
                                             W-SIZE-REST,
                                             W-MAX-BLOCKS,
                                             W-ANZ-BLOCKS,
                                             W-MAX-CHARS,
                                             BLOCK-IND,
                                             BLOCK-MAX.
           MOVE     SPACES                TO WRK-DATA-RECORD.
           MOVE     W-ANHANG(W-ANHANG-NR) TO SMAIL-FILE.
           DIVIDE   FILE-INFO-SIZE        BY 3
                                      GIVING W-SIZE
                                   REMAINDER W-SIZE-REST.
           MOVE     ZEROS                 TO W-MAX-BLOCKS.
           COMPUTE  W-MAX-BLOCKS           = FILE-INFO-SIZE / 2040.
           IF       W-MAX-BLOCKS-NK    EQUAL ZEROS
                    MOVE    2040          TO W-MAX-CHARS
           ELSE
                    COMPUTE W-MAX-CHARS    = FILE-INFO-SIZE
                                           - (W-MAX-BLOCKS-VK * 2040)
                    MOVE    ZEROS         TO W-MAX-BLOCKS-NK
           END-IF.
           MOVE     ZEROS                 TO W-IND1,
                                             W-IND2,
                                             W-IND3,
                                             W-IND4,
                                             W-IND5,
                                             W-IND6,
                                             W-IND.
           MOVE     ZEROS                 TO W-FILE-INDEX.
           MOVE     W-FILE-COMP-VK        TO W-FILE-SHOW.
      **--------------------------------------------------------------*
      **            open attachement file                             *
      **--------------------------------------------------------------*
           OPEN     INPUT                    SMAILIN.
           IF   NOT FILE-STATUS        EQUAL '00'
                    MOVE   SPACES         TO W-PROT-LINE
                    STRING '(int) can not open file : '
                           WINSPLIT-FILE(1:WINSPLIT-FILE-LEN)
                           ' !'
                                DELIMITED BY SIZE
                                        INTO W-PROT-LINE
                    PERFORM                  WRITE-PROTOKOLL
                    MOVE   60             TO WINBMAIL-RETURN
                                       GO TO B64--090
           END-IF.
      *
        B64--020.
      *
      **--------------------------------------------------------------*
      **            read input file with blocks of 2040 chars         *
      **--------------------------------------------------------------*
           MOVE     ALL X'FF'             TO SMI-SATZ.
           READ     SMAILIN           RECORD AT END
                    CLOSE                    SMAILIN
                    IF W-SIZE-REST   GREATER ZEROS
                                       GO TO B64--050
                    ELSE
                                       GO TO B64--080
                    END-IF
           END-READ.
      *
           ADD      1                     TO W-ANZ-BLOCKS.
           MOVE     ZEROS                 TO BLOCK-IND.
           MOVE     2040                  TO BLOCK-MAX.
           IF       W-ANZ-BLOCKS     GREATER W-MAX-BLOCKS-VK
                    MOVE W-MAX-CHARS      TO BLOCK-MAX
           END-IF.
      *
        B64--030.
      *
      **--------------------------------------------------------------*
      **            fro each char in the block do                     *
      **--------------------------------------------------------------*
           ADD      1                     TO BLOCK-IND.
           IF       BLOCK-IND        GREATER BLOCK-MAX
                                       GO TO B64--020
           END-IF.
      *
           ADD      1                     TO W-FILE-INDEX.
           COMPUTE  W-FILE-COMP            = W-FILE-INDEX / W-FILE-SHOW.
           IF       W-FILE-COMP-NK     EQUAL ZEROS
                    ADD 1                 TO MAI-BAR-PROZENT
                    MOVE   MAI-BAR-PROZENT
                                          TO STATUS-PROZENT
                    MODIFY MAI-PROGRESS-FRAME
                              FILL-PERCENT = MAI-BAR-PROZENT
                                     TITLE = FRAME-TITLE
           END-IF.
      *
           ADD      1                     TO W-IND.
           IF       W-IND               LESS 3
                    MOVE ZEROS            TO ORDINATE
                    MOVE SMI-DATEN(BLOCK-IND)
                                          TO ORD-ZEICHEN
                    MOVE W-BITS(ORDINATE + 1)
                                          TO W-BYTE-IN(W-IND)
                                       GO TO B64--030
           END-IF.
      *
           MOVE     ZEROS                 TO ORDINATE.
           MOVE     SMI-DATEN(BLOCK-IND)  TO ORD-ZEICHEN.
           MOVE     W-BITS(ORDINATE + 1)  TO W-BYTE-IN(W-IND).
           MOVE     ZEROS                 TO W-IND1,
                                             W-IND2,
                                             W-IND4.
      *
        B64--040.
      *
      **--------------------------------------------------------------*
      **            always work with 3 Bytes                          *
      **--------------------------------------------------------------*
           ADD      1                     TO W-IND1.
           IF       W-IND1           GREATER 4
                    MOVE ALL '0'          TO W-BYTES
                    MOVE ZEROS            TO W-IND
                                       GO TO B64--030
           END-IF.
      *
           MOVE     ZEROS                 TO W-IND2.
           COMPUTE  W-IND2                 = (W-BYTE-O(W-IND1, 1) * 32)
                                           + (W-BYTE-O(W-IND1, 2) * 16)
                                           + (W-BYTE-O(W-IND1, 3) * 8 )
                                           + (W-BYTE-O(W-IND1, 4) * 4 )
                                           + (W-BYTE-O(W-IND1, 5) * 2 )
                                           + (W-BYTE-O(W-IND1, 6) * 1 ).
           ADD      1                     TO W-IND2.
           ADD      1                     TO W-IND3.
           MOVE     W-BASE64-TXT(W-IND2)  TO WRK-DATA-RECORD(W-IND3:1).
           IF       W-IND3             EQUAL 76
                    MOVE X'0D'            TO WRK-DATA-RECORD(77:1)
                    MOVE X'0A'            TO WRK-DATA-RECORD(78:1)
                    MOVE 79               TO BYTES-TO-SEND
                    PERFORM                  SEND-DATA-MAIL
                    MOVE SPACES           TO WRK-DATA-RECORD
                    MOVE ZEROS            TO W-IND3
           END-IF.
                                       GO TO B64--040.
      *
        B64--050.
      *
      **--------------------------------------------------------------*
      **            rest of group                                     *
      **--------------------------------------------------------------*
           MOVE     ZEROS                 TO W-IND1,
                                             W-IND2.
           MOVE     W-SIZE-REST           TO W-IND4.
      *
        B64--060.
      *
           ADD      1                     TO W-IND1.
           IF       W-IND1           GREATER W-SIZE-REST + 1
                    IF W-SIZE-REST     EQUAL 2
                       MOVE 1             TO W-IND4
                    END-IF
                    IF W-SIZE-REST     EQUAL 1
                       MOVE 2             TO W-IND4
                    END-IF
                    MOVE ALL '0'          TO W-BYTES
                                       GO TO B64--070
           END-IF.
      *
           MOVE     ZEROS                 TO W-IND2.
           COMPUTE  W-IND2                 = (W-BYTE-O(W-IND1, 1) * 32)
                                           + (W-BYTE-O(W-IND1, 2) * 16)
                                           + (W-BYTE-O(W-IND1, 3) * 8 )
                                           + (W-BYTE-O(W-IND1, 4) * 4 )
                                           + (W-BYTE-O(W-IND1, 5) * 2 )
                                           + (W-BYTE-O(W-IND1, 6) * 1 ).
           ADD      1                     TO W-IND2.
           ADD      1                     TO W-IND3.
           MOVE     W-BASE64-TXT(W-IND2)  TO WRK-DATA-RECORD(W-IND3:1).
           IF       W-IND3             EQUAL 76
                    MOVE X'0D'            TO WRK-DATA-RECORD(77:1)
                    MOVE X'0A'            TO WRK-DATA-RECORD(78:1)
                    MOVE 79               TO BYTES-TO-SEND
                    PERFORM                  SEND-DATA-MAIL
                    MOVE SPACES           TO WRK-DATA-RECORD
                    MOVE ZEROS            TO W-IND3
           END-IF.
                                       GO TO B64--060.
      *
        B64--070.
      *
      **--------------------------------------------------------------*
      **            insert PAD-chars ('=') if len mod 3 <> zero       *
      **--------------------------------------------------------------*
           ADD      1                     TO W-IND3.
           MOVE     '='                   TO WRK-DATA-RECORD(W-IND3:1).
           IF       W-IND3             EQUAL 76
                    MOVE X'0D'            TO WRK-DATA-RECORD(77:1)
                    MOVE X'0A'            TO WRK-DATA-RECORD(78:1)
                    MOVE 79               TO BYTES-TO-SEND
                    PERFORM                  SEND-DATA-MAIL
                    MOVE SPACES           TO WRK-DATA-RECORD
                    MOVE ZEROS            TO W-IND3
           END-IF.
      *
           SUBTRACT 1                   FROM W-IND4.
           IF       W-IND4           GREATER ZEROS
                                       GO TO B64--070
           END-IF.
      *
        B64--080.
      *
      **--------------------------------------------------------------*
      **            end of routine an send                            *
      **--------------------------------------------------------------*
           ADD      1                     TO W-IND3.
           MOVE     X'0D'                 TO WRK-DATA-RECORD(W-IND3:1).
           ADD      1                     TO W-IND3.
           MOVE     X'0A'                 TO WRK-DATA-RECORD(W-IND3:1).
           ADD      1                     TO W-IND3.
           MOVE     W-IND3                TO BYTES-TO-SEND.
           PERFORM                           SEND-DATA-MAIL.
           MOVE     SPACES                TO WRK-DATA-RECORD.
                                       GO TO B64--090.
      *
        B64--090.
      *
           EXIT.
      /
       GET-FILEINFO SECTION.
      *
        GFI--010.
      *
      **--------------------------------------------------------------*
      **            get file information                              *
      **--------------------------------------------------------------*
           INITIALIZE                        FILE-INFO-BLOCK.
           MOVE     ZEROS                 TO FILE-INFO-STATUS.
           CALL     'C$FILEINFO'       USING FILE-INFO-NAME,
                                             FILE-INFO-BLOCK,
                                      GIVING FILE-INFO-STATUS.
           IF       FILE-INFO-STATUS   EQUAL 1
                                       GO TO GFI--020
           END-IF.
      *
           IF       FILE-INFO-SIZE     EQUAL ZEROS
                AND FILE-INFO-DATE     EQUAL ZEROS
                AND FILE-INFO-TIME     EQUAL ZEROS
                    MOVE 2                TO FILE-INFO-STATUS
           END-IF.
                                       GO TO GFI--020.
      *
        GFI--020.
      *
           EXIT.
      /
       SOCKET-ERROR SECTION.
      *
        SOE--010.
      *
      **--------------------------------------------------------------*
      **            get last error from socket                        *
      **--------------------------------------------------------------*
           CALL     'C$SOCKET'         USING AGS-LAST-ERROR,
                                             SOCKET-NUMBER.
      *
           MOVE     RETURN-CODE           TO W-ERROR.
           MOVE     SPACES                TO W-MELDUNG.
           STRING   '(int) Socket-error number : '
                    W-ERROR     DELIMITED BY SIZE
                                       INTO W-MELDUNG.
           DISPLAY                  MESSAGE BOX
                    'socket error number : '
                    W-ERROR
                    ' occured.'
                    X'0A'
                    'process will be aborted'
                    X'0A'
                                       TITLE
                    'WINBMAIL -  E-Mailsystem'
           END-DISPLAY.
                                       GO TO SOE--020.
      *
        SOE--020.
      *
           EXIT.
      /
       INTERNE-LAENGE SECTION.
      *
        ILA--010.
      *
      **--------------------------------------------------------------*
      **            get length of string                              *
      **--------------------------------------------------------------*
           MOVE     ZEROS                 TO WINLNG-SIZE.
           INSPECT  WINLNG-TEXT     TALLYING WINLNG-SIZE
                                FOR TRAILING SPACES.
           COMPUTE  WINLNG-SIZE            = 1024 - WINLNG-SIZE.
           IF       WINLNG-SIZE        EQUAL ZEROS
                    MOVE 1                TO WINLNG-SIZE
           END-IF.
                                       GO TO ILA--020.
      *
        ILA--020.
      *
           EXIT.
      /
       SPLIT-FILE SECTION.
      *
        SPF--010.
      *
      **--------------------------------------------------------------*
      **            split file                                        *
      **--------------------------------------------------------------*
           MOVE     WINSPLIT-DATEI        TO WINLNG-TEXT.
           PERFORM                           INTERNE-LAENGE.
           ADD      1                     TO WINLNG-SIZE.
      *
        SPF--020.
      *
           SUBTRACT 1                   FROM WINLNG-SIZE.
           IF       WINLNG-SIZE        EQUAL ZEROS
                    MOVE WINSPLIT-DATEI   TO WINSPLIT-FILE
                                       GO TO SPF--030
           END-IF.
      *
           IF   NOT (WINSPLIT-DATEI(WINLNG-SIZE:1)
                                       EQUAL '/' OR '\')
                                       GO TO SPF--020
           END-IF.
      *
           ADD      1                     TO WINLNG-SIZE.
           MOVE     WINSPLIT-DATEI(WINLNG-SIZE:)
                                          TO WINSPLIT-FILE.
      *
        SPF--030.
      *
           MOVE     WINSPLIT-FILE         TO WINLNG-TEXT.
           PERFORM                           INTERNE-LAENGE.
           MOVE     WINLNG-SIZE           TO WINSPLIT-FILE-LEN.
                                       GO TO SPF--040.
      *
        SPF--040.
      *
           EXIT.
      /

