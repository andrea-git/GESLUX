       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      stparallelo-p.
       AUTHOR.                          Andrea.
       REMARKS. Stampa le bolle con il layout grafico da stampare al di sotto
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "lineseq.sl". 

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.          
           copy "lineseq.fd".        

       WORKING-STORAGE SECTION. 
           copy "acugui.def".
           copy "spooler.def".
           copy "fonts.def".     
                                   
       77  wstampa               pic x(256).
       77  status-lineseq        pic xx.     
       77  nome-stampante        pic x(200).
       77  path-bmp              pic x(200).
       77  CourierNew7           handle of font.
       77  CourierNew6           handle of font.
       77  CourierNew9           handle of font.
       77  CourierNew10          handle of font.
       77  BitmapSfondoHandle    pic S9(9) comp-4.
       77  WFONT-STATUS          pic s9(5) value ZERO.
       77  font-handle         handle of font.
       77  dim-crt pic 99 value 0.            
       77 extra-dim        PIC  9.
       77 extra-passo      PIC  s9999v99.
       77 CONT PIC  9(2).

       LINKAGE SECTION.
           copy "splcrt2graf.lks".

      ******************************************************************
       PROCEDURE DIVISION using splcrt2graf-link.

      ***--- 
       MAIN-PRG.
           perform OPEN-FILES.
           perform ELABORAZIONE.
           perform CHIUDI-STAMPA.
           perform CLOSE-FILES.
           perform EXIT-PGM.

      ***---
       OPEN-FILES.
           move splcrt2graf-percorso-stampa to wstampa.
           open input lineseq.
           
      ***---
       ELABORAZIONE.                       initialize spooler-link.
           move "splcrt2graf" to spl-nome-job.|?????????
                                                         
           set spl-apertura to true.
           move 1  to spl-num-copie.
           set spl-vertical   to true.

           move 1,5    to spl-margine-inf.
           move 0,5    to spl-margine-destro
                          spl-margine-sinistro.   
           move splcrt2graf-dim-crt to dim-crt
                                          
           accept spl-nome-stampante 
           from environment "STAMPANTE_STBOLLE".
           call "spooler" using spooler-link
           if spl-sta-annu exit paragraph end-if.   

           perform CARICA-FONT.
           move font-handle  to spl-hfont.  
                                            
           set spl-stringa to true.
           |Mi riposiziono ad inizio foglio
           move 0      to spl-riga.
           move space  to spl-riga-stampa.                              
           call "spooler" using spooler-link.

           set spl-stringa   to true.
           move 0,42         to spl-passo. 
           initialize spl-riga-stampa.
           move 0,84         to spl-riga.
           move 1            to spl-tipo-colonna.

           evaluate true
           when splcrt2graf-windows
                move splcrt2graf-percorso-stampa     to WSTAMPA
           when splcrt2graf-unix
                move splcrt2graf-percorso-stampa-u   to WSTAMPA
           end-evaluate.
                                                                      
      *                 move 1                   to spl-num-copie
      *                 move nome-stampante      to spl-nome-stampante
      *                 set spl-apertura         to true
      *                 set spl-vertical         to true
      *                 set wfdevice-win-printer to true
      *                 call "spooler" using spooler-link
      *                 move splcrt2graf-dim-crt to dim-crt
      *              else
      *                 set spl-salto-pagina to true
      *                 call "spooler" using spooler-link   
      *              end-if
      *           end-if
       
           perform until 1 = 2
              read lineseq next at end exit perform end-read
              if line-riga(1:1) = "@"             
      *****           if line-riga(2:3) = "STP"  
      *****              if nome-stampante = spaces
      *****                 move line-riga(5:)       to nome-stampante              
      *****                 move 1                   to spl-num-copie
      *****                 move nome-stampante      to spl-nome-stampante
      *****                 set spl-apertura         to true
      *****                 set spl-vertical         to true
      *****                 set wfdevice-win-printer to true
      *****                 call "spooler" using spooler-link
      *****                 perform CARICA-FONT
      *****              else
      *****                 set spl-salto-pagina to true
      *****                 call "spooler" using spooler-link   
      *****              end-if
      *****           end-if
                 if line-riga(2:3) = "BMP"                  
                    move line-riga(5:) to path-bmp
                    call "W$BITMAP" using WBITMAP-LOAD, path-bmp,
                                   giving BitmapSfondoHandle
                    set spl-bitmap to true
                    move 1,5 to spl-colonna
                    move 3 to spl-riga
                    move BitmapSfondoHandle   to spl-hbitmap
                    move 26,8 to spl-bitmap-height
                    move 19,5 to spl-bitmap-width
                    call "spooler" using spooler-link
                    move 7 to spl-riga
                 end-if
                 exit perform cycle
              end-if            
              
              add spl-passo to spl-riga
              if extra-passo not = 0
                 add extra-passo to spl-riga
                 move 0 to extra-passo
              end-if
              if spl-riga > spl-altezza
                 perform SALTO-PAGINA 
                 set spl-stringa   to true
                 move 0 to spl-riga
              end-if
              move line-riga   to spl-riga-stampa
              call "spooler" using spooler-link

           end-perform.  

      ***---
       SALTO-PAGINA.
           set spl-salto-pagina to true.
           call "spooler" using spooler-link.
           set spl-stringa to true.
           |Mi riposiziono ad inizio foglio
           move 0,8       to spl-riga.  
           add spl-passo  to spl-riga.
           move spaces    to spl-riga-stampa.
           call "spooler" using spooler-link.

      ***---
       CHIUDI-STAMPA.
           set spl-chiusura to true.
           call   "spooler" using spooler-link.
           cancel "spooler".

      ***---
       CLOSE-FILES.
           close lineseq.
           
      ***---
       EXIT-PGM.           
           destroy CourierNew7.
           destroy CourierNew9.
           destroy CourierNew10.
           call "W$BITMAP" using wbitmap-destroy, BitmapSfondoHandle.

           cancel "spooler".
           goback.      
           
      ***---
       CARICA-FONT.
           initialize wfont-data font-handle.
           move dim-crt to wfont-size.
           move "Courier New"        to wfont-name.
           set  wfcharset-dont-care  to true.
           if splcrt2graf-si-grasssetto
              set  wfont-bold           to true
           else
              set  wfont-bold           to false
           end-if.
           set  wfont-italic         to false.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, font-handle, wfont-data
                        giving WFONT-STATUS.

      ****** Courier New 7
      *****     initialize wfont-data CourierNew7.
      *****     move 7 to wfont-size.
      *****     move "Courier New"        to wfont-name.
      *****     set  wfcharset-dont-care  to true.
      *****     set  wfont-bold           to true.
      *****     set  wfont-italic         to false.
      *****     set  wfont-underline      to false.
      *****     set  wfont-strikeout      to false.
      *****     set  wfont-fixed-pitch    to false.
      *****     move 0                    to wfont-char-set.
      *****     set  wfdevice-win-printer to true. |E' un carattere per la stampante
      *****     call "W$FONT" using wfont-get-font, CourierNew7, wfont-data
      *****                  giving WFONT-STATUS.
      *****    
      ****** Courier New 6
      *****     initialize wfont-data CourierNew6.
      *****     move 6 to wfont-size.
      *****     move "Courier New"        to wfont-name.
      *****     set  wfcharset-dont-care  to true.
      *****     set  wfont-bold           to true.
      *****     set  wfont-italic         to false.
      *****     set  wfont-underline      to false.
      *****     set  wfont-strikeout      to false.
      *****     set  wfont-fixed-pitch    to false.
      *****     move 0                    to wfont-char-set.
      *****     set  wfdevice-win-printer to true. |E' un carattere per la stampante
      *****     call "W$FONT" using wfont-get-font, CourierNew6, wfont-data
      *****                  giving WFONT-STATUS.               
      *****
      ****** Courier New 10
      *****     initialize wfont-data CourierNew10.
      *****     move 10 to wfont-size.
      *****     move "Courier New"        to wfont-name.
      *****     set  wfcharset-dont-care  to true.
      *****     set  wfont-bold           to false.
      *****     set  wfont-italic         to false.
      *****     set  wfont-underline      to false.
      *****     set  wfont-strikeout      to false.
      *****     set  wfont-fixed-pitch    to false.
      *****     move 0                    to wfont-char-set.
      *****     set  wfdevice-win-printer to true. |E' un carattere per la stampante
      *****     call "W$FONT" using wfont-get-font, CourierNew10, wfont-data
      *****                  giving WFONT-STATUS.
