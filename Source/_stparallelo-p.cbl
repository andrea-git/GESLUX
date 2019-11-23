       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      stparallelo-p.
       AUTHOR.                          Andrea.
       REMARKS. Stampa le bolle con il layout grafico da stampare al di sotto
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *     copy "lineseq.sl". 
           copy "STA-splcrt2graf.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.          
      *     copy "lineseq.fd".        
           copy "STA-splcrt2graf.fd".

       WORKING-STORAGE SECTION. 
           copy "acugui.def".
           copy "spooler.def".
           copy "fonts.def".     
                                   
       77  status-fd1            pic xx.
       77  wstampa               pic x(256).
       77  status-lineseq        pic xx.     
       77  nome-stampante        pic x(200).
       77  path-bmp              pic x(200).
       77  CourierNew7           handle of font.
       77  CourierNew6           handle of font.
       77  CourierNew9           handle of font.
       77  CourierNew10          handle of font.
       77  BitmapHandle    pic S9(9) comp-4.
       77  WFONT-STATUS          pic s9(5) value ZERO.
       77  cont pic 99.
       77  extra-dim pic 9.
       77  extra-passo pic s9(5)v999.
       77  font-handle   handle of font.
       77  dim-crt pic 99.

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
      *     move splcrt2graf-percorso-stampa to wstampa.
      *     open input lineseq.
           
      ***---
       ELABORAZIONE.          
           move splcrt2graf-dim-crt to dim-crt.

                      initialize spooler-link.
           move "splcrt2graf" to spl-nome-job.|?????????

           accept spl-nome-stampante 
                  from environment "STAMPANTE_STBOLLE".

           set spl-apertura to true.
           move 1  to spl-num-copie.
           set spl-vertical   to true.

           move 1,5    to spl-margine-inf.
           move 0,5    to spl-margine-destro
                          spl-margine-sinistro.

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
           move 0,8          to spl-riga.
           move 1            to spl-tipo-colonna.

           evaluate true
           when splcrt2graf-windows
                move splcrt2graf-percorso-stampa     to WSTAMPA
           when splcrt2graf-unix
                move splcrt2graf-percorso-stampa-u   to WSTAMPA
           end-evaluate.

           open input STA-splcrt2graf.                               

      *     accept path-bmp from environment "PATH_BMP_FATTURE"
      *     inspect path-bmp 
      *             replacing trailing spaces by low-value
      *     string path-bmp delimited low-value
      *            "sfondo_fatture.bmp"   delimited size
      *        into path-bmp 
      *     end-string   
      *     call "W$BITMAP" using WBITMAP-LOAD, path-bmp,
      *                    giving BitmapHandle.

      *                 accept path-bmp from environment "PATH_BOLLE_PERS"  
      *                                        
      *                 initialize BitmapHandle
      *                 call "W$BITMAP" using WBITMAP-LOAD, path-bmp,
      *                                giving BitmapHandle 
      *                 move BitmapHandle to spl-hbitmap   

           if STATUS-FD1 = "00"
              move 0 to extra-dim
              perform until 1 = 2
                 initialize splcrt2graf-rigo
                 read STA-splcrt2graf at end exit perform end-read
                 initialize spl-riga-stampa
                 initialize cont
                 if splcrt2graf-rigo(1:1) = "@" 
                    if splcrt2graf-rigo(2:3) = "BMP"          
                    move splcrt2graf-rigo(5:) to path-bmp
                    call "W$BITMAP" using WBITMAP-LOAD, path-bmp,
                                   giving BitmapHandle
                    set spl-bitmap to true
                    move 1,5 to spl-colonna
                    move 3 to spl-riga
                    move BitmapHandle   to spl-hbitmap
                    move 26,8 to spl-bitmap-height
                    move 19,5 to spl-bitmap-width
                    call "spooler" using spooler-link
                    move 1 to spl-riga
      *                 move spl-riga    to save-spl-riga
      *                 move spl-colonna to save-spl-colonna     
      *****                 move splcrt2graf-rigo(5:) to path-bmp
      *****                 initialize BitmapHandle
      *****                 call "W$BITMAP" using WBITMAP-LOAD, path-bmp,
      *****                                giving BitmapHandle     
      ******                 
      *****                 set spl-bitmap to true      
      *****                 
      *****                 move BitmapHandle to spl-hbitmap   
      *****                 move 8,0 to spl-colonna
      *****                 move 8,0 to spl-riga      
      *****                 move 28,5 to spl-bitmap-height
      *****                 move 21,7 to spl-bitmap-width
      *****                 move 8,0 to spl-bitmap-height
      *****                 move 8,0 to spl-bitmap-width
      *****                 call "spooler" using spooler-link                   
      *****                 set spl-stringa to true                             
      *                 move save-spl-riga    to spl-riga
      *                 move save-spl-colonna to spl-colonna
      
      *                 initialize BitmapHandle
      *                 move splcrt2graf-rigo(5:) to path-bmp
      *                 call "W$BITMAP" using WBITMAP-LOAD, path-bmp,
      *                                giving BitmapHandle
      *                 move BitmapHandle to spl-hbitmap
      *                 move 1,0 to spl-colonna
      *                 move 0,3 to spl-riga      
      *                 move 4,5 to spl-bitmap-height
      *                 move 21,7 to spl-bitmap-width
      *                 call "spooler" using spooler-link
      *                 call "W$BITMAP" using WBITMAP-DESTROY, 
      *                                       BitmapHandle  
      *                 move save-spl-riga    to spl-riga
      *                 move save-spl-colonna to spl-colonna

                    else
                       evaluate splcrt2graf-rigo(2:1)
                       when "+"
                            move splcrt2graf-rigo(3:1) to extra-dim  
                            add extra-dim to dim-crt
                            perform CARICA-FONT   
                            move font-handle  to spl-hfont
                       when "-"
                            move splcrt2graf-rigo(3:1) to extra-dim  
                            subtract extra-dim from dim-crt
                            perform CARICA-FONT   
                            move font-handle  to spl-hfont
                       when ">" 
                            move splcrt2graf-rigo(3:4) to extra-passo 
                            divide extra-passo by 100 giving extra-passo
                       when "<" 
                            move splcrt2graf-rigo(3:4) to extra-passo 
                            divide extra-passo by 100 giving extra-passo
                            compute extra-passo = extra-passo * - 1
                       when "B"
                            if splcrt2graf-rigo(3:1) = "+"
                               set splcrt2graf-si-grasssetto to true
                            else                            
                               set splcrt2graf-si-grasssetto to false
                            end-if
                            perform CARICA-FONT
                            move font-handle to spl-hfont
                       end-evaluate            
                    end-if                     
                    exit perform cycle
                 end-if     
                    
                 inspect splcrt2graf-rigo tallying cont for all x"09"

                 if cont = 0          
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
                    move splcrt2graf-rigo   to spl-riga-stampa
                    call "spooler" using spooler-link
                 else
                    perform SALTO-PAGINA     
                 end-if
      *           if extra-dim > 0
      *              move 0 to extra-dim
      *              move splcrt2graf-dim-crt to dim-crt
      *              perform CARICA-FONT   
      *              move font-handle  to spl-hfont
      *           end-if
              end-perform

              close sta-splcrt2graf     
      *                 call "W$BITMAP" using WBITMAP-DESTROY, BitmapHandle 
           end-if.

           set spl-chiusura to true.
           call   "spooler" using spooler-link.
           cancel "spooler".
           initialize spooler-link.

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
      *****     move 1,7    to spl-riga.     
      *****
      *****
      *****
      *****
      *****           
      *****     perform until 1 = 2
      *****        read lineseq next at end exit perform end-read
      *****        if line-riga(1:1) = "@"             
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
      *****           if line-riga(2:3) = "BMP"                  
      *****              move line-riga(5:) to path-bmp
      *****              call "W$BITMAP" using WBITMAP-LOAD, path-bmp,
      *****                             giving BitmapSfondoHandle
      *****              set spl-bitmap to true
      *****              move 1,5 to spl-colonna
      *****              move 3 to spl-riga
      *****              move BitmapSfondoHandle   to spl-hbitmap
      *****              move 26,8 to spl-bitmap-height
      *****              move 19,5 to spl-bitmap-width
      *****              call "spooler" using spooler-link
      *****              move 7 to spl-riga
      *****           end-if
      *****           exit perform cycle
      *****        end-if
      *****        set spl-stringa to true
      *****        add 0,4 to spl-riga
      *****        move line-riga to spl-riga-stampa
      *****        call "spooler" using spooler-link
      *****     end-perform.

      ***---
       CHIUDI-STAMPA.
           set spl-chiusura to true.
           call   "spooler" using spooler-link.
           cancel "spooler".

      ***---
       CLOSE-FILES.
      *     close lineseq.
           
      ***---
       EXIT-PGM.           
           destroy CourierNew7.
           destroy CourierNew9.
           destroy CourierNew10.
      *     call "W$BITMAP" using wbitmap-destroy, BitmapSfondoHandle.

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
