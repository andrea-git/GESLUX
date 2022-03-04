       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      imp-fido-ftp-p.
       AUTHOR.                          Andrea.
       REMARKS.

      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.                 

       SELECT iniFtp
           ASSIGN       TO  iniFtpPath
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE  IS SEQUENTIAL
           FILE STATUS  IS STATUS-iniFtp.

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.                        
       FD  iniFtp.
       01 iniFtp-riga        PIC  x(1000). 

       WORKING-STORAGE SECTION.                                     
       77  como-data             pic 9(8).                        
       77  como-ora              pic 9(8).

       77  status-iniFtp         pic xx.    
       77  iniFtpPath            pic x(256).
       77  ini-path-backup       pic x(256).
       77  cmd                   pic x(200).
                            
       77  ftpGetCommand         pic x(256).
       77  pathWinSCP            pic x(256).
       77  path-import           pic x(256).
       77  pathWinSCPLog         pic x(256).

       01  ftp-import.
         03 ftp-server      pic x(50).
         03 ftp-port        pic x(4).
         03 ftp-user        pic x(100).
         03 ftp-password    pic x(100).
         03 ftp-remote-dir  pic x(100).  

      ******************************************************************
       PROCEDURE DIVISION.

      ***---
       MAIN-PRG.                          
           accept como-data from century-date. 
           accept como-ora  from time. 
           accept  path-import from environment "IMP_FIDO_PATH".

           accept  iniFtpPath from environment "WINSCP_INI". 
           inspect iniFtpPath replacing trailing spaces by low-value.
           string  iniFtpPath   delimited low-value
                   "getFTP.ini" delimited size
              into iniFtpPath 
           end-string.
           inspect iniFtpPath replacing trailing low-value by spaces.

           open output iniFtp.  
      
           accept ftp-server
                  from environment "WINSCP_SERVER"
           accept ftp-port
                  from environment "WINSCP_PORT"
           accept ftp-user
                  from environment "WINSCP_USER"
           accept ftp-password
                  from environment "WINSCP_PASSWORD"
           accept ftp-remote-dir
                  from environment "IMP_FIDO_FTP_REMOTE_DIR"
      
           inspect ftp-server     replacing trailing spaces by low-value
           inspect ftp-user       replacing trailing spaces by low-value
           inspect ftp-port       replacing trailing spaces by low-value
           inspect ftp-password   replacing trailing spaces by low-value
           inspect ftp-remote-dir replacing trailing spaces by low-value
                                
           initialize iniFtp-riga.
           string "open ftp://" delimited size
                  ftp-user      delimited low-value
                  ":"           delimited size
                  ftp-password  delimited low-value
                  "@"           delimited size
                  ftp-server    delimited low-value
                  ":"           delimited size
                  ftp-port      delimited low-value
                  " -explicit"  delimited size
             into iniFtp-riga
           end-string.
           write iniFtp-riga.
                             
           initialize iniFtp-riga.
           string "get "         delimited size
                  ftp-remote-dir delimited low-value
                  "PMITRADE_"    delimited size
                  como-data      delimited size
                  ".cs* "        delimited size
                  path-import    delimited size
             into iniFtp-riga
           end-string.
           write iniFtp-riga.
      
           move "exit" to iniFtp-riga.
           write iniFtp-riga.
      
           close iniFtp.        
      
           initialize ftpGetCommand.
           accept  pathWinSCP    from environment "WINSCP_PATH".
           accept  pathWinSCPLog from environment "WINSCP_LOG".
           inspect pathWinSCP    replacing trailing spaces by low-value.
           inspect iniFtpPath    replacing trailing spaces by low-value.
           inspect pathWinSCPLog replacing trailing spaces by low-value.

           string  pathWinSCP    delimited low-value
                   " /script="   delimited size
                   iniFtpPath    delimited low-value
                   " /log="      delimited size
                   pathWinSCPLog delimited low-value
                   "getFtp"      delimited size
                   "_"           delimited size
                   como-data     delimited size
                   "-"           delimited size
                   como-ora      delimited size
                   ".log"        delimited size
              into ftpGetCommand
           end-string.
           call "C$SYSTEM" using ftpGetCommand, 255.
                  
           accept  ini-path-backup from environment "WINSCP_INI_BACKUP". 
           inspect ini-path-backup 
                   replacing trailing spaces by low-value
           string  ini-path-backup delimited low-value
                   "getFTP"        delimited size
                   "_"             delimited size
                   como-data       delimited size
                   "-"             delimited size
                   como-ora        delimited size
                   ".ini"          delimited size
              into ini-path-backup
           end-string.
           inspect ini-path-backup 
                   replacing trailing low-value by spaces. 

           inspect iniFtpPath replacing trailing spaces by low-value.
           initialize cmd.
           string "copy "         delimited size
                  iniFtpPath      delimited low-value
                  " "             delimited size
                  ini-path-backup delimited size
                  into cmd
           end-string.
           call "C$SYSTEM" using cmd, 255.

           goback.
