       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      pdfdoc.
       AUTHOR.                          Andrea.

       WORKING-STORAGE SECTION.     
       77  nargs                 pic 99  comp-1 value 0.
       77  status-pgm            pic s9.

       LINKAGE SECTION.
       copy "link-batch.def".

      ******************************************************************
       PROCEDURE DIVISION USING batch-linkage.

       MAIN-PRG.      
           call "C$NARG" using nargs.
           perform CHIAMA-PGM.
           if status-pgm = -1 
              perform CHIAMA-PGM
              if status-pgm = -1 
                 perform CHIAMA-PGM
              end-if
           end-if.          

           goback.  

      ***---
       CHIAMA-PGM.
           if nargs not = 0
              call "pdfdoc-p" using status-pgm, batch-linkage
              move status-pgm to batch-status
           else                
              call "pdfdoc-p" using status-pgm
           end-if.
           cancel "pdfdoc-p".
