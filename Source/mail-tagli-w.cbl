       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      mail-tagli-w.
       AUTHOR.                          Andrea.
       REMARKS. Batch richiamo mail-tagli e mail-tagli2 (divisione 17)
      ******************************************************************
                                                                              
       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.

       LINKAGE SECTION.
           copy "link-batch.def".

      ******************************************************************
       PROCEDURE DIVISION USING batch-linkage.
      ******************************************************************

      ***---
       MAIN-PRG.               
           call   "mail-tagli" using batch-linkage.
           cancel "mail-tagli".
           call   "mail-tagli2" using batch-linkage.
           cancel "mail-tagli2".

           goback.
