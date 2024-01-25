       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      check-prg-edi-wrap.
       AUTHOR.                          Andrea.
       REMARKS. Per chiamare il controllo e mandare la mail
      ********************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *****************************************************************
       WORKING-STORAGE SECTION.
       
       PROCEDURE DIVISION.
           set environment "CHECK_PRG_EDI_MAIL" to "S".
           call   "check-prg-edi".
           cancel "check-prg-edi".                     
           set environment "CHECK_PRG_EDI_MAIL" to " ".
           goback.
