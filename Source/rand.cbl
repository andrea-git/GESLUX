       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      rand.

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.                          

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.         

       WORKING-STORAGE SECTION.
       77  n   pic 99.
       78  val value 10.        
       01 ListOfElements.
          05 Element OCCURS 10 TIMES.
             10 ElementValue PIC X(20).
       01 NumElements PIC 9(2) VALUE 10.
       01 RandomIndex PIC 9(2).
       01 SelectedElementValue PIC X(20).
       

      ******************************************************************
       PROCEDURE DIVISION.
       MAIN.
           perform 2000 times
              compute n = function random * (val) |da 0 a [val -1]
              display n
           end-perform.
           stop "K"
           
           MOVE "Elemento 1" TO Element(1).
           MOVE "Elemento 2" TO Element(2).
           MOVE "Elemento 3" TO Element(3).
           MOVE "Elemento 4" TO Element(4).
           MOVE "Elemento 5" TO Element(5).
           MOVE "Elemento 6" TO Element(6).
           MOVE "Elemento 7" TO Element(7).
           MOVE "Elemento 8" TO Element(8).
           MOVE "Elemento 9" TO Element(9).
           MOVE "Elemento 10" TO Element(10).

           compute RandomIndex = FUNCTION RANDOM * (NumElements).

           MOVE Element(RandomIndex) TO SelectedElementValue

           STOP "J".

           goback.
