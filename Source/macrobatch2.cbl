       program-id.                      macrobatch2.
       author.                          Andrea.
       remarks. 
           PASSANDO DA MACROBATCH (per tenere tutti i controlli attivi):
           - CHIUSURA SERVIZIO
           - GENERAZIONE EVASIONI(da macrobatch)
           - RIAPERTURA SERVIZIO

       special-names. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      *****************************************************************
       DATA DIVISION.
       FILE SECTION. 

       WORKING-STORAGE SECTION.  
                          
       PROCEDURE DIVISION.

       MAIN.         
           set environment "MACROBATCH" to "2". |per utilizzo divisioni (tutte anzich� auto) in evasione clienti
           call   "macrobatch" using "X".
           cancel "macrobatch".
           goback.
