       SELECT OPTIONAL PAR
           ASSIGN       TO RANDOM "PAR"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-PAR
           RECORD KEY   IS PAR-CODICE
           ALTERNATE RECORD KEY IS PAR-CODICE1 = PAR-CODICE-PNR, 
           PAR-CODICE
           ALTERNATE RECORD KEY IS PAR-CODICE2 = PAR-CODICE-PAS, 
           PAR-CODICE.
