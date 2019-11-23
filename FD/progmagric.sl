      *E' il file coinvolto nell'aleborazione notturna "calmar" per il calcolo del margine da inizio anno al giorno stesso.
      * 
       SELECT progmagric
           ASSIGN       TO  "progmagric"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-progmagric
           RECORD KEY   IS prr-chiave OF progmagric.
