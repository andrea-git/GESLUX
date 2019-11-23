      *E' il file coinvolto nell'aleborazione notturna "calmar" per il calcolo del margine da inizio anno al giorno stesso.
      * 
       SELECT OLD-progmagric
           ASSIGN       TO  "OLD-progmagric"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-OLD-progmagric
           RECORD KEY   IS OLD-prr-chiave OF OLD-progmagric.
