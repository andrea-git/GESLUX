      ** Usato per i log sulle evasioni prima e dopo le opreazioni di modifica e cancellazione e su ricalimp-art
       SELECT log-progmag
           ASSIGN       TO DISK path-log-progmag
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE  IS SEQUENTIAL
           FILE STATUS  IS STATUS-log-progmag.
