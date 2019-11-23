      *NON TOCCARE!!!! USATO SUL LAB PER I TAGLI!!!! USATO SOLAMENTE IN GORDC
      *G
      *D
      *\
       SELECT int-tordini
           ASSIGN       TO  "int-tordini"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL
           FILE STATUS  IS STATUS-int-tordini
           RECORD KEY   IS int-tor-chiave
           ALTERNATE RECORD KEY IS int-k-causale = int-tor-causale, 
           int-tor-anno, int-tor-numero
           ALTERNATE RECORD KEY IS int-k1 = int-tor-cod-cli, 
           int-tor-prg-destino, int-tor-anno, int-tor-numero
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS int-k2 = 
           int-tor-data-passaggio-ordine, int-tor-anno, int-tor-numero
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS int-k-bolla = int-tor-anno-bolla, 
           int-tor-num-bolla
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS int-k3 = int-tor-anno-bolla, 
           int-tor-data-bolla, int-tor-num-bolla, 
           int-tor-bolla-prenotata
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS int-k-fattura = 
           int-tor-anno-fattura, int-tor-num-fattura
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS int-k4 = int-tor-anno-fattura, 
           int-tor-data-fattura, int-tor-num-fattura, 
           int-tor-num-prenot, int-tor-fatt-prenotata
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS int-k-contab = int-tor-agg-contab, 
           int-tor-anno-fattura, int-tor-num-fattura
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS int-k-tipo = int-tor-tipo, 
           int-tor-chiave
           ALTERNATE RECORD KEY IS int-k-data = int-tor-data-creazione, 
           int-tor-numero
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS int-k-agfatt = int-tor-anno-fattura, 
           int-tor-data-fattura, int-tor-num-fattura, 
           int-tor-num-prenot, int-tor-fatt-prenotata, int-tor-chiave
           ALTERNATE RECORD KEY IS int-k-stbolle = int-tor-anno-bolla, 
           int-tor-data-bolla, int-tor-num-bolla, 
           int-tor-bolla-prenotata, int-tor-chiave
           ALTERNATE RECORD KEY IS int-k-andamento-data = 
           int-tor-agg-contab, int-tor-data-fattura
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS int-k-andamento-cliente = 
           int-tor-cod-cli, int-tor-agg-contab, int-tor-data-fattura
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS int-k-andamento-clides = 
           int-tor-cod-cli, int-tor-prg-destino, int-tor-agg-contab, 
           int-tor-data-fattura
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS int-k-promo = int-tor-stato, 
           int-tor-promo, int-tor-data-ordine, int-tor-numero, 
           int-tor-cod-cli, int-tor-prg-destino
           WITH DUPLICATES .
