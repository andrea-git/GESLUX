       SELECT OLD-rmovmag
           ASSIGN       TO  "OLD-rmovmag"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL WITH ROLLBACK 
           FILE STATUS  IS STATUS-OLD-rmovmag
           RECORD KEY   IS OLD-rmo-chiave OF OLD-rmovmag
           ALTERNATE RECORD KEY IS k-articolo of OLD-rmovmag = 
           OLD-rmo-anno OF 
           OLD-rmovmag, OLD-rmo-movim OF OLD-rmovmag, 
           OLD-rmo-articolo OF OLD-rmovmag
           WITH DUPLICATES 
           ALTERNATE RECORD KEY OLD-rmo-chiave-ricerca of OLD-rmovmag = 
           OLD-rmo-tipo OF OLD-rmovmag, 
           OLD-rmo-cod-clifor OF OLD-rmovmag, OLD-rmo-causale 
           OF OLD-rmovmag, OLD-rmo-articolo OF OLD-rmovmag, 
           OLD-rmo-data-movim OF 
           OLD-rmovmag
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-progmag of OLD-rmovmag = 
           OLD-rmo-chiave-progmag OF OLD-rmovmag
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-art-data of OLD-rmovmag = 
           OLD-rmo-articolo 
           OF OLD-rmovmag, OLD-rmo-data-movim OF OLD-rmovmag
           WITH DUPLICATES .
