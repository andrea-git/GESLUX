       SELECT OLD-STO-rmovmag
           ASSIGN       TO path-OLD-STO-rmovmag
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL WITH ROLLBACK 
           FILE STATUS  IS STATUS-OLD-STO-rmovmag
           RECORD KEY   IS OLD-STO-rmo-chiave OF OLD-STO-rmovmag
           ALTERNATE RECORD KEY IS k-articolo of OLD-STO-rmovmag = 
           OLD-STO-rmo-anno OF OLD-STO-rmovmag, 
           OLD-STO-rmo-movim OF OLD-STO-rmovmag, 
           OLD-STO-rmo-articolo OF OLD-STO-rmovmag
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS 
           OLD-STO-rmo-chiave-ricerca of OLD-STO-rmovmag = 
           OLD-STO-rmo-tipo OF OLD-STO-rmovmag, 
           OLD-STO-rmo-cod-clifor OF OLD-STO-rmovmag,
           OLD-STO-rmo-causale OF OLD-STO-rmovmag, 
           OLD-STO-rmo-articolo OF OLD-STO-rmovmag, 
           OLD-STO-rmo-data-movim OF OLD-STO-rmovmag
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-progmag of OLD-STO-rmovmag = 
           OLD-STO-rmo-chiave-progmag OF OLD-STO-rmovmag
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-art-data of OLD-STO-rmovmag = 
           OLD-STO-rmo-articolo OF OLD-STO-rmovmag, 
           OLD-STO-rmo-data-movim OF OLD-STO-rmovmag
           WITH DUPLICATES .
