       SELECT STO-rmovmag
           ASSIGN       TO  path-sto-rmovmag
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL WITH ROLLBACK 
           FILE STATUS  IS STATUS-sto-rmovmag
           RECORD KEY   IS STO-rmo-chiave of STO-rmovmag
           ALTERNATE RECORD KEY IS k-articolo of STO-rmovmag = 
           STO-rmo-anno of STO-rmovmag, STO-rmo-movim of STO-rmovmag, 
           STO-rmo-articolo of STO-rmovmag
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS STO-rmo-chiave-ricerca of 
           STO-rmovmag = STO-rmo-tipo of STO-rmovmag, 
           STO-rmo-cod-clifor of STO-rmovmag, STO-rmo-causale of 
           STO-rmovmag, STO-rmo-articolo of STO-rmovmag, 
           STO-rmo-data-movim of STO-rmovmag
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-progmag of STO-rmovmag = 
           STO-rmo-chiave-progmag of STO-rmovmag
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-art-data of STO-rmovmag = 
           STO-rmo-articolo of STO-rmovmag, STO-rmo-data-movim of 
           STO-rmovmag
           WITH DUPLICATES .
