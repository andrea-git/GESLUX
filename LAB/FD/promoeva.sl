      ** SOS per evasione
       SELECT promoeva
           ASSIGN       TO  "promoeva"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-promoeva
           RECORD KEY   IS pev-chiave OF promoeva
           ALTERNATE RECORD KEY IS pev-k-art = pev-articolo OF 
           promoeva, pev-tpr-codice OF promoeva
           ALTERNATE RECORD KEY IS pev-k-descr-art = pev-descr-art OF 
           promoeva, pev-chiave OF promoeva
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS pev-k-gdo = pev-gdo OF promoeva, 
           pev-descr-art OF promoeva, pev-chiave OF promoeva
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS pev-k-fittizia = pev-fittizia OF 
           promoeva, pev-chiave OF promoeva
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS pev-k-promo = pev-tpr-codice OF 
           promoeva, pev-chiave OF promoeva
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS pev-k-descr-promo = 
           pev-tpr-descrizione OF promoeva, pev-chiave OF promoeva
           WITH DUPLICATES .
