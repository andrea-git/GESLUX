       SELECT tmp-promo-art
           ASSIGN       TO  path-tmp-promo-art
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-promo-art
           RECORD KEY   IS key01 = tpa-codice, tpa-col-art
           ALTERNATE RECORD KEY IS k-ord = tpa-col-des, 
           tpa-col-ini-dpo, tpa-col-fine-dpo
           WITH DUPLICATES .
