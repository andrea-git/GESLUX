      ***---
       PREPARA-PRODENER.
           initialize xzoom-linkage xzoom-ext-info(1).

           evaluate como-file
           when "prodener"
      *          move  0 to xzoom-file-key
                move  2 to xzoom-file-key
           when "prodener-alfa"  
                move  1 to xzoom-file-key
           end-evaluate.

           move  zero to idx.
           move  zero                    to xzoom-row.
           move  zero                    to xzoom-cln.
           move  16                      to xzoom-lw.
           move  105                     to xzoom-sw.
           move "prodener"               to xzoom-file-name(1).
           move  5                       to xzoom-fields.

      * CAMPO 1
           add 1 to idx
           move  10                      to xzoom-field-length(idx).
           move  0                       to xzoom-field-offset(idx).
           move  11                      to xzoom-field-column(idx).
           move "Codice"                 to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

      * CAMPO 2
           add 1 to idx
           move  50                      to xzoom-field-length(idx).
           move  10                      to xzoom-field-offset(idx).
           move  40                      to xzoom-field-column(idx).
           move "Descrizione"            to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

           add 1 to idx
           move  4                       to xzoom-field-length(idx).
           move  60                      to xzoom-field-offset(idx).
           move  6                       to xzoom-field-column(idx).
           move "CPA"                    to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 


           add   1                          to Idx
           move  8                          to xzoom-field-length(Idx)
           move  64                         to xzoom-field-offset(Idx)
           move "NC"                        to xzoom-field-name(Idx)
           move 8                           to xzoom-field-column(Idx)
           move 8                           to xzoom-field-digits(Idx)
           move 0                           to xzoom-field-dec(Idx)
           move "########"                  to xzoom-field-fmt(Idx)
           set xzoom-al-right(Idx)          to true
           set xzoom-field-unsigned(Idx )   to true
           set xzoom-ft-display(Idx)        to true


           add   1                          to Idx
           move  2                          to xzoom-field-length(Idx)
           move  72                         to xzoom-field-offset(Idx)
           move "Taric"                     to xzoom-field-name(Idx)
           move 8                           to xzoom-field-column(Idx)
           move 2                           to xzoom-field-digits(Idx)
           move 0                           to xzoom-field-dec(Idx)
           move "##"                        to xzoom-field-fmt(Idx)
           set xzoom-al-right(Idx)          to true
           set xzoom-field-unsigned(Idx )   to true
           set xzoom-ft-display(Idx)        to true


           add 1 to idx
           move  4                       to xzoom-field-length(idx).
           move  74                      to xzoom-field-offset(idx).
           move  6                       to xzoom-field-column(idx).
           move "DAC"                    to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 


           move  idx                        to xzoom-fields.

           move  -1                     to xzoom-delimiter-offset.
           move  5                      to xzoom-delimiter-length.
           move "000"                   to xzoom-from-value.
           move "000"                   to xzoom-to-value.

