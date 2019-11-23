      ***---
       STAMPA-CLIENTI.
           initialize stampa-linkage.
           inquire chk-excel, value in stampa-tipo.
           move user-codi        to stampa-user.
           move ef-cod-da-buf    to stampa-codice-da.
           move ef-cod-a-buf     to stampa-codice-a.
           move ef-st-tipo-buf   to stampa-tipo-cli.
           move ef-st-gdo-buf    to stampa-cod-gdo.
           move ef-st-age-buf    to stampa-agente.
           call   "W$MOUSE"   using set-mouse-shape, wait-pointer.
           call   "stclienti" using stampa-linkage.
           cancel "stclienti".
           call   "W$MOUSE"   using set-mouse-shape, arrow-pointer.

           if not StampaExcel
              initialize file-info
          
              call "C$FILEINFO" using stampa-path, file-info
      
              if file-size not = 0
                 move tope-stampa to splcrt2graf-operazione
                 set  splcrt2graf-windows       to true
                 move stampa-path
                   to splcrt2graf-percorso-stampa
                 set  splcrt2graf-verticale   to true
                 set  splcrt2graf-forza-crt   to true
                 set  splcrt2graf-10pt        to true
                 call   "splcrt2graf" using splcrt2graf-link 
                 cancel "splcrt2graf"
              end-if

              call "C$DELETE"  using stampa-path
           end-if.

      ***---
       STAMPA-DESTINI.
           initialize stampa-linkage.
           inquire chk-excel-2, value in stampa-tipo.
           move user-codi        to stampa-user.
           move ef-cod-da-buf    to stampa-codice-da.
           move ef-cod-a-buf     to stampa-codice-a.
           move ef-des-da-buf    to stampa-destino-da.
           move ef-des-a-buf     to stampa-destino-a.
           move ef-st-tipo-buf   to stampa-tipo-cli.
           move ef-st-gdo-buf    to stampa-cod-gdo.
           move ef-st-age-buf    to stampa-agente.
           call   "W$MOUSE"   using set-mouse-shape, wait-pointer.
           call   "stdestini" using stampa-linkage.
           cancel "stdestini".
           call   "W$MOUSE"   using set-mouse-shape, arrow-pointer.

           if not StampaExcel
           
              initialize file-info
          
              call "C$FILEINFO" using stampa-path, file-info
      
              if file-size not = 0
                 move tope-stampa to splcrt2graf-operazione
                 set  splcrt2graf-windows       to true
                 move stampa-path
                   to splcrt2graf-percorso-stampa
                 set  splcrt2graf-verticale   to true
                 set  splcrt2graf-forza-crt   to true
                 set  splcrt2graf-10pt        to true
                 call   "splcrt2graf" using splcrt2graf-link 
                 cancel "splcrt2graf"
              end-if

              call "C$DELETE"  using stampa-path
           end-if.

      ***---
       STAMPA-NOTE.
           initialize stampa-linkage.
           inquire chk-excel-3, value in stampa-tipo.
           move user-codi        to stampa-user.
           move ef-cod-da-buf    to stampa-codice-da.
           move ef-cod-a-buf     to stampa-codice-a.
           move ef-st-tipo-buf   to stampa-tipo-cli.
           move ef-st-gdo-buf    to stampa-cod-gdo.
           move ef-st-age-buf    to stampa-agente.
           call   "W$MOUSE"   using set-mouse-shape, wait-pointer.
           call   "stnote"    using stampa-linkage.
           cancel "stnote".
           call   "W$MOUSE"   using set-mouse-shape, arrow-pointer.

           if not StampaExcel
           
              initialize file-info
          
              call "C$FILEINFO" using stampa-path, file-info

              if file-size not = 0
                 move tope-stampa to splcrt2graf-operazione
                 set  splcrt2graf-windows       to true
                 move stampa-path
                   to splcrt2graf-percorso-stampa
                 set  splcrt2graf-verticale   to true
                 set  splcrt2graf-forza-crt   to true
                 set  splcrt2graf-10pt        to true
                 call   "splcrt2graf" using splcrt2graf-link 
                 cancel "splcrt2graf"
              end-if

              call "C$DELETE"  using stampa-path
           end-if.
