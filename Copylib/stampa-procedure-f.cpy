      ***---
       STAMPA-FORNITORI.
           initialize stampa-linkage.
           inquire chk-excel, value in stampa-tipo.
           move user-codi        to stampa-user.
           move ef-cod-da-buf    to stampa-codice-da.
           move ef-cod-a-buf     to stampa-codice-a.
           move ef-st-prov-buf   to stampa-tipo-cli.
           move ef-st-naz-buf    to stampa-cod-gdo.
           call   "W$MOUSE"   using set-mouse-shape, wait-pointer.
           call   "stfornitori" using stampa-linkage.
           cancel "stfornitori".
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
       STAMPA-DESTINI-F.
           initialize stampa-linkage.
           inquire chk-excel-2, value in stampa-tipo.
           move user-codi         to stampa-user.
           move ef-cod-da-buf     to stampa-codice-da.
           move ef-cod-a-buf      to stampa-codice-a.
           move ef-des-da-buf     to stampa-destino-da.
           move ef-des-a-buf      to stampa-destino-a.
           move ef-st-prov-buf    to stampa-tipo-cli.
           move ef-st-naz-buf     to stampa-cod-gdo.
           call   "W$MOUSE"    using set-mouse-shape, wait-pointer.
           call   "stdestinif" using stampa-linkage.
           cancel "stdestinif".
           call   "W$MOUSE"    using set-mouse-shape, arrow-pointer.

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
       STAMPA-NOTE-F.
           initialize stampa-linkage.
           inquire chk-excel-3, value in stampa-tipo.
           move user-codi        to stampa-user.
           move ef-cod-da-buf    to stampa-codice-da.
           move ef-cod-a-buf     to stampa-codice-a.
           move ef-st-prov-buf   to stampa-tipo-cli.
           move ef-st-naz-buf    to stampa-cod-gdo.
           call   "W$MOUSE"   using set-mouse-shape, wait-pointer.
           call   "stnotef"   using stampa-linkage.
           cancel "stnotef".
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
