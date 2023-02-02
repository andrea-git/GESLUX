       program-id.                      g2aggall-p.
       author.                          Andrea.
       remarks. 
           Riaggiornamento massivo files GESLUX - G2

       special-names. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.             
           copy "clienti.sl". 
           copy "ttipocli.sl".
           copy "tnazioni.sl".
           copy "agenti.sl".                             
LUBEXX     copy "tgrupgdo.sl".                           
           copy "articoli.sl".

           copy "DOCDI.sl".
           copy "CLI.sl".
           copy "FRN.sl".
           copy "TBLNA.sl". |NAZIONI
           copy "TBLCA.sl". |CATEGORIE
           copy "TBLAG.sl". |AGENTI
           copy "TBLME.sl". |GRUPPI MERCEOLOGICI
           copy "TBLCS.sl". |GRUPPI GDO
           copy "FPGRUPPICS.sl". |GRUPPI GDO COLLEGAMENTO
           copy "G2.sl".
           copy "ART.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.             
           copy "clienti.fd". 
           copy "ttipocli.fd".
           copy "tnazioni.fd".
           copy "agenti.fd".                             
LUBEXX     copy "tgrupgdo.fd".                           
           copy "articoli.fd".

           copy "DOCDI.fd".
           copy "CLI.fd".
           copy "FRN.fd".
           copy "TBLNA.fd". |NAZIONI
           copy "TBLCA.fd". |CATEGORIE
           copy "TBLAG.fd". |AGENTI
           copy "TBLME.fd". |GRUPPI MERCEOLOGICI
           copy "TBLCS.fd". |GRUPPI GDO
           copy "FPGRUPPICS.fd". |GRUPPI GDO COLLEGAMENTO
           copy "G2.fd".
           copy "ART.fd".

       WORKING-STORAGE SECTION.  
       77  status-clienti       pic xx. 
       77  status-ttipocli      pic xx.
       77  status-tnazioni      pic xx.
       77  status-agenti        pic xx.                             
LUBEXX 77  status-tgrupgdo      pic xx.                           
       77  status-articoli      pic xx.  

       77  status-TBLNA         pic xx.
       77  status-TBLAG         pic xx.
       77  status-TBLCA         pic xx.
       77  status-TBLME         pic xx.
       77  status-DOCDI         pic xx.
       77  status-CLI           pic xx.
       77  status-FRN           pic xx.
       77  status-G2            pic xx.     
       77  status-art           pic xx.    
       77  status-fpgruppics    pic xx.
       77  status-tblcs         pic xx.
                                       
      * copy "comune.def".   
       copy "link-G2Agg.def".                

       77  a  pic 99v9.
       77  b  pic 99v9.
                                      
       77  counter          pic 9(6).
       77  counter-2        pic 9(6).
       77  counter-edit     pic zz.zz9.

       LINKAGE SECTION.
       77  chk-cli          pic 9. 
       77  chk-frn          pic 9.
       77  chk-art          pic 9.
       77  chk-naz          pic 9.
       77  chk-tip          pic 9.
       77  chk-age          pic 9.
       77  chk-gdo          pic 9.
       77  chk-tut          pic 9.
       77  form1-handle     handle of window.

       PROCEDURE DIVISION USING chk-cli     
                                chk-frn     
                                chk-art     
                                chk-naz     
                                chk-tip     
                                chk-age     
                                chk-gdo     
                                chk-tut     
                                form1-handle.
       

       MAIN.             
           if chk-cli = 1 or chk-tut = 1
              move 0 to counter counter-2
              display "ELABORAZIONE CLIENTI - CLI" 
                  upon form1-handle 
                  line 20,00 
                   col  2,00
              |Elaborazione clienti - CLI                    
              open i-o cli
              move low-value to record-cli
              start cli key >= cli-codice-g2
                    invalid continue
                not invalid
                    perform until 1 = 2
                       read   cli next at end exit perform end-read
                       delete cli record invalid continue end-delete
                    end-perform
              end-start
              close cli
              open input clienti
              move low-value to cli-chiave
              set cli-tipo-c to true
              start clienti key >= cli-chiave
                    invalid continue
                not invalid
                    perform until 1 = 2
                       add 1 to counter counter-2
                       if counter-2 = 100
                          move 0 to counter-2
                          move counter to counter-edit
                          display counter-edit upon form1-handle 
                                               line 20,50
                                                col 26,00
                       end-if
                       read clienti next at end exit perform end-read
                       if cli-tipo-f            exit perform end-if
                       move cli-codice  to G2Agg-codice
                       set G2Agg-cli    to true
                       set G2Agg-insert to true
                       call   "g2agg" using G2Agg-linkage
                       cancel "g2agg"
                    end-perform
              end-start
              close clienti                                         
              display 
           "                                                           "  
                  upon form1-handle line 22,00 col 2,00
           end-if.
                                           
           if chk-frn = 1 or chk-tut = 1
              move 0 to counter counter-2
              display "ELABORAZIONE FORNITORI - FRN" 
                  upon form1-handle 
                  line 20,00 
                   col  2,00
              |Elaborazione clienti - FRN
              open i-o frn
              move low-value to record-frn
              start frn key >= frn-codice
                    invalid continue
                not invalid
                    perform until 1 = 2
                       read   frn next at end exit perform end-read
                       delete frn record invalid continue end-delete
                    end-perform
              end-start
              close frn
              open input clienti
              move low-value to cli-chiave
              set cli-tipo-f to true
              start clienti key >= cli-chiave
                    invalid continue
                not invalid
                    perform until 1 = 2
                       read clienti next at end exit perform end-read 
                       add 1 to counter counter-2
                       if counter-2 = 100
                          move 0 to counter-2
                          move counter to counter-edit
                          display counter-edit upon form1-handle 
                                               line 20,50
                                                col 26,00
                       end-if
                       move cli-codice  to G2Agg-codice
                       set G2Agg-for    to true
                       set G2Agg-insert to true
                       call   "g2agg" using G2Agg-linkage
                       cancel "g2agg"
                    end-perform
              end-start
              close clienti                                         
              display 
           "                                                           "  
                  upon form1-handle line 22,00 col 2,00
           end-if.
                                                              
           if chk-art = 1 or chk-tut = 1
              move 0 to counter counter-2
              display "ELABORAZIONE ARTICOLI - ART"
                  upon form1-handle 
                  line 20,00 
                   col  2,00
              |Elaborazione articoli - ART
              open i-o art
              move low-value to record-art
              start art key >= art-codice-g2
                    invalid continue
                not invalid
                    perform until 1 = 2
                       read   art next at end exit perform end-read 
                       delete art record invalid continue end-delete
                    end-perform
              end-start
              close art
              open input articoli
              move low-value to art-chiave
              start articoli key >= art-chiave
                    invalid continue
                not invalid
                    perform until 1 = 2
                       read articoli next at end exit perform end-read 
                       add 1 to counter counter-2
                       if counter-2 = 100
                          move 0 to counter-2
                          move counter to counter-edit
                          display counter-edit upon form1-handle 
                                               line 20,50
                                                col 26,00
                       end-if
                       move art-codice  to G2Agg-articolo
                       set G2Agg-art    to true
                       set G2Agg-insert to true
                       call   "g2agg" using G2Agg-linkage
                       cancel "g2agg"
                    end-perform
              end-start
              close articoli                                        
              display 
           "                                                           "  
                  upon form1-handle line 22,00 col 2,00
           end-if.
           
                                                              
           if chk-naz = 1 or chk-tut = 1
              move 0 to counter counter-2
              display "ELABORAZIONE NAZIONI - TBLNA"
                  upon form1-handle 
                  line 20,00 
                   col  2,00
              |Elaborazione tnazioni - TBLNA
              open i-o tblna
              move low-value to record-tblna
              move "NA" to tblna-codice1
              start tblna key >= tblna-codice
                    invalid continue
                not invalid
                    perform until 1 = 2
                       read tblna next at end      exit perform end-read
                       if tblna-codice1 not = "NA" exit perform end-if
                       delete tblna record invalid continue end-delete
                    end-perform
              end-start
              close tblna
           
              open input tnazioni 
              move low-value to naz-rec
              start tnazioni key is >= naz-chiave
                    invalid 
                not invalid
                    perform until 1 = 2
                       read tnazioni next at end exit perform end-read  
                       add 1 to counter counter-2
                       if counter-2 = 10
                          move 0 to counter-2
                          move counter to counter-edit
                          display counter-edit upon form1-handle 
                                               line 20,50
                                                col 26,00
                       end-if
                       move naz-codice  to G2Agg-nazione
                       set G2Agg-insert to true
                       set g2AGG-naz    to true
                       call   "G2Agg" using G2Agg-linkage
                       cancel "G2Agg"
                    end-perform
              end-start
              close tnazioni                                        
              display 
           "                                                           "  
                  upon form1-handle line 22,00 col 2,00
           end-if.
           
                                 
           if chk-age = 1 or chk-tut = 1
              move 0 to counter counter-2
              display "ELABORAZIONE AGENTI - TBLAG"
                  upon form1-handle 
                  line 20,00 
                   col  2,00
              |Elaborazione agenti - TBLAG
              open i-o tblag
              move low-value to record-tblag
              move "AG" to tblag-codice1
              start tblag key >= tblag-codice
                    invalid continue
                not invalid
                    perform until 1 = 2
                       read tblag next      at end exit perform end-read
                       if tblag-codice1 not = "AG" exit perform end-if
                       delete tblag record invalid continue end-delete
                    end-perform
              end-start
              close tblag
           
              open input agenti
              move low-value to age-rec
              start agenti key is >= age-chiave
                    invalid continue
                not invalid
                    perform until 1 = 2
                       read agenti next at end exit perform end-read 
                       add 1 to counter counter-2
                       if counter-2 = 10
                          move 0 to counter-2
                          move counter to counter-edit
                          display counter-edit upon form1-handle 
                                               line 20,50
                                                col 26,00
                       end-if
                       move age-codice  to G2Agg-agente
                       set G2Agg-insert to true
                       set g2AGG-age    to true
                       call   "G2Agg" using G2Agg-linkage
                       cancel "G2Agg"
                    end-perform
              end-start
              close agenti                                          
              display 
           "                                                           " 
                  upon form1-handle line 22,00 col 2,00
           end-if. 
                              
           if chk-tip = 1 or chk-tut = 1
              move 0 to counter counter-2
              display "ELABORAZIONE TIPOLOGIE - TBLCA"
                  upon form1-handle 
                  line 20,00 
                   col  2,00
              |Elaborazione ttipocli - TBLCA
              open i-o tblca
              move low-value to record-tblca
              move "CA" to tblca-codice1
              start tblca key >= tblca-codice
                    invalid continue
                not invalid
                    perform until 1 = 2
                       read tblca next      at end exit perform end-read
                       if tblca-codice1 not = "CA" exit perform end-if
                       delete tblca record invalid continue end-delete
                    end-perform
              end-start
              close tblca
           
              open input ttipocli
              move low-value to tcl-rec
              start ttipocli key is >= tcl-chiave
                    invalid continue
                not invalid
                    perform until 1 = 2
                       read ttipocli next at end exit perform end-read 
                       add 1 to counter counter-2
                       if counter-2 = 10
                          move 0 to counter-2
                          move counter to counter-edit
                          display counter-edit upon form1-handle 
                                               line 20,50
                                                col 26,00
                       end-if
                       move tcl-codice  to G2Agg-categoria
                       set G2Agg-insert to true
                       set g2AGG-cat    to true
                       call   "G2Agg" using G2Agg-linkage
                       cancel "G2Agg"
                    end-perform
              end-start
              close ttipocli 
              display 
           "                                                           " 
                  upon form1-handle line 22,00 col 2,00
           end-if.     
           
           if chk-gdo = 1 or chk-tut = 1
              move 0 to counter counter-2
              display "ELABORAZIONE GDO - FPGRUPPICS"
                  upon form1-handle 
                  line 20,00 
                   col  2,00
              |Elaborazione tgrupgdo - FPGRUPPICS/TBLCS  
              open i-o tblcs
              move low-value to record-tblcs
              move "CS" to tblcs-codice1
              start tblcs key >= tblcs-codice
                    invalid continue
                not invalid
                    perform until 1 = 2
                       read tblcs next      at end exit perform end-read
                       if tblcs-codice1 not = "CS" exit perform end-if
                       delete tblcs record invalid continue end-delete
                    end-perform
              end-start
              close tblcs
              open i-o fpgruppics
              move low-value to record-fpgruppics
              start fpgruppics key >= fpgruppics-key
                    invalid continue
                not invalid
                    perform until 1 = 2
                       read   fpgruppics next 
                           at end exit perform 
                       end-read
                       delete fpgruppics record 
                              invalid continue 
                       end-delete
                    end-perform
              end-start
              close fpgruppics
                      
              open input tgrupgdo
              move low-value to gdo-rec
              start tgrupgdo key >= gdo-chiave
                    invalid continue
                not invalid
                    perform until 1 = 2
                       read tgrupgdo next at end exit perform end-read 
                       if gdo-codice-G2 = spaces  
                          add 1 to counter counter-2
                          if counter-2 = 10
                             move 0 to counter-2
                             move counter to counter-edit
                             display counter-edit upon form1-handle 
                                                  line 20,50
                                                   col 26,00
                          end-if                                
                          initialize G2Agg-linkage 
                          set G2Agg-gdo   to true
                          move gdo-codice to G2Agg-codice-gdo
                          set G2Agg-insert to true
                          call   "G2Agg" using G2Agg-linkage
                          cancel "G2Agg"   
                       end-if
                    end-perform
              end-start
              close tgrupgdo                                        
              display 
           "                                                           "  
                  upon form1-handle line 22,00 col 2,00
           end-if.

           display message "Elaborazione terminata".

           goback.
