       Program-id.   cor-data.
       Author.       Andrea.

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       FILE-CONTROL.
           copy "tmovmag.sl".
           copy "tnotacr.sl".
           copy "btnotacr.sl".

       FILE SECTION.          
           copy "tmovmag.fd".
           copy "tnotacr.fd".
           copy "btnotacr.fd".

       WORKING-STORAGE SECTION.     
       copy "utydata.def".
       77  status-tmovmag  pic xx.
       77  status-tnotacr  pic xx.
       77  status-btnotacr pic xx.  
                                    
       77  como-aaaa       pic 9(4).
       77  como-mm         pic 9(2).
       77  como-gg         pic 9(2).

      *-----------------------------------------------------------------

       PROCEDURE DIVISION.
      ***---
       MAIN.                    
           open i-o tnotacr btnotacr tmovmag.
           move low-value to tno-chiave.
           move 2019 to tno-anno.
           start tnotacr key >= tno-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tnotacr next at end exit perform end-read
                    if tno-data-ingresso not = 0
                       move tno-data-ingresso to como-data
                       set aaaa-mm-gg to true
                       call "utydata" using test-validita,
                                            formato-data,
                                            como-data
                       end-call
                       if como-data = 99999999
                          move tno-data-ingresso(5:4) to como-aaaa
                          move tno-data-ingresso(3:2) to como-mm
                          move tno-data-ingresso(1:2) to como-gg
                          move como-aaaa to tno-data-ingresso(1:4)
                          move como-mm   to tno-data-ingresso(5:2)
                          move como-gg   to tno-data-ingresso(7:2)
                          rewrite tno-rec
                       end-if                                  
                    end-if
                 end-perform
           end-start.
           move low-value to btno-chiave.
           move 2019 to btno-anno.
           start btnotacr key >= btno-chiave
                 invalid
             not invalid
                 perform until 1 = 2
                    read btnotacr next at end exit perform end-read
                    if btno-data-ingresso not = 0
                       move btno-data-ingresso to como-data
                       set aaaa-mm-gg to true
                       call "utydata" using test-validita,
                                            formato-data,
                                            como-data
                       end-call
                       if como-data = 99999999
                          move btno-data-ingresso(5:4) to como-aaaa
                          move btno-data-ingresso(3:2) to como-mm
                          move btno-data-ingresso(1:2) to como-gg
                          move como-aaaa to btno-data-ingresso(1:4)
                          move como-mm   to btno-data-ingresso(5:2)
                          move como-gg   to btno-data-ingresso(7:2)
                          rewrite btno-rec
                       end-if                                  
                    end-if
                 end-perform
           end-start.
           move low-value to tmo-chiave.
           move 2019 to tmo-anno.
           start tmovmag key >= tmo-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tmovmag next at end exit perform end-read
                    if tmo-data-movim not = 0
                       move tmo-data-movim to como-data
                       set aaaa-mm-gg to true
                       call "utydata" using test-validita,
                                            formato-data,
                                            como-data
                       end-call
                       if como-data = 99999999
                          move tmo-data-movim(5:4) to como-aaaa
                          move tmo-data-movim(3:2) to como-mm
                          move tmo-data-movim(1:2) to como-gg
                          move como-aaaa to tmo-data-movim(1:4)
                          move como-mm   to tmo-data-movim(5:2)
                          move como-gg   to tmo-data-movim(7:2)
                          rewrite tmo-rec
                       end-if                                  
                    end-if
                 end-perform
           end-start.
           close tnotacr btnotacr tmovmag.
           goback.         
