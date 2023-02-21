       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      azztrasp.
       AUTHOR.                          Andrea.
  
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "trasporti.sl".
           copy "tmovmag.sl".
           copy "tcaumag.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.           
           copy "trasporti.fd".
           copy "tmovmag.fd".
           copy "tcaumag.fd".

       WORKING-STORAGE SECTION.
       77  status-trasporti pic xx.
       77  status-tmovmag   pic xx.
       77  status-tcaumag   pic xx.

       LINKAGE SECTION.

      *****************************************************************

       PROCEDURE DIVISION. 

       MAIN.
           open i-o   trasporti.
           open input tmovmag tcaumag.
           move low-value to trs-chiave.
           start trasporti key >= trs-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read trasporti next at end exit perform end-read
                    move 0 to trs-tmo-anno trs-tmo-numero
                    move spaces to trs-causale

                    move low-value to tmo-rec
                    set tmo-cliente to true
                    move trs-num-bolla  to tmo-numdoc-clifor
                    move trs-data-bolla to tmo-data-doc
                    start tmovmag key >= tmo-k-bolla
                          invalid continue
                      not invalid
                          perform until 1 = 2
                             read tmovmag next 
                               at end exit perform 
                             end-read
                             if tmo-fornitore or
                                trs-num-bolla not = trs-num-bolla or
                                tmo-data-doc  not = trs-data-bolla
                                exit perform
                             end-if
                             move tmo-causale to tca-codice
                             read tcaumag no lock
                                  invalid exit perform cycle
                              not invalid
                                  if tca-no-tras
                                     exit perform cycle
                                  end-if
                             end-read
                             move tmo-causale to trs-causale
                             move tmo-chiave  to trs-tmo-chiave
                             exit perform
                          end-perform
                    end-start      
                    rewrite trs-rec
                 end-perform
           end-start.
           close trasporti tmovmag tcaumag.
           display message "FINE"
           goback.
