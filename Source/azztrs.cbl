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
       77  status-trasporti  pic xx.
       77  status-tmovmag    pic xx.
       77  status-tcaumag    pic xx.   
                         
       77  como-qta-kg       PIC  9(9)v999.
       77  como-qta-arrot    PIC  9(9)v999.
       77  como-tariffa      PIC  9(9)v99.

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
                          
                    if trs-qta-kg-s1    not = 0 or
                       trs-qta-arrot-s1 not = 0 or
                       trs-tariffa-s1   not = 0
                       move trs-qta-kg-s1    to como-qta-kg
                       move trs-qta-arrot-s1 to como-qta-arrot
                       move trs-tariffa-s1   to como-tariffa
                    else
                       if trs-qta-kg-s2    not = 0 or
                          trs-qta-arrot-s2 not = 0 or
                          trs-tariffa-s2   not = 0
                          move trs-qta-kg-s2    to como-qta-kg
                          move trs-qta-arrot-s2 to como-qta-arrot
                          move trs-tariffa-s2   to como-tariffa
                       else                                    
                          move trs-qta-kg-s3    to como-qta-kg
                          move trs-qta-arrot-s3 to como-qta-arrot
                          move trs-tariffa-s3   to como-tariffa
                       end-if
                    end-if                                  
                    move 0 to trs-qta-kg-s1 
                              trs-qta-arrot-s1 
                              trs-tariffa-s1
                              trs-qta-kg-s2
                              trs-qta-arrot-s2
                              trs-tariffa-s2
                              trs-qta-kg-s3
                              trs-qta-arrot-s3
                              trs-tariffa-s3

                    if trs-num-bolla < 400001
                       move como-qta-kg    to trs-qta-kg-s1
                       move como-qta-arrot to trs-qta-arrot-s1
                       move como-tariffa   to trs-tariffa-s1
                    else                                    
                       move como-qta-kg    to trs-qta-kg-s2
                       move como-qta-arrot to trs-qta-arrot-s2
                       move como-tariffa   to trs-tariffa-s2
                    end-if
                    rewrite trs-rec
                 end-perform
           end-start.
           close trasporti tmovmag tcaumag.
           display message "FINE"
           goback.
