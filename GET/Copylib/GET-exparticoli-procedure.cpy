      ***---
       CICLO-PRODENER.
      * 01  tabella-prodener.
      *     05 prodener-el  pic x(100) occurs 10000.
           if art-cod-prodener not = space
              perform varying idx from 1 by 1 until idx > 10000
                 if prodener-el(idx) = art-cod-prodener
                    exit perform
                 else
                    if prodener-el(idx) = space
                       move art-cod-prodener   
                             to prodener-el(idx)
                       exit perform
                    end-if
                 end-if
              end-perform
           end-if.

      ***---
       GEN-ART-PARTE-FISSA.
           initialize exp-art-rec.

           move get-cod-get              to HART_COM_CODICE

           move space                    to HART_ESTENSIONE
           move art-descrizione          to HART_DESCRIZIONE
           move space                    to HART_CLASSI
           move zero                     to HART_CFCA
           move zero                     to HART_CAUDC

           move space                    to HART_CNT_CODICE_UDC
           move space                    to HART_CNT_CODICE_UDS
           move space                    to HART_SEQ_TRASF_IN
           move space                    to HART_SEQ_TRASF_OUT

           move zero                     to HART_PZ_LARGHEZZA
           move zero                     to HART_PZ_LUNGHEZZA
           move zero                     to HART_PZ_ALTEZZA

           move zero                     to HART_PZ_PESO

           move art-altezza              to HART_CF_LARGHEZZA
           move art-larghezza            to HART_CF_LUNGHEZZA
           move art-profondita           to HART_CF_ALTEZZA

           move zero                     to HART_CF_PESO
           move space                    to HART_SEQ_ELAB
           move zero                     to HART_VPK_NUMERO
           move space                    to HART_UDF1

      *     move art-marca-prodotto       to mar-codice
      *     read tmarche
      *        invalid
      *           initialize mar-descrizione
      *     end-read
      *     move mar-descrizione          to HART_UDF2
           move art-marca-prodotto       to HART_UDF2


      *     move art-settore-merceologico to sme-codice
      *     read tsetmerc
      *        invalid
      *           initialize sme-descrizione
      *     end-read
      *     move sme-descrizione          to HART_UDF3
           move art-settore-merceologico to HART_UDF3

      *     move art-classe-1             to cl1-codice
      *     read tcla1art
      *        invalid
      *           initialize cl1-descrizione
      *     end-read
      *     move cl1-descrizione          to HART_UDF4
           move art-classe-1             to HART_UDF4

           move space                    to HART_UDF5
           move space                    to HART_UDF6
           move space                    to HART_UDF7
           move space                    to HART_UDF8
           move space                    to HART_UDF9
           move space                    to HART_UDF10

           move "N"                      to HART_GEST_LOTTO
           move "N"                      to HART_GEST_DSCADENZA
           move "N"                      to HART_GEST_PESO

           move "PZ"                     to HART_UOM_CODICE_UNITA
           move space                    to HART_UOM_CODICE_PESO
           move space                    to HART_FAM_CODICE
           move "N"                      to HART_GEST_MATRICOLA
           move zero                     to HART_GIORNI_VITA
           move zero                     to HART_GIORNI_PRE_DSCADENZA
           move zero                     to HART_PESO_TOLLERANZA
           move space                    to HART_CALCVOL_PZ
           move space                    to HART_CALCVOL_CF
           move space                    to HART_CALCVOL_POSIZ
           move space                    to HART_CTRL_QUALITA
           move zero                     to HART_ORE_QUARANTENA


           perform CONTA-EAN
           evaluate cont-ean
           when zero
                move space               to HART_ARTDET_CODICE
           when 1
                continue
           when other
                move space               to HART_ARTDET_CODICE
           end-evaluate


           move "IV"                     to tbliv-codice1
           move art-codice-iva           to tbliv-codice2
           read tivaese no lock
              invalid 
                 continue
           end-read.

           if tbliv-esenzione = "S"
              move "ES"                  to HART_TIPOIVA
           else
              move TBLIV-PERCENTUALE     to como-aliquota
              move como-aliquota         to HART_TIPOIVA
           end-if.


           move zero                     to HART_B_ANIDRI-pre
                                            HART_B_ANIDRI
           move zero                     to HART_D_ANIDRI-pre
                                            HART_D_ANIDRI
           move zero                     to HART_B_IDRATI-pre
                                            HART_B_IDRATI
           move zero                     to HART_D_IDRATI-pre
                                            HART_D_IDRATI
           move space                    to HART_ADR

      *    il peso è espresso in KG get lo chiede in grammi, quindi
      *    moltiplico
      *     compute HART_PZ_PESO_NETTO = (art-peso-non-utf +
      *                                  art-peso-utf) * 1000
           compute HART_PZ_PESO_NETTO = (exp-prg-peso-utf +
                                        exp-prg-peso-non-utf) * 1000



           move space                    to HART_UOM_ALTERNATIVA

           move zero                     to HART_UOM_CONV_NUMERATORE
           move zero                     to HART_UOM_CONV_DENOMINATORE
           move art-cod-prodener         to HART_UOM_VOLUME
           move zero                     to HART_FLASHP
           move space                    to HART_IMBALLO
           move art-perce-imposte        to HART_PERC_UTIF
           move zero                     to HART_QTA_TIMBRA_EST.

      ***---
       CONTA-EAN.
           move zero   to cont-ean.
           if art-codice-ean-1 not = zero
              add 1    to cont-ean
              move art-codice-ean-1   to HART_ARTDET_CODICE
           end-if
           if art-codice-ean-2 not = zero
              add 1    to cont-ean
              move art-codice-ean-2   to HART_ARTDET_CODICE
           end-if
           if art-codice-ean-3 not = zero
              add 1    to cont-ean
              move art-codice-ean-3   to HART_ARTDET_CODICE
           end-if
           if art-codice-ean-4 not = zero
              add 1    to cont-ean
              move art-codice-ean-4   to HART_ARTDET_CODICE
           end-if
           if art-codice-ean-5 not = zero
              add 1    to cont-ean
              move art-codice-ean-5   to HART_ARTDET_CODICE
           end-if.


      ***---
       GEN-ART-PARTE-VARIABILE.
      *     05 imballo-articolo  pic x(3) occurs 200.
           string art-codice             delimited by size
                  "-"                    delimited by size
                  imballo-articolo(cont) delimited by size
                  into HART_CODICE
           move imballo-articolo(cont)   to imq-codice
           read timbalqta
              invalid
                 move zero   to imq-qta-imb      
           end-read
           move imq-qta-imb        to HART_PZCF.

           move exp-art-rec  to line-riga
           write line-riga.

           if cont-ean > zero
              perform GEN-EAN
           end-if.

      ***---
       GEN-EAN.
           initialize exp-art-det-rec

           move HART_COM_CODICE       to HARTDET_ART_COM_CODICE
           move HART_CODICE           to HARTDET_ART_CODICE
           move 1                     to HARTDET_QTAEAN
           move "PZ"                  to HARTDET_UOM_CODICE

           if art-codice-ean-1 not = zero
              move art-codice-ean-1   to HARTDET_CODICE
              move exp-art-det-rec    to line-riga2
              write line-riga2
              add 1                   to num-rec-ean
           end-if
           if art-codice-ean-2 not = zero
              move art-codice-ean-2   to HARTDET_CODICE
              move exp-art-det-rec    to line-riga2
              add 1                   to num-rec-ean
              write line-riga2
           end-if
           if art-codice-ean-3 not = zero
              move art-codice-ean-3   to HARTDET_CODICE
              move exp-art-det-rec    to line-riga2
              add 1                   to num-rec-ean
              write line-riga2
           end-if
           if art-codice-ean-4 not = zero
              move art-codice-ean-4   to HARTDET_CODICE
              move exp-art-det-rec    to line-riga2
              add 1                   to num-rec-ean
              write line-riga2
           end-if
           if art-codice-ean-5 not = zero
              move art-codice-ean-5   to HARTDET_CODICE
              move exp-art-det-rec    to line-riga2
              add 1                   to num-rec-ean
              write line-riga2
           end-if.

      ***---
       GENERA-PRODENER.
           perform varying idx from 1 by 1 until idx > 10000
              if prodener-el(idx) = space
                 exit perform
              else
                 move prodener-el(idx)  to pen-codice
                 read prodener
                    invalid
                       continue
                 end-read


                 initialize exp-pen-rec

                 move pen-codice      to exp-pen-codice     
                 move pen-descrizione to exp-pen-descrizione
                 move pen-cpa         to exp-pen-cpa        
                 move pen-nc          to exp-pen-nc         
                 move pen-taric       to exp-pen-taric      
                 move pen-dac         to exp-pen-dac        

                 move exp-pen-rec     to line-riga3
                 write line-riga3
                 add 1                to num-rec-prodener
              end-if
           end-perform.


