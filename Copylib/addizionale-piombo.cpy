      * Tenere allineato con addizionale-piombo-fornitore.cpy
      ***---
       ADDIZIONALE-PIOMBO.
           move 0 to add-piombo add-piombo-3dec.
                                   
           start tpiombo key <= tpb-chiave
                 invalid continue
             not invalid
                 read tpiombo previous
                 if tpb-marca = art-marca-prodotto and
                    tpb-data <= como-data-ordine   

                    accept calcolo-piombo 
                           from environment "CALCOLO_PIOMBO"
                    if nuovo-calcolo-piombo
                       perform TROVA-PARAMETRO
                    else
                       set prm-add-piombo-perce-si  to true
                       set prm-add-piombo-ampere-no to true
                       set prm-add-piombo-bosch-no  to true
                    end-if

                    if prm-add-piombo-perce-si                 
                       compute risultato-imposte  =
                               como-prz-unitario
                       if art-auto-cobat
                          compute add-piombo-3dec =
                          risultato-imposte -
                          risultato-imposte / 
                          (1 + (tpb-perce-auto / 100))
                       else
                          compute add-piombo-3dec =
                          risultato-imposte -
                          risultato-imposte / 
                          (1 + (tpb-perce-moto / 100))
                       end-if
                    end-if

                    if prm-add-piombo-ampere-si
                       compute add-piombo-3dec =
                               add-piombo-3dec +
                             ( art-amperaggio  * tpb-euro-ampere )
                    end-if
                    if prm-add-piombo-bosch-si
                       evaluate true
                       when   art-amperaggio >= tpb-pb-sca-1-da and
                              art-amperaggio <= tpb-pb-sca-1-a
                              compute add-piombo-3dec =
                                      add-piombo-3dec +
                                      tpb-pb-sca-1-euro

                       when   art-amperaggio >= tpb-pb-sca-2-da and
                              art-amperaggio <= tpb-pb-sca-2-a
                              compute add-piombo-3dec =
                                      add-piombo-3dec +
                                      tpb-pb-sca-2-euro

                       when   art-amperaggio >= tpb-pb-sca-3-da and
                              art-amperaggio <= tpb-pb-sca-3-a
                              compute add-piombo-3dec =
                                      add-piombo-3dec +
                                      tpb-pb-sca-3-euro

                       when   art-amperaggio >= tpb-pb-sca-4-da and
                              art-amperaggio <= tpb-pb-sca-4-a
                              compute add-piombo-3dec =
                                      add-piombo-3dec +
                                      tpb-pb-sca-4-euro 

                       when   art-amperaggio >= tpb-pb-sca-5-da and
                              art-amperaggio <= tpb-pb-sca-5-a
                              compute add-piombo-3dec =
                                      add-piombo-3dec +
                                      tpb-pb-sca-5-euro

                       when   art-amperaggio >= tpb-pb-sca-6-da and
                              art-amperaggio <= tpb-pb-sca-6-a
                              compute add-piombo-3dec =
                                      add-piombo-3dec +
                                      tpb-pb-sca-6-euro

                       when   art-amperaggio >= tpb-pb-sca-7-da and
                              art-amperaggio <= tpb-pb-sca-7-a
                              compute add-piombo-3dec =
                                      add-piombo-3dec +
                                      tpb-pb-sca-7-euro

                       when   art-amperaggio >= tpb-pb-sca-8-da and
                              art-amperaggio <= tpb-pb-sca-8-a
                              compute add-piombo-3dec =
                                      add-piombo-3dec +
                                      tpb-pb-sca-8-euro

                       when   art-amperaggio >= tpb-pb-sca-9-da and
                              art-amperaggio <= tpb-pb-sca-9-a
                              compute add-piombo-3dec =
                                      add-piombo-3dec +
                                      tpb-pb-sca-9-euro

                       when   art-amperaggio >= tpb-pb-sca-10-da and
                              art-amperaggio <= tpb-pb-sca-10-a
                              compute add-piombo-3dec =
                                      add-piombo-3dec +
                                      tpb-pb-sca-10-euro
                       end-evaluate
                    end-if   
                    add 0,005 to add-piombo-3dec giving add-piombo
                 end-if
           end-start.
