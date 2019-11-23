      ***---
       CONTROLLO.
           if not ControllaCampi
              exit paragraph
           end-if

           set tutto-ok to true.
           inquire ef-flusso, value in ef-flusso-buf

      * Elenco degli Id sui quali fare il CONTROLLO nel programma gtvettori
      * paragrafo per la struttura dei controlli sulla screen Form1
           evaluate control-id
           when 78-ID-ef-path
                inquire ef-path, value in ef-path-buf
                if ef-path-buf = spaces
                   display message box
                           "Percorso d'installazione obbligatorio"
                           title = titolo
                           icon 2
                   set errori to true
                end-if
           when 78-ID-ef-orig-rete
                inquire ef-orig-rete, value in ef-orig-rete-buf
                if ef-orig-rete-buf = spaces
                   display message box
                     "Suffisso (nome dir) per ORIGINE RETE obbligatorio"
                           title = titolo
                           icon 2
                   set errori to true
                end-if
           when 78-ID-ef-dest
                inquire ef-dest, value in ef-dest-buf
                if ef-dest-buf = spaces
                   display message box
                    "Suffisso (nome dir) per DESTINAZIONE obbligatorio"
                           title = titolo
                           icon 2
                   set errori to true
                end-if
           when 78-ID-ef-tmp
                inquire ef-tmp, value in ef-tmp-buf
                if ef-tmp-buf = spaces
                   display message box
                      "Suffisso (nome dir) per TEMPORANEI obbligatorio"
                           title = titolo
                           icon 2
                   set errori to true
                end-if
           when 78-ID-ef-err
                inquire ef-err, value in ef-err-buf
                if ef-err-buf = spaces
                   display message box
                           "Suffisso (nome dir) per ERRATI obbligatorio"
                           title = titolo
                           icon 2
                   set errori to true
                end-if
           when 78-ID-ef-log
                inquire ef-log, value in ef-log-buf
                if ef-log-buf = spaces
                   display message box
                           "Suffisso (nome dir) per LOG obbligatorio"
                           title = titolo
                           icon 2
                   set errori to true
                end-if
           when 78-ID-ef-invio
                inquire ef-invio, value in ef-invio-buf
                if ef-invio-buf = spaces
                   display message box
                           "Suffisso (nome dir) per INVIO obbligatorio"
                           title = titolo
                           icon 2
                   set errori to true
                end-if
           when 78-ID-ef-orig
                inquire ef-orig, value in ef-orig-buf
                if ef-orig-buf = spaces
                   display message box
                        "Suffisso (nome dir) per ORIGINE obbligatorio"
                           title = titolo
                           icon 2
                   set errori to true
                end-if
      *     when 78-ID-ef-flusso
      *          inquire ef-flusso, value in ef-flusso-buf
      *          inquire ef-flusso, value in ef-flusso-buf
      *          if ef-flusso-buf  = spaces
      *             display message box
      *                "Non può esserci il nome dei colli senza la BOLLA"
      *                     title = titolo
      *                     icon 2
      *             set errori to true
      *          end-if


           when 78-ID-ef-rif-forn-da
                inquire ef-rif-forn-da, value in ef-rif-forn-da-buf
                move ef-rif-forn-da-buf to vtt-rif-lubex
                if vtt-rif-lubex = 0 and ef-flusso-buf not = spaces
                   set errori to true
                   display message box
                           "Offset per Riferimento Lubex obbligatorio"
                           title = titolo
                           icon  2
                end-if
           when 78-ID-ef-rif-forn-per
                if controllo-finale and vtt-import-txt
                   inquire ef-rif-forn-per, value in ef-rif-forn-per-buf
                   move ef-rif-forn-per-buf to vtt-rif-lubex-per
                   if vtt-rif-lubex-per = 0 
                      set errori to true
                      display message box
                   "Offset per lunghezza Riferimento Lubex obbligatorio"
                           title = titolo
                           icon  2
                   end-if
                end-if

      *     when 78-ID-ef-qta-totale-per
      *          inquire ef-qta-totale-per, value ef-qta-totale-per-buf
      *          move ef-qta-totale-per-buf to vtt-qta-per
      *          if vtt-qta-per = 0 and ef-flusso-c-buf not = spaces
      *             set errori to true
      *             display message box
      *               "Offset per lunghezza quantità totale obbligatorio"
      *                     title = titolo
      *                     icon  2
      *          end-if
      *     when 78-ID-ef-num-colli-da
      *          inquire ef-num-colli-da, value in ef-num-colli-da-buf
      *          move ef-num-colli-da-buf to vtt-num-colli
      *          if vtt-num-colli = 0 and ef-flusso-c-buf not = spaces
      *             set errori to true
      *             display message box
      *                     "Offset per numero colli obbligatorio"
      *                     title = titolo
      *                     icon  2
      *          end-if
      *     when 78-ID-ef-num-colli-per
      *          inquire ef-num-colli-per, value in ef-num-colli-per-buf
      *          move ef-num-colli-per-buf to vtt-num-colli
      *          if vtt-num-colli = 0 and ef-flusso-c-buf not = spaces
      *             set errori to true
      *             display message box
      *                 "Offset per lunghezza numero colli obbligatorio"
      *                     title = titolo
      *                     icon  2
      *          end-if
      *     when 78-ID-ef-cod-art-cli-da
      *          inquire ef-cod-art-cli-da, value ef-cod-art-cli-da-buf
      *          move ef-cod-art-cli-da-buf to vtt-cod-art-cli
      *          if vtt-cod-art-cli = 0 and ef-flusso-c-buf not = spaces
      *             set errori to true
      *             display message box
      *                "Offset per codice articolo cliente obbligatorio"
      *                     title = titolo
      *                     icon  2
      *          end-if
      *     when 78-ID-ef-cod-art-cli-per
      *          inquire ef-cod-art-cli-per, value ef-cod-art-cli-per-buf
      *          move ef-cod-art-cli-per-buf to vtt-cod-art-cli-per
      *          if vtt-cod-art-cli-per = 0 and 
      *             ef-flusso-c-buf not = spaces
      *             set errori to true
      *             display message box
      *       "Offset per lunghezza codice articolo cliente obbligatorio"
      *                     title = titolo
      *                     icon  2
      *          end-if
      *     when 78-ID-ef-barcode-da
      *          inquire ef-barcode-da, value in ef-barcode-da-buf
      *          move ef-barcode-da-buf to vtt-barcode
      *          if vtt-barcode = 0 and ef-flusso-c-buf not = spaces
      *             set errori to true
      *             display message box
      *                     "Offset per barcode obbligatorio"
      *                     title = titolo
      *                     icon = 2
      *           end-if
      *     when 78-ID-ef-barcode-per
      *          inquire ef-barcode-per, value in ef-barcode-per-buf
      *          move ef-barcode-per-buf to vtt-barcode-per
      *          if vtt-barcode-per = 0 and ef-flusso-c-buf not = spaces
      *             set errori to true
      *             display message box
      *                     "Offset per lunghezza barcode obbligatorio"
      *                     title = titolo
      *                     icon = 2
      *           end-if
      *     when 78-ID-ef-rif-forn-da
      *          if controllo-finale
      *             inquire ef-rif-forn-da, value in ef-rif-forn-da-buf
      *             move ef-rif-forn-da-buf to vtt-rif-fornitore
      *             inquire ef-rif-forn-per, value in ef-rif-forn-per-buf
      *             move ef-rif-forn-per-buf to vtt-rif-fornitore-per
      *             if vtt-rif-fornitore = 0 and 
      *                vtt-rif-fornitore-per not = 0 and 
      *                ef-flusso-c-buf not = spaces
      *                set errori to true
      *                display message box
      *                   "Offset per riferimento fornitori obbligatorio"
      *                        title = titolo
      *                        icon  2
      *             end-if
      *          end-if
      *     when 78-ID-ef-rif-forn-per
      *          if controllo-finale
      *             inquire ef-rif-forn-da, value in ef-rif-forn-da-buf
      *             move ef-rif-forn-da-buf to vtt-rif-fornitore
      *             inquire ef-rif-forn-per, value in ef-rif-forn-per-buf
      *             move ef-rif-forn-per-buf to vtt-rif-fornitore-per
      *             if vtt-rif-fornitore not = 0 and 
      *                vtt-rif-fornitore-per = 0 and 
      *                ef-flusso-c-buf not = spaces
      *                set errori to true
      *                display message box
      *         "Offset per lunghezza riferimento fornitori obbligatorio"
      *                        title = titolo
      *                        icon  2
      *             end-if
      *             if vtt-rif-fornitore = 0 and
      *                vtt-rif-fornitore-per not = 0 and
      *                ef-flusso-c-buf not = spaces
      *                set errori to true
      *                display message box
      *                   "Offset per riferimento fornitori obbligatorio"
      *                        title = titolo
      *                        icon  2
      *                move 78-ID-ef-rif-forn-da to control-id
      *             end-if
      *          end-if
      *     when 78-ID-ef-note-da
      *          if controllo-finale
      *             inquire ef-note-da, value in ef-note-da-buf
      *             move ef-note-da-buf to vtt-note
      *             inquire ef-note-per, value in ef-note-per-buf
      *             move ef-note-per-buf to vtt-note-per
      *             if vtt-note = 0 and vtt-note-per not = 0 and
      *                ef-flusso-c-buf not = spaces
      *                set errori to true
      *                display message box "Offset per note obbligatorio"
      *                        title = titolo
      *                        icon  2
      *             end-if
      *          end-if
      *     when 78-ID-ef-note-per
      *          if controllo-finale
      *             inquire ef-note-per, value in ef-note-per-buf
      *             move ef-note-per-buf to vtt-note-per
      *             inquire ef-note-da, value in ef-note-da-buf
      *             move ef-note-da-buf to vtt-note
      *             if vtt-note not = 0 and vtt-note-per = 0 and
      *                ef-flusso-c-buf not = spaces
      *                set errori to true
      *                display message box
      *                        "Offset per lunghezza note obbligatorio"
      *                        title = titolo
      *                        icon  2
      *             end-if
      *             if vtt-note = 0 and vtt-note-per not = 0 and
      *                ef-flusso-c-buf not = spaces
      *                set errori to true
      *                display message box
      *                        "Offset per note obbligatorio"
      *                        title = titolo
      *                        icon  2
      *                move 78-ID-ef-note-da to control-id
      *             end-if
      *          end-if
      *     when 78-ID-ef-tipo-auto-da
      *          if controllo-finale
      *             inquire ef-tipo-auto-da, value ef-tipo-auto-da-buf
      *             move ef-tipo-auto-da-buf to vtt-tipo-automezzo
      *             inquire ef-tipo-auto-per, value ef-tipo-auto-per-buf
      *             move ef-tipo-auto-per-buf to vtt-tipo-automezzo-per
      *             if vtt-tipo-automezzo = 0 and
      *                vtt-tipo-automezzo-per not = 0 and
      *                ef-flusso-c-buf not = spaces
      *                set errori to true
      *                display message box
      *                        "Offset per tipo automezzo obbligatorio"
      *                        title = titolo
      *                        icon  2
      *             end-if
      *          end-if
      *     when 78-ID-ef-tipo-auto-per
      *          if controllo-finale
      *             inquire ef-tipo-auto-per, value ef-tipo-auto-per-buf
      *             move ef-tipo-auto-per-buf to vtt-tipo-automezzo-per
      *             if vtt-tipo-automezzo not = 0 and
      *                vtt-tipo-automezzo-per = 0 and
      *                ef-flusso-c-buf not = spaces
      *                set errori to true
      *                display message box
      *               "Offset per lunghezza tipo automezzo obbligatorio"
      *                        title = titolo
      *                        icon  2
      *             end-if
      *             if vtt-tipo-automezzo = 0 and
      *                vtt-tipo-automezzo-per not = 0 and
      *                ef-flusso-c-buf not = spaces
      *                set errori to true
      *                display message box
      *                        "Offset per tipo automezzo obbligatorio"
      *                        title = titolo
      *                        icon  2
      *                move 78-ID-ef-tipo-auto-da to control-id
      *             end-if
      *          end-if
      *     when 78-ID-ef-targa-da
      *          if controllo-finale
      *             inquire ef-targa-da, value in ef-targa-da-buf
      *             move ef-targa-da-buf to vtt-targa
      *             inquire ef-targa-per, value in ef-targa-per-buf
      *             move ef-targa-per-buf to vtt-targa-per
      *             if vtt-targa = 0 and vtt-targa-per not = 0 and
      *                ef-flusso-c-buf not = spaces
      *                set errori to true
      *                display message box
      *                        "Offset per targa obbligatorio"
      *                        title = titolo
      *                        icon  2
      *             end-if
      *          end-if
      *     when 78-ID-ef-targa-per
      *          if controllo-finale
      *             inquire ef-targa-da, value in ef-targa-da-buf
      *             move ef-targa-da-buf to vtt-targa
      *             inquire ef-targa-per, value in ef-targa-per-buf
      *             move ef-targa-per-buf to vtt-targa-per
      *             if vtt-targa not = 0 and vtt-targa-per = 0 and
      *                ef-flusso-c-buf not = spaces
      *                set errori to true
      *                display message box
      *                        "Offset per lunghezza targa obbligatorio"
      *                        title = titolo
      *                        icon  2
      *             end-if
      *             if vtt-targa = 0 and vtt-targa-per not = 0 and
      *                ef-flusso-c-buf not = spaces
      *                set errori to true
      *                display message box
      *                        "Offset per targa obbligatorio"
      *                        title = titolo
      *                        icon  2
      *                move 78-ID-ef-targa-da to control-id
      *             end-if
      *          end-if
      *     when 78-ID-ef-cod-art-forn-da
      *          if controllo-finale
      *             inquire ef-cod-art-forn-da, 
      *                                   value in ef-cod-art-forn-da-buf
      *             move ef-cod-art-forn-da-buf to vtt-cod-art-forn
      *             inquire ef-cod-art-forn-per, 
      *                                value in ef-cod-art-forn-per-buf
      *             move ef-cod-art-forn-per-buf to vtt-cod-art-forn-per
      *             if vtt-cod-art-forn = 0 and
      *                vtt-cod-art-forn-per not = 0 and
      *                ef-flusso-c-buf not = spaces
      *                set errori to true
      *                display message box
      *              "Offset per codice articolo fornitore obbligatorio"
      *                        title = titolo
      *                        icon  2
      *             end-if
      *          end-if
      *     when 78-ID-ef-cod-art-forn-per
      *          if controllo-finale
      *             inquire ef-cod-art-forn-da, 
      *                                      value ef-cod-art-forn-da-buf
      *             move ef-cod-art-forn-da-buf to vtt-cod-art-forn
      *             inquire ef-cod-art-forn-per, 
      *                                     value ef-cod-art-forn-per-buf
      *             move ef-cod-art-forn-per-buf to vtt-cod-art-forn-per
      *             if vtt-cod-art-forn not = 0 and
      *                vtt-cod-art-forn-per = 0 and
      *                ef-flusso-c-buf not = spaces
      *                set errori to true
      *                display message box
      *     "Offset per lunghezza codice articolo fornitore obbligatorio"
      *                        title = titolo
      *                        icon  2
      *             end-if
      *             if vtt-cod-art-forn = 0 and
      *                vtt-cod-art-forn-per not = 0 and
      *                ef-flusso-c-buf not = spaces
      *                set errori to true
      *                display message box
      *              "Offset per codice articolo fornitore obbligatorio"
      *                        title = titolo
      *                        icon  2
      *                move 78-ID-ef-cod-art-forn-da to control-id
      *             end-if
      *          end-if
      *     when 78-ID-ef-num-bancali-da
      *          if controllo-finale
      *             inquire ef-num-bancali-da, 
      *                                   value in ef-num-bancali-da-buf
      *             move ef-num-bancali-da-buf to vtt-num-bancali
      *             inquire ef-num-bancali-per, 
      *                                   value in ef-num-bancali-per-buf
      *             move ef-num-bancali-per-buf to vtt-num-bancali-per
      *             if vtt-num-bancali = 0 and
      *                vtt-num-bancali-per not = 0 and
      *                ef-flusso-c-buf not = spaces
      *                set errori to true
      *                display message box
      *                        "Offset per numero bancali obbligatorio"
      *                        title = titolo
      *                        icon  2
      *             end-if
      *          end-if
      *     when 78-ID-ef-num-bancali-per
      *          if controllo-finale
      *             inquire ef-num-bancali-per, 
      *                                value in ef-num-bancali-per-buf
      *             move ef-num-bancali-per-buf to vtt-num-bancali-per
      *             if vtt-num-bancali not = 0 and
      *                vtt-num-bancali-per = 0 and
      *                ef-flusso-c-buf not = spaces
      *                set errori to true
      *                display message box
      *                "Offset per lunghezza numero bancali obbligatorio"
      *                        title = titolo
      *                        icon  2
      *             end-if
      *             if vtt-num-bancali = 0 and
      *                vtt-num-bancali-per not = 0 and
      *                ef-flusso-c-buf not = spaces
      *                set errori to true
      *                display message box
      *                        "Offset per numero bancali obbligatorio"
      *                        title = titolo
      *                        icon  2
      *                move 78-ID-ef-num-bancali-da to control-id
      *             end-if
      *          end-if
      *     when 78-ID-ef-lotto-da
      *          if controllo-finale
      *             inquire ef-lotto-da, value in ef-lotto-da-buf
      *             move ef-lotto-da-buf to vtt-lotto
      *             inquire ef-lotto-per, value in ef-lotto-per-buf
      *             move ef-lotto-per-buf to vtt-lotto-per
      *             if vtt-lotto = 0 and vtt-lotto-per not = 0 and
      *                ef-flusso-c-buf not = spaces
      *                set errori to true
      *                display message box
      *                        "Offset per lotto obbligatorio"
      *                        title = titolo
      *                        icon  2
      *             end-if
      *          end-if
      *     when 78-ID-ef-lotto-per
      *          if controllo-finale
      *             inquire ef-lotto-da, value in ef-lotto-da-buf
      *             move ef-lotto-da-buf to vtt-lotto
      *             inquire ef-lotto-per, value in ef-lotto-per-buf
      *             move ef-lotto-per-buf to vtt-lotto-per
      *             if vtt-lotto not = 0 and vtt-lotto-per = 0 and
      *                ef-flusso-c-buf not = spaces
      *                set errori to true
      *                display message box
      *                        "Offset per lunghezza lotto obbligatorio"
      *                        title = titolo
      *                        icon  2
      *             end-if
      *             if vtt-lotto = 0 and vtt-lotto-per not = 0 and
      *                ef-flusso-c-buf not = spaces
      *                set errori to true
      *                display message box
      *                        "Offset per lotto obbligatorio"
      *                        title = titolo
      *                        icon  2
      *                move 78-ID-ef-lotto-da to control-id
      *             end-if
      *          end-if
      *     when 78-ID-ef-serial-da
      *          if controllo-finale
      *             inquire ef-serial-da, value in ef-serial-da-buf
      *             move ef-serial-da-buf to vtt-serial-number
      *             inquire ef-serial-per, value in ef-serial-per-buf
      *             move ef-serial-per-buf to vtt-serial-number-per
      *             if vtt-serial-number = 0 and
      *                vtt-serial-number-per not = 0 and
      *                ef-flusso-c-buf not = spaces
      *                set errori to true
      *                display message box
      *                        "Offset per serial number obbligatorio"
      *                        title = titolo
      *                        icon  2
      *             end-if
      *          end-if
      *     when 78-ID-ef-serial-per
      *          if controllo-finale
      *             inquire ef-serial-da, value in ef-serial-da-buf
      *             move ef-serial-da-buf to vtt-serial-number
      *             inquire ef-serial-per, value in ef-serial-per-buf
      *             move ef-serial-per-buf to vtt-serial-number-per
      *             if vtt-serial-number not = 0 and
      *                vtt-serial-number-per = 0 and
      *                ef-flusso-c-buf not = spaces
      *                set errori to true
      *                display message box
      *                 "Offset per lunghezza serial number obbligatorio"
      *                        title = titolo
      *                        icon  2
      *             end-if
      *             if vtt-serial-number = 0 and
      *                vtt-serial-number-per not = 0 and
      *                ef-flusso-c-buf not = spaces
      *                set errori to true
      *                display message box
      *                        "Offset per serial number obbligatorio"
      *                        title = titolo
      *                        icon  2
      *                move 78-ID-ef-serial-da to control-id
      *             end-if
      *          end-if
      *     when 78-ID-ef-vol-dec
      *          if controllo-finale
      *             inquire ef-vol-dec, value in ef-vol-dec-buf
      *             move ef-vol-dec-buf to vtt-volume-dec-t
      *             inquire ef-vol-dec-per, value in ef-vol-dec-per-buf
      *             move ef-vol-dec-per-buf to vtt-volume-dec-per-t
      *             inquire ef-vol-int, value in ef-vol-int-buf
      *             move ef-vol-int-buf to vtt-volume-int-t
      *             inquire ef-vol-int-per, value in ef-vol-int-per-buf
      *             move ef-vol-int-per-buf to vtt-volume-int-per-t
      *             if vtt-volume-dec-t = 0 and
      *                vtt-volume-dec-per-t not = 0 and
      *                ef-flusso-c-buf not = spaces
      *                set errori to true
      *                display message box
      *                        "Offset per volume decimale obbligatorio"
      *                        title = titolo
      *                        icon  2
      *             end-if
      *             if vtt-volume-int-t = 0 and vtt-volume-dec-t not = 0
      *                and vtt-volume-dec-per-t not = 0 and
      *                ef-flusso-c-buf not = spaces
      *                set errori to true
      *                display message box
      *                        "Offset per volume intero obbligatorio"
      *                        title = titolo
      *                        icon  2
      *                move 78-ID-ef-vol-int to control-id
      *             end-if
      *             if vtt-volume-int-per-t = 0 and 
      *                vtt-volume-dec-t not = 0
      *                and vtt-volume-dec-per-t not = 0 and
      *                ef-flusso-c-buf not = spaces
      *                set errori to true
      *                display message box
      *                 "Offset per lunghezza volume intero obbligatorio"
      *                        title = titolo
      *                        icon  2
      *                move 78-ID-ef-vol-int-per to control-id
      *             end-if
      *          end-if
      *     when 78-ID-ef-vol-dec-per
      *          if controllo-finale
      *             inquire ef-vol-dec, value in ef-vol-dec-buf
      *             move ef-vol-dec-buf to vtt-volume-dec-t
      *             inquire ef-vol-dec-per, value in ef-vol-dec-per-buf
      *             move ef-vol-dec-per-buf to vtt-volume-dec-per-t
      *             inquire ef-vol-int, value in ef-vol-int-buf
      *             move ef-vol-int-buf to vtt-volume-int-t
      *             inquire ef-vol-int-per, value in ef-vol-int-per-buf
      *             move ef-vol-int-per-buf to vtt-volume-int-per-t
      *             if vtt-volume-dec-t not = 0 and
      *                vtt-volume-dec-per-t = 0 and
      *                ef-flusso-c-buf not = spaces
      *                set errori to true
      *                display message box
      *               "Offset per lunghezza volume decimale obbligatorio"
      *                        title = titolo
      *                        icon  2
      *             end-if
      *             if vtt-volume-dec-t = 0 and
      *                vtt-volume-dec-per-t not = 0 and
      *                ef-flusso-c-buf not = spaces
      *                set errori to true
      *                display message box
      *                        "Offset per volume decimale obbligatorio"
      *                        title = titolo
      *                        icon  2
      *                move 78-ID-ef-vol-dec to control-id
      *             end-if
      *             if vtt-volume-int-t = 0 and vtt-volume-dec-t not = 0
      *                and vtt-volume-dec-per-t not = 0 and
      *                ef-flusso-c-buf not = spaces
      *                set errori to true
      *                display message box
      *                        "Offset per volume intero obbligatorio"
      *                        title = titolo
      *                        icon  2
      *                move 78-ID-ef-vol-int to control-id
      *             end-if
      *             if vtt-volume-int-per-t = 0 and 
      *                vtt-volume-dec-t not = 0
      *                and vtt-volume-dec-per-t not = 0 and
      *                ef-flusso-c-buf not = spaces
      *                set errori to true
      *                display message box
      *                 "Offset per lunghezza volume intero obbligatorio"
      *                        title = titolo
      *                        icon  2
      *                move 78-ID-ef-vol-int-per to control-id
      *             end-if
      *          end-if
      *     when 78-ID-ef-vol-dec-righe
      *          if controllo-finale
      *             inquire ef-vol-dec-righe, value ef-vol-dec-righe-buf
      *             move ef-vol-dec-righe-buf to vtt-volume-dec-r
      *             inquire ef-vol-dec-per-righe, 
      *                                value in ef-vol-dec-per-righe-buf
      *             move ef-vol-dec-per-righe-buf to vtt-volume-dec-per-r
      *             inquire ef-vol-int-righe, value ef-vol-int-righe-buf
      *             move ef-vol-int-righe-buf to vtt-volume-int-r
      *             inquire ef-vol-int-per-righe, 
      *                                value in ef-vol-int-per-righe-buf
      *             move ef-vol-int-per-righe-buf to vtt-volume-int-per-r
      *             if vtt-volume-dec-r = 0 and
      *                vtt-volume-dec-per-r not = 0 and
      *                ef-flusso-c-buf not = spaces
      *                set errori to true
      *                display message box
      *                        "Offset per volume decimale obbligatorio"
      *                        title = titolo
      *                        icon  2
      *             end-if
      *             if vtt-volume-int-r = 0 and vtt-volume-dec-r not = 0
      *                and vtt-volume-dec-per-r not = 0 and
      *                ef-flusso-c-buf not = spaces
      *                set errori to true
      *                display message box
      *                        "Offset per volume intero obbligatorio"
      *                        title = titolo
      *                        icon  2
      *                move 78-ID-ef-vol-int-righe to control-id
      *             end-if
      *             if vtt-volume-int-per-r = 0 and 
      *                vtt-volume-dec-r not = 0
      *                and vtt-volume-dec-per-r not = 0 and
      *                ef-flusso-c-buf not = spaces
      *                set errori to true
      *                display message box
      *                 "Offset per lunghezza volume intero obbligatorio"
      *                        title = titolo
      *                        icon  2
      *                move 78-ID-ef-vol-int-per-righe to control-id
      *             end-if
      *          end-if
      *     when 78-ID-ef-vol-dec-per-righe
      *          if controllo-finale
      *             inquire ef-vol-dec-righe, value ef-vol-dec-righe-buf
      *             move ef-vol-dec-righe-buf to vtt-volume-dec-r
      *             inquire ef-vol-dec-per-righe, 
      *                                value in ef-vol-dec-per-righe-buf
      *             move ef-vol-dec-per-righe-buf to vtt-volume-dec-per-r
      *             inquire ef-vol-int-righe, value ef-vol-int-righe-buf
      *             move ef-vol-int-righe-buf to vtt-volume-int-r
      *             inquire ef-vol-int-per-righe, 
      *                                value in ef-vol-int-per-righe-buf
      *             move ef-vol-int-per-righe-buf to vtt-volume-int-per-r
      *             if vtt-volume-dec-r not = 0 and
      *                vtt-volume-dec-per-r = 0 and
      *                ef-flusso-c-buf not = spaces
      *                set errori to true
      *                display message box
      *               "Offset per lunghezza volume decimale obbligatorio"
      *                        title = titolo
      *                        icon  2
      *
      *             end-if
      *             if vtt-volume-dec-r = 0 and
      *                vtt-volume-dec-per-r not = 0 and
      *                ef-flusso-c-buf not = spaces
      *                set errori to true
      *                display message box
      *                        "Offset per volume decimale obbligatorio"
      *                        title = titolo
      *                        icon  2
      *                move 78-ID-ef-vol-dec-righe to control-id
      *             end-if
      *             if vtt-volume-int-r = 0 and vtt-volume-dec-r not = 0
      *                and vtt-volume-dec-per-r not = 0 and
      *                ef-flusso-c-buf not = spaces
      *                set errori to true
      *                display message box
      *                        "Offset per volume intero obbligatorio"
      *                        title = titolo
      *                        icon  2
      *                move 78-ID-ef-vol-int-righe to control-id
      *             end-if
      *             if vtt-volume-int-per-r = 0 and 
      *                vtt-volume-dec-r not = 0
      *                and vtt-volume-dec-per-r not = 0 and
      *                ef-flusso-c-buf not = spaces
      *                set errori to true
      *                display message box
      *                 "Offset per lunghezza volume intero obbligatorio"
      *                        title = titolo
      *                        icon  2
      *                move 78-ID-ef-vol-int-per-righe to control-id
      *             end-if
      *          end-if
      *     when 78-ID-ef-peso-dec
      *          if controllo-finale
      *             inquire ef-peso-dec, value in ef-peso-dec-buf
      *             move ef-peso-dec-buf to vtt-peso-dec-t
      *             inquire ef-peso-dec-per, value in ef-peso-dec-per-buf
      *             move ef-peso-dec-per-buf to vtt-peso-dec-per-t
      *             inquire ef-peso-int, value in ef-peso-int-buf
      *             move ef-peso-int-buf to vtt-peso-int-t
      *             inquire ef-peso-int-per, value in ef-vol-int-per-buf
      *             move ef-peso-int-per-buf to vtt-peso-int-per-t
      *             if vtt-peso-dec-t = 0 and vtt-peso-dec-per-t not = 0
      *                and ef-flusso-c-buf not = spaces
      *                set errori to true
      *                display message box
      *                        "Offset per peso decimale obbligatorio"
      *                        title = titolo
      *                        icon  2
      *             end-if
      *             if vtt-peso-int-t = 0 and vtt-peso-dec-t not = 0
      *                and vtt-peso-dec-per-t not = 0 and
      *                ef-flusso-c-buf not = spaces
      *                set errori to true
      *                display message box
      *                        "Offset per peso intero obbligatorio"
      *                        title = titolo
      *                        icon  2
      *                move 78-ID-ef-peso-int to control-id
      *             end-if
      *             if vtt-peso-int-per-t = 0 and vtt-peso-dec-t not = 0
      *                and vtt-peso-dec-per-t not = 0 and
      *                ef-flusso-c-buf not = spaces
      *                set errori to true
      *                display message box
      *                   "Offset per lunghezza peso intero obbligatorio"
      *                        title = titolo
      *                        icon  2
      *                move 78-ID-ef-peso-int-per to control-id
      *             end-if
      *          end-if
      *     when 78-ID-ef-peso-dec-per
      *          if controllo-finale
      *             inquire ef-peso-dec, value in ef-peso-dec-buf
      *             move ef-peso-dec-buf to vtt-peso-dec-t
      *             inquire ef-peso-dec-per, value in ef-peso-dec-per-buf
      *             move ef-peso-dec-per-buf to vtt-peso-dec-per-t
      *             inquire ef-peso-int, value in ef-peso-int-buf
      *             move ef-peso-int-buf to vtt-peso-int-t
      *             inquire ef-peso-int-per, value in ef-vol-int-per-buf
      *             move ef-peso-int-per-buf to vtt-peso-int-per-t
      *             if vtt-peso-dec-t not = 0 and vtt-peso-dec-per-t = 0
      *                and ef-flusso-c-buf not = spaces
      *                set errori to true
      *                display message box
      *                 "Offset per lunghezza peso decimale obbligatorio"
      *                        title = titolo
      *                        icon  2
      *             end-if
      *             if vtt-peso-dec-t = 0 and vtt-peso-dec-per-t not = 0
      *                and ef-flusso-c-buf not = spaces
      *                set errori to true
      *                display message box
      *                        "Offset per peso decimale obbligatorio"
      *                        title = titolo
      *                        icon  2
      *                move 78-ID-ef-peso-dec to control-id
      *             end-if
      *             if vtt-peso-int-t = 0 and vtt-peso-dec-t not = 0
      *                and vtt-peso-dec-per-t not = 0 and
      *                ef-flusso-c-buf not = spaces
      *                set errori to true
      *                display message box
      *                        "Offset per peso intero obbligatorio"
      *                        title = titolo
      *                        icon  2
      *                move 78-ID-ef-peso-int to control-id
      *             end-if
      *             if vtt-peso-int-per-t = 0 and vtt-peso-dec-t not = 0
      *                and vtt-peso-dec-per-t not = 0 and
      *                ef-flusso-c-buf not = spaces
      *                set errori to true
      *                display message box
      *                   "Offset per lunghezza peso intero obbligatorio"
      *                        title = titolo
      *                        icon  2
      *                move 78-ID-ef-peso-int-per to control-id
      *             end-if
      *          end-if
      *     when 78-ID-ef-peso-dec-righe
      *          if controllo-finale
      *             inquire ef-peso-dec-righe, 
      *                                value in ef-peso-dec-righe-buf
      *             move ef-peso-dec-righe-buf to vtt-peso-dec-r
      *             inquire ef-peso-dec-per-righe, 
      *                             value in ef-peso-dec-per-righe-buf
      *             move ef-peso-dec-per-righe-buf to vtt-peso-dec-per-r
      *             inquire ef-peso-int-righe, 
      *                                   value in ef-peso-int-righe-buf
      *             move ef-peso-int-righe-buf to vtt-peso-int-r
      *             inquire ef-peso-int-per-righe, 
      *                             value in ef-peso-int-per-righe-buf
      *             move ef-peso-int-per-righe-buf to vtt-peso-int-per-r
      *             if vtt-peso-dec-r = 0 and vtt-peso-dec-per-r not = 0
      *                and ef-flusso-c-buf not = spaces
      *                set errori to true
      *                display message box
      *                        "Offset per peso decimale obbligatorio"
      *                        title = titolo
      *                        icon  2
      *             end-if
      *             if vtt-peso-int-r = 0 and vtt-peso-dec-r not = 0
      *                and vtt-peso-dec-per-r not = 0 and
      *                ef-flusso-c-buf not = spaces
      *                set errori to true
      *                display message box
      *                        "Offset per peso intero obbligatorio"
      *                        title = titolo
      *                        icon  2
      *                move 78-ID-ef-peso-int-righe to control-id
      *             end-if
      *             if vtt-peso-int-per-r = 0 and vtt-peso-dec-r not = 0
      *                and vtt-peso-dec-per-r not = 0 and
      *                ef-flusso-c-buf not = spaces
      *                set errori to true
      *                display message box
      *                   "Offset per lunghezza peso intero obbligatorio"
      *                        title = titolo
      *                        icon  2
      *                move 78-ID-ef-peso-int-per-righe to control-id
      *             end-if
      *          end-if
      *     when 78-ID-ef-peso-dec-per-righe
      *          if controllo-finale
      *             inquire ef-peso-dec-righe, 
      *                                      value ef-peso-dec-righe-buf
      *             move ef-peso-dec-righe-buf to vtt-peso-dec-r
      *             inquire ef-peso-dec-per-righe, 
      *                             value in ef-peso-dec-per-righe-buf
      *             move ef-peso-dec-per-righe-buf to vtt-peso-dec-per-r
      *             inquire ef-peso-int-righe, 
      *                                   value in ef-peso-int-righe-buf
      *             move ef-peso-int-righe-buf to vtt-peso-int-r
      *             inquire ef-peso-int-per-righe, 
      *                                value in ef-peso-int-per-righe-buf
      *             move ef-peso-int-per-righe-buf to vtt-peso-int-per-r
      *             if vtt-peso-dec-r not = 0 and vtt-peso-dec-per-r = 0
      *                and ef-flusso-c-buf not = spaces
      *                set errori to true
      *                display message box
      *                 "Offset per lunghezza peso decimale obbligatorio"
      *                        title = titolo
      *                        icon  2
      *             end-if
      *             if vtt-peso-dec-r = 0 and vtt-peso-dec-per-r not = 0
      *                and ef-flusso-c-buf not = spaces
      *                set errori to true
      *                display message box
      *                        "Offset per peso decimale obbligatorio"
      *                        title = titolo
      *                        icon  2
      *                move 78-ID-ef-peso-dec-righe to control-id
      *             end-if
      *             if vtt-peso-int-r = 0 and vtt-peso-dec-r not = 0
      *                and vtt-peso-dec-per-r not = 0 and
      *                ef-flusso-c-buf not = spaces
      *                set errori to true
      *                display message box
      *                        "Offset per peso intero obbligatorio"
      *                        title = titolo
      *                        icon  2
      *                move 78-ID-ef-peso-int-righe to control-id
      *             end-if
      *             if vtt-peso-int-per-r = 0 and vtt-peso-dec-r not = 0
      *                and vtt-peso-dec-per-r not = 0 and
      *                ef-flusso-c-buf not = spaces
      *                set errori to true
      *                display message box
      *                   "Offset per lunghezza peso intero obbligatorio"
      *                        title = titolo
      *                        icon  2
      *                move 78-ID-ef-peso-int-per-righe to control-id
      *             end-if
      *          end-if
      *     when 78-ID-ef-scad-gg
      *          if controllo-finale
      *             inquire ef-scad-gg,     value in ef-scad-gg-buf
      *             move ef-scad-gg-buf to vtt-data-scad-gg
      *             inquire ef-scad-gg-per, value in ef-scad-gg-per-buf
      *             move ef-scad-gg-per-buf to vtt-data-scad-gg-per
      *             inquire ef-scad-mm,     value in ef-scad-mm-buf
      *             move ef-scad-mm-buf to vtt-data-scad-mm
      *             inquire ef-scad-mm-per, value in ef-scad-mm-per-buf
      *             move ef-scad-mm-per-buf to vtt-data-scad-mm-per
      *             inquire ef-scad-aa,     value in ef-scad-aa-buf
      *             move ef-scad-aa-buf to vtt-data-scad-aa
      *             inquire ef-scad-aa-per, value in ef-scad-aa-per-buf
      *             move ef-scad-aa-per-buf to vtt-data-scad-aa-per
      *             if vtt-data-scad-gg = 0 and 
      *                vtt-data-scad-gg-per not = 0
      *                set errori to true
      *                display message box
      *                        "Offset per data di scadenza obbligatorio"
      *                        title = titolo
      *                        icon = 2
      *                move 78-ID-ef-scad-gg to control-id
      *             end-if
      *             if vtt-data-scad-gg = 0 and vtt-data-scad-mm not = 0
      *                set errori to true
      *                display message box
      *                        "Offset per data di scadenza obbligatorio"
      *                        title = titolo
      *                        icon = 2
      *                move 78-ID-ef-scad-gg to control-id
      *             end-if
      *             if vtt-data-scad-gg = 0 and vtt-data-scad-aa not = 0
      *                set errori to true
      *                display message box
      *                        "Offset per data di scadenza obbligatorio"
      *                        title = titolo
      *                        icon = 2
      *                move 78-ID-ef-scad-gg to control-id
      *             end-if
      *          end-if
      *     when 78-ID-ef-scad-gg-per
      *          if controllo-finale
      *             inquire ef-scad-gg,     value in ef-scad-gg-buf
      *             move ef-scad-gg-buf to vtt-data-scad-gg
      *             inquire ef-scad-gg-per, value in ef-scad-gg-per-buf
      *             move ef-scad-gg-per-buf to vtt-data-scad-gg-per
      *             inquire ef-scad-mm,     value in ef-scad-mm-buf
      *             move ef-scad-mm-buf to vtt-data-scad-mm
      *             inquire ef-scad-mm-per, value in ef-scad-mm-per-buf
      *             move ef-scad-mm-per-buf to vtt-data-scad-mm-per
      *             inquire ef-scad-aa,     value in ef-scad-aa-buf
      *             move ef-scad-aa-buf to vtt-data-scad-aa
      *             inquire ef-scad-aa-per, value in ef-scad-aa-per-buf
      *             move ef-scad-aa-per-buf to vtt-data-scad-aa-per
      *             if vtt-data-scad-gg not = 0 and 
      *                vtt-data-scad-gg-per = 0
      *                set errori to true
      *                display message box
      *              "Offset per lunghezza data di scadenza obbligatorio"
      *                        title = titolo
      *                        icon = 2
      *                move 78-ID-ef-scad-gg-per to control-id
      *             end-if
      *          end-if
      *     when 78-ID-ef-scad-mm
      *          if controllo-finale
      *             inquire ef-scad-gg,     value in ef-scad-gg-buf
      *             move ef-scad-gg-buf to vtt-data-scad-gg
      *             inquire ef-scad-gg-per, value in ef-scad-gg-per-buf
      *             move ef-scad-gg-per-buf to vtt-data-scad-gg-per
      *             inquire ef-scad-mm,     value in ef-scad-mm-buf
      *             move ef-scad-mm-buf to vtt-data-scad-mm
      *             inquire ef-scad-mm-per, value in ef-scad-mm-per-buf
      *             move ef-scad-mm-per-buf to vtt-data-scad-mm-per
      *             inquire ef-scad-aa,     value in ef-scad-aa-buf
      *             move ef-scad-aa-buf to vtt-data-scad-aa
      *             inquire ef-scad-aa-per, value in ef-scad-aa-per-buf
      *             move ef-scad-aa-per-buf to vtt-data-scad-aa-per
      *             if vtt-data-scad-mm = 0 and 
      *                vtt-data-scad-mm-per not = 0
      *                set errori to true
      *                display message box
      *                        "Offset per data di scadenza obbligatorio"
      *                        title = titolo
      *                        icon = 2
      *                move 78-ID-ef-scad-mm to control-id
      *             end-if
      *             if vtt-data-scad-mm = 0 and vtt-data-scad-gg not = 0
      *                set errori to true
      *                display message box
      *                        "Offset per data di scadenza obbligatorio"
      *                        title = titolo
      *                        icon = 2
      *                move 78-ID-ef-scad-mm to control-id
      *             end-if
      *             if vtt-data-scad-mm = 0 and vtt-data-scad-aa not = 0
      *                set errori to true
      *                display message box
      *                        "Offset per data di scadenza obbligatorio"
      *                        title = titolo
      *                        icon = 2
      *                move 78-ID-ef-scad-mm to control-id
      *             end-if
      *          end-if
      *     when 78-ID-ef-scad-mm-per
      *          if controllo-finale
      *             inquire ef-scad-gg,     value in ef-scad-gg-buf
      *             move ef-scad-gg-buf to vtt-data-scad-gg
      *             inquire ef-scad-gg-per, value in ef-scad-gg-per-buf
      *             move ef-scad-gg-per-buf to vtt-data-scad-gg-per
      *             inquire ef-scad-mm,     value in ef-scad-mm-buf
      *             move ef-scad-mm-buf to vtt-data-scad-mm
      *             inquire ef-scad-mm-per, value in ef-scad-mm-per-buf
      *             move ef-scad-mm-per-buf to vtt-data-scad-mm-per
      *             inquire ef-scad-aa,     value in ef-scad-aa-buf
      *             move ef-scad-aa-buf to vtt-data-scad-aa
      *             inquire ef-scad-aa-per, value in ef-scad-aa-per-buf
      *             move ef-scad-aa-per-buf to vtt-data-scad-aa-per
      *             if vtt-data-scad-mm not = 0 and 
      *                vtt-data-scad-mm-per = 0
      *                set errori to true
      *                display message box
      *              "Offset per lunghezza data di scadenza obbligatorio"
      *                        title = titolo
      *                        icon = 2
      *                move 78-ID-ef-scad-mm-per to control-id
      *             end-if
      *          end-if
      *     when 78-ID-ef-scad-aa
      *          if controllo-finale
      *             inquire ef-scad-gg,     value in ef-scad-gg-buf
      *             move ef-scad-gg-buf to vtt-data-scad-gg
      *             inquire ef-scad-gg-per, value in ef-scad-gg-per-buf
      *             move ef-scad-gg-per-buf to vtt-data-scad-gg-per
      *             inquire ef-scad-mm,     value in ef-scad-mm-buf
      *             move ef-scad-mm-buf to vtt-data-scad-mm
      *             inquire ef-scad-mm-per, value in ef-scad-mm-per-buf
      *             move ef-scad-mm-per-buf to vtt-data-scad-mm-per
      *             inquire ef-scad-aa,     value in ef-scad-aa-buf
      *             move ef-scad-aa-buf to vtt-data-scad-aa
      *             inquire ef-scad-aa-per, value in ef-scad-aa-per-buf
      *             move ef-scad-aa-per-buf to vtt-data-scad-aa-per
      *             if vtt-data-scad-aa = 0 and 
      *                vtt-data-scad-aa-per not = 0
      *                set errori to true
      *                display message box
      *                        "Offset per data di scadenza obbligatorio"
      *                        title = titolo
      *                        icon = 2
      *                move 78-ID-ef-scad-aa to control-id
      *             end-if
      *             if vtt-data-scad-aa = 0 and vtt-data-scad-gg not = 0
      *                set errori to true
      *                display message box
      *                        "Offset per data di scadenza obbligatorio"
      *                        title = titolo
      *                        icon = 2
      *                move 78-ID-ef-scad-aa to control-id
      *             end-if
      *             if vtt-data-scad-aa = 0 and vtt-data-scad-mm not = 0
      *                set errori to true
      *                display message box
      *                        "Offset per data di scadenza obbligatorio"
      *                        title = titolo
      *                        icon = 2
      *                move 78-ID-ef-scad-aa to control-id
      *             end-if
      *          end-if
      *     when 78-ID-ef-scad-aa-per
      *          if controllo-finale
      *             inquire ef-scad-gg,     value in ef-scad-gg-buf
      *             move ef-scad-gg-buf to vtt-data-scad-gg
      *             inquire ef-scad-gg-per, value in ef-scad-gg-per-buf
      *             move ef-scad-gg-per-buf to vtt-data-scad-gg-per
      *             inquire ef-scad-mm,     value in ef-scad-mm-buf
      *             move ef-scad-mm-buf to vtt-data-scad-mm
      *             inquire ef-scad-mm-per, value in ef-scad-mm-per-buf
      *             move ef-scad-mm-per-buf to vtt-data-scad-mm-per
      *             inquire ef-scad-aa,     value in ef-scad-aa-buf
      *             move ef-scad-aa-buf to vtt-data-scad-aa
      *             inquire ef-scad-aa-per, value in ef-scad-aa-per-buf
      *             move ef-scad-aa-per-buf to vtt-data-scad-aa-per
      *             if vtt-data-scad-aa not = 0 and 
      *                vtt-data-scad-aa-per = 0
      *                set errori to true
      *                display message box
      *              "Offset per lunghezza data di scadenza obbligatorio"
      *                        title = titolo
      *                        icon = 2
      *                move 78-ID-ef-scad-aa-per to control-id
      *             end-if
      *          end-if
           end-evaluate

           if errori
              perform CANCELLA-COLORE
              move CONTROL-ID to store-id
              move 4          to ACCEPT-CONTROL
           end-if.

      ***---
       CURRENT-RECORD.
           set tutto-ok   to true.
           set ReadSecca  to true.
      *     if mod = 1     
      *        read tvettori    lock 
      *           invalid 
      *              set errori to true 
      *        end-read
      *     else
              read tvettori no lock 
                 invalid 
                    set errori to true 
              end-read
      *     end-if.                    
           set ReadSecca to false.

           if RecLocked
              set RecLocked to false
              set errori    to true
           else
              if tutto-ok
                 if nuovo
                    move 0 to mod-txt
                              mod-csv
                    move 0 to mod           
                    move 1 to mod-k         
                    move 78-ID-ef-path to control-id    
                    move 4 to accept-control
                 end-if   
                 perform FORM1-CURR
                 set vecchio to true
                 if mod = 1
                    set StatusModifica to true
                 else                         
                    set StatusVisua    to true
                 end-if
      *           perform STATUS-BAR-MSG
              else
                 move 0 to mod
                           mod-txt
                           mod-csv
                 move 1 to mod-k
                 if vecchio
      *              perform CLEAR-SCREEN
                    set errori to true
                    if YesMessage    
                       move 78-ID-ef-path to control-id    
                       move 4 to accept-control
                       display message box MSG-Record-inesistente
                               title = tit-err
                               icon mb-warning-icon
                    end-if
                 end-if
              end-if
           end-if.

      ***---
      * DISPLAY-CAMPI.
      *     display ef-vol-dec           ef-peso-dec
      *             ef-vol-dec-per       ef-peso-dec-per
      *             ef-vol-dec-righe     ef-peso-dec-righe
      *             ef-vol-dec-per-righe ef-peso-dec-per-righe
      *             cbo-vol              cbo-peso
      *             cbo-vol-righe        cbo-peso-righe
      *             cbo-vol-DDT          cbo-peso-DDT
      *             cbo-peso-art-ord     cbo-peso-art
      *             cbo-letvet-peso      cbo-letvet-vol
      *             cbo-letvet-contrassegno.

      ***---
       SALVA.
           if MOD = zero 
              exit paragraph 
           end-if.

           set tutto-ok to true.

           |Setto a zero queste due variabili così il controllo
           |su alcuni campi della griglia non vengono RIFATTI
           |perchè già presi in considerazione precedentemente
           |con la pressione del tasto "Salva" relativo alla grid...
           move 0 to tot-righe.

           set controllo-finale to true.

           perform  varying control-id from 78-id-ef-sigla-pers by 1
                      until control-id > 78-id-cb-dt-giac
              perform CONTROLLO
              if errori
                 exit perform
              end-if
           end-perform.

           set controllo-finale to false.

           |...dopodiché riassegno a riga-nuova il suo valore originale,
           |mentre tot-righe può essre lasciato a 0 perchè
           |ricalcolato ogni volta che dev'essre testato

           if errori
              inquire Screen1-Ta-1, value in pagina
              if store-id >= min-id(pagina) and
                 store-id <= max-id(pagina)
                 move store-id to CONTROL-ID
              end-if
              |******
              move 4 to accept-control
           else
              perform FORM1-BUF-TO-FLD
              perform CANCELLA-COLORE

              accept como-ora from time
              if nuovo
                 move data-oggi to vtt-data-creazione

                 |Così appena dopo aver creato un record NUOVO
                 |metto a video la sua data di creazione
                 move vtt-data-creazione to como-data

                 move como-ora  to vtt-ora-creazione
                 move lk-user-codi to vtt-utente-creazione
              else
                 move data-oggi to vtt-data-ultima-modifica
                 move como-ora  to vtt-ora-ultima-modifica
                 move lk-user-codi to vtt-utente-ultima-modifica
              end-if
              rewrite vtt-rec
                 invalid
                    write vtt-rec
              end-write
           end-if.


      ***---
       AFTER-INIT-CODE.
           set tutto-ok   to true.
           set ControllaCampi to true
           move lk-mod to mod
           move lk-vet-codice to vet-codice
           read tvettori
              invalid
                 continue
           end-read
           move vet-codice      to work-cli-codice
           move vet-descrizione to work-cli-des.

      ***---
       SET-DEFAULT-CODE.
           move lk-vet-codice   to vtt-codice.
           read vettel no lock.
           move vtt-rec   to old-vtt-rec
           if mod = zero
              move zero   to mod-txt
                             mod-csv
           else
              if vtt-import-txt
                 move 1      to mod-txt
                                v-txt
                 move zero   to mod-csv
                 move "Da"   to tit-colonna-da
                 move "Per"  to tit-colonna-per
              else
                 move zero   to mod-txt
                                v-txt
                 move 1      to mod-csv
                 move "Colonna" to tit-colonna-da
                 move space     to tit-colonna-per
              end-if
           end-if.


      ***---
       BEFORE-ACCEPT-CODE.
           accept data-oggi from century-date.

           modify cb-dt-cons, item-to-add = 78-AAAAMMGG
           modify cb-dt-cons, item-to-add = 78-GGMMAAAA
           modify cb-dt-cons, item-to-add = 78-AAMMGG  
           modify cb-dt-cons, item-to-add = 78-GGMMAA  

           modify cb-dt-avviso, item-to-add = 78-AAAAMMGG
           modify cb-dt-avviso, item-to-add = 78-GGMMAAAA
           modify cb-dt-avviso, item-to-add = 78-AAMMGG  
           modify cb-dt-avviso, item-to-add = 78-GGMMAA  

           modify cb-dt-giac, item-to-add = 78-AAAAMMGG
           modify cb-dt-giac, item-to-add = 78-GGMMAAAA
           modify cb-dt-giac, item-to-add = 78-AAMMGG  
           modify cb-dt-giac, item-to-add = 78-GGMMAA  


           modify cb-ora-cons, item-to-add = 78-HHMM    
           modify cb-ora-cons, item-to-add = 78-HHMMSSCC
           modify cb-ora-cons, item-to-add = 78-HHMMSS  

           perform Screen1-Ta-1-Ev-Cmd-Tabchanged.
           perform SETTA-COMBO.

           move 0 to v-txt.
           display form1.

      ***---
       AFTER-BUFTOFLD-CODE.
           inquire cb-ora-cons,   value como-tipo-ora.
           evaluate como-tipo-ora
           when 78-HHMMSSCC
                set HHMMSSCC to true
           when 78-HHMMSS
                set HHMMSS   to true
           when 78-HHMM
                set HHMM     to true
           end-evaluate.
           move tipo-ora  to vtt-ora-cons-tipo

           inquire cb-dt-cons,   value como-tipo-data.
           evaluate como-tipo-data
           when 78-AAAAMMGG
                set AAAAMMGG to true
           when 78-GGMMAAAA
                set GGMMAAAA to true
           when 78-AAMMGG
                set AAMMGG   to true
           when 78-GGMMAA
                set GGMMAA   to true
           end-evaluate.
           move tipo-data    to vtt-data-cons-tipo

           inquire cb-dt-avviso, value como-tipo-data.
           evaluate como-tipo-data
           when 78-AAAAMMGG
                set AAAAMMGG to true
           when 78-GGMMAAAA
                set GGMMAAAA to true
           when 78-AAMMGG
                set AAMMGG   to true
           when 78-GGMMAA
                set GGMMAA   to true
           end-evaluate.
           move tipo-data    to vtt-data-avviso-tipo

           inquire cb-dt-giac, value como-tipo-data.
           evaluate como-tipo-data
           when 78-AAAAMMGG
                set AAAAMMGG to true
           when 78-GGMMAAAA
                set GGMMAAAA to true
           when 78-AAMMGG
                set AAMMGG   to true
           when 78-GGMMAA
                set GGMMAA   to true
           end-evaluate.
           move tipo-data    to vtt-data-ap-giac-tipo.

      ***---
       AFTER-FLDTOBUF-CODE.
           perform SETTA-COMBO.

      ***---
       SETTA-COMBO.
           move vtt-ora-cons-tipo  to tipo-ora
           evaluate true
           when HHMMSSCC
                move 78-HHMMSSCC to como-tipo-ora
           when HHMMSS
                move 78-HHMMSS   to como-tipo-ora
           when HHMM
                move 78-HHMM     to como-tipo-ora
           end-evaluate.
           modify cb-ora-cons,   value como-tipo-ora.

           move vtt-data-cons-tipo to tipo-data
           evaluate true
           when AAAAMMGG
                move 78-AAAAMMGG to como-tipo-data
           when GGMMAAAA
                move 78-GGMMAAAA to como-tipo-data
           when AAMMGG
                move 78-AAMMGG   to como-tipo-data
           when GGMMAA
                move 78-GGMMAA   to como-tipo-data
           end-evaluate.
           modify cb-dt-cons,   value como-tipo-data.


           move vtt-data-avviso-tipo to tipo-data
           evaluate true
           when AAAAMMGG
                move 78-AAAAMMGG to como-tipo-data
           when GGMMAAAA
                move 78-GGMMAAAA to como-tipo-data
           when AAMMGG
                move 78-AAMMGG   to como-tipo-data
           when GGMMAA
                move 78-GGMMAA   to como-tipo-data
           end-evaluate.
           modify cb-dt-avviso,   value como-tipo-data.

           move vtt-data-ap-giac-tipo to tipo-data
           evaluate true
           when AAAAMMGG
                move 78-AAAAMMGG to como-tipo-data
           when GGMMAAAA
                move 78-GGMMAAAA to como-tipo-data
           when AAMMGG
                move 78-AAMMGG   to como-tipo-data
           when GGMMAA
                move 78-GGMMAA   to como-tipo-data
           end-evaluate.
           modify cb-dt-giac,   value como-tipo-data.

      ***---  
       BEFORE-EXIT-CODE.
           perform SALV-MOD.


      ***---
       TABCHANGED-CODE.
           evaluate event-data-1
           when 1
                perform CHECK-PAGE-2
                if tutto-ok
                   move zero to v-txt
                   set ControllaCampi to true
                   |move 78-ID-ef-des  to control-id
                   |move 4 to accept-control
                else
                   move 2 to pagina
                   set ControllaCampi to false    
                   move 4             to ACCEPT-CONTROL
                   set NonCambiareTab to true
                end-if
           when 2
                perform CHECK-PAGE-1
                if tutto-ok
                   perform CANCELLA-COLORE
                   set ControllaCampi to true
      *             move 78-ID-gd-peso to control-id
      *             move 4             to accept-control
                    if Form1-RADIO-1-BUF = 1
                       move 1 to v-txt
                    else
                       move 0 to v-txt
                    end-if
                else
                   move 1             to pagina
                   set ControllaCampi to false    
                   move 4             to ACCEPT-CONTROL
                   set NonCambiareTab to true
                end-if
           end-evaluate.

      *
      *
      *     evaluate EVENT-DATA-1
      *     when 1
      *          perform CHECK-PAGE-2
      *          if tutto-ok
      *             perform CANCELLA-COLORE
      *             set ControllaCampi to true
      *             move 78-ID-ef-des  to control-id
      *             move 4 to accept-control
      *          else
      *             move 2 to pagina
      *             set ControllaCampi to false    
      *             move 4             to ACCEPT-CONTROL
      *             set NonCambiareTab to true
      *          end-if
      *     when 2
      *          perform CHECK-PAGE-1
      *          if tutto-ok
      *             perform CANCELLA-COLORE
      *             set ControllaCampi to true
      *             move 78-ID-gd-peso to control-id
      *             move 4             to accept-control
      *          else
      *             move 1             to pagina
      *             set ControllaCampi to false    
      *             move 4             to ACCEPT-CONTROL
      *             set NonCambiareTab to true
      *          end-if
      *     end-evaluate.


           display PAGE-2.


       SALV-MOD.
           set tutto-ok to true.
           perform FORM1-CONTROLLO-OLD.

           if NoSalvato
              display message box MSG-Salvare-le-modifiche
                            title titolo
                            type mb-yes-no-cancel 
                            giving scelta       
       
              evaluate scelta
              when mb-yes 
                   perform SALVA
              when mb-no  
                   continue
              when other
                   |modify control-handle, color = colore-or
                   set errori to true
                   move store-id   to CONTROL-ID    
                   move 4          to ACCEPT-CONTROL
              end-evaluate
           end-if 
           .

      ***---
       CHECK-PAGE-2.
           if mod = 0 
              exit paragraph 
           end-if.
           set tutto-ok to true.
           perform  varying control-id from 78-ID-Form1-DaCb-1 by 1
                      until control-id    > 78-ID-cb-dt-giac
              perform CONTROLLO
              if errori 
                 exit perform 
              end-if
           end-perform.

      ***---
       CHECK-PAGE-1.
           if mod = 0 
              exit paragraph 
           end-if.
           set tutto-ok to true.
           perform  varying control-id from 78-ID-ef-sigla-pers  by 1
                      until control-id    > 78-ID-ef-flusso
              perform CONTROLLO
              if errori 
                 exit perform 
              end-if
           end-perform.

      ***---
       CREA-CARTELLE.
           inquire ef-path   value mkv-path-environment
           inquire ef-tmp    value mkv-path-suff-tmp
           inquire ef-log    value mkv-path-suff-log
           inquire ef-orig   value mkv-path-suff-orig
           inquire ef-dest   value mkv-path-suff-dest
           inquire ef-err    value mkv-path-suff-err
           inquire ef-invio  value mkv-path-suff-invio
           
           call   "mkdir-vet" using mkdir-vettel-linkage 
           cancel "mkdir-vet".


      ***---
       PAR-COPY.
           copy "color-custom.cpy".
           Copy "utydata.cpy".


