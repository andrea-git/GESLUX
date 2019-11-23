      ***---
       ABILITA-TOOLBAR.
           evaluate livello-abil
           when 1 
                move zero to e-nuovo
                             e-cancella
                             e-salva
                             e-modifica
                 move BitmapNewDisabled    to BitmapNumNew
                 move BitmapDeleteDisabled to BitmapNumDelete
                 move BitmapSaveDisabled   to BitmapNumSave
                 move BitmapEditDisabled   to BitmapNumEdit

           when 2 
                move 1    to e-nuovo
                             e-salva
                             e-modifica
                move zero to e-cancella
                move BitmapNewEnabled    to BitmapNumNew
                move BitmapSaveEnabled   to BitmapNumSave
                move BitmapEditEnabled   to BitmapNumEdit
                
                move BitmapDeleteDisabled to BitmapNumDelete

           when 3 
                move 1    to e-nuovo
                             e-cancella
                             e-salva
                             e-modifica

                move BitmapNewEnabled    to BitmapNumNew
                move BitmapSaveEnabled   to BitmapNumSave
                move BitmapEditEnabled   to BitmapNumEdit
                move BitmapDeleteEnabled to BitmapNumDelete
           end-evaluate.

           modify tool-nuovo,    enabled = e-nuovo
           modify tool-cancella, enabled = e-cancella
           modify tool-salva,    enabled = e-salva
           modify tool-modifica, enabled = e-modifica
                  

           if e-stampa = 1
              move BitmapPrintEnabled to BitmapNumPrint
              accept stampante-anteprima
                             from environment "STAMPANTE_ANTEPRIMA"
              if stampante-anteprima = space
                 move 0 to e-anteprima
                 move BitmapPreviewDisabled to BitmapNumPreview
              else
                 move 1 to e-anteprima
                 move BitmapPreviewEnabled to BitmapNumPreview
              end-if
           else
              move 0 to e-anteprima                              
              move BitmapPreviewDisabled to BitmapNumPreview
              move BitmapPrintDisabled to BitmapNumPrint
           end-if.    
           modify tool-anteprima, enabled = e-anteprima.
           modify tool-stampa,    enabled = e-stampa.
           modify tool-anteprima, bitmap-number = BitmapNumPreview.
           modify tool-stampa,    bitmap-number = BitmapNumPrint.  

           modify tool-nuovo,    bitmap-number = BitmapNumNew.
           modify tool-modifica, bitmap-number = BitmapNumEdit.
           modify tool-salva,    bitmap-number = BitmapNumSave.
           modify tool-cancella, bitmap-number = BitmapNumDelete.

      *     MODIFY CST-BLOCKPGM, VISIBLE = V-CUSTOM.
      *     MODIFY CST-COLOR,    VISIBLE = V-CUSTOM.
