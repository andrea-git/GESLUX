      ***---
       STATUS-BAR-MSG.
           evaluate true
           when StatusModifica
                modify form1-st-1-handle, 
                       panel-index  3,
                       panel-text  "MODIFICA"
           when StatusVisua
                modify form1-st-1-handle, 
                       panel-index  3,
                       panel-text  "VISUALIZZAZIONE"
                move 0 to StatusHelp
                perform STATUS-HELP
           when StatusIns
                modify form1-st-1-handle, 
                       panel-index  3,
                       panel-text  "INSERIMENTO"
           when other
                modify form1-st-1-handle, 
                       panel-index  2,
                       panel-text   spaces
           end-evaluate.

      ***---
       STATUS-HELP.
           if StatusHelp = 1
              modify Form1-St-1-Handle, 
                     panel-index = 2, 
                     panel-text "F8 HELP record presenti"
              move BitmapZoomEnabled to BitmapNumZoom
           else
              modify Form1-St-1-Handle, 
                     panel-index = 2, 
                     panel-text "F9 Selezione altro record"
              move BitmapZoomDisabled to BitmapNumZoom
           end-if.
           move StatusHelp to e-cerca.
           modify tool-cerca, enabled = e-cerca.
           modify tool-cerca, bitmap-number = BitmapNumZoom.
