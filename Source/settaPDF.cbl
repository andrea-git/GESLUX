       IDENTIFICATION DIVISION.
       PROGRAM-ID.  settaPDF.

       Working-Storage Section.
       copy "Acugui.def".

       77  open-key-handle         usage unsigned-long.
       77  sam-desired             usage unsigned-long.
       77  subkey-handle           usage unsigned-long.

       77  data-type               usage unsigned-long.
       77  data-size               usage unsigned-long.
       77  subkey-to-be-opened     pic x(256).
       77  value-data              pic x(256).
       77  value-name              pic x(256).
       77  status-code             pic s9.

       Linkage Section.
           copy "link-settaPDF.def".

      *****************************************************************
       PROCEDURE DIVISION using settaPDF-linkage.

       MainLanc.
      *    aperture del percorso del registro
           perform APERTURA-PERCORSO-REGISTRO

           if settaPDF-setta
              if status-code = zero
                 move "AutosaveFilename" to value-name
                 initialize VALUE-DATA
                 move settaPDF-nome-file to VALUE-DATA
                 perform SCRIVI-CHIAVE
              end-if

              if status-code = zero
                 move "AutosaveDirectory"   to value-name
                 initialize VALUE-DATA
                 inspect settaPDF-percorso 
                             replacing trailing space by low-value
                 string settaPDF-percorso   delimited by low-value
                        "\"                 delimited by size
                        into VALUE-DATA
                 perform SCRIVI-CHIAVE
              end-if
           end-if

           if status-code = zero
              move "UseAutosaveDirectory" to value-name
              initialize VALUE-DATA
              if settaPDF-setta
                 move "1"   to VALUE-DATA
              else
                 move "0"   to VALUE-DATA
              end-if                 
              perform SCRIVI-CHIAVE
           end-if.

           if status-code = zero
              move "UseAutosave" to value-name
              initialize VALUE-DATA
              if settaPDF-setta
                 move "1"   to VALUE-DATA
              else
                 move "0"   to VALUE-DATA
              end-if                 
              perform SCRIVI-CHIAVE
           end-if.


           if status-code = zero
              set settaPDF-OK   to true
           else
              set settaPDF-OK   to false
           end-if.


           perform CHIUDI-PERCORSO-REGISTRO

           goback.

      ***---
       SCRIVI-CHIAVE.
           move REG_SZ to data-type
           
           inspect VALUE-DATA replacing trailing space by low-value
           initialize data-size
           inspect value-data tallying data-size 
                                   for characters before low-value


           Call "DISPLAY_REG_SET_VALUE_EX" USING OPEN-KEY-HANDLE,
                                         data-type
                                         VALUE-DATA(1:data-size),
                                         DATA-SIZE,
                                         value-name
                                  GIVING STATUS-CODE.
           if status-code not = 0
              perform DISPLAY-MSG-BOX-WRITE
           end-if.


      ***---
       APERTURA-PERCORSO-REGISTRO.
           move HKEY_CURRENT_USER     to open-key-handle.
           move KEY_ALL_ACCESS        to sam-desired.
           move "Software\PDFCreator\Program" to subkey-to-be-opened.
           perform APRI-CHIAVE.
           move subkey-handle      to open-key-handle.|value-name.

      ***---
       CHIUDI-PERCORSO-REGISTRO.
           move value-name  to subkey-handle.
           perform CHIUDI-CHIAVE.

      ***---
       APRI-CHIAVE.
           inspect subkey-to-be-opened replacing trailing spaces
                                       by low-values.
           CALL "DISPLAY_REG_OPEN_KEY_EX"  using open-key-handle
                                         subkey-to-be-opened
                                         sam-desired
                                         subkey-handle
                                  giving status-code.
           if status-code not = 0
              perform DISPLAY-MSG-BOX
           end-if.

      ***---
       CHIUDI-CHIAVE.
           CALL "DISPLAY_REG_CLOSE_KEY"  using open-key-handle
                                 giving status-code.

      ***---
       DISPLAY-MSG-BOX.
           display message box "Impossibile aprire la chiave"
                               x"0D0A"
                               subkey-to-be-opened
                   title "settaPDF".

      ***---
       DISPLAY-MSG-BOX-WRITE.
           display message box "Impossibile scrivere la voce"
                               x"0D0A"
                               value-name
                   title "settaPDF".

