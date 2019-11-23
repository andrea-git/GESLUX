      ***---
       DIREZIONA-IMPEGNATO.
           accept GdoInUsoFlag from environment "GDO_IN_USO".
           if GdoInUso
              move cli-tipo to tcl-codice
              read ttipocli no lock
              evaluate true
              when tcl-evasione-GDO
                   set link-imp-GDO to true
              when tcl-evasione-TRAD
              when tcl-evasione-ESTERO
                   set link-imp-TRAD to true
              end-evaluate
           else
               set link-imp-MASTER to true
           end-if.
