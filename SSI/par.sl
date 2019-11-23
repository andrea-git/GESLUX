     select   optional PAR    assign to disk
              lock is manual with rollback
              organization indexed
              access dynamic
              file status file-status
              record key             Key-0 = par-codice      of data-record                           | scamar 11/05/2011 12:28:16
              alternate record  key  key-1 = par-codice-pnr  of data-record                           | scamar 11/05/2011 13:52:57
                                             par-codice      of data-record                           | scamar 11/05/2011 12:28:17
              alternate record  key  key-2 = par-codice-pas  of data-record                           | scamar 11/05/2011 13:52:58
                                             par-codice      of data-record.                          | scamar 11/05/2011 12:28:17
