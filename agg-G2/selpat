           select   optional PAT    
              assign to disk
              lock is manual with rollback
              organization indexed
              access dynamic
              file status file-status
              record key             Key-0 = pat-codice             
                                             of record-pat
              alternate record  key  key-1 = pat-codice-conto       
                                             of record-pat
                                             pat-data-riferimento   
                                             of record-pat
                                             pat-numero-riferimento 
                                             of record-pat
                                             pat-codice             
                                             of record-pat
              alternate record  key  key-2 = pat-data-riferimento   
                                             of record-pat
                                             pat-codice-conto       
                                             of record-pat
                                             pat-codice             
                                             of record-pat.
