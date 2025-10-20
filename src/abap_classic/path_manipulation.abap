report z_open_example.

DATA: request TYPE REF TO IF_HTTP_REQUEST.
DATA: epsf      TYPE epsf.
DATA: file      TYPE c.
DATA: mtime     TYPE p DECIMALS 0.
DATA: dir(50).
DATA: DSN(20) VALUE '/usr/test', 
      RECORD(80),
      MSG(100).

DSN = request->get_form_field( 'dsn' ).
file = request->get_form_field( 'file' ).

* dataflow Path Manipulation
OPEN DATASET DSN FOR INPUT MESSAGE MSG.

* dataflow System Information Leak
WRITE: / MSG.
 
DO. 
* dataflow Path Manipulation
  READ DATASET DSN INTO RECORD. 
  WRITE: / RECORD. 
ENDDO. 
CLOSE DATASET DSN.

* semantic Obsolete
CALL FUNCTION 'REGISTRY_GET'
  EXPORTING
     KEY =                      'a\b\c'
  IMPORTING
     VALUE =                     dir.

* 2 dataflow Path Manipulation
CALL FUNCTION 'EPS_GET_FILE_ATTRIBUTES' "
  EXPORTING
    file_name =                 file
    dir_name =                  dir
  IMPORTING
    file_size =                 epsf-epsfilsiz
    file_owner =                epsf-epsfilown
    file_mode =                 epsf-epsfilmod
    file_type =                 epsf-epsfiltyp
    file_mtime =                mtime
  EXCEPTIONS
    READ_DIRECTORY_FAILED = 1
    READ_ATTRIBUTES_FAILED = 2.
