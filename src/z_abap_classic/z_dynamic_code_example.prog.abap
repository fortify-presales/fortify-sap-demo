REPORT z_dynamic_code_example.

TABLES rlgrap.

PARAMETER:       p_file(200) TYPE c,
                 p_temp(30)  TYPE c DEFAULT 'Z123TEMP_REPORT_FOR_CODE'.

DATA: it_tab TYPE filetable,
      gd_subrc TYPE i,
      answer TYPE c.

TYPES: BEGIN OF t_abapcode,
  row(72) TYPE c,
 END OF t_abapcode.
DATA: it_abapcode TYPE STANDARD TABLE OF t_abapcode INITIAL SIZE 0,
      it_store    TYPE STANDARD TABLE OF t_abapcode INITIAL SIZE 0.

DATA: filename TYPE  string.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  REFRESH: it_tab.
  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title     = 'Select File'
      default_filename = '*.txt'
      multiselection   = ' '
    CHANGING
      file_table       = it_tab
      rc               = gd_subrc.

  LOOP AT it_tab INTO p_file.
  ENDLOOP.

  filename = p_file.

  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename                = filename
    TABLES
      data_tab                = it_abapcode
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      OTHERS                  = 17.

  READ REPORT p_temp INTO it_store.
  INSERT REPORT p_temp FROM it_abapcode.

* dataflow Code Injection
  SUBMIT (p_temp) AND RETURN.

* dataflow Resource Injection
  DELETE REPORT p_temp.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
    text_question  = 'Report used to store temporary code already exists!!! Do you want to overwrite it?'
   IMPORTING
     answer                      = answer.

     IF answer EQ '1'.
       INSERT REPORT p_temp FROM it_abapcode.

* dataflow Code Injection
       SUBMIT (p_temp) AND RETURN.

* dataflow Resource Injection
       DELETE REPORT p_temp.
     ENDIF.
