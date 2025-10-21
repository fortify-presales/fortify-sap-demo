REPORT z_appl_log_example.

DATA: request TYPE REF TO IF_HTTP_REQUEST,
      g_s_log      TYPE bal_s_log,
      g_log_handle TYPE balloghndl,
      log_msg      TYPE bal_s_msg,
      val          TYPE c.

val = request->get_form_field( 'val' ).

log_msg-msgid = 'XY'.
log_msg-msgty = 'E'.
log_msg-msgno = '123'.
log_msg-msgv1 = 'VAL: '.
log_msg-msgv2 = val.

CALL FUNCTION 'BAL_LOG_CREATE'
    EXPORTING
      I_S_LOG                       = g_s_log
   IMPORTING
     E_LOG_HANDLE               = g_log_handle
    EXCEPTIONS
      LOG_HEADER_INCONSISTENT       = 1
      OTHERS                        = 2.

* dataflow Log Forging
CALL FUNCTION 'BAL_LOG_MSG_ADD'
    EXPORTING
     I_LOG_HANDLE           = g_log_handle
      I_S_MSG                   = val
    EXCEPTIONS
      LOG_NOT_FOUND             = 1
      MSG_INCONSISTENT          = 2
      LOG_IS_FULL               = 3
      OTHERS                    = 4.
