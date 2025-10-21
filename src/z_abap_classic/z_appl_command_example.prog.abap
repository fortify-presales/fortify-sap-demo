report z_appl_command_example.

DATA: request TYPE REF TO IF_HTTP_REQUEST.
DATA: ld_comline  LIKE sxpgcolist-name,
      com         LIKE sxpgcolist-name,
      ld_cd       LIKE sxpgcolist-name,
        ld_param    LIKE sxpgcolist-parameters,
        ld_status   LIKE extcmdexex-status,
        ld_output   LIKE btcxpm OCCURS 0 WITH HEADER LINE,
        ld_subrc    LIKE sy-subrc.
DATA lv_returncode TYPE i.

  REFRESH ld_output.
  MOVE 'FTP_DATA_IN' to ld_comline.

ld_comline = request->get_form_field( 'ld_comline' ).

* structural Command Injection
  CALL FUNCTION 'SXPG_COMMAND_EXECUTE'
       EXPORTING
            commandname                   = com
       IMPORTING
            status                        = ld_status
       TABLES
            exec_protocol                 = ld_output
       EXCEPTIONS
            no_permission                 = 1
            command_not_found             = 2
            parameters_too_long           = 3
            security_risk                 = 4
            wrong_check_call_interface    = 5
            program_start_error           = 6
            program_termination_error     = 7
            x_error                       = 8
            parameter_expected            = 9
            too_many_parameters           = 10
            illegal_command               = 11
            wrong_asynchronous_parameters = 12
            cant_enq_tbtco_entry          = 13
            jobcount_generation_error     = 14
            OTHERS                        = 15.

* dataflow Command Injection
  CALL FUNCTION 'SXPG_COMMAND_EXECUTE'
       EXPORTING
            commandname                   = ld_comline
       IMPORTING
            status                        = ld_status
       TABLES
            exec_protocol                 = ld_output
       EXCEPTIONS
            no_permission                 = 1
            command_not_found             = 2
            parameters_too_long           = 3
            security_risk                 = 4
            wrong_check_call_interface    = 5
            program_start_error           = 6
            program_termination_error     = 7
            x_error                       = 8
            parameter_expected            = 9
            too_many_parameters           = 10
            illegal_command               = 11
            wrong_asynchronous_parameters = 12
            cant_enq_tbtco_entry          = 13
            jobcount_generation_error     = 14
            OTHERS                        = 15.

* dataflow Command Injection
CALL FUNCTION 'SXPG_CALL_SYSTEM'
  EXPORTING
    commandname =               ld_comline
  IMPORTING
    status =                    ld_status
  TABLES
    exec_protocol =             ld_output
  EXCEPTIONS
    NO_PERMISSION = 1
    COMMAND_NOT_FOUND = 2
    PARAMETERS_TOO_LONG = 3
    SECURITY_RISK = 4
    WRONG_CHECK_CALL_INTERFACE = 5
    PROGRAM_START_ERROR = 6
    PROGRAM_TERMINATION_ERROR = 7
    X_ERROR = 8
    PARAMETER_EXPECTED = 9
    TOO_MANY_PARAMETERS = 10
    ILLEGAL_COMMAND = 11.

* structural Command Injection
CALL FUNCTION 'SXPG_CALL_SYSTEM'
  EXPORTING
    commandname =               'foo'
  IMPORTING
    status =                    ld_status
  TABLES
    exec_protocol =             ld_output
  EXCEPTIONS
    NO_PERMISSION = 1
    COMMAND_NOT_FOUND = 2
    PARAMETERS_TOO_LONG = 3
    SECURITY_RISK = 4
    WRONG_CHECK_CALL_INTERFACE = 5
    PROGRAM_START_ERROR = 6
    PROGRAM_TERMINATION_ERROR = 7
    X_ERROR = 8
    PARAMETER_EXPECTED = 9
    TOO_MANY_PARAMETERS = 10
    ILLEGAL_COMMAND = 11.

* semantic Command Injection
CALL FUNCTION 'SXPG_CALL_SYSTEM'
  EXPORTING
    commandname =               'C:/foo'
  IMPORTING
    status =                    ld_status
  TABLES
    exec_protocol =             ld_output
  EXCEPTIONS
    NO_PERMISSION = 1
    COMMAND_NOT_FOUND = 2
    PARAMETERS_TOO_LONG = 3
    SECURITY_RISK = 4
    WRONG_CHECK_CALL_INTERFACE = 5
    PROGRAM_START_ERROR = 6
    PROGRAM_TERMINATION_ERROR = 7
    X_ERROR = 8
    PARAMETER_EXPECTED = 9
    TOO_MANY_PARAMETERS = 10
    ILLEGAL_COMMAND = 11.

* semantic Command Injection
CALL FUNCTION 'SXPG_CALL_SYSTEM'
  EXPORTING
    commandname =               '/foo'
  IMPORTING
    status =                    ld_status
  TABLES
    exec_protocol =             ld_output
  EXCEPTIONS
    NO_PERMISSION = 1
    COMMAND_NOT_FOUND = 2
    PARAMETERS_TOO_LONG = 3
    SECURITY_RISK = 4
    WRONG_CHECK_CALL_INTERFACE = 5
    PROGRAM_START_ERROR = 6
    PROGRAM_TERMINATION_ERROR = 7
    X_ERROR = 8
    PARAMETER_EXPECTED = 9
    TOO_MANY_PARAMETERS = 10
    ILLEGAL_COMMAND = 11.

* semantic Command Injection
CALL FUNCTION 'SXPG_CALL_SYSTEM'
  EXPORTING
    commandname =               '\foo'
  IMPORTING
    status =                    ld_status
  TABLES
    exec_protocol =             ld_output
  EXCEPTIONS
    NO_PERMISSION = 1
    COMMAND_NOT_FOUND = 2
    PARAMETERS_TOO_LONG = 3
    SECURITY_RISK = 4
    WRONG_CHECK_CALL_INTERFACE = 5
    PROGRAM_START_ERROR = 6
    PROGRAM_TERMINATION_ERROR = 7
    X_ERROR = 8
    PARAMETER_EXPECTED = 9
    TOO_MANY_PARAMETERS = 10
    ILLEGAL_COMMAND = 11.

* structural Command Injection
CALL FUNCTION 'SXPG_CALL_SYSTEM'
  EXPORTING
    commandname =               '${foo}/foo'
  IMPORTING
    status =                    ld_status
  TABLES
    exec_protocol =             ld_output
  EXCEPTIONS
    NO_PERMISSION = 1
    COMMAND_NOT_FOUND = 2
    PARAMETERS_TOO_LONG = 3
    SECURITY_RISK = 4
    WRONG_CHECK_CALL_INTERFACE = 5
    PROGRAM_START_ERROR = 6
    PROGRAM_TERMINATION_ERROR = 7
    X_ERROR = 8
    PARAMETER_EXPECTED = 9
    TOO_MANY_PARAMETERS = 10
    ILLEGAL_COMMAND = 11.

* semantic Obsolete
* dataflow Command Injection
CALL FUNCTION 'WS_EXECUTE'
  EXPORTING
    cd = ld_cd
    commandline = ld_param
    program = ld_comline
  EXCEPTIONS
    FRONTEND_ERROR = 1
    NO_BATCH = 2
    PROG_NOT_FOUND = 3
    ILLEGAL_OPTION = 4.

* semantic Obsolete
* dataflow Command Injection
CALL FUNCTION 'GUI_EXEC'
  EXPORTING
    command =                   ld_comline
    parameter =                 ld_param
  IMPORTING
    returncode =                lv_returncode.

* dataflow Command injection
CALL FUNCTION 'GUI_RUN'
  EXPORTING
    command =                   ld_comline
    parameter =                 ld_param
    cd =                        ld_cd
  IMPORTING
    returncode =                lv_returncode.
