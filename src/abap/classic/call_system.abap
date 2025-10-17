report z_appl_exec_example.

DATA: request TYPE REF TO IF_HTTP_REQUEST.
DATA: ld_comline    TYPE c,
        ld_param    TYPE c,
        command     TYPE c,
        com         TYPE com,
        ld_status   LIKE extcmdexex-status,
        ld_output   LIKE btcxpm OCCURS 0 WITH HEADER LINE,
        ld_subrc    LIKE sy-subrc.

ld_comline = request->get_form_field( 'ld_comline' ).
ld_param = request->get_form_field( 'ld_param' ).

DATA: BEGIN OF TABL OCCURS 0,
  LINE(560),
END OF TABL.
REFRESH TABL.

CONCATENATE ld_comline ld_param INTO command SEPARATED BY ' '.

* dataflow Command Injection
CALL 'SYSTEM' ID 'COMMAND' FIELD ld_comline ID 'TAB' FIELD TABL[].

* dataflow Command Injection
CALL 'SYSTEM' ID 'COMMAND' FIELD command ID 'TAB' FIELD TABL[].

* structural Command Injection
CALL 'SYSTEM' ID 'COMMAND' FIELD com ID 'TAB' FIELD TABL[].

* semantic Command Injection
CALL 'SYSTEM' ID 'COMMAND' FIELD '/foo' ID 'TAB' FIELD TABL[].
