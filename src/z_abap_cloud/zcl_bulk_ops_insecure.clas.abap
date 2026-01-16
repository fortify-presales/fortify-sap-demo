" Note: ZFY_TRAVEL table is assumed to exist in the system and released for cloud use.

CLASS zcl_bulk_ops_insecure DEFINITION
  PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS delete_all.
    METHODS bulk_update_no_where.
    METHODS delete_with_dynamic_where
      IMPORTING VALUE(iv_filter) TYPE string.
    METHODS do_implicit_commit.
ENDCLASS.

CLASS zcl_bulk_ops_insecure IMPLEMENTATION.

  METHOD delete_all.
    " Bulk Operation Without Restriction
    DELETE FROM zfy_travel.
  ENDMETHOD.

  METHOD bulk_update_no_where.
    " Bulk Operation Without Restriction
    UPDATE zfy_travel SET internal_comment = 'Processed'.
  ENDMETHOD.

  METHOD delete_with_dynamic_where.
    " Insecure: SQL injection via iv_filter, e.g. `travel_id = 'X' OR 1 = 1`
    " Not supported to ABAP Cloud, only for on-premise systems
    DATA(lv_where) = iv_filter.
    DELETE FROM zfy_travel WHERE (lv_where).
  ENDMETHOD.

  METHOD do_implicit_commit.
    " Insecure: commits from within a utility method
    " Not supported to ABAP Cloud, only for on-premise systems
    COMMIT WORK AND WAIT.
  ENDMETHOD.

ENDCLASS.
