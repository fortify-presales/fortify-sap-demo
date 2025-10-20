*&---------------------------------------------------------------------*
*& Report  YHP_FORTIFY_SCA
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  yhp_fortify_sca.

*P_PKG : package
*P_PRGNAM: program
*P_FPR : The location of the fpr file eg: usr/share/result.fpr
*PBG:   Process in Background - checkbox
*
*P_TEMP:  Working Directory eg: c:\Temp\Fortify
*P_BLD :  Build-ID
*P_TRANS: Translation parameters
*P_SCAN :        Scan parameters
TYPE-POOLS: seop,
            abap,
            swbse,
            so2,
            icon .

TYPES  g_file TYPE rlgrap-filename.
TYPES: BEGIN OF type_bsp_config,
         applname TYPE o2applname,
         pagekey  TYPE  o2page,
         design   TYPE string,
         encode   TYPE string,
       END OF type_bsp_config.

DATA: clsname             TYPE  seoclsname,
      result              TYPE  seop_methods_w_include,
      result_line         TYPE LINE OF  seop_methods_w_include,
      version_text        TYPE  string VALUE 'Fortify ABAP Source Code Extractor. Version 900041',
      recomment_text      TYPE  string VALUE 'We highly recommend to turn off security options in your SAP-GUI',
      abap_filename_ext   TYPE  char4  VALUE 'abap',
      debug_prefix        TYPE  string     VALUE '',  "*dbg*
      flg_dbg_level       TYPE  i          VALUE 2,
      max_call_depth      TYPE  i          VALUE 0,
      max_fungrps         TYPE  i          VALUE 1000,
      fname               TYPE  string,
      lt_reposrc          TYPE  SORTED TABLE OF reposrc WITH UNIQUE KEY progname r3state WITH HEADER LINE,
      arg_file_name       TYPE  string, " name of file that contains the list of downloaded files, used as argument to sourceanalyzer invocation for translate step
      pkgs                TYPE  TABLE OF tdevc-devclass WITH HEADER LINE,
      pkg                 LIKE  tadir-obj_name,
      prog                LIKE  trdir,
      pkg_progs           TYPE TABLE OF tadir          WITH HEADER LINE, "data pkg_progs      TYPE TABLE OF tadir-obj_name WITH HEADER LINE.
      customer_ns         TYPE string VALUE '/0CUST/', " this namespace denotes that the object is in customer range
      ls_progdir          TYPE  progdir,
      lt_tdevc            TYPE TABLE OF tdevc, " the table of TDEVC entries whose constituents need to be downloaded.
      matching_progs      TYPE TABLE OF trdir WITH HEADER LINE,
      lt_global_functions LIKE TABLE OF tadir-obj_name,
      l_string            TYPE  string,                     "#EC NEEDED
      l_path              TYPE  string,
      l_fullpath          TYPE  string,
      l_user_action       TYPE  i,
      l_encod             TYPE  abap_encoding,
      l_filelength        TYPE  i,
      l_source            TYPE  swbse_max_line_tab,
      len                 TYPE  i,
      count               TYPE  i,
      lv_char3            TYPE  char4,
      dynp_fields         TYPE TABLE OF dynpread WITH HEADER LINE,
      g_fprpath           TYPE  string,
      g_tmpdir            TYPE  string,
      g_fileseperator     TYPE  c,
      downloaded_files    TYPE TABLE OF  string,
      work_dir            TYPE  string,
      date                LIKE  sy-datum,
      time                LIKE  sy-uzeit,
      all_progs           TYPE TABLE OF trdir-name,
      tmp_downloaded_file TYPE  string,
      progname_tab        LIKE TABLE OF trdir-name     WITH HEADER LINE,
      pkg_tab             LIKE TABLE OF tadir-obj_name WITH HEADER LINE,
      fprname_tab         TYPE TABLE OF string         WITH HEADER LINE,
      prog_tab            LIKE STANDARD TABLE OF trdir-name     WITH HEADER LINE,
      ev_prefix           TYPE  trnspace-namespace,
      ev_prefix_name      TYPE  trnspace-namespace,
      ev_stem             LIKE  e071-obj_name,
      ddir                TYPE  string,
      lv_obj_name         TYPE  e071-obj_name,
      lv_result           TYPE  match_result,
      lt_o2taglib         TYPE TABLE OF o2taglib,
      lt_o2tag            TYPE TABLE OF o2tag,
      sca_dir_arg         TYPE  string,
      sca_fpr_arg         TYPE  string,
      cmdstring           TYPE  string,
      sync                TYPE  string VALUE 'X',
      lv_info             TYPE  string,
      lv_zipfile          TYPE  string,
      s_bsp_config        TYPE  type_bsp_config,
      t_bsp_config        TYPE  STANDARD TABLE OF type_bsp_config,
      gv_is_user_mod      TYPE abap_bool VALUE abap_false.

FIELD-SYMBOLS: <downloaded_file> TYPE string,
               <default_fpr>     TYPE         string.

TABLES: trdir,
        tdevc,
        o2appl,
        wdy_component,
        sscrfields.

SELECTION-SCREEN FUNCTION KEY 1.

SELECTION-SCREEN BEGIN OF SCREEN 40 AS WINDOW.
  SELECTION-SCREEN   BEGIN OF LINE.
    SELECTION-SCREEN     COMMENT (83) htext1.
  SELECTION-SCREEN   END   OF LINE.
  SELECTION-SCREEN   BEGIN OF LINE.
    SELECTION-SCREEN     COMMENT (83) htext2.
  SELECTION-SCREEN   END   OF LINE.
  SELECTION-SCREEN   BEGIN OF LINE.
    SELECTION-SCREEN     COMMENT (83) htext3.
  SELECTION-SCREEN   END   OF LINE.
SELECTION-SCREEN END   OF SCREEN 40.


SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (60) vtitle.
SELECTION-SCREEN END   OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (83) vreco.
SELECTION-SCREEN END OF LINE.


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS : p_copt   FOR tdevc-dlvunit .                                            " no-DISPLAY
  SELECT-OPTIONS : p_pkg    FOR tdevc-devclass .                                           " LIKE pkg_tab
  SELECT-OPTIONS : p_prgnam FOR trdir-name                   MATCHCODE OBJECT y_progname . " like progname_tab, " MATCHCODE OBJECT progname_tab.
  SELECT-OPTIONS : p_wapa   FOR o2appl-applname              MATCHCODE OBJECT y_wapa .     " BSP-Application
SELECTION-SCREEN END   OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS : p_fpr   TYPE string LOWER CASE,     " OBLIGATORY,   " fpr-file path
               p_temp  TYPE string LOWER CASE,  " OBLIGATORY,   " base directory
               p_bld   TYPE string LOWER CASE,  " OBLIGATORY,   " build ID
               p_trans TYPE string LOWER CASE,  "               " translation paramters
               p_scan  TYPE string LOWER CASE,  "               " scan parameters
               p_zipn  TYPE string LOWER CASE,  "               " zip-file path
               p_depth TYPE i      DEFAULT 1 .  "               " max call chain depth to download
SELECTION-SCREEN END   OF BLOCK b2.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (70) vreco2.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
  PARAMETERS : p_down TYPE checkbox DEFAULT 'X' AS CHECKBOX USER-COMMAND down_checked,   " should download SAP program elements?
               p_buld TYPE checkbox DEFAULT '' AS CHECKBOX USER-COMMAND build_checked,  " should translate such elements?
               p_scn  TYPE checkbox DEFAULT '' AS CHECKBOX USER-COMMAND scn_checked,  " should scan such elements?
               p_awb  TYPE checkbox DEFAULT ''  AS CHECKBOX USER-COMMAND awb_checked,  " should invoke awb on the generated fpr file?
               p_zip  TYPE checkbox DEFAULT ''  AS CHECKBOX USER-COMMAND zip_checked.  " should zip all files into a .zip file?
  SELECTION-SCREEN SKIP .
  PARAMETERS : p_sap TYPE checkbox DEFAULT '' AS CHECKBOX.
SELECTION-SCREEN END   OF BLOCK b3.

AT SELECTION-SCREEN.
  CASE sscrfields-ucomm.
    WHEN 'FC01'.
      CLEAR fname.
      CLEAR p_copt.
      CLEAR p_pkg.
      CLEAR p_prgnam.
      CLEAR p_wapa.
      CLEAR p_fpr.
      CLEAR p_temp.
      CLEAR p_bld.
      CLEAR p_trans.
      CLEAR p_scan.
      CLEAR p_zipn.
      p_depth = 1.
      p_down = abap_true.
      p_buld = abap_false.
      p_scn = abap_false.
      p_awb = abap_false.
      p_zip = abap_false.
      p_sap = abap_false.
  ENDCASE.

AT SELECTION-SCREEN OUTPUT.

  IF fname IS INITIAL.
    CONCATENATE sy-datum '_' sy-uzeit INTO fname.
  ENDIF.

  IF p_zip = abap_true.
    p_down = abap_true.
    IF p_zipn IS INITIAL.
      CONCATENATE fname '.zip' INTO p_zipn.
    ENDIF.
  ENDIF.

  IF p_buld = abap_true.
    IF p_bld IS INITIAL.
      CONCATENATE 'build_' fname INTO p_bld.
    ENDIF.
  ENDIF.

  IF p_temp IS INITIAL.
    CONCATENATE ddir g_fileseperator 'build' fname INTO p_temp.
  ENDIF.

  IF p_scn = abap_true.
    IF p_fpr IS INITIAL.
      CONCATENATE p_temp g_fileseperator fname '.fpr' INTO p_fpr.
    ENDIF.
    p_buld = abap_true.
  ENDIF.

  LOOP AT SCREEN.

    CASE screen-name.
      WHEN 'P_COPT-LOW' OR 'P_COPT-HIGH'.
        IF p_pkg IS NOT INITIAL OR p_prgnam IS NOT INITIAL OR p_wapa IS NOT INITIAL.
          screen-active = 0.
        ENDIF.
      WHEN 'P_PKG-LOW' OR 'P_PKG-HIGH'.
        IF p_copt IS NOT INITIAL OR p_prgnam IS NOT INITIAL OR p_wapa IS NOT INITIAL.
          screen-active = 0.
        ENDIF.
      WHEN 'P_PRGNAM-LOW' OR 'P_PRGNAM-HIGH'.
        IF p_copt IS NOT INITIAL OR p_pkg IS NOT INITIAL OR p_wapa IS NOT INITIAL.
          screen-active = 0.
        ENDIF.
      WHEN 'P_WAPA-LOW' OR 'P_WAPA-HIGH'.
        IF p_copt IS NOT INITIAL OR p_pkg IS NOT INITIAL OR p_prgnam IS NOT INITIAL.
          screen-active = 0.
        ENDIF.
    ENDCASE.

    IF p_depth > 5.
      vreco2 = '[Warning] A depth greater than 5 can result in a long processing time!'.
    ELSE.
      vreco2 = ''.
    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fpr.

  CLEAR: dynp_fields, dynp_fields[].
  dynp_fields-fieldname = 'P_FPR'.
  dynp_fields-fieldvalue = p_fpr.
  APPEND dynp_fields.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname     = sy-cprog
      dynumb     = sy-dynnr
    TABLES
      dynpfields = dynp_fields.

  LOOP AT dynp_fields.
    g_fprpath = dynp_fields-fieldvalue.
    EXIT.
  ENDLOOP.

  DATA  l_filename  TYPE string.
  CLEAR l_filename.

  cl_gui_frontend_services=>file_save_dialog(
                                              EXPORTING
                                                with_encoding     = abap_true
                                                default_extension = 'fpr'
                                              CHANGING
                                                filename          = l_filename
                                                file_encoding     = l_encod
                                                path              = l_path
                                                fullpath          = l_fullpath
                                                user_action       = l_user_action
                                              EXCEPTIONS
                                                OTHERS            = 1
                                            ).

  IF sy-subrc <> 0 OR l_user_action = cl_gui_frontend_services=>action_cancel.

  ELSE.
    CLEAR: dynp_fields, dynp_fields[].
    dynp_fields-fieldname = 'P_FPR'.
    dynp_fields-fieldvalue =  l_fullpath   .
    APPEND dynp_fields.
    CALL FUNCTION 'DYNP_VALUES_UPDATE'
      EXPORTING
        dyname               = sy-cprog
        dynumb               = sy-dynnr
      TABLES
        dynpfields           = dynp_fields
      EXCEPTIONS
        invalid_abapworkarea = 1
        invalid_dynprofield  = 2
        invalid_dynproname   = 3
        invalid_dynpronummer = 4
        invalid_request      = 5
        no_fielddescription  = 6
        undefind_error       = 7
        OTHERS               = 8.
    IF sy-subrc <> 0.
      MESSAGE ID  sy-msgid TYPE sy-msgty  NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 .
    ENDIF.
  ENDIF.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_temp.

  CLEAR: dynp_fields, dynp_fields[].
  dynp_fields-fieldname = 'P_TEMP'.
  dynp_fields-fieldvalue = p_temp.
  APPEND dynp_fields.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname     = sy-cprog
      dynumb     = sy-dynnr
    TABLES
      dynpfields = dynp_fields.

  LOOP AT dynp_fields.
    g_tmpdir = dynp_fields-fieldvalue.
    EXIT.
  ENDLOOP.

  CALL METHOD cl_gui_frontend_services=>directory_browse
    EXPORTING
      window_title         = 'Get working directory'
      initial_folder       = g_tmpdir
    CHANGING
      selected_folder      = g_tmpdir
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4.

  IF sy-subrc <> 0.
    MESSAGE ID  sy-msgid TYPE sy-msgty  NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 .
  ELSE.

    CLEAR: dynp_fields, dynp_fields[].
    dynp_fields-fieldname = 'P_TEMP'.
    dynp_fields-fieldvalue = g_tmpdir.
    APPEND dynp_fields.

    CALL FUNCTION 'DYNP_VALUES_UPDATE'
      EXPORTING
        dyname               = sy-cprog
        dynumb               = sy-dynnr
      TABLES
        dynpfields           = dynp_fields
      EXCEPTIONS
        invalid_abapworkarea = 1
        invalid_dynprofield  = 2
        invalid_dynproname   = 3
        invalid_dynpronummer = 4
        invalid_request      = 5
        no_fielddescription  = 6
        undefind_error       = 7
        OTHERS               = 8.

    IF sy-subrc <> 0.
      MESSAGE ID  sy-msgid TYPE sy-msgty  NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 .
    ENDIF.

  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_depth.

  IF p_depth GT 5.
    MESSAGE 'A chain depth more then 5 can result in a very long download time!' TYPE 'I'.
  ENDIF.

AT SELECTION-SCREEN ON HELP-REQUEST FOR p_depth.
  htext1 = 'Defines the maximal call chain depth.'.
  htext2 = 'Every object in the call chain will be exported.'.
  htext3 = 'A chain depth more then 5 can result in a very long download time!'.
  CALL SELECTION-SCREEN '0040'.

AT SELECTION-SCREEN ON HELP-REQUEST FOR p_fpr.
  htext1 = 'Path to Fortify project file.'.
  htext2 = 'The project file will be created on this path.'.
  htext3 = 'It will be generated if no path is given and the "Scan" option is selected.'.
  CALL SELECTION-SCREEN '0040'.

AT SELECTION-SCREEN ON HELP-REQUEST FOR p_temp.
  htext1 = 'Path to working directory.'.
  htext2 = 'All objects will be extracted to this folder.'.
  htext3 = 'It will be generated if not path is given and the "Download" option is selected.'.
  CALL SELECTION-SCREEN '0040'.

AT SELECTION-SCREEN ON HELP-REQUEST FOR p_bld.
  htext1 = 'The name of the build.'.
  htext2 = 'It will be generated if no path is given and the "Build" option is selected.'.
  htext3 = ''.
  CALL SELECTION-SCREEN '0040'.

AT SELECTION-SCREEN ON HELP-REQUEST FOR p_zipn.
  htext1 = 'The name of the zip file.'.
  htext2 = 'All objects will be zipped into this file.'.
  htext3 = 'It will be generated if no name is given and the "Create ZIP File" option is selected.'.
  CALL SELECTION-SCREEN '0040'.

AT SELECTION-SCREEN ON HELP-REQUEST FOR p_trans.
  htext1 = 'Translation parameters'.
  htext2 = 'You can add additional translation parameters according to fortify manual.'.
  htext3 = 'The parameter "-b" is always set automatically.'.
  CALL SELECTION-SCREEN '0040'.

AT SELECTION-SCREEN ON HELP-REQUEST FOR p_scan.
  htext1 = 'Scan parameters'.
  htext2 = 'You can add additional scan parameters according to fortify manual.'.
  htext3 = 'The parameters "-scan -b -f" are always set automatically.'.
  CALL SELECTION-SCREEN '0040'.

INITIALIZATION.

  sscrfields-functxt_01 = 'New'.

  WHILE ddir IS INITIAL.
    cl_gui_frontend_services=>get_desktop_directory( CHANGING desktop_directory = ddir ).
  ENDWHILE.
  vreco = recomment_text.
  vtitle = version_text.
  CALL METHOD cl_gui_frontend_services=>get_file_separator
    CHANGING
      file_separator       = g_fileseperator
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4.
  IF sy-subrc <> 0.
    MESSAGE ID  sy-msgid TYPE sy-msgty  NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 .
  ENDIF.


START-OF-SELECTION.

  REFRESH: lt_tdevc, pkg_progs , matching_progs .

* some sanity-checks that coherent selections were made

  IF p_copt IS INITIAL  AND  p_pkg IS INITIAL  AND  p_prgnam IS INITIAL  AND  p_wapa IS INITIAL.
    MESSAGE 'You must specify at least one of: a component, a package, a program, or a BSP application' TYPE 'I'.
    RETURN.
  ENDIF.

  IF p_temp IS INITIAL.
    MESSAGE 'No download folder was specified.' TYPE 'I'.
    RETURN.
  ENDIF.

  IF p_buld = 'X'  AND p_bld IS INITIAL AND NOT p_scn = 'X' .
    MESSAGE 'You must specify a BuildID if you request that downloded files be translated but not scanned.' TYPE 'I'.
    RETURN.
  ENDIF.

  IF p_scn = 'X' AND p_fpr IS INITIAL.
    MESSAGE 'You must specify an FPR file name if you request an SCA scan.' TYPE 'I'.
    RETURN.
  ENDIF.

  max_call_depth = p_depth.
  IF flg_dbg_level > 2 .
    WRITE: / 'max_call_depth is ', max_call_depth.
  ENDIF.

* "software-components" are essentially collections of packages.  The selection-screen variable
* p_copt is a table of the selected software-component names.  We select all the elements
* of a software-component by looping through all the packages and pulling out the
* ones whose DVLUNIT field is in the set of such specified by the table p_copt.
*
  IF p_copt IS NOT INITIAL.
    SELECT * FROM tdevc INTO TABLE lt_tdevc WHERE dlvunit IN p_copt .
  ENDIF.
*
* tdevc is the table of packages.  tadir is the table of repository objects.
* Each repository object has a package field that names the package it belongs to.
*
* In turn packages are collections of
* function groups, classes/interfaces, programs(aka reports), type-pools, database tables, BSPs, etc.
  IF p_pkg IS NOT INITIAL.

    DATA:           lt_tmp_tdevc  TYPE TABLE OF tdevc.
    FIELD-SYMBOLS: <ls_tmp_tdevc> TYPE          tdevc.

    " can p_pkg[] NOT be initial even if p_pkg IS initial ?
    IF NOT p_pkg[] IS INITIAL.
      SELECT * FROM tdevc INTO TABLE lt_tmp_tdevc WHERE devclass IN  p_pkg[] .
    ENDIF.

    LOOP AT lt_tmp_tdevc ASSIGNING <ls_tmp_tdevc> .
      PERFORM collect_pkg_members TABLES pkgs USING <ls_tmp_tdevc>-devclass.
    ENDLOOP.

*   LOOP AT pkgs INTO pkg.
*     SELECT obj_name FROM tadir APPENDING TABLE pkg_progs WHERE devclass = pkg.
*   ENDLOOP.

    IF NOT pkgs[] IS INITIAL.
      SELECT * FROM tadir INTO TABLE pkg_progs FOR ALL ENTRIES IN pkgs[] WHERE devclass = pkgs-table_line .
    ENDIF.

  ENDIF.

  IF NOT lt_tdevc[] IS INITIAL.
    SELECT * FROM tadir APPENDING TABLE pkg_progs FOR ALL ENTRIES IN lt_tdevc[] WHERE devclass = lt_tdevc-devclass .
*     since there is no code to compute the subpackages of elements in lt_tdevc, this implies that
*     if pkg P is an element of software-component X, and pkg PP is a subpackage of P
*     then PP is also an element of X.
  ENDIF.

  IF p_prgnam IS NOT INITIAL.
    " prog = p_prgnam .
    SELECT * FROM tadir   APPENDING TABLE pkg_progs   WHERE  obj_name IN p_prgnam  .
    SELECT * FROM reposrc   INTO    TABLE lt_reposrc
*                                          for all entries in pkg_progs[]
*                                                    where PROGNAME ne pkg_progs-obj_name and
                                                     WHERE progname IN p_prgnam  AND  r3state = 'A'  .
    LOOP AT pkg_progs.
      DELETE lt_reposrc WHERE progname = pkg_progs-obj_name.
    ENDLOOP.
  ENDIF.

  IF p_wapa IS NOT INITIAL.
    WRITE / p_wapa.
    SELECT * FROM tadir APPENDING TABLE pkg_progs  WHERE pgmid = 'R3TR'  AND  object = 'WAPA'  AND  obj_name IN p_wapa .
  ENDIF.


*  IF p_wdyn IS NOT INITIAL.
*    SELECT * FROM tadir APPENDING table pkg_progs  WHERE pgmid = 'R3TR' AND object = 'WDYN' AND obj_name IN p_wdyn .
* ENDIF.

  LOOP AT  lt_reposrc.
    CLEAR pkg_progs.
    pkg_progs-obj_name = lt_reposrc-progname.
    pkg_progs-pgmid    = 'R3TR' .
    pkg_progs-object   = 'PROG' .
    APPEND pkg_progs.
  ENDLOOP.

  SORT pkg_progs .
  DELETE ADJACENT DUPLICATES FROM pkg_progs COMPARING pgmid object obj_name.

  PERFORM download_metadata USING 'TADIR'    p_temp  'TADIR'    pkg_progs[]  'MTD' .
  PERFORM download_metadata USING 'TDEVC'    p_temp  'TDEVC'    lt_tdevc     'MTD'.

  LOOP AT pkg_progs.
    prog-name = pkg_progs-obj_name.
    IF  flg_dbg_level > 2 .
      WRITE: / prog-name.
    ENDIF.

*    clear prog_pattern.
*    CONCATENATE prog '%' INTO prog_pattern.
*    SELECT name FROM trdir into TABLE matching_progs WHERE name LIKE prog_pattern .


    CASE pkg_progs-object.

      WHEN 'ENHO'.
        PERFORM get_enhancement USING pkg_progs-obj_name.

      WHEN 'SMOD'.
        PERFORM get_user_exits USING pkg_progs-obj_name.

      WHEN 'CLAS'.

        clsname = pkg_progs-obj_name.

        PERFORM get_all_class_includes  TABLES  matching_progs  USING clsname .


      WHEN 'FUGR'.

        IF pkg_progs-obj_name(1) = '/'. "partnernamespace etc.

          lv_obj_name = pkg_progs-obj_name .

          CALL FUNCTION 'TRINT_SPLIT_OBJECT'
            EXPORTING
*             IV_OBJECT      =
              iv_obj_name    = lv_obj_name
            IMPORTING
              ev_prefix      = ev_prefix
              ev_prefix_name = ev_prefix_name
              ev_stem        = ev_stem
            EXCEPTIONS
              invalid_prefix = 1
              OTHERS         = 2.
          IF sy-subrc <> 0.
            MESSAGE ID  sy-msgid TYPE sy-msgty  NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 .
          ENDIF.

          CONCATENATE ev_prefix 'SAPL' ev_stem  INTO matching_progs-name.
          APPEND      matching_progs.

        ELSE.

          CONCATENATE 'SAPL' pkg_progs-obj_name INTO matching_progs-name.
          APPEND matching_progs .

        ENDIF.


      WHEN 'PROG' .

        matching_progs-name = pkg_progs-obj_name .
        APPEND matching_progs.

      WHEN 'WDYN'.

        PERFORM download_wdyn USING pkg_progs-obj_name 0.

      WHEN 'WAPA'.

        DATA: lt_o2appl   TYPE TABLE OF o2appl.
        DATA: lt_o2pagdir TYPE TABLE OF o2pagdir .
        DATA: lt_o2pagpar TYPE TABLE OF o2pagpar.
        DATA: lt_o2pagevh TYPE TABLE OF o2pagevh.
        DATA:
          lt_source  TYPE  rswsourcet,
          ls_source  TYPE string,
          lt_bsp_ext TYPE  rswsourcet,
          ls_bspext  TYPE o2tlibid,
          lt_bspext  TYPE TABLE OF o2tlibid,
          ls_bsp_ext TYPE string,
          lt_maptab  TYPE so2_cross_mapinfo
          .

        FIELD-SYMBOLS: <ls_o2pagevh>   TYPE         o2pagevh .
        FIELD-SYMBOLS: <ls_o2taglib>   TYPE o2taglib.
        FIELD-SYMBOLS: <ls_o2tag>      TYPE o2tag.

        REFRESH: lt_o2appl,
                 lt_o2pagdir,
                 lt_o2pagpar,
                 lt_o2pagevh,
                 lt_o2taglib,
                 lt_o2tag
        .

        SELECT   * FROM o2appl   INTO TABLE lt_o2appl   WHERE applname = pkg_progs-obj_name AND version = 'A' .
        IF sy-subrc = 0.
          SELECT * FROM o2pagdir INTO TABLE lt_o2pagdir WHERE applname = pkg_progs-obj_name  .
          SELECT * FROM o2pagpar INTO TABLE lt_o2pagpar WHERE applname = pkg_progs-obj_name  .
          SELECT * FROM o2pagevh INTO TABLE lt_o2pagevh WHERE applname = pkg_progs-obj_name  .
          "download additional info like applname, pagename, related implementing classes, evt-handler classes
        ENDIF.

        PERFORM download_metadata USING  pkg_progs-obj_name  p_temp  'O2APPL'    lt_o2appl    'MTD' .
        PERFORM download_metadata USING  pkg_progs-obj_name  p_temp  'O2PAGDIR'  lt_o2pagdir  'MTD' .
        PERFORM download_metadata USING  pkg_progs-obj_name  p_temp  'O2PAGPAR'  lt_o2pagpar  'MTD' .
        PERFORM download_metadata USING  pkg_progs-obj_name  p_temp  'O2PAGEVH'  lt_o2pagevh  'MTD' .


        FIELD-SYMBOLS: <ls_o2pagdir>   TYPE o2pagdir .
        LOOP AT   lt_o2pagdir ASSIGNING <ls_o2pagdir> .

          DATA: lv_o2pagkey      TYPE o2pagkey,
                lv_filename      TYPE string,
                lv_tmp_filename  TYPE string,
                lv_source        TYPE string  ,    "PRAPAK
                download_bspname TYPE string.

          lv_o2pagkey-applname = pkg_progs-obj_name .
          lv_o2pagkey-pagekey  = <ls_o2pagdir>-pagekey .

          REFRESH lt_source.

          IF <ls_o2pagdir>-pagetype NE 'C'.
            CALL FUNCTION 'BSP_GET_SOURCE'
              EXPORTING
                p_pagekey         = lv_o2pagkey
              IMPORTING
                p_source          = lt_source
                p_maptab          = lt_maptab
              EXCEPTIONS
                page_not_existing = 1
                error_message     = 2
                OTHERS            = 3.
            IF sy-subrc <> 0.
*             Implement suitable error handling here
            ELSE.
*             " download page map info here >>>>>>>>>>>>>>>>>>>>>>>>>>

              DATA filename TYPE string .                                                             " inserted RB 20120713
              CONCATENATE lv_o2pagkey-pagekey g_fileseperator 'layout_to_meth_mapping' INTO filename. " inserted RB 20120713

              PERFORM download_metadata USING  pkg_progs-obj_name  p_temp
*                                                                         'layout_to_meth_mapping'    " deleted  RB 20120713
                                                                          filename                    " inserted RB 20120713
                                                                                    lt_maptab-layout_to_meth_mapping  'MTD' .
**PRAPAK Begin
              CLEAR: s_bsp_config.
              s_bsp_config-applname = lv_o2pagkey-applname.
              s_bsp_config-pagekey = lv_o2pagkey-pagekey.
*read design
              FIND FIRST OCCURRENCE OF REGEX '%_bsp_elem_0->design*' IN TABLE lt_source RESULTS lv_result.
              IF sy-subrc = 0.
                READ TABLE lt_source INTO lv_source INDEX lv_result-line.
                FIND FIRST OCCURRENCE OF ' = ' IN lv_source RESULTS lv_result.
                lv_result-offset = lv_result-offset + 4.
                s_bsp_config-design = lv_source+lv_result-offset.
                REPLACE FIRST OCCURRENCE OF `'.` IN s_bsp_config-design WITH ``.
              ENDIF.
*read Encode
              FIND FIRST OCCURRENCE OF REGEX '%_bsp_elem_0->forceEncode*' IN TABLE lt_source RESULTS lv_result.
              IF sy-subrc = 0.
                READ TABLE lt_source INTO lv_source INDEX lv_result-line.
                FIND FIRST OCCURRENCE OF ' = ' IN lv_source RESULTS lv_result.
                lv_result-offset = lv_result-offset + 4.
                s_bsp_config-encode = lv_source+lv_result-offset.
                REPLACE FIRST OCCURRENCE OF `'.` IN s_bsp_config-encode WITH ``.
              ENDIF.
              APPEND s_bsp_config TO t_bsp_config.
**PRAPAK End



*             "download page map info here <<<<<<<<<<<<<<<<<<<<<<<<<<

*             ">>>>>>>>>>>>>>>>>>>>>>>>>>> " deleted RB 20120713  added back by rwf 2015aug20
*             "taken out again by rwf on 3sep2015 because it causes duplicate vulns with those found in
*             "the downloaded event-handlers.
*              CLEAR: lv_tmp_filename, lv_filename.
*              CONCATENATE  'BSP_SOURCE' lv_o2pagkey-applname '_' lv_o2pagkey-pagekey  INTO     lv_tmp_filename   .
*              CONDENSE lv_tmp_filename.
*              SHIFT lv_tmp_filename LEFT DELETING LEADING space.
*              " TRANSLATE lv_tmp_filename TO UPPER CASE.
*              FIND FIRST OCCURRENCE OF '.' IN lv_tmp_filename RESULTS  lv_result .
*              if sy-subrc = 0.
*                lv_filename = lv_tmp_filename(lv_result-offset) .
*              else.
*                lv_filename = lv_tmp_filename .
*              endif.
*              replace all OCCURRENCES OF '/' in lv_filename with g_fileseperator.
*              CLEAR   download_bspname .
*              PERFORM assemble_pathname USING  p_temp lv_filename 'BSP'  CHANGING download_bspname .
*              PERFORM download_file  USING lt_source  download_bspname  download_bspname .
*              "<<<<<<<<<<<<<<<<<<<<<<<<<<< " deleted RB 20120713  added back by rwf 2015aug20 but taken out again by rwf on 3sep2015
            ENDIF.

            " get <ls_o2pagdir>-IMPLCLASS
          ELSE.
            " get <ls_o2pagdir>-IMPLCLASS
          ENDIF.


          DATA: lv_trkey        TYPE trkey,
                lo_o2_api_pages TYPE REF TO cl_o2_api_pages,

                lt_content      TYPE o2pageline_table,
                ls_content      TYPE LINE OF o2pageline_table,
                lv_xml_source   TYPE xstring,
                lt_otr_guids    TYPE bsp_guids.

          CALL METHOD cl_o2_api_pages=>load_with_access_permission
            EXPORTING
              p_mode               = 'SHOW'
              p_pagekey            = lv_o2pagkey
*             p_version            = SO2_VERSION_ACTIVE
*             p_with_all_texts     = ' '
*             p_load_for_generation = ' '
            IMPORTING
              p_transport_key      = lv_trkey
              p_page               = lo_o2_api_pages
            EXCEPTIONS
              action_cancelled     = 1
              enqueued_by_user     = 2
              error_occured        = 3
              locked_by_author     = 4
              permission_failure   = 5
              object_not_existing  = 6
              version_not_existing = 7
              OTHERS               = 8.
          IF sy-subrc <> 0.
*           Implement suitable error handling here !*!*!*!
          ENDIF.

          CALL METHOD lo_o2_api_pages->get_page
            IMPORTING
              p_content    = lt_content
              p_xml_source = lv_xml_source
              p_otr_guids  = lt_otr_guids
            EXCEPTIONS
              page_deleted = 1
              invalid_call = 2
              OTHERS       = 3.
          IF sy-subrc <> 0.
*           Implement suitable error handling here !*!*!*!
          ENDIF.

          REFRESH lt_source.
          LOOP AT lt_content INTO ls_content.
            ls_source = ls_content.
            INSERT  ls_source INTO TABLE lt_source.

*>>>>>>>>>>>>>>>>>>>>>!!!!!!!!!!!!!!!BSP-Extensions
            " check the line for <%@extension name   and collect the extensions in a list
            "    lt_bsp_ext TYPE  rswsourcet,
            "    ls_bsp_ext TYPE string,
            IF ls_source CS '<%@' AND ls_source CS 'extension' AND ls_source CS 'name'.

              DATA: la_result_wa TYPE match_result .
              FIND FIRST OCCURRENCE OF '"' IN ls_source RESULTS la_result_wa.
              la_result_wa-offset = la_result_wa-offset + 1.
              ls_bsp_ext = ls_source+la_result_wa-offset.
              FIND FIRST OCCURRENCE OF '"' IN ls_bsp_ext RESULTS la_result_wa.
              ls_bsp_ext = ls_bsp_ext(la_result_wa-offset) .
              ls_bspext = ls_bsp_ext.

              INSERT ls_bspext INTO TABLE lt_bspext.

            ENDIF.
*<<<<<<<<<<<<<<<<<<<<<<<!!!!!!!!!!!!!!!BSP-Extensions
          ENDLOOP.

          CONCATENATE  'BSP_PAGE' lv_o2pagkey-applname '_' lv_o2pagkey-pagekey  INTO     lv_filename   .
          CONDENSE lv_filename.
          SHIFT lv_filename LEFT DELETING LEADING space.
          REPLACE ALL OCCURRENCES OF '/' IN lv_filename WITH g_fileseperator.
          " can this applname have a namespace prefix on it in which case the
          " above replace stmt is wrong and we should probably sanitize the filename
          " we build

          CLEAR   download_bspname .
          PERFORM assemble_pathname USING  p_temp lv_filename 'BSP'  CHANGING download_bspname .
          PERFORM download_file     USING  lt_source  download_bspname  lv_filename.


*>>>>>>>>>>>>>>>>>>>>>    deleted RB 20120713; but added back by RWF 2015aug12 to support new BSP processing
          DATA:           lt_ev_handler  TYPE         o2pagevh_tabletype .
          FIELD-SYMBOLS: <ls_ev_handler> TYPE LINE OF o2pagevh_tabletype .
          CALL METHOD lo_o2_api_pages->get_event_handlers
            IMPORTING
              p_ev_handler = lt_ev_handler
            EXCEPTIONS
              page_deleted = 1
              invalid_call = 2
              OTHERS       = 3.
          IF sy-subrc <> 0.
            " Implement suitable error handling here
          ELSE.
            LOOP AT lt_ev_handler   ASSIGNING <ls_ev_handler> .
              DATA   lv_basename TYPE string.
              CLEAR  lv_basename.
              CONCATENATE 'BSP_EVENT'
                      <ls_ev_handler>-applname '_'
                      <ls_ev_handler>-pagekey '_'
                      <ls_ev_handler>-evhandler INTO     lv_basename .
              CONDENSE lv_basename.
              SHIFT lv_basename LEFT DELETING LEADING space.
              REPLACE ALL OCCURRENCES OF '/' IN lv_basename WITH g_fileseperator.
              CLEAR  lv_filename.
              PERFORM assemble_pathname USING p_temp lv_basename abap_filename_ext CHANGING lv_filename.
              PERFORM download_file     USING <ls_ev_handler>-source lv_filename lv_basename .
            ENDLOOP.
          ENDIF.
*<<<<<<<<<<<<<<<<<<<<<    deleted RB 20120713; but added back by RWF 2015aug12 to support new BSP processing

          DATA  lt_class_incls TYPE TABLE OF trdir WITH HEADER LINE .

          IF NOT <ls_o2pagdir>-implclass IS INITIAL.
            IF flg_dbg_level > 4 .
              WRITE: /, '    bsp application class name is', <ls_o2pagdir>-implclass.
            ENDIF.
            CLEAR lt_class_incls[].
            PERFORM get_all_class_includes           TABLES lt_class_incls  USING <ls_o2pagdir>-implclass .
*           PERFORM get_all_class_includes           TABLES matching_progs     USING <ls_o2pagdir>-implclass .

            LOOP AT lt_class_incls .
              IF flg_dbg_level > 4 .
                WRITE: /, '      bsp application class INCLUDE name is', lt_class_incls-name.
              ENDIF.
              DATA  lv_implclass_inclname_lower TYPE string.
              DATA  lv_implclass_inclname_upper TYPE string.
              CLEAR lv_implclass_inclname_lower.
              CLEAR lv_implclass_inclname_upper.
              CONCATENATE <ls_o2pagdir>-implclass 'CM000' INTO lv_implclass_inclname_lower.
              CONCATENATE <ls_o2pagdir>-implclass 'CMZZZ' INTO lv_implclass_inclname_upper.
              IF flg_dbg_level > 4 .
                WRITE: /, '      lv_implclass_includefilename_lower is ', lv_implclass_inclname_lower.
                WRITE: /, '      lv_implclass_includefilename_upper is ', lv_implclass_inclname_upper.
              ENDIF.
              IF  lt_class_incls-name NOT BETWEEN lv_implclass_inclname_lower AND lv_implclass_inclname_upper.
                INSERT lt_class_incls INTO TABLE matching_progs .
              ELSE.
                DATA lt_bspevh_source_tab TYPE TABLE OF string .
                DATA lv_bspevh_source_line1 TYPE string.
                CLEAR lt_bspevh_source_tab.
                CLEAR lv_bspevh_source_line1 .
                READ REPORT lt_class_incls-name  STATE 'A'  INTO lt_bspevh_source_tab.
                READ TABLE lt_bspevh_source_tab INDEX 1 INTO lv_bspevh_source_line1 .
                IF flg_dbg_level > 4 .
                  WRITE: /, '   first line of lt_bspevhsource_tag is ', lv_bspevh_source_line1.
                ENDIF.
                CONDENSE lv_bspevh_source_line1.
                IF flg_dbg_level > 4 .
                  WRITE: /, '   first line of lt_bspevhsource_tag, condensed, is ', lv_bspevh_source_line1.
                ENDIF.
                DATA  lv_model_line1 TYPE string.
                DATA  lv_chosen_evh  TYPE string.
                CLEAR lv_chosen_evh.
                LOOP AT lt_ev_handler   ASSIGNING <ls_ev_handler> .
                  CLEAR lv_model_line1.
                  CONCATENATE 'method _'  <ls_ev_handler>-evhandler '.' INTO lv_model_line1 .
                  IF flg_dbg_level > 4 .
                    WRITE: /, '   lv_model_line1 is ', lv_model_line1.
                  ENDIF.
                  IF lv_bspevh_source_line1 EQ lv_model_line1 .
                    lv_chosen_evh = <ls_ev_handler>-evhandler.
                  ENDIF.
                ENDLOOP.      " through lt_ev_handler
                IF lv_chosen_evh IS INITIAL.
                  INSERT lt_class_incls INTO TABLE matching_progs .
                ELSE.
                  CLEAR lt_bspevh_source_tab.
                  APPEND  lv_bspevh_source_line1 TO lt_bspevh_source_tab.
                  DATA  lv_evh_include LIKE tadir-obj_name.
                  CLEAR lv_evh_include.
                  CONCATENATE
                      'BSP_EVENT'
                      <ls_ev_handler>-applname '_'
                      <ls_ev_handler>-pagekey  '_'
                      lv_chosen_evh                          INTO  lv_evh_include.
                  CLEAR  lv_basename.
                  CONCATENATE
                     '  INCLUDE '''  lv_evh_include  '''.'   INTO  lv_basename .
                  CLEAR  lv_filename.
                  PERFORM assemble_pathname USING p_temp lt_class_incls-name abap_filename_ext CHANGING lv_filename.
                  APPEND  lv_basename   TO lt_bspevh_source_tab.
                  APPEND  'endmethod.'  TO lt_bspevh_source_tab.
                  PERFORM download_file     USING lt_bspevh_source_tab lv_filename lv_filename .
                ENDIF.
              ENDIF.
            ENDLOOP .    " through lt_class_incls
          ENDIF.

        ENDLOOP.   " through lt_o2pagdir

**PRAPAK Begin
        PERFORM download_metadata USING  pkg_progs-obj_name  p_temp  'CONFIG'    t_bsp_config    'MTD' .
**PRAPAK End

        FIELD-SYMBOLS: <ls_o2appl>     TYPE         o2appl .
        LOOP AT lt_o2appl ASSIGNING <ls_o2appl>.
          IF NOT <ls_o2appl>-applclas IS INITIAL.
            PERFORM get_all_class_includes        TABLES  matching_progs  USING <ls_o2appl>-applclas .
          ENDIF.
        ENDLOOP.

        SORT lt_bspext .
        DELETE ADJACENT DUPLICATES FROM lt_bspext.

* fill these tables:  O2TAGLIB  O2TAG

        IF NOT lt_bspext IS INITIAL.

          SELECT * FROM o2taglib INTO TABLE lt_o2taglib FOR ALL ENTRIES IN lt_bspext
                               WHERE tlibid = lt_bspext-table_line  AND  state = 'A' .
          SELECT * FROM o2tag    INTO TABLE lt_o2tag    FOR ALL ENTRIES IN lt_bspext
                               WHERE tlibid = lt_bspext-table_line  AND  state = 'A' .

          PERFORM download_metadata USING  pkg_progs-obj_name  p_temp  'O2TAGLIB' lt_o2taglib  'MTD' .
          PERFORM download_metadata USING  pkg_progs-obj_name  p_temp  'O2TAG'    lt_o2tag     'MTD' .

          LOOP AT  lt_o2taglib  ASSIGNING <ls_o2taglib> .
            IF NOT  <ls_o2taglib>-taglibclass  IS INITIAL.
              PERFORM get_all_class_includes              TABLES matching_progs  USING <ls_o2taglib>-taglibclass .
            ENDIF.
            IF NOT  <ls_o2taglib>-tagbaseclass IS INITIAL.
              PERFORM get_all_class_includes              TABLES matching_progs  USING <ls_o2taglib>-tagbaseclass .
            ENDIF.
          ENDLOOP.

          LOOP AT  lt_o2tag     ASSIGNING <ls_o2tag>  .
            IF NOT  <ls_o2tag>-tagclass        IS INITIAL.
              PERFORM get_all_class_includes              TABLES matching_progs  USING <ls_o2tag>-tagclass .
            ENDIF.
            IF NOT   <ls_o2tag>-genclass       IS INITIAL.
              PERFORM get_all_class_includes              TABLES matching_progs  USING <ls_o2tag>-genclass .
            ENDIF.
          ENDLOOP.

        ENDIF.


    ENDCASE.



    IF NOT  p_down IS INITIAL.
      SORT matching_progs BY name .
      DELETE ADJACENT DUPLICATES FROM matching_progs.
      LOOP AT matching_progs INTO prog.

        " working dir :   P_TEMP type string



        lv_char3 = pkg_progs-object(3).
**      if lv_char3 = 'WAP'.
**        lv_char3 = 'BSP'. " not necessary anymore
**      endif.
*
*         *** rf 4feb2014 *** no longer necessary since next stmt changes extension to abap_filename_ext anyway.
*       if lv_char3 = 'PRO'.
*         " break d023975.
*         " check for INC here
*         SELECT SINGLE * FROM  progdir
*             INTO ls_progdir
*             WHERE  name        = prog-name
*             AND    state       = 'A'.
*         if sy-subrc = 0  and ls_progdir-subc = 'I' .
*           lv_char3 = 'INC'.
*         endif.
*
*       endif.

        " Everything should be MTD, BSP, or ABAP at this point
        IF lv_char3 NE 'MTD'  AND  lv_char3 NE 'BSP' .
          lv_char3 = abap_filename_ext.
        ENDIF.
        PERFORM download_source_code USING p_temp  prog-name  lv_char3 ''  0  ''.

      ENDLOOP.
      REFRESH matching_progs.
    ENDIF.



  ENDLOOP.         " which downloads all the relevant ABAP files


  IF NOT p_buld IS INITIAL.

    SORT downloaded_files.
    DELETE ADJACENT DUPLICATES FROM downloaded_files.

    LOOP AT downloaded_files ASSIGNING <downloaded_file>.
      tmp_downloaded_file = <downloaded_file>.
      CLEAR <downloaded_file>.
      CONDENSE tmp_downloaded_file.
      CONCATENATE '"' tmp_downloaded_file '"' INTO <downloaded_file>.
    ENDLOOP.

    CLEAR   arg_file_name .
    PERFORM assemble_pathname USING p_temp 'args' 'txt' CHANGING arg_file_name .
    PERFORM download_file     USING downloaded_files  arg_file_name  'args.txt' .

    "p_fpr Fpr-file 2b created
    "P_TEMP temp dir

    " arg_file_name  arg_file_name

    IF p_bld IS INITIAL.
      MESSAGE 'BuildID (p_bld) is initial' TYPE 'I' .
      RETURN.
    ENDIF.

    IF arg_file_name IS INITIAL.
      MESSAGE 'arg_file_name is initial'   TYPE 'I' .
      RETURN.
    ENDIF.

    CLEAR cmdstring .
    CONCATENATE ' ' p_trans ' -b ' p_bld ' @' '"' arg_file_name '"'   INTO cmdstring IN CHARACTER MODE RESPECTING BLANKS.

    CALL METHOD cl_gui_frontend_services=>execute
      EXPORTING
        application = 'sourceanalyzer'
        parameter   = cmdstring
        synchronous = sync.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = 0
        text       = 'running build'.

  ENDIF.

*     if scanning the buildID was selected then run sourceanalyzer now with the specified parameters
  IF NOT p_scn IS INITIAL.

    CLEAR cmdstring.
    CONCATENATE p_scan ' -scan -b ' p_bld ' -f ' '"' p_fpr '"'   INTO cmdstring IN CHARACTER MODE RESPECTING BLANKS.

    IF NOT sync IS INITIAL.
      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          percentage = 0
          text       = 'running scan'.
    ENDIF.

    CALL METHOD cl_gui_frontend_services=>execute
      EXPORTING
        application = 'sourceanalyzer'
        parameter   = cmdstring
        synchronous = sync.
  ENDIF.

*    if AWB was selected then run it.
  IF NOT p_awb IS INITIAL.
    IF NOT sync IS INITIAL.
      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          percentage = 0
          text       = 'running awb'.

      CLEAR cmdstring.
      CONDENSE p_fpr.
      CONCATENATE '"' p_fpr '"' INTO cmdstring .

      CALL METHOD cl_gui_frontend_services=>execute
        EXPORTING
          application = 'auditworkbench.cmd'
          parameter   = cmdstring.

    ENDIF.
  ENDIF.

*     if zipping the results was selected then report that we are about to do so, then do it
  IF NOT p_zip IS INITIAL.

    IF p_zipn NP '*.zip'.
      CONCATENATE  p_zipn '.zip' INTO lv_zipfile.
    ELSE.
      lv_zipfile = p_zipn.
    ENDIF.

    CONDENSE                        lv_zipfile.

    CONCATENATE 'Zipping into' ` `  lv_zipfile INTO  lv_info .
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = 0
        text       = lv_info.

    WRITE: /, lv_info .

    PERFORM add_to_zip       TABLES downloaded_files   USING   p_temp lv_zipfile  .
  ENDIF.
****** this is the end of the AT START-OF-SELECTION block



*&---------------------------------------------------------------------*
*&      Form  COLLECT_PKG_MEMBERS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PKGS  text
*      -->P_P_PKG  text
*----------------------------------------------------------------------*
* I think this subroutine could be more mnemonically named
* collect_member_pkgs because it inserts all sub*-packages of its
* USING-parameter pkg into the TABLES-reference-parameter pkg_members.
*
* This subroutine is directly recursive and has only one non-recursive
* call-site.  The loop that contains such call-site should be folded
* into this subroutine.  However, care should be taken that the
* TABLES-parameter pkg_members is used correctly because it is used
* both to return results and to keep from visiting the same package
* more than once.  I'm not sure that is happening correctly in the
* recursive case.
*----------------------------------------------------------------------*
FORM collect_pkg_members TABLES pkg_members USING pkg.
  DATA:  pkgs LIKE TABLE OF tdevc-devclass WITH HEADER LINE.
  DATA:  mem  LIKE          tdevc-devclass.

  SELECT devclass FROM tdevc INTO TABLE pkgs WHERE parentcl = pkg.
  IF sy-subrc NE 0.

    SELECT devclass FROM tdevc INTO TABLE pkgs WHERE devclass = pkg.
    INSERT LINES OF pkgs INTO TABLE pkg_members.

  ELSE.

    INSERT LINES OF pkgs INTO TABLE pkg_members.

    LOOP AT pkgs INTO mem.

      PERFORM collect_pkg_members TABLES pkg_members USING mem.
    ENDLOOP.
    INSERT pkg INTO TABLE pkg_members .

  ENDIF.
ENDFORM.                    "collect_pkg_members

*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_SOURCE_CODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_TEMP  text
*      -->P_PROG  text
*----------------------------------------------------------------------*
*
*   The following declarations are put here in order to emphasize that
*   they are not used in any earlier part of the code.  They are also
*   not used outside of the subroutine download_source_code but I don't
*   know of any way to declare these variables that would enforce
*   that observation as a restriction.
*
TYPES: progname_type     LIKE                 trdir-name,
       progname_tab_type TYPE HASHED TABLE OF progname_type   WITH UNIQUE KEY table_line.

DATA   gt_progname_tab    TYPE progname_tab_type .

DATA   prefix_string(150) TYPE c  VALUE
'------------------------------------------------------------------------------------------------------------------------------------------------------' .

FORM download_source_code  USING    p_dir
                                    p_progname    TYPE progname_type
                                    p_ext         TYPE char4
                                    p_msg_prefix  TYPE string
                                    p_call_depth  TYPE i
                                    p_funmod_name TYPE progname_type .

  DATA:
    lv_ext      TYPE          char4,
    itab        TYPE TABLE OF string,
    lv_result   TYPE          match_result,
    lv_function TYPE          string
    .

  DATA  lv_progname     TYPE          string.

  lv_progname = p_progname .
  CONDENSE lv_progname .

  IF p_sap = abap_false AND gv_is_user_mod = abap_false.
    DATA is_non_std_code TYPE abap_bool VALUE abap_false.
    PERFORM is_non_std_sap_prog USING lv_progname CHANGING is_non_std_code.
    IF is_non_std_code = abap_false.
      RETURN.
    ENDIF.
  ENDIF.

  DATA   msg_buffer  TYPE string .

  READ TABLE gt_progname_tab WITH TABLE KEY table_line = p_progname TRANSPORTING NO FIELDS .
  IF   sy-subrc = 0 .
    IF flg_dbg_level > 1 .
      CONCATENATE p_msg_prefix  'downloaded  '  lv_progname  ' already' INTO msg_buffer RESPECTING BLANKS .
      WRITE /  msg_buffer.
    ENDIF.
    RETURN.
  ELSE .
    IF flg_dbg_level > 1 .
      CONCATENATE  p_msg_prefix lv_progname  ' : examine source'       INTO msg_buffer RESPECTING BLANKS  .
      WRITE /  msg_buffer .
    ENDIF.
  ENDIF.
  INSERT p_progname INTO TABLE gt_progname_tab.


  DATA lv_msg_prefix TYPE string.
  CONCATENATE p_msg_prefix '--' INTO lv_msg_prefix .



  REFRESH itab[].
  READ REPORT p_progname  STATE 'A'  INTO itab.

  IF sy-subrc NE 0.

    DATA:
      wt_smodisrc TYPE  STANDARD TABLE OF  smodisrc    WITH DEFAULT KEY,
      wt_smodilog TYPE  STANDARD TABLE OF  smodilog    WITH DEFAULT KEY,
      wt_trdir    TYPE  STANDARD TABLE OF   trdir      WITH DEFAULT KEY,
      ws_source   TYPE                     abaptxt255,
      wt_source   TYPE  STANDARD TABLE OF  abaptxt255  WITH DEFAULT KEY,
      w_typepool  TYPE                     typegroup
      .

    w_typepool = p_progname .
    CALL FUNCTION 'TYPD_GET_OBJECT'
      EXPORTING
        typdname          = w_typepool
        r3state           = 'A'
      TABLES
        psmodisrc         = wt_smodisrc
        psmodilog         = wt_smodilog
        psource           = wt_source
        ptrdir            = wt_trdir
      EXCEPTIONS
        version_not_found = 1
        reps_not_exist    = 2
        error_message     = 3
        OTHERS            = 4.
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    LOOP AT wt_source INTO ws_source.
      DATA  line  TYPE  string.
      line = ws_source.
      APPEND line  TO itab.
    ENDLOOP.

  ENDIF.

  IF flg_dbg_level > 11 .
    DATA itabline TYPE string.
    WRITE / 'all the input lines are:' .
    LOOP AT itab INTO itabline .
      WRITE / itabline .
    ENDLOOP.
    WRITE  / '<EOF>'.
  ENDIF.

*************************************************
*** "this block of text is not used for anything - rwf 25feb2014
**
**  FIELD-SYMBOLS: <progs_to_scan> TYPE         trdir-name.
**  READ TABLE progs_to_scan WITH TABLE KEY table_line = p_progname ASSIGNING <progs_to_scan>.
**  IF sy-subrc = 0.
**    RETURN.
**  ELSE.
**    INSERT prog-name INTO TABLE progs_to_scan .
**  ENDIF.
**
**
**  LOOP AT itab INTO line.
**
**    APPEND line  TO source_line.
**
**  ENDLOOP.
************************************************

  IF itab[] IS NOT INITIAL.

    DATA lt_keyword_tbl TYPE TABLE OF char30 .
    DATA lt_token_tbl   TYPE TABLE OF stokes .
    DATA lt_stmt_tbl    TYPE TABLE OF sstmnt .

    DATA            token_line  TYPE  stokes .
    DATA             stmt_line  TYPE  sstmnt .

    FIELD-SYMBOLS  <token_line> TYPE stokes .
    FIELD-SYMBOLS   <stmt_line> TYPE sstmnt .


*<<<<<<<<<<<<<<<<<< now look for INCLUDE statements  <<<<<<<<<<<<<

    DATA  lv_inc_stmt_tbl TYPE TABLE OF string .
    DATA  lv_inc_name     TYPE          progname_type .

    CLEAR lv_inc_stmt_tbl .
    PERFORM  parse_include_stmts_into_table USING itab CHANGING lv_inc_stmt_tbl.

    IF lv_inc_stmt_tbl IS NOT INITIAL .
      LOOP AT lv_inc_stmt_tbl INTO lv_inc_name .

        IF flg_dbg_level > 1 .
          CLEAR msg_buffer .
          CONCATENATE  lv_msg_prefix  'downloading INCLUDE file ' lv_inc_name '.' abap_filename_ext INTO msg_buffer RESPECTING BLANKS .
          WRITE: / msg_buffer .
        ENDIF.

        PERFORM download_source_code  USING p_dir lv_inc_name abap_filename_ext lv_msg_prefix  p_call_depth  ''.
      ENDLOOP.
    ENDIF.
*>>>>>>>>>>>>   done looking for and processing INCLUDE statements   >>>>>>>>>>


*<<<<<<<<<<< now look for TYPE_POOLS statements  <<<<<<<<<<<<<<

    REFRESH:               lt_keyword_tbl , lt_token_tbl , lt_stmt_tbl .
    APPEND 'TYPE-POOLS' TO lt_keyword_tbl .

    SCAN ABAP-SOURCE itab  TOKENS INTO lt_token_tbl  STATEMENTS INTO lt_stmt_tbl  KEYWORDS FROM lt_keyword_tbl.

    IF lt_token_tbl IS NOT INITIAL.
      DELETE lt_token_tbl WHERE str = 'TYPE-POOLS'.

      LOOP AT lt_token_tbl INTO token_line .
        DATA lv_type_pool TYPE progname_type .
        lv_type_pool = token_line-str.
        " Type pools should also get a .ABAP extension
        IF flg_dbg_level > 1 .
          CLEAR msg_buffer .
          CONCATENATE  lv_msg_prefix  'downloading type-pool ' lv_type_pool  INTO msg_buffer RESPECTING BLANKS .
          WRITE: / msg_buffer .
        ENDIF.
        PERFORM download_source_code  USING p_dir lv_type_pool abap_filename_ext lv_msg_prefix  p_call_depth  ''.
      ENDLOOP.
    ENDIF.
*>>>>>>>>>>    done looking for TYPE_POOLS   >>>>>>>>>>>>>>>>>


*>>>>>>>>> BEGIN function module transitive extraction >>>>>>>>>>>>>>>>>>>>>>>>

*    CONCATENATE debug_prefix '    ' INTO debug_prefix RESPECTING BLANKS.

*   DATA:
*         g_c4(4)      type          c       value 'CALL',
*         itab1_4      LIKE TABLE OF g_c4,
*         itab2_4      TYPE TABLE OF stokes,
*         itab3_4      TYPE TABLE OF sstmnt,
*         itab2_line_4 TYPE          stokes,
*         itab3_line_4 TYPE          sstmnt
*   .

    DATA  function     TYPE REF TO   cl_function_builder_data.
    DATA  ls_header    TYPE          header_fb.
    DATA  ls_interface TYPE          rsfbintfv.

    IF flg_dbg_level > 9 .
      WRITE: /,  debug_prefix, 'start looking for global function calls in ', lv_progname, p_ext.
    ENDIF .


    IF flg_dbg_level > 10 .
      WRITE / 'all the input lines are:' .
      LOOP AT itab INTO itabline .
        WRITE / itabline .
      ENDLOOP.
      WRITE  / '<EOF>'.
    ENDIF.

    REFRESH:         lt_keyword_tbl , lt_token_tbl , lt_stmt_tbl .
    APPEND 'CALL' TO lt_keyword_tbl .

    SCAN ABAP-SOURCE itab  TOKENS INTO lt_token_tbl  STATEMENTS INTO lt_stmt_tbl  KEYWORDS FROM lt_keyword_tbl .

    IF flg_dbg_level > 9 .
      WRITE: /,  debug_prefix, 'finished the SCAN ABAP-SOURCE stmt for keyword CALL'.
    ENDIF .

    IF lt_token_tbl IS INITIAL.
      IF flg_dbg_level > 9 .
        WRITE: /,  debug_prefix, 'no CALL tokens in file'.
      ENDIF .
    ELSE .
      IF flg_dbg_level > 9 .
        WRITE: /,  debug_prefix, 'found CALL stmt in file'.
      ENDIF .


      IF flg_dbg_level > 9 .
        "lets look at all the stmts
        WRITE: /,  debug_prefix, 'all the stmts that result from the scan:'.
        LOOP AT lt_stmt_tbl INTO stmt_line.
          WRITE: /,  debug_prefix,  stmt_line-from,  stmt_line-to, stmt_line-type.
        ENDLOOP.
      ENDIF.

      IF flg_dbg_level > 9 .
        "lets look at all the tokens
        WRITE: /,  debug_prefix, 'all the tokens that result from the scan:'.
        LOOP AT lt_token_tbl INTO token_line.
          WRITE: /,  debug_prefix,  token_line-str, token_line-type.
        ENDLOOP.
      ENDIF.


      IF lt_stmt_tbl IS INITIAL.

        IF flg_dbg_level > 2 .
          WRITE: / lv_msg_prefix,  '###  no CALL stmts in file.' .
        ENDIF.

      ELSE.
        DATA           lv_tabix     TYPE sytabix .

        LOOP AT lt_stmt_tbl ASSIGNING <stmt_line> .
          IF <stmt_line>-type = 'K' .
*         by design the first token of any statement in lt_stmt_tbl must be CALL identifier
            lv_tabix = <stmt_line>-from + 1 .
            READ TABLE lt_token_tbl ASSIGNING <token_line> INDEX lv_tabix .
            IF  <token_line>-type = 'I' AND <token_line>-str = 'FUNCTION'.
              ADD 1 TO lv_tabix.
              READ TABLE lt_token_tbl ASSIGNING <token_line> INDEX lv_tabix .
              IF  <token_line>-type = 'S' .   " i.e. this token is a string



                "lv_prog = <token_line>-str.
                "lv_function = substring( val = <token_line>-str  off = 1  len = ( strlen( <token_line>-str ) - 2 ) ) .
                "this is just removing the leading and trailing "'" characters from the lv_function.
                lv_function = <token_line>-str .
*             write: / 'function name is ', lv_function.
                SHIFT lv_function BY 1 PLACES RIGHT CIRCULAR.
                SHIFT lv_function BY 2 PLACES LEFT  .
*             write: / 'stripped function name is ', lv_function.

                "select single * from tfdir into ls_tfdir where FUNCNAME = lv_function.

                DATA  lv_skip_function_download TYPE c.
                lv_skip_function_download = 'N'.
                IF  lv_function(1) = 'y' OR lv_function(1) = 'z' OR lv_function(1) = '/' .
                  " this is a customer-function or in a namespace so it is always downloaded
                ELSEIF  p_call_depth >= max_call_depth .
                  lv_skip_function_download = 'Y'.

                  IF flg_dbg_level > 1 .
                    WRITE: / lv_msg_prefix,  '### any calls to SAP-functions in this context are ignored since current call-chain length is >= ', max_call_depth .
                  ENDIF.

                ENDIF.

                IF lv_skip_function_download EQ 'N' .


                  DATA lv_call_depth TYPE i .
                  lv_call_depth = p_call_depth + 1 .

                  CLEAR ls_header .

                  IF flg_dbg_level > 9 .
                    IF ls_header-name IS INITIAL.
                      WRITE: / debug_prefix, 'ls_header-NAME IS initial before assignment'.
                    ELSE.
                      WRITE: / debug_prefix, 'ls_header-NAME is NOT initial before assignment'.
                    ENDIF.
                  ENDIF.

                  ls_header-name = lv_function.

                  IF flg_dbg_level > 9 .
                    WRITE: /  debug_prefix, 'ls_HEADER-NAME = ', ls_header-name.

                    IF ls_header-include IS INITIAL.
                      WRITE: / debug_prefix, 'ls_header-INCLUDE IS     initial before call to FUNCTION->IMPORT'.
                    ELSE.
                      WRITE: / debug_prefix, 'ls_header-INCLUDE is NOT initial before call to FUNCTION->IMPORT'.
                    ENDIF.
                  ENDIF.

                  CREATE OBJECT function.

                  CALL METHOD function->import
*                 EXPORTING
*                   SUPPRESS_ACTIVE_CHECK        = SPACE
*                   SUPPRESS_ENHANCEMENTS        = SPACE
*                   RFCDEST INATION              =
*                   DESCRIPTION_BYPASSING_BUFFER = ABAP_FALSE
*                 IMPORTING
*                   ENHANCEMENT_TAB              =
*                   ENHO_IN_UPGRADE_MODE         =
                    CHANGING
                      header    = ls_header
                      interface = ls_interface
                    EXCEPTIONS
                      cancelled = 1
                      OTHERS    = 2.


                  IF sy-subrc NE 0.
                    IF flg_dbg_level > 1 .
                      WRITE: /,  debug_prefix, 'call to FUNCTION->IMPORT returned non-zero sy-subrc = ', sy-subrc.
                    ENDIF.
                  ELSE.

                    IF ls_header-include IS INITIAL.
                      IF flg_dbg_level > 9 .
                        WRITE: /, debug_prefix, 'ls_header-INCLUDE IS initial after call to FUNCTION->IMPORT - no function found for ', ls_header-name.
                        WRITE: /, debug_prefix.
                      ENDIF.
                    ELSE.
                      IF flg_dbg_level > 9 .
                        WRITE: /, debug_prefix, 'ls_header-INCLUDE is NOT initial after call to FUNCTION->IMPORT - for function', ls_header-name.

                        DATA: msgbody1 TYPE string, msgbody2 TYPE string.
                        CONCATENATE   '<<' ls_header-name      '>> <<' ls_header-include '>> <<' ls_header-progname '>> <<' ls_header-devclass '>>'  INTO msgbody1 RESPECTING BLANKS.
                        CONCATENATE   '<<' ls_header-namespace '>> <<' ls_header-area    '>> <<' ls_header-str_area '>> <<' ls_header-stext    '>>'  INTO msgbody2 RESPECTING BLANKS.

                        WRITE:    /  debug_prefix,
                                  'after FUNCTION->IMPORT call, ls_header fields (name include progname devclass namespace area str_area stext) = ',
                                  /  msgbody1, /  msgbody2.
                        CLEAR: msgbody1 , msgbody2.
                      ENDIF.

                      IF flg_dbg_level > 1 .
                        CLEAR msg_buffer .
                        CONCATENATE  lv_msg_prefix  'downloading '  ls_header-progname  '  to get func '  lv_function  ' in file '  ls_header-include INTO msg_buffer RESPECTING BLANKS.
                        WRITE: / msg_buffer . .
                      ENDIF.

                      PERFORM download_src_implicit_funcgrp USING  p_dir  ls_header-progname ls_header-include abap_filename_ext lv_msg_prefix  lv_call_depth .

                      IF flg_dbg_level > 1 .
                        CLEAR msg_buffer .
                        CONCATENATE  lv_msg_prefix   'downloading func '  lv_function  ' in file '  ls_header-include INTO msg_buffer RESPECTING BLANKS.
                        WRITE /  msg_buffer .
                      ENDIF.
                      PERFORM download_source_code          USING  p_dir  ls_header-include abap_filename_ext lv_msg_prefix  lv_call_depth  ls_header-include .

*            debug_prefix = substring( val = debug_prefix  off = 1  len = ( strlen( debug_prefix ) - 4 ) ) .

                    ENDIF.  " the include file to download was found and downloaded
                  ENDIF.    " the call to FUNCTION->IMPORT succeeded
                ENDIF.      " the body of this function SHOULD be downloaded
              ENDIF.        " the third token of this stmt is a character string
            ENDIF.          " the second token of this stmt is the FUNCTION keyword
          ENDIF.            " this stmt is an ABAP keyword stmt other than COMPUTE
        ENDLOOP.            " iteration through the stmts of this file
      ENDIF.                " the table of stmts for this file is not INITIAL
    ENDIF.                " the table of tokens for this file is not INITIAL

*<<<<<<<<<<<<<function module transitive extraction END<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
*
*------------------------------------------------------------------------------------------
*
*>>>>>>>>>>>>>>> if this is a FUNCTION MODULE look for PERFORM stmts; >>>>>>>>>>>>>>>>>>>>>
*>>>>>>>>>>>>>>> if any found download FORM includes for this FUNCTION GROUP >>>>>>>>>>>>>>

    IF  p_funmod_name NE '' .
      REFRESH:            lt_keyword_tbl , lt_token_tbl , lt_stmt_tbl .
      APPEND 'PERFORM' TO lt_keyword_tbl .

      SCAN ABAP-SOURCE itab  TOKENS INTO lt_token_tbl  STATEMENTS INTO lt_stmt_tbl  KEYWORDS FROM lt_keyword_tbl .

      IF flg_dbg_level > 9 .
        CLEAR msg_buffer .
        CONCATENATE  lv_msg_prefix 'finished the SCAN ABAP-SOURCE stmt for keyword PERFORM' INTO msg_buffer RESPECTING BLANKS .
        WRITE /  msg_buffer .
      ENDIF .

      IF lt_token_tbl IS INITIAL.
        IF flg_dbg_level > 9 .
          CLEAR msg_buffer .
          CONCATENATE  lv_msg_prefix   'no PERFORM tokens in file' INTO msg_buffer RESPECTING BLANKS .
          WRITE /  msg_buffer .
        ENDIF .

      ELSE .

        IF flg_dbg_level > 2 .
          CLEAR msg_buffer .
          CONCATENATE  lv_msg_prefix  'found some PERFORM stmts so download all FORM includes' INTO msg_buffer RESPECTING BLANKS .
          WRITE /  msg_buffer .
        ENDIF .

        PERFORM download_fungrp_form_includes  USING p_dir  p_funmod_name  lv_msg_prefix  lv_call_depth.

      ENDIF .
*<<<<<<<<<<<<< FINISHED conditional FORM INCLUDE download <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    ENDIF .


    lv_ext = p_ext.

    IF     lv_ext NE 'MTD' AND lv_ext NE 'BSP' .
      lv_ext = abap_filename_ext.
    ENDIF.

    DATA    download_pathname TYPE string .
    CLEAR   download_pathname.
    PERFORM assemble_pathname USING  p_dir  p_progname lv_ext CHANGING download_pathname .
    PERFORM download_file     USING  itab[] download_pathname  p_progname .

    IF flg_dbg_level > 1 .
      CLEAR msg_buffer .
      CONCATENATE  p_msg_prefix   'done downloading '  lv_progname INTO msg_buffer RESPECTING BLANKS.
      WRITE /  msg_buffer .
    ENDIF.

  ENDIF.     " itab IS NOT INITIAL

ENDFORM.                    " DOWNLOAD_SOURCE_CODE
*&---------------------------------------------------------------------*
*&      Form  ASSEMBLE_PATHNAME
*&---------------------------------------------------------------------*
*      <-- text
*----------------------------------------------------------------------*
*      -->P_P_DIR       TYPE string
*      -->P_P_PROG      TYPE string
*      -->P_P_EXTENSION TYPE char4
*----------------------------------------------------------------------*
FORM assemble_pathname  USING    p_p_dir
                                 p_p_prog
                                 p_p_extension TYPE char4
                        CHANGING result        TYPE string.
  " g_fileseperator

  DATA ls_dir       TYPE  string.
  DATA ls_prog      LIKE  trdir-name.
  DATA ls_pathname  TYPE  string.



  ls_dir = p_p_dir.
  CONDENSE ls_dir.

  ls_prog = p_p_prog .
  CONDENSE ls_prog .

  DATA  lv_sanitized TYPE string .
  CLEAR lv_sanitized .
  PERFORM sanitize_sourcecode_progname USING ls_prog CHANGING lv_sanitized .
  CLEAR ls_prog .
  ls_prog = lv_sanitized .

  CONCATENATE   ls_dir  g_fileseperator  ls_prog  '.'  p_p_extension   INTO  ls_pathname.

  CONDENSE ls_pathname . " NO-GAPS.

  result = ls_pathname .

ENDFORM.                    " ASSEMBLE_PATHNAME
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ITAB[]  text
*      -->P_P_DIR  text
*      -->P_P_PROG  text
*----------------------------------------------------------------------*
FORM download_file  USING  p_itab  p_pathname  p_progname.

  DATA objfile                    TYPE REF TO cl_gui_frontend_services.
  DATA lv_progress_indicator_msg  TYPE        string.

  IF p_down IS NOT INITIAL.

    IF p_sap = abap_false AND gv_is_user_mod = abap_false.
      DATA is_non_std_code TYPE abap_bool VALUE abap_false.
      PERFORM is_non_std_sap_prog USING p_progname CHANGING is_non_std_code.
      IF is_non_std_code = abap_false AND p_progname NP 'args.txt'.
        RETURN.
      ENDIF.
    ENDIF.

    CONCATENATE   'Downloading '  p_progname INTO  lv_progress_indicator_msg RESPECTING BLANKS.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = 0
        text       = lv_progress_indicator_msg.

    WRITE: / 'Writing: ', p_pathname.

    CREATE OBJECT objfile.

    CALL METHOD objfile->gui_download
      EXPORTING
        filename                = p_pathname
        filetype                = 'ASC'
        write_lf                = 'X'
        write_field_separator   = 'X'
        ignore_cerr             = abap_false
*       trunc_trailing_blanks   = truncatetrailingblanks
      CHANGING
        data_tab                = p_itab        "file-contents/data to be downloaded to the path denoted by filename parameter
      EXCEPTIONS
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        not_supported_by_gui    = 22
        error_no_gui            = 23.
    IF sy-subrc <> 0.
      WRITE: / 'Download file ret code: ', sy-subrc.
    ENDIF.
  ENDIF.

  INSERT p_pathname INTO TABLE downloaded_files .

ENDFORM.                    " DOWNLOAD_FILE
*&---------------------------------------------------------------------*
*&      Form  GET_ALL_CLASS_INCLUDES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_CLSNAME  text
*      -->P_MATCHING_PROGS  text
*----------------------------------------------------------------------*
FORM get_all_class_includes
    TABLES   p_matching_progs
    USING    p_clsname    TYPE seoclsname
                            .
  "Insert correct name for <...>.
  DATA: lt_matching_progs TYPE TABLE OF trdir                   WITH HEADER LINE,
        ls_result_line    TYPE LINE  OF seop_methods_w_include,
        lt_result         TYPE          seop_methods_w_include,
        lt_yield          TYPE          seoincl_t
        .

  lt_matching_progs[] = p_matching_progs[].

  " this is the only place in this REPORT where a method of YCL_OO_CLASSNAME_SERVICE is called.
  PERFORM _get_all_class_includes USING p_clsname CHANGING lt_yield.
  IF sy-subrc <> 0.
    MESSAGE ID  sy-msgid TYPE sy-msgty  NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 .
  ENDIF.

  lt_result = lt_yield.

  LOOP AT lt_result INTO ls_result_line.
    lt_matching_progs-name = ls_result_line-cpdkey.
    APPEND lt_matching_progs.
  ENDLOOP.

  p_matching_progs[]   = lt_matching_progs[]  .

ENDFORM.                    " GET_ALL_CLASS_INCLUDES
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_METADATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_O2APPL  text
*----------------------------------------------------------------------*
***** not sure what purpose is served by the "metadata" being downloaded here.  Is it ever used?
FORM download_metadata  USING
      pv_obj_name   TYPE sobj_name
      pp_tmp      TYPE string
      p_name      TYPE string
      p_lt_table
      p_ext
      .

  IF  flg_dbg_level > 2 .
    WRITE: / 'downloading metadata for ', pv_obj_name, ' from table ', p_name.
  ENDIF.


  FIELD-SYMBOLS: <lt_table> TYPE ANY TABLE .

  TYPES: xmlline(1024) TYPE x.

  DATA: lt_xml      TYPE STANDARD TABLE OF xmlline.
  DATA: ls_filename TYPE                   string,
        ls_obj_name TYPE                   string,
        lv_rc       TYPE i.
  "data len         type i.


  IF p_down IS INITIAL.
    RETURN.
  ENDIF.

  ls_obj_name = pv_obj_name .
  " ls_filename = pp_tmp.

  "len = strlen( pp_tmp ).
  CONDENSE pp_tmp.
  CONCATENATE pp_tmp g_fileseperator '_META-INF' g_fileseperator  ls_obj_name  g_fileseperator  p_name '.' p_ext  INTO ls_filename .
  CONDENSE ls_filename. " NO-GAPS.


  ASSIGN p_lt_table  TO <lt_table> .

  IF <lt_table>[] IS INITIAL.
    RETURN.
  ENDIF.

  CALL TRANSFORMATION id
    SOURCE data_node = <lt_table>
    RESULT XML lt_xml.


  " CL_ABAP_STRUCTDESCR
  WRITE: / 'Downloading: ', ls_filename.
  CALL METHOD cl_gui_frontend_services=>gui_download
    EXPORTING
      filename = ls_filename
      filetype = 'BIN'
    CHANGING
      data_tab = lt_xml.

  INSERT ls_filename INTO TABLE downloaded_files .


ENDFORM.                    " DOWNLOAD_METADATA

*&---------------------------------------------------------------------*
*&      Form  ADD_TO_ZIP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<DOWNLOADED_FILE>  text
*----------------------------------------------------------------------*
*" this FORM is only called once, at the end of the START-OF-SELECTION block.
*" the p_files parameter is only bound to the variable downloaded_files
FORM add_to_zip
                 TABLES p_files
                 USING  p_path    TYPE string
                        p_zipfile TYPE string       .

  DATA  d_files             TYPE TABLE OF string.
  DATA  zip_tool            TYPE REF TO   cl_abap_zip.
  DATA  lv_filenamewithpath TYPE          string.
*{   INSERT         S9SK900449                                        3
* Added Filename varable
  DATA filename TYPE string.
*}   INSERT

  DATA: t_data_tab TYPE TABLE OF x255,
        bin_size   TYPE          i,
        buffer_x   TYPE          xstring,
        buffer_zip TYPE          xstring.

  DATA  lv_downloaded_file TYPE string.

  d_files[] = p_files[].

**create zip tool
  CREATE OBJECT zip_tool.
*{   INSERT         S9SK900449                                        5
* Delete double Files
  SORT d_files.
  DELETE ADJACENT DUPLICATES FROM d_files.
*}   INSERT

  DELETE d_files WHERE table_line CP '*args.txt'.

  LOOP AT d_files INTO lv_downloaded_file.

    CONCATENATE p_path g_fileseperator p_zipfile INTO lv_filenamewithpath.
    CONDENSE    lv_filenamewithpath. " NO-GAPS.
*{   INSERT         S9SK900449                                        1
* Remove all quotes on Filename
    REPLACE ALL OCCURRENCES OF '"' IN lv_downloaded_file WITH ''.
*}   INSERT

**upload file from presentation server
    CLEAR: t_data_tab[],bin_size.
    CALL FUNCTION 'GUI_UPLOAD'
      EXPORTING
        filename                = lv_downloaded_file
        filetype                = 'BIN'
      IMPORTING
        filelength              = bin_size
*       HEADER                  =
      TABLES
        data_tab                = t_data_tab
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
    IF sy-subrc <> 0.
*     MESSAGE  ID SY-MSGID  TYPE SY-MSGTY  NUMBER SY-MSGNO  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

**get xstring
    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
      EXPORTING
        input_length = bin_size
      IMPORTING
        buffer       = buffer_x
      TABLES
        binary_tab   = t_data_tab
      EXCEPTIONS
        failed       = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.
*     MESSAGE  ID SY-MSGID  TYPE SY-MSGTY  NUMBER SY-MSGNO  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
*{   INSERT         S9SK900449                                        2
*Split Filename and Path for valid zip
    DATA: path    TYPE string,
          folders TYPE TABLE OF string.
    CALL FUNCTION 'SO_SPLIT_FILE_AND_PATH'
      EXPORTING
        full_name     = lv_downloaded_file
      IMPORTING
        stripped_name = filename
        file_path     = path
      EXCEPTIONS
        x_error       = 1
        OTHERS        = 2.

    SPLIT path AT g_fileseperator INTO TABLE folders.
*}   INSERT

**add binary file
*{   REPLACE        S9SK900449                                        4
*\    CALL METHOD zip_tool->add
*\      EXPORTING
*\        name    = lv_DOWNLOADED_FILE
*\        content = buffer_x.
* Using filename without path for ZIP
    CALL METHOD zip_tool->add
      EXPORTING
        name    = filename
        content = buffer_x.
*}   REPLACE

  ENDLOOP.


**get binary ZIP file
  CALL METHOD zip_tool->save
    RECEIVING
      zip = buffer_zip.

  CLEAR: t_data_tab[],bin_size.
  CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
    EXPORTING
      buffer        = buffer_zip
    IMPORTING
      output_length = bin_size
    TABLES
      binary_tab    = t_data_tab.


**download ZIP file
  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      bin_filesize            = bin_size
      filename                = lv_filenamewithpath
      filetype                = 'BIN'
    TABLES
      data_tab                = t_data_tab
    EXCEPTIONS
      file_write_error        = 1
      no_batch                = 2
      gui_refuse_filetransfer = 3
      invalid_type            = 4
      no_authority            = 5
      unknown_error           = 6
      header_not_allowed      = 7
      separator_not_allowed   = 8
      filesize_not_allowed    = 9
      header_too_long         = 10
      dp_error_create         = 11
      dp_error_send           = 12
      dp_error_write          = 13
      unknown_dp_error        = 14
      access_denied           = 15
      dp_out_of_memory        = 16
      disk_full               = 17
      dp_timeout              = 18
      file_not_found          = 19
      dataprovider_exception  = 20
      control_flush_error     = 21
      OTHERS                  = 22.
  IF sy-subrc <> 0.
*   MESSAGE  ID SY-MSGID  TYPE SY-MSGTY  NUMBER SY-MSGNO  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " ADD_TO_ZIP

*&---------------------------------------------------------------------*
*&      Form  SANITIZE_SOURCECODE_PROGNAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<NAMESPACE_NAME>  text
*----------------------------------------------------------------------*
*
FORM sanitize_sourcecode_progname
                 USING    p_name   LIKE trdir-name
                 CHANGING p_result TYPE string       .

  DATA  ev_prefix      TYPE  trnspace-namespace .
  DATA  ev_prefix_name TYPE  trnspace-namespace .
  DATA  ev_stem        LIKE  e071-obj_name .
  DATA  lv_buffer      LIKE  e071-obj_name .

  IF p_name(1) = '/' .  " this name has a namespace prefix

    lv_buffer = p_name .
    CALL FUNCTION 'TRINT_SPLIT_OBJECT'
      EXPORTING
*       IV_OBJECT      =
        iv_obj_name    = lv_buffer
      IMPORTING
        ev_prefix      = ev_prefix
        ev_prefix_name = ev_prefix_name
        ev_stem        = ev_stem
      EXCEPTIONS
        invalid_prefix = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
      MESSAGE ID  sy-msgid TYPE sy-msgty  NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 .
    ENDIF.

    CONCATENATE '%2f' ev_prefix_name '%2f' ev_stem  INTO p_result.

  ELSE.

    p_result = p_name .

  ENDIF.

* REPLACE ALL OCCURRENCES OF '%' IN p_result WITH '%25' .
*   this is clearly not right if there was a namespace prefix.
*   But its kind of fraught anyway because it assumes that we will
*   never be sanitizing a name that was already sanitized.  So
*   I'm just going to cross my fingers and hope that a sanitized
*   name doesn't collide with a name already on the server that is
*   also being downloaded.

  REPLACE ALL OCCURRENCES OF '<' IN p_result WITH '%3C' .
  REPLACE ALL OCCURRENCES OF '>' IN p_result WITH '%3E' .

ENDFORM .    " SANITIZE_SOURCECODE_PROGNAME
*
*
*
TYPES  sourcecodetabletype TYPE TABLE OF string.

TYPES includestmttype      TYPE          string .
TYPES includestmttabletype TYPE TABLE OF includestmttype .

DATA   num_fungroups_downloaded TYPE i VALUE 0 .


FORM download_src_implicit_funcgrp  USING p_dir                  TYPE string
                                          p_functiongroup_name   TYPE progname
                                          p_functioninclude_name TYPE progname
                                          p_ext                  TYPE  char4
                                          p_msg_prefix           TYPE string
                                          p_call_depth           TYPE i        .

  READ TABLE gt_progname_tab WITH TABLE KEY table_line = p_functiongroup_name TRANSPORTING NO FIELDS .
  IF sy-subrc = 0.
    IF  flg_dbg_level > 1 .
      WRITE:  p_functiongroup_name, ' has already been downloaded'.
    ENDIF.
    RETURN.
  ENDIF.
  INSERT p_functiongroup_name INTO TABLE gt_progname_tab.

  IF 1 = 0 .
    IF num_fungroups_downloaded GT max_fungrps .
      IF  flg_dbg_level > 1 .
        DATA  msg_buffer TYPE string .
        CLEAR msg_buffer .
        CONCATENATE  p_msg_prefix  '!!!! too many function groups downloaded'  INTO msg_buffer RESPECTING BLANKS.
        WRITE: / msg_buffer .
      ENDIF.
      RETURN .
    ENDIF.
    ADD 1 TO num_fungroups_downloaded .
  ENDIF.

*
*  If a global function, BAZ, is referenced in a downloaded file but its containing FUNCTION
*  GROUP is not explicitly selected to be downloaded then besided downloading that function module
*  which  contains BAZ we also need to download:
*    1) the root INCLUDE file of BAZ's Function GROUP, and
*    2) the INCLUDE file that contains the global data of the FUNCTION GROUP,
*    3) any INCLUDE files that contain subroutines (aka FORMs) of the FUNCTION GROUP, and
*    4) the ???UXX INCLUDE file that contains INCLUDE statements for all the function
*       modules of the FUNCTION GROUP.
*  We should NOT download the INCLUDE files that contain PAI and PBO code for screens, whose
*  filenames end in ???Inn or ???Onn respectively because they can not be explicitly referenced.
*  Not sure what to do with the include files that contain code for events (???Enn) or
*  local classes (???Pnn).  For now lets just not download them.
*    For any filename that is the object of an INCLUDE statement, we should either download
*  the file with that name or we should write an empty file with that name into the output
*  directory.  This is so that any INCLUDE statement that the ABAP translator runs into will
*  have a target that can be included (perhaps an empty target).  If it later turns out that
*  such a file needs to be downloaded we can just do so and overwrite the empty file without
*  any problem.
  DATA            lt_functiongroup_srccode      TYPE          sourcecodetabletype .
  DATA            lt_include_stmts              TYPE         includestmttabletype .
  FIELD-SYMBOLS   <include_stmt_object>         TYPE         string .
  DATA: lv_functiongroup_basename     TYPE         string,
        lv_functiongroup_basename_len TYPE         i,
        lv_functiongroup_top_name     TYPE         progname,
        lv_functiongroup_uxx_name     TYPE         progname,
        lv_msg_prefix                 TYPE         string.

  CONCATENATE p_msg_prefix '--' INTO lv_msg_prefix RESPECTING BLANKS .

*  Process root file of the FUGR -- as follows:
*  Process include stmt for global data file (L???TOP) -- use normal source file processing
*  Process function modules include file (L???UXX) -- write empty file for all included filenames.
*  Process all other include stmts in this file - write empty file
** lv_functiongroup_basename_len = strlen( p_functiongroup_name ) .
**  SUBTRACT 3 FROM lv_functiongroup_basename_len .
**  lv_functiongroup_basename = p_functioninclude_name(lv_functiongroup_basename_len) .
  lv_functiongroup_basename = p_functioninclude_name .
  CONDENSE lv_functiongroup_basename NO-GAPS .
  SHIFT lv_functiongroup_basename RIGHT CIRCULAR BY 3 PLACES.
  SHIFT lv_functiongroup_basename LEFT           BY 3 PLACES.
  CONCATENATE lv_functiongroup_basename 'TOP' INTO lv_functiongroup_top_name .
  CONCATENATE lv_functiongroup_basename 'UXX' INTO lv_functiongroup_uxx_name .

*    read file p_functiongroup_name into lt_functiongroup_scrcode.
  REFRESH                                           lt_functiongroup_srccode .
  READ REPORT p_functiongroup_name  STATE 'A'  INTO lt_functiongroup_srccode .

  PERFORM parse_include_stmts_into_table USING lt_functiongroup_srccode CHANGING lt_include_stmts.
  LOOP AT lt_include_stmts ASSIGNING <include_stmt_object> .
    IF     <include_stmt_object> = lv_functiongroup_top_name .
      PERFORM download_source_code  USING  p_dir  lv_functiongroup_top_name  abap_filename_ext  lv_msg_prefix  p_call_depth  '' .
    ELSEIF <include_stmt_object> = lv_functiongroup_uxx_name .
      PERFORM download_lyyyyuxx     USING  p_dir  lv_functiongroup_uxx_name          lv_msg_prefix .
    ELSE.
      PERFORM download_empty_file   USING  p_dir  <include_stmt_object>              lv_msg_prefix .
    ENDIF.
  ENDLOOP .

  DATA    download_pathname TYPE   string .
  CLEAR   download_pathname.
  PERFORM assemble_pathname USING  p_dir  p_functiongroup_name abap_filename_ext CHANGING download_pathname .
  PERFORM download_file     USING  lt_functiongroup_srccode
                                   download_pathname
                                   p_functiongroup_name .

ENDFORM.


FORM download_fungrp_form_includes  USING p_dir         TYPE string
                                          p_funmod_name TYPE progname
                                          p_msg_prefix  TYPE string
                                          p_call_depth  TYPE i        .

  DATA            lt_fungrp_srccode      TYPE          sourcecodetabletype .
  DATA            lt_include_stmts       TYPE         includestmttabletype .
  FIELD-SYMBOLS   <include_stmt_object>  TYPE         string .
  DATA: lv_fungrp_name         TYPE         progname,
        lv_fungrp_basename     TYPE         string,
        lv_fungrp_formname     TYPE         string,
        lv_fungrp_basename_len TYPE         i,
        lv_msg_prefix          TYPE         string.

**  lv_fungrp_basename_len = strlen( p_funmod_name ) .
**  lv_fungrp_basename_len = lv_fungrp_basename_len - 3 .
**  lv_fungrp_basename     = p_funmod_name(lv_fungrp_basename_len) .
  lv_fungrp_basename = p_funmod_name .
  CONDENSE lv_fungrp_basename NO-GAPS .
  SHIFT lv_fungrp_basename RIGHT CIRCULAR BY 3 PLACES .
  SHIFT lv_fungrp_basename  LEFT          BY 3 PLACES .
  CONCATENATE       lv_fungrp_basename 'F' INTO lv_fungrp_formname .

  DATA lv_fungrp_scratchspace TYPE progname .
  lv_fungrp_scratchspace = lv_fungrp_formname .
  READ TABLE gt_progname_tab WITH TABLE KEY table_line = lv_fungrp_scratchspace TRANSPORTING NO FIELDS .
  IF sy-subrc = 0.
    IF flg_dbg_level > 1 .
      WRITE:  lv_fungrp_formname, ' has already been downloaded'.
    ENDIF.
    RETURN.
  ENDIF.
  INSERT lv_fungrp_scratchspace INTO TABLE gt_progname_tab.

  CONCATENATE 'SAP' lv_fungrp_basename   INTO lv_fungrp_name .

*   read file lv_fungrp_name             into lt_fungrp_srccode .
  REFRESH                                     lt_fungrp_srccode .
  READ REPORT lv_fungrp_name  STATE 'A'  INTO lt_fungrp_srccode .

  PERFORM parse_include_stmts_into_table USING lt_fungrp_srccode CHANGING lt_include_stmts .

  LOOP AT lt_include_stmts ASSIGNING <include_stmt_object> .
    DATA lv_include_stmt_object_trim TYPE string .
    DATA lv_include_stmt_object_trim_sz TYPE i .
**    lv_include_stmt_object_stripped2_len = strlen( <include_stmt_object> ) .
**    SUBTRACT 2 FROM lv_include_stmt_object_stripped2_len .
    lv_include_stmt_object_trim = <include_stmt_object> .
    SHIFT lv_include_stmt_object_trim RIGHT CIRCULAR BY 2 PLACES .
    SHIFT lv_include_stmt_object_trim  LEFT          BY 2 PLACES .
**    lv_include_stmt_object_stripped2 = lv_include_stmt_object_stripped2(lv_include_stmt_object_stripped2_len) .
    IF lv_include_stmt_object_trim = lv_fungrp_formname .
*                      AND
*                 IS-object[ IS-object-length + 1 ] IS DIGIT
*                      AND
*                 IS-object[ IS-object-length + 2 ] IS DIGIT
      DATA foo TYPE progname_type .
      foo = <include_stmt_object> .
      DATA lv_call_depth TYPE i.
      lv_call_depth = p_call_depth + 1 .
      " we are downloading the forms of this fungrp because the body of the funmod being examined
      " calls one of those FORMs (with the PERFORM stmt) and the call_stack will have one more entry
      " on it if this FORM module code contains a reference to an external function.
      PERFORM download_source_code USING  p_dir  foo  abap_filename_ext  p_msg_prefix  lv_call_depth  '' .
    ENDIF .
  ENDLOOP .

ENDFORM.          " download_fungrp_form_includes


FORM download_lyyyyuxx  USING p_dir        TYPE string
                              p_uxxname    TYPE progname
                              p_msg_prefix TYPE string .

  DATA          lt_uxx_srccode        TYPE  sourcecodetabletype .
  DATA          lt_include_stmts      TYPE includestmttabletype .
  FIELD-SYMBOLS <include_stmt_object> TYPE      includestmttype .

*   read file p_UXXname into lt_uxx_srccode .
  REFRESH                                lt_uxx_srccode .
  READ REPORT p_uxxname  STATE 'A'  INTO lt_uxx_srccode .

  PERFORM parse_include_stmts_into_table USING lt_uxx_srccode CHANGING lt_include_stmts .

  LOOP AT lt_include_stmts ASSIGNING <include_stmt_object> .
    PERFORM download_empty_file USING  p_dir  <include_stmt_object>  p_msg_prefix .
  ENDLOOP .

  DATA    download_pathname TYPE string .
  CLEAR   download_pathname .
  PERFORM assemble_pathname USING  p_dir  p_uxxname  abap_filename_ext CHANGING download_pathname .
  PERFORM download_file     USING  lt_uxx_srccode  download_pathname  p_uxxname .
ENDFORM.


FORM  parse_include_stmts_into_table    USING p_srccode      TYPE sourcecodetabletype
                                     CHANGING p_inc_stmt_tbl TYPE includestmttabletype.

  DATA lt_keyword_tbl TYPE TABLE OF char30 .
  DATA lt_token_tbl   TYPE TABLE OF stokes .
  DATA lt_stmt_tbl    TYPE TABLE OF sstmnt .

  CLEAR: lt_keyword_tbl , lt_token_tbl , lt_stmt_tbl .
  APPEND 'INCLUDE' TO lt_keyword_tbl .

  SCAN ABAP-SOURCE p_srccode TOKENS INTO lt_token_tbl STATEMENTS INTO lt_stmt_tbl KEYWORDS FROM lt_keyword_tbl.

  IF lt_stmt_tbl IS NOT INITIAL .
    FIELD-SYMBOLS  <token_line> TYPE stokes .
    FIELD-SYMBOLS   <stmt_line> TYPE sstmnt .
    DATA             lv_tabix   TYPE sytabix .

    CLEAR p_inc_stmt_tbl .
    LOOP AT lt_stmt_tbl ASSIGNING <stmt_line> .
      IF <stmt_line>-type = 'I' .
*         by design the first token of any entry in lt_stmt_tbl must be INCLUDE identifier
        lv_tabix = <stmt_line>-from + 1 .
        READ TABLE lt_token_tbl ASSIGNING <token_line> INDEX lv_tabix .
        IF <stmt_line>-type = 'I' .   " i.e. this is an 'INCLUDE progname' stmt
*           by definition the second (and final) token of an INCLUDE stmt mus be an identifier
          DATA lv_prog LIKE trdir-name . " the name of the table that holds the text of the include file
          DATA lv_prog_len TYPE i.

          lv_prog = <token_line>-str .
          lv_prog_len = strlen( lv_prog ) .

          DATA  lv_sanitized TYPE string .
          CLEAR lv_sanitized.
          PERFORM sanitize_sourcecode_progname USING lv_prog CHANGING lv_sanitized .
          DATA lv_buffer        TYPE string .
          DATA lv_row           TYPE string .
          DATA lv_prefix_len    TYPE i .
          FIELD-SYMBOLS <lv_row> TYPE string .
          lv_prefix_len = <token_line>-col .
          READ TABLE p_srccode ASSIGNING <lv_row> INDEX <token_line>-row .
          lv_buffer = <lv_row>.
          CLEAR lv_row.
          CONCATENATE lv_buffer(lv_prefix_len) lv_sanitized INTO lv_row.
          SHIFT  lv_buffer LEFT BY lv_prefix_len  PLACES .
          SHIFT  lv_buffer LEFT BY lv_prog_len    PLACES .
          CONCATENATE  lv_row lv_buffer  INTO lv_row .
          <lv_row> = lv_row.

          APPEND lv_prog TO p_inc_stmt_tbl .
        ENDIF .
      ENDIF .
    ENDLOOP.
  ENDIF.

ENDFORM.             " parse_include_stmts_into_table


FORM download_empty_file USING p_dir        TYPE string
                               p_filename   TYPE string
                               p_msg_prefix TYPE string .

  DATA  empty_table TYPE TABLE OF string .
  CLEAR empty_table .

  DATA lv_progress_msg TYPE string.
  CONCATENATE ' empty file as ' p_filename INTO lv_progress_msg RESPECTING BLANKS.

  DATA    download_pathname TYPE string .
  CLEAR   download_pathname.
  PERFORM assemble_pathname USING  p_dir  p_filename abap_filename_ext CHANGING download_pathname .
  PERFORM download_file     USING empty_table download_pathname lv_progress_msg .

ENDFORM.     " download_empty_file

FORM _get_all_class_includes     USING     class_name   TYPE seoclsname
                                 CHANGING  result       TYPE seoincl_t.
*                                RAISING   NO_SUCH_CLASS.
  DATA: clskey          TYPE seoclskey,
        class_info      TYPE vseoclass,
        method_includes TYPE seop_methods_w_include,
        wa_result       TYPE programm.

  CLEAR result.
  " call function module SEO_CLASS_GET to retrieve info which include names are available for a class
  clskey-clsname = class_name.
  CALL FUNCTION 'SEO_CLASS_GET'
    EXPORTING
      clskey       = clskey
    IMPORTING
      class        = class_info
    EXCEPTIONS
      not_existing = 1
      deleted      = 2
      is_interface = 3
      model_only   = 4
      OTHERS       = 5.
  IF sy-subrc <> 0.
    " error -> just return an empty list
*    raise no_such_class.
    RETURN.
  ENDIF.
  " Local Classes (new or orld) = if VSEOCLASS-CLSCCINCL = 'X' -> CCDEF, CCMAC, CCIMP; if blank -> CL
  IF class_info-clsccincl = 'X'.
    "CCDEF
*   wa_result = cl_oo_classname_service=>get_ccdef_name( class_name ).
    CLEAR wa_result.
    PERFORM get_ccdef_name USING class_name CHANGING wa_result.
    INSERT wa_result INTO TABLE result.
    "CCMAC
*   wa_result = cl_oo_classname_service=>get_ccmac_name( class_name ).
    CLEAR wa_result.
    PERFORM get_ccmac_name USING class_name CHANGING wa_result.
    INSERT wa_result INTO TABLE result.
    "CCIMP
*   wa_result = cl_oo_classname_service=>get_ccimp_name( class_name ).
    CLEAR wa_result.
    PERFORM get_ccimp_name USING class_name CHANGING wa_result.
    INSERT wa_result INTO TABLE result.
  ELSEIF class_info-clsccincl = ''.
    "CL
*   wa_result = cl_oo_classname_service=>get_cl_name( class_name ).
    CLEAR wa_result.
    PERFORM get_cl_name USING class_name CHANGING wa_result.
    INSERT wa_result INTO TABLE result.
  ENDIF.


  " Abap Unit = if VSEOCLASS-WITH_UNIT_TEST -> CCAU
  IF class_info-with_unit_tests = 'X'.
*   wa_result = cl_oo_classname_service=>get_ccau_name( class_name ).
    CLEAR wa_result.
    PERFORM get_ccau_name USING class_name CHANGING wa_result.
    INSERT wa_result INTO TABLE result.
  ENDIF.

*  " Package Section = if VSEOCLASS-WITHIN_PACKAGE -> CA
*  if class_info-within_package = 'X'.
*    wa_result = cl_oo_classname_service=>get_paksec_name( class_name ).
*    insert wa_result into table result.
*  endif.

  " following include name are always relevant
  " CU = public section
  " CO = protected section
  " CI = private section
  " CP = class pool
  " CT = used internally to store compiler optimizations??
  " CS = complete source - currently not used!
* wa_result = zcl_oo_classname_service=>get_pubsec_name( class_name ).
  CLEAR wa_result.
  PERFORM get_pubsec_name USING class_name CHANGING wa_result.
  INSERT wa_result INTO TABLE result.
* wa_result = zcl_oo_classname_service=>get_prosec_name( class_name ).
  CLEAR wa_result.
  PERFORM get_prosec_name USING class_name CHANGING wa_result.
  INSERT wa_result INTO TABLE result.
* wa_result = zcl_oo_classname_service=>get_prisec_name( class_name ).
  CLEAR wa_result.
  PERFORM get_prisec_name USING class_name CHANGING wa_result.
  INSERT wa_result INTO TABLE result.
* wa_result = zcl_oo_classname_service=>get_classpool_name( class_name ).
  CLEAR wa_result.
  PERFORM get_classpool_name USING class_name CHANGING wa_result.
  INSERT wa_result INTO TABLE result.
* wa_result = zcl_oo_classname_service=>get_ct_name( class_name ).
  CLEAR wa_result.
  PERFORM get_ct_name USING class_name CHANGING wa_result.
  INSERT wa_result INTO TABLE result.
* wa_result = zcl_oo_classname_service=>get_cs_name( class_name ).
  CLEAR wa_result.
  PERFORM get_cs_name USING class_name CHANGING wa_result.
  INSERT wa_result INTO TABLE result.

  " get all method include names
* method_includes = cl_oo_classname_service=>get_all_method_includes( clsname = class_name ).
  CLEAR method_includes.
  PERFORM _get_all_method_includes USING class_name CHANGING method_includes.
  FIELD-SYMBOLS: <method_include> TYPE seop_method_w_include.
  LOOP AT method_includes ASSIGNING <method_include>.
    INSERT <method_include>-incname INTO TABLE result.
  ENDLOOP.
ENDFORM.     " get_all_class_includes

FORM get_ccdef_name   USING    clsname TYPE seoclsname
                      CHANGING result  TYPE programm.
  DATA ps TYPE progstruc.
  ps-rootname = clsname.
  TRANSLATE ps-rootname USING ' ='.
  ps-categorya = seop_inctype_class.
  ps-codea = seop_incextapp_definition+1(4).

  result = ps.
ENDFORM.

FORM get_ccmac_name   USING    clsname TYPE seoclsname
                      CHANGING result  TYPE programm.
  DATA ps TYPE progstruc.
  ps-rootname = clsname.
  TRANSLATE ps-rootname USING ' ='.
  ps-categorya = seop_inctype_class.
  ps-codea = seop_incextapp_macros+1(4).

  result = ps.
ENDFORM.

FORM get_ccimp_name   USING    clsname TYPE seoclsname
                      CHANGING result  TYPE programm.

  DATA ps TYPE progstruc.
  ps-rootname = clsname.
  TRANSLATE ps-rootname USING ' ='.
  ps-categorya = seop_inctype_class.
  ps-codea = seop_incextapp_implementation+1(4).

  result = ps.
ENDFORM.

FORM  get_cl_name   USING    clsname TYPE seoclsname
                    CHANGING result  TYPE programm.

  DATA ps TYPE progstruc.
  ps-rootname = clsname.
  TRANSLATE ps-rootname USING ' ='.
  ps-categorya = seop_inctype_class.
  ps-codea = seop_inccode_local.

  result = ps.
ENDFORM.

FORM  get_ccau_name   USING    clsname TYPE seoclsname
                      CHANGING result  TYPE programm.

  DATA ps TYPE progstruc.
  ps-rootname = clsname.
  TRANSLATE ps-rootname USING ' ='.
  ps-categorya = seop_inctype_class.
  ps-codea = 'CAU'.

  result = ps.

ENDFORM.

FORM  get_pubsec_name   USING    clsname TYPE seoclsname
                        CHANGING result  TYPE programm.

  DATA ps TYPE progstruc.
  ps-rootname = clsname.
  TRANSLATE ps-rootname USING ' ='.
  ps-categorya = seop_inctype_class.
  ps-codea = seop_inccode_public.

  result = ps.

ENDFORM.

FORM  get_prosec_name   USING    clsname TYPE seoclsname
                        CHANGING result  TYPE programm.

  DATA ps TYPE progstruc.
  ps-rootname = clsname.
  TRANSLATE ps-rootname USING ' ='.
  ps-categorya = seop_inctype_class.
  ps-codea = seop_inccode_protected.

  result = ps.

ENDFORM.

FORM  get_prisec_name   USING    clsname TYPE seoclsname
                        CHANGING result  TYPE programm.

  DATA ps TYPE progstruc.
  ps-rootname = clsname.
  TRANSLATE ps-rootname USING ' ='.
  ps-categorya = seop_inctype_class.
  ps-codea = seop_inccode_private.

  result = ps.

ENDFORM.

FORM  get_classpool_name   USING    clsname TYPE seoclsname
                           CHANGING result  TYPE programm.

  DATA ps TYPE progstruc.
  ps-rootname = clsname.
  TRANSLATE ps-rootname USING ' ='.
  ps-categorya = seop_inctype_class.
  ps-codea = seop_inccode_pool.

  result = ps.

ENDFORM.

FORM  get_ct_name   USING    clsname TYPE seoclsname
                    CHANGING result  TYPE programm.
  DATA ps TYPE progstruc.
  ps-rootname = clsname.
  TRANSLATE ps-rootname USING ' ='.
  ps-categorya = seop_inctype_class.
  ps-codea = 'T'.
  result = ps.
ENDFORM.

FORM get_cs_name   USING    clsname TYPE seoclsname
                   CHANGING result  TYPE programm.
  DATA ps TYPE progstruc.
  ps-rootname = clsname.
  TRANSLATE ps-rootname USING ' ='.
  ps-categorya = seop_inctype_class.
  ps-codea = 'S'.

  result = ps.
ENDFORM.

FORM _get_all_method_includes   USING    clsname TYPE seoclsname
                                CHANGING result  TYPE seop_methods_w_include.
  DATA:
    progname TYPE programm,
    i_tmdir  TYPE STANDARD TABLE OF tmdir,
    mwi      TYPE seop_method_w_include.
  FIELD-SYMBOLS <tmdir> TYPE tmdir.

  SYSTEM-CALL QUERY CLASS clsname.
  IF sy-subrc <> 0.
    RAISE class_not_existing.
  ENDIF.

  SELECT * FROM tmdir INTO TABLE i_tmdir  WHERE classname = clsname.

  LOOP AT i_tmdir
    ASSIGNING <tmdir>.
    CHECK <tmdir>-methodindx <> 0.
    SYSTEM-CALL QUERY METHOD   <tmdir>-methodname
                      OF CLASS <tmdir>-classname
                      INCLUDE INTO progname NO DBLOCK.
    mwi-cpdkey-clsname = <tmdir>-classname.
    mwi-cpdkey-cpdname = <tmdir>-methodname.
    mwi-incname = progname.
    APPEND  mwi  TO result.
  ENDLOOP.

ENDFORM.

" Gets the namespace for the object
FORM get_namespace USING prog_id TYPE tadir-pgmid
         obj TYPE tadir-object
         obj_name TYPE tadir-obj_name CHANGING prog_ns TYPE trnspace-namespace.
  DATA lv_prog_ns TYPE trnspace-namespace.
  CALL FUNCTION 'TRINT_GET_NAMESPACE'
    EXPORTING
      iv_pgmid       = prog_id
      iv_object      = obj
      iv_obj_name    = obj_name
    IMPORTING
      ev_namespace   = lv_prog_ns
    EXCEPTIONS
      invalid_prefix = 1
      OTHERS         = 2.

  IF sy-subrc <> 0.
    IF flg_dbg_level > 1.
      WRITE: /, 'Failed to get namespace for ', prog_id.
    ENDIF.
  ELSE.
    prog_ns = lv_prog_ns.
  ENDIF.
ENDFORM.

FORM is_custom_ns USING prog_name CHANGING is_custom TYPE abap_bool.
  TYPES: ty_prog_data TYPE tadir. " Type to capture basic program data
  DATA : lv_prog_data     TYPE ty_prog_data. " structure to capture basic program data to query object namespace

  SELECT SINGLE *
  FROM   tadir
  INTO   lv_prog_data
  WHERE  obj_name = prog_name.

  IF sy-subrc <> 0 AND lv_prog_data IS NOT INITIAL.
    DATA: prog_ns TYPE trnspace-namespace.
    PERFORM get_namespace USING lv_prog_data-pgmid lv_prog_data-object lv_prog_data-obj_name CHANGING prog_ns.
    IF prog_ns IS NOT INITIAL AND prog_ns <> customer_ns.
      is_custom = abap_false.
    ELSE.
      is_custom = abap_true.
    ENDIF.
  ENDIF.
ENDFORM.

" Checks if the object is a customer defined function group or its include program.
" Customer defined function module is an include program with the prefix "LZ" and the ending "UXX"
" in the implementation section of the function group
FORM is_custom_fn_grp USING prog_name CHANGING is_non_std TYPE abap_bool.
  IF prog_name CP 'SAPLY*' OR prog_name CP 'SAPLZ*' OR prog_name CP 'LY*' OR prog_name CP 'LZ*'.
    is_non_std = abap_true.
  ELSE.
    is_non_std = abap_false.
  ENDIF.
ENDFORM.

FORM is_custom_prog USING prog_name CHANGING is_non_std TYPE abap_bool.
  IF prog_name CP 'Z*' OR prog_name CP '_*' OR prog_name CP 'Y*'.
    is_non_std = abap_true.
    RETURN.
  ELSE.
    is_non_std = abap_false.
  ENDIF.
ENDFORM.

" Checks if the program/object is a customer defined one.
" The extractor functionality checks for patterns Y*, Z* and _* to assert that the program is a customer one.
" It used to miss out on function groups and modules as they are prefixed with SAPL and L respectively.
" For function groups and modules, the object namespace is fetched and then the prefix is checked.
" For function group, the namespace is matched and checked whether the name starts with SAPLZ or SAPLY.
" Similarly for function module, the namespace is matched and checked whether the name starts with LZ or LY.
" Note: Just checking the namespace causes a lot of files to be extracted even with standard SAP program
" field unchecked. We will maintain checking for patterns Y* and Z* to identify a custom object
" in the case of function groups and modules as well.
FORM is_non_std_sap_prog USING prog_name CHANGING is_non_std TYPE abap_bool.
  PERFORM is_custom_prog USING prog_name CHANGING is_non_std.

  IF is_non_std = abap_true.
    RETURN.
  ENDIF.

  PERFORM is_custom_fn_grp USING prog_name CHANGING is_non_std.
ENDFORM.

TYPES: tty_string TYPE TABLE OF string.

FORM serialize_abap_oo_source USING p_if_name TYPE string CHANGING p_source TYPE tty_string.
  DATA: lo_source   TYPE REF TO object,
        lo_instance TYPE REF TO object,
        lv_tadir    TYPE tadir.

  SELECT SINGLE *
  FROM   tadir
  INTO   lv_tadir
  WHERE  obj_name = p_if_name
  AND    object   = 'INTF'
  AND    pgmid    = 'R3TR'.

  IF sy-subrc = 0.
    TRY.
        CALL METHOD ('CL_OO_FACTORY')=>('CREATE_INSTANCE')
          RECEIVING
            result = lo_instance.

        CALL METHOD lo_instance->('CREATE_CLIF_SOURCE')
          EXPORTING
            clif_name = p_if_name
            version   = 'A'
          RECEIVING
            result    = lo_source.

        CALL METHOD lo_source->('GET_SOURCE')
          IMPORTING
            source = p_source.

        IF flg_dbg_level > 4.
          WRITE: /, 'Got the source for ', p_if_name.
        ENDIF.
      CATCH cx_root.
        IF flg_dbg_level > 1.
          WRITE: /, 'Failed to get source for ', p_if_name.
        ENDIF.
    ENDTRY.
  ENDIF.
ENDFORM.

FORM fetch_object USING p_obj_name TYPE string CHANGING p_tadir TYPE tadir.
  SELECT SINGLE * FROM tadir INTO p_tadir  WHERE pgmid = 'R3TR'  AND obj_name = p_obj_name.
ENDFORM.

FORM _fetch_object USING p_obj_name TYPE string p_obj_type TYPE string CHANGING p_tadir TYPE tadir.
  SELECT SINGLE * FROM tadir INTO p_tadir  WHERE pgmid = 'R3TR'  AND obj_name = p_obj_name AND object = p_obj_type.
ENDFORM.

FORM create_file USING p_itab p_filename p_progname.
  DATA: lv_download_pathname TYPE string.
  PERFORM assemble_pathname USING p_temp p_filename abap_filename_ext CHANGING lv_download_pathname.
  PERFORM download_file USING p_itab lv_download_pathname p_progname.
ENDFORM.

FORM create_file_default USING p_itab p_progname.
  DATA: lv_download_pathname TYPE string.
  PERFORM assemble_pathname USING p_temp p_progname abap_filename_ext CHANGING lv_download_pathname.
  PERFORM download_file USING p_itab lv_download_pathname p_progname.
ENDFORM.

FORM sha1_string USING p_data CHANGING cv_sha1.

  DATA: lv_hash  TYPE string,
        lv_key   TYPE xstring,
        lx_error TYPE REF TO cx_abap_message_digest.

  TRY.
      CALL METHOD cl_abap_hmac=>calculate_hmac_for_char
        EXPORTING
          if_key        = lv_key
          if_data       = p_data
        IMPORTING
          ef_hmacstring = lv_hash.

      cv_sha1 = lv_hash.
      TRANSLATE cv_sha1 TO LOWER CASE.
    CATCH cx_abap_message_digest INTO lx_error.
      IF flg_dbg_level > 1.
        WRITE: /, 'Failed to get SHA1 string for ', p_data.
      ENDIF.
  ENDTRY.
ENDFORM.

FORM gen_random_string USING p_len TYPE integer
                    CHANGING p_str.
  CALL FUNCTION 'GENERAL_GET_RANDOM_STRING'
    EXPORTING
      number_chars  = p_len
    IMPORTING
      random_string = p_str.
ENDFORM.

FORM gen_alphanum_str USING iv_current_string TYPE string
                   CHANGING rv_next_string    TYPE string.
  CONSTANTS:
    lv_charset       TYPE string VALUE '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ', " Define the alphanumeric characters
    lv_string_length TYPE i      VALUE 3. " The desired length of the string

  DATA:
    lv_char_pos_from_left TYPE i, " 0-based index from the left
    lv_char_idx_in_set    TYPE i,
    lv_current_char       TYPE c LENGTH 1,
    lv_charset_len        TYPE i.
  lv_charset_len = strlen( lv_charset ).
  DATA:
    lv_temp_string        TYPE c LENGTH lv_string_length.

  " Basic input validation: Check if length matches
  IF strlen( iv_current_string ) <> lv_string_length.
    RETURN.
  ENDIF.

  lv_temp_string = iv_current_string.

  " Iterate from the rightmost character to the leftmost
  " sy-index will go from 1 to gc_string_length
  DO lv_string_length TIMES.
    " Calculate 0-based position from the left (e.g., for length 3: 2, 1, 0)
    lv_char_pos_from_left = lv_string_length - sy-index.

    lv_current_char = lv_temp_string+lv_char_pos_from_left(1).

    " Find the current character in our defined character set
    FIND lv_current_char IN lv_charset MATCH OFFSET lv_char_idx_in_set.

    IF sy-subrc <> 0.
      " Character from input string is not found in our allowed charset
      RETURN.
    ENDIF.

    IF lv_char_idx_in_set < lv_charset_len - 1.
      " Current character is not the last one in the charset (e.g., not 'Z')
      " So, we can simply increment it
      lv_char_idx_in_set = lv_char_idx_in_set + 1.
      lv_temp_string+lv_char_pos_from_left(1) = lv_charset+lv_char_idx_in_set(1).
      rv_next_string = lv_temp_string.
      RETURN. " Next string found, exit
    ELSE.
      " Current character is the last one in the charset (e.g., 'Z')
      " Reset this character to the first in the charset (e.g., '0')
      lv_temp_string+lv_char_pos_from_left(1) = lv_charset(1). " gc_charset(1) is the first char

      " If this was the leftmost character that we just reset (e.g., "ZZZ" -> "000")
      " it means we've wrapped around. The lv_temp_string is now the first string.
      IF lv_char_pos_from_left = 0.
        rv_next_string = lv_temp_string. " e.g., "000" after "ZZZ"
        RETURN. " Wrapped around, exit
      ENDIF.
      " Otherwise, continue to the next character to the left (carry-over)
    ENDIF.
  ENDDO.

  " This line should ideally not be reached if the logic for wrap-around (e.g. ZZZ -> 000)
  " is handled correctly within the loop. However, as a safeguard,
  " if the loop finishes (e.g., after processing ZZZ, lv_temp_string is 000),
  " rv_next_string will hold the wrapped-around value.
  rv_next_string = lv_temp_string.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_WDYN_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PV_OBJ_NAME   TYPE  text
*      -->SOBJ_NAME         text
*      -->PP_TMP            text
*      -->P_NAME            text
*      -->P_WDYN_COMP       text
*      -->P_LT_TABLE        text
*----------------------------------------------------------------------*
FORM download_wdyn_data  USING
      pv_obj_name  TYPE sobj_name
      pp_tmp TYPE string
      p_name TYPE string
      p_wdyn_comp TYPE wdy_component_name
      p_lt_table.

  FIELD-SYMBOLS: <lt_table> TYPE ANY TABLE .

  TYPES: xmlline(1024) TYPE x.

  DATA: lt_xml TYPE STANDARD TABLE OF xmlline.
  DATA: ls_filename     TYPE string,
        ls_obj_name     TYPE string,
        lv_rc           TYPE i,
        lv_wdy_compname TYPE string.

  " Any exit condition?

  CONCATENATE p_wdyn_comp '.wdy' INTO lv_wdy_compname.
  ls_obj_name = pv_obj_name.
  "ls_filename = pp_tmp.
  CONDENSE pp_tmp.
  CONCATENATE pp_tmp g_fileseperator '_META-INF' g_fileseperator lv_wdy_compname g_fileseperator ls_obj_name g_fileseperator p_name '.MTD' INTO ls_FILENAME  .
  CONDENSE ls_filename. " no-gaps.

  ASSIGN p_lt_table  TO <lt_table> .

  IF <lt_table>[] IS INITIAL.
    RETURN.
  ENDIF.

  CALL TRANSFORMATION id
    SOURCE data_node = <lt_table>
    RESULT XML lt_xml.

  " CL_ABAP_STRUCTDESCR
  WRITE: / 'Downloading: ', ls_FILENAME.

  CALL METHOD cl_gui_frontend_services=>gui_download
    EXPORTING
      filename = ls_FILENAME
      filetype = 'BIN'
    CHANGING
      data_tab = lt_xml.

  INSERT ls_FILENAME INTO TABLE downloaded_files.

ENDFORM.                    " DOWNLOAD_WDYN_DATA

FORM download_wdyn_metadata USING p_progname TYPE tadir-obj_name.
  "First get all the metadata from the database
  "then download the data to the local file system

  DATA lv_name TYPE  wdy_component_name.
  lv_name = p_progname.

  "Web Dynpro Component Data

  DATA lt_wdy_component       TYPE TABLE OF wdy_component.
  DATA lt_wdy_componentt      TYPE TABLE OF wdy_componentt.
  DATA lt_wdy_controller      TYPE TABLE OF wdy_controller.
  DATA lt_wdy_controllert     TYPE TABLE OF wdy_controllert.
  DATA lt_wdy_ctlr_compo      TYPE TABLE OF wdy_ctlr_compo .
  DATA lt_wdy_ctlr_compot     TYPE TABLE OF wdy_ctlr_compot.
  DATA lt_wdy_ctlr_param      TYPE TABLE OF wdy_ctlr_param.
  DATA lt_wdy_ctlr_paramt     TYPE TABLE OF wdy_ctlr_paramt.
  DATA lt_wdy_compo_usage     TYPE TABLE OF wdy_compo_usage.
  DATA lt_wdy_ctlr_usage      TYPE STANDARD TABLE OF wdy_ctlr_usage.
  DATA lt_wdy_ctx_node        TYPE STANDARD TABLE OF wdy_ctx_node.
  DATA lt_wdy_ctx_attrib      TYPE STANDARD TABLE OF wdy_ctx_attrib.
  DATA lt_wdy_ctx_mapping     TYPE STANDARD TABLE OF wdy_ctx_mapping.

  "Web Dynpro View Data

  DATA lt_wdy_viewt           TYPE STANDARD TABLE OF wdy_viewt.
  DATA lt_wdy_iobound_plug    TYPE STANDARD TABLE OF wdy_iobound_plug.
  DATA lt_wdy_iobound_plgt    TYPE STANDARD TABLE OF wdy_iobound_plgt.
  DATA lt_wdy_ui_element      TYPE STANDARD TABLE OF wdy_ui_element.
  DATA lt_wdy_ui_property     TYPE STANDARD TABLE OF wdy_ui_property.
  DATA lt_wdy_view_cntr       TYPE STANDARD TABLE OF wdy_view_cntr.
  DATA lt_wdy_view_cntrt      TYPE STANDARD TABLE OF wdy_view_cntrt.
  DATA lt_wdy_plug_param      TYPE STANDARD TABLE OF wdy_plug_param.
  DATA lt_wdy_ui_ctx_bind     TYPE STANDARD TABLE OF wdy_ui_ctx_bind.
  DATA lt_wdy_ui_ddic_bind    TYPE STANDARD TABLE OF wdy_ui_ddic_bind.
  DATA lt_wdy_ui_evt_bind     TYPE STANDARD TABLE OF wdy_ui_evt_bind.
  DATA lt_wdy_nav_link        TYPE STANDARD TABLE OF wdy_nav_link.
  DATA lt_wdy_nav_targref     TYPE STANDARD TABLE OF wdy_nav_targref.
  DATA lt_wdy_vsh_node        TYPE STANDARD TABLE OF wdy_vsh_node.
  DATA lt_wdy_vsh_pholder     TYPE STANDARD TABLE OF wdy_vsh_pholder.
  DATA lt_wdy_vs_property     TYPE STANDARD TABLE OF wdy_vs_property.

  "Fetch the component data

  SELECT * FROM wdy_component    INTO TABLE lt_wdy_component     WHERE component_name = lv_name .
  SELECT * FROM wdy_componentt   INTO TABLE lt_wdy_componentt    WHERE component_name = lv_name AND langu = sy-langu .

  SELECT * FROM wdy_controller   INTO TABLE lt_wdy_controller    WHERE component_name = lv_name .
  SELECT * FROM wdy_controllert  INTO TABLE lt_wdy_controllert   WHERE component_name = lv_name AND langu = sy-langu .

  SELECT * FROM wdy_ctlr_compo   INTO TABLE lt_wdy_ctlr_compo    WHERE component_name = lv_name .
  SELECT * FROM wdy_ctlr_compot  INTO TABLE lt_wdy_ctlr_compot   WHERE component_name = lv_name AND langu = sy-langu .

  SELECT * FROM wdy_ctlr_param   INTO TABLE lt_wdy_ctlr_param    WHERE component_name = lv_name .
  SELECT * FROM wdy_ctlr_paramt  INTO TABLE lt_wdy_ctlr_paramt   WHERE component_name = lv_name AND langu = sy-langu .

  SELECT * FROM wdy_ctlr_usage   INTO TABLE lt_wdy_ctlr_usage    WHERE component_name = lv_name .
  SELECT * FROM wdy_compo_usage  INTO TABLE lt_wdy_compo_usage   WHERE component_name = lv_name .
  SELECT * FROM wdy_ctx_node     INTO TABLE lt_wdy_ctx_node      WHERE component_name = lv_name .
  SELECT * FROM wdy_ctx_attrib   INTO TABLE lt_wdy_ctx_attrib    WHERE component_name = lv_name .
  SELECT * FROM wdy_ctx_mapping  INTO TABLE lt_wdy_ctx_mapping   WHERE component_name = lv_name .

  "Fetch the view data

  SELECT * FROM wdy_viewt        INTO TABLE lt_wdy_viewt         WHERE component_name = lv_name .
  SELECT * FROM wdy_iobound_plug INTO TABLE lt_wdy_iobound_plug  WHERE component_name = lv_name .
  SELECT * FROM wdy_iobound_plgt INTO TABLE lt_wdy_iobound_plgt  WHERE component_name = lv_name .
  SELECT * FROM wdy_ui_element   INTO TABLE lt_wdy_ui_element    WHERE component_name = lv_name .
  SELECT * FROM wdy_ui_property  INTO TABLE lt_wdy_ui_property   WHERE component_name = lv_name .
  SELECT * FROM wdy_view_cntr    INTO TABLE lt_wdy_view_cntr     WHERE component_name = lv_name .
  SELECT * FROM wdy_view_cntrt   INTO TABLE lt_wdy_view_cntrt    WHERE component_name = lv_name .
  SELECT * FROM wdy_plug_param   INTO TABLE lt_wdy_plug_param    WHERE component_name = lv_name .
  SELECT * FROM wdy_ui_ctx_bind  INTO TABLE lt_wdy_ui_ctx_bind   WHERE component_name = lv_name .
  SELECT * FROM wdy_ui_ddic_bind INTO TABLE lt_wdy_ui_ddic_bind  WHERE component_name = lv_name .
  SELECT * FROM wdy_ui_evt_bind  INTO TABLE lt_wdy_ui_evt_bind   WHERE component_name = lv_name .
  SELECT * FROM wdy_nav_link     INTO TABLE lt_wdy_nav_link      WHERE component_name = lv_name .
  SELECT * FROM wdy_nav_targref  INTO TABLE lt_wdy_nav_targref   WHERE component_name = lv_name .
  SELECT * FROM wdy_vsh_node     INTO TABLE lt_wdy_vsh_node      WHERE component_name = lv_name .
  SELECT * FROM wdy_vsh_pholder  INTO TABLE lt_wdy_vsh_pholder   WHERE component_name = lv_name .
  SELECT * FROM wdy_vs_property  INTO TABLE lt_wdy_vs_property   WHERE component_name = lv_name .

  PERFORM download_wdyn_data USING  p_progname p_temp 'lt_wdy_component'    lv_name lt_wdy_component    .
  PERFORM download_wdyn_data USING  p_progname p_temp 'lt_wdy_componentt'   lv_name lt_wdy_componentt   .
  PERFORM download_wdyn_data USING  p_progname p_temp 'lt_wdy_controller'   lv_name lt_wdy_controller   .
  PERFORM download_wdyn_data USING  p_progname p_temp 'lt_wdy_controllert'  lv_name lt_wdy_controllert  .
  PERFORM download_wdyn_data USING  p_progname p_temp 'lt_wdy_ctlr_compo'   lv_name lt_wdy_ctlr_compo   .
  PERFORM download_wdyn_data USING  p_progname p_temp 'lt_wdy_ctlr_compot'  lv_name lt_wdy_ctlr_compot  .
  PERFORM download_wdyn_data USING  p_progname p_temp 'lt_wdy_ctlr_param'   lv_name lt_wdy_ctlr_param   .
  PERFORM download_wdyn_data USING  p_progname p_temp 'lt_wdy_ctlr_param'   lv_name lt_wdy_ctlr_param   .
  PERFORM download_wdyn_data USING  p_progname p_temp 'lt_wdy_ctlr_paramt'  lv_name lt_wdy_ctlr_paramt  .
  PERFORM download_wdyn_data USING  p_progname p_temp 'lt_wdy_compo_usage'  lv_name lt_wdy_compo_usage  .
  PERFORM download_wdyn_data USING  p_progname p_temp 'lt_wdy_ctlr_usage'   lv_name lt_wdy_ctlr_usage   .
  PERFORM download_wdyn_data USING  p_progname p_temp 'lt_wdy_ctx_node'     lv_name lt_wdy_ctx_node     .
  PERFORM download_wdyn_data USING  p_progname p_temp 'lt_wdy_ctx_attrib'   lv_name lt_wdy_ctx_attrib   .
  PERFORM download_wdyn_data USING  p_progname p_temp 'lt_wdy_ctx_mapping'  lv_name lt_wdy_ctx_mapping  .
  PERFORM download_wdyn_data USING  p_progname p_temp 'lt_wdy_viewt'        lv_name lt_wdy_viewt        .
  PERFORM download_wdyn_data USING  p_progname p_temp 'lt_wdy_iobound_plug' lv_name lt_wdy_iobound_plug .
  PERFORM download_wdyn_data USING  p_progname p_temp 'lt_wdy_iobound_plgt' lv_name lt_wdy_iobound_plgt .
  PERFORM download_wdyn_data USING  p_progname p_temp 'lt_wdy_ui_element'   lv_name lt_wdy_ui_element   .
  PERFORM download_wdyn_data USING  p_progname p_temp 'lt_wdy_ui_property'  lv_name lt_wdy_ui_property  .
  PERFORM download_wdyn_data USING  p_progname p_temp 'lt_wdy_view_cntr'    lv_name lt_wdy_view_cntr    .
  PERFORM download_wdyn_data USING  p_progname p_temp 'lt_wdy_view_cntrt'   lv_name lt_wdy_view_cntrt   .
  PERFORM download_wdyn_data USING  p_progname p_temp 'lt_wdy_plug_param'   lv_name lt_wdy_plug_param   .
  PERFORM download_wdyn_data USING  p_progname p_temp 'lt_wdy_ui_ctx_bind'  lv_name lt_wdy_ui_ctx_bind  .
  PERFORM download_wdyn_data USING  p_progname p_temp 'lt_wdy_ui_ddic_bind' lv_name lt_wdy_ui_ddic_bind .
  PERFORM download_wdyn_data USING  p_progname p_temp 'lt_wdy_ui_evt_bind'  lv_name lt_wdy_ui_evt_bind  .
  PERFORM download_wdyn_data USING  p_progname p_temp 'lt_wdy_nav_link'     lv_name lt_wdy_nav_link     .
  PERFORM download_wdyn_data USING  p_progname p_temp 'lt_wdy_nav_targref'  lv_name lt_wdy_nav_targref  .
  PERFORM download_wdyn_data USING  p_progname p_temp 'lt_wdy_vsh_node'     lv_name lt_wdy_vsh_node     .
  PERFORM download_wdyn_data USING  p_progname p_temp 'lt_wdy_vsh_pholder'  lv_name lt_wdy_vsh_pholder  .
  PERFORM download_wdyn_data USING  p_progname p_temp 'lt_wdy_vs_property'  lv_name lt_wdy_vs_property  .
ENDFORM.

TYPES: tt_wdy_ctx_node    TYPE TABLE OF wdy_ctx_node,
       tt_wdy_ctx_attrib  TYPE TABLE OF wdy_ctx_attrib,
       tt_wdy_intf_implem TYPE TABLE OF wdy_intf_implem,
       tt_wdy_ctlr_param  TYPE TABLE OF wdy_ctlr_param,
       tt_wdy_ctlr_compo  TYPE TABLE OF wdy_ctlr_compo,
       tt_wdy_compo_usage TYPE TABLE OF wdy_compo_usage,
       tt_wdy_ctlr_usage  TYPE TABLE OF wdy_ctlr_usage.

TYPES: BEGIN OF ty_wd_ctlr_attribute,
         attr_name TYPE wdy_ctlr_compo-cmpname,
         attr_type TYPE wdy_ctlr_compo-abap_type,
       END OF ty_wd_ctlr_attribute,
       tt_wd_ctlr_attributes TYPE TABLE OF ty_wd_ctlr_attribute.

CONSTANTS: " Prefixes
  gc_wd_element_prefix      TYPE string VALUE 'Element',
  gc_wd_elements_prefix     TYPE string VALUE 'Elements',
  gc_cross_cltr_intf_prefix TYPE string VALUE 'Element',
  gc_wd_ctx_prefix          TYPE string VALUE 'wdctx_',
  "gc_class_prefix           TYPE string VALUE 'CL_',
  gc_wd_prefix              TYPE string VALUE 'WD_',

* The component controller provides data and processing logic that it should be possible to display or change for all views of a component. It has three interfaces:
*    - Interface IF_<controller_name> for coding within the controller
*    - Interface IG_<controller_name> for cross-controller coding within the current component.
*    - Interface IWCI_<component_name> for cross-component coding. On the ABAP language level it represents the interface controller.
  gc_wd_ig_prefix           TYPE string VALUE 'ig_',
  gc_wd_if_prefix           TYPE string VALUE 'if_',
  gc_wd_iwci_prefix         TYPE string VALUE 'iwci_',

  " Prefixes used by methods to refer other Web Dynpro components
  gc_wd_cpuse_prefix        TYPE string VALUE 'wd_cpuse_',
  gc_wd_cpifc_prefix        TYPE string VALUE 'wd_cpifc_',

  " Suffixes
  gc_pragma_ec_needed       TYPE string VALUE '      "#EC NEEDED',

  " Web Dynpro component controller name
  gc_wd_comp_ctlr_name      TYPE string VALUE 'COMPONENTCONTROLLER',

  " Referred base interfaces, classes and other types
  gc_if_wd_action           TYPE string VALUE 'if_wd_action',
  gc_if_wd_component        TYPE string VALUE 'if_wd_component',
  gc_if_wd_component_usage  TYPE string VALUE 'if_wd_component_usage',
  gc_if_wd_context_node     TYPE string VALUE 'if_wd_context_node',
  gc_cl_wd_custom_event     TYPE string VALUE 'cl_wd_custom_event',

  " Default method parameter for event handlers
  gc_wdevent_param          TYPE string VALUE 'wdevent',

  " Default controller interface and class parameters
  gc_wd_assist_param        TYPE string VALUE 'wd_assist',
  gc_wd_this_param          TYPE string VALUE 'wd_this',
  gc_wd_context_param       TYPE string VALUE 'wd_context',
  gc_wd_comp_ctlr_param     TYPE string VALUE 'wd_comp_controller',

  " Default controller methods
  gc_wd_get_api             TYPE string VALUE 'wd_get_api',
  gc_wd_create_action       TYPE string VALUE 'wd_create_action'.

DATA: gt_aliases          TYPE tty_string.
DATA: gt_wd_includes      TYPE TABLE OF trdir WITH HEADER LINE.
DATA: gt_wd_referred_ifs  TYPE progname_tab_type.

FORM get_wd_meth_create_action_def CHANGING p_result.
  CONCATENATE 'METHODS' 'wd_create_action'
   'IMPORTING'
     'event_handler'   'TYPE' 'string'
     'is_validating'   'TYPE' 'wdy_boolean' 'default' '``'
     'command'         'TYPE' 'string'
     'text_key'        'TYPE' 'string'
   'RETURNING'
     'VALUE(result)' 'TYPE' 'REF' 'TO' gc_if_wd_action '.' gc_pragma_ec_needed INTO p_result SEPARATED BY space RESPECTING BLANKS.
ENDFORM.

FORM gen_class_name USING p_component_name p_controller_name CHANGING p_class_name p_filename.
  DATA: lv_max_len       TYPE i VALUE 40,
        lv_offset        TYPE i VALUE 5, " 5 for CCMAC, CCIMP, CCDEF etc.
        lv_min_len_delim TYPE i VALUE 5.
  DATA: lv_len TYPE i.
  DATA: lv_class_name TYPE string,
        lv_filename   TYPE string,
        lv_temp       TYPE string.

  IF p_controller_name = gc_wd_comp_ctlr_name.
    lv_temp = 'Compo_Ctlr'. " Shorten paths for 'componentcontroller'
  ELSE.
    lv_temp = p_controller_name.
  ENDIF.

  CONCATENATE p_component_name '_' lv_temp INTO lv_class_name.

  lv_len = strlen( lv_class_name ) + lv_offset + lv_min_len_delim.
  IF lv_len >= lv_max_len.
    DATA: lv_prefix TYPE string.
    IF p_component_name CP 'Z*'.
      lv_prefix = 'Z'.
    ELSEIF p_component_name CP '_*'.
      lv_prefix = '_'.
    ELSEIF p_component_name CP 'Y*'.
      lv_prefix = 'Y'.
    ENDIF.

    PERFORM sha1_string USING lv_class_name CHANGING lv_filename.
    IF strlen( lv_filename ) > 22.
      lv_filename = lv_filename(23).
    ENDIF.

    CONCATENATE gc_wd_prefix lv_filename INTO lv_filename.
    IF lv_prefix IS NOT INITIAL.
      CONCATENATE lv_prefix lv_filename INTO lv_filename.
    ENDIF.

    TRANSLATE lv_filename TO UPPER CASE.
    IF flg_dbg_level > 1 .
      WRITE: /,  'Filename ', lv_filename, ' generated for component ', p_component_name, ' and controller ', p_controller_name.
    ENDIF.
  ELSE.
    lv_filename = lv_class_name.
  ENDIF.

  lv_len = lv_max_len - strlen( lv_filename ) - lv_offset.

  DO lv_len TIMES.
    CONCATENATE lv_filename '=' INTO lv_filename.
  ENDDO.

  p_class_name = lv_class_name.
  p_filename = lv_filename.
ENDFORM.

FORM get_wd_api_def CHANGING p_result TYPE string.
  CONCATENATE 'METHODS' gc_wd_get_api 'RETURNING' 'VALUE(result)' 'TYPE' 'REF' 'TO' gc_if_wd_component INTO p_result SEPARATED BY space.
  CONCATENATE p_result '.' gc_pragma_ec_needed INTO p_result RESPECTING BLANKS.
ENDFORM.

FORM add_alias USING p_alias TYPE string.
  DATA: lv_alias TYPE string.
  lv_alias = p_alias.
  TRANSLATE lv_alias TO LOWER CASE.
  APPEND lv_alias TO gt_aliases.
ENDFORM.

FORM add_include USING p_obj_name TYPE string.
  DATA: lv_tadir TYPE tadir.
  PERFORM fetch_object USING p_obj_name CHANGING lv_tadir.
  IF lv_tadir IS NOT INITIAL.
    APPEND lv_tadir TO gt_wd_includes.
  ENDIF.
ENDFORM.

FORM _add_include USING p_obj_name TYPE string
                        p_obj_type TYPE string.
  DATA: lv_tadir TYPE tadir.
  PERFORM _fetch_object USING p_obj_name p_obj_type CHANGING lv_tadir.
  IF lv_tadir IS NOT INITIAL.
    APPEND lv_tadir TO gt_wd_includes.
  ENDIF.
ENDFORM.

FORM get_type_declaration USING p_variable TYPE string
                                p_abap_type TYPE string
                                p_is_ref TYPE abap_bool
                                p_end TYPE string
                                CHANGING p_str TYPE string.
  DATA: l_abap_type TYPE string,
        l_abap_var  TYPE string.

  l_abap_type = p_abap_type.

  l_abap_var = p_variable.
  TRANSLATE l_abap_var TO LOWER CASE.

  IF p_abap_type IS INITIAL.
    IF p_is_ref = abap_false.
      l_abap_type = 'data'.
    ELSE.
      l_abap_type = 'object'.
    ENDIF.
  ELSE.
    TRANSLATE l_abap_type TO LOWER CASE.
  ENDIF.

  IF p_is_ref = abap_true.
    CONCATENATE p_variable 'TYPE' 'REF' 'TO' l_abap_type INTO p_str SEPARATED BY space.
  ELSE.
    CONCATENATE p_variable 'TYPE' l_abap_type INTO p_str SEPARATED BY space.
  ENDIF.
  CONCATENATE p_str p_end INTO p_str.
ENDFORM.

FORM get_table_type_declaration USING p_variable TYPE string
                                      p_table_type TYPE string
                                     "p_is_ref TYPE abap_bool
                                      p_end TYPE string
                             CHANGING p_str TYPE string.
  DATA: l_table_type TYPE string,
        l_abap_var   TYPE string.

  l_abap_var = p_variable.
  TRANSLATE l_abap_var TO LOWER CASE.

  l_table_type = p_table_type.
  TRANSLATE l_table_type TO LOWER CASE.

  CONCATENATE l_abap_var 'TYPE' 'STANDARD' 'TABLE' 'OF' l_table_type 'WITH' 'DEFAULT' 'KEY' INTO p_str SEPARATED BY space.
  CONCATENATE p_str p_end gc_pragma_ec_needed INTO p_str RESPECTING BLANKS.
ENDFORM.

FORM gen_wdy_constant USING p_arg TYPE string
                   CHANGING p_text TYPE string.
  DATA: lv_temp TYPE string.
  DATA: lv_temp1 TYPE string.

  lv_temp1 = p_arg.
  TRANSLATE lv_temp1 TO LOWER CASE.
  CONCATENATE gc_wd_ctx_prefix lv_temp1 INTO lv_temp1.
  CONCATENATE 'CONSTANTS:' lv_temp1 'TYPE' 'string' 'VALUE' INTO lv_temp SEPARATED BY space.

  lv_temp1 = p_arg.
  TRANSLATE lv_temp1 TO UPPER CASE.
  CONCATENATE '`' lv_temp1 '`' INTO lv_temp1.

  CONCATENATE lv_temp lv_temp1 '.' INTO lv_temp SEPARATED BY space.
  p_text = lv_temp.
ENDFORM.

FORM extract_interface USING p_name TYPE string.
  DATA: lv_temp  TYPE string,
        lv_tadir TYPE tadir.

  lv_temp = p_name.
  TRANSLATE lv_temp TO UPPER CASE.
  PERFORM fetch_object USING lv_temp CHANGING lv_tadir.
  IF lv_tadir IS NOT INITIAL.
    READ TABLE gt_progname_tab WITH TABLE KEY table_line = lv_temp TRANSPORTING NO FIELDS .
    IF   sy-subrc = 0 .
      IF flg_dbg_level > 1 .
        WRITE: /,  'Downloaded  ',  lv_temp,  ' already'.
      ENDIF.
      RETURN.
    ELSE .
      DATA: lt_source TYPE tty_string.
      lv_temp = lv_tadir-obj_name.
      INSERT lv_tadir-obj_name INTO TABLE gt_progname_tab.
      PERFORM serialize_abap_oo_source USING lv_temp CHANGING lt_source.
      PERFORM create_file_default USING lt_source lv_temp.
    ENDIF.
  ENDIF.
ENDFORM.

" This form downloads the source code of interface IWCI_<component_name>
FORM download_wdy_cc_iwci USING p_component_name  TYPE wdy_component-component_name
                                p_controller_name TYPE wdy_controller-controller_name.
  DATA: lv_wdy_controller TYPE wdy_controller.
  DATA: lv_wdy_ctlr_runtime_obj TYPE string.
  DATA: lv_tadir TYPE tadir.

  SELECT SINGLE *
  FROM   wdy_controller
  INTO   lv_wdy_controller
  WHERE  component_name = p_component_name
  AND    controller_name = p_controller_name.

  IF sy-subrc = 0.
    IF lv_wdy_controller IS INITIAL.
      IF flg_dbg_level > 4.
        WRITE: /, 'Could not find record for component ', p_component_name, ' and controller ', p_controller_name.
      ENDIF.
      RETURN.
    ENDIF.

    IF lv_wdy_controller-runtime_object IS NOT INITIAL.
      lv_wdy_ctlr_runtime_obj = lv_wdy_controller-runtime_object.
      PERFORM extract_interface USING lv_wdy_ctlr_runtime_obj.
    ELSE.
      IF flg_dbg_level > 4.
        WRITE: /, 'No runtime object maintained for component ', p_component_name, ' and controller ', p_controller_name.
      ENDIF.
    ENDIF.
  ELSE.
    IF flg_dbg_level > 4.
      WRITE: /, 'Failed to get record for component ', p_component_name, ' and controller ', p_controller_name.
    ENDIF.
  ENDIF.
ENDFORM.

FORM get_wd_comp_cls USING p_component TYPE wdy_component-component_name.
  DATA: lv_class_name TYPE string.
  CALL METHOD cl_wdy_wb_naming_service=>get_classname_for_component
    EXPORTING
      p_component        = p_component
    RECEIVING
      p_classname        = lv_class_name
    EXCEPTIONS
      no_generation_info = 1
      OTHERS             = 2.

  IF sy-subrc <> 0.
    IF flg_dbg_level > 4.
      WRITE: /, 'Failed to get class name for WD component ', p_component.
    ENDIF.
  ENDIF.
ENDFORM.

FORM get_wdy_assistance_class USING p_component_name  TYPE wdy_component-component_name
                           CHANGING p_comp_assist_cls TYPE wdy_component-assistance_class.
  SELECT SINGLE assistance_class
  FROM wdy_component
  INTO p_comp_assist_cls
  WHERE component_name = p_component_name.
ENDFORM.

FORM get_wdy_ctx_nodes USING    p_component_name  TYPE wdy_component-component_name
                                p_controller_name TYPE wdy_controller-controller_name
                       CHANGING pt_ctx_nodes TYPE tt_wdy_ctx_node.
  SELECT * FROM   wdy_ctx_node
  INTO TABLE pt_ctx_nodes
  WHERE component_name = p_component_name
  AND   controller_name = p_controller_name
  AND   node_name <> 'CONTEXT'.
ENDFORM.

FORM get_wdy_ctx_attrib USING p_component_name  TYPE wdy_component-component_name
                              p_controller_name TYPE wdy_controller-controller_name
                              p_node_name       TYPE wdy_ctx_node-node_name
                     CHANGING pt_ctx_attrib     TYPE tt_wdy_ctx_attrib.
  SELECT * FROM   wdy_ctx_attrib
  INTO TABLE pt_ctx_attrib
  WHERE component_name = p_component_name
  AND   controller_name = p_controller_name
  AND   node_name = p_node_name.
ENDFORM.

FORM get_wdy_implem_intfs USING p_component_name TYPE wdy_component-component_name
                       CHANGING p_implem_intfs   TYPE tt_wdy_intf_implem.
  SELECT *
  FROM   wdy_intf_implem
  INTO TABLE p_implem_intfs
  WHERE component_name = p_component_name.
ENDFORM.

FORM get_wdy_intf_methods USING p_component_name  TYPE wdy_component-component_name
                                p_controller_name TYPE wdy_controller-controller_name
                       CHANGING p_intf_methods    TYPE tt_wdy_ctlr_compo.
  SELECT *
  FROM   wdy_ctlr_compo
  INTO TABLE p_intf_methods
  WHERE component_name = p_component_name
  AND   controller_name = p_controller_name
  AND   cmptype = 'CL_WDY_MD_CONTROLLER_METHOD'
  AND   is_intf_item = 'X'.
ENDFORM.

FORM get_wdy_ctlr_properties USING p_component_name    TYPE wdy_component-component_name
                                   p_controller_name   TYPE wdy_controller-controller_name
                          CHANGING p_ctlr_properties   TYPE tt_wdy_ctlr_compo.
  SELECT *
  FROM   wdy_ctlr_compo
  INTO TABLE p_ctlr_properties
  WHERE component_name = p_component_name
  AND   controller_name = p_controller_name
  AND   cmptype = 'CL_WDY_MD_CONTROLLER_PROPERTY'.
ENDFORM.

FORM get_wdy_other_ctlr_methods USING p_component_name TYPE wdy_component-component_name
                                p_controller_name TYPE wdy_controller-controller_name
                       CHANGING p_other_ctlr_methods TYPE tt_wdy_ctlr_compo.
  SELECT *
  FROM   wdy_ctlr_compo
  INTO TABLE p_other_ctlr_methods
  WHERE component_name = p_component_name
  AND   controller_name = p_controller_name
  AND   cmptype = 'CL_WDY_MD_CONTROLLER_METHOD'
  AND   ( is_intf_item <> 'X' AND cmpname NOT LIKE 'WDDO%' ).
ENDFORM.

FORM get_wdy_intf_ctlr_events USING p_component_name  TYPE wdy_component-component_name
                                    p_controller_name TYPE wdy_controller-controller_name
                           CHANGING p_ctlr_intf_evt_meths TYPE tt_wdy_ctlr_compo.
  SELECT *
  FROM   wdy_ctlr_compo
  INTO TABLE p_ctlr_intf_evt_meths
  WHERE component_name = p_component_name
  AND   controller_name = p_controller_name
  AND   cmptype = 'CL_WDY_MD_CUSTOM_EVENT'
  AND   is_intf_item = 'X'.
ENDFORM.

FORM get_wdy_other_ctlr_events USING p_component_name  TYPE wdy_component-component_name
                                     p_controller_name TYPE wdy_controller-controller_name
                            CHANGING p_ctlr_evt_meths TYPE tt_wdy_ctlr_compo.
  SELECT *
  FROM   wdy_ctlr_compo
  INTO TABLE p_ctlr_evt_meths
  WHERE component_name = p_component_name
  AND   controller_name = p_controller_name
  AND   cmptype = 'CL_WDY_MD_CUSTOM_EVENT'
  AND   is_intf_item <> 'X'.
ENDFORM.

FORM get_wdy_evt_handlers USING p_component_name  TYPE wdy_component-component_name
                                p_controller_name TYPE wdy_controller-controller_name
                       CHANGING p_ctlr_evt_handlers TYPE tt_wdy_ctlr_compo.
  SELECT *
  FROM   wdy_ctlr_compo
  INTO TABLE p_ctlr_evt_handlers
  WHERE component_name = p_component_name
  AND   controller_name = p_controller_name
  AND   cmptype = 'CL_WDY_MD_CTLR_EVENT_HANDLER'.
ENDFORM.

FORM get_wdy_ctlr_actions USING p_component_name  TYPE wdy_component-component_name
                                p_controller_name TYPE wdy_controller-controller_name
                       CHANGING p_ctlr_actions TYPE tt_wdy_ctlr_compo.

  SELECT *
  FROM   wdy_ctlr_compo
  INTO TABLE p_ctlr_actions
  WHERE component_name = p_component_name
  AND   controller_name = p_controller_name
  AND   cmptype = 'CL_WDY_MD_ACTION'.
ENDFORM.

FORM get_wdy_ctlr_meth_parameters USING p_component_name TYPE wdy_component-component_name
                                        p_controller_name TYPE wdy_controller-controller_name
                                        p_cmpname TYPE wdy_ctlr_param-cmpname
                               CHANGING p_ctlr_param TYPE tt_wdy_ctlr_param.
  SELECT     *
         FROM       wdy_ctlr_param
         INTO TABLE p_ctlr_param
         WHERE      component_name  = p_component_name
         AND        controller_name = p_controller_name
         AND        cmpname         = p_cmpname.
  SORT  p_ctlr_param BY param_position.
ENDFORM.

FORM get_wdy_compo_usage USING p_component_name  TYPE wdy_component-component_name
                      CHANGING p_wdy_compo_usage TYPE tt_wdy_compo_usage.
  SELECT *
  FROM   wdy_compo_usage
  INTO   TABLE p_wdy_compo_usage
  WHERE  component_name = p_component_name
  AND    usage_type = 'CL_WDY_MD_COMPONENT_USAGE'.
ENDFORM.

FORM get_wdy_ctlr_usage USING p_component_name  TYPE wdy_component-component_name
                              p_controller_name TYPE wdy_controller-controller_name
                      CHANGING p_wdy_ctlr_usage TYPE tt_wdy_ctlr_usage.
  SELECT *
  FROM   wdy_ctlr_usage
  INTO   TABLE p_wdy_ctlr_usage
  WHERE  component_name  = p_component_name
  AND    controller_name = p_controller_name
  AND    used_controller <> ''.
ENDFORM.

FORM get_wdy_ctlr_usage_component USING p_component_name   TYPE wdy_component-component_name
                                        p_compo_usage_name TYPE wdy_ctlr_usage-component_usage
                               CHANGING p_referred_comp    TYPE wdy_compo_usage-used_component.
  SELECT SINGLE used_component
  FROM   wdy_compo_usage
  INTO   p_referred_comp
  WHERE  component_name   = p_component_name
  AND    compo_usage_name = p_compo_usage_name.
ENDFORM.

FORM add_comp_details_comment USING p_component_name   TYPE wdy_component-component_name
                                    p_ctlr_name        TYPE wdy_ctlr_usage-component_usage
                           CHANGING pt_code_body       TYPE tty_string.
  DATA: lv_temp TYPE string.

  CONCATENATE '" Component Name: ' p_component_name INTO lv_temp RESPECTING BLANKS.
  APPEND lv_temp TO pt_code_body.

  CONCATENATE '" Controller Name: ' p_ctlr_name INTO lv_temp RESPECTING BLANKS.
  APPEND lv_temp TO pt_code_body.

  APPEND '' TO pt_code_body.
ENDFORM.

FORM create_wd_ctlr_class_def USING p_component_name TYPE wdy_component-component_name
                                    p_class_name TYPE string
                                    p_file_name  TYPE string
                                    p_ctor_def_str TYPE string
                                    p_ctlr_attr_data_def_lines TYPE tty_string
                                    p_meth_definitions TYPE tty_string.
  DATA: lv_temp              TYPE string,
        lv_wdy_filename      TYPE string,
        lv_download_pathname TYPE string,
        lt_code_body         TYPE tty_string.
  lv_temp = p_class_name.
  TRANSLATE lv_temp TO LOWER CASE.
  CONCATENATE 'CLASS' lv_temp 'DEFINITION PUBLIC CREATE PUBLIC.' INTO lv_temp SEPARATED BY space.
  APPEND lv_temp TO lt_code_body.
  APPEND 'PUBLIC SECTION.' TO lt_code_body.

  APPEND p_ctor_def_str TO lt_code_body.
  APPEND LINES OF p_meth_definitions TO lt_code_body.
  APPEND LINES OF p_ctlr_attr_data_def_lines TO lt_code_body.

  APPEND 'ENDCLASS.' TO lt_code_body.

  CONCATENATE p_file_name 'CU' INTO lv_wdy_filename.
  PERFORM create_file USING lt_code_body lv_wdy_filename p_component_name.
ENDFORM.

FORM create_wd_ctlr_class_pool USING p_component_name TYPE wdy_component-component_name
                                     p_class_name     TYPE string
                                     p_file_name      TYPE string.
  DATA: lv_temp              TYPE string,
        lv_wdy_filename      TYPE string,
        lv_download_pathname TYPE string,
        lv_include           TYPE string,
        lt_code_body         TYPE tty_string.

  CONCATENATE 'CLASS-POOL' '.' INTO lv_temp SEPARATED BY space.
  APPEND lv_temp TO lt_code_body.

  CONCATENATE '*"* class pool for class ' p_class_name INTO lv_temp RESPECTING BLANKS.
  APPEND lv_temp TO lt_code_body.

  APPEND '' TO lt_code_body.

  CONCATENATE '*"*' 'CLASS' p_class_name 'DEFINITION' INTO lv_temp SEPARATED BY space.
  APPEND lv_temp TO lt_code_body.

  APPEND '*"* public declarations' TO lt_code_body.

  CONCATENATE p_file_name 'CCDEF' INTO lv_temp.
  CONCATENATE ' INCLUDE' lv_temp INTO lv_temp SEPARATED BY space RESPECTING BLANKS.
  CONCATENATE lv_temp '.' INTO lv_temp.
  APPEND lv_temp TO lt_code_body.

  CONCATENATE p_file_name 'CU' INTO lv_temp.
  CONCATENATE ' INCLUDE' lv_temp INTO lv_temp SEPARATED BY space RESPECTING BLANKS.
  CONCATENATE lv_temp '.' INTO lv_temp.
  APPEND lv_temp TO lt_code_body.

  CONCATENATE 'ENDCLASS. "' p_class_name INTO lv_temp.
  CONCATENATE lv_temp 'DEFINITION' INTO lv_temp SEPARATED BY space.
  APPEND lv_temp TO lt_code_body.

  APPEND '' TO lt_code_body.

  APPEND '' TO lt_code_body.

  CONCATENATE 'CLASS' p_class_name 'IMPLEMENTATION.' INTO lv_temp SEPARATED BY space.
  APPEND lv_temp TO lt_code_body.

  APPEND '*"* method implementations' TO lt_code_body.

  APPEND ' INCLUDE METHODS.' TO lt_code_body.

  CONCATENATE 'ENDCLASS. "' p_class_name INTO lv_temp RESPECTING BLANKS.
  CONCATENATE lv_temp 'IMPLEMENTATION' INTO lv_temp SEPARATED BY space.
  APPEND lv_temp TO lt_code_body.

  CONCATENATE p_file_name 'CP' INTO lv_wdy_filename.
  PERFORM create_file USING lt_code_body lv_wdy_filename p_component_name.
ENDFORM.

FORM build_wdy_comp_ctlr_ig USING p_component_name  TYPE wdy_component-component_name
                                  p_controller_name TYPE wdy_controller-controller_name
                         CHANGING p_code_body TYPE tty_string.
  DATA: lv_temp TYPE string.
  DATA: lt_code_body TYPE TABLE OF string.

  CONCATENATE gc_wd_ig_prefix p_controller_name INTO lv_temp.
  TRANSLATE lv_temp TO LOWER CASE.
  CONCATENATE 'INTERFACE' lv_temp '.' INTO lv_temp SEPARATED BY space.
  APPEND lv_temp TO lt_code_body.

  CONCATENATE gc_wd_iwci_prefix p_component_name INTO lv_temp.
  TRANSLATE lv_temp TO LOWER CASE.
  CONCATENATE 'INTERFACES:' lv_temp '.' INTO lv_temp SEPARATED BY space.
  APPEND lv_temp TO lt_code_body.

  DATA: lt_wdy_intf_implem TYPE TABLE OF wdy_intf_implem,
        lv_wdy_intf_implem TYPE wdy_intf_implem.
  PERFORM get_wdy_implem_intfs USING p_component_name CHANGING lt_wdy_intf_implem.
  LOOP AT lt_wdy_intf_implem INTO lv_wdy_intf_implem.
    DATA: lv_tadir TYPE tadir.
    CONCATENATE gc_wd_iwci_prefix lv_wdy_intf_implem-interface_name INTO lv_temp.

    TRANSLATE lv_temp TO UPPER CASE.
    PERFORM fetch_object USING lv_temp CHANGING lv_tadir.
    IF lv_tadir IS NOT INITIAL.
      INSERT lv_tadir-obj_name INTO TABLE gt_wd_referred_ifs.
    ENDIF.

    TRANSLATE lv_temp TO LOWER CASE.
    CONCATENATE 'INTERFACES:' lv_temp '.' INTO lv_temp SEPARATED BY space.
    APPEND lv_temp TO lt_code_body.
  ENDLOOP.

  PERFORM gen_wdy_constant USING 'context' CHANGING lv_temp.
  APPEND lv_temp TO lt_code_body.
  PERFORM add_alias USING 'wdctx_context'.

  PERFORM gen_wdy_ig_comp_ctlr_types   USING p_component_name p_controller_name CHANGING lt_code_body.
  PERFORM gen_wdy_ig_comp_ctlr_methods USING p_component_name p_controller_name CHANGING lt_code_body.

  APPEND 'ENDINTERFACE.' TO lt_code_body.

  APPEND LINES OF lt_code_body TO p_code_body.
ENDFORM.

FORM build_wdy_other_ctlr_intf USING p_component_name TYPE wdy_component-component_name
                                     p_controller_name TYPE wdy_controller-controller_name
                            CHANGING p_code_body TYPE tty_string.
  DATA: lv_temp TYPE string.
  DATA: lt_code_body TYPE TABLE OF string.

  PERFORM gen_wdy_constant USING 'context' CHANGING lv_temp.
  APPEND lv_temp TO lt_code_body.
  PERFORM add_alias USING 'wdctx_context'.

  PERFORM gen_wdy_ig_comp_ctlr_types USING p_component_name p_controller_name CHANGING lt_code_body.
  PERFORM gen_wdy_ig_comp_ctlr_methods USING p_component_name p_controller_name CHANGING lt_code_body.

  APPEND LINES OF lt_code_body TO p_code_body.
ENDFORM.

FORM build_ref_components_cc USING p_component_name  TYPE wdy_component-component_name
                                   p_controller_name TYPE wdy_controller-controller_name
                          CHANGING p_code_body       TYPE tty_string.
  DATA: lt_wdy_compo_usage TYPE tt_wdy_compo_usage,
        lv_wdy_compo_usage TYPE wdy_compo_usage,
        lv_temp            TYPE string,
        lv_meth_def        TYPE string.

  PERFORM get_wdy_compo_usage USING p_component_name CHANGING lt_wdy_compo_usage.
  IF lt_wdy_compo_usage IS INITIAL OR lines( lt_wdy_compo_usage ) EQ 0.
    IF flg_dbg_level > 4 .
      WRITE: /, 'Returning as no component usage found for component ', p_component_name.
    ENDIF.
    RETURN.
  ENDIF.

  LOOP AT lt_wdy_compo_usage INTO lv_wdy_compo_usage.
    CONCATENATE gc_wd_cpuse_prefix lv_wdy_compo_usage-compo_usage_name INTO lv_temp.
    TRANSLATE lv_temp TO LOWER CASE.
    CONCATENATE 'METHODS' lv_temp 'RETURNING' 'VALUE(result)' 'TYPE' 'REF' 'TO' gc_if_wd_component_usage INTO lv_meth_def SEPARATED BY space.
    CONCATENATE lv_meth_def '.' gc_pragma_ec_needed INTO lv_meth_def RESPECTING BLANKS.
    APPEND lv_meth_def TO p_code_body.

    CONCATENATE gc_wd_cpifc_prefix lv_wdy_compo_usage-compo_usage_name INTO lv_temp.
    TRANSLATE lv_temp TO LOWER CASE.
    CONCATENATE gc_wd_iwci_prefix lv_wdy_compo_usage-used_component INTO lv_meth_def.
    CONCATENATE 'METHODS' lv_temp 'RETURNING' 'VALUE(result)' 'TYPE' 'REF' 'TO' lv_meth_def INTO lv_meth_def SEPARATED BY space.
    CONCATENATE lv_meth_def '.' gc_pragma_ec_needed INTO lv_meth_def RESPECTING BLANKS.
    APPEND lv_meth_def TO p_code_body.

    DATA: lv_tadir TYPE tadir.
    lv_temp = lv_wdy_compo_usage-used_component.
    PERFORM _add_include USING lv_temp 'WDYN'.
  ENDLOOP.
ENDFORM.

FORM build_ref_components_oth_ctlr USING p_component_name  TYPE wdy_component-component_name
                                p_controller_name TYPE wdy_controller-controller_name
                       CHANGING p_code_body       TYPE tty_string..
  DATA: lt_wdy_ctlr_usage TYPE tt_wdy_ctlr_usage,
        lv_wdy_ctlr_usage TYPE wdy_ctlr_usage,lv_temp            TYPE string,
        lv_meth_def       TYPE string.

  PERFORM get_wdy_ctlr_usage USING p_component_name p_controller_name CHANGING lt_wdy_ctlr_usage.
  IF lt_wdy_ctlr_usage IS INITIAL OR lines( lt_wdy_ctlr_usage ) EQ 0.
    IF flg_dbg_level > 4 .
      WRITE: /, 'Returning as no controller usage found for component ', p_component_name, ' and controller ', p_controller_name.
    ENDIF.
    RETURN.
  ENDIF.

  LOOP AT lt_wdy_ctlr_usage INTO lv_wdy_ctlr_usage.
    CONCATENATE gc_wd_cpuse_prefix lv_wdy_ctlr_usage-component_usage INTO lv_temp.
    TRANSLATE lv_temp TO LOWER CASE.
    CONCATENATE 'METHODS' lv_temp 'RETURNING' 'VALUE(result)' 'TYPE' 'REF' 'TO' gc_if_wd_component_usage INTO lv_meth_def SEPARATED BY space.
    CONCATENATE lv_meth_def '.' gc_pragma_ec_needed INTO lv_meth_def RESPECTING BLANKS.
    APPEND lv_meth_def TO p_code_body.

    DATA: lv_wd_component TYPE wdy_compo_usage-used_component.
    PERFORM get_wdy_ctlr_usage_component USING p_component_name lv_wdy_ctlr_usage-component_usage CHANGING lv_wd_component.
    IF lv_wd_component IS NOT INITIAL.
      CONCATENATE gc_wd_cpifc_prefix lv_wdy_ctlr_usage-component_usage INTO lv_temp.
      TRANSLATE lv_temp TO LOWER CASE.
      CONCATENATE gc_wd_iwci_prefix lv_wd_component INTO lv_meth_def.
      CONCATENATE 'METHODS' lv_temp 'RETURNING' 'VALUE(result)' 'TYPE' 'REF' 'TO' lv_meth_def INTO lv_meth_def SEPARATED BY space.
      CONCATENATE lv_meth_def '.' gc_pragma_ec_needed INTO lv_meth_def RESPECTING BLANKS.
      APPEND lv_meth_def TO p_code_body.
    ENDIF.
  ENDLOOP.
ENDFORM.

FORM build_ref_components USING p_component_name  TYPE wdy_component-component_name
                                p_controller_name TYPE wdy_controller-controller_name
                       CHANGING p_code_body       TYPE tty_string.

  APPEND '' TO p_code_body.
  APPEND '* +-------------------------------------------------------------------+' TO p_code_body.
  APPEND '* |  component & controller usages                                    |' TO p_code_body.
  APPEND '* +-------------------------------------------------------------------+' TO p_code_body.
  APPEND '' TO p_code_body.

  IF p_controller_name = gc_wd_comp_ctlr_name.
    PERFORM build_ref_components_cc USING p_component_name p_controller_name CHANGING p_code_body.
  ELSE.
    PERFORM build_ref_components_oth_ctlr USING p_component_name p_controller_name CHANGING p_code_body.
  ENDIF.
ENDFORM.

FORM build_wdy_ctlr_intf USING p_component_name  TYPE wdy_component-component_name
                               p_controller_name TYPE wdy_controller-controller_name
                      CHANGING p_code_body TYPE tty_string.
  DATA: lt_wdy_ctlr_compo  TYPE tt_wdy_ctlr_compo,
        lv_wdy_ctlr_compo  TYPE wdy_ctlr_compo,
        lt_wdy_compo_usage TYPE tt_wdy_compo_usage,
        lv_wdy_compo_usage TYPE wdy_compo_usage,
        lv_temp            TYPE string,
        lv_meth_def        TYPE string,
        lv_alias           TYPE string.

  CONCATENATE gc_wd_if_prefix p_controller_name INTO lv_temp.
  TRANSLATE lv_temp TO LOWER CASE.
  CONCATENATE 'INTERFACE' lv_temp '.' INTO lv_temp SEPARATED BY space.
  APPEND lv_temp TO p_code_body.

  IF p_controller_name = gc_wd_comp_ctlr_name.
    DATA: lv_ig_ctlr_name TYPE string.
    CONCATENATE gc_wd_ig_prefix p_controller_name INTO lv_ig_ctlr_name.
    CONCATENATE 'INTERFACES:' lv_ig_ctlr_name '.' INTO lv_temp SEPARATED BY space.
    APPEND lv_temp TO p_code_body.

    SORT gt_aliases.
    DELETE ADJACENT DUPLICATES FROM gt_aliases.

    LOOP AT gt_aliases INTO lv_alias.
      CONCATENATE lv_ig_ctlr_name '~' lv_alias INTO lv_temp.
      CONCATENATE 'ALIASES:' lv_alias 'FOR' lv_temp '.' INTO lv_temp SEPARATED BY space.
      APPEND lv_temp TO p_code_body.
    ENDLOOP.

    CLEAR gt_aliases.

    PERFORM get_wdy_intf_methods USING p_component_name p_controller_name CHANGING lt_wdy_ctlr_compo.
    PERFORM build_wdy_ctlr_if_meth_def USING p_component_name lt_wdy_ctlr_compo CHANGING p_code_body.
  ELSE.
    PERFORM build_wdy_other_ctlr_intf USING p_component_name p_controller_name CHANGING p_code_body.
  ENDIF.

  CLEAR lt_wdy_ctlr_compo.
  PERFORM get_wdy_ctlr_actions USING p_component_name p_controller_name CHANGING lt_wdy_ctlr_compo.
  IF lt_wdy_ctlr_compo IS NOT INITIAL AND lines( lt_wdy_ctlr_compo ) GT 0.
    APPEND '' TO p_code_body.
    APPEND '* +-------------------------------------------------------------------+' TO p_code_body.
    APPEND '* |  actions                                                          |' TO p_code_body.
    APPEND '* +-------------------------------------------------------------------+' TO p_code_body.
    APPEND '' TO p_code_body.
    PERFORM build_wdy_ctlr_act_meth_defs USING lt_wdy_ctlr_compo 'create_' '_act' CHANGING p_code_body.
  ELSE.
    IF flg_dbg_level > 4 .
      WRITE: /,  'No controller actions found for component ', p_component_name, ' and ', p_controller_name.
    ENDIF.
  ENDIF.

  PERFORM get_wd_meth_create_action_def CHANGING lv_meth_def.
  APPEND lv_meth_def TO p_code_body.

  CLEAR lt_wdy_ctlr_compo.
  PERFORM get_wdy_evt_handlers  USING p_component_name p_controller_name CHANGING lt_wdy_ctlr_compo.
  IF lt_wdy_ctlr_compo IS NOT INITIAL AND lines( lt_wdy_ctlr_compo ) GT 0.
    APPEND '' TO p_code_body.
    APPEND '* +-------------------------------------------------------------------+' TO p_code_body.
    APPEND '* |  event handlers                                                   |' TO p_code_body.
    APPEND '* +-------------------------------------------------------------------+' TO p_code_body.
    APPEND '' TO p_code_body.
    PERFORM build_wdy_evt_handle_meth_defs USING lt_wdy_ctlr_compo '' '' CHANGING p_code_body.
  ELSE.
    IF flg_dbg_level > 4 .
      WRITE: /,  'No event handler method found for component ', p_component_name, ' and ', p_controller_name.
    ENDIF.
  ENDIF.

  CLEAR lt_wdy_ctlr_compo.
  PERFORM get_wdy_other_ctlr_events USING p_component_name p_controller_name CHANGING lt_wdy_ctlr_compo.
  IF lt_wdy_ctlr_compo IS NOT INITIAL AND lines( lt_wdy_ctlr_compo ) GT 0.
    APPEND '' TO p_code_body.
    APPEND '* +-------------------------------------------------------------------+' TO p_code_body.
    APPEND '* |  fire plugs                                                       |' TO p_code_body.
    APPEND '* +-------------------------------------------------------------------+' TO p_code_body.
    APPEND '' TO p_code_body.
    PERFORM build_wdy_ctlr_meth_defs  USING lt_wdy_ctlr_compo 'fire_' '_evt' abap_false CHANGING p_code_body.
  ELSE.
    IF flg_dbg_level > 4 .
      WRITE: /,  'No other custom events found for component ', p_component_name, ' and ', p_controller_name.
    ENDIF.
  ENDIF.

  PERFORM build_ref_components USING p_component_name p_controller_name CHANGING p_code_body.

  CLEAR lt_wdy_ctlr_compo.
  PERFORM get_wdy_ctlr_properties USING p_component_name p_controller_name CHANGING lt_wdy_ctlr_compo.
  IF lt_wdy_ctlr_compo IS NOT INITIAL AND lines( lt_wdy_ctlr_compo ) GT 0.
    APPEND '' TO p_code_body.
    APPEND '* +-------------------------------------------------------------------+' TO p_code_body.
    APPEND '* |  application properties                                           |' TO p_code_body.
    APPEND '* +-------------------------------------------------------------------+' TO p_code_body.
    APPEND '' TO p_code_body.
    LOOP AT lt_wdy_ctlr_compo INTO lv_wdy_ctlr_compo.
      IF lv_wdy_ctlr_compo-abap_typing = '1'.
        CONCATENATE 'DATA:' lv_wdy_ctlr_compo-cmpname 'TYPE' 'REF' 'TO' lv_wdy_ctlr_compo-abap_type '.' INTO lv_temp SEPARATED BY space.
      ELSE.
        CONCATENATE 'DATA:' lv_wdy_ctlr_compo-cmpname 'TYPE' lv_wdy_ctlr_compo-abap_type '.' INTO lv_temp SEPARATED BY space.
      ENDIF.
      APPEND lv_temp TO p_code_body.
    ENDLOOP.
  ELSE.
    IF flg_dbg_level > 4 .
      WRITE: /,  'No application properties found for component ', p_component_name, ' and ', p_controller_name.
    ENDIF.
  ENDIF.

  DATA: lv_assistance_class TYPE wdy_component-assistance_class.

  PERFORM get_wdy_assistance_class USING p_component_name CHANGING lv_assistance_class.
  IF lv_assistance_class IS NOT INITIAL.
    CONCATENATE 'DATA:' gc_wd_assist_param 'TYPE' 'REF' 'TO' lv_assistance_class '.' INTO lv_temp SEPARATED BY space.
    APPEND lv_temp TO p_code_body.

    lv_temp = lv_assistance_class.
    PERFORM add_include USING lv_temp.
  ELSE.
    IF flg_dbg_level > 4 .
      WRITE: /, 'No assistance class found for ', p_component_name.
    ENDIF.
  ENDIF.

  APPEND 'ENDINTERFACE.' TO p_code_body.
ENDFORM.

FORM build_wd_ctlr_ctor USING p_ctlr_name       TYPE wdy_ctlr_compo-component_name
                              p_comp_assist_cls TYPE wdy_component-assistance_class
                     CHANGING p_ctor_def_str             TYPE string
                              p_ctlr_attr_data_def_lines TYPE tty_string
                              p_ctor_impl_lines          TYPE tty_string.
  DATA: lt_default_attrs TYPE tt_wd_ctlr_attributes.
  DATA: lv_ctlr_attr TYPE ty_wd_ctlr_attribute.
  DATA: lv_temp  TYPE string,
        lv_temp1 TYPE string.

  lv_ctlr_attr-attr_name = gc_wd_this_param.
  CONCATENATE 'if_' p_ctlr_name INTO lv_ctlr_attr-attr_type.
  APPEND lv_ctlr_attr TO lt_default_attrs.

  lv_ctlr_attr-attr_name = gc_wd_context_param.
  lv_ctlr_attr-attr_type = gc_if_wd_context_node.
  APPEND lv_ctlr_attr TO lt_default_attrs.

  IF p_comp_assist_cls IS NOT INITIAL.
    lv_ctlr_attr-attr_name = gc_wd_assist_param.
    lv_ctlr_attr-attr_type = p_comp_assist_cls.
    APPEND lv_ctlr_attr TO lt_default_attrs.
  ENDIF.

  CONCATENATE 'METHOD' 'constructor' INTO lv_temp SEPARATED BY space.
  CONCATENATE lv_temp '.' gc_pragma_ec_needed INTO lv_temp RESPECTING BLANKS.
  APPEND lv_temp TO p_ctor_impl_lines.
  CONCATENATE 'METHODS' 'constructor' 'IMPORTING' INTO p_ctor_def_str SEPARATED BY space.

  DATA: lv_attr_name TYPE string.
  DATA: lv_attr_type TYPE string.

  LOOP AT lt_default_attrs INTO lv_ctlr_attr.
    lv_attr_name = lv_ctlr_attr-attr_name.
    TRANSLATE lv_attr_name TO LOWER CASE.

    lv_attr_type = lv_ctlr_attr-attr_type.
    TRANSLATE lv_attr_type TO LOWER CASE.

    CONCATENATE lv_attr_name 'TYPE' 'REF' 'TO' lv_attr_type INTO lv_temp1 SEPARATED BY space.

    CONCATENATE 'DATA:' lv_temp1 '.' INTO lv_temp SEPARATED BY space.
    APPEND lv_temp TO p_ctlr_attr_data_def_lines.

    CONCATENATE p_ctor_def_str lv_temp1 INTO p_ctor_def_str SEPARATED BY space.
    CONCATENATE 'me->' lv_attr_name INTO lv_temp.
    CONCATENATE lv_temp '=' lv_attr_name '.' INTO lv_temp SEPARATED BY space.
    APPEND lv_temp TO p_ctor_impl_lines.
  ENDLOOP.

  IF p_ctlr_name <> gc_wd_comp_ctlr_name.
    lv_attr_name = gc_wd_comp_ctlr_param.
    DATA : lv_get_ctlr_meth TYPE string.

    CONCATENATE gc_wd_ig_prefix gc_wd_comp_ctlr_name INTO lv_attr_type.
    CONCATENATE 'DATA:' lv_attr_name 'TYPE' 'REF' 'TO' lv_attr_type '.' INTO lv_temp SEPARATED BY space.
    APPEND lv_temp TO p_ctlr_attr_data_def_lines.
    CONCATENATE 'me' '->' gc_wd_comp_ctlr_param INTO lv_temp.
    CONCATENATE 'get_' gc_wd_comp_ctlr_name '_ctr(' INTO lv_get_ctlr_meth.
    CONCATENATE lv_get_ctlr_meth ')' INTO lv_get_ctlr_meth SEPARATED BY space.
    CONCATENATE 'wd_this->' lv_get_ctlr_meth INTO lv_get_ctlr_meth RESPECTING BLANKS.
    CONCATENATE lv_temp '=' lv_get_ctlr_meth '.' INTO lv_temp SEPARATED BY space RESPECTING BLANKS.
    APPEND lv_temp TO p_ctor_impl_lines.
  ENDIF.

  APPEND 'ENDMETHOD.' TO p_ctor_impl_lines.
  CONCATENATE p_ctor_def_str '.' INTO p_ctor_def_str.
ENDFORM.

FORM build_wdy_ctlr_if_meth_def USING p_component_name TYPE wdy_component-component_name
                                      p_methods TYPE tt_wdy_ctlr_compo
                             CHANGING p_code_body TYPE tty_string.
  DATA: lv_method TYPE wdy_ctlr_compo.
  DATA: lv_meth_def TYPE string.
  LOOP AT p_methods INTO lv_method.
    DATA: lv_meth_name TYPE string.
    lv_meth_name = lv_method-cmpname.
    CONCATENATE gc_wd_iwci_prefix p_component_name '~' lv_meth_name INTO lv_meth_def.
    TRANSLATE lv_meth_def TO LOWER CASE.
    CONCATENATE 'ALIASES:' lv_meth_name 'FOR' lv_meth_def '.' INTO lv_meth_def SEPARATED BY space.
    APPEND lv_meth_def TO p_code_body.
  ENDLOOP.
ENDFORM.

FORM build_wdy_ctlr_meth_def USING p_meth_name TYPE string p_cur_ctlr_param TYPE tt_wdy_ctlr_param CHANGING p_meth_def TYPE string.
  DATA: lv_cur_ctlr_param TYPE wdy_ctlr_param,
        lv_temp           TYPE string.

  CONCATENATE 'METHODS' p_meth_name INTO p_meth_def SEPARATED BY space.
  IF p_cur_ctlr_param IS NOT INITIAL AND lines( p_cur_ctlr_param ) GT 0.
    CONCATENATE p_meth_def 'IMPORTING' INTO p_meth_def SEPARATED BY space.
    LOOP AT p_cur_ctlr_param INTO lv_cur_ctlr_param.
      IF lv_cur_ctlr_param-abap_typing = '1'.
        CONCATENATE p_meth_def  lv_cur_ctlr_param-parameter_name 'TYPE' 'REF' 'TO' lv_cur_ctlr_param-abap_type INTO p_meth_def SEPARATED BY space.
      ELSE.
        CONCATENATE p_meth_def  lv_cur_ctlr_param-parameter_name 'TYPE' lv_cur_ctlr_param-abap_type INTO p_meth_def SEPARATED BY space.
      ENDIF.
    ENDLOOP.
  ENDIF.
  CONCATENATE p_meth_def '.' gc_pragma_ec_needed INTO p_meth_def RESPECTING BLANKS.
ENDFORM.

FORM build_wdy_evt_handle_meth_def USING p_wdy_ctlr_compo  TYPE wdy_ctlr_compo
                                         p_prefix          TYPE string
                                         p_suffix          TYPE string
                                CHANGING p_meth_def TYPE string.
  DATA: lt_cur_ctlr_param TYPE tt_wdy_ctlr_param,
        lv_cur_ctlr_param TYPE wdy_ctlr_param,
        lv_meth           TYPE string.

  CONCATENATE p_prefix p_wdy_ctlr_compo-cmpname p_suffix INTO lv_meth.
  CONCATENATE 'METHODS' lv_meth INTO p_meth_def SEPARATED BY space.
  CONCATENATE p_meth_def 'IMPORTING' gc_wdevent_param 'TYPE' 'REF' 'TO' gc_cl_wd_custom_event INTO p_meth_def SEPARATED BY space.

  PERFORM get_wdy_ctlr_meth_parameters USING p_wdy_ctlr_compo-component_name p_wdy_ctlr_compo-controller_name p_wdy_ctlr_compo-cmpname CHANGING lt_cur_ctlr_param.

  IF lt_cur_ctlr_param IS NOT INITIAL AND lines( lt_cur_ctlr_param ) GT 0.
    LOOP AT lt_cur_ctlr_param INTO lv_cur_ctlr_param.
      IF lv_cur_ctlr_param-abap_typing = '1'.
        CONCATENATE p_meth_def  lv_cur_ctlr_param-parameter_name 'TYPE' 'REF' 'TO' lv_cur_ctlr_param-abap_type INTO p_meth_def SEPARATED BY space.
      ELSE.
        CONCATENATE p_meth_def  lv_cur_ctlr_param-parameter_name 'TYPE' lv_cur_ctlr_param-abap_type INTO p_meth_def SEPARATED BY space.
      ENDIF.
    ENDLOOP.
  ELSE.
    IF flg_dbg_level > 4 .
      WRITE: /,  'No parameters found for component ', p_wdy_ctlr_compo-component_name, ' and ', p_wdy_ctlr_compo-controller_name.
    ENDIF.
  ENDIF.

  CONCATENATE p_meth_def '.' gc_pragma_ec_needed INTO p_meth_def RESPECTING BLANKS.
ENDFORM.

FORM build_wdy_evt_handle_meth_defs USING p_wdy_ctlr_compo  TYPE tt_wdy_ctlr_compo
                                          p_prefix          TYPE string
                                          p_suffix          TYPE string
                                 CHANGING p_code_body       TYPE tty_string.
  DATA:
    lv_wdy_ctlr_compo TYPE wdy_ctlr_compo,
    lv_meth_def       TYPE string.

  LOOP AT p_wdy_ctlr_compo INTO lv_wdy_ctlr_compo.
    PERFORM build_wdy_evt_handle_meth_def USING lv_wdy_ctlr_compo '' '' CHANGING lv_meth_def.
    APPEND lv_meth_def TO p_code_body.
  ENDLOOP.
ENDFORM.

FORM build_wdy_ctlr_meth_defs USING p_wdy_ctlr_compo  TYPE tt_wdy_ctlr_compo
                                    p_prefix          TYPE string
                                    p_suffix          TYPE string
                                    p_store           TYPE abap_bool
                           CHANGING p_code_body       TYPE tty_string.
  DATA:
    lv_wdy_ctlr_compo TYPE wdy_ctlr_compo,
    lt_cur_ctlr_param TYPE tt_wdy_ctlr_param,
    lv_cur_ctlr_param TYPE wdy_ctlr_param,
    lv_meth           TYPE string,
    lv_meth_def       TYPE string.

  LOOP AT p_wdy_ctlr_compo INTO lv_wdy_ctlr_compo.
    CONCATENATE p_prefix lv_wdy_ctlr_compo-cmpname p_suffix INTO lv_meth.

    CLEAR lt_cur_ctlr_param.
    PERFORM get_wdy_ctlr_meth_parameters USING lv_wdy_ctlr_compo-component_name lv_wdy_ctlr_compo-controller_name lv_wdy_ctlr_compo-cmpname CHANGING lt_cur_ctlr_param.

    IF lt_cur_ctlr_param IS INITIAL OR lines( lt_cur_ctlr_param ) EQ 0.
      IF flg_dbg_level > 4.
        WRITE: /, 'No method parameters found for component ', lv_wdy_ctlr_compo-component_name, ' controller ',  lv_wdy_ctlr_compo-controller_name, ' method ', lv_wdy_ctlr_compo-cmpname.
      ENDIF.
    ENDIF.

    PERFORM build_wdy_ctlr_meth_def USING lv_meth lt_cur_ctlr_param CHANGING lv_meth_def.

    APPEND lv_meth_def TO p_code_body.

    IF p_store = abap_true.
      PERFORM add_alias USING lv_meth.
    ENDIF.
  ENDLOOP.
ENDFORM.

FORM build_wdy_ctlr_act_meth_defs USING p_wdy_ctlr_compo  TYPE tt_wdy_ctlr_compo
                                        p_prefix          TYPE string
                                        p_suffix          TYPE string
                               CHANGING p_code_body       TYPE tty_string.
  DATA:
    lv_wdy_ctlr_compo TYPE wdy_ctlr_compo,
    lt_cur_ctlr_param TYPE tt_wdy_ctlr_param,
    lv_cur_ctlr_param TYPE wdy_ctlr_param,
    lv_meth           TYPE string,
    lv_meth_def       TYPE string.

  LOOP AT p_wdy_ctlr_compo INTO lv_wdy_ctlr_compo.
    CONCATENATE p_prefix lv_wdy_ctlr_compo-cmpname p_suffix INTO lv_meth.
    CONCATENATE 'METHODS' lv_meth 'RETURNING' 'VALUE(result)' 'TYPE' 'REF' 'TO' gc_if_wd_action '.' gc_pragma_ec_needed INTO lv_meth_def SEPARATED BY space.
    APPEND lv_meth_def TO p_code_body.
  ENDLOOP.
ENDFORM.

FORM gen_wdy_ig_comp_ctlr_types USING p_component_name  TYPE wdy_component-component_name
                                      p_controller_name TYPE wdy_controller-controller_name
                             CHANGING p_code_body       TYPE tty_string.
  DATA: lv_wdy_ctx_node TYPE wdy_ctx_node,
        lt_wdy_ctx_node TYPE TABLE OF wdy_ctx_node.
  DATA: lv_wdy_ctx_attrib TYPE wdy_ctx_attrib,
        lt_wdy_ctx_attrib TYPE TABLE OF wdy_ctx_attrib.

  DATA: lv_temp  TYPE string,
        lv_temp1 TYPE string.

  CLEAR lt_wdy_ctx_node.
  PERFORM get_wdy_ctx_nodes USING p_component_name p_controller_name CHANGING lt_wdy_ctx_node.

  IF lt_wdy_ctx_node IS NOT INITIAL AND lines( lt_wdy_ctx_node ) GT 0.
    SORT lt_wdy_ctx_node BY node_position.

    LOOP AT lt_wdy_ctx_node INTO lv_wdy_ctx_node.
      DATA: lv_abap_type TYPE string.
      DATA: lv_node_name TYPE string.

      lv_node_name = lv_wdy_ctx_node-node_name.
      TRANSLATE lv_node_name TO LOWER CASE.

      lv_temp1 = lv_wdy_ctx_node-node_name.
      PERFORM gen_wdy_constant USING lv_temp1 CHANGING lv_temp.
      APPEND lv_temp  TO p_code_body.

      CONCATENATE gc_wd_ctx_prefix lv_temp1 INTO lv_temp1.
      PERFORM add_alias USING lv_temp1.

      APPEND 'TYPES:' TO p_code_body.

      CONCATENATE gc_wd_element_prefix '_' lv_wdy_ctx_node-node_name INTO lv_temp1.
      PERFORM add_alias USING lv_temp1.

      IF lv_wdy_ctx_node-abap_type IS INITIAL.
        CONCATENATE 'BEGIN' 'OF' INTO lv_temp SEPARATED BY space.
        TRANSLATE   lv_temp1 TO LOWER CASE.
        CONCATENATE lv_temp lv_temp1 INTO lv_temp SEPARATED BY space.
        CONCATENATE lv_temp ',' INTO lv_temp.
        APPEND lv_temp TO p_code_body.

        CLEAR lt_wdy_ctx_attrib.
        PERFORM get_wdy_ctx_attrib USING p_component_name p_controller_name lv_wdy_ctx_node-node_name CHANGING lt_wdy_ctx_attrib.
        IF lt_wdy_ctx_attrib IS NOT INITIAL AND lines( lt_wdy_ctx_attrib ) GT 0.
          SORT lt_wdy_ctx_attrib BY attrib_position.

          LOOP AT lt_wdy_ctx_attrib INTO lv_wdy_ctx_attrib.
            DATA: lv_attr_name TYPE string.

            lv_attr_name = lv_wdy_ctx_attrib-attribute_name.
            TRANSLATE lv_attr_name TO LOWER CASE.
            lv_abap_type = lv_wdy_ctx_attrib-abap_type.
            TRANSLATE lv_abap_type TO LOWER CASE.

            PERFORM get_type_declaration USING lv_attr_name lv_abap_type abap_false ',' CHANGING lv_temp.
            APPEND lv_temp TO p_code_body.
          ENDLOOP.
        ELSE.
          IF flg_dbg_level > 4.
            WRITE: /, 'No context attributes found for component ', p_component_name, ' controller ', p_controller_name, ' node ', lv_wdy_ctx_node-node_name.
          ENDIF.
        ENDIF.

        CONCATENATE 'END' 'OF' lv_temp1 INTO lv_temp SEPARATED BY space.
        CONCATENATE lv_temp ',' gc_pragma_ec_needed INTO lv_temp RESPECTING BLANKS.
        APPEND lv_temp TO p_code_body.

        CONCATENATE gc_wd_elements_prefix '_' lv_node_name INTO lv_temp.
        PERFORM add_alias USING lv_temp.

        PERFORM get_table_type_declaration USING lv_temp lv_temp1 '.' CHANGING lv_temp.
        APPEND lv_temp TO p_code_body.

      ELSE.
        lv_abap_type = lv_wdy_ctx_node-abap_type.
        TRANSLATE lv_abap_type TO LOWER CASE.

        PERFORM get_type_declaration USING lv_temp1 lv_abap_type abap_false ',' CHANGING lv_temp.
        APPEND lv_temp TO p_code_body.

        CONCATENATE gc_wd_elements_prefix '_' lv_node_name INTO lv_temp.
        PERFORM add_alias USING lv_temp.

        PERFORM get_table_type_declaration USING lv_temp lv_temp1 '.' CHANGING lv_temp.
        APPEND lv_temp TO p_code_body.
      ENDIF.
    ENDLOOP.
  ELSE.
    IF flg_dbg_level > 4.
      WRITE: /, 'No context nodes found for component ', p_component_name, ' controller ', p_controller_name.
    ENDIF.
  ENDIF.
ENDFORM.

FORM gen_wdy_ig_comp_ctlr_methods USING p_component_name  TYPE wdy_component-component_name
                                        p_controller_name TYPE wdy_controller-controller_name
                               CHANGING p_code_body       TYPE tty_string.
  DATA: lt_methods TYPE tt_wdy_ctlr_compo.
  DATA: lv_method TYPE wdy_ctlr_compo.
  DATA: lt_cur_ctlr_param TYPE tt_wdy_ctlr_param.
  DATA: lv_cur_ctlr_param TYPE wdy_ctlr_param.
  DATA: lv_meth_def TYPE string.
  DATA: lv_meth TYPE string.
  DATA: lv_ig_ctlr TYPE string.

  IF p_controller_name = gc_wd_comp_ctlr_name.
    PERFORM get_wdy_intf_methods USING p_component_name p_controller_name CHANGING lt_methods.
    PERFORM build_wdy_ctlr_if_meth_def USING p_component_name lt_methods CHANGING p_code_body.
  ELSE.
    CONCATENATE 'get_' gc_wd_comp_ctlr_name '_ctr' INTO lv_meth.
    CONCATENATE gc_wd_ig_prefix gc_wd_comp_ctlr_name  INTO lv_ig_ctlr.

    CONCATENATE 'METHODS' lv_meth 'RETURNING' 'VALUE(result)' 'TYPE' 'REF' 'TO' lv_ig_ctlr INTO lv_meth_def SEPARATED BY space.
    CONCATENATE lv_meth_def '.' gc_pragma_ec_needed INTO lv_meth_def RESPECTING BLANKS.
    APPEND lv_meth_def TO p_code_body.
  ENDIF.

  CLEAR lt_methods.

  PERFORM get_wdy_other_ctlr_methods USING p_component_name p_controller_name CHANGING lt_methods.
  IF lt_methods IS NOT INITIAL AND lines( lt_methods ) GT 0.
    PERFORM build_wdy_ctlr_meth_defs USING lt_methods '' '' abap_true CHANGING p_code_body.
  ELSE.
    IF flg_dbg_level > 4 .
      WRITE: /, 'No controller methods found for component ', p_component_name, ' and ', p_controller_name.
    ENDIF.
  ENDIF.
  CLEAR lt_methods.

  PERFORM get_wdy_intf_ctlr_events USING p_component_name p_controller_name CHANGING lt_methods.
  IF lt_methods IS NOT INITIAL AND lines( lt_methods ) GT 0.
    APPEND '' TO p_code_body.
    APPEND '* +-------------------------------------------------------------------+' TO p_code_body.
    APPEND '* |  fire plugs                                                       |' TO p_code_body.
    APPEND '* +-------------------------------------------------------------------+' TO p_code_body.
    APPEND '' TO p_code_body.
    PERFORM build_wdy_ctlr_meth_defs USING lt_methods 'fire_' '_evt' abap_true CHANGING p_code_body.
  ELSE.
    IF flg_dbg_level > 4 .
      WRITE: /, 'No fire plugs found for component ', p_component_name, ' and ', p_controller_name.
    ENDIF.
  ENDIF.

  PERFORM get_wd_api_def CHANGING lv_meth_def.
  APPEND lv_meth_def TO p_code_body.
  PERFORM add_alias USING gc_wd_get_api.
ENDFORM.

FORM download_wdy_source USING p_progname TYPE tadir-obj_name
                               p_level    TYPE  i.
  DATA: BEGIN OF ls_wdy_ctlr_code,
          ctlr_code TYPE TABLE OF string,
        END OF ls_wdy_ctlr_code.
  DATA: ty_wd_ctlr_attributes TYPE TABLE OF ty_wd_ctlr_attribute.

  TYPES: BEGIN OF ty_wdy_ctlr_code.
  TYPES: component_name TYPE wdy_component-component_name,
         ctlr_name      TYPE wdy_controller-controller_name,
         class_name     TYPE string,
         file_name      TYPE string,
         cmp_ctr        TYPE string,
         attributes     LIKE ty_wd_ctlr_attributes.
         INCLUDE STRUCTURE ls_wdy_ctlr_code AS code_body.
  TYPES: END OF ty_wdy_ctlr_code.

  FIELD-SYMBOLS: <ls_found_wdy_ex> TYPE wdy_ctlr_compo.
  DATA: lt_wdy_ctlr_compo TYPE TABLE OF wdy_ctlr_compo.
  DATA: lv_class_name TYPE string.
  DATA: lv_file_name  TYPE string.
  DATA: lt_ctlr_meth_defs TYPE TABLE OF ty_wdy_ctlr_code.
  DATA: lv_ctlr_meth_def TYPE ty_wdy_ctlr_code.
  DATA: lv_wdy_filename TYPE string.
  DATA: lv_download_pathname TYPE string.
  DATA: lv_name TYPE  wdy_component_name.
  DATA: lv_comp_assist_cls TYPE wdy_component-assistance_class.
  DATA: lv_temp TYPE string.

  REFRESH: gt_wd_referred_ifs, gt_wd_includes.

  lv_name = p_progname.

  IF flg_dbg_level > 4 .
    WRITE: /,  'Current WD component ', p_progname.
  ENDIF.

  PERFORM get_wdy_assistance_class USING lv_name CHANGING lv_comp_assist_cls.

  SELECT * FROM wdy_ctlr_compo
  INTO TABLE lt_wdy_ctlr_compo
  WHERE component_name = lv_name
  AND   cmptype IN ('CL_WDY_MD_CONTROLLER_METHOD', 'CL_WDY_MD_CTLR_EVENT_HANDLER').

  LOOP AT lt_wdy_ctlr_compo ASSIGNING <ls_found_wdy_ex>.
    DATA lt_lines TYPE TABLE OF string.
    DATA  lt_cur_ctlr_param TYPE TABLE OF wdy_ctlr_param.
    DATA lv_meth_def TYPE string.

    CLEAR lt_lines.
    SPLIT <ls_found_wdy_ex>-code_body AT cl_abap_char_utilities=>cr_lf INTO TABLE lt_lines.

    IF <ls_found_wdy_ex>-cmptype = 'CL_WDY_MD_CTLR_EVENT_HANDLER'.
      PERFORM build_wdy_evt_handle_meth_def USING <ls_found_wdy_ex> '' '' CHANGING lv_meth_def.
    ELSE.
      CLEAR lt_cur_ctlr_param.
      PERFORM get_wdy_ctlr_meth_parameters USING <ls_found_wdy_ex>-component_name <ls_found_wdy_ex>-controller_name <ls_found_wdy_ex>-cmpname CHANGING lt_cur_ctlr_param.
      IF lt_cur_ctlr_param IS INITIAL OR lines( lt_cur_ctlr_param ) = 0.
        IF flg_dbg_level > 4.
          WRITE: /, 'No method parameters found for component ', <ls_found_wdy_ex>-component_name, ' controller ',  <ls_found_wdy_ex>-controller_name, ' method ', <ls_found_wdy_ex>-cmpname.
        ENDIF.
      ENDIF.
      lv_temp = <ls_found_wdy_ex>-cmpname.
      PERFORM build_wdy_ctlr_meth_def USING lv_temp lt_cur_ctlr_param CHANGING lv_meth_def.
    ENDIF.

    CLEAR lv_ctlr_meth_def.
    READ TABLE lt_ctlr_meth_defs INTO lv_ctlr_meth_def WITH KEY component_name = <ls_found_wdy_ex>-component_name ctlr_name = <ls_found_wdy_ex>-controller_name.
    IF sy-subrc = 0 AND lv_ctlr_meth_def IS NOT INITIAL.
      APPEND lv_meth_def TO lv_ctlr_meth_def-code_body-ctlr_code.
      PERFORM gen_alphanum_str USING lv_ctlr_meth_def-cmp_ctr CHANGING lv_temp.
      lv_ctlr_meth_def-cmp_ctr = lv_temp.
      DELETE lt_ctlr_meth_defs WHERE component_name = <ls_found_wdy_ex>-component_name AND ctlr_name = <ls_found_wdy_ex>-controller_name.
    ELSE.
      lv_ctlr_meth_def-component_name = <ls_found_wdy_ex>-component_name.
      lv_ctlr_meth_def-ctlr_name = <ls_found_wdy_ex>-controller_name.

      PERFORM gen_alphanum_str USING '000' CHANGING lv_temp.
      lv_ctlr_meth_def-cmp_ctr = lv_temp.

      PERFORM gen_class_name USING <ls_found_wdy_ex>-component_name <ls_found_wdy_ex>-controller_name CHANGING lv_class_name lv_file_name.
      TRANSLATE lv_file_name  TO UPPER CASE.
      TRANSLATE lv_class_name TO UPPER CASE.
      lv_ctlr_meth_def-class_name = lv_class_name.
      lv_ctlr_meth_def-file_name  = lv_file_name.

      APPEND lv_meth_def TO lv_ctlr_meth_def-code_body-ctlr_code.
    ENDIF.
    APPEND lv_ctlr_meth_def TO lt_ctlr_meth_defs.

    CLEAR   lv_wdy_filename.
    CLEAR   lv_download_pathname.

    " Ignore empty sources
    IF lt_lines IS NOT INITIAL AND lines( lt_lines ) GT 0.
      DATA meth_ctr TYPE string.
      meth_ctr = lv_ctlr_meth_def-cmp_ctr.
      CONCATENATE lv_ctlr_meth_def-file_name 'CM' meth_ctr INTO lv_wdy_filename.
      PERFORM create_file USING lt_lines lv_wdy_filename <ls_found_wdy_ex>-component_name.
    ENDIF.
  ENDLOOP.

  IF flg_dbg_level > 4 .
    WRITE: /, 'Flushed all the controller method implementations to their respective files.'.
  ENDIF.

  DATA: wa_ctlr_meth_def TYPE ty_wdy_ctlr_code.
  DATA: lt_all_lines TYPE TABLE OF string.
  DATA: lt_ctor_impl_lines TYPE TABLE OF string.
  DATA: lv_ctor_def_str TYPE string.
  DATA: lt_ctlr_attr_data_def_lines TYPE TABLE OF string.

  LOOP AT lt_ctlr_meth_defs INTO wa_ctlr_meth_def.
    CLEAR lt_all_lines.
    CLEAR lt_ctor_impl_lines.
    CLEAR lt_ctlr_attr_data_def_lines.

    lv_class_name = wa_ctlr_meth_def-class_name.
    lv_file_name  = wa_ctlr_meth_def-file_name.

    IF flg_dbg_level > 4 .
      WRITE: /,  'Current WD controller ', wa_ctlr_meth_def-ctlr_name.
    ENDIF.

    PERFORM download_wdy_cc_iwci USING wa_ctlr_meth_def-component_name wa_ctlr_meth_def-ctlr_name.

    PERFORM build_wd_ctlr_ctor USING wa_ctlr_meth_def-ctlr_name lv_comp_assist_cls CHANGING lv_ctor_def_str lt_ctlr_attr_data_def_lines lt_ctor_impl_lines.

    CONCATENATE lv_file_name 'CM' INTO lv_wdy_filename.
    PERFORM create_file USING lt_ctor_impl_lines lv_wdy_filename wa_ctlr_meth_def-component_name.
    IF flg_dbg_level > 4 .
      WRITE: /,  'Built the constructor method for ', wa_ctlr_meth_def-ctlr_name.
    ENDIF.

    CLEAR gt_aliases.

    PERFORM add_comp_details_comment USING wa_ctlr_meth_def-component_name wa_ctlr_meth_def-ctlr_name CHANGING lt_all_lines.

    IF wa_ctlr_meth_def-ctlr_name = gc_wd_comp_ctlr_name.
      PERFORM build_wdy_comp_ctlr_ig USING wa_ctlr_meth_def-component_name wa_ctlr_meth_def-ctlr_name CHANGING lt_all_lines.
      IF flg_dbg_level > 4 .
        WRITE: /,  'Built the ig_componentcontroller interface.'.
      ENDIF.
    ENDIF.

    PERFORM build_wdy_ctlr_intf USING wa_ctlr_meth_def-component_name wa_ctlr_meth_def-ctlr_name CHANGING lt_all_lines.
    IF flg_dbg_level > 4 .
      WRITE: /,  'Built the controller interface.'.
    ENDIF.

    CONCATENATE lv_file_name 'CCDEF' INTO lv_wdy_filename.
    PERFORM create_file USING lt_all_lines lv_wdy_filename wa_ctlr_meth_def-component_name.
    IF flg_dbg_level > 4 .
      WRITE: /,  'Flushed the controller interface code to CCDEF file.'.
    ENDIF.

    PERFORM create_wd_ctlr_class_def USING wa_ctlr_meth_def-component_name lv_class_name lv_file_name lv_ctor_def_str lt_ctlr_attr_data_def_lines wa_ctlr_meth_def-code_body-ctlr_code.
    IF flg_dbg_level > 4 .
      WRITE: /,  'Built the controller class definition.'.
    ENDIF.

    PERFORM create_wd_ctlr_class_pool USING wa_ctlr_meth_def-component_name lv_class_name lv_file_name.
    IF flg_dbg_level > 4 .
      WRITE: /,  'Created the class pool file for controller class and flushed the includes'.
    ENDIF.
  ENDLOOP.
ENDFORM.

FORM download_wdyn USING p_progname TYPE tadir-obj_name
                         p_level    TYPE i.
  DATA: lv_tadir        TYPE tadir,
        lv_into         TYPE progname_type,
        lv_level        TYPE i,
        lv_temp         TYPE string,
        is_non_std_code TYPE abap_bool VALUE abap_false,
        lt_wd_includes  TYPE TABLE OF trdir.

  IF p_level > p_depth.
    RETURN.
  ENDIF.
  READ TABLE gt_progname_tab WITH TABLE KEY table_line = p_progname TRANSPORTING NO FIELDS .
  IF sy-subrc = 0 .
    IF flg_dbg_level > 1 .
      WRITE: /, 'Downloaded  ',  p_progname,  ' already'.
    ENDIF.
    RETURN.
  ELSE .
    PERFORM is_non_std_sap_prog USING p_progname CHANGING is_non_std_code.
    IF p_sap = abap_true OR ( p_sap = abap_false AND is_non_std_code = abap_true ).
      INSERT p_progname INTO TABLE gt_progname_tab.

      PERFORM download_wdyn_metadata USING p_progname.
      IF flg_dbg_level > 1 .
        WRITE: /,  'Downloaded metadata for WD component ', p_progname.
      ENDIF.

      PERFORM download_wdy_source USING p_progname 0.
      IF flg_dbg_level > 1 .
        WRITE: /, 'Downloaded source code for WD component ', p_progname.
      ENDIF.

      LOOP AT gt_wd_referred_ifs INTO lv_into.
        lv_temp = lv_into.
        PERFORM extract_interface USING lv_temp.
      ENDLOOP.

      " Clone the includes and then traverse.
      lt_wd_includes[] = gt_wd_includes[].

      LOOP AT lt_wd_includes INTO lv_tadir.
        IF lv_tadir-object = 'WDYN'.
          IF flg_dbg_level > 4 .
            WRITE: /, 'Going to download WD dependency ', p_progname.
          ENDIF.
          lv_level = p_level + 1.
          PERFORM download_wdyn USING lv_tadir-obj_name lv_level.
        ELSEIF lv_tadir-object = 'CLAS'.
          PERFORM get_all_class_includes TABLES matching_progs USING clsname.
        ELSE.
          APPEND lv_tadir-obj_name TO matching_progs.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.
ENDFORM.

CONSTANTS c_enhancement TYPE string VALUE 'ENHANCEMENT' ##NO_TEXT.
CONSTANTS c_endenhancement TYPE string VALUE 'ENDENHANCEMENT.' ##NO_TEXT.

TYPES: BEGIN OF ty_fugr_map,
         function TYPE string,
         include  TYPE progname,
       END OF ty_fugr_map.

TYPES: tty_fugr_map TYPE TABLE OF ty_fugr_map WITH KEY function.

TYPES: BEGIN OF ty_fugr,
         name          TYPE progname,
         top_name      TYPE progname,
         uxx_name      TYPE progname,
         includes      TYPE progname_tab_type,
         uxx_includes  TYPE progname_tab_type,
         uxx_incl_map  TYPE tty_fugr_map,
         uxx_cust_incl TYPE progname_tab_type,
       END OF ty_fugr.

FORM get_prefixed_fugr_name USING p_fugr_name     TYPE progname
                         CHANGING p_prefixed_name TYPE progname.
  IF p_fugr_name(1) = '/'.
    TRY.
        CALL FUNCTION 'TRINT_SPLIT_OBJECT'
          EXPORTING
            iv_obj_name    = p_fugr_name
          IMPORTING
            ev_prefix      = ev_prefix
            ev_prefix_name = ev_prefix_name
            ev_stem        = ev_stem
          EXCEPTIONS
            invalid_prefix = 1
            OTHERS         = 2.

        IF sy-subrc <> 0.
          IF flg_dbg_level > 4.
            WRITE: /, 'Failed to split ', p_fugr_name.
            RETURN.
          ENDIF.
        ENDIF.

      CATCH cx_root.
        IF flg_dbg_level > 4.
          WRITE: /, 'Exception occurred while splitting ', p_fugr_name.
          RETURN.
        ENDIF.
    ENDTRY.


    CONCATENATE ev_prefix 'SAPL' ev_stem  INTO p_prefixed_name.
  ELSE.
    CONCATENATE 'SAPL' p_fugr_name INTO p_prefixed_name.
  ENDIF.
ENDFORM.

FORM populate_fugr_map USING p_include_stmt TYPE includestmttype
                    CHANGING p_fugr_data TYPE ty_fugr.

  DATA lt_keyword_tbl TYPE TABLE OF char30.
  DATA lt_token_tbl   TYPE TABLE OF stokes.
  DATA lt_stmt_tbl    TYPE TABLE OF sstmnt.

  DATA token_line  TYPE stokes.
  DATA itab        TYPE tty_string.
  DATA lv_fugr_map TYPE ty_fugr_map.

  APPEND 'FUNCTION' TO lt_keyword_tbl .

  DATA: lv_prog_name TYPE progname.
  lv_prog_name = p_include_stmt.
  READ REPORT lv_prog_name STATE 'A' INTO itab .

  IF itab IS NOT INITIAL.
    SCAN ABAP-SOURCE itab TOKENS INTO lt_token_tbl  STATEMENTS INTO lt_stmt_tbl  KEYWORDS FROM lt_keyword_tbl.

    IF lt_token_tbl IS NOT INITIAL.
      DELETE lt_token_tbl WHERE str = 'FUNCTION'.
      LOOP AT lt_token_tbl INTO token_line .
        lv_fugr_map-function = token_line-str.
        lv_fugr_map-include = p_include_stmt.
        APPEND lv_fugr_map TO p_fugr_data-uxx_incl_map.
        " Just one record
        EXIT.
      ENDLOOP.
    ENDIF.
  ENDIF.
ENDFORM.

FORM create_fugr_rec USING p_functiongroup_name TYPE progname
                  CHANGING p_fugr_data TYPE ty_fugr.
  DATA: lt_functiongroup_srccode  TYPE sourcecodetabletype,
        lt_include_stmts          TYPE includestmttabletype,
        lv_functiongroup_basename TYPE string,
        lv_functiongroup_top_name TYPE progname,
        lv_functiongroup_uxx_name TYPE progname,
        lv_prog_name              TYPE progname.
  FIELD-SYMBOLS: <include_stmt_object> TYPE string.

  lv_functiongroup_basename = p_functiongroup_name.

  CONDENSE lv_functiongroup_basename NO-GAPS .
  SHIFT lv_functiongroup_basename LEFT BY 3 PLACES.
  CONCATENATE lv_functiongroup_basename 'TOP' INTO lv_functiongroup_top_name.
  CONCATENATE lv_functiongroup_basename 'UXX' INTO lv_functiongroup_uxx_name.

  p_fugr_data-name = p_functiongroup_name.
  p_fugr_data-top_name = lv_functiongroup_top_name.
  p_fugr_data-uxx_name = lv_functiongroup_uxx_name.

  READ REPORT p_functiongroup_name STATE 'A' INTO lt_functiongroup_srccode .

  PERFORM parse_include_stmts_into_table USING lt_functiongroup_srccode CHANGING lt_include_stmts.
  LOOP AT lt_include_stmts ASSIGNING <include_stmt_object>.
    lv_prog_name = <include_stmt_object>.
    INSERT lv_prog_name INTO TABLE p_fugr_data-includes.
    IF <include_stmt_object> = lv_functiongroup_uxx_name.
      DATA: lt_uxx_srccode    TYPE sourcecodetabletype,
            lt_uxx_incl_stmts TYPE includestmttabletype,
            lv_uxx_incl_stmt  TYPE  includestmttype.
      REFRESH: lt_uxx_srccode,  lt_uxx_incl_stmts.
      READ REPORT lv_functiongroup_uxx_name  STATE 'A' INTO lt_uxx_srccode.
      PERFORM parse_include_stmts_into_table USING lt_uxx_srccode CHANGING lt_uxx_incl_stmts.
      LOOP AT lt_uxx_incl_stmts INTO lv_uxx_incl_stmt.
        lv_prog_name = lv_uxx_incl_stmt.
        INSERT lv_prog_name INTO TABLE p_fugr_data-uxx_includes.
        PERFORM populate_fugr_map USING lv_uxx_incl_stmt CHANGING p_fugr_data.
      ENDLOOP.
    ELSEIF <include_stmt_object> = lv_functiongroup_top_name.
      " Do nothing
    ELSE.
      PERFORM download_source_code USING p_temp lv_prog_name abap_filename_ext '' 0 ''.
    ENDIF.
  ENDLOOP.

  PERFORM create_file_default USING lt_functiongroup_srccode p_functiongroup_name.
ENDFORM.

FORM download_function USING p_func_name TYPE string
                    CHANGING p_fugr TYPE ty_fugr.
  IF p_fugr IS NOT INITIAL.
    DATA: lv_fugr_map TYPE ty_fugr_map.
    READ TABLE p_fugr-uxx_incl_map INTO lv_fugr_map WITH KEY function = p_func_name.
    IF sy-subrc = 0.
      PERFORM download_source_code USING p_temp lv_fugr_map-include abap_filename_ext  ''  0  ''.
      INSERT lv_fugr_map-include INTO TABLE p_fugr-uxx_cust_incl.
    ENDIF.
  ENDIF.
ENDFORM.

FORM extract_enhs USING p_enh_id        TYPE enhincinx-enhname
                        pt_enhancements TYPE enh_hook_impl_it.
  DATA lv_source TYPE string.
  FIELD-SYMBOLS <ls_enhancement> LIKE LINE OF pt_enhancements.
  DATA: lt_enh_code TYPE tty_string.

  DATA: lv_enh_hook_ctr     TYPE i VALUE 1,
        lv_enh_hook_ctr_str TYPE string.

  LOOP AT pt_enhancements ASSIGNING <ls_enhancement>.
    " Add full name as comment and put code between enhancement statements
    lv_source = c_enhancement.
    lv_enh_hook_ctr_str = lv_enh_hook_ctr.

    CONCATENATE c_enhancement lv_enh_hook_ctr_str '.' INTO lv_source SEPARATED BY space.
    INSERT lv_source INTO <ls_enhancement>-source INDEX 1.

    CONCATENATE '"Name: ' <ls_enhancement>-full_name INTO lv_source RESPECTING BLANKS.
    INSERT lv_source INTO <ls_enhancement>-source INDEX 1.
    APPEND c_endenhancement TO <ls_enhancement>-source.

    APPEND LINES OF <ls_enhancement>-source TO lt_enh_code.
    APPEND '' TO lt_enh_code.

    ADD 1 TO lv_enh_hook_ctr.
  ENDLOOP.

  PERFORM create_file_default USING lt_enh_code p_enh_id.
ENDFORM.

FORM create_enhc_class_pool USING p_enh_id TYPE enhheader-enhname
                                  p_includes TYPE progname_tab_type.

  DATA: lv_temp         TYPE string,
        lv_wdy_filename TYPE string,
        lt_code_body    TYPE tty_string,
        lv_include      TYPE progname.

  CONCATENATE 'CLASS-POOL' '.' INTO lv_temp SEPARATED BY space.
  APPEND lv_temp TO lt_code_body.

  CONCATENATE '*"* class pool for class ' p_enh_id INTO lv_temp RESPECTING BLANKS.
  APPEND lv_temp TO lt_code_body.

  APPEND '' TO lt_code_body.

  CONCATENATE '*"*' 'CLASS' p_enh_id 'DEFINITION' INTO lv_temp SEPARATED BY space.
  APPEND lv_temp TO lt_code_body.

  LOOP AT p_includes INTO lv_include.
    CONCATENATE ' INCLUDE' lv_include INTO lv_temp SEPARATED BY space RESPECTING BLANKS.
    CONCATENATE lv_temp '.' INTO lv_temp.
    APPEND lv_temp TO lt_code_body.
  ENDLOOP.

  CONCATENATE 'ENDCLASS. "' p_enh_id INTO lv_temp.
  CONCATENATE lv_temp 'DEFINITION' INTO lv_temp SEPARATED BY space.
  APPEND lv_temp TO lt_code_body.

  APPEND '' TO lt_code_body.

  APPEND '' TO lt_code_body.

  CONCATENATE 'CLASS' p_enh_id 'IMPLEMENTATION.' INTO lv_temp SEPARATED BY space.
  APPEND lv_temp TO lt_code_body.

  APPEND '*"* method implementations' TO lt_code_body.

  APPEND ' INCLUDE METHODS.' TO lt_code_body.

  CONCATENATE 'ENDCLASS. "' p_enh_id INTO lv_temp RESPECTING BLANKS.
  CONCATENATE lv_temp 'IMPLEMENTATION' INTO lv_temp SEPARATED BY space.
  APPEND lv_temp TO lt_code_body.

  CONCATENATE p_enh_id '======' 'CP' INTO lv_wdy_filename.
  PERFORM create_file USING lt_code_body lv_wdy_filename p_enh_id.
ENDFORM.

FORM serialize_includes USING p_enh_id TYPE enhheader-enhname
                              io_class TYPE REF TO cl_enh_tool_class.

  DATA: lt_includes TYPE enhnewmeth_tabincl_plus_enha,
        lv_include  TYPE syrepid.

  FIELD-SYMBOLS: <ls_include> LIKE LINE OF lt_includes.

  " Enhancement method includes
  lt_includes = io_class->get_enh_method_includes( ).
  LOOP AT lt_includes ASSIGNING <ls_include>.
    lv_include = io_class->if_enh_tool~get_name( ).
    TRANSLATE lv_include USING ' ='.
    lv_include+30 = 'EM'.
    lv_include+32(8) = <ls_include>-includenr.
    PERFORM download_source_code USING p_temp lv_include abap_filename_ext '' 0 ''.
  ENDLOOP.

  " io_class->get_class( IMPORTING class_name = lv_cls_name ).

  " Enhancement Implementation or EIMP file
  " io_class->get_eimp_include( ).

  DATA: lt_enhincinx TYPE TABLE OF enhincinx,
        lv_enhincinx TYPE enhincinx,
        lt_enh_incls TYPE progname_tab_type.

  SELECT *
  FROM   enhincinx
  INTO   TABLE lt_enhincinx
  WHERE  enhname = p_enh_id.

  IF sy-subrc = 0.
    LOOP AT lt_enhincinx INTO lv_enhincinx.
      IF lv_enhincinx-enhinclude IS NOT INITIAL.
        PERFORM download_source_code USING p_temp lv_enhincinx-enhinclude abap_filename_ext '' 0 ''.
        READ TABLE lt_enh_incls WITH TABLE KEY table_line = lv_enhincinx-enhinclude TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          INSERT lv_enhincinx-enhinclude INTO TABLE lt_enh_incls.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

  PERFORM create_enhc_class_pool USING p_enh_id lt_enh_incls.
ENDFORM.

FORM serialize_enho USING p_enh_id TYPE enhobj-enhname.

  DATA: li_enh_tool TYPE REF TO if_enh_tool.
  TRY.
      li_enh_tool = cl_enh_factory=>get_enhancement(
                                      enhancement_id   = p_enh_id
                                      run_dark         = abap_true
                                      bypassing_buffer = abap_true ).
      CASE li_enh_tool->get_tool(  ).
        WHEN cl_enh_tool_hook_impl=>tooltype.
          PERFORM get_hook_enhancements USING p_enh_id li_enh_tool.
        WHEN cl_enh_tool_class=>tooltype.
          PERFORM get_class_enhnancements USING p_enh_id li_enh_tool.
        WHEN cl_enh_tool_intf=>tooltype.
          PERFORM get_intf_enhnancements USING p_enh_id li_enh_tool.
          "WHEN cl_wdr_cfg_enhancement=>tooltype.
        WHEN cl_enh_tool_badi_impl=>tooltype.
          PERFORM get_badi_enhancements USING li_enh_tool.
        WHEN 'FUGRENH'.
          PERFORM get_fugr_enhancements USING li_enh_tool.
        WHEN 'WDYENH'.
          PERFORM get_wdy_enhancements USING li_enh_tool.
        WHEN OTHERS.
          " Log a message
      ENDCASE.

    CATCH cx_root.
      RETURN.
  ENDTRY.
ENDFORM.

FORM get_hook_enhancements USING p_enh_id    TYPE enhheader-enhname
                                 pi_enh_tool TYPE REF TO if_enh_tool.

  DATA: lo_hook_impl       TYPE REF TO cl_enh_tool_hook_impl,
        ls_original_object TYPE enh_hook_admin,
        lt_enhancements    TYPE enh_hook_impl_it.

  lo_hook_impl ?= pi_enh_tool.

  lo_hook_impl->get_original_object(
    IMPORTING
      pgmid     = ls_original_object-pgmid
      obj_name  = ls_original_object-org_obj_name
      obj_type  = ls_original_object-org_obj_type
      main_type = ls_original_object-org_main_type
      main_name = ls_original_object-org_main_name
      program   = ls_original_object-programname ).

  lt_enhancements = lo_hook_impl->get_hook_impls( ).
  PERFORM extract_enhs USING p_enh_id lt_enhancements.
ENDFORM.

FORM get_class_enhnancements USING p_enh_id    TYPE enhheader-enhname
                                   pi_enh_tool TYPE REF TO if_enh_tool.

  DATA: lo_enh_class   TYPE REF TO cl_enh_tool_class.

  lo_enh_class ?= pi_enh_tool.
  PERFORM serialize_includes USING p_enh_id lo_enh_class.
ENDFORM.

FORM get_intf_enhnancements USING p_enh_id    TYPE enhheader-enhname
                                  pi_enh_tool TYPE REF TO if_enh_tool.

  DATA: lo_enh_intf TYPE REF TO cl_enh_tool_intf,
        lv_intf     TYPE seoclsname,
        lv_intf_str TYPE string.

  lo_enh_intf ?= pi_enh_tool.
  lo_enh_intf->get_class( IMPORTING class_name = lv_intf ).
  IF lv_intf IS INITIAL.
    IF flg_dbg_level > 4.
      WRITE: /, 'Could not find interface enhancement', p_enh_id.
    ENDIF.
  ELSE.
    lv_intf_str = lv_intf.
    PERFORM extract_interface USING lv_intf_str.
  ENDIF.

ENDFORM.

FORM get_badi_enhancements USING pi_enh_tool TYPE REF TO if_enh_tool.

  DATA: lo_badi_impl      TYPE REF TO cl_enh_tool_badi_impl,
        lv_spot_name      TYPE enhspotname,
        lt_impl           TYPE enh_badi_impl_data_it,
        lv_impl           TYPE enh_badi_impl_data,
        lv_impl_class     TYPE seoclsname,
        lt_class_includes TYPE TABLE OF trdir WITH HEADER LINE,
        lv_class_include  TYPE trdir.

  lo_badi_impl ?= pi_enh_tool.

  lv_spot_name = lo_badi_impl->get_spot_name( ).
  lt_impl      = lo_badi_impl->get_implementations( ).


  LOOP AT lt_impl INTO lv_impl.
    lv_impl_class = lv_impl-impl_class.
    REFRESH lt_class_includes.
    PERFORM get_all_class_includes TABLES lt_class_includes USING lv_impl_class.
    LOOP AT lt_class_includes INTO lv_class_include.
      PERFORM download_source_code USING p_temp lv_class_include-name abap_filename_ext '' 0 ''.
    ENDLOOP.
  ENDLOOP.

ENDFORM.

FORM get_fugr_enhancements USING ii_enh_tool TYPE REF TO if_enh_tool.

  DATA: lo_fugrdata  TYPE REF TO cl_enh_tool_fugr,
        lv_fugr      TYPE rs38l-area,
        lv_fugr_name TYPE progname,
        lv_prog_name TYPE progname.

  lo_fugrdata ?= ii_enh_tool.

  lo_fugrdata->get_fugr( IMPORTING fugr_name = lv_fugr ).
  lv_fugr_name = lv_fugr.

  PERFORM get_prefixed_fugr_name USING lv_fugr_name CHANGING lv_prog_name.
  PERFORM download_source_code USING p_temp lv_prog_name abap_filename_ext '' 0 ''.

ENDFORM.

FORM get_wdy_enhancements USING ii_enh_tool TYPE REF TO if_enh_tool.

  DATA: lo_wdyn           TYPE REF TO cl_enh_tool_wdy,
        lv_component_name TYPE wdy_component_name,
        lv_prog_name      TYPE progname.

  lo_wdyn ?= ii_enh_tool.
  lv_component_name = lo_wdyn->get_component_name( ).

  lv_prog_name = lv_component_name.
  PERFORM download_wdyn USING lv_prog_name 0.

ENDFORM.

FORM get_enhancement USING p_progname TYPE progname_type.
  DATA: lv_enhheader TYPE enhheader.

  IF p_sap = abap_true.
    RETURN.
  ENDIF.

  DATA lv_enh_id TYPE string.
  lv_enh_id = p_progname.

  SELECT SINGLE *
  FROM   enhheader
  INTO   lv_enhheader
  WHERE  enhname = p_progname
  AND    version = 'A'.

  IF sy-subrc = 0.
    gv_is_user_mod = abap_true.
    TRY.
        PERFORM serialize_enho USING lv_enhheader-enhname.
      CATCH cx_root.
        IF flg_dbg_level > 4.
          WRITE: /, 'Exception occured when serializing includes for', p_progname.
        ENDIF.
    ENDTRY.
    gv_is_user_mod = abap_false.
  ENDIF.
ENDFORM.

FORM get_user_exits USING p_progname TYPE progname_type.
  DATA: lv_modsap    TYPE modsap,
        lt_modsap    TYPE TABLE OF modsap,
        lv_area      TYPE enlfdir-area,
        lv_fugr_name TYPE progname,
        lv_prog_name TYPE progname,
        lv_func_name TYPE string,
        lv_fugr      TYPE ty_fugr,
        lt_fugr      TYPE TABLE OF ty_fugr WITH KEY name,
        lt_uxx_code  TYPE tty_string,
        lv_temp      TYPE string.

  IF p_sap = abap_true.
    RETURN.
  ENDIF.

  SELECT *
  FROM   modsap
  INTO TABLE lt_modsap
  WHERE  name = p_progname
  AND    typ = 'E'.

  gv_is_user_mod = abap_true.

  LOOP AT lt_modsap INTO lv_modsap.
    lv_func_name = lv_modsap-member.

    SELECT SINGLE area
    FROM   enlfdir
    INTO   lv_area
    WHERE  funcname = lv_func_name.

    lv_fugr_name = lv_area.
    PERFORM get_prefixed_fugr_name USING lv_fugr_name CHANGING lv_prog_name.

    CLEAR lv_fugr.
    READ TABLE lt_fugr INTO lv_fugr WITH TABLE KEY name = lv_prog_name.
    IF sy-subrc <> 0.
      PERFORM create_fugr_rec USING lv_prog_name CHANGING lv_fugr.
    ENDIF.

    PERFORM download_function USING lv_func_name CHANGING lv_fugr.

    DELETE lt_fugr WHERE name = lv_prog_name.
    INSERT lv_fugr INTO TABLE lt_fugr.
  ENDLOOP.

  LOOP AT lt_fugr INTO lv_fugr.
    PERFORM download_source_code USING p_temp lv_fugr-top_name abap_filename_ext ''  0  ''.
    REFRESH lt_uxx_code.
    LOOP AT lv_fugr-uxx_cust_incl INTO lv_prog_name.
      CONCATENATE 'INCLUDE' lv_prog_name '.' INTO lv_temp SEPARATED BY space.
      APPEND lv_temp TO lt_uxx_code.
      APPEND '' TO lt_uxx_code.
    ENDLOOP.
    PERFORM create_file USING lt_uxx_code lv_fugr-uxx_name lv_fugr-name.
  ENDLOOP.

  gv_is_user_mod = abap_false.
ENDFORM.