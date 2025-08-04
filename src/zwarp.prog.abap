*&---------------------------------------------------------------------*
*& Report ZWARP
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zwarp.

CLASS lcl_model DEFINITION.
  PUBLIC SECTION.
    TYPES ty_request     TYPE e071-trkorr.
    TYPES ty_object_clas TYPE e071-pgmid.
    TYPES ty_object_type TYPE e071-object.
    TYPES ty_object_name TYPE e071-obj_name.

    TYPES: BEGIN OF ty_object,
             object_clas TYPE ty_object_clas,
             object_type TYPE ty_object_type,
             object_name TYPE ty_object_name,
           END OF ty_object,
           tt_objects TYPE STANDARD TABLE OF ty_object WITH DEFAULT KEY.

    TYPES tr_object_types TYPE RANGE OF ty_object_type.
    TYPES tr_object_names TYPE RANGE OF ty_object_name.

    CLASS-METHODS class_constructor.

    CLASS-METHODS get_objects
      IMPORTING it_object_types   TYPE tr_object_types
                it_object_names   TYPE tr_object_names
      RETURNING VALUE(rt_objects) TYPE tt_objects.

    CLASS-METHODS transport_objects
      IMPORTING  it_objects        TYPE tt_objects
      RETURNING  VALUE(rv_request) TYPE ty_request
      EXCEPTIONS transport_error.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_supported_object,
        object_clas TYPE ty_object_clas,
        object_type TYPE ty_object_type,
        object_conv TYPE ty_object_type,
      END OF ty_supported_object,
      tt_supported_objects TYPE HASHED TABLE OF ty_supported_object
                           WITH UNIQUE KEY object_clas object_type.

    CLASS-DATA mt_supported_objects TYPE tt_supported_objects.

    CLASS-METHODS get_initial_objects
      IMPORTING it_object_types   TYPE tr_object_types
                it_object_names   TYPE tr_object_names
      RETURNING VALUE(rt_objects) TYPE tt_objects.

    CLASS-METHODS expand_with_environment
      IMPORTING iv_object_type TYPE ty_object_type
                iv_object_name TYPE ty_object_name
      CHANGING  ct_objects     TYPE tt_objects.

    CLASS-METHODS expand_with_workbench_tree
      IMPORTING iv_object_type TYPE ty_object_type
                iv_object_name TYPE ty_object_name
      CHANGING  ct_objects     TYPE tt_objects.

    CLASS-METHODS prepare_objects
      CHANGING ct_objects TYPE tt_objects.

    CLASS-METHODS show_progress
      IMPORTING iv_text    TYPE string
                iv_current TYPE i
                iv_total   TYPE i.

    CLASS-METHODS is_custom_object
      IMPORTING iv_object_type   TYPE any OPTIONAL
                iv_object_name   TYPE any
      RETURNING VALUE(rv_result) TYPE abap_bool.
ENDCLASS.


CLASS lcl_model IMPLEMENTATION.
  METHOD class_constructor.
    mt_supported_objects = VALUE #( ( object_clas = 'R3TR' object_type = 'CLAS' )
                                    ( object_clas = 'LIMU' object_type = 'CUAD' )
                                    ( object_clas = 'R3TR' object_type = 'DDLS' )
                                    ( object_clas = 'R3TR' object_type = 'DOMA' )
                                    ( object_clas = 'R3TR' object_type = 'DEVC' )
                                    ( object_clas = 'R3TR' object_type = 'DTEL' )
                                    ( object_clas = 'LIMU' object_type = 'DYNP' )
                                    ( object_clas = 'R3TR' object_type = 'ENHO' )
                                    ( object_clas = 'R3TR' object_type = 'FUGR' )
                                    ( object_clas = 'LIMU' object_type = 'FUNC' )
                                    ( object_clas = 'R3TR' object_type = 'INTF' )
                                    ( object_clas = 'R3TR' object_type = 'MSAG' )
                                    ( object_clas = 'R3TR' object_type = 'PARA' )
                                    ( object_clas = 'R3TR' object_type = 'PROG' )
                                    ( object_clas = 'LIMU' object_type = 'REPS' )
                                    ( object_clas = 'R3TR' object_type = 'SHLP' )
                                    ( object_clas = 'R3TR' object_type = 'TABL' )
                                    ( object_clas = 'R3TR' object_type = 'TTYP' )
                                    ( object_clas = 'R3TR' object_type = 'VIEW' )
                                    ( object_clas = 'R3TR' object_type = 'STRU' object_conv = 'TABL' )
                                    ( object_clas = 'R3TR' object_type = 'ENQU' ) ).
  ENDMETHOD.

  METHOD get_objects.
    rt_objects = get_initial_objects( it_object_types = it_object_types
                                      it_object_names = it_object_names ).

    LOOP AT rt_objects ASSIGNING FIELD-SYMBOL(<object>).
      show_progress( iv_text    = |Processing { <object>-object_type } { <object>-object_name }|
                     iv_current = sy-tabix
                     iv_total   = lines( rt_objects ) ).

      expand_with_environment( EXPORTING iv_object_type = <object>-object_type
                                         iv_object_name = <object>-object_name
                               CHANGING  ct_objects     = rt_objects ).

      expand_with_workbench_tree( EXPORTING iv_object_type = <object>-object_type
                                            iv_object_name = <object>-object_name
                                  CHANGING  ct_objects     = rt_objects ).
    ENDLOOP.

    prepare_objects( CHANGING ct_objects = rt_objects ).
  ENDMETHOD.

  METHOD get_initial_objects.
    SELECT pgmid    AS object_clas,
           object   AS object_type,
           obj_name AS object_name
      FROM tadir
      INTO CORRESPONDING FIELDS OF TABLE @rt_objects
      WHERE object   IN @it_object_types
        AND obj_name IN @it_object_names.
    IF sy-subrc NE 0.
      LOOP AT it_object_types ASSIGNING FIELD-SYMBOL(<fs_types>).
        LOOP AT it_object_names ASSIGNING FIELD-SYMBOL(<fs_names>).
          APPEND VALUE #(
            object_type = <fs_types>-low
            object_name = <fs_names>-low
          ) TO rt_objects.
        ENDLOOP.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD expand_with_environment.
    DATA lt_environment TYPE senvi_tab.

    CALL FUNCTION 'REPOSITORY_ENVIRONMENT_ALL'
      EXPORTING
        obj_type        = CONV euobj-id( iv_object_type )
        object_name     = CONV tadir-obj_name( iv_object_name )
        deep            = '1'
      TABLES
        environment_tab = lt_environment.

    LOOP AT lt_environment ASSIGNING FIELD-SYMBOL(<env>).
      " Include main object if custom
      IF is_custom_object( iv_object_type = <env>-type
                           iv_object_name = <env>-object ).
        COLLECT VALUE ty_object( object_type = <env>-type
                                 object_name = <env>-object ) INTO ct_objects.
      ENDIF.

      " Include enclosing object if exists
      IF <env>-encl_obj IS INITIAL.
        CONTINUE.
      ENDIF.

      SELECT SINGLE object FROM tadir WHERE obj_name = @<env>-encl_obj INTO @DATA(lv_type).
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      IF NOT is_custom_object( iv_object_type = lv_type
                               iv_object_name = <env>-encl_obj ).
        CONTINUE.
      ENDIF.

      COLLECT VALUE ty_object( object_type = lv_type
                               object_name = <env>-encl_obj ) INTO ct_objects.

    ENDLOOP.
  ENDMETHOD.

  METHOD expand_with_workbench_tree.
    TYPES snodetext_tab TYPE STANDARD TABLE OF snodetext WITH DEFAULT KEY.

    DATA lt_nodetab TYPE snodetext_tab.

    CHECK iv_object_type = 'PROG' OR iv_object_type = 'FUGR'.

    CALL FUNCTION 'WB_ANYTYPE_RETURN_OBJECT_LIST'
      EXPORTING
        p_object_type = iv_object_type
        p_object_name = iv_object_name
      TABLES
        nodetab       = lt_nodetab
      EXCEPTIONS
        OTHERS        = 1.

    LOOP AT lt_nodetab ASSIGNING FIELD-SYMBOL(<node>) WHERE type CP 'O*'.
      IF <node>-hide IS NOT INITIAL AND <node>-hide <> <node>-text1.
        CONTINUE.
      ENDIF.

      DATA(lv_object_type) = <node>-text9(4).
      DATA(lv_object_name) = <node>-text9+4.

      IF is_custom_object( iv_object_name = lv_object_name
                           iv_object_type = lv_object_type ).
        COLLECT VALUE ty_object( object_type = lv_object_type
                                 object_name = lv_object_name ) INTO ct_objects.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD prepare_objects.
    " Filter unsupported objects and fill object class
    LOOP AT ct_objects ASSIGNING FIELD-SYMBOL(<object>).
      ASSIGN mt_supported_objects[ object_type = <object>-object_type ] TO FIELD-SYMBOL(<fs_supported_object>).
      IF sy-subrc <> 0.
        DELETE ct_objects.
        CONTINUE.
      ENDIF.

      <object>-object_clas = <fs_supported_object>-object_clas.

      IF <fs_supported_object>-object_conv IS NOT INITIAL.
        <object>-object_type = <fs_supported_object>-object_conv.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD show_progress.
    cl_progress_indicator=>progress_indicate( i_text               = iv_text
                                              i_processed          = iv_current
                                              i_total              = iv_total
                                              i_output_immediately = abap_true ).
  ENDMETHOD.

  METHOD transport_objects.
    DATA(lt_objects) = VALUE tr_objects( FOR <obj> IN it_objects
                                         ( pgmid    = <obj>-object_clas
                                           object   = <obj>-object_type
                                           obj_name = <obj>-object_name ) ).

    DATA(ls_request) = VALUE trwbo_request_header( ).

    CALL FUNCTION 'TR_REQUEST_CHOICE'
      EXPORTING
        it_e071    = lt_objects
      IMPORTING
        es_request = ls_request
      EXCEPTIONS
        OTHERS     = 1.

    IF sy-subrc <> 0.
      RAISE transport_error.
    ENDIF.

    rv_request = ls_request-trkorr.
  ENDMETHOD.

  METHOD is_custom_object.
    DATA(lv_name_upper) = to_upper( iv_object_name ).

    CASE iv_object_type.
      WHEN 'ENQU'.
        rv_result = xsdbool( lv_name_upper CP 'EZ*' OR lv_name_upper CP 'EY*' ).
      WHEN OTHERS.
        rv_result = xsdbool( lv_name_upper CP 'Z*' OR lv_name_upper CP 'Y*' ).
    ENDCASE.
  ENDMETHOD.
ENDCLASS.



TYPES:
  BEGIN OF ty_data,
    checkbox    TYPE checkbox,
    object_clas TYPE e071-pgmid,
    object_type TYPE e071-object,
    object_name TYPE e071-obj_name,
  END OF ty_data.

TYPES tt_data TYPE STANDARD TABLE OF ty_data WITH DEFAULT KEY.

TYPES:
  BEGIN OF ty_report,
    data TYPE tt_data,
    fcat TYPE lvc_t_fcat,
    layo TYPE lvc_s_layo,
    glay TYPE lvc_s_glay,
  END OF ty_report.

DATA report TYPE ty_report.

TABLES e071.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE b1_title.
SELECT-OPTIONS s_type FOR e071-object   OBLIGATORY DEFAULT 'PROG' NO INTERVALS.
SELECT-OPTIONS s_name FOR e071-obj_name OBLIGATORY DEFAULT sy-repid NO INTERVALS.
SELECTION-SCREEN END OF BLOCK b1.

INITIALIZATION.
  PERFORM f_init.

START-OF-SELECTION.
  PERFORM f_run.

*&---------------------------------------------------------------------*
*&      Form  F_INIT
*&---------------------------------------------------------------------*
FORM f_init.
  report = VALUE #( ).

  b1_title = 'Selection Criteria'.
  %_s_type_%_app_%-text = 'Object Type'.
  %_s_name_%_app_%-text = 'Object Name'.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_RUN
*&---------------------------------------------------------------------*
FORM f_run.
  PERFORM f_fetch.
  PERFORM f_display.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_FETCH
*&---------------------------------------------------------------------*
FORM f_fetch.
  report-data = CORRESPONDING #( lcl_model=>get_objects( it_object_types = s_type[]
                                                         it_object_names = s_name[] ) ).
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_DISPLAY
*&---------------------------------------------------------------------*
FORM f_display.
  PERFORM f_build_glay CHANGING report-glay.

  PERFORM f_build_fcat USING    report-data
                       CHANGING report-fcat.

  PERFORM f_build_layo CHANGING report-layo.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'F_SET_PF_STATUS'
      i_callback_user_command  = 'F_USER_COMMAND'
      i_callback_top_of_page   = 'F_TOP_OF_PAGE'
      i_grid_settings          = report-glay
      is_layout_lvc            = report-layo
      it_fieldcat_lvc          = report-fcat
      i_default                = abap_true
      i_save                   = abap_true
    TABLES
      t_outtab                 = report-data
    EXCEPTIONS
      OTHERS                   = 1.
  IF sy-subrc <> 0.
    MESSAGE e001(00) WITH 'Errors occurred during ALV display!'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_BUILD_FCAT
*&---------------------------------------------------------------------*
FORM f_build_fcat USING    it_tabl TYPE STANDARD TABLE
                  CHANGING ct_fcat TYPE lvc_t_fcat.

  DATA lo_tabledescr  TYPE REF TO cl_abap_tabledescr.
  DATA lo_structdescr TYPE REF TO cl_abap_structdescr.
  DATA lo_datadescr   TYPE REF TO cl_abap_datadescr.
  DATA lo_elemdescr   TYPE REF TO cl_abap_elemdescr.
  DATA ls_ddicfield   TYPE dfies.

  lo_tabledescr ?= cl_abap_tabledescr=>describe_by_data( it_tabl ).
  lo_structdescr ?= lo_tabledescr->get_table_line_type( ).

  LOOP AT lo_structdescr->components ASSIGNING FIELD-SYMBOL(<fs_component>).
    lo_datadescr = lo_structdescr->get_component_type( <fs_component>-name ).
    IF        lo_datadescr->kind <> cl_abap_typedescr=>kind_elem
       OR NOT lo_datadescr->is_ddic_type( ).
      CONTINUE.
    ENDIF.
    lo_elemdescr ?= lo_datadescr.
    ls_ddicfield = lo_elemdescr->get_ddic_field( ).
    APPEND INITIAL LINE TO ct_fcat ASSIGNING FIELD-SYMBOL(<fs_fcat>).
    MOVE-CORRESPONDING ls_ddicfield TO <fs_fcat>.
    <fs_fcat>-fieldname = <fs_component>-name.
  ENDLOOP.

  LOOP AT ct_fcat ASSIGNING <fs_fcat>.
    CASE <fs_fcat>-fieldname.
      WHEN 'CHECKBOX'.
        <fs_fcat>-edit     = abap_true.
        <fs_fcat>-checkbox = abap_true.
    ENDCASE.
    <fs_fcat>-colddictxt = 'L'.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_BUILD_LAYO
*&---------------------------------------------------------------------*
FORM f_build_layo CHANGING cs_layo TYPE lvc_s_layo.
  cs_layo-col_opt    = abap_true.
  cs_layo-cwidth_opt = abap_true.
  cs_layo-zebra      = abap_true.
  cs_layo-no_rowmark = abap_true.
  cs_layo-no_rowins  = abap_true.
  cs_layo-no_rowmove = abap_true.
  cs_layo-no_toolbar = abap_true.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_BUILD_GLAY
*&---------------------------------------------------------------------*
FORM f_build_glay CHANGING cs_glay TYPE lvc_s_glay.
  cs_glay-edt_cll_cb = abap_true.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_USER_COMMAND
*&---------------------------------------------------------------------*
FORM f_user_command USING iv_ucomm  TYPE sy-ucomm
                          is_selfld TYPE slis_selfield.

  CASE iv_ucomm.
    WHEN '&BACK' OR '&CANC' OR '&EXIT'.
      SET SCREEN 0.
    WHEN '&SEL_ALL'.
      MODIFY report-data
             FROM VALUE #( checkbox = abap_true )
             TRANSPORTING checkbox
             WHERE checkbox = abap_false.
    WHEN '&DESEL_ALL'.
      MODIFY report-data
             FROM VALUE #( checkbox = abap_false )
             TRANSPORTING checkbox
             WHERE checkbox = abap_true.
    WHEN '&TRANS'.
      lcl_model=>transport_objects( EXPORTING  it_objects      = CORRESPONDING #( VALUE tt_data( FOR data IN report-data WHERE ( checkbox  = abap_true ) ( data ) ) )
                                    RECEIVING  rv_request      = DATA(lv_request)
                                    EXCEPTIONS transport_error = 1
                                               OTHERS          = 2 ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ELSE.
        MESSAGE |Objects assigned to request { lv_request }.| TYPE 'S'.
      ENDIF.

    WHEN '&IC1'.
  ENDCASE.

  is_selfld-refresh    = abap_true.
  is_selfld-row_stable = abap_true.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_SET_PF_STATUS
*&---------------------------------------------------------------------*
FORM f_set_pf_status USING it_excl TYPE slis_t_extab.
  SET PF-STATUS 'STANDARD' EXCLUDING it_excl.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM f_top_of_page.
  DATA lt_heading TYPE slis_t_listheader.

  SELECT SINGLE name_text FROM v_usr_name INTO @DATA(lv_name_text) WHERE bname = @sy-uname.

  lt_heading = VALUE #( ( typ = 'H' info = sy-title )
                        ( typ = 'S' info = |User Name    : { lv_name_text }| )
                        ( typ = 'S' info = |Date / Time  : { sy-datum DATE = USER } / { sy-uzeit TIME = USER }| )
                        ( typ = 'S' info = |Record Count : { lines( report-data ) }| ) ).

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = lt_heading.
ENDFORM.
