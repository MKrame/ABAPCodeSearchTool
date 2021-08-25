
CLASS lcl_event_handler IMPLEMENTATION.
  METHOD on_link_click.
    CASE column.
      WHEN 'LINNO'.
        DATA: l_operation(5).
        READ TABLE g_tab_lines[] INTO DATA(ls_tab_lines) INDEX row.


* Check object name: object name is filled via HIDE and
* event AT LINE-SELECTION
        IF ls_tab_lines-progname IS INITIAL.
          EXIT.
        ENDIF.

* Set edit mode
        l_operation = 'EDIT'.
        IF p_edit <> con_true.
          l_operation = 'SHOW'.
        ENDIF.

* Navigation to current object
        CALL FUNCTION 'RS_TOOL_ACCESS'
          EXPORTING
            operation           = l_operation
            object_name         = ls_tab_lines-progname
            object_type         = 'REPS'
*           ENCLOSING_OBJECT    =
*           POSITION            = con_false
            position            = ls_tab_lines-linno
*           DEVCLASS            =
*           INCLUDE             =
*           VERSION             = con_false
*           MONITOR_ACTIVATION  = con_true
*           WB_MANAGER          =
*           IN_NEW_WINDOW       =
*           WITH_OBJECTLIST     = con_false
* IMPORTING
*           NEW_NAME            =
*           WB_TODO_REQUEST     =
* TABLES
*           OBJLIST             =
          EXCEPTIONS
            not_executed        = 0
            invalid_object_type = 0
            OTHERS              = 0.
      WHEN ''.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_source_scan IMPLEMENTATION.
  METHOD start.
* Initialization
    REFRESH g_tab_lines.
    CALL FUNCTION 'TRINT_OBJECT_TABLE'
      EXPORTING
        iv_complete  = 'X'
      TABLES
        tt_types_out = gt_obj_types.

* Process data
    process_devc( ).

* Display scan result data
    scan_result_display( ).

  ENDMETHOD.

  METHOD process_devc.
    DATA: l_tab_tadir       TYPE TABLE OF tadir,
          l_str_tadir       TYPE tadir,
          l_cnt             TYPE i,
          l_cnt_str(10)     TYPE c,
          l_tabix           TYPE i,
          l_flg_process_tmp TYPE xfeld,
          l_answer          TYPE c,
          l_popuptext(200)  TYPE c,
          l_devclass        TYPE devclass.
* Get all packages matching with selection criteria
    REFRESH l_tab_tadir.
    SELECT * FROM tadir INTO TABLE l_tab_tadir
      WHERE pgmid    = 'R3TR' AND
            object   = 'DEVC' AND
            devclass IN s_devc.                       "#EC CI_SGLSELECT

* Ignore invalid TADIR entries.
    DELETE l_tab_tadir WHERE obj_name IS INITIAL.

    DESCRIBE TABLE l_tab_tadir LINES l_cnt.

* Check if local package $TMP in selection criteria
    CLEAR l_flg_process_tmp.
    IF c_devc_tmp IN s_devc.
      l_flg_process_tmp = con_true.
      l_cnt = l_cnt + 1.
    ENDIF.

* Check count of selected packages
    IF l_cnt > 10 AND sy-batch <> con_true.
      l_cnt_str = l_cnt.
      CONDENSE l_cnt_str.

      CONCATENATE 'Es wurden folgende Anzahl von Paketen selektiert:'(004)
                   l_cnt_str 'Scan-Vorgang trotzdem starten?'(005)
                  INTO l_popuptext SEPARATED BY space.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Sicherheitsabfrage'(006)
          text_question         = l_popuptext
          default_button        = '2'
          display_cancel_button = con_false
        IMPORTING
          answer                = l_answer.
      IF l_answer <> '1'.
        EXIT.
      ENDIF.
    ENDIF.

* Process packages
    l_tabix = 0.
    LOOP AT l_tab_tadir INTO l_str_tadir.
      l_tabix = l_tabix + 1.
      l_devclass = l_str_tadir-obj_name.
      scan_devc( i_devclass = l_devclass
                 i_tabix = l_tabix
                 i_cnt = l_cnt
                 i_lrng = p_lrng ).
    ENDLOOP.

* Process local package $TMP
    IF l_flg_process_tmp = con_true.
      l_tabix = l_tabix + 1.
      scan_devc( i_devclass = c_devc_tmp
                 i_tabix = l_tabix
                 i_cnt = l_cnt
                 i_lrng = p_lrng ).
    ENDIF.

  ENDMETHOD.


  METHOD scan_devc.
* Scan sources of current package
    IF p_prog = con_true.
      DATA(lo_prog_scan) = NEW lcl_scan_program( ).
      lo_prog_scan->scan_object(
        EXPORTING
          i_devclass = i_devclass
          i_tabix    = i_tabix
          i_cnt      = i_cnt
          i_lrng     = i_lrng
      ).
    ENDIF.
    IF p_fugr = con_true.
      DATA(lo_fugr_scan) = NEW lcl_scan_functiongroup( ).
      lo_fugr_scan->scan_object(
          EXPORTING
            i_devclass = i_devclass
            i_tabix    = i_tabix
            i_cnt      = i_cnt
            i_lrng     = i_lrng
        ).
    ENDIF.
    IF p_cinc = con_true.
      DATA(lo_class_scan) = NEW lcl_scan_class( ).
      lo_class_scan->scan_object(
          EXPORTING
            i_devclass = i_devclass
            i_tabix    = i_tabix
            i_cnt      = i_cnt
            i_lrng     = i_lrng
        ).
    ENDIF.

    IF p_enh = con_true.
      DATA(lo_enh_scan) = NEW lcl_scan_enhancement( ).
      lo_enh_scan->scan_object(
          EXPORTING
            i_devclass = i_devclass
            i_tabix    = i_tabix
            i_cnt      = i_cnt
            i_lrng     = i_lrng
        ).
    ENDIF.

    IF p_scrip = con_true.
      DATA(lo_scrip_scan) = NEW lcl_scan_sapscript( ).
      lo_scrip_scan->scan_object(
          EXPORTING
            i_devclass = i_devclass
            i_tabix    = i_tabix
            i_cnt      = i_cnt
            i_lrng     = i_lrng
        ).
    ENDIF.

    IF p_smart = con_true.
      DATA(lo_smart_scan) = NEW lcl_scan_smartform( ).
      lo_smart_scan->scan_object(
          EXPORTING
            i_devclass = i_devclass
            i_tabix    = i_tabix
            i_cnt      = i_cnt
            i_lrng     = i_lrng
        ).
    ENDIF.

    IF p_adobe = con_true.
      DATA(lo_adobe_scan) = NEW lcl_scan_adobe( ).
      lo_adobe_scan->scan_object(
          EXPORTING
            i_devclass = i_devclass
            i_tabix    = i_tabix
            i_cnt      = i_cnt
            i_lrng     = i_lrng
        ).
    ENDIF.
  ENDMETHOD.


  METHOD scan_result_display.
    DATA: lo_alv          TYPE REF TO cl_salv_table,
          lo_column_table TYPE REF TO cl_salv_column_table.

* Initialzation
    CLEAR: lo_alv.

* Create alv

    cl_salv_table=>factory(
*  EXPORTING
*    list_display   = IF_SALV_C_BOOL_SAP=>FALSE    " ALV Displayed in List Mode
*    r_container    =     " Abstract Container for GUI Controls
*    container_name =
      IMPORTING
        r_salv_table   = lo_alv     " Basis Class Simple ALV Tables
      CHANGING
        t_table        = g_tab_lines[]
    ).

* Settings..
    lo_alv->get_display_settings( )->set_striped_pattern( abap_true ).
    lo_alv->get_columns( )->set_optimize( abap_true ).

    lo_alv->get_functions( )->set_default( abap_true ).

    lo_alv->get_sorts( )->clear( ).
    lo_alv->get_sorts( )->add_sort(
      EXPORTING
        columnname         = 'DEVCLASS'    " ALV Control: Field Name of Internal Table Field
        position           = 1
        sequence           = cl_salv_sort=>if_salv_c_sort~sort_up "Ascending    " Sort Sequence
    ).
    lo_alv->get_sorts( )->add_sort(
      EXPORTING
        columnname         = 'PROGTYPE'    " ALV Control: Field Name of Internal Table Field
        position           = 2
        sequence           = cl_salv_sort=>if_salv_c_sort~sort_up "Ascending    " Sort Sequence
    ).
    lo_alv->get_sorts( )->add_sort(
      EXPORTING
        columnname         = 'PROGNAME'    " ALV Control: Field Name of Internal Table Field
        position           = 3
        sequence           = cl_salv_sort=>if_salv_c_sort~sort_up "Ascending    " Sort Sequence
    ).

    lo_alv->get_columns( )->get_column( 'LINNO' )->set_alignment( if_salv_c_alignment=>right ).
    lo_column_table ?= lo_alv->get_columns( )->get_column( 'LINNO' ).
    lo_column_table->set_cell_type( if_salv_c_cell_type=>hotspot ).

    lo_alv->get_columns( )->get_column( 'LINE' )->set_zero( abap_true ).
    lo_column_table ?= lo_alv->get_columns( )->get_column( 'LINE' ).
    lo_column_table->set_color( VALUE lvc_s_colo( col = 5 int = 0 inv =  0 ) ).

    SET HANDLER lcl_event_handler=>on_link_click FOR lo_alv->get_event( ).

    lo_alv->display( ).
  ENDMETHOD.

ENDCLASS.
CLASS lcl_object_scanner IMPLEMENTATION.
  METHOD scan_object.
  ENDMETHOD.
  METHOD scan_prog.
    DATA: l_str_source TYPE t_abapsource_long,
*        l_line         TYPE sytabix,
*        l_out_progname TYPE xfeld,   "EC NEEDED
          l_flg_found  TYPE xfeld,
          l_flg_write  TYPE xfeld,
          l_cnt_line   TYPE i,
*        l_modulo       TYPE i,
          l_str_lines  TYPE t_str_lines.

* Initialization
*  CLEAR l_out_progname.
    CLEAR l_flg_found.
    g_line_object = i_rep_name.
    l_cnt_line = 1000.

    CLEAR l_str_lines.
    l_str_lines-devclass = i_devclass.
    l_str_lines-progname = i_rep_name.
    l_str_lines-form = i_formname.

    READ TABLE gt_obj_types INTO DATA(ls_obj_types) WITH KEY pgmid = i_tadir-pgmid object = i_tadir-object.
    IF sy-subrc = 0.
      l_str_lines-progtype = ls_obj_types-text.
    ENDIF.

* Search source for selection criteria
    LOOP AT c_tab_source INTO l_str_source.
      g_line_number = sy-tabix.
      CLEAR l_flg_write.
      IF l_str_source-line CS p_strg1 AND
         ( p_strg2 IS INITIAL OR l_str_source-line CS p_strg2 ).
        IF ( p_excl1 IS INITIAL OR
             NOT l_str_source-line CS p_excl1 ) AND
           ( p_excl2 IS INITIAL OR
             NOT l_str_source-line CS p_excl2 ) AND
           ( p_excl3 IS INITIAL OR
             NOT l_str_source-line CS p_excl3 ) AND
           ( p_excomm IS INITIAL OR
             l_str_source-line(1) <> '*' ).
          l_flg_write = con_true.
          l_cnt_line  = 0.
        ENDIF.
      ENDIF.

      IF l_flg_write = con_true OR l_cnt_line < i_cnt_line.
        l_cnt_line  = l_cnt_line + 1.
        l_flg_found = con_true.
        l_str_lines-linno = g_line_number.
        l_str_lines-line  = l_str_source-line.
        APPEND l_str_lines TO g_tab_lines.
      ENDIF.

    ENDLOOP.

* No hits found
    IF p_nohits = con_true AND l_flg_found IS INITIAL.

      l_str_lines-linno = 1.
      l_str_lines-line  = 'Keine Treffer'(014).
      APPEND l_str_lines TO g_tab_lines.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
CLASS lcl_scan_program IMPLEMENTATION.
  METHOD scan_object.
    DATA: l_tab_tadir     TYPE TABLE OF tadir,
          l_str_tadir     TYPE tadir,
          l_cnt           TYPE i,
          l_cnt_str(10)   TYPE c,
          l_idx_devc(10)  TYPE c,
          l_cnt_devc(10)  TYPE c,
          l_aux_devc(20)  TYPE c,
          l_percentage    TYPE p,
          l_tabix_str(10) TYPE c,
          l_rep_name      TYPE sobj_name,
          l_tab_source    TYPE t_tab_long_lines,    "CB
          l_text          TYPE itex132.

* Initialization
    l_idx_devc = i_tabix.
    l_cnt_devc = i_cnt.
    CONCATENATE l_idx_devc '/' l_cnt_devc INTO l_aux_devc.
    CONDENSE l_aux_devc.

* Get programs of current package
    REFRESH l_tab_tadir.
    IF i_devclass <> c_devc_tmp.
      SELECT * FROM tadir INTO TABLE l_tab_tadir
        WHERE pgmid    = 'R3TR' AND
              object   = 'PROG' AND
              devclass = i_devclass AND
              obj_name IN s_rest.                     "#EC CI_SGLSELECT

    ELSE.
      SELECT * FROM tadir INTO TABLE l_tab_tadir
        WHERE pgmid    = 'R3TR' AND
              object   = 'PROG' AND
              devclass = i_devclass AND
              author   = sy-uname AND
              obj_name IN s_rest.                     "#EC CI_SGLSELECT
    ENDIF.

* Ignore invalid TADIR entries.
    DELETE l_tab_tadir WHERE obj_name IS INITIAL.

* Write count of programs into list
    DESCRIBE TABLE l_tab_tadir LINES l_cnt.
    IF l_cnt = 0.
      EXIT.
    ENDIF.

* Process all program sources
    l_cnt_str = l_cnt.
    CONDENSE l_cnt_str.
    LOOP AT l_tab_tadir INTO l_str_tadir.
      l_tabix_str = sy-tabix.
      CONDENSE l_tabix_str.

*   Display progress indicator
      l_percentage = 100 * ( sy-tabix / l_cnt ).
      CONCATENATE 'Scanne Paket'(008) i_devclass l_aux_devc
                  '(' 'Report'(009) l_tabix_str 'von'(010) l_cnt_str ')'
                  INTO l_text SEPARATED BY space.
      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          percentage = l_percentage
          text       = l_text.

*   Read program source and search for specified strings
*    write: / l_str_tadir-obj_name.
      l_rep_name = l_str_tadir-obj_name.
      REFRESH l_tab_source.
      READ REPORT l_rep_name INTO l_tab_source.
      IF sy-subrc = 0.
        scan_prog( EXPORTING i_devclass = i_devclass
                             i_rep_name = l_rep_name
                             i_cnt_line = i_lrng
                             i_tadir    = l_str_tadir
                          CHANGING c_tab_source = l_tab_source ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.

CLASS lcl_scan_functiongroup IMPLEMENTATION.
  METHOD scan_object.  DATA: l_tab_tadir     TYPE TABLE OF tadir,
  l_str_tadir     TYPE tadir,
  l_tab_e071      TYPE TABLE OF e071,
  l_str_e071      TYPE e071,
  l_str_tfdir     TYPE tfdir,
  l_cnt           TYPE i,
  l_cnt_str(10)   TYPE c,
  l_idx_devc(10)  TYPE c,
  l_cnt_devc(10)  TYPE c,
  l_aux_devc(20)  TYPE c,
  l_percentage    TYPE p,
  l_tabix_str(10) TYPE c,
  l_rep_name      TYPE sobj_name,
  l_tab_source    TYPE TABLE OF t_abapsource_long,       "CB
  l_text          TYPE itex132.

* Initialization
  l_idx_devc = i_tabix.
  l_cnt_devc = i_cnt.
  CONCATENATE l_idx_devc '/' l_cnt_devc INTO l_aux_devc.
  CONDENSE l_aux_devc.

* Get function pools of current package
  REFRESH l_tab_tadir.
  IF i_devclass <> c_devc_tmp.
    SELECT * FROM tadir INTO TABLE l_tab_tadir
      WHERE pgmid    = 'R3TR' AND
            object   = 'FUGR' AND
            devclass = i_devclass AND
            obj_name IN s_rest.                       "#EC CI_SGLSELECT
  ELSE.
    SELECT * FROM tadir INTO TABLE l_tab_tadir
      WHERE pgmid    = 'R3TR' AND
            object   = 'FUGR' AND
            devclass = i_devclass AND
            author   = sy-uname AND
            obj_name IN s_rest.                       "#EC CI_SGLSELECT
  ENDIF.

* Ignore invalid TADIR entries.
  DELETE l_tab_tadir WHERE obj_name IS INITIAL.

* Write count of function pools into list
  DESCRIBE TABLE l_tab_tadir LINES l_cnt.
  IF l_cnt = 0.
    EXIT.
  ENDIF.

* Process all function pools
  l_cnt_str = l_cnt.
  CONDENSE l_cnt_str.
  LOOP AT l_tab_tadir INTO l_str_tadir.
    l_tabix_str = sy-tabix.
    CONDENSE l_tabix_str.

*   Display progress indicator
    l_percentage = 100 * ( sy-tabix / l_cnt ).
    CONCATENATE 'Scanne Paket'(008) i_devclass l_aux_devc
                '(' 'FuGr'(011) l_tabix_str 'von'(010) l_cnt_str ')'
                INTO l_text SEPARATED BY space.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = l_percentage
        text       = l_text.

*   Get function pool objects
*    write: / l_str_tadir-obj_name.
    l_str_e071-pgmid    = l_str_tadir-pgmid.
    l_str_e071-object   = l_str_tadir-object.
    l_str_e071-obj_name = l_str_tadir-obj_name.
    REFRESH l_tab_e071.
    CALL FUNCTION 'STOR_RESOLVE_FUGR'
      EXPORTING
        is_e071 = l_str_e071
      TABLES
        tt_e071 = l_tab_e071
      EXCEPTIONS
        OTHERS  = 0.

*   Read basis program sources and search for specified strings
    LOOP AT l_tab_e071 INTO l_str_e071 WHERE object = 'REPO' .
      l_rep_name = l_str_e071-obj_name.
      REFRESH l_tab_source.
      READ REPORT l_rep_name INTO l_tab_source.
      IF sy-subrc = 0.
        scan_prog( EXPORTING i_devclass = i_devclass
                             i_rep_name = l_rep_name
                             i_cnt_line = i_lrng
                             i_tadir    = l_str_tadir
                          CHANGING c_tab_source = l_tab_source ).
      ENDIF.
    ENDLOOP .

* (A) Keine generierten Dialoge?!? Das sollte man evtl. optional
*     anbieten (Zeitpunkt-Routinen!)
*   Read function module sources and search for specified strings
    LOOP AT l_tab_e071 INTO l_str_e071 WHERE object = 'FUNC' .
      IF l_str_e071-obj_name(4) = 'VIEW'. "Keine gen. Dialoge
        CONTINUE.
      ENDIF.
      SELECT SINGLE * FROM tfdir INTO l_str_tfdir
        WHERE funcname = l_str_e071-obj_name.         "#EC CI_SGLSELECT
      IF sy-subrc = 0.
        CONCATENATE l_str_tfdir-pname 'U' l_str_tfdir-include
                    INTO l_rep_name.
        REPLACE 'SAPL' WITH 'L' INTO l_rep_name.
        REFRESH l_tab_source.
        READ REPORT l_rep_name INTO l_tab_source.
        IF sy-subrc = 0.
          scan_prog( EXPORTING i_devclass = i_devclass
                               i_rep_name = l_rep_name
                               i_cnt_line = i_lrng
                             i_tadir    = l_str_tadir
                            CHANGING c_tab_source = l_tab_source ).
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDLOOP .
ENDMETHOD.
ENDCLASS.

CLASS lcl_scan_class IMPLEMENTATION.
  METHOD scan_object.  DATA: l_tab_tadir     TYPE TABLE OF tadir,
  l_str_tadir     TYPE tadir,
  l_str_e071      TYPE e071,
  l_cnt           TYPE i,
  l_cnt_str(10)   TYPE c,
  l_idx_devc(10)  TYPE c,
  l_cnt_devc(10)  TYPE c,
  l_aux_devc(20)  TYPE c,
  l_percentage    TYPE p,
  l_tabix_str(10) TYPE c,
  l_rep_name      TYPE sobj_name,
  l_tab_source    TYPE TABLE OF t_abapsource_long,
  l_text          TYPE itex132,
  l_tab_trdir     TYPE STANDARD TABLE OF trdir,
  l_str_trdir     LIKE LINE OF l_tab_trdir,
  l_tab_selopt    TYPE STANDARD TABLE OF rsdsselopt,
  l_str_selopt    LIKE LINE OF l_tab_selopt.

* Initialization
  l_idx_devc = i_tabix.
  l_cnt_devc = i_cnt.
  CONCATENATE l_idx_devc '/' l_cnt_devc INTO l_aux_devc.
  CONDENSE l_aux_devc.

* Get classes of current package
  REFRESH l_tab_tadir.
  IF i_devclass <> c_devc_tmp.
    SELECT * FROM tadir INTO TABLE l_tab_tadir
      WHERE pgmid    = 'R3TR' AND
            object   = 'CLAS' AND
            devclass = i_devclass AND
            obj_name IN s_rest.                       "#EC CI_SGLSELECT
  ELSE.
    SELECT * FROM tadir INTO TABLE l_tab_tadir
      WHERE pgmid    = 'R3TR' AND
            object   = 'CLAS' AND
            devclass = i_devclass AND
            author   = sy-uname AND
            obj_name IN s_rest.                       "#EC CI_SGLSELECT
  ENDIF.

* Ignore invalid TADIR entries.
  DELETE l_tab_tadir WHERE obj_name IS INITIAL.

* Write count of function pools into list
  DESCRIBE TABLE l_tab_tadir LINES l_cnt.
  IF l_cnt = 0.
    EXIT.
  ENDIF.

* Process all function pools
  l_cnt_str = l_cnt.
  CONDENSE l_cnt_str.
  LOOP AT l_tab_tadir INTO l_str_tadir.
    l_tabix_str = sy-tabix.
    CONDENSE l_tabix_str.

*   Display progress indicator
    l_percentage = 100 * ( sy-tabix / l_cnt ).
    CONCATENATE 'Scanne Paket'(008) i_devclass l_aux_devc
                '(' 'Klasse'(012) l_tabix_str 'von'(010) l_cnt_str ')'
                INTO l_text SEPARATED BY space.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = l_percentage
        text       = l_text.

* get includes for current class
    REFRESH l_tab_selopt.
    l_str_selopt-sign = 'I'.
    l_str_selopt-option = 'CP'.
    CONCATENATE l_str_tadir-obj_name '*' INTO
     l_str_selopt-low.
    APPEND l_str_selopt TO l_tab_selopt.

    SELECT * FROM trdir INTO TABLE l_tab_trdir
              WHERE name IN l_tab_selopt.             "#EC CI_SGLSELECT


    LOOP AT l_tab_trdir INTO l_str_trdir.
      l_rep_name = l_str_e071-obj_name.
      REFRESH l_tab_source.
      l_rep_name = l_str_trdir-name.
      READ REPORT l_rep_name INTO l_tab_source.

      IF sy-subrc = 0.
        scan_prog( EXPORTING i_devclass = i_devclass
                             i_rep_name = l_rep_name
                             i_cnt_line = i_lrng
                             i_tadir    = l_str_tadir
                          CHANGING c_tab_source = l_tab_source ).
      ELSE.
        FORMAT COLOR COL_NEGATIVE.
        WRITE: / 'Report'(009), l_rep_name, 'nicht gefunden!'(013).
      ENDIF.
    ENDLOOP.

  ENDLOOP .

ENDMETHOD.
ENDCLASS.

CLASS lcl_scan_enhancement IMPLEMENTATION.
  METHOD scan_object.
    DATA: l_tab_tadir     TYPE TABLE OF tadir,
          l_str_tadir     TYPE tadir,
          l_cnt           TYPE i,
          l_cnt_str(10)   TYPE c,
          l_idx_devc(10)  TYPE c,
          l_cnt_devc(10)  TYPE c,
          l_aux_devc(20)  TYPE c,
          l_percentage    TYPE p,
          l_tabix_str(10) TYPE c,
          l_incl          TYPE sobj_name,
          l_rep_name      TYPE sobj_name,
          l_tab_source    TYPE t_tab_long_lines,    "CB
          l_text          TYPE itex132.

* Initialization
    l_idx_devc = i_tabix.
    l_cnt_devc = i_cnt.
    CONCATENATE l_idx_devc '/' l_cnt_devc INTO l_aux_devc.
    CONDENSE l_aux_devc.

* Get programs of current package
    REFRESH l_tab_tadir.
    IF i_devclass <> c_devc_tmp.
      SELECT * FROM tadir INTO TABLE l_tab_tadir
        WHERE pgmid    = 'R3TR' AND
              object   = 'ENHO' AND
              devclass = i_devclass AND
              obj_name IN s_rest.                     "#EC CI_SGLSELECT

    ELSE.
      SELECT * FROM tadir INTO TABLE l_tab_tadir
        WHERE pgmid    = 'R3TR' AND
              object   = 'ENHO' AND
              devclass = i_devclass AND
              author   = sy-uname AND
              obj_name IN s_rest.                     "#EC CI_SGLSELECT
    ENDIF.

* Ignore invalid TADIR entries.
    DELETE l_tab_tadir WHERE obj_name IS INITIAL.

* Write count of programs into list
    DESCRIBE TABLE l_tab_tadir LINES l_cnt.
    IF l_cnt = 0.
      EXIT.
    ENDIF.

* Process all program sources
    l_cnt_str = l_cnt.
    CONDENSE l_cnt_str.
    LOOP AT l_tab_tadir INTO l_str_tadir.
      l_tabix_str = sy-tabix.
      CONDENSE l_tabix_str.

*   Display progress indicator
      l_percentage = 100 * ( sy-tabix / l_cnt ).
      CONCATENATE 'Scanne Paket'(008) i_devclass l_aux_devc
                  '(' 'Report'(009) l_tabix_str 'von'(010) l_cnt_str ')'
                  INTO l_text SEPARATED BY space.
      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          percentage = l_percentage
          text       = l_text.

*   Read program source and search for specified strings
*    write: / l_str_tadir-obj_name.

      SELECT SINGLE enhinclude FROM enhincinx
        INTO l_incl WHERE enhname = l_str_tadir-obj_name.

      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      l_rep_name = l_incl.
      REFRESH l_tab_source.
      READ REPORT l_rep_name INTO l_tab_source.
      IF sy-subrc = 0.
        scan_prog( EXPORTING i_devclass = i_devclass
                             i_rep_name = l_rep_name
                             i_cnt_line = i_lrng
                             i_tadir    = l_str_tadir
                          CHANGING c_tab_source = l_tab_source ).
      ENDIF.

    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_scan_sapscript IMPLEMENTATION.
  METHOD scan_object.  DATA: l_tab_tadir     TYPE TABLE OF tadir,
  l_str_tadir     TYPE tadir,
  l_cnt           TYPE i,
  l_cnt_str(10)   TYPE c,
  l_idx_devc(10)  TYPE c,
  l_cnt_devc(10)  TYPE c,
  l_aux_devc(20)  TYPE c,
  l_percentage    TYPE p,
  l_tabix_str(10) TYPE c,
  l_incl          TYPE sobj_name,
  l_rep_name      TYPE sobj_name,
  l_tab_source    TYPE t_tab_long_lines,    "CB
  l_text          TYPE itex132.

  DATA lt_data TYPE TABLE OF tline.
  DATA ls_data TYPE tline.
  DATA l_element TYPE c LENGTH 10.

  DATA: l_str_source TYPE t_abapsource_long,
        l_flg_found  TYPE xfeld,
        l_flg_write  TYPE xfeld,
        l_cnt_line   TYPE i,
        l_str_lines  TYPE t_str_lines.

* Initialization
  l_idx_devc = i_tabix.
  l_cnt_devc = i_cnt.
  CONCATENATE l_idx_devc '/' l_cnt_devc INTO l_aux_devc.
  CONDENSE l_aux_devc.

* Get programs of current package
  REFRESH l_tab_tadir.
  IF i_devclass <> c_devc_tmp.
    SELECT * FROM tadir INTO TABLE l_tab_tadir
      WHERE pgmid    = 'R3TR' AND
            object   = 'FORM' AND
            devclass = i_devclass AND
            obj_name IN s_rest.                       "#EC CI_SGLSELECT

  ELSE.
    SELECT * FROM tadir INTO TABLE l_tab_tadir
      WHERE pgmid    = 'R3TR' AND
            object   = 'FORM' AND
            devclass = i_devclass AND
            author   = sy-uname AND
            obj_name IN s_rest.                       "#EC CI_SGLSELECT
  ENDIF.

* Ignore invalid TADIR entries.
  DELETE l_tab_tadir WHERE obj_name IS INITIAL.

* Write count of programs into list
  DESCRIBE TABLE l_tab_tadir LINES l_cnt.
  IF l_cnt = 0.
    EXIT.
  ENDIF.

* Process all program sources
  l_cnt_str = l_cnt.
  CONDENSE l_cnt_str.
  LOOP AT l_tab_tadir INTO l_str_tadir.
    l_tabix_str = sy-tabix.
    CONDENSE l_tabix_str.

*   Display progress indicator
    l_percentage = 100 * ( sy-tabix / l_cnt ).
    CONCATENATE 'Scanne Paket'(008) i_devclass l_aux_devc
                '(' 'Report'(009) l_tabix_str 'von'(010) l_cnt_str ')'
                INTO l_text SEPARATED BY space.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = l_percentage
        text       = l_text.


    REFRESH lt_data.

    DATA l_form TYPE itcta-tdform.

    l_form = l_str_tadir-obj_name.

    CALL FUNCTION 'READ_FORM'
      EXPORTING
*       CLIENT     = SY-MANDT
        form       = l_form
*       LANGUAGE   = SY-LANGU
*       OLANGUAGE  = ' '
*       OSTATUS    = ' '
*       STATUS     = ' '
*       THROUGHCLIENT          = ' '
*       READ_ONLY_HEADER       = ' '
*       THROUGHLANGUAGE        = ' '
* IMPORTING
*       FORM_HEADER            =
*       FOUND      =
*       HEADER     =
*       OLANGUAGE  =
      TABLES
        form_lines = lt_data
*       PAGES      =
*       PAGE_WINDOWS           =
*       PARAGRAPHS =
*       STRINGS    =
*       TABS       =
*       WINDOWS    =
      .

* Initialization
*  CLEAR l_out_progname.
    CLEAR l_flg_found.
    g_line_object = l_form.
    l_cnt_line = 1000.

    CLEAR l_str_lines.
    l_str_lines-devclass = i_devclass.


* Search source for selection criteria
    LOOP AT lt_data INTO l_str_source.
      g_line_number = sy-tabix.
      CLEAR l_flg_write.
      IF l_str_source-line(2) = '/W'.
        l_element = l_str_source-line+2(10).
      ENDIF.
      IF l_str_source-line CS p_strg1 AND
         ( p_strg2 IS INITIAL OR l_str_source-line CS p_strg2 ).
        IF ( p_excl1 IS INITIAL OR
             NOT l_str_source-line CS p_excl1 ) AND
           ( p_excl2 IS INITIAL OR
             NOT l_str_source-line CS p_excl2 ) AND
           ( p_excl3 IS INITIAL OR
             NOT l_str_source-line CS p_excl3 ) AND
           ( p_excomm IS INITIAL OR
             l_str_source-line(1) <> '*' ).
          l_flg_write = con_true.
          l_cnt_line  = 0.
        ENDIF.
      ENDIF.

      IF l_flg_write = con_true OR l_cnt_line < i_lrng.
        l_cnt_line = l_cnt_line + 1.
        l_flg_found = con_true.
        l_str_lines-linno = g_line_number.
        l_str_lines-line = l_str_source-line.
        l_str_lines-progname = l_form.
        l_str_lines-form = l_element.
        APPEND l_str_lines TO g_tab_lines.
      ENDIF.
    ENDLOOP. " No hits found
    IF p_nohits = con_true AND l_flg_found IS INITIAL.
      l_str_lines-linno = 1. l_str_lines-line = 'Keine Treffer'(014).
      APPEND l_str_lines TO g_tab_lines.
    ENDIF.
  ENDLOOP.
ENDMETHOD.
ENDCLASS.

CLASS lcl_scan_smartform IMPLEMENTATION.
  METHOD scan_object.
    DATA: l_tab_tadir     TYPE TABLE OF tadir,
          l_str_tadir     TYPE tadir,
          l_tab_e071      TYPE TABLE OF e071,
          l_str_e071      TYPE e071,
          l_str_tfdir     TYPE tfdir,
          l_cnt           TYPE i,
          l_cnt_str(10)   TYPE c,
          l_idx_devc(10)  TYPE c,
          l_cnt_devc(10)  TYPE c,
          l_aux_devc(20)  TYPE c,
          l_percentage    TYPE p,
          l_tabix_str(10) TYPE c,
          l_rep_name      TYPE sobj_name,
          l_tab_source    TYPE TABLE OF t_abapsource_long,       "CB
          l_text          TYPE itex132.

    DATA l_form TYPE  tdsfname.
    DATA l_formname TYPE  string.
    DATA l_fm_name TYPE  rs38l_fnam.

* Initialization
    l_idx_devc = i_tabix.
    l_cnt_devc = i_cnt.
    CONCATENATE l_idx_devc '/' l_cnt_devc INTO l_aux_devc.
    CONDENSE l_aux_devc.

* Get function pools of current package
    REFRESH l_tab_tadir.
    IF i_devclass <> c_devc_tmp.
      SELECT * FROM tadir INTO TABLE l_tab_tadir
        WHERE pgmid    = 'R3TR' AND
              object   = 'SSFO' AND
              devclass = i_devclass AND
              obj_name IN s_rest.                     "#EC CI_SGLSELECT
    ELSE.
      SELECT * FROM tadir INTO TABLE l_tab_tadir
        WHERE pgmid    = 'R3TR' AND
              object   = 'SSFO' AND
              devclass = i_devclass AND
              author   = sy-uname AND
              obj_name IN s_rest.                     "#EC CI_SGLSELECT
    ENDIF.

* Ignore invalid TADIR entries.
    DELETE l_tab_tadir WHERE obj_name IS INITIAL.

* Write count of function pools into list
    DESCRIBE TABLE l_tab_tadir LINES l_cnt.
    IF l_cnt = 0.
      EXIT.
    ENDIF.

* Process all function pools
    l_cnt_str = l_cnt.
    CONDENSE l_cnt_str.
    LOOP AT l_tab_tadir INTO l_str_tadir.

      CLEAR l_fm_name.
      l_form = l_str_tadir-obj_name.
      l_formname = l_str_tadir-obj_name.
      CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
        EXPORTING
          formname           = l_form
        IMPORTING
          fm_name            = l_fm_name
        EXCEPTIONS
          no_form            = 1
          no_function_module = 2
          OTHERS             = 3.

      l_tabix_str = sy-tabix.
      CONDENSE l_tabix_str.

*   Display progress indicator
      l_percentage = 100 * ( sy-tabix / l_cnt ).
      CONCATENATE 'Scanne Paket'(008) i_devclass l_aux_devc
                  '(' 'FuGr'(011) l_tabix_str 'von'(010) l_cnt_str ')'
                  INTO l_text SEPARATED BY space.
      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          percentage = l_percentage
          text       = l_text.

*   Get function pool objects
*    write: / l_str_tadir-obj_name.
      l_str_e071-pgmid    = 'R3TR'.
      l_str_e071-object   = 'FUGR'.
      l_str_e071-obj_name = l_fm_name.
      REFRESH l_tab_e071.
      CALL FUNCTION 'STOR_RESOLVE_FUGR'
        EXPORTING
          is_e071 = l_str_e071
        TABLES
          tt_e071 = l_tab_e071
        EXCEPTIONS
          OTHERS  = 0.

*   Read basis program sources and search for specified strings
      LOOP AT l_tab_e071 INTO l_str_e071 WHERE object = 'REPO' .
        l_rep_name = l_str_e071-obj_name.
        REFRESH l_tab_source.
        READ REPORT l_rep_name INTO l_tab_source.
        IF sy-subrc = 0.

          scan_prog( EXPORTING i_devclass = i_devclass
                               i_rep_name = l_rep_name
                               i_cnt_line = i_lrng
                               i_formname = l_formname
                             i_tadir    = l_str_tadir
                            CHANGING c_tab_source = l_tab_source ).

        ENDIF.
      ENDLOOP .

* (A) Keine generierten Dialoge?!? Das sollte man evtl. optional
*     anbieten (Zeitpunkt-Routinen!)
*   Read function module sources and search for specified strings
      LOOP AT l_tab_e071 INTO l_str_e071 WHERE object = 'FUNC' .
        IF l_str_e071-obj_name(4) = 'VIEW'. "Keine gen. Dialoge
          CONTINUE.
        ENDIF.
        SELECT SINGLE * FROM tfdir INTO l_str_tfdir
          WHERE funcname = l_str_e071-obj_name.       "#EC CI_SGLSELECT
        IF sy-subrc = 0.
          CONCATENATE l_str_tfdir-pname 'U' l_str_tfdir-include
                      INTO l_rep_name.
          REPLACE 'SAPL' WITH 'L' INTO l_rep_name.
          REFRESH l_tab_source.
          READ REPORT l_rep_name INTO l_tab_source.
          IF sy-subrc = 0.

            scan_prog( EXPORTING i_devclass = i_devclass
                                 i_rep_name = l_rep_name
                                 i_cnt_line = i_lrng
                                 i_formname = l_formname
                             i_tadir    = l_str_tadir
                              CHANGING c_tab_source = l_tab_source ).

          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDLOOP .
  ENDMETHOD.
ENDCLASS.

CLASS lcl_scan_adobe IMPLEMENTATION.
  METHOD scan_object.
    DATA: l_tab_tadir     TYPE TABLE OF tadir,
          l_str_tadir     TYPE tadir,
          l_tab_e071      TYPE TABLE OF e071,
          l_str_e071      TYPE e071,
          l_str_tfdir     TYPE tfdir,
          l_cnt           TYPE i,
          l_cnt_str(10)   TYPE c,
          l_idx_devc(10)  TYPE c,
          l_cnt_devc(10)  TYPE c,
          l_aux_devc(20)  TYPE c,
          l_percentage    TYPE p,
          l_tabix_str(10) TYPE c,
          l_rep_name      TYPE sobj_name,
          l_tab_source    TYPE TABLE OF t_abapsource_long,       "CB
          l_text          TYPE itex132.

    DATA l_form TYPE  fpname.
    DATA l_formname TYPE  string.
    DATA l_fm_name TYPE  funcname.

* Initialization
    l_idx_devc = i_tabix.
    l_cnt_devc = i_cnt.
    CONCATENATE l_idx_devc '/' l_cnt_devc INTO l_aux_devc.
    CONDENSE l_aux_devc.

* Get function pools of current package
    REFRESH l_tab_tadir.
    IF i_devclass <> c_devc_tmp.
      SELECT * FROM tadir INTO TABLE l_tab_tadir
        WHERE pgmid    = 'R3TR' AND
              object   = 'SFPF' AND
              devclass = i_devclass AND
              obj_name IN s_rest.                     "#EC CI_SGLSELECT
    ELSE.
      SELECT * FROM tadir INTO TABLE l_tab_tadir
        WHERE pgmid    = 'R3TR' AND
              object   = 'SFPF' AND
              devclass = i_devclass AND
              author   = sy-uname AND
              obj_name IN s_rest.                     "#EC CI_SGLSELECT
    ENDIF.

* Ignore invalid TADIR entries.
    DELETE l_tab_tadir WHERE obj_name IS INITIAL.

* Write count of function pools into list
    DESCRIBE TABLE l_tab_tadir LINES l_cnt.
    IF l_cnt = 0.
      EXIT.
    ENDIF.

* Process all function pools
    l_cnt_str = l_cnt.
    CONDENSE l_cnt_str.
    LOOP AT l_tab_tadir INTO l_str_tadir.

      CLEAR l_fm_name.
      l_form = l_str_tadir-obj_name.
      l_formname = l_str_tadir-obj_name.
      TRY.
          CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
            EXPORTING
              i_name     = l_form
            IMPORTING
              e_funcname = l_fm_name.
        CATCH cx_fp_api_repository.
          CONTINUE.
        CATCH     cx_fp_api_usage.
          CONTINUE.
        CATCH     cx_fp_api_internal.
          CONTINUE.
      ENDTRY.
      l_tabix_str = sy-tabix.
      CONDENSE l_tabix_str.

*   Display progress indicator
      l_percentage = 100 * ( sy-tabix / l_cnt ).
      CONCATENATE 'Scanne Paket'(008) i_devclass l_aux_devc
                  '(' 'FuGr'(011) l_tabix_str 'von'(010) l_cnt_str ')'
                  INTO l_text SEPARATED BY space.
      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          percentage = l_percentage
          text       = l_text.

*   Get function pool objects
*    write: / l_str_tadir-obj_name.
      l_str_e071-pgmid    = 'R3TR'.
      l_str_e071-object   = 'FUGR'.
      l_str_e071-obj_name = l_fm_name.
      REFRESH l_tab_e071.
      CALL FUNCTION 'STOR_RESOLVE_FUGR'
        EXPORTING
          is_e071 = l_str_e071
        TABLES
          tt_e071 = l_tab_e071
        EXCEPTIONS
          OTHERS  = 0.

*   Read basis program sources and search for specified strings
      LOOP AT l_tab_e071 INTO l_str_e071 WHERE object = 'REPO' .
        l_rep_name = l_str_e071-obj_name.
        REFRESH l_tab_source.
        READ REPORT l_rep_name INTO l_tab_source.
        IF sy-subrc = 0.

          scan_prog( EXPORTING i_devclass = i_devclass
                               i_rep_name = l_rep_name
                               i_cnt_line = i_lrng
                               i_formname = l_formname
                             i_tadir    = l_str_tadir
                            CHANGING c_tab_source = l_tab_source ).
        ENDIF.
      ENDLOOP .

* (A) Keine generierten Dialoge?!? Das sollte man evtl. optional
*     anbieten (Zeitpunkt-Routinen!)
*   Read function module sources and search for specified strings
      LOOP AT l_tab_e071 INTO l_str_e071 WHERE object = 'FUNC' .
        IF l_str_e071-obj_name(4) = 'VIEW'. "Keine gen. Dialoge
          CONTINUE.
        ENDIF.
        SELECT SINGLE * FROM tfdir INTO l_str_tfdir
          WHERE funcname = l_str_e071-obj_name.       "#EC CI_SGLSELECT
        IF sy-subrc = 0.
          CONCATENATE l_str_tfdir-pname 'U' l_str_tfdir-include
                      INTO l_rep_name.
          REPLACE 'SAPL' WITH 'L' INTO l_rep_name.
          REFRESH l_tab_source.
          READ REPORT l_rep_name INTO l_tab_source.
          IF sy-subrc = 0.

            scan_prog( EXPORTING i_devclass = i_devclass
                                 i_rep_name = l_rep_name
                                 i_cnt_line = i_lrng
                                 i_formname = l_formname
                             i_tadir    = l_str_tadir
                              CHANGING c_tab_source = l_tab_source ).

          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDLOOP .

  ENDMETHOD.

ENDCLASS.
