
*&---------------------------------------------------------------------*
*& Report  ZAFX_CODE_SCANNER                                           *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT  zsacs_code_scanner.

INCLUDE afx_global_data_public.

include zsacs_code_scan_types.
include zsacs_code_scan_data.
include zsacs_code_scan_clasdef.
include zsacs_code_scan_selscr.
include zsacs_code_scan_clasimp.

*-----------------------------------------------------------------------
*-----------------------------------------------------------------------
START-OF-SELECTION.
  lcl_source_scan=>start( ).
