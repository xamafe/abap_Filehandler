*&---------------------------------------------------------------------*
*& Report Z_FILESAMPLE__READ_FILE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_filesample__read_file.

TYPES:
    tv_binary_line TYPE x LENGTH 1000.

DATA:
  gt_itab TYPE STANDARD TABLE OF tv_binary_line,
  gv_dest TYPE ze_destination.

PARAMETERS:
  p_path TYPE string LOWER CASE,
  p_gui  RADIOBUTTON GROUP a,
  p_sap  RADIOBUTTON GROUP a.

START-OF-SELECTION.

  gv_dest = zcl_filehandler_base=>get_destination( iv_frontend = p_gui
                                                   iv_backend  = p_sap ).

* Read data to internal type
  DATA(go_file_internal) = zcl_filehandler_base=>create( iv_path        = p_path
                                                         iv_destination = gv_dest ).
  go_file_internal->read_file( ).
  go_file_internal->split_fullpath( ).

* Read data to object-external table
  DATA(go_file_global_table) = zcl_filehandler_base=>create( iv_path         = p_path
                                           iv_destination  = gv_dest ).
  GET REFERENCE OF gt_itab INTO go_file_global_table->mt_content.
  go_file_global_table->read_file( iv_read_as_binary = 'X' ).
  go_file_global_table->split_fullpath( ).

  WRITE: / 'Done'.
