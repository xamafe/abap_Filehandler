*&---------------------------------------------------------------------*
*& Report Z_FILESAMPLE__READ_DIR
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_filesample__read_dir.

PARAMETERS: p_path TYPE string LOWER CASE.

START-OF-SELECTION.
* Get "file" object to set path
  DATA(go_file) = NEW zcl_filehandler_frontend( p_path ).
* get list of files with references
  DATA(gt_dir_gui) = go_file->get_dir_content( iv_path         = p_path
                                               iv_only_files   = abap_true
                                               iv_build_object = abap_true ).

* Do something ...
  LOOP AT gt_dir_gui ASSIGNING FIELD-SYMBOL(<file>).
    WRITE: / <file>-obj->mv_fullpath.
    "<file>-obj->read_file( ).
  ENDLOOP.
