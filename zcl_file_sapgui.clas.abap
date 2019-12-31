CLASS zcl_file_sapgui DEFINITION
  PUBLIC
  CREATE private
  INHERITING FROM zcl_file__base
  GLOBAL FRIENDS zcl_file .

  PUBLIC SECTION.
    METHODS: zif_file~copy_file REDEFINITION,
             zif_file~delete_file REDEFINITION,
             zif_file~get_directory_content REDEFINITION,
             zif_file~get_directory_seperator REDEFINITION,
             zif_file~move_file REDEFINITION,
             zif_file~read_file REDEFINITION,
             zif_file~write_file REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    Class-methods value_help_for_sapgui RETURNING VALUE(r_return) TYPE string.
ENDCLASS.



CLASS zcl_file_sapgui IMPLEMENTATION.
  METHOD value_help_for_sapgui.
    DATA selected_files TYPE filetable.
    data rc TYPE i.
        cl_gui_frontend_services=>file_open_dialog( EXPORTING
*                                                 window_title            =
*                                                 default_extension       =
*                                                 default_filename        =
*                                                 file_filter             =
*                                                 with_encoding           =
*                                                 initial_directory       =
                                                    multiselection          = abap_false
                                                CHANGING
                                                    file_table              = selected_files
                                                    rc                      = rc
*                                                 user_action             =
                                               ).
*   No input? No value!
    if rc < 1.
      EXIT.
    endif.

    r_return = selected_files[ 1 ].
  ENDMETHOD.

  METHOD zif_file~copy_file.

  ENDMETHOD.

  METHOD zif_file~delete_file.

  ENDMETHOD.

  METHOD zif_file~get_directory_content.

  ENDMETHOD.

  METHOD zif_file~get_directory_seperator.

  ENDMETHOD.

  METHOD zif_file~move_file.

  ENDMETHOD.

  METHOD zif_file~read_file.

  ENDMETHOD.

  METHOD zif_file~write_file.

  ENDMETHOD.

ENDCLASS.
