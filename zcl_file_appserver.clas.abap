CLASS zcl_file_appserver DEFINITION
  PUBLIC
  CREATE PRIVATE
  INHERITING FROM zcl_file__base
  GLOBAL FRIENDS zcl_file .

  PUBLIC SECTION.
    METHODS: zif_file~delete_file REDEFINITION,
      zif_file~get_directory_content REDEFINITION,
      zif_file~get_directory_seperator REDEFINITION,
      zif_file~read_file REDEFINITION,
      zif_file~write_file REDEFINITION.
  PRIVATE SECTION.
    CLASS-METHODS value_help_for_appserver RETURNING VALUE(r_result) TYPE string.
    METHODS open_read_in_binary IMPORTING i_fullpath TYPE string.
    METHODS open_read_in_ascii
      IMPORTING
        i_fullpath TYPE string.
    METHODS open_read_in_utf8
      IMPORTING
        i_fullpath TYPE string
        i_bom      TYPE sychar01.
    METHODS open_read_in_utf16
      IMPORTING
        i_fullpath TYPE string
        i_bom      TYPE sychar01.
ENDCLASS.



CLASS zcl_file_appserver IMPLEMENTATION.
  METHOD value_help_for_appserver.
    DATA: startdirectory TYPE char03.

    IF sy-opsys = 'Windows NT'.
      startdirectory = c_root_windows.
    ELSE.
      startdirectory = c_root_linux.
    ENDIF.

    CALL FUNCTION '/SAPDMC/LSM_F4_SERVER_FILE'
      EXPORTING
        directory        = startdirectory
*       filemask         = iv_filemask
      IMPORTING
        serverfile       = r_result
      EXCEPTIONS
        canceled_by_user = 1
        OTHERS           = 2.

  ENDMETHOD.

  METHOD zif_file~delete_file.
    IF zif_file~get_full_path( ) IS INITIAL AND zif_file~get_filename( ) IS NOT INITIAL.
      " TODO
*    zif_file~build_fullpath_from_parts( ).
    ELSE.
      r_result = 1024.
      RETURN.
    ENDIF.

    DATA(fullpath) = zif_file~get_full_path( ).
    DELETE DATASET fullpath.

    r_result = sy-subrc.
  ENDMETHOD.

  METHOD zif_file~get_directory_content.

  ENDMETHOD.

  METHOD zif_file~get_directory_seperator.
    r_result = COND #( LET os = sy-opsys IN
                        WHEN os = c_os_windows THEN c_pathseperator_windows
                        WHEN os = c_os_linux   THEN c_pathseperator_other
                        WHEN os = c_os_hpux    THEN c_pathseperator_other
                        WHEN os = c_os_os400   THEN c_pathseperator_other
                        ELSE c_pathseperator_other ).
  ENDMETHOD.

  METHOD zif_file~read_file.

    DATA binary_line TYPE xstring.
    DATA text_line TYPE xstring.
    DATA fullpath TYPE string.

    FIELD-SYMBOLS <file_line> TYPE any.
    FIELD-SYMBOLS <file_table> TYPE table.

    fullpath = zif_file~get_full_path( ).

    "open dataset for this file
    IF i_read_as_binary = abap_true.
      open_read_in_binary( fullpath ).
    ELSE.
      cl_abap_file_utilities=>check_utf8( EXPORTING file_name = fullpath
                                          IMPORTING encoding = DATA(file_encoding)
                                                    bom      = DATA(file_byte_order_mark)
                                        ).
      CASE file_encoding.
        WHEN cl_abap_file_utilities=>encoding_7bit_ascii.
          open_read_in_ascii( fullpath ).
        WHEN cl_abap_file_utilities=>encoding_utf8.
          open_read_in_utf8( i_fullpath = fullpath
                             i_bom      = file_byte_order_mark ).
        WHEN cl_abap_file_utilities=>encoding_other.
          open_read_in_utf16( i_fullpath = fullpath
                              i_bom      = file_byte_order_mark ).
      ENDCASE.
    ENDIF.

    IF zif_file~get_file_is_binary( ) = abap_true.
      ASSIGN binary_line TO <file_line>.
    ELSE.
      ASSIGN text_line TO <file_line>.
    ENDIF.

    IF zif_file~get_content( ) IS NOT BOUND.
      create_content_table( ).
    ENDIF.

    DATA(file_content) = zif_file~get_content( ).
    ASSIGN file_content->* TO <file_table>.

    DO.
      READ DATASET fullpath INTO <file_line>.
      add_length_to_filesize( <file_line> ).
      APPEND <file_line> TO <file_table>.
    ENDDO.

    CLOSE DATASET fullpath.
    e_file_length = zif_file~get_filesize( ).

  ENDMETHOD.

  METHOD zif_file~write_file.

  ENDMETHOD.


  METHOD open_read_in_binary.
    OPEN DATASET i_fullpath FOR INPUT IN BINARY MODE.
    zif_file~set_file_is_binary( abap_true ).
  ENDMETHOD.

  METHOD open_read_in_ascii.
    OPEN DATASET i_fullpath FOR INPUT IN TEXT MODE ENCODING NON-UNICODE WITH SMART LINEFEED.
    zif_file~set_encoding( zif_codepages_const=>mc_codepage_iso8859_15 ).
    zif_file~set_file_is_binary( abap_false ).
  ENDMETHOD.


  METHOD open_read_in_utf8.
    IF i_bom IS NOT INITIAL.
      OPEN DATASET i_fullpath FOR INPUT IN TEXT MODE ENCODING UTF-8 AT POSITION 3 WITH SMART LINEFEED.
    ELSE.
      OPEN DATASET i_fullpath FOR INPUT IN TEXT MODE ENCODING UTF-8 WITH SMART LINEFEED.
    ENDIF.
    zif_file~set_encoding( zif_codepages_const=>mc_codepage_utf8 ).
    zif_file~set_file_is_binary( abap_false ).
  ENDMETHOD.


  METHOD open_read_in_utf16.
    OPEN DATASET i_fullpath FOR INPUT IN BINARY MODE.
    zif_file~set_file_is_binary( abap_true ).
    IF i_bom = cl_abap_file_utilities=>bom_utf16_be.
      zif_file~set_encoding( zif_codepages_const=>mc_codepage_utf16_be ).
    ELSE.
      zif_file~set_encoding( zif_codepages_const=>mc_codepage_utf16_le ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.
