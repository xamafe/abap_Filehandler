CLASS zcl_file__base DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PROTECTED .

  PUBLIC SECTION.

    INTERFACES zif_file
      ABSTRACT METHODS
      delete_file
      get_directory_content
      get_directory_seperator
      read_file
      write_file .

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA m_content TYPE REF TO data.
    DATA m_full_path TYPE string.
    DATA m_directory TYPE string.
    DATA m_filename TYPE string.
    DATA m_file_extension TYPE string.
    DATA m_filesize TYPE i.
    DATA m_operating_system TYPE sy-opsys.
    DATA m_encoding TYPE abap_encoding.
    DATA m_file_is_binary TYPE abap_bool.
    DATA m_location TYPE ze_file_location.

    METHODS replace_fullpath_variables .
    METHODS split_fullpath_into_parts .

ENDCLASS.



CLASS zcl_file__base IMPLEMENTATION.


  METHOD replace_fullpath_variables.
    DATA: replacement(3).

* THESE ARE JUST EXAMPLE REPLACEMENTS... Invent your own :-) Date? Timestamp? you name it.

* Sys-ID ...
    REPLACE ALL OCCURRENCES OF '<sys-id>' IN m_full_path WITH sy-sysid IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '<sys-id>' IN m_directory WITH sy-sysid IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '<sys-id>' IN m_filename WITH sy-sysid IGNORING CASE.

* Environment-ID ...
    IF sy-sysid = 'DEV' OR sy-sysid = 'QS1' OR sy-sysid = 'QS2'.
      replacement = 'development'.
    ELSE.
      replacement = 'productive'.
    ENDIF.

    REPLACE ALL OCCURRENCES OF '<env-id>' IN m_full_path WITH replacement IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '<env-id>' IN m_directory WITH replacement IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '<env-id>' IN m_filename WITH replacement IGNORING CASE.

  ENDMETHOD.


  METHOD zif_file~copy_file.
*   create the new file
    r_result = zcl_file=>create( i_full_path = i_path
                                 i_location  = i_destination ).
    IF r_result IS NOT BOUND.
      RETURN.
    ENDIF.

*   move data
    r_result->set_filesize( zif_file~get_filesize( ) ).
    r_result->set_file_is_binary( zif_file~get_file_is_binary(  ) ).

*   Write new file
    r_result->write_file( i_write_as_binary = zif_file~get_file_is_binary(  ) ).

  ENDMETHOD.


  METHOD zif_file~get_content.
    r_result = me->m_content.
  ENDMETHOD.


  METHOD zif_file~get_directory.
    r_result = me->m_directory.
  ENDMETHOD.


  METHOD zif_file~get_encoding.
    r_result = me->m_encoding.
  ENDMETHOD.


  METHOD zif_file~get_filename.
    r_result = me->m_filename.
  ENDMETHOD.


  METHOD zif_file~get_filesize.
    r_result = me->m_filesize.
  ENDMETHOD.


  METHOD zif_file~get_file_extension.
    r_result = me->m_file_extension.
  ENDMETHOD.


  METHOD zif_file~get_file_is_binary.
    r_result = me->m_file_is_binary.
  ENDMETHOD.


  METHOD zif_file~get_full_path.
    r_result = m_full_path.
  ENDMETHOD.


  METHOD zif_file~get_operating_system.
    r_result = me->m_operating_system.
  ENDMETHOD.


  METHOD zif_file~move_file.
    r_result =  zif_file~copy_file( i_destination = i_destination
                                    i_path        = i_path ).
*   Delete old file
    zif_file~delete_file( ).
  ENDMETHOD.


  METHOD zif_file~set_content.
    me->m_content = i_content.
  ENDMETHOD.


  METHOD zif_file~set_directory.
    me->m_directory = i_directory.
  ENDMETHOD.


  METHOD zif_file~set_encoding.
    me->m_encoding = i_encoding.
  ENDMETHOD.


  METHOD zif_file~set_filename.
    me->m_filename = i_filename.
  ENDMETHOD.


  METHOD zif_file~set_filesize.
    me->m_filesize = i_m_filesize.
  ENDMETHOD.


  METHOD zif_file~set_file_extension.
    me->m_file_extension = i_file_extension.
  ENDMETHOD.


  METHOD zif_file~set_file_is_binary.
    me->m_file_is_binary = i_file_is_binary.
  ENDMETHOD.


  METHOD zif_file~set_fullpath_by_logic_filename.
    DATA: emergency_flag TYPE abap_bool.

    IF m_location = zif_file=>mc_loc_sapgui.
      DATA(is_frontend) = abap_true.
    ENDIF.

    DATA(operating_system) = zif_file~get_operating_system( ).

    CALL FUNCTION 'FILE_GET_NAME'
      EXPORTING
        logical_filename        = i_log_filename
        operating_system        = operating_system
        parameter_1             = i_param1
        parameter_2             = i_param2
        parameter_3             = i_param3
        use_presentation_server = is_frontend
        with_file_extension     = abap_false
        eleminate_blanks        = abap_false
      IMPORTING
        emergency_flag          = emergency_flag
*       FILE_FORMAT             =
        file_name               = m_full_path
      EXCEPTIONS
        file_not_found          = 1
        OTHERS                  = 2.
    IF sy-subrc <> 0 OR emergency_flag IS NOT INITIAL.
      FREE: m_full_path.
      r_result = 8.
    ELSE.
      split_fullpath_into_parts( ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_file~set_fullpath_by_logic_path.

    DATA:
      filename           TYPE string,
      is_frontend_server TYPE abap_bool.

*   Build filename
    IF m_filename IS INITIAL AND m_full_path IS NOT INITIAL.
      split_fullpath_into_parts( ).
      filename = m_full_path.
    ENDIF.

    IF m_filename IS INITIAL.
      filename = zif_file=>mc_filename_replace.
    ELSE.
      filename = m_filename.
    ENDIF.

*   Where are we?
    IF m_location = zcl_filehandler_frontend=>mc_destination_frontend.
      is_frontend_server = abap_true.
    ENDIF.

    CALL FUNCTION 'FILE_GET_NAME_USING_PATH'
      EXPORTING
        logical_path               = i_logpath
        file_name                  = filename
        use_presentation_server    = is_frontend_server
        eleminate_blanks           = abap_false
      IMPORTING
        file_name_with_path        = m_full_path
      EXCEPTIONS
        path_not_found             = 1
        missing_parameter          = 2
        operating_system_not_found = 3
        file_system_not_found      = 4
        OTHERS                     = 5.
    r_result = sy-subrc.

    IF r_result = 0.
      split_fullpath_into_parts( ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_file~set_full_path.
    m_full_path = i_full_path.
  ENDMETHOD.


  METHOD zif_file~set_operating_system.
    me->m_operating_system = i_operating_system.
  ENDMETHOD.
  METHOD zif_file~get_m_location.
    r_result = me->m_location.
  ENDMETHOD.

  METHOD zif_file~set_m_location.
    me->m_location = i_m_location.
  ENDMETHOD.

  METHOD split_fullpath_into_parts.
    DATA: lv_sep(2),
          lv_regex TYPE string.

    CHECK m_full_path IS NOT INITIAL.
    lv_sep = zif_file~get_directory_seperator( ).

    IF lv_sep = '\'.
      lv_sep = '\\'.
    ENDIF.

    lv_regex = '(.*)' && lv_sep && '(.*)\.(.*)$'.
    CONDENSE lv_regex.
*   All at once, if in correct order
    FIND FIRST OCCURRENCE OF REGEX lv_regex IN m_full_path SUBMATCHES m_directory m_filename m_file_extension.
    CHECK sy-subrc <> 0.

    IF m_full_path CA '\/'.
      CLEAR lv_regex.
      lv_regex = '(.*)' && lv_sep && '(.*)'.
      CONDENSE lv_regex.
      FIND FIRST OCCURRENCE OF REGEX lv_regex IN m_full_path SUBMATCHES m_directory m_filename.
      CHECK sy-subrc <> 0.
    ELSEIF m_full_path CA '.'.
      CLEAR lv_regex.
      lv_regex = '(.*)\.(.*)$'.
      FIND FIRST OCCURRENCE OF REGEX lv_regex IN m_full_path SUBMATCHES m_directory.
      CHECK sy-subrc <> 0.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
