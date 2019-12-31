INTERFACE zif_file
  PUBLIC .
  CONSTANTS mc_loc_sapgui TYPE ze_file_location VALUE 'S'.
  CONSTANTS mc_loc_appserver TYPE ze_file_location VALUE 'A'.

  CONSTANTS mc_filename_replace TYPE dxfilename VALUE '<FILENAME>'.
  CONSTANTS mc_filetype_dir TYPE filetype   VALUE 'DIR'       .
  CONSTANTS mc_filetype_file TYPE filetype   VALUE 'FILE'      .

  METHODS copy_file IMPORTING !i_destination TYPE ze_file_location
                              !i_path        TYPE csequence
                    RETURNING VALUE(r_result) TYPE REF TO zif_file.
  METHODS delete_file.
  METHODS move_file IMPORTING !i_destination TYPE ze_file_location
                              !i_path        TYPE csequence
                    RETURNING VALUE(r_result) TYPE REF TO zif_file.
  METHODS read_file IMPORTING !i_read_as_binary TYPE flag DEFAULT abap_false
                    EXPORTING !ev_binary_length TYPE int4 .
  METHODS write_file IMPORTING i_write_as_binary TYPE flag OPTIONAL
                               i_binsize         TYPE int4 OPTIONAL.

  METHODS get_full_path RETURNING VALUE(r_result) TYPE string.
  METHODS set_full_path IMPORTING i_full_path TYPE string.
  METHODS get_directory_content
    IMPORTING
      i_only_subdir     TYPE flag DEFAULT abap_false
      i_only_files      TYPE flag DEFAULT abap_false
      i_build_object    TYPE flag DEFAULT abap_false
      i_append_rettable TYPE flag DEFAULT abap_false
    RETURNING
      VALUE(r_result)   TYPE ztt_basic_file_list.
  METHODS get_filesize RETURNING VALUE(r_result) TYPE i.
  METHODS set_filesize IMPORTING i_m_filesize TYPE i.
  METHODS get_operating_system RETURNING VALUE(r_result) TYPE sy-opsys.
  METHODS set_operating_system IMPORTING i_operating_system TYPE sy-opsys.
  METHODS set_fullpath_by_logic_filename IMPORTING i_log_filename  TYPE filename-fileintern
                                                   i_param1        TYPE csequence
                                                   i_param2        TYPE csequence
                                                   i_param3        TYPE csequence
                                         RETURNING VALUE(r_result) TYPE sy-subrc.
  METHODS set_fullpath_by_logic_path IMPORTING i_logpath       TYPE filepath-pathintern
                                     RETURNING VALUE(r_result) TYPE sy-subrc .
  METHODS get_directory_seperator RETURNING VALUE(r_result) TYPE char01.
  METHODS get_encoding RETURNING VALUE(r_result) TYPE abap_encoding.
  METHODS set_encoding IMPORTING i_encoding TYPE abap_encoding.
  METHODS set_file_extension IMPORTING i_file_extension TYPE string.
  METHODS get_file_extension RETURNING VALUE(r_result) TYPE string.
  METHODS set_filename IMPORTING i_filename TYPE string.
  METHODS get_filename RETURNING VALUE(r_result) TYPE string.
  METHODS set_directory IMPORTING i_directory TYPE string.
  METHODS get_directory RETURNING VALUE(r_result) TYPE string.
  METHODS: get_content RETURNING value(r_result) TYPE REF TO data,
           set_content IMPORTING i_content TYPE REF TO data,
           get_file_is_binary RETURNING value(r_result) TYPE abap_bool,
           set_file_is_binary IMPORTING i_file_is_binary TYPE abap_bool,
           get_m_location RETURNING value(r_result) TYPE ze_file_location,
           set_m_location IMPORTING i_m_location TYPE ze_file_location.

ENDINTERFACE.
