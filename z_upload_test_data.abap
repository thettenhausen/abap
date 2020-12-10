*&---------------------------------------------------------------------*
*& Report  Z12S_TEST_MASS_UPLOAD_DEHETT
*&
*&---------------------------------------------------------------------*
*&
*& This report uploads a csv file and tries to insert its content
*& into the given database table. It will NOT check for authorisation
*& or if the data is valid. Also it will OVERWRITE old data, so be
*& CAREFUL!
*&
*&---------------------------------------------------------------------*


report z12s_test_mass_upload_dehett.

types: begin of ttab,
         rec(1000) type c,
       end of ttab.

constants: c_delim_tab   type char1 value cl_abap_char_utilities=>horizontal_tab,
           c_delim_space type char1 value ' ',
           c_delim_semi  type char1 value ';'.

data: file_table    type filetable,
      lv_string     type string,
      file_line     like line of file_table,
      rc            type i,
      filename      type string,
      lv_delim      type char1,
      lv_field_type type char1.

data: ddobjtype        type dd02v-tabclass,
      lt_dfies         type table of dfies,
      ls_dfies         type dfies,
      lt_table         type table of ttab,
      ls_table         type ttab,
      lr_table         type ref to data,
      lr_table_line    type ref to data,
      lt_result_table  type table of string,
      lv_result_line   type string,
      lv_string_length type i,
      lv_data          type c length 1000,
      ls_key_fields    type abap_sortorder,
      lt_key_fields    type abap_sortorder_tab.

field-symbols: <fs_field>     type any,
               <fs_table>     type standard table,
               <fs_tableline>.

* start screen, allows the user to set the database table, the csv file,
* and to specify whether the csv file has a header row, and the delimiter
* (currently tab, space, semicolon are supported)
selection-screen begin of block a1 with frame title titletxt.
selection-screen comment /1(60) comment.
selection-screen skip.
parameters: tabname  type ddobjname obligatory. " db table to uload to
selection-screen skip.
parameters: header   type char1 as checkbox default 'X', " csv contains header lines
            dlm_tab  type char1 radiobutton group dlm, " csv delimiter char is tab
            dlm_spc  type char1 radiobutton group dlm, " csv delimiter char is space
            dlm_semi type char1 radiobutton group dlm default 'X', " csv delimiter char is semicolon (excel default)
            as_table type char1 as checkbox default 'X', " add all lines in one go (not per line) to db table (better performance)
            cleanup  type char1 as checkbox default 'X', " sort/delete duplicates
            addclnt  type char1 as checkbox default 'X', " add MANDT as first column to csv data
            delete   type char1 as checkbox default '', " drop old table contents
            test     type char1 as checkbox default 'X'. " don't actually change anything
selection-screen end of block a1.


initialization.
  titletxt = 'General Settings'.
  comment = 'Do not run this if you do not know what you are doing!'.
  %_tabname_%_app_%-text = 'Target table in DDIC'.
  %_header_%_app_%-text = 'CSV has a header line'.
  %_dlm_tab_%_app_%-text = 'CSV delimiter is TAB'.
  %_dlm_spc_%_app_%-text = 'CSV delimiter is SPACE'.
  %_dlm_semi_%_app_%-text = 'CSV delimiter is SEMICOLON'.
  %_as_table_%_app_%-text = 'Add all lines to DB at once'.
  %_cleanup_%_app_%-text = 'Sort/delete adj. duplicates'.
  %_addclnt_%_app_%-text = 'Add column for MANDT to data'.
  %_delete_%_app_%-text = 'Drop data in DB table first'.
  %_test_%_app_%-text = 'Test mode'.

* do NOT run in system marked as production - this was created to upload
* some test data into a development system and therefore does not have any
* validity, sanity or authorisation checks...
  call function 'PRGN_CHECK_SYSTEM_PRODUCTIVE'
    exceptions
      client_is_productive = 1
      others               = 2.

  if sy-subrc <> 0.
    exit.
  endif.

* only specific DEVELOPERS can run this - additional safety measure
* if sy-uname <> 'USERNAME'.
*   exit.
* endif.


start-of-selection.

* get structure of the DDIC table the user entered
  call function 'DDIF_NAMETAB_GET'
    exporting
      tabname   = tabname
    importing
      ddobjtype = ddobjtype
    tables
      dfies_tab = lt_dfies
    exceptions
      not_found = 1
      others    = 2.

  if sy-subrc <> 0.
* Implement suitable error handling here
    exit.
  endif.

  if ddobjtype is initial.
    exit.
  endif.

  create data lr_table type standard table of (tabname).

  assign lr_table->* to <fs_table>.
  create data lr_table_line like line of <fs_table>.
  assign lr_table_line->* to <fs_tableline>.

* delimiter
  if dlm_spc = 'X'.
    move c_delim_space to lv_delim.
  elseif dlm_tab = 'X'.
    move c_delim_tab to lv_delim.
  elseif dlm_semi = 'X'.
    move c_delim_semi to lv_delim.
  endif.

* open a file dialog for the csv file
  call method cl_gui_frontend_services=>file_open_dialog
    exporting
      window_title            = lv_string
      default_extension       = 'CSV'
    changing
      file_table              = file_table
      rc                      = rc
    exceptions
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      others                  = 5.

  if rc eq 1. "exactly one file was selected
    read table file_table index 1 into file_line.
    filename = file_line-filename.

    if filename is not initial. "a file name was found

      call method cl_gui_frontend_services=>gui_upload
        exporting
          filename                = filename
        changing
          data_tab                = lt_table
        exceptions
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
          not_supported_by_gui    = 17
          error_no_gui            = 18
          others                  = 19.

*   build correct internal table from the uploaded CSV
      loop at lt_table into ls_table.
        clear: lt_result_table, <fs_tableline>.
        if header = 'X' and sy-tabix = 1.
          continue.
        endif.
        if addclnt eq 'X'.
          concatenate sy-mandt lv_delim ls_table into ls_table.
        endif.
        split ls_table at lv_delim into table lt_result_table.
        if sy-subrc = 0.
          clear: lv_result_line.
          loop at lt_result_table into lv_result_line.
            clear: ls_dfies.
            read table lt_dfies into ls_dfies index sy-tabix.
            assign component ls_dfies-fieldname of structure <fs_tableline> to <fs_field>.
            if sy-subrc = 0 and <fs_field> is assigned.
              describe field <fs_field> type lv_field_type.
              if lv_field_type = 'P'.
                replace all occurrences of ',' in lv_result_line with ''.
                replace all occurrences of '.' in lv_result_line with ''.
              endif.
              if strlen( lv_result_line ) > 0.
                if lv_result_line(1) eq '"'. " first character is double quote
                  lv_string_length = strlen( lv_result_line ).
                  lv_string_length = lv_string_length - 1.
                  if lv_result_line+lv_string_length(1) eq '"'. " last character is double quote
                    lv_string_length = lv_string_length - 1.
                    lv_result_line = lv_result_line+1(lv_string_length).
                  endif.
                endif.
              endif.
              <fs_field> = lv_result_line.
              unassign <fs_field>.
            endif.
          endloop.
          append <fs_tableline> to <fs_table>.
        endif.
      endloop.

      if cleanup eq 'X'.
*     list of keys for sorting
        clear: lt_key_fields, ls_key_fields.
        loop at lt_dfies into ls_dfies where keyflag = 'X'.
          ls_key_fields-name = ls_dfies-fieldname.
          insert ls_key_fields into table lt_key_fields.
        endloop.

        sort <fs_table> by (lt_key_fields).
        delete adjacent duplicates from <fs_table>.
      endif.

      if test eq ''.
        if delete eq 'X' and lines( <fs_table> ) gt 0.
          delete from (tabname).
        endif.

        if as_table eq 'X'.
          insert (tabname) from table <fs_table>.
        else.
          loop at <fs_table> assigning <fs_tableline>.
            insert (tabname) from <fs_tableline>.
            if sy-subrc <> 0.
              move <fs_tableline> to lv_data.
              write: / 'Problem with line: ', lv_data.
            endif.
          endloop.
        endif.
        write: / 'Done.'.
      else.
        write: / 'Done - TEST MODE ONLY.'.
      endif.

    endif.
  endif.
