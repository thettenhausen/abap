function z_create_pdf.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(IV_VALUE) TYPE  STRING
*"     VALUE(IV_SAVE_LOCAL) TYPE  BOOLE_D DEFAULT 'X'
*"     VALUE(IV_PDF_AS_XSTRING) TYPE  BOOLE_D DEFAULT ''
*"     VALUE(IV_APPL_SERVER) TYPE  BOOLE_D DEFAULT ''
*"  EXPORTING
*"     VALUE(EV_SERVER_PATH) TYPE  STRING
*"     VALUE(EV_PDF) TYPE  XSTRING
*"     VALUE(EV_PDF_SIZE) TYPE  I
*"  EXCEPTIONS
*"      NOTHING_TO_DO
*"----------------------------------------------------------------------

  data: lv_fm_name type rs38l_fnam,
        ls_control_parameters type ssfctrlop,
        ls_output_options type ssfcompop,
        ls_output_info type ssfcrescl,
        lv_pdf_filesize type i,
        lv_pdf_xstring type xstring,
        lt_pdf_tab type table of char80,
        ls_pdf_tab type char80,
        lt_pdf_lines type table of tline,
        lv_default_filename type string,
        lv_server_path type string,
        lv_server_file type string value 'test.pdf',
        lv_lcl_filename type string,
        lv_lcl_path type string,
        lv_lcl_fullpath type string,
        lv_logical_path type pathintern.

* Sanity checks:
* check if input is good etc
* create good filename for output, and server
  concatenate 'MyFile' iv_value '.pdf' into lv_default_filename separated by space.

  ls_control_parameters-no_dialog = 'X'.
  ls_control_parameters-getotf    = 'X'.
  ls_control_parameters-preview = 'space'.
  ls_control_parameters-device = 'PRINTER'.
  ls_output_options-tdnoprev  = 'X'. " keine Vorschau zulassen
  ls_output_options-tddest = 'PDF'.

  call function 'SSF_FUNCTION_MODULE_NAME'
    exporting
      formname           = 'Z_MY_SMARTFORM'
    importing
      fm_name            = lv_fm_name
    exceptions
      no_form            = 1
      no_function_module = 2
      others             = 3.

  call function lv_fm_name " '/1BCDWB/SF00000015' will be something like this
    exporting
      control_parameters   = ls_control_parameters
      output_options       = ls_output_options
      user_settings        = ''
      iv_value             = iv_value " and other custom parameters of the smartform interface
    importing
      job_output_info      = ls_output_info
    exceptions
      formatting_error     = 1
      internal_error       = 2
      send_error           = 3
      user_canceled        = 4
      others               = 5.

  " Convert smartform OTF to PDF

  call function 'CONVERT_OTF'
    exporting
      format                = 'PDF'
    importing
      bin_filesize          = lv_pdf_filesize
      bin_file              = lv_pdf_xstring
    tables
      otf                   = ls_output_info-otfdata
      lines                 = lt_pdf_lines
    exceptions
      err_max_linewidth     = 1
      err_format            = 2
      err_conv_not_possible = 3
      err_bad_otf           = 4
      others                = 5.


* convert xstring to binary
  call function 'SCMS_XSTRING_TO_BINARY'
    exporting
      buffer     = lv_pdf_xstring
    tables
      binary_tab = lt_pdf_tab.


* Download in SAP GUI
  if iv_save_local = 'X'.

    call method cl_gui_frontend_services=>file_save_dialog
      exporting
        default_extension         = '.pdf'
        default_file_name         = lv_default_filename
        file_filter               = 'pdf'
      changing
        filename                  = lv_lcl_filename
        path                      = lv_lcl_path
        fullpath                  = lv_lcl_fullpath
      exceptions
        cntl_error                = 1
        error_no_gui              = 2
        not_supported_by_gui      = 3
        invalid_default_file_name = 4
        others                    = 5.

    call method cl_gui_frontend_services=>gui_download
      exporting
        bin_filesize            = lv_pdf_filesize
        filename                = lv_lcl_fullpath
        filetype                = 'BIN'
      changing
        data_tab                = lt_pdf_tab
      exceptions
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        not_supported_by_gui    = 22
        error_no_gui            = 23
        others                  = 24.

  endif.


* return PDF as xstring for web shop etc
  if iv_pdf_as_xstring eq 'X'.
    ev_pdf = lv_pdf_xstring.
    ev_pdf_size = xstrlen( lv_pdf_xstring ).
  endif.


* Auf Anwendungsserver speichern
  if iv_appl_server = 'X'.
    lv_logical_path = 'z_PDF Ablage'.

*  convert logical to physical path to directory
* didn't use this due to customer situation
*    call function 'FILE_GET_NAME_USING_PATH'
*      exporting
*        logical_path               = lv_logical_path
*        file_name                  = lv_default_filename
*      importing
*        file_name_with_path        = lv_server_path
*      exceptions
*        path_not_found             = 1
*        missing_parameter          = 2
*        operating_system_not_found = 3
*        file_system_not_found      = 4
*        others                     = 5.

    concatenate '/usr/C11/PDF/' lv_default_filename into lv_server_path. " check with basis

    open dataset lv_server_path for output in binary mode.
    if sy-subrc = 0.
      loop at lt_pdf_tab into ls_pdf_tab.
        transfer ls_pdf_tab to lv_server_path no end of line.
      endloop.
      close dataset lv_server_path.
    endif.
    ev_server_path = lv_server_path.

  endif.

endfunction.
