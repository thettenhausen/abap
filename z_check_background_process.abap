function z_check_background_process.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_DIALOG) TYPE  CRMT_BOOLEAN OPTIONAL
*"     REFERENCE(IV_BATCH) TYPE  CRMT_BOOLEAN OPTIONAL
*"     REFERENCE(IV_BATCH_INPUT) TYPE  CRMT_BOOLEAN OPTIONAL
*"     REFERENCE(IV_GUI) TYPE  CRMT_BOOLEAN OPTIONAL
*"     REFERENCE(IV_APO_BOP) TYPE  CRMT_BOOLEAN OPTIONAL
*"     REFERENCE(IF_MESSAGE_TYPE) TYPE  CRMT_BOOLEAN OPTIONAL
*"     REFERENCE(IV_MIDDLEWARE_DOWNLOAD) TYPE  CRMT_BOOLEAN OPTIONAL
*"     REFERENCE(IV_PPF_ACTION_FROM_REPORT) TYPE  CRMT_BOOLEAN OPTIONAL
*"  EXCEPTIONS
*"      RUNNING_IN_BACKGROUND
*"----------------------------------------------------------------------
*
* Generic function module to check whether the current processing is
* running in the background. It only performs each check if the
* appropriate import flag is set.
* If any check is positive, an exception is raised.
*
* At the moment this checks:
* - the dialog status
* - the batch and batch input flags in system field SY
* - whether SAP GUI is reported as running
* - if the APO background queue name is found on the callstack
* - if we have a message type for queue processing
* - if the PPF selection report is in the callstack

  include crm_log_states_con.

  data: lv_dialog_status type c,
        lv_gui_is_on type answer,
        lt_callstack type sys_callst,
        lv_message_type type crmt_message_type.

* need to read the callstack?
  if iv_apo_bop eq true
    or iv_ppf_action_from_report eq true.

    call function 'SYSTEM_CALLSTACK'
      exporting
        max_level    = 0
      importing
        et_callstack = lt_callstack.
  endif.

* Dialog mode
  if iv_dialog eq true.
    clear: lv_dialog_status.
    call function 'DIALOG_GET_STATUS'
      importing
        dialog_status = lv_dialog_status.
    if lv_dialog_status is not initial.
      raise running_in_background.
    endif.
  endif.

* Batch
  if iv_batch eq true.
    if not sy-batch is initial.
      raise running_in_background.
    endif.
  endif.


* Batch Input
  if iv_batch_input eq true.
    if not sy-binpt is initial.
      raise running_in_background.
    endif.
  endif.

* SAPGUI running?
* returns Y when calling from SAP and N while calling from Java/.Net etc
  if iv_gui eq true.
    clear: lv_gui_is_on.
    call function 'RFC_IS_GUI_ON'
      exporting
        login_check = space
      importing
        on          = lv_gui_is_on.
    if sy-subrc = 0 and lv_gui_is_on eq 'N'.
      raise running_in_background.
    endif.
  endif.

* Message type
  if if_message_type eq true.
    clear: lv_message_type.
    call function 'CRM_ORDER_PROCESS_MODE_GET_OW'
      importing
        ev_message_type = lv_message_type
      exceptions
        error_occurred  = 1
        parameter_error = 2
        others          = 3.
    if lv_message_type is not initial.
*     if processing originated in the web shop the message type
*     is not initial, but the processing should not be seen as
*     background processing
      read table lt_callstack with key eventname = 'CRM_ISA_BASKET_ORDER'
        eventtype = 'FUNC' transporting no fields.
      if sy-subrc <> 0.
        raise running_in_background.
      endif.
    endif.
  endif.

* Middleware download
  if iv_middleware_download eq true.
    if sy-xprog eq 'SAPLCRM_DOWNLOAD_BTMBDOC'.
      raise running_in_background.
    endif.
  endif.

* check whether APO backorder processing is active
  if iv_apo_bop eq true.
    read table lt_callstack with key eventname = 'CRM_SL_DOC_INBOUND_50A'
        transporting no fields.
    if sy-subrc = 0.
      raise running_in_background.
    endif.
  endif.

* Being called from report RSPPFPROCESS (selection report
* for triggering CRM PPF Actions)
  if iv_ppf_action_from_report eq true.
    read table lt_callstack with key progname = 'RSPPFPROCESS'
        eventtype = 'EVENT' transporting no fields.
    if sy-subrc = 0.
      raise running_in_background.
    endif.
  endif.

endfunction.
