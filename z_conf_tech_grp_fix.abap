*&---------------------------------------------------------------------*
*& Report  Z_CONF_TECH_GRP_FIX
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
report z_conf_tech_grp_fix.

* Checks given service confirmations for maintained service technician
* group (partner function 00000056). If not found tries to read it
* from technician (Z0000006) and add it.
* Fixes a problem where confirmations from a mobile solution were
* missing this specific partner function after a GoLive due to partners
* hanging in the middleware.

include: crm_direct.

data: ls_orderadm_h        type crmd_orderadm_h,
      lt_orderadm_h        type table of crmd_orderadm_h,
      lv_crm_sorg          type crmt_sales_org,
      lt_partner_wrk       type crmt_partner_external_wrkt,
      ls_partner_wrk       type crmt_partner_external_wrk,
      lt_relationships     type bapibus1006_relations_tab,
      ls_relationship      type bapibus1006_relations,
      lt_return            type bapiret2_tab,
      ls_return            type bapiret2,
      ls_partner_com       type crmt_partner_com,
      lt_partner_com       type crmt_partner_comt,
      lt_input_field_names type crmt_input_field_names_tab,
      ls_input_field_names type crmt_input_field_names,
      lt_objects_to_save   type crmt_object_guid_tab,
      lt_saved_objects     type crmt_return_objects,
      lt_exception         type crmt_exception_t,
      lt_objects_not_saved type crmt_object_guid_tab,
      lt_input_fields      type crmt_input_field_tab,
      ls_input_fields      type crmt_input_field,
      lv_partner_logic_key type comt_partner_logic_partner_key,
      lv_logical_key       type crmt_logical_key,
      lv_ref_handle        type crmt_handle,
      lv_guid_set          type crmt_object_guid,
      lv_error             type char100,
      lt_error             type table of char100,
      lv_success           type char100,
      lt_success           type table of char100,
      lv_no_save           type char100,
      lt_no_save           type table of char100,
      lv_saved             type char100,
      lt_saved             type table of char100.

* Selection screen
selection-screen begin of block b1 with frame.
parameters: p_sorg   type crmt_r3_sales_org matchcode object crm_orgman_r3_sales_org default '7700'.
select-options: s_ptype for ls_orderadm_h-process_type,
                s_date for ls_orderadm_h-posting_date default sy-datlo,
                s_conf for ls_orderadm_h-object_id.
selection-screen end of block b1.
parameters: p_test type crmt_boolean as checkbox default abap_true,
            p_logall type crmt_boolean as checkbox.

initialization.

* Select data from DB
start-of-selection.

* get CRM sales org
  select single sales_org from crmc_sorg_r3org
    into @lv_crm_sorg
    where vkorg = @p_sorg.

  select * from crmd_orderadm_h as h
    into corresponding fields of @ls_orderadm_h
    where h~object_type = @gc_object_type-srv_confirm
      and h~process_type in @s_ptype
      and h~posting_date in @s_date
      and h~object_id in @s_conf.

    select single * from crm_jest
      into @data(ls_crm_jest)
      where objnr = @ls_orderadm_h-guid
        and inact = @abap_false
        and stat = @gc_status-error.

    if sy-subrc = 0.
      clear: lv_guid_set.
      select single guid_set from crmd_link
        into @lv_guid_set
        where guid_hi = @ls_orderadm_h-guid
          and objtype_hi = '05'
          and objtype_set = '21'.

      select single * from crmd_orgman
      into @data(ls_orgman)
      where guid = @lv_guid_set
        and sales_org = @lv_crm_sorg.
      if sy-subrc = 0.
        insert ls_orderadm_h into table lt_orderadm_h.
      endif.
    endif.

  endselect.

  loop at lt_orderadm_h into ls_orderadm_h.

    clear: lt_partner_wrk,
    ls_partner_wrk,
    lt_relationships,
    ls_relationship,
    lt_return,
    ls_partner_com,
    lt_partner_com,
    lt_input_field_names,
    ls_input_field_names,
    lt_input_fields,
    ls_input_fields,
    lt_exception,
    lv_ref_handle.

*   read partners for confirmation
    call function 'CRM_PARTNER_READ_OW'
      exporting
        iv_ref_guid          = ls_orderadm_h-guid
        iv_ref_kind          = 'A'
      importing
        et_partner_wrk       = lt_partner_wrk
      exceptions
        error_occurred       = 1
        parameter_error      = 2
        entry_does_not_exist = 3
        others               = 4.

*   check if technician group is maintained
    read table lt_partner_wrk into ls_partner_wrk with key partner_fct = '00000056'. " technician group
    if sy-subrc = 0.
      continue.
    endif.

*   technician group not maintained in confirmation, read from technician relation
    read table lt_partner_wrk into ls_partner_wrk with key partner_fct = 'Z0000006'. " technician
    if sy-subrc <> 0.
      concatenate ls_orderadm_h-object_id ': ' text-e01 into lv_error respecting blanks. " No technician found
      condense lv_error.
      insert lv_error into table lt_error.
      continue.
    endif.

* get relations from technician
    call function 'BUPA_RELATIONSHIPS_GET'
      exporting
        businesspartnerguid = conv bu_partner_guid_bapi( ls_partner_wrk-bp_partner_guid )
      tables
        et_relationships    = lt_relationships
        et_return           = lt_return.

*   find technician group ("has employee" relation between technician and technician group in this case)
    loop at lt_relationships into ls_relationship where relationshipcategory = 'BUR010' and validfromdate <= sy-datlo and validuntildate >= sy-datlo.
    endloop.
    if ls_relationship is initial.
      concatenate ls_orderadm_h-object_id ': ' text-e02 into lv_error respecting blanks. " No technician group found for technician
      condense lv_error.
      insert lv_error into table lt_error.
      continue.
    endif.

*   data found, prepare write
    call function 'CRM_INTLAY_GET_HANDLE'
      importing
        ev_handle = lv_ref_handle.

    ls_partner_com-ref_guid = ls_orderadm_h-guid.
    ls_partner_com-ref_partner_handle = conv numc4( lv_ref_handle ).
    ls_partner_com-ref_kind = 'A'.
    ls_partner_com-partner_fct = '00000056'.
    ls_partner_com-partner_no = conv bu_partner( ls_relationship-partner1 ).
    ls_partner_com-display_type = 'BP'.
    ls_partner_com-kind_of_entry = 'C'.
    ls_partner_com-no_type = 'BP'.
    insert ls_partner_com into table lt_partner_com.

    ls_input_field_names-fieldname = 'PARTNER_FCT'.
    insert ls_input_field_names into table lt_input_field_names.
    ls_input_field_names-fieldname = 'PARTNER_NO'.
    insert ls_input_field_names into table lt_input_field_names.
    ls_input_field_names-fieldname = 'DISPLAY_TYPE'.
    insert ls_input_field_names into table lt_input_field_names.
    ls_input_field_names-fieldname = 'KIND_OF_ENTRY'.
    insert ls_input_field_names into table lt_input_field_names.
    ls_input_field_names-fieldname = 'NO_TYPE'.
    insert ls_input_field_names into table lt_input_field_names.
    ls_input_fields-ref_guid = ls_orderadm_h-guid.
    ls_input_fields-ref_kind = 'A'.
    ls_input_fields-logical_key = ls_partner_com-ref_partner_handle.
    ls_input_fields-objectname = 'PARTNER'.
    ls_input_fields-field_names = lt_input_field_names.
    insert ls_input_fields into table lt_input_fields.

    call function 'CRM_ORDER_MAINTAIN'
      exporting
        it_partner        = lt_partner_com
      importing
        et_exception      = lt_exception
      changing
        ct_input_fields   = lt_input_fields
      exceptions
        error_occurred    = 1
        document_locked   = 2
        no_change_allowed = 3
        no_authority      = 4
        others            = 5.

    if sy-subrc = 0.
      insert ls_orderadm_h-guid into table lt_objects_to_save.
      concatenate ls_orderadm_h-object_id ': ' text-s01 ' ' ls_relationship-partner1 into lv_success respecting blanks. " missing (and trying to add) technician group
      condense lv_success.
      insert lv_success into table lt_success.
    else.
      concatenate ls_orderadm_h-object_id ': ' text-e03 into lv_error respecting blanks. " Error calling CRM_ORDER_MAINTAIN, maybe locked?
      condense lv_error.
      insert lv_error into table lt_error.
    endif.
  endloop.

* write to DB if data was found
  if p_test is initial.
    if lt_objects_to_save is not initial.

      delete adjacent duplicates from lt_objects_to_save.

      call function 'CRM_ORDER_SAVE'
        exporting
          it_objects_to_save   = lt_objects_to_save
        importing
          et_saved_objects     = lt_saved_objects
          et_exception         = lt_exception
          et_objects_not_saved = lt_objects_not_saved
        exceptions
          document_not_saved   = 1
          others               = 2.

      loop at lt_saved_objects into data(ls_saved_object).
        concatenate ls_saved_object-object_id ': ' text-s02 into lv_saved respecting blanks. " successfully added technician group
        condense lv_saved.
        insert lv_saved into table lt_saved.
      endloop.

      loop at lt_objects_not_saved into data(lv_object_not_saved).
        select single object_id from crmd_orderadm_h
          into @data(lv_object_id_error)
          where guid = @lv_object_not_saved.
        write: lv_object_id_error to lv_no_save.
        insert lv_no_save into table lt_no_save.
      endloop.


      if lines( lt_saved_objects ) > 0.
        call function 'BAPI_TRANSACTION_COMMIT'
          exporting
            wait   = abap_true
          importing
            return = ls_return.
      endif.
    endif.
  endif.

* output

* errors - could not be saved (open?)
  if lines( lt_no_save ) > 0.
    sort lt_no_save.
    delete adjacent duplicates from lt_no_save.
    write: / text-o01. " The following confirmations could not be saved, please check manually:
    uline.
    loop at lt_no_save into lv_no_save.
      write: / lv_no_save.
    endloop.
    skip.
  endif.

* other errors (no technician, ...)
  if lines( lt_error ) > 0.
    sort lt_error.
    delete adjacent duplicates from lt_error.
    write: / text-o02. " The following confirmations could not be corrected, please check manually:
    uline.
    loop at lt_error into lv_error.
      write: / lv_error.
    endloop.
    skip.
  endif.

* success
  if lines( lt_success ) > 0 and p_logall = abap_true.
    write: / text-o03. " The following confirmations were corrected (not yet saved):
    uline.
    loop at lt_success into lv_success.
      write: / lv_success.
    endloop.
    skip.
  endif.

* success and saved
  if lines( lt_saved ) > 0.
    sort lt_saved.
    delete adjacent duplicates from lt_saved.
    write: / text-o04. " The following confirmations were actually saved:
    uline.
    loop at lt_saved into lv_saved.
      write: / lv_saved.
    endloop.
  endif.
