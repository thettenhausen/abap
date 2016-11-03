*&---------------------------------------------------------------------*
*& Report  Z_PROD_QUAL_LIST
*&
*&---------------------------------------------------------------------*
*&
*& Finds all relevant products for a given sales org and then reads
*& the qualifications that are maintained for this product, together
*& with description, and the product hierarchy for easy checking of
*& correctness.
*& Reason for this report: technician  qualifications for products were
*& maintained based on product hierarchy, but some had the wrong 
*& or missing ones. This made it easy to find for business what had to
*& be corrected.
*&
*&---------------------------------------------------------------------*
report z_prod_qual_list.

types: begin of t_output,
         product_id  type comt_product_id,
         short_text  type comt_prshtextx,
         objid       type hrobjid,
         stext       type stext,
         category_id type comt_category_id,
       end of t_output.

data: lv_crm_sorg         type crmt_sales_org,
      lv_ref_product      type comt_product_guid,
      lt_ref_product      type table of comt_product_guid,
      ls_product          type comt_product,
      lt_interlinkages    type comt_product_il_api,
      ls_output           type t_output,
      lt_output           type table of t_output,
      ls_t777guid1        type t777guid1,
      lt_product_category type comt_product_prodcat_rel_api_t.

data: gr_table           type ref to cl_salv_table.

initialization.

* Selection screen
  selection-screen begin of block b01 with frame.
  parameters: p_sorg  type vkorg default '1000' matchcode object crm_orgman_r3_sales_org.
  selection-screen end of block b01.

start-of-selection.

  convert date sy-datlo time sy-timlo into time stamp data(lv_timestamp) time zone sy-zonlo.

* get CRM sales org
  select single sales_org from crmc_sorg_r3org
    into @lv_crm_sorg
    where vkorg = @p_sorg.

* get relevant products for sales org
  select rod~product_guid from comm_pr_frg_rod as rod
    into table @lt_ref_product
    where rod~sales_org = @lv_crm_sorg.

  if sy-subrc <> 0.
    message text-m01 type 'E'. " no products found for this sales org
  endif.

* get qualifications for product
  loop at lt_ref_product into lv_ref_product.

    clear: ls_product,
    lt_interlinkages,
    lt_product_category,
    ls_output.

    ls_product-product_guid = lv_ref_product.
    ls_product-client = sy-mandt.

*   product id
    select single product_id from comm_product
      into @ls_output-product_id
      where product_guid = @lv_ref_product.

*   description
    select single short_text from comm_prshtext
      into @ls_output-short_text
      where product_guid = @lv_ref_product
        and langu = @sy-langu
        and valid_from le @lv_timestamp
        and valid_to ge @lv_timestamp.

*   hierarchy
    call function 'COM_PRODCAT_API_GET_CATEGORIES'
      exporting
        iv_product_guid               = lv_ref_product
      importing
        et_product_category           = lt_product_category
      exceptions
        wrong_call                    = 1
        not_found                     = 2
        no_hierarchy_assigned_to_appl = 3
        data_inconsistent             = 4
        others                        = 5.

    read table lt_product_category into data(ls_product_category) with key hierarchy_id = 'R3PRODHIER'.
    if sy-subrc = 0.
      ls_output-category_id = ls_product_category-category_id.
    endif.

    call function 'CRM_PRODUCT_GETDETAIL_API'
      exporting
        is_product       = ls_product
        iv_all_interlink = 'X'
      importing
        es_interlinkages = lt_interlinkages.

    if lines( lt_interlinkages-comm_il_prdqfr ) gt 0.
      loop at lt_interlinkages-comm_il_prdqfr into data(ls_comm_il_prdqfr).

*       qualification id
        select single * from t777guid1
          into @ls_t777guid1
          where guid = @ls_comm_il_prdqfr-data-destinguid.

        ls_output-objid = ls_t777guid1-objid.

*       qualification text
        call function 'RH_READ_OBJECT'
          exporting
            plvar     = ls_t777guid1-plvar
            otype     = ls_t777guid1-otype
            objid     = ls_t777guid1-objid
            langu     = sy-langu
          importing
            stext     = ls_output-stext
          exceptions
            not_found = 1
            others    = 2.

        insert ls_output into table lt_output.
      endloop.
    else.
      insert ls_output into table lt_output.
    endif.

  endloop.

  if lt_output is initial.
    message text-m02 type 'E'. " no qualifications found
  endif.

* list output

* Create Instance of ALV
  call method cl_salv_table=>factory
    importing
      r_salv_table = gr_table
    changing
      t_table      = lt_output.

* ALV menu, etc
  data(lr_functions) = gr_table->get_functions( ).
  lr_functions->set_all( abap_true ).

  data(lr_display) = gr_table->get_display_settings( ).
  lr_display->set_striped_pattern( abap_true ).

* Display ALV
  gr_table->display( ).
