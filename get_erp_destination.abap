* sample code to retrieve the default ERP RFC destination when in CRM

data: lc_sitetypeid_r3 type smw1stid value 'SMOF_ERPSITE',
      ls_smof_erpsh    type smof_erpsh,
      lt_erpsites      type standard table of smof_erpsh
      lv_param         type char1.

  call function 'SMOF_READ_SMOFERPSH'
    exporting
      i_sitetypeid = lc_sitetypeid_r3
    importing
      e_smof_erpsh = ls_smof_erpsh
    tables
      t_erpsites   = lt_erpsites.

call function 'Z_REMOTE_FUNCTION'
  destination ls_smof_erpsh-rfcdest
    exporting
      iv_param              = lv_param
    exceptions
      system_failure        = 1
      communication_failure = 2
      others                = 2.
