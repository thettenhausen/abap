function z_get_idocno_by_segm_data.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_CRETIM_LOW) TYPE  EDI_CCRTIM
*"     REFERENCE(IV_CRETIM_HIGH) TYPE  EDI_CCRTIM
*"     REFERENCE(IV_CREDAT) TYPE  EDI_CCRDAT
*"     REFERENCE(IV_UPDTIM_LOW) TYPE  EDI_UPDTIM DEFAULT '000000'
*"     REFERENCE(IV_UPDTIM_HIGH) TYPE  EDI_UPDTIM DEFAULT '240000'
*"     REFERENCE(IV_UPDDAT) TYPE  EDI_UPDDAT OPTIONAL
*"     REFERENCE(IV_DIRECT) TYPE  EDI_DIRECT DEFAULT '1'
*"     REFERENCE(IV_IDOCTP) TYPE  EDI_IDOCTP
*"     REFERENCE(IV_MESTYP) TYPE  EDI_MESTYP
*"     REFERENCE(IV_RCVPOR) TYPE  EDI_RCVPOR
*"     REFERENCE(IV_RCVPRT) TYPE  EDI_RCVPRT
*"     REFERENCE(IV_RCVPRN) TYPE  EDI_RCVPRN
*"     REFERENCE(IV_SEGMENT) TYPE  EDILSEGTYP
*"     REFERENCE(IV_FIELD) TYPE  EDI_FIELD
*"     REFERENCE(IV_VALUE) TYPE  EDI_VALUE
*"  EXPORTING
*"     REFERENCE(ES_EDIDC) TYPE  EDIDC
*"     REFERENCE(EV_SEGNUM) TYPE  EDI_SEGNUM
*"     REFERENCE(EV_STATXT) TYPE  EDI_TEXT60
*"     REFERENCE(EV_PARTNR) TYPE  EDI_PART
*"     REFERENCE(EV_IDENT) TYPE  EDI_WHOAMI
*"  EXCEPTIONS
*"      NO_IDOCS_FOUND
*"      NO_IDOCS_FOUND_WITH_THIS_VALUE
*"----------------------------------------------------------------------

* This function module reads the newest IDoc for a given
* value / field / segment combination. This can be used to search for
* IDocs for e.g. a specific OneOrder document by using the parameters
* segment = 'E1EDK01', field = 'BELNR', value = document number and
* the general parameters idoctp = 'ORDERS05', mestyp = 'ORDRSP', etc.

* The function module was written to be called from an action that
* sends data to a 3rd party via EDI, so that the actually used IDOC
* could be logged in the action log.

* Logic is based on SAP report RSEIDOC9.


  data: cretim    type range of edi_ccrtim,
        credat    type range of edi_ccrdat,
        updtim    type range of edi_updtim,
        upddat    type range of edi_upddat,
        direct    type range of edi_direct,
        idoctp    type range of edi_idoctp,
        mestyp    type range of edi_mestyp,
        rcvpor    type range of edi_rcvpor,
        rcvprt    type range of edi_rcvprt,
        rcvprn    type range of edi_rcvprn.

  data: ls_cretim like line of cretim,
        ls_credat like line of credat,
        ls_updtim like line of updtim,
        ls_upddat like line of upddat,
        ls_direct like line of direct,
        ls_idoctp like line of idoctp,
        ls_mestyp like line of mestyp,
        ls_rcvpor like line of rcvpor,
        ls_rcvprt like line of rcvprt,
        ls_rcvprn like line of rcvprn.


  data: lt_edidc        type table of edidc,
        ls_edidc        type edidc,
        lt_edidd        type table of edidd,
        ls_edidd        type edidd,
        lt_edisegstru   type table of edisegstru,
        ls_edisegstru   type edisegstru,
        lv_expleng      type i,
        lv_offset       type syfdpos.

  data: lv_segment_found  type crmt_boolean.

  data: ld_snd    type edi_part,
        ld_rcv    type edi_part.


* Data Preparation - create ranges for the SELECT
  ls_cretim-low = iv_cretim_low.
  ls_cretim-high = iv_cretim_high.
  ls_cretim-sign = 'I'.
  ls_cretim-option = 'BT'.
  insert ls_cretim into table cretim.

  ls_credat-low = iv_credat.
  ls_credat-sign = 'I'.
  ls_credat-option = 'EQ'.
  insert ls_credat into table credat.

  ls_updtim-low = iv_updtim_low.
  ls_updtim-high = iv_updtim_high.
  ls_updtim-sign = 'I'.
  ls_updtim-option = 'BT'.
  insert ls_updtim into table updtim.

  ls_idoctp-low = iv_idoctp.
  ls_idoctp-sign = 'I'.
  ls_idoctp-option = 'EQ'.
  insert ls_idoctp into table idoctp.

  ls_mestyp-low = iv_mestyp.
  ls_mestyp-sign = 'I'.
  ls_mestyp-option = 'EQ'.
  insert ls_mestyp into table mestyp.

  ls_direct-low = iv_direct.
  ls_direct-sign = 'I'.
  ls_direct-option = 'EQ'.
  insert ls_direct into table direct.

  ls_rcvpor-low = iv_rcvpor.
  ls_rcvpor-sign = 'I'.
  ls_rcvpor-option = 'EQ'.
  insert ls_rcvpor into table rcvpor.

  ls_rcvprt-low = iv_rcvprt.
  ls_rcvprt-sign = 'I'.
  ls_rcvprt-option = 'EQ'.
  insert ls_rcvprt into table rcvprt.

  ls_rcvprn-low = iv_rcvprn.
  ls_rcvprn-sign = 'I'.
  ls_rcvprn-option = 'EQ'.
  insert ls_rcvprn into table rcvprn.


* Get all IDocs that are potential solutions
  select * from edidc into table lt_edidc
         where   upddat >= iv_credat    "because of index on EDIDC
             and cretim in cretim
             and credat in credat
             and updtim in updtim
             and upddat in upddat
             and direct in direct
             and idoctp in idoctp
             and mestyp in mestyp
             and rcvpor in rcvpor
             and rcvprt in rcvprt
             and rcvprn in rcvprn
         order by credat cretim descending.

  if sy-subrc <> 0.
    raise no_idocs_found.
  endif.


* read the given segment structure of the IDoc type to search
* for the specific value
  call function 'SEGMENT_READ'
    exporting
      segmenttyp           = iv_segment
    tables
      segmentstructure     = lt_edisegstru
    exceptions
      no_authority         = 1
      segment_not_existing = 2
      others               = 3.

  if sy-subrc is not initial.
  endif.

* get the required offset of the field to find it later on
  loop at lt_edisegstru into ls_edisegstru.
    if ls_edisegstru-fieldname = iv_field.
      lv_expleng = ls_edisegstru-expleng.
      exit.
    endif.
    add ls_edisegstru-expleng to lv_offset.
  endloop.



* searching for the value in the given segment
* (e.g. order number in field BELNR)
  loop at lt_edidc into ls_edidc.

    call function 'IDOC_READ_COMPLETELY'
      exporting
        document_number         = ls_edidc-docnum
      tables
        int_edidd               = lt_edidd
      exceptions
        document_not_exist      = 1
        document_number_invalid = 2
        others                  = 3.

    loop at lt_edidd into ls_edidd
    where segnam = iv_segment.

      find iv_value in section offset lv_offset length lv_expleng
          of ls_edidd-sdata ignoring case.

      if sy-subrc = 0.
        es_edidc = ls_edidc.
        ev_segnum = ls_edidd-segnum.
        lv_segment_found = 'X'.
        exit. " max one segment per idoc
      endif.
    endloop.

    if lv_segment_found = 'X'.
      exit.
    endif.
  endloop.

  if lv_segment_found is initial.
    raise no_idocs_found_with_this_value.
  endif.


* Read additional data for output (status longtext)
  select single descrp from teds2 into ev_statxt
    where langua = sy-langu
      and status = ls_edidc-status.

  move '  /  /' to: ld_snd, ld_rcv.
  move: ls_edidc-sndprt to ld_snd(2),
        ls_edidc-sndpfc to ld_snd+3(2),
        ls_edidc-sndprn to ld_snd+6(10),
        ls_edidc-rcvprt to ld_rcv(2),
        ls_edidc-rcvpfc to ld_rcv+3(2),
        ls_edidc-rcvprn to ld_rcv+6(10).

  if ls_edidc-direct = '1'.
    move: ld_rcv to ev_partnr,
          ld_snd to ev_ident.
  else.
    move: ld_snd to ev_partnr,
          ld_rcv to ev_ident.
  endif.

endfunction.
