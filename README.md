# abap
ABAP code samples

This is (or rather: will be) a collection of ABAP code samples that I created over the years. They just might be useful somewhere else for me or you, so...

## get_erp_destination.abap
Sample code to retrieve the default ERP RFC destination when in CRM.

## z_check_background_process.abap
Generic function module to check whether the current processing is running in the background.

## z_conf_tech_grp_fix.abap
Checks given service confirmations for maintained service technician group (partner function 00000056). If not found tries to read it from technician (Z0000006) and add it.
Fixes a problem where confirmations from a mobile solution were missing this specific partner function after a GoLive due to partners  hanging in the middleware.

## z_create_pdf.abap
Creates a PDF as Xstring from an incoming Xstring

## z_get_idocno_by_segm_data.abap
This function module reads the newest IDoc for a given value / field / segment combination. This can be used to search for IDocs for e.g. a specific OneOrder document by using the parameters segment = 'E1EDK01', field = 'BELNR', value = document number and the general parameters idoctp = 'ORDERS05', mestyp = 'ORDRSP', etc.
The function module was written to be called from an action that sends data to a 3rd party via EDI, so that the actually used IDOC could be logged in the action log.

## z_prod_qual_list.abap
Finds all relevant products for a given sales org and then reads the qualifications that are maintained for this product, together with description, and the product hierarchy for easy checking of correctness.
Reason for this report: technician  qualifications for products were maintained based on product hierarchy, but some had the wrong  or missing ones. This made it easy to find for business what had to be corrected.

## z_upload_test_data.abap
This report uploads a csv file and tries to insert its content into the given database table. It will NOT check for authorisation or if the data is valid. Also it will OVERWRITE old data, so be CAREFUL!
