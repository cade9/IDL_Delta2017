Pro batch_avirisng2017_cr

  envi, /restore_base_save_files
  envi_batch_init, log_file='batch.log'

  inbands   = 425        ; number of input  bands
    
  ; position vectors for the three contrem files generation
  poscomp = indgen(inbands)
  ; continuum removal positions for water     absorption at ~980nm
  crpos1 = poscomp[113:141]
  ; continuum removal positions for water     absorption at ~1200nm
  crpos2 = poscomp[150:180]
  ; continuum removal positions for cellulose absorption at ~2100nm
  crpos3 = poscomp[332:367]
  
  startfile = 0          ; begin processing at this file number
  endfile   = -1         ; end   processing at this file number
  mask_value = -9999     ; ignore masked area if mask value is mentioned
    
  ; directory with input image folders
  infolder = 'X:\scratch\ang\Delta2017\masked_coreg_orig\'
  ; directory to output continuum removal images
  crdirectory  = 'X:\delta_sav\raster\classification\contrem\201711\'

  wildcard = '*'
  ; dirpre = 'Delta'                   ; common start string for image directories
  insuf  = '_mskd.img'                 ; input  images suffix
  crsuf1 = '_smsz_crw1.img'            ; suffix for continuum removal at 980 nm  (water)
  crsuf2 = '_smsz_crw2.img'            ; suffix for continuum removal at 1200 nm (water)
  crsuf3 = '_smsz_crc.img'             ; suffix for continuum removal at 2300 nm (cellulose)

  ; array containing all the folder names to be processed
  ;folderarray = file_search(infolder + dirpre + wildcard)
  filearray = file_search(infolder + wildcard + insuf)
  ; total number of images to be processed
  ;numfolders = n_elements(folderarray)
  numfolders = n_elements(filearray)
  
  print, 'total number of images: ', numfolders

  ; set endfile value to process all images
  if endfile lt startfile then endfile = numfolders - 1

  count = startfile
    
  for l = startfile, endfile do begin

    count = count+1

    ; input image name
    ;imagename = file_search(folderarray[l] + '/', wildcard + insuf)
    imagename = filearray[l]
    ;if (n_elements(imagename) lt 3) then continue
    basename  = file_basename(imagename[0])

    print, "Processing file: ", count, "   Name: ", basename

    ; open image file for processing
    envi_open_file, imagename[0], r_fid=fid
    envi_file_query, fid, nl = imagenl, ns = imagens, dims=dims
    
    ; get map info from input images
    map_info = envi_get_map_info(fid=fid)

    ; generate continuum removal filenames
    crname1 = crdirectory + STRMID(basename,0,STRPOS(basename, insuf, /reverse_search)) + crsuf1
    crname2 = crdirectory + STRMID(basename,0,STRPOS(basename, insuf, /reverse_search)) + crsuf2
    crname3 = crdirectory + STRMID(basename,0,STRPOS(basename, insuf, /reverse_search)) + crsuf3
      
    if (file_test(crname1) eq 0) then begin
      envi_doit, 'continuum_remove_doit', fid=fid, pos=crpos1, dims=dims, out_name=crname1, r_fid=cr1
      envi_file_mng, id=cr1, /REMOVE
    endif
    if (file_test(crname2) eq 0) then begin
      envi_doit, 'continuum_remove_doit', fid=fid, pos=crpos2, dims=dims, out_name=crname2, r_fid=cr2
      envi_file_mng, id=cr2, /REMOVE
    endif
    if (file_test(crname3) eq 0) then begin
      envi_doit, 'continuum_remove_doit', fid=fid, pos=crpos3, dims=dims, out_name=crname3, r_fid=cr3
      envi_file_mng, id=cr3, /REMOVE
    endif
      
    print, 'written continuum removal files: ', l+1

    ; close files
    envi_file_mng, id=fid, /REMOVE

endfor   ; end l

envi_batch_exit

return

End ;of Main Procedure

