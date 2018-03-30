Pro batch_mask_apply_original_image

  ; ##############################################################################
  ; applying single mask file to a bunch of files exactly the same size and extent
  ; ##############################################################################
  

  envi, /restore_base_save_files
  envi_batch_init, log_file='batch.log'

  startfile  = 12        ; begin processing at this file number
  endfile    = 15        ; end   processing at this file number
  mask_value = -9999     ; give this value to masked area

  ; directory with classification files
  indirectory   = 'X:\delta_sav\raster\coreg_orig\'
  ; directory to output masked index images
  outdirectory  = 'X:\scratch\ang\Delta2017\masked_coreg_orig\'
  
  ; common mask file
  mskfilename = 'X:\delta_sav\raster\masks\201711\fl_edge\'

  wildcard = '*'
  fldsuf   = 'Delta_fl'
  insuf    = 'v2p13_rot_unstack_fr_2_REGaffine.bsq'      ; input  image suffix
  msksuf   = 'rot_msk.img'                               ; mask   image suffix
  outsuf   = 'v2p13_rot_mskd.img'                        ; output image suffix

  ; get all image names
  fld_arr  = file_search(indirectory + fldsuf + wildcard)
  msk_list = file_search(mskfilename + wildcard + msksuf)
  
  ; total number of images to be processed
  numimages = n_elements(msk_list)
  
  ; set endfile value to process all images
  if endfile < startfile then endfile = numimages-1 

  print, 'total number of images: ', numimages

  count = 0
  
  for l = startfile, endfile do begin

    count = count+1

    
    envi_open_file,  msk_list[l], r_fid = mfid
 
    ; input image name
    basename = file_basename(msk_list[l])
    print, "Processing file: ", count, "   Name: ", basename
    
    ; get flightline ID and match it to folder for original file and generate filename
    flID = strmid(basename, 4, 2)     ; get flightline ID
    fld_name = fldsuf + flID          ; produce folder name
    pathall = indirectory + fldsuf + flID + "\"
    img_name = file_search(pathall + wildcard + insuf)
    
    outname = outdirectory + STRMID(basename, 0, STRPOS(basename, msksuf, /reverse_search)) + outsuf

    ; check if masked output file already exists
    if (file_test(outname) eq 1) then continue
    
    ; open image file for processing
    envi_open_file,  img_name, r_fid = ifid
    envi_file_query, ifid, dims = dims, nb = nb
    pos = indgen(nb)
        
    envi_doit, 'ENVI_MASK_APPLY_DOIT', dims = dims, fid = ifid, m_fid = mfid, m_pos = 0, out_name = outname, pos = pos, value = mask_value, r_fid = ofid
                  
    print, 'completed processing file: ', l+1, outname
    
    ; close the original file
    envi_file_mng, id = ifid, /REMOVE
    envi_file_mng, id = mfid, /REMOVE
    envi_file_mng, id = ofid, /REMOVE

endfor   ; end l

envi_batch_exit

return

End ;of Main Procedure

