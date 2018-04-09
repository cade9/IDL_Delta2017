pro I3_batch_create_mask_by_evf

  ; ############################################################
  ; #### create masks based on watermask evf vector file    ####
  ; #### works on ENVI 4.8 - uses undocumented routine      ####
  ; ############################################################

  compile_opt idl2     
  
  ; open vector file to use for subset and get id
  evf_fname = 'X:\delta_sav\vector\field_data\2017\background_files\watermask\p1_watermask_2017_.evf'
  
  ; make sure envi vector routine is available
  envi_check_save, /vector 
  
  ; open the vector and get its ID
  evf_open, evf_fname, vec = vec, /no_warning
  evf_id = vec.id  
  
  ; input directory
  indirectory  = 'X:\delta_sav\raster\masks\201711\fl_edge\'
  ; water mask directory
  wmdirectory  = 'X:\delta_sav\raster\masks\201711\watermask\'
  ; output final mask directory
  outdirectory = 'X:\delta_sav\raster\masks\201711\finalmask\'

  ; input image and output image suffixes
  wildcard  = '*'
  insuf     = 'msk.img'
  wmsuf     = 'watmsk.img'
  outsuf    = 'fnlmsk.img'

  ; start processing at startfile and end at endfile
  startfile = 0
  endfile   = -1

  ; populate the input image name array
  imgarray = file_search(indirectory + wildcard + insuf)
  ; number of images
  numimages = n_elements(imgarray)
  
  ; adjust endfile
  if endfile lt startfile then endfile = numimages-1

  print, 'total number of images: ', numimages
  print, 'total number of images to be processed: ', (endfile - startfile) + 1

  for l = startfile, endfile do begin
  
    ; open input file (flightline edge image just for extent)
    envi_open_file, imgarray[l], r_fid = fid
    if (fid eq -1) then begin
      print, 'unable to print file ... aborting ...', imgarray[l]
    endif
    basename = file_basename(imgarray[l])
    print, "Processing file : ", basename

    ; create names of output files
    wmname  =  wmdirectory + STRMID(basename,0,STRPOS(basename, insuf, /reverse_search)) +  wmsuf
    outname = outdirectory + STRMID(basename,0,STRPOS(basename, insuf, /reverse_search)) + outsuf
    
    ; get input file information
    envi_file_query, fid, ns = ns, nl = nl, nb = nb, dims = dims
    pos = lindgen(nb)  

    ; generate a mask from the evf (undocumented routine) - 
    ; you need to mask areas outside of EVF boundaries if the EVF is not a rectangle
    envi_mask_doit, evf_id = evf_id, evf_fid = fid, ns = ns, nl = nl, and_or = 0, /inside, /in_memory, r_fid = m_fid
         
    ; output watermask file in memory to disk
    envi_doit, 'CF_DOIT', dims = dims, fid = m_fid, out_name = wmname, pos = pos, r_fid = wm_fid, /remove

    ; apply this just produced mask to the other mask file
    envi_doit, 'ENVI_MASK_APPLY_DOIT', dims = dims, fid = fid, m_fid = wm_fid, m_pos = 0, out_name = outname, pos = 0, value = 0, r_fid = ofid

    ; Calculate enhanced NDVI
    ;expression = 'float((b4 + b2) - 2*b1) / float((b4 + b2) + 2*b1)'
    ;bandMathRaster = ENVIPixelwiseBandMathRaster(raster, expression)

    ; remove files from ENVI
    envi_file_mng, id = wm_fid, /remove
    envi_file_mng, id = fid,    /remove
    envi_file_mng, id = ofid,   /remove
    
  endfor
  
  envi_evf_close, evf_id
  
return 
  
END
