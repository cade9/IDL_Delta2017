pro batch_avirisng2017_tif_to_enviclass_v2

  ;###########################################################################
  ;######## A program to convert geoTIFF files to ENVI classification ########
  ;###########################################################################
  
  envi, /restore_base_save_files
  envi_batch_init, log_file='batch.log'
  
  ; input directory
	indir  = 'X:\delta_sav\raster\classification\methods\201711\tiffs\'
	mskdir = 'X:\delta_sav\raster\masks\201711\finalmask\'
  ; output directory
	outdir = 'X:\delta_sav\raster\classification\methods\201711\v4\'
	
	wildcard = '*'
	imgsuf = 'RFDeltav4.tif'  ; common suffix for input  classification tiff files
	outsuf = '_class_v4.img'  ; common suffix for output classification envi files
	msksuf = 'fnlmsk.img'     ; common suffix for watermask file

	imagenamearray = file_search(indir + wildcard + imgsuf)
	numimages = n_elements(imagenamearray)
  masknamearray = file_search(mskdir + wildcard + msksuf)

  startimg = 0
  endimg   = numimages-1
  ;endimg = -1
  
  ;print, imagenamearray[16]
  
  ftype = 3
  nclasses = 10
  cnames = ['unclassified', 'CAT',        'NPV',        'RIP',      'SAV',     'Soil',      'TUL',       'WAT',         'WHY',       'WPR']
  lukup  = [[0,0,0],       [0,150,0], [255,230,200], [0, 255, 0], [255,0,0], [100,50,0], [150,150,0], [0, 255, 255], [255,255,0],  [0,0,255]]
  
  pos = 0
  
  ; populate the imagenamearray
	for i = startimg, endimg do begin
	
	  ; get basename of input file
	  basename = file_basename(imagenamearray[i])
	  flightid = strmid(basename, 3, 2)
	  print, flightid
    
    for j = 0, 21 do begin
      if strmid(file_basename(masknamearray[j]), 4, 2) eq flightid then mskname = masknamearray[j]
    endfor

	  ; generate the output image filename
	  outname  = outdir + STRMID(basename, 0, STRPOS(basename, imgsuf, /reverse_search)) + outsuf
	  
    if (~(file_test(outname))) then begin
	     ; open the input image
       envi_open_data_file, imagenamearray[i], r_fid = ifid, /TIFF
       ; query image properties
       envi_file_query, ifid, dims = dims, ns = imagens, nl = imagenl, nb = imagenb, data_type = dtype, $
                        bnames = bname, interleave = ilv
       ; get map info
       minfo = envi_get_map_info(fid = ifid)
       
       ; output to ENVI file
       envi_output_to_external_format, dims = dims, fid = ifid, out_name = outname, pos = pos, /ENVI
       
       ; setup header to make it an ENVI classification file 
       envi_setup_head, fname = outname, ns = imagens, nl = imagenl, nb = 1L, interleave = ilv, map_info = minfo, $
                        data_type = dtype, bnames = bname, num_classes = nclasses, class_names = cnames, $
                        file_type = ftype, lookup = lukup, byte_order = 0L, /write

       ; open mask file
       envi_open_file, mskname, r_fid = mfid
       envi_open_file, outname, r_fid = ofid
       
       basename = file_basename(mskname)
       
       outmskname  = outdir + 'masked\' + STRMID(basename, 0, STRPOS(basename, msksuf, /reverse_search)) + '_class_v2_mskd.img'
       
       ; apply this just produced mask to the other mask file
       envi_doit, 'ENVI_MASK_APPLY_DOIT', dims = dims, fid = ofid, m_fid = mfid, m_pos = 0, out_name = outmskname, pos = 0, value = 0

       ; setup header to make it an ENVI classification file 
       envi_setup_head, fname = outmskname, ns = imagens, nl = imagenl, nb = 1L, interleave = ilv, map_info = minfo, $
                        data_type = dtype, bnames = bname, num_classes = nclasses, class_names = cnames, $
                        file_type = ftype, lookup = lukup, byte_order = 0L, /write

       envi_file_mng, id=ifid, /REMOVE
       envi_file_mng, id=mfid, /REMOVE
       envi_file_mng, id=ofid, /REMOVE
        
    endif else begin
       print, i, ' - output file already exists: ', outname
    endelse
 
    ; print status report
    print, 'Completed processing of file: ', imagenamearray[i]
    
	endfor

envi_batch_exit

return

end