pro I6_batch_avirisng2017_lsuclass


  envi, /restore_base_save_files
  envi_batch_init, log_file='batch.log'

  startfile = 15        ; begin processing at this file number
  endfile   = 60        ; end   processing at this file number

  ; directory with input image folders
  infolder     = 'X:\delta_sav\raster\coreg_orig\'
  ; directory to output index images
  outdirectory = 'X:\delta_sav\raster\classification\SAM_SMA\201711\SMA\'
  ; spectral library path and filename
  slifilename  = 'X:\delta_sav\raster\classification\SAM_SMA\201711\SMA\supporting_files\SMA_spec_lib_2017.sli'
  ; directory with mask images
  mskdirectory = 'X:\delta_sav\raster\masks\201711\fl_edge\'
  ; bad bands file path and name
  badbandfilename = 'X:\delta_sav\raster\classification\SAM_SMA\201711\SMA\supporting_files\SMA_badbands_2017.csv'

  wildcard = '*'
  dirpre = 'Delta_'               ; common prefix string for image directories
  insuf  = '_v2p13_rot_unstack_fr_2_REGaffine.bsq'      ; input  images suffix
  msksuf = '_rot_msk.img'         ; mask   images suffix
  outsuf = '_sma.img'             ; output images suffix
  slisuffix = 'sli'               ; spectral library suffix

  ; array containing all the folder names to be processed
  folderarray = file_search(infolder + dirpre + wildcard)

  mskimgarray = file_search(mskdirectory + wildcard + msksuf)

  ; total number of images to be processed
  numfolders = n_elements(folderarray)

  endfile = numfolders - 1

  ;print, 'total number of images: ', numfolders

  envi_open_file, slifilename, r_fid=slifid
  envi_file_query, slifid, nl = num_endmemb, ns = nb, spec_names = spec_names
  spec_names_exp = strarr(num_endmemb+1)
  spec_names_exp[0:num_endmemb-1] = spec_names
  spec_names_exp[num_endmemb] = 'RMS Error'
  ;spec_names_class = strarr(num_endmemb+1)
  ;spec_names_class[1:num_endmemb] = spec_names
  ;spec_names_class[0] = 'Unclassified'

  slidb=fltarr(nb,num_endmemb)
  pos=indgen(nb)
  badbands = read_ascii(badbandfilename, delimiter = ",")
  badbands = badbands.FIELD1
  badbands = badbands[1,*]
  goodpos = pos[where(badbands eq 1)]
  lsupos = indgen(num_endmemb)

  for i = 0,num_endmemb-1 do begin
      slidb[*,i] = envi_get_slice(fid=slifid,line=i)
  endfor
  slidbgood = slidb[goodpos,*]

  ; initialize count of files processed
  count = 0
  procfile_no = -1   ; to keep track of actual files processed - also mask file

  for i = startfile, endfile do begin
    
    count = count + 1
    
    ; input image name
    imagename = file_search(folderarray[i] + '\', wildcard + insuf)
    basename  = file_basename(imagename)

    if (file_test(imagename) eq 0) then begin
        print, "Input file does not exist: ", i
        continue
    endif else begin
      procfile_no = procfile_no + 1
    endelse
    
    ;procfile_no = 16
    
    print, "Processing file: ", count, "   Name: ", basename
    print, "Mask file is: ", mskimgarray[procfile_no]

    ; generate output image name
    outname = outdirectory + STRMID(basename, 0, STRPOS(basename, insuf, /reverse_search)) + outsuf

    ; check if SMA file already exists
    if (file_test(outname) eq 1) then continue

    envi_open_file, mskimgarray[procfile_no], r_fid = m_fid
    envi_open_file, imagename, r_fid=fid
    envi_file_query,fid,nl=imagenl,ns=imagens
    
    dims = [-1l, 0, imagens-1, 0, imagenl-1]
    
    envi_doit,'unmix_doit', fid = fid, pos = goodpos, dims = dims, endmem = slidbgood, out_name = outname, $
               out_bname = spec_names_exp, r_fid = lsufid, in_memory = 0, m_fid = m_fid, m_pos = 0
    print, 'completed processing file: ', basename
  
    envi_file_mng, id=lsufid, /remove
    envi_file_mng, id=fid, /remove
 
   endfor

end