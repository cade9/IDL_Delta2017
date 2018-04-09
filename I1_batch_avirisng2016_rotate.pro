Pro I1_batch_avirisng2016_rotate

  envi, /restore_base_save_files
  envi_batch_init, log_file='batch.log'

  ;outbands  = 3          ; number of output bands
  inbands   = 432        ; number of input  bands

  ; major band positions B - 0, G - 4, R - 6, NIR1 - 10, NIR2 - 14, SWIR1 - 17, SWIR2 - 20
  ;postru = [20, 42, 66]
  ;poscir = [42, 66, 91]
  
  ; position vectors for the three contrem files generation
  posall = indgen(inbands)
  
  startfile = 0         ; begin processing at this file number
  endfile   = 0         ; end   processing at this file number
  mask_value = -0.005   ; ignore masked area if mask value is mentioned

  ; directory with input image folders
  infolder = 'X:\avirisng\2016\L2\'
  ; directory to output rotated images
  rotdirectory = 'X:\scratch\ang\Delta2016\trial\'
  ; directory to output quicklook images
  ;cirdirectory = 'X:\scratch\apollo-z4\shruti\delta_sav\201510\quicklooks\CIR\'
  ;trudirectory = 'X:\scratch\apollo-z4\shruti\delta_sav\201510\quicklooks\True\'

  wildcard = '*'
  dirsuf = '_v1n2'        ; common end string for image directories
  inpsuf = '_corr_v1n2_img'         ; input   images suffix
  rotsuf = '_rot.img'     ; rotated images suffix
  ;cirsuf = '_cir.tif'    ; CIR  quicklook image suffix
  ;trusuf = '_tru.tif'    ; True quicklook image suffix

  ; array containing all the folder names to be processed
  folderarray = file_search(infolder + wildcard + dirsuf)
  ; total number of images to be processed
  numfolders = n_elements(folderarray)

  print, 'total number of images: ', numfolders

  ; set endfile value to process all images
  endfile = numfolders - 1

  count = startfile

  for l = startfile, endfile do begin

    count = count+1

    ; input image name
    imagename = file_search(folderarray[l] + '\', wildcard + inpsuf)
    basename  = file_basename(imagename[0])

    print, "Processing file: ", count, "   Name: ", basename

    ; generate output image name
    rotname = rotdirectory + STRMID(basename,0,STRPOS(basename, inpsuf, /reverse_search)) + rotsuf
    ;truname = trudirectory + STRMID(basename,0,STRPOS(basename, inpsuf, /reverse_search)) + trusuf
    ;cirname = cirdirectory + STRMID(basename,0,STRPOS(basename, inpsuf, /reverse_search)) + cirsuf

    ; check if index file already exists
    if (file_test(rotname) eq 1) then continue
    
    ; open image file for processing
    envi_open_file, imagename[0], r_fid=fid    
    envi_file_query, fid, nl = imagenl, ns = imagens, nb = imagenb, wl = wl, dims=dims
    
    ; get map info from input images
    map_info = envi_get_map_info(fid=fid)

    envi_doit, 'rotate_doit', fid=fid, pos=posall, dims=dims, out_name=rotname, r_fid=rotfid, rot_type=3
    ;envi_output_to_external_format, fid=rotfid, dims=dims, out_name=truname, pos=trupos, /TIFF
    ;envi_output_to_external_format, fid=rotfid, dims=dims, out_name=cirname, pos=cirpos, /TIFF

    envi_file_mng, id=fid,    /REMOVE
    envi_file_mng, id=rotfid, /REMOVE

  endfor ; end l
  
  envi_batch_exit

  return

End ;of Main Procedure

