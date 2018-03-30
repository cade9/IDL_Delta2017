Pro batch_build_2018_mask_zero

; input directory
indirectory  = 'X:\delta_sav\raster\coreg_orig\'
; output mask directory
cirdirectory = 'X:\delta_sav\raster\masks\201711\fl_edge\'

; mask value for black edges that is present in the two images
;mv_yr1 = -0.005   ; in this case, it is the 2014 image
mv_yr1 = -2000   ; in this case, it is the 2017 image
mv_def = 0

; input image and output image suffixes and prefixes
wildcard  = '*'
insuf     = '_v2p13_rot_unstack_fr_2_REGaffine.bsq'   ; rotated images suffix
cirsuf    = '_v2p13_rot_msk.img'                      ; CIR  quicklook image suffix

; start processing at startfile and end at endfile
startfile = 0
endfile   = -1

; populate the input image name array
folderarray = file_search(indirectory + 'Delta_fl' + wildcard)
; number of folders
numfolders = n_elements(folderarray)
; adjust endfile


if endfile lt startfile then endfile = numfolders - 1

print, 'total number of folders : ', numfolders
print, 'total number of images to be processed: ', (endfile - startfile) + 1

count = startfile

pos = [20,40,60]

iparr1 = fltarr(100, 3)    ; input array of first  image
oparr = bytarr(100)        ; output array

for l = startfile, endfile do begin

    count = count+1
    print, "Processing folder: ", count, "   Name: ", folderarray[l]
   
    imagenames = file_search(folderarray[l] + '\' + wildcard + insuf)
    if imagenames eq '' then continue
    basename   = file_basename(imagenames)

    print, "Processing file: ", count, "   Name: ", basename

    ; generate output image name
    cirname = cirdirectory + STRMID(basename,0,STRPOS(basename, insuf, /reverse_search)) + cirsuf

    ; check if index file already exists
    if (file_test(cirname) eq 1) then continue
    
    ; open input files for both years
    envi_open_file, imagenames, r_fid = fid1
    ;envi_open_file, inFiles[2*l + 1], r_fid = fid2
  
    ; get header parameters
    envi_file_query, fid1, nl = imagenl, ns = imagens, dims = dims, interleave = inter
    
    ; get map info
    map_info = envi_get_map_info(fid = fid1)
    
    ; arrays to hold input and output values
    iparr1 = congrid(iparr1, imagens, 3)
    oparr  = congrid(oparr, imagens)

    ; initialize output array
    oparr[*] = 0
    
    ; open output file for writing
    openw, ofid, cirname, /get_lun

    for j = 0, imagenl-1 do begin
        iparr1 = envi_get_slice(fid = fid1, line = j, pos = pos)
        for i = 0, imagens-1 do begin
          ; is pixel masked in year 1 image?
         	if ((iparr1[i, 0] lt mv_yr1) and (iparr1[i, 1] lt mv_yr1) and (iparr1[i, 2] lt mv_yr1)) then mask_2014 = 0 else mask_2014 = 1
          if ((iparr1[i, 0] eq mv_def) and (iparr1[i, 1] eq mv_def) and (iparr1[i, 2] eq mv_def)) then mask_gn14 = 0 else mask_gn14 = 1
          ; is pixel masked in either image - then it is masked for coregistration
         	if (mask_2014 eq 0 or mask_gn14 eq 0) then oparr[i] = 0 else oparr[i] = 1 
        endfor     ; end j
        ; write into output file
        writeu, ofid, oparr
    endfor     ; end i

    free_lun, ofid

    ; name of the mask band
    bnames = "flight line edge"
    ; setup head of new mask file
    envi_setup_head, fname=cirname, ns=imagens, nl=imagenl, nb=1, interleave=inter, map_info=map_info, bnames=bnames, data_type=1, /write
    
    print, 'written output mask file: ', l+1
    
    envi_file_mng, id=fid1, /remove

endfor   ; end l

return

End ;of Main Procedure