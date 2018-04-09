Pro I6_batch_avirisng2017_index

  ;###############################################################
  ;# This program opens all 3 continuum removal files, extracts  #
  ;# the minima band (predecided below), calculates indices and  #
  ;# stacks all bands, indices, and CR minimas into one file     #
  ;#   Last modified by Shruti Khanna on 21 February, 2018       #
  ;###############################################################

  envi, /restore_base_save_files
  envi_batch_init, log_file='batch.log'

  outbands  = 33         ; number of output bands
  inbands   = 425        ; number of input  bands in 2016 data
  usedbands = 24         ; number of bands used to calculate indices
  crposwat1 = 7          ; position of minima for first  water absorption
  crposwat2 = 8          ; position of minima for second water absorption
  crposcell = 13         ; position of mimima for cellulose    absorption
  
    pos = [14,  27,  31,  35,  36,  38,  43,  56,  60,  63,  64,  66,  74,  85, 108, 120, 135, 140, 163, 184, 259, 334, 340, 367] ; 24 bands for 2017 data
  ; index   0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23 
  ; bands 446, 512, 531, 552, 557, 567, 592, 657, 677, 692, 697, 707, 747, 802, 918, 978,1053,1078,1193,1298,1674,2050,2080,2215 
  ; major band positions B - 0, G - 3, R - 8, NIR1 - 13, NIR2 - 17, SWIR1 - 19, SWIR2 - 20, SWIR3 - 23
  
  ; need bands 21 and 103
  posext = [20, 102]
  
  posB = 14
  posG = 35
  posR = 60
  posNIR1 = 85
  posNIR2 = 140
  poSWIR1 = 259
  poSWIR2 = 367
  
  ; position vectors for the three contrem files generation
  poscomp = indgen(inbands)
  
  startfile = 0           ; begin processing at this file number
  endfile   = -1           ; end   processing at this file number (use -1 to processs all files)
  mask_value = -9999      ; ignore masked area if mask value is mentioned
  
  ;########### PSI INDICES NOT CALCULATED FOR 2017 DATA ############
  ;########### ignore slopes and intercepts below       ############
  
  ; slope of the SAV line for SVIS (bands 44 vs. 65 i.e. 43 vs. 64)
  slopes = 0.77412385
  ; intercept of the SAV line for SVIS
  inters = 0.00870287

  ; slope of the SAV line for SVIW (bands 57 vs. 65 i.e. 56 vs. 64)
  slopew = 0.893380000
  ; intercept of the SAV line for SVIW
  interw = 0.007103173
  
  ;####################### end ignore ###############################

  ; directory with input image folders
  infolder = 'X:\scratch\ang\Delta2017\masked_coreg_orig\'
  ; directory to output index images
  outdirectory = 'X:\delta_sav\raster\classification\indices\201711\'
  ; directory to output continuum removal images
  crdirectory  = 'X:\delta_sav\raster\classification\contrem\201711\'

  wildcard = '*'
  dirpre = 'Delta'                ; common start string for image directories
  insuf  = '_rot_mskd.img'        ; input  images suffix
  outsuf = '_indx.img'            ; output images suffix
  crsuf1 = '_crw1.img'            ; suffix for continuum removal at 980 nm  (water)
  crsuf2 = '_crw2.img'            ; suffix for continuum removal at 1200 nm (water)
  crsuf3 = '_crc.img'             ; suffix for continuum removal at 2300 nm (cellulose)

  ; array containing all the folder names to be processed
  ;folderarray = file_search(infolder + dirpre + wildcard)
  folderarray = file_search(infolder + wildcard + insuf)

  namearrcr1 = file_search(crdirectory + wildcard + crsuf1)
  namearrcr2 = file_search(crdirectory + wildcard + crsuf2)
  namearrcr3 = file_search(crdirectory + wildcard + crsuf3)
  
  ; total number of images to be processed
  numfolders = n_elements(folderarray)
  ;numfolders = n_elements(imagename)

  ; set endfile value to process all images
  if endfile lt startfile then endfile = numfolders - 1

  count = startfile
  
  print, 'total number of images: ', numfolders
  ;print, 'folder name : ', folderarray[startfile]

  iparr = fltarr(100, inbands)       ;input array
  oparr = fltarr(100, outbands)      ;output array
  crmin1 = fltarr(100)
  crmin2 = fltarr(100)
  crmin3 = fltarr(100)
  tmparr = fltarr(100, 2)            ; extra to add WAVI and NDAVI
  
  ;imagename = file_search(folderarray[startfile] + '/', wildcard + insuf)
  ;envi_open_file, imagename[0], r_fid=fid
  envi_open_file, folderarray[0], r_fid=fid
  envi_file_query, fid, nb = imagenb, wl = wl
  
  ; get wavelengths in micrometers for AVIRIS-NG
  anglewl = fltarr(5)
  anglewl[0] = float(wl[posG]/1000)       ; green wavelength
  anglewl[1] = float(wl[posR]/1000)       ; red   wavelength
  anglewl[2] = float(wl[posNIR1]/1000)    ; NIR   wavelength
  anglewl[3] = float(wl[poSWIR1]/1000)    ; SWIR1 wavelength
  anglewl[4] = float(wl[poSWIR2]/1000)    ; SWIR2 wavelength
  
  for l = startfile, endfile do begin

    count = count+1

    ; input image name
    ;imagename = file_search(folderarray[l] + '/', wildcard + insuf)
    ;imagename = folderarray[l]
    ;tfiles = n_elements(imagename)
    ;if tfiles lt 3 then continue
    
    namecr1 = namearrcr1[l]
    namecr2 = namearrcr2[l]
    namecr3 = namearrcr3[l]

    basename  = file_basename(namecr1)
    
    print, "Processing file: ", count, "   Name: ", basename

    ; generate output image name
    outname = outdirectory + STRMID(basename,0,STRPOS(basename, crsuf1, /reverse_search)) + outsuf

    ; check if index file already exists
    ;if (file_test(outname) eq 1) then begin
      ;print, "File already exists: ", outname
      ;continue
    ;endif
    
    ; open image file for processing
    ;envi_open_file, imagename[0], r_fid = fid
    envi_open_file, folderarray[l], r_fid = fid
    envi_open_file, namecr1,   r_fid = cr1
    envi_open_file, namecr2,   r_fid = cr2
    envi_open_file, namecr3,   r_fid = cr3
    
    envi_file_query, fid, nl = imagenl, ns = imagens, dims = dims
    
    ; get map info from input images
    map_info = envi_get_map_info(fid = fid)

    ; initial definition of input and output arrays
    iparr  = congrid(iparr, imagens, inbands)   ; input array
    oparr  = congrid(oparr, imagens, outbands)  ; output array
    crmin1 = congrid(crmin1, imagens)
    crmin2 = congrid(crmin2, imagens)
    crmin3 = congrid(crmin3, imagens)
    tmparr = congrid(tmparr, imagens, 2)

    ; open the index file for writing
    openw, ofid, outname, /get_lun
    
    for j = 0, imagenl-1 do begin

      iparr  = envi_get_slice(fid=fid, line=j, pos=pos)
      crmin1 = envi_get_slice(fid=cr1, line=j, pos=crposwat1)
      crmin2 = envi_get_slice(fid=cr2, line=j, pos=crposwat2)
      crmin3 = envi_get_slice(fid=cr3, line=j, pos=crposcell)
      tmparr = envi_get_slice(fid=fid, line=j, pos=posext)
      
      for i = 0, imagens-1 do begin

          oparr[i, *] = 0     ; initialize output variables
          pixel = iparr[i, *]
          
          if pixel[0] eq mask_value and pixel[1] eq mask_value and pixel[2] eq mask_value then continue
          if total(pixel) eq 0 then continue
          
          ; NDVI from bands, (677) and (1077) (R & NIR)
          oparr[i, 0] = diff_index(iparr[i, 17], iparr[i, 8])

          ; gNDVI (green ndvi) from bands, (557) and (677) (G & R)
          oparr[i, 1] = diff_index(iparr[i, 4], iparr[i, 8])
          
          ; R-G Ratio from bands (557) and (677) (G & R)
          oparr[i, 2] = float(iparr[i, 8])/float(iparr[i, 4])

          ; NDWI from bands, (1077) and (1674) (NIR & SWIR1)
          oparr[i, 3] = diff_index(iparr[i, 17], iparr[i, 20])

          ; NDWI2 from bands, (1077) and (2214) (NIR & SWIR2)
          oparr[i, 4] = diff_index(iparr[i, 17], iparr[i, 23])

          ; LPI (Leaf Pigment Index) from bands, (557) and (1077) (G & NIR)
          oparr[i, 5] = 1/float(10*iparr[i, 4]) - 1/float(10*iparr[i, 17])
          
          ; angle at NIR for bands, (677), (1077) and (1674) (R, NIR, SWIR1)
          oparr[i, 6] = calculate_angle(iparr[i, 8], iparr[i, 17], iparr[i, 20], anglewl[1], anglewl[2], anglewl[3])
          
          ; angle at Red for bands, (557), (677) and (1077) (G, R, NIR)
          oparr[i, 7] = calculate_angle(iparr[i, 4], iparr[i, 8], iparr[i, 17], anglewl[0], anglewl[1], anglewl[2])
          
          ; angle at SWIR1 for bands, (1077) and (1674) and (2214) (NIR, SWIR1, SWIR2)
          oparr[i, 8] = calculate_angle(iparr[i, 17], iparr[i, 20], iparr[i, 23], anglewl[2], anglewl[3], anglewl[4])
          
          ; mNDVI (red edge ndvi) from bands, (692) and (747)
          oparr[i, 9] = diff_index(iparr[i, 12], iparr[i, 9])

          ; GI (green index) from bands, (557)and (677) (G & R)
          oparr[i, 10] = diff_index(iparr[i, 4], iparr[i, 8])

          ; PRI from bands, (531) and (567)
          oparr[i, 11] = diff_index(iparr[i, 2], iparr[i, 5])

          ; CAI between bands (2049), (2079) and (2214)
          oparr[i, 12] = float(0.5*(iparr[i, 21] + iparr[i, 23]) - iparr[i, 22])
          
          ; WADI (Water Absorption Difference Index) between bands (1077) and (1298)
          oparr[i, 13] = diff_index(iparr[i, 17], iparr[i, 19])
          
          ; Absorption depth for water at 980 nm (ADW1) between bands (917), (977) and (1052)
          oparr[i, 14] = float(0.5*(iparr[i, 14] + iparr[i, 16]) - iparr[i, 15])
          
          ; Absorption depth for water at 1160 nm (ADW2) between bands (1077), (1193) and (1298)
          oparr[i, 15] = float(0.5*(iparr[i, 17] + iparr[i, 19]) - iparr[i, 18])
          
          ; Structure Insensitive Pigment index (SIPI) for bands (446), (677), (802) (B, R, NIR)
          oparr[i, 16] = float(iparr[i, 13] - iparr[i, 0])/float(iparr[i, 13] - iparr[i, 8])

          ; Carotenoid Reflectance Index (CRI_550) for bands (512) and (552)
          oparr[i, 17] = 1/(10*iparr[i, 1]) - 1/(10*iparr[i, 3])

          ; Carotenoid Reflectance Index (CRI_700) for bands (512) and (707)
          oparr[i, 18] = 1/(10*iparr[i, 1]) - 1/(10*iparr[i, 11])

          ; Anthocyanin Reflectance Index (ARI) for bands (552) and (707)
          oparr[i, 19] = 1/(10*iparr[i, 3]) - 1/(10*iparr[i, 11])
          
          ; Blue band
          oparr[i, 20] = iparr[i, 0]
          
          ; Green band
          oparr[i, 21] = iparr[i, 3]
          
          ; Red band
          oparr[i, 22] = iparr[i, 8]
          
          ; NIR band
          oparr[i, 23] = iparr[i, 17]
          
          ; SWIR1 band
          oparr[i, 24] = iparr[i, 20]
          
          ; SWIR2 band
          oparr[i, 25] = iparr[i, 23]
          
          ; Continuum Removal minima for Water 1
          oparr[i, 26] = crmin1[i]
          
          ; Continuum Removal minima for Water 2
          oparr[i, 27] = crmin2[i]
          
          ; Continuum Removal minima for Cellulose
          oparr[i, 28] = crmin3[i]
          
          ; SAV Vertical Index using SAV Line (SVIS) using bands (592) and (697) 
          ;psi = calculate_PSI( slopes, inters, iparr[i, 6], iparr[i, 10])
          ;oparr[i, 29] = psi[0]
          ; old way: 10*abs(slopes*float(iparr[i, 6]) - float(iparr[i, 9]) + inters)/(sqrt(slopes^2 + 1))
          
          ; SVIS distance from origin
          ;oparr[i, 30] = psi[1]
          
          ; SAV Vertical Index using Water Line (SVIW) using bands (657) and (697)  
          ;psi = calculate_PSI( slopew, interw, iparr[i, 7], iparr[i, 10])
          ;oparr[i, 31] = psi[0]
          
          ; SVIW distance from origin
          ;oparr[i, 32] = psi[1]
          
          ; two SAV indices, NDAVI and WAVI
          oparr[i, 29] = diff_index(tmparr[i, 1], tmparr[i, 0])
          oparr[i, 30] = ((tmparr[i, 1] - tmparr[i, 0])/(tmparr[i, 1] + tmparr[i, 0] + 0.5))*(1.5)
          
          ;oparr[i, 29] = 0
          ;oparr[i, 30] = 0
          oparr[i, 31] = 0
          oparr[i, 32] = 0
       endfor     ; end j

       writeu, ofid, oparr

    endfor     ; end i

    free_lun, ofid
    
    bnames = ['NDVI', 'gNDVI', 'RGRatio', 'NDWI', 'NDWI2', 'LPI', 'ANIR', 'ARed', 'ASWIR1', 'mNDVI',$
              'GI', 'PRI', 'CAI', 'WADI', 'ADW1', 'ADW2', 'SIPI', 'CRI550', 'CRI700', 'ARI', 'Blue',$ 
              'Green', 'Red', 'NIR', 'SWIR1', 'SWIR2', 'CRWat1', 'CRWat2', 'CRCell', 'NDAVI', 'WAVI',$
              'PSIw', 'PSIwDis']

    ; write header for index file
    envi_setup_head,fname=outname,ns=imagens,nl=imagenl,nb=outbands,interleave=1,map_info=map_info,bnames=bnames,data_type=4, /write
    print, 'written output index file: ', l+1

    ; close all files
    envi_file_mng, id=fid, /REMOVE
    envi_file_mng, id=cr1, /REMOVE
    envi_file_mng, id=cr2, /REMOVE
    envi_file_mng, id=cr3, /REMOVE


endfor   ; end l

envi_batch_exit

return

End ;of Main Procedure

function diff_index, b1, b2
  index = (float(b1) - float(b2))/(float(b1) + float(b2))
  return, index
end

function calculate_angle, b1, b2, b3, w1, w2, w3

  ; distances between vertices
  a2 = (double(abs(b1 - b2)))^2 + double(w2 - w1)^2
  d2 = (double(abs(b3 - b2)))^2 + double(w3 - w2)^2
  c2 = (double(abs(b1 - b3)))^2 + double(w3 - w1)^2

  angle = acos(double(a2 + d2 - c2)/double(2*(sqrt(a2))*(sqrt(d2))))

  ; test if angle is positive or negative
  pi = 3.14159265
  eix = w1 - w2
  eiy = b1 - b2
  ejx = w3 - w2
  ejy = b3 - b2
  test = eix*ejy - ejx*eiy
  if test lt 0 then angle = pi*2 - angle

  return, angle
end

function calculate_PSI, m, c, x0, y0

  ; Calculates PSI (using either water or SAV line) 
  ; Returns PSI & distance from origin
  ; where m=slope, c=intercept, (x0,y0) are band values of pixel in the two relevant bands
  
  retvar = fltarr(2)
  retvar[0] = 10*abs(m*float(x0) - float(y0) + c)/(sqrt(m^2 + 1))
  xpt = (float(x0) + m*float(y0) - m*c)/(m^2 + 1)
  ypt = m*xpt + c
  retvar[1] = sqrt(xpt^2 + ypt^2)
  
  return, retvar

end

function calculate_REIP, x, y
  
  ;---------------------------------------------------------------;
  ; * Baret et al. 1992 polynomial-fit technique to detect REIP * ;
  ;---------------------------------------------------------------;

  coeff = poly_fit(x, y, 4, YFIT = yfit)
  coeff = double(coeff)

  ; y_func = coeff[0] + coeff[1]*x + coeff[2]*x^2 + coeff[3]*x^3 + coeff[4]*x^4
  ; second derivative = 0 = coeff[2] + 3*coeff[3]*x + 6*coeff[4]*x^2

  a = 6.0*coeff[4]
  b = 3.0*coeff[3]
  c = coeff[2]

  x1 = (-b - SQRT(b*b - 4.0*a*c))/(2.0*a)   ; first  root of second derivative
  x2 = (-b + SQRT(b*b - 4.0*a*c))/(2.0*a)   ; second root of second derivative
  
  reip = MAX(x1, x2)                     ; larger root

  return, reip
  
end