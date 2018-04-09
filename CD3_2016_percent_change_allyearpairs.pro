Pro CD3_2016_percent_change_allyearpairs

	;**************************************************;
	; CHANGE DETECTION MATRIX FOR ALL THREE YEAR PAIRs ;
	;**************************************************;
	
	; name of input file
	infile1 = 'X:\delta_sav\raster\analysis\change_detection\LibertyIslandWP\change_matrix\LibertyIsland_04_08_chmat_new.img'
  infile2 = 'X:\delta_sav\raster\analysis\change_detection\LibertyIslandWP\change_matrix\LibertyIsland_14_16_chmat_new.img'

	; name of output file
	outfile1 = 'X:\delta_sav\raster\analysis\change_detection\LibertyIslandWP\percent_change\LibertyIslandWP_04_08_perch.img'
  outfile2 = 'X:\delta_sav\raster\analysis\change_detection\LibertyIslandWP\percent_change\LibertyIslandWP_08_14_perch.img'
  outfile3 = 'X:\delta_sav\raster\analysis\change_detection\LibertyIslandWP\percent_change\LibertyIslandWP_14_16_perch.img'
	
	; band names
	bnames = ['perch_emr', 'perch_hya', 'perch_npv', 'perch_pen', 'perch_pri', 'perch_sav', $
	          'perch_soil', 'perch_wat', 'perch_allemr', 'perch_allflt', 'perch_wpixels']
	          
	; number of output bands
	outbands = n_elements(bnames)

  ; open input file
	envi_open_file, infile1, r_fid = ifid1
	envi_open_file, infile2, r_fid = ifid2
	
	; query input file
	envi_file_query, ifid1, dims = dims, ns = imagens, nl = imagenl, nb = imagenb
  ; position of all bands
  pos = indgen(imagenb)
	
	; define output arrays
	oparr1 = fltarr(imagens, outbands)
  oparr2 = fltarr(imagens, outbands)
  oparr3 = fltarr(imagens, outbands)
	
	; get map info from input image
  map_info = envi_get_map_info(fid = ifid1)
	
	; open the output file for writing
  openw, ofid1, outfile1, /get_lun
  openw, ofid2, outfile2, /get_lun
  openw, ofid3, outfile3, /get_lun
  
  for j = 0, imagenl - 1 do begin
    
    ; read input file line by line
    iparr1  = envi_get_slice(fid = ifid1, line = j, pos = pos)
    iparr2  = envi_get_slice(fid = ifid2, line = j, pos = pos)
    
    for i = 0, imagens - 1 do begin
    
      ; for simplification, one pixel is saved at separate variable
      pixel1 = iparr1[i, *]
      pixel2 = iparr2[i, *]
      
      ; initialize output array
      oparr1[i, *] = 0
      oparr2[i, *] = 0
      oparr3[i, *] = 0
      
      ; if CD window has no data, then continue
      if ((total(pixel1[0:15]) eq 0) or total(pixel2[0:15]) eq 0) then continue
      
      ;    0           1           2           3           4           5           6           7 
      ;'2004_emr', '2004_hya', '2004_npv', '2004_pen', '2004_pri', '2004_sav', '2004_soil', '2004_wat', $
      ;'2008_emr', '2008_hya', '2008_npv', '2008_pen', '2008_pri', '2008_sav', '2008_soil', '2008_wat', 'total_pixels'] 
      ;    8           9          10          11          12          13          14           15             16
      
      ;    0           1           2           3           4           5           6           7 
      ;'2014_emr', '2014_hya', '2014_npv', '2014_pen', '2014_pri', '2014_sav', '2014_soil', '2014_wat', $
      ;'2016_emr', '2016_hya', '2016_npv', '2016_pen', '2016_pri', '2016_sav', '2016_soil', '2016_wat', 'total_pixels'] 
      ;    8           9          10          11          12          13          14           15             16

      ; total floating vegetation in 2004, 2008, 2014 & 2016
      totflt04 = pixel1[1] + pixel1[3]  + pixel1[4]
      totflt08 = pixel1[9] + pixel1[11] + pixel1[12]
      totflt14 = pixel2[1] + pixel2[3]  + pixel2[4]
      totflt16 = pixel2[9] + pixel2[11] + pixel2[12]
      
      ; ###############################
      ; ####  2004-2008 YEAR-PAIR  ####
      ; ###############################

      if(pixel1[16] gt 50) then begin

      ; percent change in emergent (8  and 0)
      oparr1[i, 0] = float(pixel1[8] - pixel1[0])/float(pixel1[16])
      ; percent change in hyacinth (9  and 1)
      oparr1[i, 1] = float(pixel1[9] - pixel1[1])/float(pixel1[16])
      ; percent change in npv      (10 and 2)
      oparr1[i, 2] = float(pixel1[10] - pixel1[2])/float(pixel1[16])
      ; percent change in pennywort(11 and 3)
      oparr1[i, 3] = float(pixel1[11] - pixel1[3])/float(pixel1[16])
      ; percent change in primrose (12 and 4)
      oparr1[i, 4] = float(pixel1[12] - pixel1[4])/float(pixel1[16])
      ; percent change in sav      (13 and 5)
      oparr1[i, 5] = float(pixel1[13] - pixel1[5])/float(pixel1[16])
      ; percent change in soil     (14 and 6)
      oparr1[i, 6] = float(pixel1[14] - pixel1[6])/float(pixel1[16])
      ; percent change in water    (15 and 7)
      oparr1[i, 7] = float(pixel1[15] - pixel1[7])/float(pixel1[16])
      ; percent change in emergent and NPV as one unit (0,2 & 8,10)
      oparr1[i, 8] = float((pixel1[8] + pixel1[10]) - (pixel1[0] + pixel1[2]))/float(pixel1[16])
      ; percent change in all floating (1,3,4 & 9,11,12)
      oparr1[i, 9] = float(totflt08 - totflt04)/float(pixel1[16])
      ; percent of pixels not masked in CD window
      oparr1[i, 10] = float(pixel1[16])/100
      
      endif
          
      ; ###############################
      ; ####  2008-2014 YEAR-PAIR  ####
      ; ###############################

      ; only calculate this window if it is full in both time periods
      if(pixel1[16] gt 50 and pixel2[16] gt 72) then begin
      
      ; percent change in emergent (8  and 0)
      oparr2[i, 0] = float(pixel2[0])/float(pixel2[16]) - float(pixel1[8])/float(pixel1[16])
      ; percent change in hyacinth (9  and 1)
      oparr2[i, 1] = float(pixel2[1])/float(pixel2[16]) - float(pixel1[9])/float(pixel1[16])
      ; percent change in npv      (10 and 2)
      oparr2[i, 2] = float(pixel2[2])/float(pixel2[16]) - float(pixel1[10])/float(pixel1[16])
      ; percent change in pennywort(11 and 3)
      oparr2[i, 3] = float(pixel2[3])/float(pixel2[16]) - float(pixel1[11])/float(pixel1[16])
      ; percent change in primrose (12 and 4)
      oparr2[i, 4] = float(pixel2[4])/float(pixel2[16]) - float(pixel1[12])/float(pixel1[16])
      ; percent change in sav      (13 and 5)
      oparr2[i, 5] = float(pixel2[5])/float(pixel2[16]) - float(pixel1[13])/float(pixel1[16])
      ; percent change in soil     (14 and 6)
      oparr2[i, 6] = float(pixel2[6])/float(pixel2[16]) - float(pixel1[14])/float(pixel1[16])
      ; percent change in water    (15 and 7)
      oparr2[i, 7] = float(pixel2[7])/float(pixel2[16]) - float(pixel1[15])/float(pixel1[16])
      ; percent change in emergent and NPV as one unit (0,2 & 8,10)
      oparr2[i, 8] = float(pixel2[0] + pixel2[2])/float(pixel2[16]) - float(pixel1[8] + pixel1[10])/float(pixel1[16])
      ; percent change in all floating (1,3,4 & 9,11,12)
      oparr2[i, 9] = float(totflt14)/float(pixel2[16]) - float(totflt08)/float(pixel1[16])
      
      ; average percent of pixels not masked in CD window in all yearpairs
      oparr2[i, 10] = (float(pixel1[16])/100 + float(pixel2[16])/144)/2
      
      endif

      ; ###############################
      ; ####  2014-2016 YEAR-PAIR  ####
      ; ###############################

      if(pixel2[16] gt 72) then begin

      ; percent change in emergent (8  and 0)
      oparr3[i, 0] = float(pixel2[8] - pixel2[0])/float(pixel2[16])
      ; percent change in hyacinth (9  and 1)
      oparr3[i, 1] = float(pixel2[9] - pixel2[1])/float(pixel2[16])
      ; percent change in npv      (10 and 2)
      oparr3[i, 2] = float(pixel2[10] - pixel2[2])/float(pixel2[16])
      ; percent change in pennywort(11 and 3)
      oparr3[i, 3] = float(pixel2[11] - pixel2[3])/float(pixel2[16])
      ; percent change in primrose (12 and 4)
      oparr3[i, 4] = float(pixel2[12] - pixel2[4])/float(pixel2[16])
      ; percent change in sav      (13 and 5)
      oparr3[i, 5] = float(pixel2[13] - pixel2[5])/float(pixel2[16])
      ; percent change in soil     (14 and 6)
      oparr3[i, 6] = float(pixel2[14] - pixel2[6])/float(pixel2[16])
      ; percent change in water    (15 and 7)
      oparr3[i, 7] = float(pixel2[15] - pixel2[7])/float(pixel2[16])
      ; percent change in emergent and NPV as one unit (0,2,6 & 8,10,14)
      oparr3[i, 8] = float((pixel2[8] + pixel2[10]) - (pixel2[0] + pixel2[2]))/float(pixel2[16])
      ; percent change in all floating (1,3,4 & 9,11,12)
      oparr3[i, 9] = float(totflt16 - totflt14)/float(pixel2[16])
      ; percent of pixels not masked in CD window
      oparr3[i, 10] = float(pixel2[16])/144
    
      endif
    
    endfor    ; end i (number of samples)
    
    ; write output line by line
    writeu, ofid1, oparr1
    writeu, ofid2, oparr2
    writeu, ofid3, oparr3
    
  endfor   ; end j  (number of lines)
      
  ; free the pointer to output file
  free_lun, ofid1
  free_lun, ofid2
  free_lun, ofid3
    
  ; write header for output file - interleave (bil), data type (floating pt.)
  envi_setup_head, fname = outfile1, ns = imagens, nl = imagenl, nb = outbands, interleave = 1, $
                   map_info = map_info, bnames = bnames, data_type = 4, /write
  envi_setup_head, fname = outfile2, ns = imagens, nl = imagenl, nb = outbands, interleave = 1, $
                   map_info = map_info, bnames = bnames, data_type = 4, /write
  envi_setup_head, fname = outfile3, ns = imagens, nl = imagenl, nb = outbands, interleave = 1, $
                   map_info = map_info, bnames = bnames, data_type = 4, /write

  ; close input files
  envi_file_mng, id = ifid1, /REMOVE
  envi_file_mng, id = ifid2, /REMOVE
  
return

End ;of Main Procedure