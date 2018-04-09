Pro CD4_2016_extract_allyearpairs

	;**************************************************;
	; CHANGE DETECTION MATRIX FOR ALL THREE YEAR PAIRs ;
	;**************************************************;
	
	; name of input files
	infile1 = 'X:\delta_sav\raster\analysis\change_detection\LibertyIslandWP\percent_change\LibertyIslandWP_04_08_perch.img'
  infile2 = 'X:\delta_sav\raster\analysis\change_detection\LibertyIslandWP\percent_change\LibertyIslandWP_08_14_perch.img'
  infile3 = 'X:\delta_sav\raster\analysis\change_detection\LibertyIslandWP\percent_change\LibertyIslandWP_14_16_perch.img'
	
  ; name of output files
  outfile1 = 'X:\delta_sav\raster\analysis\change_detection\LibertyIslandWP\percent_change\LibertyIslandWP_04_08_extractall.csv'
  outfile2 = 'X:\delta_sav\raster\analysis\change_detection\LibertyIslandWP\percent_change\LibertyIslandWP_08_14_extractall.csv'
  outfile3 = 'X:\delta_sav\raster\analysis\change_detection\LibertyIslandWP\percent_change\LibertyIslandWP_14_16_extractall.csv'

  ; open output files to write extracted data
  openw, lun1, outfile2, /get_lun

  ; open input files
	envi_open_file, infile2, r_fid = ifid1
	
	; query input file
	envi_file_query, ifid1, dims = dims, ns = imagens, nl = imagenl, nb = imagenb
  ; position of all bands
  pos = indgen(imagenb)
  ttlbands = imagenb + 1
  
  for j = 0, imagenl-1, 2 do begin
    
    ; read input file line by line
    iparr1  = envi_get_slice(fid = ifid1, line = j, pos = pos)
    
    if ((j mod 50) eq 0) then print, 'processing line: ', j

    for i = 0, imagens - 1, 2 do begin
    
      ; for simplification, one pixel is saved at separate vari♣able
      pixel1 = iparr1[i, *]

      ; if CD pixel has data, then get it
      if (pixel1[imagenb-1] ne 0) then begin
      
        ;printf, lun1, i, ",", j, ",", pixel1, FORMAT='(f10.1, f10.1, 11(f10.5, ","))'
        printf, lun1, pixel1, FORMAT='(11(f10.5, ","))'
 
      endif
    endfor    ; end i (number of samples)
  endfor   ; end j  (number of lines)
        
  ; write output line by line      
  free_lun, lun1
    
  ; close input files
  envi_file_mng, id = ifid1, /REMOVE
  
return

End ;of Main Procedure