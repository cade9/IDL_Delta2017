Pro CD2_create_change_matrix_stackfile_1416

;**********************************
; example calculating change      *
; matrix for only 2 dates         *
; for a 12x12 pixel window        *
;**********************************

;**********************************
; RECLASSIFIED NUMBERS            *
; class 0: land                   *
; class 1: soil                   *
; class 2: water                  *
; class 3: sav                    *
; class 4: emergent               *
; class 5: hyacinth               *
; class 6: pennywort              *
; class 7: primrose               *
; class 8: npv                    *
; class 9: azldkwd                *
; class 10: senflt                *
;**********************************

outbands  = 17    ; number of output bands
inbands = 3       ; number of input bands in stacked image

; input and output directories
inpdir = 'X:\delta_sav\raster\analysis\change_detection\LibertyIslandWP\class_files_same_extent\'
outdir = 'X:\delta_sav\raster\analysis\change_detection\LibertyIslandWP\change_matrix\'

; names of input and output files
insuf   = 'stacked.img'
infile  = file_search(inpdir + '*' + insuf)
outfile = 'LibertyIsland_14_16_chmat_new.img'

; output file
outimg = outdir + outfile

pos1 = [0]     ; position for 2014 data
pos2 = [2]     ; position for 2016 data

winsize = 12  ; size of moving window

nopixls = winsize^2

; bandnames of output image
; band no.    0           1           2           3           4           5           6           7 
bnames = ['2014_emr', '2014_hya', '2014_npv', '2014_pen', '2014_pri', '2014_sav', '2014_soil', '2014_wat', $
          '2016_emr', '2016_hya', '2016_npv', '2016_pen', '2016_pri', '2016_sav', '2016_soil', '2016_wat', 'total_pixels'] 
; band no.    8           9          10          11          12          13          14           15             16

; open both input files
envi_open_file, infile[1], r_fid = ifid1
;envi_open_file, infile[1], r_fid = ifid2

; query file for samples, lines, etc. and setup dims
envi_file_query, ifid1, nl = imagenl, ns = imagens, interleave = ilv, dims=dims

; get map_info and modify pixel size to reflect new size
map_info = envi_get_map_info(fid = ifid1)
map_info.ps[0] = map_info.ps[0]*winsize
map_info.ps[1] = map_info.ps[1]*winsize

; input arrays # of samples x window size e.g. 10 lines read at a time
iparr1 = bytarr(imagens, winsize)
iparr2 = bytarr(imagens, winsize)

; output array size is smaller - each window corresponds to one pixel for output
opsamp  = floor(imagens/winsize)
opline  = floor(imagenl/winsize)
oparr   = intarr(opsamp, opline, outbands)
winsub1 = bytarr(winsize, winsize)
winsub2 = bytarr(winsize, winsize)
counts  = intarr(outbands)

; initialize output array
oparr[*, *, *] = 0

; open output file for writing
openw, ofid, outimg, /get_lun

; initialize output array subscripts
m = 0
n = 0

for j = 0, imagenl-winsize-1, winsize do begin

  ; m has to be initialized within n loop
  m = 0
  
  if ((j mod 300) eq 0) then print, 'processing line: ', j
  
  ; read the first n lines of the image (line by line) where n=winsize
  for k = 0, winsize-1 do begin
    iparr1[*, k] = envi_get_slice(fid=ifid1, line=j+k, pos=pos1)  ; get 2014 data
    iparr2[*, k] = envi_get_slice(fid=ifid1, line=j+k, pos=pos2)  ; get 2016 data
  endfor
  
  for i = 0, imagens-winsize-1, winsize do begin
    
    ; initialize counts
    counts1 = 0
    counts2 = 0
    
    ; for convenience, put the first 10x10 window values in separate arrays for 2014 & 2015
    winsub1 = iparr1[i:(i+winsize-1), *]
    winsub2 = iparr2[i:(i+winsize-1), *]
    
    ; count total number of non-masked pixels
    x = where(winsub1 ne 0, counts1)
    x = where(winsub2 ne 0, counts2)
    oparr[m, n, 16] = counts1
    
    ; if less than 50% pixels are information, abort CD pixel calculations
    if(counts1 lt nopixls/2 or counts2 lt nopixls/2) then begin
      ; don't forget to advance counter for sample number
      m = m + 1
      continue
    endif
    
;**********************************
; RECLASSIFIED NUMBERS            *
; class 0: land                   *
; class 1: soil                   *
; class 2: water                  *
; class 3: sav                    *
; class 4: emergent               *
; class 5: hyacinth               *
; class 6: pennywort              *
; class 7: primrose               *
; class 8: npv                    *
; class 9: azldkwd                *
; class 10: senflt                *
;**********************************

    ; count emergent pixels
    x = where((winsub1 eq 4), counts1)
    x = where((winsub2 eq 4), counts2)
    oparr[m, n, 0] = counts1
    oparr[m, n, 8] = counts2
    ; count hyacinth pixels
    x = where((winsub1 eq 5), counts1)
    x = where((winsub2 eq 5), counts2)
    oparr[m, n, 1] = counts1
    oparr[m, n, 9] = counts2
    ; count npv pixels
    x = where((winsub1 eq 8 or winsub1 eq 10), counts1)
    x = where((winsub2 eq 8 or winsub2 eq 10), counts2)
    oparr[m, n, 2]  = counts1
    oparr[m, n, 10] = counts2
    ; count pennywort pixels
    x = where(winsub1 eq 6, counts1)
    x = where(winsub2 eq 6, counts2)
    oparr[m, n, 3]  = counts1
    oparr[m, n, 11] = counts2
    ; count primrose pixels
    x = where(winsub1 eq 7, counts1)
    x = where(winsub2 eq 7, counts2)
    oparr[m, n, 4]  = counts1
    oparr[m, n, 12] = counts2
    ; count sav pixels
    x = where(winsub1 eq 3, counts1)
    x = where(winsub2 eq 3, counts2)
    oparr[m, n, 5]  = counts1
    oparr[m, n, 13] = counts2
    ; count soil pixels
    x = where(winsub1 eq 1, counts1)
    x = where(winsub2 eq 1, counts2)
    oparr[m, n, 6]  = counts1
    oparr[m, n, 14] = counts2
    ; count water pixels
    x = where(winsub1 eq 2, counts1)
    x = where(winsub2 eq 2, counts2)
    oparr[m, n, 7]  = counts1
    oparr[m, n, 15] = counts2

    m = m + 1
  endfor     ; end i
  
  n = n + 1
endfor     ; end j

writeu, ofid, oparr
free_lun, ofid

; modified map_info, interleave is 0 or bsq (I think), data_type is "integer", file_type is regular
envi_setup_head, fname=outimg, ns=opsamp, nl=opline, nb=outbands, map_info=map_info, interleave=0, data_type=2, bnames=bnames, /write
                    
print, 'written output file: ', outfile

envi_file_mng, id=ifid1, /REMOVE
;envi_file_mng, id=ifid2, /REMOVE

return

End ;of Main Procedure
