Pro CD1_reassign_bands_201610_data

;**********************************
; RECLASS ALL CLASSIFIED FILES    *
; class 0: land                   *
; class 1: soil                   * 8
; class 2: water                  * 9
; class 3: sav                    * 7
; class 4: emergent               * 1,6
; class 5: hyacinth               * 2
; class 6: pennywort              * 0
; class 7: primrose               * 5
; class 8: npv                    * 3,4
; class 9: senflt                 * 0
; class 10: azldkwd               * 0
;**********************************

outbands  = 1    ; number of output bands
inbands   = 2    ; number of input  bands
inclasses = 10   ; number of input  classes in 2015
otclasses = 11   ; number of output classes for 2014-2015

inpdir = 'X:\delta_sav\raster\classification\methods\201610\v4\'
optdir = 'X:\delta_sav\raster\analysis\change_detection\DeltaMinExtent\class_files_same_extent\'

insuf  = '_comp_2005mask.img'
imagename = file_search(inpdir + '*' + insuf)
outfile2016   = optdir + '201610_Central_Delta_reclass.img'
;outfile2016cm = inpdir + '201610_Central_Delta_reclass_cmask.img'

; table for changing band numbers
; lu2016 = bytarr(inclasses)
; new band numbers for output file 201610
lu2016 = [0, 4, 5, 0, 8, 9, 7, 4, 3, 1, 2]
         ;0  1  2  3  4  5  6  7  8  9, 10    ; 2016
         
lukup = [[0,0,0], [150,80,40], [0,255,255], [255,0,0], [0,125,0], [0,200,80], [255,255,0], [255,0,255], [255,165,0], [255,255,255], [0,255,0]]

class_names = ['land', 'soil', 'water', 'sav', 'emergent', 'hyacinth', 'pennywort', 'primrose', 'npv', 'senflt', 'azldkwd']

    envi_open_file, imagename, r_fid = fid
    envi_file_query, fid, nl = imagenl, ns = imagens, nb = imagenb, data_type = dtype, interleave=ilv
    
    ftype = 3     ; file type - envi classification
    
    map_info = envi_get_map_info(fid = fid)

    oparr2016 = bytarr(imagens)      ; output array
    
    openw, ofid, outfile2016, /get_lun

    for j = 0, imagenl-1 do begin

       iparr2016 = envi_get_slice(fid=fid, line=j, pos=0)
       oparr2016[*] = 0

       for i = 0, imagens-1 do begin
          if iparr2016[i] eq 0 then continue
          k = byte(iparr2016[i])
          oparr2016[i] = lu2016[k]
       endfor     ; end j
       writeu, ofid, oparr2016       
    endfor     ; end i
    
    free_lun, ofid

    envi_setup_head, fname=outfile2016, ns=imagens, nl=imagenl, nb=outbands, map_info=map_info, interleave=1, data_type=1, $
                     num_classes=otclasses, file_type=ftype, class_names=class_names, lookup = lukup, /write
                    
    print, 'written output file: ', outfile2016

    envi_file_mng, id=fid, /REMOVE
    
    ; begin processing of second file (with common mask)

;    envi_open_file, imagename[0], r_fid = fid
;    envi_file_query, fid, nl = imagenl, ns = imagens, nb = imagenb, data_type = dtype, interleave=ilv
;    
;    ftype = 3     ; file type - envi classification
;    
;    map_info = envi_get_map_info(fid = fid)
;    dims = [-1, 0, imagens-1, 0, imagenl-1]
;
;    oparr2016 = bytarr(imagens)      ; output array
;
;    openw, ofid, outfile2016cm, /get_lun
;
;    for j = 0, imagenl-1 do begin
;
;       iparr2016 = envi_get_slice(fid=fid, line=j, pos=0)
;       oparr2016[*] = 0
;
;       for i = 0, imagens-1 do begin
;          if iparr2016[i] eq 0 then continue
;          k = byte(iparr2016[i])
;          oparr2016[i] = lu2016[k]
;       endfor     ; end j
;       writeu, ofid, oparr2016       
;    endfor     ; end i
;
;    envi_setup_head, fname=outfile2016cm, ns=imagens, nl=imagenl, nb=outbands, map_info=map_info, interleave=1, data_type=1, $
;                     num_classes=otclasses, file_type=ftype, class_names=class_names, lookup = lukup, /write
;                    
;    print, 'written output file: ', outfile2016cm
;
;    envi_file_mng, id=fid, /REMOVE

return

End ;of Main Procedure