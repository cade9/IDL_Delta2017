Pro I7_batch_avirisng2017_stack_bands

envi, /restore_base_save_files
envi_batch_init, log_file='batch.log'

startfile = 5    ; begin processing at this file number
endfile   = 4    ; end   processing at this file number

indxbands  = 33   ; number of index bands
sambands   = 15   ; number of SAM bands
smabands   = 7    ; number of SMA (or LSU) bands
mskbands   = 1    ; number of mask bands

; total output bands
outbands = indxbands + sambands + smabands

year = '201711\'

rootdir  = 'X:\delta_sav\raster\classification\'

mskdir   = 'X:\delta_sav\raster\masks\201711\fl_edge\'
outdir   = rootdir + 'allinputs\' + year
samdir   = rootdir + 'SAM_SMA\'   + year + 'SAM\'
smadir   = rootdir + 'SAM_SMA\'   + year + 'SMA\'
inputdir = rootdir + 'indices\'   + year

wildcard = '*'
outsuf = '_all.img'         ; all variables suffix
insuf  = '_indx.img'        ; index image suffix
samsuf = '_sam_rules.img'   ; SAM   image suffix
smasuf = '_sma.img'         ; SMA   image suffix
msksuf = '_msk.img'         ; mask  image suffix

; populate input filenames array
index_name = file_search(inputdir + wildcard + insuf )
sam_name   = file_search(samdir   + wildcard + samsuf)
sma_name   = file_search(smadir   + wildcard + smasuf)
msk_name   = file_search(mskdir   + wildcard + msksuf)
numimages  = n_elements(sam_name)

if numimages eq 0 then print, "Index files do not exist"


if endfile le startfile then endfile = numimages-1

;print, 'total number of images: ', numimages
print, 'total number of images to be processed: ', (endfile - startfile) + 1

; main base input files
imagebasenamearray=strarr(numimages)
for i = 0, numimages-1 do imagebasenamearray[i] = file_basename(msk_name[i])

; main output file name folders
outfile = strarr(numimages)

; populate the output file folder name list
for i=0,numimages-1 do outfile[i] = outdir + STRMID(imagebasenamearray[i], 0, STRPOS(imagebasenamearray[i], msksuf, /reverse_search)) + outsuf

pos_indx = indgen(indxbands)
pos_sam  = indgen(sambands)
pos_sma  = indgen(smabands)

indx_bnames  = ['NDVI', 'gNDVI', 'RGratio', 'NDWI', 'NDWI2', 'LPI', 'ANIR', 'ARed', 'ASWIR1', 'mNDVI', 'GI', 'PRI', 'CAI', 'WADI', 'ADW1', 'ADW2', 'SIPI', $
                'CRI_550', 'CRI_700', 'ARI', 'Blue', 'Green', 'Red', 'NIR', 'SWIR1', 'SWIR2', 'CRWat1', 'CRWat2', 'CRCell', 'NDAVI', 'WAVI', 'PSIw', 'PSIwDis']
sam_bnames   = ['SAM_MIL', 'SAM_PRIM', 'SAM_TULE', 'SAM_SOIL', 'SAM_WTUR', 'SAM_CLR', 'SAM_NPV', 'SAM_EGER', 'SAM_WHY', 'SAM_SAGO', 'SAM_PEN', 'SAM_CAT', $
                'SAM_CAB', 'SAM_CRLF', 'SAM_PHR']
sma_bnames   = ['SMA_WAT', 'SMA_SOIL', 'SMA_NPV', 'SMA_VEG', 'SMA_SAV', 'SMA_EMR', 'SMA_RMSE']
allbnames    = strarr(outbands)
allbnames    = [indx_bnames, sam_bnames, sma_bnames]

if endfile lt startfile then endfile = numimages-1

count = startfile

iparr  = fltarr(100, indxbands)     ; index file array
oparr  = fltarr(100, outbands)      ; output file array
smarr  = fltarr(100, smabands)      ; SMA file array
samarr = fltarr(100, sambands)      ; SAM file array
mskarr = bytarr(100)                ; mask file array

x  = indxbands - 1
xn = x + 1
y  = x + sambands
yn = y + 1
z  = y + smabands

for l = startfile, endfile do begin

    count = count+1
    print, "Processing file: ", count, "   Name: ", imagebasenamearray[l]

    if (file_test(outfile[l]) eq 1) then begin
        print, "Output file already exists: ", outfile[l]
        continue
    endif

    envi_open_file, index_name[l], r_fid = ifid
    envi_open_file, sam_name[l],   r_fid = samfid
    envi_open_file, sma_name[l],   r_fid = smafid
    envi_open_file, msk_name[l],   r_fid = mfid
    
    envi_file_query, ifid, nl = imagenl, ns = imagens, nb = imagenb
    
    map_info = envi_get_map_info(fid=ifid)
    dims = [-1, 0, imagens-1, 0, imagenl-1]

    iparr  = congrid(iparr,  imagens, indxbands)  ; input array
    oparr  = congrid(oparr,  imagens, outbands)   ; output array
    smarr  = congrid(smarr,  imagens, smabands)   ; SMA array
    samarr = congrid(samarr, imagens, sambands)   ; SAM array
    mskarr = congrid(mskarr, imagens)

    openw, opfid, outfile[l], /get_lun
    
    for j = 0, imagenl-1 do begin

       iparr  = envi_get_slice(fid = ifid,   line=j, pos=pos_indx)
       samarr = envi_get_slice(fid = samfid, line=j, pos=pos_sam)
       smarr  = envi_get_slice(fid = smafid, line=j, pos=pos_sma)
       mskarr = envi_get_slice(fid = mfid,   line=j, pos=0)

       for i = 0, imagens-1 do begin
       
          if mskarr[i] eq 0 then begin
            oparr[i, *] = 0
          endif else begin
            oparr[i,  0:x] = iparr[i, *]
            oparr[i, xn:y] = samarr[i, *]
            oparr[i, yn:z] = smarr[i, *]
          endelse
          
       endfor
       
       writeu, opfid, oparr

    endfor     ; end i

    free_lun, opfid

    envi_setup_head, fname=outfile[l], ns=imagens, nl=imagenl, nb=outbands, interleave=1, map_info=map_info, bnames=allbnames, data_type=4, /write

    print, 'written output index file: ', l+1

    envi_file_mng, id=ifid,   /REMOVE
    envi_file_mng, id=samfid, /REMOVE
    envi_file_mng, id=smafid, /REMOVE
    envi_file_mng, id=mfid,   /REMOVE

endfor   ; end l


envi_batch_exit

return

End ;of Main Procedure

