pro addpath,pathname,cut=cut
;+
; ROUTINE:  addpath
;
; PURPOSE:  
;
; USEAGE:   addpath,pathname,cut=cut
;
; INPUT:    
;   pathname   Name of new path to add to this session's directory path.
;              if pathname is already present in !path no action is taken.
;              This last feature allows addpath to be repeatedly executed
;              (e.g., within a script) without affecting !path after the
;              first invocation.
;
; KEYWORD INPUT:
;
;  cut         If set and pathname is set, the element of !path that
;              matches pathname is removed. If pathname is not
;              specified, the last element of !path is removed.
;
; OUTPUT:
;
; DISCUSSION:
;
; LIMITATIONS:
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;  
; EXAMPLES:  
;           addpath,'/home/paul/idl'            ; add a path
;
;           addpath,'/local/idl/lib/wided',/cut ; remove a specific path
;
;           addpath,/cut                        ; remove last path element 
;
;           addpath                             ; view !path
;
; AUTHOR:   Paul Ricchiazzi                        04 Nov 97
;           Institute for Computational Earth System Science
;           University of California, Santa Barbara
;           paul@icess.ucsb.edu
;
; REVISIONS:
;
;           10/24/12, TPEB -- Replaced str_sep with strplit, as the
;                             former is depricated.
;-
;
patharr=strsplit(!path,':',/extract)
npath=n_elements(patharr)
if keyword_set(cut) then begin
  if keyword_set(pathname) then begin
    ii=where(patharr ne pathname,nm)
    !path=patharr(ii(0))
    for i=1,nm-1 do !path=!path+':'+patharr(ii(i))
  endif else begin 
  !path=patharr(0)
    for i=1,npath-2 do !path=!path+':'+patharr(i)
  endelse
  return
endif

if keyword_set(pathname) then begin
  ii=where(patharr eq pathname,nc)
  if nc ne 0 then return
  !path=!path + ':' + pathname
  return
endif


print,f='(a,t40,a)', str_sep(!path,':')

end
